use super::*;
use crate::bytecode::FunctionDef;
use crate::instruction::{Instruction, Opcode};
use std::collections::{BTreeMap, HashMap};
use vo_common_core::bytecode::{
    InterfaceMeta, InterfaceMethodMeta, Itab, MethodInfo, NamedTypeMeta, StructMeta,
};
use vo_common_core::runtime_type::InterfaceMethod;
use vo_runtime::bytecode::JitInstructionMetadata;
use vo_runtime::itab::ItabCache;
use vo_runtime::objects::{closure, slice, string};
use vo_runtime::SlotType;

fn compact_pattern_position(compact: &[u8], pattern: &str) -> Option<usize> {
    vo_source_contract::compact_pattern_position(compact, pattern)
}

fn compact_contains(compact: &[u8], pattern: &str) -> bool {
    vo_source_contract::compact_contains(compact, pattern)
}

fn compact_region_between(source: &str, marker: &str, terminator: &str) -> Option<Vec<u8>> {
    vo_source_contract::compact_region_between(source, marker, terminator)
}

fn extern_replay_setup_stack_overflow_closes_scope_062(source: &str) -> bool {
    let src = crate::source_contract::production_source_without_test_modules(source);
    let Some(replay_setup) = compact_region_between(
        &src,
        "pub(crate)fncall_extern_replay_closure",
        "fnvalidate_closure_target",
    ) else {
        return false;
    };
    for (marker, terminator) in [
        (
            "try_reserve_call_window(new_bp,local_slots).is_err(){",
            "self.fiber.zero_slots_tail_at",
        ),
        (
            "try_push_call_frame(",
            "self.fiber.closure_replay.push_depth",
        ),
    ] {
        let Some(branch) = compact_region_between_compact(&replay_setup, marker, terminator) else {
            return false;
        };
        let Some(close_pos) = compact_pattern_position(
            &branch,
            "self.fiber.closure_replay.finish_extern_terminal();",
        ) else {
            return false;
        };
        let Some(trap_pos) = compact_pattern_position(&branch, "returnruntime_trap(") else {
            return false;
        };
        if !(close_pos < trap_pos && compact_contains(&branch, "RuntimeTrapKind::StackOverflow")) {
            return false;
        }
    }
    true
}

fn compact_region_between_compact(
    compact: &[u8],
    marker: &str,
    terminator: &str,
) -> Option<Vec<u8>> {
    vo_source_contract::compact_region_between_compact(compact, marker, terminator)
}

fn test_function(param_slots: u16, local_slots: u16, is_closure: bool) -> FunctionDef {
    test_function_with_recv(param_slots, local_slots, 0, is_closure)
}

fn with_capture_slots(mut func: FunctionDef, captures: usize) -> FunctionDef {
    func.capture_slot_types = vec![SlotType::GcRef; captures];
    func
}

fn test_function_with_recv(
    param_slots: u16,
    local_slots: u16,
    recv_slots: u16,
    is_closure: bool,
) -> FunctionDef {
    let slot_types = vec![SlotType::Value; local_slots as usize];
    FunctionDef {
        name: "callee".to_string(),
        param_count: param_slots,
        param_slots,
        local_slots,
        gc_scan_slots: FunctionDef::compute_gc_scan_slots(&slot_types),
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        code: Vec::<Instruction>::new(),
        jit_metadata: Vec::<JitInstructionMetadata>::new(),
        borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&slot_types),
        slot_types,
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    }
}

fn module_with_callee(callee: FunctionDef) -> Module {
    let mut module = Module::new("frame-call-builder-test".to_string());
    module.functions.push(callee);
    module
}

fn module_with_named_string_receiver_callee(callee: FunctionDef) -> Module {
    let mut module = module_with_callee(callee);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 0,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 0,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "NamedString".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::String),
        underlying_rttid: ValueRttid::new(0, ValueKind::String),
        methods,
    });
    module
}

fn empty_payload() -> TypedSlotPayload {
    TypedSlotPayload::try_new(Vec::new(), Vec::new()).expect("empty payload")
}

fn string_transfer() -> TransferType {
    TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::String).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::String).to_raw(),
        slots: 1,
    }
}

fn interface_transfer(meta_id: u32, rttid: u32) -> TransferType {
    TransferType {
        meta_raw: ValueMeta::new(meta_id, ValueKind::Interface).to_raw(),
        rttid_raw: ValueRttid::new(rttid, ValueKind::Interface).to_raw(),
        slots: 2,
    }
}

fn extern_replay_single_method_interface_module(callee: FunctionDef) -> (Module, ItabCache) {
    let mut module = module_with_callee(callee);
    module.interface_metas.push(InterfaceMeta {
        name: "A".to_string(),
        method_names: vec!["M".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "M".to_string(),
            signature_rttid: 2,
        }],
    });
    module.runtime_types.push(RuntimeType::Interface {
        methods: vec![InterfaceMethod {
            name: "M".to_string(),
            sig: ValueRttid::new(2, ValueKind::Closure),
        }],
        meta_id: 0,
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 7,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 2,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::String),
        underlying_rttid: ValueRttid::new(1, ValueKind::String),
        methods,
    });
    let itab_cache = ItabCache::from_module_itabs(vec![
        Itab::default(),
        Itab {
            iface_meta_id: 0,
            methods: vec![7],
        },
    ]);
    (module, itab_cache)
}

fn extern_replay_empty_interface_module(callee: FunctionDef) -> Module {
    let mut module = module_with_callee(callee);
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module.runtime_types.push(RuntimeType::Interface {
        methods: Vec::new(),
        meta_id: 0,
    });
    module
}

fn caller_with_call_layout(
    local_slots: u16,
    arg_layout: Vec<SlotType>,
    ret_layout: Vec<SlotType>,
) -> FunctionDef {
    let mut caller = test_function(0, local_slots, false);
    caller.jit_metadata = vec![JitInstructionMetadata::CallLayout {
        arg_layout,
        ret_layout,
    }];
    caller
}

fn mark_fetched_callsite(fiber: &mut Fiber) {
    fiber.current_frame_mut().expect("current frame").pc = 1;
}

#[test]
fn vm_call_closure_canon_001_borrowed_call_stores_canonical_slot0_for_closure_get() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let caller = caller_with_call_layout(2, Vec::new(), Vec::new());
    let callee = with_capture_slots(test_function(1, 2, true), 1);
    let mut module = Module::new("closure-call-canonical-slot0-test".to_string());
    module.functions.push(caller);
    module.functions.push(callee);

    let caller_bp = fiber.push_frame(0, 2, 0, 0, 0);
    mark_fetched_callsite(&mut fiber);
    let closure_ref = closure::create(&mut gc, 1, 1);
    // Safety: test fixture initializes a freshly allocated closure.
    unsafe { closure::set_capture(closure_ref, 0, 777) };
    let interior_ref = unsafe { closure_ref.add(closure::HEADER_SLOTS) };
    fiber.stack[caller_bp] = interior_ref as u64;

    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module).call_closure_borrowed(
        interior_ref as u64,
        1,
        0,
        0,
    );

    assert!(matches!(result, ExecResult::FrameChanged));
    let frame = *fiber.frames.last().expect("closure frame");
    assert_eq!(frame.func_id, 1);
    assert_eq!(fiber.stack[frame.bp], closure_ref as u64);

    let inst = Instruction::new(Opcode::ClosureGet, 1, 0, 0);
    crate::exec::exec_closure_get(&gc, fiber.stack_ptr(), frame.bp, &inst).expect("closure get");
    assert_eq!(fiber.stack[frame.bp + 1], 777);
}

#[test]
fn vm_closure_call_signature_002_call_closure_rejects_arg_slot_shape_drift_before_frame_push() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let caller = caller_with_call_layout(4, Vec::new(), Vec::new());
    let callee = with_capture_slots(test_function(2, 3, true), 1);
    let mut module = Module::new("closure-call-arg-shape-test".to_string());
    module.functions.push(caller);
    module.functions.push(callee);

    let caller_bp = fiber.push_frame(0, 4, 0, 0, 0);
    mark_fetched_callsite(&mut fiber);
    let closure_ref = closure::create(&mut gc, 1, 1);
    fiber.stack[caller_bp] = closure_ref as u64;

    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module).call_closure_borrowed(
        closure_ref as u64,
        1,
        0,
        0,
    );

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("CallClosure arg slot count"), "{msg}");
        }
        other => panic!("arg shape drift should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
}

#[test]
fn vm_closure_call_signature_002_call_closure_rejects_return_slot_shape_drift_before_frame_push() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let caller = caller_with_call_layout(4, Vec::new(), Vec::new());
    let mut callee = with_capture_slots(test_function(2, 3, true), 1);
    callee.ret_slots = 1;
    callee.ret_slot_types = vec![SlotType::Value];
    let mut module = Module::new("closure-call-ret-shape-test".to_string());
    module.functions.push(caller);
    module.functions.push(callee);

    let caller_bp = fiber.push_frame(0, 4, 0, 0, 0);
    mark_fetched_callsite(&mut fiber);
    let closure_ref = closure::create(&mut gc, 1, 1);
    fiber.stack[caller_bp] = closure_ref as u64;

    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module).call_closure_borrowed(
        closure_ref as u64,
        1,
        1,
        0,
    );

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("CallClosure return slot count"), "{msg}");
        }
        other => panic!("return shape drift should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
}

#[test]
fn vm_closure_call_signature_002_call_closure_rejects_arg_slot_metadata_drift_before_frame_push() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let caller = caller_with_call_layout(4, vec![SlotType::Value], Vec::new());
    let mut callee = test_function(2, 2, true);
    callee.slot_types = vec![SlotType::GcRef, SlotType::GcRef];
    callee.gc_scan_slots = 2;
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    let mut module = Module::new("closure-call-arg-metadata-test".to_string());
    module.functions.push(caller);
    module.functions.push(callee);

    let caller_bp = fiber.push_frame(0, 4, 0, 0, 0);
    mark_fetched_callsite(&mut fiber);
    let closure_ref = closure::create(&mut gc, 1, 0);
    fiber.stack[caller_bp] = closure_ref as u64;

    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module).call_closure_borrowed(
        closure_ref as u64,
        1,
        1,
        0,
    );

    match result {
        ExecResult::JitError(msg) => {
            assert!(
                msg.contains("CallClosure arg slot metadata mismatch"),
                "{msg}"
            );
        }
        other => panic!("metadata drift should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
}

#[test]
fn vm_call_closure_canon_001_extern_replay_closure_uses_canonical_ref_for_slot0() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let module = module_with_callee(with_capture_slots(test_function(1, 2, true), 1));
    let closure_ref = closure::create(&mut gc, 0, 1);
    let interior_ref = unsafe { closure_ref.add(closure::HEADER_SLOTS) };

    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(interior_ref, empty_payload());

    assert!(matches!(result, ExecResult::FrameChanged));
    let frame = fiber.frames.last().expect("closure frame");
    assert_eq!(frame.func_id, 0);
    assert_eq!(fiber.stack[frame.bp], closure_ref as u64);
    assert_eq!(fiber.closure_replay.depth, fiber.frames.len());
}

#[test]
fn vm_extern_replay_setup_stack_overflow_062_closes_scope_before_runtime_trap() {
    assert!(
            extern_replay_setup_stack_overflow_closes_scope_062(include_str!("../frame_call.rs")),
            "extern replay setup must not leave an active replay scope when stack overflow unwinds through recover"
        );
}

#[test]
fn vm_extern_replay_setup_stack_overflow_062_rejects_comment_spoofed_scope_close() {
    let spoof = r#"
            pub(crate) fn call_extern_replay_closure() {
                if self.fiber.try_reserve_call_window(new_bp, local_slots).is_err() {
                    // self.fiber.closure_replay.finish_extern_terminal();
                    return runtime_trap(RuntimeTrapKind::StackOverflow);
                }
                self.fiber.zero_slots_tail_at(new_bp, 0, 0);
                if self.fiber.try_push_call_frame().is_err() {
                    // self.fiber.closure_replay.finish_extern_terminal();
                    return runtime_trap(RuntimeTrapKind::StackOverflow);
                }
                self.fiber.closure_replay.push_depth(self.fiber.frames.len());
            }

            fn validate_closure_target() {}
        "#;

    assert!(
        !extern_replay_setup_stack_overflow_closes_scope_062(spoof),
        "comment-only extern replay stack-overflow cleanup must not satisfy source contracts"
    );
}

#[test]
fn vm_closure_call_signature_002_extern_replay_closure_rejects_non_closure_before_header_read() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let module = module_with_callee(test_function(0, 1, false));
    let string_ref = string::new_from_string(&mut gc, "not a closure".to_string());

    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(string_ref, empty_payload());

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("non-closure object kind"), "{msg}");
        }
        other => panic!("non-closure replay target should be fatal infra, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn vm_closure_call_signature_002_extern_replay_rejects_capture_count_metadata_drift_before_frame_push(
) {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 2, 0, 0, 0);
    let mut callee = test_function(1, 2, true);
    callee.capture_slot_types = vec![SlotType::GcRef];
    let module = module_with_callee(callee);
    let closure_ref = closure::create(&mut gc, 0, 0);

    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(closure_ref, empty_payload());

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("closure capture count"), "{msg}");
            assert!(msg.contains("expected 1"), "{msg}");
        }
        other => panic!("capture metadata drift should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn vm_extern_replay_call_frame_shape_062_rejects_scan_slots_beyond_locals_before_frame_publication()
{
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function(1, 1, true);
    callee.gc_scan_slots = 2;
    let module = module_with_callee(callee);
    let closure_ref = closure::create(&mut gc, 0, 0);
    let before_frames = fiber.frames.len();
    let before_sp = fiber.sp;
    let before_stack = fiber.stack.clone();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        FrameCallBuilder::new(&mut gc, &mut fiber, &module)
            .call_extern_replay_closure(closure_ref, empty_payload())
    }));

    let result = result.expect("extern replay frame-shape drift must not panic");
    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("gc_scan_slots"), "{msg}");
        }
        other => panic!("extern replay frame-shape drift should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), before_frames);
    assert_eq!(fiber.sp, before_sp);
    assert_eq!(fiber.stack, before_stack);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn vm_jit_extern_suspend_062_typed_replay_args_rejects_scan_slots_beyond_locals_before_payload_publication(
) {
    let mut gc = Gc::new();
    let mut callee = test_function(1, 1, true);
    callee.gc_scan_slots = 2;
    let module = module_with_callee(callee);
    let closure_ref = closure::create(&mut gc, 0, 0);

    let err = typed_extern_replay_args(&gc, &module, &ItabCache::new(), closure_ref, Vec::new())
        .expect_err("typed extern replay args must reject malformed callee frames");

    assert!(err.contains("invalid target frame shape"), "{err}");
    assert!(err.contains("gc_scan_slots=2"), "{err}");
    assert!(err.contains("local_slots=1"), "{err}");
}

#[test]
fn vm_closure_call_signature_002_extern_replay_rejects_closure_allocation_drift_before_frame_push()
{
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 2, 0, 0, 0);
    let module = module_with_callee(test_function(1, 2, true));
    let closure_ref = closure::create(&mut gc, 0, 0);
    unsafe { Gc::header_mut(closure_ref) }.slots = (closure::HEADER_SLOTS + 1) as u16;

    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(closure_ref, empty_payload());

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("closure layout slot count mismatch"), "{msg}");
            assert!(msg.contains("allocation 1"), "{msg}");
        }
        other => panic!("closure allocation drift should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn extern_replay_closure_accepts_recv_slots_arg_offset() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let module = module_with_callee(test_function_with_recv(2, 3, 2, false));
    let closure_ref = closure::create(&mut gc, 0, 1);
    // Safety: test fixture initializes a freshly allocated closure.
    unsafe { closure::set_capture(closure_ref, 0, 444) };

    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(closure_ref, empty_payload());

    match result {
        ExecResult::JitError(msg) => {
            assert!(
                msg.contains("closure capture count 1 does not match expected 2"),
                "{msg}"
            );
        }
        other => panic!("partial method closure receiver should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);

    let closure_ref = closure::create(&mut gc, 0, 2);
    // Safety: test fixture initializes a freshly allocated closure.
    unsafe { closure::set_capture(closure_ref, 0, 444) };
    // Safety: test fixture initializes a freshly allocated closure.
    unsafe { closure::set_capture(closure_ref, 1, 555) };

    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(closure_ref, empty_payload());

    assert!(matches!(result, ExecResult::FrameChanged));
    let frame = fiber.frames.last().expect("closure frame");
    assert_eq!(frame.func_id, 0);
    assert_eq!(fiber.stack[frame.bp], 444);
    assert_eq!(fiber.stack[frame.bp + 1], 555);
}

#[test]
fn vm_extern_replay_transfer_contract_061_accepts_explicit_receiver_prefix() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function_with_recv(2, 2, 1, false);
    callee.slot_types = vec![SlotType::GcRef, SlotType::GcRef];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.param_types = vec![string_transfer()];
    let module = module_with_named_string_receiver_callee(callee);
    let closure_ref = closure::create(&mut gc, 0, 0);
    let receiver = string::new_from_string(&mut gc, "receiver".to_string());
    let name = string::new_from_string(&mut gc, "field".to_string());

    let payload = typed_extern_replay_args(
        &gc,
        &module,
        &ItabCache::new(),
        closure_ref,
        vec![receiver as u64, name as u64],
    )
    .expect("explicit receiver slot should prefix user param transfer metadata");

    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(closure_ref, payload);

    assert!(matches!(result, ExecResult::FrameChanged));
    let frame = fiber.frames.last().expect("closure frame");
    assert_eq!(frame.func_id, 0);
    assert_eq!(fiber.stack[frame.bp], receiver as u64);
    assert_eq!(fiber.stack[frame.bp + 1], name as u64);
}

#[test]
fn vm_extern_replay_transfer_contract_061_validates_synthesized_receiver_prefix() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function_with_recv(2, 2, 1, false);
    callee.slot_types = vec![SlotType::GcRef, SlotType::GcRef];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.param_types = vec![string_transfer()];
    let module = module_with_named_string_receiver_callee(callee);
    let closure_ref = closure::create(&mut gc, 0, 0);
    let wrong_receiver = closure_ref;
    let name = string::new_from_string(&mut gc, "field".to_string());

    let payload = typed_extern_replay_args(
        &gc,
        &module,
        &ItabCache::new(),
        closure_ref,
        vec![wrong_receiver as u64, name as u64],
    )
    .expect_err("synthesized receiver transfer metadata must validate receiver object kind");
    assert!(payload.contains("object kind"), "{payload}");
}

#[test]
fn vm_extern_replay_transfer_contract_061_validates_receiver_with_empty_param_types_and_scalar_suffix(
) {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function_with_recv(2, 2, 1, false);
    callee.slot_types = vec![SlotType::GcRef, SlotType::Value];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    let module = module_with_named_string_receiver_callee(callee);
    let closure_ref = closure::create(&mut gc, 0, 0);
    let wrong_receiver = closure_ref;

    let payload = typed_extern_replay_args(
        &gc,
        &module,
        &ItabCache::new(),
        closure_ref,
        vec![wrong_receiver as u64, 7],
    )
    .expect_err("empty param_types must still synthesize receiver transfer metadata");
    assert!(payload.contains("object kind"), "{payload}");

    let args = TypedSlotPayload::try_new(
        vec![wrong_receiver as u64, 7],
        vec![SlotType::GcRef, SlotType::Value],
    )
    .expect("typed payload");
    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(closure_ref, args);
    match result {
        ExecResult::JitError(msg) => assert!(msg.contains("object kind"), "{msg}"),
        other => panic!("invalid receiver replay slot should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn vm_extern_replay_transfer_contract_061_rejects_empty_param_types_with_gcref_suffix() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function_with_recv(2, 2, 1, false);
    callee.slot_types = vec![SlotType::GcRef, SlotType::GcRef];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    let module = module_with_named_string_receiver_callee(callee);
    let closure_ref = closure::create(&mut gc, 0, 0);
    let receiver = string::new_from_string(&mut gc, "receiver".to_string());
    let suffix = string::new_from_string(&mut gc, "suffix".to_string());

    let err = typed_extern_replay_args(
        &gc,
        &module,
        &ItabCache::new(),
        closure_ref,
        vec![receiver as u64, suffix as u64],
    )
    .expect_err("empty param_types must not allow a GC-visible suffix after receiver metadata");
    assert!(err.contains("missing param_types"), "{err}");

    let args = TypedSlotPayload::try_new(
        vec![receiver as u64, suffix as u64],
        vec![SlotType::GcRef, SlotType::GcRef],
    )
    .expect("typed payload");
    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(closure_ref, args);

    match result {
        ExecResult::JitError(msg) => assert!(msg.contains("missing param_types"), "{msg}"),
        other => {
            panic!("GC-visible suffix without transfer metadata should fail, got {other:?}")
        }
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn vm_extern_replay_transfer_contract_061_validates_receiver_in_inclusive_metadata() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function_with_recv(2, 2, 1, false);
    callee.slot_types = vec![SlotType::GcRef, SlotType::GcRef];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.param_types = vec![string_transfer(), string_transfer()];
    let mut module = module_with_callee(callee);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let closure_ref = closure::create(&mut gc, 0, 0);
    let name = string::new_from_string(&mut gc, "field".to_string());

    let payload = typed_extern_replay_args(
        &gc,
        &module,
        &ItabCache::new(),
        closure_ref,
        vec![0xdead_beef, name as u64],
    )
    .expect_err("receiver-inclusive param_types must validate the receiver slot");
    assert!(payload.contains("invalid GcRef"), "{payload}");

    let args = TypedSlotPayload::try_new(
        vec![0xdead_beef, name as u64],
        vec![SlotType::GcRef, SlotType::GcRef],
    )
    .expect("typed payload");
    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(closure_ref, args);
    match result {
        ExecResult::JitError(msg) => assert!(msg.contains("invalid GcRef"), "{msg}"),
        other => panic!("invalid receiver replay slot should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn vm_closure_call_signature_002_extern_replay_closure_rejects_payload_slot_metadata_drift() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let module = module_with_callee(test_function(1, 1, false));
    let closure_ref = closure::create(&mut gc, 0, 0);
    let args = TypedSlotPayload::try_new(vec![0], vec![SlotType::GcRef]).expect("typed payload");

    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(closure_ref, args);

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("arg slot metadata mismatch"), "{msg}");
        }
        other => panic!("metadata drift should be fatal infra, got {other:?}"),
    }
}

#[test]
fn vm_extern_replay_closure_rejects_invalid_gcref_arg_before_frame_push_059() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function(1, 1, false);
    callee.slot_types = vec![SlotType::GcRef];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    let module = module_with_callee(callee);
    let closure_ref = closure::create(&mut gc, 0, 0);
    let args =
        TypedSlotPayload::try_new(vec![0xdead_beef], vec![SlotType::GcRef]).expect("typed payload");

    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(closure_ref, args);

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("invalid GcRef"), "{msg}");
        }
        other => panic!("invalid replay GcRef arg should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn vm_extern_replay_transfer_contract_061_rejects_missing_param_types_before_frame_push() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function(1, 1, false);
    callee.slot_types = vec![SlotType::GcRef];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    let module = module_with_callee(callee);
    let closure_ref = closure::create(&mut gc, 0, 0);
    let arg = string::new_from_string(&mut gc, "payload".to_string());

    let payload = typed_extern_replay_args(
        &gc,
        &module,
        &ItabCache::new(),
        closure_ref,
        vec![arg as u64],
    )
    .expect_err("GC-visible extern replay args require transfer metadata");

    assert!(payload.contains("missing param_types"), "{payload}");

    let args =
        TypedSlotPayload::try_new(vec![arg as u64], vec![SlotType::GcRef]).expect("typed payload");
    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(closure_ref, args);
    match result {
        ExecResult::JitError(msg) => {
            assert!(
                msg.contains("missing param_types") || msg.contains("requires transfer metadata"),
                "{msg}"
            );
        }
        other => panic!("missing transfer metadata should reject replay, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn vm_extern_replay_transfer_contract_061_rejects_wrong_object_kind_before_frame_push() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function(1, 1, false);
    callee.slot_types = vec![SlotType::GcRef];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.param_types = vec![string_transfer()];
    let mut module = module_with_callee(callee);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let closure_ref = closure::create(&mut gc, 0, 0);
    let wrong_kind = slice::create(&mut gc, ValueMeta::new(0, ValueKind::Int64), 8, 0, 0);

    let err = typed_extern_replay_args(
        &gc,
        &module,
        &ItabCache::new(),
        closure_ref,
        vec![wrong_kind as u64],
    )
    .expect_err("extern replay must reject wrong object kind before frame publication");

    assert!(err.contains("object kind"), "{err}");

    let args = TypedSlotPayload::try_new(vec![wrong_kind as u64], vec![SlotType::GcRef])
        .expect("typed payload");
    let result = FrameCallBuilder::new(&mut gc, &mut fiber, &module)
        .call_extern_replay_closure(closure_ref, args);
    match result {
        ExecResult::JitError(msg) => assert!(msg.contains("object kind"), "{msg}"),
        other => panic!("wrong object kind should reject replay, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn vm_extern_replay_transfer_contract_061_rejects_interface_slot1_heap_kind_before_frame_push() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function(2, 2, false);
    callee.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.param_types = vec![interface_transfer(0, 0)];
    let (module, itab_cache) = extern_replay_single_method_interface_module(callee);
    let closure_ref = closure::create(&mut gc, 0, 0);
    let slot0 = vo_runtime::objects::interface::pack_slot0(1, 3, ValueKind::String);
    let wrong_kind = slice::create(&mut gc, ValueMeta::new(0, ValueKind::Int64), 8, 0, 0);

    let err = typed_extern_replay_args(
        &gc,
        &module,
        &itab_cache,
        closure_ref,
        vec![slot0, wrong_kind as u64],
    )
    .expect_err("extern replay must reject forged interface data object kind");

    assert!(err.contains("interface arg data object kind"), "{err}");

    let args = TypedSlotPayload::try_new(
        vec![slot0, wrong_kind as u64],
        vec![SlotType::Interface0, SlotType::Interface1],
    )
    .expect("typed payload");
    let result = FrameCallBuilder::new_with_itab_cache(&mut gc, &mut fiber, &module, &itab_cache)
        .call_extern_replay_closure(closure_ref, args);
    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("interface arg data object kind"), "{msg}")
        }
        other => panic!("forged interface data kind should reject replay, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn vm_extern_replay_transfer_contract_061_accepts_empty_interface_arg_with_concrete_itab() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function(2, 2, false);
    callee.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.param_types = vec![interface_transfer(0, 0)];
    let mut module = extern_replay_empty_interface_module(callee);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let itab_cache = ItabCache::from_module_itabs(vec![Itab::default(), Itab::default()]);
    let closure_ref = closure::create(&mut gc, 0, 0);
    let data = string::new_from_string(&mut gc, "value".to_string());
    let slot0 = vo_runtime::objects::interface::pack_slot0(1, 1, ValueKind::String);

    let payload = typed_extern_replay_args(
        &gc,
        &module,
        &itab_cache,
        closure_ref,
        vec![slot0, data as u64],
    )
    .expect("empty-interface replay args may preserve concrete itab identity");

    let result = FrameCallBuilder::new_with_itab_cache(&mut gc, &mut fiber, &module, &itab_cache)
        .call_extern_replay_closure(closure_ref, payload);

    assert!(matches!(result, ExecResult::FrameChanged));
}

#[test]
fn vm_extern_replay_transfer_contract_061_rejects_null_struct_interface_data() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function(2, 2, false);
    callee.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.param_types = vec![interface_transfer(0, 0)];
    let mut module = extern_replay_empty_interface_module(callee);
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });
    let itab_cache = ItabCache::new();
    let closure_ref = closure::create(&mut gc, 0, 0);
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 1, ValueKind::Struct);

    let err = typed_extern_replay_args(&gc, &module, &itab_cache, closure_ref, vec![slot0, 0])
        .expect_err("extern replay must reject null struct data boxes");

    assert!(err.contains("data missing object"), "{err}");

    let args = TypedSlotPayload::try_new(
        vec![slot0, 0],
        vec![SlotType::Interface0, SlotType::Interface1],
    )
    .expect("typed payload");
    let result = FrameCallBuilder::new_with_itab_cache(&mut gc, &mut fiber, &module, &itab_cache)
        .call_extern_replay_closure(closure_ref, args);
    match result {
        ExecResult::JitError(msg) => assert!(msg.contains("data missing object"), "{msg}"),
        other => panic!("null struct data should reject replay, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn vm_extern_replay_transfer_contract_061_rejects_null_array_interface_data() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function(2, 2, false);
    callee.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.param_types = vec![interface_transfer(0, 0)];
    let mut module = extern_replay_empty_interface_module(callee);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Array {
        len: 2,
        elem: ValueRttid::new(1, ValueKind::String),
    });
    let itab_cache = ItabCache::new();
    let closure_ref = closure::create(&mut gc, 0, 0);
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 2, ValueKind::Array);

    let err = typed_extern_replay_args(&gc, &module, &itab_cache, closure_ref, vec![slot0, 0])
        .expect_err("extern replay must reject null array data boxes");

    assert!(err.contains("data missing object"), "{err}");

    let args = TypedSlotPayload::try_new(
        vec![slot0, 0],
        vec![SlotType::Interface0, SlotType::Interface1],
    )
    .expect("typed payload");
    let result = FrameCallBuilder::new_with_itab_cache(&mut gc, &mut fiber, &module, &itab_cache)
        .call_extern_replay_closure(closure_ref, args);
    match result {
        ExecResult::JitError(msg) => assert!(msg.contains("data missing object"), "{msg}"),
        other => panic!("null array data should reject replay, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn vm_extern_replay_transfer_contract_061_accepts_interface_array_value_slot_box() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function(2, 2, false);
    callee.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.param_types = vec![interface_transfer(0, 0)];
    let mut module = extern_replay_empty_interface_module(callee);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Array {
        len: 2,
        elem: ValueRttid::new(1, ValueKind::String),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::GcRef],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    let itab_cache = ItabCache::new();
    let closure_ref = closure::create(&mut gc, 0, 0);
    let first = string::new_from_string(&mut gc, String::from("a"));
    let second = string::new_from_string(&mut gc, String::from("b"));
    let array_box = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 2);
    unsafe {
        Gc::write_slot(array_box, 0, first as u64);
        Gc::write_slot(array_box, 1, second as u64);
    }
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 2, ValueKind::Array);

    let payload = typed_extern_replay_args(
        &gc,
        &module,
        &itab_cache,
        closure_ref,
        vec![slot0, array_box as u64],
    )
    .expect("extern replay must accept canonical value-slot boxes for array interface data");

    assert_eq!(payload.values[0], slot0);
    assert_eq!(payload.values[1], array_box as u64);
    let result = FrameCallBuilder::new_with_itab_cache(&mut gc, &mut fiber, &module, &itab_cache)
        .call_extern_replay_closure(closure_ref, payload);
    match result {
        ExecResult::FrameChanged => {}
        other => panic!("array value-slot interface data should replay, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 2);
}

#[test]
fn vm_extern_replay_transfer_contract_061_rejects_interface_array_value_slot_box_slot_count_drift()
{
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function(2, 2, false);
    callee.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.param_types = vec![interface_transfer(0, 0)];
    let mut module = extern_replay_empty_interface_module(callee);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Array {
        len: 2,
        elem: ValueRttid::new(1, ValueKind::String),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::GcRef],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    let itab_cache = ItabCache::new();
    let closure_ref = closure::create(&mut gc, 0, 0);
    let undersized_array_box = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 2, ValueKind::Array);

    let err = typed_extern_replay_args(
        &gc,
        &module,
        &itab_cache,
        closure_ref,
        vec![slot0, undersized_array_box as u64],
    )
    .expect_err("extern replay must reject undersized array value-slot boxes");

    assert!(err.contains("interface arg data allocation slots"), "{err}");

    let args = TypedSlotPayload::try_new(
        vec![slot0, undersized_array_box as u64],
        vec![SlotType::Interface0, SlotType::Interface1],
    )
    .expect("typed payload");
    let result = FrameCallBuilder::new_with_itab_cache(&mut gc, &mut fiber, &module, &itab_cache)
        .call_extern_replay_closure(closure_ref, args);
    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("interface arg data allocation slots"), "{msg}")
        }
        other => panic!("array value-slot box slot drift should reject replay, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn vm_extern_replay_transfer_contract_061_rejects_interface_struct_data_slot_count_drift() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut callee = test_function(2, 2, false);
    callee.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.param_types = vec![interface_transfer(0, 0)];
    let mut module = extern_replay_empty_interface_module(callee);
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::GcRef],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });
    let itab_cache = ItabCache::new();
    let closure_ref = closure::create(&mut gc, 0, 0);
    let undersized = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 1, ValueKind::Struct);

    let err = typed_extern_replay_args(
        &gc,
        &module,
        &itab_cache,
        closure_ref,
        vec![slot0, undersized as u64],
    )
    .expect_err("extern replay must reject struct data allocation width drift");

    assert!(err.contains("interface arg data allocation slots"), "{err}");

    let args = TypedSlotPayload::try_new(
        vec![slot0, undersized as u64],
        vec![SlotType::Interface0, SlotType::Interface1],
    )
    .expect("typed payload");
    let result = FrameCallBuilder::new_with_itab_cache(&mut gc, &mut fiber, &module, &itab_cache)
        .call_extern_replay_closure(closure_ref, args);
    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("interface arg data allocation slots"), "{msg}")
        }
        other => panic!("struct data slot drift should reject replay, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
    assert_eq!(fiber.closure_replay.depth, 0);
}

#[test]
fn typed_extern_replay_args_derives_payload_metadata_from_closure_target() {
    let mut gc = Gc::new();
    let module = module_with_callee(test_function(1, 1, false));
    let closure_ref = closure::create(&mut gc, 0, 0);
    let itab_cache = ItabCache::new();

    let payload = typed_extern_replay_args(&gc, &module, &itab_cache, closure_ref, vec![123])
        .expect("typed args");

    assert_eq!(payload.values, vec![123]);
    assert_eq!(payload.slot_types, vec![SlotType::Value]);
}

#[test]
fn typed_extern_replay_args_rejects_invalid_gcref_arg_059() {
    let mut gc = Gc::new();
    let mut callee = test_function(1, 1, false);
    callee.slot_types = vec![SlotType::GcRef];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    let module = module_with_callee(callee);
    let closure_ref = closure::create(&mut gc, 0, 0);
    let itab_cache = ItabCache::new();

    let err = typed_extern_replay_args(&gc, &module, &itab_cache, closure_ref, vec![0xdead_beef])
        .expect_err("invalid GcRef arg should fail");

    assert!(err.contains("invalid GcRef"), "{err}");
}

#[test]
fn typed_extern_replay_args_rejects_invalid_interface_gcref_arg_059() {
    let mut gc = Gc::new();
    let mut callee = test_function(2, 2, false);
    callee.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    let module = module_with_callee(callee);
    let closure_ref = closure::create(&mut gc, 0, 0);
    let iface = vo_runtime::InterfaceSlot::from_ref(0xdead_beef as GcRef, 0, ValueKind::String);
    let itab_cache = ItabCache::new();

    let err = typed_extern_replay_args(
        &gc,
        &module,
        &itab_cache,
        closure_ref,
        vec![iface.slot0, iface.slot1],
    )
    .expect_err("invalid interface GcRef arg should fail");

    assert!(err.contains("invalid interface GcRef"), "{err}");
}

#[test]
fn typed_extern_replay_args_rejects_wrong_interface_arg_metadata_060() {
    let mut gc = Gc::new();
    let mut callee = test_function(2, 2, false);
    callee.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Interface).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Interface).to_raw(),
        slots: 2,
    }];
    let mut module = module_with_callee(callee);
    module.interface_metas.push(InterfaceMeta {
        name: "A".to_string(),
        method_names: vec!["M".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "M".to_string(),
            signature_rttid: 2,
        }],
    });
    module.interface_metas.push(InterfaceMeta {
        name: "B".to_string(),
        method_names: vec!["M".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "M".to_string(),
            signature_rttid: 2,
        }],
    });
    module.runtime_types.push(RuntimeType::Interface {
        methods: vec![InterfaceMethod {
            name: "M".to_string(),
            sig: ValueRttid::new(2, ValueKind::Closure),
        }],
        meta_id: 0,
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 7,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 2,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(1, ValueKind::Int64),
        methods,
    });
    let itab_cache = ItabCache::from_module_itabs(vec![
        Itab::default(),
        Itab {
            iface_meta_id: 1,
            methods: vec![7],
        },
    ]);
    let closure_ref = closure::create(&mut gc, 0, 0);
    let slot0 = vo_runtime::objects::interface::pack_slot0(1, 3, ValueKind::Int64);

    let err = typed_extern_replay_args(&gc, &module, &itab_cache, closure_ref, vec![slot0, 123])
        .expect_err("wrong interface itab must fail before closure frame replay");

    assert!(err.contains("does not match expected interface"), "{err}");
}
