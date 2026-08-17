use super::*;
use std::collections::BTreeMap;
use vo_runtime::bytecode::{
    FunctionDef, InterfaceMeta, InterfaceMethodMeta, Itab, MethodInfo, NamedTypeMeta,
};
use vo_runtime::instruction::Opcode;
use vo_runtime::itab::ItabCache;
use vo_runtime::{RuntimeType, SlotType, ValueKind, ValueMeta, ValueRttid};

fn function(local_slots: u16) -> FunctionDef {
    FunctionDef {
        name: "f".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots,
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: true,
        has_call_extern: false,
        code: Vec::new(),
        instruction_metadata: Vec::new(),
        slot_types: vec![SlotType::Value; local_slots as usize],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    }
}

fn iface_target_function(
    param_slots: u16,
    recv_slots: u16,
    ret_slots: u16,
    local_slots: u16,
) -> FunctionDef {
    let mut func = function(local_slots);
    func.name = "iface_target".to_string();
    func.param_count = param_slots;
    func.param_slots = param_slots;
    func.recv_slots = recv_slots;
    func.ret_slots = ret_slots;
    func.ret_slot_types = vec![SlotType::Value; ret_slots as usize];
    func
}

fn add_named_receiver_method(
    module: &mut Module,
    name: &str,
    kind: ValueKind,
    func_id: u32,
    signature_rttid: u32,
) -> u32 {
    let underlying_rttid = module.runtime_types.len() as u32;
    module.runtime_types.push(RuntimeType::Basic(kind));
    let named_id = module.named_type_metas.len() as u32;
    let named_rttid = module.runtime_types.len() as u32;
    module.runtime_types.push(RuntimeType::Named {
        id: named_id,
        struct_meta_id: None,
    });
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: name.to_string(),
        underlying_meta: ValueMeta::new(0, kind),
        underlying_rttid: ValueRttid::new(underlying_rttid, kind),
        methods,
    });
    named_rttid
}

#[test]
fn jit_iface_prepared_ic_admission_is_limited_to_direct_pointer_receivers() {
    let mut module = Module::new("jit-iface-pointer-ic-admission".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    module
        .runtime_types
        .push(RuntimeType::Pointer(ValueRttid::new(1, ValueKind::Int64)));
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 7,
            is_pointer_receiver: true,
            receiver_is_iface_boxed: false,
            signature_rttid: 0,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods,
    });

    let pointer_slot0 = interface::pack_slot0(0, 2, ValueKind::Pointer);
    assert!(iface_call_target_is_direct_pointer_receiver(
        &module,
        pointer_slot0,
        7
    ));

    module.named_type_metas[0]
        .methods
        .get_mut("M")
        .unwrap()
        .receiver_is_iface_boxed = true;
    assert!(!iface_call_target_is_direct_pointer_receiver(
        &module,
        pointer_slot0,
        7
    ));
}

#[test]
fn call_iface_missing_itab_is_jit_error_instead_of_raw_panic() {
    let mut module = Module::new("missing-itab-test".to_string());
    let mut caller = function(4);
    caller.instruction_metadata =
        vec![vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }];
    module.functions.push(caller);
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 4, 0, 0);
    fiber.current_frame_mut().unwrap().pc = 1;
    fiber.stack[bp] = (99u64 << 32) | 1;
    fiber.stack[bp + 1] = 0;
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 1, 0);
    let cache = ItabCache::default();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache)
    }));

    match result {
        Ok(ExecResult::JitError(msg)) => {
            assert!(msg.contains("missing itab_id=99"), "{msg}");
        }
        Ok(other) => panic!("missing itab should be a JitError, got {other:?}"),
        Err(_) => panic!("missing itab must not panic in CallIface"),
    }
}

#[test]
fn call_closure_missing_function_is_jit_error_instead_of_index_panic() {
    let mut module = Module::new("missing-closure-target-test".to_string());
    module.functions.push(function(4));
    let mut gc = Gc::new();
    let closure_ref = closure::create(&mut gc, 7, 0);
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 4, 0, 0);
    fiber.stack[bp] = closure_ref as u64;
    let inst = Instruction::new(Opcode::CallClosure, 0, 0, 0);

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        exec_call_closure(&mut gc, &mut fiber, &inst, &module)
    }));

    match result {
        Ok(ExecResult::JitError(msg)) => {
            assert!(msg.contains("CallClosure missing function id 7"), "{msg}");
        }
        Ok(other) => panic!("missing closure target should be a JitError, got {other:?}"),
        Err(_) => panic!("missing closure target must not panic in CallClosure"),
    }
}

#[test]
fn call_closure_without_active_frame_rejects_before_stack_read_056() {
    let mut module = Module::new("call-closure-no-frame-test".to_string());
    module.functions.push(function(1));
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    fiber.stack.push(0);
    fiber.sp = 1;
    let inst = Instruction::new(Opcode::CallClosure, 0, 0, 0);

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        exec_call_closure(&mut gc, &mut fiber, &inst, &module)
    }));

    match result {
        Ok(ExecResult::JitError(msg)) => {
            assert!(
                msg.contains("CallClosure requested without an active caller frame"),
                "{msg}"
            );
        }
        Ok(other) => panic!("missing caller frame should be a JitError, got {other:?}"),
        Err(_) => panic!("missing caller frame must not panic in CallClosure"),
    }
}

#[test]
fn verified_call_closure_publishes_and_reuses_shared_target_proof() {
    let mut module = Module::new("cached-closure-target-test".to_string());
    let mut caller = function(1);
    caller.instruction_metadata = vec![vo_runtime::bytecode::InstructionMetadata::CallLayout {
        arg_layout: Vec::new(),
        ret_layout: Vec::new(),
    }];
    module.functions.push(caller);
    module.functions.push(function(1));

    let mut gc = Gc::new();
    let closure_ref = closure::create(&mut gc, 1, 0);
    let inst = Instruction::new(Opcode::CallClosure, 0, 0, 0);
    let mut entry = vo_runtime::DynCallIC::default();

    for _ in 0..2 {
        let mut fiber = Fiber::new(0);
        let bp = fiber.push_frame(0, 1, 0, 0);
        fiber.stack[bp] = closure_ref as u64;
        fiber.current_frame_mut().unwrap().pc = 1;
        assert!(matches!(
            exec_call_closure_cached(&mut gc, &mut fiber, &inst, &module, &mut entry),
            ExecResult::FrameChanged
        ));
    }

    assert_eq!(
        entry.probe(1),
        Some(vo_runtime::DynamicCallTarget {
            func_id: 1,
            local_slots: 1,
        })
    );
}

#[test]
fn verified_call_iface_consumes_cached_target_and_initializes_exact_roots() {
    let mut module = Module::new("cached-iface-target-test".to_string());
    let mut caller = function(5);
    caller.slot_types = vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ];
    module.functions.push(caller);

    let mut target = iface_target_function(1, 1, 0, 3);
    target.slot_types = vec![SlotType::Value, SlotType::GcRef, SlotType::Value];
    module.functions.push(target);

    let loaded_module = crate::vm::test_loaded_module(module);
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 5, 0, 0);
    let slot0 = interface::pack_slot0(1, 7, ValueKind::Pointer);
    fiber.stack[bp] = slot0;
    fiber.stack[bp + 1] = 0x1234;
    fiber.stack[bp + 2] = u64::MAX;
    fiber.current_frame_mut().unwrap().pc = 1;

    let mut entry = vo_runtime::DynCallIC::default();
    entry.publish_interpreter_target(
        slot0,
        vo_runtime::DynamicCallTarget {
            func_id: 1,
            local_slots: 3,
        },
    );
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 2, 0);

    assert!(matches!(
        exec_verified_call_iface_cached(
            &mut gc,
            &mut fiber,
            &inst,
            &loaded_module,
            &ItabCache::default(),
            &mut entry,
        ),
        ExecResult::FrameChanged
    ));
    let callee = fiber.current_frame().expect("cached target pushed a frame");
    assert_eq!(callee.func_id, 1);
    assert_eq!(callee.bp, bp + 1);
    assert_eq!(fiber.stack[callee.bp], 0x1234);
    assert_eq!(fiber.stack[callee.bp + 1], 0);
}

#[test]
fn cached_closure_proof_never_authorizes_an_unrelated_gc_object() {
    let mut module = Module::new("cached-closure-object-kind-test".to_string());
    let mut caller = function(1);
    caller.instruction_metadata = vec![vo_runtime::bytecode::InstructionMetadata::CallLayout {
        arg_layout: Vec::new(),
        ret_layout: Vec::new(),
    }];
    module.functions.push(caller);
    module.functions.push(function(1));

    let mut gc = Gc::new();
    let unrelated = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    assert!(!unrelated.is_null());
    unsafe { *unrelated = 1 };
    let mut entry = vo_runtime::DynCallIC::default();
    entry.publish_interpreter_target(
        1,
        vo_runtime::DynamicCallTarget {
            func_id: 1,
            local_slots: 1,
        },
    );

    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 1, 0, 0);
    fiber.stack[bp] = unrelated as u64;
    fiber.current_frame_mut().unwrap().pc = 1;
    let inst = Instruction::new(Opcode::CallClosure, 0, 0, 0);
    let result = exec_call_closure_cached(&mut gc, &mut fiber, &inst, &module, &mut entry);

    match result {
        ExecResult::JitError(message) => {
            assert!(message.contains("object kind Struct"), "{message}")
        }
        other => panic!("unrelated managed object reached cached closure target: {other:?}"),
    }
}

#[test]
fn vm_closure_call_signature_002_call_iface_rejects_arg_slot_shape_drift_before_frame_push() {
    let mut module = Module::new("call-iface-arg-shape-test".to_string());
    let mut caller = function(5);
    caller.instruction_metadata =
        vec![vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: vec![SlotType::Value],
            ret_layout: Vec::new(),
        }];
    module.functions.push(caller);
    let mut target = iface_target_function(3, 1, 0, 4);
    target.slot_types[0] = SlotType::GcBase;
    module.functions.push(target);
    let receiver_rttid = add_named_receiver_method(&mut module, "R", ValueKind::String, 1, 0);
    let cache = ItabCache::from_module_itabs(vec![
        Itab::default(),
        Itab {
            iface_meta_id: 0,
            methods: vec![1],
        },
    ]);

    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 5, 0, 0);
    fiber.stack[bp] = interface::pack_slot0(1, receiver_rttid, ValueKind::String);
    fiber.stack[bp + 1] = 123;
    fiber.current_frame_mut().unwrap().pc = 1;
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 2, 0);

    let result = exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache);

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("CallIface arg slot count"), "{msg}");
        }
        other => panic!("arg shape drift should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
}

#[test]
fn vm_closure_call_signature_002_call_iface_rejects_return_slot_shape_drift_before_frame_push() {
    let mut module = Module::new("call-iface-ret-shape-test".to_string());
    let mut caller = function(5);
    caller.instruction_metadata =
        vec![vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: vec![SlotType::Value],
            ret_layout: Vec::new(),
        }];
    module.functions.push(caller);
    module.functions.push(iface_target_function(2, 1, 1, 3));
    let receiver_rttid = add_named_receiver_method(&mut module, "R", ValueKind::Int, 1, 0);
    let cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![1],
    }]);

    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 5, 0, 0);
    fiber.stack[bp] = interface::pack_slot0(0, receiver_rttid, ValueKind::Int);
    fiber.stack[bp + 1] = 456;
    fiber.current_frame_mut().unwrap().pc = 1;
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 2, 0);

    let result = exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache);

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("CallIface return slot count"), "{msg}");
        }
        other => panic!("return shape drift should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
}

#[test]
fn vm_call_iface_contract_061_rejects_return_offset_overflow_before_ic_mutation() {
    let mut module = Module::new("call-iface-ret-offset-overflow-test".to_string());
    let mut caller = function(u16::MAX);
    caller.slot_types[0] = SlotType::Interface0;
    caller.slot_types[1] = SlotType::Interface1;
    caller.instruction_metadata =
        vec![vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: vec![SlotType::Value],
            ret_layout: Vec::new(),
        }];
    module.functions.push(caller);
    module.functions.push(iface_target_function(2, 1, 0, 2));
    let receiver_rttid = add_named_receiver_method(&mut module, "R", ValueKind::Int, 1, 0);
    let cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![1],
    }]);

    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, u16::MAX, 0, 0);
    fiber.stack[bp] = interface::pack_slot0(0, receiver_rttid, ValueKind::Int);
    fiber.stack[bp + 1] = 456;
    fiber.current_frame_mut().unwrap().pc = 1;
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, u16::MAX, 0);

    let result = exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache);

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("CallIface return offset overflow"), "{msg}");
        }
        other => panic!("return offset overflow should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
}

#[test]
fn vm_call_iface_contract_061_rejects_frame_capacity_before_ic_mutation() {
    let mut module = Module::new("call-iface-frame-capacity-test".to_string());
    let mut caller = function(4);
    caller.slot_types = vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
        SlotType::Value,
    ];
    caller.instruction_metadata =
        vec![vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: vec![SlotType::Value],
            ret_layout: Vec::new(),
        }];
    module.functions.push(caller);
    module.functions.push(iface_target_function(2, 1, 0, 2));
    let receiver_rttid = add_named_receiver_method(&mut module, "R", ValueKind::Int, 1, 0);
    let cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![1],
    }]);

    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    while fiber.try_reserve_call_frames(2).is_ok() {
        fiber
            .try_push_call_frame_extended(0, 0, 0, 0, 0)
            .expect("reserve check should leave room for a filler frame");
    }
    let bp = fiber.push_frame(0, 4, 0, 0);
    fiber.stack[bp] = interface::pack_slot0(0, receiver_rttid, ValueKind::Int);
    fiber.stack[bp + 1] = 456;
    fiber.stack[bp + 2] = 789;
    fiber.current_frame_mut().unwrap().pc = 1;
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 2, 0);

    let result = exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache);

    assert!(
        matches!(result, ExecResult::Panic),
        "frame capacity failure should become a runtime stack-overflow panic, got {result:?}"
    );
}

#[test]
fn vm_closure_call_signature_002_call_iface_rejects_arg_slot_metadata_drift_before_frame_push() {
    let mut module = Module::new("call-iface-arg-metadata-test".to_string());
    let mut caller = function(4);
    caller.slot_types = vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
        SlotType::Value,
    ];
    caller.instruction_metadata =
        vec![vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: vec![SlotType::Value],
            ret_layout: Vec::new(),
        }];
    module.functions.push(caller);

    let mut target = iface_target_function(2, 1, 0, 2);
    target.slot_types = vec![SlotType::GcRef, SlotType::GcRef];
    target.slot_types[0] = SlotType::Value;
    module.functions.push(target);
    let receiver_rttid = add_named_receiver_method(&mut module, "R", ValueKind::Int, 1, 0);
    let cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![1],
    }]);

    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 4, 0, 0);
    fiber.stack[bp] = interface::pack_slot0(0, receiver_rttid, ValueKind::Int);
    fiber.stack[bp + 1] = 456;
    fiber.stack[bp + 2] = 789;
    fiber.current_frame_mut().unwrap().pc = 1;
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 2, 0);

    let result = exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache);

    match result {
        ExecResult::JitError(msg) => {
            assert!(
                msg.contains("CallIface arg slot metadata mismatch"),
                "{msg}"
            );
        }
        other => panic!("metadata drift should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
}

#[test]
fn vm_call_iface_rejects_raw_interface_receiver_layout_drift_before_frame_push_060() {
    let mut module = Module::new("call-iface-raw-interface-receiver-layout-test".to_string());
    let mut caller = function(2);
    caller.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    caller.instruction_metadata =
        vec![vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }];
    module.functions.push(caller);

    let mut target = iface_target_function(1, 1, 0, 1);
    target.slot_types = vec![SlotType::GcRef];
    module.functions.push(target);
    let cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![1],
    }]);

    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 2, 0, 0);
    fiber.stack[bp] = interface::pack_slot0(0, ValueKind::Int as u32, ValueKind::Int);
    fiber.stack[bp + 1] = 0xdead_beef;
    fiber.current_frame_mut().unwrap().pc = 1;
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 1, 0);

    let result = exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache);

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("receiver layout"), "{msg}");
        }
        other => {
            panic!("raw interface receiver layout drift should be rejected, got {other:?}")
        }
    }
    assert_eq!(fiber.frames.len(), 1);
}

#[test]
fn vm_call_iface_rejects_raw_interface_kind_receiver_before_frame_push_060() {
    let mut module = Module::new("call-iface-raw-interface-kind-receiver-test".to_string());
    let mut caller = function(2);
    caller.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    caller.instruction_metadata =
        vec![vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }];
    module.functions.push(caller);

    let mut target = iface_target_function(1, 1, 0, 1);
    target.slot_types = vec![SlotType::GcRef];
    module.functions.push(target);
    let cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![1],
    }]);

    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 2, 0, 0);
    fiber.stack[bp] = interface::pack_slot0(0, 16, ValueKind::Interface);
    fiber.stack[bp + 1] = 0xdead_beef;
    fiber.current_frame_mut().unwrap().pc = 1;
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 1, 0);

    let result = exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache);

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("receiver layout"), "{msg}");
        }
        other => panic!("raw interface-kind receiver should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
}

#[test]
fn vm_call_iface_rejects_itab_target_not_owned_by_receiver_rttid_060() {
    let mut module = Module::new("call-iface-itab-target-owner-test".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 1,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "A".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods: BTreeMap::new(),
    });
    let mut b_methods = BTreeMap::new();
    b_methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 3,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "B".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods: b_methods,
    });

    let mut caller = function(2);
    caller.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    caller.instruction_metadata =
        vec![vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }];
    module.functions.push(caller);

    let mut target = iface_target_function(1, 1, 0, 1);
    target.name = "B.M".to_string();
    target.slot_types = vec![SlotType::Value];
    module.functions.push(target);
    let cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![1],
    }]);

    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 2, 0, 0);
    fiber.stack[bp] = interface::pack_slot0(0, 1, ValueKind::Int64);
    fiber.stack[bp + 1] = 123;
    fiber.current_frame_mut().unwrap().pc = 1;
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 1, 0);

    let result = exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache);

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("not a method"), "{msg}");
        }
        other => panic!("foreign itab target should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
}

#[test]
fn vm_call_iface_rejects_pointer_receiver_target_for_non_pointer_reference_receiver_before_frame_push_060(
) {
    let mut module = Module::new("call-iface-pointer-receiver-owner-test".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: true,
            receiver_is_iface_boxed: false,
            signature_rttid: 2,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "R".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::String),
        underlying_rttid: ValueRttid::new(0, ValueKind::String),
        methods,
    });

    let mut caller = function(2);
    caller.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    caller.instruction_metadata =
        vec![vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }];
    module.functions.push(caller);

    let mut target = iface_target_function(1, 1, 0, 1);
    target.name = "(*R).M".to_string();
    target.slot_types = vec![SlotType::GcRef];
    module.functions.push(target);
    let cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![1],
    }]);

    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 2, 0, 0);
    fiber.stack[bp] = interface::pack_slot0(0, 1, ValueKind::String);
    fiber.stack[bp + 1] = 0x1234;
    fiber.current_frame_mut().unwrap().pc = 1;
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 1, 0);

    let result = exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache);

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("pointer receiver"), "{msg}");
        }
        other => panic!("pointer-receiver itab target should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
}

#[test]
fn vm_call_iface_rejects_noncanonical_pointer_kind_rttid_before_frame_push_060() {
    let mut module = Module::new("call-iface-noncanonical-pointer-kind-test".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: true,
            receiver_is_iface_boxed: false,
            signature_rttid: 2,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "R".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::String),
        underlying_rttid: ValueRttid::new(0, ValueKind::String),
        methods,
    });

    let mut caller = function(2);
    caller.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    caller.instruction_metadata =
        vec![vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }];
    module.functions.push(caller);

    let mut target = iface_target_function(1, 1, 0, 1);
    target.name = "(*R).M".to_string();
    target.slot_types = vec![SlotType::GcRef];
    module.functions.push(target);
    let cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![1],
    }]);

    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 2, 0, 0);
    fiber.stack[bp] = interface::pack_slot0(0, 1, ValueKind::Pointer);
    fiber.stack[bp + 1] = 0x1234;
    fiber.current_frame_mut().unwrap().pc = 1;
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 1, 0);

    let result = exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache);

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("non-canonical"), "{msg}");
        }
        other => {
            panic!("non-canonical pointer-kind receiver should be rejected, got {other:?}")
        }
    }
    assert_eq!(fiber.frames.len(), 1);
}

#[test]
fn vm_closure_call_signature_002_call_iface_rejects_unadvanced_pc_before_ic_mutation() {
    let mut module = Module::new("call-iface-unadvanced-pc-test".to_string());
    let mut caller = function(2);
    caller.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    caller.instruction_metadata =
        vec![vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }];
    module.functions.push(caller);
    module.functions.push(iface_target_function(1, 1, 0, 1));
    let cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![1],
    }]);

    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 2, 0, 0);
    fiber.stack[bp] = interface::pack_slot0(0, ValueKind::Int as u32, ValueKind::Int);
    fiber.stack[bp + 1] = 456;
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 1, 0);

    let result = exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache);

    match result {
        ExecResult::JitError(msg) => {
            assert!(
                msg.contains("CallIface requested before caller pc advanced"),
                "{msg}"
            );
        }
        other => panic!("unadvanced pc should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
}

#[test]
fn vm_call_iface_contract_061_rejects_foreign_same_receiver_same_shape_itab_before_frame_push() {
    let mut module = Module::new("call-iface-foreign-itab-same-shape-test".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module.interface_metas.push(InterfaceMeta {
        name: "I".to_string(),
        method_names: vec!["M".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "M".to_string(),
            signature_rttid: 2,
        }],
    });
    module.interface_metas.push(InterfaceMeta {
        name: "J".to_string(),
        method_names: vec!["N".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "N".to_string(),
            signature_rttid: 2,
        }],
    });
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 2,
        },
    );
    methods.insert(
        "N".to_string(),
        MethodInfo {
            func_id: 2,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 2,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods,
    });

    let mut caller = function(2);
    caller.slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    caller.instruction_metadata =
        vec![vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }];
    module.functions.push(caller);

    let mut method_m = iface_target_function(1, 1, 0, 1);
    method_m.name = "T.M".to_string();
    method_m.slot_types = vec![SlotType::Value];
    module.functions.push(method_m);
    let mut method_n = iface_target_function(1, 1, 0, 1);
    method_n.name = "T.N".to_string();
    method_n.slot_types = vec![SlotType::Value];
    module.functions.push(method_n);

    let cache = ItabCache::from_module_itabs(vec![
        Itab::default(),
        Itab {
            iface_meta_id: 1,
            methods: vec![2],
        },
    ]);
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let bp = fiber.push_frame(0, 2, 0, 0);
    fiber.stack[bp] = interface::pack_slot0(1, 1, ValueKind::Int64);
    fiber.stack[bp + 1] = 456;
    fiber.current_frame_mut().unwrap().pc = 1;
    let inst = Instruction::with_flags(Opcode::CallIface, 0, 0, 1, 0);

    let result = exec_call_iface(&mut gc, &mut fiber, &inst, &module, &cache);

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("callsite interface"), "{msg}");
        }
        other => panic!("foreign same-shape itab should be rejected, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 1);
}
