use super::*;
use crate::fiber::{
    DeferArgLayout, DeferEntry, Fiber, PanicContext, PanicState, QueueWaitState, ReturnValues,
    SelectState, UnwindingMode, UnwindingState,
};
use crate::test_support::queue as test_queue;
#[cfg(feature = "std")]
use std::sync::atomic::AtomicBool;
#[cfg(feature = "std")]
use std::sync::Arc;
use vo_runtime::bytecode::{
    Constant, ExternDef, FunctionDef, GlobalDef, InstructionMetadata, InterfaceMeta, MethodInfo,
    NamedTypeMeta, ParamShape, ReturnShape, StructMeta,
};
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::island::{EndpointResponseKind, IslandCommand};
use vo_runtime::objects::interface;
use vo_runtime::objects::queue_state::SelectWaitKind;
#[cfg(all(feature = "jit", feature = "std"))]
use vo_runtime::objects::queue_state::{QueueKind, QueueMessage, QueueWaiter};
#[cfg(all(feature = "jit", feature = "std"))]
use vo_runtime::ValueRttid;
use vo_runtime::{RuntimeType, SlotType, ValueKind, ValueMeta};

#[test]
fn outbound_transport_frame_drain_preserves_envelope_and_order() {
    let mut vm = Vm::new();
    vm.state.outbound_commands.push_back((
        7,
        vo_runtime::island::IslandCommandEnvelope::new(3, IslandCommand::Shutdown),
    ));
    vm.state.outbound_commands.push_back((
        11,
        vo_runtime::island::IslandCommandEnvelope::new(5, IslandCommand::Shutdown),
    ));

    let frames = vm
        .try_take_outbound_transport_frames()
        .expect("valid outbound commands must encode");

    assert!(vm.state.outbound_commands.is_empty());
    assert_eq!(frames.len(), 2);
    for (frame, expected_target, expected_source) in [(&frames[0], 7, 3), (&frames[1], 11, 5)] {
        let (target, source, command) =
            vo_runtime::island_msg::decode_island_transport_frame(frame)
                .expect("freshly encoded frame must decode");
        assert_eq!(target, expected_target);
        assert_eq!(source, expected_source);
        assert!(matches!(command, IslandCommand::Shutdown));
    }
}

#[test]
fn integer_float_conversion_helpers_cover_signedness_and_target_widths() {
    use vo_runtime::instruction::{
        CONV_FLAG_FLOAT32, CONV_FLAG_UNSIGNED, CONV_WIDTH_16, CONV_WIDTH_32, CONV_WIDTH_8,
    };

    assert_eq!(
        f64::from_bits(conv_int_bits_to_float_bits(u64::MAX, CONV_FLAG_UNSIGNED)),
        u64::MAX as f64
    );
    assert_eq!(
        f64::from_bits(conv_int_bits_to_float_bits(u64::MAX, 0)),
        -1.0
    );
    let direct_f32_source = 4_611_686_293_305_294_849_i64;
    assert_eq!(
        f32::from_bits(
            conv_int_bits_to_float_bits(direct_f32_source as u64, CONV_FLAG_FLOAT32) as u32
        ),
        direct_f32_source as f32
    );

    for value in [f64::NAN, f64::NEG_INFINITY, -1.0] {
        assert_eq!(
            conv_f64_to_int_bits(value, CONV_FLAG_UNSIGNED),
            value as u64
        );
    }
    assert_eq!(
        conv_f64_to_int_bits(f64::INFINITY, CONV_FLAG_UNSIGNED),
        u64::MAX
    );
    assert_eq!(
        conv_f64_to_int_bits(300.0, CONV_FLAG_UNSIGNED | CONV_WIDTH_8),
        u8::MAX as u64
    );
    assert_eq!(
        conv_f64_to_int_bits(-300.0, CONV_WIDTH_8),
        i8::MIN as i64 as u64
    );
    assert_eq!(
        conv_f64_to_int_bits(70_000.0, CONV_FLAG_UNSIGNED | CONV_WIDTH_16),
        u16::MAX as u64
    );
    assert_eq!(
        conv_f64_to_int_bits(i32::MAX as f64 * 2.0, CONV_WIDTH_32),
        i32::MAX as i64 as u64
    );
    assert_eq!(conv_f64_to_int_bits(-3.9, 0), (-3_i64) as u64);
}

fn extern_def_for_test(
    name: &str,
    params: ParamShape,
    returns: ReturnShape,
    effects: vo_runtime::bytecode::ExternEffects,
) -> ExternDef {
    let name = if vo_common_core::extern_key::classify_extern_name(name).is_ok() {
        name.to_string()
    } else {
        vo_common_core::extern_key::ExternKeyRef::new("github.com/volang/vm-tests", name)
            .encode()
            .expect("VM test extern identity must be canonical")
    };
    ExternDef {
        name,
        params,
        returns,
        allowed_effects: effects,
        param_kinds: Vec::new(),
    }
}

fn gc_test_module() -> Module {
    gc_test_module_with_root_slots(1)
}

fn add_named_string_receiver_metadata(module: &mut Module, func_id: u32) {
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    let mut named_meta = NamedTypeMeta {
        name: "NamedString".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::String),
        underlying_rttid: vo_runtime::ValueRttid::new(0, ValueKind::String),
        methods: Default::default(),
    };
    named_meta.methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 0,
        },
    );
    module.named_type_metas.push(named_meta);
}

fn gc_test_module_with_root_slots(root_slots: u16) -> Module {
    let mut module = Module::new("gc-test".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: Vec::new(),
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: Vec::new(),
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.functions.push(FunctionDef {
        name: "root_frame".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: root_slots,
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        instruction_metadata: vec![vo_runtime::bytecode::InstructionMetadata::None],
        code: vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        slot_types: vec![SlotType::GcRef; root_slots as usize],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    module
}

/// GC tests that place roots in frame slots need bytecode whose current
/// liveness genuinely observes those slots. The unverified test image uses a
/// synthetic wide return solely to publish that exact root state.
fn gc_live_test_module_with_root_slots(root_slots: u16) -> Module {
    let mut module = gc_test_module_with_root_slots(root_slots);
    module.functions[0].code[0] = Instruction::new(Opcode::Return, 0, root_slots, 0);
    module
}

fn gc_live_test_module() -> Module {
    gc_live_test_module_with_root_slots(1)
}

fn gc_live_test_module_with_global() -> Module {
    let mut module = gc_live_test_module();
    module.globals.push(GlobalDef {
        name: "root_global".to_string(),
        slots: 1,
        value_kind: ValueKind::Struct as u8,
        meta_id: 1,
        slot_types: vec![SlotType::GcRef],
    });
    module
}

fn alloc_gc_test_object(vm: &mut Vm) -> GcRef {
    vm.state.gc.alloc(ValueMeta::new(1, ValueKind::Struct), 0)
}

fn assert_gc_roots_survive(vm: &mut Vm, roots: &[GcRef]) {
    run_gc_until_pause(vm);
    for &root in roots {
        assert_eq!(vm.state.gc.canonicalize_ref(root), Some(root));
    }
}

#[cfg(feature = "std")]
fn apply_gc_env_pairs(vm: &mut Vm, pairs: &[(&str, &str)]) {
    vm.apply_gc_environment_from(|name| {
        pairs
            .iter()
            .find_map(|(key, value)| (*key == name).then(|| (*value).to_string()))
    });
}

fn malformed_single_instruction_module(
    name: &str,
    mut code: Vec<Instruction>,
    constants: Vec<Constant>,
) -> Module {
    if code.is_empty() {
        code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    }
    let mut module = Module::new(name.to_string());
    module.constants = constants;
    module.functions.push(FunctionDef {
        name: "main".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 4,
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        instruction_metadata: vec![vo_runtime::bytecode::InstructionMetadata::None; code.len()],
        code,
        slot_types: vec![SlotType::Value; 4],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    module
}

fn assert_vm_load_rejects(module: Module, expected: &[&str]) {
    let mut vm = Vm::new();
    let err = vm
        .load(module)
        .expect_err("invalid module must be rejected during VM load");
    match err {
        VmError::Jit(msg) => {
            assert!(msg.contains("invalid module metadata"), "{msg}");
            for needle in expected {
                assert!(msg.contains(needle), "missing `{needle}` in `{msg}`");
            }
        }
        other => panic!("invalid module load should return Jit error, got {other:?}"),
    }
}

fn refresh_vm_test_function_metadata(func: &mut FunctionDef) {
    func.has_defer = func
        .code
        .iter()
        .any(|inst| matches!(inst.opcode(), Opcode::DeferPush | Opcode::ErrDeferPush));
    let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&func.code);
    func.has_calls = has_calls;
    func.has_call_extern = has_call_extern;
}

fn terminate_vm_test_module(module: &mut Module) {
    for func in &mut module.functions {
        let already_terminated = func.code.last().is_some_and(|inst| {
            matches!(inst.opcode(), Opcode::Jump | Opcode::Return | Opcode::Panic)
        });
        if !already_terminated {
            func.code.push(Instruction::new(Opcode::Return, 0, 0, 0));
            func.instruction_metadata.push(InstructionMetadata::None);
        }
        refresh_vm_test_function_metadata(func);
    }
}

fn finish_load_and_resolve_externs_for_test(
    vm: &mut Vm,
    module: Module,
    registrations: &[(
        u32,
        vo_runtime::ffi::ExternFn,
        vo_runtime::bytecode::ExternEffects,
    )],
) {
    vm.finish_load(module);
    let externs = vm.module.as_ref().expect("loaded module").externs.clone();
    let registry = Arc::make_mut(&mut vm.state.extern_registry);
    for (id, func, effects) in registrations {
        let name = externs
            .get(*id as usize)
            .unwrap_or_else(|| panic!("test extern id {id} missing from loaded module"))
            .name
            .clone();
        registry.register_test_named_with_effects(*id, name, *func, *effects);
    }
    registry
        .resolve_and_freeze(&externs)
        .expect("resolve test externs");
}

fn run_gc_until_pause(vm: &mut Vm) {
    for _ in 0..10_000 {
        if !vm.state.gc.should_step() && vm.state.gc.state() == vo_runtime::gc::GcState::Pause {
            return;
        }
        vm.gc_step_after_fiber(None);
    }
    panic!(
        "GC did not reach pause state; state={:?} root_scan_pending={}",
        vm.state.gc.state(),
        vm.state.gc_root_scan.is_some(),
    );
}

fn run_until_atomic_root_scan_pending(vm: &mut Vm) {
    for _ in 0..10_000 {
        vm.gc_step_after_fiber(None);
        if vm.state.gc.state() == vo_runtime::gc::GcState::Atomic && vm.state.gc_root_scan.is_some()
        {
            return;
        }
    }
    panic!(
        "GC did not enter pending atomic root scan; state={:?} root_scan_pending={}",
        vm.state.gc.state(),
        vm.state.gc_root_scan.is_some(),
    );
}

mod extern_replay;
mod gc_roots;
mod go_island;
#[cfg(feature = "std")]
mod host_services;
mod load_validation;
mod pending_transitions;
mod runtime_wake;
mod scheduler_and_frame;
mod spawn_and_host;
#[cfg(feature = "jit")]
mod strict_jit;
