use super::*;
use crate::test_support::queue;
use crate::vm::Vm;
use core::ffi::c_void;
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;
use vo_common_core::bytecode::{FieldMeta, MethodInfo, NamedTypeMeta, StructMeta};
use vo_common_core::{ChanDir, RuntimeType, StructField};
use vo_runtime::bytecode::{FunctionDef, InstructionMetadata, Module, TransferType};
use vo_runtime::ffi::SentinelErrorCache;
use vo_runtime::itab::ItabCache;
use vo_runtime::jit_api::{
    JitRuntimeTrapKind, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, JIT_INFRA_ERROR_SENTINEL,
};
use vo_runtime::objects::closure;
use vo_runtime::objects::interface::InterfaceSlot;
use vo_runtime::objects::queue_state::QueueKind;
use vo_runtime::output::CaptureSink;
use vo_runtime::{SlotType, ValueKind, ValueMeta, ValueRttid};

fn install_port_string_runtime_types(module: &mut Module) {
    module.runtime_types = vec![
        RuntimeType::Port {
            dir: ChanDir::Send,
            elem: ValueRttid::new(1, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Basic(ValueKind::String),
    ];
}

fn minimal_func() -> FunctionDef {
    FunctionDef {
        name: "callee".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 1,
        gc_scan_slots: 0,
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
        code: Vec::new(),
        instruction_metadata: Vec::new(),
        slot_types: vec![SlotType::Value],
        borrowed_scan_slots_prefix: vec![0, 0],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    }
}

fn install_go_callsite(func: &mut FunctionDef, arg_layout: Vec<SlotType>) {
    func.instruction_metadata = vec![InstructionMetadata::CallLayout {
        arg_layout,
        ret_layout: Vec::new(),
    }];
}

fn attach_go_callsite(ctx: &mut JitContext, fiber: &mut Fiber, func_id: u32) {
    if fiber.current_frame().is_none() {
        fiber.push_frame(func_id, 0, 0, 0, 0);
    }
    ctx.runtime_trap_pc = 0;
}

fn closure_func_with_one_user_arg() -> FunctionDef {
    let mut func = minimal_func();
    func.param_slots = 2;
    func.local_slots = 2;
    func.gc_scan_slots = 1;
    func.is_closure = true;
    func.slot_types = vec![SlotType::GcRef, SlotType::Value];
    func
}

fn closure_func_with_port_capture() -> FunctionDef {
    let mut func = minimal_func();
    func.param_slots = 1;
    func.local_slots = 1;
    func.gc_scan_slots = 1;
    func.is_closure = true;
    func.slot_types = vec![SlotType::GcRef];
    func.capture_slot_types = vec![SlotType::GcRef];
    func.capture_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Port).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Port).to_raw(),
        slots: 1,
    }];
    func
}

fn closure_func_with_port_and_string_captures() -> FunctionDef {
    let mut func = closure_func_with_port_capture();
    func.capture_slot_types = vec![SlotType::GcRef, SlotType::GcRef];
    func.capture_types.push(TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::String).to_raw(),
        rttid_raw: ValueRttid::new(2, ValueKind::String).to_raw(),
        slots: 1,
    });
    func
}

fn direct_method_one_slot_struct_func() -> FunctionDef {
    FunctionDef {
        name: "Receiver.Send".to_string(),
        param_count: 1,
        param_slots: 1,
        local_slots: 1,
        gc_scan_slots: 1,
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: 1,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        code: Vec::new(),
        instruction_metadata: Vec::new(),
        slot_types: vec![SlotType::GcRef],
        borrowed_scan_slots_prefix: vec![0, 1],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    }
}

fn direct_method_one_slot_struct_module() -> Module {
    let mut methods = BTreeMap::new();
    methods.insert(
        "Send".to_string(),
        MethodInfo {
            func_id: 0,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 0,
        },
    );
    let mut module = Module::new("jit-direct-method-one-slot-struct".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: vec![FieldMeta {
            name: "out".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(2, ValueKind::Port),
            embedded: false,
            tag: None,
        }],
        field_index: HashMap::new(),
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "Receiver".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(1, ValueKind::Struct),
        methods,
    });
    module.runtime_types = vec![
        RuntimeType::Named {
            id: 0,
            struct_meta_id: Some(0),
        },
        RuntimeType::Struct {
            fields: vec![StructField {
                name: "out".to_string(),
                typ: ValueRttid::new(2, ValueKind::Port),
                tag: String::new(),
                embedded: false,
                pkg: String::new(),
            }],
            meta_id: 0,
        },
        RuntimeType::Port {
            dir: ChanDir::Send,
            elem: ValueRttid::new(3, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    module.functions.push(direct_method_one_slot_struct_func());
    module
}

#[allow(clippy::too_many_arguments)]
fn test_context<'a>(
    vm: &'a mut Vm,
    module: &'a Module,
    fiber: &'a mut Fiber,
    itab_cache: &'a mut ItabCache,
    panic_flag: &'a mut bool,
    is_user_panic: &'a mut bool,
    panic_msg: &'a mut InterfaceSlot,
    program_args: &'a Vec<Vec<u8>>,
    sentinel_errors: &'a mut SentinelErrorCache,
    output: &'a CaptureSink,
    host_output: &'a mut Option<Vec<u8>>,
) -> JitContext {
    vm.module = Some(crate::vm::test_loaded_module(module.clone()));
    let loaded_module = Arc::as_ptr(vm.module.as_ref().expect("test loaded module"));
    JitContext {
        gc: &mut vm.state.gc,
        globals: core::ptr::null_mut(),
        panic_flag,
        is_user_panic,
        panic_msg,
        user_panic_pc: u32::MAX,
        runtime_trap_kind: JitRuntimeTrapKind::None as u8,
        runtime_trap_arg0: 0,
        runtime_trap_arg1: 0,
        runtime_trap_pc: u32::MAX,
        current_func_id: u32::MAX,
        infra_error_message: core::ptr::null_mut(),
        callback_state: vm as *mut Vm as *mut c_void,
        fiber: fiber as *mut Fiber as *mut c_void,
        itab_cache,
        extern_registry: core::ptr::null(),
        callbacks: &vo_runtime::jit_api::JitContextCallbacks::EMPTY,
        jit_func_table: core::ptr::null(),
        jit_func_count: 0,
        jit_profile_table: core::ptr::null_mut(),
        optimizing_threshold: u64::MAX,
        program_args,
        sentinel_errors,
        output: output as *const dyn vo_runtime::output::OutputSink,
        host_output,
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
        wait_io_token: 0,
        loop_exit_pc: 0,
        stack_ptr: core::ptr::null_mut(),
        stack_cap: 0,
        stack_limit: 0,
        call_depth: 0,
        call_depth_limit: 64,
        jit_bp: 0,
        fiber_sp: 0,
        push_frame_fn: None,
        pop_frame_fn: None,
        stack_overflow_fn: None,
        push_resume_point_fn: None,
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        link_function_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: vo_runtime::EXECUTION_TIMESLICE_INSTRUCTIONS,
        execution_budget_refilled: 0,
        host_services_v2: core::ptr::null(),
        loaded_module,
        native_frame: core::ptr::null_mut(),
        deopt_state_id: u32::MAX,
        deopt_func_id: u32::MAX,
        deopt_resume_pc: u32::MAX,
        deopt_osr_pc: u32::MAX,
        deopt_reason: vo_runtime::jit_api::JitDeoptReason::None as u8,
        deopt_tier: 0,
    }
}

fn assert_invalid_regular_go_start_rejected(func: FunctionDef, arg_slots: u32) {
    let mut module = Module::new("go-start-callback-abi-test".to_string());
    module.functions.push(func);
    let mut vm = Vm::new();
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let args = [11_u64, 22, 33];
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );

    let result = jit_go_start(&mut ctx, 0, 0, 0, args.as_ptr(), arg_slots);

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
    drop(ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(vm.scheduler.fibers.is_empty());
}

fn assert_invalid_callback_state(ctx: &JitContext) {
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
}

#[test]
fn vm_jit_go_callback_abi_rejects_noncanonical_closure_boolean_before_spawn() {
    let module = Module::new("jit-go-boolean-abi".to_string());
    let mut vm = Vm::new();
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );

    let result = jit_go_start(&mut ctx, 0, 2, 0, core::ptr::null(), 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    drop(ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(vm.scheduler.fibers.is_empty());
}

#[test]
fn vm_jit_go_callback_abi_012_nil_closure_rejects_null_non_empty_args_before_trap() {
    let module = Module::new("jit-go-nil-closure-raw-abi".to_string());
    let mut vm = Vm::new();
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );

    let result = jit_go_start(&mut ctx, 0, 1, 0, core::ptr::null(), 1);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::None as u8);
    assert!(!unsafe { *ctx.panic_flag });
    drop(ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(vm.scheduler.fibers.is_empty());
}

#[test]
fn vm_jit_goisland_callback_abi_012_nil_closure_rejects_null_non_empty_args_before_trap() {
    let module = Module::new("jit-goisland-nil-closure-raw-abi".to_string());
    let mut vm = Vm::new();
    let island = vo_runtime::island::create(&mut vm.state.gc, vm.state.current_island_id);
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );

    let result = jit_go_island(&mut ctx, island as u64, 0, core::ptr::null(), 1);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::None as u8);
    assert!(!unsafe { *ctx.panic_flag });
    drop(ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(vm.state.outbound_commands.is_empty());
}

#[test]
fn vm_jit_go_callback_abi_057_nil_closure_requires_active_caller_frame_before_trap() {
    let module = Module::new("jit-go-nil-closure-no-caller-frame".to_string());
    let mut vm = Vm::new();
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    ctx.runtime_trap_pc = 0;

    let result = jit_go_start(&mut ctx, 0, 1, 0, core::ptr::null(), 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::None as u8);
    assert!(!unsafe { *ctx.panic_flag });
    drop(ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(vm.scheduler.fibers.is_empty());
}

#[test]
fn vm_jit_goisland_callback_abi_057_nil_island_requires_active_caller_frame_before_trap() {
    let module = Module::new("jit-goisland-nil-island-no-caller-frame".to_string());
    let mut vm = Vm::new();
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    ctx.runtime_trap_pc = 0;

    let result = jit_go_island(&mut ctx, 0, 0, core::ptr::null(), 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::None as u8);
    assert!(!unsafe { *ctx.panic_flag });
    drop(ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(vm.state.outbound_commands.is_empty());
}

#[cfg(feature = "jit")]
#[test]
fn vm_jit_gostart_pending_001_go_start_spawn_survives_later_terminal_result() {
    let mut module = Module::new("go-start-pending-spawn-test".to_string());
    let mut func = minimal_func();
    install_go_callsite(&mut func, Vec::new());
    func.code = vec![vo_runtime::instruction::Instruction::with_flags(
        vo_runtime::instruction::Opcode::GoStart,
        0,
        0,
        0,
        0,
    )];
    module.functions.push(func);
    let mut vm = Vm::new();
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    attach_go_callsite(&mut ctx, &mut fiber, 0);

    let result = jit_go_start(&mut ctx, 0, 0, 0, core::ptr::null(), 0);

    assert_eq!(result, JitResult::ExternSuspend);
    drop(ctx);
    assert_eq!(
        fiber.jit_extern_suspend.take(),
        Some(JitExternSuspend::Yield { resume_pc: 1 })
    );
    assert_eq!(vm.scheduler.fibers.len(), 0);
    assert!(vm
        .pending_runtime_transitions
        .iter()
        .any(|transition| { !transition.spawns.is_empty() }));

    let result = vm.attach_pending_runtime_transitions(crate::vm::ExecResult::Panic);

    assert!(matches!(result, crate::vm::ExecResult::Panic));
    assert!(vm.pending_runtime_transitions.is_empty());
    assert_eq!(vm.scheduler.fibers.len(), 1);
}

#[cfg(feature = "jit")]
#[test]
fn vm_jit_gostart_pending_001_go_island_command_survives_later_terminal_result() {
    let mut module = Module::new("go-island-pending-command-test".to_string());
    let mut func = minimal_func();
    install_go_callsite(&mut func, Vec::new());
    module.functions.push(func);
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 7;
    let remote_island = vo_runtime::island::create(&mut vm.state.gc, 99);
    let closure_ref = closure::create(&mut vm.state.gc, 0, 0);
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    attach_go_callsite(&mut ctx, &mut fiber, 0);

    let result = jit_go_island(
        &mut ctx,
        remote_island as u64,
        closure_ref as u64,
        core::ptr::null(),
        0,
    );

    assert_eq!(result, JitResult::ExternSuspend);
    drop(ctx);
    assert_eq!(
        fiber.jit_extern_suspend.take(),
        Some(JitExternSuspend::Yield { resume_pc: 1 })
    );
    assert_eq!(vm.state.outbound_commands.len(), 0);
    assert!(vm
        .pending_runtime_transitions
        .iter()
        .any(|transition| { !transition.island_commands.is_empty() }));

    let result = vm.attach_pending_runtime_transitions(crate::vm::ExecResult::Panic);

    assert!(matches!(result, crate::vm::ExecResult::Panic));
    assert!(vm.pending_runtime_transitions.is_empty());
    assert_eq!(vm.state.outbound_commands.len(), 1);
    let (island_id, command) = vm.state.outbound_commands.front().unwrap();
    assert_eq!(*island_id, 99);
    assert_eq!(command.source_island_id, 7);
    assert!(matches!(
        &command.command,
        vo_runtime::island::IslandCommand::SpawnFiber { .. }
    ));
}

#[cfg(feature = "jit")]
#[test]
fn vm_jit_goisland_route_preflight_058_missing_target_route_preserves_no_pending_transition() {
    let mut module = Module::new("go-island-route-preflight-test".to_string());
    let mut func = minimal_func();
    install_go_callsite(&mut func, Vec::new());
    module.functions.push(func);
    let mut vm = Vm::new();
    vm.state.current_island_id = 7;
    let remote_island = vo_runtime::island::create(&mut vm.state.gc, 99);
    let closure_ref = closure::create(&mut vm.state.gc, 0, 0);
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    attach_go_callsite(&mut ctx, &mut fiber, 0);

    let result = jit_go_island(
        &mut ctx,
        remote_island as u64,
        closure_ref as u64,
        core::ptr::null(),
        0,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    drop(ctx);
    assert_eq!(fiber.jit_extern_suspend.take(), None);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(vm.state.outbound_commands.is_empty());
}

#[cfg(feature = "jit")]
#[test]
fn vm_jit_goisland_transfer_txn_006_jit_error_commits_spawn_after_local_endpoint_prepare() {
    let mut module = Module::new("go-island-local-port-capture-terminal-test".to_string());
    let mut func = closure_func_with_port_capture();
    install_go_callsite(&mut func, Vec::new());
    module.functions.push(func);
    install_port_string_runtime_types(&mut module);
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 7;
    let remote_island = vo_runtime::island::create(&mut vm.state.gc, 99);
    let port = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(1, ValueKind::Int64),
        1,
        0,
    );
    let capture_box = vm.state.gc.alloc(
        vo_runtime::island_msg::capture_box_meta(ValueMeta::new(0, ValueKind::Port)),
        1,
    );
    unsafe {
        vo_runtime::gc::Gc::write_slot(capture_box, 0, port as u64);
    }
    let closure_ref = closure::create(&mut vm.state.gc, 0, 1);
    // Safety: test fixture initializes a freshly allocated closure.
    unsafe { closure::set_capture(closure_ref, 0, capture_box as u64) };
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    attach_go_callsite(&mut ctx, &mut fiber, 0);

    let result = jit_go_island(
        &mut ctx,
        remote_island as u64,
        closure_ref as u64,
        core::ptr::null(),
        0,
    );

    assert_eq!(result, JitResult::ExternSuspend);
    drop(ctx);
    assert_eq!(
        fiber.jit_extern_suspend.take(),
        Some(JitExternSuspend::Yield { resume_pc: 1 })
    );
    assert!(vm.state.endpoint_registry.has_live());
    assert_eq!(vm.state.outbound_commands.len(), 0);

    let result = vm.attach_pending_runtime_transitions(crate::vm::ExecResult::JitError(
        "injected terminal JIT error".to_string(),
    ));
    let crate::vm::ExecResult::Transition(transition) = result else {
        panic!("terminal JIT error should carry committed local endpoint GoIsland effects");
    };
    assert!(
        !transition.island_commands.is_empty(),
        "spawn command must survive once queue transfer has committed local endpoint state"
    );
    let _ = vm.apply_runtime_transition(None, transition);

    assert_eq!(vm.state.outbound_commands.len(), 1);
    let (island_id, command) = vm.state.outbound_commands.front().unwrap();
    assert_eq!(*island_id, 99);
    assert_eq!(command.source_island_id, 7);
    assert!(matches!(
        &command.command,
        vo_runtime::island::IslandCommand::SpawnFiber { .. }
    ));
}

#[cfg(feature = "jit")]
#[test]
fn vm_jit_goisland_transfer_txn_006_validates_later_capture_before_endpoint_publication() {
    let mut module = Module::new("go-island-later-capture-preflight-terminal-test".to_string());
    let mut func = closure_func_with_port_and_string_captures();
    install_go_callsite(&mut func, Vec::new());
    module.functions.push(func);
    install_port_string_runtime_types(&mut module);
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 7;
    let remote_island = vo_runtime::island::create(&mut vm.state.gc, 99);
    let port = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(1, ValueKind::Int64),
        1,
        0,
    );
    let capture_box = vm.state.gc.alloc(
        vo_runtime::island_msg::capture_box_meta(ValueMeta::new(0, ValueKind::Port)),
        1,
    );
    unsafe {
        vo_runtime::gc::Gc::write_slot(capture_box, 0, port as u64);
    }
    let malformed_string_capture = vm.state.gc.alloc(ValueMeta::new(0, ValueKind::String), 0);
    let closure_ref = closure::create(&mut vm.state.gc, 0, 2);
    // Safety: test fixture initializes a freshly allocated closure.
    unsafe { closure::set_capture(closure_ref, 0, capture_box as u64) };
    // Safety: test fixture initializes a freshly allocated closure.
    unsafe { closure::set_capture(closure_ref, 1, malformed_string_capture as u64) };
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    attach_go_callsite(&mut ctx, &mut fiber, 0);

    let result = jit_go_island(
        &mut ctx,
        remote_island as u64,
        closure_ref as u64,
        core::ptr::null(),
        0,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    drop(ctx);
    assert!(
        queue::home_info(port).is_none(),
        "later capture-box drift must fail before local endpoint state is published"
    );
    assert!(!vm.state.endpoint_registry.has_live());
    assert!(vm.pending_runtime_transitions.is_empty());
    assert_eq!(vm.state.outbound_commands.len(), 0);
}

#[cfg(feature = "jit")]
#[test]
fn vm_direct_method_capture_protocol_006_jit_goisland_transfers_one_slot_struct_receiver_raw() {
    let mut module = direct_method_one_slot_struct_module();
    install_go_callsite(&mut module.functions[0], Vec::new());
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 7;
    let remote_island = vo_runtime::island::create(&mut vm.state.gc, 99);
    let port = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(3, ValueKind::Int64),
        1,
        0,
    );
    let closure_ref = closure::create(&mut vm.state.gc, 0, 1);
    // Safety: test fixture initializes a freshly allocated closure.
    unsafe { closure::set_capture(closure_ref, 0, port as u64) };
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    attach_go_callsite(&mut ctx, &mut fiber, 0);

    let result = jit_go_island(
        &mut ctx,
        remote_island as u64,
        closure_ref as u64,
        core::ptr::null(),
        0,
    );

    assert_eq!(result, JitResult::ExternSuspend);
    drop(ctx);
    assert_eq!(
        fiber.jit_extern_suspend.take(),
        Some(JitExternSuspend::Yield { resume_pc: 1 })
    );
    assert!(queue::home_info(port).is_some());
    assert!(vm.state.endpoint_registry.has_live());
    assert_eq!(vm.state.outbound_commands.len(), 0);

    let result = vm.attach_pending_runtime_transitions(crate::vm::ExecResult::JitError(
        "injected terminal JIT error".to_string(),
    ));
    let crate::vm::ExecResult::Transition(transition) = result else {
        panic!("terminal JIT error should carry committed direct-method island effects");
    };
    assert!(
        !transition.island_commands.is_empty(),
        "direct receiver spawn command must survive once receiver port state is committed"
    );
    let _ = vm.apply_runtime_transition(None, transition);

    assert_eq!(vm.state.outbound_commands.len(), 1);
    let (island_id, command) = vm.state.outbound_commands.front().unwrap();
    assert_eq!(*island_id, 99);
    assert_eq!(command.source_island_id, 7);
    assert!(matches!(
        &command.command,
        vo_runtime::island::IslandCommand::SpawnFiber { .. }
    ));
}

#[test]
fn vm_jit_001_go_start_rejects_arg_slot_drift_before_spawn_publication() {
    let mut func = minimal_func();
    func.param_slots = 1;
    func.local_slots = 1;
    func.gc_scan_slots = 0;
    func.slot_types = vec![SlotType::Value];

    assert_invalid_regular_go_start_rejected(func, 2);
}

#[test]
fn vm_closure_spawn_shape_002_jit_go_start_rejects_closure_arg_slot_drift_before_spawn_publication()
{
    let mut module = Module::new("go-start-closure-callback-abi-test".to_string());
    module.functions.push(closure_func_with_one_user_arg());
    let mut vm = Vm::new();
    let closure_ref = closure::create(&mut vm.state.gc, 0, 0);
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let args = [11_u64, 22];
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );

    let result = jit_go_start(&mut ctx, 0, 1, closure_ref as u64, args.as_ptr(), 2);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    drop(ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(vm.scheduler.fibers.is_empty());
}

#[test]
fn vm_gostart_closure_signature_003_jit_rejects_arg_slot_metadata_drift_before_spawn_publication() {
    let mut module = Module::new("go-start-closure-callsite-layout-test".to_string());
    let mut func = closure_func_with_one_user_arg();
    install_go_callsite(&mut func, vec![SlotType::GcRef]);
    module.functions.push(func);
    let mut vm = Vm::new();
    let closure_ref = closure::create(&mut vm.state.gc, 0, 0);
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let args = [11_u64];
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    attach_go_callsite(&mut ctx, &mut fiber, 0);

    let result = jit_go_start(&mut ctx, 0, 1, closure_ref as u64, args.as_ptr(), 1);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    drop(ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(vm.scheduler.fibers.is_empty());
}

#[test]
fn vm_goisland_remote_shape_002_jit_rejects_arg_slot_metadata_drift_before_island_effects() {
    let mut module = Module::new("go-island-closure-callsite-layout-test".to_string());
    let mut func = closure_func_with_one_user_arg();
    install_go_callsite(&mut func, vec![SlotType::GcRef]);
    module.functions.push(func);
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 7;
    let remote_island = vo_runtime::island::create(&mut vm.state.gc, 99);
    let closure_ref = closure::create(&mut vm.state.gc, 0, 0);
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let args = [11_u64];
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    attach_go_callsite(&mut ctx, &mut fiber, 0);

    let result = jit_go_island(
        &mut ctx,
        remote_island as u64,
        closure_ref as u64,
        args.as_ptr(),
        1,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    drop(ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert_eq!(vm.state.outbound_commands.len(), 0);
    assert!(vm.scheduler.fibers.is_empty());
}

#[test]
fn vm_goisland_object_kind_002_jit_rejects_null_non_empty_args_before_island_effect() {
    let mut module = Module::new("go-island-closure-callback-abi-test".to_string());
    module.functions.push(closure_func_with_one_user_arg());
    let mut vm = Vm::new();
    let remote_island = vo_runtime::island::create(&mut vm.state.gc, 99);
    let closure_ref = closure::create(&mut vm.state.gc, 0, 0);
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );

    let result = jit_go_island(
        &mut ctx,
        remote_island as u64,
        closure_ref as u64,
        core::ptr::null(),
        1,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    drop(ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(vm.scheduler.fibers.is_empty());
}

#[test]
fn vm_jit_001_go_start_rejects_param_slots_past_local_bounds() {
    let mut func = minimal_func();
    func.param_slots = 2;
    func.local_slots = 1;
    func.gc_scan_slots = 0;
    func.slot_types = vec![SlotType::Value];

    assert_invalid_regular_go_start_rejected(func, 2);
}

#[test]
fn vm_goisland_object_kind_002_jit_rejects_nil_island_before_object_header_read() {
    let mut module = Module::new("test".to_string());
    let mut caller = minimal_func();
    install_go_callsite(&mut caller, Vec::new());
    module.functions.push(caller);
    let mut vm = Vm::new();
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    attach_go_callsite(&mut ctx, &mut fiber, 0);

    let result = jit_go_island(&mut ctx, 0, 0, core::ptr::null(), 0);

    assert_eq!(result, JitResult::Panic);
    assert_eq!(
        ctx.runtime_trap_kind,
        JitRuntimeTrapKind::NilPointerDereference as u8
    );
}

#[test]
fn vm_goisland_object_kind_002_jit_rejects_nil_closure_before_object_header_read() {
    let mut module = Module::new("test".to_string());
    let mut caller = minimal_func();
    install_go_callsite(&mut caller, Vec::new());
    module.functions.push(caller);
    let mut vm = Vm::new();
    let island = vo_runtime::island::create(&mut vm.state.gc, vm.state.current_island_id);
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    attach_go_callsite(&mut ctx, &mut fiber, 0);

    let result = jit_go_island(&mut ctx, island as u64, 0, core::ptr::null(), 0);

    assert_eq!(result, JitResult::Panic);
    assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::NilFuncCall as u8);
}

#[test]
fn vm_goisland_object_kind_002_jit_rejects_non_island_gcref_before_island_header_read() {
    let mut module = Module::new("test".to_string());
    module.functions.push(minimal_func());
    let mut vm = Vm::new();
    let wrong_island = closure::create(&mut vm.state.gc, 0, 0);
    let closure_ref = closure::create(&mut vm.state.gc, 0, 0);
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );

    let result = jit_go_island(
        &mut ctx,
        wrong_island as u64,
        closure_ref as u64,
        core::ptr::null(),
        0,
    );

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
}

#[test]
fn vm_goisland_object_kind_002_jit_rejects_non_closure_gcref_before_closure_header_read() {
    let mut module = Module::new("test".to_string());
    module.functions.push(minimal_func());
    let mut vm = Vm::new();
    let island = vo_runtime::island::create(&mut vm.state.gc, vm.state.current_island_id);
    let wrong_closure = vo_runtime::island::create(&mut vm.state.gc, vm.state.current_island_id);
    let mut fiber = Fiber::new(0);
    let mut itab_cache = ItabCache::new();
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut vm,
        &module,
        &mut fiber,
        &mut itab_cache,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );

    let result = jit_go_island(
        &mut ctx,
        island as u64,
        wrong_closure as u64,
        core::ptr::null(),
        0,
    );

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
}
