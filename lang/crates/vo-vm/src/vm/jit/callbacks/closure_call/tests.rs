use super::*;
use core::ffi::c_void;
use core::sync::atomic::{AtomicUsize, Ordering};
use vo_runtime::bytecode::{
    InterfaceMeta, Itab, JitInstructionMetadata, MethodInfo, Module, NamedTypeMeta,
};
use vo_runtime::ffi::SentinelErrorCache;
use vo_runtime::gc::Gc;
use vo_runtime::island;
use vo_runtime::itab::ItabCache;
use vo_runtime::jit_api::{JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, JIT_INFRA_ERROR_SENTINEL};
use vo_runtime::objects::{closure, interface};
use vo_runtime::output::CaptureSink;
use vo_runtime::{InterfaceSlot, SlotType, ValueKind};

fn func(has_defer: bool, has_calls: bool, has_call_extern: bool) -> FunctionDef {
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
        has_defer,
        has_calls,
        has_call_extern,
        code: Vec::new(),
        jit_metadata: Vec::new(),
        slot_types: Vec::new(),
        borrowed_scan_slots_prefix: Vec::new(),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    }
}

extern "C" fn test_push_frame(
    ctx: *mut JitContext,
    _func_id: u32,
    _local_slots: u32,
    _ret_reg: u32,
    _ret_slots: u32,
    _caller_resume_pc: u32,
) -> *mut u64 {
    unsafe { (*ctx).stack_ptr }
}

static COUNTING_PUSH_FRAME_CALLS: AtomicUsize = AtomicUsize::new(0);

extern "C" fn counting_push_frame(
    ctx: *mut JitContext,
    _func_id: u32,
    _local_slots: u32,
    _ret_reg: u32,
    _ret_slots: u32,
    _caller_resume_pc: u32,
) -> *mut u64 {
    COUNTING_PUSH_FRAME_CALLS.fetch_add(1, Ordering::SeqCst);
    unsafe { (*ctx).stack_ptr }
}

extern "C" fn failing_push_frame_with_infra_error(
    ctx: *mut JitContext,
    _func_id: u32,
    _local_slots: u32,
    _ret_reg: u32,
    _ret_slots: u32,
    _caller_resume_pc: u32,
) -> *mut u64 {
    set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, 62);
    core::ptr::null_mut()
}

fn attach_current_frame(ctx: &mut JitContext, fiber: &mut Fiber, func_id: u32) {
    if fiber.current_frame().is_none() {
        fiber.push_frame(func_id, 0, 0, 0, 0);
    }
    ctx.fiber = fiber as *mut Fiber as *mut c_void;
}

fn assert_invalid_callback_state(ctx: &JitContext) {
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
}

fn assert_trapped_prepared_call(out: &PreparedCall) {
    assert!(out.jit_func_ptr.is_null());
    assert!(out.callee_args_ptr.is_null());
    assert!(out.ret_ptr.is_null());
    assert_eq!(out.callee_local_slots, 0);
    assert_eq!(out.func_id, 0);
    assert_eq!(out.arg_offset, 0);
    assert_eq!(out.slot0_kind, DynCallIC::SLOT0_NONE);
    assert_eq!(out.is_leaf, 0);
}

#[test]
fn vm_jit_prepare_iface_call_validates_receiver_layout_before_frame_push_060() {
    let source = crate::source_contract::production_source_without_test_modules(include_str!(
        "../closure_call.rs"
    ));
    let iface_call = source
        .split("pub extern \"C\" fn jit_prepare_iface_call(")
        .nth(1)
        .and_then(|rest| rest.split("/// Prepare").next())
        .expect("jit_prepare_iface_call section");
    let validate_pos = iface_call
        .find("validate_iface_receiver_layout(")
        .expect("JIT CallIface must validate receiver layout");
    let push_pos = iface_call
        .find("push_frame_fn(")
        .expect("JIT CallIface must push a frame");
    assert!(
        validate_pos < push_pos,
        "JIT CallIface must reject receiver layout drift before frame push"
    );
}

#[allow(clippy::too_many_arguments)]
fn test_context(
    gc: &mut Gc,
    module: &Module,
    itab_cache: &mut ItabCache,
    stack: &mut [u64],
    safepoint_flag: &bool,
    panic_flag: &mut bool,
    is_user_panic: &mut bool,
    panic_msg: &mut InterfaceSlot,
    program_args: &Vec<Vec<u8>>,
    sentinel_errors: &mut SentinelErrorCache,
    output: &CaptureSink,
    host_output: &mut Option<Vec<u8>>,
) -> JitContext {
    JitContext {
        gc,
        globals: core::ptr::null_mut(),
        safepoint_flag,
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
        vm: core::ptr::null_mut::<c_void>(),
        fiber: core::ptr::null_mut::<c_void>(),
        itab_cache,
        extern_registry: core::ptr::null(),
        call_extern_fn: None,
        module,
        jit_func_table: core::ptr::null(),
        jit_func_count: 0,
        direct_call_table: core::ptr::null(),
        direct_call_count: 0,
        program_args,
        sentinel_errors,
        output: output as *const dyn vo_runtime::output::OutputSink,
        host_output,
        #[cfg(feature = "std")]
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
        #[cfg(feature = "std")]
        wait_io_token: 0,
        loop_exit_pc: 0,
        stack_ptr: stack.as_mut_ptr(),
        stack_cap: stack.len() as u32,
        stack_limit: stack.len() as u32,
        call_depth: 0,
        call_depth_limit: 64,
        jit_bp: 0,
        fiber_sp: 0,
        push_frame_fn: Some(test_push_frame),
        pop_frame_fn: None,
        stack_overflow_fn: None,
        push_resume_point_fn: None,
        create_island_fn: None,
        queue_len_fn: None,
        queue_cap_fn: None,
        queue_close_fn: None,
        queue_send_fn: None,
        queue_recv_fn: None,
        go_start_fn: None,
        go_island_fn: None,
        defer_push_fn: None,
        recover_fn: None,
        select_begin_fn: None,
        select_send_fn: None,
        select_recv_fn: None,
        select_exec_fn: None,
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: vo_runtime::EXECUTION_TIMESLICE_INSTRUCTIONS,
    }
}

#[test]
fn vm_direct_call_lookup_predicate_uses_frame_elision_contract() {
    assert!(can_use_direct_call_table_entry(&func(false, false, false)));
    assert!(!can_use_direct_call_table_entry(&func(true, false, false)));
    assert!(!can_use_direct_call_table_entry(&func(false, true, false)));
    assert!(!can_use_direct_call_table_entry(&func(false, false, true)));

    let mut alloc = func(false, false, false);
    alloc.code = vec![vo_runtime::instruction::Instruction::new(
        vo_runtime::instruction::Opcode::PtrNew,
        0,
        1,
        1,
    )];
    assert!(!can_use_direct_call_table_entry(&alloc));
}

#[test]
fn vm_jit_prepared_call_abi_012_nil_closure_rejects_null_out_before_trap() {
    let module = Module::new("prepared-nil-closure-raw-abi".to_string());
    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::new();
    let mut stack = [0_u64; 4];
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );

    let result = jit_prepare_closure_call(
        &mut ctx,
        0,
        0,
        0,
        1,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        core::ptr::null_mut(),
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::None as u8);
    assert!(!unsafe { *ctx.panic_flag });
}

#[test]
fn vm_jit_prepared_call_abi_012_nil_iface_rejects_null_out_before_trap() {
    let module = Module::new("prepared-nil-iface-raw-abi".to_string());
    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::new();
    let mut stack = [0_u64; 4];
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );

    let result = jit_prepare_iface_call(
        &mut ctx,
        0,
        0,
        0,
        0,
        0,
        1,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        core::ptr::null_mut(),
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::None as u8);
    assert!(!unsafe { *ctx.panic_flag });
}

#[test]
fn vm_jit_prepared_call_abi_013_nil_closure_rejects_zero_resume_pc_before_trap() {
    let module = Module::new("prepared-nil-closure-resume-pc".to_string());
    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::new();
    let mut stack = [0_u64; 4];
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_closure_call(
        &mut ctx,
        0,
        0,
        0,
        0,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::None as u8);
    assert!(!unsafe { *ctx.panic_flag });
}

#[test]
fn vm_jit_prepared_call_abi_013_nil_iface_rejects_zero_resume_pc_before_trap() {
    let module = Module::new("prepared-nil-iface-resume-pc".to_string());
    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::new();
    let mut stack = [0_u64; 4];
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_iface_call(
        &mut ctx,
        0,
        0,
        0,
        0,
        0,
        0,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::None as u8);
    assert!(!unsafe { *ctx.panic_flag });
}

#[test]
fn vm_jit_prepared_call_abi_057_nil_closure_requires_active_caller_frame_before_trap() {
    let module = Module::new("prepared-nil-closure-no-caller-frame".to_string());
    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::new();
    let mut stack = [0_u64; 4];
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut fiber = Fiber::new(0);
    ctx.fiber = &mut fiber as *mut Fiber as *mut c_void;
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_closure_call(
        &mut ctx,
        0,
        0,
        0,
        1,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::None as u8);
    assert!(!unsafe { *ctx.panic_flag });
}

#[test]
fn vm_jit_prepared_call_abi_057_nil_iface_requires_active_caller_frame_before_trap() {
    let module = Module::new("prepared-nil-iface-no-caller-frame".to_string());
    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::new();
    let mut stack = [0_u64; 4];
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut fiber = Fiber::new(0);
    ctx.fiber = &mut fiber as *mut Fiber as *mut c_void;
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_iface_call(
        &mut ctx,
        0,
        0,
        0,
        0,
        0,
        1,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::None as u8);
    assert!(!unsafe { *ctx.panic_flag });
}

#[test]
fn vm_closure_call_signature_002_jit_prepare_closure_call_rejects_arg_slot_drift() {
    let mut module = Module::new("test".to_string());
    let mut callee = func(false, false, false);
    callee.param_slots = 2;
    callee.local_slots = 4;
    callee.ret_slots = 1;
    module.functions.push(callee);

    let mut gc = Gc::new();
    let closure_ref = closure::create(&mut gc, 0, 0) as u64;
    let mut itab_cache = ItabCache::new();
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let user_args = [11_u64];
    let mut returns = [0_u64; 1];
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_closure_call(
        &mut ctx,
        closure_ref,
        4,
        1,
        10,
        user_args.as_ptr(),
        user_args.len() as u32,
        returns.as_mut_ptr(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
}

#[test]
fn vm_jit_prepared_call_frame_shape_062_closure_rejects_param_slots_beyond_locals_before_frame_push(
) {
    let mut module = Module::new("jit-closure-frame-shape-test".to_string());
    let mut callee = func(false, false, false);
    callee.param_slots = 2;
    callee.local_slots = 1;
    callee.slot_types = vec![SlotType::Value, SlotType::Value];
    callee.jit_metadata = vec![JitInstructionMetadata::None; 10];
    callee.jit_metadata[9] = JitInstructionMetadata::CallLayout {
        arg_layout: vec![SlotType::Value, SlotType::Value],
        ret_layout: Vec::new(),
    };
    module.functions.push(callee);

    let mut gc = Gc::new();
    let closure_ref = closure::create(&mut gc, 0, 0) as u64;
    let mut itab_cache = ItabCache::new();
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut fiber = Fiber::new(0);
    attach_current_frame(&mut ctx, &mut fiber, 0);
    ctx.push_frame_fn = Some(counting_push_frame);
    COUNTING_PUSH_FRAME_CALLS.store(0, Ordering::SeqCst);
    let user_args = [11_u64, 22_u64];
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_closure_call(
        &mut ctx,
        closure_ref,
        0,
        0,
        10,
        user_args.as_ptr(),
        user_args.len() as u32,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(COUNTING_PUSH_FRAME_CALLS.load(Ordering::SeqCst), 0);
}

#[test]
fn vm_jit_prepared_call_return_window_beyond_caller_locals_062_closure_rejects_before_frame_push() {
    let mut module = Module::new("jit-closure-return-window-test".to_string());
    let mut callee = func(false, false, false);
    callee.local_slots = 1;
    callee.ret_slots = 1;
    callee.ret_slot_types = vec![SlotType::Value];
    callee.jit_metadata = vec![JitInstructionMetadata::None; 10];
    callee.jit_metadata[9] = JitInstructionMetadata::CallLayout {
        arg_layout: Vec::new(),
        ret_layout: vec![SlotType::Value],
    };
    module.functions.push(callee);

    let mut gc = Gc::new();
    let closure_ref = closure::create(&mut gc, 0, 0) as u64;
    let mut itab_cache = ItabCache::new();
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut fiber = Fiber::new(0);
    attach_current_frame(&mut ctx, &mut fiber, 0);
    ctx.push_frame_fn = Some(counting_push_frame);
    COUNTING_PUSH_FRAME_CALLS.store(0, Ordering::SeqCst);
    let mut returns = [0_u64; 1];
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_closure_call(
        &mut ctx,
        closure_ref,
        1,
        1,
        10,
        core::ptr::null(),
        0,
        returns.as_mut_ptr(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(COUNTING_PUSH_FRAME_CALLS.load(Ordering::SeqCst), 0);
}

#[test]
fn vm_closure_call_signature_002_jit_prepare_closure_call_rejects_non_closure_gcref_before_header_read(
) {
    let mut module = Module::new("test".to_string());
    module.functions.push(func(false, false, false));

    let mut gc = Gc::new();
    let wrong_ref = island::create(&mut gc, 0) as u64;
    let mut itab_cache = ItabCache::new();
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_closure_call(
        &mut ctx,
        wrong_ref,
        0,
        0,
        10,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
}

#[test]
fn vm_closure_call_signature_002_jit_prepare_closure_call_rejects_capture_count_metadata_drift_before_frame_push(
) {
    let mut module = Module::new("test".to_string());
    let mut callee = func(false, false, false);
    callee.is_closure = true;
    callee.param_slots = 1;
    callee.local_slots = 1;
    callee.capture_slot_types = vec![SlotType::GcRef];
    module.functions.push(callee);

    let mut gc = Gc::new();
    let closure_ref = closure::create(&mut gc, 0, 0) as u64;
    let mut itab_cache = ItabCache::new();
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    ctx.push_frame_fn = Some(counting_push_frame);
    COUNTING_PUSH_FRAME_CALLS.store(0, Ordering::SeqCst);
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_closure_call(
        &mut ctx,
        closure_ref,
        0,
        0,
        10,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
    assert_eq!(COUNTING_PUSH_FRAME_CALLS.load(Ordering::SeqCst), 0);
}

#[test]
fn vm_closure_call_signature_002_jit_prepare_closure_call_rejects_closure_allocation_drift_before_frame_push(
) {
    let mut module = Module::new("test".to_string());
    let mut callee = func(false, false, false);
    callee.is_closure = true;
    callee.param_slots = 1;
    callee.local_slots = 1;
    module.functions.push(callee);

    let mut gc = Gc::new();
    let closure_ref = closure::create(&mut gc, 0, 0);
    unsafe { vo_runtime::gc::Gc::header_mut(closure_ref) }.slots =
        (closure::HEADER_SLOTS + 1) as u16;
    let mut itab_cache = ItabCache::new();
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    ctx.push_frame_fn = Some(counting_push_frame);
    COUNTING_PUSH_FRAME_CALLS.store(0, Ordering::SeqCst);
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_closure_call(
        &mut ctx,
        closure_ref as u64,
        0,
        0,
        10,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
    assert_eq!(COUNTING_PUSH_FRAME_CALLS.load(Ordering::SeqCst), 0);
}

#[test]
fn vm_jit_closure_canon_002_prepare_closure_call_stores_canonical_slot0() {
    let mut module = Module::new("test".to_string());
    let mut callee = func(false, false, false);
    callee.is_closure = true;
    callee.param_slots = 1;
    callee.local_slots = 1;
    callee.slot_types = vec![SlotType::GcRef];
    callee.capture_slot_types = vec![SlotType::GcRef];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.jit_metadata = vec![JitInstructionMetadata::None; 10];
    callee.jit_metadata[9] = JitInstructionMetadata::CallLayout {
        arg_layout: Vec::new(),
        ret_layout: Vec::new(),
    };
    module.functions.push(callee);

    let mut gc = Gc::new();
    let closure_ref = closure::create(&mut gc, 0, 1);
    let interior_ref = unsafe { closure_ref.add(closure::HEADER_SLOTS) } as u64;
    let mut itab_cache = ItabCache::new();
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut fiber = Fiber::new(0);
    attach_current_frame(&mut ctx, &mut fiber, 0);
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_closure_call(
        &mut ctx,
        interior_ref,
        0,
        0,
        10,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::Ok);
    assert_eq!(stack[0], closure_ref as u64);
    assert_eq!(out.arg_offset, 1);
}

#[test]
fn vm_jit_shadow_capacity_roots_062_prepare_closure_null_push_frame_is_fatal() {
    let mut module = Module::new("jit-closure-null-frame-test".to_string());
    let mut callee = func(false, false, false);
    callee.is_closure = true;
    callee.param_slots = 1;
    callee.local_slots = 1;
    callee.slot_types = vec![SlotType::GcRef];
    callee.capture_slot_types = vec![SlotType::GcRef];
    callee.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&callee.slot_types);
    callee.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
    callee.jit_metadata = vec![JitInstructionMetadata::None; 10];
    callee.jit_metadata[9] = JitInstructionMetadata::CallLayout {
        arg_layout: Vec::new(),
        ret_layout: Vec::new(),
    };
    module.functions.push(callee);

    let mut gc = Gc::new();
    let closure_ref = closure::create(&mut gc, 0, 1);
    let interior_ref = unsafe { closure_ref.add(closure::HEADER_SLOTS) } as u64;
    let mut itab_cache = ItabCache::new();
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut fiber = Fiber::new(0);
    attach_current_frame(&mut ctx, &mut fiber, 0);
    ctx.push_frame_fn = Some(failing_push_frame_with_infra_error);
    let mut out = PreparedCall {
        jit_func_ptr: 1 as *const u8,
        callee_args_ptr: 1 as *mut u64,
        ret_ptr: 1 as *mut u64,
        callee_local_slots: 7,
        func_id: 7,
        arg_offset: 7,
        slot0_kind: DynCallIC::SLOT0_CLOSURE_REF,
        is_leaf: 1,
    };

    let result = jit_prepare_closure_call(
        &mut ctx,
        interior_ref,
        0,
        0,
        10,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(ctx.runtime_trap_pc, 62);
    assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::None as u8);
    assert!(!unsafe { *ctx.panic_flag });
    assert_trapped_prepared_call(&out);
}

#[test]
fn vm_jit_shadow_capacity_roots_062_prepare_iface_null_push_frame_is_fatal() {
    let mut module = Module::new("jit-iface-null-frame-test".to_string());
    let mut caller = func(false, false, false);
    caller.jit_metadata = vec![JitInstructionMetadata::CallIfaceLayout {
        iface_meta_id: 0,
        method_idx: 0,
        arg_layout: Vec::new(),
        ret_layout: Vec::new(),
    }];
    module.functions.push(caller);
    let mut target = func(false, false, false);
    target.param_slots = 1;
    target.recv_slots = 1;
    target.local_slots = 1;
    target.slot_types = vec![SlotType::Value];
    module.functions.push(target);
    module
        .runtime_types
        .push(vo_runtime::RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(vo_runtime::RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    let mut methods = std::collections::BTreeMap::new();
    methods.insert(
        "m".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 1,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: vo_runtime::ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: vo_runtime::ValueRttid::new(0, ValueKind::Int64),
        methods,
    });

    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![1],
    }]);
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut fiber = Fiber::new(0);
    attach_current_frame(&mut ctx, &mut fiber, 0);
    ctx.push_frame_fn = Some(failing_push_frame_with_infra_error);
    let slot0 = interface::pack_slot0(0, 1, ValueKind::Int64);
    let mut out = PreparedCall {
        jit_func_ptr: 1 as *const u8,
        callee_args_ptr: 1 as *mut u64,
        ret_ptr: 1 as *mut u64,
        callee_local_slots: 7,
        func_id: 7,
        arg_offset: 7,
        slot0_kind: DynCallIC::SLOT0_IFACE_RECEIVER,
        is_leaf: 1,
    };

    let result = jit_prepare_iface_call(
        &mut ctx,
        slot0,
        123,
        0,
        0,
        0,
        1,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(ctx.runtime_trap_pc, 62);
    assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::None as u8);
    assert!(!unsafe { *ctx.panic_flag });
    assert_trapped_prepared_call(&out);
}

#[test]
fn vm_closure_call_signature_002_jit_prepare_closure_call_rejects_slot_metadata_drift_before_frame_push(
) {
    let mut module = Module::new("jit-closure-callsite-layout-test".to_string());
    let mut callee = func(false, false, false);
    callee.param_slots = 1;
    callee.local_slots = 1;
    callee.ret_slots = 1;
    callee.slot_types = vec![SlotType::Value];
    callee.ret_slot_types = vec![SlotType::Value];
    callee.jit_metadata = vec![JitInstructionMetadata::None; 10];
    callee.jit_metadata[9] = JitInstructionMetadata::CallLayout {
        arg_layout: vec![SlotType::GcRef],
        ret_layout: vec![SlotType::GcRef],
    };
    module.functions.push(callee);

    let mut gc = Gc::new();
    let closure_ref = closure::create(&mut gc, 0, 0) as u64;
    let mut itab_cache = ItabCache::new();
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut fiber = Fiber::new(0);
    attach_current_frame(&mut ctx, &mut fiber, 0);
    ctx.push_frame_fn = Some(counting_push_frame);
    COUNTING_PUSH_FRAME_CALLS.store(0, Ordering::SeqCst);
    let user_args = [11_u64];
    let mut returns = [0_u64; 1];
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_closure_call(
        &mut ctx,
        closure_ref,
        4,
        1,
        10,
        user_args.as_ptr(),
        user_args.len() as u32,
        returns.as_mut_ptr(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(COUNTING_PUSH_FRAME_CALLS.load(Ordering::SeqCst), 0);
}

#[test]
fn vm_closure_call_signature_002_jit_prepare_iface_call_rejects_slot_metadata_drift_before_frame_push(
) {
    let mut module = Module::new("jit-iface-callsite-layout-test".to_string());
    let mut callee = func(false, false, false);
    callee.param_slots = 2;
    callee.recv_slots = 1;
    callee.local_slots = 2;
    callee.ret_slots = 1;
    callee.slot_types = vec![SlotType::Value, SlotType::Value];
    callee.ret_slot_types = vec![SlotType::Value];
    callee.jit_metadata = vec![JitInstructionMetadata::None; 10];
    callee.jit_metadata[9] = JitInstructionMetadata::CallLayout {
        arg_layout: vec![SlotType::GcRef],
        ret_layout: vec![SlotType::GcRef],
    };
    module.functions.push(callee);

    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![0],
    }]);
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut fiber = Fiber::new(0);
    attach_current_frame(&mut ctx, &mut fiber, 0);
    ctx.push_frame_fn = Some(counting_push_frame);
    COUNTING_PUSH_FRAME_CALLS.store(0, Ordering::SeqCst);
    let slot0 = interface::pack_slot0(0, 0, ValueKind::Pointer);
    let user_args = [22_u64];
    let mut returns = [0_u64; 1];
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_iface_call(
        &mut ctx,
        slot0,
        123,
        0,
        4,
        1,
        10,
        user_args.as_ptr(),
        user_args.len() as u32,
        returns.as_mut_ptr(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(COUNTING_PUSH_FRAME_CALLS.load(Ordering::SeqCst), 0);
}

#[test]
fn vm_call_iface_contract_061_jit_prepare_rejects_foreign_same_receiver_same_shape_itab_before_frame_push(
) {
    let mut module = Module::new("jit-iface-foreign-itab-test".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "I".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module.interface_metas.push(InterfaceMeta {
        name: "J".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    let mut caller = func(false, false, false);
    caller.jit_metadata = vec![JitInstructionMetadata::CallIfaceLayout {
        iface_meta_id: 0,
        method_idx: 0,
        arg_layout: Vec::new(),
        ret_layout: Vec::new(),
    }];
    module.functions.push(caller);
    let mut target = func(false, false, false);
    target.param_slots = 1;
    target.recv_slots = 1;
    target.local_slots = 1;
    target.slot_types = vec![SlotType::Value];
    module.functions.push(target);

    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::from_module_itabs(vec![
        Itab::default(),
        Itab {
            iface_meta_id: 1,
            methods: vec![1],
        },
    ]);
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut fiber = Fiber::new(0);
    attach_current_frame(&mut ctx, &mut fiber, 0);
    ctx.push_frame_fn = Some(counting_push_frame);
    COUNTING_PUSH_FRAME_CALLS.store(0, Ordering::SeqCst);
    let slot0 = interface::pack_slot0(1, 0, ValueKind::Struct);
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_iface_call(
        &mut ctx,
        slot0,
        123,
        0,
        0,
        0,
        1,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(COUNTING_PUSH_FRAME_CALLS.load(Ordering::SeqCst), 0);
}

#[test]
fn vm_jit_prepared_call_abi_002_prepare_closure_call_rejects_null_out_before_frame_push() {
    let mut module = Module::new("test".to_string());
    let mut callee = func(false, false, false);
    callee.is_closure = true;
    callee.param_slots = 1;
    callee.local_slots = 1;
    module.functions.push(callee);

    let mut gc = Gc::new();
    let closure_ref = closure::create(&mut gc, 0, 0) as u64;
    let mut itab_cache = ItabCache::new();
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    ctx.push_frame_fn = Some(counting_push_frame);
    COUNTING_PUSH_FRAME_CALLS.store(0, Ordering::SeqCst);

    let result = jit_prepare_closure_call(
        &mut ctx,
        closure_ref,
        0,
        0,
        10,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        core::ptr::null_mut(),
    );

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
    assert_eq!(COUNTING_PUSH_FRAME_CALLS.load(Ordering::SeqCst), 0);
}

#[test]
fn vm_closure_call_signature_002_jit_prepare_iface_call_rejects_arg_slot_drift() {
    let mut module = Module::new("test".to_string());
    let mut callee = func(false, false, false);
    callee.param_slots = 3;
    callee.recv_slots = 1;
    callee.local_slots = 4;
    callee.ret_slots = 1;
    module.functions.push(callee);

    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![0],
    }]);
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let slot0 = interface::pack_slot0(0, 0, ValueKind::Pointer);
    let user_args = [22_u64];
    let mut returns = [0_u64; 1];
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_iface_call(
        &mut ctx,
        slot0,
        0,
        0,
        4,
        1,
        10,
        user_args.as_ptr(),
        user_args.len() as u32,
        returns.as_mut_ptr(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
}

#[test]
fn vm_jit_prepared_call_frame_shape_062_iface_rejects_scan_slots_beyond_locals_before_frame_push() {
    let mut module = Module::new("jit-iface-frame-shape-test".to_string());
    let mut callee = func(false, false, false);
    callee.param_slots = 1;
    callee.recv_slots = 1;
    callee.local_slots = 1;
    callee.gc_scan_slots = 2;
    callee.slot_types = vec![SlotType::Value];
    callee.jit_metadata = vec![JitInstructionMetadata::None; 10];
    callee.jit_metadata[9] = JitInstructionMetadata::CallIfaceLayout {
        iface_meta_id: 0,
        method_idx: 0,
        arg_layout: Vec::new(),
        ret_layout: Vec::new(),
    };
    module.functions.push(callee);
    module.runtime_types.push(vo_runtime::RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    let mut methods = std::collections::BTreeMap::new();
    methods.insert(
        "m".to_string(),
        MethodInfo {
            func_id: 0,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 0,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: vo_runtime::ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: vo_runtime::ValueRttid::new(0, ValueKind::Int64),
        methods,
    });

    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![0],
    }]);
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut fiber = Fiber::new(0);
    attach_current_frame(&mut ctx, &mut fiber, 0);
    ctx.push_frame_fn = Some(counting_push_frame);
    COUNTING_PUSH_FRAME_CALLS.store(0, Ordering::SeqCst);
    let slot0 = interface::pack_slot0(0, 0, ValueKind::Int64);
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_iface_call(
        &mut ctx,
        slot0,
        123,
        0,
        0,
        0,
        10,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(COUNTING_PUSH_FRAME_CALLS.load(Ordering::SeqCst), 0);
}

#[test]
fn vm_jit_prepared_call_return_window_beyond_caller_locals_062_iface_rejects_before_frame_push() {
    let mut module = Module::new("jit-iface-return-window-test".to_string());
    let mut callee = func(false, false, false);
    callee.param_slots = 1;
    callee.recv_slots = 1;
    callee.local_slots = 1;
    callee.ret_slots = 1;
    callee.slot_types = vec![SlotType::Value];
    callee.ret_slot_types = vec![SlotType::Value];
    callee.jit_metadata = vec![JitInstructionMetadata::None; 10];
    callee.jit_metadata[9] = JitInstructionMetadata::CallIfaceLayout {
        iface_meta_id: 0,
        method_idx: 0,
        arg_layout: Vec::new(),
        ret_layout: vec![SlotType::Value],
    };
    module.functions.push(callee);
    module.runtime_types.push(vo_runtime::RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    let mut methods = std::collections::BTreeMap::new();
    methods.insert(
        "m".to_string(),
        MethodInfo {
            func_id: 0,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 0,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: vo_runtime::ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: vo_runtime::ValueRttid::new(0, ValueKind::Int64),
        methods,
    });

    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![0],
    }]);
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    let mut fiber = Fiber::new(0);
    attach_current_frame(&mut ctx, &mut fiber, 0);
    ctx.push_frame_fn = Some(counting_push_frame);
    COUNTING_PUSH_FRAME_CALLS.store(0, Ordering::SeqCst);
    let slot0 = interface::pack_slot0(0, 0, ValueKind::Int64);
    let mut returns = [0_u64; 1];
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_iface_call(
        &mut ctx,
        slot0,
        123,
        0,
        1,
        1,
        10,
        core::ptr::null(),
        0,
        returns.as_mut_ptr(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx);
    assert_eq!(COUNTING_PUSH_FRAME_CALLS.load(Ordering::SeqCst), 0);
}

#[test]
fn vm_closure_call_signature_002_jit_prepare_iface_call_rejects_recv_slot_drift_before_push_frame()
{
    let mut module = Module::new("test".to_string());
    let mut callee = func(false, false, false);
    callee.param_slots = 2;
    callee.recv_slots = 2;
    callee.local_slots = 3;
    module.functions.push(callee);

    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![0],
    }]);
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    ctx.push_frame_fn = Some(counting_push_frame);
    COUNTING_PUSH_FRAME_CALLS.store(0, Ordering::SeqCst);
    let slot0 = interface::pack_slot0(0, 0, ValueKind::Struct);
    let mut out = PreparedCall::vm_materialization(0, 0);

    let result = jit_prepare_iface_call(
        &mut ctx,
        slot0,
        123,
        0,
        4,
        0,
        10,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        &mut out,
    );

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
    assert_eq!(COUNTING_PUSH_FRAME_CALLS.load(Ordering::SeqCst), 0);
}

#[test]
fn vm_jit_prepared_call_abi_002_prepare_iface_call_rejects_null_out_before_frame_push() {
    let mut module = Module::new("test".to_string());
    let mut callee = func(false, false, false);
    callee.param_slots = 1;
    callee.recv_slots = 1;
    callee.local_slots = 1;
    module.functions.push(callee);

    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::from_module_itabs(vec![Itab {
        iface_meta_id: 0,
        methods: vec![0],
    }]);
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let program_args = Vec::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let output = CaptureSink::new();
    let mut host_output = None;
    let mut stack = [0_u64; 16];
    let mut ctx = test_context(
        &mut gc,
        &module,
        &mut itab_cache,
        &mut stack,
        &safepoint_flag,
        &mut panic_flag,
        &mut is_user_panic,
        &mut panic_msg,
        &program_args,
        &mut sentinel_errors,
        &output,
        &mut host_output,
    );
    ctx.push_frame_fn = Some(counting_push_frame);
    COUNTING_PUSH_FRAME_CALLS.store(0, Ordering::SeqCst);
    let slot0 = interface::pack_slot0(0, 0, ValueKind::Pointer);

    let result = jit_prepare_iface_call(
        &mut ctx,
        slot0,
        321,
        0,
        0,
        0,
        10,
        core::ptr::null(),
        0,
        core::ptr::null_mut(),
        core::ptr::null_mut(),
    );

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
    assert_eq!(COUNTING_PUSH_FRAME_CALLS.load(Ordering::SeqCst), 0);
}
