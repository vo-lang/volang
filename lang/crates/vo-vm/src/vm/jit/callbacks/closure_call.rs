//! JIT-to-JIT direct call support for closures and interface methods.
//!
//! These callbacks prepare a closure/iface call for potential JIT-to-JIT direct dispatch.
//! They handle: func_id resolution, jit_func_table lookup, push_frame, and arg layout.

use vo_runtime::bytecode::FunctionDef;
use vo_runtime::gc::GcRef;
use vo_runtime::jit_api::{
    set_jit_infra_error, DynCallIC, JitContext, JitResult, JitRuntimeTrapKind, PreparedCall,
    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, JIT_INFRA_ERROR_MISSING_CALLBACK,
};
use vo_runtime::objects::closure;

#[inline]
fn can_use_direct_call_table_entry(func_def: &FunctionDef) -> bool {
    vo_jit::can_elide_frame_for_direct_jit(func_def)
}

/// Look up a function in the direct_call_table for JIT-to-JIT fast path.
/// Returns non-null only when callee is a no-defer leaf accepted by
/// vo_jit::can_elide_frame_for_direct_jit. The local predicate keeps the callback safe
/// even if an older or test-built table is over-populated.
#[inline]
fn lookup_direct_call_ptr(ctx: &JitContext, func_id: u32, func_def: &FunctionDef) -> *const u8 {
    if can_use_direct_call_table_entry(func_def) && func_id < ctx.direct_call_count {
        unsafe { *ctx.direct_call_table.add(func_id as usize) }
    } else {
        core::ptr::null()
    }
}

#[inline]
fn record_runtime_trap(ctx: &mut JitContext, kind: JitRuntimeTrapKind, pc: u32) {
    unsafe {
        *ctx.panic_flag = true;
        *ctx.is_user_panic = false;
    }
    ctx.runtime_trap_kind = kind as u8;
    ctx.runtime_trap_arg0 = 0;
    ctx.runtime_trap_arg1 = 0;
    ctx.runtime_trap_pc = pc;
}

#[inline]
unsafe fn write_trapped_prepared_call(out: *mut PreparedCall) {
    if !out.is_null() {
        *out = PreparedCall::fallback(0, 0);
    }
}

#[inline]
fn reject_prepared_call_state(
    ctx: &mut JitContext,
    out: *mut PreparedCall,
    detail: u64,
) -> JitResult {
    unsafe { write_trapped_prepared_call(out) };
    set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, detail)
}

#[inline]
fn validate_prepared_call_shape(
    ctx: &mut JitContext,
    out: *mut PreparedCall,
    expected_user_arg_count: usize,
    user_arg_count: u32,
    expected_ret_slots: u32,
    ret_slots: u32,
    user_args: *const u64,
    ret_ptr: *mut u64,
) -> Option<JitResult> {
    if user_arg_count as usize != expected_user_arg_count {
        return Some(reject_prepared_call_state(ctx, out, user_arg_count as u64));
    }
    if ret_slots != expected_ret_slots {
        return Some(reject_prepared_call_state(ctx, out, ret_slots as u64));
    }
    if user_arg_count != 0 && user_args.is_null() {
        return Some(reject_prepared_call_state(ctx, out, user_arg_count as u64));
    }
    if ret_slots != 0 && ret_ptr.is_null() {
        return Some(reject_prepared_call_state(ctx, out, ret_slots as u64));
    }
    None
}

/// Prepare a closure call for JIT dispatch.
///
/// Always does push_frame + arg layout so callee_args_ptr is valid for both paths.
/// jit_func_ptr is non-null only when callee is in direct_call_table (safe for fast path).
pub extern "C" fn jit_prepare_closure_call(
    ctx: *mut JitContext,
    closure_ref: u64,
    ret_reg: u32,
    ret_slots: u32,
    caller_resume_pc: u32,
    user_args: *const u64,
    user_arg_count: u32,
    ret_ptr: *mut u64,
    out: *mut PreparedCall,
) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let module = unsafe { &*(ctx.module) };
    if closure_ref == 0 {
        record_runtime_trap(
            ctx,
            JitRuntimeTrapKind::NilFuncCall,
            caller_resume_pc.saturating_sub(1),
        );
        unsafe { write_trapped_prepared_call(out) };
        return JitResult::Panic;
    }

    // 1. Resolve func_id from closure
    let closure_gcref = closure_ref as GcRef;
    let func_id = closure::func_id(closure_gcref);
    let Some(func_def) = module.functions.get(func_id as usize) else {
        unsafe { write_trapped_prepared_call(out) };
        return set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, func_id as u64);
    };
    let local_slots = func_def.local_slots as usize;
    let layout = closure::call_layout(
        closure_ref,
        closure_gcref,
        func_def.recv_slots as usize,
        func_def.is_closure,
    );
    let Some(expected_user_arg_count) =
        (func_def.param_slots as usize).checked_sub(layout.arg_offset)
    else {
        return reject_prepared_call_state(ctx, out, func_def.param_slots as u64);
    };
    if let Some(result) = validate_prepared_call_shape(
        ctx,
        out,
        expected_user_arg_count,
        user_arg_count,
        func_def.ret_slots as u32,
        ret_slots,
        user_args,
        ret_ptr,
    ) {
        return result;
    }

    // 2. Determine if callee can use JIT fast path
    let jit_func_ptr = lookup_direct_call_ptr(ctx, func_id, func_def);

    // 3. push_frame: always allocate callee frame on fiber.stack.
    //    Both fast path (JIT direct call) and slow path (call_vm trampoline) need valid callee_args_ptr.
    let Some(push_frame_fn) = ctx.push_frame_fn else {
        unsafe { write_trapped_prepared_call(out) };
        return set_jit_infra_error(ctx, JIT_INFRA_ERROR_MISSING_CALLBACK, 0);
    };
    let callee_args_ptr = push_frame_fn(
        ctx,
        func_id,
        local_slots as u32,
        ret_reg,
        ret_slots,
        caller_resume_pc,
    );
    if callee_args_ptr.is_null() {
        unsafe { write_trapped_prepared_call(out) };
        return JitResult::Panic;
    }

    // 4. Copy args with correct closure layout
    if let Some(slot0_val) = layout.slot0 {
        unsafe { *callee_args_ptr = slot0_val };
    }

    let arg_offset = layout.arg_offset;
    for i in 0..user_arg_count as usize {
        unsafe {
            *callee_args_ptr.add(arg_offset + i) = *user_args.add(i);
        }
    }

    // Determine slot0_kind for IC population
    let cap_count = closure::capture_count(closure_gcref);
    let slot0_kind = if func_def.recv_slots > 0 && cap_count > 0 {
        DynCallIC::SLOT0_CAPTURE0
    } else if cap_count > 0 || func_def.is_closure {
        DynCallIC::SLOT0_CLOSURE_REF
    } else {
        DynCallIC::SLOT0_NONE
    };

    unsafe {
        *out = PreparedCall {
            jit_func_ptr,
            callee_args_ptr,
            ret_ptr,
            callee_local_slots: local_slots as u32,
            func_id,
            arg_offset: arg_offset as u32,
            slot0_kind,
            is_leaf: (!func_def.has_calls && !func_def.has_call_extern) as u32,
        };
    }
    JitResult::Ok
}

/// Prepare an interface method call for JIT dispatch.
///
/// Always does push_frame + arg layout so callee_args_ptr is valid for both paths.
pub extern "C" fn jit_prepare_iface_call(
    ctx: *mut JitContext,
    iface_slot0: u64,
    iface_slot1: u64, // receiver
    method_idx: u32,
    ret_reg: u32,
    ret_slots: u32,
    caller_resume_pc: u32,
    user_args: *const u64,
    user_arg_count: u32,
    ret_ptr: *mut u64,
    out: *mut PreparedCall,
) -> JitResult {
    use vo_runtime::objects::interface;

    let ctx_ref = unsafe { &mut *ctx };
    let module = unsafe { &*(ctx_ref.module) };
    let itab_cache = unsafe { &*ctx_ref.itab_cache };
    if interface::is_nil(iface_slot0) {
        record_runtime_trap(
            ctx_ref,
            JitRuntimeTrapKind::NilPointerDereference,
            caller_resume_pc.saturating_sub(1),
        );
        unsafe { write_trapped_prepared_call(out) };
        return JitResult::Panic;
    }

    // 1. Resolve func_id from itab
    let itab_id = interface::unpack_itab_id(iface_slot0);
    let Some(itab) = itab_cache.get_itab(itab_id) else {
        unsafe { write_trapped_prepared_call(out) };
        return set_jit_infra_error(
            ctx_ref,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            itab_id as u64,
        );
    };
    let Some(&func_id) = itab.methods.get(method_idx as usize) else {
        unsafe { write_trapped_prepared_call(out) };
        return set_jit_infra_error(
            ctx_ref,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            method_idx as u64,
        );
    };
    let Some(func_def) = module.functions.get(func_id as usize) else {
        unsafe { write_trapped_prepared_call(out) };
        return set_jit_infra_error(
            ctx_ref,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            func_id as u64,
        );
    };
    let local_slots = func_def.local_slots as usize;
    let recv_slots = func_def.recv_slots as usize;
    let param_slots = func_def.param_slots as usize;
    if recv_slots > param_slots {
        return reject_prepared_call_state(ctx_ref, out, func_def.param_slots as u64);
    }
    let expected_user_arg_count = param_slots - recv_slots;
    if let Some(result) = validate_prepared_call_shape(
        ctx_ref,
        out,
        expected_user_arg_count,
        user_arg_count,
        func_def.ret_slots as u32,
        ret_slots,
        user_args,
        ret_ptr,
    ) {
        return result;
    }

    // 2. Determine if callee can use JIT fast path
    let jit_func_ptr = lookup_direct_call_ptr(ctx_ref, func_id, func_def);

    // 3. push_frame: always allocate callee frame on fiber.stack
    let Some(push_frame_fn) = ctx_ref.push_frame_fn else {
        unsafe { write_trapped_prepared_call(out) };
        return set_jit_infra_error(ctx_ref, JIT_INFRA_ERROR_MISSING_CALLBACK, 0);
    };
    let callee_args_ptr = push_frame_fn(
        ctx,
        func_id,
        local_slots as u32,
        ret_reg,
        ret_slots,
        caller_resume_pc,
    );
    if callee_args_ptr.is_null() {
        unsafe { write_trapped_prepared_call(out) };
        return JitResult::Panic;
    }

    // 4. Copy args: receiver at slot 0, user args at recv_slots
    unsafe {
        *callee_args_ptr = iface_slot1;

        for i in 0..user_arg_count as usize {
            *callee_args_ptr.add(recv_slots + i) = *user_args.add(i);
        }
    }

    unsafe {
        *out = PreparedCall {
            jit_func_ptr,
            callee_args_ptr,
            ret_ptr,
            callee_local_slots: local_slots as u32,
            func_id,
            arg_offset: recv_slots as u32,
            slot0_kind: DynCallIC::SLOT0_IFACE_RECEIVER,
            is_leaf: (!func_def.has_calls && !func_def.has_call_extern) as u32,
        };
    }
    JitResult::Ok
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::ffi::c_void;
    use vo_runtime::bytecode::{Itab, Module};
    use vo_runtime::ffi::SentinelErrorCache;
    use vo_runtime::gc::Gc;
    use vo_runtime::itab::ItabCache;
    use vo_runtime::jit_api::{JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, JIT_INFRA_ERROR_SENTINEL};
    use vo_runtime::objects::interface;
    use vo_runtime::output::CaptureSink;
    use vo_runtime::{InterfaceSlot, ValueKind};

    fn func(has_defer: bool, has_calls: bool, has_call_extern: bool) -> FunctionDef {
        FunctionDef {
            name: "callee".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 1,
            gc_scan_slots: 0,
            ret_slots: 0,
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
        program_args: &Vec<String>,
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
        }
    }

    #[test]
    fn direct_call_lookup_predicate_uses_frame_elision_contract() {
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
    fn prepare_closure_call_rejects_arg_slot_drift() {
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
        let mut out = PreparedCall::fallback(0, 0);

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
    fn prepare_iface_call_rejects_arg_slot_drift() {
        let mut module = Module::new("test".to_string());
        let mut callee = func(false, false, false);
        callee.param_slots = 3;
        callee.recv_slots = 1;
        callee.local_slots = 4;
        callee.ret_slots = 1;
        module.functions.push(callee);

        let mut gc = Gc::new();
        let mut itab_cache = ItabCache::from_module_itabs(vec![Itab { methods: vec![0] }]);
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
        let mut out = PreparedCall::fallback(0, 0);

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
}
