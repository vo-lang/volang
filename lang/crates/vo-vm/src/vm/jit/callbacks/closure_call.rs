//! JIT-to-JIT direct call support for closures and interface methods.
//!
//! These callbacks prepare a closure/iface call for potential JIT-to-JIT direct dispatch.
//! They handle: func_id resolution, jit_func_table lookup, push_frame, and arg layout.

use vo_runtime::bytecode::FunctionDef;
use vo_runtime::jit_api::{
    set_jit_infra_error, DynCallIC, JitContext, JitResult, JitRuntimeTrapKind, PreparedCall,
    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, JIT_INFRA_ERROR_MISSING_CALLBACK,
    JIT_INFRA_ERROR_SENTINEL,
};
use vo_runtime::objects::closure;
use vo_runtime::SlotType;

use crate::exec::{
    resolve_iface_call_target, validate_call_iface_itab_for_callsite,
    validate_iface_receiver_layout,
};
use crate::fiber::Fiber;
use crate::frame_call::{
    call_iface_layout_for_callsite, call_layout_for_callsite, validate_call_frame_shape,
    validate_call_return_window, validate_closure_callsite_layout, validate_closure_target,
    validate_function_callsite_layout, ValidClosureTarget,
};

use super::helpers::record_runtime_trap;

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
unsafe fn write_trapped_prepared_call(out: *mut PreparedCall) {
    if !out.is_null() {
        *out = PreparedCall::vm_materialization(0, 0);
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
fn reject_null_prepared_callee_args(
    ctx: &mut JitContext,
    out: *mut PreparedCall,
    detail: u64,
) -> JitResult {
    unsafe { write_trapped_prepared_call(out) };
    if ctx.runtime_trap_arg0 == JIT_INFRA_ERROR_SENTINEL {
        JitResult::JitError
    } else {
        set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, detail)
    }
}

struct PreparedCallShape {
    expected_user_arg_count: usize,
    user_arg_count: u32,
    expected_ret_slots: u32,
    ret_slots: u32,
    user_args: *const u64,
    ret_ptr: *mut u64,
}

#[inline]
fn validate_prepared_call_shape(
    ctx: &mut JitContext,
    out: *mut PreparedCall,
    shape: PreparedCallShape,
) -> Option<JitResult> {
    if out.is_null() {
        return Some(reject_prepared_call_state(ctx, out, 0));
    }
    if shape.user_arg_count as usize != shape.expected_user_arg_count {
        return Some(reject_prepared_call_state(
            ctx,
            out,
            shape.user_arg_count as u64,
        ));
    }
    if shape.ret_slots != shape.expected_ret_slots {
        return Some(reject_prepared_call_state(ctx, out, shape.ret_slots as u64));
    }
    if shape.user_arg_count != 0 && shape.user_args.is_null() {
        return Some(reject_prepared_call_state(
            ctx,
            out,
            shape.user_arg_count as u64,
        ));
    }
    if shape.ret_slots != 0 && shape.ret_ptr.is_null() {
        return Some(reject_prepared_call_state(ctx, out, shape.ret_slots as u64));
    }
    None
}

#[inline]
fn validate_prepared_call_raw_abi(
    ctx: &mut JitContext,
    out: *mut PreparedCall,
    user_args: *const u64,
    user_arg_count: u32,
    ret_ptr: *mut u64,
    ret_slots: u32,
) -> Option<JitResult> {
    if out.is_null() {
        return Some(reject_prepared_call_state(ctx, out, 0));
    }
    if user_arg_count != 0 && user_args.is_null() {
        return Some(reject_prepared_call_state(ctx, out, user_arg_count as u64));
    }
    if ret_slots != 0 && ret_ptr.is_null() {
        return Some(reject_prepared_call_state(ctx, out, ret_slots as u64));
    }
    None
}

#[inline]
fn validate_prepared_call_resume_pc(
    ctx: &mut JitContext,
    out: *mut PreparedCall,
    caller_resume_pc: u32,
) -> Result<u32, JitResult> {
    caller_resume_pc
        .checked_sub(1)
        .ok_or_else(|| reject_prepared_call_state(ctx, out, caller_resume_pc as u64))
}

fn validate_prepared_callback_return_window(
    ctx: &mut JitContext,
    out: *mut PreparedCall,
    module: &vo_runtime::bytecode::Module,
    ret_reg: u32,
    ret_slots: u32,
) -> Result<(), JitResult> {
    let ret_reg =
        u16::try_from(ret_reg).map_err(|_| reject_prepared_call_state(ctx, out, ret_reg as u64))?;
    let ret_slots = u16::try_from(ret_slots)
        .map_err(|_| reject_prepared_call_state(ctx, out, ret_slots as u64))?;
    let fiber = unsafe { (ctx.fiber as *const Fiber).as_ref() }
        .ok_or_else(|| reject_prepared_call_state(ctx, out, ret_reg as u64))?;
    let caller_frame = fiber
        .current_frame()
        .ok_or_else(|| reject_prepared_call_state(ctx, out, ret_reg as u64))?;
    let caller_func = module
        .functions
        .get(caller_frame.func_id as usize)
        .ok_or_else(|| reject_prepared_call_state(ctx, out, caller_frame.func_id as u64))?;
    validate_call_return_window(caller_func, ret_reg, ret_slots)
        .map_err(|_| reject_prepared_call_state(ctx, out, ret_reg as u64))
}

fn prepared_callsite_layout<'a>(
    ctx: &mut JitContext,
    out: *mut PreparedCall,
    module: &'a vo_runtime::bytecode::Module,
    caller_resume_pc: u32,
    context: &str,
) -> Result<(u32, &'a [SlotType], &'a [SlotType]), JitResult> {
    let Some(callsite_pc) = caller_resume_pc.checked_sub(1) else {
        return Err(reject_prepared_call_state(
            ctx,
            out,
            caller_resume_pc as u64,
        ));
    };
    let fiber = unsafe { (ctx.fiber as *const Fiber).as_ref() }
        .ok_or_else(|| reject_prepared_call_state(ctx, out, caller_resume_pc as u64))?;
    let caller_frame = fiber
        .current_frame()
        .ok_or_else(|| reject_prepared_call_state(ctx, out, caller_resume_pc as u64))?;
    let caller_func = module
        .functions
        .get(caller_frame.func_id as usize)
        .ok_or_else(|| reject_prepared_call_state(ctx, out, caller_frame.func_id as u64))?;
    let (arg_layout, ret_layout) =
        call_layout_for_callsite(caller_func, callsite_pc as usize, context)
            .map_err(|_| reject_prepared_call_state(ctx, out, callsite_pc as u64))?;
    Ok((callsite_pc, arg_layout, ret_layout))
}

fn prepared_iface_callsite_layout<'a>(
    ctx: &mut JitContext,
    out: *mut PreparedCall,
    module: &'a vo_runtime::bytecode::Module,
    caller_resume_pc: u32,
    context: &str,
) -> Result<(u32, u32, &'a [SlotType], &'a [SlotType]), JitResult> {
    let Some(callsite_pc) = caller_resume_pc.checked_sub(1) else {
        return Err(reject_prepared_call_state(
            ctx,
            out,
            caller_resume_pc as u64,
        ));
    };
    let fiber = unsafe { (ctx.fiber as *const Fiber).as_ref() }
        .ok_or_else(|| reject_prepared_call_state(ctx, out, caller_resume_pc as u64))?;
    let caller_frame = fiber
        .current_frame()
        .ok_or_else(|| reject_prepared_call_state(ctx, out, caller_resume_pc as u64))?;
    let caller_func = module
        .functions
        .get(caller_frame.func_id as usize)
        .ok_or_else(|| reject_prepared_call_state(ctx, out, caller_frame.func_id as u64))?;
    let (iface_meta_id, arg_layout, ret_layout) =
        call_iface_layout_for_callsite(caller_func, callsite_pc as usize, context)
            .map_err(|_| reject_prepared_call_state(ctx, out, callsite_pc as u64))?;
    Ok((callsite_pc, iface_meta_id, arg_layout, ret_layout))
}

fn validate_jit_closure_callsite(
    ctx: &mut JitContext,
    out: *mut PreparedCall,
    arg_layout: &[SlotType],
    ret_layout: &[SlotType],
    target: &ValidClosureTarget<'_>,
) -> Result<(), JitResult> {
    validate_closure_callsite_layout("JIT CallClosure", target, arg_layout, ret_layout)
        .map_err(|_| reject_prepared_call_state(ctx, out, target.func_id as u64))
}

#[allow(clippy::too_many_arguments)]
fn validate_jit_iface_callsite(
    ctx: &mut JitContext,
    out: *mut PreparedCall,
    arg_layout: &[SlotType],
    ret_layout: &[SlotType],
    func_id: u32,
    func: &FunctionDef,
    recv_slots: usize,
    expected_user_arg_count: usize,
) -> Result<(), JitResult> {
    validate_function_callsite_layout(
        "JIT CallIface",
        func_id,
        func,
        recv_slots,
        expected_user_arg_count,
        arg_layout,
        ret_layout,
    )
    .map_err(|_| reject_prepared_call_state(ctx, out, func_id as u64))
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
    if let Some(result) =
        validate_prepared_call_raw_abi(ctx, out, user_args, user_arg_count, ret_ptr, ret_slots)
    {
        return result;
    }
    let callsite_pc = match validate_prepared_call_resume_pc(ctx, out, caller_resume_pc) {
        Ok(callsite_pc) => callsite_pc,
        Err(result) => return result,
    };
    let (_, arg_layout, ret_layout) =
        match prepared_callsite_layout(ctx, out, module, caller_resume_pc, "JIT CallClosure") {
            Ok(layout) => layout,
            Err(result) => return result,
        };
    if closure_ref == 0 {
        record_runtime_trap(ctx, JitRuntimeTrapKind::NilFuncCall, callsite_pc);
        unsafe { write_trapped_prepared_call(out) };
        return JitResult::Panic;
    }

    // 1. Resolve and validate closure target through the VM-shared frame-call contract.
    let gc = unsafe { &*ctx.gc };
    let target = match validate_closure_target(gc, module, closure_ref, "JIT closure call") {
        Ok(target) => target,
        Err(_) => {
            unsafe { write_trapped_prepared_call(out) };
            return set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, closure_ref);
        }
    };
    let closure_gcref = target.closure_gcref;
    let func_id = target.func_id;
    let func_def = target.func;
    let layout = target.layout;
    if validate_call_frame_shape(func_def).is_err() {
        return reject_prepared_call_state(ctx, out, func_id as u64);
    }
    let local_slots = func_def.local_slots as usize;
    let Some(expected_user_arg_count) =
        (func_def.param_slots as usize).checked_sub(layout.arg_offset)
    else {
        return reject_prepared_call_state(ctx, out, func_def.param_slots as u64);
    };
    if let Some(result) = validate_prepared_call_shape(
        ctx,
        out,
        PreparedCallShape {
            expected_user_arg_count,
            user_arg_count,
            expected_ret_slots: func_def.ret_slots as u32,
            ret_slots,
            user_args,
            ret_ptr,
        },
    ) {
        return result;
    }
    if let Err(result) = validate_jit_closure_callsite(ctx, out, arg_layout, ret_layout, &target) {
        return result;
    }
    if let Err(result) =
        validate_prepared_callback_return_window(ctx, out, module, ret_reg, ret_slots)
    {
        return result;
    }

    // 2. Determine if callee can use JIT fast path
    let jit_func_ptr = if layout.receiver_capture_count > 1 {
        core::ptr::null()
    } else {
        lookup_direct_call_ptr(ctx, func_id, func_def)
    };

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
        return reject_null_prepared_callee_args(ctx, out, local_slots as u64);
    }

    // 4. Copy args with correct closure layout
    for i in 0..layout.receiver_capture_count {
        unsafe {
            *callee_args_ptr.add(i) = closure::get_capture(closure_gcref, i);
        }
    }
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
    let slot0_kind = if layout.receiver_capture_count == 1 {
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
    if let Some(result) =
        validate_prepared_call_raw_abi(ctx_ref, out, user_args, user_arg_count, ret_ptr, ret_slots)
    {
        return result;
    }
    let callsite_pc = match validate_prepared_call_resume_pc(ctx_ref, out, caller_resume_pc) {
        Ok(callsite_pc) => callsite_pc,
        Err(result) => return result,
    };
    let (_, expected_iface_meta_id, arg_layout, ret_layout) = match prepared_iface_callsite_layout(
        ctx_ref,
        out,
        module,
        caller_resume_pc,
        "JIT CallIface",
    ) {
        Ok(layout) => layout,
        Err(result) => return result,
    };
    if interface::is_nil(iface_slot0) {
        record_runtime_trap(
            ctx_ref,
            JitRuntimeTrapKind::NilPointerDereference,
            callsite_pc,
        );
        unsafe { write_trapped_prepared_call(out) };
        return JitResult::Panic;
    }

    // 1. Resolve func_id from itab through the same ABI validator as the interpreter.
    let itab_id = interface::unpack_itab_id(iface_slot0);
    if validate_call_iface_itab_for_callsite(
        itab_cache,
        itab_id,
        method_idx as usize,
        expected_iface_meta_id,
        "JIT CallIface",
    )
    .is_err()
    {
        unsafe { write_trapped_prepared_call(out) };
        return set_jit_infra_error(
            ctx_ref,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            method_idx as u64,
        );
    }
    let target = match resolve_iface_call_target(module, itab_cache, itab_id, method_idx as usize) {
        Ok(target) => target,
        Err(_) => {
            unsafe { write_trapped_prepared_call(out) };
            return set_jit_infra_error(
                ctx_ref,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                method_idx as u64,
            );
        }
    };
    let func_id = target.func_id;
    let Some(func_def) = module.functions.get(func_id as usize) else {
        unsafe { write_trapped_prepared_call(out) };
        return set_jit_infra_error(
            ctx_ref,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            func_id as u64,
        );
    };
    if validate_call_frame_shape(func_def).is_err() {
        return reject_prepared_call_state(ctx_ref, out, func_id as u64);
    }
    let local_slots = target.local_slots as usize;
    let recv_slots = func_def.recv_slots as usize;
    let param_slots = func_def.param_slots as usize;
    if recv_slots > param_slots {
        return reject_prepared_call_state(ctx_ref, out, func_def.param_slots as u64);
    }
    if validate_iface_receiver_layout(module, iface_slot0, func_def, func_id, "JIT CallIface")
        .is_err()
    {
        return reject_prepared_call_state(ctx_ref, out, func_id as u64);
    }
    let expected_user_arg_count = param_slots - recv_slots;
    if let Some(result) = validate_prepared_call_shape(
        ctx_ref,
        out,
        PreparedCallShape {
            expected_user_arg_count,
            user_arg_count,
            expected_ret_slots: func_def.ret_slots as u32,
            ret_slots,
            user_args,
            ret_ptr,
        },
    ) {
        return result;
    }
    if let Err(result) = validate_jit_iface_callsite(
        ctx_ref,
        out,
        arg_layout,
        ret_layout,
        func_id,
        func_def,
        recv_slots,
        expected_user_arg_count,
    ) {
        return result;
    }
    if let Err(result) =
        validate_prepared_callback_return_window(ctx_ref, out, module, ret_reg, ret_slots)
    {
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
        return reject_null_prepared_callee_args(ctx_ref, out, local_slots as u64);
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
mod tests;
