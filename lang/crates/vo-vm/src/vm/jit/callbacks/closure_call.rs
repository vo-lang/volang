//! JIT-to-JIT direct call support for closures and interface methods.
//!
//! These callbacks prepare a closure/iface call for potential JIT-to-JIT direct dispatch.
//! They handle: func_id resolution, jit_func_table lookup, push_frame, and arg layout.

use vo_runtime::bytecode::FunctionDef;
use vo_runtime::gc::GcRef;
use vo_runtime::jit_api::{DynCallIC, JitContext, PreparedCall};
use vo_runtime::objects::closure;

#[inline]
fn can_use_direct_call_table_entry(func_def: &FunctionDef) -> bool {
    !func_def.has_defer && !func_def.has_calls && !func_def.has_call_extern
}

/// Look up a function in the direct_call_table for JIT-to-JIT fast path.
/// Returns non-null only when callee is a no-defer leaf accepted by
/// vo_jit::can_direct_jit_call. The local predicate keeps the callback safe
/// even if an older or test-built table is over-populated.
#[inline]
fn lookup_direct_call_ptr(ctx: &JitContext, func_id: u32, func_def: &FunctionDef) -> *const u8 {
    if can_use_direct_call_table_entry(func_def) && func_id < ctx.direct_call_count {
        unsafe { *ctx.direct_call_table.add(func_id as usize) }
    } else {
        core::ptr::null()
    }
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
) {
    let ctx = unsafe { &mut *ctx };
    let module = unsafe { &*(ctx.module) };

    // 1. Resolve func_id from closure
    let closure_gcref = closure_ref as GcRef;
    let func_id = closure::func_id(closure_gcref);
    let func_def = &module.functions[func_id as usize];
    let local_slots = func_def.local_slots as usize;

    // 2. Determine if callee can use JIT fast path
    let jit_func_ptr = lookup_direct_call_ptr(ctx, func_id, func_def);

    // 3. push_frame: always allocate callee frame on fiber.stack.
    //    Both fast path (JIT direct call) and slow path (call_vm trampoline) need valid callee_args_ptr.
    let push_frame_fn = ctx.push_frame_fn.expect("push_frame_fn not set");
    let callee_args_ptr = push_frame_fn(
        ctx,
        func_id,
        local_slots as u32,
        ret_reg,
        ret_slots,
        caller_resume_pc,
    );

    // 4. Copy args with correct closure layout
    let layout = closure::call_layout(
        closure_ref,
        closure_gcref,
        func_def.recv_slots as usize,
        func_def.is_closure,
    );

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
) {
    use vo_runtime::objects::interface;

    let ctx_ref = unsafe { &mut *ctx };
    let module = unsafe { &*(ctx_ref.module) };
    let itab_cache = unsafe { &*ctx_ref.itab_cache };

    // 1. Resolve func_id from itab
    let itab_id = interface::unpack_itab_id(iface_slot0);
    let func_id = itab_cache.lookup_method(itab_id, method_idx as usize);
    let func_def = &module.functions[func_id as usize];
    let local_slots = func_def.local_slots as usize;
    let recv_slots = func_def.recv_slots as usize;

    // 2. Determine if callee can use JIT fast path
    let jit_func_ptr = lookup_direct_call_ptr(ctx_ref, func_id, func_def);

    // 3. push_frame: always allocate callee frame on fiber.stack
    let push_frame_fn = ctx_ref.push_frame_fn.expect("push_frame_fn not set");
    let callee_args_ptr = push_frame_fn(
        ctx,
        func_id,
        local_slots as u32,
        ret_reg,
        ret_slots,
        caller_resume_pc,
    );

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
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn direct_call_lookup_predicate_rejects_defer_and_nested_calls() {
        assert!(can_use_direct_call_table_entry(&func(false, false, false)));
        assert!(!can_use_direct_call_table_entry(&func(true, false, false)));
        assert!(!can_use_direct_call_table_entry(&func(false, true, false)));
        assert!(!can_use_direct_call_table_entry(&func(false, false, true)));
    }
}
