//! JIT-to-JIT direct call support for closures and interface methods.
//!
//! These callbacks prepare a closure/iface call for potential JIT-to-JIT direct dispatch.
//! They handle: func_id resolution, jit_func_table lookup, push_frame, and arg layout.

use vo_runtime::jit_api::{JitContext, PreparedCall};
use vo_runtime::gc::GcRef;
use vo_runtime::objects::closure;

/// Prepare a closure call for JIT-to-JIT dispatch.
///
/// 1. Resolve func_id from closure_ref
/// 2. Look up func_def → check has_defer (needs VM)
/// 3. Check jit_func_table[func_id] → if null, needs trampoline
/// 4. push_frame(func_id, local_slots, ...) → allocate fiber.stack
/// 5. Copy args with correct closure layout (slot0 + user_args)
/// 6. Return PreparedCall with jit_func_ptr (or null for fallback)
pub extern "C" fn jit_prepare_closure_call(
    ctx: *mut JitContext,
    closure_ref: u64,
    ret_reg: u32,
    ret_slots: u32,
    caller_resume_pc: u32,
    user_args: *const u64,
    user_arg_count: u32,
    ret_ptr: *mut u64,
) -> PreparedCall {
    let ctx = unsafe { &mut *ctx };
    let module = unsafe { &*(ctx.module as *const vo_runtime::bytecode::Module) };
    
    // 1. Resolve func_id from closure
    let closure_gcref = closure_ref as GcRef;
    let func_id = closure::func_id(closure_gcref);
    let func_def = &module.functions[func_id as usize];
    let local_slots = func_def.local_slots as usize;
    
    // 2. Check if callee needs VM (has_defer implies potential complex control flow)
    if func_def.has_defer {
        return PreparedCall::fallback(func_id, local_slots as u32);
    }
    
    // 3. Check if callee is JIT-compiled
    let jit_func_ptr = if func_id < ctx.jit_func_count {
        let table = ctx.jit_func_table;
        unsafe { *table.add(func_id as usize) }
    } else {
        core::ptr::null()
    };
    
    if jit_func_ptr.is_null() {
        return PreparedCall::fallback(func_id, local_slots as u32);
    }
    
    // 4. push_frame: allocate callee frame on fiber.stack
    let push_frame_fn = ctx.push_frame_fn.expect("push_frame_fn not set");
    let callee_args_ptr = push_frame_fn(
        ctx,
        func_id,
        local_slots as u32,
        ret_reg,
        ret_slots,
        caller_resume_pc,
    );
    
    // 5. Copy args with correct closure layout
    let layout = closure::call_layout(
        closure_ref,
        closure_gcref,
        func_def.recv_slots as usize,
        func_def.is_closure,
    );
    
    // Write slot0 if present
    if let Some(slot0_val) = layout.slot0 {
        unsafe { *callee_args_ptr = slot0_val };
    }
    
    // Copy user args at arg_offset
    let arg_offset = layout.arg_offset;
    for i in 0..user_arg_count as usize {
        unsafe {
            *callee_args_ptr.add(arg_offset + i) = *user_args.add(i);
        }
    }
    
    PreparedCall {
        jit_func_ptr,
        callee_args_ptr,
        ret_ptr,
        callee_local_slots: local_slots as u32,
        func_id,
    }
}

/// Prepare an interface method call for JIT-to-JIT dispatch.
pub extern "C" fn jit_prepare_iface_call(
    ctx: *mut JitContext,
    iface_slot0: u64,
    iface_slot1: u64,  // receiver
    method_idx: u32,
    ret_reg: u32,
    ret_slots: u32,
    caller_resume_pc: u32,
    user_args: *const u64,
    user_arg_count: u32,
    ret_ptr: *mut u64,
) -> PreparedCall {
    use vo_runtime::objects::interface;
    
    let ctx_ref = unsafe { &mut *ctx };
    let module = unsafe { &*(ctx_ref.module as *const vo_runtime::bytecode::Module) };
    let itab_cache = unsafe { &*ctx_ref.itab_cache };
    
    // 1. Resolve func_id from itab
    let itab_id = interface::unpack_itab_id(iface_slot0);
    let func_id = itab_cache.lookup_method(itab_id, method_idx as usize);
    let func_def = &module.functions[func_id as usize];
    let local_slots = func_def.local_slots as usize;
    let recv_slots = func_def.recv_slots as usize;
    
    // 2. Check if callee needs VM
    if func_def.has_defer {
        return PreparedCall::fallback(func_id, local_slots as u32);
    }
    
    // 3. Check if callee is JIT-compiled
    let jit_func_ptr = if func_id < ctx_ref.jit_func_count {
        let table = ctx_ref.jit_func_table;
        unsafe { *table.add(func_id as usize) }
    } else {
        core::ptr::null()
    };
    
    if jit_func_ptr.is_null() {
        return PreparedCall::fallback(func_id, local_slots as u32);
    }
    
    // 4. push_frame
    let push_frame_fn = ctx_ref.push_frame_fn.expect("push_frame_fn not set");
    let callee_args_ptr = push_frame_fn(
        ctx,
        func_id,
        local_slots as u32,
        ret_reg,
        ret_slots,
        caller_resume_pc,
    );
    
    // 5. Copy args: receiver at slot 0, user args at recv_slots
    // For interface calls: slot0 = itab, slot1 = receiver
    // Callee expects: [receiver (recv_slots), user_args...]
    unsafe {
        // Copy receiver (iface_slot1) to slot 0
        *callee_args_ptr = iface_slot1;
        
        // Copy user args starting at recv_slots (typically 1)
        for i in 0..user_arg_count as usize {
            *callee_args_ptr.add(recv_slots + i) = *user_args.add(i);
        }
    }
    
    PreparedCall {
        jit_func_ptr,
        callee_args_ptr,
        ret_ptr,
        callee_local_slots: local_slots as u32,
        func_id,
    }
}
