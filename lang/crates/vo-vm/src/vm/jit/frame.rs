//! JIT frame management callbacks.

use vo_runtime::jit_api::JitContext;

use crate::fiber::Fiber;

/// Push a new frame for JIT-to-JIT call (fast path).
///
/// This is the optimized version that does NOT push to resume_stack.
/// Resume points are only pushed lazily via jit_push_resume_point when
/// callee returns Call/WaitIo (slow path).
///
/// Called by JIT code for JIT-to-JIT calls:
/// 1. Ensures fiber.stack has capacity for local_slots
/// 2. Zeros the new frame region
/// 3. Updates fiber.sp
/// 4. Updates ctx.jit_bp and ctx.stack_ptr (in case of reallocation)
///
/// # Returns
/// args_ptr for the new frame (fiber.stack_ptr + new_bp)
///
/// # Safety
/// All pointers must be valid. Called from JIT-generated code.
pub extern "C" fn jit_push_frame(
    ctx: *mut JitContext,
    func_id: u32,
    local_slots: u32,
    ret_reg: u32,
    ret_slots: u32,
    caller_resume_pc: u32,
) -> *mut u64 {
    let ctx_ref = unsafe { &mut *ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    
    // Set caller's frame.pc so VM can resume the caller at the correct
    // position if the callee returns Call (e.g., PREPARED trampoline).
    if let Some(caller_frame) = fiber.frames.last_mut() {
        caller_frame.pc = caller_resume_pc as usize;
    }
    
    // New frame base is current sp.
    // Use ctx.fiber_sp (not fiber.sp) as the canonical stack pointer.
    // In JIT non-OK propagation blocks, ctx.fiber_sp is correctly restored
    // to the caller's sp, but fiber.sp may still reflect a deeper call's sp.
    let new_bp = ctx_ref.fiber_sp as usize;
    let new_sp = new_bp + local_slots as usize;
    
    // Ensure capacity (may reallocate fiber.stack)
    fiber.ensure_capacity(new_sp);
    
    // NOTE: No zeroing here! With mixed stack, callee initializes its own
    // locals_slot in prologue. fiber.stack is only used for parameter passing
    // and spilling on slow path.
    
    // Update fiber.sp
    fiber.sp = new_sp;
    
    // NOTE: No resume_stack.push here! This is the fast path.
    // Resume points are pushed lazily via jit_push_resume_point only when
    // callee returns Call/WaitIo.
    
    // Update ctx fields (stack_ptr may have changed due to reallocation)
    ctx_ref.stack_ptr = fiber.stack_ptr();
    ctx_ref.stack_cap = fiber.stack.len() as u32;
    ctx_ref.jit_bp = new_bp as u32;
    ctx_ref.fiber_sp = new_sp as u32;
    
    // Return args_ptr for the new frame
    unsafe { ctx_ref.stack_ptr.add(new_bp) }
}

/// Pop the current JIT frame after callee returns (fast path).
///
/// Restores fiber.sp and ctx.jit_bp to caller's state.
/// The caller passes its saved bp directly since we don't use resume_stack anymore.
///
/// # Safety
/// All pointers must be valid. Called from JIT-generated code.
pub extern "C" fn jit_pop_frame(ctx: *mut JitContext, caller_bp: u32) {
    let ctx_ref = unsafe { &mut *ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    
    // Restore fiber.sp to caller's sp (which is callee's bp, stored in ctx.jit_bp)
    fiber.sp = ctx_ref.jit_bp as usize;
    ctx_ref.fiber_sp = ctx_ref.jit_bp;
    
    // Restore jit_bp to caller's bp (passed as parameter)
    ctx_ref.jit_bp = caller_bp;
    
    // NOTE: No resume_stack.pop here! This is the fast path.
    // Resume points are pushed lazily via jit_push_resume_point only when
    // callee returns Call/WaitIo.
}

/// Push a resume point on side-exit (Call/WaitIo) - slow path.
///
/// Called by JIT code when callee returns non-OK, before propagating the result.
/// This builds the call chain in reverse order (callee-to-caller).
///
/// # Safety
/// All pointers must be valid. Called from JIT-generated code.
pub extern "C" fn jit_push_resume_point(
    ctx: *mut JitContext,
    func_id: u32,
    resume_pc: u32,
    bp: u32,
    caller_bp: u32,
    ret_reg: u32,
    ret_slots: u32,
) {
    let ctx_ref = unsafe { &mut *ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    
    // Push to resume_stack (builds chain in reverse: callee first, then caller)
    #[cfg(feature = "jit")]
    fiber.resume_stack.push(crate::fiber::ResumePoint {
        func_id,
        resume_pc,
        bp: bp as usize,
        caller_bp: caller_bp as usize,
        ret_reg: ret_reg as u16,
        ret_slots: ret_slots as u16,
    });
}
