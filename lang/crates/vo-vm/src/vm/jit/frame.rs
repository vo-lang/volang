//! JIT frame management callbacks.

use vo_runtime::jit_api::{JitContext, JitResult, JitRuntimeTrapKind};

use crate::fiber::Fiber;

use super::callbacks::helpers::record_runtime_trap;

/// Reserve a callee stack window for a prepared JIT-to-JIT call.
///
/// This does not push `fiber.frames` or `resume_stack`. Resume points are
/// recorded lazily via `jit_push_resume_point` only when the callee side-exits.
///
/// Called by JIT code for JIT-to-JIT calls:
/// 1. Ensures fiber.stack has capacity for local_slots
/// 2. Reserves the new frame region without initializing locals
/// 3. Updates fiber.sp
/// 4. Updates ctx.jit_bp, ctx.fiber_sp, and stack_ptr/cap if reallocated
///
/// # Returns
/// args_ptr for the new frame (fiber.stack_ptr + new_bp)
///
/// # Safety
/// All pointers must be valid. Called from JIT-generated code.
pub extern "C" fn jit_push_frame(
    ctx: *mut JitContext,
    // ABI-required: JIT codegen emits all 6 args per import_push_frame_sig.
    // func_id, ret_reg, ret_slots are passed by the caller but only used by
    // jit_push_resume_point on the slow path; this fast-path callback ignores them.
    _func_id: u32,
    local_slots: u32,
    _ret_reg: u32,
    _ret_slots: u32,
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
    let new_sp = match fiber.try_reserve_slots_at(new_bp, local_slots as usize) {
        Ok(sp) => sp,
        Err(_) => {
            record_runtime_trap(
                ctx_ref,
                JitRuntimeTrapKind::StackOverflow,
                caller_resume_pc.saturating_sub(1),
            );
            return core::ptr::null_mut();
        }
    };

    // NOTE: No zeroing here! With mixed stack, callee initializes its own
    // locals_slot in prologue. fiber.stack is only used for parameter passing
    // and spilling on slow path.

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
/// Restores fiber.sp and ctx.jit_bp to caller's state. The caller passes its
/// saved bp directly; `resume_stack` is only involved after side-exits.
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

    // NOTE: No resume_stack.pop here. Prepared-call resume points are only
    // pushed lazily after a side-exit, so the OK path has nothing to pop.
}

/// Push a resume point on side-exit (Call/WaitIo) - slow path.
///
/// Called by JIT code when callee returns non-OK, before propagating the result.
/// This builds the call chain in reverse order (callee-to-caller).
///
/// Push a resume point to the fiber's resume_stack.
///
/// - `func_id`: CALLEE's func_id (the function whose frame is at `bp`)
/// - `resume_pc`: CALLER's resume pc (where the caller should continue after callee returns)
/// - `bp`: callee's base pointer (where callee's frame lives in fiber.stack)
/// - `caller_bp`: caller's base pointer
/// - `ret_reg`/`ret_slots`: caller's return destination (where callee's return values go)
///
/// materialize_jit_frames creates CallFrame(func_id, bp) from each entry.
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
) -> JitResult {
    let ctx_ref = unsafe { &mut *ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };

    // Push to resume_stack (builds chain in reverse: innermost callee first, outermost caller last)
    #[cfg(feature = "jit")]
    {
        let pending = fiber.resume_stack.len().saturating_add(1);
        if fiber.try_reserve_call_frames(pending).is_err() {
            record_runtime_trap(
                ctx_ref,
                JitRuntimeTrapKind::StackOverflow,
                resume_pc.saturating_sub(1),
            );
            return JitResult::Panic;
        }
        fiber.resume_stack.push(crate::fiber::ResumePoint {
            func_id,
            resume_pc,
            bp: bp as usize,
            caller_bp: caller_bp as usize,
            ret_reg: ret_reg as u16,
            ret_slots: ret_slots as u16,
        });
    }
    JitResult::Ok
}

#[cfg(test)]
mod tests {
    use super::super::context::build_jit_context;
    use super::super::test_support::function;
    use super::*;
    use crate::fiber::{Fiber, MAX_STACK_CAPACITY};
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;

    #[test]
    fn jit_push_frame_capacity_failure_records_recoverable_trap() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-push-frame-contract-test".to_string());
        module.functions.push(function(1, 0));
        vm.load(module).unwrap();

        let module_ptr = vm.module.as_ref().unwrap() as *const Module;
        let mut fiber = Fiber::new(7);
        fiber.push_frame(0, 1, 0, 0, 0);
        let mut ctx =
            unsafe { build_jit_context(&mut vm, &mut fiber, &*module_ptr) }.expect("jit context");
        ctx.ctx.fiber_sp = MAX_STACK_CAPACITY as u32;

        let args = jit_push_frame(ctx.as_ptr(), 0, 1, 0, 0, 12);

        assert!(args.is_null());
        assert!(fiber.jit_panic_flag);
        assert_eq!(
            ctx.ctx.runtime_trap_kind,
            JitRuntimeTrapKind::StackOverflow as u8
        );
        assert_eq!(ctx.ctx.runtime_trap_pc, 11);
    }
}
