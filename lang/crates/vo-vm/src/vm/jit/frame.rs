//! JIT frame management callbacks.

use vo_runtime::jit_api::{
    set_jit_infra_error, JitContext, JitResult, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
};

use crate::fiber::Fiber;
use crate::frame_call::{validate_call_frame_shape, validate_call_return_window};

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
    func_id: u32,
    local_slots: u32,
    ret_reg: u32,
    ret_slots: u32,
    _caller_resume_pc: u32,
) -> *mut u64 {
    let ctx_ref = unsafe { &mut *ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    let ret_reg = match u16::try_from(ret_reg) {
        Ok(reg) => reg,
        Err(_) => {
            let _ =
                set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, ret_reg as u64);
            return core::ptr::null_mut();
        }
    };
    let ret_slots = match u16::try_from(ret_slots) {
        Ok(slots) => slots,
        Err(_) => {
            let _ = set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                ret_slots as u64,
            );
            return core::ptr::null_mut();
        }
    };
    if let Err(detail) =
        validate_push_frame_publication(ctx_ref, func_id, local_slots, ret_reg, ret_slots)
    {
        let _ = set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, detail);
        return core::ptr::null_mut();
    }

    // New frame base is current sp.
    // Use ctx.fiber_sp (not fiber.sp) as the canonical stack pointer.
    // In JIT non-OK propagation blocks, ctx.fiber_sp is correctly restored
    // to the caller's sp, but fiber.sp may still reflect a deeper call's sp.
    let new_bp = ctx_ref.fiber_sp as usize;
    let new_sp = match fiber.try_reserve_slots_at(new_bp, local_slots as usize) {
        Ok(sp) => sp,
        Err(_) => {
            let _ = set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                local_slots as u64,
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

fn validate_push_frame_publication(
    ctx: &JitContext,
    func_id: u32,
    local_slots: u32,
    ret_reg: u16,
    ret_slots: u16,
) -> Result<(), u64> {
    let module = unsafe { ctx.module.as_ref() }.ok_or(func_id as u64)?;
    let callee = module
        .functions
        .get(func_id as usize)
        .ok_or(func_id as u64)?;
    validate_call_frame_shape(callee).map_err(|_| func_id as u64)?;
    if local_slots != u32::from(callee.local_slots) {
        return Err(local_slots as u64);
    }
    if ret_slots != callee.ret_slots {
        return Err(ret_slots as u64);
    }
    let caller = module
        .functions
        .get(ctx.current_func_id as usize)
        .ok_or(ctx.current_func_id as u64)?;
    validate_call_return_window(caller, ret_reg, ret_slots).map_err(|_| ret_reg as u64)
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
    let ret_reg = match u16::try_from(ret_reg) {
        Ok(reg) => reg,
        Err(_) => {
            return set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, ret_reg as u64)
        }
    };
    let ret_slots = match u16::try_from(ret_slots) {
        Ok(slots) => slots,
        Err(_) => {
            return set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                ret_slots as u64,
            )
        }
    };
    let ctx_ref = unsafe { &mut *ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    if let Err(detail) = validate_resume_point_publication(ctx_ref, func_id, ret_reg, ret_slots) {
        return set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, detail);
    }

    // Push to resume_stack (builds chain in reverse: innermost callee first, outermost caller last)
    #[cfg(feature = "jit")]
    {
        let pending = fiber.resume_stack.len().saturating_add(1);
        if fiber.try_reserve_call_frames(pending).is_err() {
            return set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                pending as u64,
            );
        }
        fiber.resume_stack.push(crate::fiber::ResumePoint {
            func_id,
            resume_pc,
            bp: bp as usize,
            caller_bp: caller_bp as usize,
            ret_reg,
            ret_slots,
        });
    }
    JitResult::Ok
}

fn validate_resume_point_publication(
    ctx: &JitContext,
    func_id: u32,
    ret_reg: u16,
    ret_slots: u16,
) -> Result<(), u64> {
    let module = unsafe { ctx.module.as_ref() }.ok_or(func_id as u64)?;
    let callee = module
        .functions
        .get(func_id as usize)
        .ok_or(func_id as u64)?;
    validate_call_frame_shape(callee).map_err(|_| func_id as u64)?;
    if ret_slots != callee.ret_slots {
        return Err(ret_slots as u64);
    }
    let caller = module
        .functions
        .get(ctx.current_func_id as usize)
        .ok_or(ctx.current_func_id as u64)?;
    validate_call_return_window(caller, ret_reg, ret_slots).map_err(|_| ret_reg as u64)
}

#[cfg(test)]
mod tests {
    use super::super::context::build_jit_context;
    use super::super::test_support::function;
    use super::*;
    use crate::fiber::{Fiber, MAX_STACK_CAPACITY};
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;
    use vo_runtime::jit_api::{JitRuntimeTrapKind, JIT_INFRA_ERROR_SENTINEL};
    use vo_runtime::SlotType;

    #[test]
    fn vm_jit_shadow_capacity_roots_062_push_frame_capacity_before_publication_is_fatal() {
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
        assert!(!fiber.jit_panic_flag);
        assert_eq!(ctx.ctx.runtime_trap_kind, JitRuntimeTrapKind::None as u8);
        assert_eq!(ctx.ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(
            ctx.ctx.runtime_trap_arg1,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
        );
    }

    #[test]
    fn vm_jit_resume_point_abi_006_rejects_ret_register_width_drift_before_push() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let module = Module::new("jit-resume-point-contract-test".to_string());
        let mut fiber = Fiber::new(7);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

        let result = jit_push_resume_point(ctx.as_ptr(), 0, 12, 0, 0, u32::from(u16::MAX) + 1, 1);

        assert_eq!(result, JitResult::JitError);
        assert!(fiber.resume_stack.is_empty());
        assert_eq!(ctx.ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(
            ctx.ctx.runtime_trap_arg1,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
        );
    }

    #[test]
    fn vm_jit_shadow_capacity_roots_062_push_resume_point_capacity_before_publication_is_fatal() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-resume-point-capacity-test".to_string());
        module.functions.push(function(1, 0));
        let mut fiber = Fiber::new(7);
        fiber.push_frame(0, 1, 0, 0, 0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        ctx.ctx.current_func_id = 0;
        while fiber
            .try_push_call_frame_extended(0, 0, 0, 0, 0, 0, None, 0, 0)
            .is_ok()
        {}

        let result = jit_push_resume_point(ctx.as_ptr(), 0, 12, 0, 0, 0, 0);

        assert_eq!(result, JitResult::JitError);
        assert!(fiber.resume_stack.is_empty());
        assert_eq!(ctx.ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(
            ctx.ctx.runtime_trap_arg1,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
        );
    }

    fn function_with_returns(
        local_slots: u16,
        gc_scan_slots: u16,
        ret_slots: u16,
    ) -> vo_runtime::bytecode::FunctionDef {
        let mut func = function(local_slots, gc_scan_slots);
        func.ret_slots = ret_slots;
        func.ret_slot_types = vec![SlotType::Value; ret_slots as usize];
        func
    }

    fn assert_resume_point_contract_rejects_before_publication(
        module: Module,
        current_func_id: u32,
        func_id: u32,
        ret_reg: u32,
        ret_slots: u32,
    ) {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut fiber = Fiber::new(7);
        fiber.push_frame(current_func_id, 1, 0, 0, 0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        ctx.ctx.current_func_id = current_func_id;

        let result = jit_push_resume_point(ctx.as_ptr(), func_id, 12, 0, 0, ret_reg, ret_slots);

        assert_eq!(result, JitResult::JitError);
        assert!(fiber.resume_stack.is_empty());
        assert_eq!(ctx.ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(
            ctx.ctx.runtime_trap_arg1,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
        );
    }

    fn assert_push_frame_contract_rejects_before_publication(
        module: Module,
        current_func_id: u32,
        func_id: u32,
        local_slots: u32,
        ret_reg: u32,
        ret_slots: u32,
    ) {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut fiber = Fiber::new(7);
        fiber.push_frame(current_func_id, 1, 0, 0, 0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        ctx.ctx.current_func_id = current_func_id;
        let old_sp = fiber.sp;
        let old_jit_bp = ctx.ctx.jit_bp;
        let old_fiber_sp = ctx.ctx.fiber_sp;

        let args = jit_push_frame(ctx.as_ptr(), func_id, local_slots, ret_reg, ret_slots, 12);

        assert!(args.is_null());
        assert_eq!(fiber.sp, old_sp);
        assert_eq!(ctx.ctx.jit_bp, old_jit_bp);
        assert_eq!(ctx.ctx.fiber_sp, old_fiber_sp);
        assert_eq!(ctx.ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(
            ctx.ctx.runtime_trap_arg1,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
        );
    }

    #[test]
    fn vm_jit_resume_point_contract_062_push_frame_rejects_frame_shape_before_stack_publication() {
        let mut module = Module::new("jit-push-frame-shape-test".to_string());
        module.functions.push(function(1, 0));
        let mut malformed = function(1, 0);
        malformed.param_count = 2;
        malformed.param_slots = 2;
        module.functions.push(malformed);

        assert_push_frame_contract_rejects_before_publication(module, 0, 1, 1, 0, 0);
    }

    #[test]
    fn vm_jit_resume_point_contract_062_push_frame_rejects_return_window_before_stack_publication()
    {
        let mut module = Module::new("jit-push-frame-return-window-test".to_string());
        module.functions.push(function(1, 0));
        module.functions.push(function_with_returns(1, 0, 1));

        assert_push_frame_contract_rejects_before_publication(module, 0, 1, 1, 1, 1);
    }

    #[test]
    fn vm_jit_resume_point_contract_062_rejects_missing_func_before_resume_publication() {
        let mut module = Module::new("jit-resume-point-missing-func-test".to_string());
        module.functions.push(function(1, 0));

        assert_resume_point_contract_rejects_before_publication(module, 0, 99, 0, 0);
    }

    #[test]
    fn vm_jit_resume_point_contract_062_rejects_frame_shape_before_resume_publication() {
        let mut module = Module::new("jit-resume-point-frame-shape-test".to_string());
        module.functions.push(function(1, 0));
        let mut malformed = function(1, 0);
        malformed.param_count = 2;
        malformed.param_slots = 2;
        module.functions.push(malformed);

        assert_resume_point_contract_rejects_before_publication(module, 0, 1, 0, 0);
    }

    #[test]
    fn vm_jit_resume_point_contract_062_rejects_ret_slot_mismatch_before_resume_publication() {
        let mut module = Module::new("jit-resume-point-ret-slot-test".to_string());
        module.functions.push(function(1, 0));
        module.functions.push(function_with_returns(1, 0, 1));

        assert_resume_point_contract_rejects_before_publication(module, 0, 1, 0, 0);
    }

    #[test]
    fn vm_jit_resume_point_contract_062_rejects_return_window_before_resume_publication() {
        let mut module = Module::new("jit-resume-point-return-window-test".to_string());
        module.functions.push(function(1, 0));
        module.functions.push(function_with_returns(1, 0, 1));

        assert_resume_point_contract_rejects_before_publication(module, 0, 1, 1, 1);
    }
}
