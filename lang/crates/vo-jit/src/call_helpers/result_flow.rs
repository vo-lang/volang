use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, FuncRef, InstBuilder, MemFlags, StackSlot, Value};

use vo_runtime::jit_api::{JitContext, JitResult};

use crate::translator::{HelperCallEmitter, IrEmitter};
use crate::JitError;

use super::{
    emit_checked_jit_result_indirect_callback_call, emit_raw_jit_context_callback_call,
    restore_caller_execution_context, NON_OK_SLOW_PATH_PUSH_FRAME_CALLSITE,
    NON_OK_SLOW_PATH_PUSH_RESUME_POINT_CALLSITE,
};

// JitResult constants for readability in lowering code.
pub const JIT_RESULT_OK: i32 = 0;
pub const JIT_RESULT_CALL: i32 = 2;
pub const JIT_RESULT_REPLAY: i32 = 5;

/// Emit a helper/callback wrapper that returns `JitResult`, and route every
/// non-Ok result back to the VM before local execution can continue.
pub fn emit_checked_jit_result_helper_call<'a, E: HelperCallEmitter<'a>>(
    emitter: &mut E,
    func: FuncRef,
    args: &[Value],
    spill_vars: bool,
) -> Value {
    let call = crate::translator::emit_funcref_call(emitter, func, args);
    let result = emitter.builder().inst_results(call)[0];
    check_call_result(emitter, result, spill_vars);
    result
}

/// Parameters for the non-OK slow path (shared by direct/indirect/self-recursive calls).
///
/// When a JIT callee returns non-OK (Call/WaitIo/Panic), the caller must:
/// 1. Restore ctx.jit_bp and ctx.fiber_sp to caller's values
/// 2. Spill SSA variables to fiber.stack
/// 3. push_frame to materialize callee frame
/// 4. Optionally copy args from native stack to fiber.stack
/// 5. push_resume_point for frame chain
/// 6. Return the JIT result
pub struct NonOkSlowPathParams {
    pub jit_result: Value,
    pub ctx: Value,
    pub caller_bp: Value,
    pub old_fiber_sp: Value,
    pub caller_func_id: Value,
    /// CALLEE's func_id, used in push_resume_point to create CallFrame(callee_func_id, callee_bp).
    pub callee_func_id_val: Value,
    pub local_slots_val: Value,
    pub ret_reg_val: Value,
    pub ret_slots_val: Value,
    pub caller_resume_pc_val: Value,
    /// Optional: (args_slot, arg_count) to copy args from native stack to fiber.stack after push_frame.
    pub copy_args: Option<(StackSlot, usize)>,
}

/// Emit the non-OK slow path: restore ctx, spill, push_frame, push_resume_point, return.
///
/// Caller is responsible for creating/switching to the non-OK block before calling this.
pub fn emit_non_ok_slow_path<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    p: NonOkSlowPathParams,
) -> Result<(), JitError> {
    let ctx = p.ctx;

    // The inline update set fiber_sp = old_fiber_sp + callee_local_slots; push_frame
    // uses fiber_sp as new_bp, so restore ctx to the caller window first.
    restore_caller_execution_context(emitter, ctx, p.caller_bp, p.old_fiber_sp, p.caller_func_id);
    emitter.refresh_stack_base_after_reallocation();

    emitter.spill_all_vars();

    let push_frame_fn_ptr = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_PUSH_FRAME_FN,
    );
    let callee_fiber_args_ptr = emit_raw_jit_context_callback_call(
        emitter,
        NON_OK_SLOW_PATH_PUSH_FRAME_CALLSITE,
        push_frame_fn_ptr,
        &[
            ctx,
            p.callee_func_id_val,
            p.local_slots_val,
            p.ret_reg_val,
            p.ret_slots_val,
            p.caller_resume_pc_val,
        ],
    )
    .and_then(|value| {
        value.ok_or_else(|| {
            JitError::Internal("push_frame_fn ABI did not return callee args pointer".into())
        })
    })?;
    emit_return_jit_error_if_null_callee_args(emitter, callee_fiber_args_ptr);

    // Self-recursive calls pass args via native stack slot. Regular/indirect
    // calls already have callee state spilled into fiber.stack.
    if let Some((args_slot, arg_count)) = p.copy_args {
        for i in 0..arg_count {
            let val = emitter
                .builder()
                .ins()
                .stack_load(types::I64, args_slot, (i * 8) as i32);
            emitter.builder().ins().store(
                MemFlags::trusted(),
                val,
                callee_fiber_args_ptr,
                (i * 8) as i32,
            );
        }
    }

    let push_resume_point_fn_ptr = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_PUSH_RESUME_POINT_FN,
    );
    let callee_bp = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_JIT_BP,
    );
    emit_checked_jit_result_indirect_callback_call(
        emitter,
        NON_OK_SLOW_PATH_PUSH_RESUME_POINT_CALLSITE,
        push_resume_point_fn_ptr,
        &[
            ctx,
            p.callee_func_id_val,
            p.caller_resume_pc_val,
            callee_bp,
            p.caller_bp,
            p.ret_reg_val,
            p.ret_slots_val,
        ],
        true,
    )?;

    emitter.builder().ins().return_(&[p.jit_result]);
    Ok(())
}

fn emit_return_jit_error_if_null_callee_args<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    callee_fiber_args_ptr: Value,
) {
    let zero = emitter.builder().ins().iconst(types::I64, 0);
    let is_null = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, callee_fiber_args_ptr, zero);
    let error_block = emitter.builder().create_block();
    let ok_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(is_null, error_block, &[], ok_block, &[]);

    emitter.builder().switch_to_block(error_block);
    emitter.builder().seal_block(error_block);
    let jit_error = emitter
        .builder()
        .ins()
        .iconst(types::I32, JitResult::JitError as i64);
    emitter.builder().ins().return_(&[jit_error]);

    emitter.builder().switch_to_block(ok_block);
    emitter.builder().seal_block(ok_block);
}

/// Check call result and handle non-Ok cases.
///
/// JitResult: Ok=0, Panic=1, Call=2, WaitIo=3, WaitQueue=4
pub fn check_call_result<'a, E: HelperCallEmitter<'a>>(
    emitter: &mut E,
    result: Value,
    spill_vars: bool,
) {
    let ok_block = emitter.builder().create_block();
    let non_ok_block = emitter.builder().create_block();

    let ok_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter.builder().ins().icmp(IntCC::Equal, result, ok_val);
    emitter
        .builder()
        .ins()
        .brif(is_ok, ok_block, &[], non_ok_block, &[]);

    emitter.builder().switch_to_block(non_ok_block);
    emitter.builder().seal_block(non_ok_block);

    if spill_vars {
        emitter.spill_all_vars();
    }

    emitter.builder().ins().return_(&[result]);

    emitter.builder().switch_to_block(ok_block);
    emitter.builder().seal_block(ok_block);
}

#[cfg(test)]
mod tests {
    #[test]
    fn vm_osr_slowpath_stack_001_non_ok_slow_path_refreshes_before_spill() {
        let src = vo_source_contract::production_source_without_test_modules(include_str!(
            "result_flow.rs"
        ));
        let slow_path = src
            .split("pub fn emit_non_ok_slow_path")
            .nth(1)
            .expect("non-OK slow path should exist")
            .split("let push_frame_fn_ptr")
            .next()
            .expect("non-OK slow path should push a frame after spilling");
        let refresh = slow_path
            .find("refresh_stack_base_after_reallocation")
            .expect("non-OK slow path must refresh cached stack bases");
        let spill = slow_path
            .find("spill_all_vars")
            .expect("non-OK slow path should still spill caller locals");

        assert!(
            refresh < spill,
            "non-OK slow path must refresh cached stack bases before spilling caller locals"
        );
    }

    #[test]
    fn vm_jit_resume_point_contract_062_non_ok_slow_path_restores_current_func_id_before_push_resume(
    ) {
        let src = vo_source_contract::production_source_without_test_modules(include_str!(
            "result_flow.rs"
        ));
        let slow_path = src
            .split("pub fn emit_non_ok_slow_path")
            .nth(1)
            .expect("non-OK slow path should exist");
        let restore_current_func = slow_path
            .find("restore_caller_execution_context")
            .expect("non-OK slow path must restore caller execution context");
        let push_resume = slow_path
            .find("NON_OK_SLOW_PATH_PUSH_RESUME_POINT_CALLSITE")
            .expect("non-OK slow path must push a resume point");

        assert!(
            restore_current_func < push_resume,
            "non-OK slow path must restore caller current_func_id before resume-point publication"
        );
    }

    #[test]
    fn vm_jit_shadow_capacity_roots_062_non_ok_push_frame_failure_is_fatal_before_resume_publication(
    ) {
        let src = vo_source_contract::production_source_without_test_modules(include_str!(
            "result_flow.rs"
        ));
        let slow_path = src
            .split("pub fn emit_non_ok_slow_path")
            .nth(1)
            .expect("non-OK slow path should exist");
        let push_frame = slow_path
            .find("NON_OK_SLOW_PATH_PUSH_FRAME_CALLSITE")
            .expect("non-OK slow path must push a frame");
        let null_guard = slow_path
            .find("emit_return_jit_error_if_null_callee_args")
            .expect("non-OK slow path must return JitError when frame publication fails");
        let push_resume = slow_path
            .find("NON_OK_SLOW_PATH_PUSH_RESUME_POINT_CALLSITE")
            .expect("non-OK slow path must push a resume point");
        assert!(
            push_frame < null_guard && null_guard < push_resume,
            "non-OK slow path must reject push-frame publication failure before resume publication"
        );
        assert!(
            !slow_path[push_frame..push_resume].contains("emit_return_if_runtime_trap_recorded"),
            "push-frame publication failures before resume publication must not become recoverable language panics"
        );
    }
}
