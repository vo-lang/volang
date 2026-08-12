use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlagsData as MemFlags, StackSlot, Value};

use vo_runtime::jit_api::{JitContextField, JitResult};

use crate::helpers::RuntimeHelper;
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
    helper: RuntimeHelper,
    args: &[Value],
) -> Value {
    let call = crate::translator::emit_runtime_helper_call(emitter, helper, args);
    let result = emitter.builder().inst_results(call)[0];
    check_call_result(emitter, result, true);
    result
}

/// Parameters for the non-OK slow path (shared by direct/indirect/self-recursive calls).
///
/// When a JIT callee returns non-OK, the caller must:
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
    restore_caller_execution_context(emitter, p.caller_bp, p.old_fiber_sp, p.caller_func_id);
    emitter.refresh_stack_base_after_reallocation();

    emitter.spill_all_vars();

    let push_frame_fn_ptr = emitter.load_context_field(types::I64, JitContextField::PushFrameFn);
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

    // Dynamic calls may pass args via native stack scratch. Static direct calls
    // already placed callee state in the fiber shadow window.
    if let Some((args_slot, arg_count)) = p.copy_args {
        for i in 0..arg_count {
            let val = emitter.builder().ins().stack_load(
                types::I64,
                types::I64,
                args_slot,
                (i * 8) as i32,
            );
            emitter.builder().ins().store(
                MemFlags::trusted(),
                val,
                callee_fiber_args_ptr,
                (i * 8) as i32,
            );
        }
    }

    let push_resume_point_fn_ptr =
        emitter.load_context_field(types::I64, JitContextField::PushResumePointFn);
    let callee_bp = emitter.load_context_field(types::I32, JitContextField::JitBp);
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
    let error_block = crate::compile_common::cold_block(emitter.builder());
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
/// Every non-zero `JitResult`, including VM-owned runtime-transition exits,
/// returns to the VM before generated code can execute the next instruction.
pub fn check_call_result<'a, E: HelperCallEmitter<'a>>(
    emitter: &mut E,
    result: Value,
    spill_vars: bool,
) {
    let ok_block = emitter.builder().create_block();
    let non_ok_block = crate::compile_common::cold_block(emitter.builder());

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
