use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, InstBuilder, StackSlot, Value};

use vo_runtime::jit_api::{JitContext, JitContextField};

use crate::translator::{HelperKind, IrEmitter};

use super::{
    emit_call_depth_enter, emit_call_depth_leave, emit_checked_jit_result_indirect_callback_call,
    emit_effect_aware_jit_call, emit_native_link, emit_raw_jit_context_callback_call,
    import_jit_func_sig, load_current_func_id, load_native_arg_lanes_dynamic,
    restore_caller_execution_context, JitCallGcMode, JitCallOperands, JIT_RESULT_CALL,
    JIT_RESULT_OK, PREPARED_CALL_POP_FRAME_CALLSITE, PREPARED_CALL_PUSH_RESUME_POINT_CALLSITE,
};

/// Parameters for the common prepared-call dispatch.
pub(super) struct PreparedCallParams {
    pub(super) jit_func_ptr: Value,
    pub(super) callee_args_ptr: Value,
    pub(super) func_id: Value,
    pub(super) callee_local_slots: Value,
    pub(super) jit_may_gc: Value,
    pub(super) native_link_eligible: Value,
    pub(super) ret_ptr: Value,
    /// Caller's bp, saved BEFORE the prepare callback (which changes ctx.jit_bp via push_frame).
    pub(super) caller_bp: Value,
    /// Caller's fiber_sp, saved BEFORE the prepare callback.
    pub(super) old_fiber_sp: Value,
    pub(super) arg_start: usize,
    pub(super) ret_slots: usize,
    pub(super) ret_slot: StackSlot,
    pub(super) resume_pc_val: Value,
    pub(super) ret_reg_val: Value,
    pub(super) ret_slots_val: Value,
    /// If Some, jump to this block on OK instead of creating a new merge block.
    /// The block will be switched to and sealed by this function.
    pub(super) merge_block: Option<Block>,
}

/// Emit the common dispatch logic after a prepare callback returns PreparedCall.
///
/// Handles: null check -> VM call materialization trampoline or JIT direct call
///          -> non-OK (push_resume_point + propagate) or OK (pop_frame) -> merge.
///
/// If merge_block is Some, OK path jumps there (caller owns the merge block and ret copy).
/// If merge_block is None, creates its own merge block and copies return values.
pub(super) fn emit_prepared_call<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    p: PreparedCallParams,
) -> Result<(), crate::JitError> {
    let ctx = emitter.ctx_param();

    // caller_bp was saved BEFORE the prepare callback (which updates ctx.jit_bp via push_frame).
    let caller_bp = p.caller_bp;
    let caller_func_id = load_current_func_id(emitter);

    // Load pop_frame resources before branching (needed in both trampoline and JIT-OK paths)
    let pop_frame_fn_ptr = emitter.load_context_field(types::I64, JitContextField::PopFrameFn);

    // Check if jit_func_ptr is null (needs the VM call materialization trampoline).
    let null_ptr = emitter.builder().ins().iconst(types::I64, 0);
    let is_null = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, p.jit_func_ptr, null_ptr);
    let depth = emitter.load_context_field(types::I32, JitContextField::CallDepth);
    let depth_limit = emitter.load_context_field(types::I32, JitContextField::CallDepthLimit);
    let depth_exhausted =
        emitter
            .builder()
            .ins()
            .icmp(IntCC::UnsignedGreaterThanOrEqual, depth, depth_limit);
    let link_or_call_block = crate::compile_common::cold_block(emitter.builder());
    let link_block = crate::compile_common::cold_block(emitter.builder());
    let trampoline_block = crate::compile_common::cold_block(emitter.builder());
    let jit_call_block = emitter.builder().create_block();
    emitter
        .builder()
        .append_block_param(jit_call_block, types::I64);
    let merge_block = match p.merge_block {
        Some(block) => block,
        None => emitter.builder().create_block(),
    };

    emitter.builder().ins().brif(
        depth_exhausted,
        trampoline_block,
        &[],
        link_or_call_block,
        &[],
    );

    emitter.builder().switch_to_block(link_or_call_block);
    emitter.builder().seal_block(link_or_call_block);
    emitter.builder().ins().brif(
        is_null,
        link_block,
        &[],
        jit_call_block,
        &[p.jit_func_ptr.into()],
    );

    emitter.builder().switch_to_block(link_block);
    emitter.builder().seal_block(link_block);
    let may_link = emitter
        .builder()
        .ins()
        .icmp_imm_u(IntCC::NotEqual, p.native_link_eligible, 0);
    let perform_link_block = crate::compile_common::cold_block(emitter.builder());
    emitter
        .builder()
        .ins()
        .brif(may_link, perform_link_block, &[], trampoline_block, &[]);

    emitter.builder().switch_to_block(perform_link_block);
    emitter.builder().seal_block(perform_link_block);
    let linked_ptr = emit_native_link(emitter, p.func_id)?;
    let linked = emitter
        .builder()
        .ins()
        .icmp_imm_u(IntCC::NotEqual, linked_ptr, 0);
    emitter.builder().ins().brif(
        linked,
        jit_call_block,
        &[linked_ptr.into()],
        trampoline_block,
        &[],
    );

    // === Trampoline path: return JitResult::Call with CALL_KIND_PREPARED ===
    // prepare callback already did push_frame + arg layout on fiber.stack,
    // but it leaves caller frame pc uncommitted until VM materialization accepts
    // the prepared transition.
    //
    // We save callee_bp in call_resume_pc field because ctx.jit_bp may be
    // overwritten by intermediate JIT non-OK blocks (their push_frame calls).
    // PREPARED handler reads callee_bp from call_resume_pc, not ctx.jit_bp.
    emitter.builder().switch_to_block(trampoline_block);
    emitter.builder().seal_block(trampoline_block);

    // Save callee_bp (set by prepare's push_frame) into call_resume_pc
    let callee_bp_val = emitter.load_context_field(types::I32, JitContextField::JitBp);

    // Restore caller ctx before refreshing locals; prepare may have grown fiber.stack.
    restore_caller_execution_context(emitter, caller_bp, p.old_fiber_sp, caller_func_id);
    emitter.refresh_stack_base_after_reallocation();

    // Spill SSA-only vars so VM can read caller state after callee returns.
    emitter.spill_all_vars();

    let set_call_request_func = emitter.helper(HelperKind::set_call_request);
    let prepared_kind = emitter
        .builder()
        .ins()
        .iconst(types::I32, JitContext::CALL_KIND_PREPARED as i64);
    // For PREPARED: arg_start field stores caller_resume_pc (not arg copying position).
    // PREPARED doesn't need arg_start: args are already copied by the prepare callback.
    // The PREPARED handler commits this resume pc only after callee resolution
    // and materialization preflight succeed.
    crate::translator::emit_funcref_call_raw(
        emitter,
        set_call_request_func.func_ref(),
        &[
            ctx,
            p.func_id,
            p.resume_pc_val,
            callee_bp_val,
            p.ret_slots_val,
            p.ret_reg_val,
            prepared_kind,
        ],
    );

    let call_result = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_CALL as i64);
    emitter.builder().ins().return_(&[call_result]);

    // === JIT call path (fast): direct call + result check ===
    emitter.builder().switch_to_block(jit_call_block);
    emitter.builder().seal_block(jit_call_block);
    let jit_func_ptr = emitter.builder().block_params(jit_call_block)[0];

    let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
    let jit_func_sig = import_jit_func_sig(emitter);
    let arg_lanes = load_native_arg_lanes_dynamic(emitter, p.callee_args_ptr, p.callee_local_slots);
    let jit_result = emit_effect_aware_jit_call(
        emitter,
        jit_func_sig,
        jit_func_ptr,
        JitCallOperands {
            ctx,
            args_ptr: p.callee_args_ptr,
            ret_ptr: p.ret_ptr,
            arg_lanes: &arg_lanes,
        },
        JitCallGcMode::Dynamic(p.jit_may_gc),
    );
    emit_call_depth_leave(emitter, old_call_depth);

    let ok_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, jit_result, ok_val);

    let jit_non_ok_block = crate::compile_common::cold_block(emitter.builder());
    let jit_ok_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(is_ok, jit_ok_block, &[], jit_non_ok_block, &[]);

    // Non-OK: restore ctx, spill caller vars, push resume point, propagate.
    //
    // The prepare callback already did push_frame, so the callee frame exists
    // on fiber.stack and fiber.sp/ctx are at callee's state.
    // We must NOT call push_frame again (emit_non_ok_slow_path would double-push).
    emitter.builder().switch_to_block(jit_non_ok_block);
    emitter.builder().seal_block(jit_non_ok_block);

    // Save callee_bp from ctx.jit_bp (still points to callee's frame).
    let callee_bp = emitter.load_context_field(types::I32, JitContextField::JitBp);

    // Restore ctx.jit_bp and ctx.fiber_sp to caller's values.
    restore_caller_execution_context(emitter, caller_bp, p.old_fiber_sp, caller_func_id);
    emitter.refresh_stack_base_after_reallocation();

    emitter.spill_all_vars();

    // Push resume point with CALLEE's func_id.
    // materialize_jit_frames creates CallFrame(rp.func_id, rp.bp); both must be callee's.
    let push_resume_point_fn_ptr =
        emitter.load_context_field(types::I64, JitContextField::PushResumePointFn);
    emit_checked_jit_result_indirect_callback_call(
        emitter,
        PREPARED_CALL_PUSH_RESUME_POINT_CALLSITE,
        push_resume_point_fn_ptr,
        &[
            ctx,
            p.func_id,
            p.resume_pc_val,
            callee_bp,
            caller_bp,
            p.ret_reg_val,
            p.ret_slots_val,
        ],
        true,
    )?;
    emitter.builder().ins().return_(&[jit_result]);

    // OK: pop_frame, jump to merge.
    emitter.builder().switch_to_block(jit_ok_block);
    emitter.builder().seal_block(jit_ok_block);
    emit_raw_jit_context_callback_call(
        emitter,
        PREPARED_CALL_POP_FRAME_CALLSITE,
        pop_frame_fn_ptr,
        &[ctx, caller_bp],
    )?;
    restore_caller_execution_context(emitter, caller_bp, p.old_fiber_sp, caller_func_id);
    emitter.builder().ins().jump(merge_block, &[]);

    // === Merge block ===
    emitter.builder().switch_to_block(merge_block);
    emitter.builder().seal_block(merge_block);

    // If caller owns merge_block, they handle ret value copy.
    if p.merge_block.is_none() {
        for i in 0..p.ret_slots {
            let val = emitter.builder().ins().stack_load(
                types::I64,
                types::I64,
                p.ret_slot,
                (i * 8) as i32,
            );
            emitter.write_var((p.arg_start + i) as u16, val);
        }
    }
    Ok(())
}
