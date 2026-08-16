use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlagsData as MemFlags};

use vo_runtime::jit_api::{JitContext, JitContextField};

use crate::translator::{HelperKind, IrEmitter, NativeScratchKind};

use super::{
    emit_call_depth_enter, emit_call_depth_leave, emit_effect_aware_direct_jit_call,
    emit_effect_aware_jit_call, emit_non_ok_slow_path, emit_stack_capacity_check,
    import_jit_func_sig, restore_caller_execution_context, CallPlan, CallViaVmConfig,
    JitCallGcMode, JitCallOperands, NonOkSlowPathParams, JIT_RESULT_CALL, JIT_RESULT_OK,
};

/// Emit a call by materializing a VM-owned call request.
///
/// This is used when the callee needs a real CallFrame in `fiber.frames`, or
/// when the generated code intentionally returns control to the scheduler.
pub fn emit_call_via_vm<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    config: CallViaVmConfig,
) -> Result<(), crate::JitError> {
    let set_call_request_func = emitter.helper(HelperKind::set_call_request);

    // Spill all variables to fiber.stack before returning Call.
    emitter.spill_all_vars();

    let ctx = emitter.ctx_param();
    let func_id_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.func_id as i64);
    let arg_start_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.arg_start as i64);
    let resume_pc_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.resume_pc as i64);
    let ret_slots_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.ret_slots as i64);
    let ret_reg_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.ret_reg as i64);
    let call_kind_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, JitContext::CALL_KIND_REGULAR as i64);

    crate::translator::emit_funcref_call_raw(
        emitter,
        set_call_request_func.func_ref(),
        &[
            ctx,
            func_id_val,
            arg_start_val,
            resume_pc_val,
            ret_slots_val,
            ret_reg_val,
            call_kind_val,
        ],
    );

    let call_result = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_CALL as i64);
    emitter.builder().ins().return_(&[call_result]);
    Ok(())
}

/// Emit a JIT-to-JIT call with runtime check for compiled callee.
///
/// Fast path (JIT-to-JIT):
/// - Args passed through the capacity-checked fiber shadow window
/// - No push_frame/pop_frame calls
///
/// VM materialization path:
/// - Spill vars and materialize a callee call request in JitContext
/// - Return JitResult::Call so the VM scheduler owns frame setup and dispatch
///
/// If jit_func_table[func_id] != null: direct JIT call.
/// If jit_func_table[func_id] == null: materialize a VM call via set_call_request + return Call.
pub fn emit_jit_call_with_vm_materialization<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    plan: CallPlan,
    direct_native: Option<cranelift_codegen::ir::FuncRef>,
    recursive_edge: bool,
) -> Result<(), crate::JitError> {
    let ctx = emitter.ctx_param();

    let caller_bp = emitter.call_caller_bp();
    let old_fiber_sp = emitter.call_old_fiber_sp();
    let caller_func_id = emitter.call_caller_func_id();

    let mut arg_values = Vec::with_capacity(plan.arg_slots);
    for i in 0..plan.arg_slots {
        arg_values.push(emitter.read_var((plan.arg_start + i) as u16));
    }
    let ret_slot = emitter.native_scratch_slot(
        NativeScratchKind::StaticReturns,
        plan.call_ret_slots.max(1) * 8,
    );
    let ret_ptr = emitter.builder().ins().stack_addr(types::I64, ret_slot, 0);

    let func_id_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, plan.func_id as i64);
    let local_slots_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, plan.callee_local_slots as i64);
    let ret_reg_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, plan.ret_reg as i64);
    let ret_slots_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, plan.call_ret_slots as i64);
    let current_pc = emitter.current_pc();
    let caller_resume_pc_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, (current_pc + 1) as i64);

    let new_bp = old_fiber_sp;
    let new_sp = emitter
        .builder()
        .ins()
        .iadd_imm_u(new_bp, plan.callee_local_slots as i64);
    let (capacity_materialize_block, capacity_ok_block) =
        emit_stack_capacity_check(emitter, ctx, new_sp)?;
    emitter
        .builder()
        .switch_to_block(capacity_materialize_block);
    emitter.builder().seal_block(capacity_materialize_block);
    emit_call_via_vm(
        emitter,
        CallViaVmConfig {
            func_id: plan.func_id,
            arg_start: plan.arg_start,
            ret_reg: plan.ret_reg,
            resume_pc: current_pc + 1,
            ret_slots: plan.call_ret_slots,
        },
    )?;

    emitter.builder().switch_to_block(capacity_ok_block);
    emitter.builder().seal_block(capacity_ok_block);
    let eager_context_transition = !plan.eligibility.frame_elided;
    if eager_context_transition {
        emitter.store_context_field(new_bp, JitContextField::JitBp);
        emitter.store_context_field(new_sp, JitContextField::FiberSp);
    }
    let stack_ptr = emitter.load_context_field(types::I64, JitContextField::StackPtr);
    let bp_offset = emitter.builder().ins().uextend(types::I64, new_bp);
    let bp_offset = emitter.builder().ins().imul_imm_u(bp_offset, 8);
    let args_ptr = emitter.builder().ins().iadd(stack_ptr, bp_offset);
    for (i, val) in arg_values.iter().enumerate() {
        emitter
            .builder()
            .ins()
            .store(MemFlags::trusted(), *val, args_ptr, (i * 8) as i32);
    }

    let merge_block = emitter.builder().create_block();

    let jit_call_block = emitter.builder().create_block();
    emitter
        .builder()
        .append_block_param(jit_call_block, types::I64);
    let link_block = crate::compile_common::cold_block(emitter.builder());
    let vm_call_block = crate::compile_common::cold_block(emitter.builder());
    if direct_native.is_some() {
        let unused = emitter.builder().ins().iconst(types::I64, 0);
        emitter
            .builder()
            .ins()
            .jump(jit_call_block, &[unused.into()]);
    } else {
        let jit_func_table = emitter.load_context_field(types::I64, JitContextField::JitFuncTable);
        let func_id_i64 = emitter
            .builder()
            .ins()
            .iconst(types::I64, plan.func_id as i64);
        let offset = emitter.builder().ins().imul_imm_u(
            func_id_i64,
            vo_runtime::jit_api::JitDispatchEntry::SIZE as i64,
        );
        let func_ptr_addr = emitter.builder().ins().iadd(jit_func_table, offset);
        let func_ptr = emitter.builder().ins().load(
            types::I64,
            MemFlags::trusted(),
            func_ptr_addr,
            vo_runtime::jit_api::JitDispatchEntry::OFFSET_NATIVE,
        );
        let zero = emitter.builder().ins().iconst(types::I64, 0);
        let is_null = emitter.builder().ins().icmp(IntCC::Equal, func_ptr, zero);
        emitter
            .builder()
            .ins()
            .brif(is_null, link_block, &[], jit_call_block, &[func_ptr.into()]);
    }

    emitter.builder().switch_to_block(link_block);
    emitter.builder().seal_block(link_block);
    let linked_ptr = super::emit_native_link(emitter, func_id_val)?;
    let zero = emitter.builder().ins().iconst(types::I64, 0);
    let unavailable = emitter.builder().ins().icmp(IntCC::Equal, linked_ptr, zero);
    emitter.builder().ins().brif(
        unavailable,
        vm_call_block,
        &[],
        jit_call_block,
        &[linked_ptr.into()],
    );

    emitter.builder().switch_to_block(jit_call_block);
    emitter.builder().seal_block(jit_call_block);
    let linked_func_ptr = emitter.builder().block_params(jit_call_block)[0];

    let old_call_depth = plan
        .requires_depth_guard(recursive_edge)
        .then(|| emit_call_depth_enter(emitter, ctx))
        .transpose()?;
    let gc_mode = if plan.eligibility.may_gc {
        JitCallGcMode::MayGc
    } else {
        JitCallGcMode::Never
    };
    let zero = emitter.builder().ins().iconst(types::I64, 0);
    let arg_lanes = std::array::from_fn(|lane| arg_values.get(lane).copied().unwrap_or(zero));
    let jit_result_indirect = if let Some(func_ref) = direct_native {
        emit_effect_aware_direct_jit_call(
            emitter, func_ref, ctx, args_ptr, ret_ptr, &arg_lanes, gc_mode,
        )
    } else {
        let sig = import_jit_func_sig(emitter);
        emit_effect_aware_jit_call(
            emitter,
            sig,
            linked_func_ptr,
            JitCallOperands {
                ctx,
                args_ptr,
                ret_ptr,
                arg_lanes: &arg_lanes,
            },
            gc_mode,
        )
    };
    if let Some(old_call_depth) = old_call_depth {
        emit_call_depth_leave(emitter, old_call_depth);
    }

    let ok_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, jit_result_indirect, ok_val);

    let jit_non_ok_block = crate::compile_common::cold_block(emitter.builder());
    let jit_ok_block = emitter.builder().create_block();

    emitter
        .builder()
        .ins()
        .brif(is_ok, jit_ok_block, &[], jit_non_ok_block, &[]);

    emitter.builder().switch_to_block(jit_non_ok_block);
    emitter.builder().seal_block(jit_non_ok_block);

    emit_non_ok_slow_path(
        emitter,
        NonOkSlowPathParams {
            jit_result: jit_result_indirect,
            ctx,
            caller_bp,
            old_fiber_sp,
            caller_func_id,
            callee_func_id_val: func_id_val,
            local_slots_val,
            ret_reg_val,
            ret_slots_val,
            caller_resume_pc_val,
        },
    )?;

    emitter.builder().switch_to_block(jit_ok_block);
    emitter.builder().seal_block(jit_ok_block);
    if eager_context_transition {
        restore_caller_execution_context(emitter, caller_bp, old_fiber_sp, caller_func_id);
    }
    emitter.refresh_stack_base_after_reallocation();
    emitter.builder().ins().jump(merge_block, &[]);

    emitter.builder().switch_to_block(vm_call_block);
    emitter.builder().seal_block(vm_call_block);

    if eager_context_transition {
        restore_caller_execution_context(emitter, caller_bp, old_fiber_sp, caller_func_id);
    }

    emitter.spill_all_vars();

    let set_call_request_func = emitter.helper(HelperKind::set_call_request);
    let arg_start_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, plan.arg_start as i64);
    let call_kind_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, JitContext::CALL_KIND_REGULAR as i64);
    crate::translator::emit_funcref_call_raw(
        emitter,
        set_call_request_func.func_ref(),
        &[
            ctx,
            func_id_val,
            arg_start_val,
            caller_resume_pc_val,
            ret_slots_val,
            ret_reg_val,
            call_kind_val,
        ],
    );

    let call_result = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_CALL as i64);
    emitter.builder().ins().return_(&[call_result]);

    emitter.builder().switch_to_block(merge_block);
    emitter.builder().seal_block(merge_block);

    for i in 0..plan.call_ret_slots {
        let val =
            emitter
                .builder()
                .ins()
                .stack_load(types::I64, types::I64, ret_slot, (i * 8) as i32);
        emitter.write_var((plan.ret_reg + i) as u16, val);
    }
    Ok(())
}
