use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlags, StackSlotData, StackSlotKind};

use vo_runtime::jit_api::JitContext;

use crate::translator::IrEmitter;

use super::{
    emit_call_depth_enter, emit_call_depth_leave, emit_non_ok_slow_path, emit_stack_capacity_check,
    import_jit_func_sig, load_current_func_id, restore_caller_execution_context, CallViaVmConfig,
    JitCallWithVmMaterializationConfig, NonOkSlowPathParams, JIT_RESULT_CALL, JIT_RESULT_OK,
};

/// Emit a call by materializing a VM-owned call request.
///
/// This is used when the callee needs a real CallFrame in `fiber.frames`, or
/// when the generated code intentionally returns control to the scheduler.
pub fn emit_call_via_vm<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    config: CallViaVmConfig,
) -> Result<(), crate::JitError> {
    let set_call_request_func =
        crate::translate::require_helper(emitter.helpers().set_call_request, "set_call_request")?;

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
        set_call_request_func,
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
/// - Args passed via native stack slot (no fiber.stack access)
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
    config: JitCallWithVmMaterializationConfig,
) -> Result<(), crate::JitError> {
    let ctx = emitter.ctx_param();

    let caller_bp = emitter.call_caller_bp();
    let old_fiber_sp = emitter.call_old_fiber_sp();
    let caller_func_id = load_current_func_id(emitter, ctx);

    let mut arg_values = Vec::with_capacity(config.arg_slots);
    for i in 0..config.arg_slots {
        arg_values.push(emitter.read_var((config.arg_start + i) as u16));
    }

    let args_slot = emitter
        .builder()
        .create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (config.callee_local_slots.max(1) * 8) as u32,
            8,
        ));
    let args_ptr = emitter.builder().ins().stack_addr(types::I64, args_slot, 0);

    for (i, val) in arg_values.iter().enumerate() {
        emitter
            .builder()
            .ins()
            .stack_store(*val, args_slot, (i * 8) as i32);
    }

    let ret_slot = emitter
        .builder()
        .create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (config.func_ret_slots.max(1) * 8) as u32,
            8,
        ));
    let ret_ptr = emitter.builder().ins().stack_addr(types::I64, ret_slot, 0);

    let func_id_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.func_id as i64);
    let local_slots_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.callee_local_slots as i64);
    let ret_reg_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.ret_reg as i64);
    let ret_slots_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.call_ret_slots as i64);
    let current_pc = emitter.current_pc();
    let caller_resume_pc_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, (current_pc + 1) as i64);

    let new_bp = old_fiber_sp;
    let new_sp = emitter
        .builder()
        .ins()
        .iadd_imm(new_bp, config.callee_local_slots as i64);
    let (capacity_materialize_block, capacity_ok_block) =
        emit_stack_capacity_check(emitter, ctx, new_sp)?;
    emitter
        .builder()
        .switch_to_block(capacity_materialize_block);
    emitter.builder().seal_block(capacity_materialize_block);
    emit_call_via_vm(
        emitter,
        CallViaVmConfig {
            func_id: config.func_id,
            arg_start: config.arg_start,
            ret_reg: config.ret_reg,
            resume_pc: current_pc + 1,
            ret_slots: config.call_ret_slots,
        },
    )?;

    emitter.builder().switch_to_block(capacity_ok_block);
    emitter.builder().seal_block(capacity_ok_block);
    emitter
        .builder()
        .ins()
        .store(MemFlags::trusted(), new_bp, ctx, JitContext::OFFSET_JIT_BP);
    emitter.builder().ins().store(
        MemFlags::trusted(),
        new_sp,
        ctx,
        JitContext::OFFSET_FIBER_SP,
    );

    let merge_block = emitter.builder().create_block();

    let jit_result = if let Some(func_ref) = config.callee_func_ref {
        let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
        let call =
            crate::translator::emit_funcref_call_raw(emitter, func_ref, &[ctx, args_ptr, ret_ptr]);
        let result = emitter.builder().inst_results(call)[0];
        emit_call_depth_leave(emitter, ctx, old_call_depth);
        result
    } else {
        let jit_func_table = emitter.builder().ins().load(
            types::I64,
            MemFlags::trusted(),
            ctx,
            JitContext::OFFSET_JIT_FUNC_TABLE,
        );
        let func_id_i64 = emitter
            .builder()
            .ins()
            .iconst(types::I64, config.func_id as i64);
        let offset = emitter.builder().ins().imul_imm(func_id_i64, 8);
        let func_ptr_addr = emitter.builder().ins().iadd(jit_func_table, offset);
        let jit_func_ptr =
            emitter
                .builder()
                .ins()
                .load(types::I64, MemFlags::trusted(), func_ptr_addr, 0);

        let zero = emitter.builder().ins().iconst(types::I64, 0);
        let is_null = emitter
            .builder()
            .ins()
            .icmp(IntCC::Equal, jit_func_ptr, zero);

        let jit_call_block = emitter.builder().create_block();
        let vm_call_block = emitter.builder().create_block();

        emitter
            .builder()
            .ins()
            .brif(is_null, vm_call_block, &[], jit_call_block, &[]);

        emitter.builder().switch_to_block(jit_call_block);
        emitter.builder().seal_block(jit_call_block);

        let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
        let sig = import_jit_func_sig(emitter);
        let jit_call =
            emitter
                .builder()
                .ins()
                .call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
        let jit_result_indirect = emitter.builder().inst_results(jit_call)[0];
        emit_call_depth_leave(emitter, ctx, old_call_depth);

        let ok_val = emitter
            .builder()
            .ins()
            .iconst(types::I32, JIT_RESULT_OK as i64);
        let is_ok = emitter
            .builder()
            .ins()
            .icmp(IntCC::Equal, jit_result_indirect, ok_val);

        let jit_non_ok_block = emitter.builder().create_block();
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
                copy_args: None,
            },
        )?;

        emitter.builder().switch_to_block(jit_ok_block);
        emitter.builder().seal_block(jit_ok_block);
        restore_caller_execution_context(emitter, ctx, caller_bp, old_fiber_sp, caller_func_id);
        emitter.refresh_stack_base_after_reallocation();
        emitter.builder().ins().jump(merge_block, &[]);

        emitter.builder().switch_to_block(vm_call_block);
        emitter.builder().seal_block(vm_call_block);

        restore_caller_execution_context(emitter, ctx, caller_bp, old_fiber_sp, caller_func_id);

        emitter.spill_all_vars();

        let set_call_request_func = crate::translate::require_helper(
            emitter.helpers().set_call_request,
            "set_call_request",
        )?;
        let arg_start_val = emitter
            .builder()
            .ins()
            .iconst(types::I32, config.arg_start as i64);
        let call_kind_val = emitter
            .builder()
            .ins()
            .iconst(types::I32, JitContext::CALL_KIND_REGULAR as i64);
        crate::translator::emit_funcref_call_raw(
            emitter,
            set_call_request_func,
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

        for i in 0..config.call_ret_slots {
            let val = emitter
                .builder()
                .ins()
                .stack_load(types::I64, ret_slot, (i * 8) as i32);
            emitter.write_var((config.ret_reg + i) as u16, val);
        }
        return Ok(());
    };

    let ok_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, jit_result, ok_val);

    let jit_non_ok_block = emitter.builder().create_block();
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
            jit_result,
            ctx,
            caller_bp,
            old_fiber_sp,
            caller_func_id,
            callee_func_id_val: func_id_val,
            local_slots_val,
            ret_reg_val,
            ret_slots_val,
            caller_resume_pc_val,
            copy_args: None,
        },
    )?;

    emitter.builder().switch_to_block(jit_ok_block);
    emitter.builder().seal_block(jit_ok_block);
    restore_caller_execution_context(emitter, ctx, caller_bp, old_fiber_sp, caller_func_id);
    emitter.refresh_stack_base_after_reallocation();

    for i in 0..config.call_ret_slots {
        let val = emitter
            .builder()
            .ins()
            .stack_load(types::I64, ret_slot, (i * 8) as i32);
        emitter.write_var((config.ret_reg + i) as u16, val);
    }
    Ok(())
}

#[cfg(test)]
mod tests;
