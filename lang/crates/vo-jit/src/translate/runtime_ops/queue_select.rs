use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
    types, InstBuilder, MemFlagsData as MemFlags, StackSlotData, StackSlotKind,
};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitRuntimeTrapKind;

use crate::call_helpers::emit_checked_jit_result_helper_call;
use crate::helpers::RuntimeHelper;
use crate::translate::{emit_jit_error_if_zero, emit_runtime_trap_if, mark_runtime_trap_pc};
use crate::translator::{emit_runtime_helper_call, HelperKind, RuntimeOpsEmitter};
use crate::JitError;

fn queue_elem_slots<'a>(
    e: &impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<u16, JitError> {
    e.queue_elem_slots(inst).ok_or(JitError::MissingJitLayout {
        pc: e.current_pc(),
        opcode: inst.opcode(),
        layout: "QueueLayout",
    })
}

pub(in crate::translate) fn queue_new<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = e.helper(HelperKind::queue_new_checked);
    let ctx = e.ctx_param();
    let queue_kind = e
        .builder()
        .ins()
        .iconst(types::I32, if inst.queue_new_is_port() { 1 } else { 0 });
    let elem_type = e.read_var(inst.b);
    let elem_slots = queue_elem_slots(e, inst)?;
    let elem_slots_i32 = e.builder().ins().iconst(types::I32, i64::from(elem_slots));
    let cap = e.read_var(inst.c);

    let out_slot =
        e.builder()
            .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 8));
    let out_ptr = e.builder().ins().stack_addr(types::I64, out_slot, 0);

    let call = emit_runtime_helper_call(
        e,
        func,
        &[ctx, queue_kind, elem_type, elem_slots_i32, cap, out_ptr],
    );
    let error_code = e.builder().inst_results(call)[0];

    let zero = e.builder().ins().iconst(types::I32, 0);
    let has_error = e.builder().ins().icmp(IntCC::NotEqual, error_code, zero);
    let kind = if inst.queue_new_is_port() {
        JitRuntimeTrapKind::MakePort
    } else {
        JitRuntimeTrapKind::MakeChan
    };
    let error_arg = e.builder().ins().sextend(types::I64, error_code);
    emit_runtime_trap_if(e, has_error, kind, Some(error_arg), None);

    let result = e
        .builder()
        .ins()
        .stack_load(types::I64, types::I64, out_slot, 0);
    emit_jit_error_if_zero(e, result);
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn queue_len<'a>(e: &mut impl RuntimeOpsEmitter<'a>, inst: &Instruction) {
    let func = e.helper(HelperKind::queue_len);
    let ctx = e.ctx_param();
    let ch = e.read_var(inst.b);
    let out_slot =
        e.builder()
            .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 8));
    let out_ptr = e.builder().ins().stack_addr(types::I64, out_slot, 0);

    emit_checked_jit_result_helper_call(e, func, &[ctx, ch, out_ptr]);
    let result = e
        .builder()
        .ins()
        .stack_load(types::I64, types::I64, out_slot, 0);
    e.write_var(inst.a, result);
}

pub(in crate::translate) fn queue_cap<'a>(e: &mut impl RuntimeOpsEmitter<'a>, inst: &Instruction) {
    let func = e.helper(HelperKind::queue_cap);
    let ctx = e.ctx_param();
    let ch = e.read_var(inst.b);
    let out_slot =
        e.builder()
            .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 8));
    let out_ptr = e.builder().ins().stack_addr(types::I64, out_slot, 0);

    emit_checked_jit_result_helper_call(e, func, &[ctx, ch, out_ptr]);
    let result = e
        .builder()
        .ins()
        .stack_load(types::I64, types::I64, out_slot, 0);
    e.write_var(inst.a, result);
}

pub(in crate::translate) fn queue_close<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) {
    let queue_close_func = e.helper(HelperKind::queue_close);
    let ctx = e.ctx_param();
    let obj = e.read_var(inst.a);

    mark_runtime_trap_pc(e);
    emit_checked_jit_result_helper_call(e, queue_close_func, &[ctx, obj]);
}

pub(in crate::translate) fn queue_send<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let queue_send_func = e.helper(HelperKind::queue_send);
    emit_queue_send(e, inst, queue_send_func)
}

pub(in crate::translate) fn queue_recv<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let queue_recv_func = e.helper(HelperKind::queue_recv);
    emit_queue_recv(e, inst, queue_recv_func)
}

pub(in crate::translate) fn emit_queue_send<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
    send_func: RuntimeHelper,
) -> Result<(), JitError> {
    use vo_runtime::jit_api::JitContextField;

    let resume_pc = (e.current_pc() + 1) as i32;
    let ctx = e.ctx_param();
    let resume_pc_val = e.builder().ins().iconst(types::I32, resume_pc as i64);
    e.builder().ins().store(
        MemFlags::trusted(),
        resume_pc_val,
        ctx,
        JitContextField::CallResumePc.offset(),
    );
    mark_runtime_trap_pc(e);

    let queue = e.read_var(inst.a);
    let val_slots = u32::from(queue_elem_slots(e, inst)?);
    let val_ptr = e.var_addr(inst.b);
    let val_slots_val = e.builder().ins().iconst(types::I32, val_slots as i64);

    emit_checked_jit_result_helper_call(e, send_func, &[ctx, queue, val_ptr, val_slots_val]);
    Ok(())
}

pub(in crate::translate) fn emit_queue_recv<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
    recv_func: RuntimeHelper,
) -> Result<(), JitError> {
    use vo_runtime::jit_api::JitContextField;

    let resume_pc = e.current_pc() as i32;
    let ctx = e.ctx_param();
    let resume_pc_val = e.builder().ins().iconst(types::I32, resume_pc as i64);
    e.builder().ins().store(
        MemFlags::trusted(),
        resume_pc_val,
        ctx,
        JitContextField::CallResumePc.offset(),
    );
    mark_runtime_trap_pc(e);

    let queue = e.read_var(inst.b);
    let dst_ptr = e.var_addr(inst.a);
    let elem_slots = u32::from(queue_elem_slots(e, inst)?);
    let has_ok = u32::from(inst.recv_has_ok());
    let written_slots = elem_slots + has_ok;
    let elem_slots_val = e.builder().ins().iconst(types::I32, elem_slots as i64);
    let has_ok_val = e.builder().ins().iconst(types::I32, has_ok as i64);

    emit_checked_jit_result_helper_call(
        e,
        recv_func,
        &[ctx, queue, dst_ptr, elem_slots_val, has_ok_val],
    );
    e.sync_written_slots(inst.a, written_slots as u16)?;
    Ok(())
}

pub(in crate::translate) fn select_begin<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) {
    let func = e.helper(HelperKind::select_begin);
    let ctx = e.ctx_param();
    let case_count = e.builder().ins().iconst(types::I32, inst.a as i64);
    let has_default = e
        .builder()
        .ins()
        .iconst(types::I32, (inst.flags & 1) as i64);
    e.begin_select_tracking();
    emit_checked_jit_result_helper_call(e, func, &[ctx, case_count, has_default]);
}

pub(in crate::translate) fn select_send<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = e.helper(HelperKind::select_send);
    let ctx = e.ctx_param();
    let queue_reg = e.builder().ins().iconst(types::I32, inst.a as i64);
    let val_reg = e.builder().ins().iconst(types::I32, inst.b as i64);
    let elem_slot_count = queue_elem_slots(e, inst)?;
    let elem_slots = e
        .builder()
        .ins()
        .iconst(types::I32, i64::from(elem_slot_count));
    let case_idx = e.builder().ins().iconst(types::I32, inst.c as i64);
    e.record_select_send_case(inst.c);
    mark_runtime_trap_pc(e);
    emit_checked_jit_result_helper_call(e, func, &[ctx, queue_reg, val_reg, elem_slots, case_idx]);
    Ok(())
}

pub(in crate::translate) fn select_recv<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = e.helper(HelperKind::select_recv);
    let ctx = e.ctx_param();

    let dst_reg = e.builder().ins().iconst(types::I32, inst.a as i64);
    let queue_reg = e.builder().ins().iconst(types::I32, inst.b as i64);
    let elem_slot_count = queue_elem_slots(e, inst)?;
    let elem_slots_u32 = u32::from(elem_slot_count);
    let has_ok_u32 = u32::from(inst.recv_has_ok());
    let elem_slots = e.builder().ins().iconst(types::I32, elem_slots_u32 as i64);
    let has_ok = e.builder().ins().iconst(types::I32, has_ok_u32 as i64);
    let case_idx = e.builder().ins().iconst(types::I32, inst.c as i64);
    e.record_select_recv_case(inst.c, inst.a, elem_slot_count, has_ok_u32 != 0);
    mark_runtime_trap_pc(e);

    emit_checked_jit_result_helper_call(
        e,
        func,
        &[ctx, dst_reg, queue_reg, elem_slots, has_ok, case_idx],
    );
    Ok(())
}

pub(in crate::translate) fn select_exec<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    use vo_runtime::jit_api::JitContextField;

    let func = e.helper(HelperKind::select_exec);
    let ctx = e.ctx_param();

    let resume_pc = e.current_pc() as i32;
    let resume_pc_val = e.builder().ins().iconst(types::I32, resume_pc as i64);
    e.builder().ins().store(
        MemFlags::trusted(),
        resume_pc_val,
        ctx,
        JitContextField::CallResumePc.offset(),
    );
    mark_runtime_trap_pc(e);

    let result_reg = e.builder().ins().iconst(types::I32, inst.a as i64);
    emit_checked_jit_result_helper_call(e, func, &[ctx, result_reg]);

    e.refresh_stack_base_after_reallocation();
    e.sync_select_exec_state(inst.a)?;

    Ok(())
}
