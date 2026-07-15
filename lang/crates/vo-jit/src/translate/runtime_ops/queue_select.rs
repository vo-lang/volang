use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, FuncRef, InstBuilder, MemFlags, StackSlotData, StackSlotKind};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitRuntimeTrapKind;

use crate::call_helpers::emit_checked_jit_result_helper_call;
use crate::translate::{emit_runtime_trap_if, mark_runtime_trap_pc, require_helper};
use crate::translator::{emit_funcref_call, RuntimeOpsEmitter};
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
    let func = require_helper(e.helpers().queue_new_checked, "queue_new_checked")?;
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

    let call = emit_funcref_call(
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

    let result = e.builder().ins().stack_load(types::I64, out_slot, 0);
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn queue_len<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().queue_len, "queue_len")?;
    let ctx = e.ctx_param();
    let ch = e.read_var(inst.b);
    let out_slot =
        e.builder()
            .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 8));
    let out_ptr = e.builder().ins().stack_addr(types::I64, out_slot, 0);

    emit_checked_jit_result_helper_call(e, func, &[ctx, ch, out_ptr], true);
    let result = e.builder().ins().stack_load(types::I64, out_slot, 0);
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn queue_cap<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().queue_cap, "queue_cap")?;
    let ctx = e.ctx_param();
    let ch = e.read_var(inst.b);
    let out_slot =
        e.builder()
            .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 8));
    let out_ptr = e.builder().ins().stack_addr(types::I64, out_slot, 0);

    emit_checked_jit_result_helper_call(e, func, &[ctx, ch, out_ptr], true);
    let result = e.builder().ins().stack_load(types::I64, out_slot, 0);
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn queue_close<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let queue_close_func = require_helper(e.helpers().queue_close, "queue_close")?;
    let ctx = e.ctx_param();
    let obj = e.read_var(inst.a);

    mark_runtime_trap_pc(e);
    emit_checked_jit_result_helper_call(e, queue_close_func, &[ctx, obj], true);
    Ok(())
}

pub(in crate::translate) fn queue_send<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let queue_send_func = require_helper(e.helpers().queue_send, "queue_send")?;
    emit_queue_send(e, inst, queue_send_func)
}

pub(in crate::translate) fn queue_recv<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let queue_recv_func = require_helper(e.helpers().queue_recv, "queue_recv")?;
    emit_queue_recv(e, inst, queue_recv_func)
}

pub(in crate::translate) fn emit_queue_send<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
    send_func: FuncRef,
) -> Result<(), JitError> {
    use vo_runtime::jit_api::JitContext;

    let resume_pc = (e.current_pc() + 1) as i32;
    let ctx = e.ctx_param();
    let resume_pc_val = e.builder().ins().iconst(types::I32, resume_pc as i64);
    e.builder().ins().store(
        MemFlags::trusted(),
        resume_pc_val,
        ctx,
        JitContext::OFFSET_CALL_RESUME_PC,
    );
    mark_runtime_trap_pc(e);

    let queue = e.read_var(inst.a);
    let val_slots = u32::from(queue_elem_slots(e, inst)?);
    let val_ptr = e.var_addr(inst.b);
    let val_slots_val = e.builder().ins().iconst(types::I32, val_slots as i64);

    emit_checked_jit_result_helper_call(e, send_func, &[ctx, queue, val_ptr, val_slots_val], true);
    Ok(())
}

pub(in crate::translate) fn emit_queue_recv<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
    recv_func: FuncRef,
) -> Result<(), JitError> {
    use vo_runtime::jit_api::JitContext;

    let resume_pc = e.current_pc() as i32;
    let ctx = e.ctx_param();
    let resume_pc_val = e.builder().ins().iconst(types::I32, resume_pc as i64);
    e.builder().ins().store(
        MemFlags::trusted(),
        resume_pc_val,
        ctx,
        JitContext::OFFSET_CALL_RESUME_PC,
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
        true,
    );
    e.sync_written_slots(inst.a, written_slots as u16)?;
    Ok(())
}

pub(in crate::translate) fn select_begin<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().select_begin, "select_begin")?;
    let ctx = e.ctx_param();
    let case_count = e.builder().ins().iconst(types::I32, inst.a as i64);
    let has_default = e
        .builder()
        .ins()
        .iconst(types::I32, (inst.flags & 1) as i64);
    e.begin_select_tracking();
    emit_checked_jit_result_helper_call(e, func, &[ctx, case_count, has_default], true);
    Ok(())
}

pub(in crate::translate) fn select_send<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().select_send, "select_send")?;
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
    emit_checked_jit_result_helper_call(
        e,
        func,
        &[ctx, queue_reg, val_reg, elem_slots, case_idx],
        true,
    );
    Ok(())
}

pub(in crate::translate) fn select_recv<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().select_recv, "select_recv")?;
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
        true,
    );
    Ok(())
}

pub(in crate::translate) fn select_exec<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    use vo_runtime::jit_api::JitContext;

    let func = require_helper(e.helpers().select_exec, "select_exec")?;
    let ctx = e.ctx_param();

    let resume_pc = e.current_pc() as i32;
    let resume_pc_val = e.builder().ins().iconst(types::I32, resume_pc as i64);
    e.builder().ins().store(
        MemFlags::trusted(),
        resume_pc_val,
        ctx,
        JitContext::OFFSET_CALL_RESUME_PC,
    );
    mark_runtime_trap_pc(e);

    let result_reg = e.builder().ins().iconst(types::I32, inst.a as i64);
    emit_checked_jit_result_helper_call(e, func, &[ctx, result_reg], true);

    e.refresh_stack_base_after_reallocation();
    e.sync_select_exec_state(inst.a)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn vm_queue_handle_validation_002_jit_queue_len_cap_use_checked_callbacks() {
        let src = vo_source_contract::production_source_without_test_modules(include_str!(
            "queue_select.rs"
        ));
        let len_body = src
            .split("pub(in crate::translate) fn queue_len")
            .nth(1)
            .expect("queue_len lowering should exist")
            .split("pub(in crate::translate) fn queue_cap")
            .next()
            .expect("queue_len should precede queue_cap");
        let cap_body = src
            .split("pub(in crate::translate) fn queue_cap")
            .nth(1)
            .expect("queue_cap lowering should exist")
            .split("pub(in crate::translate) fn queue_close")
            .next()
            .expect("queue_cap should precede queue_close");

        for (name, body) in [("QueueLen", len_body), ("QueueCap", cap_body)] {
            assert!(
                body.contains("emit_checked_jit_result_helper_call"),
                "{name} must use checked JitResult helper routing"
            );
            assert!(
                body.contains("ctx_param()"),
                "{name} must pass JitContext into the VM-owned queue callback wrapper"
            );
            assert!(
                !body.contains("FrameIndependent"),
                "{name} must not use the raw frame-independent runtime helper path"
            );
        }
    }

    #[test]
    fn jit_queue_select_lowering_uses_metadata_layout_061() {
        let src = vo_source_contract::production_source_without_test_modules(include_str!(
            "queue_select.rs"
        ));

        assert!(
            src.matches("queue_elem_slots(e, inst)?").count() >= 4,
            "QueueSend/QueueRecv/SelectSend/SelectRecv lowering must derive helper ABI width from QueueLayout metadata"
        );
        assert!(
            !src.contains("let val_slots = inst.flags as u32")
                && !src.contains("let elem_slots_u32 = inst.recv_elem_slots() as u32")
                && !src.contains("inst.flags as i64"),
            "Queue/Select lowering must not derive helper ABI width from encoded flags"
        );
    }
}
