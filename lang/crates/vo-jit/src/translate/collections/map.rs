use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, StackSlot, StackSlotData, StackSlotKind, Value};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitRuntimeTrapKind;

use crate::translator::{
    emit_funcref_call, emit_funcref_call_with_effect, CollectionEmitter, HelperCallEffect,
};
use crate::JitError;

use super::emit_return_if_u64_jit_error;
use crate::translate::{emit_runtime_trap_if, require_helper};

pub(in crate::translate) fn map_new<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_new, "map_new")?;
    let gc_ptr = e.gc_ptr();
    let packed_meta = e.read_var(inst.b);
    let key_rttid = e.read_var(inst.b + 1);
    let key_meta = e.builder().ins().ushr_imm(packed_meta, 32);
    let key_meta_i32 = e.builder().ins().ireduce(types::I32, key_meta);
    let val_meta_i32 = e.builder().ins().ireduce(types::I32, packed_meta);
    let key_slots = e
        .builder()
        .ins()
        .iconst(types::I32, inst.map_new_key_slots() as i64);
    let val_slots = e
        .builder()
        .ins()
        .iconst(types::I32, inst.map_new_val_slots() as i64);
    let key_rttid_i32 = e.builder().ins().ireduce(types::I32, key_rttid);
    let call = emit_funcref_call(
        e,
        func,
        &[
            gc_ptr,
            key_meta_i32,
            val_meta_i32,
            key_slots,
            val_slots,
            key_rttid_i32,
        ],
    );
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn map_len<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_len, "map_len")?;
    let m = e.read_var(inst.b);
    let call = emit_funcref_call_with_effect(e, func, &[m], HelperCallEffect::FrameIndependent);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
    Ok(())
}

fn store_to_stack<'a>(
    e: &mut impl CollectionEmitter<'a>,
    start_reg: u16,
    slots: usize,
) -> (StackSlot, Value, Value) {
    let stack_slot = e.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (slots.max(1) * 8) as u32,
        8,
    ));
    for i in 0..slots {
        let val = e.read_var(start_reg + i as u16);
        e.builder()
            .ins()
            .stack_store(val, stack_slot, (i * 8) as i32);
    }
    let ptr = e.builder().ins().stack_addr(types::I64, stack_slot, 0);
    let slots_i32 = e.builder().ins().iconst(types::I32, slots as i64);
    (stack_slot, ptr, slots_i32)
}

pub(in crate::translate) fn map_get<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_get, "map_get")?;
    let layout = e.map_get_layout(inst).ok_or(JitError::MissingJitLayout {
        pc: e.current_pc(),
        opcode: inst.opcode(),
        layout: "MapGet",
    })?;
    let key_slots = layout.key_slots as usize;
    let val_slots = layout.val_slots as usize;
    let has_ok = layout.has_ok;

    let m = e.read_var(inst.b);
    let (_, key_ptr, key_slots_i32) = store_to_stack(e, inst.c + 1, key_slots);

    let out_slots = val_slots + if has_ok { 1 } else { 0 };
    let val_slot = e.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (out_slots.max(1) * 8) as u32,
        8,
    ));
    let val_ptr = e.builder().ins().stack_addr(types::I64, val_slot, 0);
    let val_slots_i32 = e.builder().ins().iconst(types::I32, val_slots as i64);

    let ctx = e.ctx_param();
    let call = emit_funcref_call(
        e,
        func,
        &[ctx, m, key_ptr, key_slots_i32, val_ptr, val_slots_i32],
    );
    let found = e.builder().inst_results(call)[0];
    emit_return_if_u64_jit_error(e, found);

    for i in 0..val_slots {
        let val = e
            .builder()
            .ins()
            .stack_load(types::I64, val_slot, (i * 8) as i32);
        e.write_var(inst.a + i as u16, val);
    }
    if has_ok {
        e.write_var(inst.a + val_slots as u16, found);
    }
    Ok(())
}

pub(in crate::translate) fn map_set<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_set, "map_set")?;
    let layout = e.map_set_layout(inst).ok_or(JitError::MissingJitLayout {
        pc: e.current_pc(),
        opcode: inst.opcode(),
        layout: "MapSet",
    })?;
    let key_slots = layout.key_slots as usize;
    let val_slots = layout.val_slots as usize;

    let m = e.read_var(inst.a);

    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_nil = e.builder().ins().icmp(IntCC::Equal, m, zero);
    emit_runtime_trap_if(e, is_nil, JitRuntimeTrapKind::NilMapWrite, None, None);

    let (_, key_ptr, key_slots_i32) = store_to_stack(e, inst.b + 1, key_slots);
    let (_, val_ptr, val_slots_i32) = store_to_stack(e, inst.c, val_slots);

    let ctx = e.ctx_param();
    let call = emit_funcref_call(
        e,
        func,
        &[ctx, m, key_ptr, key_slots_i32, val_ptr, val_slots_i32],
    );
    let result = e.builder().inst_results(call)[0];
    emit_return_if_u64_jit_error(e, result);

    let is_panic = e.builder().ins().icmp(IntCC::NotEqual, result, zero);
    emit_runtime_trap_if(e, is_panic, JitRuntimeTrapKind::UnhashableType, None, None);
    Ok(())
}

pub(in crate::translate) fn map_delete<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_delete, "map_delete")?;
    let key_slots = e
        .map_delete_key_slots(inst)
        .ok_or(JitError::MissingJitLayout {
            pc: e.current_pc(),
            opcode: inst.opcode(),
            layout: "MapDelete",
        })? as usize;

    let m = e.read_var(inst.a);
    let (_, key_ptr, key_slots_i32) = store_to_stack(e, inst.b + 1, key_slots);

    let ctx = e.ctx_param();
    let call = emit_funcref_call(e, func, &[ctx, m, key_ptr, key_slots_i32]);
    let result = e.builder().inst_results(call)[0];
    emit_return_if_u64_jit_error(e, result);
    Ok(())
}

const MAP_ITER_SLOTS: usize = vo_runtime::objects::map::MAP_ITER_SLOTS;
const MAP_ITER_BYTES: u32 = (MAP_ITER_SLOTS * 8) as u32;

pub(in crate::translate) fn map_iter_init<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_iter_init, "map_iter_init")?;
    let m = e.read_var(inst.b);
    let iter_slot = e.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        MAP_ITER_BYTES,
        8,
    ));
    let iter_ptr = e.builder().ins().stack_addr(types::I64, iter_slot, 0);
    emit_funcref_call(e, func, &[m, iter_ptr]);
    for i in 0..MAP_ITER_SLOTS {
        let val = e
            .builder()
            .ins()
            .stack_load(types::I64, iter_slot, (i * 8) as i32);
        e.write_var(inst.a + i as u16, val);
    }
    Ok(())
}

pub(in crate::translate) fn map_iter_next<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_iter_next, "map_iter_next")?;
    let key_slots = inst.map_iter_key_slots() as usize;
    let val_slots = inst.map_iter_val_slots() as usize;

    let iter_slot = e.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        MAP_ITER_BYTES,
        8,
    ));
    let key_slot = e.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (key_slots.max(1) * 8) as u32,
        8,
    ));
    let val_slot = e.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (val_slots.max(1) * 8) as u32,
        8,
    ));

    for i in 0..MAP_ITER_SLOTS {
        let val = e.read_var(inst.b + i as u16);
        e.builder()
            .ins()
            .stack_store(val, iter_slot, (i * 8) as i32);
    }

    let iter_ptr = e.builder().ins().stack_addr(types::I64, iter_slot, 0);
    let key_ptr = e.builder().ins().stack_addr(types::I64, key_slot, 0);
    let val_ptr = e.builder().ins().stack_addr(types::I64, val_slot, 0);
    let key_slots_i32 = e.builder().ins().iconst(types::I32, key_slots as i64);
    let val_slots_i32 = e.builder().ins().iconst(types::I32, val_slots as i64);
    let ctx = e.ctx_param();

    let call = emit_funcref_call(
        e,
        func,
        &[
            ctx,
            iter_ptr,
            key_ptr,
            key_slots_i32,
            val_ptr,
            val_slots_i32,
        ],
    );
    let ok = e.builder().inst_results(call)[0];
    emit_return_if_u64_jit_error(e, ok);

    for i in 0..MAP_ITER_SLOTS {
        let val = e
            .builder()
            .ins()
            .stack_load(types::I64, iter_slot, (i * 8) as i32);
        e.write_var(inst.b + i as u16, val);
    }

    for i in 0..key_slots {
        let val = e
            .builder()
            .ins()
            .stack_load(types::I64, key_slot, (i * 8) as i32);
        e.write_var(inst.a + i as u16, val);
    }
    for i in 0..val_slots {
        let val = e
            .builder()
            .ins()
            .stack_load(types::I64, val_slot, (i * 8) as i32);
        e.write_var(inst.a + key_slots as u16 + i as u16, val);
    }
    e.write_var(inst.c, ok);
    Ok(())
}
