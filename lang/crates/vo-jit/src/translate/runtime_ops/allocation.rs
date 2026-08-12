use cranelift_codegen::ir::{
    condcodes::IntCC, types, InstBuilder, MemFlagsData as MemFlags, StackSlotData, StackSlotKind,
};
use vo_runtime::bytecode::Constant;
use vo_runtime::gc::{JitGcPollField, JitSmallAllocLaneField, JIT_GC_HEADER_MARKED_OFFSET};
use vo_runtime::instruction::Instruction;
use vo_runtime::ValueMeta;

use crate::translate::emit_jit_error_if_zero;
use crate::translator::{
    emit_funcref_call_raw, emit_gc_safepoint_poll, emit_runtime_helper_call, HelperKind,
    JitMemoryRegion, RuntimeOpsEmitter,
};
use crate::JitError;

pub(in crate::translate) fn ptr_new<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let slots = e
        .ptr_layout()
        .ok_or(JitError::MissingJitLayout {
            pc: e.current_pc(),
            opcode: inst.opcode(),
            layout: "PtrLayout",
        })?
        .len();
    let total_size = vo_runtime::gc::GcHeader::SIZE
        .checked_add(slots.saturating_mul(vo_runtime::slot::SLOT_BYTES));
    let constant_meta = e
        .get_reg_const(inst.b)
        .and_then(|raw| u32::try_from(raw).ok())
        .and_then(ValueMeta::try_from_raw);

    if let (Some(total_size), Some(value_meta)) = (total_size, constant_meta) {
        if JitSmallAllocLaneField::Cursor
            .offset_for_size(total_size)
            .is_some()
        {
            emit_jit_small_ptr_new(e, inst, slots, total_size, value_meta.to_raw());
            return Ok(());
        }
    }

    let func = e.helper(HelperKind::gc_alloc);
    let gc_ptr = e.gc_ptr();
    let meta_raw = e.read_var(inst.b);
    let meta_i32 = e.builder().ins().ireduce(types::I32, meta_raw);
    let slots_i32 = e.builder().ins().iconst(types::I32, slots as i64);
    let call = emit_runtime_helper_call(e, func, &[gc_ptr, meta_i32, slots_i32]);
    let result = e.builder().inst_results(call)[0];
    emit_jit_error_if_zero(e, result);
    e.write_var(inst.a, result);
    Ok(())
}

fn emit_jit_small_ptr_new<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
    slots: usize,
    total_size: usize,
    meta_raw: u32,
) {
    emit_gc_safepoint_poll(e);

    let gc = e.gc_ptr();
    let lane_offset = |field: JitSmallAllocLaneField| {
        field
            .offset_for_size(total_size)
            .expect("verified small allocation must have a JIT lane")
    };
    let cursor = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I64,
        gc,
        lane_offset(JitSmallAllocLaneField::Cursor),
    );
    let limit = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I64,
        gc,
        lane_offset(JitSmallAllocLaneField::Limit),
    );
    let has_cell = e
        .builder()
        .ins()
        .icmp(IntCC::UnsignedLessThan, cursor, limit);
    let fast = e.builder().create_block();
    let slow = crate::compile_common::cold_block(e.builder());
    let done = e.builder().create_block();
    e.builder().append_block_param(done, types::I64);
    e.builder().ins().brif(has_cell, fast, &[], slow, &[]);

    e.builder().switch_to_block(fast);
    e.builder().seal_block(fast);
    let class_size = total_size.max(16).next_power_of_two();
    let next_cursor = e.builder().ins().iadd_imm_u(cursor, class_size as i64);
    e.store_trusted(
        JitMemoryRegion::Gc,
        next_cursor,
        gc,
        lane_offset(JitSmallAllocLaneField::Cursor),
    );

    let bitmap_word = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I64,
        gc,
        lane_offset(JitSmallAllocLaneField::BitmapWord),
    );
    let next_bit = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I64,
        gc,
        lane_offset(JitSmallAllocLaneField::NextBit),
    );
    let allocated = e
        .builder()
        .ins()
        .load(types::I64, MemFlags::trusted(), bitmap_word, 0);
    let allocated = e.builder().ins().bor(allocated, next_bit);
    e.builder()
        .ins()
        .store(MemFlags::trusted(), allocated, bitmap_word, 0);
    let following_bit = e.builder().ins().ishl_imm_u(next_bit, 1);
    e.store_trusted(
        JitMemoryRegion::Gc,
        following_bit,
        gc,
        lane_offset(JitSmallAllocLaneField::NextBit),
    );

    let logical_size_cursor = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I64,
        gc,
        lane_offset(JitSmallAllocLaneField::LogicalSizeCursor),
    );
    let logical_size = e.builder().ins().iconst(types::I16, total_size as i64);
    e.builder()
        .ins()
        .store(MemFlags::trusted(), logical_size, logical_size_cursor, 0);
    let next_logical_size = e.builder().ins().iadd_imm_u(logical_size_cursor, 2);
    e.store_trusted(
        JitMemoryRegion::Gc,
        next_logical_size,
        gc,
        lane_offset(JitSmallAllocLaneField::LogicalSizeCursor),
    );

    let live_cells_ptr = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I64,
        gc,
        lane_offset(JitSmallAllocLaneField::LiveCells),
    );
    let live_cells = e
        .builder()
        .ins()
        .load(types::I16, MemFlags::trusted(), live_cells_ptr, 0);
    let live_cells = e.builder().ins().iadd_imm_u(live_cells, 1);
    e.builder()
        .ins()
        .store(MemFlags::trusted(), live_cells, live_cells_ptr, 0);

    increment_gc_usize(e, gc, JitGcPollField::AllocatedSpanBytes, class_size);
    let logical_bytes_ptr = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I64,
        gc,
        lane_offset(JitSmallAllocLaneField::LogicalBytes),
    );
    let logical_bytes =
        e.builder()
            .ins()
            .load(types::I64, MemFlags::trusted(), logical_bytes_ptr, 0);
    let logical_bytes = e
        .builder()
        .ins()
        .iadd_imm_u(logical_bytes, total_size as i64);
    e.builder()
        .ins()
        .store(MemFlags::trusted(), logical_bytes, logical_bytes_ptr, 0);

    let current_white = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I8,
        gc,
        JitGcPollField::CurrentWhite.offset(),
    );
    e.builder()
        .ins()
        .store(MemFlags::trusted(), current_white, cursor, 0);
    let slots_value = e.builder().ins().iconst(types::I16, slots as i64);
    e.builder()
        .ins()
        .store(MemFlags::trusted(), slots_value, cursor, 2);
    let meta_value = e.builder().ins().iconst(types::I32, i64::from(meta_raw));
    e.builder()
        .ins()
        .store(MemFlags::trusted(), meta_value, cursor, 4);

    increment_gc_usize(e, gc, JitGcPollField::TotalBytes, total_size);
    increment_gc_usize(e, gc, JitGcPollField::LiveObjectCount, 1);
    increment_gc_usize(e, gc, JitGcPollField::YoungLiveBytes, total_size);
    increment_gc_u64(e, gc, JitGcPollField::AllocationBytesTotal, total_size);
    let debt = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I64,
        gc,
        JitGcPollField::Debt.offset(),
    );
    let debt = e.builder().ins().iadd_imm_u(debt, total_size as i64);
    e.store_trusted(JitMemoryRegion::Gc, debt, gc, JitGcPollField::Debt.offset());
    let automatic = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I8,
        gc,
        JitGcPollField::AutomaticGc.offset(),
    );
    let automatic = e.builder().ins().icmp_imm_u(IntCC::NotEqual, automatic, 0);
    let debt_due = e
        .builder()
        .ins()
        .icmp_imm_s(IntCC::SignedGreaterThan, debt, 0);
    let poll_due = e.builder().ins().band(automatic, debt_due);
    let required = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I8,
        gc,
        JitGcPollField::Required.offset(),
    );
    let one = e.builder().ins().iconst(types::I8, 1);
    let required = e.builder().ins().select(poll_due, one, required);
    e.store_trusted(
        JitMemoryRegion::Gc,
        required,
        gc,
        JitGcPollField::Required.offset(),
    );

    let object = e
        .builder()
        .ins()
        .iadd_imm_u(cursor, -i64::from(JIT_GC_HEADER_MARKED_OFFSET));
    e.builder().ins().jump(done, &[object.into()]);

    e.builder().switch_to_block(slow);
    e.builder().seal_block(slow);
    let func = e.helper(HelperKind::gc_alloc);
    let meta_value = e.builder().ins().iconst(types::I32, i64::from(meta_raw));
    let slots_value = e.builder().ins().iconst(types::I32, slots as i64);
    let call = emit_funcref_call_raw(e, func.func_ref(), &[gc, meta_value, slots_value]);
    let object = e.builder().inst_results(call)[0];
    e.builder().ins().jump(done, &[object.into()]);

    e.builder().switch_to_block(done);
    e.builder().seal_block(done);
    let object = e.builder().block_params(done)[0];
    emit_jit_error_if_zero(e, object);
    e.write_var(inst.a, object);
}

fn increment_gc_usize<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    gc: cranelift_codegen::ir::Value,
    field: JitGcPollField,
    amount: usize,
) {
    increment_gc_u64(e, gc, field, amount);
}

fn increment_gc_u64<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    gc: cranelift_codegen::ir::Value,
    field: JitGcPollField,
    amount: usize,
) {
    let value = e.load_trusted(JitMemoryRegion::Gc, types::I64, gc, field.offset());
    let value = e.builder().ins().iadd_imm_u(value, amount as i64);
    e.store_trusted(JitMemoryRegion::Gc, value, gc, field.offset());
}

pub(in crate::translate) fn str_new<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = e.helper(HelperKind::str_new);
    let const_idx = inst.b as usize;
    let bytes: Vec<u8> = match e.vo_module().constants.get(const_idx) {
        Some(Constant::String(s)) => s.as_bytes().to_vec(),
        Some(other) => {
            return Err(JitError::Internal(format!(
                "StrNew constant at pc {} must be String, got {other:?}",
                e.current_pc()
            )));
        }
        None => {
            return Err(JitError::Internal(format!(
                "StrNew constant index {const_idx} missing at pc {}",
                e.current_pc()
            )));
        }
    };
    let len = bytes.len();
    if len == 0 {
        let zero = e.builder().ins().iconst(types::I64, 0);
        e.write_var(inst.a, zero);
    } else {
        let gc_ptr = e.gc_ptr();
        let stack_slot = e.builder().create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            len as u32,
            0,
        ));
        for (i, &b) in bytes.iter().enumerate() {
            let byte_val = e.builder().ins().iconst(types::I8, b as i64);
            e.builder()
                .ins()
                .stack_store(types::I64, byte_val, stack_slot, i as i32);
        }
        let data_ptr = e.builder().ins().stack_addr(types::I64, stack_slot, 0);
        let len_val = e.builder().ins().iconst(types::I64, len as i64);
        let call = emit_runtime_helper_call(e, func, &[gc_ptr, data_ptr, len_val]);
        let result = e.builder().inst_results(call)[0];
        emit_jit_error_if_zero(e, result);
        e.write_var(inst.a, result);
    }
    Ok(())
}
