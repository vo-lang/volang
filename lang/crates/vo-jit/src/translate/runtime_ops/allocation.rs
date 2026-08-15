use cranelift_codegen::ir::{
    condcodes::IntCC, types, InstBuilder, MemFlagsData as MemFlags, StackSlotData, StackSlotKind,
};
use vo_runtime::bytecode::Constant;
use vo_runtime::gc::{JitAllocationRegionField, JIT_GC_HEADER_MARKED_OFFSET};
use vo_runtime::instruction::Instruction;
use vo_runtime::ValueMeta;

use crate::translate::emit_jit_error_if_zero;
use crate::translator::{
    emit_funcref_call_raw, emit_gc_safepoint_poll, emit_runtime_helper_call, HelperKind,
    JitMemoryRegion, RuntimeOpsEmitter,
};
use crate::JitError;

/// Preserve the complete allocation effect of a scalar-replaced `PtrNew`.
/// Field accesses use SSA values, while the otherwise-unreferenced object keeps
/// GC debt, telemetry, hard-limit and sticky OOM behavior identical to the VM.
pub(crate) fn materialize_scalar_replaced_ptr_new<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    ptr_new(e, inst)
}

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
        if JitAllocationRegionField::Cursor
            .offset_for_size(total_size)
            .is_some()
        {
            emit_jit_small_ptr_new(e, inst, slots, total_size, value_meta.to_raw());
            return Ok(());
        }
    }

    let func = e.helper(HelperKind::gc_alloc);
    let ctx = e.ctx_param();
    let meta_raw = e.read_var(inst.b);
    let meta_i32 = e.builder().ins().ireduce(types::I32, meta_raw);
    let slots_i32 = e.builder().ins().iconst(types::I32, slots as i64);
    let call = emit_runtime_helper_call(e, func, &[ctx, meta_i32, slots_i32]);
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
    let _ = slots;
    let gc = e.gc_ptr();
    let region_offset = |field: JitAllocationRegionField| {
        field
            .offset_for_size(total_size)
            .expect("verified small allocation must have a JIT region")
    };
    let expected_shape = JitAllocationRegionField::shape(total_size, meta_raw);
    let active_shape = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I64,
        gc,
        region_offset(JitAllocationRegionField::Shape),
    );
    let region_matches =
        e.builder()
            .ins()
            .icmp_imm_u(IntCC::Equal, active_shape, expected_shape as i64);
    let cursor = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I64,
        gc,
        region_offset(JitAllocationRegionField::Cursor),
    );
    let limit = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I64,
        gc,
        region_offset(JitAllocationRegionField::Limit),
    );
    let cursor_available = e
        .builder()
        .ins()
        .icmp(IntCC::UnsignedLessThan, cursor, limit);
    let has_cell = e.builder().ins().band(region_matches, cursor_available);
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
        region_offset(JitAllocationRegionField::Cursor),
    );

    let bitmap_word = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I64,
        gc,
        region_offset(JitAllocationRegionField::BitmapWord),
    );
    let next_bit = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I64,
        gc,
        region_offset(JitAllocationRegionField::NextBit),
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
        region_offset(JitAllocationRegionField::NextBit),
    );

    let object = e
        .builder()
        .ins()
        .iadd_imm_u(cursor, -i64::from(JIT_GC_HEADER_MARKED_OFFSET));
    e.builder().ins().jump(done, &[object.into()]);

    e.builder().switch_to_block(slow);
    e.builder().seal_block(slow);
    emit_gc_safepoint_poll(e);
    let func = e.helper(HelperKind::gc_alloc);
    let meta_value = e.builder().ins().iconst(types::I32, i64::from(meta_raw));
    let slots_value = e.builder().ins().iconst(types::I32, slots as i64);
    let ctx = e.ctx_param();
    let call = emit_funcref_call_raw(e, func.func_ref(), &[ctx, meta_value, slots_value]);
    let object = e.builder().inst_results(call)[0];
    e.builder().ins().jump(done, &[object.into()]);

    e.builder().switch_to_block(done);
    e.builder().seal_block(done);
    let object = e.builder().block_params(done)[0];
    emit_jit_error_if_zero(e, object);
    e.write_var(inst.a, object);
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
