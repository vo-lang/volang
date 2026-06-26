use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Value};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitRuntimeTrapKind;

use crate::call_helpers::emit_checked_jit_result_helper_call;
use crate::translate::{emit_runtime_trap_if, require_helper};
use crate::translator::{emit_funcref_call, CollectionEmitter};
use crate::JitError;

use super::element::{emit_elem_bytes_i32, load_element, resolve_elem_bytes, store_element};
use super::slice::emit_nil_guarded_load;

pub(in crate::translate) const ARRAY_HEADER_BYTES: i64 = 16; // 2 slots

/// Emit VM-equivalent array bounds checks. A nil array has length 0, so any
/// access traps as IndexOutOfBounds before touching the ArrayHeader.
pub(in crate::translate) fn emit_array_bounds_check<'a>(
    e: &mut impl CollectionEmitter<'a>,
    arr: Value,
    idx: Value,
) {
    let len = emit_nil_guarded_load(e, arr, 0);
    let out_of_bounds = e
        .builder()
        .ins()
        .icmp(IntCC::UnsignedGreaterThanOrEqual, idx, len);
    emit_runtime_trap_if(
        e,
        out_of_bounds,
        JitRuntimeTrapKind::IndexOutOfBounds,
        Some(idx),
        Some(len),
    );
}

pub(in crate::translate) fn array_new<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let array_new_func = require_helper(e.helpers().array_new, "array_new")?;
    let gc_ptr = e.gc_ptr();
    let meta_raw = e.read_var(inst.b);
    let meta_i32 = e.builder().ins().ireduce(types::I32, meta_raw);
    let elem_bytes_i32 = emit_elem_bytes_i32(e, inst.opcode(), inst.flags, inst.c + 1)?;
    let len = e.read_var(inst.c);
    let call = emit_funcref_call(e, array_new_func, &[gc_ptr, meta_i32, elem_bytes_i32, len]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn array_get<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let arr = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    emit_array_bounds_check(e, arr, idx);

    let (elem_bytes, needs_sext) = resolve_elem_bytes(e, inst.opcode(), inst.flags, inst.c + 1)?;
    if elem_bytes == 0 {
        return Ok(());
    }
    let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
    let off = e.builder().ins().imul(idx, eb);
    let off = e.builder().ins().iadd_imm(off, ARRAY_HEADER_BYTES);

    if elem_bytes <= 8 {
        let addr = e.builder().ins().iadd(arr, off);
        let val = load_element(e, addr, elem_bytes, needs_sext);
        e.write_var(inst.a, val);
    } else {
        let elem_slots = elem_bytes.div_ceil(8);
        for i in 0..elem_slots {
            let slot_off = e.builder().ins().iadd_imm(off, (i * 8) as i64);
            let addr = e.builder().ins().iadd(arr, slot_off);
            let val = e
                .builder()
                .ins()
                .load(types::I64, MemFlags::trusted(), addr, 0);
            e.write_var(inst.a + i as u16, val);
        }
    }
    Ok(())
}

pub(in crate::translate) fn array_set<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let arr = e.read_var(inst.a);
    let idx = e.read_var(inst.b);
    emit_array_bounds_check(e, arr, idx);

    let (elem_bytes, _) = resolve_elem_bytes(e, inst.opcode(), inst.flags, inst.b + 1)?;
    if elem_bytes == 0 {
        return Ok(());
    }
    let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
    let off = e.builder().ins().imul(idx, eb);
    let off = e.builder().ins().iadd_imm(off, ARRAY_HEADER_BYTES);

    if elem_bytes <= 8 {
        let val = e.read_var(inst.c);
        let addr = e.builder().ins().iadd(arr, off);
        if elem_bytes == 8 {
            emit_array_typed_write_barrier_single(e, arr, val)?;
        }
        store_element(e, addr, val, elem_bytes);
    } else {
        let elem_slots = elem_bytes.div_ceil(8);
        // Use elem_meta from the array header so struct/interface barriers match the VM.
        emit_array_write_barrier_multi(e, arr, inst.c, elem_slots)?;
        for i in 0..elem_slots {
            let v = e.read_var(inst.c + i as u16);
            let slot_off = e.builder().ins().iadd_imm(off, (i * 8) as i64);
            let addr = e.builder().ins().iadd(arr, slot_off);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
    }
    Ok(())
}

/// Emit a typed write barrier for a single-slot array/slice element write.
/// Checks elem_meta.value_kind >= Array (14) at runtime to skip primitive elements,
/// then lets the shared metadata helper decide whether the slot is a real GcRef.
pub(in crate::translate) fn emit_array_typed_write_barrier_single<'a>(
    e: &mut impl CollectionEmitter<'a>,
    arr: Value,
    val: Value,
) -> Result<(), JitError> {
    let elem_meta_raw = e
        .builder()
        .ins()
        .load(types::I32, MemFlags::trusted(), arr, 8);
    let vk = e.builder().ins().band_imm(elem_meta_raw, 0xFF);
    let gc_ref_threshold = e
        .builder()
        .ins()
        .iconst(types::I32, vo_runtime::ValueKind::Array as i64);
    let needs_barrier =
        e.builder()
            .ins()
            .icmp(IntCC::UnsignedGreaterThanOrEqual, vk, gc_ref_threshold);

    let barrier_block = e.builder().create_block();
    let continue_block = e.builder().create_block();
    e.builder()
        .ins()
        .brif(needs_barrier, barrier_block, &[], continue_block, &[]);

    e.builder().switch_to_block(barrier_block);
    e.builder().seal_block(barrier_block);
    let typed_barrier = require_helper(
        e.helpers().typed_write_barrier_by_meta,
        "typed_write_barrier_by_meta",
    )?;
    let vals_slot =
        e.builder()
            .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 8));
    e.builder().ins().stack_store(val, vals_slot, 0);
    let vals_ptr = e.builder().ins().stack_addr(types::I64, vals_slot, 0);
    let ctx = e.ctx_param();
    let val_slots = e.builder().ins().iconst(types::I32, 1);
    emit_checked_jit_result_helper_call(
        e,
        typed_barrier,
        &[ctx, arr, vals_ptr, val_slots, elem_meta_raw],
        true,
    );
    e.builder().ins().jump(continue_block, &[]);

    e.builder().switch_to_block(continue_block);
    e.builder().seal_block(continue_block);
    Ok(())
}

/// Emit a typed write barrier for multi-slot array/slice element writes.
pub(in crate::translate) fn emit_array_write_barrier_multi<'a>(
    e: &mut impl CollectionEmitter<'a>,
    arr: Value,
    src_start: u16,
    elem_slots: usize,
) -> Result<(), JitError> {
    let elem_meta_raw = e
        .builder()
        .ins()
        .load(types::I32, MemFlags::trusted(), arr, 8);
    let typed_barrier = require_helper(
        e.helpers().typed_write_barrier_by_meta,
        "typed_write_barrier_by_meta",
    )?;
    let vals_slot = e.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (elem_slots * 8) as u32,
        8,
    ));
    let vals_ptr = e.builder().ins().stack_addr(types::I64, vals_slot, 0);
    for i in 0..elem_slots {
        let v = e.read_var(src_start + i as u16);
        e.builder().ins().stack_store(v, vals_slot, (i * 8) as i32);
    }
    let ctx = e.ctx_param();
    let val_slots = e.builder().ins().iconst(types::I32, elem_slots as i64);
    emit_checked_jit_result_helper_call(
        e,
        typed_barrier,
        &[ctx, arr, vals_ptr, val_slots, elem_meta_raw],
        true,
    );
    Ok(())
}

pub(in crate::translate) fn array_addr<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let arr = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    emit_array_bounds_check(e, arr, idx);

    let (elem_bytes, _) = resolve_elem_bytes(e, inst.opcode(), inst.flags, inst.c + 1)?;
    let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
    let off = e.builder().ins().imul(idx, eb);
    let off = e.builder().ins().iadd_imm(off, ARRAY_HEADER_BYTES);
    let addr = e.builder().ins().iadd(arr, off);
    e.write_var(inst.a, addr);
    Ok(())
}
