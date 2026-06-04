use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Value};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitRuntimeTrapKind;

use crate::translate::{emit_runtime_trap_if, require_helper};
use crate::translator::{emit_funcref_call, CollectionEmitter};
use crate::JitError;

use super::array::{emit_array_write_barrier, emit_array_write_barrier_multi};
use super::element::{
    emit_elem_bytes_i32, emit_return_if_u64_jit_error, load_element, resolve_elem_bytes,
    store_element,
};

use vo_runtime::objects::slice::{
    FIELD_DATA_PTR as SLICE_FIELD_DATA_PTR_SLOT, FIELD_LEN as SLICE_FIELD_LEN_SLOT,
};
pub(in crate::translate) const SLICE_FIELD_DATA_PTR: i32 = (SLICE_FIELD_DATA_PTR_SLOT * 8) as i32;
pub(in crate::translate) const SLICE_FIELD_LEN: i32 = (SLICE_FIELD_LEN_SLOT * 8) as i32;

/// Emit bounds check for slice access. Panics if idx >= len or slice is nil.
/// Returns data_ptr for the slice (only valid if bounds check passed).
pub(in crate::translate) fn emit_slice_bounds_check<'a>(
    e: &mut impl CollectionEmitter<'a>,
    s: Value,
    idx: Value,
) -> Value {
    // len = 0 if nil, otherwise load from slice
    let len = emit_nil_guarded_load(e, s, SLICE_FIELD_LEN);

    // Check idx >= len (nil slice has len=0, so any idx will be out of bounds)
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

    e.builder()
        .ins()
        .load(types::I64, MemFlags::trusted(), s, SLICE_FIELD_DATA_PTR)
}

pub(in crate::translate) fn slice_new<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().slice_new_checked, "slice_new_checked")?;
    let elem_bytes_val = emit_elem_bytes_i32(e, inst.opcode(), inst.flags, inst.c + 2)?;
    let gc_ptr = e.gc_ptr();
    let meta_raw = e.read_var(inst.b);
    let meta_i32 = e.builder().ins().ireduce(types::I32, meta_raw);
    let len = e.read_var(inst.c);
    let cap = e.read_var(inst.c + 1);

    // Create stack slot for output
    let out_slot =
        e.builder()
            .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 8));
    let out_ptr = e.builder().ins().stack_addr(types::I64, out_slot, 0);

    // Call checked helper: (gc, meta, elem_bytes, len, cap, out) -> error_code
    let call = emit_funcref_call(
        e,
        func,
        &[gc_ptr, meta_i32, elem_bytes_val, len, cap, out_ptr],
    );
    let error_code = e.builder().inst_results(call)[0];

    // Panic if error_code != 0
    let zero = e.builder().ins().iconst(types::I32, 0);
    let has_error = e.builder().ins().icmp(IntCC::NotEqual, error_code, zero);
    let error_arg = e.builder().ins().sextend(types::I64, error_code);
    emit_runtime_trap_if(
        e,
        has_error,
        JitRuntimeTrapKind::MakeSlice,
        Some(error_arg),
        None,
    );

    // Load result from output slot
    let result = e.builder().ins().stack_load(types::I64, out_slot, 0);
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn slice_get<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let s = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let (elem_bytes, needs_sext) = resolve_elem_bytes(e, inst.opcode(), inst.flags, inst.c + 1)?;

    let data_ptr = emit_slice_bounds_check(e, s, idx);
    if elem_bytes == 0 {
        return Ok(());
    }
    let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
    let off = e.builder().ins().imul(idx, eb);

    if elem_bytes <= 8 {
        let addr = e.builder().ins().iadd(data_ptr, off);
        let val = load_element(e, addr, elem_bytes, needs_sext);
        e.write_var(inst.a, val);
    } else {
        let elem_slots = elem_bytes.div_ceil(8);
        for i in 0..elem_slots {
            let slot_off = e.builder().ins().iadd_imm(off, (i * 8) as i64);
            let addr = e.builder().ins().iadd(data_ptr, slot_off);
            let val = e
                .builder()
                .ins()
                .load(types::I64, MemFlags::trusted(), addr, 0);
            e.write_var(inst.a + i as u16, val);
        }
    }
    Ok(())
}

pub(in crate::translate) fn slice_set<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let s = e.read_var(inst.a);
    let idx = e.read_var(inst.b);
    let (elem_bytes, _) = resolve_elem_bytes(e, inst.opcode(), inst.flags, inst.b + 1)?;

    let data_ptr = emit_slice_bounds_check(e, s, idx);
    if elem_bytes == 0 {
        return Ok(());
    }
    let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
    let off = e.builder().ins().imul(idx, eb);

    if elem_bytes <= 8 {
        let val = e.read_var(inst.c);
        let addr = e.builder().ins().iadd(data_ptr, off);
        store_element(e, addr, val, elem_bytes);
        // Write barrier for 8-byte elements that may be GcRefs.
        // Load backing array from SliceData.array (offset 0) and use it as barrier parent.
        if elem_bytes == 8 {
            let arr = e
                .builder()
                .ins()
                .load(types::I64, MemFlags::trusted(), s, 0);
            emit_array_write_barrier(e, arr, val)?;
        }
    } else {
        let elem_slots = elem_bytes.div_ceil(8);
        for i in 0..elem_slots {
            let v = e.read_var(inst.c + i as u16);
            let slot_off = e.builder().ins().iadd_imm(off, (i * 8) as i64);
            let addr = e.builder().ins().iadd(data_ptr, slot_off);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
        // Write barrier for multi-slot elements: load backing array for barrier parent.
        let arr = e
            .builder()
            .ins()
            .load(types::I64, MemFlags::trusted(), s, 0);
        emit_array_write_barrier_multi(e, arr, inst.c, elem_slots)?;
    }
    Ok(())
}

/// Load a field from a pointer, returning 0 if pointer is nil.
/// Pattern: if ptr == 0 { 0 } else { ptr.field }
pub(in crate::translate) fn emit_nil_guarded_load<'a>(
    e: &mut impl CollectionEmitter<'a>,
    ptr: Value,
    offset: i32,
) -> Value {
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_nil = e.builder().ins().icmp(IntCC::Equal, ptr, zero);

    let nil_block = e.builder().create_block();
    let not_nil_block = e.builder().create_block();
    let merge_block = e.builder().create_block();
    e.builder().append_block_param(merge_block, types::I64);
    e.builder()
        .ins()
        .brif(is_nil, nil_block, &[], not_nil_block, &[]);

    e.builder().switch_to_block(nil_block);
    e.builder().seal_block(nil_block);
    e.builder().ins().jump(merge_block, &[zero]);

    e.builder().switch_to_block(not_nil_block);
    e.builder().seal_block(not_nil_block);
    let val = e
        .builder()
        .ins()
        .load(types::I64, MemFlags::trusted(), ptr, offset);
    e.builder().ins().jump(merge_block, &[val]);

    e.builder().switch_to_block(merge_block);
    e.builder().seal_block(merge_block);
    e.builder().block_params(merge_block)[0]
}

pub(in crate::translate) fn slice_len<'a>(e: &mut impl CollectionEmitter<'a>, inst: &Instruction) {
    let s = e.read_var(inst.b);
    let result = emit_nil_guarded_load(e, s, SLICE_FIELD_LEN);
    e.write_var(inst.a, result);
}

pub(in crate::translate) const SLICE_FIELD_CAP: i32 =
    (vo_runtime::objects::slice::FIELD_CAP * 8) as i32;

pub(in crate::translate) fn slice_cap<'a>(e: &mut impl CollectionEmitter<'a>, inst: &Instruction) {
    let s = e.read_var(inst.b);
    let result = emit_nil_guarded_load(e, s, SLICE_FIELD_CAP);
    e.write_var(inst.a, result);
}

pub(in crate::translate) fn slice_slice<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let gc_ptr = e.gc_ptr();
    let src = e.read_var(inst.b);
    let lo = e.read_var(inst.c);
    let hi = e.read_var(inst.c + 1);

    let is_array = (inst.flags & 0b01) != 0;
    let has_max = (inst.flags & 0b10) != 0;

    // Helper functions do bounds checking and return u64::MAX on error
    let result = if has_max {
        let max = e.read_var(inst.c + 2);
        if is_array {
            let func = require_helper(e.helpers().slice_from_array3, "slice_from_array3")?;
            let call = emit_funcref_call(e, func, &[gc_ptr, src, lo, hi, max]);
            e.builder().inst_results(call)[0]
        } else {
            let func = require_helper(e.helpers().slice_slice3, "slice_slice3")?;
            let call = emit_funcref_call(e, func, &[gc_ptr, src, lo, hi, max]);
            e.builder().inst_results(call)[0]
        }
    } else if is_array {
        let func = require_helper(e.helpers().slice_from_array, "slice_from_array")?;
        let call = emit_funcref_call(e, func, &[gc_ptr, src, lo, hi]);
        e.builder().inst_results(call)[0]
    } else {
        let func = require_helper(e.helpers().slice_slice, "slice_slice")?;
        let call = emit_funcref_call(e, func, &[gc_ptr, src, lo, hi]);
        e.builder().inst_results(call)[0]
    };

    // Check for bounds error (helper returns u64::MAX on error)
    let error_val = e.builder().ins().iconst(types::I64, -1i64);
    let is_error = e.builder().ins().icmp(IntCC::Equal, result, error_val);
    emit_runtime_trap_if(
        e,
        is_error,
        JitRuntimeTrapKind::SliceBoundsOutOfRange,
        Some(lo),
        Some(hi),
    );

    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn slice_append<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let slice_append_func = require_helper(e.helpers().slice_append, "slice_append")?;
    let ctx = e.ctx_param();
    let s = e.read_var(inst.b);

    // Instruction format:
    // - c = elem_meta slot
    // - flags==0: c+1 = elem_bytes, c+2.. = elem value
    // - flags!=0: c+1.. = elem value (elem_bytes derived from flags)

    // elem_meta from slot c (as i32)
    let elem_meta_raw = e.read_var(inst.c);
    let elem_meta = e.builder().ins().ireduce(types::I32, elem_meta_raw);

    // elem_bytes (as i32)
    let elem_bytes = emit_elem_bytes_i32(e, inst.opcode(), inst.flags, inst.c + 1)?;

    // val_ptr: pointer to element value in stack
    let elem_slot = inst.c + if inst.flags == 0 { 2 } else { 1 };
    let val_ptr = e.var_addr(elem_slot);

    // vo_slice_append(ctx, elem_meta: u32, elem_bytes: u32, s: u64, val_ptr: *const u64) -> u64
    let call = emit_funcref_call(
        e,
        slice_append_func,
        &[ctx, elem_meta, elem_bytes, s, val_ptr],
    );
    let result = e.builder().inst_results(call)[0];
    emit_return_if_u64_jit_error(e, result);
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn slice_addr<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let s = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let (elem_bytes, _) = resolve_elem_bytes(e, inst.opcode(), inst.flags, inst.c + 1)?;
    let data_ptr = emit_slice_bounds_check(e, s, idx);
    let eb = e.builder().ins().iconst(types::I64, elem_bytes as i64);
    let off = e.builder().ins().imul(idx, eb);
    let addr = e.builder().ins().iadd(data_ptr, off);
    e.write_var(inst.a, addr);
    Ok(())
}
