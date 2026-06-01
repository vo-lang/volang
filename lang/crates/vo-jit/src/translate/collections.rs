#![allow(unused_imports)]

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{
    types, InstBuilder, MemFlags, StackSlot, StackSlotData, StackSlotKind, Value,
};
use vo_runtime::bytecode::Constant;
use vo_runtime::instruction::{Instruction, Opcode, QUEUE_KIND_PORT_FLAG};
use vo_runtime::jit_api::{JitResult, JitRuntimeTrapKind, JIT_HELPER_U64_ERROR};

use crate::call_helpers::emit_checked_jit_result_helper_call;
use crate::translator::{
    emit_funcref_call, emit_funcref_call_with_effect, HelperCallEffect, IrEmitter,
};
use crate::JitError;

use super::{emit_runtime_trap_if, require_helper};

// =============================================================================
// Slice/Array element size helpers
// =============================================================================

/// Resolve elem_bytes from instruction flags or per-PC JIT metadata.
/// When flags==0, verifier requires metadata; register constants are not a
/// layout authority.
/// Returns (elem_bytes, needs_sign_extend).
pub(super) fn resolve_elem_bytes<'a>(
    e: &impl IrEmitter<'a>,
    opcode: Opcode,
    flags: u8,
    eb_reg: u16,
) -> Result<(usize, bool), JitError> {
    let layout = e
        .elem_layout(flags, eb_reg)
        .ok_or(JitError::MissingJitLayout {
            pc: e.current_pc(),
            opcode,
            layout: "ElemLayout",
        })?;
    Ok((layout.bytes, layout.needs_sign_extend))
}

/// Load a single element (1/2/4/8 bytes) from memory address, with optional sign extension.
pub(super) fn load_element<'a>(
    e: &mut impl IrEmitter<'a>,
    addr: Value,
    elem_bytes: usize,
    needs_sext: bool,
) -> Value {
    match elem_bytes {
        1 => {
            let v = e
                .builder()
                .ins()
                .load(types::I8, MemFlags::trusted(), addr, 0);
            if needs_sext {
                e.builder().ins().sextend(types::I64, v)
            } else {
                e.builder().ins().uextend(types::I64, v)
            }
        }
        2 => {
            let v = e
                .builder()
                .ins()
                .load(types::I16, MemFlags::trusted(), addr, 0);
            if needs_sext {
                e.builder().ins().sextend(types::I64, v)
            } else {
                e.builder().ins().uextend(types::I64, v)
            }
        }
        4 => {
            let v = e
                .builder()
                .ins()
                .load(types::I32, MemFlags::trusted(), addr, 0);
            if needs_sext {
                e.builder().ins().sextend(types::I64, v)
            } else {
                e.builder().ins().uextend(types::I64, v)
            }
        }
        _ => e
            .builder()
            .ins()
            .load(types::I64, MemFlags::trusted(), addr, 0),
    }
}

/// Store a single element (1/2/4/8 bytes) to memory address.
pub(super) fn store_element<'a>(
    e: &mut impl IrEmitter<'a>,
    addr: Value,
    val: Value,
    elem_bytes: usize,
) {
    match elem_bytes {
        1 => {
            let v = e.builder().ins().ireduce(types::I8, val);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
        2 => {
            let v = e.builder().ins().ireduce(types::I16, val);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
        4 => {
            let v = e.builder().ins().ireduce(types::I32, val);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
        _ => {
            e.builder().ins().store(MemFlags::trusted(), val, addr, 0);
        }
    }
}

pub(super) fn emit_elem_bytes_i32<'a>(
    e: &mut impl IrEmitter<'a>,
    opcode: Opcode,
    flags: u8,
    eb_reg: u16,
) -> Result<Value, JitError> {
    let (elem_bytes, _) = resolve_elem_bytes(e, opcode, flags, eb_reg)?;
    Ok(e.builder().ins().iconst(types::I32, elem_bytes as i64))
}

// =============================================================================
// Slice operations
// =============================================================================

use vo_runtime::objects::slice::{
    FIELD_DATA_PTR as SLICE_FIELD_DATA_PTR_SLOT, FIELD_LEN as SLICE_FIELD_LEN_SLOT,
};
pub(super) const SLICE_FIELD_DATA_PTR: i32 = (SLICE_FIELD_DATA_PTR_SLOT * 8) as i32;
pub(super) const SLICE_FIELD_LEN: i32 = (SLICE_FIELD_LEN_SLOT * 8) as i32;

/// Emit bounds check for slice access. Panics if idx >= len or slice is nil.
/// Returns data_ptr for the slice (only valid if bounds check passed).
pub(super) fn emit_slice_bounds_check<'a>(
    e: &mut impl IrEmitter<'a>,
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

pub(super) fn slice_new<'a>(
    e: &mut impl IrEmitter<'a>,
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

pub(super) fn slice_get<'a>(
    e: &mut impl IrEmitter<'a>,
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

pub(super) fn slice_set<'a>(
    e: &mut impl IrEmitter<'a>,
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
pub(super) fn emit_nil_guarded_load<'a>(
    e: &mut impl IrEmitter<'a>,
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

pub(super) fn slice_len<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let s = e.read_var(inst.b);
    let result = emit_nil_guarded_load(e, s, SLICE_FIELD_LEN);
    e.write_var(inst.a, result);
}

pub(super) const SLICE_FIELD_CAP: i32 = (vo_runtime::objects::slice::FIELD_CAP * 8) as i32;

pub(super) fn slice_cap<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let s = e.read_var(inst.b);
    let result = emit_nil_guarded_load(e, s, SLICE_FIELD_CAP);
    e.write_var(inst.a, result);
}

pub(super) fn slice_slice<'a>(
    e: &mut impl IrEmitter<'a>,
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
    } else {
        if is_array {
            let func = require_helper(e.helpers().slice_from_array, "slice_from_array")?;
            let call = emit_funcref_call(e, func, &[gc_ptr, src, lo, hi]);
            e.builder().inst_results(call)[0]
        } else {
            let func = require_helper(e.helpers().slice_slice, "slice_slice")?;
            let call = emit_funcref_call(e, func, &[gc_ptr, src, lo, hi]);
            e.builder().inst_results(call)[0]
        }
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

pub(super) fn slice_append<'a>(
    e: &mut impl IrEmitter<'a>,
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

pub(super) fn slice_addr<'a>(
    e: &mut impl IrEmitter<'a>,
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

// =============================================================================
// Array operations
// =============================================================================

pub(super) const ARRAY_HEADER_BYTES: i64 = 16; // 2 slots

/// Emit VM-equivalent array bounds checks. A nil array has length 0, so any
/// access traps as IndexOutOfBounds before touching the ArrayHeader.
pub(super) fn emit_array_bounds_check<'a>(e: &mut impl IrEmitter<'a>, arr: Value, idx: Value) {
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

pub(super) fn array_new<'a>(
    e: &mut impl IrEmitter<'a>,
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

pub(super) fn array_get<'a>(
    e: &mut impl IrEmitter<'a>,
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

pub(super) fn array_set<'a>(
    e: &mut impl IrEmitter<'a>,
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
        store_element(e, addr, val, elem_bytes);
        // Write barrier for 8-byte elements that may be GcRefs.
        // Read elem_meta from ArrayHeader (offset 8, low byte is value_kind).
        // may_contain_gc_refs iff value_kind >= ValueKind::Array (14).
        if elem_bytes == 8 {
            emit_array_write_barrier(e, arr, val)?;
        }
    } else {
        let elem_slots = elem_bytes.div_ceil(8);
        for i in 0..elem_slots {
            let v = e.read_var(inst.c + i as u16);
            let slot_off = e.builder().ins().iadd_imm(off, (i * 8) as i64);
            let addr = e.builder().ins().iadd(arr, slot_off);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
        // Use elem_meta from the array header so struct/interface barriers match the VM.
        emit_array_write_barrier_multi(e, arr, inst.c, elem_slots)?;
    }
    Ok(())
}

/// Emit write barrier for a single-slot array/slice element write.
/// Checks elem_meta.value_kind >= Array (14) at runtime to skip non-GcRef elements.
pub(super) fn emit_array_write_barrier<'a>(
    e: &mut impl IrEmitter<'a>,
    arr: Value,
    val: Value,
) -> Result<(), JitError> {
    // ArrayHeader layout: [len:8][elem_meta:4 (low byte = value_kind)][elem_bytes:4]
    // elem_meta is at offset 8 from arr (which points to data after GcHeader).
    // Read the low byte (value_kind) of elem_meta.
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
    let wb_ref = require_helper(e.helpers().write_barrier, "write_barrier")?;
    let gc = e.gc_ptr();
    let zero_offset = e.builder().ins().iconst(types::I32, 0);
    emit_funcref_call(e, wb_ref, &[gc, arr, zero_offset, val]);
    e.builder().ins().jump(continue_block, &[]);

    e.builder().switch_to_block(continue_block);
    e.builder().seal_block(continue_block);
    Ok(())
}

/// Emit a typed write barrier for multi-slot array/slice element writes.
pub(super) fn emit_array_write_barrier_multi<'a>(
    e: &mut impl IrEmitter<'a>,
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

pub(super) fn array_addr<'a>(
    e: &mut impl IrEmitter<'a>,
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

// =============================================================================
// String operations
// =============================================================================

pub(super) fn str_len<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    // String uses SliceData layout — len is at the same offset as slice len.
    // Inline: if s==0 { 0 } else { load(s, SLICE_FIELD_LEN) }
    let s = e.read_var(inst.b);
    let result = emit_nil_guarded_load(e, s, SLICE_FIELD_LEN);
    e.write_var(inst.a, result);
}

pub(super) fn str_index<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let str_index_func = require_helper(e.helpers().str_index, "str_index")?;
    let s = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    // Bounds check: inline len (String uses SliceData layout) instead of calling vo_str_len
    let len = emit_nil_guarded_load(e, s, SLICE_FIELD_LEN);
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
    let call = emit_funcref_call_with_effect(
        e,
        str_index_func,
        &[s, idx],
        HelperCallEffect::FrameIndependent,
    );
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
    Ok(())
}

pub(super) fn str_concat<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().str_concat, "str_concat")?;
    let gc_ptr = e.gc_ptr();
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = emit_funcref_call(e, func, &[gc_ptr, a, b]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
    Ok(())
}

pub(super) fn str_slice<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().str_slice, "str_slice")?;
    let gc_ptr = e.gc_ptr();
    let s = e.read_var(inst.b);
    let lo = e.read_var(inst.c);
    let hi = e.read_var(inst.c + 1);
    let call = emit_funcref_call(e, func, &[gc_ptr, s, lo, hi]);
    let result = e.builder().inst_results(call)[0];
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

pub(super) fn str_eq<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) -> Result<(), JitError> {
    let func = require_helper(e.helpers().str_eq, "str_eq")?;
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = emit_funcref_call_with_effect(e, func, &[a, b], HelperCallEffect::FrameIndependent);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
    Ok(())
}

pub(super) fn str_ne<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) -> Result<(), JitError> {
    let func = require_helper(e.helpers().str_eq, "str_eq")?;
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = emit_funcref_call_with_effect(e, func, &[a, b], HelperCallEffect::FrameIndependent);
    let eq_result = e.builder().inst_results(call)[0];
    let zero = e.builder().ins().iconst(types::I64, 0);
    let cmp = e.builder().ins().icmp(IntCC::Equal, eq_result, zero);
    let result = e.builder().ins().uextend(types::I64, cmp);
    e.write_var(inst.a, result);
    Ok(())
}

pub(super) fn str_cmp<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
    cc: IntCC,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().str_cmp, "str_cmp")?;
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = emit_funcref_call_with_effect(e, func, &[a, b], HelperCallEffect::FrameIndependent);
    let cmp_result = e.builder().inst_results(call)[0]; // i32
    let zero = e.builder().ins().iconst(types::I32, 0);
    let cmp = e.builder().ins().icmp(cc, cmp_result, zero);
    let result = e.builder().ins().uextend(types::I64, cmp);
    e.write_var(inst.a, result);
    Ok(())
}

pub(super) fn str_decode_rune<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().str_decode_rune, "str_decode_rune")?;
    let s = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let call =
        emit_funcref_call_with_effect(e, func, &[s, idx], HelperCallEffect::FrameIndependent);
    let packed = e.builder().inst_results(call)[0];
    // Unpack: packed = (rune << 32) | width
    let rune = e.builder().ins().ushr_imm(packed, 32);
    let width = e.builder().ins().band_imm(packed, 0xFFFFFFFF);
    e.write_var(inst.a, rune);
    e.write_var(inst.a + 1, width);
    Ok(())
}

// =============================================================================
// Map operations
// =============================================================================

pub(super) fn map_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_new, "map_new")?;
    let gc_ptr = e.gc_ptr();
    // b = packed_meta, b+1 = key_rttid
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

pub(super) fn map_len<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_len, "map_len")?;
    let m = e.read_var(inst.b);
    let call = emit_funcref_call_with_effect(e, func, &[m], HelperCallEffect::FrameIndependent);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
    Ok(())
}

/// Helper: create stack slot and store values from consecutive registers
pub(super) fn store_to_stack<'a>(
    e: &mut impl IrEmitter<'a>,
    start_reg: u16,
    slots: usize,
) -> (StackSlot, Value, Value) {
    use cranelift_codegen::ir::{StackSlotData, StackSlotKind};
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

fn emit_return_if_u64_jit_error<'a>(e: &mut impl IrEmitter<'a>, result: Value) {
    let sentinel = e
        .builder()
        .ins()
        .iconst(types::I64, JIT_HELPER_U64_ERROR as i64);
    let is_error = e.builder().ins().icmp(IntCC::Equal, result, sentinel);
    let error_block = e.builder().create_block();
    let ok_block = e.builder().create_block();
    e.builder()
        .ins()
        .brif(is_error, error_block, &[], ok_block, &[]);

    e.builder().switch_to_block(error_block);
    e.builder().seal_block(error_block);
    e.spill_all_vars();
    let jit_error = e
        .builder()
        .ins()
        .iconst(types::I32, JitResult::JitError as i64);
    e.builder().ins().return_(&[jit_error]);

    e.builder().switch_to_block(ok_block);
    e.builder().seal_block(ok_block);
}

pub(super) fn map_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_get, "map_get")?;
    // MapGet: a=dst, b=map, c=meta_slot, key at c+1
    // meta: key_slots<<16 | val_slots<<1 | has_ok
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

    // Create output buffer for val (+ ok flag if needed)
    let out_slots = val_slots + if has_ok { 1 } else { 0 };
    let val_slot = e
        .builder()
        .create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
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

    // Load results to dst registers
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

pub(super) fn map_set<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_set, "map_set")?;
    // MapSet: a=map, b=meta_slot, c=val_start, key at b+1
    // meta: key_slots<<8 | val_slots
    let layout = e.map_set_layout(inst).ok_or(JitError::MissingJitLayout {
        pc: e.current_pc(),
        opcode: inst.opcode(),
        layout: "MapSet",
    })?;
    let key_slots = layout.key_slots as usize;
    let val_slots = layout.val_slots as usize;

    let m = e.read_var(inst.a);

    // nil map write panics (Go semantics)
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

    // Check if vo_map_set returned panic (unhashable interface key)
    let is_panic = e.builder().ins().icmp(IntCC::NotEqual, result, zero);
    emit_runtime_trap_if(e, is_panic, JitRuntimeTrapKind::UnhashableType, None, None);
    Ok(())
}

pub(super) fn map_delete<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_delete, "map_delete")?;
    // MapDelete: a=map, b=meta_slot (=key_slots), key at b+1
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

pub(super) const MAP_ITER_SLOTS: usize = vo_runtime::objects::map::MAP_ITER_SLOTS;
pub(super) const MAP_ITER_BYTES: u32 = (MAP_ITER_SLOTS * 8) as u32;

pub(super) fn map_iter_init<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_iter_init, "map_iter_init")?;
    let m = e.read_var(inst.b);
    let iter_slot = e
        .builder()
        .create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
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

pub(super) fn map_iter_next<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().map_iter_next, "map_iter_next")?;
    let key_slots = inst.map_iter_key_slots() as usize;
    let val_slots = inst.map_iter_val_slots() as usize;

    let iter_slot = e
        .builder()
        .create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            MAP_ITER_BYTES,
            8,
        ));
    let key_slot = e
        .builder()
        .create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (key_slots.max(1) * 8) as u32,
            8,
        ));
    let val_slot = e
        .builder()
        .create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
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

    // Copy key to VM stack
    for i in 0..key_slots {
        let val = e
            .builder()
            .ins()
            .stack_load(types::I64, key_slot, (i * 8) as i32);
        e.write_var(inst.a + i as u16, val);
    }
    // Copy val to VM stack (at key_slot + key_slots)
    for i in 0..val_slots {
        let val = e
            .builder()
            .ins()
            .stack_load(types::I64, val_slot, (i * 8) as i32);
        e.write_var(inst.a + key_slots as u16 + i as u16, val);
    }
    // Write ok flag to inst.c
    e.write_var(inst.c, ok);
    Ok(())
}
