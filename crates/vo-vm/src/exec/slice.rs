//! Slice instructions: SliceNew, SliceGet, SliceSet, SliceLen, SliceCap, SliceSlice, SliceAppend

use vo_common_core::types::ValueMeta;
use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::slice;

use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_slice_new(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let meta_raw = fiber.read_reg(inst.b) as u32;
    let elem_meta = ValueMeta::from_raw(meta_raw);
    let len = fiber.read_reg(inst.c) as usize;
    let cap = fiber.read_reg(inst.c + 1) as usize;
    let elem_slots = inst.flags as usize;
    let s = slice::create(gc, elem_meta, elem_slots, len, cap);
    fiber.write_reg(inst.a, s as u64);
}

#[inline]
pub fn exec_slice_get(fiber: &mut Fiber, inst: &Instruction) {
    let s = fiber.read_reg(inst.b) as GcRef;
    let idx = fiber.read_reg(inst.c) as usize;
    let elem_slots = inst.flags as usize;
    let offset = idx * elem_slots;

    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let dst_start = bp + inst.a as usize;

    for i in 0..elem_slots {
        fiber.stack[dst_start + i] = slice::get(s, offset + i);
    }
}

#[inline]
pub fn exec_slice_set(fiber: &mut Fiber, inst: &Instruction) {
    let s = fiber.read_reg(inst.a) as GcRef;
    let idx = fiber.read_reg(inst.b) as usize;
    let elem_slots = inst.flags as usize;
    let offset = idx * elem_slots;

    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let src_start = bp + inst.c as usize;

    for i in 0..elem_slots {
        slice::set(s, offset + i, fiber.stack[src_start + i]);
    }
}

#[inline]
pub fn exec_slice_len(fiber: &mut Fiber, inst: &Instruction) {
    let s = fiber.read_reg(inst.b) as GcRef;
    let len = if s.is_null() { 0 } else { slice::len(s) };
    fiber.write_reg(inst.a, len as u64);
}

#[inline]
pub fn exec_slice_cap(fiber: &mut Fiber, inst: &Instruction) {
    let s = fiber.read_reg(inst.b) as GcRef;
    let cap = if s.is_null() { 0 } else { slice::cap(s) };
    fiber.write_reg(inst.a, cap as u64);
}

#[inline]
pub fn exec_slice_slice(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let s = fiber.read_reg(inst.b) as GcRef;
    let lo = fiber.read_reg(inst.c) as usize;
    let hi = fiber.read_reg(inst.c + 1) as usize;
    
    // flags: bit0 = 1 means input is array, 0 means slice
    let is_array = (inst.flags & 1) != 0;
    let result = if is_array {
        // Input is array: create slice from array range
        slice::from_array_range(gc, s, lo, hi - lo, hi - lo)
    } else {
        // Input is slice: use slice_of
        slice::slice_of(gc, s, lo, hi)
    };
    fiber.write_reg(inst.a, result as u64);
}

#[inline]
pub fn exec_slice_append(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let s = fiber.read_reg(inst.b) as GcRef;
    let elem_slots = inst.flags as usize;
    let elem_meta = if s.is_null() {
        ValueMeta::from_raw(0)
    } else {
        slice::elem_meta(s)
    };

    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let src_start = bp + inst.c as usize;

    let val: &[u64] = &fiber.stack[src_start..src_start + elem_slots];
    let result = slice::append(gc, elem_meta, elem_slots, s, val);
    fiber.write_reg(inst.a, result as u64);
}
