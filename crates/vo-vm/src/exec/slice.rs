//! Slice instructions: SliceNew, SliceSlice, SliceAppend

use vo_runtime::ValueMeta;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::slice;

use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_slice_new(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let meta_raw = fiber.read_reg(inst.b) as u32;
    let elem_meta = ValueMeta::from_raw(meta_raw);
    let len = fiber.read_reg(inst.c) as usize;
    let cap = fiber.read_reg(inst.c + 1) as usize;
    let elem_bytes = inst.flags as usize; // flags is now elem_bytes
    let s = slice::create(gc, elem_meta, elem_bytes, len, cap);
    fiber.write_reg(inst.a, s as u64);
}

/// SliceSlice: a[lo:hi] or a[lo:hi:max]
/// flags: bit0 = input is array, bit1 = has max (three-index slice)
#[inline]
pub fn exec_slice_slice(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let s = fiber.read_reg(inst.b) as GcRef;
    let lo = fiber.read_reg(inst.c) as usize;
    let hi = fiber.read_reg(inst.c + 1) as usize;
    
    let is_array = (inst.flags & 0b01) != 0;
    let has_max = (inst.flags & 0b10) != 0;
    
    let result = if is_array {
        // Input is array: create slice from array range
        let cap = if has_max {
            fiber.read_reg(inst.c + 2) as usize - lo
        } else {
            hi - lo
        };
        slice::from_array_range(gc, s, lo, hi - lo, cap)
    } else {
        // Input is slice
        if has_max {
            let max = fiber.read_reg(inst.c + 2) as usize;
            slice::slice_of_with_cap(gc, s, lo, hi, max)
        } else {
            slice::slice_of(gc, s, lo, hi)
        }
    };
    fiber.write_reg(inst.a, result as u64);
}

#[inline]
pub fn exec_slice_append(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let s = fiber.read_reg(inst.b) as GcRef;
    // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
    let elem_bytes = match inst.flags {
        0x81 => 1,   // int8
        0x82 => 2,   // int16
        0x84 | 0x44 => 4,   // int32 or float32
        f => f as usize,
    };
    let elem_slots = (elem_bytes + 7) / 8;
    
    // c points to meta_and_elem: [elem_meta (1 slot)][elem (elem_slots)]
    // Read elem_meta from c, elem from c+1
    let elem_meta = ValueMeta::from_raw(fiber.read_reg(inst.c) as u32);
    
    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let src_start = bp + inst.c as usize + 1; // elem starts at c+1

    let val: &[u64] = &fiber.stack[src_start..src_start + elem_slots];
    let result = slice::append(gc, elem_meta, elem_bytes, s, val);
    fiber.write_reg(inst.a, result as u64);
}
