//! Array instructions: ArrayNew, ArrayGet, ArraySet, ArrayLen

use vo_common_core::types::ValueMeta;
use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::array;

use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_array_new(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let meta_raw = fiber.read_reg(inst.b) as u32;
    let elem_meta = ValueMeta::from_raw(meta_raw);
    let len = fiber.read_reg(inst.c) as usize;
    let elem_slots = inst.flags as usize;
    let arr = array::create(gc, elem_meta, elem_slots, len);
    fiber.write_reg(inst.a, arr as u64);
}

#[inline]
pub fn exec_array_get(fiber: &mut Fiber, inst: &Instruction) {
    let arr = fiber.read_reg(inst.b) as GcRef;
    let idx = fiber.read_reg(inst.c) as usize;
    let elem_slots = inst.flags as usize;
    let offset = idx * elem_slots;

    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let dst_start = bp + inst.a as usize;

    for i in 0..elem_slots {
        fiber.stack[dst_start + i] = array::get(arr, offset + i);
    }
}

#[inline]
pub fn exec_array_set(fiber: &mut Fiber, inst: &Instruction) {
    let arr = fiber.read_reg(inst.a) as GcRef;
    let idx = fiber.read_reg(inst.b) as usize;
    let elem_slots = inst.flags as usize;
    let offset = idx * elem_slots;

    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let src_start = bp + inst.c as usize;

    for i in 0..elem_slots {
        array::set(arr, offset + i, fiber.stack[src_start + i]);
    }
}

#[inline]
pub fn exec_array_len(fiber: &mut Fiber, inst: &Instruction) {
    let arr = fiber.read_reg(inst.b) as GcRef;
    let len = if arr.is_null() { 0 } else { array::len(arr) };
    fiber.write_reg(inst.a, len as u64);
}
