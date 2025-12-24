//! Copy instructions: Copy, CopyN, SlotGet, SlotSet, SlotGetN, SlotSetN

use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_copy(fiber: &mut Fiber, inst: &Instruction) {
    let val = fiber.read_reg(inst.b);
    fiber.write_reg(inst.a, val);
}

#[inline]
pub fn exec_copy_n(fiber: &mut Fiber, inst: &Instruction) {
    let count = inst.c as usize;
    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let src_start = bp + inst.b as usize;
    let dst_start = bp + inst.a as usize;

    for i in 0..count {
        let val = fiber.stack[src_start + i];
        fiber.stack[dst_start + i] = val;
    }
}

#[inline]
pub fn exec_slot_get(fiber: &mut Fiber, inst: &Instruction) {
    let idx = fiber.read_reg(inst.c) as usize;
    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let val = fiber.stack[bp + inst.b as usize + idx];
    fiber.write_reg(inst.a, val);
}

#[inline]
pub fn exec_slot_set(fiber: &mut Fiber, inst: &Instruction) {
    let idx = fiber.read_reg(inst.b) as usize;
    let val = fiber.read_reg(inst.c);
    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    fiber.stack[bp + inst.a as usize + idx] = val;
}

#[inline]
pub fn exec_slot_get_n(fiber: &mut Fiber, inst: &Instruction) {
    let elem_slots = inst.flags as usize;
    let idx = fiber.read_reg(inst.c) as usize;
    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let src_start = bp + inst.b as usize + idx * elem_slots;
    let dst_start = bp + inst.a as usize;

    for i in 0..elem_slots {
        fiber.stack[dst_start + i] = fiber.stack[src_start + i];
    }
}

#[inline]
pub fn exec_slot_set_n(fiber: &mut Fiber, inst: &Instruction) {
    let elem_slots = inst.flags as usize;
    let idx = fiber.read_reg(inst.b) as usize;
    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let dst_start = bp + inst.a as usize + idx * elem_slots;
    let src_start = bp + inst.c as usize;

    for i in 0..elem_slots {
        fiber.stack[dst_start + i] = fiber.stack[src_start + i];
    }
}
