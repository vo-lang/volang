//! Global variable instructions: GlobalGet, GlobalGetN, GlobalSet, GlobalSetN

use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_global_get(fiber: &mut Fiber, inst: &Instruction, globals: &[u64]) {
    let val = globals[inst.b as usize];
    fiber.write_reg(inst.a, val);
}

#[inline]
pub fn exec_global_get_n(fiber: &mut Fiber, inst: &Instruction, globals: &[u64]) {
    let count = inst.flags as usize;
    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let dst_start = bp + inst.a as usize;
    let src_start = inst.b as usize;

    for i in 0..count {
        fiber.stack[dst_start + i] = globals[src_start + i];
    }
}

#[inline]
pub fn exec_global_set(fiber: &mut Fiber, inst: &Instruction, globals: &mut [u64]) {
    let val = fiber.read_reg(inst.b);
    globals[inst.a as usize] = val;
}

#[inline]
pub fn exec_global_set_n(fiber: &mut Fiber, inst: &Instruction, globals: &mut [u64]) {
    let count = inst.flags as usize;
    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let src_start = bp + inst.b as usize;
    let dst_start = inst.a as usize;

    for i in 0..count {
        globals[dst_start + i] = fiber.stack[src_start + i];
    }
}
