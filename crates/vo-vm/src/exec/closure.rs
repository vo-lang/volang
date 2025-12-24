//! Closure instructions: ClosureNew, ClosureGet, ClosureSet

use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::closure;

use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_closure_new(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let func_id = (inst.b as u32) | ((inst.flags as u32) << 16);
    let capture_count = inst.c as usize;
    let c = closure::create(gc, func_id, capture_count);
    fiber.write_reg(inst.a, c as u64);
}

#[inline]
pub fn exec_closure_get(fiber: &mut Fiber, inst: &Instruction) {
    let c = fiber.read_reg(0) as GcRef;
    let val = closure::get_capture(c, inst.b as usize);
    fiber.write_reg(inst.a, val);
}

#[inline]
pub fn exec_closure_set(fiber: &mut Fiber, inst: &Instruction) {
    let c = fiber.read_reg(0) as GcRef;
    let val = fiber.read_reg(inst.b);
    closure::set_capture(c, inst.a as usize, val);
}
