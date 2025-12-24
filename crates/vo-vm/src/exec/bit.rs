//! Bitwise and logical instructions: And, Or, Xor, Not, Shl, ShrS, ShrU, BoolNot

use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_and(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b);
    let b = fiber.read_reg(inst.c);
    fiber.write_reg(inst.a, a & b);
}

#[inline]
pub fn exec_or(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b);
    let b = fiber.read_reg(inst.c);
    fiber.write_reg(inst.a, a | b);
}

#[inline]
pub fn exec_xor(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b);
    let b = fiber.read_reg(inst.c);
    fiber.write_reg(inst.a, a ^ b);
}

#[inline]
pub fn exec_not(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b);
    fiber.write_reg(inst.a, !a);
}

#[inline]
pub fn exec_shl(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b);
    let b = fiber.read_reg(inst.c) as u32;
    fiber.write_reg(inst.a, a.wrapping_shl(b));
}

#[inline]
pub fn exec_shr_s(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as i64;
    let b = fiber.read_reg(inst.c) as u32;
    fiber.write_reg(inst.a, a.wrapping_shr(b) as u64);
}

#[inline]
pub fn exec_shr_u(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b);
    let b = fiber.read_reg(inst.c) as u32;
    fiber.write_reg(inst.a, a.wrapping_shr(b));
}

#[inline]
pub fn exec_bool_not(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b);
    fiber.write_reg(inst.a, (a == 0) as u64);
}
