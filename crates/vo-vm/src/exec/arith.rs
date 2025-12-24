//! Arithmetic instructions: AddI, SubI, MulI, DivI, ModI, NegI, AddF, SubF, MulF, DivF, NegF

use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_add_i(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as i64;
    let b = fiber.read_reg(inst.c) as i64;
    fiber.write_reg(inst.a, a.wrapping_add(b) as u64);
}

#[inline]
pub fn exec_sub_i(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as i64;
    let b = fiber.read_reg(inst.c) as i64;
    fiber.write_reg(inst.a, a.wrapping_sub(b) as u64);
}

#[inline]
pub fn exec_mul_i(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as i64;
    let b = fiber.read_reg(inst.c) as i64;
    fiber.write_reg(inst.a, a.wrapping_mul(b) as u64);
}

#[inline]
pub fn exec_div_i(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as i64;
    let b = fiber.read_reg(inst.c) as i64;
    fiber.write_reg(inst.a, a.wrapping_div(b) as u64);
}

#[inline]
pub fn exec_mod_i(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as i64;
    let b = fiber.read_reg(inst.c) as i64;
    fiber.write_reg(inst.a, a.wrapping_rem(b) as u64);
}

#[inline]
pub fn exec_neg_i(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as i64;
    fiber.write_reg(inst.a, a.wrapping_neg() as u64);
}

#[inline]
pub fn exec_add_f(fiber: &mut Fiber, inst: &Instruction) {
    let a = f64::from_bits(fiber.read_reg(inst.b));
    let b = f64::from_bits(fiber.read_reg(inst.c));
    fiber.write_reg(inst.a, (a + b).to_bits());
}

#[inline]
pub fn exec_sub_f(fiber: &mut Fiber, inst: &Instruction) {
    let a = f64::from_bits(fiber.read_reg(inst.b));
    let b = f64::from_bits(fiber.read_reg(inst.c));
    fiber.write_reg(inst.a, (a - b).to_bits());
}

#[inline]
pub fn exec_mul_f(fiber: &mut Fiber, inst: &Instruction) {
    let a = f64::from_bits(fiber.read_reg(inst.b));
    let b = f64::from_bits(fiber.read_reg(inst.c));
    fiber.write_reg(inst.a, (a * b).to_bits());
}

#[inline]
pub fn exec_div_f(fiber: &mut Fiber, inst: &Instruction) {
    let a = f64::from_bits(fiber.read_reg(inst.b));
    let b = f64::from_bits(fiber.read_reg(inst.c));
    fiber.write_reg(inst.a, (a / b).to_bits());
}

#[inline]
pub fn exec_neg_f(fiber: &mut Fiber, inst: &Instruction) {
    let a = f64::from_bits(fiber.read_reg(inst.b));
    fiber.write_reg(inst.a, (-a).to_bits());
}
