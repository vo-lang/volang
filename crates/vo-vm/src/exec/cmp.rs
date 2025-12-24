//! Comparison instructions: EqI, NeI, LtI, LeI, GtI, GeI, EqF, NeF, LtF, LeF, GtF, GeF, EqRef, NeRef, IsNil

use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_eq_i(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as i64;
    let b = fiber.read_reg(inst.c) as i64;
    fiber.write_reg(inst.a, (a == b) as u64);
}

#[inline]
pub fn exec_ne_i(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as i64;
    let b = fiber.read_reg(inst.c) as i64;
    fiber.write_reg(inst.a, (a != b) as u64);
}

#[inline]
pub fn exec_lt_i(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as i64;
    let b = fiber.read_reg(inst.c) as i64;
    fiber.write_reg(inst.a, (a < b) as u64);
}

#[inline]
pub fn exec_le_i(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as i64;
    let b = fiber.read_reg(inst.c) as i64;
    fiber.write_reg(inst.a, (a <= b) as u64);
}

#[inline]
pub fn exec_gt_i(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as i64;
    let b = fiber.read_reg(inst.c) as i64;
    fiber.write_reg(inst.a, (a > b) as u64);
}

#[inline]
pub fn exec_ge_i(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as i64;
    let b = fiber.read_reg(inst.c) as i64;
    fiber.write_reg(inst.a, (a >= b) as u64);
}

#[inline]
pub fn exec_eq_f(fiber: &mut Fiber, inst: &Instruction) {
    let a = f64::from_bits(fiber.read_reg(inst.b));
    let b = f64::from_bits(fiber.read_reg(inst.c));
    fiber.write_reg(inst.a, (a == b) as u64);
}

#[inline]
pub fn exec_ne_f(fiber: &mut Fiber, inst: &Instruction) {
    let a = f64::from_bits(fiber.read_reg(inst.b));
    let b = f64::from_bits(fiber.read_reg(inst.c));
    fiber.write_reg(inst.a, (a != b) as u64);
}

#[inline]
pub fn exec_lt_f(fiber: &mut Fiber, inst: &Instruction) {
    let a = f64::from_bits(fiber.read_reg(inst.b));
    let b = f64::from_bits(fiber.read_reg(inst.c));
    fiber.write_reg(inst.a, (a < b) as u64);
}

#[inline]
pub fn exec_le_f(fiber: &mut Fiber, inst: &Instruction) {
    let a = f64::from_bits(fiber.read_reg(inst.b));
    let b = f64::from_bits(fiber.read_reg(inst.c));
    fiber.write_reg(inst.a, (a <= b) as u64);
}

#[inline]
pub fn exec_gt_f(fiber: &mut Fiber, inst: &Instruction) {
    let a = f64::from_bits(fiber.read_reg(inst.b));
    let b = f64::from_bits(fiber.read_reg(inst.c));
    fiber.write_reg(inst.a, (a > b) as u64);
}

#[inline]
pub fn exec_ge_f(fiber: &mut Fiber, inst: &Instruction) {
    let a = f64::from_bits(fiber.read_reg(inst.b));
    let b = f64::from_bits(fiber.read_reg(inst.c));
    fiber.write_reg(inst.a, (a >= b) as u64);
}

#[inline]
pub fn exec_eq_ref(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b);
    let b = fiber.read_reg(inst.c);
    fiber.write_reg(inst.a, (a == b) as u64);
}

#[inline]
pub fn exec_ne_ref(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b);
    let b = fiber.read_reg(inst.c);
    fiber.write_reg(inst.a, (a != b) as u64);
}

#[inline]
pub fn exec_is_nil(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b);
    fiber.write_reg(inst.a, (a == 0) as u64);
}
