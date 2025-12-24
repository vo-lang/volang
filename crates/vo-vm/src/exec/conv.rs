//! Type conversion instructions: ConvI2F, ConvF2I, ConvI32I64, ConvI64I32

use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_conv_i2f(fiber: &mut Fiber, inst: &Instruction) {
    let i = fiber.read_reg(inst.b) as i64;
    let f = i as f64;
    fiber.write_reg(inst.a, f.to_bits());
}

#[inline]
pub fn exec_conv_f2i(fiber: &mut Fiber, inst: &Instruction) {
    let f = f64::from_bits(fiber.read_reg(inst.b));
    let i = f as i64;
    fiber.write_reg(inst.a, i as u64);
}

#[inline]
pub fn exec_conv_i32_i64(fiber: &mut Fiber, inst: &Instruction) {
    let i32_val = fiber.read_reg(inst.b) as i32;
    let i64_val = i32_val as i64;
    fiber.write_reg(inst.a, i64_val as u64);
}

#[inline]
pub fn exec_conv_i64_i32(fiber: &mut Fiber, inst: &Instruction) {
    let i64_val = fiber.read_reg(inst.b) as i64;
    let i32_val = i64_val as i32;
    fiber.write_reg(inst.a, i32_val as u64);
}
