//! String instructions: StrNew, StrLen, StrIndex, StrConcat, StrSlice, StrEq, StrNe, StrLt, StrLe, StrGt, StrGe

use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::string;

use crate::bytecode::Constant;
use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_str_new(fiber: &mut Fiber, inst: &Instruction, constants: &[Constant], gc: &mut Gc) {
    if let Constant::String(s) = &constants[inst.b as usize] {
        let str_ref = string::from_rust_str(gc, s);
        fiber.write_reg(inst.a, str_ref as u64);
    } else {
        fiber.write_reg(inst.a, 0);
    }
}

#[inline]
pub fn exec_str_len(fiber: &mut Fiber, inst: &Instruction) {
    let s = fiber.read_reg(inst.b) as GcRef;
    let len = if s.is_null() { 0 } else { string::len(s) };
    fiber.write_reg(inst.a, len as u64);
}

#[inline]
pub fn exec_str_index(fiber: &mut Fiber, inst: &Instruction) {
    let s = fiber.read_reg(inst.b) as GcRef;
    let idx = fiber.read_reg(inst.c) as usize;
    let byte = string::index(s, idx);
    fiber.write_reg(inst.a, byte as u64);
}

#[inline]
pub fn exec_str_concat(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let a = fiber.read_reg(inst.b) as GcRef;
    let b = fiber.read_reg(inst.c) as GcRef;
    let result = string::concat(gc, a, b);
    fiber.write_reg(inst.a, result as u64);
}

#[inline]
pub fn exec_str_slice(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let s = fiber.read_reg(inst.b) as GcRef;
    let lo = fiber.read_reg(inst.c) as usize;
    let hi = fiber.read_reg(inst.c + 1) as usize;
    let result = string::slice_of(gc, s, lo, hi);
    fiber.write_reg(inst.a, result as u64);
}

#[inline]
pub fn exec_str_eq(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as GcRef;
    let b = fiber.read_reg(inst.c) as GcRef;
    fiber.write_reg(inst.a, string::eq(a, b) as u64);
}

#[inline]
pub fn exec_str_ne(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as GcRef;
    let b = fiber.read_reg(inst.c) as GcRef;
    fiber.write_reg(inst.a, string::ne(a, b) as u64);
}

#[inline]
pub fn exec_str_lt(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as GcRef;
    let b = fiber.read_reg(inst.c) as GcRef;
    fiber.write_reg(inst.a, string::lt(a, b) as u64);
}

#[inline]
pub fn exec_str_le(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as GcRef;
    let b = fiber.read_reg(inst.c) as GcRef;
    fiber.write_reg(inst.a, string::le(a, b) as u64);
}

#[inline]
pub fn exec_str_gt(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as GcRef;
    let b = fiber.read_reg(inst.c) as GcRef;
    fiber.write_reg(inst.a, string::gt(a, b) as u64);
}

#[inline]
pub fn exec_str_ge(fiber: &mut Fiber, inst: &Instruction) {
    let a = fiber.read_reg(inst.b) as GcRef;
    let b = fiber.read_reg(inst.c) as GcRef;
    fiber.write_reg(inst.a, string::ge(a, b) as u64);
}
