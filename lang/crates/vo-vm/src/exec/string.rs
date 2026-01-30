//! String instructions: StrNew, StrConcat, StrSlice

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::string;
use vo_runtime::slot::Slot;

use crate::bytecode::Constant;
use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

#[inline]
pub fn exec_str_new(stack: *mut Slot, bp: usize, inst: &Instruction, constants: &[Constant], gc: &mut Gc) {
    if let Constant::String(s) = &constants[inst.b as usize] {
        let str_ref = string::from_rust_str(gc, s);
        stack_set(stack, bp + inst.a as usize, str_ref as u64);
    } else {
        stack_set(stack, bp + inst.a as usize, 0);
    }
}

#[inline]
pub fn exec_str_concat(stack: *mut Slot, bp: usize, inst: &Instruction, gc: &mut Gc) {
    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
    let result = string::concat(gc, a, b);
    stack_set(stack, bp + inst.a as usize, result as u64);
}

#[inline]
pub fn exec_str_slice(stack: *mut Slot, bp: usize, inst: &Instruction, gc: &mut Gc) {
    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
    let lo = stack_get(stack, bp + inst.c as usize) as usize;
    let hi = stack_get(stack, bp + inst.c as usize + 1) as usize;
    let result = string::slice_of(gc, s, lo, hi);
    stack_set(stack, bp + inst.a as usize, result as u64);
}
