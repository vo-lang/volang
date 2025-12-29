//! String instructions: StrNew, StrConcat, StrSlice

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::string;

use crate::bytecode::Constant;
use crate::instruction::Instruction;

#[inline]
pub fn exec_str_new(stack: &mut [u64], bp: usize, inst: &Instruction, constants: &[Constant], gc: &mut Gc) {
    if let Constant::String(s) = &constants[inst.b as usize] {
        let str_ref = string::from_rust_str(gc, s);
        stack[bp + inst.a as usize] = str_ref as u64;
    } else {
        stack[bp + inst.a as usize] = 0;
    }
}

#[inline]
pub fn exec_str_concat(stack: &mut [u64], bp: usize, inst: &Instruction, gc: &mut Gc) {
    let a = stack[bp + inst.b as usize] as GcRef;
    let b = stack[bp + inst.c as usize] as GcRef;
    let result = string::concat(gc, a, b);
    stack[bp + inst.a as usize] = result as u64;
}

#[inline]
pub fn exec_str_slice(stack: &mut [u64], bp: usize, inst: &Instruction, gc: &mut Gc) {
    let s = stack[bp + inst.b as usize] as GcRef;
    let lo = stack[bp + inst.c as usize] as usize;
    let hi = stack[bp + inst.c as usize + 1] as usize;
    let result = string::slice_of(gc, s, lo, hi);
    stack[bp + inst.a as usize] = result as u64;
}
