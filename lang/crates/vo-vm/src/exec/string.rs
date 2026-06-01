//! String instructions: StrNew, StrConcat, StrSlice

#[cfg(not(feature = "std"))]
use alloc::{format, string::String};
#[cfg(feature = "std")]
use std::string::String;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::string;
use vo_runtime::slot::Slot;

use crate::bytecode::Constant;
use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

#[inline]
pub fn exec_str_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    constants: &[Constant],
    gc: &mut Gc,
) -> Result<(), String> {
    let constant = constants.get(inst.b as usize).ok_or_else(|| {
        format!(
            "StrNew constant index {} out of bounds for {} constants",
            inst.b,
            constants.len()
        )
    })?;
    let Constant::String(s) = constant else {
        return Err(format!(
            "StrNew constant {} expected string, got {constant:?}",
            inst.b
        ));
    };
    let str_ref = string::from_rust_str(gc, s);
    stack_set(stack, bp + inst.a as usize, str_ref as u64);
    Ok(())
}

#[inline]
pub fn exec_str_concat(stack: *mut Slot, bp: usize, inst: &Instruction, gc: &mut Gc) {
    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
    let result = string::concat(gc, a, b);
    stack_set(stack, bp + inst.a as usize, result as u64);
}

#[inline]
pub fn exec_str_slice(stack: *mut Slot, bp: usize, inst: &Instruction, gc: &mut Gc) -> bool {
    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
    let lo = stack_get(stack, bp + inst.c as usize) as usize;
    let hi = stack_get(stack, bp + inst.c as usize + 1) as usize;
    match string::slice_of(gc, s, lo, hi) {
        Some(result) => {
            stack_set(stack, bp + inst.a as usize, result as u64);
            true
        }
        None => false,
    }
}
