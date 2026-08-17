//! String instructions: StrNew, StrConcat, StrSlice

#[cfg(not(feature = "std"))]
use alloc::format;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::string;
use vo_runtime::slot::Slot;

use crate::bytecode::Constant;
use crate::exec::InstructionError;
use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

#[inline]
pub fn exec_str_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    constants: &[Constant],
    gc: &mut Gc,
) -> Result<(), InstructionError> {
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
        )
        .into());
    };
    let str_ref = string::try_from_rust_str(gc, s)?;
    stack_set(stack, bp + inst.a as usize, str_ref as u64);
    Ok(())
}

#[inline]
pub fn exec_str_concat(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
) -> Result<(), InstructionError> {
    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
    // Safety: bytecode verification establishes live string operands.
    let result = unsafe { string::try_concat(gc, a, b) }?;
    stack_set(stack, bp + inst.a as usize, result as u64);
    Ok(())
}

#[inline]
pub fn exec_str_slice(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
) -> Result<bool, InstructionError> {
    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
    let Ok(lo) = usize::try_from(stack_get(stack, bp + inst.c as usize)) else {
        return Ok(false);
    };
    let Ok(hi) = usize::try_from(stack_get(stack, bp + inst.c as usize + 1)) else {
        return Ok(false);
    };
    // Safety: bytecode verification establishes a live string operand.
    match unsafe { string::try_slice_of(gc, s, lo, hi) }? {
        Some(result) => {
            stack_set(stack, bp + inst.a as usize, result as u64);
            Ok(true)
        }
        None => Ok(false),
    }
}
