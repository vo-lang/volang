//! Load instructions: Nop, LoadConst

#[cfg(not(feature = "std"))]
use alloc::{format, string::String};
#[cfg(feature = "std")]
use std::string::String;

use crate::bytecode::Constant;
use crate::instruction::Instruction;
use crate::vm::helpers::stack_set;
use vo_runtime::slot::Slot;

#[inline]
pub fn exec_load_const(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    constants: &[Constant],
) -> Result<(), String> {
    let constant = constants.get(inst.b as usize).ok_or_else(|| {
        format!(
            "LoadConst constant index {} out of bounds for {} constants",
            inst.b,
            constants.len()
        )
    })?;
    let val = match constant.clone() {
        Constant::Nil => 0,
        Constant::Bool(b) => b as u64,
        Constant::Int(i) => i as u64,
        Constant::Float(f) => f.to_bits(),
        Constant::String(_) => 0, // String handled separately via StrNew
    };
    stack_set(stack, bp + inst.a as usize, val);
    Ok(())
}
