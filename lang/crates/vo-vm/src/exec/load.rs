//! Load instructions: Nop, LoadConst

use vo_runtime::slot::Slot;
use crate::bytecode::Constant;
use crate::instruction::Instruction;
use crate::vm::helpers::stack_set;

#[inline]
pub fn exec_load_const(stack: *mut Slot, bp: usize, inst: &Instruction, constants: &[Constant]) {
    let val = match &constants[inst.b as usize] {
        Constant::Nil => 0,
        Constant::Bool(b) => *b as u64,
        Constant::Int(i) => *i as u64,
        Constant::Float(f) => f.to_bits(),
        Constant::String(_) => 0, // String handled separately via StrNew
    };
    stack_set(stack, bp + inst.a as usize, val);
}
