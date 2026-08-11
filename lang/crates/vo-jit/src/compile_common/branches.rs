use vo_runtime::instruction::{Instruction, Opcode};

use crate::JitError;

pub(crate) fn checked_branch_target(
    code_len: usize,
    pc: usize,
    offset: i32,
    opcode: Opcode,
) -> Result<usize, JitError> {
    let target = pc as i64 + offset as i64;
    if target >= 0 && (target as usize) < code_len {
        Ok(target as usize)
    } else {
        Err(JitError::Internal(format!(
            "{opcode:?} at pc {pc} targets invalid pc {target} (code_len={code_len})",
        )))
    }
}

pub(crate) fn checked_forloop_target(
    code_len: usize,
    pc: usize,
    inst: &Instruction,
) -> Result<usize, JitError> {
    let target = pc as i64 + 1 + i64::from(inst.c as i16);
    if target >= 0 && (target as usize) < code_len {
        Ok(target as usize)
    } else {
        Err(JitError::Internal(format!(
            "ForLoop at pc {pc} targets invalid pc {target} (code_len={code_len})",
        )))
    }
}
