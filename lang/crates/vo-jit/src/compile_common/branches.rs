use vo_runtime::instruction::{Instruction, Opcode};

use super::driver::ControlPolicy;
use crate::JitError;

pub(crate) fn jump_targets_for_policy(
    code: &[Instruction],
    policy: ControlPolicy,
) -> Result<Vec<usize>, JitError> {
    let mut targets = Vec::new();
    if let ControlPolicy::LoopOsr { begin_pc, .. } = policy {
        targets.push(begin_pc);
    }

    for pc in policy.pc_range() {
        let inst = code.get(pc).ok_or(JitError::InvalidOsrTarget(pc))?;
        match inst.opcode() {
            Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot => {
                let target =
                    checked_branch_target(policy.code_len(), pc, inst.imm32(), inst.opcode())?;
                if policy.compiled_target(target) {
                    targets.push(target);
                }
            }
            Opcode::ForLoop => {
                let target = checked_forloop_target(policy.code_len(), pc, inst)?;
                if policy.compiled_target(target) {
                    targets.push(target);
                }
            }
            _ => {}
        }
    }
    Ok(targets)
}

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
