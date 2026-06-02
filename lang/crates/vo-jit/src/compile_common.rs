use std::collections::{HashMap, HashSet};

use cranelift_frontend::{FunctionBuilder, Variable};
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::{Instruction, Opcode};

use crate::JitError;

pub(crate) fn declare_variables(
    builder: &mut FunctionBuilder<'_>,
    func_def: &FunctionDef,
) -> Vec<Variable> {
    crate::translator::declare_variables(
        builder,
        func_def.local_slots as usize,
        &func_def.slot_types,
    )
}

#[inline]
pub(crate) fn is_float_slot(func_def: &FunctionDef, slot: u16) -> bool {
    crate::translator::is_float_slot(&func_def.slot_types, slot)
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

pub(crate) fn clear_flow_facts(
    checked_non_nil: &mut HashSet<u16>,
    reg_consts: &mut HashMap<u16, i64>,
) {
    checked_non_nil.clear();
    reg_consts.clear();
}

pub(crate) fn apply_reg_const_facts(
    reg_consts: &mut HashMap<u16, i64>,
    reg_const_facts: &[HashMap<u16, i64>],
    pc: usize,
) -> Result<(), JitError> {
    *reg_consts = reg_const_facts.get(pc).cloned().ok_or_else(|| {
        JitError::Internal(format!("missing per-PC register-constant facts at pc {pc}"))
    })?;
    Ok(())
}
