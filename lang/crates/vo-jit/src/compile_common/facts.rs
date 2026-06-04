use std::collections::{HashMap, HashSet};

use crate::JitError;

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
