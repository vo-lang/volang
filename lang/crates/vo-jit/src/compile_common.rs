use std::collections::{HashMap, HashSet};

use cranelift_codegen::ir::{Block, InstBuilder};
use cranelift_frontend::{FunctionBuilder, Variable};
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::{Instruction, Opcode};

use crate::JitError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ControlPolicy {
    FullFunction {
        code_len: usize,
    },
    LoopOsr {
        begin_pc: usize,
        end_pc: usize,
        exit_pc: usize,
        code_len: usize,
    },
}

impl ControlPolicy {
    pub(crate) fn full_function(code_len: usize) -> Self {
        Self::FullFunction { code_len }
    }

    pub(crate) fn loop_osr(
        begin_pc: usize,
        end_pc: usize,
        exit_pc: usize,
        code_len: usize,
    ) -> Self {
        Self::LoopOsr {
            begin_pc,
            end_pc,
            exit_pc,
            code_len,
        }
    }

    pub(crate) fn pc_range(self) -> std::ops::Range<usize> {
        match self {
            Self::FullFunction { code_len } => 0..code_len,
            Self::LoopOsr {
                begin_pc, end_pc, ..
            } => begin_pc..(end_pc + 1),
        }
    }

    pub(crate) fn compiled_target(self, target: usize) -> bool {
        match self {
            Self::FullFunction { .. } => true,
            Self::LoopOsr {
                begin_pc, end_pc, ..
            } => target >= begin_pc && target <= end_pc,
        }
    }

    fn code_len(self) -> usize {
        match self {
            Self::FullFunction { code_len } | Self::LoopOsr { code_len, .. } => code_len,
        }
    }
}

pub(crate) trait CompileDriver {
    fn control_policy(&self) -> ControlPolicy;
    fn set_current_pc(&mut self, pc: usize);
    fn enter_pc_block(&mut self, pc: usize, block_terminated: &mut bool) -> Result<(), JitError>;
    fn apply_pc_facts(&mut self, pc: usize) -> Result<(), JitError>;
    fn instruction_for_pc(&self, pc: usize) -> Result<Instruction, JitError>;
    fn should_skip_instruction(&self, _inst: &Instruction) -> bool {
        false
    }
    fn translate_pc_instruction(&mut self, inst: &Instruction) -> Result<bool, JitError>;
    fn finish_fallthrough(&mut self, block_terminated: bool) -> Result<(), JitError>;
}

pub(crate) fn drive_compile(driver: &mut impl CompileDriver) -> Result<(), JitError> {
    let policy = driver.control_policy();
    let mut block_terminated = false;

    for pc in policy.pc_range() {
        driver.set_current_pc(pc);
        driver.enter_pc_block(pc, &mut block_terminated)?;
        driver.apply_pc_facts(pc)?;
        let inst = driver.instruction_for_pc(pc)?;
        if driver.should_skip_instruction(&inst) {
            continue;
        }
        block_terminated = driver.translate_pc_instruction(&inst)?;
    }

    driver.finish_fallthrough(block_terminated)
}

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

pub(crate) fn enter_compile_pc(
    builder: &mut FunctionBuilder<'_>,
    blocks: &HashMap<usize, Block>,
    pc: usize,
    block_terminated: &mut bool,
) -> bool {
    if let Some(&block) = blocks.get(&pc) {
        if !*block_terminated {
            builder.ins().jump(block, &[]);
        }
        builder.switch_to_block(block);
        *block_terminated = false;
        true
    } else if *block_terminated {
        let dummy = builder.create_block();
        builder.switch_to_block(dummy);
        *block_terminated = false;
        true
    } else {
        false
    }
}

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
