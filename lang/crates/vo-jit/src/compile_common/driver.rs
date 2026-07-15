use std::collections::{BTreeMap, BTreeSet, HashMap};

use cranelift_codegen::ir::{Block, InstBuilder};
use cranelift_frontend::{FunctionBuilder, Variable};
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::Instruction;
use vo_runtime::instruction::Opcode;

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

    pub(super) fn code_len(self) -> usize {
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

/// Maximum bytecode span charged by one native scheduling checkpoint.
///
/// Artificial region boundaries keep long straight-line native code
/// preemptible while avoiding a budget load/branch/store on every instruction.
pub(crate) const EXECUTION_BUDGET_REGION_INSTRUCTIONS: usize = 64;

pub(crate) fn execution_budget_regions(
    code: &[Instruction],
    policy: ControlPolicy,
) -> Result<BTreeMap<usize, u32>, JitError> {
    let range = policy.pc_range();
    if range.is_empty() {
        return Ok(BTreeMap::new());
    }

    let mut starts = BTreeSet::new();
    starts.insert(range.start);

    let mut checkpoint = range.start + EXECUTION_BUDGET_REGION_INSTRUCTIONS;
    while checkpoint < range.end {
        starts.insert(checkpoint);
        checkpoint += EXECUTION_BUDGET_REGION_INSTRUCTIONS;
    }

    starts.extend(super::jump_targets_for_policy(code, policy)?);

    for pc in range.clone() {
        let inst = code.get(pc).ok_or(JitError::InvalidOsrTarget(pc))?;
        match inst.opcode() {
            Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot => {
                if pc + 1 < range.end {
                    starts.insert(pc + 1);
                }
            }
            Opcode::ForLoop => {
                if pc + 1 < range.end {
                    starts.insert(pc + 1);
                }
            }
            Opcode::Return => {
                if pc + 1 < range.end {
                    starts.insert(pc + 1);
                }
            }
            _ => {}
        }
    }

    let ordered: Vec<_> = starts.into_iter().collect();
    let mut regions = BTreeMap::new();
    for (index, start) in ordered.iter().copied().enumerate() {
        let end = ordered.get(index + 1).copied().unwrap_or(range.end);
        let cost = end.saturating_sub(start).try_into().unwrap_or(u32::MAX);
        if cost > 0 {
            regions.insert(start, cost);
        }
    }
    Ok(regions)
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
    memory_only_start: u16,
) -> Vec<Variable> {
    let ssa_slots = usize::from(memory_only_start).min(func_def.local_slots as usize);
    let mut vars = Vec::with_capacity(ssa_slots);
    for i in 0..ssa_slots {
        let var = Variable::from_u32(i as u32);
        let ty = crate::compile_common::slot_ir_type(&func_def.slot_types, i as u16);
        builder.declare_var(var, ty);
        vars.push(var);
    }
    vars
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::instruction::Instruction;

    #[test]
    fn variable_declarations_stop_at_the_bounded_ssa_prefix() {
        let func_def = crate::test_fixtures::function(Vec::new(), 512);
        let mut func = cranelift_codegen::ir::Function::new();
        let mut func_ctx = cranelift_frontend::FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut func_ctx);

        let vars = declare_variables(
            &mut builder,
            &func_def,
            crate::compile_common::MAX_SSA_LOCAL_SLOTS,
        );
        assert_eq!(
            vars.len(),
            usize::from(crate::compile_common::MAX_SSA_LOCAL_SLOTS)
        );

        let block = builder.create_block();
        builder.switch_to_block(block);
        builder.seal_block(block);
        builder.ins().return_(&[]);
        builder.finalize();
    }

    #[test]
    fn execution_budget_regions_split_long_straight_line_code() {
        let code = vec![Instruction::new(Opcode::LoadInt, 0, 0, 0); 130];
        let regions = execution_budget_regions(&code, ControlPolicy::full_function(code.len()))
            .expect("budget regions");

        assert_eq!(regions.get(&0), Some(&64));
        assert_eq!(regions.get(&64), Some(&64));
        assert_eq!(regions.get(&128), Some(&2));
    }

    #[test]
    fn execution_budget_regions_start_at_branch_targets_and_fallthroughs() {
        let code = vec![
            Instruction::new(Opcode::LoadInt, 0, 0, 0),
            Instruction::new(Opcode::JumpIf, 0, 2, 0),
            Instruction::new(Opcode::LoadInt, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        let regions = execution_budget_regions(&code, ControlPolicy::full_function(code.len()))
            .expect("budget regions");

        assert_eq!(regions.get(&0), Some(&2));
        assert_eq!(regions.get(&2), Some(&1));
        assert_eq!(regions.get(&3), Some(&1));
    }
}
