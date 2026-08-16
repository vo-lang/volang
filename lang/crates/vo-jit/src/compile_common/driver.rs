use std::collections::{BTreeMap, BTreeSet, HashMap};

use cranelift_codegen::ir::{Block, InstBuilder};
use cranelift_frontend::FunctionBuilder;
use vo_runtime::instruction::Opcode;

use crate::ir::{FunctionIr, TypedInstruction};
use crate::JitError;

/// One instruction from the graph selected for lowering. Optimizing
/// compilation receives its rewritten node directly; baseline compilation
/// receives the typed semantic instruction without an optimization overlay.
#[derive(Clone, Copy, Debug)]
pub(crate) enum LoweringInstruction {
    Baseline(TypedInstruction),
    Optimized(crate::optimizer::OptimizedInstruction),
}

impl LoweringInstruction {
    #[inline]
    pub(crate) fn typed(self) -> TypedInstruction {
        match self {
            Self::Baseline(instruction) => instruction,
            Self::Optimized(instruction) => instruction.typed(),
        }
    }

    #[inline]
    pub(crate) fn optimized(self) -> Option<crate::optimizer::OptimizedInstruction> {
        match self {
            Self::Baseline(_) => None,
            Self::Optimized(instruction) => Some(instruction),
        }
    }
}

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
    fn is_pc_executable(&self, _pc: usize) -> bool {
        true
    }
    fn set_current_pc(&mut self, pc: usize);
    fn enter_pc_block(&mut self, pc: usize, block_terminated: &mut bool) -> Result<(), JitError>;
    fn apply_pc_facts(&mut self, pc: usize) -> Result<(), JitError>;
    fn instruction_for_pc(&self, pc: usize) -> Result<LoweringInstruction, JitError>;
    fn should_skip_instruction(&self, _inst: LoweringInstruction) -> bool {
        false
    }
    fn translate_pc_instruction(&mut self, inst: LoweringInstruction) -> Result<bool, JitError>;
    fn finish_fallthrough(&mut self, block_terminated: bool) -> Result<(), JitError>;
}

/// Maximum straight-line bytecode span between native scheduling checkpoints.
///
/// Natural loops poll at their backedge target. A wider artificial interval is
/// enough for acyclic code because the VM scheduling quantum is itself much
/// larger, while keeping poll and deoptimization state out of ordinary basic
/// blocks.
pub(crate) const EXECUTION_BUDGET_REGION_INSTRUCTIONS: usize = 256;

#[inline]
fn instruction_is_executable(
    ir: &FunctionIr,
    optimized: Option<&crate::optimizer::OptimizedFunction>,
    pc: usize,
) -> bool {
    optimized.map_or_else(
        || {
            ir.instruction(pc)
                .is_some_and(|instruction| ir.is_executable_block(instruction.block()))
        },
        |graph| graph.is_executable(pc),
    )
}

pub(crate) fn execution_budget_regions(
    ir: &FunctionIr,
    policy: ControlPolicy,
    executable_only: bool,
    optimized: Option<&crate::optimizer::OptimizedFunction>,
) -> Result<BTreeMap<usize, u32>, JitError> {
    let range = policy.pc_range();
    if range.is_empty() {
        return Ok(BTreeMap::new());
    }

    let mut starts = BTreeSet::new();
    starts.insert(range.start);

    // Use one artifact-independent global lattice. Full functions and OSR
    // regions can then share the same first-class sparse frame states while
    // every straight-line span remains bounded by one interval.
    let mut checkpoint = range
        .start
        .saturating_sub(range.start % EXECUTION_BUDGET_REGION_INSTRUCTIONS)
        .saturating_add(EXECUTION_BUDGET_REGION_INSTRUCTIONS);
    while checkpoint < range.end {
        if !executable_only || instruction_is_executable(ir, optimized, checkpoint) {
            starts.insert(checkpoint);
        }
        checkpoint = checkpoint.saturating_add(EXECUTION_BUDGET_REGION_INSTRUCTIONS);
    }

    for pc in range.clone() {
        let typed = ir.instruction(pc).ok_or(JitError::InvalidOsrTarget(pc))?;
        if executable_only && !instruction_is_executable(ir, optimized, pc) {
            continue;
        }
        let inst = typed.source();
        match inst.opcode() {
            Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot => {
                let target = super::checked_branch_target(
                    policy.code_len(),
                    pc,
                    inst.imm32(),
                    inst.opcode(),
                )?;
                if target <= pc
                    && policy.compiled_target(target)
                    && (!executable_only || instruction_is_executable(ir, optimized, target))
                {
                    starts.insert(target);
                }
            }
            Opcode::ForLoop => {
                let target = super::checked_forloop_target(policy.code_len(), pc, &inst)?;
                if target <= pc
                    && policy.compiled_target(target)
                    && (!executable_only || instruction_is_executable(ir, optimized, target))
                {
                    starts.insert(target);
                }
            }
            _ => {}
        }
    }

    // Once every backward-edge target is a checkpoint, all paths inside one
    // region move forward in bytecode order. A reverse pass therefore computes
    // the longest charge to the next checkpoint without recursion or a second
    // graph representation.
    let mut cost_to_checkpoint = vec![0_u32; ir.instruction_count()];
    for pc in range.clone().rev() {
        if starts.contains(&pc)
            || (executable_only && !instruction_is_executable(ir, optimized, pc))
        {
            continue;
        }
        let mut tail = 0;
        for successor in instruction_budget_successors(ir, policy, executable_only, optimized, pc)?
        {
            if successor <= pc && !starts.contains(&successor) {
                return Err(JitError::Internal(format!(
                    "native execution-budget region contains an uncut backedge {pc} -> {successor}"
                )));
            }
            if !starts.contains(&successor) {
                tail = tail.max(cost_to_checkpoint[successor]);
            }
        }
        cost_to_checkpoint[pc] = instruction_budget_cost(optimized, pc).saturating_add(tail);
    }

    let mut regions = BTreeMap::new();
    for &start in &starts {
        let mut tail = 0;
        for successor in
            instruction_budget_successors(ir, policy, executable_only, optimized, start)?
        {
            if !starts.contains(&successor) {
                tail = tail.max(cost_to_checkpoint[successor]);
            }
        }
        let cost = instruction_budget_cost(optimized, start).saturating_add(tail);
        regions.insert(start, cost.max(1));
    }
    Ok(regions)
}

fn instruction_budget_cost(
    optimized: Option<&crate::optimizer::OptimizedFunction>,
    pc: usize,
) -> u32 {
    1_u32.saturating_add(optimized.map_or(0, |graph| graph.inline_expansion_cost(pc)))
}

fn instruction_budget_successors(
    ir: &FunctionIr,
    policy: ControlPolicy,
    executable_only: bool,
    optimized: Option<&crate::optimizer::OptimizedFunction>,
    pc: usize,
) -> Result<Vec<usize>, JitError> {
    let instruction = ir
        .instruction(pc)
        .ok_or(JitError::InvalidOsrTarget(pc))?
        .source();
    let mut successors = crate::ir::instruction_successors(pc, instruction, policy.code_len())?;
    successors.retain(|&successor| {
        policy.compiled_target(successor)
            && (!executable_only || instruction_is_executable(ir, optimized, successor))
    });
    successors.sort_unstable();
    successors.dedup();
    Ok(successors)
}

pub(crate) fn prepare_control_flow(
    builder: &mut FunctionBuilder<'_>,
    blocks: &mut HashMap<usize, Block>,
    ir: &FunctionIr,
    policy: ControlPolicy,
    vars: &crate::compile_common::SsaSlotVariables,
    executable_only: bool,
    optimized: Option<&crate::optimizer::OptimizedFunction>,
) -> Result<BTreeMap<usize, u32>, JitError> {
    let regions = execution_budget_regions(ir, policy, executable_only, optimized)?;
    for block in ir.blocks() {
        if executable_only && !instruction_is_executable(ir, optimized, block.start_pc as usize) {
            continue;
        }
        let start = block.start_pc as usize;
        if policy.pc_range().contains(&start) {
            let clif_block = *blocks
                .entry(start)
                .or_insert_with(|| builder.create_block());
            for parameter in ir
                .block_parameters(block.id)
                .iter()
                .filter(|parameter| vars.get(parameter.slot).is_some())
            {
                let ty = ir.value(parameter.value).ty;
                builder.append_block_param(clif_block, value_type_to_ir_type(ty));
            }
        }
    }
    for start in regions.keys().copied() {
        if executable_only && !instruction_is_executable(ir, optimized, start) {
            continue;
        }
        blocks
            .entry(start)
            .or_insert_with(|| builder.create_block());
    }
    Ok(regions)
}

fn value_type_to_ir_type(value_type: crate::ir::ValueType) -> cranelift_codegen::ir::Type {
    match value_type {
        crate::ir::ValueType::Float64 => cranelift_codegen::ir::types::F64,
        crate::ir::ValueType::Word
        | crate::ir::ValueType::GcRef(_)
        | crate::ir::ValueType::InterfaceHeader
        | crate::ir::ValueType::InterfaceData => cranelift_codegen::ir::types::I64,
    }
}

pub(crate) fn drive_compile(driver: &mut impl CompileDriver) -> Result<(), JitError> {
    let policy = driver.control_policy();
    let mut block_terminated = false;

    for pc in policy.pc_range() {
        if !driver.is_pc_executable(pc) {
            continue;
        }
        driver.set_current_pc(pc);
        driver.enter_pc_block(pc, &mut block_terminated)?;
        driver.apply_pc_facts(pc)?;
        let inst = driver.instruction_for_pc(pc)?;
        if driver.should_skip_instruction(inst) {
            continue;
        }
        block_terminated = driver.translate_pc_instruction(inst)?;
    }

    driver.finish_fallthrough(block_terminated)
}

pub(crate) struct CompileBlockView<'a> {
    pub blocks: &'a HashMap<usize, Block>,
    pub ir: &'a FunctionIr,
    pub vars: &'a crate::compile_common::SsaSlotVariables,
    pub executable_only: bool,
    pub optimized: Option<&'a crate::optimizer::OptimizedFunction>,
}

pub(crate) fn enter_compile_pc(
    builder: &mut FunctionBuilder<'_>,
    view: CompileBlockView<'_>,
    pc: usize,
    block_terminated: &mut bool,
) -> bool {
    if view.executable_only && !instruction_is_executable(view.ir, view.optimized, pc) {
        return false;
    }
    if let Some(&block) = view.blocks.get(&pc) {
        if !*block_terminated {
            let arguments = block_arguments(builder, view.ir, view.vars, pc);
            builder.ins().jump(block, &arguments);
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

pub(crate) fn block_arguments(
    builder: &mut FunctionBuilder<'_>,
    ir: &FunctionIr,
    vars: &crate::compile_common::SsaSlotVariables,
    target_pc: usize,
) -> Vec<cranelift_codegen::ir::BlockArg> {
    let Some(instruction) = ir.instruction(target_pc).copied() else {
        return Vec::new();
    };
    let block = &ir.blocks()[instruction.block().index()];
    if block.start_pc as usize != target_pc {
        return Vec::new();
    }
    ir.block_parameters(block.id)
        .iter()
        .filter_map(|parameter| Some(builder.use_var(vars.get(parameter.slot)?).into()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::Module;
    use vo_runtime::instruction::Instruction;

    fn test_ir(code: Vec<Instruction>) -> FunctionIr {
        let mut module = Module::new("control-driver".into());
        module
            .functions
            .push(crate::test_fixtures::function(code, 1));
        FunctionIr::build(&module.functions[0], &module).expect("test control-flow IR")
    }

    #[test]
    fn variable_declarations_follow_semantic_slot_use_without_a_numeric_cliff() {
        let mut module = Module::new("sparse-variable-declarations".into());
        module.functions.push(crate::test_fixtures::function(
            vec![
                Instruction::new(Opcode::LoadInt, 511, 7, 0),
                Instruction::new(Opcode::Return, 511, 1, 0),
            ],
            512,
        ));
        let ir = FunctionIr::build(&module.functions[0], &module).expect("wide sparse IR");
        let mut func = cranelift_codegen::ir::Function::new();
        let mut func_ctx = cranelift_frontend::FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut func_ctx);

        let vars = crate::compile_common::SsaSlotVariables::declare(
            &mut builder,
            &module.functions[0],
            &ir,
            u16::MAX,
        );
        assert_eq!(vars.iter().count(), 1);
        assert!(vars.get(511).is_some());
        assert!(vars.get(255).is_none());

        let block = builder.create_block();
        builder.switch_to_block(block);
        builder.seal_block(block);
        builder.ins().return_(&[]);
        builder.finalize(crate::test_frontend_config());
    }

    #[test]
    fn execution_budget_regions_split_long_straight_line_code() {
        let region = EXECUTION_BUDGET_REGION_INSTRUCTIONS;
        let len = region * 2 + 2;
        let code = vec![Instruction::new(Opcode::LoadInt, 0, 0, 0); len];
        let ir = test_ir(code);
        let regions = execution_budget_regions(&ir, ControlPolicy::full_function(len), false, None)
            .expect("budget regions");

        assert_eq!(regions.get(&0), Some(&(region as u32)));
        assert_eq!(regions.get(&region), Some(&(region as u32)));
        assert_eq!(regions.get(&(region * 2)), Some(&2));
    }

    #[test]
    fn execution_budget_regions_charge_the_longest_acyclic_branch_path() {
        let code = vec![
            Instruction::new(Opcode::LoadInt, 0, 0, 0),
            Instruction::new(Opcode::JumpIf, 0, 2, 0),
            Instruction::new(Opcode::LoadInt, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        let len = code.len();
        let ir = test_ir(code);
        let regions = execution_budget_regions(&ir, ControlPolicy::full_function(len), false, None)
            .expect("budget regions");

        assert_eq!(regions, BTreeMap::from([(0, 4)]));
    }

    #[test]
    fn execution_budget_regions_poll_once_per_natural_loop_iteration() {
        let code = vec![
            Instruction::new(Opcode::LoadInt, 0, 0, 0),
            Instruction::new(Opcode::AddI, 0, 0, 0),
            Instruction::new(Opcode::JumpIf, 0, u16::MAX, u16::MAX),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        let len = code.len();
        let ir = test_ir(code);
        let regions = execution_budget_regions(&ir, ControlPolicy::full_function(len), false, None)
            .expect("budget regions");

        assert_eq!(regions, BTreeMap::from([(0, 1), (1, 3)]));
    }

    #[test]
    fn osr_regions_ignore_branch_targets_outside_the_compiled_loop() {
        let code = vec![
            Instruction::new(Opcode::Jump, 0, 2, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        let len = code.len();
        let ir = test_ir(code);
        let regions =
            execution_budget_regions(&ir, ControlPolicy::loop_osr(0, 0, 1, len), true, None)
                .expect("OSR budget regions");

        assert_eq!(regions, BTreeMap::from([(0, 1)]));
    }

    #[test]
    fn osr_regions_share_the_global_periodic_checkpoint_lattice() {
        let region = EXECUTION_BUDGET_REGION_INSTRUCTIONS;
        let code = vec![Instruction::new(Opcode::LoadInt, 0, 0, 0); region * 3];
        let ir = test_ir(code);
        let begin = region / 2;
        let end = region * 2 + region / 2;
        let regions = execution_budget_regions(
            &ir,
            ControlPolicy::loop_osr(begin, end, end + 1, region * 3),
            false,
            None,
        )
        .expect("OSR budget regions");

        assert_eq!(
            regions.keys().copied().collect::<Vec<_>>(),
            vec![begin, region, region * 2]
        );
    }
}
