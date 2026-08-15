use std::collections::{BTreeMap, BTreeSet, HashMap};

use cranelift_codegen::ir::{Block, InstBuilder};
use cranelift_frontend::{FunctionBuilder, Variable};
use vo_runtime::bytecode::FunctionDef;
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

/// Maximum bytecode span charged by one native scheduling checkpoint.
///
/// Artificial region boundaries keep long straight-line native code
/// preemptible while avoiding a budget load/branch/store on every instruction.
pub(crate) const EXECUTION_BUDGET_REGION_INSTRUCTIONS: usize = 64;

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

    let mut checkpoint = range.start + EXECUTION_BUDGET_REGION_INSTRUCTIONS;
    while checkpoint < range.end {
        if !executable_only || instruction_is_executable(ir, optimized, checkpoint) {
            starts.insert(checkpoint);
        }
        checkpoint += EXECUTION_BUDGET_REGION_INSTRUCTIONS;
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
                if policy.compiled_target(target)
                    && (!executable_only || instruction_is_executable(ir, optimized, target))
                {
                    starts.insert(target);
                }
                if pc + 1 < range.end
                    && (!executable_only || instruction_is_executable(ir, optimized, pc + 1))
                {
                    starts.insert(pc + 1);
                }
            }
            Opcode::ForLoop => {
                let target = super::checked_forloop_target(policy.code_len(), pc, &inst)?;
                if policy.compiled_target(target)
                    && (!executable_only || instruction_is_executable(ir, optimized, target))
                {
                    starts.insert(target);
                }
                if pc + 1 < range.end
                    && (!executable_only || instruction_is_executable(ir, optimized, pc + 1))
                {
                    starts.insert(pc + 1);
                }
            }
            Opcode::Return => {
                if pc + 1 < range.end
                    && (!executable_only || instruction_is_executable(ir, optimized, pc + 1))
                {
                    starts.insert(pc + 1);
                }
            }
            _ => {}
        }
    }

    let mut regions = BTreeMap::new();
    let mut starts = starts.into_iter().peekable();
    while let Some(start) = starts.next() {
        let end = starts.peek().copied().unwrap_or(range.end);
        let base_cost = if executable_only {
            (start..end)
                .filter(|&pc| instruction_is_executable(ir, optimized, pc))
                .count()
                .try_into()
                .unwrap_or(u32::MAX)
        } else {
            end.saturating_sub(start).try_into().unwrap_or(u32::MAX)
        };
        let inline_cost = (start..end)
            .map(|pc| optimized.map_or(0, |graph| graph.inline_expansion_cost(pc)))
            .fold(0_u32, u32::saturating_add);
        let cost = base_cost.saturating_add(inline_cost);
        if cost > 0 {
            regions.insert(start, cost);
        }
    }
    Ok(regions)
}

pub(crate) fn prepare_control_flow(
    builder: &mut FunctionBuilder<'_>,
    blocks: &mut HashMap<usize, Block>,
    ir: &FunctionIr,
    policy: ControlPolicy,
    memory_only_start: u16,
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
                .filter(|parameter| parameter.slot < memory_only_start)
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
    pub vars: &'a [Variable],
    pub memory_only_start: u16,
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
            let arguments =
                block_arguments(builder, view.ir, view.vars, view.memory_only_start, pc);
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
    vars: &[Variable],
    memory_only_start: u16,
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
        .filter(|parameter| parameter.slot < memory_only_start)
        .map(|parameter| builder.use_var(vars[parameter.slot as usize]).into())
        .collect()
}

pub(crate) fn declare_variables(
    builder: &mut FunctionBuilder<'_>,
    func_def: &FunctionDef,
    memory_only_start: u16,
) -> Vec<Variable> {
    let ssa_slots = usize::from(memory_only_start).min(func_def.local_slots as usize);
    let mut vars = Vec::with_capacity(ssa_slots);
    for i in 0..ssa_slots {
        let ty = crate::compile_common::slot_ir_type(&func_def.slot_types, i as u16);
        let var = builder.declare_var(ty);
        vars.push(var);
    }
    vars
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
        builder.finalize(crate::test_frontend_config());
    }

    #[test]
    fn execution_budget_regions_split_long_straight_line_code() {
        let code = vec![Instruction::new(Opcode::LoadInt, 0, 0, 0); 130];
        let ir = test_ir(code);
        let regions = execution_budget_regions(&ir, ControlPolicy::full_function(130), false, None)
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
        let len = code.len();
        let ir = test_ir(code);
        let regions = execution_budget_regions(&ir, ControlPolicy::full_function(len), false, None)
            .expect("budget regions");

        assert_eq!(regions.get(&0), Some(&2));
        assert_eq!(regions.get(&2), Some(&1));
        assert_eq!(regions.get(&3), Some(&1));
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
}
