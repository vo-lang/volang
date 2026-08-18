//! Immutable module-wide facts consumed only by the optimizing tier.

use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;
use vo_runtime::bytecode::{Constant, FunctionDef, Module, IFACE_ASSIGN_NO_ITAB};
use vo_runtime::instruction::Opcode;

use crate::call_graph::ModuleCallGraph;
use crate::call_helpers::{SmallFunctionInline, SMALL_INLINE_BUDGET};
use crate::JitError;

/// The canonical optimized form of one function or OSR region.
///
/// Every source instruction has exactly one lowering node. Keeping the
/// transformation, safety and expansion decisions together prevents lowering
/// from reconstructing an optimizer pipeline out of unrelated side tables.
pub(crate) struct OptimizedFunction {
    instructions: Box<[OptimizedInstruction]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum LoweringAction {
    Emit,
    Eliminate,
    Replace(crate::ir::ValueId),
    AlwaysJump,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct InlineExpansion {
    target: u32,
    cost: u32,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct OptimizedInstruction {
    typed: crate::ir::TypedInstruction,
    block_executable: bool,
    pub(crate) action: LoweringAction,
    pub(crate) bounds_check_elided: bool,
    pub(crate) nil_check_elided: bool,
    pub(crate) inline: Option<InlineExpansion>,
    pub(crate) scalar_replacement: Option<crate::escape::ScalarReplacement>,
    pub(crate) virtual_object: Option<u32>,
    pub(crate) fresh_shape: Option<crate::shape::FreshShapeAccess>,
}

impl OptimizedInstruction {
    fn emit(typed: crate::ir::TypedInstruction) -> Self {
        Self {
            typed,
            block_executable: true,
            action: LoweringAction::Emit,
            bounds_check_elided: false,
            nil_check_elided: false,
            inline: None,
            scalar_replacement: None,
            virtual_object: None,
            fresh_shape: None,
        }
    }

    #[inline]
    pub(crate) fn typed(self) -> crate::ir::TypedInstruction {
        self.typed
    }

    #[inline]
    pub(crate) fn inline_target(self) -> Option<u32> {
        self.inline.map(|inline| inline.target)
    }
}

const NO_REPLACEMENT: u32 = u32::MAX;
const NO_DYNAMIC_TARGET: u32 = u32::MAX;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ExpressionKey {
    opcode: u8,
    flags: u8,
    result_type: u8,
    immediate: u32,
    inputs: Box<[u32]>,
}

struct GvnPlan {
    replacement_values: Vec<u32>,
    redundant_instructions: Vec<u64>,
    elided_bounds_checks: Vec<u64>,
    elided_nil_checks: Vec<u64>,
}

/// Artifact-local availability and liveness decisions. The canonical graph
/// owns program facts; each native entry shape derives only the decisions that
/// depend on where execution enters and which state must remain materializable.
struct ArtifactLowering {
    actions: Vec<LoweringAction>,
    executable_blocks: Vec<bool>,
    elided_bounds_checks: Vec<u64>,
    elided_nil_checks: Vec<u64>,
}

/// Control-flow and value facts are properties of a native entry point.
/// Full-function compilation can consume the canonical SCCP result directly.
/// An OSR entry imports VM locals without guards, so its entry values start
/// overdefined and SCCP is re-run over the structural loop region.
struct ArtifactFlow {
    executable_blocks: Vec<bool>,
    successors: Vec<Vec<crate::ir::BlockEdge>>,
    constants: Vec<Option<i64>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ArtifactConstant {
    Unknown,
    Known(i64),
    Overdefined,
}

impl ArtifactConstant {
    fn join(self, other: Self) -> Self {
        match (self, other) {
            (Self::Unknown, value) | (value, Self::Unknown) => value,
            (Self::Known(lhs), Self::Known(rhs)) if lhs == rhs => Self::Known(lhs),
            (Self::Known(_), Self::Known(_)) | (Self::Overdefined, _) | (_, Self::Overdefined) => {
                Self::Overdefined
            }
        }
    }
}

impl ArtifactFlow {
    fn for_region(
        ir: &crate::ir::FunctionIr,
        pc_range: &std::ops::Range<usize>,
        rebase_entry: bool,
    ) -> Self {
        if rebase_entry {
            Self::for_osr(ir, pc_range)
        } else {
            Self {
                executable_blocks: ir
                    .blocks()
                    .iter()
                    .map(|block| ir.is_executable_block(block.id))
                    .collect(),
                successors: ir
                    .blocks()
                    .iter()
                    .map(|block| ir.executable_successors(block.id).collect())
                    .collect(),
                constants: (0..ir.value_count())
                    .map(|index| ir.constant(crate::ir::ValueId::from_index(index)))
                    .collect(),
            }
        }
    }

    fn for_osr(ir: &crate::ir::FunctionIr, pc_range: &std::ops::Range<usize>) -> Self {
        let block_count = ir.blocks().len();
        let empty = || Self {
            executable_blocks: vec![false; block_count],
            successors: vec![Vec::new(); block_count],
            constants: vec![None; ir.value_count()],
        };
        let Some(entry) = ir
            .instruction(pc_range.start)
            .map(|instruction| instruction.block())
        else {
            return empty();
        };

        // Values materialized by the VM at the OSR boundary are deliberately
        // overdefined. Region-local definitions and block parameters then rise
        // monotonically through the usual SCCP lattice.
        let mut constants = vec![ArtifactConstant::Overdefined; ir.value_count()];
        for pc in pc_range.clone() {
            let Some(instruction) = ir.instruction(pc).copied() else {
                continue;
            };
            for &output in ir.outputs(instruction) {
                constants[output.index()] = ArtifactConstant::Unknown;
            }
        }
        for block in ir.blocks() {
            let start = block.start_pc as usize;
            if block.id != entry && pc_range.contains(&start) {
                for parameter in ir.block_parameters(block.id) {
                    constants[parameter.value.index()] = ArtifactConstant::Unknown;
                }
            }
        }

        let mut executable_blocks = vec![false; block_count];
        let mut executable_edges = ir
            .blocks()
            .iter()
            .map(|block| vec![false; ir.successors(block.id).len()])
            .collect::<Vec<_>>();
        executable_blocks[entry.index()] = true;

        let iteration_limit = ir
            .value_count()
            .saturating_mul(2)
            .saturating_add(block_count)
            .saturating_add(executable_edges.iter().map(Vec::len).sum::<usize>())
            .saturating_add(1);
        for _ in 0..iteration_limit {
            let mut changed = false;
            for block in ir.blocks() {
                if !executable_blocks[block.id.index()] {
                    continue;
                }
                let start = (block.start_pc as usize).max(pc_range.start);
                let end = (block.end_pc as usize).min(pc_range.end);
                for pc in start..end {
                    let instruction = *ir
                        .instruction(pc)
                        .expect("OSR block bounds must stay inside the IR");
                    for &output in ir.outputs(instruction) {
                        let desired =
                            artifact_instruction_constant(ir, instruction, output, &constants);
                        let current = constants[output.index()];
                        let merged = current.join(desired);
                        if merged != current {
                            constants[output.index()] = merged;
                            changed = true;
                        }
                    }
                }

                let Some(last_pc) = (block.end_pc as usize).checked_sub(1) else {
                    continue;
                };
                if !pc_range.contains(&last_pc) {
                    continue;
                }
                let selection = artifact_branch_target(ir, last_pc, &constants);
                for (edge_index, edge) in ir.successors(block.id).iter().copied().enumerate() {
                    let selected = match selection {
                        Some(Some(target)) => edge.target == target,
                        Some(None) => false,
                        None => true,
                    };
                    if !selected {
                        continue;
                    }
                    if !executable_edges[block.id.index()][edge_index] {
                        executable_edges[block.id.index()][edge_index] = true;
                        changed = true;
                    }
                    let target_start = ir.blocks()[edge.target.index()].start_pc as usize;
                    let target_in_region = edge.target == entry || pc_range.contains(&target_start);
                    if target_in_region && !executable_blocks[edge.target.index()] {
                        executable_blocks[edge.target.index()] = true;
                        changed = true;
                    }
                    if !target_in_region {
                        continue;
                    }
                    for argument in ir.edge_arguments(edge) {
                        let Some(parameter) = ir
                            .block_parameters(edge.target)
                            .iter()
                            .find(|parameter| parameter.slot == argument.slot)
                        else {
                            continue;
                        };
                        let current = constants[parameter.value.index()];
                        let merged = current.join(constants[argument.value.index()]);
                        if merged != current {
                            constants[parameter.value.index()] = merged;
                            changed = true;
                        }
                    }
                }
            }
            if !changed {
                break;
            }
        }

        let successors = ir
            .blocks()
            .iter()
            .map(|block| {
                ir.successors(block.id)
                    .iter()
                    .copied()
                    .zip(executable_edges[block.id.index()].iter().copied())
                    .filter_map(|(edge, executable)| executable.then_some(edge))
                    .collect()
            })
            .collect();
        let constants = constants
            .into_iter()
            .map(|constant| match constant {
                ArtifactConstant::Known(value) => Some(value),
                ArtifactConstant::Unknown | ArtifactConstant::Overdefined => None,
            })
            .collect();
        Self {
            executable_blocks,
            successors,
            constants,
        }
    }

    #[inline]
    fn constant(&self, value: crate::ir::ValueId) -> Option<i64> {
        self.constants.get(value.index()).copied().flatten()
    }

    /// Values visible on every executable edge that leaves an OSR artifact.
    /// This uses the entry-specific CFG, including paths that full-function
    /// SCCP could prune under assumptions unavailable at the OSR boundary.
    fn osr_exit_values(
        &self,
        ir: &crate::ir::FunctionIr,
        pc_range: &std::ops::Range<usize>,
    ) -> Vec<crate::ir::ValueId> {
        if pc_range.is_empty() {
            return Vec::new();
        }

        let mut observable = vec![false; ir.value_count()];
        for block in ir.blocks() {
            if !self.executable_blocks[block.id.index()] {
                continue;
            }
            let start = (block.start_pc as usize).max(pc_range.start);
            let end = (block.end_pc as usize).min(pc_range.end);
            if start >= end {
                continue;
            }

            let mut current = ir
                .block_parameters(block.id)
                .iter()
                .map(|parameter| (parameter.slot, parameter.value))
                .collect::<HashMap<_, _>>();
            for pc in start..end {
                let instruction = *ir
                    .instruction(pc)
                    .expect("OSR region bounds must stay inside the IR");
                for &output in ir.outputs(instruction) {
                    current.insert(ir.value(output).slot, output);
                }
            }

            let has_external_edge = self.successors[block.id.index()].iter().any(|edge| {
                let target_pc = ir.blocks()[edge.target.index()].start_pc as usize;
                !pc_range.contains(&target_pc)
            });
            let cuts_block = pc_range.end < block.end_pc as usize && end == pc_range.end;
            let terminal_without_edge = end == pc_range.end
                && self.successors[block.id.index()].is_empty()
                && ir
                    .instruction(end - 1)
                    .is_some_and(|instruction| instruction.source().opcode() != Opcode::Panic);
            if has_external_edge || cuts_block || terminal_without_edge {
                for value in current.values().copied() {
                    observable[value.index()] = true;
                }
            }
        }

        observable
            .into_iter()
            .enumerate()
            .filter_map(|(index, visible)| visible.then_some(crate::ir::ValueId::from_index(index)))
            .collect()
    }
}

fn artifact_instruction_constant(
    ir: &crate::ir::FunctionIr,
    instruction: crate::ir::TypedInstruction,
    output: crate::ir::ValueId,
    constants: &[ArtifactConstant],
) -> ArtifactConstant {
    let Some(value) = ir.constant(output) else {
        return ArtifactConstant::Overdefined;
    };
    let inputs = ir.inputs(instruction);
    if inputs.is_empty() {
        return ArtifactConstant::Known(value);
    }
    let mut saw_unknown = false;
    for &input in inputs {
        match constants[input.index()] {
            ArtifactConstant::Unknown => saw_unknown = true,
            ArtifactConstant::Overdefined => return ArtifactConstant::Overdefined,
            ArtifactConstant::Known(input_value) => {
                if ir.constant(input) != Some(input_value) {
                    return ArtifactConstant::Overdefined;
                }
            }
        }
    }
    if saw_unknown {
        ArtifactConstant::Unknown
    } else {
        ArtifactConstant::Known(value)
    }
}

/// `Some(Some(block))` selects one edge, `Some(None)` delays edge discovery,
/// and `None` keeps every structural successor executable.
fn artifact_branch_target(
    ir: &crate::ir::FunctionIr,
    pc: usize,
    constants: &[ArtifactConstant],
) -> Option<Option<crate::ir::BlockId>> {
    let instruction = *ir.instruction(pc)?;
    let source = instruction.source();
    match source.opcode() {
        Opcode::Return | Opcode::Panic => Some(None),
        Opcode::JumpIf | Opcode::JumpIfNot => {
            let condition = ir
                .input_value(pc, source.a)
                .map(|value| constants[value.index()])
                .unwrap_or(ArtifactConstant::Overdefined);
            match condition {
                ArtifactConstant::Unknown => Some(None),
                ArtifactConstant::Overdefined => None,
                ArtifactConstant::Known(value) => {
                    let taken = match source.opcode() {
                        Opcode::JumpIf => value != 0,
                        Opcode::JumpIfNot => value == 0,
                        _ => unreachable!(),
                    };
                    let target_pc = if taken {
                        crate::compile_common::checked_branch_target(
                            ir.instruction_count(),
                            pc,
                            source.imm32(),
                            source.opcode(),
                        )
                        .ok()?
                    } else {
                        pc.checked_add(1)
                            .filter(|&pc| pc < ir.instruction_count())?
                    };
                    Some(
                        ir.instruction(target_pc)
                            .map(|instruction| instruction.block()),
                    )
                }
            }
        }
        _ => None,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct BoundsCheckKey {
    kind: u8,
    container: u32,
    index: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct FieldKey {
    pointer: u32,
    offset: u16,
}

enum FieldUndo {
    Remove(FieldKey),
    Restore(FieldKey, crate::ir::ValueId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum RangeFact {
    NonNegative(u32),
    LessThan {
        index: u32,
        limit: u32,
        unsigned: bool,
    },
}

const EXPLICIT_INDEX_CHECK: u8 = 0;
const ARRAY_BOUNDS_CHECK: u8 = 1;
const SLICE_BOUNDS_CHECK: u8 = 2;
const STRING_BOUNDS_CHECK: u8 = 3;

impl OptimizedFunction {
    #[cfg(test)]
    pub(crate) fn inline_cost_probe(ir: &crate::ir::FunctionIr, cost: u32) -> Self {
        let mut instructions = (0..ir.instruction_count())
            .map(|pc| {
                OptimizedInstruction::emit(
                    *ir.instruction(pc)
                        .expect("IR instruction cardinality is internally verified"),
                )
            })
            .collect::<Vec<_>>();
        if let Some(node) = instructions.first_mut() {
            node.inline = Some(InlineExpansion { target: 0, cost });
        }
        Self {
            instructions: instructions.into_boxed_slice(),
        }
    }

    pub(crate) fn baseline_with_module(
        ir: &crate::ir::FunctionIr,
        module: &ModuleInlinePlan,
        caller_id: u32,
    ) -> Self {
        let pc_range = 0..ir.instruction_count();
        let dynamic_targets = vec![NO_DYNAMIC_TARGET; ir.instruction_count()];
        let (inline_targets, inline_costs) =
            plan_inlines(ir, &pc_range, module, caller_id, &dynamic_targets, false);
        let instructions = (0..ir.instruction_count())
            .map(|pc| {
                let typed = *ir
                    .instruction(pc)
                    .expect("IR instruction cardinality is internally verified");
                let mut instruction = OptimizedInstruction::emit(typed);
                instruction.block_executable = ir.blocks()[typed.block().index()].reachable;
                instruction.inline =
                    (inline_targets[pc] != NO_DYNAMIC_TARGET).then_some(InlineExpansion {
                        target: inline_targets[pc],
                        cost: inline_costs[pc],
                    });
                instruction
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Self { instructions }
    }
    #[cfg(test)]
    pub(crate) fn analyze(ir: &crate::ir::FunctionIr) -> Self {
        Self::analyze_region(ir, 0..ir.instruction_count(), false, None, None, None)
    }

    pub(crate) fn analyze_with_module(
        ir: &crate::ir::FunctionIr,
        function: &FunctionDef,
        module: &ModuleOptimizationPlan,
        caller_id: u32,
    ) -> Self {
        Self::analyze_region(
            ir,
            0..ir.instruction_count(),
            false,
            Some(function),
            Some(module),
            Some(caller_id),
        )
    }

    #[cfg(test)]
    pub(crate) fn analyze_osr(
        ir: &crate::ir::FunctionIr,
        pc_range: std::ops::Range<usize>,
    ) -> Self {
        Self::analyze(ir).project_osr(ir, pc_range)
    }

    fn analyze_region(
        ir: &crate::ir::FunctionIr,
        pc_range: std::ops::Range<usize>,
        preserve_osr_exit_state: bool,
        function: Option<&FunctionDef>,
        module: Option<&ModuleOptimizationPlan>,
        caller_id: Option<u32>,
    ) -> Self {
        let lowering = analyze_artifact_lowering(ir, pc_range.clone(), preserve_osr_exit_state);
        let dynamic_call_targets = module
            .map(|module| analyze_callable_values(ir, module))
            .unwrap_or_else(|| vec![NO_DYNAMIC_TARGET; ir.instruction_count()]);
        let (inline_targets, inline_expansion_costs) = match (module, caller_id) {
            (Some(module), Some(caller_id)) => plan_inlines(
                ir,
                &pc_range,
                &module.inline_plan,
                caller_id,
                &dynamic_call_targets,
                true,
            ),
            _ => (
                vec![NO_DYNAMIC_TARGET; ir.instruction_count()],
                vec![0; ir.instruction_count()],
            ),
        };
        let escape = function.map(|function| crate::escape::EscapePlan::analyze(function, ir));
        let shapes = function.map(|function| crate::shape::ShapePlan::analyze(function, ir));
        let instructions = (0..ir.instruction_count())
            .map(|pc| {
                let typed = *ir
                    .instruction(pc)
                    .expect("IR instruction cardinality is internally verified");
                let inline = (inline_targets[pc] != NO_DYNAMIC_TARGET).then_some(InlineExpansion {
                    target: inline_targets[pc],
                    cost: inline_expansion_costs[pc],
                });
                OptimizedInstruction {
                    typed,
                    block_executable: lowering.executable_blocks[typed.block().index()],
                    action: lowering.actions[pc],
                    bounds_check_elided: bit_is_set(&lowering.elided_bounds_checks, pc),
                    nil_check_elided: bit_is_set(&lowering.elided_nil_checks, pc),
                    inline,
                    scalar_replacement: escape.as_ref().and_then(|escape| escape.replacement(pc)),
                    virtual_object: escape.as_ref().and_then(|escape| escape.access(pc)),
                    fresh_shape: shapes.as_ref().and_then(|shapes| shapes.fresh_access(pc)),
                }
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Self { instructions }
    }

    /// Derive an OSR entry view from the canonical optimized graph. GVN and
    /// liveness are deliberately re-rooted at the OSR entry because values
    /// produced before that entry have no native definition. Module facts,
    /// devirtualization, inline choices and shape analysis remain shared.
    pub(crate) fn project_osr(
        &self,
        ir: &crate::ir::FunctionIr,
        pc_range: std::ops::Range<usize>,
    ) -> Self {
        let lowering = analyze_artifact_lowering(ir, pc_range.clone(), true);
        let instructions = self
            .instructions
            .iter()
            .copied()
            .enumerate()
            .map(|(pc, mut instruction)| {
                instruction.block_executable =
                    lowering.executable_blocks[instruction.typed.block().index()];
                instruction.action = lowering.actions[pc];
                instruction.bounds_check_elided = bit_is_set(&lowering.elided_bounds_checks, pc);
                instruction.nil_check_elided = bit_is_set(&lowering.elided_nil_checks, pc);
                instruction.scalar_replacement = None;
                instruction.virtual_object = None;
                instruction.fresh_shape = instruction
                    .fresh_shape
                    .filter(|shape| pc_range.contains(&(shape.allocation_pc as usize)));
                instruction
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Self { instructions }
    }

    pub(crate) fn retained_bytes(&self) -> usize {
        core::mem::size_of::<Self>().saturating_add(
            self.instructions
                .len()
                .saturating_mul(core::mem::size_of::<OptimizedInstruction>()),
        )
    }

    #[inline]
    pub(crate) fn instruction(&self, pc: usize) -> Option<OptimizedInstruction> {
        self.instructions.get(pc).copied()
    }

    #[inline]
    pub(crate) fn is_executable(&self, pc: usize) -> bool {
        self.instructions
            .get(pc)
            .is_some_and(|instruction| instruction.block_executable)
    }

    #[inline]
    #[cfg(test)]
    pub(crate) fn eliminates(&self, pc: usize) -> bool {
        self.instructions
            .get(pc)
            .is_some_and(|node| node.action == LoweringAction::Eliminate)
    }

    #[inline]
    #[cfg(test)]
    pub(crate) fn always_takes_branch(&self, pc: usize) -> bool {
        self.instructions
            .get(pc)
            .is_some_and(|node| node.action == LoweringAction::AlwaysJump)
    }

    #[inline]
    #[cfg(test)]
    pub(crate) fn elides_bounds_check(&self, pc: usize) -> bool {
        self.instructions
            .get(pc)
            .is_some_and(|node| node.bounds_check_elided)
    }

    #[inline]
    #[cfg(test)]
    fn elides_nil_check(&self, pc: usize) -> bool {
        self.instructions
            .get(pc)
            .is_some_and(|node| node.nil_check_elided)
    }

    #[cfg(test)]
    pub(crate) fn replacement_value(&self, pc: usize) -> Option<crate::ir::ValueId> {
        match self.instructions.get(pc)?.action {
            LoweringAction::Replace(value) => Some(value),
            LoweringAction::Emit | LoweringAction::Eliminate | LoweringAction::AlwaysJump => None,
        }
    }

    #[cfg(test)]
    pub(crate) fn inline_target(&self, pc: usize) -> Option<u32> {
        self.instructions
            .get(pc)?
            .inline
            .map(|inline| inline.target)
    }

    pub(crate) fn inline_expansion_cost(&self, pc: usize) -> u32 {
        self.instructions
            .get(pc)
            .and_then(|node| node.inline)
            .map_or(0, |inline| inline.cost)
    }

    pub(crate) fn scalar_object_slots(&self) -> Vec<u16> {
        let object_count = self
            .instructions
            .iter()
            .filter_map(|instruction| instruction.scalar_replacement)
            .map(|replacement| replacement.object as usize + 1)
            .max()
            .unwrap_or(0);
        let mut slots = vec![0; object_count];
        for replacement in self
            .instructions
            .iter()
            .filter_map(|instruction| instruction.scalar_replacement)
        {
            slots[replacement.object as usize] = replacement.slots;
        }
        slots
    }

    #[cfg(test)]
    fn eliminated_count(&self) -> usize {
        self.instructions
            .iter()
            .filter(|node| node.action == LoweringAction::Eliminate)
            .count()
    }
}

fn analyze_artifact_lowering(
    ir: &crate::ir::FunctionIr,
    pc_range: std::ops::Range<usize>,
    preserve_osr_exit_state: bool,
) -> ArtifactLowering {
    let flow = ArtifactFlow::for_region(ir, &pc_range, preserve_osr_exit_state);
    let mut producers = vec![None; ir.value_count()];
    let mut uses = vec![0_u32; ir.value_count()];
    let mut candidates = vec![false; ir.instruction_count()];
    let gvn = global_gvn(ir, pc_range.clone(), &flow);
    let mut eliminated = gvn.redundant_instructions;
    let mut always_taken_branches = vec![0_u64; ir.instruction_count().div_ceil(64)];
    let replacement_values = gvn.replacement_values;

    for pc in 0..ir.instruction_count() {
        let instruction = *ir
            .instruction(pc)
            .expect("IR instruction cardinality is internally verified");
        if !pc_range.contains(&pc) {
            continue;
        }
        if !flow.executable_blocks[instruction.block().index()] {
            set_bit(&mut eliminated, pc);
            continue;
        }
        if bit_is_set(&eliminated, pc) {
            continue;
        }
        let source = instruction.source();
        let folded_branch = folded_conditional_branch(ir, &flow, pc);
        match folded_branch {
            Some(true) => set_bit(&mut always_taken_branches, pc),
            Some(false) => set_bit(&mut eliminated, pc),
            None if replacement_values[pc] != NO_REPLACEMENT => {
                uses[replacement_values[pc] as usize] =
                    uses[replacement_values[pc] as usize].saturating_add(1);
            }
            None => {
                for &input in ir.inputs(instruction) {
                    uses[input.index()] = uses[input.index()].saturating_add(1);
                }
            }
        }
        if let Some(state) = ir.frame_state(pc).copied() {
            for frame_value in ir.frame_values(state) {
                uses[frame_value.value.index()] = uses[frame_value.value.index()].saturating_add(1);
            }
        }
        for &output in ir.outputs(instruction) {
            producers[output.index()] = Some(pc);
        }
        candidates[pc] = instruction.effects().can_eliminate()
            && !matches!(
                source.opcode(),
                Opcode::Jump
                    | Opcode::JumpIf
                    | Opcode::JumpIfNot
                    | Opcode::ForLoop
                    | Opcode::Return
                    | Opcode::Panic
            );
    }
    if preserve_osr_exit_state {
        for value in flow.osr_exit_values(ir, &pc_range) {
            uses[value.index()] = uses[value.index()].saturating_add(1);
        }
    }
    for block in ir.blocks() {
        if !flow.executable_blocks[block.id.index()] {
            continue;
        }
        for &edge in &flow.successors[block.id.index()] {
            for argument in ir.edge_arguments(edge) {
                uses[argument.value.index()] = uses[argument.value.index()].saturating_add(1);
            }
        }
    }
    let mut pending = VecDeque::new();
    for (pc, &candidate) in candidates.iter().enumerate() {
        if candidate && outputs_are_dead(ir, pc, &uses) {
            pending.push_back(pc);
        }
    }
    while let Some(pc) = pending.pop_front() {
        let word = pc / 64;
        let mask = 1_u64 << (pc % 64);
        if eliminated[word] & mask != 0 || !outputs_are_dead(ir, pc, &uses) {
            continue;
        }
        eliminated[word] |= mask;
        let instruction = *ir
            .instruction(pc)
            .expect("queued DCE instruction must remain in the IR");
        if replacement_values[pc] != NO_REPLACEMENT {
            remove_dce_use(
                crate::ir::ValueId::from_index(replacement_values[pc] as usize),
                ir,
                &producers,
                &candidates,
                &mut uses,
                &mut pending,
            );
        } else {
            for &input in ir.inputs(instruction) {
                remove_dce_use(input, ir, &producers, &candidates, &mut uses, &mut pending);
            }
        }
    }

    let actions = (0..ir.instruction_count())
        .map(|pc| {
            if bit_is_set(&eliminated, pc) {
                LoweringAction::Eliminate
            } else if bit_is_set(&always_taken_branches, pc) {
                LoweringAction::AlwaysJump
            } else if replacement_values[pc] != NO_REPLACEMENT {
                LoweringAction::Replace(crate::ir::ValueId::from_index(
                    replacement_values[pc] as usize,
                ))
            } else {
                LoweringAction::Emit
            }
        })
        .collect();
    ArtifactLowering {
        actions,
        executable_blocks: flow.executable_blocks,
        elided_bounds_checks: gvn.elided_bounds_checks,
        elided_nil_checks: gvn.elided_nil_checks,
    }
}

fn remove_dce_use(
    value: crate::ir::ValueId,
    ir: &crate::ir::FunctionIr,
    producers: &[Option<usize>],
    candidates: &[bool],
    uses: &mut [u32],
    pending: &mut VecDeque<usize>,
) {
    let count = &mut uses[value.index()];
    *count = count.saturating_sub(1);
    if let Some(producer) = producers[value.index()] {
        if candidates[producer] && outputs_are_dead(ir, producer, uses) {
            pending.push_back(producer);
        }
    }
}

fn folded_conditional_branch(
    ir: &crate::ir::FunctionIr,
    flow: &ArtifactFlow,
    pc: usize,
) -> Option<bool> {
    let instruction = *ir.instruction(pc)?;
    let source = instruction.source();
    if !matches!(source.opcode(), Opcode::JumpIf | Opcode::JumpIfNot) {
        return None;
    }
    let fallthrough_pc = pc
        .checked_add(1)
        .filter(|&pc| pc < ir.instruction_count())?;
    let target_pc = crate::compile_common::checked_branch_target(
        ir.instruction_count(),
        pc,
        source.imm32(),
        source.opcode(),
    )
    .ok()?;
    let fallthrough = ir.instruction(fallthrough_pc)?.block();
    let target = ir.instruction(target_pc)?.block();
    if fallthrough == target {
        return None;
    }

    let mut fallthrough_executable = false;
    let mut target_executable = false;
    for edge in &flow.successors[instruction.block().index()] {
        fallthrough_executable |= edge.target == fallthrough;
        target_executable |= edge.target == target;
    }
    match (fallthrough_executable, target_executable) {
        (true, false) => Some(false),
        (false, true) => Some(true),
        (true, true) | (false, false) => None,
    }
}

fn global_gvn(
    ir: &crate::ir::FunctionIr,
    pc_range: std::ops::Range<usize>,
    flow: &ArtifactFlow,
) -> GvnPlan {
    let mut replacements = vec![NO_REPLACEMENT; ir.instruction_count()];
    let mut redundant_instructions = vec![0_u64; ir.instruction_count().div_ceil(64)];
    let mut elided_bounds_checks = vec![0_u64; ir.instruction_count().div_ceil(64)];
    let mut elided_nil_checks = vec![0_u64; ir.instruction_count().div_ceil(64)];
    if pc_range.is_empty() {
        return GvnPlan {
            replacement_values: replacements,
            redundant_instructions,
            elided_bounds_checks,
            elided_nil_checks,
        };
    }

    let Some(entry) = ir
        .instruction(pc_range.start)
        .map(|instruction| instruction.block())
    else {
        return GvnPlan {
            replacement_values: replacements,
            redundant_instructions,
            elided_bounds_checks,
            elided_nil_checks,
        };
    };
    let block_count = ir.blocks().len();
    let mut incoming = vec![Vec::<(crate::ir::BlockId, crate::ir::BlockEdge)>::new(); block_count];
    for block in ir.blocks() {
        if !flow.executable_blocks[block.id.index()] {
            continue;
        }
        for &edge in &flow.successors[block.id.index()] {
            let target_pc = ir.blocks()[edge.target.index()].start_pc as usize;
            if pc_range.contains(&target_pc) {
                incoming[edge.target.index()].push((block.id, edge));
            }
        }
    }

    let rpo = region_reverse_postorder(ir, flow, entry, &pc_range);
    if rpo.is_empty() {
        return GvnPlan {
            replacement_values: replacements,
            redundant_instructions,
            elided_bounds_checks,
            elided_nil_checks,
        };
    }
    let mut rpo_index = vec![usize::MAX; block_count];
    for (index, block) in rpo.iter().copied().enumerate() {
        rpo_index[block.index()] = index;
    }
    let mut idom = vec![None; block_count];
    idom[entry.index()] = Some(entry);
    loop {
        let mut changed = false;
        for &block in rpo.iter().skip(1) {
            let mut predecessors = incoming[block.index()]
                .iter()
                .map(|(source, _)| *source)
                .filter(|source| {
                    rpo_index[source.index()] != usize::MAX && idom[source.index()].is_some()
                });
            let Some(mut new_idom) = predecessors.next() else {
                continue;
            };
            for predecessor in predecessors {
                new_idom = intersect_dominators(predecessor, new_idom, &idom, &rpo_index);
            }
            if idom[block.index()] != Some(new_idom) {
                idom[block.index()] = Some(new_idom);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    let mut dominator_children = vec![Vec::new(); block_count];
    for &block in rpo.iter().skip(1) {
        if let Some(parent) = idom[block.index()] {
            dominator_children[parent.index()].push(block);
        }
    }

    // A compact union-find assigns the same value number to constants, copies,
    // and block parameters whose executable incoming values are congruent.
    // This lets a dominating expression remain available after SSA edge
    // arguments have introduced fresh block-parameter ValueIds.
    let mut leaders = (0..ir.value_count() as u32).collect::<Vec<_>>();
    let mut constant_leaders = HashMap::<(u8, i64), crate::ir::ValueId>::new();
    for index in 0..ir.value_count() {
        let value = crate::ir::ValueId::from_index(index);
        let Some(constant) = flow.constant(value) else {
            continue;
        };
        let key = (value_type_key(ir.value(value).ty), constant);
        if let Some(&representative) = constant_leaders.get(&key) {
            union_values(&mut leaders, value, representative);
        } else {
            constant_leaders.insert(key, value);
        }
    }
    for pc in pc_range.clone() {
        let Some(instruction) = ir.instruction(pc).copied() else {
            continue;
        };
        if instruction.source().opcode() == Opcode::Copy {
            if let (Some(&input), Some(&output)) = (
                ir.inputs(instruction).first(),
                ir.outputs(instruction).first(),
            ) {
                if ir.value(input).ty == ir.value(output).ty {
                    union_values(&mut leaders, input, output);
                }
            }
        }
    }
    loop {
        let mut changed = false;
        for &block in rpo.iter().skip(1) {
            for parameter in ir.block_parameters(block) {
                let parameter_leader = find_leader(&mut leaders, parameter.value);
                let mut incoming_leader = None;
                let mut congruent = true;
                for (source, edge) in &incoming[block.index()] {
                    if rpo_index[source.index()] == usize::MAX {
                        continue;
                    }
                    let Some(argument) = ir
                        .edge_arguments(*edge)
                        .iter()
                        .find(|argument| argument.slot == parameter.slot)
                    else {
                        continue;
                    };
                    let leader = find_leader(&mut leaders, argument.value);
                    if leader == parameter_leader {
                        continue;
                    }
                    match incoming_leader {
                        None => incoming_leader = Some(leader),
                        Some(expected) if expected == leader => {}
                        Some(_) => {
                            congruent = false;
                            break;
                        }
                    }
                }
                if congruent {
                    if let Some(incoming_leader) = incoming_leader {
                        changed |= union_values(&mut leaders, parameter.value, incoming_leader);
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }

    mark_range_proven_bounds_checks(
        ir,
        flow,
        &pc_range,
        &incoming,
        &rpo,
        &mut leaders,
        &mut elided_bounds_checks,
    );

    enum Visit {
        Enter(crate::ir::BlockId),
        Exit {
            expressions: Vec<ExpressionKey>,
            checks: Vec<BoundsCheckKey>,
            non_nil_values: Vec<u32>,
            field_undo: Vec<FieldUndo>,
        },
    }
    let mut expressions = HashMap::<ExpressionKey, crate::ir::ValueId>::new();
    let mut checked_bounds = HashSet::<BoundsCheckKey>::new();
    let mut non_nil_values = HashSet::<u32>::new();
    let mut field_values = HashMap::<FieldKey, crate::ir::ValueId>::new();
    let mut visits = vec![Visit::Enter(entry)];
    while let Some(visit) = visits.pop() {
        match visit {
            Visit::Enter(block) => {
                let record = &ir.blocks()[block.index()];
                let start = (record.start_pc as usize).max(pc_range.start);
                let end = (record.end_pc as usize).min(pc_range.end);
                let mut inserted = Vec::new();
                let mut inserted_checks = Vec::new();
                let mut inserted_non_nil = Vec::new();
                let mut field_undo = Vec::new();
                for (offset, replacement) in replacements[start..end].iter_mut().enumerate() {
                    let pc = start + offset;
                    let instruction = *ir
                        .instruction(pc)
                        .expect("dominator block bounds were verified against the IR");
                    let source = instruction.source();
                    if let Some(pointer) = checked_pointer_input(ir, pc) {
                        let pointer = find_leader(&mut leaders, pointer).index() as u32;
                        if non_nil_values.contains(&pointer) {
                            set_bit(&mut elided_nil_checks, pc);
                        } else if non_nil_values.insert(pointer) {
                            inserted_non_nil.push(pointer);
                        }
                    }
                    match source.opcode() {
                        Opcode::PtrGet => {
                            let Some(pointer) = ir.input_value(pc, source.b) else {
                                continue;
                            };
                            let Some(&output) = ir.outputs(instruction).first() else {
                                continue;
                            };
                            let key = FieldKey {
                                pointer: find_leader(&mut leaders, pointer).index() as u32,
                                offset: source.c,
                            };
                            if let Some(&value) = field_values.get(&key) {
                                *replacement = value.index() as u32;
                                union_values(&mut leaders, output, value);
                                set_bit(&mut elided_nil_checks, pc);
                            } else {
                                set_field_value(&mut field_values, &mut field_undo, key, output);
                            }
                        }
                        Opcode::PtrSet => {
                            let Some(pointer) = ir.input_value(pc, source.a) else {
                                continue;
                            };
                            let Some(value) = ir.input_value(pc, source.c) else {
                                continue;
                            };
                            let key = FieldKey {
                                pointer: find_leader(&mut leaders, pointer).index() as u32,
                                offset: source.b,
                            };
                            invalidate_field_values_for_store(
                                &mut field_values,
                                &mut field_undo,
                                key,
                            );
                            set_field_value(&mut field_values, &mut field_undo, key, value);
                        }
                        opcode if invalidates_field_values(opcode) => {
                            clear_field_values(&mut field_values, &mut field_undo);
                        }
                        _ => {}
                    }
                    if source.opcode() == Opcode::IndexCheck {
                        let inputs = ir.inputs(instruction);
                        if inputs.len() == 2 {
                            let key = BoundsCheckKey {
                                kind: EXPLICIT_INDEX_CHECK,
                                container: find_leader(&mut leaders, inputs[0]).index() as u32,
                                index: find_leader(&mut leaders, inputs[1]).index() as u32,
                            };
                            if checked_bounds.contains(&key) {
                                set_bit(&mut redundant_instructions, pc);
                            } else {
                                checked_bounds.insert(key);
                                inserted_checks.push(key);
                            }
                        }
                        continue;
                    }
                    if let Some(key) = collection_bounds_check_key(ir, pc, &mut leaders) {
                        if checked_bounds.contains(&key) {
                            set_bit(&mut elided_bounds_checks, pc);
                        } else {
                            checked_bounds.insert(key);
                            inserted_checks.push(key);
                        }
                    }
                    if let Some(key) = expression_key(ir, instruction, &mut leaders) {
                        let output = ir.outputs(instruction)[0];
                        if let Some(&value) = expressions.get(&key) {
                            if !is_checked_collection_address(source.opcode())
                                || bit_is_set(&elided_bounds_checks, pc)
                            {
                                *replacement = value.index() as u32;
                                union_values(&mut leaders, output, value);
                            }
                        } else {
                            expressions.insert(key.clone(), output);
                            inserted.push(key);
                        }
                    }
                    if successful_output_is_non_nil(source.opcode()) {
                        if let Some(&output) = ir.outputs(instruction).first() {
                            let output = find_leader(&mut leaders, output).index() as u32;
                            if non_nil_values.insert(output) {
                                inserted_non_nil.push(output);
                            }
                        }
                    }
                }
                visits.push(Visit::Exit {
                    expressions: inserted,
                    checks: inserted_checks,
                    non_nil_values: inserted_non_nil,
                    field_undo,
                });
                for &child in dominator_children[block.index()].iter().rev() {
                    visits.push(Visit::Enter(child));
                }
            }
            Visit::Exit {
                expressions: inserted,
                checks,
                non_nil_values: inserted_non_nil,
                field_undo,
            } => {
                for key in inserted {
                    expressions.remove(&key);
                }
                for key in checks {
                    checked_bounds.remove(&key);
                }
                for value in inserted_non_nil {
                    non_nil_values.remove(&value);
                }
                for undo in field_undo.into_iter().rev() {
                    match undo {
                        FieldUndo::Remove(key) => {
                            field_values.remove(&key);
                        }
                        FieldUndo::Restore(key, value) => {
                            field_values.insert(key, value);
                        }
                    }
                }
            }
        }
    }
    GvnPlan {
        replacement_values: replacements,
        redundant_instructions,
        elided_bounds_checks,
        elided_nil_checks,
    }
}

fn collection_bounds_check_key(
    ir: &crate::ir::FunctionIr,
    pc: usize,
    leaders: &mut [u32],
) -> Option<BoundsCheckKey> {
    let source = ir.instruction(pc)?.source();
    let (kind, container_slot, index_slot) = match source.opcode() {
        Opcode::ArrayGet | Opcode::ArrayAddr => (ARRAY_BOUNDS_CHECK, source.b, source.c),
        Opcode::ArraySet => (ARRAY_BOUNDS_CHECK, source.a, source.b),
        Opcode::SliceGet | Opcode::SliceAddr => (SLICE_BOUNDS_CHECK, source.b, source.c),
        Opcode::SliceSet => (SLICE_BOUNDS_CHECK, source.a, source.b),
        Opcode::StrIndex => (STRING_BOUNDS_CHECK, source.b, source.c),
        _ => return None,
    };
    let container = find_leader(leaders, ir.input_value(pc, container_slot)?).index() as u32;
    let index = find_leader(leaders, ir.input_value(pc, index_slot)?).index() as u32;
    Some(BoundsCheckKey {
        kind,
        container,
        index,
    })
}

fn mark_range_proven_bounds_checks(
    ir: &crate::ir::FunctionIr,
    flow: &ArtifactFlow,
    pc_range: &std::ops::Range<usize>,
    incoming: &[Vec<(crate::ir::BlockId, crate::ir::BlockEdge)>],
    rpo: &[crate::ir::BlockId],
    leaders: &mut [u32],
    elided_bounds_checks: &mut [u64],
) {
    let mut lengths = HashMap::<(u8, u32), u32>::new();
    let mut comparisons = HashMap::<u32, (Opcode, u32, u32)>::new();

    for pc in pc_range.clone() {
        let Some(instruction) = ir.instruction(pc).copied() else {
            continue;
        };
        let source = instruction.source();
        match source.opcode() {
            Opcode::SliceLen | Opcode::StrLen => {
                let Some(&output) = ir.outputs(instruction).first() else {
                    continue;
                };
                let Some(container) = ir.input_value(pc, source.b) else {
                    continue;
                };
                let kind = if source.opcode() == Opcode::SliceLen {
                    SLICE_BOUNDS_CHECK
                } else {
                    STRING_BOUNDS_CHECK
                };
                lengths.insert(
                    (kind, find_leader(leaders, container).index() as u32),
                    find_leader(leaders, output).index() as u32,
                );
            }
            Opcode::LtI
            | Opcode::LtU
            | Opcode::LeI
            | Opcode::LeU
            | Opcode::GtI
            | Opcode::GtU
            | Opcode::GeI
            | Opcode::GeU => {
                let inputs = ir.inputs(instruction);
                let Some((&lhs, &rhs, &output)) = inputs
                    .first()
                    .zip(inputs.get(1))
                    .zip(ir.outputs(instruction).first())
                    .map(|((lhs, rhs), output)| (lhs, rhs, output))
                else {
                    continue;
                };
                comparisons.insert(
                    find_leader(leaders, output).index() as u32,
                    (
                        source.opcode(),
                        find_leader(leaders, lhs).index() as u32,
                        find_leader(leaders, rhs).index() as u32,
                    ),
                );
            }
            _ => {}
        }
    }

    let Some(&entry) = rpo.first() else {
        return;
    };
    let mut in_region = vec![false; ir.blocks().len()];
    for &block in rpo {
        in_region[block.index()] = true;
    }

    // Range facts live on CFG edges and are renamed through the successor's
    // block parameters. This one representation covers canonical ForLoop,
    // ordinary compare/branch/backedge loops and nested control flow.
    let mut block_facts = vec![None::<HashSet<RangeFact>>; ir.blocks().len()];
    block_facts[entry.index()] = Some(HashSet::new());
    let mut converged = false;
    let max_iterations = rpo.len().saturating_mul(4).max(8);
    for _ in 0..max_iterations {
        let mut block_out = vec![None::<HashSet<RangeFact>>; ir.blocks().len()];
        for &block in rpo {
            let Some(mut facts) = block_facts[block.index()].clone() else {
                continue;
            };
            let record = &ir.blocks()[block.index()];
            for pc in (record.start_pc as usize).max(pc_range.start)
                ..(record.end_pc as usize).min(pc_range.end)
            {
                transfer_range_instruction(ir, flow, pc, leaders, &mut facts);
            }
            block_out[block.index()] = Some(facts);
        }

        let mut next = vec![None::<HashSet<RangeFact>>; ir.blocks().len()];
        next[entry.index()] = Some(HashSet::new());
        for &target in rpo.iter().filter(|&&block| block != entry) {
            let mut merged = None::<HashSet<RangeFact>>;
            for (source, edge) in &incoming[target.index()] {
                if !in_region[source.index()] {
                    continue;
                }
                let Some(mut candidate) = block_out[source.index()].clone() else {
                    continue;
                };
                candidate.extend(edge_range_facts(
                    ir,
                    *source,
                    *edge,
                    pc_range,
                    &comparisons,
                    leaders,
                ));
                candidate = candidate
                    .into_iter()
                    .map(|fact| translate_range_fact(ir, *edge, fact, leaders))
                    .collect();
                match &mut merged {
                    None => merged = Some(candidate),
                    Some(current) => current.retain(|fact| candidate.contains(fact)),
                }
            }
            next[target.index()] = merged;
        }
        if next == block_facts {
            converged = true;
            break;
        }
        block_facts = next;
    }
    if !converged {
        return;
    }

    for &block in rpo {
        let Some(mut facts) = block_facts[block.index()].clone() else {
            continue;
        };
        let record = &ir.blocks()[block.index()];
        let start = (record.start_pc as usize).max(pc_range.start);
        let end = (record.end_pc as usize).min(pc_range.end);
        for pc in start..end {
            if let Some(key) = collection_bounds_check_key(ir, pc, leaders) {
                if let Some(&limit) = lengths.get(&(key.kind, key.container)) {
                    let signed_proof = facts.contains(&RangeFact::NonNegative(key.index))
                        && facts.contains(&RangeFact::LessThan {
                            index: key.index,
                            limit,
                            unsigned: false,
                        });
                    let unsigned_proof = facts.contains(&RangeFact::LessThan {
                        index: key.index,
                        limit,
                        unsigned: true,
                    });
                    if signed_proof || unsigned_proof {
                        set_bit(elided_bounds_checks, pc);
                    }
                }
            }
            transfer_range_instruction(ir, flow, pc, leaders, &mut facts);
        }
    }
}

fn transfer_range_instruction(
    ir: &crate::ir::FunctionIr,
    flow: &ArtifactFlow,
    pc: usize,
    leaders: &mut [u32],
    facts: &mut HashSet<RangeFact>,
) {
    let Some(instruction) = ir.instruction(pc).copied() else {
        return;
    };
    for &output in ir.outputs(instruction) {
        if flow.constant(output).is_some_and(|value| value >= 0) {
            facts.insert(RangeFact::NonNegative(
                find_leader(leaders, output).index() as u32
            ));
        }
    }
    let source = instruction.source();
    if matches!(source.opcode(), Opcode::SliceLen | Opcode::StrLen) {
        if let Some(&output) = ir.outputs(instruction).first() {
            facts.insert(RangeFact::NonNegative(
                find_leader(leaders, output).index() as u32
            ));
        }
        return;
    }

    let increment = match source.opcode() {
        Opcode::AddI => {
            let inputs = ir.inputs(instruction);
            let Some((&lhs, &rhs, &output)) = inputs
                .first()
                .zip(inputs.get(1))
                .zip(ir.outputs(instruction).first())
                .map(|((lhs, rhs), output)| (lhs, rhs, output))
            else {
                return;
            };
            if flow.constant(rhs) == Some(1) {
                Some((lhs, output))
            } else if flow.constant(lhs) == Some(1) {
                Some((rhs, output))
            } else {
                None
            }
        }
        Opcode::ForLoop if source.flags & 0b110 == 0 => ir
            .input_value(pc, source.a)
            .zip(ir.output_value(pc, source.a)),
        _ => None,
    };
    let Some((index, next_index)) = increment else {
        return;
    };
    let index = find_leader(leaders, index).index() as u32;
    let next_index = find_leader(leaders, next_index).index() as u32;
    let preserves_nonnegative = facts.contains(&RangeFact::NonNegative(index))
        && facts.iter().any(|fact| {
            matches!(
                fact,
                RangeFact::LessThan {
                    index: bounded,
                    unsigned: false,
                    ..
                } if *bounded == index
            )
        });
    if preserves_nonnegative {
        facts.insert(RangeFact::NonNegative(next_index));
    }
}

fn edge_range_facts(
    ir: &crate::ir::FunctionIr,
    block: crate::ir::BlockId,
    edge: crate::ir::BlockEdge,
    pc_range: &std::ops::Range<usize>,
    comparisons: &HashMap<u32, (Opcode, u32, u32)>,
    leaders: &mut [u32],
) -> Vec<RangeFact> {
    let record = &ir.blocks()[block.index()];
    let Some(pc) = (record.end_pc as usize).checked_sub(1) else {
        return Vec::new();
    };
    if !pc_range.contains(&pc) {
        return Vec::new();
    }
    let Some(instruction) = ir.instruction(pc).copied() else {
        return Vec::new();
    };
    let source = instruction.source();
    if matches!(source.opcode(), Opcode::JumpIf | Opcode::JumpIfNot) {
        let Ok(target_pc) = crate::compile_common::checked_branch_target(
            ir.instruction_count(),
            pc,
            source.imm32(),
            source.opcode(),
        ) else {
            return Vec::new();
        };
        if target_pc == pc + 1 {
            return Vec::new();
        }
        let edge_pc = ir.blocks()[edge.target.index()].start_pc as usize;
        let condition_is_true = match source.opcode() {
            Opcode::JumpIf => edge_pc == target_pc,
            Opcode::JumpIfNot => edge_pc == pc + 1,
            _ => false,
        };
        let Some(condition) = ir.input_value(pc, source.a) else {
            return Vec::new();
        };
        let condition = find_leader(leaders, condition).index() as u32;
        let Some(&(opcode, lhs, rhs)) = comparisons.get(&condition) else {
            return Vec::new();
        };
        let proof = match (opcode, condition_is_true) {
            (Opcode::LtI, true) | (Opcode::GeI, false) => Some((lhs, rhs, false)),
            (Opcode::LtU, true) | (Opcode::GeU, false) => Some((lhs, rhs, true)),
            (Opcode::GtI, true) | (Opcode::LeI, false) => Some((rhs, lhs, false)),
            (Opcode::GtU, true) | (Opcode::LeU, false) => Some((rhs, lhs, true)),
            _ => None,
        };
        return proof
            .map(|(index, limit, unsigned)| {
                vec![RangeFact::LessThan {
                    index,
                    limit,
                    unsigned,
                }]
            })
            .unwrap_or_default();
    }

    if source.opcode() == Opcode::ForLoop && source.flags & 0b110 == 0 {
        let Ok(target_pc) =
            crate::compile_common::checked_forloop_target(ir.instruction_count(), pc, &source)
        else {
            return Vec::new();
        };
        if ir.blocks()[edge.target.index()].start_pc as usize != target_pc {
            return Vec::new();
        }
        let Some(index) = ir.output_value(pc, source.a) else {
            return Vec::new();
        };
        let Some(limit) = ir.input_value(pc, source.b) else {
            return Vec::new();
        };
        return vec![RangeFact::LessThan {
            index: find_leader(leaders, index).index() as u32,
            limit: find_leader(leaders, limit).index() as u32,
            unsigned: source.flags & 0b001 != 0,
        }];
    }
    Vec::new()
}

fn translate_range_fact(
    ir: &crate::ir::FunctionIr,
    edge: crate::ir::BlockEdge,
    fact: RangeFact,
    leaders: &mut [u32],
) -> RangeFact {
    let translate = |leader: u32, leaders: &mut [u32]| {
        let mut translated = None;
        for argument in ir.edge_arguments(edge) {
            if find_leader(leaders, argument.value).index() as u32 != leader {
                continue;
            }
            let Some(parameter) = ir
                .block_parameters(edge.target)
                .iter()
                .find(|parameter| parameter.slot == argument.slot)
            else {
                continue;
            };
            let parameter = find_leader(leaders, parameter.value).index() as u32;
            match translated {
                None => translated = Some(parameter),
                Some(previous) if previous == parameter => {}
                Some(_) => return leader,
            }
        }
        translated.unwrap_or(leader)
    };
    match fact {
        RangeFact::NonNegative(value) => RangeFact::NonNegative(translate(value, leaders)),
        RangeFact::LessThan {
            index,
            limit,
            unsigned,
        } => RangeFact::LessThan {
            index: translate(index, leaders),
            limit: translate(limit, leaders),
            unsigned,
        },
    }
}

fn region_reverse_postorder(
    ir: &crate::ir::FunctionIr,
    flow: &ArtifactFlow,
    entry: crate::ir::BlockId,
    pc_range: &std::ops::Range<usize>,
) -> Vec<crate::ir::BlockId> {
    let mut visited = vec![false; ir.blocks().len()];
    let mut postorder = Vec::new();
    let mut pending = vec![(entry, false)];
    while let Some((block, expanded)) = pending.pop() {
        if expanded {
            postorder.push(block);
            continue;
        }
        if std::mem::replace(&mut visited[block.index()], true) {
            continue;
        }
        pending.push((block, true));
        let mut successors = flow.successors[block.index()]
            .iter()
            .map(|edge| edge.target)
            .filter(|target| {
                let target_pc = ir.blocks()[target.index()].start_pc as usize;
                pc_range.contains(&target_pc)
            })
            .collect::<Vec<_>>();
        successors.reverse();
        pending.extend(successors.into_iter().map(|successor| (successor, false)));
    }
    postorder.reverse();
    postorder
}

fn intersect_dominators(
    mut lhs: crate::ir::BlockId,
    mut rhs: crate::ir::BlockId,
    idom: &[Option<crate::ir::BlockId>],
    rpo_index: &[usize],
) -> crate::ir::BlockId {
    while lhs != rhs {
        while rpo_index[lhs.index()] > rpo_index[rhs.index()] {
            lhs = idom[lhs.index()].expect("processed dominator predecessor");
        }
        while rpo_index[rhs.index()] > rpo_index[lhs.index()] {
            rhs = idom[rhs.index()].expect("processed dominator predecessor");
        }
    }
    lhs
}

fn find_leader(leaders: &mut [u32], value: crate::ir::ValueId) -> crate::ir::ValueId {
    let index = value.index();
    let parent = leaders[index] as usize;
    if parent == index {
        return value;
    }
    let root = find_leader(leaders, crate::ir::ValueId::from_index(parent));
    leaders[index] = root.index() as u32;
    root
}

fn union_values(leaders: &mut [u32], lhs: crate::ir::ValueId, rhs: crate::ir::ValueId) -> bool {
    let lhs = find_leader(leaders, lhs);
    let rhs = find_leader(leaders, rhs);
    if lhs == rhs {
        return false;
    }
    let (leader, merged) = if lhs < rhs { (lhs, rhs) } else { (rhs, lhs) };
    leaders[merged.index()] = leader.index() as u32;
    true
}

fn value_type_key(value_type: crate::ir::ValueType) -> u8 {
    match value_type {
        crate::ir::ValueType::Word => 0,
        crate::ir::ValueType::Float64 => 1,
        crate::ir::ValueType::GcRef(_) => 2,
        crate::ir::ValueType::InterfaceHeader => 3,
        crate::ir::ValueType::InterfaceData => 4,
    }
}

fn expression_key(
    ir: &crate::ir::FunctionIr,
    instruction: crate::ir::TypedInstruction,
    leaders: &mut [u32],
) -> Option<ExpressionKey> {
    let source = instruction.source();
    let checked_collection_address = is_checked_collection_address(source.opcode());
    if (!instruction.effects().can_eliminate() && !checked_collection_address)
        || ir.outputs(instruction).len() != 1
        || !matches!(
            source.opcode(),
            Opcode::LoadInt
                | Opcode::LoadConst
                | Opcode::Copy
                | Opcode::AddI
                | Opcode::SubI
                | Opcode::MulI
                | Opcode::NegI
                | Opcode::AddF
                | Opcode::SubF
                | Opcode::MulF
                | Opcode::DivF
                | Opcode::NegF
                | Opcode::EqI
                | Opcode::NeI
                | Opcode::LtI
                | Opcode::LeI
                | Opcode::GtI
                | Opcode::GeI
                | Opcode::LtU
                | Opcode::LeU
                | Opcode::GtU
                | Opcode::GeU
                | Opcode::EqF
                | Opcode::NeF
                | Opcode::LtF
                | Opcode::LeF
                | Opcode::GtF
                | Opcode::GeF
                | Opcode::Not
                | Opcode::BoolNot
                | Opcode::And
                | Opcode::Or
                | Opcode::Xor
                | Opcode::AndNot
                | Opcode::Shl
                | Opcode::ShrS
                | Opcode::ShrU
                | Opcode::ConvI2F
                | Opcode::ConvF2I
                | Opcode::ConvF64F32
                | Opcode::ConvF32F64
                | Opcode::Trunc
                | Opcode::SliceLen
                | Opcode::SliceCap
                | Opcode::StrLen
                | Opcode::ArrayAddr
                | Opcode::SliceAddr
        )
    {
        return None;
    }
    let mut inputs = ir
        .inputs(instruction)
        .iter()
        .map(|&input| find_leader(leaders, input).index() as u32)
        .collect::<Vec<_>>();
    if matches!(
        source.opcode(),
        Opcode::AddI
            | Opcode::MulI
            | Opcode::EqI
            | Opcode::NeI
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
    ) {
        inputs.sort_unstable();
    }
    let immediate = match source.opcode() {
        Opcode::LoadInt => source.imm32() as u32,
        Opcode::LoadConst => u32::from(source.b),
        _ => 0,
    };
    Some(ExpressionKey {
        opcode: source.opcode() as u8,
        flags: source.flags,
        result_type: value_type_key(ir.value(ir.outputs(instruction)[0]).ty),
        immediate,
        inputs: inputs.into_boxed_slice(),
    })
}

fn is_checked_collection_address(opcode: Opcode) -> bool {
    matches!(opcode, Opcode::ArrayAddr | Opcode::SliceAddr)
}

fn checked_pointer_input(ir: &crate::ir::FunctionIr, pc: usize) -> Option<crate::ir::ValueId> {
    let source = ir.instruction(pc)?.source();
    let slot = match source.opcode() {
        Opcode::PtrGet | Opcode::PtrGetN => source.b,
        Opcode::PtrSet | Opcode::PtrSetN => source.a,
        _ => return None,
    };
    ir.input_value(pc, slot)
}

fn successful_output_is_non_nil(opcode: Opcode) -> bool {
    matches!(opcode, Opcode::ArrayAddr | Opcode::SliceAddr)
}

fn set_field_value(
    values: &mut HashMap<FieldKey, crate::ir::ValueId>,
    undo: &mut Vec<FieldUndo>,
    key: FieldKey,
    value: crate::ir::ValueId,
) {
    match values.insert(key, value) {
        Some(previous) => undo.push(FieldUndo::Restore(key, previous)),
        None => undo.push(FieldUndo::Remove(key)),
    }
}

fn invalidate_field_values_for_store(
    values: &mut HashMap<FieldKey, crate::ir::ValueId>,
    undo: &mut Vec<FieldUndo>,
    written: FieldKey,
) {
    let invalidated = values
        .keys()
        .copied()
        .filter(|key| key.pointer != written.pointer || key.offset == written.offset)
        .collect::<Vec<_>>();
    for key in invalidated {
        if let Some(value) = values.remove(&key) {
            undo.push(FieldUndo::Restore(key, value));
        }
    }
}

fn clear_field_values(
    values: &mut HashMap<FieldKey, crate::ir::ValueId>,
    undo: &mut Vec<FieldUndo>,
) {
    undo.extend(
        values
            .drain()
            .map(|(key, value)| FieldUndo::Restore(key, value)),
    );
}

fn invalidates_field_values(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::PtrSetN
            | Opcode::ArraySet
            | Opcode::SliceSet
            | Opcode::MapSet
            | Opcode::MapDelete
            | Opcode::QueueSend
            | Opcode::QueueRecv
            | Opcode::QueueClose
            | Opcode::SelectExec
            | Opcode::Call
            | Opcode::CallExtern
            | Opcode::CallClosure
            | Opcode::CallIface
            | Opcode::GoStart
            | Opcode::GoIsland
            | Opcode::DeferPush
            | Opcode::ErrDeferPush
            | Opcode::Recover
    )
}

fn set_bit(words: &mut [u64], index: usize) {
    words[index / 64] |= 1_u64 << (index % 64);
}

fn bit_is_set(words: &[u64], index: usize) -> bool {
    words
        .get(index / 64)
        .is_some_and(|word| word & (1_u64 << (index % 64)) != 0)
}

fn outputs_are_dead(ir: &crate::ir::FunctionIr, pc: usize, uses: &[u32]) -> bool {
    let instruction = *ir
        .instruction(pc)
        .expect("DCE pc must name an IR instruction");
    ir.outputs(instruction)
        .iter()
        .all(|output| uses[output.index()] == 0)
}

fn plan_inlines(
    ir: &crate::ir::FunctionIr,
    pc_range: &std::ops::Range<usize>,
    module: &ModuleInlinePlan,
    caller_id: u32,
    dynamic_targets: &[u32],
    allow_self_recursion: bool,
) -> (Vec<u32>, Vec<u32>) {
    let mut targets = vec![NO_DYNAMIC_TARGET; ir.instruction_count()];
    let mut costs = vec![0_u32; ir.instruction_count()];
    let mut retained_cost = 0usize;
    for pc in pc_range.clone() {
        let Some(instruction) = ir.instruction(pc).copied() else {
            continue;
        };
        if !ir.is_executable_block(instruction.block()) {
            continue;
        }
        let source = instruction.source();
        let target = match source.opcode() {
            Opcode::Call => source.static_call_func_id(),
            Opcode::CallClosure | Opcode::CallIface => {
                let target = dynamic_targets[pc];
                if target == NO_DYNAMIC_TARGET {
                    continue;
                }
                target
            }
            _ => continue,
        };
        let recipe = if source.opcode() == Opcode::Call && allow_self_recursion {
            module.static_inline(caller_id, target)
        } else {
            module.pure_leaf_inline(caller_id, target)
        };
        let Some(recipe) = recipe else {
            continue;
        };
        if matches!(source.opcode(), Opcode::CallClosure | Opcode::CallIface) {
            let receiver_slots = if source.opcode() == Opcode::CallClosure {
                1
            } else {
                2
            };
            let arg_slots = ir.inputs(instruction).len().saturating_sub(receiver_slots);
            if !recipe.supports_dynamic_layout(arg_slots, ir.outputs(instruction).len()) {
                continue;
            }
        }
        let Some(next_cost) = retained_cost.checked_add(recipe.cost()) else {
            continue;
        };
        if next_cost > SMALL_INLINE_BUDGET {
            continue;
        }
        retained_cost = next_cost;
        targets[pc] = target;
        costs[pc] = recipe.cost().try_into().unwrap_or(u32::MAX);
    }
    (targets, costs)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CallableValue {
    Bottom,
    Closure(u32),
    InterfaceItab(u32),
    Top,
}

impl CallableValue {
    fn join(self, other: Self) -> Self {
        match (self, other) {
            (Self::Bottom, value) | (value, Self::Bottom) => value,
            (Self::Closure(lhs), Self::Closure(rhs)) if lhs == rhs => Self::Closure(lhs),
            (Self::InterfaceItab(lhs), Self::InterfaceItab(rhs)) if lhs == rhs => {
                Self::InterfaceItab(lhs)
            }
            (Self::Top, _) | (_, Self::Top) => Self::Top,
            _ => Self::Top,
        }
    }
}

fn analyze_callable_values(
    ir: &crate::ir::FunctionIr,
    module: &ModuleOptimizationPlan,
) -> Vec<u32> {
    let mut defined = vec![false; ir.value_count()];
    for block in ir.blocks() {
        for parameter in ir.block_parameters(block.id) {
            defined[parameter.value.index()] = true;
        }
    }
    for pc in 0..ir.instruction_count() {
        let instruction = *ir
            .instruction(pc)
            .expect("callable analysis follows verified IR cardinality");
        for &output in ir.outputs(instruction) {
            defined[output.index()] = true;
        }
    }
    let mut values = defined
        .into_iter()
        .map(|defined| {
            if defined {
                CallableValue::Bottom
            } else {
                CallableValue::Top
            }
        })
        .collect::<Vec<_>>();

    loop {
        let mut changed = false;
        for block in ir.blocks() {
            if !ir.is_executable_block(block.id) {
                continue;
            }
            for parameter in ir.block_parameters(block.id) {
                let mut incoming = CallableValue::Bottom;
                for &predecessor in ir.predecessors(block.id) {
                    let Some(edge) = ir
                        .executable_successors(predecessor)
                        .find(|edge| edge.target == block.id)
                    else {
                        continue;
                    };
                    let Some(argument) = ir
                        .edge_arguments(edge)
                        .iter()
                        .find(|argument| argument.slot == parameter.slot)
                    else {
                        incoming = incoming.join(CallableValue::Top);
                        continue;
                    };
                    incoming = incoming.join(values[argument.value.index()]);
                }
                let index = parameter.value.index();
                let refined = values[index].join(incoming);
                if refined != values[index] {
                    values[index] = refined;
                    changed = true;
                }
            }
        }

        for pc in 0..ir.instruction_count() {
            let instruction = *ir
                .instruction(pc)
                .expect("callable analysis follows verified IR cardinality");
            if !ir.is_executable_block(instruction.block()) {
                continue;
            }
            let source = instruction.source();
            for &output in ir.outputs(instruction) {
                let output_record = ir.value(output);
                let candidate = match source.opcode() {
                    Opcode::ClosureNew if output_record.slot == source.a => {
                        CallableValue::Closure(source.closure_new_func_id())
                    }
                    Opcode::IfaceAssign if output_record.slot == source.a => module
                        .concrete_iface_assign_itab(source)
                        .map(CallableValue::InterfaceItab)
                        .unwrap_or(CallableValue::Top),
                    Opcode::Copy | Opcode::CopyN => {
                        let offset = output_record.slot.saturating_sub(source.a);
                        ir.input_value(pc, source.b.saturating_add(offset))
                            .map(|input| values[input.index()])
                            .unwrap_or(CallableValue::Top)
                    }
                    Opcode::Call => {
                        let callee = source.static_call_func_id();
                        let return_slot = module
                            .function(callee)
                            .map(|function| source.b.saturating_add(function.param_slots));
                        if return_slot == Some(output_record.slot) {
                            module
                                .returned_closure_target(callee)
                                .map(CallableValue::Closure)
                                .unwrap_or(CallableValue::Top)
                        } else {
                            CallableValue::Top
                        }
                    }
                    _ => CallableValue::Top,
                };
                let index = output.index();
                let refined = values[index].join(candidate);
                if refined != values[index] {
                    values[index] = refined;
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }

    let mut targets = vec![NO_DYNAMIC_TARGET; ir.instruction_count()];
    for (pc, target) in targets.iter_mut().enumerate() {
        let Some(instruction) = ir.instruction(pc).copied() else {
            continue;
        };
        let source = instruction.source();
        *target = match source.opcode() {
            Opcode::CallClosure => ir
                .input_value(pc, source.a)
                .and_then(|value| match values[value.index()] {
                    CallableValue::Closure(target) => Some(target),
                    _ => None,
                })
                .unwrap_or(NO_DYNAMIC_TARGET),
            Opcode::CallIface => ir
                .input_value(pc, source.a)
                .and_then(|value| match values[value.index()] {
                    CallableValue::InterfaceItab(itab) => ir
                        .call_iface_method_index(pc)
                        .and_then(|method| module.interface_method_target(itab, method)),
                    _ => None,
                })
                .unwrap_or(NO_DYNAMIC_TARGET),
            _ => NO_DYNAMIC_TARGET,
        };
    }
    targets
}

fn returned_closure_targets(module: &Module) -> Vec<u32> {
    let mut summaries = vec![NO_DYNAMIC_TARGET; module.functions.len()];
    loop {
        let mut changed = false;
        for (func_id, function) in module.functions.iter().enumerate() {
            if summaries[func_id] != NO_DYNAMIC_TARGET {
                continue;
            }
            let Some(target) = returned_closure_target(function, module, &summaries) else {
                continue;
            };
            summaries[func_id] = target;
            changed = true;
        }
        if !changed {
            break;
        }
    }
    summaries
}

fn returned_closure_target(
    function: &FunctionDef,
    module: &Module,
    summaries: &[u32],
) -> Option<u32> {
    if function.ret_slots == 0
        || function.ret_slot_types.first() != Some(&vo_runtime::SlotType::GcBase)
        || function.code.iter().any(|instruction| {
            matches!(
                instruction.opcode(),
                Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot | Opcode::ForLoop
            )
        })
    {
        return None;
    }
    let mut writers = vec![Vec::<usize>::new(); usize::from(function.local_slots)];
    for (pc, instruction) in function.code.iter().enumerate() {
        let facts =
            crate::effects::EffectFacts::from_instruction(function.instruction_metadata.get(pc));
        let writes = crate::effects::try_write_regs_with_module_context(
            instruction,
            facts,
            &module.externs,
            &module.functions,
        )
        .ok()?;
        for slot in writes {
            writers.get_mut(slot as usize)?.push(pc);
        }
    }

    let mut common = None;
    let mut saw_return = false;
    for (return_pc, instruction) in function.code.iter().enumerate() {
        if instruction.opcode() != Opcode::Return {
            continue;
        }
        saw_return = true;
        let target = trace_returned_closure(
            function,
            module,
            &writers,
            summaries,
            instruction.a,
            return_pc,
            &mut Vec::new(),
        )?;
        match common {
            None => common = Some(target),
            Some(expected) if expected == target => {}
            Some(_) => return None,
        }
    }
    saw_return.then_some(common?)
}

fn trace_returned_closure(
    function: &FunctionDef,
    module: &Module,
    writers: &[Vec<usize>],
    summaries: &[u32],
    slot: u16,
    before_pc: usize,
    visiting: &mut Vec<u16>,
) -> Option<u32> {
    if visiting.contains(&slot) {
        return None;
    }
    visiting.push(slot);
    let result = (|| {
        let &[pc] = writers.get(slot as usize)?.as_slice() else {
            return None;
        };
        if pc >= before_pc {
            return None;
        }
        let instruction = function.code.get(pc)?;
        match instruction.opcode() {
            Opcode::ClosureNew if instruction.a == slot => Some(instruction.closure_new_func_id()),
            Opcode::Copy if instruction.a == slot => trace_returned_closure(
                function,
                module,
                writers,
                summaries,
                instruction.b,
                pc,
                visiting,
            ),
            Opcode::CopyN if slot >= instruction.a => {
                let offset = slot.checked_sub(instruction.a)?;
                trace_returned_closure(
                    function,
                    module,
                    writers,
                    summaries,
                    instruction.b.checked_add(offset)?,
                    pc,
                    visiting,
                )
            }
            Opcode::Call => {
                let callee = instruction.static_call_func_id();
                let callee_shape = module.functions.get(callee as usize)?.param_slots;
                let first_return = instruction.b.checked_add(callee_shape)?;
                if first_return != slot {
                    return None;
                }
                let target = *summaries.get(callee as usize)?;
                (target != NO_DYNAMIC_TARGET).then_some(target)
            }
            _ => None,
        }
    })();
    visiting.pop();
    result
}

pub(crate) struct ModuleInlinePlan {
    graph: Arc<ModuleCallGraph>,
    small_inlines: Box<[Option<Arc<SmallFunctionInline>>]>,
}

impl ModuleInlinePlan {
    pub(crate) fn build_with_graph(
        module: &Module,
        graph: Arc<ModuleCallGraph>,
        limit_bytes: usize,
    ) -> Result<Self, JitError> {
        let fixed_bytes = core::mem::size_of::<Self>().saturating_add(
            module
                .functions
                .len()
                .saturating_mul(core::mem::size_of::<Option<Arc<SmallFunctionInline>>>()),
        );
        if fixed_bytes > limit_bytes {
            return Err(JitError::AnalysisResourceLimitExceeded {
                limit_bytes,
                requested_bytes: fixed_bytes,
            });
        }
        let mut retained_bytes = fixed_bytes;
        let mut small_inlines = Vec::new();
        small_inlines
            .try_reserve_exact(module.functions.len())
            .map_err(|_| JitError::AnalysisResourceLimitExceeded {
                limit_bytes,
                requested_bytes: fixed_bytes,
            })?;
        for (func_id, function) in module.functions.iter().enumerate() {
            let inline = SmallFunctionInline::analyze_leaf(function, module)
                .or_else(|| {
                    SmallFunctionInline::analyze_self_recursive(
                        u32::try_from(func_id).ok()?,
                        function,
                        module,
                    )
                })
                .map(Arc::new);
            retained_bytes = retained_bytes.saturating_add(
                inline
                    .as_deref()
                    .map_or(0, SmallFunctionInline::retained_bytes),
            );
            if retained_bytes > limit_bytes {
                return Err(JitError::AnalysisResourceLimitExceeded {
                    limit_bytes,
                    requested_bytes: retained_bytes,
                });
            }
            small_inlines.push(inline);
        }
        Ok(Self {
            graph,
            small_inlines: small_inlines.into_boxed_slice(),
        })
    }

    pub(crate) fn retained_bytes(&self) -> usize {
        core::mem::size_of::<Self>()
            .saturating_add(
                self.small_inlines
                    .len()
                    .saturating_mul(core::mem::size_of::<Option<Arc<SmallFunctionInline>>>()),
            )
            .saturating_add(
                self.small_inlines
                    .iter()
                    .flatten()
                    .map(|inline| inline.retained_bytes())
                    .sum::<usize>(),
            )
    }

    pub(crate) fn pure_leaf_inline(
        &self,
        caller_id: u32,
        callee_id: u32,
    ) -> Option<&SmallFunctionInline> {
        let caller = caller_id as usize;
        let callee = callee_id as usize;
        if self.graph.is_recursive_edge(caller, callee) {
            return None;
        }
        let inline = self.small_inlines.get(callee)?.as_deref()?;
        (!inline.is_self_recursive(callee_id)).then_some(inline)
    }

    pub(crate) fn static_inline(
        &self,
        caller_id: u32,
        callee_id: u32,
    ) -> Option<&SmallFunctionInline> {
        if self
            .graph
            .is_recursive_edge(caller_id as usize, callee_id as usize)
        {
            if caller_id != callee_id {
                return None;
            }
            let inline = self.small_inlines.get(callee_id as usize)?.as_deref()?;
            return inline.is_self_recursive(callee_id).then_some(inline);
        }
        self.pure_leaf_inline(caller_id, callee_id)
    }

    pub(crate) fn is_recursive_edge(&self, caller_id: u32, callee_id: u32) -> bool {
        self.graph
            .is_recursive_edge(caller_id as usize, callee_id as usize)
    }

    fn direct_self_call(&self, caller_id: u32, callee_id: u32) -> bool {
        caller_id == callee_id
            && self
                .graph
                .is_recursive_edge(caller_id as usize, callee_id as usize)
    }
}

pub(crate) struct ModuleOptimizationPlan {
    inline_plan: Arc<ModuleInlinePlan>,
    function_param_slots: Box<[u16]>,
    returned_closure_targets: Box<[u32]>,
    concrete_iface_itabs: Box<[u32]>,
    interface_methods: Box<[Box<[u32]>]>,
}

impl ModuleOptimizationPlan {
    #[cfg(test)]
    pub(crate) fn build(module: &Module) -> Self {
        let graph = Arc::new(ModuleCallGraph::build(module));
        let inline_plan = Arc::new(
            ModuleInlinePlan::build_with_graph(module, graph, crate::MAX_JIT_ANALYSIS_BYTES)
                .expect("test inline plan must fit the standard analysis budget"),
        );
        Self::build_with_inline_plan(module, inline_plan, crate::MAX_JIT_ANALYSIS_BYTES)
            .expect("test optimization plan must fit the standard analysis budget")
    }

    pub(crate) fn build_with_inline_plan(
        module: &Module,
        inline_plan: Arc<ModuleInlinePlan>,
        limit_bytes: usize,
    ) -> Result<Self, JitError> {
        let fixed_bytes = core::mem::size_of::<Self>()
            .saturating_add(
                module
                    .functions
                    .len()
                    .saturating_mul(core::mem::size_of::<u16>() + core::mem::size_of::<u32>()),
            )
            .saturating_add(
                module
                    .constants
                    .len()
                    .saturating_mul(core::mem::size_of::<u32>()),
            )
            .saturating_add(
                module
                    .itabs
                    .len()
                    .saturating_mul(core::mem::size_of::<Box<[u32]>>()),
            )
            .saturating_add(
                module
                    .itabs
                    .iter()
                    .map(|itab| {
                        itab.methods
                            .len()
                            .saturating_mul(core::mem::size_of::<u32>())
                    })
                    .sum::<usize>(),
            );
        if fixed_bytes > limit_bytes {
            return Err(JitError::AnalysisResourceLimitExceeded {
                limit_bytes,
                requested_bytes: fixed_bytes,
            });
        }
        let function_param_slots = module
            .functions
            .iter()
            .map(|function| function.param_slots)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let returned_closure_targets = returned_closure_targets(module).into_boxed_slice();
        let concrete_iface_itabs = module
            .constants
            .iter()
            .map(|constant| match constant {
                Constant::Int(packed) => {
                    let itab = (*packed as u64 & u64::from(u32::MAX)) as u32;
                    if itab == 0 || itab == IFACE_ASSIGN_NO_ITAB {
                        NO_DYNAMIC_TARGET
                    } else {
                        itab
                    }
                }
                _ => NO_DYNAMIC_TARGET,
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let interface_methods = module
            .itabs
            .iter()
            .map(|itab| itab.methods.clone().into_boxed_slice())
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Ok(Self {
            inline_plan,
            function_param_slots,
            returned_closure_targets,
            concrete_iface_itabs,
            interface_methods,
        })
    }

    pub(crate) fn is_recursive_edge(&self, caller_id: u32, callee_id: u32) -> bool {
        self.inline_plan.is_recursive_edge(caller_id, callee_id)
    }

    pub(crate) fn retained_bytes(&self) -> usize {
        core::mem::size_of::<Self>()
            .saturating_add(self.function_param_slots.len() * core::mem::size_of::<u16>())
            .saturating_add(self.returned_closure_targets.len() * core::mem::size_of::<u32>())
            .saturating_add(self.concrete_iface_itabs.len() * core::mem::size_of::<u32>())
            .saturating_add(
                self.interface_methods.len() * core::mem::size_of::<Box<[u32]>>()
                    + self
                        .interface_methods
                        .iter()
                        .map(|methods| methods.len() * core::mem::size_of::<u32>())
                        .sum::<usize>(),
            )
    }

    pub(crate) fn pure_leaf_inline(
        &self,
        caller_id: u32,
        callee_id: u32,
    ) -> Option<&SmallFunctionInline> {
        self.inline_plan.pure_leaf_inline(caller_id, callee_id)
    }

    pub(crate) fn static_inline(
        &self,
        caller_id: u32,
        callee_id: u32,
    ) -> Option<&SmallFunctionInline> {
        self.inline_plan.static_inline(caller_id, callee_id)
    }

    pub(crate) fn direct_self_call(&self, caller_id: u32, callee_id: u32) -> bool {
        self.inline_plan.direct_self_call(caller_id, callee_id)
    }

    fn function(&self, func_id: u32) -> Option<FunctionPlanShape> {
        self.function_param_slots
            .get(func_id as usize)
            .copied()
            .map(|param_slots| FunctionPlanShape { param_slots })
    }

    fn returned_closure_target(&self, func_id: u32) -> Option<u32> {
        let target = *self.returned_closure_targets.get(func_id as usize)?;
        (target != NO_DYNAMIC_TARGET).then_some(target)
    }

    fn concrete_iface_assign_itab(
        &self,
        instruction: vo_runtime::instruction::Instruction,
    ) -> Option<u32> {
        if instruction.flags == 16 {
            return None;
        }
        let itab = *self.concrete_iface_itabs.get(instruction.c as usize)?;
        (itab != NO_DYNAMIC_TARGET).then_some(itab)
    }

    fn interface_method_target(&self, itab: u32, method: u32) -> Option<u32> {
        self.interface_methods
            .get(itab as usize)?
            .get(method as usize)
            .copied()
    }
}

#[derive(Clone, Copy)]
struct FunctionPlanShape {
    param_slots: u16,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixtures::{function_with_sig, function_with_slot_types_and_sig};
    use vo_runtime::bytecode::InstructionMetadata;
    use vo_runtime::instruction::{Instruction, Opcode};
    use vo_runtime::SlotType;

    fn branch(opcode: Opcode, condition: u16, offset: i32) -> Instruction {
        Instruction::with_flags(
            opcode,
            0,
            condition,
            offset as u32 as u16,
            (offset as u32 >> 16) as u16,
        )
    }

    #[test]
    fn plan_selects_bounded_self_recursion_separately_from_leaf_inlining() {
        let mut module = Module::new("optimizer-plan".into());
        module.functions = vec![
            function_with_sig(
                vec![
                    Instruction::new(Opcode::Call, 0, 0, 0),
                    Instruction::new(Opcode::Return, 0, 0, 0),
                ],
                0,
                0,
                1,
                0,
            ),
            function_with_sig(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 0, 0, 1, 0),
        ];

        let plan = ModuleOptimizationPlan::build(&module);
        assert!(plan.direct_self_call(0, 0));
        assert!(plan.pure_leaf_inline(0, 0).is_none());
        assert!(plan.static_inline(0, 0).is_some());
        assert!(plan.pure_leaf_inline(0, 1).is_some());

        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let baseline = OptimizedFunction::baseline_with_module(&ir, &plan.inline_plan, 0);
        let optimized = OptimizedFunction::analyze_with_module(&ir, &module.functions[0], &plan, 0);
        assert_eq!(baseline.inline_target(0), None);
        assert_eq!(optimized.inline_target(0), Some(0));
    }

    #[test]
    fn baseline_graph_owns_shared_inline_selection_and_budget_cost() {
        let mut module = Module::new("baseline-inline-graph".into());
        module.functions = vec![
            function_with_sig(
                vec![
                    Instruction::new(Opcode::Call, 1, 0, 0),
                    Instruction::new(Opcode::Return, 0, 0, 0),
                ],
                0,
                0,
                0,
                0,
            ),
            function_with_sig(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 0, 0, 0, 0),
        ];
        let graph = Arc::new(ModuleCallGraph::build(&module));
        let inline_plan =
            ModuleInlinePlan::build_with_graph(&module, graph, crate::MAX_JIT_ANALYSIS_BYTES)
                .expect("baseline inline plan");
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let baseline = OptimizedFunction::baseline_with_module(&ir, &inline_plan, 0);

        assert_eq!(baseline.inline_target(0), Some(1));
        assert!(baseline.inline_expansion_cost(0) > 0);
    }

    #[test]
    fn callable_ssa_traces_a_factory_result_into_a_closure_inline() {
        let mut caller = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::Call, 1, 0, 0),
                Instruction::new(Opcode::CallClosure, 0, 1, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
            ],
            vec![SlotType::GcBase, SlotType::Value],
            0,
            0,
            1,
        );
        caller.ret_slot_types = vec![SlotType::Value];
        caller.instruction_metadata[1] = InstructionMetadata::CallLayout {
            arg_layout: vec![],
            ret_layout: vec![SlotType::Value],
        };

        let mut factory = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::ClosureNew, 0, 2, 0),
                Instruction::new(Opcode::Return, 0, 1, 0),
            ],
            vec![SlotType::GcBase],
            0,
            0,
            1,
        );
        factory.ret_slot_types = vec![SlotType::GcBase];

        let mut closure = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 1, 7, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
            ],
            vec![SlotType::GcBase, SlotType::Value],
            1,
            1,
            1,
        );
        closure.is_closure = true;
        closure.ret_slot_types = vec![SlotType::Value];

        let mut module = Module::new("callable-ssa-factory".into());
        module.functions = vec![caller, factory, closure];
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let module_plan = ModuleOptimizationPlan::build(&module);
        let optimized =
            OptimizedFunction::analyze_with_module(&ir, &module.functions[0], &module_plan, 0);
        assert_eq!(module_plan.returned_closure_target(1), Some(2));
        assert_eq!(optimized.inline_target(1), Some(2));
    }

    #[test]
    fn closure_factory_summary_rejects_control_flow() {
        let mut factory = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::ClosureNew, 0, 1, 0),
                branch(Opcode::JumpIf, 1, 2),
                Instruction::new(Opcode::Return, 0, 1, 0),
                Instruction::new(Opcode::Return, 0, 1, 0),
            ],
            vec![SlotType::GcBase, SlotType::Value],
            0,
            0,
            1,
        );
        factory.ret_slot_types = vec![SlotType::GcBase];
        let target = function_with_slot_types_and_sig(
            vec![Instruction::new(Opcode::Return, 0, 0, 0)],
            vec![SlotType::GcBase],
            1,
            1,
            0,
        );
        let mut module = Module::new("closure-summary-control-flow".into());
        module.functions = vec![factory, target];
        assert_eq!(returned_closure_targets(&module)[0], NO_DYNAMIC_TARGET);
    }

    #[test]
    fn semantic_dce_removes_an_unused_pure_chain() {
        let mut module = Module::new("optimizer-dce".into());
        module.functions.push(function_with_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 0, 1, 0),
                Instruction::new(Opcode::LoadInt, 1, 2, 0),
                Instruction::new(Opcode::AddI, 2, 0, 1),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            0,
            0,
            3,
            0,
        ));
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);
        assert_eq!(plan.eliminated_count(), 3);
        assert!((0..3).all(|pc| plan.eliminates(pc)));
        assert!(!plan.eliminates(3));
    }

    #[test]
    fn semantic_dce_keeps_a_value_reachable_from_return() {
        let mut module = Module::new("optimizer-live-return".into());
        module.functions.push(function_with_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 0, 1, 0),
                Instruction::new(Opcode::LoadInt, 1, 2, 0),
                Instruction::new(Opcode::AddI, 2, 0, 1),
                Instruction::new(Opcode::Return, 2, 1, 0),
            ],
            0,
            1,
            3,
            1,
        ));
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);
        assert_eq!(plan.eliminated_count(), 0);
    }

    #[test]
    fn sccp_prunes_never_taken_branch_and_dead_arm() {
        let mut module = Module::new("optimizer-sccp-never".into());
        module.functions.push(function_with_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 0, 0, 0),
                branch(Opcode::JumpIf, 0, 3),
                Instruction::new(Opcode::LoadInt, 1, 7, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
                Instruction::new(Opcode::LoadInt, 1, 9, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
            ],
            0,
            1,
            2,
            1,
        ));
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert!(plan.eliminates(0));
        assert!(plan.eliminates(1));
        assert!(plan.eliminates(4));
        assert!(plan.eliminates(5));
        assert!(!plan.always_takes_branch(1));
    }

    #[test]
    fn sccp_turns_constant_true_branch_into_direct_jump() {
        let mut module = Module::new("optimizer-sccp-always".into());
        module.functions.push(function_with_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 0, 1, 0),
                branch(Opcode::JumpIf, 0, 3),
                Instruction::new(Opcode::LoadInt, 1, 7, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
                Instruction::new(Opcode::LoadInt, 1, 9, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
            ],
            0,
            1,
            2,
            1,
        ));
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert!(plan.eliminates(0));
        assert!(plan.always_takes_branch(1));
        assert!(plan.eliminates(2));
        assert!(plan.eliminates(3));
        assert!(!plan.eliminates(4));
    }

    #[test]
    fn local_gvn_reuses_identical_integer_expression() {
        let mut module = Module::new("optimizer-local-gvn".into());
        module.functions.push(function_with_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 0, 4, 0),
                Instruction::new(Opcode::LoadInt, 1, 5, 0),
                Instruction::new(Opcode::AddI, 2, 0, 1),
                Instruction::new(Opcode::AddI, 3, 0, 1),
                Instruction::new(Opcode::AddI, 4, 2, 3),
                Instruction::new(Opcode::Return, 4, 1, 0),
            ],
            0,
            1,
            5,
            1,
        ));
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        let first_sum = ir.output_value(2, 2).expect("first sum SSA value");
        assert_eq!(plan.replacement_value(3), Some(first_sum));
        assert!(!plan.eliminates(3));
    }

    #[test]
    fn local_gvn_reuses_identical_float_expression_without_reassociation() {
        let mut module = Module::new("optimizer-float-gvn".into());
        module.functions.push(function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::AddF, 2, 0, 1),
                Instruction::new(Opcode::AddF, 3, 0, 1),
                Instruction::new(Opcode::SubF, 4, 2, 3),
                Instruction::new(Opcode::Return, 4, 1, 0),
            ],
            vec![
                SlotType::Float,
                SlotType::Float,
                SlotType::Float,
                SlotType::Float,
                SlotType::Float,
            ],
            2,
            2,
            1,
        ));
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert_eq!(plan.replacement_value(1), ir.output_value(0, 2));
    }

    #[test]
    fn dominator_gvn_reuses_expression_across_a_control_flow_join() {
        let mut module = Module::new("optimizer-dominator-gvn".into());
        module.functions.push(function_with_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 1, 4, 0),
                Instruction::new(Opcode::LoadInt, 2, 5, 0),
                Instruction::new(Opcode::AddI, 3, 1, 2),
                branch(Opcode::JumpIf, 0, 2),
                Instruction::new(Opcode::Hint, 0, 0, 0),
                Instruction::new(Opcode::AddI, 4, 1, 2),
                Instruction::new(Opcode::Return, 4, 1, 0),
            ],
            1,
            1,
            5,
            1,
        ));
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        let dominating_sum = ir.output_value(2, 3).expect("dominating sum SSA value");
        assert_eq!(plan.replacement_value(5), Some(dominating_sum));
    }

    #[test]
    fn dominator_bce_eliminates_a_repeated_check_of_congruent_values() {
        let mut module = Module::new("optimizer-dominator-bce".into());
        module.functions.push(function_with_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 0, 0, 0),
                Instruction::new(Opcode::LoadInt, 1, 2, 0),
                Instruction::new(Opcode::IndexCheck, 0, 1, 0),
                Instruction::new(Opcode::Copy, 2, 0, 0),
                Instruction::new(Opcode::IndexCheck, 2, 1, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            0,
            0,
            3,
            0,
        ));
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert!(!plan.eliminates(2));
        assert!(plan.eliminates(4));
    }

    #[test]
    fn dominator_bce_elides_the_implicit_check_on_a_repeated_slice_access() {
        let mut module = Module::new("optimizer-slice-bce".into());
        let mut function = function_with_slot_types_and_sig(
            vec![
                Instruction::with_flags(Opcode::SliceGet, 0, 2, 0, 1),
                Instruction::with_flags(Opcode::SliceGet, 0, 3, 0, 1),
                Instruction::new(Opcode::Return, 3, 1, 0),
            ],
            vec![
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
            2,
            2,
            1,
        );
        let element = InstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        };
        function.instruction_metadata[0] = element.clone();
        function.instruction_metadata[1] = element;
        module.functions.push(function);

        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert!(!plan.elides_bounds_check(0));
        assert!(plan.elides_bounds_check(1));
        assert!(!plan.eliminates(1));
    }

    #[test]
    fn gvn_reuses_a_checked_slice_element_address() {
        let mut module = Module::new("optimizer-slice-address-gvn".into());
        let mut function = function_with_slot_types_and_sig(
            vec![
                Instruction::with_flags(Opcode::SliceAddr, 0, 2, 0, 1),
                Instruction::with_flags(Opcode::SliceAddr, 0, 3, 0, 1),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            vec![
                SlotType::GcRef,
                SlotType::Value,
                SlotType::GcRef,
                SlotType::GcRef,
            ],
            2,
            2,
            0,
        );
        let element = InstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        };
        function.instruction_metadata[0] = element.clone();
        function.instruction_metadata[1] = element;
        module.functions.push(function);

        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert!(plan.elides_bounds_check(1));
        assert_eq!(plan.replacement_value(1), ir.output_value(0, 2));
    }

    #[test]
    fn non_nil_fact_from_a_dominating_dereference_crosses_basic_blocks() {
        let mut module = Module::new("optimizer-dominating-non-nil".into());
        let mut function = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::PtrGet, 2, 0, 0),
                branch(Opcode::JumpIf, 1, 2),
                Instruction::new(Opcode::Hint, 0, 0, 0),
                Instruction::new(Opcode::PtrGet, 3, 0, 0),
                Instruction::new(Opcode::Return, 3, 1, 0),
            ],
            vec![
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
            2,
            2,
            1,
        );
        for pc in [0, 3] {
            function.instruction_metadata[pc] = InstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::Value],
            };
        }
        module.functions.push(function);
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert!(!plan.elides_nil_check(0));
        assert!(plan.elides_nil_check(3));
    }

    #[test]
    fn non_nil_fact_does_not_escape_one_arm_of_a_branch() {
        let mut module = Module::new("optimizer-branch-local-non-nil".into());
        let mut function = function_with_slot_types_and_sig(
            vec![
                branch(Opcode::JumpIf, 1, 3),
                Instruction::new(Opcode::PtrGet, 2, 0, 0),
                branch(Opcode::Jump, 0, 2),
                Instruction::new(Opcode::Hint, 0, 0, 0),
                Instruction::new(Opcode::PtrGet, 3, 0, 0),
                Instruction::new(Opcode::Return, 3, 1, 0),
            ],
            vec![
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
            2,
            2,
            1,
        );
        for pc in [1, 4] {
            function.instruction_metadata[pc] = InstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::Value],
            };
        }
        module.functions.push(function);
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert!(!plan.elides_nil_check(4));
    }

    fn pointer_field_function(
        name: &str,
        code: Vec<Instruction>,
        slot_types: Vec<SlotType>,
        param_slots: u16,
    ) -> Module {
        let mut function =
            function_with_slot_types_and_sig(code, slot_types, param_slots, param_slots, 1);
        for (pc, instruction) in function.code.iter().enumerate() {
            if matches!(
                instruction.opcode(),
                Opcode::PtrGet | Opcode::PtrSet | Opcode::PtrGetN | Opcode::PtrSetN
            ) {
                function.instruction_metadata[pc] = InstructionMetadata::PtrLayout {
                    value_layout: vec![SlotType::Value],
                };
            }
        }
        let mut module = Module::new(name.into());
        module.functions.push(function);
        module
    }

    #[test]
    fn field_memory_gvn_reuses_a_dominating_pointer_load() {
        let module = pointer_field_function(
            "optimizer-field-load-gvn",
            vec![
                Instruction::new(Opcode::PtrGet, 1, 0, 0),
                Instruction::new(Opcode::PtrGet, 2, 0, 0),
                Instruction::new(Opcode::AddI, 3, 1, 2),
                Instruction::new(Opcode::Return, 3, 1, 0),
            ],
            vec![
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
            1,
        );
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert_eq!(plan.replacement_value(1), ir.output_value(0, 1));
        assert!(plan.elides_nil_check(1));
    }

    #[test]
    fn field_memory_gvn_keeps_other_offsets_of_the_same_pointer() {
        let module = pointer_field_function(
            "optimizer-field-store-gvn",
            vec![
                Instruction::new(Opcode::PtrGet, 1, 0, 0),
                Instruction::new(Opcode::LoadInt, 2, 9, 0),
                Instruction::new(Opcode::PtrSet, 0, 1, 2),
                Instruction::new(Opcode::PtrGet, 3, 0, 0),
                Instruction::new(Opcode::Return, 3, 1, 0),
            ],
            vec![
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
            1,
        );
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert_eq!(plan.replacement_value(3), ir.output_value(0, 1));
    }

    #[test]
    fn field_memory_gvn_invalidates_loads_for_a_may_alias_pointer() {
        let module = pointer_field_function(
            "optimizer-field-alias-gvn",
            vec![
                Instruction::new(Opcode::PtrGet, 2, 0, 0),
                Instruction::new(Opcode::LoadInt, 3, 9, 0),
                Instruction::new(Opcode::PtrSet, 1, 1, 3),
                Instruction::new(Opcode::PtrGet, 4, 0, 0),
                Instruction::new(Opcode::Return, 4, 1, 0),
            ],
            vec![
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
            2,
        );
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert_eq!(plan.replacement_value(3), None);
    }

    #[test]
    fn field_memory_gvn_forwards_an_exact_pointer_store() {
        let module = pointer_field_function(
            "optimizer-field-store-forwarding",
            vec![
                Instruction::new(Opcode::LoadInt, 1, 7, 0),
                Instruction::new(Opcode::PtrSet, 0, 0, 1),
                Instruction::new(Opcode::PtrGet, 2, 0, 0),
                Instruction::new(Opcode::Return, 2, 1, 0),
            ],
            vec![SlotType::GcRef, SlotType::Value, SlotType::Value],
            1,
        );
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert_eq!(plan.replacement_value(2), ir.output_value(0, 1));
    }

    fn canonical_slice_loop(initial_index: i16) -> Module {
        let mut module = Module::new("optimizer-slice-range-bce".into());
        let initial_bits = i32::from(initial_index) as u32;
        let mut function = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::SliceLen, 2, 0, 0),
                Instruction::new(
                    Opcode::LoadInt,
                    3,
                    initial_bits as u16,
                    (initial_bits >> 16) as u16,
                ),
                Instruction::new(Opcode::LtI, 4, 3, 2),
                branch(Opcode::JumpIfNot, 4, 4),
                Instruction::with_flags(Opcode::SliceGet, 0, 5, 0, 3),
                Instruction::new(Opcode::Hint, 0, 0, 0),
                Instruction::new(Opcode::ForLoop, 3, 2, (-3_i16) as u16),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            vec![
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
            1,
            1,
            0,
        );
        function.instruction_metadata[4] = InstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        };
        module.functions.push(function);
        module
    }

    #[test]
    fn range_bce_elides_a_canonical_nonnegative_slice_loop_check() {
        let module = canonical_slice_loop(0);
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert!(plan.elides_bounds_check(4));
    }

    #[test]
    fn range_bce_keeps_a_signed_loop_check_with_a_negative_initial_index() {
        let module = canonical_slice_loop(-1);
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert!(!plan.elides_bounds_check(4));
    }

    fn natural_slice_loop(initial_index: i16) -> Module {
        let mut module = Module::new("optimizer-natural-slice-range-bce".into());
        let initial_bits = i32::from(initial_index) as u32;
        let mut function = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::SliceLen, 2, 0, 0),
                Instruction::new(
                    Opcode::LoadInt,
                    3,
                    initial_bits as u16,
                    (initial_bits >> 16) as u16,
                ),
                Instruction::new(Opcode::LoadInt, 6, 1, 0),
                Instruction::new(Opcode::LtI, 4, 3, 2),
                branch(Opcode::JumpIfNot, 4, 4),
                Instruction::with_flags(Opcode::SliceGet, 0, 5, 0, 3),
                Instruction::new(Opcode::AddI, 3, 3, 6),
                branch(Opcode::Jump, 0, -4),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            vec![
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
            1,
            1,
            0,
        );
        function.instruction_metadata[5] = InstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        };
        module.functions.push(function);
        module
    }

    #[test]
    fn range_bce_elides_a_natural_compare_branch_loop_check() {
        let module = natural_slice_loop(0);
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert!(plan.elides_bounds_check(5));
    }

    #[test]
    fn range_bce_keeps_a_negative_natural_loop_index_check() {
        let module = natural_slice_loop(-1);
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze(&ir);

        assert!(!plan.elides_bounds_check(5));
    }

    #[test]
    fn osr_dce_roots_only_the_final_definition_of_each_exit_slot() {
        let mut module = Module::new("optimizer-osr-exit-state".into());
        module.functions.push(function_with_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 0, 1, 0),
                Instruction::new(Opcode::LoadInt, 0, 2, 0),
            ],
            0,
            0,
            1,
            0,
        ));
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze_osr(&ir, 0..2);

        assert!(plan.eliminates(0));
        assert!(!plan.eliminates(1));
    }

    #[test]
    fn dce_removes_the_dependency_it_counted_for_a_gvn_replacement() {
        let mut module = Module::new("optimizer-gvn-dce-accounting".into());
        module.functions.push(function_with_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 0, 0, 0),
                Instruction::new(Opcode::LoadInt, 1, 2, 0),
                Instruction::new(Opcode::LtI, 2, 0, 1),
                Instruction::new(Opcode::LoadInt, 3, 0, 0),
                Instruction::new(Opcode::LoadInt, 4, 2, 0),
                Instruction::new(Opcode::LtI, 5, 3, 4),
                Instruction::new(Opcode::LoadInt, 5, 9, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            0,
            0,
            6,
            0,
        ));
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze_osr(&ir, 0..8);

        assert!(!plan.eliminates(3));
        assert!(!plan.eliminates(4));
        assert!(plan.eliminates(5));
    }

    #[test]
    fn osr_gvn_starts_a_fresh_value_numbering_scope_at_region_entry() {
        let mut module = Module::new("optimizer-osr-gvn-entry".into());
        module.functions.push(function_with_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 0, 4, 0),
                Instruction::new(Opcode::LoadInt, 1, 5, 0),
                Instruction::new(Opcode::AddI, 2, 0, 1),
                Instruction::new(Opcode::AddI, 3, 0, 1),
            ],
            0,
            0,
            4,
            0,
        ));
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let plan = OptimizedFunction::analyze_osr(&ir, 3..4);

        assert_eq!(plan.replacement_value(3), None);
        assert!(!plan.eliminates(3));
    }

    #[test]
    fn osr_rebases_sccp_when_the_entry_imports_a_full_function_constant() {
        let mut module = Module::new("optimizer-osr-sccp-entry".into());
        module.functions.push(function_with_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 0, 0, 0),
                branch(Opcode::JumpIf, 0, 2),
                Instruction::new(Opcode::Return, 0, 0, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            0,
            0,
            1,
            0,
        ));
        let ir = crate::ir::FunctionIr::build(&module.functions[0], &module).unwrap();
        let full = OptimizedFunction::analyze(&ir);
        assert!(full.eliminates(1));
        assert!(!full.is_executable(3));

        let osr = full.project_osr(&ir, 1..4);
        assert!(!osr.eliminates(1));
        assert!(osr.is_executable(2));
        assert!(osr.is_executable(3));
    }
}
