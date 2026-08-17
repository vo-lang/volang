//! Typed, effect-aware SSA shared by every JIT compilation tier.
//!
//! The IR uses basic-block parameters as phi nodes. This keeps construction
//! independent of dominance order, represents loop-carried values directly,
//! and gives full-function compilation and loop OSR one control-flow model.
//! Sparse frame states retain every live typed value needed for deoptimization
//! plus a compact root projection consumed by the GC safepoint path.

use std::collections::{BTreeMap, BTreeSet, VecDeque};

use vo_runtime::bytecode::{Constant, FunctionDef, Module};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

use crate::contract::EffectContract;
use crate::effects::{self, EffectFacts, MemorySyncEffect};
use crate::{JitError, MAX_JIT_COMPILE_WORK_BYTES};

const NONE_ID: u32 = u32::MAX;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct BlockId(u32);

impl BlockId {
    #[inline]
    pub(crate) fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct ValueId(u32);

impl ValueId {
    #[inline]
    pub(crate) fn from_index(index: usize) -> Self {
        Self(index as u32)
    }

    #[inline]
    pub(crate) fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct FrameStateId(u32);

impl FrameStateId {
    #[inline]
    fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RootProvenance {
    /// No reachable predecessor has supplied this block parameter yet.
    Unreachable,
    ExactBase,
    Interior,
    /// The value may be either an object base or an interior pointer.
    Unknown,
}

impl RootProvenance {
    fn join(self, other: Self) -> Self {
        use RootProvenance::*;
        match (self, other) {
            (Unreachable, value) | (value, Unreachable) => value,
            (ExactBase, ExactBase) => ExactBase,
            (Interior, Interior) => Interior,
            _ => Unknown,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ValueType {
    Word,
    Float64,
    GcRef(RootProvenance),
    InterfaceHeader,
    InterfaceData,
}

impl ValueType {
    fn for_slot(slot_type: SlotType, provenance: RootProvenance) -> Self {
        match slot_type {
            SlotType::Value => Self::Word,
            SlotType::Float => Self::Float64,
            SlotType::GcBase => Self::GcRef(RootProvenance::ExactBase),
            SlotType::GcRef => Self::GcRef(provenance),
            SlotType::Interface0 => Self::InterfaceHeader,
            SlotType::Interface1 => Self::InterfaceData,
        }
    }

    fn root_provenance(self) -> Option<RootProvenance> {
        match self {
            Self::GcRef(provenance) => Some(provenance),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValueOrigin {
    EntrySlot,
    BlockParameter,
    Alias(ValueId),
    Instruction,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SsaValue {
    pub ty: ValueType,
    pub slot: u16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ValueUse {
    pub slot: u16,
    pub value: ValueId,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct Span {
    start: u32,
    len: u32,
}

impl Span {
    fn append<T>(storage: &mut Vec<T>, values: impl IntoIterator<Item = T>) -> Self {
        let start = storage.len();
        storage.extend(values);
        Self {
            start: start.try_into().unwrap_or(NONE_ID),
            len: (storage.len() - start).try_into().unwrap_or(NONE_ID),
        }
    }

    fn slice<T>(self, storage: &[T]) -> &[T] {
        let start = self.start as usize;
        &storage[start..start + self.len as usize]
    }
}

/// Compact effect lattice carried by every instruction.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct EffectSet(u16);

impl EffectSet {
    const MAY_GC: u16 = 1 << 0;
    const MAY_ALLOC: u16 = 1 << 1;
    const MAY_PANIC: u16 = 1 << 2;
    const MAY_UNWIND: u16 = 1 << 3;
    const MAY_CALL: u16 = 1 << 4;
    const MAY_SCHEDULE: u16 = 1 << 5;
    const MAY_OBSERVE_FRAME: u16 = 1 << 6;
    const NEEDS_FRAME: u16 = 1 << 7;
    const NEEDS_SLOT_METADATA: u16 = 1 << 8;
    const NEEDS_TYPE_METADATA: u16 = 1 << 9;
    const NEEDS_WRITE_BARRIER: u16 = 1 << 10;
    const TOUCHES_INTERFACE: u16 = 1 << 11;
    const MATERIALIZES_CLOSURE: u16 = 1 << 12;
    const WRITES_OBSERVABLE_STATE: u16 = 1 << 13;

    fn from_contract(opcode: Opcode, contract: EffectContract) -> Self {
        let mut bits = 0;
        bits |= u16::from(contract.may_gc) * Self::MAY_GC;
        bits |= u16::from(contract.may_alloc) * Self::MAY_ALLOC;
        bits |= u16::from(contract.may_panic) * Self::MAY_PANIC;
        bits |= u16::from(contract.may_unwind) * Self::MAY_UNWIND;
        bits |= u16::from(contract.may_call) * Self::MAY_CALL;
        bits |= u16::from(contract.may_schedule) * Self::MAY_SCHEDULE;
        bits |= u16::from(contract.may_observe_frame) * Self::MAY_OBSERVE_FRAME;
        bits |= u16::from(contract.needs_frame) * Self::NEEDS_FRAME;
        bits |= u16::from(contract.needs_slot_metadata) * Self::NEEDS_SLOT_METADATA;
        bits |= u16::from(contract.needs_type_metadata) * Self::NEEDS_TYPE_METADATA;
        bits |= u16::from(contract.needs_write_barrier) * Self::NEEDS_WRITE_BARRIER;
        bits |= u16::from(contract.touches_interface) * Self::TOUCHES_INTERFACE;
        bits |= u16::from(contract.materializes_closure) * Self::MATERIALIZES_CLOSURE;
        bits |= u16::from(matches!(opcode, Opcode::GlobalSet | Opcode::GlobalSetN))
            * Self::WRITES_OBSERVABLE_STATE;
        Self(bits)
    }

    #[inline]
    pub(crate) fn requires_frame_state(self) -> bool {
        self.0
            & (Self::MAY_GC
                | Self::MAY_ALLOC
                | Self::MAY_PANIC
                | Self::MAY_UNWIND
                | Self::MAY_CALL
                | Self::MAY_SCHEDULE
                | Self::MAY_OBSERVE_FRAME
                | Self::NEEDS_FRAME)
            != 0
    }

    #[inline]
    pub(crate) fn can_eliminate(self) -> bool {
        self.0 == 0
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct TypedInstruction {
    source: Instruction,
    block: BlockId,
    values: Span,
    input_count: u16,
    effects: EffectSet,
    memory_sync: MemorySyncEffect,
    frame_state: u32,
}

impl TypedInstruction {
    #[inline]
    pub(crate) fn source(self) -> Instruction {
        self.source
    }

    #[inline]
    pub(crate) fn block(self) -> BlockId {
        self.block
    }

    #[inline]
    pub(crate) fn effects(self) -> EffectSet {
        self.effects
    }

    #[inline]
    pub(crate) fn memory_sync(self) -> MemorySyncEffect {
        self.memory_sync
    }

    #[inline]
    pub(crate) fn requires_frame_state(self) -> bool {
        self.effects.requires_frame_state()
    }

    pub(crate) fn frame_state_id(self) -> Option<FrameStateId> {
        (self.frame_state != NONE_ID).then_some(FrameStateId(self.frame_state))
    }
}

#[derive(Debug, Clone, Copy)]
struct FrameLiveness {
    live_slots: Span,
    direct_roots: Span,
    conditional_roots: Span,
}

pub(crate) type FrameValue = ValueUse;

#[derive(Debug, Clone, Copy)]
pub(crate) struct FrameState {
    /// Bytecode PC at which execution can be reconstructed.
    pub resume_pc: u32,
    values: Span,
    direct_roots: Span,
    conditional_roots: Span,
    /// Inlined frame states form a parent chain through this field.
    parent: u32,
}

impl FrameState {
    pub(crate) fn parent(self) -> Option<FrameStateId> {
        (self.parent != NONE_ID).then_some(FrameStateId(self.parent))
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct BlockEdge {
    pub target: BlockId,
    arguments: Span,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct BasicBlock {
    pub id: BlockId,
    pub start_pc: u32,
    pub end_pc: u32,
    parameters: Span,
    predecessors: Span,
    successors: Span,
    pub reachable: bool,
}

#[derive(Debug)]
pub(crate) struct FunctionIr {
    instructions: Box<[TypedInstruction]>,
    blocks: Box<[BasicBlock]>,
    values: Box<[SsaValue]>,
    instruction_values: Box<[ValueId]>,
    frame_states: Box<[FrameState]>,
    frame_values: Box<[FrameValue]>,
    root_slots: Box<[u16]>,
    block_parameters: Box<[ValueUse]>,
    predecessors: Box<[BlockId]>,
    edges: Box<[BlockEdge]>,
    edge_arguments: Box<[ValueUse]>,
    constant_values: Box<[i64]>,
    constant_known: Box<[u64]>,
    executable_blocks: Box<[u64]>,
    executable_edges: Box<[u64]>,
    call_iface_method_indices: Box<[u32]>,
    retained_bytes: usize,
}

struct ConstantPropagation {
    values: Vec<ConstantLattice>,
    executable_blocks: Vec<bool>,
    executable_edges: Vec<bool>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConstantLattice {
    Unknown,
    Known(i64),
    Overdefined,
}

impl ConstantLattice {
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

fn bitset_from_bools(values: &[bool]) -> Vec<u64> {
    let mut words = vec![0_u64; values.len().div_ceil(64)];
    for (index, &value) in values.iter().enumerate() {
        if value {
            words[index / 64] |= 1_u64 << (index % 64);
        }
    }
    words
}

fn bitset_contains(words: &[u64], index: usize) -> bool {
    words
        .get(index / 64)
        .is_some_and(|word| word & (1_u64 << (index % 64)) != 0)
}

#[derive(Debug)]
struct RawInstruction {
    source: Instruction,
    reads: Vec<u16>,
    writes: Vec<u16>,
    effects: EffectSet,
    memory_sync: MemorySyncEffect,
}

#[derive(Debug, Default)]
struct BlockFacts {
    start: usize,
    end: usize,
    predecessors: Vec<BlockId>,
    successors: Vec<BlockId>,
    uses: BTreeSet<u16>,
    defs: BTreeSet<u16>,
    live_in: BTreeSet<u16>,
    live_out: BTreeSet<u16>,
    reachable: bool,
}

impl FunctionIr {
    #[cfg(test)]
    pub(crate) fn build(func: &FunctionDef, module: &Module) -> Result<Self, JitError> {
        Self::build_with_limit(func, module, crate::MAX_JIT_ANALYSIS_BYTES)
    }

    pub(crate) fn build_with_limit(
        func: &FunctionDef,
        module: &Module,
        retained_limit_bytes: usize,
    ) -> Result<Self, JitError> {
        Self::build_with_limit_and_return_summaries(func, module, &[], retained_limit_bytes)
    }

    pub(crate) fn build_with_limit_and_return_summaries(
        func: &FunctionDef,
        module: &Module,
        exact_base_returns: &[Box<[bool]>],
        retained_limit_bytes: usize,
    ) -> Result<Self, JitError> {
        if func.code.is_empty() {
            return Ok(Self::empty());
        }

        let mut raw = Vec::with_capacity(func.code.len());
        for (pc, source) in func.code.iter().copied().enumerate() {
            let facts = EffectFacts::from_instruction(func.instruction_metadata.get(pc));
            let instruction_effects = effects::try_instruction_effects_with_module_context(
                &source,
                facts,
                &module.externs,
                &module.functions,
            )
            .map_err(|error| {
                JitError::Internal(format!(
                    "verified SSA effects failed for {} at pc {pc}: {error:?}",
                    func.name
                ))
            })?;
            validate_slots(func, pc, &instruction_effects.reads, "read")?;
            validate_slots(func, pc, &instruction_effects.writes, "write")?;
            raw.push(RawInstruction {
                source,
                reads: instruction_effects.reads,
                writes: instruction_effects.writes,
                effects: EffectSet::from_contract(
                    source.opcode(),
                    crate::contract::opcode_contract(source.opcode()),
                ),
                memory_sync: instruction_effects.memory_sync,
            });
        }

        let (mut blocks, pc_to_block) = build_cfg(&raw)?;
        compute_block_liveness(&mut blocks, &raw)?;
        let (liveness_at_frame_state, live_slots, root_slots) =
            compute_sparse_frame_liveness(&blocks, &raw, &func.slot_types, retained_limit_bytes)?;

        let mut values = Vec::new();
        let mut value_origins = Vec::new();
        let mut block_parameters = Vec::new();
        let mut parameter_maps = Vec::with_capacity(blocks.len());
        for (block_index, block) in blocks.iter().enumerate() {
            let block_id = BlockId(block_index as u32);
            let mut parameters = BTreeMap::new();
            for &slot in &block.live_in {
                let provenance = if block_index == 0 {
                    RootProvenance::Unknown
                } else {
                    RootProvenance::Unreachable
                };
                let origin = if block_index == 0 {
                    ValueOrigin::EntrySlot
                } else {
                    ValueOrigin::BlockParameter
                };
                let value = push_value(
                    &mut values,
                    &mut value_origins,
                    func,
                    slot,
                    provenance,
                    origin,
                )?;
                parameters.insert(slot, value);
                block_parameters.push(ValueUse { slot, value });
            }
            parameter_maps.push(parameters);
            let _ = block_id;
        }

        let mut instruction_values = Vec::new();
        let mut typed = Vec::with_capacity(raw.len());
        let mut frame_states = Vec::new();
        let mut frame_values = Vec::new();
        let mut edges = Vec::new();
        let mut edge_arguments = Vec::new();

        for (block_index, block) in blocks.iter().enumerate() {
            let block_id = BlockId(block_index as u32);
            let mut current = parameter_maps[block_index].clone();
            for pc in block.start..block.end {
                let instruction = &raw[pc];
                let value_start = instruction_values.len();
                for &slot in &instruction.reads {
                    let value = current.get(&slot).copied().ok_or_else(|| {
                        JitError::Internal(format!(
                            "SSA value for {} slot {slot} is absent at pc {pc}",
                            func.name
                        ))
                    })?;
                    instruction_values.push(value);
                }
                let input_count = (instruction_values.len() - value_start) as u16;

                let frame_state = if let Some(liveness) = liveness_at_frame_state[pc] {
                    let values = Span::append(
                        &mut frame_values,
                        liveness.live_slots.slice(&live_slots).iter().map(|&slot| {
                            let value = current.get(&slot).copied().expect(
                                "live-before slots must have an SSA value at instruction entry",
                            );
                            ValueUse { slot, value }
                        }),
                    );
                    let id = frame_states.len() as u32;
                    frame_states.push(FrameState {
                        resume_pc: pc as u32,
                        values,
                        direct_roots: liveness.direct_roots,
                        conditional_roots: liveness.conditional_roots,
                        parent: NONE_ID,
                    });
                    id
                } else {
                    NONE_ID
                };

                for &slot in &instruction.writes {
                    let alias = {
                        let input_slice = &instruction_values
                            [value_start..value_start + usize::from(input_count)];
                        alias_source(instruction.source, slot, input_slice, &values)
                    };
                    let provenance = fixed_output_provenance(
                        instruction.source,
                        slot,
                        alias,
                        module,
                        exact_base_returns,
                    );
                    let origin = alias
                        .map(ValueOrigin::Alias)
                        .unwrap_or(ValueOrigin::Instruction);
                    let value = push_value(
                        &mut values,
                        &mut value_origins,
                        func,
                        slot,
                        provenance,
                        origin,
                    )?;
                    current.insert(slot, value);
                    instruction_values.push(value);
                }
                let instruction_value_span = Span {
                    start: value_start as u32,
                    len: (instruction_values.len() - value_start) as u32,
                };
                typed.push(TypedInstruction {
                    source: instruction.source,
                    block: block_id,
                    values: instruction_value_span,
                    input_count,
                    effects: instruction.effects,
                    memory_sync: instruction.memory_sync,
                    frame_state,
                });
            }

            for &target in &block.successors {
                let arguments = Span::append(
                    &mut edge_arguments,
                    blocks[target.index()].live_in.iter().map(|&slot| {
                        let value = current
                            .get(&slot)
                            .copied()
                            .expect("successor live-in slots must have an SSA value on every edge");
                        ValueUse { slot, value }
                    }),
                );
                edges.push(BlockEdge { target, arguments });
            }
        }

        propagate_root_provenance(
            &mut values,
            &value_origins,
            &parameter_maps,
            &edges,
            &edge_arguments,
        )?;
        let propagation = propagate_constants(
            &typed,
            &values,
            &value_origins,
            &blocks,
            &parameter_maps,
            &edges,
            &edge_arguments,
            &module.constants,
            &instruction_values,
        );
        let mut constant_values = Vec::with_capacity(propagation.values.len());
        let mut constant_known = vec![0_u64; propagation.values.len().div_ceil(64)];
        for (index, constant) in propagation.values.into_iter().enumerate() {
            let value = match constant {
                ConstantLattice::Known(value) => {
                    constant_known[index / 64] |= 1_u64 << (index % 64);
                    value
                }
                ConstantLattice::Unknown | ConstantLattice::Overdefined => 0,
            };
            constant_values.push(value);
        }
        let executable_blocks = bitset_from_bools(&propagation.executable_blocks);
        let executable_edges = bitset_from_bools(&propagation.executable_edges);

        let mut predecessor_storage = Vec::new();
        let mut block_records = Vec::with_capacity(blocks.len());
        let mut parameter_cursor = 0_u32;
        let mut edge_cursor = 0_u32;
        for (index, block) in blocks.iter().enumerate() {
            let predecessors =
                Span::append(&mut predecessor_storage, block.predecessors.iter().copied());
            let parameter_count = block.live_in.len() as u32;
            let successor_count = block.successors.len() as u32;
            block_records.push(BasicBlock {
                id: BlockId(index as u32),
                start_pc: block.start as u32,
                end_pc: block.end as u32,
                parameters: Span {
                    start: parameter_cursor,
                    len: parameter_count,
                },
                predecessors,
                successors: Span {
                    start: edge_cursor,
                    len: successor_count,
                },
                reachable: block.reachable,
            });
            parameter_cursor += parameter_count;
            edge_cursor += successor_count;
        }

        let call_iface_method_indices = func
            .code
            .iter()
            .enumerate()
            .map(|(pc, instruction)| {
                if instruction.opcode() != Opcode::CallIface {
                    return NONE_ID;
                }
                func.instruction_metadata
                    .get(pc)
                    .and_then(crate::metadata::call_iface_method_index_from_instruction)
                    .unwrap_or(NONE_ID)
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();

        let mut ir = Self {
            instructions: typed.into_boxed_slice(),
            blocks: block_records.into_boxed_slice(),
            values: values.into_boxed_slice(),
            instruction_values: instruction_values.into_boxed_slice(),
            frame_states: frame_states.into_boxed_slice(),
            frame_values: frame_values.into_boxed_slice(),
            root_slots: root_slots.into_boxed_slice(),
            block_parameters: block_parameters.into_boxed_slice(),
            predecessors: predecessor_storage.into_boxed_slice(),
            edges: edges.into_boxed_slice(),
            edge_arguments: edge_arguments.into_boxed_slice(),
            constant_values: constant_values.into_boxed_slice(),
            constant_known: constant_known.into_boxed_slice(),
            executable_blocks: executable_blocks.into_boxed_slice(),
            executable_edges: executable_edges.into_boxed_slice(),
            call_iface_method_indices,
            retained_bytes: 0,
        };
        ir.retained_bytes = ir.compute_retained_bytes();
        if ir.retained_bytes > retained_limit_bytes {
            return Err(JitError::AnalysisResourceLimitExceeded {
                limit_bytes: retained_limit_bytes,
                requested_bytes: ir.retained_bytes,
            });
        }
        ir.verify(func, &pc_to_block)?;
        Ok(ir)
    }

    fn empty() -> Self {
        Self {
            instructions: Box::new([]),
            blocks: Box::new([]),
            values: Box::new([]),
            instruction_values: Box::new([]),
            frame_states: Box::new([]),
            frame_values: Box::new([]),
            root_slots: Box::new([]),
            block_parameters: Box::new([]),
            predecessors: Box::new([]),
            edges: Box::new([]),
            edge_arguments: Box::new([]),
            constant_values: Box::new([]),
            constant_known: Box::new([]),
            executable_blocks: Box::new([]),
            executable_edges: Box::new([]),
            call_iface_method_indices: Box::new([]),
            retained_bytes: 0,
        }
    }

    #[inline]
    pub(crate) fn instruction(&self, pc: usize) -> Option<&TypedInstruction> {
        self.instructions.get(pc)
    }

    pub(crate) fn blocks(&self) -> &[BasicBlock] {
        &self.blocks
    }

    pub(crate) fn block_parameters(&self, block: BlockId) -> &[ValueUse] {
        self.blocks[block.index()]
            .parameters
            .slice(&self.block_parameters)
    }

    /// Values that must survive a side exit before the instruction at `pc`.
    /// Effect, periodic, loop-header, and terminal OSR checkpoints own
    /// explicit frame states. Other basic-block entries reuse their canonical
    /// SSA parameters, avoiding duplicate state.
    pub(crate) fn resume_values(&self, pc: usize) -> Option<&[FrameValue]> {
        if let Some(state) = self.frame_state(pc).copied() {
            return Some(self.frame_values(state));
        }
        let instruction = self.instruction(pc)?;
        let block = &self.blocks[instruction.block().index()];
        (block.start_pc as usize == pc).then(|| self.block_parameters(block.id))
    }

    pub(crate) fn predecessors(&self, block: BlockId) -> &[BlockId] {
        self.blocks[block.index()]
            .predecessors
            .slice(&self.predecessors)
    }

    pub(crate) fn successors(&self, block: BlockId) -> &[BlockEdge] {
        self.blocks[block.index()].successors.slice(&self.edges)
    }

    pub(crate) fn edge_arguments(&self, edge: BlockEdge) -> &[ValueUse] {
        edge.arguments.slice(&self.edge_arguments)
    }

    pub(crate) fn inputs(&self, instruction: TypedInstruction) -> &[ValueId] {
        let values = instruction.values.slice(&self.instruction_values);
        &values[..usize::from(instruction.input_count)]
    }

    pub(crate) fn outputs(&self, instruction: TypedInstruction) -> &[ValueId] {
        let values = instruction.values.slice(&self.instruction_values);
        &values[usize::from(instruction.input_count)..]
    }

    pub(crate) fn value(&self, value: ValueId) -> SsaValue {
        self.values[value.index()]
    }

    pub(crate) fn constant(&self, value: ValueId) -> Option<i64> {
        let index = value.index();
        let known = self
            .constant_known
            .get(index / 64)
            .is_some_and(|word| word & (1_u64 << (index % 64)) != 0);
        known.then(|| self.constant_values[index])
    }

    pub(crate) fn input_constants(&self, pc: usize) -> impl Iterator<Item = (u16, i64)> + '_ {
        self.instruction(pc)
            .into_iter()
            .flat_map(|instruction| self.inputs(*instruction).iter().copied())
            .filter_map(|value| {
                self.constant(value)
                    .map(|constant| (self.value(value).slot, constant))
            })
    }

    pub(crate) fn input_value(&self, pc: usize, slot: u16) -> Option<ValueId> {
        let instruction = *self.instruction(pc)?;
        self.inputs(instruction)
            .iter()
            .copied()
            .find(|&value| self.value(value).slot == slot)
    }

    pub(crate) fn output_value(&self, pc: usize, slot: u16) -> Option<ValueId> {
        let instruction = *self.instruction(pc)?;
        self.outputs(instruction)
            .iter()
            .copied()
            .find(|&value| self.value(value).slot == slot)
    }

    pub(crate) fn frame_value(&self, pc: usize, slot: u16) -> Option<ValueId> {
        let state = *self.frame_state(pc)?;
        self.frame_values(state)
            .iter()
            .find_map(|value| (value.slot == slot).then_some(value.value))
    }

    pub(crate) fn input_constant(&self, pc: usize, slot: u16) -> Option<i64> {
        self.input_constants(pc)
            .find_map(|(input_slot, value)| (input_slot == slot).then_some(value))
    }

    pub(crate) fn call_iface_method_index(&self, pc: usize) -> Option<u32> {
        let index = *self.call_iface_method_indices.get(pc)?;
        (index != NONE_ID).then_some(index)
    }

    pub(crate) fn is_executable_block(&self, block: BlockId) -> bool {
        bitset_contains(&self.executable_blocks, block.index())
    }

    pub(crate) fn executable_successors(
        &self,
        block: BlockId,
    ) -> impl Iterator<Item = BlockEdge> + '_ {
        let span = self.blocks[block.index()].successors;
        let start = span.start as usize;
        span.slice(&self.edges)
            .iter()
            .copied()
            .enumerate()
            .filter_map(move |(offset, edge)| {
                bitset_contains(&self.executable_edges, start + offset).then_some(edge)
            })
    }

    #[inline]
    pub(crate) fn instruction_count(&self) -> usize {
        self.instructions.len()
    }

    #[inline]
    pub(crate) fn value_count(&self) -> usize {
        self.values.len()
    }

    pub(crate) fn used_slots(&self) -> impl Iterator<Item = u16> + '_ {
        self.values.iter().map(|value| value.slot)
    }

    pub(crate) fn frame_state(&self, pc: usize) -> Option<&FrameState> {
        let id = self.instruction(pc)?.frame_state_id()?;
        self.frame_states.get(id.index())
    }

    pub(crate) fn frame_values(&self, state: FrameState) -> &[FrameValue] {
        state.values.slice(&self.frame_values)
    }

    pub(crate) fn direct_roots(&self, state: FrameState) -> &[u16] {
        state.direct_roots.slice(&self.root_slots)
    }

    pub(crate) fn conditional_roots(&self, state: FrameState) -> &[u16] {
        state.conditional_roots.slice(&self.root_slots)
    }

    pub(crate) fn deopt_metadata(
        &self,
        pc_range: std::ops::Range<usize>,
    ) -> Vec<crate::native_stack_map::DeoptFrameState> {
        self.frame_states
            .iter()
            .copied()
            .enumerate()
            .filter(|(_, state)| pc_range.contains(&(state.resume_pc as usize)))
            .map(
                |(state_id, state)| crate::native_stack_map::DeoptFrameState {
                    state_id: state_id as u32,
                    resume_pc: state.resume_pc,
                    parent_state_id: state
                        .parent()
                        .map_or(crate::native_stack_map::DeoptFrameState::NO_PARENT, |id| {
                            id.0
                        }),
                    values: self
                        .frame_values(state)
                        .iter()
                        .map(|value| {
                            let ssa = self.value(value.value);
                            crate::native_stack_map::DeoptValue {
                                slot: value.slot,
                                kind: match ssa.ty {
                                    ValueType::Word => {
                                        crate::native_stack_map::DeoptValueKind::Word
                                    }
                                    ValueType::Float64 => {
                                        crate::native_stack_map::DeoptValueKind::Float64
                                    }
                                    ValueType::GcRef(_) => {
                                        crate::native_stack_map::DeoptValueKind::GcRef
                                    }
                                    ValueType::InterfaceHeader => {
                                        crate::native_stack_map::DeoptValueKind::InterfaceHeader
                                    }
                                    ValueType::InterfaceData => {
                                        crate::native_stack_map::DeoptValueKind::InterfaceData
                                    }
                                },
                                location: self.constant(value.value).map_or(
                                    crate::native_stack_map::DeoptValueLocation::FiberSlot(
                                        value.slot,
                                    ),
                                    |constant| {
                                        crate::native_stack_map::DeoptValueLocation::Constant(
                                            constant as u64,
                                        )
                                    },
                                ),
                            }
                        })
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                },
            )
            .collect()
    }

    #[inline]
    pub(crate) fn retained_bytes(&self) -> usize {
        self.retained_bytes
    }

    fn compute_retained_bytes(&self) -> usize {
        self.instructions.len() * core::mem::size_of::<TypedInstruction>()
            + self.blocks.len() * core::mem::size_of::<BasicBlock>()
            + self.values.len() * core::mem::size_of::<SsaValue>()
            + self.instruction_values.len() * core::mem::size_of::<ValueId>()
            + self.frame_states.len() * core::mem::size_of::<FrameState>()
            + self.frame_values.len() * core::mem::size_of::<FrameValue>()
            + self.root_slots.len() * core::mem::size_of::<u16>()
            + self.block_parameters.len() * core::mem::size_of::<ValueUse>()
            + self.predecessors.len() * core::mem::size_of::<BlockId>()
            + self.edges.len() * core::mem::size_of::<BlockEdge>()
            + self.edge_arguments.len() * core::mem::size_of::<ValueUse>()
            + self.constant_values.len() * core::mem::size_of::<i64>()
            + self.constant_known.len() * core::mem::size_of::<u64>()
            + self.executable_blocks.len() * core::mem::size_of::<u64>()
            + self.executable_edges.len() * core::mem::size_of::<u64>()
            + self.call_iface_method_indices.len() * core::mem::size_of::<u32>()
    }

    fn verify(&self, func: &FunctionDef, pc_to_block: &[BlockId]) -> Result<(), JitError> {
        if self.instructions.len() != func.code.len()
            || pc_to_block.len() != func.code.len()
            || self.call_iface_method_indices.len() != func.code.len()
        {
            return Err(JitError::Internal(format!(
                "SSA instruction cardinality drift for {}",
                func.name
            )));
        }
        for (pc, instruction) in self.instructions.iter().copied().enumerate() {
            if instruction.source != func.code[pc] || instruction.block != pc_to_block[pc] {
                return Err(JitError::Internal(format!(
                    "SSA source/control-flow drift for {} at pc {pc}",
                    func.name
                )));
            }
            for &value_id in self
                .inputs(instruction)
                .iter()
                .chain(self.outputs(instruction))
            {
                self.values.get(value_id.index()).ok_or_else(|| {
                    JitError::Internal(format!(
                        "SSA value id overflow for {} at pc {pc}",
                        func.name
                    ))
                })?;
            }
            if instruction.effects.requires_frame_state() && instruction.frame_state_id().is_none()
            {
                return Err(JitError::Internal(format!(
                    "SSA effect/frame-state drift for {} at pc {pc}",
                    func.name
                )));
            }
        }
        for (index, block) in self.blocks.iter().copied().enumerate() {
            if block.id.index() != index
                || block.start_pc >= block.end_pc
                || block.end_pc as usize > self.instructions.len()
            {
                return Err(JitError::Internal(format!(
                    "SSA basic-block bounds drift for {} at block {index}",
                    func.name
                )));
            }
            let parameters = block.parameters.slice(&self.block_parameters);
            let predecessors = block.predecessors.slice(&self.predecessors);
            let successors = block.successors.slice(&self.edges);
            if block.reachable && index != 0 && predecessors.is_empty() {
                return Err(JitError::Internal(format!(
                    "SSA reachable block has no predecessor for {} at block {index}",
                    func.name
                )));
            }
            if parameters
                .windows(2)
                .any(|pair| pair[0].slot >= pair[1].slot)
                || successors
                    .iter()
                    .any(|edge| edge.target.index() >= self.blocks.len())
            {
                return Err(JitError::Internal(format!(
                    "SSA block edge/parameter drift for {} at block {index}",
                    func.name
                )));
            }
        }
        Ok(())
    }
}

fn validate_slots(
    func: &FunctionDef,
    pc: usize,
    slots: &[u16],
    access: &'static str,
) -> Result<(), JitError> {
    if let Some(&slot) = slots.iter().find(|&&slot| slot >= func.local_slots) {
        return Err(JitError::Internal(format!(
            "verified SSA {access} for {} at pc {pc} exceeds local slots: {slot} >= {}",
            func.name, func.local_slots
        )));
    }
    Ok(())
}

pub(crate) fn instruction_successors(
    pc: usize,
    instruction: Instruction,
    code_len: usize,
) -> Result<Vec<usize>, JitError> {
    let fallthrough = || (pc + 1 < code_len).then_some(pc + 1);
    Ok(match instruction.opcode() {
        Opcode::Jump => vec![crate::compile_common::checked_branch_target(
            code_len,
            pc,
            instruction.imm32(),
            instruction.opcode(),
        )?],
        Opcode::JumpIf | Opcode::JumpIfNot => {
            let mut successors = fallthrough().into_iter().collect::<Vec<_>>();
            successors.push(crate::compile_common::checked_branch_target(
                code_len,
                pc,
                instruction.imm32(),
                instruction.opcode(),
            )?);
            successors
        }
        Opcode::ForLoop => {
            let mut successors = fallthrough().into_iter().collect::<Vec<_>>();
            successors.push(crate::compile_common::checked_forloop_target(
                code_len,
                pc,
                &instruction,
            )?);
            successors
        }
        Opcode::Return | Opcode::Panic => Vec::new(),
        _ => fallthrough().into_iter().collect(),
    })
}

fn build_cfg(raw: &[RawInstruction]) -> Result<(Vec<BlockFacts>, Vec<BlockId>), JitError> {
    let mut leaders = BTreeSet::from([0_usize]);
    for (pc, instruction) in raw.iter().enumerate() {
        match instruction.source.opcode() {
            Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot | Opcode::ForLoop => {
                for successor in instruction_successors(pc, instruction.source, raw.len())? {
                    leaders.insert(successor);
                }
                if pc + 1 < raw.len() {
                    leaders.insert(pc + 1);
                }
            }
            Opcode::Return | Opcode::Panic => {
                if pc + 1 < raw.len() {
                    leaders.insert(pc + 1);
                }
            }
            _ => {}
        }
    }

    let leaders = leaders.into_iter().collect::<Vec<_>>();
    let mut blocks = Vec::with_capacity(leaders.len());
    let mut pc_to_block = vec![BlockId(0); raw.len()];
    for (index, &start) in leaders.iter().enumerate() {
        let end = leaders.get(index + 1).copied().unwrap_or(raw.len());
        for block in &mut pc_to_block[start..end] {
            *block = BlockId(index as u32);
        }
        blocks.push(BlockFacts {
            start,
            end,
            ..BlockFacts::default()
        });
    }

    for block in &mut blocks {
        let last_pc = block.end - 1;
        let mut successors = instruction_successors(last_pc, raw[last_pc].source, raw.len())?
            .into_iter()
            .map(|pc| pc_to_block[pc])
            .collect::<Vec<_>>();
        successors.sort_unstable();
        successors.dedup();
        block.successors = successors;
    }
    for index in 0..blocks.len() {
        let predecessor = BlockId(index as u32);
        let successors = blocks[index].successors.clone();
        for successor in successors {
            blocks[successor.index()].predecessors.push(predecessor);
        }
    }

    let mut pending = VecDeque::from([BlockId(0)]);
    blocks[0].reachable = true;
    while let Some(block) = pending.pop_front() {
        let successors = blocks[block.index()].successors.clone();
        for successor in successors {
            if !blocks[successor.index()].reachable {
                blocks[successor.index()].reachable = true;
                pending.push_back(successor);
            }
        }
    }
    Ok((blocks, pc_to_block))
}

fn compute_block_liveness(
    blocks: &mut [BlockFacts],
    raw: &[RawInstruction],
) -> Result<(), JitError> {
    for block in blocks.iter_mut() {
        for instruction in &raw[block.start..block.end] {
            for &slot in &instruction.reads {
                if !block.defs.contains(&slot) {
                    block.uses.insert(slot);
                }
            }
            block.defs.extend(instruction.writes.iter().copied());
        }
        block.live_in = block.uses.clone();
    }

    let mut pending = (0..blocks.len()).rev().collect::<VecDeque<_>>();
    let mut queued = vec![true; blocks.len()];
    let mut sparse_cells = blocks
        .iter()
        .map(|block| block.live_in.len() + block.live_out.len())
        .sum::<usize>();
    ensure_sparse_liveness_budget(sparse_cells)?;
    while let Some(index) = pending.pop_front() {
        queued[index] = false;
        let mut live_out = BTreeSet::new();
        for successor in blocks[index].successors.iter().copied() {
            live_out.extend(blocks[successor.index()].live_in.iter().copied());
        }
        let mut live_in = blocks[index].uses.clone();
        live_in.extend(live_out.difference(&blocks[index].defs).copied());
        if live_in != blocks[index].live_in || live_out != blocks[index].live_out {
            sparse_cells = sparse_cells
                .saturating_sub(blocks[index].live_in.len() + blocks[index].live_out.len())
                .saturating_add(live_in.len() + live_out.len());
            ensure_sparse_liveness_budget(sparse_cells)?;
            blocks[index].live_in = live_in;
            blocks[index].live_out = live_out;
            for predecessor in blocks[index].predecessors.iter().copied() {
                if !queued[predecessor.index()] {
                    queued[predecessor.index()] = true;
                    pending.push_back(predecessor.index());
                }
            }
        }
    }
    Ok(())
}

fn ensure_sparse_liveness_budget(sparse_cells: usize) -> Result<(), JitError> {
    let requested_bytes = sparse_cells.saturating_mul(core::mem::size_of::<u16>() * 4);
    if requested_bytes > MAX_JIT_COMPILE_WORK_BYTES {
        return Err(JitError::CompileWorkLimitExceeded {
            limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
            requested_bytes,
        });
    }
    Ok(())
}

/// Frame-memory cells that may be observed by a future dynamically indexed
/// inline-array access. This is separate from SSA liveness because the
/// addressed cells stay authoritative in frame memory, yet it follows the
/// same kill/use dataflow so later definitions do not keep stale roots alive.
fn compute_aliased_memory_live_out(
    blocks: &[BlockFacts],
    raw: &[RawInstruction],
    slot_types: &[SlotType],
) -> Result<Vec<BTreeSet<u16>>, JitError> {
    let mut uses = vec![BTreeSet::new(); blocks.len()];
    let mut defs = vec![BTreeSet::new(); blocks.len()];
    for (index, block) in blocks.iter().enumerate() {
        for instruction in &raw[block.start..block.end] {
            if let MemorySyncEffect::AliasedRange { start, count } = instruction.memory_sync {
                for offset in 0..count {
                    if let Some(root) = root_slot_for_cell(slot_types, start + offset) {
                        if !defs[index].contains(&root) {
                            uses[index].insert(root);
                        }
                    }
                }
            }
            defs[index].extend(
                instruction
                    .writes
                    .iter()
                    .filter_map(|&slot| root_slot_for_cell(slot_types, slot)),
            );
        }
    }
    let mut live_in = uses.clone();
    let mut live_out = vec![BTreeSet::new(); blocks.len()];
    let mut sparse_cells = live_in.iter().map(BTreeSet::len).sum::<usize>();
    ensure_sparse_liveness_budget(sparse_cells)?;
    let mut pending = (0..blocks.len()).rev().collect::<VecDeque<_>>();
    let mut queued = vec![true; blocks.len()];
    while let Some(index) = pending.pop_front() {
        queued[index] = false;
        let mut outgoing = BTreeSet::new();
        for successor in blocks[index].successors.iter().copied() {
            outgoing.extend(live_in[successor.index()].iter().copied());
        }
        let mut incoming = uses[index].clone();
        incoming.extend(outgoing.difference(&defs[index]).copied());
        if incoming != live_in[index] || outgoing != live_out[index] {
            sparse_cells = sparse_cells
                .saturating_sub(live_in[index].len() + live_out[index].len())
                .saturating_add(incoming.len() + outgoing.len());
            ensure_sparse_liveness_budget(sparse_cells)?;
            live_in[index] = incoming;
            live_out[index] = outgoing;
            for predecessor in blocks[index].predecessors.iter().copied() {
                if !queued[predecessor.index()] {
                    queued[predecessor.index()] = true;
                    pending.push_back(predecessor.index());
                }
            }
        }
    }
    Ok(live_out)
}

#[inline]
fn root_slot_for_cell(slot_types: &[SlotType], slot: u16) -> Option<u16> {
    match slot_types.get(usize::from(slot)).copied()? {
        SlotType::GcBase | SlotType::GcRef | SlotType::Interface0 => Some(slot),
        SlotType::Interface1 => slot
            .checked_sub(1)
            .filter(|&header| slot_types.get(usize::from(header)) == Some(&SlotType::Interface0)),
        SlotType::Value | SlotType::Float => None,
    }
}

type SparseFrameLiveness = (Vec<Option<FrameLiveness>>, Vec<u16>, Vec<u16>);

fn compute_sparse_frame_liveness(
    blocks: &[BlockFacts],
    raw: &[RawInstruction],
    slot_types: &[SlotType],
    retained_limit_bytes: usize,
) -> Result<SparseFrameLiveness, JitError> {
    let result_bytes = raw
        .len()
        .saturating_mul(core::mem::size_of::<Option<FrameLiveness>>());
    if result_bytes > MAX_JIT_COMPILE_WORK_BYTES {
        return Err(JitError::CompileWorkLimitExceeded {
            limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
            requested_bytes: result_bytes,
        });
    }
    if result_bytes > retained_limit_bytes {
        return Err(JitError::AnalysisResourceLimitExceeded {
            limit_bytes: retained_limit_bytes,
            requested_bytes: result_bytes,
        });
    }
    let mut result = Vec::new();
    result
        .try_reserve_exact(raw.len())
        .map_err(|_| JitError::CompileWorkLimitExceeded {
            limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
            requested_bytes: result_bytes,
        })?;
    result.resize_with(raw.len(), || None);
    let mut live_slots = Vec::new();
    let mut root_slots = Vec::new();
    let mut frame_state_count = 0usize;
    let memory_live_out = compute_aliased_memory_live_out(blocks, raw, slot_types)?;
    for (block_index, block) in blocks.iter().enumerate() {
        let mut live = block.live_out.clone();
        let mut memory_live = memory_live_out[block_index].clone();
        for pc in (block.start..block.end).rev() {
            for slot in &raw[pc].writes {
                live.remove(slot);
                if let Some(root) = root_slot_for_cell(slot_types, *slot) {
                    memory_live.remove(&root);
                }
            }
            live.extend(raw[pc].reads.iter().copied());
            if let MemorySyncEffect::AliasedRange { start, count } = raw[pc].memory_sync {
                memory_live.extend(
                    (0..count).filter_map(|offset| root_slot_for_cell(slot_types, start + offset)),
                );
            }
            let is_periodic_checkpoint =
                pc % crate::compile_common::EXECUTION_BUDGET_REGION_INSTRUCTIONS == 0;
            let is_loop_header_checkpoint = pc == block.start
                && block
                    .predecessors
                    .iter()
                    .any(|predecessor| blocks[predecessor.index()].start >= block.start);
            let is_terminal_osr_checkpoint = raw[pc].source.opcode() == Opcode::Return;
            if raw[pc].effects.requires_frame_state()
                || is_periodic_checkpoint
                || is_loop_header_checkpoint
                || is_terminal_osr_checkpoint
            {
                let live_count = live.len();
                let direct_root_count = slot_types
                    .iter()
                    .enumerate()
                    .filter(|(slot, ty)| {
                        ty.is_managed_ref()
                            && (live.contains(&(*slot as u16))
                                || memory_live.contains(&(*slot as u16)))
                    })
                    .count();
                let conditional_root_count = slot_types
                    .iter()
                    .enumerate()
                    .filter(|(slot, ty)| {
                        **ty == SlotType::Interface0
                            && (live.contains(&(*slot as u16))
                                || live.contains(&((*slot + 1) as u16))
                                || memory_live.contains(&(*slot as u16)))
                    })
                    .count();
                let root_count = direct_root_count.saturating_add(conditional_root_count);
                frame_state_count = frame_state_count.saturating_add(1);
                let retained_bytes = live_slots
                    .len()
                    .saturating_add(live_count)
                    .saturating_mul(core::mem::size_of::<FrameValue>())
                    .saturating_add(
                        root_slots
                            .len()
                            .saturating_add(root_count)
                            .saturating_mul(core::mem::size_of::<u16>()),
                    )
                    .saturating_add(
                        frame_state_count.saturating_mul(core::mem::size_of::<FrameState>()),
                    )
                    .saturating_add(result_bytes);
                if retained_bytes > retained_limit_bytes {
                    return Err(JitError::AnalysisResourceLimitExceeded {
                        limit_bytes: retained_limit_bytes,
                        requested_bytes: retained_bytes,
                    });
                }
                let work_bytes = live_slots
                    .len()
                    .saturating_add(live_count)
                    .saturating_mul(core::mem::size_of::<u16>());
                if work_bytes > MAX_JIT_COMPILE_WORK_BYTES {
                    return Err(JitError::CompileWorkLimitExceeded {
                        limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
                        requested_bytes: work_bytes,
                    });
                }
                live_slots.try_reserve_exact(live_count).map_err(|_| {
                    JitError::AnalysisResourceLimitExceeded {
                        limit_bytes: retained_limit_bytes,
                        requested_bytes: retained_bytes,
                    }
                })?;
                root_slots.try_reserve_exact(root_count).map_err(|_| {
                    JitError::AnalysisResourceLimitExceeded {
                        limit_bytes: retained_limit_bytes,
                        requested_bytes: retained_bytes,
                    }
                })?;
                let live_slots_span = Span::append(&mut live_slots, live.iter().copied());
                let direct_roots = Span::append(
                    &mut root_slots,
                    slot_types.iter().enumerate().filter_map(|(slot, ty)| {
                        (ty.is_managed_ref()
                            && (live.contains(&(slot as u16))
                                || memory_live.contains(&(slot as u16))))
                        .then_some(slot as u16)
                    }),
                );
                let conditional_roots = Span::append(
                    &mut root_slots,
                    slot_types.iter().enumerate().filter_map(|(slot, ty)| {
                        (*ty == SlotType::Interface0
                            && (live.contains(&(slot as u16))
                                || live.contains(&((slot + 1) as u16))
                                || memory_live.contains(&(slot as u16))))
                        .then_some(slot as u16)
                    }),
                );
                result[pc] = Some(FrameLiveness {
                    live_slots: live_slots_span,
                    direct_roots,
                    conditional_roots,
                });
            }
        }
    }
    Ok((result, live_slots, root_slots))
}

fn push_value(
    values: &mut Vec<SsaValue>,
    origins: &mut Vec<ValueOrigin>,
    func: &FunctionDef,
    slot: u16,
    provenance: RootProvenance,
    origin: ValueOrigin,
) -> Result<ValueId, JitError> {
    let slot_type = func.slot_types.get(slot as usize).copied().ok_or_else(|| {
        JitError::Internal(format!(
            "SSA type for {} slot {slot} is absent from slot metadata",
            func.name
        ))
    })?;
    let id = ValueId(
        values
            .len()
            .try_into()
            .map_err(|_| JitError::CompileWorkLimitExceeded {
                limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
                requested_bytes: usize::MAX,
            })?,
    );
    values.push(SsaValue {
        ty: ValueType::for_slot(slot_type, provenance),
        slot,
    });
    origins.push(origin);
    Ok(id)
}

fn alias_source(
    source: Instruction,
    output_slot: u16,
    inputs: &[ValueId],
    values: &[SsaValue],
) -> Option<ValueId> {
    let source_slot = match source.opcode() {
        Opcode::Copy if output_slot == source.a => Some(source.b),
        Opcode::CopyN => output_slot
            .checked_sub(source.a)
            .filter(|&offset| offset < source.copy_n_count())
            .and_then(|offset| source.b.checked_add(offset)),
        _ => None,
    }?;
    inputs.iter().copied().find(|&value| {
        values
            .get(value.index())
            .is_some_and(|ssa| ssa.slot == source_slot)
    })
}

fn fixed_output_provenance(
    source: Instruction,
    output_slot: u16,
    alias: Option<ValueId>,
    module: &Module,
    exact_base_returns: &[Box<[bool]>],
) -> RootProvenance {
    if alias.is_some() {
        return RootProvenance::Unreachable;
    }
    if source.opcode() == Opcode::Call {
        let callee_id = source.static_call_func_id() as usize;
        let Some(callee) = module.functions.get(callee_id) else {
            return RootProvenance::Unknown;
        };
        let Some(first_return) = source.b.checked_add(callee.param_slots) else {
            return RootProvenance::Unknown;
        };
        let Some(return_offset) = output_slot.checked_sub(first_return) else {
            return RootProvenance::Unknown;
        };
        return if exact_base_returns
            .get(callee_id)
            .and_then(|summary| summary.get(return_offset as usize))
            .copied()
            .unwrap_or(false)
        {
            RootProvenance::ExactBase
        } else {
            RootProvenance::Unknown
        };
    }
    match source.opcode() {
        Opcode::PtrNew
        | Opcode::StrNew
        | Opcode::StrConcat
        | Opcode::StrSlice
        | Opcode::ArrayNew
        | Opcode::SliceNew
        | Opcode::SliceSlice
        | Opcode::MapNew
        | Opcode::QueueNew
        | Opcode::ClosureNew
        | Opcode::IslandNew
        | Opcode::LoadConst => RootProvenance::ExactBase,
        Opcode::PtrAdd | Opcode::ArrayAddr | Opcode::SliceAddr => RootProvenance::Interior,
        _ => RootProvenance::Unknown,
    }
}

fn propagate_root_provenance(
    values: &mut [SsaValue],
    origins: &[ValueOrigin],
    parameter_maps: &[BTreeMap<u16, ValueId>],
    edges: &[BlockEdge],
    edge_arguments: &[ValueUse],
) -> Result<(), JitError> {
    let dependency_count = edge_arguments.len().saturating_add(
        origins
            .iter()
            .filter(|origin| matches!(origin, ValueOrigin::Alias(_)))
            .count(),
    );
    let requested_bytes = values
        .len()
        .saturating_mul(core::mem::size_of::<Vec<ValueId>>())
        .saturating_add(dependency_count.saturating_mul(core::mem::size_of::<ValueId>()));
    if requested_bytes > MAX_JIT_COMPILE_WORK_BYTES {
        return Err(JitError::CompileWorkLimitExceeded {
            limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
            requested_bytes,
        });
    }
    let mut dependents = vec![Vec::<ValueId>::new(); values.len()];
    for edge in edges {
        for argument in edge.arguments.slice(edge_arguments) {
            if let Some(&parameter) = parameter_maps[edge.target.index()].get(&argument.slot) {
                dependents[argument.value.index()].push(parameter);
            }
        }
    }
    for (index, origin) in origins.iter().copied().enumerate() {
        if let ValueOrigin::Alias(source) = origin {
            dependents[source.index()].push(ValueId::from_index(index));
        }
    }

    let mut pending = (0..values.len())
        .map(ValueId::from_index)
        .collect::<VecDeque<_>>();
    while let Some(source) = pending.pop_front() {
        let Some(source_provenance) = values[source.index()].ty.root_provenance() else {
            continue;
        };
        for &target in &dependents[source.index()] {
            let Some(current) = values[target.index()].ty.root_provenance() else {
                continue;
            };
            let merged = current.join(source_provenance);
            if merged != current {
                values[target.index()].ty = ValueType::GcRef(merged);
                pending.push_back(target);
            }
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn propagate_constants(
    instructions: &[TypedInstruction],
    values: &[SsaValue],
    origins: &[ValueOrigin],
    blocks: &[BlockFacts],
    parameter_maps: &[BTreeMap<u16, ValueId>],
    edges: &[BlockEdge],
    edge_arguments: &[ValueUse],
    module_constants: &[Constant],
    instruction_values: &[ValueId],
) -> ConstantPropagation {
    let mut constants = origins
        .iter()
        .map(|origin| match origin {
            ValueOrigin::EntrySlot => ConstantLattice::Overdefined,
            ValueOrigin::BlockParameter | ValueOrigin::Alias(_) | ValueOrigin::Instruction => {
                ConstantLattice::Unknown
            }
        })
        .collect::<Vec<_>>();
    let mut executable_blocks = vec![false; blocks.len()];
    let mut executable_edges = vec![false; edges.len()];
    if !blocks.is_empty() {
        executable_blocks[0] = true;
    }

    // Every value rises at most twice and every block/edge becomes executable
    // once. This bound closes conservatively if a future IR extension violates
    // one of those monotonicity rules.
    let iteration_limit = values
        .len()
        .saturating_mul(2)
        .saturating_add(blocks.len())
        .saturating_add(edges.len())
        .saturating_add(1);
    for _ in 0..iteration_limit {
        let mut changed = false;

        for (block_index, block) in blocks.iter().enumerate() {
            if !executable_blocks[block_index] {
                continue;
            }
            for instruction in instructions[block.start..block.end].iter().copied() {
                let all_values = instruction.values.slice(instruction_values);
                let inputs = &all_values[..usize::from(instruction.input_count)];
                let outputs = &all_values[usize::from(instruction.input_count)..];
                for &output in outputs {
                    let desired = match origins[output.index()] {
                        ValueOrigin::Alias(source) => constants[source.index()],
                        ValueOrigin::Instruction => instruction_constant(
                            instruction.source,
                            values[output.index()].slot,
                            inputs,
                            values,
                            &constants,
                            module_constants,
                        ),
                        ValueOrigin::EntrySlot | ValueOrigin::BlockParameter => continue,
                    };
                    let current = constants[output.index()];
                    let merged = current.join(desired);
                    if merged != current {
                        constants[output.index()] = merged;
                        changed = true;
                    }
                }
            }
        }

        let mut edge_index = 0;
        for (block_index, block) in blocks.iter().enumerate() {
            let edge_range = edge_index..edge_index + block.successors.len();
            edge_index = edge_range.end;
            if !executable_blocks[block_index] {
                continue;
            }
            let last_pc = block.end - 1;
            let last = instructions[last_pc];
            let selected_target = selected_constant_branch_target(
                last_pc,
                last,
                instructions,
                instruction_values,
                values,
                &constants,
            );
            for candidate in edge_range {
                let edge = edges[candidate];
                let executable = match selected_target {
                    Some(Some(target)) => edge.target == target,
                    Some(None) => false,
                    None => true,
                };
                if executable && !executable_edges[candidate] {
                    executable_edges[candidate] = true;
                    changed = true;
                }
                if executable && !executable_blocks[edge.target.index()] {
                    executable_blocks[edge.target.index()] = true;
                    changed = true;
                }
            }
        }

        for (edge_index, edge) in edges.iter().copied().enumerate() {
            if !executable_edges[edge_index] {
                continue;
            }
            for argument in edge.arguments.slice(edge_arguments) {
                let Some(&parameter) = parameter_maps[edge.target.index()].get(&argument.slot)
                else {
                    continue;
                };
                let incoming = constants[argument.value.index()];
                let current = constants[parameter.index()];
                let merged = current.join(incoming);
                if merged != current {
                    constants[parameter.index()] = merged;
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }

    for constant in &mut constants {
        if *constant == ConstantLattice::Unknown {
            *constant = ConstantLattice::Overdefined;
        }
    }
    ConstantPropagation {
        values: constants,
        executable_blocks,
        executable_edges,
    }
}

/// `Some(Some(block))` selects one proven successor, `Some(None)` means no
/// successor, and `None` keeps every structural successor executable.
fn selected_constant_branch_target(
    pc: usize,
    instruction: TypedInstruction,
    instructions: &[TypedInstruction],
    instruction_values: &[ValueId],
    values: &[SsaValue],
    constants: &[ConstantLattice],
) -> Option<Option<BlockId>> {
    let source = instruction.source;
    match source.opcode() {
        Opcode::Return | Opcode::Panic => Some(None),
        Opcode::Jump => {
            let target = crate::compile_common::checked_branch_target(
                instructions.len(),
                pc,
                source.imm32(),
                source.opcode(),
            )
            .expect("CFG validation already checked the unconditional target");
            Some(Some(instructions[target].block))
        }
        Opcode::JumpIf | Opcode::JumpIfNot => {
            let all_values = instruction.values.slice(instruction_values);
            let condition = all_values[..usize::from(instruction.input_count)]
                .iter()
                .copied()
                .find(|&value| values[value.index()].slot == source.a)
                .map(|value| constants[value.index()])
                .unwrap_or(ConstantLattice::Overdefined);
            match condition {
                ConstantLattice::Unknown => Some(None),
                ConstantLattice::Overdefined => None,
                ConstantLattice::Known(value) => {
                    let taken = match source.opcode() {
                        Opcode::JumpIf => value != 0,
                        Opcode::JumpIfNot => value == 0,
                        _ => unreachable!(),
                    };
                    let target = if taken {
                        crate::compile_common::checked_branch_target(
                            instructions.len(),
                            pc,
                            source.imm32(),
                            source.opcode(),
                        )
                        .expect("CFG validation already checked the conditional target")
                    } else if pc + 1 < instructions.len() {
                        pc + 1
                    } else {
                        return Some(None);
                    };
                    Some(Some(instructions[target].block))
                }
            }
        }
        _ => None,
    }
}

fn instruction_constant(
    instruction: Instruction,
    output_slot: u16,
    inputs: &[ValueId],
    values: &[SsaValue],
    constants: &[ConstantLattice],
    module_constants: &[Constant],
) -> ConstantLattice {
    if output_slot != instruction.a {
        return ConstantLattice::Overdefined;
    }
    if crate::ir_constants::single_slot_result(&instruction, module_constants, |_| None).is_none() {
        return ConstantLattice::Overdefined;
    }
    if inputs
        .iter()
        .any(|value| constants[value.index()] == ConstantLattice::Unknown)
    {
        return ConstantLattice::Unknown;
    }
    if inputs
        .iter()
        .any(|value| constants[value.index()] == ConstantLattice::Overdefined)
    {
        return ConstantLattice::Overdefined;
    }

    match crate::ir_constants::single_slot_result(&instruction, module_constants, |slot| {
        inputs.iter().copied().find_map(|value| {
            (values[value.index()].slot == slot).then(|| match constants[value.index()] {
                ConstantLattice::Known(constant) => Some(constant),
                ConstantLattice::Unknown | ConstantLattice::Overdefined => None,
            })?
        })
    }) {
        Some(Some(value)) => ConstantLattice::Known(value),
        Some(None) | None => ConstantLattice::Overdefined,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::{InstructionMetadata, Module};

    fn branch(opcode: Opcode, a: u16, offset: i32) -> Instruction {
        Instruction::with_flags(
            opcode,
            0,
            a,
            offset as u32 as u16,
            (offset as u32 >> 16) as u16,
        )
    }

    #[test]
    fn gc_base_slots_seed_exact_ssa_provenance() {
        assert_eq!(
            ValueType::for_slot(SlotType::GcBase, RootProvenance::Unknown),
            ValueType::GcRef(RootProvenance::ExactBase)
        );
        assert_eq!(
            ValueType::for_slot(SlotType::GcRef, RootProvenance::Unknown),
            ValueType::GcRef(RootProvenance::Unknown)
        );
    }

    #[test]
    fn observable_global_writes_are_not_dce_pure() {
        for opcode in [Opcode::GlobalSet, Opcode::GlobalSetN] {
            let effects =
                EffectSet::from_contract(opcode, crate::contract::opcode_contract(opcode));
            assert!(!effects.can_eliminate());
        }
        let pure = EffectSet::from_contract(
            Opcode::LoadInt,
            crate::contract::opcode_contract(Opcode::LoadInt),
        );
        assert!(pure.can_eliminate());
    }

    fn module_with(code: Vec<Instruction>, slot_types: Vec<SlotType>) -> Module {
        let mut func = crate::test_fixtures::function(code, slot_types.len() as u16);
        func.slot_types = slot_types;
        func.local_slots = func.slot_types.len() as u16;
        func.instruction_metadata = vec![InstructionMetadata::None; func.code.len()];
        let mut module = Module::new("ssa".into());
        module.functions.push(func);
        module
    }

    #[test]
    fn diamond_merge_uses_a_typed_block_parameter() {
        let code = vec![
            branch(Opcode::JumpIf, 0, 3),
            Instruction::new(Opcode::LoadInt, 1, 10, 0),
            branch(Opcode::Jump, 0, 2),
            Instruction::new(Opcode::LoadInt, 1, 20, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ];
        let module = module_with(code, vec![SlotType::Value; 2]);
        let ir = FunctionIr::build(&module.functions[0], &module).unwrap();
        let merge = ir.instruction(4).unwrap().block();
        let parameters = ir.block_parameters(merge);
        assert!(parameters.iter().any(|parameter| parameter.slot == 1));
        assert_eq!(ir.predecessors(merge).len(), 2);
    }

    #[test]
    fn loop_carried_value_flows_through_edge_arguments() {
        let code = vec![
            Instruction::new(Opcode::LoadInt, 0, 0, 0),
            Instruction::new(Opcode::LoadInt, 1, 10, 0),
            Instruction::new(Opcode::ForLoop, 0, 1, (-1_i16) as u16),
            Instruction::new(Opcode::Return, 0, 1, 0),
        ];
        let module = module_with(code, vec![SlotType::Value; 2]);
        let ir = FunctionIr::build(&module.functions[0], &module).unwrap();
        let loop_block = ir.instruction(2).unwrap().block();
        assert!(
            ir.frame_state(2).is_some(),
            "native loop-header polls require an exact root projection"
        );
        let back_edge = ir
            .successors(loop_block)
            .iter()
            .copied()
            .find(|edge| edge.target == loop_block)
            .expect("loop back edge");
        assert!(ir
            .edge_arguments(back_edge)
            .iter()
            .any(|argument| argument.slot == 0));
    }

    #[test]
    fn agreeing_branch_constants_live_on_the_merge_value() {
        let code = vec![
            branch(Opcode::JumpIf, 0, 3),
            Instruction::new(Opcode::LoadInt, 1, 42, 0),
            branch(Opcode::Jump, 0, 2),
            Instruction::new(Opcode::LoadInt, 1, 42, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ];
        let module = module_with(code, vec![SlotType::Value; 2]);
        let ir = FunctionIr::build(&module.functions[0], &module).unwrap();
        assert_eq!(ir.input_constants(4).collect::<Vec<_>>(), vec![(1, 42)]);
    }

    #[test]
    fn disagreeing_branch_constants_make_the_merge_value_overdefined() {
        let code = vec![
            branch(Opcode::JumpIf, 0, 3),
            Instruction::new(Opcode::LoadInt, 1, 41, 0),
            branch(Opcode::Jump, 0, 2),
            Instruction::new(Opcode::LoadInt, 1, 42, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ];
        let module = module_with(code, vec![SlotType::Value; 2]);
        let ir = FunctionIr::build(&module.functions[0], &module).unwrap();
        assert!(ir.input_constants(4).next().is_none());
    }

    #[test]
    fn frame_state_is_sparse_and_is_the_gc_root_authority() {
        let code = vec![
            Instruction::new(Opcode::LoadConst, 0, 0, 0),
            Instruction::new(Opcode::StrSlice, 1, 0, 2),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ];
        let module = module_with(
            code,
            vec![
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
        );
        let ir = FunctionIr::build(&module.functions[0], &module).unwrap();
        let state = *ir.frame_state(1).expect("allocating string slice state");
        assert_eq!(ir.direct_roots(state), &[0]);
        assert!(ir.instruction(1).unwrap().effects().requires_frame_state());
    }

    #[test]
    fn return_owns_the_sparse_state_needed_by_an_osr_exit() {
        let code = vec![
            Instruction::new(Opcode::LoadInt, 0, 42, 0),
            Instruction::new(Opcode::LoadInt, 1, 77, 0),
            Instruction::new(Opcode::Return, 0, 1, 0),
        ];
        let module = module_with(code, vec![SlotType::Value; 2]);
        let ir = FunctionIr::build(&module.functions[0], &module).unwrap();
        let state = *ir.frame_state(2).expect("OSR return recovery state");

        assert_eq!(
            ir.frame_values(state)
                .iter()
                .map(|value| value.slot)
                .collect::<Vec<_>>(),
            vec![0]
        );
    }

    #[test]
    fn memory_aliased_interface_cells_remain_roots_until_observed() {
        let code = vec![
            Instruction::with_flags(Opcode::SlotSetN, 0, 0, 6, 8),
            Instruction::new(Opcode::StrNew, 10, 0, 0),
            Instruction::with_flags(Opcode::SlotGetN, 0, 12, 0, 6),
            Instruction::new(Opcode::Return, 12, 2, 0),
        ];
        let mut slot_types = vec![SlotType::Value; 14];
        for header in [0, 2, 4, 8, 12] {
            slot_types[header] = SlotType::Interface0;
            slot_types[header + 1] = SlotType::Interface1;
        }
        slot_types[10] = SlotType::GcRef;
        let mut module = module_with(code, slot_types);
        for pc in [0, 2] {
            module.functions[0].instruction_metadata[pc] = InstructionMetadata::SlotLayout {
                array_len: 3,
                elem_layout: vec![SlotType::Interface0, SlotType::Interface1],
            };
        }

        let ir = FunctionIr::build(&module.functions[0], &module).unwrap();
        let state = *ir.frame_state(1).expect("allocation frame state");
        assert_eq!(ir.conditional_roots(state), &[0, 2, 4]);
        assert!(!ir.conditional_roots(state).contains(&8));
        assert!(!ir.conditional_roots(state).contains(&12));
    }

    #[test]
    fn frame_state_budget_rejects_wide_live_roots_before_snapshot_allocation() {
        let root_slots = 128usize;
        let safepoints = 32usize;
        let raw = (0..safepoints)
            .map(|_| RawInstruction {
                source: Instruction::new(Opcode::StrSlice, 0, 0, 0),
                reads: Vec::new(),
                writes: Vec::new(),
                effects: EffectSet::from_contract(
                    Opcode::StrSlice,
                    crate::contract::opcode_contract(Opcode::StrSlice),
                ),
                memory_sync: MemorySyncEffect::None,
            })
            .collect::<Vec<_>>();
        let blocks = vec![BlockFacts {
            start: 0,
            end: safepoints,
            live_out: (0..root_slots as u16).collect(),
            ..BlockFacts::default()
        }];
        let slot_types = vec![SlotType::GcRef; root_slots];

        let limit = 4 * 1024;
        let error = compute_sparse_frame_liveness(&blocks, &raw, &slot_types, limit)
            .expect_err("wide root snapshots must fail within the configured budget");
        assert!(matches!(
            error,
            JitError::AnalysisResourceLimitExceeded {
                limit_bytes,
                requested_bytes,
            } if limit_bytes == limit && requested_bytes > limit
        ));
    }

    #[test]
    fn pointer_provenance_survives_copy() {
        let code = vec![
            Instruction::new(Opcode::PtrNew, 0, 2, 1),
            Instruction::new(Opcode::Copy, 1, 0, 0),
            Instruction::new(Opcode::PtrAdd, 2, 1, 1),
            Instruction::new(Opcode::Return, 2, 1, 0),
        ];
        let module = module_with(
            code,
            vec![SlotType::GcRef, SlotType::GcRef, SlotType::GcRef],
        );
        let ir = FunctionIr::build(&module.functions[0], &module).unwrap();
        let copy = *ir.instruction(1).unwrap();
        let copied = ir.value(ir.outputs(copy)[0]);
        let address = *ir.instruction(2).unwrap();
        let interior = ir.value(ir.outputs(address)[0]);
        assert_eq!(copied.ty, ValueType::GcRef(RootProvenance::ExactBase));
        assert_eq!(interior.ty, ValueType::GcRef(RootProvenance::Interior));
    }
}
