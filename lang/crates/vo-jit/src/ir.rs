//! Typed, effect-aware SSA shared by every JIT compilation tier.
//!
//! The IR uses basic-block parameters as phi nodes. This keeps construction
//! independent of dominance order, represents loop-carried values directly,
//! and gives full-function compilation and loop OSR one control-flow model.
//! Frame states are sparse snapshots of live bytecode slots at observable
//! instructions; GC roots, deoptimization, and future inlining all consume the
//! same snapshots.

use std::collections::{BTreeMap, BTreeSet, VecDeque};

use vo_runtime::bytecode::{FunctionDef, Module};
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
    fn index(self) -> usize {
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

    fn slice<'a, T>(self, storage: &'a [T]) -> &'a [T] {
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

    fn from_contract(contract: EffectContract) -> Self {
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

    #[cfg(test)]
    #[inline]
    pub(crate) fn block(self) -> BlockId {
        self.block
    }

    #[cfg(test)]
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

    fn frame_state_id(self) -> Option<FrameStateId> {
        (self.frame_state != NONE_ID).then_some(FrameStateId(self.frame_state))
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FrameValue {
    pub slot: u16,
    pub value: ValueId,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FrameState {
    /// Bytecode PC at which execution can be reconstructed.
    pub resume_pc: u32,
    values: Span,
    direct_roots: Span,
    pub has_conditional_roots: bool,
    /// Future inlined frame states form a parent chain through this field.
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
    retained_bytes: usize,
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
    pub(crate) fn build(func: &FunctionDef, module: &Module) -> Result<Self, JitError> {
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
                effects: EffectSet::from_contract(crate::contract::opcode_contract(
                    source.opcode(),
                )),
                memory_sync: instruction_effects.memory_sync,
            });
        }

        let (mut blocks, pc_to_block) = build_cfg(&raw)?;
        compute_block_liveness(&mut blocks, &raw)?;
        let live_at_frame_state = compute_sparse_frame_liveness(&blocks, &raw);

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
        let mut root_slots = Vec::new();
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

                let frame_state = if instruction.effects.requires_frame_state() {
                    let live = live_at_frame_state[pc].as_deref().unwrap_or_default();
                    let values_span = Span::append(
                        &mut frame_values,
                        live.iter().map(|&slot| {
                            let value = current.get(&slot).copied().expect(
                                "live-before slots must have an SSA value at instruction entry",
                            );
                            FrameValue { slot, value }
                        }),
                    );
                    let roots_span = Span::append(
                        &mut root_slots,
                        live.iter().copied().filter(|&slot| {
                            func.slot_types.get(slot as usize) == Some(&SlotType::GcRef)
                        }),
                    );
                    let has_conditional_roots = live.iter().any(|&slot| {
                        matches!(
                            func.slot_types.get(slot as usize),
                            Some(SlotType::Interface0 | SlotType::Interface1)
                        )
                    });
                    let id = frame_states.len() as u32;
                    frame_states.push(FrameState {
                        resume_pc: pc as u32,
                        values: values_span,
                        direct_roots: roots_span,
                        has_conditional_roots,
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
                    let provenance = fixed_output_provenance(instruction.source.opcode(), alias);
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
            &blocks,
            &parameter_maps,
            &edges,
            &edge_arguments,
        );

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
            retained_bytes: 0,
        };
        ir.retained_bytes = ir.compute_retained_bytes();
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

    pub(crate) fn any_slot_live_out(&self, block: BlockId, slots: &BTreeSet<u16>) -> bool {
        self.blocks[block.index()]
            .successors
            .slice(&self.edges)
            .iter()
            .flat_map(|edge| edge.arguments.slice(&self.edge_arguments))
            .any(|argument| slots.contains(&argument.slot))
    }

    #[cfg(test)]
    pub(crate) fn block_parameters(&self, block: BlockId) -> &[ValueUse] {
        self.blocks[block.index()]
            .parameters
            .slice(&self.block_parameters)
    }

    #[cfg(test)]
    pub(crate) fn predecessors(&self, block: BlockId) -> &[BlockId] {
        self.blocks[block.index()]
            .predecessors
            .slice(&self.predecessors)
    }

    #[cfg(test)]
    pub(crate) fn successors(&self, block: BlockId) -> &[BlockEdge] {
        self.blocks[block.index()].successors.slice(&self.edges)
    }

    #[cfg(test)]
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
                                location: crate::native_stack_map::DeoptValueLocation::FiberSlot(
                                    value.slot,
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
    }

    fn verify(&self, func: &FunctionDef, pc_to_block: &[BlockId]) -> Result<(), JitError> {
        if self.instructions.len() != func.code.len() || pc_to_block.len() != func.code.len() {
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
            if instruction.effects.requires_frame_state() != instruction.frame_state_id().is_some()
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

fn instruction_successors(
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

    for index in 0..blocks.len() {
        let last_pc = blocks[index].end - 1;
        let mut successors = instruction_successors(last_pc, raw[last_pc].source, raw.len())?
            .into_iter()
            .map(|pc| pc_to_block[pc])
            .collect::<Vec<_>>();
        successors.sort_unstable();
        successors.dedup();
        blocks[index].successors = successors;
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
    while let Some(index) = pending.pop_front() {
        queued[index] = false;
        let mut live_out = BTreeSet::new();
        for successor in blocks[index].successors.iter().copied() {
            live_out.extend(blocks[successor.index()].live_in.iter().copied());
        }
        let mut live_in = blocks[index].uses.clone();
        live_in.extend(live_out.difference(&blocks[index].defs).copied());
        if live_in != blocks[index].live_in || live_out != blocks[index].live_out {
            blocks[index].live_in = live_in;
            blocks[index].live_out = live_out;
            for predecessor in blocks[index].predecessors.iter().copied() {
                if !queued[predecessor.index()] {
                    queued[predecessor.index()] = true;
                    pending.push_back(predecessor.index());
                }
            }
        }
        let sparse_cells = blocks
            .iter()
            .map(|block| block.live_in.len() + block.live_out.len())
            .sum::<usize>();
        let requested_bytes = sparse_cells.saturating_mul(core::mem::size_of::<u16>() * 4);
        if requested_bytes > MAX_JIT_COMPILE_WORK_BYTES {
            return Err(JitError::CompileWorkLimitExceeded {
                limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
                requested_bytes,
            });
        }
    }
    Ok(())
}

fn compute_sparse_frame_liveness(
    blocks: &[BlockFacts],
    raw: &[RawInstruction],
) -> Vec<Option<Box<[u16]>>> {
    let mut result = vec![None; raw.len()];
    for block in blocks {
        let mut live = block.live_out.clone();
        for pc in (block.start..block.end).rev() {
            for slot in &raw[pc].writes {
                live.remove(slot);
            }
            live.extend(raw[pc].reads.iter().copied());
            if raw[pc].effects.requires_frame_state() {
                result[pc] = Some(live.iter().copied().collect::<Vec<_>>().into_boxed_slice());
            }
        }
    }
    result
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

fn fixed_output_provenance(opcode: Opcode, alias: Option<ValueId>) -> RootProvenance {
    if alias.is_some() {
        return RootProvenance::Unreachable;
    }
    match opcode {
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
    blocks: &[BlockFacts],
    parameter_maps: &[BTreeMap<u16, ValueId>],
    edges: &[BlockEdge],
    edge_arguments: &[ValueUse],
) {
    loop {
        let mut changed = false;
        let mut edge_index = 0;
        for block in blocks {
            for _ in &block.successors {
                let edge = edges[edge_index];
                edge_index += 1;
                for argument in edge.arguments.slice(edge_arguments) {
                    let Some(&parameter) = parameter_maps[edge.target.index()].get(&argument.slot)
                    else {
                        continue;
                    };
                    let Some(incoming) = values[argument.value.index()].ty.root_provenance() else {
                        continue;
                    };
                    let Some(current) = values[parameter.index()].ty.root_provenance() else {
                        continue;
                    };
                    let merged = current.join(incoming);
                    if merged != current {
                        values[parameter.index()].ty = ValueType::GcRef(merged);
                        changed = true;
                    }
                }
            }
        }
        for index in 0..values.len() {
            let ValueOrigin::Alias(source) = origins[index] else {
                continue;
            };
            let Some(source_provenance) = values[source.index()].ty.root_provenance() else {
                continue;
            };
            let Some(current) = values[index].ty.root_provenance() else {
                continue;
            };
            let merged = current.join(source_provenance);
            if merged != current {
                values[index].ty = ValueType::GcRef(merged);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::{DynamicCallsiteMap, InstructionMetadata, Module};

    fn branch(opcode: Opcode, a: u16, offset: i32) -> Instruction {
        Instruction::with_flags(
            opcode,
            0,
            a,
            offset as u32 as u16,
            (offset as u32 >> 16) as u16,
        )
    }

    fn module_with(
        code: Vec<Instruction>,
        slot_types: Vec<SlotType>,
    ) -> (Module, std::sync::Arc<DynamicCallsiteMap>) {
        let mut func = crate::test_fixtures::function(code, slot_types.len() as u16);
        func.slot_types = slot_types;
        func.local_slots = func.slot_types.len() as u16;
        func.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&func.slot_types);
        func.borrowed_scan_slots_prefix =
            FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
        func.instruction_metadata = vec![InstructionMetadata::None; func.code.len()];
        let mut module = Module::new("ssa".into());
        module.functions.push(func);
        let calls = std::sync::Arc::new(DynamicCallsiteMap::for_module(&module));
        (module, calls)
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
        let (module, _) = module_with(code, vec![SlotType::Value; 2]);
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
        let (module, _) = module_with(code, vec![SlotType::Value; 2]);
        let ir = FunctionIr::build(&module.functions[0], &module).unwrap();
        let loop_block = ir.instruction(2).unwrap().block();
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
    fn frame_state_is_sparse_and_is_the_gc_root_authority() {
        let code = vec![
            Instruction::new(Opcode::LoadConst, 0, 0, 0),
            Instruction::new(Opcode::StrSlice, 1, 0, 2),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ];
        let (module, _) = module_with(
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
        assert_eq!(state.resume_pc, 1);
        assert_eq!(ir.direct_roots(state), &[0]);
        assert_eq!(ir.frame_values(state).len(), 3);
        assert!(ir.instruction(1).unwrap().effects().requires_frame_state());
    }

    #[test]
    fn pointer_provenance_survives_copy() {
        let code = vec![
            Instruction::new(Opcode::PtrNew, 0, 2, 1),
            Instruction::new(Opcode::Copy, 1, 0, 0),
            Instruction::new(Opcode::PtrAdd, 2, 1, 1),
            Instruction::new(Opcode::Return, 2, 1, 0),
        ];
        let (module, _) = module_with(
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
