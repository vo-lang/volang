//! Exact interpreter-frame root liveness derived from verified bytecode.
//!
//! A loaded module owns these immutable facts. VM frames publish a compact
//! state id, while the interpreter and JIT consume the same direct and
//! conditional root sets.

#[cfg(not(feature = "std"))]
use alloc::{
    boxed::Box,
    collections::{BTreeMap, BTreeSet, VecDeque},
    vec,
    vec::Vec,
};
#[cfg(feature = "std")]
use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    vec,
    vec::Vec,
};

use crate::bytecode::{ExternEffects, FunctionDef, Module};
use crate::instruction::{Instruction, Opcode};
use crate::instruction_effects::{
    instruction_frame_memory_effect, visit_instruction_register_reads,
    visit_instruction_register_writes, FrameMemoryEffect,
};
use crate::SlotType;

const NO_SUSPENDED_STATE: u32 = u32::MAX;
const MAX_FRAME_ROOT_DERIVED_BYTES: usize = 128 * 1024 * 1024;
const MAX_FRAME_ROOT_WORK_UNITS: usize = 256 * 1024 * 1024;

#[derive(Debug)]
struct BuildBudget {
    derived_bytes_left: usize,
    work_left: usize,
}

impl BuildBudget {
    const fn new() -> Self {
        Self {
            derived_bytes_left: MAX_FRAME_ROOT_DERIVED_BYTES,
            work_left: MAX_FRAME_ROOT_WORK_UNITS,
        }
    }

    fn charge_bytes(
        &mut self,
        function: usize,
        pc: usize,
        bytes: usize,
        detail: &'static str,
    ) -> Result<(), FrameRootMapBuildError> {
        self.derived_bytes_left = self
            .derived_bytes_left
            .checked_sub(bytes)
            .ok_or_else(|| FrameRootMapBuildError::resource(function, pc, detail))?;
        Ok(())
    }

    fn charge_work(
        &mut self,
        function: usize,
        pc: usize,
        units: usize,
        detail: &'static str,
    ) -> Result<(), FrameRootMapBuildError> {
        self.work_left = self
            .work_left
            .checked_sub(units)
            .ok_or_else(|| FrameRootMapBuildError::resource(function, pc, detail))?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FrameRootStateId(u32);

impl FrameRootStateId {
    /// The canonical state containing no interpreter-frame roots.
    pub const EMPTY: Self = Self(0);

    #[inline]
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy)]
struct RootSetSpan {
    start: u32,
    direct_count: u16,
    total_count: u16,
}

#[derive(Debug, Clone, Copy)]
pub struct FrameRootSet<'a> {
    pub direct: &'a [u16],
    /// Header slots of live two-word interfaces.
    pub conditional: &'a [u16],
}

#[derive(Debug)]
pub struct FunctionFrameRootMap {
    initialization: u32,
    before: Box<[u32]>,
    suspended: Box<[u32]>,
    sets: Box<[RootSetSpan]>,
    slots: Box<[u16]>,
}

impl FunctionFrameRootMap {
    /// Non-parameter root cells that require a valid zero value before a newly
    /// admitted interpreted frame can execute. `None` means the frame needs no
    /// initialization work.
    #[inline(always)]
    pub fn initialization_roots_to_clear(&self) -> Option<FrameRootSet<'_>> {
        (self.initialization != FrameRootStateId::EMPTY.0).then(|| {
            self.roots(FrameRootStateId(self.initialization))
                .expect("frame initialization root state is internally valid")
        })
    }

    #[inline]
    pub fn before(&self, pc: usize) -> Option<FrameRootStateId> {
        self.before.get(pc).copied().map(FrameRootStateId)
    }

    #[inline]
    pub fn suspended_call(&self, pc: usize) -> Option<FrameRootStateId> {
        let state = *self.suspended.get(pc)?;
        (state != NO_SUSPENDED_STATE).then_some(FrameRootStateId(state))
    }

    #[inline]
    pub fn roots(&self, state: FrameRootStateId) -> Option<FrameRootSet<'_>> {
        let span = *self.sets.get(state.index())?;
        let start = span.start as usize;
        let direct_end = start.checked_add(span.direct_count as usize)?;
        let end = start.checked_add(span.total_count as usize)?;
        Some(FrameRootSet {
            direct: self.slots.get(start..direct_end)?,
            conditional: self.slots.get(direct_end..end)?,
        })
    }
}

#[derive(Debug)]
pub struct FrameRootMaps {
    functions: Box<[FunctionFrameRootMap]>,
}

impl FrameRootMaps {
    pub(crate) fn build(module: &Module) -> Result<Self, FrameRootMapBuildError> {
        let mut budget = BuildBudget::new();
        budget.charge_bytes(
            0,
            0,
            module
                .functions
                .len()
                .saturating_mul(core::mem::size_of::<FunctionFrameRootMap>()),
            "function root map table",
        )?;
        let mut functions = Vec::new();
        functions
            .try_reserve_exact(module.functions.len())
            .map_err(|_| FrameRootMapBuildError::resource(0, 0, "function root maps"))?;
        for (func_id, func) in module.functions.iter().enumerate() {
            functions.push(build_function_map(module, func_id, func, &mut budget)?);
        }
        Ok(Self {
            functions: functions.into_boxed_slice(),
        })
    }

    #[cfg(feature = "test-support")]
    pub(crate) fn conservative(module: &Module) -> Self {
        let functions = module
            .functions
            .iter()
            .map(|func| {
                let encode_roots = |first_slot: usize| {
                    let mut encoded = Vec::new();
                    encoded.extend(func.slot_types.iter().enumerate().filter_map(|(slot, ty)| {
                        (slot >= first_slot && ty.is_managed_ref()).then_some(slot as u16)
                    }));
                    let direct_count = encoded.len() as u16;
                    encoded.extend(func.slot_types.iter().enumerate().filter_map(|(slot, ty)| {
                        (slot >= first_slot && *ty == SlotType::Interface0).then_some(slot as u16)
                    }));
                    (encoded, direct_count)
                };
                let (all_roots, all_direct_count) = encode_roots(0);
                let (initialization_roots, initialization_direct_count) =
                    encode_roots(usize::from(func.param_slots));
                let mut sets = vec![RootSetSpan {
                    start: 0,
                    direct_count: 0,
                    total_count: 0,
                }];
                let mut slots = Vec::new();
                let all_state = if all_roots.is_empty() {
                    0
                } else {
                    sets.push(RootSetSpan {
                        start: 0,
                        direct_count: all_direct_count,
                        total_count: all_roots.len() as u16,
                    });
                    slots.extend_from_slice(&all_roots);
                    1
                };
                let initialization = if initialization_roots.is_empty() {
                    0
                } else if initialization_roots == all_roots {
                    all_state
                } else {
                    let state = sets.len() as u32;
                    sets.push(RootSetSpan {
                        start: slots.len() as u32,
                        direct_count: initialization_direct_count,
                        total_count: initialization_roots.len() as u16,
                    });
                    slots.extend_from_slice(&initialization_roots);
                    state
                };
                FunctionFrameRootMap {
                    initialization,
                    before: vec![all_state; func.code.len()].into_boxed_slice(),
                    suspended: vec![all_state; func.code.len()].into_boxed_slice(),
                    sets: sets.into_boxed_slice(),
                    slots: slots.into_boxed_slice(),
                }
            })
            .collect::<Vec<_>>();
        Self {
            functions: functions.into_boxed_slice(),
        }
    }

    #[inline]
    pub fn function(&self, func_id: u32) -> Option<&FunctionFrameRootMap> {
        self.functions.get(func_id as usize)
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FrameRootMapBuildError {
    pub function: usize,
    pub pc: usize,
    pub detail: &'static str,
}

impl FrameRootMapBuildError {
    const fn semantic(function: usize, pc: usize, detail: &'static str) -> Self {
        Self {
            function,
            pc,
            detail,
        }
    }

    const fn resource(function: usize, pc: usize, detail: &'static str) -> Self {
        Self {
            function,
            pc,
            detail,
        }
    }
}

#[derive(Debug, Default)]
struct RootEffects {
    reads: BTreeSet<u16>,
    writes: BTreeSet<u16>,
}

#[derive(Debug, Default)]
struct Block {
    start: usize,
    end: usize,
    predecessors: Vec<usize>,
    successors: Vec<usize>,
    uses: BTreeSet<u16>,
    defs: BTreeSet<u16>,
    live_in: BTreeSet<u16>,
    live_out: BTreeSet<u16>,
}

fn build_function_map(
    module: &Module,
    func_id: usize,
    func: &FunctionDef,
    budget: &mut BuildBudget,
) -> Result<FunctionFrameRootMap, FrameRootMapBuildError> {
    budget.charge_bytes(
        func_id,
        0,
        func.code
            .len()
            .saturating_mul(core::mem::size_of::<RootEffects>()),
        "instruction root effects",
    )?;
    let mut effects = Vec::new();
    effects
        .try_reserve_exact(func.code.len())
        .map_err(|_| FrameRootMapBuildError::resource(func_id, 0, "root effects"))?;
    let logical_roots = logical_root_slots(func);

    for (pc, inst) in func.code.iter().copied().enumerate() {
        let metadata = func.instruction_metadata.get(pc);
        let mut effect = RootEffects::default();
        let mut range_error = None;
        visit_instruction_register_reads(&inst, metadata, &module.functions, |start, count| {
            if range_error.is_none() {
                range_error =
                    add_range_roots(func_id, pc, func, start, count, &mut effect.reads, budget)
                        .err();
            }
        })
        .map_err(|_| FrameRootMapBuildError::semantic(func_id, pc, "register read effects"))?;
        if let Some(error) = range_error.take() {
            return Err(error);
        }
        visit_instruction_register_writes(
            &inst,
            metadata,
            &module.externs,
            &module.functions,
            |start, count| {
                if range_error.is_none() {
                    range_error = add_range_roots(
                        func_id,
                        pc,
                        func,
                        start,
                        count,
                        &mut effect.writes,
                        budget,
                    )
                    .err();
                }
            },
        )
        .map_err(|_| FrameRootMapBuildError::semantic(func_id, pc, "register write effects"))?;
        if let Some(error) = range_error {
            return Err(error);
        }
        match instruction_frame_memory_effect(&inst, metadata)
            .map_err(|_| FrameRootMapBuildError::semantic(func_id, pc, "frame memory effects"))?
        {
            FrameMemoryEffect::None => {}
            FrameMemoryEffect::AliasedRange { start, count } => {
                budget.charge_work(
                    func_id,
                    pc,
                    logical_roots.len(),
                    "aliased frame root effects",
                )?;
                let end = start.saturating_add(count.saturating_sub(1));
                effect
                    .reads
                    .extend(logical_roots.iter().copied().filter(|&root| {
                        count != 0 && root <= end && root_cell_end(func, root) >= start
                    }));
            }
        }
        budget.charge_bytes(
            func_id,
            pc,
            effect
                .reads
                .len()
                .saturating_add(effect.writes.len())
                .saturating_mul(32),
            "root effect cells",
        )?;
        effects.push(effect);
    }

    if effects.is_empty() {
        let mut sets = Vec::new();
        let mut slots = Vec::new();
        let mut interned = BTreeMap::<Vec<u16>, u32>::new();
        let initialization = intern_root_set(
            func_id,
            0,
            func,
            &BTreeSet::new(),
            &mut interned,
            &mut sets,
            &mut slots,
            budget,
        )?;
        return Ok(FunctionFrameRootMap {
            initialization,
            before: Box::new([]),
            suspended: Box::new([]),
            sets: sets.into_boxed_slice(),
            slots: slots.into_boxed_slice(),
        });
    }

    let mut blocks = build_blocks(func_id, &func.code, budget)?;
    compute_liveness(func_id, &mut blocks, &effects, budget)?;
    // A reused stack cell needs its language zero value only when its old
    // contents can flow into the function from entry. Backward liveness has
    // already proved exactly that property: roots written on every path before
    // their first use do not appear in the entry block's live-in set.
    let initialization_roots = blocks[0]
        .live_in
        .iter()
        .copied()
        .filter(|slot| *slot >= func.param_slots)
        .collect::<BTreeSet<_>>();

    budget.charge_bytes(
        func_id,
        0,
        func.code
            .len()
            .saturating_mul(core::mem::size_of::<u32>() * 2),
        "per-instruction root states",
    )?;
    let mut before = vec![0_u32; func.code.len()];
    let mut suspended = vec![NO_SUSPENDED_STATE; func.code.len()];
    let mut sets = Vec::new();
    let mut slots = Vec::new();
    let mut interned = BTreeMap::<Vec<u16>, u32>::new();
    intern_root_set(
        func_id,
        0,
        func,
        &BTreeSet::new(),
        &mut interned,
        &mut sets,
        &mut slots,
        budget,
    )?;
    let initialization = intern_root_set(
        func_id,
        0,
        func,
        &initialization_roots,
        &mut interned,
        &mut sets,
        &mut slots,
        budget,
    )?;

    for block in &blocks {
        let mut live = block.live_out.clone();
        for pc in (block.start..block.end).rev() {
            if is_call(func.code[pc].opcode()) {
                let mut suspended_live = live
                    .difference(&effects[pc].writes)
                    .copied()
                    .collect::<BTreeSet<_>>();
                if call_replays_while_suspended(module, func.code[pc]) {
                    suspended_live.extend(effects[pc].reads.iter().copied());
                }
                suspended[pc] = intern_root_set(
                    func_id,
                    pc,
                    func,
                    &suspended_live,
                    &mut interned,
                    &mut sets,
                    &mut slots,
                    budget,
                )?;
            }
            for root in &effects[pc].writes {
                live.remove(root);
            }
            live.extend(effects[pc].reads.iter().copied());
            before[pc] = intern_root_set(
                func_id,
                pc,
                func,
                &live,
                &mut interned,
                &mut sets,
                &mut slots,
                budget,
            )?;
        }
    }

    Ok(FunctionFrameRootMap {
        initialization,
        before: before.into_boxed_slice(),
        suspended: suspended.into_boxed_slice(),
        sets: sets.into_boxed_slice(),
        slots: slots.into_boxed_slice(),
    })
}

fn logical_root_slots(func: &FunctionDef) -> Vec<u16> {
    func.slot_types
        .iter()
        .enumerate()
        .filter_map(|(slot, ty)| {
            matches!(
                ty,
                SlotType::GcBase | SlotType::GcRef | SlotType::Interface0
            )
            .then_some(slot as u16)
        })
        .collect()
}

fn root_slot_for_cell(func: &FunctionDef, slot: u16) -> Option<u16> {
    match func.slot_types.get(slot as usize).copied()? {
        SlotType::GcBase | SlotType::GcRef | SlotType::Interface0 => Some(slot),
        SlotType::Interface1 => slot
            .checked_sub(1)
            .filter(|&header| func.slot_types.get(header as usize) == Some(&SlotType::Interface0)),
        SlotType::Value | SlotType::Float => None,
    }
}

fn root_cell_end(func: &FunctionDef, root: u16) -> u16 {
    if func.slot_types.get(root as usize) == Some(&SlotType::Interface0) {
        root.saturating_add(1)
    } else {
        root
    }
}

fn add_range_roots(
    func_id: usize,
    pc: usize,
    func: &FunctionDef,
    start: u16,
    count: u16,
    roots: &mut BTreeSet<u16>,
    budget: &mut BuildBudget,
) -> Result<(), FrameRootMapBuildError> {
    budget.charge_work(func_id, pc, usize::from(count), "register root range")?;
    for offset in 0..count {
        if let Some(root) = start
            .checked_add(offset)
            .and_then(|slot| root_slot_for_cell(func, slot))
        {
            roots.insert(root);
        }
    }
    Ok(())
}

fn intern_root_set(
    func_id: usize,
    pc: usize,
    func: &FunctionDef,
    live: &BTreeSet<u16>,
    interned: &mut BTreeMap<Vec<u16>, u32>,
    sets: &mut Vec<RootSetSpan>,
    slots: &mut Vec<u16>,
    budget: &mut BuildBudget,
) -> Result<u32, FrameRootMapBuildError> {
    budget.charge_work(func_id, pc, live.len(), "root-set interning")?;
    let mut key = Vec::new();
    key.try_reserve_exact(live.len())
        .map_err(|_| FrameRootMapBuildError::resource(func_id, pc, "root set"))?;
    key.extend(live.iter().copied().filter(|&slot| {
        func.slot_types
            .get(slot as usize)
            .is_some_and(SlotType::is_managed_ref)
    }));
    let direct_count = u16::try_from(key.len())
        .map_err(|_| FrameRootMapBuildError::resource(func_id, pc, "direct roots"))?;
    key.extend(
        live.iter()
            .copied()
            .filter(|&slot| func.slot_types.get(slot as usize) == Some(&SlotType::Interface0)),
    );
    if let Some(&state) = interned.get(&key) {
        return Ok(state);
    }
    budget.charge_bytes(
        func_id,
        pc,
        core::mem::size_of::<RootSetSpan>()
            .saturating_add(key.len().saturating_mul(core::mem::size_of::<u16>() * 2)),
        "interned frame roots",
    )?;
    let total_count = u16::try_from(key.len())
        .map_err(|_| FrameRootMapBuildError::resource(func_id, pc, "frame roots"))?;
    let state = u32::try_from(sets.len())
        .map_err(|_| FrameRootMapBuildError::resource(func_id, pc, "root states"))?;
    let start = u32::try_from(slots.len())
        .map_err(|_| FrameRootMapBuildError::resource(func_id, pc, "root slots"))?;
    slots
        .try_reserve_exact(key.len())
        .map_err(|_| FrameRootMapBuildError::resource(func_id, pc, "root slots"))?;
    slots.extend_from_slice(&key);
    sets.push(RootSetSpan {
        start,
        direct_count,
        total_count,
    });
    interned.insert(key, state);
    Ok(state)
}

fn build_blocks(
    func_id: usize,
    code: &[Instruction],
    budget: &mut BuildBudget,
) -> Result<Vec<Block>, FrameRootMapBuildError> {
    budget.charge_work(func_id, 0, code.len(), "root control-flow leaders")?;
    let mut leaders = BTreeSet::from([0_usize]);
    for (pc, inst) in code.iter().copied().enumerate() {
        if matches!(
            inst.opcode(),
            Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot | Opcode::ForLoop
        ) {
            for successor in instruction_successors(func_id, pc, inst, code.len())? {
                leaders.insert(successor);
            }
            if pc + 1 < code.len() {
                leaders.insert(pc + 1);
            }
        } else if matches!(inst.opcode(), Opcode::Return | Opcode::Panic) && pc + 1 < code.len() {
            leaders.insert(pc + 1);
        }
    }

    let leaders = leaders.into_iter().collect::<Vec<_>>();
    budget.charge_bytes(
        func_id,
        0,
        leaders
            .len()
            .saturating_mul(core::mem::size_of::<Block>())
            .saturating_add(code.len().saturating_mul(core::mem::size_of::<usize>())),
        "root control-flow graph",
    )?;
    let mut blocks = Vec::new();
    let mut pc_to_block = vec![0_usize; code.len()];
    for (index, &start) in leaders.iter().enumerate() {
        let end = leaders.get(index + 1).copied().unwrap_or(code.len());
        pc_to_block[start..end].fill(index);
        blocks.push(Block {
            start,
            end,
            ..Block::default()
        });
    }
    for block in &mut blocks {
        let last = block.end - 1;
        let mut successors = instruction_successors(func_id, last, code[last], code.len())?
            .into_iter()
            .map(|pc| pc_to_block[pc])
            .collect::<Vec<_>>();
        successors.sort_unstable();
        successors.dedup();
        block.successors = successors;
    }
    for index in 0..blocks.len() {
        for successor in blocks[index].successors.clone() {
            blocks[successor].predecessors.push(index);
        }
    }
    Ok(blocks)
}

fn instruction_successors(
    func_id: usize,
    pc: usize,
    inst: Instruction,
    code_len: usize,
) -> Result<Vec<usize>, FrameRootMapBuildError> {
    let fallthrough = || (pc + 1 < code_len).then_some(pc + 1);
    let target = |raw: i64| {
        usize::try_from(raw)
            .ok()
            .filter(|&target| target < code_len)
            .ok_or_else(|| FrameRootMapBuildError::semantic(func_id, pc, "branch target"))
    };
    match inst.opcode() {
        Opcode::Jump => Ok(vec![target(pc as i64 + i64::from(inst.imm32()))?]),
        Opcode::JumpIf | Opcode::JumpIfNot => {
            let mut successors = fallthrough().into_iter().collect::<Vec<_>>();
            successors.push(target(pc as i64 + i64::from(inst.imm32()))?);
            Ok(successors)
        }
        Opcode::ForLoop => {
            let mut successors = fallthrough().into_iter().collect::<Vec<_>>();
            successors.push(target(pc as i64 + 1 + i64::from(inst.c as i16))?);
            Ok(successors)
        }
        Opcode::Return | Opcode::Panic => Ok(Vec::new()),
        _ => Ok(fallthrough().into_iter().collect()),
    }
}

fn compute_liveness(
    func_id: usize,
    blocks: &mut [Block],
    effects: &[RootEffects],
    budget: &mut BuildBudget,
) -> Result<(), FrameRootMapBuildError> {
    for block in blocks.iter_mut() {
        for effect in &effects[block.start..block.end] {
            budget.charge_work(
                func_id,
                block.start,
                effect.reads.len().saturating_add(effect.writes.len()),
                "local root liveness",
            )?;
            block
                .uses
                .extend(effect.reads.difference(&block.defs).copied());
            block.defs.extend(effect.writes.iter().copied());
        }
        block.live_in = block.uses.clone();
    }

    let mut pending = (0..blocks.len()).rev().collect::<VecDeque<_>>();
    let mut queued = vec![true; blocks.len()];
    while let Some(index) = pending.pop_front() {
        queued[index] = false;
        let mut live_out = BTreeSet::new();
        for &successor in &blocks[index].successors {
            budget.charge_work(
                func_id,
                blocks[index].start,
                blocks[successor].live_in.len(),
                "global root liveness",
            )?;
            live_out.extend(blocks[successor].live_in.iter().copied());
        }
        let mut live_in = blocks[index].uses.clone();
        live_in.extend(live_out.difference(&blocks[index].defs).copied());
        if live_in != blocks[index].live_in || live_out != blocks[index].live_out {
            blocks[index].live_in = live_in;
            blocks[index].live_out = live_out;
            for &predecessor in &blocks[index].predecessors {
                if !queued[predecessor] {
                    queued[predecessor] = true;
                    pending.push_back(predecessor);
                }
            }
        }
    }
    let retained_cells = blocks.iter().fold(0usize, |total, block| {
        total
            .saturating_add(block.uses.len())
            .saturating_add(block.defs.len())
            .saturating_add(block.live_in.len())
            .saturating_add(block.live_out.len())
    });
    budget.charge_bytes(
        func_id,
        0,
        retained_cells.saturating_mul(32),
        "root liveness sets",
    )?;
    Ok(())
}

#[inline]
const fn is_call(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::Call | Opcode::CallExtern | Opcode::CallClosure | Opcode::CallIface
    )
}

#[inline]
fn call_replays_while_suspended(module: &Module, instruction: Instruction) -> bool {
    if instruction.opcode() != Opcode::CallExtern {
        return false;
    }
    module
        .externs
        .get(instruction.b as usize)
        .is_some_and(|extern_def| {
            extern_def
                .allowed_effects
                .intersects(ExternEffects::MAY_CALL_CLOSURE_REPLAY | ExternEffects::UNKNOWN_CONTROL)
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::InstructionMetadata;

    fn function(
        name: &str,
        code: Vec<Instruction>,
        slot_types: Vec<SlotType>,
        param_slots: u16,
        ret_slots: u16,
    ) -> FunctionDef {
        let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&code);
        FunctionDef {
            name: name.to_string(),
            param_count: param_slots,
            param_slots,
            local_slots: slot_types.len() as u16,
            ret_slots,
            ret_slot_types: slot_types[..ret_slots as usize].to_vec(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls,
            has_call_extern,
            instruction_metadata: vec![InstructionMetadata::None; code.len()],
            code,
            slot_types,
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    #[test]
    fn maps_only_roots_live_before_each_instruction() {
        let mut module = Module::new("frame-roots".to_string());
        module.functions.push(function(
            "main",
            vec![
                Instruction::new(Opcode::Copy, 1, 0, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
            ],
            vec![
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::Interface0,
                SlotType::Interface1,
            ],
            1,
            1,
        ));

        let maps = FrameRootMaps::build(&module).expect("build exact roots");
        let function = maps.function(0).unwrap();
        let first = function.roots(function.before(0).unwrap()).unwrap();
        let ret = function.roots(function.before(1).unwrap()).unwrap();
        assert_eq!(first.direct, &[0]);
        assert!(first.conditional.is_empty());
        assert_eq!(ret.direct, &[1]);
        assert!(ret.conditional.is_empty());
    }

    #[test]
    fn initialization_roots_contain_only_non_parameter_cells() {
        let mut module = Module::new("frame-initialization-roots".to_string());
        module.functions.push(function(
            "with-roots",
            vec![
                Instruction::new(Opcode::CopyN, 6, 4, 2),
                Instruction::new(Opcode::Return, 3, 1, 0),
            ],
            vec![
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::GcRef,
                SlotType::Interface0,
                SlotType::Interface1,
                SlotType::Interface0,
                SlotType::Interface1,
            ],
            1,
            1,
        ));
        module.functions.push(function(
            "scalar-only",
            vec![Instruction::new(Opcode::Return, 0, 0, 0)],
            vec![SlotType::Value, SlotType::Float],
            1,
            0,
        ));

        let maps = FrameRootMaps::build(&module).expect("valid frame root maps");
        let roots = maps
            .function(0)
            .expect("first function root map")
            .initialization_roots_to_clear()
            .expect("root-shaped locals require initialization");
        assert_eq!(roots.direct, &[3]);
        assert_eq!(roots.conditional, &[4]);
        assert!(maps
            .function(1)
            .expect("second function root map")
            .initialization_roots_to_clear()
            .is_none());
    }

    #[test]
    fn initialization_skips_roots_written_before_every_use() {
        let mut module = Module::new("frame-initialization-definite-write".to_string());
        module.functions.push(function(
            "copy-before-use",
            vec![
                Instruction::new(Opcode::Copy, 1, 0, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
            ],
            vec![SlotType::GcRef, SlotType::GcRef],
            1,
            1,
        ));

        let maps = FrameRootMaps::build(&module).expect("valid frame root maps");
        assert!(maps
            .function(0)
            .expect("function root map")
            .initialization_roots_to_clear()
            .is_none());
    }

    #[cfg(feature = "test-support")]
    #[test]
    fn conservative_maps_keep_parameter_roots_in_scan_states() {
        let mut module = Module::new("conservative-frame-roots".to_string());
        module.functions.push(function(
            "f",
            vec![Instruction::new(Opcode::Return, 0, 0, 0)],
            vec![SlotType::GcRef, SlotType::Value, SlotType::GcRef],
            2,
            0,
        ));

        let maps = FrameRootMaps::conservative(&module);
        let map = maps.function(0).expect("function root map");
        let before = map
            .roots(map.before(0).expect("instruction root state"))
            .expect("valid instruction root state");
        assert_eq!(before.direct, &[0, 2]);
        let initialization = map
            .initialization_roots_to_clear()
            .expect("local root requires initialization");
        assert_eq!(initialization.direct, &[2]);
    }

    #[test]
    fn suspended_call_state_drops_consumed_arguments_and_outputs() {
        let mut module = Module::new("suspended-call-roots".to_string());
        module.functions.push(function(
            "caller",
            vec![
                Instruction::with_flags(Opcode::Call, 0, 1, 1, 0),
                Instruction::new(Opcode::Return, 0, 1, 0),
            ],
            vec![SlotType::GcRef; 3],
            2,
            1,
        ));
        module.functions.push(function(
            "callee",
            vec![Instruction::new(Opcode::Return, 0, 1, 0)],
            vec![SlotType::GcRef],
            1,
            1,
        ));

        let maps = FrameRootMaps::build(&module).expect("build call roots");
        let caller = maps.function(0).unwrap();
        let before = caller.roots(caller.before(0).unwrap()).unwrap();
        let suspended = caller.roots(caller.suspended_call(0).unwrap()).unwrap();
        assert_eq!(before.direct, &[0, 1]);
        assert_eq!(suspended.direct, &[0]);
    }

    #[test]
    fn closure_replay_suspension_keeps_current_extern_inputs_rooted() {
        let mut module = Module::new("closure-replay-suspended-roots".to_string());
        module.externs.push(crate::bytecode::ExternDef {
            name: "replay".to_string(),
            params: crate::bytecode::ParamShape::Exact { slots: 1 },
            returns: crate::bytecode::ReturnShape::with_slot_types(vec![SlotType::GcRef]),
            allowed_effects: ExternEffects::MAY_CALL_CLOSURE_REPLAY,
            param_kinds: Vec::new(),
        });
        let mut caller = function(
            "caller",
            vec![
                Instruction::new(Opcode::CallExtern, 0, 0, 2),
                Instruction::new(Opcode::Return, 0, 1, 0),
            ],
            vec![SlotType::GcRef, SlotType::Value, SlotType::GcRef],
            3,
            1,
        );
        caller.instruction_metadata[0] = InstructionMetadata::CallExternLayout {
            arg_layout: vec![SlotType::GcRef],
            ret_layout: vec![SlotType::GcRef],
        };
        module.functions.push(caller);

        let maps = FrameRootMaps::build(&module).expect("build closure replay roots");
        let caller = maps.function(0).unwrap();
        let suspended = caller.roots(caller.suspended_call(0).unwrap()).unwrap();

        assert_eq!(suspended.direct, &[2]);
    }

    #[test]
    fn suspended_call_roots_bound_future_dynamic_stack_accesses() {
        let mut module = Module::new("bounded-slot-roots".to_string());
        let mut caller = function(
            "caller",
            vec![
                Instruction::with_flags(Opcode::Call, 0, 1, 0, 0),
                Instruction::new(Opcode::SlotGet, 6, 4, 2),
                Instruction::new(Opcode::StrNew, 8, 0, 0),
                Instruction::new(Opcode::Return, 8, 1, 0),
            ],
            vec![
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::GcRef,
            ],
            1,
            1,
        );
        caller.instruction_metadata[1] = InstructionMetadata::SlotLayout {
            array_len: 2,
            elem_layout: vec![SlotType::GcRef],
        };
        module.functions.push(caller);
        module.functions.push(function(
            "callee",
            vec![Instruction::new(Opcode::Return, 0, 0, 0)],
            vec![SlotType::GcRef],
            1,
            0,
        ));

        let maps = FrameRootMaps::build(&module).expect("build bounded dynamic roots");
        let caller = maps.function(0).unwrap();
        let suspended = caller.roots(caller.suspended_call(0).unwrap()).unwrap();

        assert_eq!(suspended.direct, &[4, 5]);
        assert!(!suspended.direct.contains(&8));
    }

    #[test]
    fn select_loop_keeps_only_declared_transaction_roots_live() {
        let mut module = Module::new("select-loop-roots".to_string());
        let jump = (-4_i32) as u32;
        let code = vec![
            Instruction::new(Opcode::Copy, 5, 0, 0),
            Instruction::new(Opcode::Hint, 0, 0, 0),
            Instruction::new(Opcode::SelectBegin, 1, 0, 0),
            Instruction::new(Opcode::SelectSend, 1, 2, 0),
            Instruction::new(Opcode::SelectExec, 4, 0, 0),
            Instruction::new(Opcode::Jump, 0, jump as u16, (jump >> 16) as u16),
        ];
        let mut func = function(
            "select_loop",
            code,
            vec![
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::GcRef,
            ],
            3,
            0,
        );
        func.instruction_metadata = vec![
            InstructionMetadata::None,
            InstructionMetadata::None,
            InstructionMetadata::None,
            InstructionMetadata::QueueLayout {
                elem_layout: vec![SlotType::GcRef],
            },
            InstructionMetadata::SelectExecLayout {
                cases: vec![crate::bytecode::SelectCaseLayout::Send {
                    queue: 1,
                    value: 2,
                    elem_slots: 1,
                }],
            },
            InstructionMetadata::None,
        ];
        module.functions.push(func);

        let maps = FrameRootMaps::build(&module).expect("build select roots");
        let function = maps.function(0).unwrap();
        let before_exec = function.roots(function.before(4).unwrap()).unwrap();
        let loop_back = function.roots(function.before(5).unwrap()).unwrap();

        assert_eq!(before_exec.direct, &[1, 2]);
        assert_eq!(loop_back.direct, &[1, 2]);
        assert!(!loop_back.direct.contains(&0));
        assert!(!loop_back.direct.contains(&3));
        assert!(!loop_back.direct.contains(&5));
    }
}
