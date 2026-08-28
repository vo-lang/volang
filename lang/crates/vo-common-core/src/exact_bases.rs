//! Verified exact-allocation-base provenance shared by execution backends.
//!
//! Managed language pointers may name an object interior, while collector
//! headers and the cheapest write barrier require an allocation base. This
//! analysis proves the narrower property over verified bytecode. Static-call
//! return summaries are solved to the greatest fixed point, which preserves
//! recursive factories until a concrete return path disproves exactness.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, collections::VecDeque, vec, vec::Vec};
#[cfg(feature = "std")]
use std::collections::VecDeque;

use crate::bytecode::{FunctionDef, InstructionMetadata, Module};
use crate::instruction::{Instruction, Opcode};
use crate::instruction_effects::visit_instruction_register_writes;
use crate::SlotType;

const MAX_TRANSIENT_EXACT_BASE_BYTES: usize = 128 * 1024 * 1024;
const MAX_EXACT_BASE_WORK_UNITS: usize = 256 * 1024 * 1024;

const EXACT_PARENT: u8 = 1 << 0;
const EXACT_CHILD: u8 = 1 << 1;

/// Proven base properties for one managed pointer store.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(transparent)]
pub struct WriteBarrierBaseProvenance(u8);

impl WriteBarrierBaseProvenance {
    pub const UNKNOWN: Self = Self(0);

    #[inline]
    pub const fn parent_is_exact(self) -> bool {
        self.0 & EXACT_PARENT != 0
    }

    #[inline]
    pub const fn child_is_exact(self) -> bool {
        self.0 & EXACT_CHILD != 0
    }

    #[inline]
    pub const fn both_are_exact(self) -> bool {
        self.0 == EXACT_PARENT | EXACT_CHILD
    }
}

#[derive(Debug)]
pub struct FunctionExactBaseMap {
    write_barriers: Box<[WriteBarrierBaseProvenance]>,
}

impl FunctionExactBaseMap {
    #[inline]
    pub fn write_barrier(&self, pc: usize) -> WriteBarrierBaseProvenance {
        self.write_barriers
            .get(pc)
            .copied()
            .unwrap_or(WriteBarrierBaseProvenance::UNKNOWN)
    }
}

/// Immutable exact-base facts bound to one verified module image.
#[derive(Debug)]
pub struct ExactBaseMaps {
    functions: Box<[FunctionExactBaseMap]>,
    exact_base_returns: Box<[Box<[bool]>]>,
}

struct ExactBaseWorkBudget {
    left: usize,
}

impl ExactBaseWorkBudget {
    const fn new() -> Self {
        Self {
            left: MAX_EXACT_BASE_WORK_UNITS,
        }
    }

    fn charge(
        &mut self,
        function: usize,
        pc: usize,
        units: usize,
    ) -> Result<(), ExactBaseMapBuildError> {
        self.left = self
            .left
            .checked_sub(units)
            .ok_or_else(|| ExactBaseMapBuildError::resource(function, pc, "analysis work"))?;
        Ok(())
    }
}

impl ExactBaseMaps {
    pub(crate) fn build(module: &Module) -> Result<Self, ExactBaseMapBuildError> {
        let mut budget = ExactBaseWorkBudget::new();
        let mut exact_base_returns = module
            .functions
            .iter()
            .map(|function| {
                function
                    .ret_slot_types
                    .iter()
                    .map(SlotType::is_managed_ref)
                    .collect::<Vec<_>>()
                    .into_boxed_slice()
            })
            .collect::<Vec<_>>();

        let mut callers = vec![Vec::<usize>::new(); module.functions.len()];
        for (caller, function) in module.functions.iter().enumerate() {
            for instruction in &function.code {
                if instruction.opcode() != Opcode::Call {
                    continue;
                }
                let callee = instruction.static_call_func_id() as usize;
                let Some(callee_callers) = callers.get_mut(callee) else {
                    return Err(ExactBaseMapBuildError::semantic(
                        caller,
                        0,
                        "static call target",
                    ));
                };
                callee_callers.push(caller);
            }
        }
        for callee_callers in &mut callers {
            callee_callers.sort_unstable();
            callee_callers.dedup();
        }

        // Exactness is a must property. Start every managed return at the
        // lattice top and only remove claims. Recursive forwarding therefore
        // stays exact unless a reachable concrete path disproves it.
        let mut queued = vec![true; module.functions.len()];
        let mut worklist = (0..module.functions.len()).collect::<Vec<_>>();
        while let Some(function_id) = worklist.pop() {
            queued[function_id] = false;
            let derived =
                analyze_function(module, function_id, &exact_base_returns, false, &mut budget)?;
            let summary = &mut exact_base_returns[function_id];
            let mut changed = false;
            for (known, proven) in summary.iter_mut().zip(derived.returns.iter().copied()) {
                if *known && !proven {
                    *known = false;
                    changed = true;
                }
            }
            if !changed {
                continue;
            }
            for &caller in &callers[function_id] {
                if !queued[caller] {
                    queued[caller] = true;
                    worklist.push(caller);
                }
            }
        }

        let mut functions = Vec::new();
        functions
            .try_reserve_exact(module.functions.len())
            .map_err(|_| ExactBaseMapBuildError::resource(0, 0, "function maps"))?;
        for function_id in 0..module.functions.len() {
            let analysis =
                analyze_function(module, function_id, &exact_base_returns, true, &mut budget)?;
            functions.push(FunctionExactBaseMap {
                write_barriers: analysis.write_barriers,
            });
        }
        Ok(Self {
            functions: functions.into_boxed_slice(),
            exact_base_returns: exact_base_returns.into_boxed_slice(),
        })
    }

    pub(crate) fn conservative(module: &Module) -> Self {
        Self {
            functions: module
                .functions
                .iter()
                .map(|function| FunctionExactBaseMap {
                    write_barriers: vec![WriteBarrierBaseProvenance::UNKNOWN; function.code.len()]
                        .into_boxed_slice(),
                })
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            exact_base_returns: module
                .functions
                .iter()
                .map(|function| vec![false; usize::from(function.ret_slots)].into_boxed_slice())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        }
    }

    #[inline]
    pub fn function(&self, function_id: u32) -> Option<&FunctionExactBaseMap> {
        self.functions.get(function_id as usize)
    }

    #[inline]
    pub fn exact_base_returns(&self) -> &[Box<[bool]>] {
        &self.exact_base_returns
    }
}

#[derive(Debug)]
pub(crate) struct ExactBaseMapBuildError {
    pub function: usize,
    pub pc: usize,
    pub detail: &'static str,
    pub resource_limit: bool,
}

impl ExactBaseMapBuildError {
    const fn semantic(function: usize, pc: usize, detail: &'static str) -> Self {
        Self {
            function,
            pc,
            detail,
            resource_limit: false,
        }
    }

    const fn resource(function: usize, pc: usize, detail: &'static str) -> Self {
        Self {
            function,
            pc,
            detail,
            resource_limit: true,
        }
    }
}

struct FunctionAnalysis {
    returns: Box<[bool]>,
    write_barriers: Box<[WriteBarrierBaseProvenance]>,
}

fn analyze_function(
    module: &Module,
    function_id: usize,
    exact_base_returns: &[Box<[bool]>],
    record_barriers: bool,
    budget: &mut ExactBaseWorkBudget,
) -> Result<FunctionAnalysis, ExactBaseMapBuildError> {
    let function = module
        .functions
        .get(function_id)
        .ok_or_else(|| ExactBaseMapBuildError::semantic(function_id, 0, "function definition"))?;
    let code_len = function.code.len();
    let slots = usize::from(function.local_slots);
    let state_bytes = code_len
        .checked_mul(slots)
        .ok_or_else(|| ExactBaseMapBuildError::resource(function_id, 0, "instruction states"))?;
    if state_bytes > MAX_TRANSIENT_EXACT_BASE_BYTES {
        return Err(ExactBaseMapBuildError::resource(
            function_id,
            0,
            "instruction states",
        ));
    }

    let mut states = Vec::<Option<Box<[bool]>>>::new();
    states
        .try_reserve_exact(code_len)
        .map_err(|_| ExactBaseMapBuildError::resource(function_id, 0, "instruction states"))?;
    states.resize_with(code_len, || None);
    let entry = function
        .slot_types
        .iter()
        .map(SlotType::is_exact_gc_base)
        .collect::<Vec<_>>()
        .into_boxed_slice();
    if let Some(first) = states.first_mut() {
        *first = Some(entry);
    }
    let mut queued = vec![false; code_len];
    let mut worklist = VecDeque::new();
    if code_len != 0 {
        queued[0] = true;
        worklist.push_back(0);
    }

    while let Some(pc) = worklist.pop_front() {
        queued[pc] = false;
        let Some(before) = states[pc].as_deref() else {
            continue;
        };
        let successors = instruction_successors(function_id, pc, function.code[pc], code_len)?;
        budget.charge(
            function_id,
            pc,
            1_usize.saturating_add(slots.saturating_mul(2_usize.saturating_add(successors.len()))),
        )?;
        let mut after = before.to_vec();
        apply_transfer(module, function_id, pc, &mut after, exact_base_returns)?;
        for successor in successors {
            let changed = match states[successor].as_mut() {
                None => {
                    states[successor] = Some(after.clone().into_boxed_slice());
                    true
                }
                Some(existing) => {
                    let mut changed = false;
                    for (current, incoming) in existing.iter_mut().zip(after.iter().copied()) {
                        let merged = *current && incoming;
                        changed |= merged != *current;
                        *current = merged;
                    }
                    changed
                }
            };
            if changed && !queued[successor] {
                queued[successor] = true;
                worklist.push_back(successor);
            }
        }
    }

    let mut returns = function
        .ret_slot_types
        .iter()
        .map(SlotType::is_managed_ref)
        .collect::<Vec<_>>();
    let mut saw_return = false;
    let mut write_barriers = vec![WriteBarrierBaseProvenance::UNKNOWN; code_len];
    for (pc, instruction) in function.code.iter().copied().enumerate() {
        let Some(state) = states[pc].as_deref() else {
            continue;
        };
        if instruction.opcode() == Opcode::Return {
            budget.charge(function_id, pc, returns.len())?;
            saw_return = true;
            for (offset, exact) in returns.iter_mut().enumerate() {
                if !*exact {
                    continue;
                }
                *exact = instruction
                    .a
                    .checked_add(offset as u16)
                    .and_then(|slot| state.get(slot as usize))
                    .copied()
                    .unwrap_or(false);
            }
        }
        if record_barriers && instruction.opcode() == Opcode::PtrSet {
            let managed_child = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::ptr_value_layout)
                .and_then(|layout| layout.first())
                .is_some_and(|slot| matches!(slot, SlotType::GcBase | SlotType::GcRef));
            if managed_child {
                let parent_exact = state.get(instruction.a as usize).copied().unwrap_or(false);
                let child_exact = state.get(instruction.c as usize).copied().unwrap_or(false);
                write_barriers[pc] = WriteBarrierBaseProvenance(
                    (u8::from(parent_exact) * EXACT_PARENT) | (u8::from(child_exact) * EXACT_CHILD),
                );
            }
        }
    }
    if !saw_return {
        returns.fill(false);
    }
    Ok(FunctionAnalysis {
        returns: returns.into_boxed_slice(),
        write_barriers: write_barriers.into_boxed_slice(),
    })
}

fn apply_transfer(
    module: &Module,
    function_id: usize,
    pc: usize,
    state: &mut [bool],
    exact_base_returns: &[Box<[bool]>],
) -> Result<(), ExactBaseMapBuildError> {
    let function = &module.functions[function_id];
    let instruction = function.code[pc];
    let metadata = function.instruction_metadata.get(pc);
    let aliases = match instruction.opcode() {
        Opcode::Copy => vec![(instruction.a, exact_at(state, instruction.b))],
        Opcode::CopyN => (0..instruction.copy_n_count())
            .filter_map(|offset| {
                Some((
                    instruction.a.checked_add(offset)?,
                    exact_at(state, instruction.b.checked_add(offset)?),
                ))
            })
            .collect(),
        _ => Vec::new(),
    };

    visit_instruction_register_writes(
        &instruction,
        metadata,
        &module.externs,
        &module.functions,
        |start, count| {
            for offset in 0..count {
                let slot = usize::from(start + offset);
                if let Some(exact) = state.get_mut(slot) {
                    *exact = function
                        .slot_types
                        .get(slot)
                        .is_some_and(SlotType::is_exact_gc_base);
                }
            }
        },
    )
    .map_err(|_| ExactBaseMapBuildError::semantic(function_id, pc, "register writes"))?;

    for (destination, exact) in aliases {
        set_exact_if_managed(function, state, destination, exact);
    }
    if instruction.opcode() == Opcode::Call {
        let callee_id = instruction.static_call_func_id() as usize;
        let callee = module.functions.get(callee_id).ok_or_else(|| {
            ExactBaseMapBuildError::semantic(function_id, pc, "static call target")
        })?;
        let first_return = instruction
            .b
            .checked_add(callee.param_slots)
            .ok_or_else(|| {
                ExactBaseMapBuildError::semantic(function_id, pc, "static return window")
            })?;
        for (offset, exact) in exact_base_returns
            .get(callee_id)
            .into_iter()
            .flatten()
            .copied()
            .enumerate()
        {
            let destination = first_return.checked_add(offset as u16).ok_or_else(|| {
                ExactBaseMapBuildError::semantic(function_id, pc, "static return window")
            })?;
            set_exact_if_managed(function, state, destination, exact);
        }
    }

    match instruction.opcode() {
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
        | Opcode::LoadConst => set_exact_if_managed(function, state, instruction.a, true),
        Opcode::LoadInt if instruction.imm32() == 0 => {
            set_exact_if_managed(function, state, instruction.a, true);
        }
        _ => {}
    }
    Ok(())
}

#[inline]
fn exact_at(state: &[bool], slot: u16) -> bool {
    state.get(slot as usize).copied().unwrap_or(false)
}

#[inline]
fn set_exact_if_managed(function: &FunctionDef, state: &mut [bool], slot: u16, exact: bool) {
    let slot = slot as usize;
    if function
        .slot_types
        .get(slot)
        .is_some_and(SlotType::is_managed_ref)
    {
        if let Some(target) = state.get_mut(slot) {
            *target = exact;
        }
    }
}

fn instruction_successors(
    function_id: usize,
    pc: usize,
    instruction: Instruction,
    code_len: usize,
) -> Result<Vec<usize>, ExactBaseMapBuildError> {
    let fallthrough = || (pc + 1 < code_len).then_some(pc + 1);
    let target = |raw: i64| {
        usize::try_from(raw)
            .ok()
            .filter(|&target| target < code_len)
            .ok_or_else(|| ExactBaseMapBuildError::semantic(function_id, pc, "branch target"))
    };
    match instruction.opcode() {
        Opcode::Jump => Ok(vec![target(pc as i64 + i64::from(instruction.imm32()))?]),
        Opcode::JumpIf | Opcode::JumpIfNot => {
            let mut successors = fallthrough().into_iter().collect::<Vec<_>>();
            successors.push(target(pc as i64 + i64::from(instruction.imm32()))?);
            Ok(successors)
        }
        Opcode::ForLoop => {
            let mut successors = fallthrough().into_iter().collect::<Vec<_>>();
            successors.push(target(pc as i64 + 1 + i64::from(instruction.c as i16))?);
            Ok(successors)
        }
        Opcode::Return | Opcode::Panic => Ok(Vec::new()),
        _ => Ok(fallthrough().into_iter().collect()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::{InstructionMetadata, TransferType};

    fn branch(opcode: Opcode, condition: u16, offset: i32) -> Instruction {
        Instruction::with_flags(
            opcode,
            0,
            condition,
            offset as u32 as u16,
            (offset as u32 >> 16) as u16,
        )
    }

    fn recursive_factory() -> FunctionDef {
        let code = vec![
            Instruction::new(Opcode::PtrNew, 2, 3, 0),
            Instruction::with_flags(Opcode::JumpIf, 0, 0, 4, 0),
            Instruction::new(Opcode::Call, 0, 0, 0),
            Instruction::new(Opcode::PtrSet, 2, 0, 1),
            Instruction::new(Opcode::Return, 2, 1, 0),
            Instruction::new(Opcode::Return, 2, 1, 0),
        ];
        FunctionDef {
            name: "recursive_factory".into(),
            param_count: 1,
            param_slots: 1,
            local_slots: 4,
            ret_slots: 1,
            ret_slot_types: vec![SlotType::GcRef],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: true,
            has_call_extern: false,
            code,
            instruction_metadata: vec![
                InstructionMetadata::PtrLayout {
                    value_layout: vec![SlotType::GcRef],
                },
                InstructionMetadata::None,
                InstructionMetadata::None,
                InstructionMetadata::PtrLayout {
                    value_layout: vec![SlotType::GcRef],
                },
                InstructionMetadata::None,
                InstructionMetadata::None,
            ],
            slot_types: vec![
                SlotType::Value,
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::Value,
            ],
            capture_types: Vec::<TransferType>::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    #[test]
    fn recursive_factory_returns_and_stores_exact_bases() {
        let mut module = Module::new("exact-recursive-factory".into());
        module.functions.push(recursive_factory());
        let maps = ExactBaseMaps::build(&module).expect("exact-base analysis");

        assert_eq!(maps.exact_base_returns()[0].as_ref(), [true]);
        let barrier = maps.function(0).unwrap().write_barrier(3);
        assert!(barrier.both_are_exact());
    }

    #[test]
    fn interior_return_disproves_recursive_and_forwarded_exactness() {
        let mut recursive = recursive_factory();
        recursive
            .code
            .insert(5, Instruction::new(Opcode::PtrAdd, 2, 2, 1));
        recursive
            .instruction_metadata
            .insert(5, InstructionMetadata::None);
        recursive.code[1] = branch(Opcode::JumpIf, 0, 4);

        let forwarding = FunctionDef {
            name: "forwarding_factory".into(),
            param_count: 1,
            param_slots: 1,
            local_slots: 2,
            ret_slots: 1,
            ret_slot_types: vec![SlotType::GcRef],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: true,
            has_call_extern: false,
            code: vec![
                Instruction::new(Opcode::Call, 0, 0, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
            ],
            instruction_metadata: vec![InstructionMetadata::None; 2],
            slot_types: vec![SlotType::Value, SlotType::GcRef],
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        };

        let mut module = Module::new("interior-return".into());
        module.functions = vec![recursive, forwarding];
        let maps = ExactBaseMaps::build(&module).expect("exact-base analysis");

        assert_eq!(maps.exact_base_returns()[0].as_ref(), [false]);
        assert_eq!(maps.exact_base_returns()[1].as_ref(), [false]);
    }
}
