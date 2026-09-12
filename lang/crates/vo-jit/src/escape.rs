//! Conservative escape analysis for field-level scalar replacement.

use std::collections::VecDeque;

// Scalar replacement is optional. A compact candidate universe gives every
// alias operation a fixed cost and makes its temporary memory pre-admittable.
const MAX_SCALAR_CANDIDATES: usize = u64::BITS as usize;
const MAX_ESCAPE_WORK: usize = 8 * 1024 * 1024;

use vo_runtime::bytecode::{FunctionDef, InstructionMetadata};
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

use crate::ir::FunctionIr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ScalarReplacement {
    pub pc: u32,
    pub slots: u16,
    pub object: u32,
}

#[derive(Debug, Default)]
pub(crate) struct EscapePlan {
    replacements: Box<[ScalarReplacement]>,
    accesses: Box<[(u32, u32)]>,
    live_ranges: Box<[Box<[std::ops::Range<u32>]>]>,
}

impl EscapePlan {
    pub(crate) fn analyze(function: &FunctionDef, ir: &FunctionIr) -> Self {
        let mut candidates = Vec::new();
        let mut allocation_ids = vec![None; function.code.len()];
        for block in ir.blocks().iter().filter(|block| block.reachable) {
            for (pc, allocation_id) in allocation_ids
                .iter_mut()
                .enumerate()
                .take(block.end_pc as usize)
                .skip(block.start_pc as usize)
            {
                let instruction = function.code[pc];
                if instruction.opcode() != Opcode::PtrNew {
                    continue;
                }
                let Some(layout) = scalar_ptr_layout(function, pc) else {
                    continue;
                };
                let Ok(slots) = u16::try_from(layout.len()) else {
                    continue;
                };
                if slots == 0 {
                    continue;
                }
                if candidates.len() == MAX_SCALAR_CANDIDATES {
                    return Self::default();
                }
                let object = candidates.len() as u32;
                *allocation_id = Some(object);
                candidates.push(ScalarReplacement {
                    pc: pc as u32,
                    slots,
                    object,
                });
            }
        }
        if candidates.is_empty() {
            return Self::default();
        }

        // Two persistent states per block, one transfer scratch state, and
        // CopyN scratch. Include candidate liveness output in the admission.
        let state_cells = ir
            .blocks()
            .len()
            .saturating_mul(2)
            .saturating_add(2)
            .saturating_mul(usize::from(function.local_slots));
        let temporary_bytes = state_cells
            .saturating_mul(core::mem::size_of::<Option<u32>>() + core::mem::size_of::<u64>())
            .saturating_add(
                function.code.len().saturating_mul(
                    candidates
                        .len()
                        .saturating_mul(core::mem::size_of::<std::ops::Range<u32>>())
                        + 32,
                ),
            )
            .saturating_add(ir.blocks().len().saturating_mul(256));
        if temporary_bytes > crate::MAX_JIT_COMPILE_WORK_BYTES / 4 {
            return Self::default();
        }

        let Some(block_states) = analyze_alias_states(function, ir, &allocation_ids) else {
            return Self::default();
        };
        let mut invalid = vec![false; candidates.len()];
        let mut accesses = Vec::new();
        let mut live_ranges = vec![Vec::<std::ops::Range<u32>>::new(); candidates.len()];
        let mut remaining_alias_work = MAX_ESCAPE_WORK;
        for block in ir.blocks().iter().filter(|block| block.reachable) {
            let Some(mut state) = block_states[block.id.index()].clone() else {
                continue;
            };
            for pc in block.start_pc as usize..block.end_pc as usize {
                // Recovery and GC use the same bytecode liveness. An object's
                // private native pointer must never outlive its rooted aliases.
                let mut live_objects = 0u64;
                for value in ir.resume_values(pc).unwrap_or_default() {
                    live_objects |= state.possible[usize::from(value.slot)];
                }
                for object in candidate_ids(live_objects) {
                    let ranges = &mut live_ranges[object as usize];
                    if let Some(last) = ranges.last_mut().filter(|last| last.end == pc as u32) {
                        last.end += 1;
                    } else {
                        ranges.push(pc as u32..pc as u32 + 1);
                    }
                    // One scalar record represents one dynamic allocation. A
                    // loop may revisit this site while an earlier instance is
                    // still live; such overlapping instances cannot share it.
                    if allocation_ids[pc] == Some(object) {
                        invalid[object as usize] = true;
                    }
                }
                if !validate_alias_uses(
                    ir,
                    pc,
                    &state,
                    &mut invalid,
                    &mut accesses,
                    &mut remaining_alias_work,
                ) {
                    return Self::default();
                }
                transfer_alias_state(ir, pc, &allocation_ids, &mut state);
            }
        }

        let mut old_to_new = vec![None; candidates.len()];
        let mut next_object = 0_u32;
        let replacements = candidates
            .into_iter()
            .filter_map(|mut replacement| {
                if invalid[replacement.object as usize] {
                    return None;
                }
                let object = next_object;
                next_object += 1;
                old_to_new[replacement.object as usize] = Some(object);
                replacement.object = object;
                Some(replacement)
            })
            .collect::<Vec<_>>();
        let accesses = accesses
            .into_iter()
            .filter_map(|(pc, old_object)| Some((pc, old_to_new[old_object as usize]?)))
            .collect::<Vec<_>>();
        Self {
            replacements: replacements.into_boxed_slice(),
            accesses: accesses.into_boxed_slice(),
            live_ranges: live_ranges
                .into_iter()
                .enumerate()
                .filter(|(object, _)| old_to_new[*object].is_some())
                .map(|(_, ranges)| ranges.into_boxed_slice())
                .collect(),
        }
    }

    pub(crate) fn into_live_ranges(self) -> Box<[Box<[std::ops::Range<u32>]>]> {
        self.live_ranges
    }

    #[inline]
    pub(crate) fn replacement(&self, pc: usize) -> Option<ScalarReplacement> {
        self.replacements
            .binary_search_by_key(&(pc as u32), |replacement| replacement.pc)
            .ok()
            .map(|index| self.replacements[index])
    }

    #[inline]
    pub(crate) fn access(&self, pc: usize) -> Option<u32> {
        self.accesses
            .binary_search_by_key(&(pc as u32), |(access_pc, _)| *access_pc)
            .ok()
            .map(|index| self.accesses[index].1)
    }

    #[cfg(test)]
    fn replacements(&self) -> &[ScalarReplacement] {
        &self.replacements
    }
}

fn scalar_ptr_layout(function: &FunctionDef, pc: usize) -> Option<&[SlotType]> {
    match function.instruction_metadata.get(pc)? {
        InstructionMetadata::PtrLayout { value_layout }
            if value_layout
                .iter()
                .all(|ty| matches!(ty, SlotType::Value | SlotType::Float)) =>
        {
            Some(value_layout)
        }
        _ => None,
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct AliasState {
    definite: Vec<Option<u32>>,
    possible: Vec<u64>,
}

impl AliasState {
    fn empty(slots: usize) -> Self {
        Self {
            definite: vec![None; slots],
            possible: vec![0; slots],
        }
    }

    fn merge(&mut self, other: &Self) -> bool {
        let mut changed = false;
        for slot in 0..self.definite.len() {
            if self.definite[slot] != other.definite[slot] {
                changed |= self.definite[slot].is_some();
                self.definite[slot] = None;
            }
            let possible = self.possible[slot] | other.possible[slot];
            changed |= possible != self.possible[slot];
            self.possible[slot] = possible;
        }
        changed
    }

    fn clear(&mut self, slot: u16) {
        let slot = usize::from(slot);
        self.definite[slot] = None;
        self.possible[slot] = 0;
    }

    fn copy_slot(&mut self, destination: u16, source: u16) {
        let definite = self.definite[usize::from(source)];
        let possible = self.possible[usize::from(source)];
        let destination = usize::from(destination);
        self.definite[destination] = definite;
        self.possible[destination] = possible;
    }

    fn define_object(&mut self, slot: u16, object: u32) {
        let slot = usize::from(slot);
        self.definite[slot] = Some(object);
        self.possible[slot] = 1u64 << object;
    }
}

fn analyze_alias_states(
    function: &FunctionDef,
    ir: &FunctionIr,
    allocation_ids: &[Option<u32>],
) -> Option<Vec<Option<AliasState>>> {
    let entry = ir.blocks().first()?.id;
    let mut block_states = vec![None; ir.blocks().len()];
    let mut block_out = vec![None; ir.blocks().len()];
    block_states[entry.index()] = Some(AliasState::empty(usize::from(function.local_slots)));
    let mut queue = VecDeque::from([entry]);
    let mut queued = vec![false; ir.blocks().len()];
    queued[entry.index()] = true;
    let mut remaining_work = MAX_ESCAPE_WORK;
    while let Some(id) = queue.pop_front() {
        queued[id.index()] = false;
        let block = &ir.blocks()[id.index()];
        let successor_count = ir.executable_successors(id).count();
        let cost = usize::from(function.local_slots)
            .saturating_mul(successor_count.saturating_add(2))
            .saturating_add((block.end_pc - block.start_pc) as usize);
        remaining_work = remaining_work.checked_sub(cost)?;
        let mut state = block_states[id.index()].as_ref()?.clone();
        for pc in block.start_pc as usize..block.end_pc as usize {
            transfer_alias_state(ir, pc, allocation_ids, &mut state);
        }
        if block_out[id.index()].as_ref() == Some(&state) {
            continue;
        }
        for edge in ir.executable_successors(id) {
            if edge.target == entry {
                continue;
            }
            let input = &mut block_states[edge.target.index()];
            let changed = match input {
                None => {
                    *input = Some(state.clone());
                    true
                }
                Some(previous) => previous.merge(&state),
            };
            if changed && !queued[edge.target.index()] {
                queued[edge.target.index()] = true;
                queue.push_back(edge.target);
            }
        }
        block_out[id.index()] = Some(state);
    }
    Some(block_states)
}

fn transfer_alias_state(
    ir: &FunctionIr,
    pc: usize,
    allocation_ids: &[Option<u32>],
    state: &mut AliasState,
) {
    let typed = *ir
        .instruction(pc)
        .expect("escape analysis traverses a verified IR instruction");
    let instruction = typed.source();
    match instruction.opcode() {
        Opcode::Copy => state.copy_slot(instruction.a, instruction.b),
        Opcode::CopyN => {
            let source = usize::from(instruction.b);
            let end = source + usize::from(instruction.copy_n_count());
            let destination = usize::from(instruction.a);
            state.definite.copy_within(source..end, destination);
            state.possible.copy_within(source..end, destination);
        }
        _ => {
            for &output in ir.outputs(typed) {
                state.clear(ir.value(output).slot);
            }
            if let Some(object) = allocation_ids[pc] {
                state.define_object(instruction.a, object);
            }
        }
    }
}

fn candidate_ids(mut mask: u64) -> impl Iterator<Item = u32> {
    std::iter::from_fn(move || {
        if mask == 0 {
            return None;
        }
        let object = mask.trailing_zeros();
        mask &= mask - 1;
        Some(object)
    })
}

fn invalidate_possible(objects: &u64, invalid: &mut [bool]) {
    for object in candidate_ids(*objects) {
        invalid[object as usize] = true;
    }
}

fn validate_alias_uses(
    ir: &FunctionIr,
    pc: usize,
    state: &AliasState,
    invalid: &mut [bool],
    accesses: &mut Vec<(u32, u32)>,
    remaining_alias_work: &mut usize,
) -> bool {
    let typed = *ir
        .instruction(pc)
        .expect("escape validation traverses a verified IR instruction");
    let instruction = typed.source();
    // Indexed frame accesses expose references without scalar SSA operands.
    // A pointer loaded through this range can escape the private field state;
    // a store can also replace an alias without an ordinary register write.
    // Retain canonical object fields for every candidate visible in the range.
    if let crate::effects::MemorySyncEffect::AliasedRange { start, count } = typed.memory_sync() {
        let Some(remaining) = remaining_alias_work.checked_sub(usize::from(count)) else {
            return false;
        };
        *remaining_alias_work = remaining;
        let start = usize::from(start);
        for possible in &state.possible[start..start + usize::from(count)] {
            invalidate_possible(possible, invalid);
        }
    }
    let input_slots = ir.inputs(typed).iter().map(|value| ir.value(*value).slot);
    match instruction.opcode() {
        Opcode::Copy | Opcode::CopyN => {}
        Opcode::PtrGet | Opcode::PtrGetN => {
            let pointer = usize::from(instruction.b);
            if let Some(object) = state.definite[pointer] {
                accesses.push((pc as u32, object));
            } else {
                invalidate_possible(&state.possible[pointer], invalid);
            }
            for slot in input_slots {
                if slot != instruction.b {
                    invalidate_possible(&state.possible[usize::from(slot)], invalid);
                }
            }
        }
        Opcode::PtrSet | Opcode::PtrSetN => {
            let pointer = usize::from(instruction.a);
            if let Some(object) = state.definite[pointer] {
                accesses.push((pc as u32, object));
            } else {
                invalidate_possible(&state.possible[pointer], invalid);
            }
            for slot in input_slots {
                if slot != instruction.a {
                    invalidate_possible(&state.possible[usize::from(slot)], invalid);
                }
            }
        }
        _ => {
            for slot in input_slots {
                invalidate_possible(&state.possible[usize::from(slot)], invalid);
            }
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixtures::function_with_slot_types_and_sig;
    use vo_runtime::bytecode::{Constant, Module};
    use vo_runtime::instruction::Instruction;

    fn scalar_object_function(return_pointer: bool) -> (Module, FunctionDef) {
        let mut function = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::LoadConst, 0, 0, 0),
                Instruction::new(Opcode::PtrNew, 1, 0, 0),
                Instruction::new(Opcode::LoadInt, 2, 42, 0),
                Instruction::new(Opcode::PtrSet, 1, 0, 2),
                Instruction::new(Opcode::PtrGet, 3, 1, 0),
                Instruction::new(Opcode::Return, if return_pointer { 1 } else { 3 }, 1, 0),
            ],
            vec![
                SlotType::Value,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
            ],
            0,
            0,
            1,
        );
        for pc in [1, 3, 4] {
            function.instruction_metadata[pc] = InstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::Value],
            };
        }
        function.ret_slot_types = vec![if return_pointer {
            SlotType::GcRef
        } else {
            SlotType::Value
        }];
        let mut module = Module::new("escape-analysis".into());
        module.constants.push(Constant::Int(
            vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int64).to_raw() as i64,
        ));
        module.functions.push(function.clone());
        (module, function)
    }

    #[test]
    fn scalar_replacement_requires_a_non_escaping_use_chain() {
        let (module, local) = scalar_object_function(false);
        let local_ir = FunctionIr::build(&local, &module).expect("local object IR");
        assert_eq!(
            EscapePlan::analyze(&local, &local_ir).replacements(),
            &[ScalarReplacement {
                pc: 1,
                slots: 1,
                object: 0,
            }]
        );

        let (module, escaping) = scalar_object_function(true);
        let escaping_ir = FunctionIr::build(&escaping, &module).expect("escaping object IR");
        assert!(EscapePlan::analyze(&escaping, &escaping_ir)
            .replacements()
            .is_empty());
    }

    #[test]
    fn scalar_replacement_rejects_array_alias_reads_and_overwrites() {
        for opcode in [
            Opcode::SlotGet,
            Opcode::SlotGetN,
            Opcode::SlotSet,
            Opcode::SlotSetN,
        ] {
            let (mut module, mut function) = scalar_object_function(false);
            function.local_slots = 8;
            function.slot_types.extend([
                SlotType::GcRef,
                SlotType::Value,
                SlotType::GcRef,
                SlotType::Value,
            ]);
            function.code.splice(
                4..4,
                [
                    Instruction::new(Opcode::Copy, 4, 1, 0),
                    Instruction::new(Opcode::LoadInt, 5, 0, 0),
                    if matches!(opcode, Opcode::SlotGet | Opcode::SlotGetN) {
                        Instruction::new(opcode, 6, 4, 5)
                    } else {
                        Instruction::new(opcode, 4, 5, 1)
                    },
                ],
            );
            function.instruction_metadata.splice(
                4..4,
                [
                    InstructionMetadata::None,
                    InstructionMetadata::None,
                    InstructionMetadata::SlotLayout {
                        array_len: 1,
                        elem_layout: vec![SlotType::GcRef],
                    },
                ],
            );
            module.functions[0] = function.clone();
            let ir = FunctionIr::build(&function, &module).unwrap();
            assert!(
                EscapePlan::analyze(&function, &ir)
                    .replacements()
                    .is_empty(),
                "{opcode:?}"
            );
        }
    }

    #[test]
    fn scalar_replacement_tracks_one_object_across_a_cfg_merge() {
        let mut function = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::LoadConst, 1, 0, 0),
                Instruction::new(Opcode::PtrNew, 2, 1, 0),
                Instruction::with_flags(Opcode::JumpIf, 0, 0, 3, 0),
                Instruction::new(Opcode::LoadInt, 3, 41, 0),
                Instruction::with_flags(Opcode::Jump, 0, 0, 2, 0),
                Instruction::new(Opcode::LoadInt, 3, 42, 0),
                Instruction::new(Opcode::PtrSet, 2, 0, 3),
                Instruction::new(Opcode::PtrGet, 4, 2, 0),
                Instruction::new(Opcode::Return, 4, 1, 0),
            ],
            vec![
                SlotType::Value,
                SlotType::Value,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
            ],
            1,
            1,
            1,
        );
        for pc in [1, 6, 7] {
            function.instruction_metadata[pc] = InstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::Value],
            };
        }
        let mut module = Module::new("escape-analysis-cfg".into());
        module.constants.push(Constant::Int(
            vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int64).to_raw() as i64,
        ));
        module.functions.push(function.clone());
        let ir = FunctionIr::build(&function, &module).expect("cross-block object IR");
        let plan = EscapePlan::analyze(&function, &ir);

        assert_eq!(
            plan.replacements(),
            &[ScalarReplacement {
                pc: 1,
                slots: 1,
                object: 0,
            }]
        );
        assert_eq!(plan.access(6), Some(0));
        assert_eq!(plan.access(7), Some(0));
    }
}
