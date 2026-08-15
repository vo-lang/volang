//! Conservative escape analysis for field-level scalar replacement.

use std::collections::BTreeSet;

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

        let state_cells = ir
            .blocks()
            .len()
            .saturating_mul(usize::from(function.local_slots));
        let retained_state_bytes = state_cells.saturating_mul(
            core::mem::size_of::<Option<u32>>() + core::mem::size_of::<BTreeSet<u32>>(),
        );
        if retained_state_bytes > crate::MAX_JIT_COMPILE_WORK_BYTES {
            return Self::default();
        }

        let Some(block_states) = analyze_alias_states(function, ir, &allocation_ids) else {
            return Self::default();
        };
        let mut invalid = vec![false; candidates.len()];
        let mut accesses = Vec::new();
        for block in ir.blocks().iter().filter(|block| block.reachable) {
            let Some(mut state) = block_states[block.id.index()].clone() else {
                continue;
            };
            for pc in block.start_pc as usize..block.end_pc as usize {
                validate_alias_uses(ir, pc, &state, &mut invalid, &mut accesses);
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
        }
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
    possible: Vec<BTreeSet<u32>>,
}

impl AliasState {
    fn empty(slots: usize) -> Self {
        Self {
            definite: vec![None; slots],
            possible: vec![BTreeSet::new(); slots],
        }
    }

    fn merge(&mut self, other: &Self) {
        for slot in 0..self.definite.len() {
            if self.definite[slot] != other.definite[slot] {
                self.definite[slot] = None;
            }
            self.possible[slot].extend(other.possible[slot].iter().copied());
        }
    }

    fn clear(&mut self, slot: u16) {
        let slot = usize::from(slot);
        self.definite[slot] = None;
        self.possible[slot].clear();
    }

    fn copy_slot(&mut self, destination: u16, source: u16) {
        let definite = self.definite[usize::from(source)];
        let possible = self.possible[usize::from(source)].clone();
        let destination = usize::from(destination);
        self.definite[destination] = definite;
        self.possible[destination] = possible;
    }

    fn define_object(&mut self, slot: u16, object: u32) {
        let slot = usize::from(slot);
        self.definite[slot] = Some(object);
        self.possible[slot] = BTreeSet::from([object]);
    }
}

fn analyze_alias_states(
    function: &FunctionDef,
    ir: &FunctionIr,
    allocation_ids: &[Option<u32>],
) -> Option<Vec<Option<AliasState>>> {
    let entry = ir.blocks().first()?.id;
    let mut incoming = vec![Vec::<crate::ir::BlockId>::new(); ir.blocks().len()];
    for block in ir
        .blocks()
        .iter()
        .filter(|block| ir.is_executable_block(block.id))
    {
        for edge in ir.executable_successors(block.id) {
            incoming[edge.target.index()].push(block.id);
        }
    }

    let empty = AliasState::empty(usize::from(function.local_slots));
    let mut block_states = vec![None; ir.blocks().len()];
    block_states[entry.index()] = Some(empty.clone());
    let max_iterations = ir.blocks().len().saturating_mul(4).max(8);
    for _ in 0..max_iterations {
        let mut block_out = vec![None; ir.blocks().len()];
        for block in ir
            .blocks()
            .iter()
            .filter(|block| ir.is_executable_block(block.id))
        {
            let Some(mut state) = block_states[block.id.index()].clone() else {
                continue;
            };
            for pc in block.start_pc as usize..block.end_pc as usize {
                transfer_alias_state(ir, pc, allocation_ids, &mut state);
            }
            block_out[block.id.index()] = Some(state);
        }

        let mut next = vec![None; ir.blocks().len()];
        next[entry.index()] = Some(empty.clone());
        for block in ir
            .blocks()
            .iter()
            .filter(|block| block.id != entry && ir.is_executable_block(block.id))
        {
            let mut merged = None::<AliasState>;
            for predecessor in &incoming[block.id.index()] {
                let Some(candidate) = block_out[predecessor.index()].as_ref() else {
                    continue;
                };
                match &mut merged {
                    None => merged = Some(candidate.clone()),
                    Some(current) => current.merge(candidate),
                }
            }
            next[block.id.index()] = merged;
        }
        if next == block_states {
            return Some(next);
        }
        block_states = next;
    }
    None
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
            let copies = (0..instruction.copy_n_count())
                .map(|offset| {
                    (
                        state.definite[usize::from(instruction.b + offset)],
                        state.possible[usize::from(instruction.b + offset)].clone(),
                    )
                })
                .collect::<Vec<_>>();
            for (offset, (definite, possible)) in copies.into_iter().enumerate() {
                let destination = usize::from(instruction.a) + offset;
                state.definite[destination] = definite;
                state.possible[destination] = possible;
            }
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

fn invalidate_possible(objects: &BTreeSet<u32>, invalid: &mut [bool]) {
    for &object in objects {
        invalid[object as usize] = true;
    }
}

fn validate_alias_uses(
    ir: &FunctionIr,
    pc: usize,
    state: &AliasState,
    invalid: &mut [bool],
    accesses: &mut Vec<(u32, u32)>,
) {
    let typed = *ir
        .instruction(pc)
        .expect("escape validation traverses a verified IR instruction");
    let instruction = typed.source();
    let input_slots = ir
        .inputs(typed)
        .iter()
        .map(|value| ir.value(*value).slot)
        .collect::<Vec<_>>();
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
