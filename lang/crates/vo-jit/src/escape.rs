//! Conservative escape analysis for native-frame scalar replacement.

use std::collections::BTreeSet;

use vo_runtime::bytecode::{FunctionDef, InstructionMetadata};
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

use crate::ir::FunctionIr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct StackAllocation {
    pub pc: u32,
    pub slots: u16,
}

#[derive(Debug, Default)]
pub(crate) struct EscapePlan {
    allocations: Box<[StackAllocation]>,
}

impl EscapePlan {
    pub(crate) fn analyze(function: &FunctionDef, ir: &FunctionIr) -> Self {
        let mut allocations = Vec::new();
        for block in ir.blocks().iter().filter(|block| block.reachable) {
            for pc in block.start_pc as usize..block.end_pc as usize {
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
                if allocation_is_local(ir, block.id, pc, instruction.a) {
                    allocations.push(StackAllocation {
                        pc: pc as u32,
                        slots,
                    });
                }
            }
        }
        Self {
            allocations: allocations.into_boxed_slice(),
        }
    }

    #[inline]
    pub(crate) fn allocation(&self, pc: usize) -> Option<StackAllocation> {
        self.allocations
            .binary_search_by_key(&(pc as u32), |allocation| allocation.pc)
            .ok()
            .map(|index| self.allocations[index])
    }

    #[inline]
    pub(crate) fn retained_bytes(&self) -> usize {
        self.allocations.len() * core::mem::size_of::<StackAllocation>()
    }

    #[cfg(test)]
    fn allocations(&self) -> &[StackAllocation] {
        &self.allocations
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

fn allocation_is_local(
    ir: &FunctionIr,
    block: crate::ir::BlockId,
    allocation_pc: usize,
    destination: u16,
) -> bool {
    let block_record = &ir.blocks()[block.index()];
    let mut aliases = BTreeSet::from([destination]);

    for pc in allocation_pc + 1..block_record.end_pc as usize {
        if pc % crate::compile_common::EXECUTION_BUDGET_REGION_INSTRUCTIONS == 0 {
            return false;
        }
        let typed = *ir
            .instruction(pc)
            .expect("escape analysis traverses a verified IR block");
        let instruction = typed.source();
        let input_slots = ir
            .inputs(typed)
            .iter()
            .map(|value| ir.value(*value).slot)
            .collect::<Vec<_>>();
        let output_slots = ir
            .outputs(typed)
            .iter()
            .map(|value| ir.value(*value).slot)
            .collect::<Vec<_>>();

        match instruction.opcode() {
            Opcode::Copy => {
                let source_is_alias = aliases.contains(&instruction.b);
                aliases.remove(&instruction.a);
                if source_is_alias {
                    aliases.insert(instruction.a);
                }
            }
            Opcode::CopyN => {
                let copies = (0..instruction.copy_n_count())
                    .map(|offset| {
                        (
                            instruction.a + offset,
                            aliases.contains(&(instruction.b + offset)),
                        )
                    })
                    .collect::<Vec<_>>();
                for (destination, _) in &copies {
                    aliases.remove(destination);
                }
                for (destination, source_is_alias) in copies {
                    if source_is_alias {
                        aliases.insert(destination);
                    }
                }
            }
            Opcode::PtrGet | Opcode::PtrGetN if aliases.contains(&instruction.b) => {
                if input_slots
                    .iter()
                    .any(|slot| *slot != instruction.b && aliases.contains(slot))
                {
                    return false;
                }
                for output in output_slots {
                    aliases.remove(&output);
                }
            }
            Opcode::PtrSet | Opcode::PtrSetN if aliases.contains(&instruction.a) => {
                if input_slots
                    .iter()
                    .any(|slot| *slot != instruction.a && aliases.contains(slot))
                {
                    return false;
                }
            }
            _ => {
                if input_slots.iter().any(|slot| aliases.contains(slot)) {
                    return false;
                }
                if typed.requires_frame_state() && !aliases.is_empty() {
                    return false;
                }
                for output in output_slots {
                    aliases.remove(&output);
                }
            }
        }

        if aliases.is_empty() {
            return true;
        }
    }

    !ir.any_slot_live_out(block, &aliases)
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
            EscapePlan::analyze(&local, &local_ir).allocations(),
            &[StackAllocation { pc: 1, slots: 1 }]
        );

        let (module, escaping) = scalar_object_function(true);
        let escaping_ir = FunctionIr::build(&escaping, &module).expect("escaping object IR");
        assert!(EscapePlan::analyze(&escaping, &escaping_ir)
            .allocations()
            .is_empty());
    }
}
