//! Immutable allocation-shape facts for optimizing pointer construction.
//!
//! A freshly allocated object has an exact, verifier-owned slot layout and is
//! non-null.  Within the same scheduling region, before the object is exposed
//! to an unknown operation, pointer loads can omit their nil branch and stores
//! can use the fresh-parent write barrier.  The analysis deliberately stops at
//! control-flow and frame-state boundaries so lowering consumes a proof rather
//! than reconstructing bytecode aliases while emitting native code.

use std::collections::BTreeMap;

use vo_runtime::bytecode::{FunctionDef, InstructionMetadata};
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

use crate::ir::FunctionIr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct FreshShapeAccess {
    pub allocation_pc: u32,
}

#[derive(Debug, Default)]
pub(crate) struct ShapePlan {
    fresh_accesses: Box<[(u32, FreshShapeAccess)]>,
}

impl ShapePlan {
    pub(crate) fn analyze(function: &FunctionDef, ir: &FunctionIr) -> Self {
        let mut fresh_accesses = Vec::new();

        for block in ir.blocks().iter().filter(|block| block.reachable) {
            let mut aliases = BTreeMap::<u16, u32>::new();

            for pc in block.start_pc as usize..block.end_pc as usize {
                if pc != block.start_pc as usize
                    && pc % crate::compile_common::EXECUTION_BUDGET_REGION_INSTRUCTIONS == 0
                {
                    aliases.clear();
                }

                let typed = *ir
                    .instruction(pc)
                    .expect("shape analysis traverses a verified IR block");
                let instruction = typed.source();
                let opcode = instruction.opcode();

                match opcode {
                    Opcode::PtrNew => {
                        // Allocation may poll and run an incremental GC slice.
                        // Only the object produced after that boundary starts a
                        // new fresh window.
                        aliases.clear();
                        if allocation_layout(function, pc).is_some() {
                            aliases.insert(instruction.a, pc as u32);
                        }
                    }
                    Opcode::Copy => {
                        let source = aliases.get(&instruction.b).copied();
                        aliases.remove(&instruction.a);
                        if let Some(allocation_pc) = source {
                            aliases.insert(instruction.a, allocation_pc);
                        }
                    }
                    Opcode::CopyN => {
                        let copies = (0..instruction.copy_n_count())
                            .map(|offset| {
                                (
                                    instruction.a + offset,
                                    aliases.get(&(instruction.b + offset)).copied(),
                                )
                            })
                            .collect::<Vec<_>>();
                        for (destination, _) in &copies {
                            aliases.remove(destination);
                        }
                        for (destination, allocation_pc) in copies {
                            if let Some(allocation_pc) = allocation_pc {
                                aliases.insert(destination, allocation_pc);
                            }
                        }
                    }
                    Opcode::PtrGet | Opcode::PtrGetN => {
                        let allocation_pc = aliases.get(&instruction.b).copied();
                        if let Some(allocation_pc) = allocation_pc.filter(|allocation_pc| {
                            access_matches_allocation(
                                function,
                                *allocation_pc as usize,
                                pc,
                                instruction.c,
                            )
                        }) {
                            fresh_accesses.push((pc as u32, FreshShapeAccess { allocation_pc }));
                        }
                        remove_outputs(ir, typed, &mut aliases);
                    }
                    Opcode::PtrSet | Opcode::PtrSetN => {
                        let allocation_pc = aliases.get(&instruction.a).copied();
                        if let Some(allocation_pc) = allocation_pc.filter(|allocation_pc| {
                            access_matches_allocation(
                                function,
                                *allocation_pc as usize,
                                pc,
                                instruction.b,
                            )
                        }) {
                            fresh_accesses.push((pc as u32, FreshShapeAccess { allocation_pc }));
                        }
                    }
                    _ => {
                        let consumes_fresh = ir
                            .inputs(typed)
                            .iter()
                            .any(|value| aliases.contains_key(&ir.value(*value).slot));
                        remove_outputs(ir, typed, &mut aliases);
                        if consumes_fresh || typed.requires_frame_state() {
                            aliases.clear();
                        }
                    }
                }
            }
        }

        Self {
            fresh_accesses: fresh_accesses.into_boxed_slice(),
        }
    }

    #[inline]
    pub(crate) fn fresh_access(&self, pc: usize) -> Option<FreshShapeAccess> {
        self.fresh_accesses
            .binary_search_by_key(&(pc as u32), |(access_pc, _)| *access_pc)
            .ok()
            .map(|index| self.fresh_accesses[index].1)
    }

    #[cfg(test)]
    fn accesses(&self) -> &[(u32, FreshShapeAccess)] {
        &self.fresh_accesses
    }
}

fn remove_outputs(
    ir: &FunctionIr,
    instruction: crate::ir::TypedInstruction,
    aliases: &mut BTreeMap<u16, u32>,
) {
    for output in ir.outputs(instruction) {
        aliases.remove(&ir.value(*output).slot);
    }
}

fn allocation_layout(function: &FunctionDef, pc: usize) -> Option<&[SlotType]> {
    match function.instruction_metadata.get(pc)? {
        InstructionMetadata::PtrLayout { value_layout } => Some(value_layout),
        _ => None,
    }
}

fn access_matches_allocation(
    function: &FunctionDef,
    allocation_pc: usize,
    access_pc: usize,
    offset: u16,
) -> bool {
    let Some(allocation) = allocation_layout(function, allocation_pc) else {
        return false;
    };
    let Some(access) = allocation_layout(function, access_pc) else {
        return false;
    };
    let start = usize::from(offset);
    allocation
        .get(start..start.saturating_add(access.len()))
        .is_some_and(|fields| fields == access)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixtures::function_with_slot_types_and_sig;
    use vo_runtime::bytecode::{Constant, Module};
    use vo_runtime::instruction::Instruction;
    use vo_runtime::{ValueKind, ValueMeta};

    fn shape_function(with_call_boundary: bool, mismatched_access: bool) -> (Module, FunctionDef) {
        let mut code = vec![
            Instruction::new(Opcode::LoadConst, 0, 0, 0),
            Instruction::new(Opcode::PtrNew, 1, 0, 0),
            Instruction::new(Opcode::Copy, 2, 1, 0),
        ];
        if with_call_boundary {
            code.push(Instruction::new(Opcode::Call, 1, 0, 0));
        }
        code.extend([
            Instruction::new(Opcode::PtrSet, 2, 0, 3),
            Instruction::new(Opcode::PtrGet, 4, 2, 0),
            Instruction::new(Opcode::Return, 2, 1, 0),
        ]);
        let mut function = function_with_slot_types_and_sig(
            code,
            vec![
                SlotType::Value,
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::GcRef,
            ],
            0,
            0,
            1,
        );
        function.ret_slot_types = vec![SlotType::GcRef];
        function.instruction_metadata[1] = InstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::GcRef],
        };
        let access_layout = if mismatched_access {
            SlotType::Value
        } else {
            SlotType::GcRef
        };
        let set_pc = 3 + usize::from(with_call_boundary);
        function.instruction_metadata[set_pc] = InstructionMetadata::PtrLayout {
            value_layout: vec![access_layout],
        };
        function.instruction_metadata[set_pc + 1] = InstructionMetadata::PtrLayout {
            value_layout: vec![access_layout],
        };

        let mut module = Module::new("shape-analysis".into());
        module.constants.push(Constant::Int(
            ValueMeta::new(0, ValueKind::Int64).to_raw() as i64
        ));
        if with_call_boundary {
            module.functions.push(function_with_slot_types_and_sig(
                vec![Instruction::new(Opcode::Return, 0, 0, 0)],
                vec![],
                0,
                0,
                0,
            ));
        }
        module.functions.push(function.clone());
        (module, function)
    }

    #[test]
    fn exact_fresh_shape_survives_copy_for_pointer_construction() {
        let (module, function) = shape_function(false, false);
        let ir = FunctionIr::build(&function, &module).expect("shape IR");
        let plan = ShapePlan::analyze(&function, &ir);
        assert_eq!(
            plan.accesses(),
            &[
                (3, FreshShapeAccess { allocation_pc: 1 }),
                (4, FreshShapeAccess { allocation_pc: 1 }),
            ]
        );
    }

    #[test]
    fn call_boundary_and_layout_drift_end_fresh_specialization() {
        let (module, function) = shape_function(true, false);
        let ir = FunctionIr::build(&function, &module).expect("call-boundary IR");
        assert!(ShapePlan::analyze(&function, &ir).accesses().is_empty());

        let (module, function) = shape_function(false, true);
        let ir = FunctionIr::build(&function, &module).expect("layout-drift IR");
        assert!(ShapePlan::analyze(&function, &ir).accesses().is_empty());
    }

    #[test]
    fn a_later_allocation_ends_the_earlier_fresh_window() {
        let (mut module, mut function) = shape_function(false, false);
        function
            .code
            .insert(3, Instruction::new(Opcode::PtrNew, 4, 0, 0));
        function.instruction_metadata.insert(
            3,
            InstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::GcRef],
            },
        );
        module.functions.clear();
        module.functions.push(function.clone());
        let ir = FunctionIr::build(&function, &module).expect("allocation-boundary IR");
        assert!(ShapePlan::analyze(&function, &ir).accesses().is_empty());
    }
}
