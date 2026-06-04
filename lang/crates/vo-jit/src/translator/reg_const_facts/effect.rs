use std::collections::HashMap;

use vo_runtime::bytecode::{Constant, ExternDef, FunctionDef, JitInstructionMetadata};
use vo_runtime::instruction::{Instruction, Opcode};

use crate::effects::{self, MemorySyncEffect};

use super::fold::single_slot_const_result;
use super::merge::{kill_slot, kill_slots, kill_slots_at_or_after, set_slot_const};

#[derive(Debug, PartialEq, Eq)]
enum RegConstEffect {
    Preserve,
    Clear,
    KillSlotsAtOrAfter {
        start: u16,
    },
    KillSlotList(Vec<u16>),
    SetSlot {
        slot: u16,
        value: Option<i64>,
    },
    SetSlots {
        start: u16,
        values: Vec<Option<i64>>,
    },
}

impl RegConstEffect {
    fn apply(self, facts: &mut HashMap<u16, i64>) {
        match self {
            RegConstEffect::Preserve => {}
            RegConstEffect::Clear => facts.clear(),
            RegConstEffect::KillSlotsAtOrAfter { start } => {
                kill_slots_at_or_after(facts, start);
            }
            RegConstEffect::KillSlotList(slots) => {
                for slot in slots {
                    kill_slot(facts, slot);
                }
            }
            RegConstEffect::SetSlot { slot, value } => set_slot_const(facts, slot, value),
            RegConstEffect::SetSlots { start, values } => {
                kill_slots(facts, start, values.len() as u16);
                for (i, value) in values.into_iter().enumerate() {
                    if let Some(value) = value {
                        let Some(slot) = start.checked_add(i as u16) else {
                            break;
                        };
                        facts.insert(slot, value);
                    }
                }
            }
        }
    }
}

pub(super) fn transfer_reg_const_facts(
    inst: &Instruction,
    jit_metadata: Option<&JitInstructionMetadata>,
    constants: &[Constant],
    functions: &[FunctionDef],
    externs: &[ExternDef],
    facts: &mut HashMap<u16, i64>,
) {
    reg_const_effect(inst, jit_metadata, constants, functions, externs, facts).apply(facts);
}

fn reg_const_effect(
    inst: &Instruction,
    jit_metadata: Option<&JitInstructionMetadata>,
    constants: &[Constant],
    functions: &[FunctionDef],
    externs: &[ExternDef],
    facts: &HashMap<u16, i64>,
) -> RegConstEffect {
    if let Some(value) = single_slot_const_result(inst, constants, facts) {
        return RegConstEffect::SetSlot {
            slot: inst.a,
            value,
        };
    }

    let metadata_facts = crate::metadata::MetadataFacts::from_instruction(jit_metadata);

    match inst.opcode() {
        Opcode::CopyN => {
            let count = inst.copy_n_count();
            let values = (0..count)
                .map(|i| {
                    inst.b
                        .checked_add(i)
                        .and_then(|slot| facts.get(&slot).copied())
                })
                .collect();
            RegConstEffect::SetSlots {
                start: inst.a,
                values,
            }
        }
        Opcode::SelectExec => RegConstEffect::Clear,
        Opcode::SlotSet | Opcode::SlotSetN => match effects::try_memory_sync_effect(inst) {
            Ok(MemorySyncEffect::From(start)) => RegConstEffect::KillSlotsAtOrAfter { start },
            Ok(MemorySyncEffect::None | MemorySyncEffect::All) => RegConstEffect::Clear,
            Err(_) => RegConstEffect::Clear,
        },
        _ => effects::try_instruction_effects_with_module_context(
            inst,
            metadata_facts,
            externs,
            functions,
        )
        .map(reg_const_effect_from_instruction_effects)
        .unwrap_or_else(|_| reg_const_effect_for_unknown_effects(inst)),
    }
}

fn reg_const_effect_from_instruction_effects(
    effects: effects::InstructionEffects,
) -> RegConstEffect {
    if effects.writes.is_empty() {
        RegConstEffect::Preserve
    } else {
        RegConstEffect::KillSlotList(effects.writes)
    }
}

fn reg_const_effect_for_unknown_effects(inst: &Instruction) -> RegConstEffect {
    let shape = crate::semantics::opcode_register_effects(inst.opcode());
    if !shape.has_write_effects() {
        RegConstEffect::Preserve
    } else {
        RegConstEffect::Clear
    }
}
