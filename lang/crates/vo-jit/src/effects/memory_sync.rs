use vo_runtime::instruction::Instruction;

use crate::semantics::{opcode_register_effects, MemorySyncSpec};

use super::operand_eval::{checked_slot_offset, operand_slot};
use super::SlotRangeError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemorySyncEffect {
    None,
    From(u16),
    All,
}

pub fn try_memory_sync_effect(inst: &Instruction) -> Result<MemorySyncEffect, SlotRangeError> {
    match opcode_register_effects(inst.opcode()).memory_sync {
        MemorySyncSpec::None => Ok(MemorySyncEffect::None),
        MemorySyncSpec::All => Ok(MemorySyncEffect::All),
        MemorySyncSpec::FromOperand(operand) => {
            Ok(MemorySyncEffect::From(operand_slot(inst, operand)))
        }
        MemorySyncSpec::SliceAppendValueStart => {
            let elem_slot =
                checked_slot_offset(inst.c, if inst.flags == 0 { 2 } else { 1 }, "memory")?;
            Ok(MemorySyncEffect::From(elem_slot))
        }
    }
}
