use vo_runtime::instruction::Instruction;

use super::{EffectError, EffectFacts, SlotRangeError};

pub use vo_common_core::instruction_effects::FrameMemoryEffect as MemorySyncEffect;

pub fn try_memory_sync_effect(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<MemorySyncEffect, EffectError> {
    match vo_common_core::instruction_effects::instruction_frame_memory_effect(
        inst,
        facts.instruction(),
    ) {
        Ok(effect) => Ok(effect),
        Err(vo_common_core::instruction_effects::InstructionReadError::SlotRangeOverflow {
            start,
            count,
        }) => Err(
            SlotRangeError::new("memory", start, u16::try_from(count).unwrap_or(u16::MAX)).into(),
        ),
        Err(vo_common_core::instruction_effects::InstructionReadError::MissingMetadata(opcode)) => {
            Err(EffectError::MissingLayout {
                opcode,
                layout: "SlotLayout",
            })
        }
        Err(vo_common_core::instruction_effects::InstructionReadError::MissingFunction(
            func_id,
        )) => Err(EffectError::MissingFunction { func_id }),
    }
}
