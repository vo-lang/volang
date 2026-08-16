use vo_runtime::instruction::Instruction;

use super::{EffectError, EffectFacts, SlotRangeError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemorySyncEffect {
    None,
    AliasedRange { start: u16, count: u16 },
    From(u16),
}

pub fn try_memory_sync_effect(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<MemorySyncEffect, EffectError> {
    match vo_common_core::instruction_effects::instruction_frame_memory_effect(
        inst,
        facts.instruction(),
    ) {
        Ok(vo_common_core::instruction_effects::FrameMemoryEffect::None) => {
            Ok(MemorySyncEffect::None)
        }
        Ok(vo_common_core::instruction_effects::FrameMemoryEffect::AliasedRange {
            start,
            count,
        }) => Ok(MemorySyncEffect::AliasedRange { start, count }),
        Ok(vo_common_core::instruction_effects::FrameMemoryEffect::From(start)) => {
            Ok(MemorySyncEffect::From(start))
        }
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
