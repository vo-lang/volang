use vo_runtime::instruction::Instruction;

use super::SlotRangeError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemorySyncEffect {
    None,
    AliasedFrom(u16),
    From(u16),
    All,
}

pub fn try_memory_sync_effect(inst: &Instruction) -> Result<MemorySyncEffect, SlotRangeError> {
    match vo_common_core::instruction_effects::instruction_frame_memory_effect(inst) {
        Ok(vo_common_core::instruction_effects::FrameMemoryEffect::None) => {
            Ok(MemorySyncEffect::None)
        }
        Ok(vo_common_core::instruction_effects::FrameMemoryEffect::AliasedFrom(start)) => {
            Ok(MemorySyncEffect::AliasedFrom(start))
        }
        Ok(vo_common_core::instruction_effects::FrameMemoryEffect::From(start)) => {
            Ok(MemorySyncEffect::From(start))
        }
        Ok(vo_common_core::instruction_effects::FrameMemoryEffect::All) => {
            Ok(MemorySyncEffect::All)
        }
        Err(vo_common_core::instruction_effects::InstructionReadError::SlotRangeOverflow {
            start,
            count,
        }) => Err(SlotRangeError::new(
            "memory",
            start,
            u16::try_from(count).unwrap_or(u16::MAX),
        )),
        Err(_) => unreachable!("frame-memory effects do not depend on module metadata"),
    }
}
