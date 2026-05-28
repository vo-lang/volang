//! Typed JIT metadata facts decoded from the current bytecode representation.
//!
//! Dynamic layouts must come from per-instruction metadata. Compile-time
//! register constants are optimization facts only; they are not a layout
//! authority for JIT lowering or effects analysis.

use vo_runtime::bytecode::JitInstructionMetadata;
use vo_runtime::instruction::Instruction;

#[derive(Debug, Clone, Copy, Default)]
pub struct MetadataFacts<'a> {
    instruction: Option<&'a JitInstructionMetadata>,
}

impl<'a> MetadataFacts<'a> {
    pub fn none() -> Self {
        Self { instruction: None }
    }

    pub fn from_instruction(metadata: Option<&'a JitInstructionMetadata>) -> Self {
        Self {
            instruction: metadata,
        }
    }

    pub fn has_facts(self) -> bool {
        self.instruction.is_some()
    }

    fn elem_layout(self) -> Option<ElemLayout> {
        self.instruction.and_then(elem_layout_from_instruction)
    }

    fn map_get_layout(self) -> Option<MapGetLayout> {
        self.instruction.and_then(map_get_layout_from_instruction)
    }

    fn map_set_layout(self) -> Option<MapSetLayout> {
        self.instruction.and_then(map_set_layout_from_instruction)
    }

    fn map_delete_key_slots(self) -> Option<u16> {
        self.instruction
            .and_then(map_delete_key_slots_from_instruction)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ElemLayout {
    pub bytes: usize,
    pub slots: u16,
    pub needs_sign_extend: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MapGetLayout {
    pub key_slots: u16,
    pub val_slots: u16,
    pub has_ok: bool,
}

impl MapGetLayout {
    pub fn output_slots(self) -> Option<u16> {
        self.val_slots.checked_add(u16::from(self.has_ok))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MapSetLayout {
    pub key_slots: u16,
    pub val_slots: u16,
}

pub fn elem_layout_from_flags(flags: u8) -> ElemLayout {
    let (bytes, needs_sign_extend) = match flags {
        0 => (64, false),
        0x81 => (1, true),
        0x82 => (2, true),
        0x84 => (4, true),
        0x44 => (4, false),
        f => (f as usize, false),
    };
    elem_layout_from_bytes(bytes, needs_sign_extend).expect("valid elem layout")
}

pub fn elem_layout_from_instruction(metadata: &JitInstructionMetadata) -> Option<ElemLayout> {
    match *metadata {
        JitInstructionMetadata::ElemLayout {
            elem_bytes,
            needs_sign_extend,
        } => elem_layout_from_bytes(elem_bytes as usize, needs_sign_extend),
        _ => None,
    }
}

fn elem_layout_from_bytes(bytes: usize, needs_sign_extend: bool) -> Option<ElemLayout> {
    if bytes == 0 {
        return (!needs_sign_extend).then_some(ElemLayout {
            bytes: 0,
            slots: 0,
            needs_sign_extend: false,
        });
    }
    let slots = u16::try_from(bytes.div_ceil(8)).ok()?;
    Some(ElemLayout {
        bytes,
        slots,
        needs_sign_extend,
    })
}

fn elem_layout_from_flags_or_fact(flags: u8, facts: MetadataFacts<'_>) -> Option<ElemLayout> {
    if flags == 0 {
        facts.elem_layout()
    } else {
        Some(elem_layout_from_flags(flags))
    }
}

pub fn indexed_get_result_slots(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    elem_layout_from_flags_or_fact(inst.flags, facts).map(|layout| layout.slots)
}

pub fn indexed_set_value_slots(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    elem_layout_from_flags_or_fact(inst.flags, facts).map(|layout| layout.slots)
}

pub fn slice_append_value_slots(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    elem_layout_from_flags_or_fact(inst.flags, facts).map(|layout| layout.slots)
}

pub fn map_get_layout_from_instruction(metadata: &JitInstructionMetadata) -> Option<MapGetLayout> {
    match *metadata {
        JitInstructionMetadata::MapGet {
            key_slots,
            val_slots,
            has_ok,
        } => Some(MapGetLayout {
            key_slots,
            val_slots,
            has_ok,
        }),
        _ => None,
    }
}

pub fn map_get_layout(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<MapGetLayout> {
    let _ = inst;
    facts.map_get_layout()
}

pub fn map_set_layout_from_instruction(metadata: &JitInstructionMetadata) -> Option<MapSetLayout> {
    match *metadata {
        JitInstructionMetadata::MapSet {
            key_slots,
            val_slots,
        } => Some(MapSetLayout {
            key_slots,
            val_slots,
        }),
        _ => None,
    }
}

pub fn map_set_layout(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<MapSetLayout> {
    let _ = inst;
    facts.map_set_layout()
}

pub fn map_delete_key_slots_from_instruction(metadata: &JitInstructionMetadata) -> Option<u16> {
    match *metadata {
        JitInstructionMetadata::MapDelete { key_slots } => Some(key_slots),
        _ => None,
    }
}

pub fn map_delete_key_slots(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    let _ = inst;
    facts.map_delete_key_slots()
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::instruction::Opcode;

    #[test]
    fn decodes_static_elem_layouts() {
        assert_eq!(
            elem_layout_from_flags(0x82),
            ElemLayout {
                bytes: 2,
                slots: 1,
                needs_sign_extend: true
            }
        );
    }

    #[test]
    fn metadata_facts_read_expected_instruction_layouts() {
        let map_get = Instruction::new(Opcode::MapGet, 10, 2, 5);
        let slice_get = Instruction::with_flags(Opcode::SliceGet, 0, 20, 2, 7);
        let map_meta = JitInstructionMetadata::MapGet {
            key_slots: 2,
            val_slots: 3,
            has_ok: false,
        };
        let elem_meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
        };

        assert_eq!(
            map_get_layout(&map_get, MetadataFacts::from_instruction(Some(&map_meta)))
                .unwrap()
                .output_slots(),
            Some(3)
        );
        assert_eq!(
            indexed_get_result_slots(
                &slice_get,
                MetadataFacts::from_instruction(Some(&elem_meta))
            ),
            Some(3)
        );
    }

    #[test]
    fn metadata_facts_allow_zero_size_elem_layouts() {
        let slice_get = Instruction::with_flags(Opcode::SliceGet, 0, 20, 2, 7);
        let elem_meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 0,
            needs_sign_extend: false,
        };

        assert_eq!(
            indexed_get_result_slots(
                &slice_get,
                MetadataFacts::from_instruction(Some(&elem_meta))
            ),
            Some(0)
        );
    }

    #[test]
    fn missing_dynamic_layout_has_no_register_fallback() {
        let map_get = Instruction::new(Opcode::MapGet, 10, 2, 5);
        let slice_get = Instruction::with_flags(Opcode::SliceGet, 0, 20, 2, 7);

        let map_meta = JitInstructionMetadata::MapGet {
            key_slots: 4,
            val_slots: 3,
            has_ok: true,
        };
        let elem_meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
        };

        assert_eq!(
            map_get_layout(&map_get, MetadataFacts::from_instruction(Some(&map_meta)))
                .unwrap()
                .output_slots(),
            Some(4)
        );
        assert_eq!(
            indexed_get_result_slots(
                &slice_get,
                MetadataFacts::from_instruction(Some(&elem_meta))
            ),
            Some(3)
        );
        assert_eq!(map_get_layout(&map_get, MetadataFacts::none()), None);
        assert_eq!(
            indexed_get_result_slots(&slice_get, MetadataFacts::none()),
            None
        );
    }
}
