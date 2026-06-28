//! Typed JIT metadata facts decoded from the current bytecode representation.
//!
//! Dynamic layouts must come from per-instruction metadata. Compile-time
//! register constants are optimization facts only; they are not a layout
//! authority for JIT lowering or effects analysis.

use vo_runtime::bytecode::JitInstructionMetadata;
use vo_runtime::instruction::Instruction;
use vo_runtime::SlotType;

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

    fn map_iter_next_layout(self) -> Option<MapIterNextLayout> {
        self.instruction
            .and_then(map_iter_next_layout_from_instruction)
    }

    fn iface_assert_layout(self) -> Option<IfaceAssertLayout> {
        self.instruction
            .and_then(iface_assert_layout_from_instruction)
    }

    fn queue_elem_slots(self) -> Option<u16> {
        self.instruction.and_then(queue_elem_slots_from_instruction)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElemLayout {
    pub bytes: usize,
    pub slots: u16,
    pub needs_sign_extend: bool,
    pub slot_layout: Vec<SlotType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapGetLayout {
    pub key_layout: Vec<SlotType>,
    pub val_layout: Vec<SlotType>,
    pub key_slots: u16,
    pub val_slots: u16,
    pub has_ok: bool,
}

impl MapGetLayout {
    pub fn output_slots(&self) -> Option<u16> {
        self.val_slots.checked_add(u16::from(self.has_ok))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapSetLayout {
    pub key_layout: Vec<SlotType>,
    pub val_layout: Vec<SlotType>,
    pub key_slots: u16,
    pub val_slots: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapIterNextLayout {
    pub key_layout: Vec<SlotType>,
    pub val_layout: Vec<SlotType>,
    pub key_slots: u16,
    pub val_slots: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfaceAssertLayout {
    pub result_layout: Vec<SlotType>,
    pub result_slots: u16,
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
    let slot = if (flags & vo_common_core::ELEM_FLAG_FLOAT_BIT) != 0 {
        SlotType::Float
    } else {
        SlotType::Value
    };
    let slots = bytes.div_ceil(8) as u16;
    ElemLayout {
        bytes,
        slots,
        needs_sign_extend,
        slot_layout: vec![slot; slots as usize],
    }
}

pub fn elem_layout_from_instruction(metadata: &JitInstructionMetadata) -> Option<ElemLayout> {
    match metadata {
        JitInstructionMetadata::ElemLayout {
            elem_bytes,
            needs_sign_extend,
            slot_layout,
        } => elem_layout_from_bytes(*elem_bytes as usize, *needs_sign_extend, slot_layout),
        _ => None,
    }
}

fn elem_layout_from_bytes(
    bytes: usize,
    needs_sign_extend: bool,
    slot_layout: &[SlotType],
) -> Option<ElemLayout> {
    if bytes == 0 {
        if needs_sign_extend {
            return None;
        }
        if !slot_layout.is_empty() && slot_layout != [SlotType::Value] {
            return None;
        }
        return Some(ElemLayout {
            bytes: 0,
            slots: 0,
            needs_sign_extend: false,
            slot_layout: slot_layout.to_vec(),
        });
    }
    let slots = u16::try_from(bytes.div_ceil(8)).ok()?;
    if slot_layout.len() != slots as usize {
        return None;
    }
    Some(ElemLayout {
        bytes,
        slots,
        needs_sign_extend,
        slot_layout: slot_layout.to_vec(),
    })
}

fn elem_layout_from_flags_or_fact(flags: u8, facts: MetadataFacts<'_>) -> Option<ElemLayout> {
    facts
        .elem_layout()
        .or_else(|| (flags != 0).then(|| elem_layout_from_flags(flags)))
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
    match metadata {
        JitInstructionMetadata::MapGet {
            key_layout,
            val_layout,
            has_ok,
        } => Some(MapGetLayout {
            key_layout: key_layout.clone(),
            val_layout: val_layout.clone(),
            key_slots: u16::try_from(key_layout.len()).ok()?,
            val_slots: u16::try_from(val_layout.len()).ok()?,
            has_ok: *has_ok,
        }),
        _ => None,
    }
}

pub fn map_get_layout(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<MapGetLayout> {
    let _ = inst;
    facts.map_get_layout()
}

pub fn map_set_layout_from_instruction(metadata: &JitInstructionMetadata) -> Option<MapSetLayout> {
    match metadata {
        JitInstructionMetadata::MapSet {
            key_layout,
            val_layout,
        } => Some(MapSetLayout {
            key_layout: key_layout.clone(),
            val_layout: val_layout.clone(),
            key_slots: u16::try_from(key_layout.len()).ok()?,
            val_slots: u16::try_from(val_layout.len()).ok()?,
        }),
        _ => None,
    }
}

pub fn map_set_layout(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<MapSetLayout> {
    let _ = inst;
    facts.map_set_layout()
}

pub fn map_delete_key_slots_from_instruction(metadata: &JitInstructionMetadata) -> Option<u16> {
    match metadata {
        JitInstructionMetadata::MapDelete { key_layout } => {
            Some(u16::try_from(key_layout.len()).ok()?)
        }
        _ => None,
    }
}

pub fn map_delete_key_slots(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    let _ = inst;
    facts.map_delete_key_slots()
}

pub fn map_iter_next_layout_from_instruction(
    metadata: &JitInstructionMetadata,
) -> Option<MapIterNextLayout> {
    match metadata {
        JitInstructionMetadata::MapIterNext {
            key_layout,
            val_layout,
        } => Some(MapIterNextLayout {
            key_layout: key_layout.clone(),
            val_layout: val_layout.clone(),
            key_slots: u16::try_from(key_layout.len()).ok()?,
            val_slots: u16::try_from(val_layout.len()).ok()?,
        }),
        _ => None,
    }
}

pub fn map_iter_next_layout(
    inst: &Instruction,
    facts: MetadataFacts<'_>,
) -> Option<MapIterNextLayout> {
    let _ = inst;
    facts.map_iter_next_layout()
}

pub fn iface_assert_layout_from_instruction(
    metadata: &JitInstructionMetadata,
) -> Option<IfaceAssertLayout> {
    match metadata {
        JitInstructionMetadata::IfaceAssertLayout { result_layout } => Some(IfaceAssertLayout {
            result_layout: result_layout.clone(),
            result_slots: u16::try_from(result_layout.len()).ok()?,
        }),
        _ => None,
    }
}

pub fn iface_assert_layout(
    inst: &Instruction,
    facts: MetadataFacts<'_>,
) -> Option<IfaceAssertLayout> {
    let _ = inst;
    facts.iface_assert_layout()
}

pub fn queue_elem_slots_from_instruction(metadata: &JitInstructionMetadata) -> Option<u16> {
    match metadata {
        JitInstructionMetadata::QueueLayout { elem_layout } => {
            Some(u16::try_from(elem_layout.len()).ok()?)
        }
        _ => None,
    }
}

pub fn queue_elem_slots(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    let _ = inst;
    facts.queue_elem_slots()
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
                needs_sign_extend: true,
                slot_layout: vec![SlotType::Value]
            }
        );
    }

    #[test]
    fn metadata_facts_prefer_precise_elem_layout_over_compact_flags() {
        let slice_get = Instruction::with_flags(Opcode::SliceGet, 8, 20, 2, 7);
        let elem_meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::GcRef],
        };

        assert_eq!(
            elem_layout_from_flags(8),
            ElemLayout {
                bytes: 8,
                slots: 1,
                needs_sign_extend: false,
                slot_layout: vec![SlotType::Value]
            }
        );
        assert_eq!(
            indexed_get_result_slots(
                &slice_get,
                MetadataFacts::from_instruction(Some(&elem_meta))
            ),
            Some(1)
        );
        assert_eq!(
            elem_layout_from_flags_or_fact(8, MetadataFacts::from_instruction(Some(&elem_meta)))
                .unwrap()
                .slot_layout,
            vec![SlotType::GcRef]
        );
    }

    #[test]
    fn metadata_facts_read_expected_instruction_layouts() {
        let map_get = Instruction::new(Opcode::MapGet, 10, 2, 5);
        let slice_get = Instruction::with_flags(Opcode::SliceGet, 0, 20, 2, 7);
        let map_meta = JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Interface0, SlotType::Interface1],
            val_layout: vec![SlotType::Value, SlotType::GcRef, SlotType::Float],
            has_ok: false,
        };
        let elem_meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value; 3],
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
            slot_layout: Vec::new(),
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
    fn metadata_facts_allow_zero_size_empty_struct_logical_layout() {
        let slice_get = Instruction::with_flags(Opcode::SliceGet, 0, 20, 2, 7);
        let elem_meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 0,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        };

        let layout =
            elem_layout_from_flags_or_fact(0, MetadataFacts::from_instruction(Some(&elem_meta)))
                .expect("zero-byte empty struct sentinel layout should decode");
        assert_eq!(layout.slots, 0);
        assert_eq!(layout.slot_layout, vec![SlotType::Value]);
        assert_eq!(
            indexed_get_result_slots(
                &slice_get,
                MetadataFacts::from_instruction(Some(&elem_meta))
            ),
            Some(0)
        );
    }

    #[test]
    fn missing_dynamic_layout_has_no_register_substitute() {
        let map_get = Instruction::new(Opcode::MapGet, 10, 2, 5);
        let slice_get = Instruction::with_flags(Opcode::SliceGet, 0, 20, 2, 7);

        let map_meta = JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value; 4],
            val_layout: vec![SlotType::Value, SlotType::GcRef, SlotType::Float],
            has_ok: true,
        };
        let elem_meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value; 3],
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
