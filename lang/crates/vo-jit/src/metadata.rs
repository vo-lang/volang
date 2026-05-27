//! Typed JIT metadata facts decoded from the current bytecode representation.
//!
//! Today these facts are recovered from compile-time constant registers. Keeping
//! the decoding here gives callers a typed contract and leaves room to move the
//! source to bytecode operands or metadata tables later.

use std::collections::HashMap;

use vo_runtime::instruction::Instruction;

#[derive(Debug, Clone, Copy, Default)]
pub struct MetadataFacts<'a> {
    reg_consts: Option<&'a HashMap<u16, i64>>,
}

impl<'a> MetadataFacts<'a> {
    pub fn none() -> Self {
        Self { reg_consts: None }
    }

    pub fn from_reg_consts(reg_consts: &'a HashMap<u16, i64>) -> Self {
        Self {
            reg_consts: Some(reg_consts),
        }
    }

    pub fn has_reg_consts(self) -> bool {
        self.reg_consts.is_some()
    }

    fn const_i64(self, slot: u16) -> Option<i64> {
        self.reg_consts
            .and_then(|reg_consts| reg_consts.get(&slot).copied())
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
    pub fn output_slots(self) -> u16 {
        self.val_slots + u16::from(self.has_ok)
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
    elem_layout_from_bytes(bytes, needs_sign_extend).expect("non-zero elem layout")
}

pub fn elem_layout_from_dynamic_bytes(elem_bytes: i64) -> Option<ElemLayout> {
    let elem_bytes = usize::try_from(elem_bytes).ok()?;
    elem_layout_from_bytes(elem_bytes, false)
}

pub fn elem_layout_from_flags_or_dynamic_bytes(
    flags: u8,
    dynamic_elem_bytes: Option<i64>,
) -> Option<ElemLayout> {
    if flags == 0 {
        elem_layout_from_dynamic_bytes(dynamic_elem_bytes?)
    } else {
        Some(elem_layout_from_flags(flags))
    }
}

fn elem_layout_from_bytes(bytes: usize, needs_sign_extend: bool) -> Option<ElemLayout> {
    let slots = u16::try_from(bytes.div_ceil(8)).ok()?;
    Some(ElemLayout {
        bytes,
        slots,
        needs_sign_extend,
    })
}

fn elem_layout_from_flags_or_fact(
    flags: u8,
    dynamic_bytes_slot: u16,
    facts: MetadataFacts<'_>,
) -> Option<ElemLayout> {
    elem_layout_from_flags_or_dynamic_bytes(flags, facts.const_i64(dynamic_bytes_slot))
}

pub fn indexed_get_result_slots(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    elem_layout_from_flags_or_fact(inst.flags, inst.c + 1, facts).map(|layout| layout.slots)
}

pub fn indexed_set_value_slots(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    elem_layout_from_flags_or_fact(inst.flags, inst.b + 1, facts).map(|layout| layout.slots)
}

pub fn slice_append_value_slots(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    elem_layout_from_flags_or_fact(inst.flags, inst.c + 1, facts).map(|layout| layout.slots)
}

pub fn map_get_layout_from_meta(meta: i64) -> MapGetLayout {
    let meta = meta as u64;
    MapGetLayout {
        key_slots: ((meta >> 16) & 0xFFFF) as u16,
        val_slots: ((meta >> 1) & 0x7FFF) as u16,
        has_ok: (meta & 1) != 0,
    }
}

pub fn map_get_layout(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<MapGetLayout> {
    facts.const_i64(inst.c).map(map_get_layout_from_meta)
}

pub fn map_set_layout_from_meta(meta: i64) -> MapSetLayout {
    let meta = meta as u64;
    MapSetLayout {
        key_slots: ((meta >> 8) & 0xFF) as u16,
        val_slots: (meta & 0xFF) as u16,
    }
}

pub fn map_set_layout(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<MapSetLayout> {
    facts.const_i64(inst.b).map(map_set_layout_from_meta)
}

pub fn map_delete_key_slots_from_meta(meta: i64) -> Option<u16> {
    u16::try_from(meta).ok()
}

pub fn map_delete_key_slots(inst: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    map_delete_key_slots_from_meta(facts.const_i64(inst.b)?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::instruction::Opcode;

    fn fact_map(entries: &[(u16, i64)]) -> HashMap<u16, i64> {
        entries.iter().copied().collect()
    }

    #[test]
    fn decodes_static_and_dynamic_elem_layouts() {
        assert_eq!(
            elem_layout_from_flags(0x82),
            ElemLayout {
                bytes: 2,
                slots: 1,
                needs_sign_extend: true
            }
        );
        assert_eq!(
            elem_layout_from_flags_or_dynamic_bytes(0, Some(24)),
            Some(ElemLayout {
                bytes: 24,
                slots: 3,
                needs_sign_extend: false
            })
        );
        assert_eq!(
            elem_layout_from_flags_or_dynamic_bytes(0, Some(0)),
            Some(ElemLayout {
                bytes: 0,
                slots: 0,
                needs_sign_extend: false
            })
        );
    }

    #[test]
    fn decodes_map_layouts_from_metadata_values() {
        let get_meta = (3i64 << 16) | (2i64 << 1) | 1;
        assert_eq!(
            map_get_layout_from_meta(get_meta),
            MapGetLayout {
                key_slots: 3,
                val_slots: 2,
                has_ok: true
            }
        );

        let set_meta = (2i64 << 8) | 3;
        assert_eq!(
            map_set_layout_from_meta(set_meta),
            MapSetLayout {
                key_slots: 2,
                val_slots: 3
            }
        );
    }

    #[test]
    fn metadata_facts_read_expected_operand_slots() {
        let facts = fact_map(&[(5, (2i64 << 16) | (3i64 << 1)), (8, 24)]);
        let map_get = Instruction::new(Opcode::MapGet, 10, 2, 5);
        let slice_get = Instruction::with_flags(Opcode::SliceGet, 0, 20, 2, 7);
        let facts = MetadataFacts::from_reg_consts(&facts);

        assert_eq!(map_get_layout(&map_get, facts).unwrap().output_slots(), 3);
        assert_eq!(indexed_get_result_slots(&slice_get, facts), Some(3));
    }
}
