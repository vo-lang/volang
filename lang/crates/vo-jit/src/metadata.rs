//! JIT adapters for common bytecode metadata views.
//!
//! Layout decoding and validation live with `InstructionMetadata`; this module
//! only preserves the small effect/translator adapter used inside the JIT.

use vo_runtime::bytecode::InstructionMetadata;
use vo_runtime::instruction::Instruction;

pub use vo_runtime::bytecode::{
    ElemLayout, IfaceAssertLayout, MapGetLayout, MapIterNextLayout, MapNewLayout, MapSetLayout,
};

#[derive(Debug, Clone, Copy, Default)]
pub struct MetadataFacts<'a> {
    instruction: Option<&'a InstructionMetadata>,
}

impl<'a> MetadataFacts<'a> {
    #[cfg(test)]
    pub fn none() -> Self {
        Self { instruction: None }
    }

    pub fn from_instruction(instruction: Option<&'a InstructionMetadata>) -> Self {
        Self { instruction }
    }

    pub(crate) fn instruction(self) -> Option<&'a InstructionMetadata> {
        self.instruction
    }

    pub(crate) fn call_layout_slots(self) -> Option<(u16, u16)> {
        self.instruction?.call_layout_slots()
    }
}

#[inline]
pub fn elem_layout_from_instruction(metadata: &InstructionMetadata) -> Option<ElemLayout> {
    metadata.elem_layout()
}

#[inline]
pub fn map_get_layout_from_instruction(metadata: &InstructionMetadata) -> Option<MapGetLayout> {
    metadata.map_get_layout()
}

#[inline]
pub fn map_new_layout_from_instruction(metadata: &InstructionMetadata) -> Option<MapNewLayout> {
    metadata.map_new_layout()
}

#[inline]
pub fn map_set_layout_from_instruction(metadata: &InstructionMetadata) -> Option<MapSetLayout> {
    metadata.map_set_layout()
}

#[inline]
pub fn map_delete_key_slots_from_instruction(metadata: &InstructionMetadata) -> Option<u16> {
    metadata.map_delete_key_slots()
}

#[inline]
pub(crate) fn call_iface_method_index_from_instruction(
    metadata: &InstructionMetadata,
) -> Option<u32> {
    metadata.call_iface_method_index()
}

#[inline]
pub fn indexed_set_value_slots(_: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    facts.instruction?.elem_layout().map(|layout| layout.slots)
}

#[inline]
pub fn slice_append_value_slots(_: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    facts.instruction?.elem_layout().map(|layout| layout.slots)
}

#[inline]
pub fn map_get_layout(_: &Instruction, facts: MetadataFacts<'_>) -> Option<MapGetLayout> {
    facts.instruction?.map_get_layout()
}

#[inline]
pub fn map_set_layout(_: &Instruction, facts: MetadataFacts<'_>) -> Option<MapSetLayout> {
    facts.instruction?.map_set_layout()
}

#[inline]
pub fn map_delete_key_slots(_: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    facts.instruction?.map_delete_key_slots()
}

#[inline]
pub fn map_iter_next_layout(
    _: &Instruction,
    facts: MetadataFacts<'_>,
) -> Option<MapIterNextLayout> {
    facts.instruction?.map_iter_next_layout()
}

#[inline]
pub fn iface_assert_layout(_: &Instruction, facts: MetadataFacts<'_>) -> Option<IfaceAssertLayout> {
    facts.instruction?.iface_assert_layout()
}

#[inline]
pub fn queue_elem_slots(_: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    facts.instruction?.queue_elem_slots()
}

#[inline]
pub fn slot_elem_slots(_: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    facts.instruction?.slot_elem_slots()
}

#[inline]
pub fn ptr_value_slots(_: &Instruction, facts: MetadataFacts<'_>) -> Option<u16> {
    facts.instruction?.ptr_value_slots()
}
