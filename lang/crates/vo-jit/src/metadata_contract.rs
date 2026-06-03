//! Single source of truth for strict JIT instruction metadata contracts.
//!
//! This module answers three related questions for every opcode:
//! which current metadata kind it may consume, which metadata table entry is
//! required before lowering, and which serialized legacy kinds are compatibility
//! input only. Verifier, semantics, and contract graph code should depend on
//! this table instead of maintaining parallel opcode lists.

use vo_runtime::bytecode::JitInstructionMetadata;
use vo_runtime::instruction::{Opcode, HINT_LOOP};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JitMetadataRequirement {
    None,
    ElemLayoutWhenFlagsZero,
    MapGet,
    MapSet,
    MapDelete,
    PtrLayout,
    SlotLayout,
    CallLayout,
    CallLayoutWhenClosureShape,
    CallExternLayout,
    QueueLayout,
    MapIterNext,
    IfaceAssertLayout,
    LoopEndForHintLoop,
}

impl JitMetadataRequirement {
    pub fn required_layout_name(self, flags: u8) -> Option<&'static str> {
        match self {
            Self::None => None,
            Self::ElemLayoutWhenFlagsZero => Some("ElemLayout"),
            Self::MapGet => Some("MapGet"),
            Self::MapSet => Some("MapSet"),
            Self::MapDelete => Some("MapDelete"),
            Self::PtrLayout => Some("PtrLayout"),
            Self::SlotLayout => Some("SlotLayout"),
            Self::CallLayout => Some("CallLayout"),
            Self::CallLayoutWhenClosureShape if (flags & 1) != 0 => Some("CallLayout"),
            Self::CallLayoutWhenClosureShape => None,
            Self::CallExternLayout => Some("CallExternLayout"),
            Self::QueueLayout => Some("QueueLayout"),
            Self::MapIterNext => Some("MapIterNext"),
            Self::IfaceAssertLayout => Some("IfaceAssertLayout"),
            Self::LoopEndForHintLoop => None,
        }
    }

    fn accepts_kind(self, flags: u8, kind: JitMetadataKind) -> bool {
        match (self, kind) {
            (Self::ElemLayoutWhenFlagsZero, JitMetadataKind::ElemLayout) => true,
            (Self::MapGet, JitMetadataKind::MapGet) => true,
            (Self::MapSet, JitMetadataKind::MapSet) => true,
            (Self::MapDelete, JitMetadataKind::MapDelete) => true,
            (Self::PtrLayout, JitMetadataKind::PtrLayout) => true,
            (Self::SlotLayout, JitMetadataKind::SlotLayout) => true,
            (Self::CallLayout, JitMetadataKind::CallLayout) => true,
            (Self::CallLayoutWhenClosureShape, JitMetadataKind::CallLayout) => (flags & 1) != 0,
            (Self::CallExternLayout, JitMetadataKind::CallExternLayout) => true,
            (Self::QueueLayout, JitMetadataKind::QueueLayout) => true,
            (Self::MapIterNext, JitMetadataKind::MapIterNext) => true,
            (Self::IfaceAssertLayout, JitMetadataKind::IfaceAssertLayout) => true,
            (Self::LoopEndForHintLoop, JitMetadataKind::LoopEnd) => flags == HINT_LOOP,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum JitMetadataKind {
    None,
    ElemLayout,
    MapGet,
    MapSet,
    MapDelete,
    PtrLayout,
    SlotLayout,
    CallLayout,
    CallExternLayout,
    QueueLayout,
    MapIterNext,
    IfaceAssertLayout,
    LegacyMapGet,
    LegacyMapSet,
    LegacyMapDelete,
    LoopEnd,
}

impl JitMetadataKind {
    pub const STRICT_CURRENT: &'static [Self] = &[
        Self::None,
        Self::ElemLayout,
        Self::MapGet,
        Self::MapSet,
        Self::MapDelete,
        Self::PtrLayout,
        Self::SlotLayout,
        Self::CallLayout,
        Self::CallExternLayout,
        Self::QueueLayout,
        Self::MapIterNext,
        Self::IfaceAssertLayout,
        Self::LoopEnd,
    ];

    pub const LEGACY_COMPAT: &'static [Self] = &[
        Self::LegacyMapGet,
        Self::LegacyMapSet,
        Self::LegacyMapDelete,
    ];

    pub fn name(self) -> &'static str {
        match self {
            Self::None => "None",
            Self::ElemLayout => "ElemLayout",
            Self::MapGet => "MapGet",
            Self::MapSet => "MapSet",
            Self::MapDelete => "MapDelete",
            Self::PtrLayout => "PtrLayout",
            Self::SlotLayout => "SlotLayout",
            Self::CallLayout => "CallLayout",
            Self::CallExternLayout => "CallExternLayout",
            Self::QueueLayout => "QueueLayout",
            Self::MapIterNext => "MapIterNext",
            Self::IfaceAssertLayout => "IfaceAssertLayout",
            Self::LegacyMapGet => "LegacyMapGet",
            Self::LegacyMapSet => "LegacyMapSet",
            Self::LegacyMapDelete => "LegacyMapDelete",
            Self::LoopEnd => "LoopEnd",
        }
    }

    pub fn is_legacy_compat_only(self) -> bool {
        matches!(
            self,
            Self::LegacyMapGet | Self::LegacyMapSet | Self::LegacyMapDelete
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MetadataContractViolation {
    MissingRequiredLayout { layout: &'static str },
    WrongKind { metadata: &'static str },
    UnsupportedLegacy { metadata: &'static str },
}

pub fn metadata_kind(metadata: &JitInstructionMetadata) -> JitMetadataKind {
    match metadata {
        JitInstructionMetadata::None => JitMetadataKind::None,
        JitInstructionMetadata::ElemLayout { .. } => JitMetadataKind::ElemLayout,
        JitInstructionMetadata::MapGet { .. } => JitMetadataKind::MapGet,
        JitInstructionMetadata::MapSet { .. } => JitMetadataKind::MapSet,
        JitInstructionMetadata::MapDelete { .. } => JitMetadataKind::MapDelete,
        JitInstructionMetadata::PtrLayout { .. } => JitMetadataKind::PtrLayout,
        JitInstructionMetadata::SlotLayout { .. } => JitMetadataKind::SlotLayout,
        JitInstructionMetadata::CallLayout { .. } => JitMetadataKind::CallLayout,
        JitInstructionMetadata::CallExternLayout { .. } => JitMetadataKind::CallExternLayout,
        JitInstructionMetadata::QueueLayout { .. } => JitMetadataKind::QueueLayout,
        JitInstructionMetadata::MapIterNext { .. } => JitMetadataKind::MapIterNext,
        JitInstructionMetadata::IfaceAssertLayout { .. } => JitMetadataKind::IfaceAssertLayout,
        JitInstructionMetadata::LegacyMapGet { .. } => JitMetadataKind::LegacyMapGet,
        JitInstructionMetadata::LegacyMapSet { .. } => JitMetadataKind::LegacyMapSet,
        JitInstructionMetadata::LegacyMapDelete { .. } => JitMetadataKind::LegacyMapDelete,
        JitInstructionMetadata::LoopEnd { .. } => JitMetadataKind::LoopEnd,
    }
}

pub fn opcode_metadata_requirement(opcode: Opcode) -> JitMetadataRequirement {
    crate::semantics::opcode_metadata_requirement_from_semantics(opcode)
}

pub fn strict_metadata_contract_violation(
    opcode: Opcode,
    flags: u8,
    metadata: &JitInstructionMetadata,
) -> Option<MetadataContractViolation> {
    let requirement = opcode_metadata_requirement(opcode);
    let kind = metadata_kind(metadata);
    if kind == JitMetadataKind::None {
        return requirement
            .required_layout_name(flags)
            .map(|layout| MetadataContractViolation::MissingRequiredLayout { layout });
    }
    if kind.is_legacy_compat_only() {
        return Some(MetadataContractViolation::UnsupportedLegacy {
            metadata: kind.name(),
        });
    }
    if requirement.accepts_kind(flags, kind) {
        None
    } else {
        Some(MetadataContractViolation::WrongKind {
            metadata: kind.name(),
        })
    }
}
