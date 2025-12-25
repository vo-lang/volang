//! Common type definitions shared across the Vo compiler and runtime.
//! Reference: docs/spec/memory-model-and-instructions.md

use num_enum::TryFromPrimitive;

// =============================================================================
// Runtime Types
// =============================================================================

/// Meta ID - 24-bit index.
/// Meaning varies by value_kind:
/// - Array, Slice, Channel: elem_meta_id (element's meta_id)
/// - Struct, Pointer: meta_id of the pointed object
/// - Interface: meta_id of the interface type
/// - Map: 0 (key/val type info stored in Container data)
/// - Others: 0
pub type MetaId = u32;

pub const INVALID_META_ID: MetaId = 0xFF_FFFF; // 24-bit max
pub const META_ID_MASK: MetaId = 0xFF_FFFF;    // 24-bit mask

/// Value metadata - packed 32-bit representation.
/// Layout: [meta_id:24 | value_kind:8]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ValueMeta(u32);

impl ValueMeta {
    pub const VOID: ValueMeta = ValueMeta(ValueKind::Void as u32);

    #[inline]
    pub fn new(meta_id: u32, value_kind: ValueKind) -> Self {
        Self(((meta_id & META_ID_MASK) << 8) | (value_kind as u32))
    }

    #[inline]
    pub fn from_raw(raw: u32) -> Self {
        Self(raw)
    }

    #[inline]
    pub fn to_raw(self) -> u32 {
        self.0
    }

    #[inline]
    pub fn value_kind(self) -> ValueKind {
        ValueKind::from_u8(self.0 as u8)
    }

    #[inline]
    pub fn meta_id(self) -> u32 {
        self.0 >> 8
    }
}

/// Value kind - runtime classification of Vo values.
///
/// Layout:
/// - Primitives (0-14): 1 slot, no GC
/// - Compound value types (16, 21, 23): multi-slot, may contain GC refs
/// - Reference types (15, 17-20, 22): 1 slot GcRef, heap allocated
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, TryFromPrimitive)]
#[repr(u8)]
pub enum ValueKind {
    // === Primitive Types (1 slot, no GC) ===
    Void = 0,  // No type (distinct from semantic nil like nil pointer/slice/map)
    Bool = 1,
    Int = 2,
    Int8 = 3,
    Int16 = 4,
    Int32 = 5,
    Int64 = 6,
    Uint = 7,
    Uint8 = 8,
    Uint16 = 9,
    Uint32 = 10,
    Uint64 = 11,
    Float32 = 12,
    Float64 = 13,
    FuncPtr = 14,

    // === Compound Value Types (multi-slot, may contain GC refs) ===
    Array = 16,
    Struct = 21,
    Interface = 23,

    // === Reference Types (1 slot GcRef, heap allocated) ===
    String = 15,
    Slice = 17,
    Map = 18,
    Channel = 19,
    Closure = 20,
    Pointer = 22,
}

impl ValueKind {
    #[inline]
    pub fn from_u8(v: u8) -> Self {
        Self::try_from(v).unwrap_or(ValueKind::Void)
    }

    #[inline]
    pub fn may_contain_gc_refs(&self) -> bool {
        (*self as u8) > Self::FuncPtr as u8
    }

    pub fn fixed_slot_count(&self) -> u16 {
        match self {
            Self::Array | Self::Struct => {
                panic!("Array/Struct have variable slot count, use TypeMeta")
            }
            Self::Interface => 2,
            _ => 1,
        }
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Self::Int
                | Self::Int8
                | Self::Int16
                | Self::Int32
                | Self::Int64
                | Self::Uint
                | Self::Uint8
                | Self::Uint16
                | Self::Uint32
                | Self::Uint64
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float32 | Self::Float64)
    }

    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    pub fn elem_bytes(&self) -> usize {
        match self {
            Self::Bool | Self::Int8 | Self::Uint8 => 1,
            Self::Int16 | Self::Uint16 => 2,
            Self::Int32 | Self::Uint32 | Self::Float32 => 4,
            _ => 8,
        }
    }
}

/// Slot type for GC stack scanning.
///
/// Interface0 and Interface1 are paired:
/// - Interface0: header slot (contains value_kind)
/// - Interface1: data slot (scan depends on Interface0's value_kind)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, TryFromPrimitive)]
#[repr(u8)]
pub enum SlotType {
    #[default]
    Value = 0,
    GcRef = 1,
    Interface0 = 2,
    Interface1 = 3,
}

impl SlotType {
    #[inline]
    pub fn from_u8(v: u8) -> Self {
        Self::try_from(v).unwrap_or(SlotType::Value)
    }

    #[inline]
    pub fn is_gc_ref(&self) -> bool {
        matches!(self, SlotType::GcRef)
    }

    #[inline]
    pub fn is_interface_data(&self) -> bool {
        matches!(self, SlotType::Interface1)
    }
}
