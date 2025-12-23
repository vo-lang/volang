//! Common type definitions shared across the Vo compiler and runtime.
//! Reference: docs/spec/memory-model-and-instructions.md

use num_enum::TryFromPrimitive;

// =============================================================================
// Compiler IDs
// =============================================================================

/// Expression unique ID (assigned by Parser).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ExprId(pub u32);

impl ExprId {
    pub const DUMMY: ExprId = ExprId(u32::MAX);
}

/// Type expression unique ID (assigned by Parser).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TypeExprId(pub u32);

impl TypeExprId {
    pub const DUMMY: TypeExprId = TypeExprId(u32::MAX);
}

/// Type unique ID (assigned by TypeInterner).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TypeId(pub u32);

impl TypeId {
    pub const INVALID: TypeId = TypeId(u32::MAX);
}

// =============================================================================
// Runtime Types
// =============================================================================

/// Meta ID - 24-bit index into meta tables.
/// Stored in GcHeader as 3 bytes.
///
/// Meaning depends on value_kind:
/// - Struct: indexes struct_metas[]
/// - Interface: indexes interface_metas[]
/// - Array (is_array=1): element's meta_id if element is Struct/Interface
/// - Others: 0
pub type MetaId = u32;

pub const INVALID_META_ID: MetaId = 0xFF_FFFF; // 24-bit max
pub const META_ID_MASK: MetaId = 0xFF_FFFF;    // 24-bit mask

// Backward compatibility alias
pub type RuntimeTypeId = MetaId;
pub const INVALID_RUNTIME_TYPE_ID: RuntimeTypeId = INVALID_META_ID;

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
    Nil = 0,
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
        Self::try_from(v).unwrap_or(ValueKind::Nil)
    }

    #[inline]
    pub fn is_ref_type(&self) -> bool {
        matches!(
            self,
            Self::String
                | Self::Slice
                | Self::Map
                | Self::Channel
                | Self::Closure
                | Self::Pointer
        )
    }

    #[inline]
    pub fn is_value_type(&self) -> bool {
        !self.is_ref_type()
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
