//! Common type definitions shared across the GoX compiler.

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

/// Runtime type ID - index into meta tables.
/// Only meaningful for Struct and Interface:
/// - Struct: indexes struct_metas[]
/// - Interface: indexes interface_metas[]
pub type RuntimeTypeId = u16;

pub const INVALID_RUNTIME_TYPE_ID: RuntimeTypeId = u16::MAX;

/// Value kind - runtime classification of GoX values.
///
/// Layout: primitives (0-14) followed by reference types (15+).
/// This allows `needs_gc()` to use a simple comparison.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, TryFromPrimitive)]
#[repr(u8)]
pub enum ValueKind {
    // Primitives (no GC needed)
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
    FuncPtr = 14,   // bare function pointer (no captures, no GC)

    // Reference types (need GC)
    String = 15,
    Array = 16,
    Slice = 17,
    Map = 18,
    Channel = 19,
    Closure = 20,   // closure with captures
    Struct = 21,
    Pointer = 22,   // *StructType
    Interface = 23,
}

impl ValueKind {
    #[inline]
    pub fn from_u8(v: u8) -> Self {
        Self::try_from(v).unwrap_or(ValueKind::Nil)
    }

    /// Whether this type needs GC scanning.
    #[inline]
    pub fn needs_gc(&self) -> bool {
        (*self as u8) >= Self::String as u8
    }

    /// Whether this type has a RuntimeTypeId (only Struct/Interface).
    #[inline]
    pub fn has_type_id(&self) -> bool {
        matches!(self, Self::Struct | Self::Interface)
    }

    /// Is this an integer type?
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

    /// Is this a floating-point type?
    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float32 | Self::Float64)
    }

    /// Is this a numeric type (integer or float)?
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    /// Number of register slots needed for this type.
    /// Interface needs 2 slots (type_id + data), others need 1.
    pub fn slot_count(&self) -> u16 {
        if *self == Self::Interface {
            2
        } else {
            1
        }
    }

    /// Byte size for array element storage.
    pub fn elem_bytes(&self) -> usize {
        match self {
            Self::Bool | Self::Int8 | Self::Uint8 => 1,
            Self::Int16 | Self::Uint16 => 2,
            Self::Int32 | Self::Uint32 | Self::Float32 => 4,
            _ => 8,
        }
    }
}

/// Slot type for GC scanning.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, TryFromPrimitive)]
#[repr(u8)]
pub enum SlotType {
    /// Non-pointer value (int, float, bool). No scanning needed.
    #[default]
    Value = 0,
    /// GC-managed pointer (string, slice, map, struct, *T, closure, chan). Must be scanned.
    GcRef = 1,
    /// First slot of interface (packed type info). Not a pointer, skip.
    Interface0 = 2,
    /// Second slot of interface (data). May be pointer depending on value_kind.
    Interface1 = 3,
}

impl SlotType {
    /// Check if this slot type needs GC scanning.
    /// Note: Interface1 requires runtime check of value_kind.
    #[inline]
    pub fn needs_scan(&self) -> bool {
        matches!(self, SlotType::GcRef | SlotType::Interface1)
    }

    #[inline]
    pub fn from_u8(v: u8) -> Self {
        Self::try_from(v).unwrap_or(SlotType::Value)
    }
}
