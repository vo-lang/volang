//! Common type definitions shared across the GoX compiler.

use num_enum::TryFromPrimitive;

// =============================================================================
// Compiler IDs (for type system refactoring)
// =============================================================================

/// Expression unique ID (assigned by Parser).
/// Used as key for expression → type mapping.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ExprId(pub u32);

impl ExprId {
    pub const DUMMY: ExprId = ExprId(u32::MAX);
}

/// Type expression unique ID (assigned by Parser).
/// Used as key for type expression → TypeId mapping.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TypeExprId(pub u32);

impl TypeExprId {
    pub const DUMMY: TypeExprId = TypeExprId(u32::MAX);
}

/// Type unique ID (assigned by TypeInterner).
/// Structurally equal types share the same TypeId.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TypeId(pub u32);

impl TypeId {
    pub const INVALID: TypeId = TypeId(u32::MAX);
}

// =============================================================================
// Runtime Types
// =============================================================================

/// Value kind - the runtime classification of GoX values.
///
/// This is a simplified type tag used for:
/// - Code generation (register allocation, instruction selection)
/// - FFI (argument passing, return value handling)
/// - VM runtime (GC, type checking)
///
/// Unlike `gox_analysis::Type` which carries full type information
/// (generics, fields, methods), `ValueKind` is a flat enum suitable
/// for runtime operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, TryFromPrimitive)]
#[repr(u8)]
pub enum ValueKind {
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
    String = 14,
    Slice = 15,
    Map = 16,
    Struct = 17,
    Pointer = 18,
    Interface = 19,
    Array = 20,
    Channel = 21,
    Closure = 22,
}

impl ValueKind {
    /// Create a ValueKind from its u8 representation.
    #[inline]
    pub fn from_u8(v: u8) -> Self {
        Self::try_from(v).unwrap_or(ValueKind::Nil)
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
    /// Returns actual byte size for primitives, 8 for references.
    pub fn elem_bytes(&self) -> usize {
        match self {
            Self::Bool | Self::Int8 | Self::Uint8 => 1,
            Self::Int16 | Self::Uint16 => 2,
            Self::Int32 | Self::Uint32 | Self::Float32 => 4,
            // 64-bit types and all references use 8 bytes
            _ => 8,
        }
    }
}

/// Runtime type ID used for GC and type checks.
///
/// Layout:
/// - 0-13: primitives (no GC needed)
/// - 14+: reference types (GC needed)
/// - 100+: user-defined structs
/// - 2^31+: user-defined interfaces
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum RuntimeTypeId {
    Nil = ValueKind::Nil as u32,
    Bool = ValueKind::Bool as u32,
    Int = ValueKind::Int as u32,
    Int8 = ValueKind::Int8 as u32,
    Int16 = ValueKind::Int16 as u32,
    Int32 = ValueKind::Int32 as u32,
    Int64 = ValueKind::Int64 as u32,
    Uint = ValueKind::Uint as u32,
    Uint8 = ValueKind::Uint8 as u32,
    Uint16 = ValueKind::Uint16 as u32,
    Uint32 = ValueKind::Uint32 as u32,
    Uint64 = ValueKind::Uint64 as u32,
    Float32 = ValueKind::Float32 as u32,
    Float64 = ValueKind::Float64 as u32,
    String = ValueKind::String as u32,
    Slice = ValueKind::Slice as u32,
    Map = ValueKind::Map as u32,
    // 17 = Struct (use FirstStruct instead)
    // 18 = Pointer (removed - use struct's type_id directly)
    // 19 = Interface (use FirstInterface instead)
    Array = ValueKind::Array as u32,
    Channel = ValueKind::Channel as u32,
    Closure = ValueKind::Closure as u32,
    // User-defined structs: 100..(2^31-1)
    FirstStruct = 100,
    // User-defined interfaces: 2^31+
    FirstInterface = 0x8000_0000,
}

impl RuntimeTypeId {
    /// Check if this type needs GC scanning.
    #[inline]
    pub fn needs_gc(type_id: u32) -> bool {
        type_id >= Self::String as u32
    }
    
    /// Check if this is a user-defined struct type.
    #[inline]
    pub fn is_struct(type_id: u32) -> bool {
        type_id >= Self::FirstStruct as u32 && type_id < Self::FirstInterface as u32
    }
    
    /// Check if this is a user-defined interface type.
    #[inline]
    pub fn is_interface(type_id: u32) -> bool {
        type_id >= Self::FirstInterface as u32
    }
}


/// Register/slot type for GC scanning.
/// 
/// Used for both stack scanning (function slot_types) and heap object scanning
/// (struct slot_types). Tells the GC whether a slot contains a pointer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, TryFromPrimitive)]
#[repr(u8)]
pub enum SlotType {
    /// Non-pointer value (int, float, bool). No scanning needed.
    #[default]
    Value = 0,
    /// GC-managed pointer (string, slice, map, *T, closure, chan). Must be scanned.
    GcRef = 1,
    /// First slot of interface (packed type_ids). Not a pointer, skip.
    Interface0 = 2,
    /// Second slot of interface (data). May be pointer depending on value_type.
    /// Requires dynamic check: if RuntimeTypeId::needs_gc(slot[i-1] as u32) then scan.
    Interface1 = 3,
}

impl SlotType {
    /// Check if this slot type needs GC scanning.
    /// Note: Interface1 may or may not need scanning (requires runtime check).
    #[inline]
    pub fn needs_scan(&self) -> bool {
        matches!(self, SlotType::GcRef | SlotType::Interface1)
    }
    
    /// Convert from u8.
    #[inline]
    pub fn from_u8(v: u8) -> Self {
        Self::try_from(v).unwrap_or(SlotType::Value)
    }
}
