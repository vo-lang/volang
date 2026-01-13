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
/// 
/// The meaning of `meta_id` depends on `value_kind`:
/// - Struct/Pointer: `struct_meta_id` - index into `struct_metas[]` (for GC scan)
/// - Interface: `iface_meta_id` - index into `interface_metas[]` (for itab lookup)
/// - Other types: 0 (unused, never store rttid here to avoid confusion)
/// 
/// Note: For runtime type identification, use `ValueRttid` which stores `rttid`.
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

/// Runtime type ID with value kind - packed 32-bit representation.
/// Layout: [rttid:24 | value_kind:8]
/// Used in FieldMeta for dynamic access.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct ValueRttid(u32);

impl ValueRttid {
    #[inline]
    pub fn new(rttid: u32, value_kind: ValueKind) -> Self {
        Self(((rttid & META_ID_MASK) << 8) | (value_kind as u32))
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
    pub fn rttid(self) -> u32 {
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

    // === Compound Value Types (multi-slot, may contain GC refs) ===
    Array = 14,
    Struct = 15,
    Interface = 16,

    // === Reference Types (1 slot GcRef, heap allocated) ===
    String = 17,
    Slice = 18,
    Map = 19,
    Channel = 20,
    Closure = 21,
    Pointer = 22,
}

impl ValueKind {
    /// Basic types that don't have internal type info.
    /// Used for pre-registering RuntimeType::Basic in TypeInterner.
    pub const BASIC: [ValueKind; 15] = [
        Self::Void, Self::Bool, Self::Int, Self::Int8, Self::Int16, Self::Int32, Self::Int64,
        Self::Uint, Self::Uint8, Self::Uint16, Self::Uint32, Self::Uint64,
        Self::Float32, Self::Float64, Self::String,
    ];
    
    #[inline]
    pub fn from_u8(v: u8) -> Self {
        Self::try_from(v).unwrap_or(ValueKind::Void)
    }

    #[inline]
    pub fn may_contain_gc_refs(&self) -> bool {
        (*self as u8) >= Self::Array as u8
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

    /// Returns true if this type needs boxing (heap allocation) when assigned to interface.
    /// Struct/Array are multi-slot and need to be copied to heap via PtrNew.
    /// Other types (primitives, references) are stored directly in interface slot1.
    #[inline]
    pub fn needs_boxing(&self) -> bool {
        matches!(self, Self::Struct | Self::Array)
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

    /// Check if this is a signed integer type that needs sign extension when widening
    /// Uses range check: Int=2, Int8=3, Int16=4, Int32=5, Int64=6
    #[inline]
    pub fn is_signed_int(&self) -> bool {
        let v = *self as u8;
        v >= 2 && v <= 6
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

// =============================================================================
// Element Access Flags (for ArrayGet/Set, SliceGet/Set, ArrayNew, SliceNew, etc.)
// =============================================================================
//
// Bit layout (8 bits):
//   bit 7 (0x80): SIGN_BIT  - needs sign extension (for signed integers)
//   bit 6 (0x40): FLOAT_BIT - is floating point (only float32 needs special handling)
//   bit 5-0 (0x3F): elem_bytes (0-63)
//
// Values:
//   0x00 = 0   = dynamic (elem_bytes > 63, read from header at runtime)
//   0x01 = 1   = uint8/bool (1 byte, zero extend)
//   0x02 = 2   = uint16 (2 bytes, zero extend)
//   0x04 = 4   = uint32 (4 bytes, zero extend)
//   0x08 = 8   = int64/uint64/float64/pointer (8 bytes, no extension)
//   9-63       = multi-slot struct/array (elem_bytes, direct copy)
//
//   0x81 = 129 = int8 (1 byte, sign extend)
//   0x82 = 130 = int16 (2 bytes, sign extend)
//   0x84 = 132 = int32 (4 bytes, sign extend)
//
//   0x44 = 68  = float32 (4 bytes, stored as u32 bits)
//
// Decoding:
//   is_signed = (flags & 0x80) != 0
//   is_float  = (flags & 0x40) != 0
//   elem_bytes = (flags & 0x3F) as usize
//
// Note: For Get operations, signed integers need sign extension to i64.
//       For Set operations, all same-size types are handled identically.

pub const ELEM_FLAG_SIGN_BIT: u8 = 0x80;
pub const ELEM_FLAG_FLOAT_BIT: u8 = 0x40;
pub const ELEM_FLAG_BYTES_MASK: u8 = 0x3F;

pub const ELEM_FLAG_INT8: u8 = ELEM_FLAG_SIGN_BIT | 1;     // 0x81 = 129
pub const ELEM_FLAG_INT16: u8 = ELEM_FLAG_SIGN_BIT | 2;    // 0x82 = 130
pub const ELEM_FLAG_INT32: u8 = ELEM_FLAG_SIGN_BIT | 4;    // 0x84 = 132
pub const ELEM_FLAG_FLOAT32: u8 = ELEM_FLAG_FLOAT_BIT | 4; // 0x44 = 68

/// Convert elem_bytes and ValueKind to flags for instructions.
#[inline]
pub fn elem_flags(elem_bytes: usize, vk: ValueKind) -> u8 {
    match vk {
        ValueKind::Int8 => ELEM_FLAG_INT8,
        ValueKind::Int16 => ELEM_FLAG_INT16,
        ValueKind::Int32 => ELEM_FLAG_INT32,
        ValueKind::Float32 => ELEM_FLAG_FLOAT32,
        _ => if elem_bytes > (ELEM_FLAG_BYTES_MASK as usize) { 0 } else { elem_bytes as u8 }
    }
}
