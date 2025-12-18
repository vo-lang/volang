//! Common type definitions shared across the GoX compiler.

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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

/// First type_id reserved for user-defined types.
/// Type IDs below this are builtin types (ValueKind enum values).
pub const FIRST_USER_TYPE_ID: u32 = 32;

impl ValueKind {
    /// Create a ValueKind from its u8 representation.
    pub fn from_u8(v: u8) -> Self {
        match v {
            0 => ValueKind::Nil,
            1 => ValueKind::Bool,
            2 => ValueKind::Int,
            3 => ValueKind::Int8,
            4 => ValueKind::Int16,
            5 => ValueKind::Int32,
            6 => ValueKind::Int64,
            7 => ValueKind::Uint,
            8 => ValueKind::Uint8,
            9 => ValueKind::Uint16,
            10 => ValueKind::Uint32,
            11 => ValueKind::Uint64,
            12 => ValueKind::Float32,
            13 => ValueKind::Float64,
            14 => ValueKind::String,
            15 => ValueKind::Slice,
            16 => ValueKind::Map,
            17 => ValueKind::Struct,
            18 => ValueKind::Pointer,
            19 => ValueKind::Interface,
            20 => ValueKind::Array,
            21 => ValueKind::Channel,
            22 => ValueKind::Closure,
            _ => ValueKind::Nil,
        }
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

    /// Is this a reference type (GC-managed)?
    pub fn is_reference(&self) -> bool {
        matches!(
            self,
            Self::String
                | Self::Slice
                | Self::Map
                | Self::Array
                | Self::Channel
                | Self::Closure
                | Self::Pointer
        )
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

