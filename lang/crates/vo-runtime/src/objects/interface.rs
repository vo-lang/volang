//! Interface operations and InterfaceSlot type.
//!
//! Interface is a value type (2 slots on stack):
//! - Slot 0: [itab_id:32 | rttid:24 | value_kind:8]
//! - Slot 1: data = immediate value or GcRef
//!
//! nil check: value_kind == Void (same as Go: typed nil is NOT nil interface)

use vo_common_core::types::ValueKind;
use crate::gc::GcRef;
use super::string;

pub const SLOT_COUNT: usize = 2;

// =============================================================================
// InterfaceSlot - Core interface value type
// =============================================================================

/// Represents a Vo interface value with 2 slots.
///
/// All Vo interfaces (`any`, `error`, named interfaces) share this layout:
/// - slot0: `[itab_id:32 | rttid:24 | value_kind:8]`
/// - slot1: data (immediate value or GcRef)
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct InterfaceSlot {
    /// Metadata: `[itab_id:32 | rttid:24 | value_kind:8]`
    pub slot0: u64,
    /// Data: immediate value or GcRef
    pub slot1: u64,
}

impl InterfaceSlot {
    /// Create a new InterfaceSlot from raw slot values.
    #[inline]
    pub fn new(slot0: u64, slot1: u64) -> Self {
        Self { slot0, slot1 }
    }

    /// Create an InterfaceSlot containing an i64 value.
    #[inline]
    pub fn from_i64(val: i64) -> Self {
        Self {
            slot0: pack_slot0(0, ValueKind::Int as u32, ValueKind::Int),
            slot1: val as u64,
        }
    }

    /// Create an InterfaceSlot containing a bool value.
    #[inline]
    pub fn from_bool(val: bool) -> Self {
        Self {
            slot0: pack_slot0(0, ValueKind::Bool as u32, ValueKind::Bool),
            slot1: val as u64,
        }
    }

    /// Create an InterfaceSlot containing a f64 value.
    #[inline]
    pub fn from_f64(val: f64) -> Self {
        Self {
            slot0: pack_slot0(0, ValueKind::Float64 as u32, ValueKind::Float64),
            slot1: val.to_bits(),
        }
    }

    /// Create an InterfaceSlot containing a GcRef (for reference types).
    #[inline]
    pub fn from_ref(gc_ref: GcRef, rttid: u32, vk: ValueKind) -> Self {
        Self {
            slot0: pack_slot0(0, rttid, vk),
            slot1: gc_ref as u64,
        }
    }

    /// Create a nil InterfaceSlot.
    #[inline]
    pub fn nil() -> Self {
        Self { slot0: 0, slot1: 0 }
    }

    /// Check if this is a nil interface (slot0 == 0 means vk == Void).
    #[inline]
    pub fn is_nil(&self) -> bool {
        self.slot0 == 0
    }

    /// Get the value kind.
    #[inline]
    pub fn value_kind(&self) -> ValueKind {
        unpack_value_kind(self.slot0)
    }

    /// Get the runtime type ID.
    #[inline]
    pub fn rttid(&self) -> u32 {
        unpack_rttid(self.slot0)
    }

    /// Get the itab ID (for interface method dispatch).
    #[inline]
    pub fn itab_id(&self) -> u32 {
        unpack_itab_id(self.slot0)
    }

    /// Get the data as i64 (for Int types).
    #[inline]
    pub fn as_i64(&self) -> i64 {
        self.slot1 as i64
    }

    /// Get the data as u64.
    #[inline]
    pub fn as_u64(&self) -> u64 {
        self.slot1
    }

    /// Get the data as f64 (for Float types).
    #[inline]
    pub fn as_f64(&self) -> f64 {
        f64::from_bits(self.slot1)
    }

    /// Get the data as bool (for Bool types).
    #[inline]
    pub fn as_bool(&self) -> bool {
        self.slot1 != 0
    }

    /// Get the data as GcRef (for reference types).
    #[inline]
    pub fn as_ref(&self) -> GcRef {
        self.slot1 as GcRef
    }
    
    /// Get the data as string.
    #[inline]
    pub fn as_str(&self) -> &'static str {
        string::as_str(self.slot1 as GcRef)
    }
    
    // ---- Type checking ----
    
    /// Check if int.
    #[inline]
    pub fn is_int(&self) -> bool {
        self.value_kind() == ValueKind::Int
    }
    
    /// Check if float.
    #[inline]
    pub fn is_float(&self) -> bool {
        self.value_kind() == ValueKind::Float64
    }
    
    /// Check if bool.
    #[inline]
    pub fn is_bool(&self) -> bool {
        self.value_kind() == ValueKind::Bool
    }
    
    /// Check if string.
    #[inline]
    pub fn is_string(&self) -> bool {
        self.value_kind() == ValueKind::String
    }
    
    /// Check if reference type.
    #[inline]
    pub fn is_ref_type(&self) -> bool {
        matches!(self.value_kind(), 
            ValueKind::Slice | ValueKind::Map | ValueKind::Pointer |
            ValueKind::Struct | ValueKind::Array | ValueKind::Channel |
            ValueKind::Closure | ValueKind::String)
    }
    
    /// Create from u64.
    #[inline]
    pub fn from_u64(val: u64) -> Self {
        Self {
            slot0: pack_slot0(0, ValueKind::Int as u32, ValueKind::Int),
            slot1: val,
        }
    }
}

// =============================================================================
// Low-level interface slot operations
// =============================================================================

/// Pack slot0 from itab_id, rttid, and value_kind
#[inline]
pub fn pack_slot0(itab_id: u32, rttid: u32, vk: ValueKind) -> u64 {
    ((itab_id as u64) << 32) | ((rttid as u64) << 8) | (vk as u64)
}

/// Extract itab_id from slot0 (high 32 bits)
#[inline]
pub fn unpack_itab_id(slot0: u64) -> u32 {
    (slot0 >> 32) as u32
}

/// Extract rttid from slot0 (bits 8-31)
#[inline]
pub fn unpack_rttid(slot0: u64) -> u32 {
    ((slot0 >> 8) & 0xFFFFFF) as u32
}

/// Extract value_kind from slot0 (low 8 bits)
#[inline]
pub fn unpack_value_kind(slot0: u64) -> ValueKind {
    ValueKind::from_u8((slot0 & 0xFF) as u8)
}

/// Check if interface is nil (value_kind == Void)
/// Note: typed nil (e.g. (*T)(nil)) is NOT nil interface (same as Go)
#[inline]
pub fn is_nil(slot0: u64) -> bool {
    unpack_value_kind(slot0) == ValueKind::Void
}

/// Check if slot1 data is a GC reference
#[inline]
pub fn data_is_gc_ref(slot0: u64) -> bool {
    let vk = unpack_value_kind(slot0);
    vk.may_contain_gc_refs()
}
