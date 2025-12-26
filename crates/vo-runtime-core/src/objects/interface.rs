//! Interface operations.
//!
//! Interface is a value type (2 slots on stack):
//! - Slot 0: [itab_id:32 | named_type_id:24 | value_kind:8]
//! - Slot 1: data = immediate value or GcRef
//!
//! nil check: value_kind == Void (same as Go: typed nil is NOT nil interface)

use vo_common_core::types::ValueKind;

pub const SLOT_COUNT: usize = 2;

/// Pack slot0 from itab_id, named_type_id, and value_kind
#[inline]
pub fn pack_slot0(itab_id: u32, named_type_id: u32, vk: ValueKind) -> u64 {
    ((itab_id as u64) << 32) | ((named_type_id as u64) << 8) | (vk as u64)
}

/// Extract itab_id from slot0 (high 32 bits)
#[inline]
pub fn unpack_itab_id(slot0: u64) -> u32 {
    (slot0 >> 32) as u32
}

/// Extract named_type_id from slot0 (bits 8-31)
#[inline]
pub fn unpack_named_type_id(slot0: u64) -> u32 {
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
