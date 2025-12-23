//! Interface operations.
//!
//! Interface is a value type (2 slots on stack):
//! - Slot 0: header = pack(iface_meta_id:24, reserved:8, value_meta_id:24, value_kind:8)
//! - Slot 1: data = immediate value or GcRef

use vo_common_core::types::ValueKind;

pub const SLOT_COUNT: usize = 2;

/// Pack interface slot0.
/// Layout (64 bits):
///   High 32: iface_meta_id (24) | reserved (8)
///   Low 32:  value_meta_id (24) | value_kind (8)
#[inline]
pub fn pack_slot0(iface_meta_id: u32, value_meta_id: u32, value_kind: u8) -> u64 {
    ((iface_meta_id as u64 & 0xFFFFFF) << 40)
        | ((value_meta_id as u64 & 0xFFFFFF) << 8)
        | (value_kind as u64)
}

#[inline]
pub fn unpack_iface_meta_id(slot0: u64) -> u32 {
    ((slot0 >> 40) & 0xFFFFFF) as u32
}

#[inline]
pub fn unpack_value_meta_id(slot0: u64) -> u32 {
    ((slot0 >> 8) & 0xFFFFFF) as u32
}

#[inline]
pub fn unpack_value_kind(slot0: u64) -> ValueKind {
    ValueKind::from_u8(slot0 as u8)
}

// Backward compatibility aliases
#[inline]
pub fn unpack_iface_type_id(slot0: u64) -> u32 {
    unpack_iface_meta_id(slot0)
}

#[inline]
pub fn unpack_value_type_id(slot0: u64) -> u32 {
    unpack_value_meta_id(slot0)
}

/// Box a value into interface slots.
/// Returns (slot0, slot1).
#[inline]
pub fn box_value(iface_meta_id: u32, value_meta_id: u32, value_kind: u8, data: u64) -> (u64, u64) {
    (pack_slot0(iface_meta_id, value_meta_id, value_kind), data)
}

/// Unbox data from interface slot1.
#[inline]
pub fn unbox_data(slot1: u64) -> u64 {
    slot1
}

/// Check if interface is nil.
#[inline]
pub fn is_nil(slot0: u64) -> bool {
    unpack_value_kind(slot0) == ValueKind::Nil
}

/// Check if the data slot contains a GC reference.
#[inline]
pub fn data_is_gc_ref(slot0: u64) -> bool {
    unpack_value_kind(slot0).is_ref_type()
}
