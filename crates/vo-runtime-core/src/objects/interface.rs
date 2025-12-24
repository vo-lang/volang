//! Interface operations.
//!
//! Interface is a value type (2 slots on stack):
//! - Slot 0: [iface_meta_id:24 | reserved:8 | ValueMeta:32]
//! - Slot 1: data = immediate value or GcRef

use vo_common_core::types::{ValueKind, ValueMeta};

pub const SLOT_COUNT: usize = 2;

#[inline]
pub fn pack_slot0(iface_meta_id: u32, value_meta: ValueMeta) -> u64 {
    ((iface_meta_id as u64 & 0xFFFFFF) << 40) | (value_meta.to_raw() as u64)
}

#[inline]
pub fn unpack_iface_meta_id(slot0: u64) -> u32 {
    ((slot0 >> 40) & 0xFFFFFF) as u32
}

#[inline]
pub fn unpack_value_meta(slot0: u64) -> ValueMeta {
    ValueMeta::from_raw(slot0 as u32)
}

#[inline]
pub fn unpack_value_kind(slot0: u64) -> ValueKind {
    unpack_value_meta(slot0).value_kind()
}

#[inline]
pub fn is_nil(slot0: u64) -> bool {
    unpack_value_kind(slot0) == ValueKind::Nil
}

#[inline]
pub fn data_is_gc_ref(slot0: u64) -> bool {
    unpack_value_kind(slot0).is_ref_type()
}
