//! Struct operations.
//!
//! Struct layout on heap: [field0, field1, ...]
//! GcHeader.meta_id contains the MetaId for type metadata lookup.

use crate::gc::{Gc, GcRef};
use vo_common_core::types::{ValueKind, ValueMeta};

/// Compute hash for a struct value (based on field values, not pointer).
/// Uses FxHash algorithm - fast polynomial hash for integers.
/// # Safety
/// obj must be a valid GcRef.
pub unsafe fn hash(obj: GcRef, field_count: usize) -> u64 {
    const K: u64 = 0xf1357aea2e62a9c5;
    const SEED: u64 = 0x517cc1b727220a95;

    let mut h = SEED;
    for i in 0..field_count {
        let val = unsafe { Gc::read_slot(obj, i) };
        h = h.wrapping_add(val).wrapping_mul(K);
    }
    h.rotate_left(5)
}

/// Create a new struct with zero-initialized fields.
pub fn create(gc: &mut Gc, meta_id: u32, size_slots: usize) -> GcRef {
    gc.alloc(ValueMeta::new(meta_id, ValueKind::Struct), size_slots as u16)
}

/// Get field value (safe for FFI use).
#[inline]
pub fn get_field(obj: GcRef, idx: usize) -> u64 {
    unsafe { Gc::read_slot(obj, idx) }
}

/// Set field value (safe for FFI use).
#[inline]
pub fn set_field(obj: GcRef, idx: usize, val: u64) {
    unsafe { Gc::write_slot(obj, idx, val) }
}

/// Get multiple fields (for nested struct).
/// # Safety
/// obj must be a valid GcRef and range must be within bounds.
pub unsafe fn get_fields(obj: GcRef, start: usize, dest: &mut [u64]) {
    for (i, d) in dest.iter_mut().enumerate() {
        *d = Gc::read_slot(obj, start + i);
    }
}

/// Set multiple fields (for nested struct).
/// # Safety
/// obj must be a valid GcRef and range must be within bounds.
pub unsafe fn set_fields(obj: GcRef, start: usize, src: &[u64]) {
    for (i, &s) in src.iter().enumerate() {
        Gc::write_slot(obj, start + i, s);
    }
}
