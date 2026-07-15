#![allow(clippy::missing_safety_doc, clippy::not_unsafe_ptr_arg_deref)]
//! Struct operations.
//!
//! Struct layout on heap: [field0, field1, ...]
//! GcHeader.meta_id contains the MetaId for type metadata lookup.
//!
//! # Safety contract
//! Unsafe operations require canonical live struct allocations matching the
//! supplied `StructMeta` slot layout.

use crate::gc::{Gc, GcRef};
use vo_common_core::types::{ValueKind, ValueMeta};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StructCreateError {
    SizeTooLarge { size_slots: usize },
    AllocationFailed { size_slots: u16 },
}

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
pub fn try_create(
    gc: &mut Gc,
    meta_id: u32,
    size_slots: usize,
) -> Result<GcRef, StructCreateError> {
    let size_slots =
        u16::try_from(size_slots).map_err(|_| StructCreateError::SizeTooLarge { size_slots })?;
    let object = gc.alloc(ValueMeta::new(meta_id, ValueKind::Struct), size_slots);
    if object.is_null() {
        return Err(StructCreateError::AllocationFailed { size_slots });
    }
    Ok(object)
}

pub fn create(gc: &mut Gc, meta_id: u32, size_slots: usize) -> GcRef {
    try_create(gc, meta_id, size_slots).unwrap_or_else(|error| match error {
        StructCreateError::SizeTooLarge { size_slots } => {
            panic!("struct slot count {size_slots} exceeds u16::MAX")
        }
        StructCreateError::AllocationFailed { size_slots } => {
            panic!("struct allocation failed for {size_slots} slots")
        }
    })
}

/// Get field value (safe for FFI use).
#[inline]
pub unsafe fn get_field(obj: GcRef, idx: usize) -> u64 {
    unsafe { Gc::read_slot(obj, idx) }
}

/// Set field value.
///
/// # Safety
/// Caller must ensure `obj` is a valid struct object and either apply the
/// required write barrier before publishing a GC-visible field, or only use
/// this during fresh object initialization before the object is scanned.
#[inline]
pub unsafe fn set_field(obj: GcRef, idx: usize, val: u64) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn struct_allocation_preserves_u16_max_and_rejects_the_next_width() {
        let mut gc = Gc::new();
        let object = try_create(&mut gc, 0, u16::MAX as usize)
            .expect("the complete u16 struct slot domain must remain available");
        assert!(!object.is_null());
        assert_eq!(unsafe { Gc::header(object) }.slots, u16::MAX);

        assert_eq!(
            try_create(&mut gc, 0, u16::MAX as usize + 1),
            Err(StructCreateError::SizeTooLarge {
                size_slots: u16::MAX as usize + 1,
            })
        );
    }

    #[test]
    fn raw_struct_set_field_is_unsafe_public_primitive_058() {
        let source = vo_source_contract::production_source_without_test_modules(include_str!(
            "struct_ops.rs"
        ));
        assert!(
            source.contains("pub unsafe fn set_field("),
            "raw struct field mutation must stay behind an unsafe contract"
        );
        assert!(
            source.contains("required write barrier"),
            "set_field safety docs must name the write-barrier obligation"
        );
    }
}
