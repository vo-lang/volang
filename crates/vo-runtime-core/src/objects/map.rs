//! Map object operations.
//!
//! Map layout: [inner_ptr, key_kind, val_kind] (3 slots)
//! Uses IndexMap for ordered iteration (Go map iteration order).

#[cfg(feature = "std")]
use indexmap::IndexMap;

use crate::gc::{Gc, GcRef};
use vo_common_core::types::ValueKind;

pub const SLOT_INNER: usize = 0;
pub const SLOT_KEY_KIND: usize = 1;
pub const SLOT_VAL_KIND: usize = 2;
pub const SLOT_COUNT: u16 = 3;

#[cfg(feature = "std")]
type MapInner = IndexMap<u64, u64>;

#[cfg(feature = "std")]
pub fn create(gc: &mut Gc, key_kind: u8, val_kind: u8) -> GcRef {
    let m = gc.alloc(ValueKind::Map as u8, 0, SLOT_COUNT);
    let inner = Box::new(MapInner::new());
    Gc::write_slot(m, SLOT_INNER, Box::into_raw(inner) as u64);
    Gc::write_slot(m, SLOT_KEY_KIND, key_kind as u64);
    Gc::write_slot(m, SLOT_VAL_KIND, val_kind as u64);
    m
}

#[cfg(feature = "std")]
fn get_inner(m: GcRef) -> &'static mut MapInner {
    let ptr = Gc::read_slot(m, SLOT_INNER) as *mut MapInner;
    unsafe { &mut *ptr }
}

#[cfg(feature = "std")]
pub fn len(m: GcRef) -> usize {
    get_inner(m).len()
}

#[cfg(feature = "std")]
pub fn get(m: GcRef, key: u64) -> Option<u64> {
    get_inner(m).get(&key).copied()
}

#[cfg(feature = "std")]
pub fn get_with_ok(m: GcRef, key: u64) -> (u64, bool) {
    match get_inner(m).get(&key) {
        Some(&v) => (v, true),
        None => (0, false),
    }
}

#[cfg(feature = "std")]
pub fn set(m: GcRef, key: u64, val: u64) {
    get_inner(m).insert(key, val);
}

#[cfg(feature = "std")]
pub fn delete(m: GcRef, key: u64) {
    get_inner(m).swap_remove(&key);
}

#[cfg(feature = "std")]
pub fn contains(m: GcRef, key: u64) -> bool {
    get_inner(m).contains_key(&key)
}

#[cfg(feature = "std")]
pub fn iter_at(m: GcRef, idx: usize) -> Option<(u64, u64)> {
    get_inner(m).get_index(idx).map(|(&k, &v)| (k, v))
}

pub fn key_kind(m: GcRef) -> ValueKind {
    ValueKind::from_u8(Gc::read_slot(m, SLOT_KEY_KIND) as u8)
}

pub fn val_kind(m: GcRef) -> ValueKind {
    ValueKind::from_u8(Gc::read_slot(m, SLOT_VAL_KIND) as u8)
}

#[cfg(feature = "std")]
pub unsafe fn drop_inner(m: GcRef) {
    let ptr = Gc::read_slot(m, SLOT_INNER) as *mut MapInner;
    if !ptr.is_null() {
        drop(Box::from_raw(ptr));
        Gc::write_slot(m, SLOT_INNER, 0);
    }
}
