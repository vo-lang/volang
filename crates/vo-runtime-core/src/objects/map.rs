//! Map object operations.
//!
//! Layout: GcHeader + MapData
//! Uses hashbrown HashMap (no_std compatible, unordered - matches Go map semantics).

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
#[cfg(feature = "std")]
use std::boxed::Box;

use hashbrown::HashMap;

use crate::gc::{Gc, GcRef};
use vo_common_core::types::{ValueKind, ValueMeta};

type MapInner = HashMap<u64, u64>;

#[repr(C)]
pub struct MapData {
    pub inner: *mut MapInner,
    pub key_meta: ValueMeta,
    pub val_meta: ValueMeta,
}

const DATA_SLOTS: u16 = 2;
const _: () = assert!(core::mem::size_of::<MapData>() == DATA_SLOTS as usize * 8);

impl MapData {
    #[inline]
    fn as_ref(m: GcRef) -> &'static Self {
        unsafe { &*(m as *const Self) }
    }

    #[inline]
    fn as_mut(m: GcRef) -> &'static mut Self {
        unsafe { &mut *(m as *mut Self) }
    }
}

pub fn create(gc: &mut Gc, key_meta: ValueMeta, val_meta: ValueMeta) -> GcRef {
    let m = gc.alloc(ValueMeta::new(0, ValueKind::Map), DATA_SLOTS);
    let inner = Box::new(MapInner::new());
    let data = MapData::as_mut(m);
    data.inner = Box::into_raw(inner);
    data.key_meta = key_meta;
    data.val_meta = val_meta;
    m
}

#[inline]
pub fn key_meta(m: GcRef) -> ValueMeta { MapData::as_ref(m).key_meta }
#[inline]
pub fn val_meta(m: GcRef) -> ValueMeta { MapData::as_ref(m).val_meta }
#[inline]
pub fn key_kind(m: GcRef) -> ValueKind { key_meta(m).value_kind() }
#[inline]
pub fn val_kind(m: GcRef) -> ValueKind { val_meta(m).value_kind() }

#[inline]
fn get_inner(m: GcRef) -> &'static mut MapInner {
    unsafe { &mut *MapData::as_ref(m).inner }
}

pub fn len(m: GcRef) -> usize { get_inner(m).len() }
pub fn get(m: GcRef, key: u64) -> Option<u64> { get_inner(m).get(&key).copied() }
pub fn get_with_ok(m: GcRef, key: u64) -> (u64, bool) {
    match get_inner(m).get(&key) {
        Some(&v) => (v, true),
        None => (0, false),
    }
}
pub fn set(m: GcRef, key: u64, val: u64) { get_inner(m).insert(key, val); }
pub fn delete(m: GcRef, key: u64) { get_inner(m).remove(&key); }
pub fn contains(m: GcRef, key: u64) -> bool { get_inner(m).contains_key(&key) }
pub fn iter_at(m: GcRef, idx: usize) -> Option<(u64, u64)> {
    get_inner(m).iter().nth(idx).map(|(&k, &v)| (k, v))
}

#[cfg(feature = "std")]
pub unsafe fn drop_inner(m: GcRef) {
    let data = MapData::as_mut(m);
    if !data.inner.is_null() {
        drop(Box::from_raw(data.inner));
        data.inner = core::ptr::null_mut();
    }
}
