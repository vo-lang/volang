//! Map object operations.
//!
//! Layout: GcHeader + MapData
//! Uses hashbrown HashMap (no_std compatible, unordered - matches Go map semantics).
//!
//! Supports two modes:
//! - SingleKey: key is 1 slot (common case, optimized)
//! - MultiKey: key is multiple slots (rare case)
//!
//! Value always supports multiple slots.

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
#[cfg(feature = "std")]
use std::boxed::Box;

use hashbrown::HashMap;

use crate::gc::{Gc, GcRef};
use crate::objects::string;
use vo_common_core::types::{ValueKind, ValueMeta};

type SingleKeyMap = HashMap<u64, Box<[u64]>>;
type MultiKeyMap = HashMap<Box<[u64]>, Box<[u64]>>;
type StringKeyMap = HashMap<Box<[u8]>, (GcRef, Box<[u64]>)>;  // key=string content, val=(original GcRef, value)

pub enum MapInner {
    SingleKey(SingleKeyMap),
    MultiKey(MultiKeyMap),
    StringKey(StringKeyMap),  // For string keys: compare by content
}

#[repr(C)]
pub struct MapData {
    pub inner: *mut MapInner,
    pub key_meta: ValueMeta,
    pub val_meta: ValueMeta,
    pub key_slots: u16,
    pub val_slots: u16,
}

pub const DATA_SLOTS: u16 = 3;
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

pub fn create(gc: &mut Gc, key_meta: ValueMeta, val_meta: ValueMeta, key_slots: u16, val_slots: u16) -> GcRef {
    let m = gc.alloc(ValueMeta::new(0, ValueKind::Map), DATA_SLOTS);
    let inner = if key_meta.value_kind() == ValueKind::String {
        // String keys: use content-based comparison
        MapInner::StringKey(StringKeyMap::new())
    } else if key_slots == 1 {
        MapInner::SingleKey(SingleKeyMap::new())
    } else {
        MapInner::MultiKey(MultiKeyMap::new())
    };
    let data = MapData::as_mut(m);
    data.inner = Box::into_raw(Box::new(inner));
    data.key_meta = key_meta;
    data.val_meta = val_meta;
    data.key_slots = key_slots;
    data.val_slots = val_slots;
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
pub fn key_slots(m: GcRef) -> u16 { MapData::as_ref(m).key_slots }
#[inline]
pub fn val_slots(m: GcRef) -> u16 { MapData::as_ref(m).val_slots }

#[inline]
fn get_inner(m: GcRef) -> &'static mut MapInner {
    unsafe { &mut *MapData::as_ref(m).inner }
}

pub fn len(m: GcRef) -> usize {
    match get_inner(m) {
        MapInner::SingleKey(map) => map.len(),
        MapInner::MultiKey(map) => map.len(),
        MapInner::StringKey(map) => map.len(),
    }
}

pub fn get(m: GcRef, key: &[u64]) -> Option<&'static [u64]> {
    match get_inner(m) {
        MapInner::SingleKey(map) => {
            map.get(&key[0]).map(|v| v.as_ref())
        }
        MapInner::MultiKey(map) => {
            map.get(key).map(|v| v.as_ref())
        }
        MapInner::StringKey(map) => {
            let str_ref = key[0] as GcRef;
            let str_bytes = string::as_bytes(str_ref);
            map.get(str_bytes).map(|(_, v)| v.as_ref())
        }
    }
}

pub fn get_with_ok(m: GcRef, key: &[u64]) -> (Option<&'static [u64]>, bool) {
    match get_inner(m) {
        MapInner::SingleKey(map) => {
            match map.get(&key[0]) {
                Some(v) => (Some(v.as_ref()), true),
                None => (None, false),
            }
        }
        MapInner::MultiKey(map) => {
            match map.get(key) {
                Some(v) => (Some(v.as_ref()), true),
                None => (None, false),
            }
        }
        MapInner::StringKey(map) => {
            let str_ref = key[0] as GcRef;
            let str_bytes = string::as_bytes(str_ref);
            match map.get(str_bytes) {
                Some((_, v)) => (Some(v.as_ref()), true),
                None => (None, false),
            }
        }
    }
}

pub fn set(m: GcRef, key: &[u64], val: &[u64]) {
    let val_box: Box<[u64]> = val.into();
    match get_inner(m) {
        MapInner::SingleKey(map) => {
            map.insert(key[0], val_box);
        }
        MapInner::MultiKey(map) => {
            let key_box: Box<[u64]> = key.into();
            map.insert(key_box, val_box);
        }
        MapInner::StringKey(map) => {
            let str_ref = key[0] as GcRef;
            let str_bytes: Box<[u8]> = string::as_bytes(str_ref).into();
            map.insert(str_bytes, (str_ref, val_box));
        }
    }
}

pub fn delete(m: GcRef, key: &[u64]) {
    match get_inner(m) {
        MapInner::SingleKey(map) => { map.remove(&key[0]); }
        MapInner::MultiKey(map) => { map.remove(key); }
        MapInner::StringKey(map) => {
            let str_ref = key[0] as GcRef;
            let str_bytes = string::as_bytes(str_ref);
            map.remove(str_bytes);
        }
    }
}

pub fn contains(m: GcRef, key: &[u64]) -> bool {
    match get_inner(m) {
        MapInner::SingleKey(map) => map.contains_key(&key[0]),
        MapInner::MultiKey(map) => map.contains_key(key),
        MapInner::StringKey(map) => {
            let str_ref = key[0] as GcRef;
            let str_bytes = string::as_bytes(str_ref);
            map.contains_key(str_bytes)
        }
    }
}

pub fn iter_at(m: GcRef, idx: usize) -> Option<(&'static [u64], &'static [u64])> {
    match get_inner(m) {
        MapInner::SingleKey(map) => {
            map.iter().nth(idx).map(|(k, v)| {
                let k_slice: &'static [u64] = unsafe {
                    core::slice::from_raw_parts(k as *const u64, 1)
                };
                (k_slice, v.as_ref())
            })
        }
        MapInner::MultiKey(map) => {
            map.iter().nth(idx).map(|(k, v)| (k.as_ref(), v.as_ref()))
        }
        MapInner::StringKey(map) => {
            map.iter().nth(idx).map(|(_, (str_ref, v))| {
                let k_slice: &'static [u64] = unsafe {
                    core::slice::from_raw_parts(str_ref as *const GcRef as *const u64, 1)
                };
                (k_slice, v.as_ref())
            })
        }
    }
}

/// # Safety
/// m must be a valid Map GcRef.
pub unsafe fn drop_inner(m: GcRef) {
    let data = MapData::as_mut(m);
    if !data.inner.is_null() {
        drop(Box::from_raw(data.inner));
        data.inner = core::ptr::null_mut();
    }
}
