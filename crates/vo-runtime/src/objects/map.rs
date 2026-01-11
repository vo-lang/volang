//! Map object operations.
//!
//! Layout: GcHeader + MapData
//! Uses hashbrown HashMap (no_std compatible, unordered - matches Go map semantics).
//!
//! Supports modes:
//! - SingleKey: key is 1 slot primitive (common case, optimized)
//! - MultiKey: key is multiple slots without GcRef fields
//! - StringKey: key is a single string
//! - StructKey: key is a struct that may contain string/interface fields (needs deep comparison)
//!
//! Value always supports multiple slots.

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
#[cfg(feature = "std")]
use std::boxed::Box;

use hashbrown::hash_table::HashTable;
use hashbrown::HashMap;

use crate::gc::{Gc, GcRef};
use crate::objects::string;
use vo_common_core::bytecode::Module;
use vo_common_core::types::{ValueKind, ValueMeta};

use super::compare::{deep_eq_struct_inline, deep_hash_struct_inline};
use super::impl_gc_object;

type SingleKeyMap = HashMap<u64, Box<[u64]>>;
type MultiKeyMap = HashMap<Box<[u64]>, Box<[u64]>>;
type StringKeyMap = HashMap<Box<[u8]>, (GcRef, Box<[u64]>)>;  // key=string content, val=(original GcRef, value)

/// Entry for struct key map: stores key slots and value slots
struct StructKeyEntry {
    key: Box<[u64]>,
    val: Box<[u64]>,
    hash: u64,  // Cached hash for the key
}

type StructKeyMap = HashTable<StructKeyEntry>;

pub enum MapInner {
    SingleKey(SingleKeyMap),
    MultiKey(MultiKeyMap),
    StringKey(StringKeyMap),  // For string keys: compare by content
    StructKey(StructKeyMap),  // For struct keys: deep comparison
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

impl_gc_object!(MapData);

pub fn create(gc: &mut Gc, key_meta: ValueMeta, val_meta: ValueMeta, key_slots: u16, val_slots: u16) -> GcRef {
    let m = gc.alloc(ValueMeta::new(0, ValueKind::Map), DATA_SLOTS);
    let key_vk = key_meta.value_kind();
    let inner = if key_vk == ValueKind::String {
        // String keys: use content-based comparison
        MapInner::StringKey(StringKeyMap::new())
    } else if key_vk == ValueKind::Struct {
        // Struct keys: use deep comparison (handles string fields, etc.)
        MapInner::StructKey(StructKeyMap::new())
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

/// Helper for StructKey operations: get rttid and compute hash.
#[inline]
fn struct_key_hash(m: GcRef, key: &[u64], module: &Module) -> (u32, u64) {
    let rttid = key_meta(m).meta_id();
    let hash = deep_hash_struct_inline(key, rttid, module);
    (rttid, hash)
}

pub fn len(m: GcRef) -> usize {
    match get_inner(m) {
        MapInner::SingleKey(map) => map.len(),
        MapInner::MultiKey(map) => map.len(),
        MapInner::StringKey(map) => map.len(),
        MapInner::StructKey(map) => map.len(),
    }
}

pub fn get(m: GcRef, key: &[u64], module: Option<&Module>) -> Option<&'static [u64]> {
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
        MapInner::StructKey(map) => {
            let module = module.expect("StructKey requires Module");
            let (rttid, hash) = struct_key_hash(m, key, module);
            map.find(hash, |e| deep_eq_struct_inline(key, &e.key, rttid, module))
                .map(|e| e.val.as_ref())
        }
    }
}

pub fn get_with_ok(m: GcRef, key: &[u64], module: Option<&Module>) -> (Option<&'static [u64]>, bool) {
    match get(m, key, module) {
        Some(v) => (Some(v), true),
        None => (None, false),
    }
}

pub fn set(m: GcRef, key: &[u64], val: &[u64], module: Option<&Module>) {
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
        MapInner::StructKey(map) => {
            let module = module.expect("StructKey requires Module");
            let (rttid, hash) = struct_key_hash(m, key, module);
            match map.find_mut(hash, |e| deep_eq_struct_inline(key, &e.key, rttid, module)) {
                Some(e) => e.val = val_box,
                None => {
                    let key_box: Box<[u64]> = key.into();
                    map.insert_unique(hash, StructKeyEntry { key: key_box, val: val_box, hash }, |e| e.hash);
                }
            }
        }
    }
}

pub fn delete(m: GcRef, key: &[u64], module: Option<&Module>) {
    match get_inner(m) {
        MapInner::SingleKey(map) => { map.remove(&key[0]); }
        MapInner::MultiKey(map) => { map.remove(key); }
        MapInner::StringKey(map) => {
            let str_ref = key[0] as GcRef;
            let str_bytes = string::as_bytes(str_ref);
            map.remove(str_bytes);
        }
        MapInner::StructKey(map) => {
            let module = module.expect("StructKey requires Module");
            let (rttid, hash) = struct_key_hash(m, key, module);
            let _ = map.find_entry(hash, |e| deep_eq_struct_inline(key, &e.key, rttid, module))
                .map(|e| e.remove());
        }
    }
}

pub fn contains(m: GcRef, key: &[u64], module: Option<&Module>) -> bool {
    get(m, key, module).is_some()
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
        MapInner::StructKey(map) => {
            map.iter().nth(idx).map(|entry| {
                (entry.key.as_ref(), entry.val.as_ref())
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
