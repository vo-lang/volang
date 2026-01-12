//! Map object operations.
//!
//! Layout: GcHeader + MapData
//! Uses VoMap (custom hash map with iteration-safe semantics).
//!
//! Iteration safety:
//! - Delete during iteration: tombstones ensure safe traversal
//! - Insert during iteration: if resize happens, generation changes, iteration continues
//!   (may skip or repeat elements, matching Go semantics)

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
#[cfg(feature = "std")]
use std::boxed::Box;

use super::vo_map::VoMap;

use crate::gc::{Gc, GcRef};
use crate::objects::string;
use vo_common_core::bytecode::Module;
use vo_common_core::types::{ValueKind, ValueMeta};

use super::compare::{deep_eq_struct_inline, deep_hash_struct_inline, iface_eq, iface_hash};
use super::impl_gc_object;

type SingleKeyMap = VoMap<u64, Box<[u64]>>;
type MultiKeyMap = VoMap<Box<[u64]>, Box<[u64]>>;
type StringKeyMap = VoMap<Box<[u8]>, (GcRef, Box<[u64]>)>;

pub struct StructKeyEntry {
    pub key: Box<[u64]>,
    pub val: Box<[u64]>,
}

type StructKeyMap = VoMap<u64, StructKeyEntry>;

pub struct InterfaceKeyEntry {
    pub key: [u64; 2],
    pub val: Box<[u64]>,
}

type InterfaceKeyMap = VoMap<u64, InterfaceKeyEntry>;

pub enum MapInner {
    SingleKey(SingleKeyMap),
    MultiKey(MultiKeyMap),
    StringKey(StringKeyMap),
    StructKey(StructKeyMap),
    InterfaceKey(InterfaceKeyMap),
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
        MapInner::StringKey(StringKeyMap::new())
    } else if key_vk == ValueKind::Struct {
        MapInner::StructKey(StructKeyMap::new())
    } else if key_vk == ValueKind::Interface {
        MapInner::InterfaceKey(InterfaceKeyMap::new())
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

#[inline]
pub fn generation(m: GcRef) -> u32 {
    match get_inner(m) {
        MapInner::SingleKey(map) => map.generation(),
        MapInner::MultiKey(map) => map.generation(),
        MapInner::StringKey(map) => map.generation(),
        MapInner::StructKey(map) => map.generation(),
        MapInner::InterfaceKey(map) => map.generation(),
    }
}

#[inline]
fn struct_key_hash(m: GcRef, key: &[u64], module: &Module) -> u64 {
    let rttid = key_meta(m).meta_id();
    deep_hash_struct_inline(key, rttid, module)
}

pub fn len(m: GcRef) -> usize {
    match get_inner(m) {
        MapInner::SingleKey(map) => map.len(),
        MapInner::MultiKey(map) => map.len(),
        MapInner::StringKey(map) => map.len(),
        MapInner::StructKey(map) => map.len(),
        MapInner::InterfaceKey(map) => map.len(),
    }
}

pub fn get(m: GcRef, key: &[u64], module: Option<&Module>) -> Option<&'static [u64]> {
    match get_inner(m) {
        MapInner::SingleKey(map) => map.get(&key[0]).map(|v| v.as_ref()),
        MapInner::MultiKey(map) => {
            let key_box: Box<[u64]> = key.into();
            map.get(&key_box).map(|v| v.as_ref())
        }
        MapInner::StringKey(map) => {
            let str_ref = key[0] as GcRef;
            let str_bytes: Box<[u8]> = string::as_bytes(str_ref).into();
            map.get(&str_bytes).map(|(_, v)| v.as_ref())
        }
        MapInner::StructKey(map) => {
            let module = module.expect("StructKey requires Module");
            let rttid = key_meta(m).meta_id();
            for (_, entry) in map.iter() {
                if deep_eq_struct_inline(key, &entry.key, rttid, module) {
                    return Some(entry.val.as_ref());
                }
            }
            None
        }
        MapInner::InterfaceKey(map) => {
            let module = module.expect("InterfaceKey requires Module");
            let (slot0, slot1) = (key[0], key[1]);
            for (_, entry) in map.iter() {
                if iface_eq(slot0, slot1, entry.key[0], entry.key[1], module) == 1 {
                    return Some(entry.val.as_ref());
                }
            }
            None
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
        MapInner::SingleKey(map) => { map.insert(key[0], val_box); }
        MapInner::MultiKey(map) => {
            map.insert(key.into(), val_box);
        }
        MapInner::StringKey(map) => {
            let str_ref = key[0] as GcRef;
            let str_bytes: Box<[u8]> = string::as_bytes(str_ref).into();
            map.insert(str_bytes, (str_ref, val_box));
        }
        MapInner::StructKey(map) => {
            let module = module.expect("StructKey requires Module");
            let hash = struct_key_hash(m, key, module);
            let rttid = key_meta(m).meta_id();
            // Check if key exists and update
            for (_, entry) in map.iter_mut() {
                if deep_eq_struct_inline(key, &entry.key, rttid, module) {
                    entry.val = val_box;
                    return;
                }
            }
            // Key not found, insert new
            map.insert(hash, StructKeyEntry { key: key.into(), val: val_box });
        }
        MapInner::InterfaceKey(map) => {
            let module = module.expect("InterfaceKey requires Module");
            let (slot0, slot1) = (key[0], key[1]);
            let hash = iface_hash(slot0, slot1, module);
            // Check if key exists and update
            for (_, entry) in map.iter_mut() {
                if iface_eq(slot0, slot1, entry.key[0], entry.key[1], module) == 1 {
                    entry.val = val_box;
                    return;
                }
            }
            // Key not found, insert new
            map.insert(hash, InterfaceKeyEntry { key: [slot0, slot1], val: val_box });
        }
    }
}

pub fn delete(m: GcRef, key: &[u64], module: Option<&Module>) {
    match get_inner(m) {
        MapInner::SingleKey(map) => { map.remove(&key[0]); }
        MapInner::MultiKey(map) => {
            let key_box: Box<[u64]> = key.into();
            map.remove(&key_box);
        }
        MapInner::StringKey(map) => {
            let str_ref = key[0] as GcRef;
            let str_bytes: Box<[u8]> = string::as_bytes(str_ref).into();
            map.remove(&str_bytes);
        }
        MapInner::StructKey(map) => {
            let module = module.expect("StructKey requires Module");
            let rttid = key_meta(m).meta_id();
            let mut to_remove = None;
            for (h, entry) in map.iter() {
                if deep_eq_struct_inline(key, &entry.key, rttid, module) {
                    to_remove = Some(*h);
                    break;
                }
            }
            if let Some(h) = to_remove {
                map.remove(&h);
            }
        }
        MapInner::InterfaceKey(map) => {
            let module = module.expect("InterfaceKey requires Module");
            let (slot0, slot1) = (key[0], key[1]);
            let mut to_remove = None;
            for (h, entry) in map.iter() {
                if iface_eq(slot0, slot1, entry.key[0], entry.key[1], module) == 1 {
                    to_remove = Some(*h);
                    break;
                }
            }
            if let Some(h) = to_remove {
                map.remove(&h);
            }
        }
    }
}

pub fn contains(m: GcRef, key: &[u64], module: Option<&Module>) -> bool {
    get(m, key, module).is_some()
}

// =============================================================================
// Index-based Map Iterator
// =============================================================================

#[repr(C)]
pub struct MapIterator {
    pub tag: u8,
    pub _pad: [u8; 3],
    pub init_generation: u32,
    pub current_index: u64,
    pub _reserved: [u64; 4],
    pub map_ref: u64,
}

pub const MAP_ITER_SLOTS: usize = core::mem::size_of::<MapIterator>() / 8;
const _: () = assert!(core::mem::size_of::<MapIterator>() == MAP_ITER_SLOTS * 8);
const _: () = assert!(MAP_ITER_SLOTS == 7);

const TAG_SINGLE_KEY: u8 = 0;
const TAG_MULTI_KEY: u8 = 1;
const TAG_STRING_KEY: u8 = 2;
const TAG_STRUCT_KEY: u8 = 3;
const TAG_INTERFACE_KEY: u8 = 4;
const TAG_EXHAUSTED: u8 = 255;

pub fn iter_init(m: GcRef) -> MapIterator {
    if m.is_null() {
        return MapIterator {
            tag: TAG_EXHAUSTED,
            _pad: [0; 3],
            init_generation: 0,
            current_index: 0,
            _reserved: [0; 4],
            map_ref: 0,
        };
    }
    
    let tag = match get_inner(m) {
        MapInner::SingleKey(_) => TAG_SINGLE_KEY,
        MapInner::MultiKey(_) => TAG_MULTI_KEY,
        MapInner::StringKey(_) => TAG_STRING_KEY,
        MapInner::StructKey(_) => TAG_STRUCT_KEY,
        MapInner::InterfaceKey(_) => TAG_INTERFACE_KEY,
    };
    
    MapIterator {
        tag,
        _pad: [0; 3],
        init_generation: generation(m),
        current_index: 0,
        _reserved: [0; 4],
        map_ref: m as u64,
    }
}

pub fn iter_next(iter: &mut MapIterator) -> Option<(&'static [u64], &'static [u64])> {
    if iter.tag == TAG_EXHAUSTED {
        return None;
    }
    
    let m = iter.map_ref as GcRef;
    if m.is_null() {
        iter.tag = TAG_EXHAUSTED;
        return None;
    }
    
    // If rehash happened, update generation and continue from current index
    // This matches Go semantics: may or may not see new elements, but won't crash
    let current_gen = generation(m);
    if current_gen != iter.init_generation {
        iter.init_generation = current_gen;
        // Continue from current index - may skip or repeat elements, which is Go-like behavior
    }
    
    let idx = iter.current_index as usize;
    
    match get_inner(m) {
        MapInner::SingleKey(map) => {
            if let Some((new_idx, k, v)) = map.iter_from_index(idx) {
                iter.current_index = (new_idx + 1) as u64;
                let k_slice = unsafe { core::slice::from_raw_parts(k, 1) };
                Some((k_slice, v.as_ref()))
            } else {
                iter.tag = TAG_EXHAUSTED;
                None
            }
        }
        MapInner::MultiKey(map) => {
            if let Some((new_idx, k, v)) = map.iter_from_index(idx) {
                iter.current_index = (new_idx + 1) as u64;
                Some((k.as_ref(), v.as_ref()))
            } else {
                iter.tag = TAG_EXHAUSTED;
                None
            }
        }
        MapInner::StringKey(map) => {
            if let Some((new_idx, _, (str_ref, v))) = map.iter_from_index(idx) {
                iter.current_index = (new_idx + 1) as u64;
                let k_slice = unsafe { core::slice::from_raw_parts(str_ref as *const GcRef as *const u64, 1) };
                Some((k_slice, v.as_ref()))
            } else {
                iter.tag = TAG_EXHAUSTED;
                None
            }
        }
        MapInner::StructKey(map) => {
            if let Some((new_idx, _, entry)) = map.iter_from_index(idx) {
                iter.current_index = (new_idx + 1) as u64;
                Some((entry.key.as_ref(), entry.val.as_ref()))
            } else {
                iter.tag = TAG_EXHAUSTED;
                None
            }
        }
        MapInner::InterfaceKey(map) => {
            if let Some((new_idx, _, entry)) = map.iter_from_index(idx) {
                iter.current_index = (new_idx + 1) as u64;
                Some((entry.key.as_slice(), entry.val.as_ref()))
            } else {
                iter.tag = TAG_EXHAUSTED;
                None
            }
        }
    }
}

pub unsafe fn drop_inner(m: GcRef) {
    let data = MapData::as_mut(m);
    if !data.inner.is_null() {
        drop(Box::from_raw(data.inner));
        data.inner = core::ptr::null_mut();
    }
}
