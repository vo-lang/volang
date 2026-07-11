#![allow(clippy::missing_safety_doc)]
//! Map object operations.
//!
//! Layout: GcHeader + MapData
//! Uses VoMap (custom hash map with iteration-safe semantics).
//!
//! # Safety contract
//! Unsafe accessors require a canonical live map allocation and key/value
//! slices whose widths match its recorded layout.
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
use crate::slot::{ptr_to_slot, slot_to_ptr, Slot, SLOT_BYTES};
use vo_common_core::bytecode::Module;
pub use vo_common_core::bytecode::{MAP_ITER_SLOTS, MAP_ITER_SLOT_TYPES};
use vo_common_core::types::{ValueKind, ValueMeta};

use super::compare::{
    deep_eq_struct_inline, deep_hash_struct_inline_checked, iface_eq, iface_hash_checked,
    UnhashableType,
};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MapKeyError {
    UnhashableInterfaceKey,
    SlotCountMismatch,
    MissingModule,
}

pub type OwnedMapValue = Box<[u64]>;
pub type OwnedMapEntry = (Box<[u64]>, Box<[u64]>);

impl From<UnhashableType> for MapKeyError {
    fn from(_: UnhashableType) -> Self {
        Self::UnhashableInterfaceKey
    }
}

#[repr(C)]
pub struct MapData {
    pub inner: Slot,
    pub key_meta: ValueMeta,
    pub val_meta: ValueMeta,
    pub key_slots: u16,
    pub val_slots: u16,
    /// Runtime type ID for struct keys, used for deep hash/eq.
    /// For non-struct keys, this is 0 (unused).
    pub key_rttid: u32,
}

pub const DATA_SLOTS: u16 = 3;
const _: () = assert!(core::mem::size_of::<MapData>() == DATA_SLOTS as usize * SLOT_BYTES);

impl_gc_object!(MapData);

pub fn create(
    gc: &mut Gc,
    key_meta: ValueMeta,
    val_meta: ValueMeta,
    key_slots: u16,
    val_slots: u16,
    key_rttid: u32,
) -> GcRef {
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
    // Safety: `m` is freshly allocated and not visible to the collector yet.
    let data = unsafe { MapData::as_mut(m) };
    data.inner = ptr_to_slot(Box::into_raw(Box::new(inner)));
    data.key_meta = key_meta;
    data.val_meta = val_meta;
    data.key_slots = key_slots;
    data.val_slots = val_slots;
    data.key_rttid = key_rttid;
    m
}

#[inline]
pub unsafe fn key_meta(m: GcRef) -> ValueMeta {
    unsafe { MapData::as_ref(m) }.key_meta
}
#[inline]
pub unsafe fn val_meta(m: GcRef) -> ValueMeta {
    unsafe { MapData::as_ref(m) }.val_meta
}
#[inline]
pub unsafe fn key_kind(m: GcRef) -> ValueKind {
    key_meta(m).value_kind()
}
#[inline]
pub unsafe fn val_kind(m: GcRef) -> ValueKind {
    val_meta(m).value_kind()
}
#[inline]
pub unsafe fn key_rttid(m: GcRef) -> u32 {
    unsafe { MapData::as_ref(m) }.key_rttid
}
#[inline]
pub unsafe fn key_slots(m: GcRef) -> u16 {
    unsafe { MapData::as_ref(m) }.key_slots
}
#[inline]
pub unsafe fn val_slots(m: GcRef) -> u16 {
    unsafe { MapData::as_ref(m) }.val_slots
}

#[inline]
unsafe fn inner_ref<'a>(m: GcRef) -> &'a MapInner {
    unsafe { &*slot_to_ptr(MapData::as_ref(m).inner) }
}

#[inline]
unsafe fn inner_mut<'a>(m: GcRef) -> &'a mut MapInner {
    unsafe { &mut *slot_to_ptr(MapData::as_ref(m).inner) }
}

#[inline]
pub unsafe fn generation(m: GcRef) -> u32 {
    match inner_ref(m) {
        MapInner::SingleKey(map) => map.generation(),
        MapInner::MultiKey(map) => map.generation(),
        MapInner::StringKey(map) => map.generation(),
        MapInner::StructKey(map) => map.generation(),
        MapInner::InterfaceKey(map) => map.generation(),
    }
}

#[inline]
unsafe fn struct_key_hash_checked(
    m: GcRef,
    key: &[u64],
    module: &Module,
) -> Result<u64, MapKeyError> {
    let rttid = key_rttid(m);
    deep_hash_struct_inline_checked(key, rttid, module).map_err(Into::into)
}

pub unsafe fn len(m: GcRef) -> usize {
    match inner_ref(m) {
        MapInner::SingleKey(map) => map.len(),
        MapInner::MultiKey(map) => map.len(),
        MapInner::StringKey(map) => map.len(),
        MapInner::StructKey(map) => map.len(),
        MapInner::InterfaceKey(map) => map.len(),
    }
}

unsafe fn with_value_checked<R>(
    m: GcRef,
    key: &[u64],
    module: Option<&Module>,
    consume: impl FnOnce(Option<&[u64]>) -> R,
) -> Result<R, MapKeyError> {
    if key.len() != key_slots(m) as usize {
        return Err(MapKeyError::SlotCountMismatch);
    }
    match inner_ref(m) {
        MapInner::SingleKey(map) => Ok(consume(map.get(&key[0]).map(AsRef::as_ref))),
        MapInner::MultiKey(map) => Ok(consume(map.get_borrowed(key).map(AsRef::as_ref))),
        MapInner::StringKey(map) => {
            let str_ref = key[0] as GcRef;
            // Safety: string keys are live while the owning map operation runs.
            let str_bytes = unsafe { string::bytes_unchecked(str_ref) };
            Ok(consume(
                map.get_borrowed(str_bytes).map(|(_, value)| value.as_ref()),
            ))
        }
        MapInner::StructKey(map) => {
            let module = module.ok_or(MapKeyError::MissingModule)?;
            let rttid = key_rttid(m);
            let hash = struct_key_hash_checked(m, key, module)?;
            Ok(consume(
                map.find_by(hash, |_, entry| {
                    deep_eq_struct_inline(key, &entry.key, rttid, module)
                })
                .map(|entry| entry.val.as_ref()),
            ))
        }
        MapInner::InterfaceKey(map) => {
            let module = module.ok_or(MapKeyError::MissingModule)?;
            let (slot0, slot1) = (key[0], key[1]);
            let hash = iface_hash_checked(slot0, slot1, module)?;
            Ok(consume(
                map.find_by(hash, |_, entry| {
                    iface_eq(slot0, slot1, entry.key[0], entry.key[1], module) == 1
                })
                .map(|entry| entry.val.as_ref()),
            ))
        }
    }
}

pub unsafe fn get_checked(
    m: GcRef,
    key: &[u64],
    module: Option<&Module>,
) -> Result<Option<Box<[u64]>>, MapKeyError> {
    with_value_checked(m, key, module, |value| value.map(Into::into))
}

/// Copy a map value into a caller-owned buffer without allocating.
///
/// Returns `true` when the key exists. A missing key zeroes `out` so callers
/// can directly implement the language's zero-value lookup semantics.
pub unsafe fn get_checked_into(
    m: GcRef,
    key: &[u64],
    module: Option<&Module>,
    out: &mut [u64],
) -> Result<bool, MapKeyError> {
    if out.len() != val_slots(m) as usize {
        return Err(MapKeyError::SlotCountMismatch);
    }
    with_value_checked(m, key, module, |value| {
        if let Some(value) = value {
            out.copy_from_slice(value);
            true
        } else {
            out.fill(0);
            false
        }
    })
}

pub unsafe fn get_with_ok_checked(
    m: GcRef,
    key: &[u64],
    module: Option<&Module>,
) -> Result<(Option<OwnedMapValue>, bool), MapKeyError> {
    match get_checked(m, key, module)? {
        Some(v) => Ok((Some(v), true)),
        None => Ok((None, false)),
    }
}

pub unsafe fn validate_entry_slot_counts(
    m: GcRef,
    key_slots: usize,
    val_slots: usize,
) -> Result<(), MapKeyError> {
    if key_slots != self::key_slots(m) as usize || val_slots != self::val_slots(m) as usize {
        return Err(MapKeyError::SlotCountMismatch);
    }
    Ok(())
}

/// Insert or replace a map entry after the caller has handled GC publication.
///
/// # Safety
/// `m` must be a valid live map object. If the map may already be visible to an
/// incremental GC cycle, callers must apply the precise key/value write barriers
/// before calling this raw mutator. Initialization-only callers must arrange for
/// the new map to be scanned before it can be swept.
pub unsafe fn set_checked(
    m: GcRef,
    key: &[u64],
    val: &[u64],
    module: Option<&Module>,
) -> Result<(), MapKeyError> {
    validate_entry_slot_counts(m, key.len(), val.len())?;
    let val_box: Box<[u64]> = val.into();
    match inner_mut(m) {
        MapInner::SingleKey(map) => {
            map.insert(key[0], val_box);
        }
        MapInner::MultiKey(map) => {
            map.insert(key.into(), val_box);
        }
        MapInner::StringKey(map) => {
            let str_ref = key[0] as GcRef;
            // Safety: string keys are live while the owning map operation runs.
            let str_bytes: Box<[u8]> = unsafe { string::to_bytes(str_ref) }.into();
            map.insert(str_bytes, (str_ref, val_box));
        }
        MapInner::StructKey(map) => {
            let module = module.ok_or(MapKeyError::MissingModule)?;
            let hash = struct_key_hash_checked(m, key, module)?;
            let rttid = key_rttid(m);
            // Check if key exists and update (O(1) average via hash probe)
            if let Some(entry) = map.find_by_mut(hash, |_, e| {
                deep_eq_struct_inline(key, &e.key, rttid, module)
            }) {
                entry.val = val_box;
                return Ok(());
            }
            // Key not found, insert new
            map.insert(
                hash,
                StructKeyEntry {
                    key: key.into(),
                    val: val_box,
                },
            );
        }
        MapInner::InterfaceKey(map) => {
            let module = module.ok_or(MapKeyError::MissingModule)?;
            let (slot0, slot1) = (key[0], key[1]);
            let hash = iface_hash_checked(slot0, slot1, module)?;
            // Check if key exists and update (O(1) average via hash probe)
            if let Some(entry) = map.find_by_mut(hash, |_, e| {
                iface_eq(slot0, slot1, e.key[0], e.key[1], module) == 1
            }) {
                entry.val = val_box;
                return Ok(());
            }
            // Key not found, insert new
            map.insert(
                hash,
                InterfaceKeyEntry {
                    key: [slot0, slot1],
                    val: val_box,
                },
            );
        }
    }
    Ok(())
}

pub unsafe fn delete_checked(
    m: GcRef,
    key: &[u64],
    module: Option<&Module>,
) -> Result<(), MapKeyError> {
    match inner_mut(m) {
        MapInner::SingleKey(map) => {
            map.remove(&key[0]);
        }
        MapInner::MultiKey(map) => {
            map.remove_borrowed(key);
        }
        MapInner::StringKey(map) => {
            let str_ref = key[0] as GcRef;
            // Safety: string keys are live while the owning map operation runs.
            let str_bytes = unsafe { string::bytes_unchecked(str_ref) };
            map.remove_borrowed(str_bytes);
        }
        MapInner::StructKey(map) => {
            let module = module.ok_or(MapKeyError::MissingModule)?;
            let rttid = key_rttid(m);
            let hash = struct_key_hash_checked(m, key, module)?;
            map.remove_by(hash, |_, entry| {
                deep_eq_struct_inline(key, &entry.key, rttid, module)
            });
        }
        MapInner::InterfaceKey(map) => {
            let module = module.ok_or(MapKeyError::MissingModule)?;
            let (slot0, slot1) = (key[0], key[1]);
            let hash = iface_hash_checked(slot0, slot1, module)?;
            map.remove_by(hash, |_, entry| {
                iface_eq(slot0, slot1, entry.key[0], entry.key[1], module) == 1
            });
        }
    }
    Ok(())
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

const _: () = assert!(core::mem::size_of::<MapIterator>() == MAP_ITER_SLOTS * SLOT_BYTES);
const _: () = assert!(MAP_ITER_SLOTS == 7);

const TAG_SINGLE_KEY: u8 = 0;
const TAG_MULTI_KEY: u8 = 1;
const TAG_STRING_KEY: u8 = 2;
const TAG_STRUCT_KEY: u8 = 3;
const TAG_INTERFACE_KEY: u8 = 4;
const TAG_EXHAUSTED: u8 = 255;

pub unsafe fn iter_init(m: GcRef) -> MapIterator {
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

    let tag = match inner_ref(m) {
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

pub unsafe fn iter_next(iter: &mut MapIterator) -> Option<OwnedMapEntry> {
    unsafe {
        with_next(iter, |entry| {
            entry.map(|(key, value)| (key.into(), value.into()))
        })
    }
}

/// Advance an iterator while constraining borrowed entry slices to a callback.
///
/// This is the zero-copy traversal primitive for internal consumers that can
/// finish all work before the map may be mutated or collected.
pub unsafe fn with_next<R>(
    iter: &mut MapIterator,
    consume: impl FnOnce(Option<(&[u64], &[u64])>) -> R,
) -> R {
    if iter.tag == TAG_EXHAUSTED {
        return consume(None);
    }

    let m = iter.map_ref as GcRef;
    if m.is_null() {
        iter.tag = TAG_EXHAUSTED;
        return consume(None);
    }

    // If rehash happened, update generation and continue from current index
    // This matches Go semantics: may or may not see new elements, but won't crash
    let current_gen = generation(m);
    if current_gen != iter.init_generation {
        iter.init_generation = current_gen;
        // Continue from current index - may skip or repeat elements, which is Go-like behavior
    }

    let idx = iter.current_index as usize;

    match inner_ref(m) {
        MapInner::SingleKey(map) => {
            if let Some((new_idx, k, v)) = map.iter_from_index(idx) {
                iter.current_index = (new_idx + 1) as u64;
                let key = [*k];
                consume(Some((&key, v)))
            } else {
                iter.tag = TAG_EXHAUSTED;
                consume(None)
            }
        }
        MapInner::MultiKey(map) => {
            if let Some((new_idx, k, v)) = map.iter_from_index(idx) {
                iter.current_index = (new_idx + 1) as u64;
                consume(Some((k, v)))
            } else {
                iter.tag = TAG_EXHAUSTED;
                consume(None)
            }
        }
        MapInner::StringKey(map) => {
            if let Some((new_idx, _, (str_ref, v))) = map.iter_from_index(idx) {
                iter.current_index = (new_idx + 1) as u64;
                let raw = ptr_to_slot(*str_ref);
                iter._reserved[0] = raw;
                let key = [raw];
                consume(Some((&key, v)))
            } else {
                iter.tag = TAG_EXHAUSTED;
                consume(None)
            }
        }
        MapInner::StructKey(map) => {
            if let Some((new_idx, _, entry)) = map.iter_from_index(idx) {
                iter.current_index = (new_idx + 1) as u64;
                consume(Some((&entry.key, &entry.val)))
            } else {
                iter.tag = TAG_EXHAUSTED;
                consume(None)
            }
        }
        MapInner::InterfaceKey(map) => {
            if let Some((new_idx, _, entry)) = map.iter_from_index(idx) {
                iter.current_index = (new_idx + 1) as u64;
                consume(Some((&entry.key, &entry.val)))
            } else {
                iter.tag = TAG_EXHAUSTED;
                consume(None)
            }
        }
    }
}

/// Advance an iterator and copy the entry into caller-owned buffers.
///
/// This is the allocation-free hot-path API used by the interpreter and JIT.
pub unsafe fn iter_next_into(
    iter: &mut MapIterator,
    key_out: &mut [u64],
    val_out: &mut [u64],
) -> Result<bool, MapKeyError> {
    if iter.tag == TAG_EXHAUSTED {
        key_out.fill(0);
        val_out.fill(0);
        return Ok(false);
    }

    let m = iter.map_ref as GcRef;
    if m.is_null() {
        iter.tag = TAG_EXHAUSTED;
        key_out.fill(0);
        val_out.fill(0);
        return Ok(false);
    }
    validate_entry_slot_counts(m, key_out.len(), val_out.len())?;

    let found = unsafe {
        with_next(iter, |entry| {
            let Some((key, value)) = entry else {
                return false;
            };
            key_out.copy_from_slice(key);
            val_out.copy_from_slice(value);
            true
        })
    };

    if !found {
        iter.tag = TAG_EXHAUSTED;
        key_out.fill(0);
        val_out.fill(0);
    }
    Ok(found)
}

/// # Safety
/// Caller must ensure `m` is a valid `GcRef` pointing to a live map object.
pub unsafe fn drop_inner(m: GcRef) {
    // Safety: `m` is a valid map object owned by the GC finalization path.
    let data = unsafe { MapData::as_mut(m) };
    if data.inner != 0 {
        drop(Box::from_raw(slot_to_ptr::<MapInner>(data.inner)));
        data.inner = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{gc::Gc, ValueKind, ValueMeta};

    #[test]
    fn raw_map_set_checked_is_unsafe_public_primitive_058() {
        let source =
            vo_source_contract::production_source_without_test_modules(include_str!("map.rs"));
        assert!(
            source.contains("pub unsafe fn set_checked("),
            "raw map insertion publishes key/value roots and must stay behind an unsafe contract"
        );
        assert!(
            source.contains("callers must apply the precise key/value write barriers"),
            "set_checked safety docs must name the write-barrier obligation"
        );
    }

    #[test]
    fn raw_map_set_checked_rejects_key_value_width_drift_060() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = create(&mut gc, int_meta, int_meta, 1, 2, 0);

        assert!(
            unsafe { set_checked(m, &[7], &[11], None) }.is_err(),
            "raw map publication must reject values shorter than map value slots"
        );
        assert!(
            unsafe { set_checked(m, &[7, 8], &[11, 22], None) }.is_err(),
            "raw map publication must reject keys wider than map key slots"
        );
        assert!(
            unsafe { get_checked(m, &[7], None) }
                .expect("map read")
                .is_none(),
            "rejected width drift must not publish an entry"
        );
    }

    #[test]
    fn metadata_dependent_keys_report_missing_module() {
        let mut gc = Gc::new();
        let struct_meta = ValueMeta::new(0, ValueKind::Struct);
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = create(&mut gc, struct_meta, int_meta, 1, 1, 7);

        assert_eq!(
            unsafe { set_checked(m, &[1], &[2], None) },
            Err(MapKeyError::MissingModule)
        );
        assert_eq!(
            unsafe { get_checked(m, &[1], None) },
            Err(MapKeyError::MissingModule)
        );
        assert_eq!(
            unsafe { delete_checked(m, &[1], None) },
            Err(MapKeyError::MissingModule)
        );
    }
}
