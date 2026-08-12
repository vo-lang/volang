#![allow(clippy::missing_safety_doc)]
//! Map object operations.
//!
//! Layout: GcHeader + MapData
//! Bucket storage lives in an Island-GC-managed open-addressing table.
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

use crate::gc::{Gc, GcRef, MemoryError};
use crate::objects::string;
use crate::slot::{Slot, SLOT_BYTES};
#[cfg(test)]
use vo_common_core::bytecode::Module;
use vo_common_core::bytecode::{ModuleRuntimeMetadata, RuntimeTypeMetadata};
pub use vo_common_core::bytecode::{MAP_ITER_SLOTS, MAP_ITER_SLOT_TYPES};
use vo_common_core::types::{ValueKind, ValueMeta, ValueRttid};

use super::compare::{
    deep_eq_value_inline, deep_hash_value_inline_checked, float_key_hash, float_slot_eq, iface_eq,
    iface_hash_checked, UnhashableType,
};

const BACKING_HEADER_SLOTS: usize = 4;
const BACKING_CAPACITY_SLOT: usize = 0;
const BACKING_LEN_SLOT: usize = 1;
const BACKING_USED_SLOT: usize = 2;
const BACKING_GENERATION_SLOT: usize = 3;
const BUCKET_PREFIX_SLOTS: usize = 2;
const BUCKET_STATE_EMPTY: u64 = 0;
const BUCKET_STATE_TOMBSTONE: u64 = 1;
const BUCKET_STATE_OCCUPIED: u64 = 2;
const MIN_CAPACITY: usize = 8;
const LOAD_FACTOR_NUM: usize = 3;
const LOAD_FACTOR_DEN: usize = 4;

#[inline]
fn exceeds_load_factor(entries: usize, capacity: usize) -> bool {
    entries.saturating_mul(LOAD_FACTOR_DEN) > capacity.saturating_mul(LOAD_FACTOR_NUM)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MapKeyError {
    UnhashableInterfaceKey,
    SlotCountMismatch,
    MissingModule,
    AllocationFailed(MemoryError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MapSetOutcome {
    Set,
    NeedsAllocation,
}

#[derive(Clone, Copy)]
struct MapRuntimeMetadata<'a> {
    type_metadata: RuntimeTypeMetadata<'a>,
    full_metadata: Option<ModuleRuntimeMetadata<'a>>,
}

impl<'a> MapRuntimeMetadata<'a> {
    #[inline]
    fn from_module(metadata: ModuleRuntimeMetadata<'a>) -> Self {
        Self {
            type_metadata: metadata.type_metadata(),
            full_metadata: Some(metadata),
        }
    }

    #[inline]
    fn from_types(type_metadata: RuntimeTypeMetadata<'a>) -> Self {
        Self {
            type_metadata,
            full_metadata: None,
        }
    }
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
    if m.is_null() {
        return m;
    }
    // Safety: `m` is freshly allocated and not visible to the collector yet.
    let data = unsafe { MapData::as_mut(m) };
    data.inner = 0;
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
pub unsafe fn backing_ref(m: GcRef) -> GcRef {
    unsafe { MapData::as_ref(m) }.inner as GcRef
}

/// Validate the managed bucket allocation owned by a canonical live map.
///
/// A null backing is the valid representation of an empty map. Callers use
/// this before traversing untrusted or cross-Island map state.
pub unsafe fn has_valid_managed_backing_layout(gc: &Gc, m: GcRef) -> bool {
    let backing = unsafe { backing_ref(m) };
    if backing.is_null() {
        return true;
    }
    if gc.canonicalize_ref(backing) != Some(backing) {
        return false;
    }
    let header = unsafe { Gc::header(backing) };
    if !header.is_runtime_backing_object() {
        return false;
    }
    let Some(data_bytes) = gc.allocated_data_size_bytes(backing) else {
        return false;
    };
    if !data_bytes.is_multiple_of(SLOT_BYTES) {
        return false;
    }
    let allocated_slots = data_bytes / SLOT_BYTES;
    if allocated_slots < BACKING_HEADER_SLOTS {
        return false;
    }

    let capacity = unsafe { backing_capacity(backing) };
    if capacity < MIN_CAPACITY || !capacity.is_power_of_two() {
        return false;
    }
    let Some(expected_slots) = capacity
        .checked_mul(bucket_stride(m))
        .and_then(|slots| slots.checked_add(BACKING_HEADER_SLOTS))
    else {
        return false;
    };
    if allocated_slots != expected_slots {
        return false;
    }

    let len = unsafe { backing_len(backing) };
    let used = unsafe { backing_slot(backing, BACKING_USED_SLOT) as usize };
    if len > used || used > capacity {
        return false;
    }
    let mut occupied = 0usize;
    let mut non_empty = 0usize;
    for index in 0..capacity {
        match unsafe { bucket_state(m, backing, index) } {
            BUCKET_STATE_EMPTY => {}
            BUCKET_STATE_TOMBSTONE => {
                non_empty += 1;
            }
            BUCKET_STATE_OCCUPIED => {
                occupied += 1;
                non_empty += 1;
            }
            _ => return false,
        }
    }
    occupied == len && non_empty == used
}

#[inline]
fn bucket_stride(m: GcRef) -> usize {
    BUCKET_PREFIX_SLOTS + unsafe { key_slots(m) as usize } + unsafe { val_slots(m) as usize }
}

#[inline]
unsafe fn backing_slot(backing: GcRef, index: usize) -> u64 {
    unsafe { Gc::read_slot(backing, index) }
}

#[inline]
unsafe fn set_backing_slot(backing: GcRef, index: usize, value: u64) {
    unsafe { Gc::write_slot(backing, index, value) };
}

#[inline]
unsafe fn backing_capacity(backing: GcRef) -> usize {
    if backing.is_null() {
        0
    } else {
        unsafe { backing_slot(backing, BACKING_CAPACITY_SLOT) as usize }
    }
}

#[inline]
unsafe fn backing_len(backing: GcRef) -> usize {
    if backing.is_null() {
        0
    } else {
        unsafe { backing_slot(backing, BACKING_LEN_SLOT) as usize }
    }
}

#[inline]
unsafe fn bucket_offset(m: GcRef, index: usize) -> usize {
    BACKING_HEADER_SLOTS + index * bucket_stride(m)
}

#[inline]
unsafe fn bucket_state(m: GcRef, backing: GcRef, index: usize) -> u64 {
    unsafe { backing_slot(backing, bucket_offset(m, index)) }
}

#[inline]
unsafe fn bucket_hash(m: GcRef, backing: GcRef, index: usize) -> u64 {
    unsafe { backing_slot(backing, bucket_offset(m, index) + 1) }
}

#[inline]
unsafe fn bucket_key<'a>(m: GcRef, backing: GcRef, index: usize) -> &'a [u64] {
    let start = unsafe { bucket_offset(m, index) } + BUCKET_PREFIX_SLOTS;
    unsafe { core::slice::from_raw_parts(backing.add(start), key_slots(m) as usize) }
}

#[inline]
unsafe fn bucket_value<'a>(m: GcRef, backing: GcRef, index: usize) -> &'a [u64] {
    let start = unsafe { bucket_offset(m, index) } + BUCKET_PREFIX_SLOTS + key_slots(m) as usize;
    unsafe { core::slice::from_raw_parts(backing.add(start), val_slots(m) as usize) }
}

#[inline]
unsafe fn bucket_key_mut<'a>(m: GcRef, backing: GcRef, index: usize) -> &'a mut [u64] {
    let start = unsafe { bucket_offset(m, index) } + BUCKET_PREFIX_SLOTS;
    unsafe { core::slice::from_raw_parts_mut(backing.add(start), key_slots(m) as usize) }
}

#[inline]
unsafe fn bucket_value_mut<'a>(m: GcRef, backing: GcRef, index: usize) -> &'a mut [u64] {
    let start = unsafe { bucket_offset(m, index) } + BUCKET_PREFIX_SLOTS + key_slots(m) as usize;
    unsafe { core::slice::from_raw_parts_mut(backing.add(start), val_slots(m) as usize) }
}

#[inline]
pub unsafe fn generation(m: GcRef) -> u32 {
    let backing = unsafe { backing_ref(m) };
    if backing.is_null() {
        0
    } else {
        unsafe { backing_slot(backing, BACKING_GENERATION_SLOT) as u32 }
    }
}

#[inline]
unsafe fn semantic_key_hash_checked(
    m: GcRef,
    key: &[u64],
    module: Option<MapRuntimeMetadata<'_>>,
) -> Result<u64, MapKeyError> {
    let kind = key_kind(m);
    if matches!(kind, ValueKind::Float32 | ValueKind::Float64) {
        let bits = key.first().copied().ok_or(MapKeyError::SlotCountMismatch)?;
        return Ok(float_key_hash(kind, bits));
    }
    let module = module.ok_or(MapKeyError::MissingModule)?;
    let rttid = key_rttid(m);
    deep_hash_value_inline_checked(key, ValueRttid::new(rttid, kind), module.type_metadata)
        .map_err(Into::into)
}

unsafe fn semantic_key_eq(
    m: GcRef,
    a: &[u64],
    b: &[u64],
    module: Option<MapRuntimeMetadata<'_>>,
) -> bool {
    let kind = key_kind(m);
    if matches!(kind, ValueKind::Float32 | ValueKind::Float64) {
        return a
            .first()
            .zip(b.first())
            .is_some_and(|(&a, &b)| float_slot_eq(kind, a, b));
    }
    let Some(module) = module else {
        return false;
    };
    deep_eq_value_inline(
        a,
        b,
        ValueRttid::new(key_rttid(m), kind),
        module.type_metadata,
    )
}

pub unsafe fn len(m: GcRef) -> usize {
    unsafe { backing_len(backing_ref(m)) }
}

fn hash_bytes(bytes: &[u8]) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for &byte in bytes {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

fn hash_slots(slots: &[u64]) -> u64 {
    if let [value] = slots {
        // One-slot scalar keys dominate ordinary maps. A full eight-byte FNV
        // loop costs eight dependent multiplies; this stable integer mix has
        // stronger avalanche with three multiplies and is shared by VM and JIT.
        let mut hash = *value;
        hash ^= hash >> 30;
        hash = hash.wrapping_mul(0xbf58_476d_1ce4_e5b9);
        hash ^= hash >> 27;
        hash = hash.wrapping_mul(0x94d0_49bb_1331_11eb);
        return hash ^ (hash >> 31);
    }
    let bytes = unsafe {
        core::slice::from_raw_parts(slots.as_ptr().cast::<u8>(), core::mem::size_of_val(slots))
    };
    hash_bytes(bytes)
}

#[inline]
pub unsafe fn supports_trusted_scalar_key(m: GcRef) -> bool {
    (unsafe { key_slots(m) == 1 && val_slots(m) == 1 })
        && !matches!(
            unsafe { key_kind(m) },
            ValueKind::Struct | ValueKind::Array | ValueKind::Interface
        )
}

unsafe fn key_hash_checked(
    m: GcRef,
    key: &[u64],
    module: Option<MapRuntimeMetadata<'_>>,
) -> Result<u64, MapKeyError> {
    match unsafe { key_kind(m) } {
        ValueKind::String => {
            let string_ref = key.first().copied().ok_or(MapKeyError::SlotCountMismatch)? as GcRef;
            Ok(hash_bytes(unsafe { string::bytes_unchecked(string_ref) }))
        }
        ValueKind::Struct | ValueKind::Array | ValueKind::Float32 | ValueKind::Float64 => unsafe {
            semantic_key_hash_checked(m, key, module)
        },
        ValueKind::Interface => {
            let module = module.ok_or(MapKeyError::MissingModule)?;
            let [slot0, slot1] =
                <[u64; 2]>::try_from(key).map_err(|_| MapKeyError::SlotCountMismatch)?;
            iface_hash_checked(slot0, slot1, module.type_metadata).map_err(Into::into)
        }
        _ => Ok(hash_slots(key)),
    }
}

unsafe fn key_eq(
    m: GcRef,
    left: &[u64],
    right: &[u64],
    module: Option<MapRuntimeMetadata<'_>>,
) -> bool {
    match unsafe { key_kind(m) } {
        ValueKind::String => {
            let Some((&left, &right)) = left.first().zip(right.first()) else {
                return false;
            };
            unsafe {
                string::bytes_unchecked(left as GcRef) == string::bytes_unchecked(right as GcRef)
            }
        }
        ValueKind::Struct | ValueKind::Array | ValueKind::Float32 | ValueKind::Float64 => unsafe {
            semantic_key_eq(m, left, right, module)
        },
        ValueKind::Interface => {
            let Some(module) = module else {
                return false;
            };
            left.len() == 2
                && right.len() == 2
                && iface_eq(left[0], left[1], right[0], right[1], module.type_metadata) == 1
        }
        _ => left == right,
    }
}

unsafe fn find_bucket(
    m: GcRef,
    backing: GcRef,
    key: &[u64],
    hash: u64,
    module: Option<MapRuntimeMetadata<'_>>,
) -> (Option<usize>, usize) {
    let capacity = unsafe { backing_capacity(backing) };
    let mut index = hash as usize & (capacity - 1);
    let mut tombstone = None;
    for _ in 0..capacity {
        match unsafe { bucket_state(m, backing, index) } {
            BUCKET_STATE_EMPTY => return (None, tombstone.unwrap_or(index)),
            BUCKET_STATE_TOMBSTONE => {
                tombstone.get_or_insert(index);
            }
            BUCKET_STATE_OCCUPIED => {
                if unsafe { bucket_hash(m, backing, index) } == hash
                    && unsafe { key_eq(m, key, bucket_key(m, backing, index), module) }
                {
                    return (Some(index), index);
                }
            }
            state => panic!("invalid Island map bucket state {state}"),
        }
        index = (index + 1) & (capacity - 1);
    }
    (
        None,
        tombstone.expect("map table must retain an insertion slot"),
    )
}

unsafe fn allocate_backing(
    gc: &mut Gc,
    m: GcRef,
    capacity: usize,
    generation: u32,
) -> Result<GcRef, MapKeyError> {
    let total_slots = capacity
        .checked_mul(bucket_stride(m))
        .and_then(|slots| slots.checked_add(BACKING_HEADER_SLOTS))
        .ok_or(MapKeyError::AllocationFailed(
            MemoryError::HardLimitExceeded,
        ))?;
    let backing = gc.alloc_runtime_backing(total_slots);
    if backing.is_null() {
        return Err(MapKeyError::AllocationFailed(
            gc.last_memory_error()
                .unwrap_or(MemoryError::SystemAllocationFailed),
        ));
    }
    for index in 0..total_slots {
        unsafe { set_backing_slot(backing, index, 0) };
    }
    unsafe {
        set_backing_slot(backing, BACKING_CAPACITY_SLOT, capacity as u64);
        set_backing_slot(backing, BACKING_GENERATION_SLOT, u64::from(generation));
    }
    Ok(backing)
}

unsafe fn write_bucket(
    m: GcRef,
    backing: GcRef,
    index: usize,
    hash: u64,
    key: &[u64],
    val: &[u64],
) {
    let offset = unsafe { bucket_offset(m, index) };
    unsafe {
        set_backing_slot(backing, offset, BUCKET_STATE_OCCUPIED);
        set_backing_slot(backing, offset + 1, hash);
        bucket_key_mut(m, backing, index).copy_from_slice(key);
        bucket_value_mut(m, backing, index).copy_from_slice(val);
    }
}

fn write_resize_barrier(
    gc: &mut Gc,
    parent: GcRef,
    values: &[u64],
    meta: ValueMeta,
    metadata: Option<MapRuntimeMetadata<'_>>,
) {
    match metadata {
        Some(metadata) => crate::gc_types::typed_write_barrier_by_type_metadata(
            gc,
            parent,
            values,
            meta,
            metadata.type_metadata,
            metadata
                .full_metadata
                .and_then(ModuleRuntimeMetadata::runtime_type_facts),
        ),
        None => crate::gc_types::typed_write_barrier_by_meta(gc, parent, values, meta, None),
    }
}

unsafe fn resize(
    gc: &mut Gc,
    m: GcRef,
    new_capacity: usize,
    module: Option<MapRuntimeMetadata<'_>>,
) -> Result<GcRef, MapKeyError> {
    let old = unsafe { backing_ref(m) };
    let generation = unsafe { generation(m) }
        .checked_add(1)
        .expect("Island map generation exhausted");
    let new = unsafe { allocate_backing(gc, m, new_capacity, generation)? };
    if !old.is_null() {
        for old_index in 0..unsafe { backing_capacity(old) } {
            if unsafe { bucket_state(m, old, old_index) } != BUCKET_STATE_OCCUPIED {
                continue;
            }
            let key = unsafe { bucket_key(m, old, old_index) };
            let val = unsafe { bucket_value(m, old, old_index) };
            write_resize_barrier(gc, m, key, unsafe { key_meta(m) }, module);
            write_resize_barrier(gc, m, val, unsafe { val_meta(m) }, module);
            let hash = unsafe { bucket_hash(m, old, old_index) };
            let (_, new_index) = unsafe { find_bucket(m, new, key, hash, module) };
            unsafe { write_bucket(m, new, new_index, hash, key, val) };
            let len = unsafe { backing_len(new) } + 1;
            unsafe {
                set_backing_slot(new, BACKING_LEN_SLOT, len as u64);
                set_backing_slot(new, BACKING_USED_SLOT, len as u64);
            }
        }
    }
    gc.write_barrier(m, new);
    unsafe { MapData::as_mut(m) }.inner = new as Slot;
    Ok(new)
}

unsafe fn with_value_checked<R>(
    m: GcRef,
    key: &[u64],
    module: Option<MapRuntimeMetadata<'_>>,
    consume: impl FnOnce(Option<&[u64]>) -> R,
) -> Result<R, MapKeyError> {
    if key.len() != key_slots(m) as usize {
        return Err(MapKeyError::SlotCountMismatch);
    }
    let hash = unsafe { key_hash_checked(m, key, module)? };
    let backing = unsafe { backing_ref(m) };
    if backing.is_null() {
        return Ok(consume(None));
    }
    let (found, _) = unsafe { find_bucket(m, backing, key, hash, module) };
    Ok(consume(
        found.map(|index| unsafe { bucket_value(m, backing, index) }),
    ))
}

pub unsafe fn get_checked(
    m: GcRef,
    key: &[u64],
    module: Option<ModuleRuntimeMetadata<'_>>,
) -> Result<Option<Box<[u64]>>, MapKeyError> {
    with_value_checked(
        m,
        key,
        module.map(MapRuntimeMetadata::from_module),
        |value| value.map(Into::into),
    )
}

/// Copy a map value into a caller-owned buffer without allocating.
///
/// Returns `true` when the key exists. A missing key zeroes `out` so callers
/// can directly implement the language's zero-value lookup semantics.
pub unsafe fn get_checked_into(
    m: GcRef,
    key: &[u64],
    module: Option<ModuleRuntimeMetadata<'_>>,
    out: &mut [u64],
) -> Result<bool, MapKeyError> {
    if out.len() != val_slots(m) as usize {
        return Err(MapKeyError::SlotCountMismatch);
    }
    with_value_checked(
        m,
        key,
        module.map(MapRuntimeMetadata::from_module),
        |value| {
            if let Some(value) = value {
                out.copy_from_slice(value);
                true
            } else {
                out.fill(0);
                false
            }
        },
    )
}

/// Return the borrowed value cell for a metadata-free one-slot key lookup.
/// The pointer remains valid until the map is mutated or collected.
pub unsafe fn get_trusted_scalar_ptr(m: GcRef, key: u64) -> Result<*const u64, MapKeyError> {
    if !unsafe { supports_trusted_scalar_key(m) } {
        return Err(MapKeyError::SlotCountMismatch);
    }
    unsafe {
        with_value_checked(m, &[key], None, |value| {
            value.map_or(core::ptr::null(), <[u64]>::as_ptr)
        })
    }
}

pub unsafe fn get_with_ok_checked(
    m: GcRef,
    key: &[u64],
    module: Option<ModuleRuntimeMetadata<'_>>,
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
    gc: &mut Gc,
    m: GcRef,
    key: &[u64],
    val: &[u64],
    module: Option<ModuleRuntimeMetadata<'_>>,
) -> Result<(), MapKeyError> {
    unsafe {
        set_checked_with_metadata(
            gc,
            m,
            key,
            val,
            module.map(MapRuntimeMetadata::from_module),
            true,
        )
        .map(|_| ())
    }
}

pub(crate) unsafe fn set_checked_deferred(
    gc: &mut Gc,
    m: GcRef,
    key: &[u64],
    val: &[u64],
    module: Option<ModuleRuntimeMetadata<'_>>,
    allow_allocation: bool,
) -> Result<MapSetOutcome, MapKeyError> {
    unsafe {
        set_checked_with_metadata(
            gc,
            m,
            key,
            val,
            module.map(MapRuntimeMetadata::from_module),
            allow_allocation,
        )
    }
}

pub(crate) unsafe fn set_trusted_scalar_deferred(
    gc: &mut Gc,
    m: GcRef,
    key: u64,
    val: u64,
    module: Option<ModuleRuntimeMetadata<'_>>,
    allow_allocation: bool,
) -> Result<MapSetOutcome, MapKeyError> {
    if !unsafe { supports_trusted_scalar_key(m) } {
        return Err(MapKeyError::SlotCountMismatch);
    }
    unsafe { set_checked_deferred(gc, m, &[key], &[val], module, allow_allocation) }
}

pub(crate) unsafe fn set_checked_with_type_metadata(
    gc: &mut Gc,
    m: GcRef,
    key: &[u64],
    val: &[u64],
    type_metadata: Option<RuntimeTypeMetadata<'_>>,
) -> Result<(), MapKeyError> {
    unsafe {
        set_checked_with_metadata(
            gc,
            m,
            key,
            val,
            type_metadata.map(MapRuntimeMetadata::from_types),
            true,
        )
        .map(|_| ())
    }
}

unsafe fn set_checked_with_metadata(
    gc: &mut Gc,
    m: GcRef,
    key: &[u64],
    val: &[u64],
    module: Option<MapRuntimeMetadata<'_>>,
    allow_allocation: bool,
) -> Result<MapSetOutcome, MapKeyError> {
    validate_entry_slot_counts(m, key.len(), val.len())?;
    let hash = unsafe { key_hash_checked(m, key, module)? };
    let mut backing = unsafe { backing_ref(m) };
    if backing.is_null() {
        if !allow_allocation {
            return Ok(MapSetOutcome::NeedsAllocation);
        }
        backing = unsafe { resize(gc, m, MIN_CAPACITY, module)? };
    }

    let capacity = unsafe { backing_capacity(backing) };
    let (found, mut insertion) = unsafe { find_bucket(m, backing, key, hash, module) };
    if let Some(index) = found {
        unsafe { write_bucket(m, backing, index, hash, key, val) };
        return Ok(MapSetOutcome::Set);
    }

    let mut previous_state = unsafe { bucket_state(m, backing, insertion) };
    let used = unsafe { backing_slot(backing, BACKING_USED_SLOT) as usize };
    let projected_used = used.saturating_add(usize::from(previous_state == BUCKET_STATE_EMPTY));
    if exceeds_load_factor(projected_used, capacity) {
        if !allow_allocation {
            return Ok(MapSetOutcome::NeedsAllocation);
        }
        let live = unsafe { backing_len(backing) };
        let new_capacity = if exceeds_load_factor(live.saturating_add(1), capacity) {
            capacity
                .checked_mul(2)
                .ok_or(MapKeyError::AllocationFailed(
                    MemoryError::HardLimitExceeded,
                ))?
        } else {
            capacity
        };
        backing = unsafe { resize(gc, m, new_capacity, module)? };
        insertion = unsafe { find_bucket(m, backing, key, hash, module).1 };
        previous_state = unsafe { bucket_state(m, backing, insertion) };
    }

    unsafe { write_bucket(m, backing, insertion, hash, key, val) };
    let len = unsafe { backing_len(backing) } + 1;
    unsafe { set_backing_slot(backing, BACKING_LEN_SLOT, len as u64) };
    if previous_state == BUCKET_STATE_EMPTY {
        let used = unsafe { backing_slot(backing, BACKING_USED_SLOT) as usize } + 1;
        unsafe { set_backing_slot(backing, BACKING_USED_SLOT, used as u64) };
    }
    Ok(MapSetOutcome::Set)
}

pub unsafe fn delete_checked(
    m: GcRef,
    key: &[u64],
    module: Option<ModuleRuntimeMetadata<'_>>,
) -> Result<(), MapKeyError> {
    let module = module.map(MapRuntimeMetadata::from_module);
    if key.len() != unsafe { key_slots(m) as usize } {
        return Err(MapKeyError::SlotCountMismatch);
    }
    let hash = unsafe { key_hash_checked(m, key, module)? };
    let backing = unsafe { backing_ref(m) };
    if backing.is_null() {
        return Ok(());
    }
    let (found, _) = unsafe { find_bucket(m, backing, key, hash, module) };
    if let Some(index) = found {
        let offset = unsafe { bucket_offset(m, index) };
        unsafe {
            set_backing_slot(backing, offset, BUCKET_STATE_TOMBSTONE);
            bucket_key_mut(m, backing, index).fill(0);
            bucket_value_mut(m, backing, index).fill(0);
            set_backing_slot(
                backing,
                BACKING_LEN_SLOT,
                backing_len(backing).saturating_sub(1) as u64,
            );
        }
    }
    Ok(())
}

pub unsafe fn delete_trusted_scalar(m: GcRef, key: u64) -> Result<(), MapKeyError> {
    if !unsafe { supports_trusted_scalar_key(m) } {
        return Err(MapKeyError::SlotCountMismatch);
    }
    unsafe { delete_checked(m, &[key], None) }
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

const TAG_ACTIVE: u8 = 0;
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

    MapIterator {
        tag: TAG_ACTIVE,
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

    let backing = unsafe { backing_ref(m) };
    let capacity = unsafe { backing_capacity(backing) };
    let mut index = iter.current_index as usize;
    while index < capacity {
        iter.current_index = (index + 1) as u64;
        if unsafe { bucket_state(m, backing, index) } == BUCKET_STATE_OCCUPIED {
            return consume(Some(unsafe {
                (
                    bucket_key(m, backing, index),
                    bucket_value(m, backing, index),
                )
            }));
        }
        index += 1;
    }
    iter.tag = TAG_EXHAUSTED;
    consume(None)
}

/// Visit exactly one backing bucket without probing past it.
///
/// The outer `Option` is `None` once `index` reaches capacity. The callback
/// receives `None` for an empty/tombstone bucket and an entry for an occupied
/// bucket. GC uses this primitive so every bucket inspection is one bounded
/// work unit.
pub unsafe fn with_bucket_at<R>(
    m: GcRef,
    index: usize,
    consume: impl FnOnce(Option<(&[u64], &[u64])>) -> R,
) -> Option<R> {
    let backing = unsafe { backing_ref(m) };
    if backing.is_null() || index >= unsafe { backing_capacity(backing) } {
        return None;
    }
    if unsafe { bucket_state(m, backing, index) } == BUCKET_STATE_OCCUPIED {
        Some(consume(Some(unsafe {
            (
                bucket_key(m, backing, index),
                bucket_value(m, backing, index),
            )
        })))
    } else {
        Some(consume(None))
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
    // Map backing is a managed Island allocation and is reclaimed by GC.
    unsafe { MapData::as_mut(m) }.inner = 0;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{gc::Gc, objects::string, RuntimeType, ValueKind, ValueMeta, ValueRttid};

    #[test]
    fn empty_map_has_valid_lazy_managed_backing_layout() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let map_ref = create(&mut gc, int_meta, int_meta, 1, 1, 0);
        assert!(!map_ref.is_null());
        assert!(unsafe { has_valid_managed_backing_layout(&gc, map_ref) });

        let forged = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
        unsafe { MapData::as_mut(map_ref) }.inner = forged as Slot;
        assert!(!unsafe { has_valid_managed_backing_layout(&gc, map_ref) });
    }

    #[test]
    fn deferred_set_stops_before_managed_backing_allocation() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = create(&mut gc, int_meta, int_meta, 1, 1, 0);

        assert_eq!(
            unsafe { set_checked_deferred(&mut gc, m, &[7], &[11], None, false) },
            Ok(MapSetOutcome::NeedsAllocation)
        );
        assert!(unsafe { backing_ref(m) }.is_null());
        assert_eq!(unsafe { len(m) }, 0);

        assert_eq!(
            unsafe { set_checked_deferred(&mut gc, m, &[7], &[11], None, true) },
            Ok(MapSetOutcome::Set)
        );
        assert_eq!(unsafe { len(m) }, 1);
        assert_eq!(
            unsafe { get_checked(m, &[7], None) }
                .expect("map read")
                .as_deref(),
            Some(&[11][..])
        );
    }

    #[test]
    fn trusted_scalar_path_matches_generic_get_set_delete() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = create(&mut gc, int_meta, int_meta, 1, 1, 0);

        assert!(unsafe { supports_trusted_scalar_key(m) });
        assert_eq!(
            unsafe { set_trusted_scalar_deferred(&mut gc, m, 7, 21, None, false) },
            Ok(MapSetOutcome::NeedsAllocation)
        );
        assert_eq!(
            unsafe { set_trusted_scalar_deferred(&mut gc, m, 7, 21, None, true) },
            Ok(MapSetOutcome::Set)
        );
        let value = unsafe { get_trusted_scalar_ptr(m, 7) }.expect("scalar lookup");
        assert!(!value.is_null());
        assert_eq!(unsafe { *value }, 21);
        assert!(unsafe { get_trusted_scalar_ptr(m, 8) }
            .expect("missing scalar lookup")
            .is_null());

        unsafe { delete_trusted_scalar(m, 7) }.expect("scalar delete");
        assert!(unsafe { get_trusted_scalar_ptr(m, 7) }
            .expect("deleted scalar lookup")
            .is_null());
    }

    #[test]
    fn raw_map_set_checked_rejects_key_value_width_drift_060() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = create(&mut gc, int_meta, int_meta, 1, 2, 0);

        assert!(
            unsafe { set_checked(&mut gc, m, &[7], &[11], None) }.is_err(),
            "raw map publication must reject values shorter than map value slots"
        );
        assert!(
            unsafe { set_checked(&mut gc, m, &[7, 8], &[11, 22], None) }.is_err(),
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
    fn type_only_metadata_supports_array_keys_across_resize() {
        let mut gc = Gc::new();
        let int_rttid = ValueRttid::new(0, ValueKind::Int64);
        let runtime_types = vec![
            RuntimeType::Basic(ValueKind::Int64),
            RuntimeType::Array {
                len: 2,
                elem: int_rttid,
            },
        ];
        let metadata = RuntimeTypeMetadata::new(&[], &[], &runtime_types);
        let array_meta = ValueMeta::new(1, ValueKind::Array);
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = create(&mut gc, array_meta, int_meta, 2, 1, 1);

        for key in 0..7u64 {
            unsafe {
                set_checked_with_type_metadata(
                    &mut gc,
                    m,
                    &[key, key + 1],
                    &[key + 10],
                    Some(metadata),
                )
            }
            .unwrap();
        }

        assert_eq!(unsafe { len(m) }, 7);
        assert_eq!(unsafe { generation(m) }, 2);
    }

    #[test]
    fn type_only_metadata_barriers_array_values_across_resize() {
        let mut gc = Gc::new();
        let string_rttid = ValueRttid::new(0, ValueKind::String);
        let runtime_types = vec![
            RuntimeType::Basic(ValueKind::String),
            RuntimeType::Array {
                len: 1,
                elem: string_rttid,
            },
        ];
        let metadata = RuntimeTypeMetadata::new(&[], &[], &runtime_types);
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let array_meta = ValueMeta::new(1, ValueKind::Array);
        let m = create(&mut gc, int_meta, array_meta, 1, 1, 0);

        for key in 0..7u64 {
            let value = string::from_rust_str(&mut gc, "array-value");
            unsafe {
                set_checked_with_type_metadata(&mut gc, m, &[key], &[value as u64], Some(metadata))
            }
            .unwrap();
        }

        assert_eq!(unsafe { len(m) }, 7);
        assert_eq!(unsafe { generation(m) }, 2);
    }

    #[test]
    fn updating_existing_entry_does_not_resize_threshold_map() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = create(&mut gc, int_meta, int_meta, 1, 1, 0);
        for key in 0..6 {
            unsafe { set_checked(&mut gc, m, &[key], &[key + 10], None) }.unwrap();
        }

        let backing = unsafe { backing_ref(m) };
        let capacity = unsafe { backing_capacity(backing) };
        let generation_before = unsafe { generation(m) };
        unsafe { set_checked(&mut gc, m, &[3], &[99], None) }.unwrap();

        assert_eq!(unsafe { backing_ref(m) }, backing);
        assert_eq!(unsafe { backing_capacity(backing_ref(m)) }, capacity);
        assert_eq!(unsafe { generation(m) }, generation_before);
        assert_eq!(unsafe { len(m) }, 6);
        assert_eq!(
            unsafe { get_checked(m, &[3], None) }.unwrap().as_deref(),
            Some(&[99][..])
        );
    }

    #[test]
    fn insert_delete_churn_keeps_empty_map_capacity_bounded() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = create(&mut gc, int_meta, int_meta, 1, 1, 0);

        for key in 0..1_024 {
            unsafe { set_checked(&mut gc, m, &[key], &[key], None) }.unwrap();
            unsafe { delete_checked(m, &[key], None) }.unwrap();
        }

        assert_eq!(unsafe { len(m) }, 0);
        assert_eq!(unsafe { backing_capacity(backing_ref(m)) }, MIN_CAPACITY);
        assert!(unsafe { has_valid_managed_backing_layout(&gc, m) });
    }

    #[test]
    fn tombstone_compaction_preserves_entries_iteration_and_gc_roots() {
        let mut gc = Gc::new();
        let string_meta = ValueMeta::new(0, ValueKind::String);
        let m = create(&mut gc, string_meta, string_meta, 1, 1, 0);
        let mut entries = Vec::new();
        for index in 0..6 {
            let key = string::from_rust_str(&mut gc, &format!("key-{index}"));
            let value = string::from_rust_str(&mut gc, &format!("value-{index}"));
            unsafe { set_checked(&mut gc, m, &[key as u64], &[value as u64], None) }.unwrap();
            entries.push((key, value));
        }
        for (key, _) in entries.iter().take(4) {
            unsafe { delete_checked(m, &[*key as u64], None) }.unwrap();
        }

        let backing = unsafe { backing_ref(m) };
        let capacity = unsafe { backing_capacity(backing) };
        let generation_before = unsafe { generation(m) };
        let new_key = (0..64)
            .find_map(|index| {
                let key = string::from_rust_str(&mut gc, &format!("key-new-{index}"));
                let hash = unsafe { key_hash_checked(m, &[key as u64], None) }.unwrap();
                let insertion = unsafe { find_bucket(m, backing, &[key as u64], hash, None).1 };
                (unsafe { bucket_state(m, backing, insertion) } == BUCKET_STATE_EMPTY)
                    .then_some(key)
            })
            .expect("test map must retain an empty insertion bucket");
        let new_value = string::from_rust_str(&mut gc, "value-new");
        unsafe { set_checked(&mut gc, m, &[new_key as u64], &[new_value as u64], None) }.unwrap();

        assert_eq!(unsafe { backing_capacity(backing_ref(m)) }, capacity);
        assert_eq!(unsafe { generation(m) }, generation_before + 1);
        assert_eq!(unsafe { len(m) }, 3);
        for (key, value) in entries.iter().skip(4) {
            assert_eq!(
                unsafe { get_checked(m, &[*key as u64], None) }
                    .unwrap()
                    .as_deref(),
                Some(&[*value as u64][..])
            );
        }
        assert_eq!(
            unsafe { get_checked(m, &[new_key as u64], None) }
                .unwrap()
                .as_deref(),
            Some(&[new_value as u64][..])
        );

        let mut iter = unsafe { iter_init(m) };
        let mut iter_entries = Vec::new();
        let mut key = [0];
        let mut value = [0];
        while unsafe { iter_next_into(&mut iter, &mut key, &mut value) }.unwrap() {
            iter_entries.push((key[0], value[0]));
        }
        iter_entries.sort_unstable();
        let mut expected = vec![
            (entries[4].0 as u64, entries[4].1 as u64),
            (entries[5].0 as u64, entries[5].1 as u64),
            (new_key as u64, new_value as u64),
        ];
        expected.sort_unstable();
        assert_eq!(iter_entries, expected);

        let mut traced = Vec::new();
        crate::test_support::trace_object_children_with_context(
            m,
            crate::gc_types::GcScanContext::new(&[]),
            &|_| crate::gc_types::ClosureScanLayout::default(),
            |child| traced.push(child),
        );
        for (key, value) in entries.iter().skip(4) {
            assert!(traced.contains(key));
            assert!(traced.contains(value));
        }
        assert!(traced.contains(&new_key));
        assert!(traced.contains(&new_value));
        for (key, value) in entries.iter().take(4) {
            assert!(!traced.contains(key));
            assert!(!traced.contains(value));
        }
    }

    #[test]
    fn metadata_dependent_keys_report_missing_module() {
        let mut gc = Gc::new();
        let struct_meta = ValueMeta::new(0, ValueKind::Struct);
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = create(&mut gc, struct_meta, int_meta, 1, 1, 7);

        assert_eq!(
            unsafe { set_checked(&mut gc, m, &[1], &[2], None) },
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

    #[test]
    fn float_keys_use_numeric_equality_and_preserve_nan_entries() {
        let mut gc = Gc::new();
        let float_meta = ValueMeta::new(0, ValueKind::Float64);
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = create(&mut gc, float_meta, int_meta, 1, 1, 0);
        let positive_zero = 0.0_f64.to_bits();
        let negative_zero = (-0.0_f64).to_bits();
        let nan = f64::NAN.to_bits();

        unsafe { set_checked(&mut gc, m, &[positive_zero], &[11], None) }.unwrap();
        assert_eq!(
            unsafe { get_checked(m, &[negative_zero], None) }
                .unwrap()
                .as_deref(),
            Some(&[11][..])
        );
        unsafe { set_checked(&mut gc, m, &[negative_zero], &[22], None) }.unwrap();
        assert_eq!(unsafe { len(m) }, 1);

        unsafe { set_checked(&mut gc, m, &[nan], &[31], None) }.unwrap();
        unsafe { set_checked(&mut gc, m, &[nan], &[32], None) }.unwrap();
        assert_eq!(unsafe { len(m) }, 3);
        assert!(unsafe { get_checked(m, &[nan], None) }.unwrap().is_none());
    }

    #[test]
    fn array_keys_use_recursive_float_and_string_semantics() {
        let mut module = Module::new("array-key-map".to_string());
        let f32_rttid = module.runtime_types.len() as u32;
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::Float32));
        let float_array_rttid = module.runtime_types.len() as u32;
        module.runtime_types.push(RuntimeType::Array {
            len: 2,
            elem: ValueRttid::new(f32_rttid, ValueKind::Float32),
        });
        let string_rttid = module.runtime_types.len() as u32;
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::String));
        let string_array_rttid = module.runtime_types.len() as u32;
        module.runtime_types.push(RuntimeType::Array {
            len: 1,
            elem: ValueRttid::new(string_rttid, ValueKind::String),
        });

        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let float_map = create(
            &mut gc,
            ValueMeta::new(float_array_rttid, ValueKind::Array),
            int_meta,
            2,
            1,
            float_array_rttid,
        );
        let positive = [u64::from(0.0_f32.to_bits()), u64::from(1.0_f32.to_bits())];
        let negative = [
            u64::from((-0.0_f32).to_bits()),
            u64::from(1.0_f32.to_bits()),
        ];
        unsafe { set_checked(&mut gc, float_map, &positive, &[7], Some((&module).into())) }
            .unwrap();
        assert_eq!(
            unsafe { get_checked(float_map, &negative, Some((&module).into())) }
                .unwrap()
                .as_deref(),
            Some(&[7][..])
        );

        let nan = [u64::from(f32::NAN.to_bits()), u64::from(1.0_f32.to_bits())];
        unsafe { set_checked(&mut gc, float_map, &nan, &[8], Some((&module).into())) }.unwrap();
        unsafe { set_checked(&mut gc, float_map, &nan, &[9], Some((&module).into())) }.unwrap();
        assert_eq!(unsafe { len(float_map) }, 3);
        assert!(
            unsafe { get_checked(float_map, &nan, Some((&module).into())) }
                .unwrap()
                .is_none()
        );

        let first = string::from_rust_str(&mut gc, "same");
        let second = string::from_rust_str(&mut gc, "same");
        let string_map = create(
            &mut gc,
            ValueMeta::new(string_array_rttid, ValueKind::Array),
            int_meta,
            1,
            1,
            string_array_rttid,
        );
        unsafe {
            set_checked(
                &mut gc,
                string_map,
                &[first as u64],
                &[13],
                Some((&module).into()),
            )
        }
        .unwrap();
        assert_eq!(
            unsafe { get_checked(string_map, &[second as u64], Some((&module).into())) }
                .unwrap()
                .as_deref(),
            Some(&[13][..])
        );
    }
}
