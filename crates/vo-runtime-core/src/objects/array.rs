//! Array object operations.
//!
//! Array layout: GcHeader + [header slot] + [data...]
//! - GcHeader: is_array=1, kind=elem_kind, meta_id=elem_meta_id
//! - Slot 0 (array header): packed as [len:48 | elem_slots:16]
//! - Slot 1+: element data

use crate::gc::{Gc, GcRef, IS_ARRAY_FLAG};
use vo_common_core::types::ValueKind;

pub const DATA_START: usize = 1;

/// Pack array header: [len:48 | elem_slots:16]
#[inline]
fn pack_header(len: usize, elem_slots: u16) -> u64 {
    ((len as u64) << 16) | (elem_slots as u64)
}

/// Unpack length from array header
#[inline]
pub fn unpack_len(header: u64) -> usize {
    (header >> 16) as usize
}

/// Unpack elem_slots from array header
#[inline]
pub fn unpack_elem_slots(header: u64) -> u16 {
    header as u16
}

/// Calculate data slots needed for array
#[inline]
fn data_slots(length: usize, elem_slots: usize) -> usize {
    length * elem_slots
}

/// Create a new array.
/// elem_kind: element ValueKind
/// elem_meta_id: element's meta_id (for Struct/Interface elements)
/// elem_slots: slots per element (1 for primitives, N for structs)
/// length: number of elements
pub fn create(gc: &mut Gc, elem_kind: u8, elem_meta_id: u32, elem_slots: u16, length: usize) -> GcRef {
    let total_slots = DATA_START + data_slots(length, elem_slots as usize);
    // GcHeader: is_array=1, kind=elem_kind, meta_id=elem_meta_id
    let value_kind = IS_ARRAY_FLAG | elem_kind;
    let arr = gc.alloc(value_kind, elem_meta_id, total_slots as u16);
    let header = pack_header(length, elem_slots);
    Gc::write_slot(arr, 0, header);
    arr
}

/// Get array header slot
#[inline]
fn header(arr: GcRef) -> u64 {
    Gc::read_slot(arr, 0)
}

/// Get array length
pub fn len(arr: GcRef) -> usize {
    unpack_len(header(arr))
}

/// Get elem_slots (slots per element)
pub fn elem_slots(arr: GcRef) -> u16 {
    unpack_elem_slots(header(arr))
}

/// Get element kind from GcHeader
pub fn elem_kind(arr: GcRef) -> ValueKind {
    Gc::header(arr).kind()
}

/// Get element meta_id from GcHeader
pub fn elem_meta_id(arr: GcRef) -> u32 {
    Gc::header(arr).meta_id()
}

/// Get single-slot element at index
pub fn get(arr: GcRef, idx: usize) -> u64 {
    let es = elem_slots(arr) as usize;
    Gc::read_slot(arr, DATA_START + idx * es)
}

/// Set single-slot element at index
pub fn set(arr: GcRef, idx: usize, val: u64) {
    let es = elem_slots(arr) as usize;
    Gc::write_slot(arr, DATA_START + idx * es, val);
}

/// Get multi-slot element at index
pub fn get_n(arr: GcRef, idx: usize, dest: &mut [u64]) {
    let es = elem_slots(arr) as usize;
    for i in 0..es {
        dest[i] = Gc::read_slot(arr, DATA_START + idx * es + i);
    }
}

/// Set multi-slot element at index
pub fn set_n(arr: GcRef, idx: usize, src: &[u64]) {
    let es = elem_slots(arr) as usize;
    for i in 0..es {
        Gc::write_slot(arr, DATA_START + idx * es + i, src[i]);
    }
}

pub fn data_ptr(arr: GcRef) -> *mut u64 {
    unsafe { (arr as *mut u64).add(DATA_START) }
}

pub fn as_bytes(arr: GcRef) -> *const u8 {
    unsafe { (arr as *const u8).add(DATA_START * 8) }
}

pub fn as_bytes_mut(arr: GcRef) -> *mut u8 {
    unsafe { (arr as *mut u8).add(DATA_START * 8) }
}
