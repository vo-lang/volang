//! Array object operations.
//!
//! Layout: GcHeader + ArrayHeader + [elements...]
//! - GcHeader: is_array=1, kind=elem_kind, meta_id=elem_meta_id
//! - ArrayHeader: len, elem_slots (1 slot = 8 bytes)
//! - Elements: len * elem_slots slots

use crate::gc::{Gc, GcRef, IS_ARRAY_FLAG};
use vo_common_core::types::{ValueKind, ValueMeta};

#[repr(C)]
pub struct ArrayHeader {
    pub len: u32,
    pub elem_slots: u16,
    _reserved: u16,
}

const HEADER_SLOTS: usize = 1;
const _: () = assert!(core::mem::size_of::<ArrayHeader>() == HEADER_SLOTS * 8);

impl ArrayHeader {
    #[inline]
    fn as_ref(arr: GcRef) -> &'static Self {
        unsafe { &*(arr as *const Self) }
    }

    #[inline]
    fn as_mut(arr: GcRef) -> &'static mut Self {
        unsafe { &mut *(arr as *mut Self) }
    }
}

pub fn create(gc: &mut Gc, elem_meta: ValueMeta, elem_slots: u16, length: usize) -> GcRef {
    let total_slots = HEADER_SLOTS + length * elem_slots as usize;
    let array_meta = ValueMeta::from_raw(
        (elem_meta.meta_id() << 8) | (IS_ARRAY_FLAG | elem_meta.value_kind() as u8) as u32
    );
    let arr = gc.alloc(array_meta, total_slots as u16);
    let header = ArrayHeader::as_mut(arr);
    header.len = length as u32;
    header.elem_slots = elem_slots;
    arr
}

#[inline]
pub fn len(arr: GcRef) -> usize {
    ArrayHeader::as_ref(arr).len as usize
}

#[inline]
pub fn elem_slots(arr: GcRef) -> u16 {
    ArrayHeader::as_ref(arr).elem_slots
}

#[inline]
pub fn elem_kind(arr: GcRef) -> ValueKind {
    Gc::header(arr).kind()
}

#[inline]
pub fn elem_meta_id(arr: GcRef) -> u32 {
    Gc::header(arr).meta_id()
}

#[inline]
pub fn elem_meta(arr: GcRef) -> ValueMeta {
    ValueMeta::new(elem_meta_id(arr), elem_kind(arr))
}

#[inline]
fn data_ptr(arr: GcRef) -> *mut u64 {
    unsafe { (arr as *mut u64).add(HEADER_SLOTS) }
}

#[inline]
pub fn get(arr: GcRef, idx: usize) -> u64 {
    let es = elem_slots(arr) as usize;
    unsafe { *data_ptr(arr).add(idx * es) }
}

#[inline]
pub fn set(arr: GcRef, idx: usize, val: u64) {
    let es = elem_slots(arr) as usize;
    unsafe { *data_ptr(arr).add(idx * es) = val }
}

pub fn get_n(arr: GcRef, idx: usize, dest: &mut [u64]) {
    let es = elem_slots(arr) as usize;
    let ptr = unsafe { data_ptr(arr).add(idx * es) };
    for i in 0..es {
        dest[i] = unsafe { *ptr.add(i) };
    }
}

pub fn set_n(arr: GcRef, idx: usize, src: &[u64]) {
    let es = elem_slots(arr) as usize;
    let ptr = unsafe { data_ptr(arr).add(idx * es) };
    for i in 0..es {
        unsafe { *ptr.add(i) = src[i] };
    }
}

pub fn as_bytes(arr: GcRef) -> *const u8 {
    data_ptr(arr) as *const u8
}

pub fn as_bytes_mut(arr: GcRef) -> *mut u8 {
    data_ptr(arr) as *mut u8
}
