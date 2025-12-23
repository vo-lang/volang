//! String object operations.
//!
//! String layout: [array_ref, start, len] (3 slots)
//! - Slot 0: GcRef to byte array
//! - Slot 1: start offset in array
//! - Slot 2: length in bytes

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::{Gc, GcRef};
use crate::objects::array;
use vo_common_core::types::ValueKind;

pub const SLOT_ARRAY: usize = 0;
pub const SLOT_START: usize = 1;
pub const SLOT_LEN: usize = 2;
pub const SLOT_COUNT: u16 = 3;

pub fn create(gc: &mut Gc, bytes: &[u8]) -> GcRef {
    let arr = array::create(gc, ValueKind::Uint8 as u8, 0, 1, bytes.len());
    let data_ptr = array::as_bytes_mut(arr);
    unsafe {
        core::ptr::copy_nonoverlapping(bytes.as_ptr(), data_ptr, bytes.len());
    }
    
    let s = gc.alloc(ValueKind::String as u8, 0, SLOT_COUNT);
    Gc::write_slot(s, SLOT_ARRAY, arr as u64);
    Gc::write_slot(s, SLOT_START, 0);
    Gc::write_slot(s, SLOT_LEN, bytes.len() as u64);
    s
}

pub fn from_rust_str(gc: &mut Gc, s: &str) -> GcRef {
    create(gc, s.as_bytes())
}

pub fn len(s: GcRef) -> usize {
    Gc::read_slot(s, SLOT_LEN) as usize
}

pub fn array_ref(s: GcRef) -> GcRef {
    Gc::read_slot(s, SLOT_ARRAY) as GcRef
}

pub fn start(s: GcRef) -> usize {
    Gc::read_slot(s, SLOT_START) as usize
}

pub fn as_bytes(s: GcRef) -> &'static [u8] {
    let arr = Gc::read_slot(s, SLOT_ARRAY) as GcRef;
    let start_off = Gc::read_slot(s, SLOT_START) as usize;
    let length = Gc::read_slot(s, SLOT_LEN) as usize;
    
    let data_ptr = unsafe { array::as_bytes(arr).add(start_off) };
    unsafe { core::slice::from_raw_parts(data_ptr, length) }
}

pub fn as_str(s: GcRef) -> &'static str {
    unsafe { core::str::from_utf8_unchecked(as_bytes(s)) }
}

pub fn index(s: GcRef, idx: usize) -> u8 {
    as_bytes(s)[idx]
}

pub fn concat(gc: &mut Gc, a: GcRef, b: GcRef) -> GcRef {
    let a_bytes = as_bytes(a);
    let b_bytes = as_bytes(b);
    let mut combined = Vec::with_capacity(a_bytes.len() + b_bytes.len());
    combined.extend_from_slice(a_bytes);
    combined.extend_from_slice(b_bytes);
    create(gc, &combined)
}

pub fn slice_of(gc: &mut Gc, s: GcRef, new_start: usize, new_end: usize) -> GcRef {
    let array = Gc::read_slot(s, SLOT_ARRAY) as GcRef;
    let base_start = Gc::read_slot(s, SLOT_START) as usize;
    
    let new_s = gc.alloc(ValueKind::String as u8, 0, SLOT_COUNT);
    Gc::write_slot(new_s, SLOT_ARRAY, array as u64);
    Gc::write_slot(new_s, SLOT_START, (base_start + new_start) as u64);
    Gc::write_slot(new_s, SLOT_LEN, (new_end - new_start) as u64);
    new_s
}

pub fn eq(a: GcRef, b: GcRef) -> bool {
    if a == b {
        return true;
    }
    if a.is_null() || b.is_null() {
        return a.is_null() && b.is_null();
    }
    as_bytes(a) == as_bytes(b)
}

pub fn ne(a: GcRef, b: GcRef) -> bool {
    !eq(a, b)
}

pub fn lt(a: GcRef, b: GcRef) -> bool {
    as_bytes(a) < as_bytes(b)
}

pub fn le(a: GcRef, b: GcRef) -> bool {
    as_bytes(a) <= as_bytes(b)
}

pub fn gt(a: GcRef, b: GcRef) -> bool {
    as_bytes(a) > as_bytes(b)
}

pub fn ge(a: GcRef, b: GcRef) -> bool {
    as_bytes(a) >= as_bytes(b)
}

pub fn cmp(a: GcRef, b: GcRef) -> i32 {
    let a_bytes = as_bytes(a);
    let b_bytes = as_bytes(b);
    match a_bytes.cmp(b_bytes) {
        core::cmp::Ordering::Less => -1,
        core::cmp::Ordering::Equal => 0,
        core::cmp::Ordering::Greater => 1,
    }
}
