//! String object operations.
//!
//! Layout: GcHeader + StringData
//! String references an underlying byte array with start offset.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::{Gc, GcRef};
use crate::objects::array;
use vo_common_core::types::{ValueKind, ValueMeta};

#[repr(C)]
pub struct StringData {
    pub array: GcRef,
    pub start: u32,
    pub len: u32,
}

const DATA_SLOTS: u16 = 2;
const _: () = assert!(core::mem::size_of::<StringData>() == DATA_SLOTS as usize * 8);

impl StringData {
    #[inline]
    fn as_ref(s: GcRef) -> &'static Self {
        unsafe { &*(s as *const Self) }
    }

    #[inline]
    fn as_mut(s: GcRef) -> &'static mut Self {
        unsafe { &mut *(s as *mut Self) }
    }
}

pub fn create(gc: &mut Gc, bytes: &[u8]) -> GcRef {
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, bytes.len());
    let data_ptr = array::as_bytes_mut(arr);
    unsafe { core::ptr::copy_nonoverlapping(bytes.as_ptr(), data_ptr, bytes.len()); }
    
    let s = gc.alloc(ValueMeta::new(0, ValueKind::String), DATA_SLOTS);
    let data = StringData::as_mut(s);
    data.array = arr;
    data.start = 0;
    data.len = bytes.len() as u32;
    s
}

pub fn from_rust_str(gc: &mut Gc, s: &str) -> GcRef {
    create(gc, s.as_bytes())
}

#[inline]
pub fn len(s: GcRef) -> usize { StringData::as_ref(s).len as usize }
#[inline]
pub fn array_ref(s: GcRef) -> GcRef { StringData::as_ref(s).array }
#[inline]
pub fn start(s: GcRef) -> usize { StringData::as_ref(s).start as usize }

pub fn as_bytes(s: GcRef) -> &'static [u8] {
    let data = StringData::as_ref(s);
    let ptr = unsafe { array::as_bytes(data.array).add(data.start as usize) };
    unsafe { core::slice::from_raw_parts(ptr, data.len as usize) }
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
    let src = StringData::as_ref(s);
    let new_s = gc.alloc(ValueMeta::new(0, ValueKind::String), DATA_SLOTS);
    let data = StringData::as_mut(new_s);
    data.array = src.array;
    data.start = src.start + new_start as u32;
    data.len = (new_end - new_start) as u32;
    new_s
}

pub fn eq(a: GcRef, b: GcRef) -> bool {
    if a == b { return true; }
    if a.is_null() || b.is_null() { return a.is_null() && b.is_null(); }
    as_bytes(a) == as_bytes(b)
}

pub fn ne(a: GcRef, b: GcRef) -> bool { !eq(a, b) }
pub fn lt(a: GcRef, b: GcRef) -> bool { as_bytes(a) < as_bytes(b) }
pub fn le(a: GcRef, b: GcRef) -> bool { as_bytes(a) <= as_bytes(b) }
pub fn gt(a: GcRef, b: GcRef) -> bool { as_bytes(a) > as_bytes(b) }
pub fn ge(a: GcRef, b: GcRef) -> bool { as_bytes(a) >= as_bytes(b) }

pub fn cmp(a: GcRef, b: GcRef) -> i32 {
    match as_bytes(a).cmp(as_bytes(b)) {
        core::cmp::Ordering::Less => -1,
        core::cmp::Ordering::Equal => 0,
        core::cmp::Ordering::Greater => 1,
    }
}
