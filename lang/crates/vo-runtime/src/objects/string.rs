//! String object operations.
//!
//! String uses SliceData layout for unified ABI (only ValueKind differs).

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::{Gc, GcRef};
use crate::objects::{array, slice};
use crate::objects::slice::SliceData;
use crate::slot::{ptr_to_slot, slot_to_ptr, Slot};
use vo_common_core::types::{ValueKind, ValueMeta};

pub fn create(gc: &mut Gc, bytes: &[u8]) -> GcRef {
    if bytes.is_empty() {
        return core::ptr::null_mut();
    }
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, bytes.len());
    let arr_data_ptr = array::data_ptr_bytes(arr);
    unsafe { core::ptr::copy_nonoverlapping(bytes.as_ptr(), arr_data_ptr, bytes.len()); }
    alloc_string(gc, arr, arr_data_ptr, bytes.len())
}

#[inline]
fn alloc_string(gc: &mut Gc, arr: GcRef, data_ptr: *mut u8, len: usize) -> GcRef {
    let s = gc.alloc(ValueMeta::new(0, ValueKind::String), slice::DATA_SLOTS);
    let data = SliceData::as_mut(s);
    data.array = ptr_to_slot(arr);
    data.data_ptr = ptr_to_slot(data_ptr);
    data.len = len as Slot;
    data.cap = len as Slot;
    s
}

#[inline]
pub fn from_rust_str(gc: &mut Gc, s: &str) -> GcRef {
    create(gc, s.as_bytes())
}

#[inline]
pub fn len(s: GcRef) -> usize {
    if s.is_null() { return 0; }
    slice::len(s)
}
#[inline]
pub fn data_ptr(s: GcRef) -> *mut u8 { slice::data_ptr(s) }

pub fn as_bytes(s: GcRef) -> &'static [u8] {
    if s.is_null() { return &[]; }
    unsafe { core::slice::from_raw_parts(slice::data_ptr(s), slice::len(s)) }
}

pub fn as_str(s: GcRef) -> &'static str {
    unsafe { core::str::from_utf8_unchecked(as_bytes(s)) }
}

pub fn index(s: GcRef, idx: usize) -> u8 {
    as_bytes(s)[idx]
}

/// Decode UTF-8 rune at byte position. Returns (rune, width).
pub fn decode_rune_at(s: GcRef, pos: usize) -> (i32, usize) {
    let bytes = as_bytes(s);
    if pos >= bytes.len() {
        return (RUNE_ERROR, 0);
    }
    decode_rune(&bytes[pos..])
}

/// Unicode replacement character returned for invalid UTF-8.
pub const RUNE_ERROR: i32 = 0xFFFD;

/// Decode a single UTF-8 rune from bytes.
/// Returns (rune, width). For invalid UTF-8, returns (RUNE_ERROR, 1).
fn decode_rune(bytes: &[u8]) -> (i32, usize) {
    match core::str::from_utf8(bytes).ok().and_then(|s| s.chars().next()) {
        Some(c) => (c as i32, c.len_utf8()),
        None if !bytes.is_empty() => (RUNE_ERROR, 1),
        None => (RUNE_ERROR, 0),
    }
}

pub fn concat(gc: &mut Gc, a: GcRef, b: GcRef) -> GcRef {
    if a.is_null() { return b; }
    if b.is_null() { return a; }
    let a_len = slice::len(a);
    let b_len = slice::len(b);
    let total = a_len + b_len;
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, total);
    let arr_ptr = array::data_ptr_bytes(arr);
    unsafe {
        core::ptr::copy_nonoverlapping(slice::data_ptr(a), arr_ptr, a_len);
        core::ptr::copy_nonoverlapping(slice::data_ptr(b), arr_ptr.add(a_len), b_len);
    }
    alloc_string(gc, arr, arr_ptr, total)
}

pub fn slice_of(gc: &mut Gc, s: GcRef, start: usize, end: usize) -> GcRef {
    if s.is_null() || start >= end { return core::ptr::null_mut(); }
    let src = SliceData::as_ref(s);
    let arr = slot_to_ptr(src.array);
    let data_ptr = slot_to_ptr::<u8>(src.data_ptr);
    alloc_string(gc, arr, unsafe { data_ptr.add(start) }, end - start)
}

pub fn eq(a: GcRef, b: GcRef) -> bool {
    if a == b { return true; }
    if a.is_null() || b.is_null() { return false; }
    as_bytes(a) == as_bytes(b)
}

pub fn ne(a: GcRef, b: GcRef) -> bool { !eq(a, b) }

macro_rules! str_cmp {
    ($name:ident, $op:tt) => {
        pub fn $name(a: GcRef, b: GcRef) -> bool { as_bytes(a) $op as_bytes(b) }
    };
}
str_cmp!(lt, <);
str_cmp!(le, <=);
str_cmp!(gt, >);
str_cmp!(ge, >=);

pub fn cmp(a: GcRef, b: GcRef) -> i32 {
    match as_bytes(a).cmp(as_bytes(b)) {
        core::cmp::Ordering::Less => -1,
        core::cmp::Ordering::Equal => 0,
        core::cmp::Ordering::Greater => 1,
    }
}

/// Create string from a Rust String (takes ownership).
#[inline]
pub fn new_from_string(gc: &mut Gc, s: String) -> GcRef {
    create(gc, s.as_bytes())
}

/// Create string from a byte slice object. Shares the underlying array.
pub fn from_slice(gc: &mut Gc, slice_ref: GcRef) -> GcRef {
    if slice_ref.is_null() { return core::ptr::null_mut(); }
    let len = slice::len(slice_ref);
    if len == 0 { return core::ptr::null_mut(); }
    alloc_string(gc, slice::array_ref(slice_ref), slice::data_ptr(slice_ref), len)
}

/// Convert string to []byte slice object. Returns slice GcRef.
pub fn to_byte_slice_obj(gc: &mut Gc, s: GcRef) -> GcRef {
    let bytes = as_bytes(s);
    let len = bytes.len();
    if len == 0 {
        return core::ptr::null_mut();
    }
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, len);
    let arr_data_ptr = array::data_ptr_bytes(arr);
    unsafe { core::ptr::copy_nonoverlapping(bytes.as_ptr(), arr_data_ptr, len); }
    slice::from_array_range(gc, arr, 0, len)
}

/// Create string from a rune slice object (GcRef to SliceData).
pub fn from_rune_slice_obj(gc: &mut Gc, slice_ref: GcRef) -> GcRef {
    if slice_ref.is_null() {
        return core::ptr::null_mut();
    }
    let rune_data_ptr = slice::data_ptr(slice_ref) as *const i32;
    let len = slice::len(slice_ref);
    if len == 0 {
        return core::ptr::null_mut();
    }
    // Read runes and encode to UTF-8
    let mut utf8_bytes = Vec::new();
    for i in 0..len {
        let rune = unsafe { *rune_data_ptr.add(i) } as u32;
        if let Some(c) = char::from_u32(rune) {
            let mut buf = [0u8; 4];
            let encoded = c.encode_utf8(&mut buf);
            utf8_bytes.extend_from_slice(encoded.as_bytes());
        } else {
            utf8_bytes.extend_from_slice("\u{FFFD}".as_bytes());
        }
    }
    create(gc, &utf8_bytes)
}

/// Convert string to []rune slice object. Returns slice GcRef.
pub fn to_rune_slice_obj(gc: &mut Gc, s: GcRef) -> GcRef {
    let str_data = as_str(s);
    let len = str_data.chars().count();
    if len == 0 {
        return core::ptr::null_mut();
    }
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Int32), 4, len);
    let arr_data_ptr = array::data_ptr_bytes(arr) as *mut i32;
    for (i, c) in str_data.chars().enumerate() {
        unsafe { *arr_data_ptr.add(i) = c as i32; }
    }
    slice::from_array_range(gc, arr, 0, len)
}
