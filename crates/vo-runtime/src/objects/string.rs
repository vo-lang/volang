//! String object operations.
//!
//! Layout: GcHeader + StringData
//! String references an underlying byte array with start offset.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::{Gc, GcRef};
use crate::objects::array;
use vo_common_core::types::{ValueKind, ValueMeta};

use super::impl_gc_object;

#[repr(C)]
pub struct StringData {
    pub array: GcRef,
    pub start: u32,
    pub len: u32,
}

pub const DATA_SLOTS: u16 = 2;
const _: () = assert!(core::mem::size_of::<StringData>() == DATA_SLOTS as usize * 8);

// Field offsets for inline access
pub const FIELD_ARRAY: usize = 0;   // u64 offset for array GcRef
pub const FIELD_START: usize = 2;   // u32 offset (byte 8)
pub const FIELD_LEN: usize = 3;     // u32 offset (byte 12)

impl_gc_object!(StringData);

pub fn create(gc: &mut Gc, bytes: &[u8]) -> GcRef {
    // Empty string is represented as null GcRef (zero value)
    if bytes.is_empty() {
        return core::ptr::null_mut();
    }
    
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, bytes.len());
    let data_ptr = array::data_ptr_bytes(arr);
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
pub fn len(s: GcRef) -> usize {
    if s.is_null() { return 0; }
    StringData::as_ref(s).len as usize
}
#[inline]
pub fn array_ref(s: GcRef) -> GcRef { StringData::as_ref(s).array }
#[inline]
pub fn start(s: GcRef) -> usize { StringData::as_ref(s).start as usize }

pub fn as_bytes(s: GcRef) -> &'static [u8] {
    if s.is_null() { return &[]; }
    let data = StringData::as_ref(s);
    let ptr = unsafe { array::data_ptr_bytes(data.array).add(data.start as usize) };
    unsafe { core::slice::from_raw_parts(ptr, data.len as usize) }
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
    if bytes.is_empty() {
        return (RUNE_ERROR, 0);
    }
    
    let b0 = bytes[0];
    
    // 1-byte (ASCII): 0xxxxxxx
    if b0 < 0x80 {
        return (b0 as i32, 1);
    }
    
    // 2-byte: 110xxxxx 10xxxxxx
    if b0 & 0xE0 == 0xC0 && bytes.len() >= 2 {
        let b1 = bytes[1];
        if b1 & 0xC0 == 0x80 {
            let r = ((b0 as i32 & 0x1F) << 6) | (b1 as i32 & 0x3F);
            if r >= 0x80 {
                return (r, 2);
            }
        }
    }
    
    // 3-byte: 1110xxxx 10xxxxxx 10xxxxxx
    if b0 & 0xF0 == 0xE0 && bytes.len() >= 3 {
        let b1 = bytes[1];
        let b2 = bytes[2];
        if b1 & 0xC0 == 0x80 && b2 & 0xC0 == 0x80 {
            let r = ((b0 as i32 & 0x0F) << 12) | ((b1 as i32 & 0x3F) << 6) | (b2 as i32 & 0x3F);
            if r >= 0x800 && !(0xD800..=0xDFFF).contains(&r) {
                return (r, 3);
            }
        }
    }
    
    // 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    if b0 & 0xF8 == 0xF0 && bytes.len() >= 4 {
        let b1 = bytes[1];
        let b2 = bytes[2];
        let b3 = bytes[3];
        if b1 & 0xC0 == 0x80 && b2 & 0xC0 == 0x80 && b3 & 0xC0 == 0x80 {
            let r = ((b0 as i32 & 0x07) << 18) | ((b1 as i32 & 0x3F) << 12) 
                  | ((b2 as i32 & 0x3F) << 6) | (b3 as i32 & 0x3F);
            if r >= 0x10000 && r <= 0x10FFFF {
                return (r, 4);
            }
        }
    }
    
    // Invalid UTF-8: return replacement character and advance 1 byte
    (RUNE_ERROR, 1)
}

pub fn concat(gc: &mut Gc, a: GcRef, b: GcRef) -> GcRef {
    // Handle null (empty string) cases
    if a.is_null() { return b; }
    if b.is_null() { return a; }
    let a_bytes = as_bytes(a);
    let b_bytes = as_bytes(b);
    let mut combined = Vec::with_capacity(a_bytes.len() + b_bytes.len());
    combined.extend_from_slice(a_bytes);
    combined.extend_from_slice(b_bytes);
    create(gc, &combined)
}

pub fn slice_of(gc: &mut Gc, s: GcRef, new_start: usize, new_end: usize) -> GcRef {
    // null is empty string, slicing returns null
    if s.is_null() || new_start >= new_end { return core::ptr::null_mut(); }
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
    if a.is_null() || b.is_null() { return false; }
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
