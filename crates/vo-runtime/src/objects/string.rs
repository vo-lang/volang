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
    match core::str::from_utf8(bytes).ok().and_then(|s| s.chars().next()) {
        Some(c) => (c as i32, c.len_utf8()),
        None if !bytes.is_empty() => (RUNE_ERROR, 1),
        None => (RUNE_ERROR, 0),
    }
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
pub fn new_from_string(gc: &mut Gc, s: String) -> GcRef {
    create(gc, s.as_bytes())
}

/// Create string from a byte slice object (GcRef to SliceData).
/// Shares the underlying array - no copy needed since string is immutable.
pub fn from_slice(gc: &mut Gc, slice_ref: GcRef) -> GcRef {
    if slice_ref.is_null() {
        return core::ptr::null_mut();
    }
    let len = super::slice::len(slice_ref);
    if len == 0 {
        return core::ptr::null_mut();
    }
    // Get slice's underlying array and calculate start offset
    let arr = super::slice::array_ref(slice_ref);
    let arr_data_ptr = array::data_ptr_bytes(arr) as usize;
    let slice_data_ptr = super::slice::data_ptr(slice_ref) as usize;
    let start = (slice_data_ptr - arr_data_ptr) as u32;
    
    // Create string sharing the same underlying array
    let s = gc.alloc(ValueMeta::new(0, ValueKind::String), DATA_SLOTS);
    let data = StringData::as_mut(s);
    data.array = arr;
    data.start = start;
    data.len = len as u32;
    s
}

/// Convert string to []byte slice object. Returns slice GcRef.
pub fn to_byte_slice_obj(gc: &mut Gc, s: GcRef) -> GcRef {
    let bytes = as_bytes(s);
    let len = bytes.len();
    if len == 0 {
        return core::ptr::null_mut();
    }
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, len);
    let data_ptr = array::data_ptr_bytes(arr);
    unsafe { core::ptr::copy_nonoverlapping(bytes.as_ptr(), data_ptr, len); }
    super::slice::from_array_range(gc, arr, 0, len, len)
}

/// Create string from a rune slice object (GcRef to SliceData).
pub fn from_rune_slice_obj(gc: &mut Gc, slice_ref: GcRef) -> GcRef {
    if slice_ref.is_null() {
        return core::ptr::null_mut();
    }
    let data_ptr = super::slice::data_ptr(slice_ref) as *const i32;
    let len = super::slice::len(slice_ref);
    if len == 0 {
        return core::ptr::null_mut();
    }
    // Read runes and encode to UTF-8
    let mut utf8_bytes = Vec::new();
    for i in 0..len {
        let rune = unsafe { *data_ptr.add(i) } as u32;
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
    let data_ptr = array::data_ptr_bytes(arr) as *mut i32;
    for (i, c) in str_data.chars().enumerate() {
        unsafe { *data_ptr.add(i) = c as i32; }
    }
    super::slice::from_array_range(gc, arr, 0, len, len)
}
