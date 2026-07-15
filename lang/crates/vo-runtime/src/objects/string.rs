//! String object operations.
//!
//! String uses SliceData layout for unified ABI (only ValueKind differs).

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::{Gc, GcRef};
use crate::objects::slice::SliceData;
use crate::objects::{array, slice};
use crate::slot::{ptr_to_slot, slot_to_ptr, Slot};
use vo_common_core::types::{ValueKind, ValueMeta};

pub fn create(gc: &mut Gc, bytes: &[u8]) -> GcRef {
    if bytes.is_empty() {
        return core::ptr::null_mut();
    }
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, bytes.len());
    // Safety: `arr` is the live array allocated immediately above.
    let arr_data_ptr = unsafe { array::data_ptr_bytes(arr) };
    unsafe {
        core::ptr::copy_nonoverlapping(bytes.as_ptr(), arr_data_ptr, bytes.len());
    }
    alloc_string(gc, arr, arr_data_ptr, bytes.len())
}

#[inline]
fn alloc_string(gc: &mut Gc, arr: GcRef, data_ptr: *mut u8, len: usize) -> GcRef {
    let s = gc.alloc(ValueMeta::new(0, ValueKind::String), slice::DATA_SLOTS);
    // Safety: `s` is freshly allocated and will be marked for scanning before collection.
    let data = unsafe { SliceData::as_mut(s) };
    data.owner = ptr_to_slot(arr);
    data.data_ptr = ptr_to_slot(data_ptr);
    data.len = len as Slot;
    data.cap = len as Slot;
    data.elem_meta = ValueMeta::new(0, ValueKind::Uint8).to_raw() as Slot;
    data.elem_bytes = 1;
    data.backing_ptr = ptr_to_slot(unsafe { array::data_ptr_bytes(arr) });
    data.backing_len = unsafe { array::len(arr) } as Slot;
    data.storage_stride = 1;
    data.storage_mode = slice::STORAGE_MODE_PACKED;
    gc.mark_allocated_for_scan(s);
    s
}

#[inline]
pub fn from_rust_str(gc: &mut Gc, s: &str) -> GcRef {
    create(gc, s.as_bytes())
}

#[inline]
/// Return the byte length of a live VM string.
///
/// # Safety
///
/// A non-null `s` must point to a live string object for this call.
pub unsafe fn len(s: GcRef) -> usize {
    if s.is_null() {
        return 0;
    }
    slice::len(s)
}
#[inline]
/// Return the byte storage pointer of a live VM string.
///
/// # Safety
///
/// `s` must point to a live string object.
pub unsafe fn data_ptr(s: GcRef) -> *mut u8 {
    slice::data_ptr(s)
}

/// Borrow raw string bytes inside a VM-owned lifetime boundary.
///
/// # Safety
///
/// `s` must remain a live string object for the full returned lifetime, and no
/// GC step may reclaim it while the borrow is used.
pub(crate) unsafe fn bytes_unchecked<'a>(s: GcRef) -> &'a [u8] {
    if s.is_null() {
        return &[];
    }
    core::slice::from_raw_parts(slice::data_ptr(s), slice::len(s))
}

/// Copy the byte representation into host-owned storage.
///
/// # Safety
///
/// A non-null `s` must point to a live string object for this call.
pub unsafe fn to_bytes(s: GcRef) -> Vec<u8> {
    unsafe { bytes_unchecked(s) }.to_vec()
}

/// Copy a VM string into host-owned UTF-8 text after strict validation.
///
/// # Safety
///
/// A non-null `s` must point to a live string object for this call.
pub unsafe fn try_to_rust_string(s: GcRef) -> Result<String, core::str::Utf8Error> {
    core::str::from_utf8(unsafe { bytes_unchecked(s) }).map(String::from)
}

/// Render a VM string for host diagnostics without silently replacing bytes.
///
/// Valid UTF-8 remains readable and each malformed byte is emitted as
/// `\xNN`. Protocol and data boundaries should use [`to_bytes`] or
/// [`try_to_rust_string`] instead.
///
/// # Safety
///
/// A non-null `s` must point to a live string object for this call.
pub unsafe fn to_display_string(s: GcRef) -> String {
    crate::output::render_output_text(unsafe { bytes_unchecked(s) })
}

/// Return one byte from a live VM string.
///
/// # Safety
///
/// `s` must point to a live string object and `idx` must be in bounds.
pub unsafe fn index(s: GcRef, idx: usize) -> u8 {
    (unsafe { bytes_unchecked(s) })[idx]
}

/// Decode UTF-8 rune at byte position. Returns (rune, width).
///
/// # Safety
///
/// A non-null `s` must point to a live string object for this call.
pub unsafe fn decode_rune_at(s: GcRef, pos: usize) -> (i32, usize) {
    let bytes = unsafe { bytes_unchecked(s) };
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
    let Some(&lead) = bytes.first() else {
        return (RUNE_ERROR, 0);
    };
    if lead.is_ascii() {
        return (i32::from(lead), 1);
    }
    let width = match lead {
        0xC2..=0xDF => 2,
        0xE0..=0xEF => 3,
        0xF0..=0xF4 => 4,
        _ => return (RUNE_ERROR, 1),
    };
    let Some(prefix) = bytes.get(..width) else {
        return (RUNE_ERROR, 1);
    };
    match core::str::from_utf8(prefix)
        .ok()
        .and_then(|s| s.chars().next())
    {
        Some(c) => (c as i32, width),
        None => (RUNE_ERROR, 1),
    }
}

/// Concatenate two live VM strings.
///
/// # Safety
///
/// Each non-null input must point to a live string object owned by `gc`.
pub unsafe fn concat(gc: &mut Gc, a: GcRef, b: GcRef) -> GcRef {
    if a.is_null() {
        return b;
    }
    if b.is_null() {
        return a;
    }
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

/// Create an immutable view over part of a live VM string.
///
/// # Safety
///
/// `s` must point to a live string object owned by `gc` when non-null.
pub unsafe fn slice_of(gc: &mut Gc, s: GcRef, start: usize, end: usize) -> Option<GcRef> {
    let len = len(s);
    if start > end || end > len {
        return None;
    }
    if start == end {
        return Some(core::ptr::null_mut());
    }
    let src = SliceData::as_ref(s);
    let arr = slot_to_ptr(src.owner);
    let data_ptr = slot_to_ptr::<u8>(src.data_ptr);
    Some(alloc_string(
        gc,
        arr,
        unsafe { data_ptr.add(start) },
        end - start,
    ))
}

/// Compare two live VM strings by bytes.
///
/// # Safety
///
/// Each non-null input must point to a live string object.
pub unsafe fn eq(a: GcRef, b: GcRef) -> bool {
    if a == b {
        return true;
    }
    if a.is_null() || b.is_null() {
        return false;
    }
    unsafe { bytes_unchecked(a) == bytes_unchecked(b) }
}

/// Compare two live VM strings by bytes.
///
/// # Safety
///
/// Each non-null input must point to a live string object.
pub unsafe fn ne(a: GcRef, b: GcRef) -> bool {
    !unsafe { eq(a, b) }
}

macro_rules! str_cmp {
    ($name:ident, $op:tt) => {
        #[doc = "Compare two live VM strings lexicographically."]
        ///
        /// # Safety
        ///
        /// Each non-null input must point to a live string object.
        pub unsafe fn $name(a: GcRef, b: GcRef) -> bool {
            unsafe { bytes_unchecked(a) $op bytes_unchecked(b) }
        }
    };
}
str_cmp!(lt, <);
str_cmp!(le, <=);
str_cmp!(gt, >);
str_cmp!(ge, >=);

/// Compare two live VM strings lexicographically.
///
/// # Safety
///
/// Each non-null input must point to a live string object.
pub unsafe fn cmp(a: GcRef, b: GcRef) -> i32 {
    match unsafe { bytes_unchecked(a).cmp(bytes_unchecked(b)) } {
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

/// Create string from a byte slice object. Copies the data (strings are immutable).
/// Copy a live byte slice into a VM string.
///
/// # Safety
///
/// `slice_ref` must point to a live byte-slice object owned by `gc` when non-null.
pub unsafe fn from_slice(gc: &mut Gc, slice_ref: GcRef) -> GcRef {
    if slice_ref.is_null() {
        return core::ptr::null_mut();
    }
    let len = slice::len(slice_ref);
    if len == 0 {
        return core::ptr::null_mut();
    }
    // Must copy data - strings are immutable, but the source slice may be mutated later.
    let mut bytes = Vec::with_capacity(len);
    for index in 0..len {
        bytes.push(unsafe { slice::get(slice_ref, index, 1) } as u8);
    }
    create(gc, &bytes)
}

/// Convert string to []byte slice object. Returns slice GcRef.
/// Copy a live VM string into a byte-slice object.
///
/// # Safety
///
/// `s` must point to a live string object owned by `gc` when non-null.
pub unsafe fn to_byte_slice_obj(gc: &mut Gc, s: GcRef) -> GcRef {
    let bytes = unsafe { bytes_unchecked(s) };
    let len = bytes.len();
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, len);
    let arr_data_ptr = array::data_ptr_bytes(arr);
    unsafe {
        core::ptr::copy_nonoverlapping(bytes.as_ptr(), arr_data_ptr, len);
    }
    slice::from_array_range(gc, arr, 0, len)
}

/// Create string from a rune slice object (GcRef to SliceData).
/// Encode a live rune slice as a VM string.
///
/// # Safety
///
/// `slice_ref` must point to a live rune-slice object owned by `gc` when non-null.
pub unsafe fn from_rune_slice_obj(gc: &mut Gc, slice_ref: GcRef) -> GcRef {
    if slice_ref.is_null() {
        return core::ptr::null_mut();
    }
    let len = slice::len(slice_ref);
    if len == 0 {
        return core::ptr::null_mut();
    }
    // Read runes and encode to UTF-8
    let mut utf8_bytes = Vec::new();
    for i in 0..len {
        let mut rune = [0u64; 1];
        unsafe { slice::read_logical_slots(slice_ref, i, &mut rune) };
        let rune = rune[0] as i32 as u32;
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
/// Decode a live VM string into a rune-slice object.
///
/// # Safety
///
/// `s` must point to a live string object owned by `gc` when non-null.
pub unsafe fn to_rune_slice_obj(gc: &mut Gc, s: GcRef) -> GcRef {
    let bytes = unsafe { bytes_unchecked(s) };
    let mut runes = Vec::new();
    let mut offset = 0;
    while offset < bytes.len() {
        let (rune, width) = decode_rune(&bytes[offset..]);
        runes.push(rune);
        offset += width;
    }
    let len = runes.len();
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Int32), 4, len);
    let arr_data_ptr = array::data_ptr_bytes(arr) as *mut i32;
    for (i, rune) in runes.into_iter().enumerate() {
        unsafe {
            *arr_data_ptr.add(i) = rune;
        }
    }
    slice::from_array_range(gc, arr, 0, len)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_to_slices_preserves_non_nil_empty_and_invalid_utf8_per_byte() {
        let mut gc = Gc::new();
        let empty_bytes = unsafe { to_byte_slice_obj(&mut gc, core::ptr::null_mut()) };
        let empty_runes = unsafe { to_rune_slice_obj(&mut gc, core::ptr::null_mut()) };
        assert!(!empty_bytes.is_null());
        assert!(!empty_runes.is_null());
        assert_eq!(unsafe { slice::len(empty_bytes) }, 0);
        assert_eq!(unsafe { slice::len(empty_runes) }, 0);

        let invalid = create(&mut gc, &[0xFF, 0xFE, b'A']);
        let decoded = unsafe { to_rune_slice_obj(&mut gc, invalid) };
        assert_eq!(unsafe { slice::len(decoded) }, 3);
        let data = unsafe { slice::data_ptr(decoded) } as *const i32;
        let values = unsafe { core::slice::from_raw_parts(data, 3) };
        assert_eq!(values, &[RUNE_ERROR, RUNE_ERROR, 'A' as i32]);
    }

    #[test]
    fn rune_decode_uses_only_the_leading_sequence_and_recovers_one_invalid_byte() {
        assert_eq!(decode_rune(&[b'A', 0xFF]), ('A' as i32, 1));
        assert_eq!(decode_rune("€\u{FFFD}".as_bytes()), ('€' as i32, 3));
        assert_eq!(decode_rune(&[0xE2, 0x82]), (RUNE_ERROR, 1));
        assert_eq!(decode_rune(&[0xE2, b'A', 0xAC]), (RUNE_ERROR, 1));
        assert_eq!(decode_rune(&[]), (RUNE_ERROR, 0));
    }

    #[test]
    fn host_text_conversion_is_strict_and_diagnostics_escape_invalid_bytes() {
        let mut gc = Gc::new();
        let raw = create(&mut gc, b"a\xffz");

        assert!(unsafe { try_to_rust_string(raw) }.is_err());
        assert_eq!(unsafe { to_bytes(raw) }, b"a\xffz");
        assert_eq!(unsafe { to_display_string(raw) }, "a\\xffz");
    }
}
