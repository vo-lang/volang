//! bytes package C ABI for JIT.

use vo_runtime_core::builtins::bytes as core;
use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::{array, slice};
use vo_common_core::ValueKind;

/// Helper to get bytes from a byte slice GcRef (now packed storage)
unsafe fn slice_to_bytes(s: GcRef) -> &'static [u8] {
    let len = slice::len(s);
    let arr = slice::array_ref(s);
    let start = slice::start(s);
    let ptr = array::as_bytes(arr).add(start);
    std::slice::from_raw_parts(ptr, len)
}

/// Helper to create a byte slice from Rust bytes (packed storage)
unsafe fn bytes_to_slice(gc: &mut Gc, bytes: &[u8]) -> GcRef {
    let len = bytes.len();
    if len == 0 {
        return std::ptr::null_mut();
    }
    
    let arr = array::create(gc, ValueKind::Uint8 as u8, 0, 1, len);
    // Direct memory copy for packed bytes
    let dest = array::as_bytes_mut(arr);
    std::ptr::copy_nonoverlapping(bytes.as_ptr(), dest, len);
    slice::create(gc, arr, 0, len, len)
}

#[no_mangle]
pub unsafe extern "C" fn vo_bytes_index(s: GcRef, sep: GcRef) -> i64 {
    core::index(slice_to_bytes(s), slice_to_bytes(sep))
}

#[no_mangle]
pub unsafe extern "C" fn vo_bytes_last_index(s: GcRef, sep: GcRef) -> i64 {
    core::last_index(slice_to_bytes(s), slice_to_bytes(sep))
}

#[no_mangle]
pub unsafe extern "C" fn vo_bytes_count(s: GcRef, sep: GcRef) -> i64 {
    core::count(slice_to_bytes(s), slice_to_bytes(sep))
}

#[no_mangle]
pub unsafe extern "C" fn vo_bytes_compare(a: GcRef, b: GcRef) -> i64 {
    core::compare(slice_to_bytes(a), slice_to_bytes(b))
}

#[no_mangle]
pub unsafe extern "C" fn vo_bytes_equal(a: GcRef, b: GcRef) -> bool {
    core::equal(slice_to_bytes(a), slice_to_bytes(b))
}

#[no_mangle]
pub unsafe extern "C" fn vo_bytes_index_byte(s: GcRef, c: u8) -> i64 {
    core::index_byte(slice_to_bytes(s), c)
}

#[no_mangle]
pub unsafe extern "C" fn vo_bytes_last_index_byte(s: GcRef, c: u8) -> i64 {
    core::last_index_byte(slice_to_bytes(s), c)
}

#[no_mangle]
pub unsafe extern "C" fn vo_bytes_contains(s: GcRef, subslice: GcRef) -> bool {
    core::contains(slice_to_bytes(s), slice_to_bytes(subslice))
}

#[no_mangle]
pub unsafe extern "C" fn vo_bytes_has_prefix(s: GcRef, prefix: GcRef) -> bool {
    core::has_prefix(slice_to_bytes(s), slice_to_bytes(prefix))
}

#[no_mangle]
pub unsafe extern "C" fn vo_bytes_has_suffix(s: GcRef, suffix: GcRef) -> bool {
    core::has_suffix(slice_to_bytes(s), slice_to_bytes(suffix))
}

#[no_mangle]
pub unsafe extern "C" fn vo_bytes_to_lower(gc: *mut Gc, s: GcRef) -> GcRef {
    let result = core::to_lower(slice_to_bytes(s));
    bytes_to_slice(&mut *gc, &result)
}

#[no_mangle]
pub unsafe extern "C" fn vo_bytes_to_upper(gc: *mut Gc, s: GcRef) -> GcRef {
    let result = core::to_upper(slice_to_bytes(s));
    bytes_to_slice(&mut *gc, &result)
}

#[no_mangle]
pub unsafe extern "C" fn vo_bytes_repeat(gc: *mut Gc, s: GcRef, count: i64) -> GcRef {
    let result = core::repeat(slice_to_bytes(s), count);
    bytes_to_slice(&mut *gc, &result)
}
