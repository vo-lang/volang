//! bytes package C ABI for AOT.

use gox_runtime_core::builtins::bytes as core;
use gox_runtime_core::gc::GcRef;
use gox_runtime_core::objects::slice;

/// Helper to get bytes from a byte slice GcRef
unsafe fn slice_to_bytes(s: GcRef) -> &'static [u8] {
    let len = slice::len(s);
    let arr = slice::array_ref(s);
    let start = slice::start(s);
    let data_ptr = gox_runtime_core::gc::Gc::get_data_ptr(arr).add(3 + start) as *const u8;
    std::slice::from_raw_parts(data_ptr, len)
}

#[no_mangle]
pub unsafe extern "C" fn gox_bytes_index(s: GcRef, sep: GcRef) -> i64 {
    core::index(slice_to_bytes(s), slice_to_bytes(sep))
}

#[no_mangle]
pub unsafe extern "C" fn gox_bytes_last_index(s: GcRef, sep: GcRef) -> i64 {
    core::last_index(slice_to_bytes(s), slice_to_bytes(sep))
}

#[no_mangle]
pub unsafe extern "C" fn gox_bytes_count(s: GcRef, sep: GcRef) -> i64 {
    core::count(slice_to_bytes(s), slice_to_bytes(sep))
}

#[no_mangle]
pub unsafe extern "C" fn gox_bytes_compare(a: GcRef, b: GcRef) -> i64 {
    core::compare(slice_to_bytes(a), slice_to_bytes(b))
}

#[no_mangle]
pub unsafe extern "C" fn gox_bytes_equal(a: GcRef, b: GcRef) -> bool {
    core::equal(slice_to_bytes(a), slice_to_bytes(b))
}

#[no_mangle]
pub unsafe extern "C" fn gox_bytes_contains(s: GcRef, subslice: GcRef) -> bool {
    core::contains(slice_to_bytes(s), slice_to_bytes(subslice))
}

#[no_mangle]
pub unsafe extern "C" fn gox_bytes_has_prefix(s: GcRef, prefix: GcRef) -> bool {
    core::has_prefix(slice_to_bytes(s), slice_to_bytes(prefix))
}

#[no_mangle]
pub unsafe extern "C" fn gox_bytes_has_suffix(s: GcRef, suffix: GcRef) -> bool {
    core::has_suffix(slice_to_bytes(s), slice_to_bytes(suffix))
}
