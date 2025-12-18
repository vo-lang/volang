//! sort package C ABI for AOT.

use gox_runtime_core::gc::{Gc, GcRef};
use gox_runtime_core::objects::slice;

/// Helper to get mutable i64 slice data
unsafe fn get_i64_slice_mut(s: GcRef) -> &'static mut [i64] {
    let len = slice::len(s);
    let arr = slice::array_ref(s);
    let start = slice::start(s);
    let data_ptr = Gc::get_data_ptr(arr).add(3 + start) as *mut i64;
    std::slice::from_raw_parts_mut(data_ptr, len)
}

/// Helper to get i64 slice data
unsafe fn get_i64_slice(s: GcRef) -> &'static [i64] {
    let len = slice::len(s);
    let arr = slice::array_ref(s);
    let start = slice::start(s);
    let data_ptr = Gc::get_data_ptr(arr).add(3 + start) as *const i64;
    std::slice::from_raw_parts(data_ptr, len)
}

/// Helper to get mutable f64 slice data
unsafe fn get_f64_slice_mut(s: GcRef) -> &'static mut [f64] {
    let len = slice::len(s);
    let arr = slice::array_ref(s);
    let start = slice::start(s);
    let data_ptr = Gc::get_data_ptr(arr).add(3 + start) as *mut f64;
    std::slice::from_raw_parts_mut(data_ptr, len)
}

/// Helper to get f64 slice data
unsafe fn get_f64_slice(s: GcRef) -> &'static [f64] {
    let len = slice::len(s);
    let arr = slice::array_ref(s);
    let start = slice::start(s);
    let data_ptr = Gc::get_data_ptr(arr).add(3 + start) as *const f64;
    std::slice::from_raw_parts(data_ptr, len)
}

#[no_mangle]
pub unsafe extern "C" fn gox_sort_ints(s: GcRef) {
    get_i64_slice_mut(s).sort();
}

#[no_mangle]
pub unsafe extern "C" fn gox_sort_float64s(s: GcRef) {
    get_f64_slice_mut(s).sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
}

#[no_mangle]
pub unsafe extern "C" fn gox_sort_ints_are_sorted(s: GcRef) -> bool {
    get_i64_slice(s).windows(2).all(|w| w[0] <= w[1])
}

#[no_mangle]
pub unsafe extern "C" fn gox_sort_float64s_are_sorted(s: GcRef) -> bool {
    get_f64_slice(s).windows(2).all(|w| w[0] <= w[1])
}

#[no_mangle]
pub unsafe extern "C" fn gox_sort_search_ints(s: GcRef, x: i64) -> i64 {
    get_i64_slice(s).binary_search(&x).unwrap_or_else(|i| i) as i64
}

#[no_mangle]
pub unsafe extern "C" fn gox_sort_search_float64s(s: GcRef, x: f64) -> i64 {
    get_f64_slice(s).binary_search_by(|a| a.partial_cmp(&x).unwrap_or(std::cmp::Ordering::Equal))
        .unwrap_or_else(|i| i) as i64
}
