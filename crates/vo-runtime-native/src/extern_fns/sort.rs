//! sort package C ABI for JIT.

use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::slice;

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
pub unsafe extern "C" fn vo_sort_ints(s: GcRef) {
    if s.is_null() {
        return;
    }
    let len = slice::len(s);
    if len == 0 {
        return;
    }
    get_i64_slice_mut(s).sort();
}

#[no_mangle]
pub unsafe extern "C" fn vo_sort_float64s(s: GcRef) {
    let len = slice::len(s);
    if len == 0 {
        return;
    }
    get_f64_slice_mut(s).sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
}

#[no_mangle]
pub unsafe extern "C" fn vo_sort_strings(s: GcRef) {
    use vo_runtime_core::objects::string;
    
    let len = slice::len(s);
    if len == 0 {
        return;
    }
    let arr = slice::array_ref(s);
    let start = slice::start(s);
    
    // Collect string refs and their string values
    let mut pairs: Vec<(usize, &str)> = Vec::with_capacity(len);
    for i in 0..len {
        let str_ref = Gc::read_slot(arr, 3 + start + i) as GcRef;
        pairs.push((i, string::as_str(str_ref)));
    }
    
    // Sort by string content
    pairs.sort_by(|a, b| a.1.cmp(b.1));
    
    // Collect sorted refs
    let sorted_refs: Vec<u64> = pairs.iter().map(|(idx, _)| {
        Gc::read_slot(arr, 3 + start + idx)
    }).collect();
    
    // Write back sorted refs
    for (i, &ref_val) in sorted_refs.iter().enumerate() {
        Gc::write_slot(arr, 3 + start + i, ref_val);
    }
}

#[no_mangle]
pub unsafe extern "C" fn vo_sort_ints_are_sorted(s: GcRef) -> bool {
    get_i64_slice(s).windows(2).all(|w| w[0] <= w[1])
}

#[no_mangle]
pub unsafe extern "C" fn vo_sort_float64s_are_sorted(s: GcRef) -> bool {
    get_f64_slice(s).windows(2).all(|w| w[0] <= w[1])
}

#[no_mangle]
pub unsafe extern "C" fn vo_sort_search_ints(s: GcRef, x: i64) -> i64 {
    get_i64_slice(s).binary_search(&x).unwrap_or_else(|i| i) as i64
}

#[no_mangle]
pub unsafe extern "C" fn vo_sort_search_float64s(s: GcRef, x: f64) -> i64 {
    get_f64_slice(s).binary_search_by(|a| a.partial_cmp(&x).unwrap_or(std::cmp::Ordering::Equal))
        .unwrap_or_else(|i| i) as i64
}
