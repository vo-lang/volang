//! strings package C ABI for JIT.

use vo_runtime_core::builtins::strings as core;
use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::string;
use vo_common_core::ValueKind;

#[no_mangle]
pub unsafe extern "C" fn vo_strings_index(s: GcRef, substr: GcRef) -> i64 {
    core::index(string::as_str(s), string::as_str(substr))
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_last_index(s: GcRef, substr: GcRef) -> i64 {
    core::last_index(string::as_str(s), string::as_str(substr))
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_count(s: GcRef, substr: GcRef) -> i64 {
    core::count(string::as_str(s), string::as_str(substr))
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_to_lower(gc: *mut Gc, s: GcRef) -> GcRef {
    let result = core::to_lower(string::as_str(s));
    string::from_rust_str(&mut *gc, &result)
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_to_upper(gc: *mut Gc, s: GcRef) -> GcRef {
    let result = core::to_upper(string::as_str(s));
    string::from_rust_str(&mut *gc, &result)
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_trim_space(gc: *mut Gc, s: GcRef) -> GcRef {
    let result = core::trim_space(string::as_str(s));
    string::from_rust_str(&mut *gc, result)
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_trim(gc: *mut Gc, s: GcRef, cutset: GcRef) -> GcRef {
    let result = core::trim(string::as_str(s), string::as_str(cutset));
    string::from_rust_str(&mut *gc, result)
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_equal_fold(s: GcRef, t: GcRef) -> bool {
    core::equal_fold(string::as_str(s), string::as_str(t))
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_contains(s: GcRef, substr: GcRef) -> bool {
    core::contains(string::as_str(s), string::as_str(substr))
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_has_prefix(s: GcRef, prefix: GcRef) -> bool {
    core::has_prefix(string::as_str(s), string::as_str(prefix))
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_has_suffix(s: GcRef, suffix: GcRef) -> bool {
    core::has_suffix(string::as_str(s), string::as_str(suffix))
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_repeat(gc: *mut Gc, s: GcRef, n: i64) -> GcRef {
    let result = core::repeat(string::as_str(s), n);
    string::from_rust_str(&mut *gc, &result)
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_replace(
    gc: *mut Gc, s: GcRef, old: GcRef, new: GcRef, n: i64
) -> GcRef {
    let result = core::replace(string::as_str(s), string::as_str(old), string::as_str(new), n);
    string::from_rust_str(&mut *gc, &result)
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_split(
    gc: *mut Gc, s: GcRef, sep: GcRef
) -> GcRef {
    use vo_runtime_core::objects::{array, slice};
    
    let parts = core::split(string::as_str(s), string::as_str(sep));
    let len = parts.len();
    
    // Create array to hold strings (elem_kind=String, elem_bytes=8 for GcRef pointers)
    let arr = array::create(&mut *gc, ValueKind::String as u8, 0, 8, len);
    
    // Fill array with string GcRefs
    for (i, part) in parts.iter().enumerate() {
        let str_ref = string::from_rust_str(&mut *gc, part);
        array::set(arr, i, str_ref as u64);
    }
    
    // Create slice pointing to array
    slice::create(&mut *gc, arr, 0, len, len)
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_split_n(
    gc: *mut Gc, s: GcRef, sep: GcRef, n: i64
) -> GcRef {
    use vo_runtime_core::objects::{array, slice};
    
    let parts = core::split_n(string::as_str(s), string::as_str(sep), n);
    let len = parts.len();
    
    let arr = array::create(&mut *gc, ValueKind::String as u8, 0, 8, len);
    
    for (i, part) in parts.iter().enumerate() {
        let str_ref = string::from_rust_str(&mut *gc, part);
        array::set(arr, i, str_ref as u64);
    }
    
    slice::create(&mut *gc, arr, 0, len, len)
}

#[no_mangle]
pub unsafe extern "C" fn vo_strings_join(
    gc: *mut Gc, parts: GcRef, sep: GcRef
) -> GcRef {
    use vo_runtime_core::objects::slice;
    
    let len = slice::len(parts);
    let sep_str = string::as_str(sep);
    
    // Collect string parts
    let mut strs: Vec<&str> = Vec::with_capacity(len);
    for i in 0..len {
        let str_ref = slice::get(parts, i) as GcRef;
        strs.push(string::as_str(str_ref));
    }
    
    let result = core::join(&strs, sep_str);
    string::from_rust_str(&mut *gc, &result)
}
