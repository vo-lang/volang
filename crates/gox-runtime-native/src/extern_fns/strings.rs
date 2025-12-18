//! strings package C ABI for AOT.

use gox_runtime_core::builtins::strings as core;
use gox_runtime_core::gc::{Gc, GcRef};
use gox_runtime_core::objects::string;
use gox_runtime_core::gc::TypeId;

#[no_mangle]
pub unsafe extern "C" fn gox_strings_index(s: GcRef, substr: GcRef) -> i64 {
    core::index(string::as_str(s), string::as_str(substr))
}

#[no_mangle]
pub unsafe extern "C" fn gox_strings_last_index(s: GcRef, substr: GcRef) -> i64 {
    core::last_index(string::as_str(s), string::as_str(substr))
}

#[no_mangle]
pub unsafe extern "C" fn gox_strings_count(s: GcRef, substr: GcRef) -> i64 {
    core::count(string::as_str(s), string::as_str(substr))
}

#[no_mangle]
pub unsafe extern "C" fn gox_strings_to_lower(gc: *mut Gc, s: GcRef, type_id: TypeId) -> GcRef {
    let result = core::to_lower(string::as_str(s));
    string::from_rust_str(&mut *gc, type_id, &result)
}

#[no_mangle]
pub unsafe extern "C" fn gox_strings_to_upper(gc: *mut Gc, s: GcRef, type_id: TypeId) -> GcRef {
    let result = core::to_upper(string::as_str(s));
    string::from_rust_str(&mut *gc, type_id, &result)
}

#[no_mangle]
pub unsafe extern "C" fn gox_strings_trim_space(gc: *mut Gc, s: GcRef, type_id: TypeId) -> GcRef {
    let result = core::trim_space(string::as_str(s));
    string::from_rust_str(&mut *gc, type_id, result)
}

#[no_mangle]
pub unsafe extern "C" fn gox_strings_trim(gc: *mut Gc, s: GcRef, cutset: GcRef, type_id: TypeId) -> GcRef {
    let result = core::trim(string::as_str(s), string::as_str(cutset));
    string::from_rust_str(&mut *gc, type_id, result)
}

#[no_mangle]
pub unsafe extern "C" fn gox_strings_equal_fold(s: GcRef, t: GcRef) -> bool {
    core::equal_fold(string::as_str(s), string::as_str(t))
}

#[no_mangle]
pub unsafe extern "C" fn gox_strings_contains(s: GcRef, substr: GcRef) -> bool {
    core::contains(string::as_str(s), string::as_str(substr))
}

#[no_mangle]
pub unsafe extern "C" fn gox_strings_has_prefix(s: GcRef, prefix: GcRef) -> bool {
    core::has_prefix(string::as_str(s), string::as_str(prefix))
}

#[no_mangle]
pub unsafe extern "C" fn gox_strings_has_suffix(s: GcRef, suffix: GcRef) -> bool {
    core::has_suffix(string::as_str(s), string::as_str(suffix))
}

#[no_mangle]
pub unsafe extern "C" fn gox_strings_repeat(gc: *mut Gc, s: GcRef, n: i64, type_id: TypeId) -> GcRef {
    let result = core::repeat(string::as_str(s), n);
    string::from_rust_str(&mut *gc, type_id, &result)
}

#[no_mangle]
pub unsafe extern "C" fn gox_strings_replace(
    gc: *mut Gc, s: GcRef, old: GcRef, new: GcRef, n: i64, type_id: TypeId
) -> GcRef {
    let result = core::replace(string::as_str(s), string::as_str(old), string::as_str(new), n);
    string::from_rust_str(&mut *gc, type_id, &result)
}
