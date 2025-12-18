//! strconv package C ABI for AOT.

use gox_runtime_core::builtins::strconv as core;
use gox_runtime_core::gc::{Gc, GcRef};
use gox_runtime_core::objects::string;
use gox_runtime_core::gc::TypeId;

#[no_mangle]
pub unsafe extern "C" fn gox_strconv_atoi(gc: *mut Gc, s: GcRef, type_id: TypeId) -> (i64, GcRef) {
    match core::atoi(string::as_str(s)) {
        Ok(n) => (n, std::ptr::null_mut()),
        Err(e) => (0, string::from_rust_str(&mut *gc, type_id, &e)),
    }
}

#[no_mangle]
pub unsafe extern "C" fn gox_strconv_itoa(gc: *mut Gc, n: i64, type_id: TypeId) -> GcRef {
    string::from_rust_str(&mut *gc, type_id, &core::itoa(n))
}

#[no_mangle]
pub unsafe extern "C" fn gox_strconv_parse_int(gc: *mut Gc, s: GcRef, base: i64, type_id: TypeId) -> (i64, GcRef) {
    match core::parse_int(string::as_str(s), base) {
        Ok(n) => (n, std::ptr::null_mut()),
        Err(e) => (0, string::from_rust_str(&mut *gc, type_id, &e)),
    }
}

#[no_mangle]
pub unsafe extern "C" fn gox_strconv_parse_float(gc: *mut Gc, s: GcRef, type_id: TypeId) -> (f64, GcRef) {
    match core::parse_float(string::as_str(s)) {
        Ok(f) => (f, std::ptr::null_mut()),
        Err(e) => (0.0, string::from_rust_str(&mut *gc, type_id, &e)),
    }
}

#[no_mangle]
pub unsafe extern "C" fn gox_strconv_format_int(gc: *mut Gc, n: i64, base: i64, type_id: TypeId) -> GcRef {
    string::from_rust_str(&mut *gc, type_id, &core::format_int(n, base))
}

#[no_mangle]
pub unsafe extern "C" fn gox_strconv_format_float(gc: *mut Gc, f: f64, fmt: u8, prec: i64, type_id: TypeId) -> GcRef {
    string::from_rust_str(&mut *gc, type_id, &core::format_float(f, fmt as char, prec))
}

#[no_mangle]
pub unsafe extern "C" fn gox_strconv_format_bool(gc: *mut Gc, b: bool, type_id: TypeId) -> GcRef {
    string::from_rust_str(&mut *gc, type_id, &core::format_bool(b))
}

#[no_mangle]
pub unsafe extern "C" fn gox_strconv_parse_bool(gc: *mut Gc, s: GcRef, type_id: TypeId) -> (bool, GcRef) {
    match core::parse_bool(string::as_str(s)) {
        Ok(b) => (b, std::ptr::null_mut()),
        Err(e) => (false, string::from_rust_str(&mut *gc, type_id, &e)),
    }
}

#[no_mangle]
pub unsafe extern "C" fn gox_strconv_quote(gc: *mut Gc, s: GcRef, type_id: TypeId) -> GcRef {
    string::from_rust_str(&mut *gc, type_id, &core::quote(string::as_str(s)))
}
