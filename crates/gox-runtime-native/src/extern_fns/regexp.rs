//! regexp package C ABI for AOT.

use gox_runtime_core::builtins::regexp as core;
use gox_runtime_core::gc::{Gc, GcRef};
use gox_runtime_core::objects::string;
use gox_runtime_core::gc::TypeId;

#[no_mangle]
pub unsafe extern "C" fn gox_regexp_match_string(
    gc: *mut Gc, pattern: GcRef, s: GcRef, type_id: TypeId
) -> (bool, GcRef) {
    match core::match_string(string::as_str(pattern), string::as_str(s)) {
        Ok(matched) => (matched, std::ptr::null_mut()),
        Err(e) => (false, string::from_rust_str(&mut *gc, type_id, &e)),
    }
}
