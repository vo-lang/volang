//! regexp package C ABI for JIT.

use vo_runtime_core::builtins::regexp as core;
use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::string;

#[no_mangle]
pub unsafe extern "C" fn vo_regexp_match_string(
    gc: *mut Gc, pattern: GcRef, s: GcRef
) -> (bool, GcRef) {
    match core::match_string(string::as_str(pattern), string::as_str(s)) {
        Ok(matched) => (matched, std::ptr::null_mut()),
        Err(e) => (false, string::from_rust_str(&mut *gc, &e)),
    }
}
