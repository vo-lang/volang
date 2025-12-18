//! encoding/json package C ABI for AOT.

use gox_runtime_core::builtins::json as core;
use gox_runtime_core::gc::GcRef;
use gox_runtime_core::objects::string;

#[no_mangle]
pub unsafe extern "C" fn gox_json_valid(s: GcRef) -> bool {
    core::valid(string::as_str(s))
}
