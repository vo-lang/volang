//! encoding/json package C ABI for JIT.

use vo_runtime_core::builtins::json as core;
use vo_runtime_core::gc::GcRef;
use vo_runtime_core::objects::string;

#[no_mangle]
pub unsafe extern "C" fn vo_json_valid(s: GcRef) -> bool {
    core::valid(string::as_str(s))
}
