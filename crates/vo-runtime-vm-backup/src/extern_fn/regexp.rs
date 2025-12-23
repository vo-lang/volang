//! regexp package extern functions.

use vo_vm::{ExternRegistry, ExternCtx, ExternResult};
use vo_runtime_core::builtins::regexp as core;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("regexp.MatchString", extern_match_string);
}

fn extern_match_string(ctx: &mut ExternCtx) -> ExternResult {
    let pattern = ctx.arg_str(0);
    let s = ctx.arg_str(1);
    match core::match_string(pattern, s) {
        Ok(matched) => {
            ctx.ret_bool(0, matched);
            ctx.ret_nil(1);
        }
        Err(e) => {
            ctx.ret_bool(0, false);
            ctx.ret_string(1, &e);
        }
    }
    ExternResult::Ok(2)
}
