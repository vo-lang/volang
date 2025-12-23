//! encoding/json package extern functions.

use vo_vm::{ExternRegistry, ExternCtx, ExternResult};
use vo_runtime_core::builtins::json as core;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("json.Valid", extern_valid);
}

fn extern_valid(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    ctx.ret_bool(0, core::valid(s));
    ExternResult::Ok(1)
}
