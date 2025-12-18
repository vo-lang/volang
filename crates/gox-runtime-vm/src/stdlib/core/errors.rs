//! Native implementations for the errors package.

use gox_vm::extern_fn::{ExternCtx, ExternResult, ExternRegistry};

pub fn register(registry: &mut ExternRegistry) {
    registry.register("errors.New", native_new);
}

fn native_new(ctx: &mut ExternCtx) -> ExternResult {
    let text = ctx.arg_str(0).to_string();
    // For now, we return the string as the error
    // In a full implementation, we'd create an error object
    ctx.ret_string(0, &text);
    ExternResult::Ok(1)
}

