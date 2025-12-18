//! bytes package extern functions.

use gox_vm::{ExternRegistry, ExternCtx, ExternResult};
use gox_runtime_core::builtins::bytes as core;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("bytes.Index", extern_index);
    registry.register("bytes.LastIndex", extern_last_index);
    registry.register("bytes.Count", extern_count);
    registry.register("bytes.Compare", extern_compare);
    registry.register("bytes.Equal", extern_equal);
}

fn extern_index(ctx: &mut ExternCtx) -> ExternResult {
    // TODO: Need byte slice access from ExternCtx
    let _ = ctx;
    ExternResult::Ok(1)
}

fn extern_last_index(ctx: &mut ExternCtx) -> ExternResult {
    let _ = ctx;
    ExternResult::Ok(1)
}

fn extern_count(ctx: &mut ExternCtx) -> ExternResult {
    let _ = ctx;
    ExternResult::Ok(1)
}

fn extern_compare(ctx: &mut ExternCtx) -> ExternResult {
    let _ = ctx;
    ctx.ret_i64(0, 0);
    ExternResult::Ok(1)
}

fn extern_equal(ctx: &mut ExternCtx) -> ExternResult {
    let _ = ctx;
    ctx.ret_bool(0, false);
    ExternResult::Ok(1)
}

// Suppress unused import warning for now
const _: () = { let _ = core::index; };
