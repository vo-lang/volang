//! bytes package extern functions.

use vo_vm::{ExternRegistry, ExternCtx, ExternResult};
use vo_runtime_core::builtins::bytes as core;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("bytes.Index", extern_index);
    registry.register("bytes.LastIndex", extern_last_index);
    registry.register("bytes.IndexByte", extern_index_byte);
    registry.register("bytes.LastIndexByte", extern_last_index_byte);
    registry.register("bytes.Count", extern_count);
    registry.register("bytes.Compare", extern_compare);
    registry.register("bytes.Equal", extern_equal);
    registry.register("bytes.ToLower", extern_to_lower);
    registry.register("bytes.ToUpper", extern_to_upper);
    registry.register("bytes.Repeat", extern_repeat);
}

fn extern_index(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_bytes(0);
    let sep = ctx.arg_bytes(1);
    ctx.ret_i64(0, core::index(&s, &sep));
    ExternResult::Ok(1)
}

fn extern_last_index(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_bytes(0);
    let sep = ctx.arg_bytes(1);
    ctx.ret_i64(0, core::last_index(&s, &sep));
    ExternResult::Ok(1)
}

fn extern_index_byte(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_bytes(0);
    let c = ctx.arg_i64(1) as u8;
    ctx.ret_i64(0, core::index_byte(&s, c));
    ExternResult::Ok(1)
}

fn extern_last_index_byte(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_bytes(0);
    let c = ctx.arg_i64(1) as u8;
    ctx.ret_i64(0, core::last_index_byte(&s, c));
    ExternResult::Ok(1)
}

fn extern_count(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_bytes(0);
    let sep = ctx.arg_bytes(1);
    ctx.ret_i64(0, core::count(&s, &sep));
    ExternResult::Ok(1)
}

fn extern_compare(ctx: &mut ExternCtx) -> ExternResult {
    let a = ctx.arg_bytes(0);
    let b = ctx.arg_bytes(1);
    ctx.ret_i64(0, core::compare(&a, &b));
    ExternResult::Ok(1)
}

fn extern_equal(ctx: &mut ExternCtx) -> ExternResult {
    let a = ctx.arg_bytes(0);
    let b = ctx.arg_bytes(1);
    ctx.ret_bool(0, core::equal(&a, &b));
    ExternResult::Ok(1)
}

fn extern_to_lower(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_bytes(0);
    let result = core::to_lower(&s);
    ctx.ret_byte_slice(0, &result);
    ExternResult::Ok(1)
}

fn extern_to_upper(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_bytes(0);
    let result = core::to_upper(&s);
    ctx.ret_byte_slice(0, &result);
    ExternResult::Ok(1)
}

fn extern_repeat(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_bytes(0);
    let n = ctx.arg_i64(1);
    let result = core::repeat(&s, n);
    ctx.ret_byte_slice(0, &result);
    ExternResult::Ok(1)
}
