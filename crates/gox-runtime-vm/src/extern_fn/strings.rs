//! strings package extern functions.

use gox_vm::{ExternRegistry, ExternCtx, ExternResult};
use gox_runtime_core::builtins::strings as core;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("strings.Index", extern_index);
    registry.register("strings.LastIndex", extern_last_index);
    registry.register("strings.Count", extern_count);
    registry.register("strings.ToLower", extern_to_lower);
    registry.register("strings.ToUpper", extern_to_upper);
    registry.register("strings.TrimSpace", extern_trim_space);
    registry.register("strings.Trim", extern_trim);
    registry.register("strings.TrimLeft", extern_trim_left);
    registry.register("strings.TrimRight", extern_trim_right);
    registry.register("strings.Split", extern_split);
    registry.register("strings.SplitN", extern_split_n);
    registry.register("strings.Replace", extern_replace);
    registry.register("strings.EqualFold", extern_equal_fold);
}

fn extern_index(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    let substr = ctx.arg_str(1);
    ctx.ret_i64(0, core::index(s, substr));
    ExternResult::Ok(1)
}

fn extern_last_index(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    let substr = ctx.arg_str(1);
    ctx.ret_i64(0, core::last_index(s, substr));
    ExternResult::Ok(1)
}

fn extern_count(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    let substr = ctx.arg_str(1);
    ctx.ret_i64(0, core::count(s, substr));
    ExternResult::Ok(1)
}

fn extern_to_lower(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    let result = core::to_lower(s);
    ctx.ret_string(0, &result);
    ExternResult::Ok(1)
}

fn extern_to_upper(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    let result = core::to_upper(s);
    ctx.ret_string(0, &result);
    ExternResult::Ok(1)
}

fn extern_trim_space(ctx: &mut ExternCtx) -> ExternResult {
    let result = core::trim_space(ctx.arg_str(0)).to_string();
    ctx.ret_string(0, &result);
    ExternResult::Ok(1)
}

fn extern_trim(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0).to_string();
    let cutset = ctx.arg_str(1).to_string();
    let result = core::trim(&s, &cutset).to_string();
    ctx.ret_string(0, &result);
    ExternResult::Ok(1)
}

fn extern_trim_left(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0).to_string();
    let cutset = ctx.arg_str(1).to_string();
    let result = core::trim_left(&s, &cutset).to_string();
    ctx.ret_string(0, &result);
    ExternResult::Ok(1)
}

fn extern_trim_right(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0).to_string();
    let cutset = ctx.arg_str(1).to_string();
    let result = core::trim_right(&s, &cutset).to_string();
    ctx.ret_string(0, &result);
    ExternResult::Ok(1)
}

fn extern_split(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0).to_string();
    let sep = ctx.arg_str(1).to_string();
    let parts = core::split(&s, &sep);
    ctx.ret_string_slice(0, &parts);
    ExternResult::Ok(1)
}

fn extern_split_n(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0).to_string();
    let sep = ctx.arg_str(1).to_string();
    let n = ctx.arg_i64(2);
    let parts = core::split_n(&s, &sep, n);
    ctx.ret_string_slice(0, &parts);
    ExternResult::Ok(1)
}

fn extern_replace(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    let old = ctx.arg_str(1);
    let new = ctx.arg_str(2);
    let n = ctx.arg_i64(3);
    let result = core::replace(s, old, new, n);
    ctx.ret_string(0, &result);
    ExternResult::Ok(1)
}

fn extern_equal_fold(ctx: &mut ExternCtx) -> ExternResult {
    let a = ctx.arg_str(0);
    let b = ctx.arg_str(1);
    ctx.ret_bool(0, core::equal_fold(a, b));
    ExternResult::Ok(1)
}
