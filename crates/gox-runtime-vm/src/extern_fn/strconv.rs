//! strconv package extern functions.

use gox_vm::{ExternRegistry, ExternCtx, ExternResult};
use gox_runtime_core::builtins::strconv as core;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("strconv.Atoi", extern_atoi);
    registry.register("strconv.Itoa", extern_itoa);
    registry.register("strconv.ParseInt", extern_parse_int);
    registry.register("strconv.ParseFloat", extern_parse_float);
    registry.register("strconv.FormatInt", extern_format_int);
    registry.register("strconv.FormatFloat", extern_format_float);
    registry.register("strconv.FormatBool", extern_format_bool);
    registry.register("strconv.ParseBool", extern_parse_bool);
    registry.register("strconv.Quote", extern_quote);
}

fn extern_atoi(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    match core::atoi(s) {
        Ok(n) => {
            ctx.ret_i64(0, n);
            ctx.ret_nil(1);
        }
        Err(e) => {
            ctx.ret_i64(0, 0);
            ctx.ret_string(1, &e);
        }
    }
    ExternResult::Ok(2)
}

fn extern_itoa(ctx: &mut ExternCtx) -> ExternResult {
    let n = ctx.arg_i64(0);
    ctx.ret_string(0, &core::itoa(n));
    ExternResult::Ok(1)
}

fn extern_parse_int(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    let base = ctx.arg_i64(1);
    match core::parse_int(s, base) {
        Ok(n) => {
            ctx.ret_i64(0, n);
            ctx.ret_nil(1);
        }
        Err(e) => {
            ctx.ret_i64(0, 0);
            ctx.ret_string(1, &e);
        }
    }
    ExternResult::Ok(2)
}

fn extern_parse_float(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    match core::parse_float(s) {
        Ok(f) => {
            ctx.ret_f64(0, f);
            ctx.ret_nil(1);
        }
        Err(e) => {
            ctx.ret_f64(0, 0.0);
            ctx.ret_string(1, &e);
        }
    }
    ExternResult::Ok(2)
}

fn extern_format_int(ctx: &mut ExternCtx) -> ExternResult {
    let n = ctx.arg_i64(0);
    let base = ctx.arg_i64(1);
    ctx.ret_string(0, &core::format_int(n, base));
    ExternResult::Ok(1)
}

fn extern_format_float(ctx: &mut ExternCtx) -> ExternResult {
    let f = ctx.arg_f64(0);
    let fmt_str = ctx.arg_str(1);
    let fmt = fmt_str.chars().next().unwrap_or('f');
    let prec = ctx.arg_i64(2);
    ctx.ret_string(0, &core::format_float(f, fmt, prec));
    ExternResult::Ok(1)
}

fn extern_format_bool(ctx: &mut ExternCtx) -> ExternResult {
    let b = ctx.arg_bool(0);
    ctx.ret_string(0, &core::format_bool(b));
    ExternResult::Ok(1)
}

fn extern_parse_bool(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    match core::parse_bool(s) {
        Ok(b) => {
            ctx.ret_bool(0, b);
            ctx.ret_nil(1);
        }
        Err(e) => {
            ctx.ret_bool(0, false);
            ctx.ret_string(1, &e);
        }
    }
    ExternResult::Ok(2)
}

fn extern_quote(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    ctx.ret_string(0, &core::quote(s));
    ExternResult::Ok(1)
}
