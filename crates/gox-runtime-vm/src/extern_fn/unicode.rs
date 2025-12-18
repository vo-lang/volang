//! unicode package extern functions.

use gox_vm::{ExternRegistry, ExternCtx, ExternResult};
use gox_runtime_core::builtins::unicode as core;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("unicode.IsLetter", extern_is_letter);
    registry.register("unicode.IsDigit", extern_is_digit);
    registry.register("unicode.IsSpace", extern_is_space);
    registry.register("unicode.IsUpper", extern_is_upper);
    registry.register("unicode.IsLower", extern_is_lower);
    registry.register("unicode.ToLower", extern_to_lower);
    registry.register("unicode.ToUpper", extern_to_upper);
}

fn extern_is_letter(ctx: &mut ExternCtx) -> ExternResult {
    let c = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, core::is_letter(c));
    ExternResult::Ok(1)
}

fn extern_is_digit(ctx: &mut ExternCtx) -> ExternResult {
    let c = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, core::is_digit(c));
    ExternResult::Ok(1)
}

fn extern_is_space(ctx: &mut ExternCtx) -> ExternResult {
    let c = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, core::is_space(c));
    ExternResult::Ok(1)
}

fn extern_is_upper(ctx: &mut ExternCtx) -> ExternResult {
    let c = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, core::is_upper(c));
    ExternResult::Ok(1)
}

fn extern_is_lower(ctx: &mut ExternCtx) -> ExternResult {
    let c = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, core::is_lower(c));
    ExternResult::Ok(1)
}

fn extern_to_lower(ctx: &mut ExternCtx) -> ExternResult {
    let c = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_i64(0, core::to_lower(c) as i64);
    ExternResult::Ok(1)
}

fn extern_to_upper(ctx: &mut ExternCtx) -> ExternResult {
    let c = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_i64(0, core::to_upper(c) as i64);
    ExternResult::Ok(1)
}
