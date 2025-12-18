//! VM bindings for the unicode package.
//!
//! All logic is in gox-runtime-core/src/stdlib/unicode.rs

use gox_vm::extern_fn::{ExternCtx, ExternResult, ExternRegistry};

pub fn register(registry: &mut ExternRegistry) {
    registry.register("unicode.IsLetter", native_is_letter);
    registry.register("unicode.IsDigit", native_is_digit);
    registry.register("unicode.IsSpace", native_is_space);
    registry.register("unicode.IsUpper", native_is_upper);
    registry.register("unicode.IsLower", native_is_lower);
    registry.register("unicode.IsPrint", native_is_print);
    registry.register("unicode.IsGraphic", native_is_graphic);
    registry.register("unicode.IsControl", native_is_control);
    registry.register("unicode.IsPunct", native_is_punct);
    registry.register("unicode.IsSymbol", native_is_symbol);
    registry.register("unicode.IsNumber", native_is_number);
    registry.register("unicode.ToUpper", native_to_upper);
    registry.register("unicode.ToLower", native_to_lower);
    registry.register("unicode.ToTitle", native_to_title);
}

fn native_is_letter(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, gox_runtime_core::stdlib::unicode::is_letter(r));
    ExternResult::Ok(1)
}

fn native_is_digit(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, gox_runtime_core::stdlib::unicode::is_digit(r));
    ExternResult::Ok(1)
}

fn native_is_space(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, gox_runtime_core::stdlib::unicode::is_space(r));
    ExternResult::Ok(1)
}

fn native_is_upper(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, gox_runtime_core::stdlib::unicode::is_upper(r));
    ExternResult::Ok(1)
}

fn native_is_lower(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, gox_runtime_core::stdlib::unicode::is_lower(r));
    ExternResult::Ok(1)
}

fn native_is_print(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, gox_runtime_core::stdlib::unicode::is_print(r));
    ExternResult::Ok(1)
}

fn native_is_graphic(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, gox_runtime_core::stdlib::unicode::is_graphic(r));
    ExternResult::Ok(1)
}

fn native_is_control(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, gox_runtime_core::stdlib::unicode::is_control(r));
    ExternResult::Ok(1)
}

fn native_is_punct(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, gox_runtime_core::stdlib::unicode::is_punct(r));
    ExternResult::Ok(1)
}

fn native_is_symbol(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, gox_runtime_core::stdlib::unicode::is_symbol(r));
    ExternResult::Ok(1)
}

fn native_is_number(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_bool(0, gox_runtime_core::stdlib::unicode::is_number(r));
    ExternResult::Ok(1)
}

fn native_to_upper(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_i64(0, gox_runtime_core::stdlib::unicode::to_upper(r) as i64);
    ExternResult::Ok(1)
}

fn native_to_lower(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_i64(0, gox_runtime_core::stdlib::unicode::to_lower(r) as i64);
    ExternResult::Ok(1)
}

fn native_to_title(ctx: &mut ExternCtx) -> ExternResult {
    let r = char::from_u32(ctx.arg_i64(0) as u32).unwrap_or('\0');
    ctx.ret_i64(0, gox_runtime_core::stdlib::unicode::to_title(r) as i64);
    ExternResult::Ok(1)
}

