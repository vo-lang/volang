//! VM bindings for the strconv package.
//!
//! All logic is in gox-runtime-core/src/stdlib/strconv.rs
//! GoX implementations: FormatBool, ParseBool (in stdlib/strconv/strconv.gox)

use gox_vm::{ExternCtx, ExternRegistry, ExternResult};
use gox_runtime_core::stdlib::strconv;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("strconv.Atoi", native_atoi);
    registry.register("strconv.Itoa", native_itoa);
    registry.register("strconv.ParseInt", native_parse_int);
    registry.register("strconv.ParseFloat", native_parse_float);
    registry.register("strconv.FormatInt", native_format_int);
    registry.register("strconv.FormatFloat", native_format_float);
    registry.register("strconv.Quote", native_quote);
}

fn native_atoi(ctx: &mut ExternCtx) -> ExternResult {
    match strconv::atoi(ctx.arg_str(0)) {
        Ok(v) => {
            ctx.ret_i64(0, v);
            ctx.ret_nil(1);
        }
        Err(_) => {
            ctx.ret_i64(0, 0);
            ctx.ret_i64(1, 1);
        }
    }
    ExternResult::Ok(2)
}

fn native_itoa(ctx: &mut ExternCtx) -> ExternResult {
    let result = strconv::itoa(ctx.arg_i64(0));
    ctx.ret_string(0, &result);
    ExternResult::Ok(1)
}

fn native_parse_int(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    let base = ctx.arg_i64(1) as u32;
    match strconv::parse_int(s, base) {
        Ok(v) => {
            ctx.ret_i64(0, v);
            ctx.ret_nil(1);
        }
        Err(_) => {
            ctx.ret_i64(0, 0);
            ctx.ret_i64(1, 1);
        }
    }
    ExternResult::Ok(2)
}

fn native_parse_float(ctx: &mut ExternCtx) -> ExternResult {
    match strconv::parse_float(ctx.arg_str(0)) {
        Ok(v) => {
            ctx.ret_f64(0, v);
            ctx.ret_nil(1);
        }
        Err(_) => {
            ctx.ret_f64(0, 0.0);
            ctx.ret_i64(1, 1);
        }
    }
    ExternResult::Ok(2)
}

fn native_format_int(ctx: &mut ExternCtx) -> ExternResult {
    let result = strconv::format_int(ctx.arg_i64(0), ctx.arg_i64(1) as u32);
    ctx.ret_string(0, &result);
    ExternResult::Ok(1)
}

fn native_format_float(ctx: &mut ExternCtx) -> ExternResult {
    let f = ctx.arg_f64(0);
    let fmt = ctx.arg_i64(1) as u8 as char;
    let prec = ctx.arg_i64(2) as i32;
    let result = strconv::format_float(f, fmt, prec);
    ctx.ret_string(0, &result);
    ExternResult::Ok(1)
}

fn native_quote(ctx: &mut ExternCtx) -> ExternResult {
    let result = strconv::quote(ctx.arg_str(0));
    ctx.ret_string(0, &result);
    ExternResult::Ok(1)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_register() {
        let mut registry = ExternRegistry::new();
        register(&mut registry);
        
        assert!(registry.get("strconv.Atoi").is_some());
        assert!(registry.get("strconv.Itoa").is_some());
        assert!(registry.get("strconv.ParseInt").is_some());
    }
}

