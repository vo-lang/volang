//! VM bindings for the time package.
//!
//! All logic is in gox-runtime-core/src/stdlib/time.rs

use gox_vm::extern_fn::{ExternCtx, ExternResult, ExternRegistry};

pub fn register(registry: &mut ExternRegistry) {
    registry.register("time.Now", native_now);
    registry.register("time.Sleep", native_sleep);
    registry.register("time.Since", native_since);
    registry.register("time.Unix", native_unix);
    registry.register("time.UnixMilli", native_unix_milli);
    registry.register("time.ParseDuration", native_parse_duration);
}

fn native_now(ctx: &mut ExternCtx) -> ExternResult {
    ctx.ret_i64(0, gox_runtime_core::stdlib::time::now());
    ExternResult::Ok(1)
}

fn native_sleep(ctx: &mut ExternCtx) -> ExternResult {
    gox_runtime_core::stdlib::time::sleep(ctx.arg_i64(0));
    ExternResult::Ok(0)
}

fn native_since(ctx: &mut ExternCtx) -> ExternResult {
    ctx.ret_i64(0, gox_runtime_core::stdlib::time::since(ctx.arg_i64(0)));
    ExternResult::Ok(1)
}

fn native_unix(ctx: &mut ExternCtx) -> ExternResult {
    ctx.ret_i64(0, gox_runtime_core::stdlib::time::unix(ctx.arg_i64(0), ctx.arg_i64(1)));
    ExternResult::Ok(1)
}

fn native_unix_milli(ctx: &mut ExternCtx) -> ExternResult {
    ctx.ret_i64(0, gox_runtime_core::stdlib::time::unix_milli(ctx.arg_i64(0)));
    ExternResult::Ok(1)
}

fn native_parse_duration(ctx: &mut ExternCtx) -> ExternResult {
    ctx.ret_i64(0, gox_runtime_core::stdlib::time::parse_duration(ctx.arg_str(0)));
    ExternResult::Ok(1)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_duration() {
        assert_eq!(gox_runtime_core::stdlib::time::parse_duration("1s"), 1_000_000_000);
        assert_eq!(gox_runtime_core::stdlib::time::parse_duration("100ms"), 100_000_000);
    }
}

