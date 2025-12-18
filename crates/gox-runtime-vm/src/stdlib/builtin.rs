//! Built-in native functions (len, cap, panic, etc.)
//!
//! These use the zero-copy native API for maximum performance.

use gox_vm::{ExternCtx, ExternRegistry, ExternResult, TypeTag};
use gox_vm::objects::{array, slice, string, map, channel};

/// Register builtin functions.
pub fn register(registry: &mut ExternRegistry) {
    registry.register("len", native_len);
    registry.register("cap", native_cap);
    registry.register("panic", native_panic);
}

/// len(x) - returns length of string, slice, array, map, or channel.
fn native_len(ctx: &mut ExternCtx) -> ExternResult {
    if ctx.argc() < 1 {
        ctx.ret_i64(0, 0);
        return ExternResult::Ok(1);
    }
    
    let tag = ctx.arg_type(0);
    let ptr = ctx.arg_ref(0);
    
    let len = if ptr.is_null() {
        0
    } else {
        match tag {
            TypeTag::String => string::len(ptr),
            TypeTag::Array => array::len(ptr),
            TypeTag::Slice => slice::len(ptr),
            TypeTag::Map => map::len(ptr),
            TypeTag::Channel => channel::len(ptr),
            _ => 0,
        }
    };
    
    ctx.ret_i64(0, len as i64);
    ExternResult::Ok(1)
}

/// cap(x) - returns capacity of slice, array, or channel.
fn native_cap(ctx: &mut ExternCtx) -> ExternResult {
    if ctx.argc() < 1 {
        ctx.ret_i64(0, 0);
        return ExternResult::Ok(1);
    }
    
    let tag = ctx.arg_type(0);
    let ptr = ctx.arg_ref(0);
    
    let cap = if ptr.is_null() {
        0
    } else {
        match tag {
            TypeTag::Array => array::len(ptr),  // array cap = len
            TypeTag::Slice => slice::cap(ptr),
            TypeTag::Channel => channel::capacity(ptr),
            _ => 0,
        }
    };
    
    ctx.ret_i64(0, cap as i64);
    ExternResult::Ok(1)
}

/// panic(msg) - triggers a panic.
fn native_panic(ctx: &mut ExternCtx) -> ExternResult {
    let msg = if ctx.argc() > 0 && ctx.arg_type(0) == TypeTag::String {
        ctx.arg_str(0).to_string()
    } else if ctx.argc() > 0 {
        ctx.format_arg(0)
    } else {
        "panic".to_string()
    };
    
    ExternResult::Panic(msg)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_register() {
        let mut registry = ExternRegistry::new();
        register(&mut registry);
        
        assert!(registry.get("len").is_some());
        assert!(registry.get("cap").is_some());
        assert!(registry.get("panic").is_some());
    }
}
