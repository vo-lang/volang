//! VM bindings for the encoding/json package.
//!
//! All logic is in gox-runtime-core/src/stdlib/json.rs

use gox_vm::extern_fn::{ExternCtx, ExternResult, ExternRegistry};
use gox_vm::objects::slice;
use gox_runtime_core::stdlib::json;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("json.Valid", native_valid);
    registry.register("json.MarshalString", native_marshal_string);
    registry.register("json.UnmarshalString", native_unmarshal_string);
}

fn native_valid(ctx: &mut ExternCtx) -> ExternResult {
    let data_ref = ctx.arg_ref(0);
    if data_ref.is_null() {
        ctx.ret_bool(0, false);
        return ExternResult::Ok(1);
    }
    
    let len = slice::len(data_ref);
    let data: Vec<u8> = (0..len).map(|i| slice::get(data_ref, i) as u8).collect();
    let s = String::from_utf8_lossy(&data);
    ctx.ret_bool(0, json::is_valid(&s));
    ExternResult::Ok(1)
}

fn native_marshal_string(ctx: &mut ExternCtx) -> ExternResult {
    let result = json::marshal_string(ctx.arg_str(0));
    ctx.ret_string(0, &result);
    ExternResult::Ok(1)
}

fn native_unmarshal_string(ctx: &mut ExternCtx) -> ExternResult {
    let (value, err) = json::unmarshal_string(ctx.arg_str(0));
    ctx.ret_string(0, &value);
    match err {
        Some(msg) => ctx.ret_string(1, msg),
        None => ctx.ret_nil(1),
    }
    ExternResult::Ok(2)
}
