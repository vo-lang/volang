//! encoding/hex package extern functions.

use gox_vm::{ExternRegistry, ExternCtx, ExternResult};
use gox_runtime_core::builtins::hex as core;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("hex.EncodeToString", extern_encode_to_string);
    registry.register("hex.DecodeString", extern_decode_string);
}

fn extern_encode_to_string(ctx: &mut ExternCtx) -> ExternResult {
    // TODO: Need byte slice access
    let _ = ctx;
    let _ = core::encode;
    ctx.ret_string(0, "");
    ExternResult::Ok(1)
}

fn extern_decode_string(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    match core::decode(s) {
        Ok(_bytes) => {
            // TODO: Return byte slice
            ctx.ret_nil(0);
            ctx.ret_nil(1);
        }
        Err(e) => {
            ctx.ret_nil(0);
            ctx.ret_string(1, &e);
        }
    }
    ExternResult::Ok(2)
}
