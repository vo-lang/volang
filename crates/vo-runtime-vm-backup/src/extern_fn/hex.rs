//! encoding/hex package extern functions.

use vo_vm::{ExternRegistry, ExternCtx, ExternResult};
use vo_runtime_core::builtins::hex as core;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("hex.EncodeToString", extern_encode_to_string);
    registry.register("hex.DecodeString", extern_decode_string);
}

fn extern_encode_to_string(ctx: &mut ExternCtx) -> ExternResult {
    let src = ctx.arg_bytes(0);
    let encoded = core::encode(&src);
    ctx.ret_string(0, &encoded);
    ExternResult::Ok(1)
}

fn extern_decode_string(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    match core::decode(s) {
        Ok(bytes) => {
            ctx.ret_byte_slice(0, &bytes);
            ctx.ret_nil(1); // no error
        }
        Err(e) => {
            ctx.ret_nil(0);
            ctx.ret_string(1, &e);
        }
    }
    ExternResult::Ok(2)
}
