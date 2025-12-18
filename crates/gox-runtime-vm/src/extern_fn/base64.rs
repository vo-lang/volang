//! encoding/base64 package extern functions.

use gox_vm::{ExternRegistry, ExternCtx, ExternResult};
use gox_runtime_core::builtins::base64 as core;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("base64.StdEncoding.EncodeToString", extern_std_encode);
    registry.register("base64.StdEncoding.DecodeString", extern_std_decode);
}

fn extern_std_encode(ctx: &mut ExternCtx) -> ExternResult {
    // TODO: Need byte slice access
    let _ = ctx;
    let _ = core::encode_std;
    ctx.ret_string(0, "");
    ExternResult::Ok(1)
}

fn extern_std_decode(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    match core::decode_std(s) {
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
