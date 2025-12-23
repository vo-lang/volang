//! encoding/base64 package extern functions.

use vo_vm::{ExternRegistry, ExternCtx, ExternResult};
use vo_runtime_core::builtins::base64 as core;

pub fn register(registry: &mut ExternRegistry) {
    // Standard encoding (with padding)
    registry.register("base64.EncodeToString", extern_encode_std);
    registry.register("base64.DecodeString", extern_decode_std);
    // URL encoding (with padding)
    registry.register("base64.URLEncodeToString", extern_encode_url);
    registry.register("base64.URLDecodeString", extern_decode_url);
    // Raw standard (no padding)
    registry.register("base64.RawEncodeToString", extern_encode_raw_std);
    registry.register("base64.RawDecodeString", extern_decode_raw_std);
    // Raw URL (no padding)
    registry.register("base64.RawURLEncodeToString", extern_encode_raw_url);
    registry.register("base64.RawURLDecodeString", extern_decode_raw_url);
}

fn extern_encode_std(ctx: &mut ExternCtx) -> ExternResult {
    let src = ctx.arg_bytes(0);
    let encoded = core::encode_std(&src);
    ctx.ret_string(0, &encoded);
    ExternResult::Ok(1)
}

fn extern_decode_std(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0).to_string();
    match core::decode_std(&s) {
        Ok(bytes) => {
            ctx.ret_byte_slice(0, &bytes);
            ctx.ret_nil(1);
        }
        Err(e) => {
            ctx.ret_nil(0);
            ctx.ret_string(1, &e);
        }
    }
    ExternResult::Ok(2)
}

fn extern_encode_url(ctx: &mut ExternCtx) -> ExternResult {
    let src = ctx.arg_bytes(0);
    let encoded = core::encode_url(&src);
    ctx.ret_string(0, &encoded);
    ExternResult::Ok(1)
}

fn extern_decode_url(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0).to_string();
    match core::decode_url(&s) {
        Ok(bytes) => {
            ctx.ret_byte_slice(0, &bytes);
            ctx.ret_nil(1);
        }
        Err(e) => {
            ctx.ret_nil(0);
            ctx.ret_string(1, &e);
        }
    }
    ExternResult::Ok(2)
}

fn extern_encode_raw_std(ctx: &mut ExternCtx) -> ExternResult {
    let src = ctx.arg_bytes(0);
    let encoded = core::encode_std_no_pad(&src);
    ctx.ret_string(0, &encoded);
    ExternResult::Ok(1)
}

fn extern_decode_raw_std(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0).to_string();
    match core::decode_std_no_pad(&s) {
        Ok(bytes) => {
            ctx.ret_byte_slice(0, &bytes);
            ctx.ret_nil(1);
        }
        Err(e) => {
            ctx.ret_nil(0);
            ctx.ret_string(1, &e);
        }
    }
    ExternResult::Ok(2)
}

fn extern_encode_raw_url(ctx: &mut ExternCtx) -> ExternResult {
    let src = ctx.arg_bytes(0);
    let encoded = core::encode_url_no_pad(&src);
    ctx.ret_string(0, &encoded);
    ExternResult::Ok(1)
}

fn extern_decode_raw_url(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0).to_string();
    match core::decode_url_no_pad(&s) {
        Ok(bytes) => {
            ctx.ret_byte_slice(0, &bytes);
            ctx.ret_nil(1);
        }
        Err(e) => {
            ctx.ret_nil(0);
            ctx.ret_string(1, &e);
        }
    }
    ExternResult::Ok(2)
}
