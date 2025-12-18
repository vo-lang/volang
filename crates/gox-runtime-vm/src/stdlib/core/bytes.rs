//! VM bindings for the bytes package.
//!
//! All logic is in gox-runtime-core/src/stdlib/bytes.rs
//! GoX implementations: Equal, Compare, HasPrefix, HasSuffix, Contains, TrimPrefix, TrimSuffix

use gox_vm::gc::{Gc, GcRef};
use gox_vm::extern_fn::{ExternCtx, ExternResult, ExternRegistry};
use gox_vm::objects::{array, slice};
use gox_vm::types::builtin;
use gox_runtime_core::stdlib::bytes;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("bytes.Index", native_index);
    registry.register("bytes.LastIndex", native_last_index);
    registry.register("bytes.Count", native_count);
    registry.register("bytes.IndexByte", native_index_byte);
    registry.register("bytes.LastIndexByte", native_last_index_byte);
    registry.register("bytes.ToLower", native_to_lower);
    registry.register("bytes.ToUpper", native_to_upper);
    registry.register("bytes.TrimSpace", native_trim_space);
    registry.register("bytes.Trim", native_trim);
    registry.register("bytes.Repeat", native_repeat);
    registry.register("bytes.Join", native_join);
    registry.register("bytes.Split", native_split);
}

fn read_bytes(slice_ref: GcRef) -> Vec<u8> {
    if slice_ref.is_null() {
        return Vec::new();
    }
    let len = slice::len(slice_ref);
    (0..len).map(|i| slice::get(slice_ref, i) as u8).collect()
}

fn create_byte_slice(gc: &mut Gc, data: &[u8]) -> GcRef {
    let arr = array::create(gc, builtin::ARRAY, builtin::UINT8, 1, data.len());
    for (i, &b) in data.iter().enumerate() {
        array::set(arr, i, b as u64);
    }
    slice::from_array(gc, builtin::SLICE, arr)
}

fn native_index(ctx: &mut ExternCtx) -> ExternResult {
    let b = read_bytes(ctx.arg_ref(0));
    let sep = read_bytes(ctx.arg_ref(1));
    ctx.ret_i64(0, bytes::index(&b, &sep));
    ExternResult::Ok(1)
}

fn native_last_index(ctx: &mut ExternCtx) -> ExternResult {
    let b = read_bytes(ctx.arg_ref(0));
    let sep = read_bytes(ctx.arg_ref(1));
    ctx.ret_i64(0, bytes::last_index(&b, &sep));
    ExternResult::Ok(1)
}

fn native_count(ctx: &mut ExternCtx) -> ExternResult {
    let b = read_bytes(ctx.arg_ref(0));
    let sep = read_bytes(ctx.arg_ref(1));
    ctx.ret_i64(0, bytes::count(&b, &sep) as i64);
    ExternResult::Ok(1)
}

fn native_index_byte(ctx: &mut ExternCtx) -> ExternResult {
    let b = read_bytes(ctx.arg_ref(0));
    ctx.ret_i64(0, bytes::index_byte(&b, ctx.arg_i64(1) as u8));
    ExternResult::Ok(1)
}

fn native_last_index_byte(ctx: &mut ExternCtx) -> ExternResult {
    let b = read_bytes(ctx.arg_ref(0));
    ctx.ret_i64(0, bytes::last_index_byte(&b, ctx.arg_i64(1) as u8));
    ExternResult::Ok(1)
}

fn native_to_lower(ctx: &mut ExternCtx) -> ExternResult {
    let b = read_bytes(ctx.arg_ref(0));
    let result = bytes::to_lower(&b);
    let slice_ref = create_byte_slice(ctx.gc(), &result);
    ctx.ret_ref(0, slice_ref);
    ExternResult::Ok(1)
}

fn native_to_upper(ctx: &mut ExternCtx) -> ExternResult {
    let b = read_bytes(ctx.arg_ref(0));
    let result = bytes::to_upper(&b);
    let slice_ref = create_byte_slice(ctx.gc(), &result);
    ctx.ret_ref(0, slice_ref);
    ExternResult::Ok(1)
}

fn native_trim_space(ctx: &mut ExternCtx) -> ExternResult {
    let b = read_bytes(ctx.arg_ref(0));
    let result = bytes::trim_space(&b);
    let slice_ref = create_byte_slice(ctx.gc(), result);
    ctx.ret_ref(0, slice_ref);
    ExternResult::Ok(1)
}

fn native_trim(ctx: &mut ExternCtx) -> ExternResult {
    let b = read_bytes(ctx.arg_ref(0));
    let cutset_str = ctx.arg_str(1).to_string();
    let result = bytes::trim(&b, cutset_str.as_bytes());
    let slice_ref = create_byte_slice(ctx.gc(), result);
    ctx.ret_ref(0, slice_ref);
    ExternResult::Ok(1)
}

fn native_repeat(ctx: &mut ExternCtx) -> ExternResult {
    let b = read_bytes(ctx.arg_ref(0));
    let count = ctx.arg_i64(1) as usize;
    let result = bytes::repeat(&b, count);
    let slice_ref = create_byte_slice(ctx.gc(), &result);
    ctx.ret_ref(0, slice_ref);
    ExternResult::Ok(1)
}

fn native_join(ctx: &mut ExternCtx) -> ExternResult {
    let s_slice = ctx.arg_ref(0);
    let sep = read_bytes(ctx.arg_ref(1));
    
    if s_slice.is_null() || slice::len(s_slice) == 0 {
        let slice_ref = create_byte_slice(ctx.gc(), &[]);
        ctx.ret_ref(0, slice_ref);
        return ExternResult::Ok(1);
    }
    
    let s_len = slice::len(s_slice);
    let parts: Vec<Vec<u8>> = (0..s_len)
        .map(|i| read_bytes(slice::get(s_slice, i) as GcRef))
        .collect();
    let refs: Vec<&[u8]> = parts.iter().map(|v| v.as_slice()).collect();
    let result = bytes::join(&refs, &sep);
    let slice_ref = create_byte_slice(ctx.gc(), &result);
    ctx.ret_ref(0, slice_ref);
    ExternResult::Ok(1)
}

fn native_split(ctx: &mut ExternCtx) -> ExternResult {
    let b = read_bytes(ctx.arg_ref(0));
    let sep = read_bytes(ctx.arg_ref(1));
    let parts = bytes::split(&b, &sep);
    
    let gc = ctx.gc();
    let arr = array::create(gc, builtin::ARRAY, builtin::SLICE, 1, parts.len());
    for (i, part) in parts.iter().enumerate() {
        let part_slice = create_byte_slice(gc, part);
        array::set(arr, i, part_slice as u64);
    }
    let result = slice::from_array(gc, builtin::SLICE, arr);
    ctx.ret_ref(0, result);
    ExternResult::Ok(1)
}

