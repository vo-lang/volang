//! VM bindings for the rand package.
//!
//! All logic is in gox-runtime-core/src/stdlib/rand.rs

use gox_vm::extern_fn::{ExternCtx, ExternResult, ExternRegistry};
use gox_vm::objects::{array, slice};
use gox_vm::types::builtin;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("rand.Seed", native_seed);
    registry.register("rand.Int", native_int);
    registry.register("rand.Int64", native_int64);
    registry.register("rand.Intn", native_intn);
    registry.register("rand.Int63n", native_int63n);
    registry.register("rand.Float64", native_float64);
    registry.register("rand.Float32", native_float32);
    registry.register("rand.Perm", native_perm);
}

fn native_seed(ctx: &mut ExternCtx) -> ExternResult {
    gox_runtime_core::stdlib::rand::seed(ctx.arg_i64(0));
    ExternResult::Ok(0)
}

fn native_int(ctx: &mut ExternCtx) -> ExternResult {
    ctx.ret_i64(0, gox_runtime_core::stdlib::rand::int());
    ExternResult::Ok(1)
}

fn native_int64(ctx: &mut ExternCtx) -> ExternResult {
    ctx.ret_i64(0, gox_runtime_core::stdlib::rand::int64());
    ExternResult::Ok(1)
}

fn native_intn(ctx: &mut ExternCtx) -> ExternResult {
    ctx.ret_i64(0, gox_runtime_core::stdlib::rand::intn(ctx.arg_i64(0)));
    ExternResult::Ok(1)
}

fn native_int63n(ctx: &mut ExternCtx) -> ExternResult {
    ctx.ret_i64(0, gox_runtime_core::stdlib::rand::int63n(ctx.arg_i64(0)));
    ExternResult::Ok(1)
}

fn native_float64(ctx: &mut ExternCtx) -> ExternResult {
    ctx.ret_f64(0, gox_runtime_core::stdlib::rand::float64());
    ExternResult::Ok(1)
}

fn native_float32(ctx: &mut ExternCtx) -> ExternResult {
    ctx.ret_f64(0, gox_runtime_core::stdlib::rand::float32() as f64);
    ExternResult::Ok(1)
}

fn native_perm(ctx: &mut ExternCtx) -> ExternResult {
    let n = ctx.arg_i64(0) as usize;
    let perm = gox_runtime_core::stdlib::rand::perm(n);
    
    let gc = ctx.gc();
    let arr = array::create(gc, builtin::ARRAY, builtin::INT, 1, n);
    for (i, &v) in perm.iter().enumerate() {
        array::set(arr, i, v as u64);
    }
    
    let result = slice::from_array(gc, builtin::SLICE, arr);
    ctx.ret_ref(0, result);
    ExternResult::Ok(1)
}

