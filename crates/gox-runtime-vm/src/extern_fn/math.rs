//! math package extern functions.

use gox_vm::{ExternRegistry, ExternCtx, ExternResult};
use gox_runtime_core::builtins::math as core;

pub fn register(registry: &mut ExternRegistry) {
    // Basic
    registry.register("math.Abs", extern_abs);
    registry.register("math.Floor", extern_floor);
    registry.register("math.Ceil", extern_ceil);
    registry.register("math.Round", extern_round);
    registry.register("math.Trunc", extern_trunc);
    registry.register("math.Max", extern_max);
    registry.register("math.Min", extern_min);
    registry.register("math.Dim", extern_dim);
    
    // Power and roots
    registry.register("math.Sqrt", extern_sqrt);
    registry.register("math.Cbrt", extern_cbrt);
    registry.register("math.Pow", extern_pow);
    registry.register("math.Pow10", extern_pow10);
    registry.register("math.Hypot", extern_hypot);
    
    // Exponential and log
    registry.register("math.Exp", extern_exp);
    registry.register("math.Log", extern_log);
    registry.register("math.Log2", extern_log2);
    registry.register("math.Log10", extern_log10);
    
    // Trigonometric
    registry.register("math.Sin", extern_sin);
    registry.register("math.Cos", extern_cos);
    registry.register("math.Tan", extern_tan);
    registry.register("math.Asin", extern_asin);
    registry.register("math.Acos", extern_acos);
    registry.register("math.Atan", extern_atan);
    registry.register("math.Atan2", extern_atan2);
    
    // Hyperbolic
    registry.register("math.Sinh", extern_sinh);
    registry.register("math.Cosh", extern_cosh);
    registry.register("math.Tanh", extern_tanh);
    
    // Utility
    registry.register("math.Mod", extern_mod);
    registry.register("math.Copysign", extern_copysign);
    registry.register("math.IsNaN", extern_is_nan);
    registry.register("math.IsInf", extern_is_inf);
    registry.register("math.Inf", extern_inf);
    registry.register("math.NaN", extern_nan);
}

fn extern_abs(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::abs(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_floor(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::floor(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_ceil(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::ceil(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_round(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::round(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_trunc(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::trunc(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_max(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::max(ctx.arg_f64(0), ctx.arg_f64(1))); ExternResult::Ok(1) }
fn extern_min(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::min(ctx.arg_f64(0), ctx.arg_f64(1))); ExternResult::Ok(1) }
fn extern_dim(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::dim(ctx.arg_f64(0), ctx.arg_f64(1))); ExternResult::Ok(1) }

fn extern_sqrt(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::sqrt(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_cbrt(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::cbrt(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_pow(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::pow(ctx.arg_f64(0), ctx.arg_f64(1))); ExternResult::Ok(1) }
fn extern_pow10(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::pow10(ctx.arg_i64(0))); ExternResult::Ok(1) }
fn extern_hypot(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::hypot(ctx.arg_f64(0), ctx.arg_f64(1))); ExternResult::Ok(1) }

fn extern_exp(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::exp(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_log(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::log(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_log2(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::log2(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_log10(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::log10(ctx.arg_f64(0))); ExternResult::Ok(1) }

fn extern_sin(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::sin(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_cos(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::cos(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_tan(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::tan(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_asin(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::asin(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_acos(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::acos(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_atan(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::atan(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_atan2(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::atan2(ctx.arg_f64(0), ctx.arg_f64(1))); ExternResult::Ok(1) }

fn extern_sinh(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::sinh(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_cosh(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::cosh(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_tanh(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::tanh(ctx.arg_f64(0))); ExternResult::Ok(1) }

fn extern_mod(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::mod_f(ctx.arg_f64(0), ctx.arg_f64(1))); ExternResult::Ok(1) }
fn extern_copysign(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::copysign(ctx.arg_f64(0), ctx.arg_f64(1))); ExternResult::Ok(1) }
fn extern_is_nan(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_bool(0, core::is_nan(ctx.arg_f64(0))); ExternResult::Ok(1) }
fn extern_is_inf(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_bool(0, core::is_inf(ctx.arg_f64(0), ctx.arg_i64(1))); ExternResult::Ok(1) }
fn extern_inf(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::inf(ctx.arg_i64(0))); ExternResult::Ok(1) }
fn extern_nan(ctx: &mut ExternCtx) -> ExternResult { ctx.ret_f64(0, core::nan()); ExternResult::Ok(1) }
