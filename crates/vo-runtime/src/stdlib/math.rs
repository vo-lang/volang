//! math package native function implementations.
//!
//! Provides mathematical functions for the math standard library package.

use vo_ffi_macro::vo_extern_std;

// ==================== Basic operations ====================

#[vo_extern_std("math", "Abs")]
fn abs(x: f64) -> f64 {
    x.abs()
}

#[vo_extern_std("math", "Floor")]
fn floor(x: f64) -> f64 {
    x.floor()
}

#[vo_extern_std("math", "Ceil")]
fn ceil(x: f64) -> f64 {
    x.ceil()
}

#[vo_extern_std("math", "Round")]
fn round(x: f64) -> f64 {
    x.round()
}

#[vo_extern_std("math", "Trunc")]
fn trunc(x: f64) -> f64 {
    x.trunc()
}

#[vo_extern_std("math", "Max")]
fn max(x: f64, y: f64) -> f64 {
    x.max(y)
}

#[vo_extern_std("math", "Min")]
fn min(x: f64, y: f64) -> f64 {
    x.min(y)
}

#[vo_extern_std("math", "Dim")]
fn dim(x: f64, y: f64) -> f64 {
    if x > y { x - y } else { 0.0 }
}

// ==================== Power and root ====================

#[vo_extern_std("math", "Sqrt")]
fn sqrt(x: f64) -> f64 {
    x.sqrt()
}

#[vo_extern_std("math", "Cbrt")]
fn cbrt(x: f64) -> f64 {
    x.cbrt()
}

#[vo_extern_std("math", "Pow")]
fn pow(x: f64, y: f64) -> f64 {
    x.powf(y)
}

#[vo_extern_std("math", "Pow10")]
fn pow10(n: i64) -> f64 {
    10.0_f64.powi(n as i32)
}

#[vo_extern_std("math", "Hypot")]
fn hypot(x: f64, y: f64) -> f64 {
    x.hypot(y)
}

// ==================== Exponential and logarithm ====================

#[vo_extern_std("math", "Exp")]
fn exp(x: f64) -> f64 {
    x.exp()
}

#[vo_extern_std("math", "Log")]
fn log(x: f64) -> f64 {
    x.ln()
}

#[vo_extern_std("math", "Log2")]
fn log2(x: f64) -> f64 {
    x.log2()
}

#[vo_extern_std("math", "Log10")]
fn log10(x: f64) -> f64 {
    x.log10()
}

// ==================== Trigonometric ====================

#[vo_extern_std("math", "Sin")]
fn sin(x: f64) -> f64 {
    x.sin()
}

#[vo_extern_std("math", "Cos")]
fn cos(x: f64) -> f64 {
    x.cos()
}

#[vo_extern_std("math", "Tan")]
fn tan(x: f64) -> f64 {
    x.tan()
}

#[vo_extern_std("math", "Asin")]
fn asin(x: f64) -> f64 {
    x.asin()
}

#[vo_extern_std("math", "Acos")]
fn acos(x: f64) -> f64 {
    x.acos()
}

#[vo_extern_std("math", "Atan")]
fn atan(x: f64) -> f64 {
    x.atan()
}

#[vo_extern_std("math", "Atan2")]
fn atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}

// ==================== Hyperbolic ====================

#[vo_extern_std("math", "Sinh")]
fn sinh(x: f64) -> f64 {
    x.sinh()
}

#[vo_extern_std("math", "Cosh")]
fn cosh(x: f64) -> f64 {
    x.cosh()
}

#[vo_extern_std("math", "Tanh")]
fn tanh(x: f64) -> f64 {
    x.tanh()
}

// ==================== Other ====================

#[vo_extern_std("math", "Mod")]
fn mod_fn(x: f64, y: f64) -> f64 {
    x % y
}

#[vo_extern_std("math", "Copysign")]
fn copysign(x: f64, y: f64) -> f64 {
    x.copysign(y)
}

#[vo_extern_std("math", "IsNaN")]
fn is_nan(x: f64) -> bool {
    x.is_nan()
}

#[vo_extern_std("math", "IsInf")]
fn is_inf(x: f64, sign: i64) -> bool {
    match sign {
        1 => x == f64::INFINITY,
        -1 => x == f64::NEG_INFINITY,
        _ => x.is_infinite(),
    }
}

#[vo_extern_std("math", "Inf")]
fn inf(sign: i64) -> f64 {
    if sign >= 0 { f64::INFINITY } else { f64::NEG_INFINITY }
}

#[vo_extern_std("math", "NaN")]
fn nan() -> f64 {
    f64::NAN
}
