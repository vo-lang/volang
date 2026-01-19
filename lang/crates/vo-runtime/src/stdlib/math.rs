//! math package native function implementations.
//!
//! Native functions that require libm or hardware instructions.
//! Simple functions (Abs, Max, Min, Dim, IsNaN, IsInf, Signbit, Copysign, Pow10)
//! are implemented in Vo for better JIT inlining.

use vo_ffi_macro::vo_extern_std;

// ==================== Rounding (hardware instructions) ====================

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

// ==================== Power and root (libm) ====================

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

#[vo_extern_std("math", "Hypot")]
fn hypot(x: f64, y: f64) -> f64 {
    x.hypot(y)
}

// ==================== Exponential and logarithm (libm) ====================

#[vo_extern_std("math", "Exp")]
fn exp(x: f64) -> f64 {
    x.exp()
}

#[vo_extern_std("math", "Exp2")]
fn exp2(x: f64) -> f64 {
    x.exp2()
}

#[vo_extern_std("math", "Expm1")]
fn expm1(x: f64) -> f64 {
    x.exp_m1()
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

#[vo_extern_std("math", "Log1p")]
fn log1p(x: f64) -> f64 {
    x.ln_1p()
}

// ==================== Trigonometric (libm) ====================

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

// ==================== Hyperbolic (libm) ====================

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

#[vo_extern_std("math", "Asinh")]
fn asinh(x: f64) -> f64 {
    x.asinh()
}

#[vo_extern_std("math", "Acosh")]
fn acosh(x: f64) -> f64 {
    x.acosh()
}

#[vo_extern_std("math", "Atanh")]
fn atanh(x: f64) -> f64 {
    x.atanh()
}

// ==================== IEEE 754 operations ====================

#[vo_extern_std("math", "Mod")]
fn mod_fn(x: f64, y: f64) -> f64 {
    x % y
}

#[vo_extern_std("math", "Modf")]
fn modf(x: f64) -> (f64, f64) {
    let int_part = x.trunc();
    let frac_part = x - int_part;
    (int_part, frac_part)
}

#[vo_extern_std("math", "Frexp")]
fn frexp(x: f64) -> (f64, i64) {
    if x == 0.0 || x.is_nan() || x.is_infinite() {
        return (x, 0);
    }
    let bits = x.to_bits();
    let sign = bits & 0x8000_0000_0000_0000;
    let exp = ((bits >> 52) & 0x7FF) as i64;
    let mantissa = bits & 0x000F_FFFF_FFFF_FFFF;
    
    if exp == 0 {
        // Subnormal - normalize first
        let normalized = x * (1u64 << 54) as f64;
        let (frac, e) = frexp(normalized);
        return (frac, e - 54);
    }
    
    // Normal number: return mantissa in [0.5, 1) and exponent
    let frac_bits = sign | 0x3FE0_0000_0000_0000 | mantissa;
    let frac = f64::from_bits(frac_bits);
    let e = exp - 1022;
    (frac, e)
}

#[vo_extern_std("math", "Ldexp")]
fn ldexp(frac: f64, exp: i64) -> f64 {
    frac * (2.0_f64).powi(exp as i32)
}

#[vo_extern_std("math", "FMA")]
fn fma(x: f64, y: f64, z: f64) -> f64 {
    x.mul_add(y, z)
}

#[vo_extern_std("math", "Inf")]
fn inf(sign: i64) -> f64 {
    if sign >= 0 { f64::INFINITY } else { f64::NEG_INFINITY }
}

#[vo_extern_std("math", "NaN")]
fn nan() -> f64 {
    f64::NAN
}

// Register all math extern functions using the stdlib_register! macro.
// The macro automatically handles ExternFn vs ExternFnWithContext based on __STDLIB_* consts.
crate::stdlib_register!(math:
    Floor, Ceil, Round, Trunc,
    Sqrt, Cbrt, Pow, Hypot,
    Exp, Exp2, Expm1,
    Log, Log2, Log10, Log1p,
    Sin, Cos, Tan,
    Asin, Acos, Atan, Atan2,
    Sinh, Cosh, Tanh,
    Asinh, Acosh, Atanh,
    Mod, Modf, Frexp, Ldexp,
    FMA, Inf, NaN,
);
