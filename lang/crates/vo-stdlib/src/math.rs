//! math package native function implementations.
//!
//! Native functions that require libm or hardware instructions.
//! Simple functions (Abs, Max, Min, Dim, IsNaN, IsInf, Signbit, Copysign, Pow10)
//! are implemented in Vo for better JIT inlining.

use vo_ffi_macro::vostd_extern;

// ==================== Rounding (hardware instructions) ====================

#[vostd_extern("math", "Floor")]
fn floor(x: f64) -> f64 {
    x.floor()
}

#[vostd_extern("math", "Ceil")]
fn ceil(x: f64) -> f64 {
    x.ceil()
}

#[vostd_extern("math", "Round")]
fn round(x: f64) -> f64 {
    x.round()
}

#[vostd_extern("math", "Trunc")]
fn trunc(x: f64) -> f64 {
    x.trunc()
}

// ==================== Power and root (libm) ====================

#[vostd_extern("math", "Sqrt")]
fn sqrt(x: f64) -> f64 {
    x.sqrt()
}

#[vostd_extern("math", "Cbrt")]
fn cbrt(x: f64) -> f64 {
    x.cbrt()
}

#[vostd_extern("math", "Pow")]
fn pow(x: f64, y: f64) -> f64 {
    x.powf(y)
}

#[vostd_extern("math", "Hypot")]
fn hypot(x: f64, y: f64) -> f64 {
    x.hypot(y)
}

// ==================== Exponential and logarithm (libm) ====================

#[vostd_extern("math", "Exp")]
fn exp(x: f64) -> f64 {
    x.exp()
}

#[vostd_extern("math", "Exp2")]
fn exp2(x: f64) -> f64 {
    x.exp2()
}

#[vostd_extern("math", "Expm1")]
fn expm1(x: f64) -> f64 {
    x.exp_m1()
}

#[vostd_extern("math", "Log")]
fn log(x: f64) -> f64 {
    x.ln()
}

#[vostd_extern("math", "Log2")]
fn log2(x: f64) -> f64 {
    x.log2()
}

#[vostd_extern("math", "Log10")]
fn log10(x: f64) -> f64 {
    x.log10()
}

#[vostd_extern("math", "Log1p")]
fn log1p(x: f64) -> f64 {
    x.ln_1p()
}

// ==================== Trigonometric (libm) ====================

#[vostd_extern("math", "Sin")]
fn sin(x: f64) -> f64 {
    x.sin()
}

#[vostd_extern("math", "Cos")]
fn cos(x: f64) -> f64 {
    x.cos()
}

#[vostd_extern("math", "Tan")]
fn tan(x: f64) -> f64 {
    x.tan()
}

#[vostd_extern("math", "Asin")]
fn asin(x: f64) -> f64 {
    x.asin()
}

#[vostd_extern("math", "Acos")]
fn acos(x: f64) -> f64 {
    x.acos()
}

#[vostd_extern("math", "Atan")]
fn atan(x: f64) -> f64 {
    x.atan()
}

#[vostd_extern("math", "Atan2")]
fn atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}

// ==================== Hyperbolic (libm) ====================

#[vostd_extern("math", "Sinh")]
fn sinh(x: f64) -> f64 {
    x.sinh()
}

#[vostd_extern("math", "Cosh")]
fn cosh(x: f64) -> f64 {
    x.cosh()
}

#[vostd_extern("math", "Tanh")]
fn tanh(x: f64) -> f64 {
    x.tanh()
}

#[vostd_extern("math", "Asinh")]
fn asinh(x: f64) -> f64 {
    x.asinh()
}

#[vostd_extern("math", "Acosh")]
fn acosh(x: f64) -> f64 {
    x.acosh()
}

#[vostd_extern("math", "Atanh")]
fn atanh(x: f64) -> f64 {
    x.atanh()
}

// ==================== IEEE 754 operations ====================

#[vostd_extern("math", "Mod")]
fn mod_fn(x: f64, y: f64) -> f64 {
    x % y
}

#[vostd_extern("math", "Modf")]
fn modf(x: f64) -> (f64, f64) {
    let int_part = x.trunc();
    let frac_part = x - int_part;
    (int_part, frac_part)
}

#[vostd_extern("math", "Frexp")]
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

#[vostd_extern("math", "Ldexp")]
fn ldexp(frac: f64, exp: i64) -> f64 {
    frac * (2.0_f64).powi(exp as i32)
}

#[vostd_extern("math", "FMA")]
fn fma(x: f64, y: f64, z: f64) -> f64 {
    x.mul_add(y, z)
}

#[vostd_extern("math", "Inf")]
fn inf(sign: i64) -> f64 {
    if sign >= 0 { f64::INFINITY } else { f64::NEG_INFINITY }
}

#[vostd_extern("math", "NaN")]
fn nan() -> f64 {
    f64::NAN
}

// Register all math extern functions using the stdlib_register! macro.
// The macro automatically handles ExternFn vs ExternFnWithContext based on __STDLIB_* consts.
vo_runtime::stdlib_register!(math:
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
