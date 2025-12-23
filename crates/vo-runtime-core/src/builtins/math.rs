//! Math operations (pure logic).

use std::f64::consts;

// Constants
pub const E: f64 = consts::E;
pub const PI: f64 = consts::PI;
pub const PHI: f64 = 1.618033988749895;
pub const SQRT2: f64 = consts::SQRT_2;
pub const SQRT_E: f64 = 1.6487212707001282;
pub const SQRT_PI: f64 = 1.7724538509055159;
pub const SQRT_PHI: f64 = 1.272019649514069;
pub const LN2: f64 = consts::LN_2;
pub const LN10: f64 = consts::LN_10;
pub const LOG2_E: f64 = consts::LOG2_E;
pub const LOG10_E: f64 = consts::LOG10_E;

// Basic operations
pub fn abs(x: f64) -> f64 { x.abs() }
pub fn floor(x: f64) -> f64 { x.floor() }
pub fn ceil(x: f64) -> f64 { x.ceil() }
pub fn round(x: f64) -> f64 { x.round() }
pub fn trunc(x: f64) -> f64 { x.trunc() }

// Min/Max
pub fn max(x: f64, y: f64) -> f64 { x.max(y) }
pub fn min(x: f64, y: f64) -> f64 { x.min(y) }
pub fn dim(x: f64, y: f64) -> f64 { (x - y).max(0.0) }

// Power and roots
pub fn sqrt(x: f64) -> f64 { x.sqrt() }
pub fn cbrt(x: f64) -> f64 { x.cbrt() }
pub fn pow(x: f64, y: f64) -> f64 { x.powf(y) }
pub fn pow10(n: i64) -> f64 { 10f64.powi(n as i32) }
pub fn hypot(x: f64, y: f64) -> f64 { x.hypot(y) }

// Exponential and logarithm
pub fn exp(x: f64) -> f64 { x.exp() }
pub fn exp2(x: f64) -> f64 { x.exp2() }
pub fn expm1(x: f64) -> f64 { x.exp_m1() }
pub fn log(x: f64) -> f64 { x.ln() }
pub fn log2(x: f64) -> f64 { x.log2() }
pub fn log10(x: f64) -> f64 { x.log10() }
pub fn log1p(x: f64) -> f64 { x.ln_1p() }

// Trigonometric
pub fn sin(x: f64) -> f64 { x.sin() }
pub fn cos(x: f64) -> f64 { x.cos() }
pub fn tan(x: f64) -> f64 { x.tan() }
pub fn asin(x: f64) -> f64 { x.asin() }
pub fn acos(x: f64) -> f64 { x.acos() }
pub fn atan(x: f64) -> f64 { x.atan() }
pub fn atan2(y: f64, x: f64) -> f64 { y.atan2(x) }

// Hyperbolic
pub fn sinh(x: f64) -> f64 { x.sinh() }
pub fn cosh(x: f64) -> f64 { x.cosh() }
pub fn tanh(x: f64) -> f64 { x.tanh() }
pub fn asinh(x: f64) -> f64 { x.asinh() }
pub fn acosh(x: f64) -> f64 { x.acosh() }
pub fn atanh(x: f64) -> f64 { x.atanh() }

// Special functions
pub fn erf(x: f64) -> f64 {
    // Approximation using Horner's method
    let a1 =  0.254829592;
    let a2 = -0.284496736;
    let a3 =  1.421413741;
    let a4 = -1.453152027;
    let a5 =  1.061405429;
    let p  =  0.3275911;

    let sign = if x < 0.0 { -1.0 } else { 1.0 };
    let x = x.abs();
    let t = 1.0 / (1.0 + p * x);
    let y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * (-x * x).exp();
    sign * y
}

pub fn erfc(x: f64) -> f64 { 1.0 - erf(x) }

pub fn gamma(x: f64) -> f64 {
    // Use Lanczos approximation
    libm::tgamma(x)
}

pub fn lgamma(x: f64) -> f64 {
    libm::lgamma(x)
}

// Utility
pub fn mod_f(x: f64, y: f64) -> f64 { x % y }
pub fn modf(x: f64) -> (f64, f64) { (x.trunc(), x.fract()) }
pub fn frexp(x: f64) -> (f64, i64) {
    let (frac, exp) = libm::frexp(x);
    (frac, exp as i64)
}
pub fn ldexp(frac: f64, exp: i64) -> f64 {
    libm::ldexp(frac, exp as i32)
}

pub fn copysign(x: f64, y: f64) -> f64 { x.copysign(y) }
pub fn signbit(x: f64) -> bool { x.is_sign_negative() }

// Classification
pub fn is_nan(x: f64) -> bool { x.is_nan() }
pub fn is_inf(x: f64, sign: i64) -> bool {
    match sign {
        1 => x.is_infinite() && x > 0.0,
        -1 => x.is_infinite() && x < 0.0,
        _ => x.is_infinite(),
    }
}

// Special values
pub fn inf(sign: i64) -> f64 {
    if sign >= 0 { f64::INFINITY } else { f64::NEG_INFINITY }
}
pub fn nan() -> f64 { f64::NAN }
