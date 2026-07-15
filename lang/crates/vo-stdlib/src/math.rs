//! math package native function implementations.
//!
//! Native functions that require libm or hardware instructions.
//! Simple functions (Abs, Max, Min, Dim, IsNaN, IsInf, Signbit, Copysign, Pow10)
//! are implemented in Vo for better JIT inlining.

use vo_ffi_macro::vostd_fn;

// ==================== Rounding (hardware instructions) ====================

#[vostd_fn("math", "Floor")]
fn floor(x: f64) -> f64 {
    libm::floor(x)
}

#[vostd_fn("math", "Ceil")]
fn ceil(x: f64) -> f64 {
    libm::ceil(x)
}

#[vostd_fn("math", "Round")]
fn round(x: f64) -> f64 {
    libm::round(x)
}

#[vostd_fn("math", "Trunc")]
fn trunc(x: f64) -> f64 {
    libm::trunc(x)
}

// ==================== Power and root (libm) ====================

#[vostd_fn("math", "Sqrt")]
fn sqrt(x: f64) -> f64 {
    libm::sqrt(x)
}

#[vostd_fn("math", "Cbrt")]
fn cbrt(x: f64) -> f64 {
    libm::cbrt(x)
}

#[vostd_fn("math", "Pow")]
fn pow(x: f64, y: f64) -> f64 {
    libm::pow(x, y)
}

#[vostd_fn("math", "Hypot")]
fn hypot(x: f64, y: f64) -> f64 {
    libm::hypot(x, y)
}

// ==================== Exponential and logarithm (libm) ====================

#[vostd_fn("math", "Exp")]
fn exp(x: f64) -> f64 {
    libm::exp(x)
}

#[vostd_fn("math", "Exp2")]
fn exp2(x: f64) -> f64 {
    libm::exp2(x)
}

#[vostd_fn("math", "Expm1")]
fn expm1(x: f64) -> f64 {
    libm::expm1(x)
}

#[vostd_fn("math", "Log")]
fn log(x: f64) -> f64 {
    libm::log(x)
}

#[vostd_fn("math", "Log2")]
fn log2(x: f64) -> f64 {
    libm::log2(x)
}

#[vostd_fn("math", "Log10")]
fn log10(x: f64) -> f64 {
    libm::log10(x)
}

#[vostd_fn("math", "Log1p")]
fn log1p(x: f64) -> f64 {
    libm::log1p(x)
}

// ==================== Trigonometric (libm) ====================

#[vostd_fn("math", "Sin")]
fn sin(x: f64) -> f64 {
    libm::sin(x)
}

#[vostd_fn("math", "Cos")]
fn cos(x: f64) -> f64 {
    libm::cos(x)
}

#[vostd_fn("math", "Tan")]
fn tan(x: f64) -> f64 {
    libm::tan(x)
}

#[vostd_fn("math", "Asin")]
fn asin(x: f64) -> f64 {
    libm::asin(x)
}

#[vostd_fn("math", "Acos")]
fn acos(x: f64) -> f64 {
    libm::acos(x)
}

#[vostd_fn("math", "Atan")]
fn atan(x: f64) -> f64 {
    libm::atan(x)
}

#[vostd_fn("math", "Atan2")]
fn atan2(y: f64, x: f64) -> f64 {
    libm::atan2(y, x)
}

// ==================== Hyperbolic (libm) ====================

#[vostd_fn("math", "Sinh")]
fn sinh(x: f64) -> f64 {
    libm::sinh(x)
}

#[vostd_fn("math", "Cosh")]
fn cosh(x: f64) -> f64 {
    libm::cosh(x)
}

#[vostd_fn("math", "Tanh")]
fn tanh(x: f64) -> f64 {
    libm::tanh(x)
}

#[vostd_fn("math", "Asinh")]
fn asinh(x: f64) -> f64 {
    libm::asinh(x)
}

#[vostd_fn("math", "Acosh")]
fn acosh(x: f64) -> f64 {
    libm::acosh(x)
}

#[vostd_fn("math", "Atanh")]
fn atanh(x: f64) -> f64 {
    libm::atanh(x)
}

// ==================== IEEE 754 operations ====================

#[vostd_fn("math", "Mod")]
fn mod_fn(x: f64, y: f64) -> f64 {
    x % y
}

#[vostd_fn("math", "Modf")]
fn modf(x: f64) -> (f64, f64) {
    let int_part = libm::trunc(x);
    let frac_part = (x - int_part).copysign(x);
    (int_part, frac_part)
}

#[vostd_fn("math", "Frexp")]
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

#[vostd_fn("math", "Ldexp")]
fn ldexp(frac: f64, exp: i64) -> f64 {
    if frac == 0.0 || !frac.is_finite() {
        return frac;
    }

    const SIGN_MASK: u64 = 1 << 63;
    const EXP_MASK: u64 = 0x7ff;
    const EXP_SHIFT: u32 = 52;
    const EXP_BIAS: i64 = 1023;

    let mut normalized = frac;
    let mut exponent = exp;
    if normalized.abs() < 2.225_073_858_507_201_4e-308 {
        normalized *= (1_u64 << 52) as f64;
        exponent = match exponent.checked_sub(52) {
            Some(value) => value,
            None => return 0.0_f64.copysign(frac),
        };
    }

    let mut bits = normalized.to_bits();
    let encoded_exp = ((bits >> EXP_SHIFT) & EXP_MASK) as i64;
    exponent = match exponent.checked_add(encoded_exp - EXP_BIAS) {
        Some(value) => value,
        None if exponent.is_positive() => return f64::INFINITY.copysign(frac),
        None => return 0.0_f64.copysign(frac),
    };
    if exponent < -1075 {
        return 0.0_f64.copysign(frac);
    }
    if exponent > 1023 {
        return f64::INFINITY.copysign(frac);
    }

    let mut multiplier = 1.0;
    if exponent < -1022 {
        exponent += 53;
        multiplier = 1.0 / (1_u64 << 53) as f64;
    }
    bits &= SIGN_MASK | ((1_u64 << EXP_SHIFT) - 1);
    bits |= ((exponent + EXP_BIAS) as u64) << EXP_SHIFT;
    multiplier * f64::from_bits(bits)
}

#[vostd_fn("math", "FMA")]
fn fma(x: f64, y: f64, z: f64) -> f64 {
    libm::fma(x, y, z)
}

#[vostd_fn("math", "Inf")]
fn inf(sign: i64) -> f64 {
    if sign >= 0 {
        f64::INFINITY
    } else {
        f64::NEG_INFINITY
    }
}

#[vostd_fn("math", "NaN")]
fn nan() -> f64 {
    // Match Go's canonical quiet-NaN payload (uvnan), including its low bit.
    f64::from_bits(0x7ff8_0000_0000_0001)
}

// ==================== Bit conversion ====================

#[vostd_fn("math", "Float64bits")]
fn float64bits(x: f64) -> u64 {
    x.to_bits()
}

#[vostd_fn("math", "Float64frombits")]
fn float64frombits(b: u64) -> f64 {
    f64::from_bits(b)
}

#[vostd_fn("math", "Float32bits")]
fn float32bits(x: f32) -> u32 {
    x.to_bits()
}

#[vostd_fn("math", "Float32frombits")]
fn float32frombits(b: u32) -> f32 {
    f32::from_bits(b)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nan_matches_go_canonical_payload() {
        assert_eq!(nan().to_bits(), 0x7ff8_0000_0000_0001);
    }
}

// Register all math extern functions using the stdlib_register! macro.
// The macro registers all listed functions via __STDLIB_* consts.
vo_ffi_macro::vostd_register!("math":
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
    Float64bits, Float64frombits,
    Float32bits, Float32frombits,
);
