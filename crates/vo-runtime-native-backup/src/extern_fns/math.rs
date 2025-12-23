//! math package C ABI for JIT.

use vo_runtime_core::builtins::math as core;

#[no_mangle]
pub extern "C" fn vo_math_sqrt(x: f64) -> f64 { core::sqrt(x) }

#[no_mangle]
pub extern "C" fn vo_math_sin(x: f64) -> f64 { core::sin(x) }

#[no_mangle]
pub extern "C" fn vo_math_cos(x: f64) -> f64 { core::cos(x) }

#[no_mangle]
pub extern "C" fn vo_math_tan(x: f64) -> f64 { core::tan(x) }

#[no_mangle]
pub extern "C" fn vo_math_exp(x: f64) -> f64 { core::exp(x) }

#[no_mangle]
pub extern "C" fn vo_math_log(x: f64) -> f64 { core::log(x) }

#[no_mangle]
pub extern "C" fn vo_math_pow(x: f64, y: f64) -> f64 { core::pow(x, y) }

#[no_mangle]
pub extern "C" fn vo_math_floor(x: f64) -> f64 { core::floor(x) }

#[no_mangle]
pub extern "C" fn vo_math_ceil(x: f64) -> f64 { core::ceil(x) }

#[no_mangle]
pub extern "C" fn vo_math_abs(x: f64) -> f64 { core::abs(x) }

#[no_mangle]
pub extern "C" fn vo_math_round(x: f64) -> f64 { core::round(x) }

#[no_mangle]
pub extern "C" fn vo_math_trunc(x: f64) -> f64 { core::trunc(x) }

#[no_mangle]
pub extern "C" fn vo_math_min(x: f64, y: f64) -> f64 { core::min(x, y) }

#[no_mangle]
pub extern "C" fn vo_math_max(x: f64, y: f64) -> f64 { core::max(x, y) }

#[no_mangle]
pub extern "C" fn vo_math_dim(x: f64, y: f64) -> f64 { core::dim(x, y) }

#[no_mangle]
pub extern "C" fn vo_math_cbrt(x: f64) -> f64 { core::cbrt(x) }

#[no_mangle]
pub extern "C" fn vo_math_pow10(n: i64) -> f64 { core::pow10(n) }

#[no_mangle]
pub extern "C" fn vo_math_hypot(x: f64, y: f64) -> f64 { core::hypot(x, y) }

#[no_mangle]
pub extern "C" fn vo_math_log2(x: f64) -> f64 { core::log2(x) }

#[no_mangle]
pub extern "C" fn vo_math_log10(x: f64) -> f64 { core::log10(x) }

#[no_mangle]
pub extern "C" fn vo_math_asin(x: f64) -> f64 { core::asin(x) }

#[no_mangle]
pub extern "C" fn vo_math_acos(x: f64) -> f64 { core::acos(x) }

#[no_mangle]
pub extern "C" fn vo_math_atan(x: f64) -> f64 { core::atan(x) }

#[no_mangle]
pub extern "C" fn vo_math_atan2(y: f64, x: f64) -> f64 { core::atan2(y, x) }

#[no_mangle]
pub extern "C" fn vo_math_sinh(x: f64) -> f64 { core::sinh(x) }

#[no_mangle]
pub extern "C" fn vo_math_cosh(x: f64) -> f64 { core::cosh(x) }

#[no_mangle]
pub extern "C" fn vo_math_tanh(x: f64) -> f64 { core::tanh(x) }

#[no_mangle]
pub extern "C" fn vo_math_mod(x: f64, y: f64) -> f64 { core::mod_f(x, y) }

#[no_mangle]
pub extern "C" fn vo_math_copysign(x: f64, y: f64) -> f64 { core::copysign(x, y) }

#[no_mangle]
pub extern "C" fn vo_math_isnan(x: f64) -> bool { core::is_nan(x) }

#[no_mangle]
pub extern "C" fn vo_math_isinf(x: f64, sign: i64) -> bool { core::is_inf(x, sign) }

#[no_mangle]
pub extern "C" fn vo_math_inf(sign: i64) -> f64 { core::inf(sign) }

#[no_mangle]
pub extern "C" fn vo_math_nan() -> f64 { core::nan() }
