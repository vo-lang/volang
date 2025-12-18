//! math package C ABI for AOT.

use gox_runtime_core::builtins::math as core;

#[no_mangle]
pub extern "C" fn gox_math_sqrt(x: f64) -> f64 { core::sqrt(x) }

#[no_mangle]
pub extern "C" fn gox_math_sin(x: f64) -> f64 { core::sin(x) }

#[no_mangle]
pub extern "C" fn gox_math_cos(x: f64) -> f64 { core::cos(x) }

#[no_mangle]
pub extern "C" fn gox_math_tan(x: f64) -> f64 { core::tan(x) }

#[no_mangle]
pub extern "C" fn gox_math_exp(x: f64) -> f64 { core::exp(x) }

#[no_mangle]
pub extern "C" fn gox_math_log(x: f64) -> f64 { core::log(x) }

#[no_mangle]
pub extern "C" fn gox_math_pow(x: f64, y: f64) -> f64 { core::pow(x, y) }

#[no_mangle]
pub extern "C" fn gox_math_floor(x: f64) -> f64 { core::floor(x) }

#[no_mangle]
pub extern "C" fn gox_math_ceil(x: f64) -> f64 { core::ceil(x) }

#[no_mangle]
pub extern "C" fn gox_math_abs(x: f64) -> f64 { core::abs(x) }
