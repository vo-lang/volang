//! Extern function C ABI wrappers for AOT/JIT.
//!
//! These are `extern "C"` functions that Cranelift-generated code calls directly.

pub mod errors;
pub mod strings;
pub mod strconv;
pub mod bytes;
pub mod unicode;
pub mod math;
pub mod sort;
pub mod hex;
pub mod base64;
pub mod json;
pub mod regexp;

use crate::extern_dispatch::ExternDispatchFn;
use gox_runtime_core::gc::TypeId;

// Type ID constants for runtime object creation
const STRING_TYPE_ID: TypeId = 3; // Same as gox_vm::types::builtin::STRING

/// Register all extern functions for JIT dispatch.
pub fn register_all(register: &mut dyn FnMut(&str, ExternDispatchFn)) {
    // math package
    register("math.Sqrt", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_sqrt(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Sin", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_sin(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Cos", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_cos(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Tan", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_tan(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Exp", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_exp(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Log", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_log(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Pow", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_pow(f64::from_bits(args[0]), f64::from_bits(args[1])));
        Ok(())
    });
    register("math.Floor", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_floor(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Ceil", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_ceil(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Abs", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_abs(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Round", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_round(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Trunc", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_trunc(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Min", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_min(f64::from_bits(args[0]), f64::from_bits(args[1])));
        Ok(())
    });
    register("math.Max", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_max(f64::from_bits(args[0]), f64::from_bits(args[1])));
        Ok(())
    });
    register("math.Dim", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_dim(f64::from_bits(args[0]), f64::from_bits(args[1])));
        Ok(())
    });
    register("math.Cbrt", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_cbrt(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Pow10", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_pow10(args[0] as i64));
        Ok(())
    });
    register("math.Hypot", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_hypot(f64::from_bits(args[0]), f64::from_bits(args[1])));
        Ok(())
    });
    register("math.Log2", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_log2(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Log10", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_log10(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Asin", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_asin(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Acos", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_acos(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Atan", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_atan(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Atan2", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_atan2(f64::from_bits(args[0]), f64::from_bits(args[1])));
        Ok(())
    });
    register("math.Sinh", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_sinh(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Cosh", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_cosh(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Tanh", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_tanh(f64::from_bits(args[0])));
        Ok(())
    });
    register("math.Mod", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_mod(f64::from_bits(args[0]), f64::from_bits(args[1])));
        Ok(())
    });
    register("math.Copysign", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_copysign(f64::from_bits(args[0]), f64::from_bits(args[1])));
        Ok(())
    });
    register("math.IsNaN", |args, rets| {
        rets[0] = if math::gox_math_isnan(f64::from_bits(args[0])) { 1 } else { 0 };
        Ok(())
    });
    register("math.IsInf", |args, rets| {
        rets[0] = if math::gox_math_isinf(f64::from_bits(args[0]), args[1] as i64) { 1 } else { 0 };
        Ok(())
    });
    register("math.Inf", |args, rets| {
        rets[0] = f64::to_bits(math::gox_math_inf(args[0] as i64));
        Ok(())
    });
    register("math.NaN", |_args, rets| {
        rets[0] = f64::to_bits(math::gox_math_nan());
        Ok(())
    });
    
    // strings package
    register("strings.Index", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let substr = args[1] as gox_runtime_core::gc::GcRef;
        rets[0] = unsafe { strings::gox_strings_index(s, substr) } as u64;
        Ok(())
    });
    register("strings.LastIndex", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let substr = args[1] as gox_runtime_core::gc::GcRef;
        rets[0] = unsafe { strings::gox_strings_last_index(s, substr) } as u64;
        Ok(())
    });
    register("strings.Count", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let substr = args[1] as gox_runtime_core::gc::GcRef;
        rets[0] = unsafe { strings::gox_strings_count(s, substr) } as u64;
        Ok(())
    });
    register("strings.Contains", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let substr = args[1] as gox_runtime_core::gc::GcRef;
        rets[0] = if unsafe { strings::gox_strings_contains(s, substr) } { 1 } else { 0 };
        Ok(())
    });
    register("strings.HasPrefix", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let prefix = args[1] as gox_runtime_core::gc::GcRef;
        rets[0] = if unsafe { strings::gox_strings_has_prefix(s, prefix) } { 1 } else { 0 };
        Ok(())
    });
    register("strings.HasSuffix", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let suffix = args[1] as gox_runtime_core::gc::GcRef;
        rets[0] = if unsafe { strings::gox_strings_has_suffix(s, suffix) } { 1 } else { 0 };
        Ok(())
    });
    register("strings.EqualFold", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let t = args[1] as gox_runtime_core::gc::GcRef;
        rets[0] = if unsafe { strings::gox_strings_equal_fold(s, t) } { 1 } else { 0 };
        Ok(())
    });
    register("strings.ToLower", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strings::gox_strings_to_lower(gc, s, STRING_TYPE_ID) } as u64;
        });
        Ok(())
    });
    register("strings.ToUpper", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strings::gox_strings_to_upper(gc, s, STRING_TYPE_ID) } as u64;
        });
        Ok(())
    });
    register("strings.TrimSpace", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strings::gox_strings_trim_space(gc, s, STRING_TYPE_ID) } as u64;
        });
        Ok(())
    });
    register("strings.Trim", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let cutset = args[1] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strings::gox_strings_trim(gc, s, cutset, STRING_TYPE_ID) } as u64;
        });
        Ok(())
    });
    register("strings.Repeat", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let n = args[1] as i64;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strings::gox_strings_repeat(gc, s, n, STRING_TYPE_ID) } as u64;
        });
        Ok(())
    });
    register("strings.Replace", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let old = args[1] as gox_runtime_core::gc::GcRef;
        let new = args[2] as gox_runtime_core::gc::GcRef;
        let n = args[3] as i64;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strings::gox_strings_replace(gc, s, old, new, n, STRING_TYPE_ID) } as u64;
        });
        Ok(())
    });
}
