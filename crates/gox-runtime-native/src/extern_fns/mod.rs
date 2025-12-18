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
use gox_common_core::ValueKind;

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
            rets[0] = unsafe { strings::gox_strings_to_lower(gc, s, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    register("strings.ToUpper", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strings::gox_strings_to_upper(gc, s, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    register("strings.TrimSpace", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strings::gox_strings_trim_space(gc, s, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    register("strings.Trim", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let cutset = args[1] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strings::gox_strings_trim(gc, s, cutset, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    register("strings.Repeat", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let n = args[1] as i64;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strings::gox_strings_repeat(gc, s, n, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    register("strings.Replace", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let old = args[1] as gox_runtime_core::gc::GcRef;
        let new = args[2] as gox_runtime_core::gc::GcRef;
        let n = args[3] as i64;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strings::gox_strings_replace(gc, s, old, new, n, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    
    // strconv package
    register("strconv.Atoi", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            let result = unsafe { strconv::gox_strconv_atoi(gc, s, ValueKind::String as TypeId) };
            rets[0] = result.0 as u64;  // value
            rets[1] = result.1 as u64;  // error
        });
        Ok(())
    });
    register("strconv.Itoa", |args, rets| {
        let n = args[0] as i64;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strconv::gox_strconv_itoa(gc, n, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    register("strconv.ParseInt", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let base = args[1] as i64;
        crate::gc_global::with_gc(|gc| {
            let result = unsafe { strconv::gox_strconv_parse_int(gc, s, base, ValueKind::String as TypeId) };
            rets[0] = result.0 as u64;
            rets[1] = result.1 as u64;
        });
        Ok(())
    });
    register("strconv.ParseFloat", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            let result = unsafe { strconv::gox_strconv_parse_float(gc, s, ValueKind::String as TypeId) };
            rets[0] = f64::to_bits(result.0);
            rets[1] = result.1 as u64;
        });
        Ok(())
    });
    register("strconv.FormatInt", |args, rets| {
        let n = args[0] as i64;
        let base = args[1] as i64;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strconv::gox_strconv_format_int(gc, n, base, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    register("strconv.FormatFloat", |args, rets| {
        let f = f64::from_bits(args[0]);
        let fmt = args[1] as u8 as char;
        let prec = args[2] as i64;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strconv::gox_strconv_format_float(gc, f, fmt as u8, prec, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    register("strconv.FormatBool", |args, rets| {
        let b = args[0] != 0;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strconv::gox_strconv_format_bool(gc, b, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    register("strconv.ParseBool", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            let result = unsafe { strconv::gox_strconv_parse_bool(gc, s, ValueKind::String as TypeId) };
            rets[0] = if result.0 { 1 } else { 0 };
            rets[1] = result.1 as u64;
        });
        Ok(())
    });
    register("strconv.Quote", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { strconv::gox_strconv_quote(gc, s, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    
    // unicode package
    register("unicode.IsLetter", |args, rets| {
        let r = args[0] as i32;
        rets[0] = if unsafe { unicode::gox_unicode_is_letter(r) } { 1 } else { 0 };
        Ok(())
    });
    register("unicode.IsDigit", |args, rets| {
        let r = args[0] as i32;
        rets[0] = if unsafe { unicode::gox_unicode_is_digit(r) } { 1 } else { 0 };
        Ok(())
    });
    register("unicode.IsSpace", |args, rets| {
        let r = args[0] as i32;
        rets[0] = if unsafe { unicode::gox_unicode_is_space(r) } { 1 } else { 0 };
        Ok(())
    });
    register("unicode.IsUpper", |args, rets| {
        let r = args[0] as i32;
        rets[0] = if unsafe { unicode::gox_unicode_is_upper(r) } { 1 } else { 0 };
        Ok(())
    });
    register("unicode.IsLower", |args, rets| {
        let r = args[0] as i32;
        rets[0] = if unsafe { unicode::gox_unicode_is_lower(r) } { 1 } else { 0 };
        Ok(())
    });
    register("unicode.ToLower", |args, rets| {
        let r = args[0] as i32;
        rets[0] = unsafe { unicode::gox_unicode_to_lower(r) } as u64;
        Ok(())
    });
    register("unicode.ToUpper", |args, rets| {
        let r = args[0] as i32;
        rets[0] = unsafe { unicode::gox_unicode_to_upper(r) } as u64;
        Ok(())
    });
    register("unicode.IsControl", |args, rets| {
        let r = args[0] as i32;
        rets[0] = if unsafe { unicode::gox_unicode_is_control(r) } { 1 } else { 0 };
        Ok(())
    });
    
    // hex package
    register("hex.EncodedLen", |args, rets| {
        let n = args[0] as i64;
        rets[0] = (n * 2) as u64;
        Ok(())
    });
    register("hex.DecodedLen", |args, rets| {
        let n = args[0] as i64;
        rets[0] = (n / 2) as u64;
        Ok(())
    });
    register("hex.EncodeToString", |args, rets| {
        let src = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { hex::gox_hex_encode_to_string(gc, src, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    register("hex.DecodeString", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            let result = unsafe { hex::gox_hex_decode_string(gc, s, ValueKind::String as TypeId) };
            rets[0] = result.0 as u64;  // bytes slice
            rets[1] = result.1 as u64;  // error string
        });
        Ok(())
    });
    
    // base64 package
    register("base64.EncodeToString", |args, rets| {
        let src = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { base64::gox_base64_std_encode(gc, src, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    register("base64.URLEncodeToString", |args, rets| {
        let src = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { base64::gox_base64_url_encode(gc, src, ValueKind::String as TypeId) } as u64;
        });
        Ok(())
    });
    
    // regexp package
    register("regexp.MatchString", |args, rets| {
        let pattern = args[0] as gox_runtime_core::gc::GcRef;
        let s = args[1] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            let result = unsafe { regexp::gox_regexp_match_string(gc, pattern, s, ValueKind::String as TypeId) };
            rets[0] = if result.0 { 1 } else { 0 };
            rets[1] = result.1 as u64;  // error
        });
        Ok(())
    });
    
    // sort package (modify slices in place)
    register("sort.Ints", |args, _rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        unsafe { sort::gox_sort_ints(s) };
        Ok(())
    });
    register("sort.Float64s", |args, _rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        unsafe { sort::gox_sort_float64s(s) };
        Ok(())
    });
    register("sort.Strings", |args, _rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        unsafe { sort::gox_sort_strings(s) };
        Ok(())
    });
    
    // bytes package
    register("bytes.Index", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let sep = args[1] as gox_runtime_core::gc::GcRef;
        rets[0] = unsafe { bytes::gox_bytes_index(s, sep) } as u64;
        Ok(())
    });
    register("bytes.LastIndex", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let sep = args[1] as gox_runtime_core::gc::GcRef;
        rets[0] = unsafe { bytes::gox_bytes_last_index(s, sep) } as u64;
        Ok(())
    });
    register("bytes.IndexByte", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let c = args[1] as u8;
        rets[0] = unsafe { bytes::gox_bytes_index_byte(s, c) } as u64;
        Ok(())
    });
    register("bytes.LastIndexByte", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let c = args[1] as u8;
        rets[0] = unsafe { bytes::gox_bytes_last_index_byte(s, c) } as u64;
        Ok(())
    });
    register("bytes.Count", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let sep = args[1] as gox_runtime_core::gc::GcRef;
        rets[0] = unsafe { bytes::gox_bytes_count(s, sep) } as u64;
        Ok(())
    });
    register("bytes.Compare", |args, rets| {
        let a = args[0] as gox_runtime_core::gc::GcRef;
        let b = args[1] as gox_runtime_core::gc::GcRef;
        rets[0] = unsafe { bytes::gox_bytes_compare(a, b) } as u64;
        Ok(())
    });
    register("bytes.Equal", |args, rets| {
        let a = args[0] as gox_runtime_core::gc::GcRef;
        let b = args[1] as gox_runtime_core::gc::GcRef;
        rets[0] = if unsafe { bytes::gox_bytes_equal(a, b) } { 1 } else { 0 };
        Ok(())
    });
    register("bytes.ToLower", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { bytes::gox_bytes_to_lower(gc, s) } as u64;
        });
        Ok(())
    });
    register("bytes.ToUpper", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { bytes::gox_bytes_to_upper(gc, s) } as u64;
        });
        Ok(())
    });
    register("bytes.Repeat", |args, rets| {
        let s = args[0] as gox_runtime_core::gc::GcRef;
        let count = args[1] as i64;
        crate::gc_global::with_gc(|gc| {
            rets[0] = unsafe { bytes::gox_bytes_repeat(gc, s, count) } as u64;
        });
        Ok(())
    });
    
    // json package
    register("json.Valid", |args, rets| {
        let data = args[0] as gox_runtime_core::gc::GcRef;
        rets[0] = if unsafe { json::gox_json_valid(data) } { 1 } else { 0 };
        Ok(())
    });
}
