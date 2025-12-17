//! strings package native functions.
//!
//! Provides string manipulation functions using zero-copy native API.

use gox_vm::{NativeCtx, NativeRegistry, NativeResult};
use gox_vm::gc::GcRef;
use gox_vm::objects::{array, slice, string};
use gox_vm::types::builtin;

/// Register strings functions.
/// GoX implementations: HasPrefix, HasSuffix, TrimPrefix, TrimSuffix, Contains,
///                      Repeat, Compare, ReplaceAll (in stdlib/strings/strings.gox)
pub fn register(registry: &mut NativeRegistry) {
    // Search (native: string search algorithms)
    registry.register("strings.Index", native_index);
    registry.register("strings.LastIndex", native_last_index);
    registry.register("strings.Count", native_count);
    registry.register("strings.ContainsAny", native_contains_any);
    
    // Transform (native: Unicode tables)
    registry.register("strings.ToLower", native_to_lower);
    registry.register("strings.ToUpper", native_to_upper);
    registry.register("strings.TrimSpace", native_trim_space);
    registry.register("strings.Trim", native_trim);
    registry.register("strings.Replace", native_replace);
    
    // Split/Join (native: slice allocation)
    registry.register("strings.Split", native_split);
    registry.register("strings.SplitN", native_split_n);
    registry.register("strings.Join", native_join);
    
    // Compare (native: Unicode case folding)
    registry.register("strings.EqualFold", native_equal_fold);
}

// ==================== Search Functions ====================

fn native_contains_any(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0);
    let chars = ctx.arg_str(1);
    ctx.ret_bool(0, gox_runtime_core::stdlib::strings::contains_any(s, chars));
    NativeResult::Ok(1)
}

fn native_index(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0);
    let substr = ctx.arg_str(1);
    ctx.ret_i64(0, gox_runtime_core::stdlib::strings::index(s, substr));
    NativeResult::Ok(1)
}

fn native_last_index(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0);
    let substr = ctx.arg_str(1);
    ctx.ret_i64(0, gox_runtime_core::stdlib::strings::last_index(s, substr));
    NativeResult::Ok(1)
}

fn native_count(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0);
    let substr = ctx.arg_str(1);
    ctx.ret_i64(0, gox_runtime_core::stdlib::strings::count(s, substr) as i64);
    NativeResult::Ok(1)
}

// ==================== Transform Functions ====================

fn native_to_lower(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0);
    let result = gox_runtime_core::stdlib::strings::to_lower(s);
    ctx.ret_string(0, &result);
    NativeResult::Ok(1)
}

fn native_to_upper(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0);
    let result = gox_runtime_core::stdlib::strings::to_upper(s);
    ctx.ret_string(0, &result);
    NativeResult::Ok(1)
}

fn native_trim_space(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0).to_string();
    let result = gox_runtime_core::stdlib::strings::trim_space(&s);
    ctx.ret_string(0, result);
    NativeResult::Ok(1)
}

fn native_trim(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0);
    let cutset = ctx.arg_str(1);
    let result = gox_runtime_core::stdlib::strings::trim(s, cutset);
    ctx.ret_string(0, &result);
    NativeResult::Ok(1)
}

fn native_replace(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0);
    let old = ctx.arg_str(1);
    let new = ctx.arg_str(2);
    let n = ctx.arg_i64(3);
    let result = gox_runtime_core::stdlib::strings::replace(s, old, new, n);
    ctx.ret_string(0, &result);
    NativeResult::Ok(1)
}

// ==================== Split/Join Functions ====================

/// Helper to create a GoX string slice from owned Rust strings
fn create_string_slice_owned(ctx: &mut NativeCtx, strings: Vec<String>) -> GcRef {
    let gc = ctx.gc();
    
    // Create array to hold string references
    let arr = array::create(gc, builtin::ARRAY, builtin::STRING, 1, strings.len());
    
    // Create and store each string
    for (i, s) in strings.iter().enumerate() {
        let str_ref = string::from_rust_str(gc, builtin::STRING, s);
        array::set(arr, i, str_ref as u64);
    }
    
    // Create slice from array
    slice::from_array(gc, builtin::SLICE, arr)
}

fn native_split(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0);
    let sep = ctx.arg_str(1);
    let parts = gox_runtime_core::stdlib::strings::split(s, sep);
    let slice_ref = create_string_slice_owned(ctx, parts);
    ctx.ret_ref(0, slice_ref);
    NativeResult::Ok(1)
}

fn native_split_n(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0);
    let sep = ctx.arg_str(1);
    let n = ctx.arg_i64(2) as usize;
    let parts = gox_runtime_core::stdlib::strings::split_n(s, sep, n);
    let slice_ref = create_string_slice_owned(ctx, parts);
    ctx.ret_ref(0, slice_ref);
    NativeResult::Ok(1)
}

fn native_join(ctx: &mut NativeCtx) -> NativeResult {
    let elems_ref = ctx.arg_ref(0);
    let sep = ctx.arg_str(1);
    
    if elems_ref.is_null() {
        ctx.ret_string(0, "");
        return NativeResult::Ok(1);
    }
    
    // Read strings from slice
    let len = slice::len(elems_ref);
    let mut parts = Vec::with_capacity(len);
    
    for i in 0..len {
        let str_ref = slice::get(elems_ref, i) as GcRef;
        if !str_ref.is_null() {
            parts.push(string::as_str(str_ref));
        } else {
            parts.push("");
        }
    }
    
    let result = parts.join(sep);
    ctx.ret_string(0, &result);
    NativeResult::Ok(1)
}

// ==================== Compare Functions ====================

fn native_equal_fold(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0);
    let t = ctx.arg_str(1);
    ctx.ret_bool(0, gox_runtime_core::stdlib::strings::equal_fold(s, t));
    NativeResult::Ok(1)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_register() {
        let mut registry = NativeRegistry::new();
        register(&mut registry);
        
        // Native functions only (GoX implementations don't register here)
        assert!(registry.get("strings.Index").is_some());
        assert!(registry.get("strings.Split").is_some());
        assert!(registry.get("strings.Join").is_some());
    }
}

