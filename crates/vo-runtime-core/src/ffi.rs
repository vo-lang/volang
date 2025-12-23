//! FFI exports for Cranelift and external code.
//!
//! This module re-exports all `extern "C"` functions that can be called
//! by Cranelift-generated code or external native code.
//!
//! # Usage from Cranelift
//!
//! ```ignore
//! // Import runtime function
//! let func_ref = module.declare_function("vo_gc_alloc", ...);
//! // Call it
//! builder.ins().call(func_ref, &[gc_ptr, type_id, slots]);
//! ```

// Re-export core types
pub use crate::gc::{Gc, GcRef, GcHeader, NULL_REF};

// =============================================================================
// GC C ABI - All moved to vo-runtime-native/src/gc_global.rs
// =============================================================================
// JIT/AOT uses vo_gc_read_slot, vo_gc_write_slot from gc_global.rs
// VM uses Gc::read_slot, Gc::write_slot directly

// =============================================================================
// String C ABI (6 functions)
// =============================================================================
pub use crate::objects::{
    vo_string_len,
    vo_string_index,
    vo_string_concat,
    vo_string_eq,
    vo_string_ne,
    vo_string_lt,
    vo_string_le,
    vo_string_gt,
    vo_string_ge,
};

/// Create a string from a raw pointer and length.
/// Used by AOT/JIT for string constants stored in data sections.
#[no_mangle]
pub unsafe extern "C" fn vo_string_from_ptr(
    gc: *mut Gc,
    ptr: *const u8,
    len: usize,
) -> GcRef {
    let bytes = core::slice::from_raw_parts(ptr, len);
    crate::objects::string::create(&mut *gc, bytes)
}

// =============================================================================
// Array C ABI (4 functions)
// =============================================================================
pub use crate::objects::{
    vo_array_create,
    vo_array_len,
    vo_array_get,
    vo_array_set,
};

// =============================================================================
// Slice C ABI (7 functions)
// =============================================================================
pub use crate::objects::{
    vo_slice_create,
    vo_slice_len,
    vo_slice_cap,
    vo_slice_get,
    vo_slice_set,
    vo_slice_append,
    vo_slice_slice,
};

// =============================================================================
// Struct Hash C ABI (1 function)
// =============================================================================
pub use crate::objects::vo_struct_hash;

// =============================================================================
// Map C ABI (6 functions, requires std feature)
// =============================================================================
#[cfg(feature = "std")]
pub use crate::objects::{
    vo_map_create,
    vo_map_len,
    vo_map_get,
    vo_map_set,
    vo_map_delete,
    vo_map_contains,
};

// =============================================================================
// Closure C ABI (8 functions)
// =============================================================================
pub use crate::objects::{
    vo_closure_create,
    vo_closure_func_id,
    vo_closure_upvalue_count,
    vo_closure_get_upvalue,
    vo_closure_set_upvalue,
    vo_upval_box_create,
    vo_upval_box_get,
    vo_upval_box_set,
};

// =============================================================================
// Interface C ABI (3 functions)
// =============================================================================
pub use crate::objects::{
    vo_interface_unbox_value_kind,
    vo_interface_unbox_value_type_id,
    vo_interface_unbox_data,
    vo_interface_is_nil,
};

// =============================================================================
// Stdlib: Builtin C ABI (2 functions)
// =============================================================================
#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_builtin_len(type_tag: u8, ptr: GcRef) -> usize {
    crate::builtins::builtin::len_impl(type_tag, ptr)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_builtin_cap(type_tag: u8, ptr: GcRef) -> usize {
    crate::builtins::builtin::cap_impl(type_tag, ptr)
}

// =============================================================================
// Stdlib: Strings C ABI (native functions only)
// Note: Vo-implemented functions (HasPrefix, HasSuffix, Contains, Repeat,
//       Compare, ReplaceAll) are compiled directly by Cranelift
// =============================================================================
#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_strings_index(s: GcRef, substr: GcRef) -> i64 {
    use crate::objects::string;
    crate::builtins::strings::index(string::as_str(s), string::as_str(substr))
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_strings_count(s: GcRef, substr: GcRef) -> usize {
    use crate::objects::string;
    crate::builtins::strings::count(string::as_str(s), string::as_str(substr)) as usize
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_strings_to_lower(gc: *mut Gc, s: GcRef) -> GcRef {
    use crate::objects::string;
    let result = crate::builtins::strings::to_lower(string::as_str(s));
    string::from_rust_str(&mut *gc, &result)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_strings_to_upper(gc: *mut Gc, s: GcRef) -> GcRef {
    use crate::objects::string;
    let result = crate::builtins::strings::to_upper(string::as_str(s));
    string::from_rust_str(&mut *gc, &result)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_strings_equal_fold(s: GcRef, t: GcRef) -> bool {
    use crate::objects::string;
    crate::builtins::strings::equal_fold(string::as_str(s), string::as_str(t))
}

// =============================================================================
// Stdlib: Fmt C ABI (2 functions)
// =============================================================================
#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_fmt_format_value(
    gc: *mut Gc, val: u64, type_tag: u8
) -> GcRef {
    use crate::objects::string;
    let result = crate::builtins::fmt::format_value(val, type_tag);
    string::from_rust_str(&mut *gc, &result)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_fmt_println(args: *const u64, tags: *const u8, argc: usize) -> usize {
    use alloc::vec::Vec;
    let args_slice = core::slice::from_raw_parts(args, argc);
    let tags_slice = core::slice::from_raw_parts(tags, argc);
    let pairs: Vec<(u64, u8)> = args_slice.iter().zip(tags_slice.iter())
        .map(|(&v, &t)| (v, t)).collect();
    let output = crate::builtins::fmt::format_args(&pairs);
    #[cfg(feature = "std")]
    println!("{}", output);
    output.len() + 1
}

// =============================================================================
// Summary: Total 51 C ABI functions (in runtime-core)
// =============================================================================
//
// GC:        5 (alloc, read_slot, write_slot, write_barrier, mark_gray)
// String:    5 (len, index, concat, eq, ne)
// Array:     4 (create, len, get, set)
// Slice:     7 (create, len, cap, get, set, append, slice)
// Struct:    1 (hash)
// Map:       6 (create, len, get, set, delete, contains) [std only]
// Closure:   8 (create, func_id, upvalue_count, get/set_upvalue, upval_box_*)
// Interface: 3 (unbox_type, unbox_data, is_nil)
// Builtin:   2 (len, cap) [std only]
// Strings:   5 (index, count, to_lower, to_upper, equal_fold) [std only]
//            Note: Vo-implemented: contains, has_prefix, has_suffix, repeat, compare, replace_all
// Fmt:       2 (format_value, println) [std only]
//
// Note: Goroutine functions (4) are in vo-runtime-native, not here.
//
