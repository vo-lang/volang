//! FFI exports for Cranelift and external code.
//!
//! This module re-exports all `extern "C"` functions that can be called
//! by Cranelift-generated code or external native code.
//!
//! # Usage from Cranelift
//!
//! ```ignore
//! // Import runtime function
//! let func_ref = module.declare_function("gox_gc_alloc", ...);
//! // Call it
//! builder.ins().call(func_ref, &[gc_ptr, type_id, slots]);
//! ```

// Re-export core types
pub use crate::gc::{Gc, GcRef, GcHeader, TypeId, NULL_REF};

// =============================================================================
// GC C ABI (5 functions)
// =============================================================================
pub use crate::gc::{
    gox_gc_alloc,
    gox_gc_read_slot,
    gox_gc_write_slot,
    gox_gc_write_barrier,
    gox_gc_mark_gray,
};

// =============================================================================
// String C ABI (6 functions)
// =============================================================================
pub use crate::objects::{
    gox_string_len,
    gox_string_index,
    gox_string_concat,
    gox_string_eq,
    gox_string_ne,
};

/// Create a string from a raw pointer and length.
/// Used by AOT/JIT for string constants stored in data sections.
#[no_mangle]
pub unsafe extern "C" fn gox_string_from_ptr(
    gc: *mut Gc,
    ptr: *const u8,
    len: usize,
    type_id: TypeId,
) -> GcRef {
    let bytes = core::slice::from_raw_parts(ptr, len);
    crate::objects::string::create(&mut *gc, type_id, bytes)
}

// =============================================================================
// Array C ABI (4 functions)
// =============================================================================
pub use crate::objects::{
    gox_array_create,
    gox_array_len,
    gox_array_get,
    gox_array_set,
};

// =============================================================================
// Slice C ABI (7 functions)
// =============================================================================
pub use crate::objects::{
    gox_slice_create,
    gox_slice_len,
    gox_slice_cap,
    gox_slice_get,
    gox_slice_set,
    gox_slice_append,
    gox_slice_slice,
};

// =============================================================================
// Struct Hash C ABI (1 function)
// =============================================================================
pub use crate::objects::gox_struct_hash;

// =============================================================================
// Map C ABI (6 functions, requires std feature)
// =============================================================================
#[cfg(feature = "std")]
pub use crate::objects::{
    gox_map_create,
    gox_map_len,
    gox_map_get,
    gox_map_set,
    gox_map_delete,
    gox_map_contains,
};

// =============================================================================
// Closure C ABI (8 functions)
// =============================================================================
pub use crate::objects::{
    gox_closure_create,
    gox_closure_func_id,
    gox_closure_upvalue_count,
    gox_closure_get_upvalue,
    gox_closure_set_upvalue,
    gox_upval_box_create,
    gox_upval_box_get,
    gox_upval_box_set,
};

// =============================================================================
// Interface C ABI (3 functions)
// =============================================================================
pub use crate::objects::{
    gox_interface_unbox_type,
    gox_interface_unbox_data,
    gox_interface_is_nil,
};

// =============================================================================
// Stdlib: Builtin C ABI (2 functions)
// =============================================================================
#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_builtin_len(type_tag: u8, ptr: GcRef) -> usize {
    crate::builtins::builtin::len_impl(type_tag, ptr)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_builtin_cap(type_tag: u8, ptr: GcRef) -> usize {
    crate::builtins::builtin::cap_impl(type_tag, ptr)
}

// =============================================================================
// Stdlib: Strings C ABI (native functions only)
// Note: GoX-implemented functions (HasPrefix, HasSuffix, Contains, Repeat,
//       Compare, ReplaceAll) are compiled directly by Cranelift
// =============================================================================
#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_strings_index(s: GcRef, substr: GcRef) -> i64 {
    use crate::objects::string;
    crate::builtins::strings::index(string::as_str(s), string::as_str(substr))
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_strings_count(s: GcRef, substr: GcRef) -> usize {
    use crate::objects::string;
    crate::builtins::strings::count(string::as_str(s), string::as_str(substr)) as usize
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_strings_to_lower(gc: *mut Gc, s: GcRef, type_id: TypeId) -> GcRef {
    use crate::objects::string;
    let result = crate::builtins::strings::to_lower(string::as_str(s));
    string::from_rust_str(&mut *gc, type_id, &result)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_strings_to_upper(gc: *mut Gc, s: GcRef, type_id: TypeId) -> GcRef {
    use crate::objects::string;
    let result = crate::builtins::strings::to_upper(string::as_str(s));
    string::from_rust_str(&mut *gc, type_id, &result)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_strings_equal_fold(s: GcRef, t: GcRef) -> bool {
    use crate::objects::string;
    crate::builtins::strings::equal_fold(string::as_str(s), string::as_str(t))
}

// =============================================================================
// Stdlib: Fmt C ABI (2 functions)
// =============================================================================
#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_fmt_format_value(
    gc: *mut Gc, val: u64, type_tag: u8, type_id: TypeId
) -> GcRef {
    use crate::objects::string;
    let result = crate::builtins::fmt::format_value(val, type_tag);
    string::from_rust_str(&mut *gc, type_id, &result)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_fmt_println(args: *const u64, tags: *const u8, argc: usize) -> usize {
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
//            Note: GoX-implemented: contains, has_prefix, has_suffix, repeat, compare, replace_all
// Fmt:       2 (format_value, println) [std only]
//
// Note: Goroutine functions (4) are in gox-runtime-native, not here.
//
