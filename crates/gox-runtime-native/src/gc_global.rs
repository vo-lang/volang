//! Global GC and runtime state for AOT/JIT.
//!
//! Provides thread-local GC and global variable storage that runtime
//! functions can access without explicit pointer parameters.

use std::cell::RefCell;
use gox_runtime_core::gc::{Gc, GcRef, TypeId};

thread_local! {
    static GLOBAL_GC: RefCell<Gc> = RefCell::new(Gc::new());
    static GLOBALS: RefCell<Vec<u64>> = RefCell::new(Vec::new());
    static FUNC_TABLE: RefCell<Vec<*const u8>> = RefCell::new(Vec::new());
}

// Global pointer to function table for Cranelift symbol access
static mut FUNC_TABLE_PTR: *const *const u8 = std::ptr::null();

/// Initialize or reset the global GC.
pub fn init_gc() {
    GLOBAL_GC.with(|gc| {
        *gc.borrow_mut() = Gc::new();
    });
}

/// Initialize globals storage with the given size.
pub fn init_globals(size: usize) {
    GLOBALS.with(|g| {
        let mut globals = g.borrow_mut();
        globals.clear();
        globals.resize(size, 0);
    });
}

/// Initialize function pointer table with the given size.
pub fn init_func_table(size: usize) {
    FUNC_TABLE.with(|t| {
        let mut table = t.borrow_mut();
        table.clear();
        table.resize(size, std::ptr::null());
        // Update global pointer for Cranelift access
        unsafe {
            FUNC_TABLE_PTR = table.as_ptr();
        }
    });
}

/// Set a function pointer in the table.
pub fn set_func_ptr(func_id: u32, ptr: *const u8) {
    FUNC_TABLE.with(|t| {
        let mut table = t.borrow_mut();
        debug_assert!((func_id as usize) < table.len());
        table[func_id as usize] = ptr;
    });
}

/// Get the function table pointer (for JIT symbol registration).
#[no_mangle]
pub extern "C" fn gox_func_table_ptr() -> *const *const u8 {
    unsafe { FUNC_TABLE_PTR }
}

// =============================================================================
// Global variable access functions for AOT/JIT
// =============================================================================

/// Get a global variable by index.
#[no_mangle]
pub extern "C" fn gox_rt_get_global(idx: usize) -> u64 {
    GLOBALS.with(|g| g.borrow()[idx])
}

/// Set a global variable by index.
#[no_mangle]
pub extern "C" fn gox_rt_set_global(idx: usize, value: u64) {
    GLOBALS.with(|g| g.borrow_mut()[idx] = value);
}

/// Access the global GC for operations.
pub fn with_gc<F, R>(f: F) -> R
where
    F: FnOnce(&mut Gc) -> R,
{
    GLOBAL_GC.with(|gc| f(&mut gc.borrow_mut()))
}

// =============================================================================
// GC wrapper functions for AOT/JIT (no GC pointer parameter)
// =============================================================================

/// Allocate an object using the global GC.
#[no_mangle]
pub extern "C" fn gox_rt_alloc(type_id: TypeId, size_slots: usize) -> GcRef {
    with_gc(|gc| gc.alloc(type_id, size_slots))
}

/// Create a string from raw bytes using the global GC.
#[no_mangle]
pub extern "C" fn gox_rt_string_from_ptr(ptr: *const u8, len: usize, type_id: TypeId) -> GcRef {
    with_gc(|gc| {
        let bytes = unsafe { std::slice::from_raw_parts(ptr, len) };
        gox_runtime_core::objects::string::create(gc, type_id, bytes)
    })
}

/// Concatenate two strings using the global GC.
#[no_mangle]
pub extern "C" fn gox_rt_string_concat(type_id: TypeId, a: GcRef, b: GcRef) -> GcRef {
    with_gc(|gc| gox_runtime_core::objects::string::concat(gc, type_id, a, b))
}

// =============================================================================
// Closure wrapper functions for AOT/JIT
// =============================================================================

/// Create a closure using the global GC.
#[no_mangle]
pub extern "C" fn gox_rt_closure_create(type_id: TypeId, func_id: u32, upvalue_count: usize) -> GcRef {
    with_gc(|gc| gox_runtime_core::objects::closure::create(gc, type_id, func_id, upvalue_count))
}

/// Create an upval box using the global GC.
#[no_mangle]
pub extern "C" fn gox_rt_upval_box_create(type_id: TypeId) -> GcRef {
    with_gc(|gc| gox_runtime_core::objects::closure::create_upval_box(gc, type_id))
}

// =============================================================================
// Array/Slice wrapper functions for AOT/JIT
// =============================================================================

/// Create an array using the global GC.
#[no_mangle]
pub extern "C" fn gox_rt_array_create(type_id: TypeId, elem_type: TypeId, elem_size: usize, len: usize) -> GcRef {
    with_gc(|gc| gox_runtime_core::objects::array::create(gc, type_id, elem_type, elem_size, len))
}

/// Create a slice using the global GC.
#[no_mangle]
pub extern "C" fn gox_rt_slice_create(type_id: TypeId, array: GcRef, start: usize, len: usize, cap: usize) -> GcRef {
    with_gc(|gc| gox_runtime_core::objects::slice::create(gc, type_id, array, start, len, cap))
}

/// Slice a slice using the global GC.
#[no_mangle]
pub extern "C" fn gox_rt_slice_slice(type_id: TypeId, slice: GcRef, start: usize, end: usize) -> GcRef {
    with_gc(|gc| gox_runtime_core::objects::slice::slice_of(gc, type_id, slice, start, end))
}

/// Append to a slice using the global GC.
#[no_mangle]
pub extern "C" fn gox_rt_slice_append(type_id: TypeId, arr_type_id: TypeId, slice: GcRef, val: u64) -> GcRef {
    with_gc(|gc| gox_runtime_core::objects::slice::append(gc, type_id, arr_type_id, slice, val))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_global_gc_alloc() {
        init_gc();
        let obj = gox_rt_alloc(1, 2);
        assert!(!obj.is_null());
    }

    #[test]
    fn test_global_gc_string() {
        init_gc();
        let data = b"hello";
        let s = gox_rt_string_from_ptr(data.as_ptr(), data.len(), 1);
        assert!(!s.is_null());
    }
}
