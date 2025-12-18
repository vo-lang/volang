//! Global GC instance for AOT/JIT runtime.
//!
//! Provides a thread-local GC that runtime functions can access
//! without needing an explicit GC pointer parameter.

use std::cell::RefCell;
use gox_runtime_core::gc::{Gc, GcRef, TypeId};

thread_local! {
    static GLOBAL_GC: RefCell<Gc> = RefCell::new(Gc::new());
}

/// Initialize or reset the global GC.
pub fn init_gc() {
    GLOBAL_GC.with(|gc| {
        *gc.borrow_mut() = Gc::new();
    });
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
