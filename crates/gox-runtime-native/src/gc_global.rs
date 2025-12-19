//! Global GC and runtime state for JIT.
//!
//! Provides thread-local GC and global variable storage that runtime
//! functions can access without explicit pointer parameters.

use std::cell::RefCell;
use gox_runtime_core::gc::{Gc, GcRef, TypeId};

thread_local! {
    static GLOBAL_GC: RefCell<Gc> = RefCell::new(Gc::new());
    static GLOBALS: RefCell<Vec<u64>> = RefCell::new(Vec::new());
    static GLOBALS_IS_REF: RefCell<Vec<bool>> = RefCell::new(Vec::new());
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

/// Initialize globals storage with the given size and type metadata.
pub fn init_globals(size: usize, is_ref: Vec<bool>) {
    GLOBALS.with(|g| {
        let mut globals = g.borrow_mut();
        globals.clear();
        globals.resize(size, 0);
    });
    GLOBALS_IS_REF.with(|r| {
        *r.borrow_mut() = is_ref;
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
// Global variable access functions for JIT
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

/// Get the number of GC objects.
pub fn gc_object_count() -> usize {
    with_gc(|gc| gc.object_count())
}

/// Get total bytes used by GC.
pub fn gc_total_bytes() -> usize {
    with_gc(|gc| gc.total_bytes())
}

/// Force garbage collection.
/// 
/// Note: JIT GC currently only scans globals, not the native stack.
/// Full stack scanning requires Cranelift stack maps (TODO).
pub fn collect_garbage() {
    // Mark roots from globals (only slots marked as GC refs)
    GLOBALS.with(|g| {
        GLOBALS_IS_REF.with(|r| {
            let globals = g.borrow();
            let is_ref = r.borrow();
            debug_assert_eq!(globals.len(), is_ref.len(), "globals and is_ref length mismatch");
            with_gc(|gc| {
                for (&val, &is_gc_ref) in globals.iter().zip(is_ref.iter()) {
                    if is_gc_ref && val != 0 {
                        gc.mark_gray(val as GcRef);
                    }
                }
            });
        });
    });
    
    // Perform collection
    with_gc(|gc| {
        gc.collect(|gc, obj| {
            scan_object(gc, obj);
        });
    });
}

/// Scan a GC object for nested references.
fn scan_object(gc: &mut Gc, obj: GcRef) {
    use gox_common_core::ValueKind;
    
    if obj.is_null() {
        return;
    }
    
    let type_id = unsafe { (*obj).header.type_id };
    
    // Skip user-defined types for now (need TypeTable)
    if type_id >= gox_common_core::FIRST_USER_TYPE_ID {
        return;
    }
    
    let kind = ValueKind::from_u8(type_id as u8);
    match kind {
        ValueKind::String => {
            // String: [array_ref, start, len]
            let array_ref = Gc::read_slot(obj, 0);
            if array_ref != 0 {
                gc.mark_gray(array_ref as GcRef);
            }
        }
        ValueKind::Slice => {
            // Slice: [array_ref, start, len, cap]
            let array_ref = Gc::read_slot(obj, 0);
            if array_ref != 0 {
                gc.mark_gray(array_ref as GcRef);
            }
        }
        ValueKind::Closure => {
            // Closure: [func_id, upvalue_count, upvalues...]
            let upval_count = (Gc::read_slot(obj, 1) as usize).min(256);
            for i in 0..upval_count {
                let upval = Gc::read_slot(obj, 2 + i);
                if upval != 0 {
                    gc.mark_gray(upval as GcRef);
                }
            }
        }
        _ => {}
    }
}

// =============================================================================
// GC wrapper functions for JIT (no GC pointer parameter)
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
// Closure wrapper functions for JIT
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
// Array/Slice wrapper functions for JIT
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

/// Slice a string using the global GC.
#[no_mangle]
pub extern "C" fn gox_rt_string_slice(type_id: TypeId, str_ref: GcRef, start: usize, end: usize) -> GcRef {
    with_gc(|gc| gox_runtime_core::objects::string::slice_of(gc, type_id, str_ref, start, end))
}

/// Append to a slice using the global GC.
#[no_mangle]
pub extern "C" fn gox_rt_slice_append(type_id: TypeId, arr_type_id: TypeId, slice: GcRef, val: u64) -> GcRef {
    with_gc(|gc| gox_runtime_core::objects::slice::append(gc, type_id, arr_type_id, slice, val))
}

#[cfg(test)]
mod tests {
    use super::*;
    use gox_common_core::ValueKind;

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
        let s = gox_rt_string_from_ptr(data.as_ptr(), data.len(), ValueKind::String as u32);
        assert!(!s.is_null());
    }
    
    #[test]
    fn test_gc_collect_reclaims_memory() {
        init_gc();
        
        // Create some strings that will become garbage
        let type_id = ValueKind::String as u32;
        let _s1 = gox_rt_string_from_ptr(b"temp1".as_ptr(), 5, type_id);
        let _s2 = gox_rt_string_from_ptr(b"temp2".as_ptr(), 5, type_id);
        let _s3 = gox_rt_string_from_ptr(b"temp3".as_ptr(), 5, type_id);
        
        let objects_before = gc_object_count();
        let bytes_before = gc_total_bytes();
        
        // Force GC - these strings are not reachable from globals
        collect_garbage();
        
        let objects_after = gc_object_count();
        let bytes_after = gc_total_bytes();
        
        println!("Before GC: {} objects, {} bytes", objects_before, bytes_before);
        println!("After GC:  {} objects, {} bytes", objects_after, bytes_after);
        
        // All strings should be collected since they're not rooted
        assert!(objects_after < objects_before, "GC should reclaim unreachable objects");
        assert!(bytes_after < bytes_before, "GC should free memory");
    }
}
