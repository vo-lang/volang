//! GC types and C ABI functions.
//!
//! This module provides the core GC types that are shared across all backends.
//! The actual GC implementation lives in gox-vm, but these types define the
//! stable ABI for Cranelift-generated code to call.

use alloc::alloc::{alloc, dealloc, Layout};
use alloc::vec::Vec;

/// GC color for tri-color marking.
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GcColor {
    White = 0,
    Gray = 1,
    Black = 2,
}

/// GC generation for generational collection.
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GcGen {
    Young = 0,
    Old = 1,
    Touched = 2,
}

/// GC state machine for incremental collection.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GcState {
    Pause,
    Propagate,
    Atomic,
    Sweep,
}

/// Object header (8 bytes).
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct GcHeader {
    pub mark: u8,
    pub gen: u8,
    pub flags: u8,
    pub _pad: u8,
    pub type_id: u32,
}

impl GcHeader {
    pub fn new(type_id: u32) -> Self {
        Self {
            mark: GcColor::White as u8,
            gen: GcGen::Young as u8,
            flags: 0,
            _pad: 0,
            type_id,
        }
    }
}

/// A GC-managed object.
#[repr(C)]
pub struct GcObject {
    pub header: GcHeader,
    // Variable-length data follows (accessed via pointer arithmetic)
}

/// Pointer to a GC object.
/// 
/// GcRef is a raw pointer that is safe to send between threads because:
/// 1. The pointer itself is just a number (address)
/// 2. Actual access to GC objects must go through proper synchronization
/// 3. The GC system ensures objects aren't freed while referenced
pub type GcRef = *mut GcObject;

/// Null GC reference.
pub const NULL_REF: GcRef = core::ptr::null_mut();

// SAFETY: GcRef is just a pointer value. Thread safety is ensured by:
// - GC operations are synchronized at a higher level
// - Object access goes through Gc methods that handle synchronization
unsafe impl Send for GcObject {}
unsafe impl Sync for GcObject {}

/// Type ID alias.
pub type TypeId = u32;

/// Check if a u64 value is potentially a GC reference.
#[inline]
pub fn is_gc_ref(val: u64) -> bool {
    val != 0
}

/// Object entry in the GC tracking list.
struct ObjectEntry {
    ptr: GcRef,
    size_bytes: usize,
}

/// The garbage collector.
pub struct Gc {
    all_objects: Vec<ObjectEntry>,
    gray_queue: Vec<GcRef>,
    #[allow(dead_code)]
    state: GcState,
    current_white: u8,
    
    // Generational (reserved for Phase 4)
    #[allow(dead_code)]
    young_list: Vec<GcRef>,
    #[allow(dead_code)]
    old_list: Vec<GcRef>,
    
    // Statistics
    total_bytes: usize,
    threshold: usize,
    
    // Parameters
    pause: usize,
    #[allow(dead_code)]
    stepmul: usize,
    
    // Pause control
    pause_count: u32,
}

impl Gc {
    pub fn new() -> Self {
        Self {
            all_objects: Vec::new(),
            gray_queue: Vec::new(),
            state: GcState::Pause,
            current_white: 0,
            young_list: Vec::new(),
            old_list: Vec::new(),
            total_bytes: 0,
            threshold: 1024 * 1024, // 1MB
            pause: 200,
            stepmul: 200,
            pause_count: 0,
        }
    }
    
    /// Allocate a GC object with given number of data slots.
    pub fn alloc(&mut self, type_id: TypeId, size_slots: usize) -> GcRef {
        let size_bytes = core::mem::size_of::<GcHeader>() + size_slots * 8;
        let layout = Layout::from_size_align(size_bytes, 8).unwrap();
        
        let ptr = unsafe {
            let ptr = alloc(layout) as *mut GcObject;
            if ptr.is_null() {
                panic!("GC allocation failed");
            }
            (*ptr).header = GcHeader::new(type_id);
            // Zero-initialize data slots
            let data_ptr = Self::get_data_ptr(ptr);
            for i in 0..size_slots {
                *data_ptr.add(i) = 0;
            }
            ptr
        };
        
        self.all_objects.push(ObjectEntry { ptr, size_bytes });
        self.total_bytes += size_bytes;
        
        ptr
    }
    
    /// Get data slot pointer for an object.
    #[inline]
    pub fn get_data_ptr(obj: GcRef) -> *mut u64 {
        unsafe {
            (obj as *mut u8).add(core::mem::size_of::<GcHeader>()) as *mut u64
        }
    }
    
    /// Read a data slot.
    #[inline]
    pub fn read_slot(obj: GcRef, idx: usize) -> u64 {
        unsafe { *Self::get_data_ptr(obj).add(idx) }
    }
    
    /// Write a data slot.
    #[inline]
    pub fn write_slot(obj: GcRef, idx: usize, val: u64) {
        unsafe { *Self::get_data_ptr(obj).add(idx) = val; }
    }
    
    /// Pause GC (for native calls).
    pub fn pause_gc(&mut self) {
        self.pause_count += 1;
    }
    
    /// Resume GC.
    pub fn resume_gc(&mut self) {
        debug_assert!(self.pause_count > 0);
        self.pause_count -= 1;
    }
    
    /// Check if GC is paused.
    pub fn is_paused(&self) -> bool {
        self.pause_count > 0
    }
    
    /// Mark an object as gray.
    pub fn mark_gray(&mut self, obj: GcRef) {
        if obj.is_null() {
            return;
        }
        unsafe {
            if (*obj).header.mark == self.current_white {
                (*obj).header.mark = GcColor::Gray as u8;
                self.gray_queue.push(obj);
            }
        }
    }
    
    /// Check if GC should run based on memory threshold.
    pub fn should_collect(&self) -> bool {
        self.total_bytes > self.threshold && self.pause_count == 0
    }
    
    /// Force full collection (Phase 1: stop-the-world).
    pub fn collect<F>(&mut self, mut scan_fn: F)
    where
        F: FnMut(&mut Self, GcRef),
    {
        if self.pause_count > 0 {
            return;
        }
        
        // Propagate: scan all gray objects
        while let Some(obj) = self.gray_queue.pop() {
            scan_fn(self, obj);
            unsafe {
                (*obj).header.mark = GcColor::Black as u8;
            }
        }
        
        // Sweep: free white objects
        let white = self.current_white;
        let mut new_objects = Vec::new();
        
        for entry in self.all_objects.drain(..) {
            unsafe {
                if (*entry.ptr).header.mark == white {
                    // Garbage - free it
                    let layout = Layout::from_size_align(entry.size_bytes, 8).unwrap();
                    dealloc(entry.ptr as *mut u8, layout);
                    self.total_bytes -= entry.size_bytes;
                } else {
                    // Alive - reset to white for next GC
                    (*entry.ptr).header.mark = white;
                    new_objects.push(entry);
                }
            }
        }
        
        self.all_objects = new_objects;
        
        // Adjust threshold
        self.threshold = (self.total_bytes * self.pause / 100).max(1024 * 1024);
    }
    
    /// Write barrier (Phase 3+, currently no-op).
    #[inline]
    pub fn write_barrier(&mut self, _parent: GcRef, _child: GcRef) {
        // Phase 1: no-op
    }
    
    /// Get total allocated bytes.
    pub fn total_bytes(&self) -> usize {
        self.total_bytes
    }
    
    /// Get number of live objects.
    pub fn object_count(&self) -> usize {
        self.all_objects.len()
    }
}

impl Default for Gc {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Gc {
    fn drop(&mut self) {
        // Free all objects
        for entry in self.all_objects.drain(..) {
            unsafe {
                let layout = Layout::from_size_align(entry.size_bytes, 8).unwrap();
                dealloc(entry.ptr as *mut u8, layout);
            }
        }
    }
}

// =============================================================================
// C ABI Functions - Called by Cranelift-generated code
// =============================================================================

/// Allocate a GC object. Called by Cranelift-generated code.
///
/// # Safety
/// `gc` must be a valid pointer to a Gc instance.
#[no_mangle]
pub unsafe extern "C" fn gox_gc_alloc(gc: *mut Gc, type_id: TypeId, size_slots: usize) -> GcRef {
    (*gc).alloc(type_id, size_slots)
}

/// Read a slot from a GC object.
///
/// # Safety
/// `obj` must be a valid GcRef.
#[no_mangle]
pub unsafe extern "C" fn gox_gc_read_slot(obj: GcRef, idx: usize) -> u64 {
    Gc::read_slot(obj, idx)
}

/// Write a slot to a GC object.
///
/// # Safety
/// `obj` must be a valid GcRef.
#[no_mangle]
pub unsafe extern "C" fn gox_gc_write_slot(obj: GcRef, idx: usize, val: u64) {
    Gc::write_slot(obj, idx, val)
}

/// Write barrier for GC.
///
/// # Safety
/// Both pointers must be valid.
#[no_mangle]
pub unsafe extern "C" fn gox_gc_write_barrier(gc: *mut Gc, parent: GcRef, child: GcRef) {
    (*gc).write_barrier(parent, child)
}

/// Mark object as gray (for GC roots).
///
/// # Safety
/// Both pointers must be valid.
#[no_mangle]
pub unsafe extern "C" fn gox_gc_mark_gray(gc: *mut Gc, obj: GcRef) {
    (*gc).mark_gray(obj)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_alloc() {
        let mut gc = Gc::new();
        let obj = gc.alloc(1, 2);
        assert!(!obj.is_null());
        assert_eq!(gc.object_count(), 1);
        
        // Write and read slots
        Gc::write_slot(obj, 0, 42);
        Gc::write_slot(obj, 1, 100);
        assert_eq!(Gc::read_slot(obj, 0), 42);
        assert_eq!(Gc::read_slot(obj, 1), 100);
    }
    
    #[test]
    fn test_c_abi() {
        let mut gc = Gc::new();
        unsafe {
            let obj = gox_gc_alloc(&mut gc, 1, 2);
            assert!(!obj.is_null());
            
            gox_gc_write_slot(obj, 0, 42);
            assert_eq!(gox_gc_read_slot(obj, 0), 42);
        }
    }
}
