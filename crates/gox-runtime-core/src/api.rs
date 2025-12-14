//! Runtime API trait definitions.

use std::alloc::Layout;

/// Runtime context trait that all backends implement.
pub trait RuntimeContext {
    /// Allocate memory.
    fn alloc(&mut self, layout: Layout) -> *mut u8;
    
    /// Deallocate memory.
    fn dealloc(&mut self, ptr: *mut u8, layout: Layout);
    
    /// Add a GC root.
    fn gc_root(&mut self, ptr: *mut u8);
    
    /// Remove a GC root.
    fn gc_unroot(&mut self, ptr: *mut u8);
}
