//! Stack map support for native stack GC scanning.
//!
//! Stack maps record which stack slots contain GC references at each safepoint
//! (call instruction). During GC, we walk the native call stack and use these
//! maps to find all live GC references.
//!
//! ## Architecture
//!
//! ### JIT (Phase 1 - Implemented)
//! - Cranelift's `declare_var_needs_stack_map()` marks GC ref variables
//! - `cranelift-frontend` auto-inserts spills/reloads around safepoints
//! - After compilation, we extract `user_stack_maps()` from compiled code
//! - Register maps with `register_stack_map(return_addr, entry)`
//! - During GC, `scan_native_stack()` walks frames and scans GC refs
//!
//! ### AOT (Phase 2 - TODO)
//! - Same compilation-time marking
//! - Stack maps serialized to `.vo_stackmap` section in .o file
//! - At load time, read section and call `register_stack_maps_batch()`
//! - Same runtime scanning

use vo_runtime_core::gc::{Gc, GcRef};
use once_cell::sync::Lazy;
use parking_lot::RwLock;
use std::collections::HashMap;

/// A stack map entry describing GC reference locations at a safepoint.
///
/// Each entry corresponds to a single return address (safepoint).
/// The offsets are relative to the stack pointer at that point.
#[derive(Debug, Clone, Default)]
pub struct StackMapEntry {
    /// Stack pointer offsets where GC references are stored.
    /// Positive = above SP, negative = below SP (architecture dependent).
    pub sp_offsets: Vec<i32>,
}

impl StackMapEntry {
    /// Create a new stack map entry with the given offsets.
    pub fn new(sp_offsets: Vec<i32>) -> Self {
        Self { sp_offsets }
    }

    /// Check if this entry has any GC references.
    pub fn is_empty(&self) -> bool {
        self.sp_offsets.is_empty()
    }
}

// =============================================================================
// Global Stack Map Registry
// =============================================================================

/// Global stack map table: return_address -> StackMapEntry
static STACK_MAPS: Lazy<RwLock<HashMap<usize, StackMapEntry>>> = 
    Lazy::new(|| RwLock::new(HashMap::new()));

/// Register a stack map for a single return address.
///
/// Called by JIT after compiling each function.
///
/// # Arguments
/// * `return_addr` - The return address (instruction pointer after call)
/// * `entry` - The stack map entry describing GC ref locations
pub fn register_stack_map(return_addr: usize, entry: StackMapEntry) {
    if entry.is_empty() {
        return; // No GC refs at this safepoint
    }
    STACK_MAPS.write().insert(return_addr, entry);
}

/// Register multiple stack maps with a base address.
///
/// Called by AOT loader after loading a module.
///
/// # Arguments
/// * `base_addr` - Base address of the loaded code
/// * `maps` - List of (offset_from_base, entry) pairs
pub fn register_stack_maps_batch(base_addr: usize, maps: &[(u32, StackMapEntry)]) {
    let mut table = STACK_MAPS.write();
    for (offset, entry) in maps {
        if !entry.is_empty() {
            table.insert(base_addr + *offset as usize, entry.clone());
        }
    }
}

/// Look up a stack map by return address.
pub fn lookup_stack_map(return_addr: usize) -> Option<StackMapEntry> {
    STACK_MAPS.read().get(&return_addr).cloned()
}

/// Clear all registered stack maps.
///
/// Called when unloading a module or resetting the runtime.
pub fn clear_stack_maps() {
    STACK_MAPS.write().clear();
}

/// Get the number of registered stack maps (for debugging/testing).
pub fn stack_map_count() -> usize {
    STACK_MAPS.read().len()
}

// =============================================================================
// Native Stack Scanning
// =============================================================================

/// Scan the native call stack and mark all GC references.
///
/// This function walks up the call stack, looks up stack maps for each
/// return address, and marks any GC references found on the stack.
///
/// # How it works
///
/// 1. Uses `backtrace::trace` to walk the call stack
/// 2. For each frame, checks if we have a stack map registered for that IP
/// 3. If found, uses the frame's stack pointer to read GC refs at known offsets
/// 4. Marks each non-null GC reference as gray for the tri-color GC
///
/// # Limitations
///
/// - The `backtrace` crate provides IP but SP access is platform-specific
/// - Currently uses frame pointer chain on supported platforms
/// - Frames without stack maps (runtime functions) are skipped
pub fn scan_native_stack(gc: &mut Gc) {
    // Track how many GC refs we found (for debugging)
    let mut refs_found = 0;
    
    // Walk the call stack
    backtrace::trace(|frame| {
        let ip = frame.ip() as usize;
        
        // Look up stack map for this return address
        if let Some(entry) = lookup_stack_map(ip) {
            // Get stack pointer for this frame
            // On x86_64/aarch64, frame.sp() gives us the stack pointer
            let sp = frame.sp() as usize;
            
            if sp != 0 {
                // Scan each GC ref slot
                for &offset in &entry.sp_offsets {
                    let slot_addr = if offset >= 0 {
                        sp.wrapping_add(offset as usize)
                    } else {
                        sp.wrapping_sub((-offset) as usize)
                    };
                    
                    // Safety: We trust the stack map offsets from Cranelift
                    let gc_ref = unsafe { *(slot_addr as *const u64) };
                    
                    if gc_ref != 0 {
                        gc.mark_gray(gc_ref as GcRef);
                        refs_found += 1;
                    }
                }
            }
        }
        
        true // Continue walking
    });
    
    // Debug: log if we found any refs
    if refs_found > 0 {
        // Could add logging here if needed
        let _ = refs_found;
    }
}

// =============================================================================
// AOT Support (Phase 2 - TODO)
// =============================================================================

/// Serialized stack map format for AOT object files.
///
/// This will be written to a `.vo_stackmap` section in the .o file.
/// Format:
/// ```text
/// magic: u32 = 0x474F5853 ("VOS")
/// version: u32 = 1
/// num_functions: u32
/// [
///   func_offset: u32        // offset from code section start
///   num_entries: u32
///   [
///     entry_offset: u32     // offset within function (return addr)
///     num_slots: u16
///     slots: [i16; num_slots]
///   ] * num_entries
/// ] * num_functions
/// ```
pub mod aot {
    use super::StackMapEntry;

    /// Magic number for stack map section: "VOS"
    pub const MAGIC: u32 = 0x474F5853;
    /// Current format version
    pub const VERSION: u32 = 1;

    /// Serialize stack maps to bytes for embedding in object file.
    ///
    /// # Arguments
    /// * `func_maps` - List of (func_offset, [(entry_offset, entry)]) per function
    ///
    /// # Returns
    /// Serialized bytes to write to `.vo_stackmap` section
    ///
    /// TODO: Implement when AOT support is needed
    pub fn serialize_stack_maps(
        _func_maps: &[(u32, Vec<(u32, StackMapEntry)>)],
    ) -> Vec<u8> {
        // Phase 2: Implement serialization
        // For now, return empty
        Vec::new()
    }

    /// Deserialize stack maps from bytes read from object file.
    ///
    /// # Arguments
    /// * `data` - Raw bytes from `.vo_stackmap` section
    ///
    /// # Returns
    /// List of (func_offset, [(entry_offset, entry)]) per function
    ///
    /// TODO: Implement when AOT support is needed
    pub fn deserialize_stack_maps(
        _data: &[u8],
    ) -> Result<Vec<(u32, Vec<(u32, StackMapEntry)>)>, &'static str> {
        // Phase 2: Implement deserialization
        // For now, return empty
        Ok(Vec::new())
    }

    /// Get the section name for stack maps in object files.
    pub const fn section_name() -> &'static str {
        ".vo_stackmap"
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_register_and_lookup() {
        clear_stack_maps();
        
        let entry = StackMapEntry::new(vec![8, 16, 24]);
        register_stack_map(0x1000, entry.clone());
        
        let found = lookup_stack_map(0x1000);
        assert!(found.is_some());
        assert_eq!(found.unwrap().sp_offsets, vec![8, 16, 24]);
        
        assert!(lookup_stack_map(0x2000).is_none());
        
        clear_stack_maps();
        assert_eq!(stack_map_count(), 0);
    }

    #[test]
    fn test_empty_entry_not_registered() {
        // Use unique address to avoid test parallelism issues
        let addr = 0x2000;
        
        let empty = StackMapEntry::new(vec![]);
        register_stack_map(addr, empty);
        
        // Empty entry should not be registered
        assert!(lookup_stack_map(addr).is_none());
    }

    #[test]
    fn test_batch_register() {
        // Use unique base address to avoid test parallelism issues
        let base = 0x30000;
        
        let maps = vec![
            (0x100, StackMapEntry::new(vec![8])),
            (0x200, StackMapEntry::new(vec![16, 24])),
            (0x300, StackMapEntry::new(vec![])), // Empty, should not register
        ];
        
        register_stack_maps_batch(base, &maps);
        
        assert!(lookup_stack_map(base + 0x100).is_some());
        assert!(lookup_stack_map(base + 0x200).is_some());
        assert!(lookup_stack_map(base + 0x300).is_none()); // Empty not registered
    }
}
