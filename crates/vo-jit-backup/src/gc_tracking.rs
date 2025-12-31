//! GC reference tracking and stack map generation.
//!
//! This module tracks which local variables contain GcRefs and generates
//! stack maps for GC scanning.
//!
//! # Strategy: GcRef Spill to Stack
//!
//! All GcRef variables are spilled to Cranelift stack slots (not kept in
//! registers across safepoints). This simplifies stack map generation:
//! - Stack map is static per function (list of stack slot offsets)
//! - No need to track register liveness at each safepoint
//! - Performance cost: ~10-15% (acceptable for simplicity)

use std::collections::HashSet;

// =============================================================================
// StackMap
// =============================================================================

/// Stack map for GC scanning of JIT frames.
///
/// Records which stack slots contain GcRefs. The GC uses this to find
/// and update GcRefs when scanning JIT stack frames.
#[derive(Debug, Clone, Default)]
pub struct StackMap {
    /// Offsets from frame pointer where GcRefs are stored.
    /// These are slot indices (not byte offsets).
    pub gc_ref_slots: Vec<u16>,
}

impl StackMap {
    pub fn new() -> Self {
        Self {
            gc_ref_slots: Vec::new(),
        }
    }

    /// Check if this stack map has any GcRefs.
    pub fn is_empty(&self) -> bool {
        self.gc_ref_slots.is_empty()
    }

    /// Get the number of GcRef slots.
    pub fn len(&self) -> usize {
        self.gc_ref_slots.len()
    }
}

// =============================================================================
// GcRefTracker
// =============================================================================

/// Tracks GcRef variables during JIT compilation.
///
/// Used to determine which local variables need to be included in the
/// stack map for GC scanning.
pub struct GcRefTracker {
    /// Set of slot indices that contain GcRefs.
    gc_ref_slots: HashSet<u16>,
}

impl GcRefTracker {
    pub fn new() -> Self {
        Self {
            gc_ref_slots: HashSet::new(),
        }
    }

    /// Mark a slot as containing a GcRef.
    ///
    /// Called during variable declaration when the slot type is GcRef.
    pub fn mark_gc_ref(&mut self, slot: u16) {
        self.gc_ref_slots.insert(slot);
    }

    /// Check if a slot contains a GcRef.
    pub fn is_gc_ref(&self, slot: u16) -> bool {
        self.gc_ref_slots.contains(&slot)
    }

    /// Build the final stack map.
    ///
    /// Called after compilation is complete.
    pub fn build_stack_map(self) -> StackMap {
        let mut slots: Vec<u16> = self.gc_ref_slots.into_iter().collect();
        slots.sort_unstable();
        StackMap {
            gc_ref_slots: slots,
        }
    }
}

impl Default for GcRefTracker {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// Future: Per-Safepoint Stack Maps
// =============================================================================

// NOTE: The current implementation uses a single static stack map per function.
// This is conservative - it includes all GcRef slots even if some are not live
// at certain safepoints.
//
// For better precision (and slightly better GC performance), we could track
// liveness per safepoint:
//
// pub struct SafepointStackMap {
//     /// PC offset -> live GcRef slots at that point
//     safepoints: Vec<(u32, Vec<u16>)>,
// }
//
// This would require:
// 1. Track which GcRefs are live at each safepoint
// 2. Use Cranelift's liveness analysis
// 3. Generate separate stack map entries per safepoint
//
// For now, the simpler approach is sufficient.
