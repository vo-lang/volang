//! Hot function detection for JIT compilation.
//!
//! This module tracks function call counts and loop back-edge counts to
//! determine when functions should be JIT compiled.
//!
//! # Detection Strategy
//!
//! - **Call count**: Tracked in VM's `exec_call`. When a function is called
//!   100 times, it's considered hot and queued for JIT compilation.
//!
//! - **Loop back-edge count**: Tracked in VM's `exec_jump` when the target
//!   is before the current PC. When a function has 1000 back-edge executions
//!   (and hasn't hit the call threshold), it's queued for JIT.
//!
//! # Thresholds
//!
//! | Trigger | Threshold | Rationale |
//! |---------|-----------|-----------|
//! | Call count | 100 | Balance between startup time and optimization |
//! | Back-edge count | 1000 | Catch hot loops in rarely-called functions |

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(not(feature = "std"))]
use hashbrown::HashMap;

// =============================================================================
// Constants
// =============================================================================

/// Default number of calls before a function is considered hot.
pub const DEFAULT_CALL_THRESHOLD: u32 = 100;

/// Default number of loop back-edge executions before a function is considered hot.
/// Only triggers if call count hasn't reached CALL_THRESHOLD.
pub const DEFAULT_LOOP_THRESHOLD: u32 = 1000;

// =============================================================================
// HotCounter
// =============================================================================

/// Tracks function hotness for JIT compilation decisions.
///
/// Maintains per-function counters for calls and loop back-edges.
/// When a function exceeds the threshold, it should be compiled to native code.
///
/// Use `with_thresholds()` to configure custom thresholds (e.g., for testing).
pub struct HotCounter {
    /// Call count per function: func_id -> count
    call_counts: HashMap<u32, u32>,
    /// Loop back-edge count per function: func_id -> count
    loop_counts: HashMap<u32, u32>,
    /// Functions that have been marked for compilation.
    /// Prevents duplicate compilation requests.
    marked_for_jit: HashMap<u32, bool>,
    /// Call threshold
    call_threshold: u32,
    /// Loop threshold
    loop_threshold: u32,
}

impl HotCounter {
    /// Create a HotCounter with default thresholds.
    pub fn new() -> Self {
        Self::with_thresholds(DEFAULT_CALL_THRESHOLD, DEFAULT_LOOP_THRESHOLD)
    }
    
    /// Create a HotCounter with custom thresholds.
    pub fn with_thresholds(call_threshold: u32, loop_threshold: u32) -> Self {
        Self {
            call_counts: HashMap::new(),
            loop_counts: HashMap::new(),
            marked_for_jit: HashMap::new(),
            call_threshold,
            loop_threshold,
        }
    }

    /// Record a function call.
    ///
    /// Returns `true` if this function should be JIT compiled (just hit threshold).
    /// Returns `false` if already compiled or below threshold.
    ///
    /// Called from VM's exec_call.
    pub fn record_call(&mut self, func_id: u32) -> bool {
        // Skip if already marked
        if self.marked_for_jit.get(&func_id).copied().unwrap_or(false) {
            return false;
        }

        let count = self.call_counts.entry(func_id).or_insert(0);
        *count += 1;

        if *count == self.call_threshold {
            self.marked_for_jit.insert(func_id, true);
            return true;
        }

        false
    }

    /// Record a loop back-edge execution.
    ///
    /// Returns `true` if this function should be OSR compiled.
    /// OSR triggers when a hot loop is detected, regardless of call count.
    ///
    /// Called from VM's Jump/JumpIf/JumpIfNot when target < current_pc.
    pub fn record_back_edge(&mut self, func_id: u32) -> bool {
        // Skip if already marked (prevents duplicate OSR compilation)
        if self.marked_for_jit.get(&func_id).copied().unwrap_or(false) {
            return false;
        }

        let count = self.loop_counts.entry(func_id).or_insert(0);
        *count += 1;

        if *count == self.loop_threshold {
            self.marked_for_jit.insert(func_id, true);
            return true;
        }

        false
    }

    /// Check if a function has been marked for JIT compilation.
    pub fn is_marked(&self, func_id: u32) -> bool {
        self.marked_for_jit.get(&func_id).copied().unwrap_or(false)
    }

    /// Mark a function as compiled (or failed to compile).
    ///
    /// Called after JIT compilation attempt (success or failure) to prevent
    /// repeated compilation attempts.
    pub fn mark_compiled(&mut self, func_id: u32) {
        self.marked_for_jit.insert(func_id, true);
    }

    /// Get the call count for a function.
    pub fn get_call_count(&self, func_id: u32) -> u32 {
        self.call_counts.get(&func_id).copied().unwrap_or(0)
    }

    /// Get the loop back-edge count for a function.
    pub fn get_loop_count(&self, func_id: u32) -> u32 {
        self.loop_counts.get(&func_id).copied().unwrap_or(0)
    }

    /// Reset all counters (for testing).
    #[cfg(test)]
    pub fn reset(&mut self) {
        self.call_counts.clear();
        self.loop_counts.clear();
        self.marked_for_jit.clear();
    }
}

impl Default for HotCounter {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_call_threshold() {
        let mut counter = HotCounter::new();
        
        // 99 calls - not hot yet
        for _ in 0..99 {
            assert!(!counter.record_call(1));
        }
        
        // 100th call - now hot
        assert!(counter.record_call(1));
        
        // Further calls don't trigger again
        assert!(!counter.record_call(1));
        assert!(!counter.record_call(1));
    }

    #[test]
    fn test_loop_threshold() {
        let mut counter = HotCounter::new();
        
        // 999 back-edges - not hot yet
        for _ in 0..999 {
            assert!(!counter.record_back_edge(1));
        }
        
        // 1000th back-edge - now hot
        assert!(counter.record_back_edge(1));
        
        // Further back-edges don't trigger again
        assert!(!counter.record_back_edge(1));
    }

    #[test]
    fn test_call_threshold_suppresses_loop() {
        let mut counter = HotCounter::new();
        
        // Hit call threshold first
        for _ in 0..100 {
            counter.record_call(1);
        }
        
        // Loop threshold won't trigger for this function
        for _ in 0..1000 {
            assert!(!counter.record_back_edge(1));
        }
    }
}
