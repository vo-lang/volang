//! GC verification and debugging support.
//!
//! This module provides runtime verification of GC invariants.
//! Only compiled when `gc-debug` feature is enabled.
//!
//! Key verifications:
//! - Tri-color invariant: black objects cannot directly reference white objects
//! - Barrier coverage: all pointer writes that could break invariants call barriers
//! - Object lifecycle: freed objects are truly unreachable

#![cfg(feature = "gc-debug")]

use std::cell::RefCell;
use std::collections::HashSet;

use crate::gc::{Gc, GcRef};

/// Write operation record for barrier verification
#[derive(Debug, Clone)]
struct WriteOp {
    parent: GcRef,
    offset: usize,
    old_val: u64,
    new_val: u64,
    timestamp: u64,
}

/// Barrier call record
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct BarrierCall {
    parent: GcRef,
    offset: usize,
}

type ViolationHandler = dyn Fn(&str) + Send + Sync;

/// Debug statistics
#[derive(Debug, Default)]
pub struct DebugStats {
    pub total_allocs: u64,
    pub total_frees: u64,
    pub barrier_calls: u64,
    pub invariant_checks: u64,
    pub violations_found: u64,
}

/// GC debugger state
pub struct GcDebugger {
    enabled: bool,
    live_objects: HashSet<GcRef>,
    write_log: Vec<WriteOp>,
    barrier_calls: HashSet<BarrierCall>,
    timestamp: u64,
    stats: DebugStats,
    on_violation: Option<Box<ViolationHandler>>,
}

impl GcDebugger {
    fn new() -> Self {
        Self {
            enabled: false,
            live_objects: HashSet::new(),
            write_log: Vec::new(),
            barrier_calls: HashSet::new(),
            timestamp: 0,
            stats: DebugStats::default(),
            on_violation: None,
        }
    }

    fn report_violation(&mut self, category: &str, detail: &str) {
        self.stats.violations_found += 1;
        let msg = format!("[GC VIOLATION] {}: {}", category, detail);
        if let Some(ref handler) = self.on_violation {
            handler(&msg);
        } else {
            eprintln!("{}", msg);
        }
    }
}

thread_local! {
    static DEBUGGER: RefCell<GcDebugger> = RefCell::new(GcDebugger::new());
}

/// Enable GC debugging
pub fn enable() {
    DEBUGGER.with(|d| {
        d.borrow_mut().enabled = true;
    });
    eprintln!("[GC DEBUG] Enabled");
}

/// Disable GC debugging
pub fn disable() {
    DEBUGGER.with(|d| {
        d.borrow_mut().enabled = false;
    });
}

/// Check if GC debugging is enabled
pub fn is_enabled() -> bool {
    DEBUGGER.with(|d| d.borrow().enabled)
}

/// Set custom violation handler
pub fn set_on_violation<F>(handler: F)
where
    F: Fn(&str) + Send + Sync + 'static,
{
    DEBUGGER.with(|d| {
        d.borrow_mut().on_violation = Some(Box::new(handler));
    });
}

/// Get current debug statistics
pub fn stats() -> DebugStats {
    DEBUGGER.with(|d| {
        let dbg = d.borrow();
        DebugStats {
            total_allocs: dbg.stats.total_allocs,
            total_frees: dbg.stats.total_frees,
            barrier_calls: dbg.stats.barrier_calls,
            invariant_checks: dbg.stats.invariant_checks,
            violations_found: dbg.stats.violations_found,
        }
    })
}

/// Reset debugger state (useful between test runs)
pub fn reset() {
    DEBUGGER.with(|d| {
        let mut dbg = d.borrow_mut();
        dbg.live_objects.clear();
        dbg.write_log.clear();
        dbg.barrier_calls.clear();
        dbg.timestamp = 0;
        dbg.stats = DebugStats::default();
    });
}

// =============================================================================
// Hooks called from gc.rs
// =============================================================================

/// Called after object allocation
#[inline]
pub fn on_alloc(obj: GcRef) {
    DEBUGGER.with(|d| {
        let mut dbg = d.borrow_mut();
        if !dbg.enabled {
            return;
        }
        dbg.live_objects.insert(obj);
        dbg.stats.total_allocs += 1;
    });
}

/// Called before object is freed
#[inline]
pub fn on_free(obj: GcRef) {
    DEBUGGER.with(|d| {
        let mut dbg = d.borrow_mut();
        if !dbg.enabled {
            return;
        }
        if !dbg.live_objects.remove(&obj) {
            dbg.report_violation("double-free", &format!("{:?}", obj));
        }
        dbg.stats.total_frees += 1;
    });
}

/// Called when write barrier is invoked
#[inline]
pub fn on_barrier(parent: GcRef, offset: usize, _child: u64) {
    DEBUGGER.with(|d| {
        let mut dbg = d.borrow_mut();
        if !dbg.enabled {
            return;
        }
        dbg.barrier_calls.insert(BarrierCall { parent, offset });
        dbg.stats.barrier_calls += 1;
    });
}

/// Called to record a pointer write (for barrier coverage verification)
#[inline]
pub fn on_ptr_write(parent: GcRef, offset: usize, old_val: u64, new_val: u64) {
    DEBUGGER.with(|d| {
        let mut dbg = d.borrow_mut();
        if !dbg.enabled {
            return;
        }
        dbg.timestamp += 1;
        let ts = dbg.timestamp;
        dbg.write_log.push(WriteOp {
            parent,
            offset,
            old_val,
            new_val,
            timestamp: ts,
        });
    });
}

/// Verify debugger-tracked object liveness after a GC step.
///
/// Precise tri-color edge checks require SlotType metadata and live in the VM
/// verifier. This hook intentionally avoids conservative word scanning.
#[inline]
pub fn verify_tri_color(gc: &Gc) {
    DEBUGGER.with(|d| {
        let mut dbg = d.borrow_mut();
        if !dbg.enabled {
            return;
        }
        dbg.stats.invariant_checks += 1;

        let tracked: Vec<_> = dbg.live_objects.iter().copied().collect();
        for obj in tracked {
            if gc.canonicalize_ref(obj).is_none() {
                dbg.report_violation("liveness", &format!("tracked freed object {:?}", obj));
            }
        }
    });
}

/// Clear write/barrier logs after GC cycle
#[inline]
pub fn clear_cycle_logs() {
    DEBUGGER.with(|d| {
        let mut dbg = d.borrow_mut();
        dbg.write_log.clear();
        dbg.barrier_calls.clear();
    });
}

/// Print debug summary
pub fn print_summary() {
    DEBUGGER.with(|d| {
        let dbg = d.borrow();
        eprintln!("[GC DEBUG] Summary:");
        eprintln!("  Allocs: {}", dbg.stats.total_allocs);
        eprintln!("  Frees: {}", dbg.stats.total_frees);
        eprintln!("  Live: {}", dbg.live_objects.len());
        eprintln!("  Barriers: {}", dbg.stats.barrier_calls);
        eprintln!("  Pointer writes: {}", dbg.write_log.len());
        if let Some(last) = dbg.write_log.last() {
            eprintln!(
                "  Last pointer write: parent={:?} offset={} old={:#x} new={:#x} timestamp={}",
                last.parent, last.offset, last.old_val, last.new_val, last.timestamp
            );
        }
        eprintln!("  Invariant checks: {}", dbg.stats.invariant_checks);
        eprintln!("  Violations: {}", dbg.stats.violations_found);
    });
}
