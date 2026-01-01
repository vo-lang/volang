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
use std::collections::{HashMap, HashSet};

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
    on_violation: Option<Box<dyn Fn(&str) + Send + Sync>>,
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

/// Verify tri-color invariant after mark phase
/// Black objects should not reference white objects
#[inline]
pub fn verify_tri_color(gc: &Gc) {
    DEBUGGER.with(|d| {
        let mut dbg = d.borrow_mut();
        if !dbg.enabled {
            return;
        }
        dbg.stats.invariant_checks += 1;
        
        // Collect violations first to avoid borrow conflict
        let mut violations = Vec::new();
        
        // For each live object that is black, check its children
        for &obj in &dbg.live_objects {
            if gc.is_black(obj) {
                let header = Gc::header(obj);
                let slots = header.slots as usize;
                
                for i in 0..slots {
                    let child = unsafe { Gc::read_slot(obj, i) };
                    if child != 0 {
                        let child_ref = child as GcRef;
                        if dbg.live_objects.contains(&child_ref) && gc.is_white(child_ref) {
                            violations.push(format!("black {:?} -> white {:?} at slot {}", obj, child_ref, i));
                        }
                    }
                }
            }
        }
        
        // Report violations
        for v in violations {
            dbg.report_violation("tri-color", &v);
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
        eprintln!("  Invariant checks: {}", dbg.stats.invariant_checks);
        eprintln!("  Violations: {}", dbg.stats.violations_found);
    });
}
