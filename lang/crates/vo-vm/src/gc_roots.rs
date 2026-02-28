//! GC root scanning for VM.

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

use vo_runtime::gc::{scan_slots_by_types, Gc, GcRef};
use vo_runtime::ffi::SentinelErrorCache;

use crate::bytecode::{FunctionDef, GlobalDef};
use crate::fiber::{DeferEntry, Fiber, PanicState};
use crate::vm::Vm;

/// Scan a slice of raw GcRefs.
#[inline]
fn scan_gcrefs(gc: &mut Gc, gcrefs: &[u64]) {
    for &raw in gcrefs {
        let gcref = raw as GcRef;
        if !gcref.is_null() {
            gc.mark_gray(gcref);
        }
    }
}

/// Scan DeferEntry for GC refs.
#[inline]
fn scan_defer_entry(gc: &mut Gc, entry: &DeferEntry) {
    if !entry.closure.is_null() {
        gc.mark_gray(entry.closure);
    }
    if !entry.args.is_null() {
        gc.mark_gray(entry.args);
    }
}

impl Vm {
    pub fn scan_roots(&mut self) {
        if self.module.is_none() {
            return;
        }

        let module = self.module.as_ref().unwrap();
        scan_globals(&mut self.state.gc, &self.state.globals, &module.globals);
        scan_fibers(&mut self.state.gc, &self.scheduler.fibers, &module.functions);
        scan_sentinel_errors(&mut self.state.gc, &self.state.sentinel_errors);
    }

    /// Run incremental GC step if debt > 0.
    ///
    /// Called at scheduling boundaries (between fiber timeslices).
    /// Uses raw pointer to split the borrow: gc.step() takes &mut Gc,
    /// while the scan_roots callback reads scheduler.fibers, state.globals, etc.
    ///
    /// SAFETY: Called only between fiber runs — no fiber is executing,
    /// so all fiber stacks are stable and safe to scan.
    pub fn gc_step(&mut self) {
        if !self.state.gc.should_step() {
            return;
        }
        let module = match &self.module {
            Some(m) => m as *const crate::bytecode::Module,
            None => return,
        };
        // SAFETY: Split borrow via raw pointer. gc is exclusively accessed by step(),
        // while scan_roots/scan_object/finalize read other fields (globals, fibers, etc).
        // No aliasing because gc is a distinct field from globals/fibers/sentinel_errors.
        let gc_ptr = &mut self.state.gc as *mut vo_runtime::gc::Gc;
        let globals = &self.state.globals;
        let sentinel_errors = &self.state.sentinel_errors;
        let fibers = &self.scheduler.fibers;
        let module_ref = unsafe { &*module };

        // Collect capture_slot_types for closure scanning.
        // Each entry is indexed by func_id — used by scan_closure to get capture types.
        let func_capture_slot_types: Vec<&[vo_runtime::SlotType]> = module_ref.functions.iter()
            .map(|f| f.capture_slot_types.as_slice())
            .collect();

        unsafe { &mut *gc_ptr }.step(
            |gc| {
                scan_globals(gc, globals, &module_ref.globals);
                scan_fibers(gc, fibers, &module_ref.functions);
                scan_sentinel_errors(gc, sentinel_errors);
            },
            |gc, obj| {
                vo_runtime::gc_types::scan_object(gc, obj, &module_ref.struct_metas, &func_capture_slot_types);
            },
            |obj| {
                vo_runtime::gc_types::finalize_object(obj);
            },
        );
    }
}

/// Scan sentinel error cache — interface pairs (slot0, slot1) may contain GcRefs.
fn scan_sentinel_errors(gc: &mut Gc, cache: &SentinelErrorCache) {
    for errors in cache.iter_values() {
        for &(slot0, slot1) in errors {
            if vo_runtime::objects::interface::data_is_gc_ref(slot0) && slot1 != 0 {
                gc.mark_gray(slot1 as GcRef);
            }
        }
    }
}

fn scan_globals(gc: &mut Gc, globals: &[u64], global_defs: &[GlobalDef]) {
    let mut global_idx = 0;
    for def in global_defs {
        let global_slice = &globals[global_idx..global_idx + def.slots as usize];
        scan_slots_by_types(gc, global_slice, &def.slot_types);
        global_idx += def.slots as usize;
    }
}

fn scan_fibers(gc: &mut Gc, fibers: &[Box<Fiber>], functions: &[FunctionDef]) {
    for fiber in fibers {
        // Dead fibers are waiting for slot reuse — their stack may contain stale GcRefs
        // that would incorrectly keep objects alive. Skip them.
        if fiber.state.is_dead() { continue; }

        // Scan stack frames (VM frames)
        // Note: resume_stack (JIT shadow frames) doesn't need scanning because
        // GC cannot trigger during pure JIT execution (no safepoints).
        // When JIT returns Call/WaitIo, materialize_jit_frames converts
        // resume_stack to fiber.frames before VM takes over.
        for frame in &fiber.frames {
            let func = &functions[frame.func_id as usize];
            let stack_slice = &fiber.stack[frame.bp..];
            scan_slots_by_types(gc, stack_slice, &func.slot_types);
        }

        // Scan defer_stack
        for entry in &fiber.defer_stack {
            scan_defer_entry(gc, entry);
        }

        // Scan unwinding state (return/panic unwinding with pending defers)
        if let Some(state) = &fiber.unwinding {
            for entry in &state.pending {
                scan_defer_entry(gc, entry);
            }
            // Scan return values
            if let Some(ref rv) = state.return_values {
                match rv {
                    crate::fiber::ReturnValues::Stack { vals, slot_types } => {
                        scan_slots_by_types(gc, vals, slot_types);
                    }
                    crate::fiber::ReturnValues::Heap { gcrefs, .. } => {
                        scan_gcrefs(gc, gcrefs);
                    }
                }
            }
        }

        // Scan panic value (only Recoverable has interface{})
        if let Some(PanicState::Recoverable(val)) = fiber.panic_state {
            if val.is_ref_type() && val.slot1 != 0 {
                gc.mark_gray(val.as_ref());
            }
        }

        // Scan closure replay results using slot_types for type-safe scanning.
        // Each result carries its own slot_types extracted from the returning function.
        for (vals, slot_types) in &fiber.closure_replay.results {
            scan_slots_by_types(gc, vals, slot_types);
        }

        // Scan select state — registered_channels holds channel GcRefs while blocked in select.
        if let Some(ref ss) = fiber.select_state {
            for &ch in &ss.registered_channels {
                if !ch.is_null() { gc.mark_gray(ch); }
            }
        }

        // Scan JIT panic message (InterfaceSlot may contain GcRef).
        #[cfg(feature = "jit")]
        if fiber.jit_panic_flag {
            let val = fiber.jit_panic_msg;
            if val.is_ref_type() && val.slot1 != 0 {
                gc.mark_gray(val.as_ref());
            }
        }
    }
}
