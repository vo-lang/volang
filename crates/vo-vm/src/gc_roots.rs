//! GC root scanning for VM.

use vo_runtime::gc::{scan_slots_by_types, Gc, GcRef};

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
        // Also scan trampoline fibers (used for JIT->VM calls)
        scan_fibers(&mut self.state.gc, &self.scheduler.trampoline_fibers, &module.functions);
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

fn scan_fibers(gc: &mut Gc, fibers: &[Fiber], functions: &[FunctionDef]) {
    for fiber in fibers {
        // Scan stack frames
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
            // Scan return values based on unwinding kind
            match &state.kind {
                crate::fiber::UnwindingKind::Return { return_kind, .. } => {
                    match return_kind {
                        crate::fiber::PendingReturnKind::Stack { vals, slot_types } => {
                            scan_slots_by_types(gc, vals, slot_types);
                        }
                        crate::fiber::PendingReturnKind::Heap { gcrefs, .. } => {
                            scan_gcrefs(gc, gcrefs);
                        }
                    }
                }
                crate::fiber::UnwindingKind::Panic { heap_gcrefs, .. } => {
                    if let Some(gcrefs) = heap_gcrefs {
                        scan_gcrefs(gc, gcrefs);
                    }
                }
            }
        }

        // Scan panic value (only Recoverable has interface{})
        // slot0 = packed metadata, slot1 = data (may be GcRef)
        if let Some(PanicState::Recoverable(slot0, slot1)) = fiber.panic_state {
            if vo_runtime::objects::interface::data_is_gc_ref(slot0) && slot1 != 0 {
                gc.mark_gray(slot1 as GcRef);
            }
        }
    }
}
