//! GC root scanning for VM.

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

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
            // Scan return values based on unwinding kind
            match &state.kind {
                crate::fiber::UnwindingKind::Return { return_kind, .. } => {
                    match return_kind {
                        crate::fiber::PendingReturnKind::None => {}
                        crate::fiber::PendingReturnKind::Stack { vals, slot_types } => {
                            scan_slots_by_types(gc, &vals, &slot_types);
                        }
                        crate::fiber::PendingReturnKind::Heap { gcrefs, .. } => {
                            scan_gcrefs(gc, &gcrefs);
                        }
                    }
                }
                crate::fiber::UnwindingKind::Panic { saved_return_kind, .. } => {
                    match saved_return_kind {
                        crate::fiber::PendingReturnKind::None => {}
                        crate::fiber::PendingReturnKind::Stack { vals, slot_types } => {
                            scan_slots_by_types(gc, &vals, &slot_types);
                        }
                        crate::fiber::PendingReturnKind::Heap { gcrefs, .. } => {
                            scan_gcrefs(gc, &gcrefs);
                        }
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
    }
}
