//! GC root scanning for VM.

use vo_common_core::types::SlotType;
use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::interface;

use crate::bytecode::{FunctionDef, GlobalDef};
use crate::fiber::{Fiber, Iterator};
use crate::vm::Vm;

impl Vm {
    pub fn scan_roots(&mut self) {
        if self.module.is_none() {
            return;
        }

        scan_globals(&mut self.gc, &self.globals, &self.module.as_ref().unwrap().globals);
        scan_fibers(&mut self.gc, &self.scheduler.fibers, &self.module.as_ref().unwrap().functions);
    }
}

fn scan_globals(gc: &mut Gc, globals: &[u64], global_defs: &[GlobalDef]) {
    let mut global_idx = 0;
    for def in global_defs {
        let vk = vo_common_core::types::ValueKind::from_u8(def.value_kind);
        if vk.may_contain_gc_refs() {
            for i in 0..def.slots as usize {
                let val = globals[global_idx + i];
                if val != 0 {
                    gc.mark_gray(val as GcRef);
                }
            }
        }
        global_idx += def.slots as usize;
    }
}

fn scan_fibers(gc: &mut Gc, fibers: &[Fiber], functions: &[FunctionDef]) {
    for fiber in fibers {
        for frame in &fiber.frames {
            let func = &functions[frame.func_id as usize];
            for (i, &st) in func.slot_types.iter().enumerate() {
                let slot_idx = frame.bp + i;
                if slot_idx >= fiber.stack.len() {
                    break;
                }
                match st {
                    SlotType::GcRef => {
                        let val = fiber.stack[slot_idx];
                        if val != 0 {
                            gc.mark_gray(val as GcRef);
                        }
                    }
                    SlotType::Interface1 => {
                        if slot_idx > 0 {
                            let header = fiber.stack[slot_idx - 1];
                            if interface::data_is_gc_ref(header) {
                                let val = fiber.stack[slot_idx];
                                if val != 0 {
                                    gc.mark_gray(val as GcRef);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        for entry in &fiber.defer_stack {
            if !entry.closure.is_null() {
                gc.mark_gray(entry.closure);
            }
        }

        if let Some(state) = &fiber.defer_state {
            for entry in &state.pending {
                if !entry.closure.is_null() {
                    gc.mark_gray(entry.closure);
                }
            }
        }

        for iter in &fiber.iter_stack {
            match iter {
                Iterator::HeapArray { arr, .. } => {
                    if !arr.is_null() {
                        gc.mark_gray(*arr);
                    }
                }
                Iterator::StackArray { .. } => {}
                Iterator::Map { map, .. } => {
                    if !map.is_null() {
                        gc.mark_gray(*map);
                    }
                }
                Iterator::String { s, .. } => {
                    if !s.is_null() {
                        gc.mark_gray(*s);
                    }
                }
                Iterator::IntRange { .. } => {}
                Iterator::Channel { ch, .. } => {
                    if !ch.is_null() {
                        gc.mark_gray(*ch);
                    }
                }
            }
        }

        if let Some(panic_val) = fiber.panic_value {
            if !panic_val.is_null() {
                gc.mark_gray(panic_val);
            }
        }
    }
}
