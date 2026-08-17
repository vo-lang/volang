//! Pointer instructions: PtrNew, PtrGet, PtrSet, PtrGetN, PtrSetN

#[cfg(not(feature = "std"))]
use alloc::string::ToString;
#[cfg(feature = "std")]
use std::string::ToString;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::slot::Slot;
use vo_runtime::{SlotType, ValueMeta};

use crate::exec::InstructionError;
use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

#[inline]
pub fn exec_ptr_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    value_layout: &[SlotType],
) -> Result<(), InstructionError> {
    let meta_raw = stack_get(stack, bp + inst.b as usize) as u32;
    let value_meta = ValueMeta::from_raw(meta_raw);
    let slots = u16::try_from(value_layout.len())
        .map_err(|_| "PtrNew value layout exceeds u16 slots".to_string())?;
    let ptr = gc.try_alloc_value_slots(value_meta, slots)?;
    stack_set(stack, bp + inst.a as usize, ptr as u64);
    Ok(())
}

/// Returns false if ptr is nil (caller should trigger panic)
#[inline]
pub fn exec_ptr_get(stack: *mut Slot, bp: usize, inst: &Instruction) -> bool {
    let ptr = stack_get(stack, bp + inst.b as usize) as GcRef;
    if ptr.is_null() {
        return false;
    }
    debug_assert!(
        (ptr as usize) & 7 == 0,
        "exec_ptr_get: misaligned ptr={:#x} bp={} a={} b={} c={} flags={}",
        ptr as usize,
        bp,
        inst.a,
        inst.b,
        inst.c,
        inst.flags,
    );
    let offset = inst.c as usize;
    let val = unsafe { Gc::read_slot(ptr, offset) };
    stack_set(stack, bp + inst.a as usize, val);
    true
}

/// PtrSet: a=ptr, b=offset, c=val
/// PtrLayout determines whether the value needs a write barrier.
/// Returns false if ptr is nil (caller should trigger panic)
#[inline]
pub fn exec_ptr_set(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    value_layout: &[SlotType],
) -> bool {
    let ptr = stack_get(stack, bp + inst.a as usize) as GcRef;
    if ptr.is_null() {
        return false;
    }
    let offset = inst.b as usize;
    let val = stack_get(stack, bp + inst.c as usize);
    if value_layout
        .first()
        .is_some_and(|slot| slot.needs_write_barrier())
    {
        gc.write_barrier(ptr, val as GcRef);
    }
    unsafe { Gc::write_slot(ptr, offset, val) };
    true
}

/// Returns false if ptr is nil (caller should trigger panic)
#[inline]
pub fn exec_ptr_get_n(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    value_layout: &[SlotType],
) -> bool {
    let ptr = stack_get(stack, bp + inst.b as usize) as GcRef;
    if ptr.is_null() {
        return false;
    }
    let offset = inst.c as usize;
    let count = value_layout.len();
    let dst_start = bp + inst.a as usize;

    for i in 0..count {
        let val = unsafe { Gc::read_slot(ptr, offset + i) };
        stack_set(stack, dst_start + i, val);
    }
    true
}

/// PtrSetN: a=ptr, b=offset, c=src_start. PtrLayout owns the count.
/// Note: PtrSetN has no barrier support. For structs containing GcRefs,
/// codegen emits individual PtrSet instructions (with one-slot metadata) for
/// each slot using emit_ptr_set_with_slot_types().
/// Returns false if ptr is nil (caller should trigger panic)
#[inline]
pub fn exec_ptr_set_n(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    value_layout: &[SlotType],
) -> bool {
    let ptr = stack_get(stack, bp + inst.a as usize) as GcRef;
    if ptr.is_null() {
        return false;
    }
    let offset = inst.b as usize;
    let count = value_layout.len();
    let src_start = bp + inst.c as usize;

    for i in 0..count {
        let val = stack_get(stack, src_start + i);
        unsafe { Gc::write_slot(ptr, offset + i, val) };
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::Opcode;
    use vo_runtime::gc::{GcObjectScanChunk, GcRootScanChunk, GcRootState, GcState, G_OLD};
    use vo_runtime::slot::SLOT_BYTES;
    use vo_runtime::ValueKind;

    #[test]
    fn ptr_set_gc_base_store_keeps_old_to_young_edge_alive() {
        let mut gc = Gc::new();
        let parent = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
        let child = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
        unsafe { Gc::header_mut(parent) }.set_age(G_OLD);

        let stack = [parent as u64, child as u64];
        let store = Instruction::new(Opcode::PtrSet, 0, 0, 1);
        assert!(exec_ptr_set(
            stack.as_ptr(),
            0,
            &store,
            &mut gc,
            &[SlotType::GcBase],
        ));

        let minor_cycles = gc.memory_stats().minor_cycles;
        gc.gc_request_cycle();
        for _ in 0..1024 {
            unsafe {
                gc.step_with_scanners_budget(
                    GcRootState::MayHaveChanged,
                    1,
                    |_, _, _| GcRootScanChunk::complete(0),
                    |gc, object, _, _| {
                        if Gc::header(object).slots > 0 {
                            let referenced = Gc::read_slot(object, 0) as GcRef;
                            if !referenced.is_null() {
                                gc.mark_gray(referenced);
                            }
                        }
                        GcObjectScanChunk::complete(SLOT_BYTES)
                    },
                    |_| {},
                );
            }
            if gc.state() == GcState::Pause && gc.memory_stats().minor_cycles > minor_cycles {
                break;
            }
        }

        assert_eq!(gc.state(), GcState::Pause);
        assert!(gc.memory_stats().minor_cycles > minor_cycles);
        assert!(gc.objects().any(|object| object == child));
    }
}
