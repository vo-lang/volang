//! Pointer instructions: PtrNew, PtrGet, PtrSet, PtrGetN, PtrSetN

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(feature = "std")]
use std::string::{String, ToString};

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::slot::Slot;
use vo_runtime::{SlotType, ValueMeta};

use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

#[inline]
pub fn exec_ptr_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    value_layout: &[SlotType],
) -> Result<(), String> {
    let meta_raw = stack_get(stack, bp + inst.b as usize) as u32;
    let value_meta = ValueMeta::from_raw(meta_raw);
    let slots = u16::try_from(value_layout.len())
        .map_err(|_| "PtrNew value layout exceeds u16 slots".to_string())?;
    let ptr = gc.alloc(value_meta, slots);
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
        .is_some_and(|slot| matches!(slot, SlotType::GcRef | SlotType::Interface1))
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
