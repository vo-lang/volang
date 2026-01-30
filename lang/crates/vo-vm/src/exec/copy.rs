//! Copy instructions: CopyN, SlotGet, SlotSet, SlotGetN, SlotSetN

use vo_runtime::slot::Slot;
use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

#[inline]
pub fn exec_copy_n(stack: *mut Slot, bp: usize, inst: &Instruction) {
    let count = inst.c as usize;
    if count == 0 { return; }
    let src_start = bp + inst.b as usize;
    let dst_start = bp + inst.a as usize;

    // Use ptr::copy for efficient bulk copy (handles overlapping regions)
    unsafe { core::ptr::copy(stack.add(src_start), stack.add(dst_start), count) };
}

#[inline]
pub fn exec_slot_get(stack: *mut Slot, bp: usize, inst: &Instruction) {
    let idx = stack_get(stack, bp + inst.c as usize) as usize;
    let val = stack_get(stack, bp + inst.b as usize + idx);
    stack_set(stack, bp + inst.a as usize, val);
}

#[inline]
pub fn exec_slot_set(stack: *mut Slot, bp: usize, inst: &Instruction) {
    let idx = stack_get(stack, bp + inst.b as usize) as usize;
    let val = stack_get(stack, bp + inst.c as usize);
    stack_set(stack, bp + inst.a as usize + idx, val);
}

#[inline]
pub fn exec_slot_get_n(stack: *mut Slot, bp: usize, inst: &Instruction) {
    let elem_slots = inst.flags as usize;
    if elem_slots == 0 { return; }
    let idx = stack_get(stack, bp + inst.c as usize) as usize;
    let src_start = bp + inst.b as usize + idx * elem_slots;
    let dst_start = bp + inst.a as usize;

    unsafe { core::ptr::copy(stack.add(src_start), stack.add(dst_start), elem_slots) };
}

#[inline]
pub fn exec_slot_set_n(stack: *mut Slot, bp: usize, inst: &Instruction) {
    let elem_slots = inst.flags as usize;
    if elem_slots == 0 { return; }
    let idx = stack_get(stack, bp + inst.b as usize) as usize;
    let dst_start = bp + inst.a as usize + idx * elem_slots;
    let src_start = bp + inst.c as usize;

    unsafe { core::ptr::copy(stack.add(src_start), stack.add(dst_start), elem_slots) };
}
