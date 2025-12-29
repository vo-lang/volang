//! Copy instructions: CopyN, SlotGet, SlotSet, SlotGetN, SlotSetN

use crate::instruction::Instruction;

#[inline]
pub fn exec_copy_n(stack: &mut [u64], bp: usize, inst: &Instruction) {
    let count = inst.c as usize;
    let src_start = bp + inst.b as usize;
    let dst_start = bp + inst.a as usize;

    for i in 0..count {
        let val = stack[src_start + i];
        stack[dst_start + i] = val;
    }
}

#[inline]
pub fn exec_slot_get(stack: &mut [u64], bp: usize, inst: &Instruction) {
    let idx = stack[bp + inst.c as usize] as usize;
    let val = stack[bp + inst.b as usize + idx];
    stack[bp + inst.a as usize] = val;
}

#[inline]
pub fn exec_slot_set(stack: &mut [u64], bp: usize, inst: &Instruction) {
    let idx = stack[bp + inst.b as usize] as usize;
    let val = stack[bp + inst.c as usize];
    stack[bp + inst.a as usize + idx] = val;
}

#[inline]
pub fn exec_slot_get_n(stack: &mut [u64], bp: usize, inst: &Instruction) {
    let elem_slots = inst.flags as usize;
    let idx = stack[bp + inst.c as usize] as usize;
    let src_start = bp + inst.b as usize + idx * elem_slots;
    let dst_start = bp + inst.a as usize;

    for i in 0..elem_slots {
        stack[dst_start + i] = stack[src_start + i];
    }
}

#[inline]
pub fn exec_slot_set_n(stack: &mut [u64], bp: usize, inst: &Instruction) {
    let elem_slots = inst.flags as usize;
    let idx = stack[bp + inst.b as usize] as usize;
    let dst_start = bp + inst.a as usize + idx * elem_slots;
    let src_start = bp + inst.c as usize;

    for i in 0..elem_slots {
        stack[dst_start + i] = stack[src_start + i];
    }
}
