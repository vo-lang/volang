//! Global variable instructions: GlobalGet, GlobalGetN, GlobalSet, GlobalSetN

use vo_runtime::slot::Slot;
use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

#[inline]
pub fn exec_global_get(stack: *mut Slot, bp: usize, inst: &Instruction, globals: &[u64]) {
    let val = globals[inst.b as usize];
    stack_set(stack, bp + inst.a as usize, val);
}

#[inline]
pub fn exec_global_get_n(stack: *mut Slot, bp: usize, inst: &Instruction, globals: &[u64]) {
    let count = inst.flags as usize;
    let dst_start = bp + inst.a as usize;
    let src_start = inst.b as usize;

    for i in 0..count {
        stack_set(stack, dst_start + i, globals[src_start + i]);
    }
}

#[inline]
pub fn exec_global_set(stack: *const Slot, bp: usize, inst: &Instruction, globals: &mut [u64]) {
    let val = stack_get(stack, bp + inst.b as usize);
    globals[inst.a as usize] = val;
}

#[inline]
pub fn exec_global_set_n(stack: *const Slot, bp: usize, inst: &Instruction, globals: &mut [u64]) {
    let count = inst.flags as usize;
    let src_start = bp + inst.b as usize;
    let dst_start = inst.a as usize;

    for i in 0..count {
        globals[dst_start + i] = stack_get(stack, src_start + i);
    }
}
