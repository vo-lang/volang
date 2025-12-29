//! Global variable instructions: GlobalGet, GlobalGetN, GlobalSet, GlobalSetN

use crate::instruction::Instruction;

#[inline]
pub fn exec_global_get(stack: &mut [u64], bp: usize, inst: &Instruction, globals: &[u64]) {
    let val = globals[inst.b as usize];
    stack[bp + inst.a as usize] = val;
}

#[inline]
pub fn exec_global_get_n(stack: &mut [u64], bp: usize, inst: &Instruction, globals: &[u64]) {
    let count = inst.flags as usize;
    let dst_start = bp + inst.a as usize;
    let src_start = inst.b as usize;

    for i in 0..count {
        stack[dst_start + i] = globals[src_start + i];
    }
}

#[inline]
pub fn exec_global_set(stack: &[u64], bp: usize, inst: &Instruction, globals: &mut [u64]) {
    let val = stack[bp + inst.b as usize];
    globals[inst.a as usize] = val;
}

#[inline]
pub fn exec_global_set_n(stack: &[u64], bp: usize, inst: &Instruction, globals: &mut [u64]) {
    let count = inst.flags as usize;
    let src_start = bp + inst.b as usize;
    let dst_start = inst.a as usize;

    for i in 0..count {
        globals[dst_start + i] = stack[src_start + i];
    }
}
