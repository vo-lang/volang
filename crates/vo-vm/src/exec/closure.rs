//! Closure instructions: ClosureNew, ClosureGet

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::closure;

use crate::instruction::Instruction;

#[inline]
pub fn exec_closure_new(stack: &mut [u64], bp: usize, inst: &Instruction, gc: &mut Gc) {
    let func_id = (inst.b as u32) | ((inst.flags as u32) << 16);
    let capture_count = inst.c as usize;
    let c = closure::create(gc, func_id, capture_count);
    stack[bp + inst.a as usize] = c as u64;
}

#[inline]
pub fn exec_closure_get(stack: &mut [u64], bp: usize, inst: &Instruction) {
    let c = stack[bp] as GcRef;
    let val = closure::get_capture(c, inst.b as usize);
    stack[bp + inst.a as usize] = val;
}
