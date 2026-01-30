//! Array instructions: ArrayNew

use vo_runtime::ValueMeta;
use vo_runtime::gc::Gc;
use vo_runtime::objects::array;
use vo_runtime::slot::Slot;

use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

#[inline]
pub fn exec_array_new(stack: *mut Slot, bp: usize, inst: &Instruction, gc: &mut Gc) {
    let meta_raw = stack_get(stack, bp + inst.b as usize) as u32;
    let elem_meta = ValueMeta::from_raw(meta_raw);
    let len = stack_get(stack, bp + inst.c as usize) as usize;
    // flags: 0=dynamic (read from c+1), 1-63=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
    let elem_bytes = match inst.flags {
        0 => stack_get(stack, bp + inst.c as usize + 1) as usize,  // dynamic: elem_bytes in c+1
        0x81 => 1,   // int8
        0x82 => 2,   // int16
        0x84 | 0x44 => 4,   // int32 or float32
        f => f as usize,
    };
    let arr = array::create(gc, elem_meta, elem_bytes, len);
    stack_set(stack, bp + inst.a as usize, arr as u64);
}
