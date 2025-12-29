//! Slice instructions: SliceNew, SliceSlice, SliceAppend

use vo_runtime::ValueMeta;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::slice;

use crate::instruction::Instruction;

#[inline]
pub fn exec_slice_new(stack: &mut [u64], bp: usize, inst: &Instruction, gc: &mut Gc) {
    let meta_raw = stack[bp + inst.b as usize] as u32;
    let elem_meta = ValueMeta::from_raw(meta_raw);
    let len = stack[bp + inst.c as usize] as usize;
    let cap = stack[bp + inst.c as usize + 1] as usize;
    // flags: 0=dynamic (read from c+2), 1-63=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
    let elem_bytes = match inst.flags {
        0 => stack[bp + inst.c as usize + 2] as usize,  // dynamic: elem_bytes in c+2
        0x81 => 1,   // int8
        0x82 => 2,   // int16
        0x84 | 0x44 => 4,   // int32 or float32
        f => f as usize,
    };
    let s = slice::create(gc, elem_meta, elem_bytes, len, cap);
    stack[bp + inst.a as usize] = s as u64;
}

/// SliceSlice: a[lo:hi] or a[lo:hi:max]
/// flags: bit0 = input is array, bit1 = has max (three-index slice)
#[inline]
pub fn exec_slice_slice(stack: &mut [u64], bp: usize, inst: &Instruction, gc: &mut Gc) {
    let s = stack[bp + inst.b as usize] as GcRef;
    let lo = stack[bp + inst.c as usize] as usize;
    let hi = stack[bp + inst.c as usize + 1] as usize;
    
    let is_array = (inst.flags & 0b01) != 0;
    let has_max = (inst.flags & 0b10) != 0;
    
    let result = if is_array {
        // Input is array: create slice from array range
        let cap = if has_max {
            stack[bp + inst.c as usize + 2] as usize - lo
        } else {
            hi - lo
        };
        slice::from_array_range(gc, s, lo, hi - lo, cap)
    } else {
        // Input is slice
        if has_max {
            let max = stack[bp + inst.c as usize + 2] as usize;
            slice::slice_of_with_cap(gc, s, lo, hi, max)
        } else {
            slice::slice_of(gc, s, lo, hi)
        }
    };
    stack[bp + inst.a as usize] = result as u64;
}

#[inline]
pub fn exec_slice_append(stack: &mut [u64], bp: usize, inst: &Instruction, gc: &mut Gc) {
    let s = stack[bp + inst.b as usize] as GcRef;
    // flags: 0=dynamic (read from c+1), 1-63=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
    // When flags!=0: c=[elem_meta], c+1..=[elem]
    // When flags==0: c=[elem_meta], c+1=[elem_bytes], c+2..=[elem]
    let (elem_bytes, elem_offset) = match inst.flags {
        0 => (stack[bp + inst.c as usize + 1] as usize, 2usize),  // dynamic: elem_bytes in c+1, elem at c+2
        0x81 => (1, 1),   // int8
        0x82 => (2, 1),   // int16
        0x84 | 0x44 => (4, 1),   // int32 or float32
        f => (f as usize, 1),
    };
    let elem_slots = (elem_bytes + 7) / 8;
    
    // Read elem_meta from c
    let elem_meta = ValueMeta::from_raw(stack[bp + inst.c as usize] as u32);
    
    let src_start = bp + inst.c as usize + elem_offset;

    let val: &[u64] = &stack[src_start..src_start + elem_slots];
    let result = slice::append(gc, elem_meta, elem_bytes, s, val);
    stack[bp + inst.a as usize] = result as u64;
}
