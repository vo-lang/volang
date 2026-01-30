//! Slice instructions: SliceNew, SliceSlice, SliceAppend

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::ValueMeta;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::slice;
use vo_runtime::slot::Slot;

use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

/// Result of exec_slice_new: Ok(()) on success, Err(msg) on invalid parameters
pub type SliceNewResult = Result<(), String>;

#[inline]
pub fn exec_slice_new(stack: *mut Slot, bp: usize, inst: &Instruction, gc: &mut Gc) -> SliceNewResult {
    let meta_raw = stack_get(stack, bp + inst.b as usize) as u32;
    let elem_meta = ValueMeta::from_raw(meta_raw);
    
    // Read as i64 first to check for negative values
    let len_i64 = stack_get(stack, bp + inst.c as usize) as i64;
    let cap_i64 = stack_get(stack, bp + inst.c as usize + 1) as i64;
    
    // Check for negative length
    if len_i64 < 0 {
        return Err(format!("runtime error: makeslice: len out of range"));
    }
    // Check for negative capacity
    if cap_i64 < 0 {
        return Err(format!("runtime error: makeslice: cap out of range"));
    }
    
    let len = len_i64 as usize;
    let cap = cap_i64 as usize;
    
    // Check len > cap
    if len > cap {
        return Err(format!("runtime error: makeslice: len larger than cap"));
    }
    
    // flags: 0=dynamic (read from c+2), 1-63=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
    let elem_bytes = match inst.flags {
        0 => stack_get(stack, bp + inst.c as usize + 2) as usize,  // dynamic: elem_bytes in c+2
        0x81 => 1,   // int8
        0x82 => 2,   // int16
        0x84 | 0x44 => 4,   // int32 or float32
        f => f as usize,
    };
    
    // Check for overflow in allocation size
    // Use isize::MAX as limit (similar to Go's maxAlloc)
    match cap.checked_mul(elem_bytes) {
        Some(total) if total <= isize::MAX as usize => {}
        _ => return Err(format!("runtime error: makeslice: cap out of range")),
    }
    
    let s = slice::create(gc, elem_meta, elem_bytes, len, cap);
    stack_set(stack, bp + inst.a as usize, s as u64);
    Ok(())
}

/// SliceSlice: a[lo:hi] or a[lo:hi:max]
/// flags: bit0 = input is array, bit1 = has max (three-index slice)
/// Returns false on bounds error.
#[inline]
pub fn exec_slice_slice(stack: *mut Slot, bp: usize, inst: &Instruction, gc: &mut Gc) -> bool {
    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
    let lo = stack_get(stack, bp + inst.c as usize) as usize;
    let hi = stack_get(stack, bp + inst.c as usize + 1) as usize;
    
    let is_array = (inst.flags & 0b01) != 0;
    let has_max = (inst.flags & 0b10) != 0;
    
    // nil slice slicing returns nil (Go semantics: nil[0:0] == nil)
    if s.is_null() && !is_array {
        stack_set(stack, bp + inst.a as usize, 0);
        return true;
    }
    
    let result = if is_array {
        if has_max {
            let max = stack_get(stack, bp + inst.c as usize + 2) as usize;
            slice::array_slice_with_cap(gc, s, lo, hi, max)
        } else {
            slice::array_slice(gc, s, lo, hi)
        }
    } else {
        if has_max {
            let max = stack_get(stack, bp + inst.c as usize + 2) as usize;
            slice::slice_of_with_cap(gc, s, lo, hi, max)
        } else {
            slice::slice_of(gc, s, lo, hi)
        }
    };
    
    match result {
        Some(r) => {
            stack_set(stack, bp + inst.a as usize, r as u64);
            true
        }
        None => false,
    }
}

#[inline]
pub fn exec_slice_append(stack: *mut Slot, bp: usize, inst: &Instruction, gc: &mut Gc) {
    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
    // flags: 0=dynamic (read from c+1), 1-63=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
    // When flags!=0: c=[elem_meta], c+1..=[elem]
    // When flags==0: c=[elem_meta], c+1=[elem_bytes], c+2..=[elem]
    let (elem_bytes, elem_offset) = match inst.flags {
        0 => (stack_get(stack, bp + inst.c as usize + 1) as usize, 2usize),  // dynamic: elem_bytes in c+1, elem at c+2
        0x81 => (1, 1),   // int8
        0x82 => (2, 1),   // int16
        0x84 | 0x44 => (4, 1),   // int32 or float32
        f => (f as usize, 1),
    };
    let elem_slots = (elem_bytes + 7) / 8;
    
    // Read elem_meta from c
    let elem_meta = ValueMeta::from_raw(stack_get(stack, bp + inst.c as usize) as u32);
    
    let src_start = bp + inst.c as usize + elem_offset;

    // Copy values to a Vec for the append call
    let val: Vec<u64> = (0..elem_slots).map(|i| stack_get(stack, src_start + i)).collect();
    let result = slice::append(gc, elem_meta, elem_bytes, s, &val);
    stack_set(stack, bp + inst.a as usize, result as u64);
}
