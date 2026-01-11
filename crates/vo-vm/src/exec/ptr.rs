//! Pointer instructions: PtrNew, PtrGet, PtrSet, PtrGetN, PtrSetN

use vo_runtime::ValueMeta;
use vo_runtime::gc::{Gc, GcRef};

use crate::instruction::Instruction;

#[inline]
pub fn exec_ptr_new(stack: &mut [u64], bp: usize, inst: &Instruction, gc: &mut Gc) {
    let meta_raw = stack[bp + inst.b as usize] as u32;
    let value_meta = ValueMeta::from_raw(meta_raw);
    let slots = inst.flags as u16;
    let ptr = gc.alloc(value_meta, slots);
    stack[bp + inst.a as usize] = ptr as u64;
}

/// Returns false if ptr is nil (caller should trigger panic)
#[inline]
pub fn exec_ptr_get(stack: &mut [u64], bp: usize, inst: &Instruction) -> bool {
    let ptr = stack[bp + inst.b as usize] as GcRef;
    if ptr.is_null() {
        return false;
    }
    let offset = inst.c as usize;
    let val = unsafe { Gc::read_slot(ptr, offset) };
    stack[bp + inst.a as usize] = val;
    true
}

/// PtrSet: a=ptr, b=offset, c=val
/// flags: bit0 = val is GcRef (needs write barrier)
/// Returns false if ptr is nil (caller should trigger panic)
#[inline]
pub fn exec_ptr_set(stack: &[u64], bp: usize, inst: &Instruction, gc: &mut Gc) -> bool {
    let ptr = stack[bp + inst.a as usize] as GcRef;
    if ptr.is_null() {
        return false;
    }
    let offset = inst.b as usize;
    let val = stack[bp + inst.c as usize];
    unsafe { Gc::write_slot(ptr, offset, val) };
    // Write barrier if val may be GcRef
    if (inst.flags & 1) != 0 {
        gc.write_barrier(ptr, val as GcRef);
    }
    true
}

/// Returns false if ptr is nil (caller should trigger panic)
#[inline]
pub fn exec_ptr_get_n(stack: &mut [u64], bp: usize, inst: &Instruction) -> bool {
    let ptr = stack[bp + inst.b as usize] as GcRef;
    if ptr.is_null() {
        return false;
    }
    let offset = inst.c as usize;
    let count = inst.flags as usize;
    let dst_start = bp + inst.a as usize;

    for i in 0..count {
        let val = unsafe { Gc::read_slot(ptr, offset + i) };
        stack[dst_start + i] = val;
    }
    true
}

/// PtrSetN: a=ptr, b=offset, c=src_start, flags=count
/// Note: PtrSetN has no barrier support. For structs containing GcRefs,
/// codegen emits individual PtrSet instructions (with barrier flags) for
/// each slot using emit_ptr_set_with_slot_types().
/// Returns false if ptr is nil (caller should trigger panic)
#[inline]
pub fn exec_ptr_set_n(stack: &[u64], bp: usize, inst: &Instruction) -> bool {
    let ptr = stack[bp + inst.a as usize] as GcRef;
    if ptr.is_null() {
        return false;
    }
    let offset = inst.b as usize;
    let count = inst.flags as usize;
    let src_start = bp + inst.c as usize;

    for i in 0..count {
        let val = stack[src_start + i];
        unsafe { Gc::write_slot(ptr, offset + i, val) };
    }
    true
}
