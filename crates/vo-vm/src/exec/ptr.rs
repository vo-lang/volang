//! Pointer instructions: PtrNew, PtrClone, PtrGet, PtrSet, PtrGetN, PtrSetN

use vo_common_core::types::ValueMeta;
use vo_runtime_core::gc::{Gc, GcRef};

use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_ptr_new(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let meta_raw = fiber.read_reg(inst.b) as u32;
    let value_meta = ValueMeta::from_raw(meta_raw);
    let slots = inst.flags as u16;
    let ptr = gc.alloc(value_meta, slots);
    fiber.write_reg(inst.a, ptr as u64);
}

#[inline]
pub fn exec_ptr_clone(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let src = fiber.read_reg(inst.b) as GcRef;
    let dst = unsafe { gc.ptr_clone(src) };
    fiber.write_reg(inst.a, dst as u64);
}

#[inline]
pub fn exec_ptr_get(fiber: &mut Fiber, inst: &Instruction) {
    let ptr = fiber.read_reg(inst.b) as GcRef;
    let offset = inst.c as usize;
    let val = unsafe { Gc::read_slot(ptr, offset) };
    fiber.write_reg(inst.a, val);
}

#[inline]
pub fn exec_ptr_set(fiber: &mut Fiber, inst: &Instruction) {
    let ptr = fiber.read_reg(inst.a) as GcRef;
    let offset = inst.b as usize;
    let val = fiber.read_reg(inst.c);
    unsafe { Gc::write_slot(ptr, offset, val) };
}

#[inline]
pub fn exec_ptr_get_n(fiber: &mut Fiber, inst: &Instruction) {
    let ptr = fiber.read_reg(inst.b) as GcRef;
    let offset = inst.c as usize;
    let count = inst.flags as usize;
    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let dst_start = bp + inst.a as usize;

    for i in 0..count {
        let val = unsafe { Gc::read_slot(ptr, offset + i) };
        fiber.stack[dst_start + i] = val;
    }
}

#[inline]
pub fn exec_ptr_set_n(fiber: &mut Fiber, inst: &Instruction) {
    let ptr = fiber.read_reg(inst.a) as GcRef;
    let offset = inst.b as usize;
    let count = inst.flags as usize;
    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let src_start = bp + inst.c as usize;

    for i in 0..count {
        let val = fiber.stack[src_start + i];
        unsafe { Gc::write_slot(ptr, offset + i, val) };
    }
}
