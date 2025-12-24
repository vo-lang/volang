//! Map instructions: MapNew, MapGet, MapSet, MapDelete, MapLen

use vo_common_core::types::ValueMeta;
use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::map;

use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_map_new(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let packed = fiber.read_reg(inst.b);
    let key_meta = ValueMeta::from_raw((packed >> 32) as u32);
    let val_meta = ValueMeta::from_raw(packed as u32);
    let key_slots = (inst.c >> 8) as u16;
    let val_slots = (inst.c & 0xFF) as u16;
    let m = map::create(gc, key_meta, val_meta, key_slots, val_slots);
    fiber.write_reg(inst.a, m as u64);
}

#[inline]
pub fn exec_map_get(fiber: &mut Fiber, inst: &Instruction) {
    let m = fiber.read_reg(inst.b) as GcRef;
    let meta = fiber.read_reg(inst.c);
    let key_slots = ((meta >> 16) & 0xFFFF) as usize;
    let val_slots = ((meta >> 1) & 0x7FFF) as usize;
    let has_ok = (meta & 1) != 0;

    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let key_start = bp + inst.c as usize + 1;
    let key: &[u64] = &fiber.stack[key_start..key_start + key_slots];

    let dst_start = bp + inst.a as usize;

    let (val_opt, ok) = map::get_with_ok(m, key);
    if let Some(val) = val_opt {
        for i in 0..val_slots.min(val.len()) {
            fiber.stack[dst_start + i] = val[i];
        }
    } else {
        for i in 0..val_slots {
            fiber.stack[dst_start + i] = 0;
        }
    }
    if has_ok {
        fiber.stack[dst_start + val_slots] = ok as u64;
    }
}

#[inline]
pub fn exec_map_set(fiber: &mut Fiber, inst: &Instruction) {
    let m = fiber.read_reg(inst.a) as GcRef;
    let meta = fiber.read_reg(inst.b);
    let key_slots = ((meta >> 8) & 0xFF) as usize;
    let val_slots = (meta & 0xFF) as usize;

    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let key_start = bp + inst.b as usize + 1;
    let val_start = bp + inst.c as usize;

    let key: &[u64] = &fiber.stack[key_start..key_start + key_slots];
    let val: &[u64] = &fiber.stack[val_start..val_start + val_slots];

    map::set(m, key, val);
}

#[inline]
pub fn exec_map_delete(fiber: &mut Fiber, inst: &Instruction) {
    let m = fiber.read_reg(inst.a) as GcRef;
    let meta = fiber.read_reg(inst.b);
    let key_slots = meta as usize;

    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let key_start = bp + inst.b as usize + 1;

    let key: &[u64] = &fiber.stack[key_start..key_start + key_slots];

    map::delete(m, key);
}

#[inline]
pub fn exec_map_len(fiber: &mut Fiber, inst: &Instruction) {
    let m = fiber.read_reg(inst.b) as GcRef;
    let len = if m.is_null() { 0 } else { map::len(m) };
    fiber.write_reg(inst.a, len as u64);
}
