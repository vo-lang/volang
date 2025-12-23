//! Closure object operations.
//!
//! Closure layout: [func_id, upval_count, upval[0], upval[1], ...]
//! - Slot 0: func_id (u32)
//! - Slot 1: upvalue count
//! - Slot 2+: captured values (GcRef to escaped variables)

use crate::gc::{Gc, GcRef};
use vo_common_core::types::ValueKind;

pub const SLOT_FUNC_ID: usize = 0;
pub const SLOT_UPVAL_COUNT: usize = 1;
pub const SLOT_UPVAL_START: usize = 2;

pub fn create(gc: &mut Gc, func_id: u32, upval_count: usize) -> GcRef {
    let total_slots = SLOT_UPVAL_START + upval_count;
    let c = gc.alloc(ValueKind::Closure as u8, 0, total_slots as u16);
    Gc::write_slot(c, SLOT_FUNC_ID, func_id as u64);
    Gc::write_slot(c, SLOT_UPVAL_COUNT, upval_count as u64);
    c
}

pub fn func_id(c: GcRef) -> u32 {
    Gc::read_slot(c, SLOT_FUNC_ID) as u32
}

pub fn upval_count(c: GcRef) -> usize {
    Gc::read_slot(c, SLOT_UPVAL_COUNT) as usize
}

pub fn get_upvalue(c: GcRef, idx: usize) -> u64 {
    Gc::read_slot(c, SLOT_UPVAL_START + idx)
}

pub fn set_upvalue(c: GcRef, idx: usize, val: u64) {
    Gc::write_slot(c, SLOT_UPVAL_START + idx, val);
}

pub fn create_upval_box(gc: &mut Gc, value_kind: u8) -> GcRef {
    gc.alloc(value_kind, 0, 1)
}

pub fn get_upval_box(uv: GcRef) -> u64 {
    Gc::read_slot(uv, 0)
}

pub fn set_upval_box(uv: GcRef, val: u64) {
    Gc::write_slot(uv, 0, val);
}
