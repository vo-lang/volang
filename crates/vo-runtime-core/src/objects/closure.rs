//! Closure object operations.
//!
//! Layout: GcHeader + ClosureHeader + [upvalues...]
//! - ClosureHeader: func_id, upval_count (1 slot)
//! - Upvalues: upval_count slots (GcRef to escaped variables)

use crate::gc::{Gc, GcRef};
use vo_common_core::types::{ValueKind, ValueMeta};

#[repr(C)]
pub struct ClosureHeader {
    pub func_id: u32,
    pub upval_count: u32,
}

const HEADER_SLOTS: usize = 1;
const _: () = assert!(core::mem::size_of::<ClosureHeader>() == HEADER_SLOTS * 8);

impl ClosureHeader {
    #[inline]
    fn as_ref(c: GcRef) -> &'static Self {
        unsafe { &*(c as *const Self) }
    }

    #[inline]
    fn as_mut(c: GcRef) -> &'static mut Self {
        unsafe { &mut *(c as *mut Self) }
    }
}

pub fn create(gc: &mut Gc, func_id: u32, upval_count: usize) -> GcRef {
    let total_slots = HEADER_SLOTS + upval_count;
    let c = gc.alloc(ValueMeta::new(0, ValueKind::Closure), total_slots as u16);
    let header = ClosureHeader::as_mut(c);
    header.func_id = func_id;
    header.upval_count = upval_count as u32;
    c
}

#[inline]
pub fn func_id(c: GcRef) -> u32 { ClosureHeader::as_ref(c).func_id }
#[inline]
pub fn upval_count(c: GcRef) -> usize { ClosureHeader::as_ref(c).upval_count as usize }

#[inline]
fn upvals_ptr(c: GcRef) -> *mut u64 {
    unsafe { (c as *mut u64).add(HEADER_SLOTS) }
}

#[inline]
pub fn get_upvalue(c: GcRef, idx: usize) -> u64 {
    unsafe { *upvals_ptr(c).add(idx) }
}

#[inline]
pub fn set_upvalue(c: GcRef, idx: usize, val: u64) {
    unsafe { *upvals_ptr(c).add(idx) = val }
}

pub fn create_upval_box(gc: &mut Gc, value_meta: ValueMeta) -> GcRef {
    gc.alloc(value_meta, 1)
}

#[inline]
pub fn get_upval_box(uv: GcRef) -> u64 {
    unsafe { *uv }
}

#[inline]
pub fn set_upval_box(uv: GcRef, val: u64) {
    unsafe { *uv = val }
}
