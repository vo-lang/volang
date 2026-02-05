//! Port stubs for no_std environments.
//!
//! All port operations are unsupported in no_std mode.
//! Functions that return values use sentinel values; functions that would
//! modify state are no-ops.

use crate::gc::{Gc, GcRef};
use vo_common_core::types::ValueMeta;

use super::WaiterInfo;

/// Ports are not supported in no_std mode.
/// This will cause a runtime error when actually used.
pub fn create(_gc: &mut Gc, _elem_meta: ValueMeta, _elem_slots: u16, _cap: usize) -> GcRef {
    // Return null GcRef - caller should check and handle
    0
}

#[inline]
pub fn len(_port: GcRef) -> usize { 0 }

#[inline]
pub fn is_closed(_port: GcRef) -> bool { true }

#[inline]
pub fn close(_port: GcRef) { }

pub fn register_receiver(_port: GcRef, _waiter: WaiterInfo) { }

pub fn get_state_ptr(_port: GcRef) -> u64 { 0 }

pub fn clone_state_ptr_for_transfer(_port: GcRef) -> u64 { 0 }

pub fn get_metadata(_port: GcRef) -> (u64, ValueMeta, u16) {
    (0, ValueMeta::default(), 0)
}
