//! Channel-specific operations.
//!
//! Shared: QueueData, QueueState, capacity/elem_meta/elem_slots (in queue_state.rs)
//! Channel-specific: create, get_state, len/close/is_closed, GC helpers, drop_inner

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

#[cfg(feature = "std")]
use std::boxed::Box;

use crate::gc::{Gc, GcRef};
use crate::slot::{ptr_to_slot, slot_to_ptr, Slot};
use vo_common_core::types::{ValueKind, ValueMeta};

use super::queue_state::{QueueData, ChannelState, DATA_SLOTS};

pub use super::queue_state::{SendResult, RecvResult, GoId, ChannelMessage};

impl ChannelState {
    pub fn iter_buffer(&self) -> impl Iterator<Item = &[u64]> {
        self.buffer.iter().map(|b| b.as_ref())
    }

    pub fn iter_waiting_values(&self) -> impl Iterator<Item = &[u64]> {
        self.waiting_senders.iter().map(|(_, v)| v.as_ref())
    }
}

pub fn create(gc: &mut Gc, elem_meta: ValueMeta, elem_slots: u16, cap: usize) -> GcRef {
    let chan = gc.alloc(ValueMeta::new(0, ValueKind::Channel), DATA_SLOTS);
    let state = Box::new(ChannelState::new(cap));
    let data = QueueData::as_mut(chan);
    data.state = ptr_to_slot(Box::into_raw(state));
    data.cap = cap as Slot;
    data.elem_meta = elem_meta;
    data.elem_slots = elem_slots;
    chan
}

/// Create a new channel with validation (unified logic for VM and JIT).
/// 
/// Validates:
/// - cap >= 0
/// 
/// Returns Ok(GcRef) on success, Err(error_code) on failure.
pub fn create_checked(gc: &mut Gc, elem_meta: ValueMeta, elem_slots: u16, cap: i64) -> Result<GcRef, i32> {
    use super::alloc_error;
    if cap < 0 { return Err(alloc_error::NEGATIVE_CAP); }
    Ok(create(gc, elem_meta, elem_slots, cap as usize))
}

#[inline]
pub fn get_state(chan: GcRef) -> &'static mut ChannelState {
    unsafe { &mut *slot_to_ptr(QueueData::as_ref(chan).state) }
}

#[inline]
pub fn len(chan: GcRef) -> usize { get_state(chan).len() }
#[inline]
pub fn is_closed(chan: GcRef) -> bool { get_state(chan).is_closed() }
#[inline]
pub fn close(chan: GcRef) { get_state(chan).close(); }

/// # Safety
/// chan must be a valid Channel GcRef.
pub unsafe fn drop_inner(chan: GcRef) {
    let data = QueueData::as_mut(chan);
    if data.state != 0 {
        drop(Box::from_raw(slot_to_ptr::<ChannelState>(data.state)));
        data.state = 0;
    }
}
