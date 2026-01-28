//! Port-specific operations for cross-island communication.
//!
//! Shared: QueueData, QueueState, capacity/elem_meta/elem_slots (in queue_state.rs)
//! Port-specific: create, with_state, len/close/is_closed, send/recv, cross-island helpers, drop_inner

#[cfg(feature = "std")]
use std::sync::{Arc, Mutex};

use crate::gc::{Gc, GcRef};
use crate::slot::{ptr_to_slot, slot_to_ptr, Slot};
use vo_common_core::types::{ValueKind, ValueMeta};

use super::queue_state::{QueueData, DATA_SLOTS};

#[cfg(feature = "std")]
use super::queue_state::{PortState, PackedValue};

pub use super::queue_state::{SendResult, RecvResult};

#[cfg(feature = "std")]
pub use super::queue_state::WaiterInfo;

#[cfg(not(feature = "std"))]
#[derive(Debug, Clone, Copy)]
pub struct WaiterInfo {
    pub island_id: u32,
    pub fiber_id: u64,
}

#[cfg(feature = "std")]
pub type PortSendResult = SendResult<WaiterInfo, PackedValue>;
#[cfg(feature = "std")]
pub type PortRecvResult = RecvResult<WaiterInfo>;

/// Create a new port.
#[cfg(feature = "std")]
pub fn create(gc: &mut Gc, elem_meta: ValueMeta, elem_slots: u16, cap: usize) -> GcRef {
    let port = gc.alloc(ValueMeta::new(0, ValueKind::Port), DATA_SLOTS);
    let state = Arc::new(Mutex::new(PortState::new(cap)));
    let data = QueueData::as_mut(port);
    data.state = ptr_to_slot(Arc::into_raw(state) as *mut u8);
    data.cap = cap as Slot;
    data.elem_meta = elem_meta;
    data.elem_slots = elem_slots;
    port
}

#[cfg(not(feature = "std"))]
pub fn create(_gc: &mut Gc, _elem_meta: ValueMeta, _elem_slots: u16, _cap: usize) -> GcRef {
    panic!("Port not supported in no_std mode")
}

/// Access port state via closure. Guard lifetime is bounded to the closure.
#[cfg(feature = "std")]
#[inline]
pub fn with_state<T, F: FnOnce(&mut PortState) -> T>(port: GcRef, f: F) -> T {
    let arc_ptr = slot_to_ptr::<Mutex<PortState>>(QueueData::as_ref(port).state);
    let mut guard = unsafe { (*arc_ptr).lock().unwrap() };
    f(&mut guard)
}

#[cfg(feature = "std")]
#[inline]
pub fn len(port: GcRef) -> usize { with_state(port, |s| s.len()) }

#[cfg(not(feature = "std"))]
#[inline]
pub fn len(_port: GcRef) -> usize {
    panic!("Port not supported in no_std mode")
}

#[cfg(feature = "std")]
#[inline]
pub fn is_closed(port: GcRef) -> bool { with_state(port, |s| s.is_closed()) }

#[cfg(not(feature = "std"))]
#[inline]
pub fn is_closed(_port: GcRef) -> bool {
    panic!("Port not supported in no_std mode")
}

#[cfg(feature = "std")]
#[inline]
pub fn close(port: GcRef) { with_state(port, |s| s.close()); }

#[cfg(not(feature = "std"))]
#[inline]
pub fn close(_port: GcRef) {
    panic!("Port not supported in no_std mode")
}

/// Try to send a packed value through the port.
#[cfg(feature = "std")]
pub fn try_send(port: GcRef, value: PackedValue) -> PortSendResult {
    let cap = super::queue_state::capacity(port);
    with_state(port, |s| s.try_send(value, cap))
}

/// Try to receive a packed value from the port.
#[cfg(feature = "std")]
pub fn try_recv(port: GcRef) -> (PortRecvResult, Option<PackedValue>) {
    with_state(port, |s| s.try_recv())
}

/// Register a sender to wait.
#[cfg(feature = "std")]
pub fn register_sender(port: GcRef, waiter: WaiterInfo, value: PackedValue) {
    with_state(port, |s| s.register_sender(waiter, value));
}

/// Register a receiver to wait.
#[cfg(feature = "std")]
pub fn register_receiver(port: GcRef, waiter: WaiterInfo) {
    with_state(port, |s| s.register_receiver(waiter));
}

#[cfg(not(feature = "std"))]
pub fn register_receiver(_port: GcRef, _waiter: WaiterInfo) {
    panic!("Port not supported in no_std mode")
}

/// Take all waiting receivers (for close notification).
#[cfg(feature = "std")]
pub fn take_waiting_receivers(port: GcRef) -> Vec<WaiterInfo> {
    with_state(port, |s| s.take_waiting_receivers())
}

/// Take all waiting senders (for close notification).
#[cfg(feature = "std")]
pub fn take_waiting_senders(port: GcRef) -> Vec<(WaiterInfo, PackedValue)> {
    with_state(port, |s| s.take_waiting_senders())
}

// =============================================================================
// Cross-island transfer helpers
// =============================================================================

/// Increment Arc refcount and return new raw pointer.
#[cfg(feature = "std")]
#[inline]
fn clone_arc_state(state_ptr: Slot) -> Slot {
    let arc_ptr = slot_to_ptr::<Mutex<PortState>>(state_ptr);
    let arc: Arc<Mutex<PortState>> = unsafe { Arc::from_raw(arc_ptr) };
    let arc_clone = Arc::clone(&arc);
    let _ = Arc::into_raw(arc); // Don't drop original
    ptr_to_slot(Arc::into_raw(arc_clone) as *mut u8)
}

/// Clone a port for cross-island transfer.
#[cfg(feature = "std")]
pub fn clone_for_island(gc: &mut Gc, src_port: GcRef) -> GcRef {
    let src = QueueData::as_ref(src_port);
    let port = gc.alloc(ValueMeta::new(0, ValueKind::Port), DATA_SLOTS);
    let data = QueueData::as_mut(port);
    data.state = clone_arc_state(src.state);
    data.cap = src.cap;
    data.elem_meta = src.elem_meta;
    data.elem_slots = src.elem_slots;
    port
}

/// Get the raw state pointer for cross-island transfer.
#[cfg(feature = "std")]
pub fn get_state_ptr(port: GcRef) -> u64 {
    QueueData::as_ref(port).state
}

#[cfg(not(feature = "std"))]
pub fn get_state_ptr(_port: GcRef) -> u64 {
    panic!("Port not supported in no_std mode")
}

/// Get port metadata for cross-island transfer.
#[cfg(feature = "std")]
pub fn get_metadata(port: GcRef) -> (u64, ValueMeta, u16) {
    let data = QueueData::as_ref(port);
    (data.cap, data.elem_meta, data.elem_slots)
}

#[cfg(not(feature = "std"))]
pub fn get_metadata(_port: GcRef) -> (u64, ValueMeta, u16) {
    panic!("Port not supported in no_std mode")
}

/// Create a port from raw state pointer (for cross-island transfer).
#[cfg(feature = "std")]
pub fn create_from_raw(gc: &mut Gc, state_ptr: u64, cap: u64, elem_meta: ValueMeta, elem_slots: u16) -> GcRef {
    let port = gc.alloc(ValueMeta::new(0, ValueKind::Port), DATA_SLOTS);
    let data = QueueData::as_mut(port);
    data.state = clone_arc_state(state_ptr);
    data.cap = cap;
    data.elem_meta = elem_meta;
    data.elem_slots = elem_slots;
    port
}

// =============================================================================
// Cleanup
// =============================================================================

/// # Safety
/// port must be a valid Port GcRef.
#[cfg(feature = "std")]
pub unsafe fn drop_inner(port: GcRef) {
    let data = QueueData::as_mut(port);
    if data.state != 0 {
        let arc_ptr = slot_to_ptr::<Mutex<PortState>>(data.state) as *const Mutex<PortState>;
        drop(Arc::from_raw(arc_ptr));
        data.state = 0;
    }
}

#[cfg(not(feature = "std"))]
pub unsafe fn drop_inner(_port: GcRef) {
    // No-op in no_std - ports not supported
}
