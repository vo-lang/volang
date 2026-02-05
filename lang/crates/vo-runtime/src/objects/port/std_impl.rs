//! Port implementation for std environments.
//!
//! Uses Arc<Mutex<PortState>> for thread-safe cross-island communication.

use std::sync::{Arc, Mutex};

use crate::gc::{Gc, GcRef};
use crate::slot::{ptr_to_slot, slot_to_ptr, Slot};
use vo_common_core::types::{ValueKind, ValueMeta};

use crate::objects::queue_state::{QueueData, DATA_SLOTS, PortState, PackedValue, WaiterInfo};

pub use crate::objects::queue_state::{SendResult, RecvResult};

pub type PortSendResult = SendResult<WaiterInfo, PackedValue>;
pub type PortRecvResult = RecvResult<WaiterInfo>;

/// Create a new port.
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

/// Access port state via closure. Guard lifetime is bounded to the closure.
#[inline]
pub fn with_state<T, F: FnOnce(&mut PortState) -> T>(port: GcRef, f: F) -> T {
    let arc_ptr = slot_to_ptr::<Mutex<PortState>>(QueueData::as_ref(port).state);
    let mut guard = unsafe { (*arc_ptr).lock().unwrap() };
    f(&mut guard)
}

#[inline]
pub fn len(port: GcRef) -> usize { with_state(port, |s| s.len()) }

#[inline]
pub fn is_closed(port: GcRef) -> bool { with_state(port, |s| s.is_closed()) }

#[inline]
pub fn close(port: GcRef) { with_state(port, |s| s.close()); }

/// Try to send a packed value through the port.
pub fn try_send(port: GcRef, value: PackedValue) -> PortSendResult {
    let cap = crate::objects::queue_state::capacity(port);
    with_state(port, |s| s.try_send(value, cap))
}

/// Try to receive a packed value from the port.
pub fn try_recv(port: GcRef) -> (PortRecvResult, Option<PackedValue>) {
    with_state(port, |s| s.try_recv())
}

/// Register a sender to wait.
pub fn register_sender(port: GcRef, waiter: WaiterInfo, value: PackedValue) {
    with_state(port, |s| s.register_sender(waiter, value));
}

/// Register a receiver to wait.
pub fn register_receiver(port: GcRef, waiter: WaiterInfo) {
    with_state(port, |s| s.register_receiver(waiter));
}

/// Take all waiting receivers (for close notification).
pub fn take_waiting_receivers(port: GcRef) -> Vec<WaiterInfo> {
    with_state(port, |s| s.take_waiting_receivers())
}

/// Take all waiting senders (for close notification).
pub fn take_waiting_senders(port: GcRef) -> Vec<(WaiterInfo, PackedValue)> {
    with_state(port, |s| s.take_waiting_senders())
}

// =============================================================================
// Cross-island transfer helpers
// =============================================================================

/// Increment Arc refcount and return new raw pointer.
#[inline]
fn clone_arc_state(state_ptr: Slot) -> Slot {
    let arc_ptr = slot_to_ptr::<Mutex<PortState>>(state_ptr);
    let arc: Arc<Mutex<PortState>> = unsafe { Arc::from_raw(arc_ptr) };
    let arc_clone = Arc::clone(&arc);
    let _ = Arc::into_raw(arc); // Don't drop original
    ptr_to_slot(Arc::into_raw(arc_clone) as *mut u8)
}

/// Clone a port for cross-island transfer.
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
pub fn get_state_ptr(port: GcRef) -> u64 {
    QueueData::as_ref(port).state
}

/// Clone the Arc state and return new raw pointer for cross-island transfer.
/// This MUST be used when encoding ports for transfer to ensure the Arc
/// refcount is incremented before the data is sent, preventing use-after-free
/// if GC runs before the receiving island processes the message.
pub fn clone_state_ptr_for_transfer(port: GcRef) -> u64 {
    clone_arc_state(QueueData::as_ref(port).state)
}

/// Get port metadata for cross-island transfer.
pub fn get_metadata(port: GcRef) -> (u64, ValueMeta, u16) {
    let data = QueueData::as_ref(port);
    (data.cap, data.elem_meta, data.elem_slots)
}

/// Create a port from raw state pointer (for cross-island transfer).
/// IMPORTANT: The state_ptr must already have its Arc refcount incremented
/// (via clone_state_ptr_for_transfer) before calling this function.
/// This function takes ownership of that reference.
pub fn create_from_raw(gc: &mut Gc, state_ptr: u64, cap: u64, elem_meta: ValueMeta, elem_slots: u16) -> GcRef {
    let port = gc.alloc(ValueMeta::new(0, ValueKind::Port), DATA_SLOTS);
    let data = QueueData::as_mut(port);
    data.state = state_ptr;
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
pub unsafe fn drop_inner(port: GcRef) {
    let data = QueueData::as_mut(port);
    if data.state != 0 {
        let arc_ptr = slot_to_ptr::<Mutex<PortState>>(data.state) as *const Mutex<PortState>;
        drop(Arc::from_raw(arc_ptr));
        data.state = 0;
    }
}
