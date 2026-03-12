//! Unified queue operations for channels (local and remote/cross-island).
//!
//! All queue objects use ValueKind::Channel. The backing field in QueueData
//! distinguishes LOCAL (in-process state) from REMOTE (cross-island proxy).
//!
//! Shared: QueueData, QueueState, capacity/elem_meta/elem_slots (in queue_state.rs)
//! This module: create, create_remote_proxy, get_state, HomeInfo/RemoteProxy
//! access, len/close/is_closed, send_or_block/recv_or_block, GC drop_inner.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, vec::Vec};

#[cfg(feature = "std")]
use std::{boxed::Box, vec::Vec};

use crate::gc::{Gc, GcRef};
use crate::slot::{ptr_to_slot, slot_to_ptr, Slot};
use vo_common_core::types::{ValueKind, ValueMeta};

use super::queue_state::{ChannelState, QueueData, DATA_SLOTS, BACKING_LOCAL,
    QueueWaiter, ChannelMessage};
#[cfg(feature = "std")]
use super::queue_state::{BACKING_REMOTE, HomeInfo, RemoteProxy};

pub use super::queue_state::{SendResult, RecvResult};

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
    data.backing = BACKING_LOCAL;
    data.endpoint_ptr = 0;
    chan
}

/// Create a REMOTE proxy channel (no ChannelState, operations go through messages).
#[cfg(feature = "std")]
pub fn create_remote_proxy(
    gc: &mut Gc,
    endpoint_id: u64,
    home_island: u32,
    cap: u64,
    elem_meta: ValueMeta,
    elem_slots: u16,
) -> GcRef {
    create_remote_proxy_with_closed(gc, endpoint_id, home_island, cap, elem_meta, elem_slots, false)
}

#[cfg(feature = "std")]
pub fn create_remote_proxy_with_closed(
    gc: &mut Gc,
    endpoint_id: u64,
    home_island: u32,
    cap: u64,
    elem_meta: ValueMeta,
    elem_slots: u16,
    closed: bool,
) -> GcRef {
    let chan = gc.alloc(ValueMeta::new(0, ValueKind::Channel), DATA_SLOTS);
    let proxy = Box::new(RemoteProxy {
        endpoint_id,
        home_island,
        closed,
    });
    let data = QueueData::as_mut(chan);
    data.state = 0; // no ChannelState for REMOTE
    data.cap = cap;
    data.elem_meta = elem_meta;
    data.elem_slots = elem_slots;
    data.backing = BACKING_REMOTE;
    data.endpoint_ptr = ptr_to_slot(Box::into_raw(proxy) as *mut u8);
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
    debug_assert!(QueueData::as_ref(chan).backing == BACKING_LOCAL,
        "get_state called on REMOTE channel");
    unsafe { &mut *slot_to_ptr(QueueData::as_ref(chan).state) }
}

/// Check if this channel is a REMOTE proxy.
#[cfg(feature = "std")]
#[inline]
pub fn is_remote(chan: GcRef) -> bool {
    QueueData::as_ref(chan).backing == BACKING_REMOTE
}

#[cfg(not(feature = "std"))]
#[inline]
pub fn is_remote(_chan: GcRef) -> bool { false }

/// Get RemoteProxy for a REMOTE channel.
#[cfg(feature = "std")]
#[inline]
pub fn remote_proxy(chan: GcRef) -> &'static RemoteProxy {
    debug_assert!(is_remote(chan), "remote_proxy called on LOCAL channel");
    unsafe { &*(QueueData::as_ref(chan).endpoint_ptr as *const RemoteProxy) }
}

/// Get mutable RemoteProxy for a REMOTE channel.
#[cfg(feature = "std")]
#[inline]
pub fn remote_proxy_mut(chan: GcRef) -> &'static mut RemoteProxy {
    debug_assert!(is_remote(chan), "remote_proxy_mut called on LOCAL channel");
    unsafe { &mut *(QueueData::as_ref(chan).endpoint_ptr as *mut RemoteProxy) }
}

/// Get HomeInfo for a LOCAL channel that has been transferred cross-island.
/// Returns None if never transferred (endpoint_ptr == 0).
#[cfg(feature = "std")]
pub fn home_info(chan: GcRef) -> Option<&'static HomeInfo> {
    let data = QueueData::as_ref(chan);
    if data.backing != BACKING_LOCAL || data.endpoint_ptr == 0 { return None; }
    Some(unsafe { &*(data.endpoint_ptr as *const HomeInfo) })
}

/// Get mutable HomeInfo for a LOCAL channel.
#[cfg(feature = "std")]
pub fn home_info_mut(chan: GcRef) -> Option<&'static mut HomeInfo> {
    let data = QueueData::as_ref(chan);
    if data.backing != BACKING_LOCAL || data.endpoint_ptr == 0 { return None; }
    Some(unsafe { &mut *(data.endpoint_ptr as *mut HomeInfo) })
}

/// Install HomeInfo on a LOCAL channel for first-time cross-island transfer.
#[cfg(feature = "std")]
pub fn install_home_info(chan: GcRef, endpoint_id: u64, home_island: u32) {
    let data = QueueData::as_mut(chan);
    debug_assert!(data.backing == BACKING_LOCAL, "install_home_info on non-LOCAL channel");
    debug_assert!(data.endpoint_ptr == 0, "HomeInfo already installed");
    let info = Box::new(HomeInfo {
        endpoint_id,
        home_island,
        peers: std::collections::HashSet::new(),
    });
    data.endpoint_ptr = ptr_to_slot(Box::into_raw(info) as *mut u8);
}

/// Get channel metadata for cross-island transfer.
pub fn get_metadata(chan: GcRef) -> (u64, ValueMeta, u16) {
    let data = QueueData::as_ref(chan);
    (data.cap, data.elem_meta, data.elem_slots)
}

/// Access channel state via closure. LOCAL backing only.
#[inline]
pub fn with_state<T, F: FnOnce(&mut ChannelState) -> T>(chan: GcRef, f: F) -> T {
    f(get_state(chan))
}

#[inline]
pub fn len(chan: GcRef) -> usize {
    #[cfg(feature = "std")]
    if is_remote(chan) { return 0; }
    get_state(chan).len()
}
#[inline]
pub fn is_closed(chan: GcRef) -> bool {
    #[cfg(feature = "std")]
    if is_remote(chan) { return remote_proxy(chan).closed; }
    get_state(chan).is_closed()
}
#[inline]
pub fn close(chan: GcRef) {
    debug_assert!(!is_remote(chan), "close called on REMOTE channel — use message passing");
    get_state(chan).close();
}

/// Atomic send: try to send, if would block, register waiter in same operation.
pub fn send_or_block(chan: GcRef, value: ChannelMessage, waiter: QueueWaiter) -> SendResult<QueueWaiter, ChannelMessage> {
    let cap = super::queue_state::capacity(chan);
    with_state(chan, |s| s.send_or_block(value, cap, waiter))
}

/// Atomic recv: try to receive, if would block, register waiter in same operation.
pub fn recv_or_block(chan: GcRef, waiter: QueueWaiter) -> (RecvResult<QueueWaiter>, Option<ChannelMessage>) {
    with_state(chan, |s| s.recv_or_block(waiter))
}

/// Pop the last value from the buffer (undo a DirectSend push for remote receivers).
pub fn pop_back_buffer(chan: GcRef) {
    with_state(chan, |s| { s.buffer.pop_back(); });
}

/// Take all waiting receivers (for close notification).
pub fn take_waiting_receivers(chan: GcRef) -> Vec<QueueWaiter> {
    with_state(chan, |s| s.take_waiting_receivers())
}

/// Take all waiting senders (for close notification).
pub fn take_waiting_senders(chan: GcRef) -> Vec<(QueueWaiter, ChannelMessage)> {
    with_state(chan, |s| s.take_waiting_senders())
}

/// # Safety
/// chan must be a valid Channel GcRef.
pub unsafe fn drop_inner(chan: GcRef) {
    let data = QueueData::as_mut(chan);
    match data.backing {
        BACKING_LOCAL => {
            if data.state != 0 {
                drop(Box::from_raw(slot_to_ptr::<ChannelState>(data.state)));
                data.state = 0;
            }
            #[cfg(feature = "std")]
            if data.endpoint_ptr != 0 {
                drop(Box::from_raw(data.endpoint_ptr as *mut HomeInfo));
                data.endpoint_ptr = 0;
            }
        }
        #[cfg(feature = "std")]
        BACKING_REMOTE => {
            // REMOTE channels have no ChannelState (state == 0)
            if data.endpoint_ptr != 0 {
                drop(Box::from_raw(data.endpoint_ptr as *mut RemoteProxy));
                data.endpoint_ptr = 0;
            }
        }
        _ => {}
    }
}
