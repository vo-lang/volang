#![allow(clippy::missing_safety_doc)]
//! Unified queue operations for channels (local and remote/cross-island).
//!
//! QueueKind distinguishes local-only `chan` from remote-capable `port`.
//! The backing field in QueueData distinguishes LOCAL (in-process state)
//! from REMOTE (cross-island proxy).
//!
//! # Safety contract
//! Unsafe accessors require a canonical live queue allocation with the stated
//! backing, plus exclusive access whenever local state is mutated.
//!
//! Shared: QueueData, QueueState, capacity/elem_meta/elem_slots (in queue_state.rs)
//! This module: create, create_remote_proxy, get_state, HomeInfo/RemoteProxy
//! access, len/close/is_closed, send_or_block/recv_or_block, GC drop_inner.

#[cfg(not(feature = "std"))]
use alloc::{alloc as heap_alloc, boxed::Box, vec::Vec};

#[cfg(feature = "std")]
use std::{alloc as heap_alloc, boxed::Box, vec::Vec};

use core::alloc::Layout;

use crate::gc::{Gc, GcRef, MemoryError};
use crate::slot::{ptr_to_slot, slot_to_ptr, Slot};
use crate::Module;
use vo_common_core::types::{ValueMeta, ValueRttid};

use super::queue_state::{
    HomeInfo, LocalQueueState, QueueBacking, QueueData, QueueKind, QueueMessage, QueueWaiter,
    RemoteProxy, DATA_SLOTS,
};

pub use super::queue_state::{
    BlockingRecvResult, BlockingSendResult, RecvResult, ResolvedSendResult, SendResult,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HomeInfoSnapshot {
    pub endpoint_id: u64,
    pub home_island: u32,
    pub peers: Vec<u32>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueueEndpointError {
    MissingHomeInfo,
    AllocationFailed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueueCreateError {
    Invalid(i32),
    Memory(MemoryError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HomeInfoUndo {
    remove_home_info: bool,
    peer_island: u32,
    remove_peer: bool,
}

fn try_box<T>(value: T) -> Result<Box<T>, ()> {
    let layout = Layout::new::<T>();
    let raw = if layout.size() == 0 {
        core::ptr::NonNull::<T>::dangling().as_ptr()
    } else {
        let raw = unsafe { heap_alloc::alloc(layout) }.cast::<T>();
        if raw.is_null() {
            return Err(());
        }
        raw
    };
    unsafe {
        raw.write(value);
        Ok(Box::from_raw(raw))
    }
}

fn validate_local_queue_capacity(cap: usize) -> Result<(), i32> {
    Layout::array::<QueueMessage>(cap)
        .map(|_| ())
        .map_err(|_| super::alloc_error::OVERFLOW)
}

impl LocalQueueState {
    pub fn iter_buffer(&self) -> impl Iterator<Item = &[u64]> {
        self.buffer.iter().map(|b| b.as_ref())
    }

    pub fn iter_waiting_values(&self) -> impl Iterator<Item = &[u64]> {
        self.waiting_senders.iter().map(|(_, v)| v.as_ref())
    }
}

pub fn create(
    gc: &mut Gc,
    kind: QueueKind,
    elem_meta: ValueMeta,
    elem_rttid: ValueRttid,
    elem_slots: u16,
    cap: usize,
) -> GcRef {
    match try_create(gc, kind, elem_meta, elem_rttid, elem_slots, cap) {
        Ok(queue) => queue,
        Err(error) => gc.sticky_allocation_failure(error),
    }
}

pub fn try_create(
    gc: &mut Gc,
    kind: QueueKind,
    elem_meta: ValueMeta,
    elem_rttid: ValueRttid,
    elem_slots: u16,
    cap: usize,
) -> Result<GcRef, MemoryError> {
    let state = LocalQueueState::try_new(cap)
        .map_err(|_| MemoryError::SystemAllocationFailed)
        .or_else(|error| gc.allocation_failure(error))?;
    let state = try_box(state)
        .map_err(|_| MemoryError::SystemAllocationFailed)
        .or_else(|error| gc.allocation_failure(error))?;
    try_create_with_state(gc, kind, elem_meta, elem_rttid, elem_slots, cap, state)
}

fn try_create_with_state(
    gc: &mut Gc,
    kind: QueueKind,
    elem_meta: ValueMeta,
    elem_rttid: ValueRttid,
    elem_slots: u16,
    cap: usize,
    state: Box<LocalQueueState>,
) -> Result<GcRef, MemoryError> {
    let chan = gc.try_alloc(ValueMeta::new(0, kind.value_kind()), DATA_SLOTS)?;
    // Safety: `chan` is freshly allocated and not visible to the collector yet.
    let data = unsafe { QueueData::as_mut(chan) };
    data.state = ptr_to_slot(Box::into_raw(state));
    data.cap = cap as Slot;
    data.elem_meta = elem_meta;
    data.elem_slots = elem_slots;
    data.kind = kind as u16;
    data.reserved = 0;
    data.elem_rttid = elem_rttid.to_raw();
    data.backing = QueueBacking::Local as u16;
    data.endpoint_ptr = 0;
    Ok(chan)
}

/// Create a REMOTE proxy channel (no ChannelState, operations go through messages).
#[allow(clippy::too_many_arguments)]
pub fn create_remote_proxy(
    gc: &mut Gc,
    endpoint_id: u64,
    home_island: u32,
    cap: u64,
    elem_meta: ValueMeta,
    elem_rttid: ValueRttid,
    elem_slots: u16,
) -> GcRef {
    create_remote_proxy_with_closed(
        gc,
        endpoint_id,
        home_island,
        cap,
        elem_meta,
        elem_rttid,
        elem_slots,
        false,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn create_remote_proxy_with_closed(
    gc: &mut Gc,
    endpoint_id: u64,
    home_island: u32,
    cap: u64,
    elem_meta: ValueMeta,
    elem_rttid: ValueRttid,
    elem_slots: u16,
    closed: bool,
) -> GcRef {
    let kind = QueueKind::Port;
    let Ok(proxy) = try_box(RemoteProxy {
        endpoint_id,
        home_island,
        closed,
    }) else {
        gc.record_system_allocation_failure();
        return core::ptr::null_mut();
    };
    let chan = gc.alloc(ValueMeta::new(0, kind.value_kind()), DATA_SLOTS);
    if chan.is_null() {
        return chan;
    }
    // Safety: `chan` is freshly allocated and not visible to the collector yet.
    let data = unsafe { QueueData::as_mut(chan) };
    data.state = 0; // no ChannelState for REMOTE
    data.cap = cap;
    data.elem_meta = elem_meta;
    data.elem_slots = elem_slots;
    data.kind = kind as u16;
    data.reserved = 0;
    data.elem_rttid = elem_rttid.to_raw();
    data.backing = QueueBacking::Remote as u16;
    data.endpoint_ptr = ptr_to_slot(Box::into_raw(proxy) as *mut u8);
    chan
}

/// Create a new channel with validation (unified logic for VM and JIT).
///
/// Validates:
/// - cap >= 0
///
/// Returns Ok(GcRef) on success, Err(error_code) on failure.
pub fn create_checked(
    gc: &mut Gc,
    kind: QueueKind,
    elem_meta: ValueMeta,
    elem_rttid: ValueRttid,
    elem_slots: u16,
    cap: i64,
) -> Result<GcRef, i32> {
    match try_create_checked(gc, kind, elem_meta, elem_rttid, elem_slots, cap) {
        Ok(queue) => Ok(queue),
        Err(QueueCreateError::Invalid(code)) => Err(code),
        Err(QueueCreateError::Memory(error)) => Ok(gc.sticky_allocation_failure(error)),
    }
}

pub fn try_create_checked(
    gc: &mut Gc,
    kind: QueueKind,
    elem_meta: ValueMeta,
    elem_rttid: ValueRttid,
    elem_slots: u16,
    cap: i64,
) -> Result<GcRef, QueueCreateError> {
    use super::alloc_error;
    if cap < 0 || (kind == QueueKind::Port && cap == 0) {
        return Err(QueueCreateError::Invalid(alloc_error::NEGATIVE_CAP));
    }
    let cap = usize::try_from(cap).map_err(|_| QueueCreateError::Invalid(alloc_error::OVERFLOW))?;
    validate_local_queue_capacity(cap).map_err(QueueCreateError::Invalid)?;
    try_create(gc, kind, elem_meta, elem_rttid, elem_slots, cap).map_err(QueueCreateError::Memory)
}

/// Create a new channel with module-backed element metadata validation.
pub fn create_checked_with_module(
    gc: &mut Gc,
    kind: QueueKind,
    elem_meta: ValueMeta,
    elem_rttid: ValueRttid,
    elem_slots: u16,
    cap: i64,
    module: &Module,
) -> Result<GcRef, i32> {
    match try_create_checked_with_module(gc, kind, elem_meta, elem_rttid, elem_slots, cap, module) {
        Ok(queue) => Ok(queue),
        Err(QueueCreateError::Invalid(code)) => Err(code),
        Err(QueueCreateError::Memory(error)) => Ok(gc.sticky_allocation_failure(error)),
    }
}

pub fn try_create_checked_with_module(
    gc: &mut Gc,
    kind: QueueKind,
    elem_meta: ValueMeta,
    elem_rttid: ValueRttid,
    elem_slots: u16,
    cap: i64,
    module: &Module,
) -> Result<GcRef, QueueCreateError> {
    use super::alloc_error;
    if cap < 0 || (kind == QueueKind::Port && cap == 0) {
        return Err(QueueCreateError::Invalid(alloc_error::NEGATIVE_CAP));
    }
    validate_element_layout(module, elem_meta, elem_rttid, elem_slots)
        .map_err(|_| QueueCreateError::Invalid(alloc_error::OVERFLOW))?;
    let cap = usize::try_from(cap).map_err(|_| QueueCreateError::Invalid(alloc_error::OVERFLOW))?;
    validate_local_queue_capacity(cap).map_err(QueueCreateError::Invalid)?;
    try_create(gc, kind, elem_meta, elem_rttid, elem_slots, cap).map_err(QueueCreateError::Memory)
}

pub fn validate_element_layout(
    module: &Module,
    elem_meta: ValueMeta,
    elem_rttid: ValueRttid,
    elem_slots: u16,
) -> Result<(), QueueElementLayoutError> {
    let canonical_meta = module
        .canonical_value_meta_for_value_rttid(elem_rttid)
        .ok_or(QueueElementLayoutError::UnresolvedMeta)?;
    if elem_meta != canonical_meta {
        return Err(QueueElementLayoutError::MetaMismatch);
    }
    let expected_slots = module
        .slot_count_for_value_rttid(elem_rttid)
        .ok_or(QueueElementLayoutError::UnresolvedSlotWidth)?;
    if expected_slots > u16::MAX as usize || elem_slots as usize != expected_slots {
        return Err(QueueElementLayoutError::SlotMismatch);
    }
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueueElementLayoutError {
    UnresolvedMeta,
    MetaMismatch,
    UnresolvedSlotWidth,
    SlotMismatch,
}

/// # Safety
/// `chan` must be a live local queue, and the caller must provide exclusive
/// access to its state for the returned borrow.
#[inline]
pub unsafe fn local_state<'a>(chan: GcRef) -> &'a mut LocalQueueState {
    debug_assert!(
        super::queue_state::backing(chan) == QueueBacking::Local,
        "get_state called on non-LOCAL queue"
    );
    unsafe { &mut *slot_to_ptr(QueueData::as_ref(chan).state) }
}

/// # Safety
/// `chan` must be a live local queue, and its state must remain allocated for
/// the returned borrow.
#[inline]
pub unsafe fn local_state_ref<'a>(chan: GcRef) -> &'a LocalQueueState {
    debug_assert!(
        super::queue_state::backing(chan) == QueueBacking::Local,
        "get_state_ref called on non-LOCAL queue"
    );
    unsafe { &*slot_to_ptr(QueueData::as_ref(chan).state) }
}

/// Check if this channel is a REMOTE proxy.
#[inline]
pub unsafe fn is_remote(chan: GcRef) -> bool {
    super::queue_state::backing(chan) == QueueBacking::Remote
}

#[inline]
pub unsafe fn kind(chan: GcRef) -> QueueKind {
    super::queue_state::kind(chan)
}

#[inline]
pub unsafe fn is_port(chan: GcRef) -> bool {
    kind(chan) == QueueKind::Port
}

/// Get RemoteProxy for a REMOTE channel.
/// # Safety
/// `chan` must be a live remote queue for the returned borrow.
#[inline]
pub unsafe fn remote_proxy<'a>(chan: GcRef) -> &'a RemoteProxy {
    debug_assert!(is_remote(chan), "remote_proxy called on LOCAL channel");
    unsafe { &*(QueueData::as_ref(chan).endpoint_ptr as *const RemoteProxy) }
}

/// Get mutable RemoteProxy for a REMOTE channel.
/// # Safety
/// `chan` must be a live remote queue, and the caller must provide exclusive
/// access to its proxy state for the returned borrow.
#[inline]
pub unsafe fn remote_proxy_mut<'a>(chan: GcRef) -> &'a mut RemoteProxy {
    debug_assert!(is_remote(chan), "remote_proxy_mut called on LOCAL channel");
    unsafe { &mut *(QueueData::as_ref(chan).endpoint_ptr as *mut RemoteProxy) }
}

#[inline]
pub unsafe fn mark_remote_closed(chan: GcRef) {
    unsafe { remote_proxy_mut(chan).closed = true };
}

/// Get HomeInfo for a LOCAL channel that has been transferred cross-island.
/// Returns None if never transferred (endpoint_ptr == 0).
pub unsafe fn home_info<'a>(chan: GcRef) -> Option<&'a HomeInfo> {
    let data = unsafe { QueueData::as_ref(chan) };
    if super::queue_state::backing(chan) != QueueBacking::Local || data.endpoint_ptr == 0 {
        return None;
    }
    Some(unsafe { &*(data.endpoint_ptr as *const HomeInfo) })
}

/// Get mutable HomeInfo for a LOCAL channel.
pub unsafe fn home_info_mut<'a>(chan: GcRef) -> Option<&'a mut HomeInfo> {
    let data = unsafe { QueueData::as_ref(chan) };
    if super::queue_state::backing(chan) != QueueBacking::Local || data.endpoint_ptr == 0 {
        return None;
    }
    Some(unsafe { &mut *(data.endpoint_ptr as *mut HomeInfo) })
}

pub unsafe fn home_info_snapshot(chan: GcRef) -> Option<HomeInfoSnapshot> {
    unsafe { home_info(chan) }.map(|info| {
        let mut peers: Vec<u32> = info.peers.iter().copied().collect();
        peers.sort_unstable();
        HomeInfoSnapshot {
            endpoint_id: info.endpoint_id,
            home_island: info.home_island,
            peers,
        }
    })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub unsafe fn restore_home_info_snapshot(chan: GcRef, snapshot: Option<HomeInfoSnapshot>) {
    // Safety: callers restore an already-live local queue object as part of a
    // VM transaction rollback. The pointer being replaced is non-GC metadata.
    let data = unsafe { QueueData::as_mut(chan) };
    debug_assert!(
        super::queue_state::backing(chan) == QueueBacking::Local,
        "restore_home_info_snapshot on non-LOCAL channel"
    );
    if data.endpoint_ptr != 0 {
        // Safety: LOCAL endpoint_ptr always stores a Box<HomeInfo>.
        unsafe {
            drop(Box::from_raw(data.endpoint_ptr as *mut HomeInfo));
        }
        data.endpoint_ptr = 0;
    }
    if let Some(snapshot) = snapshot {
        assert!(
            kind(chan) == QueueKind::Port,
            "restore_home_info_snapshot: chan cannot cross islands"
        );
        let info = Box::new(HomeInfo {
            endpoint_id: snapshot.endpoint_id,
            home_island: snapshot.home_island,
            peers: snapshot.peers.into_iter().collect(),
        });
        data.endpoint_ptr = ptr_to_slot(Box::into_raw(info) as *mut u8);
    }
}

pub unsafe fn add_home_peer(chan: GcRef, peer_island: u32) -> Result<u64, QueueEndpointError> {
    let info = unsafe { home_info_mut(chan) }.ok_or(QueueEndpointError::MissingHomeInfo)?;
    if !info.peers.contains(&peer_island) {
        info.peers
            .try_reserve(1)
            .map_err(|_| QueueEndpointError::AllocationFailed)?;
        info.peers.insert(peer_island);
    }
    Ok(info.endpoint_id)
}

pub unsafe fn home_info_undo(chan: GcRef, peer_island: u32) -> HomeInfoUndo {
    let info = unsafe { home_info(chan) };
    HomeInfoUndo {
        remove_home_info: info.is_none(),
        peer_island,
        remove_peer: info.is_some_and(|info| !info.peers.contains(&peer_island)),
    }
}

pub unsafe fn restore_home_info_undo(chan: GcRef, undo: HomeInfoUndo) {
    let data = unsafe { QueueData::as_mut(chan) };
    if undo.remove_home_info {
        if data.endpoint_ptr != 0 {
            unsafe { drop(Box::from_raw(data.endpoint_ptr as *mut HomeInfo)) };
            data.endpoint_ptr = 0;
        }
    } else if undo.remove_peer {
        if let Some(info) = unsafe { home_info_mut(chan) } {
            info.peers.remove(&undo.peer_island);
        }
    }
}

/// Install HomeInfo on a LOCAL channel for first-time cross-island transfer.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub unsafe fn install_home_info(chan: GcRef, endpoint_id: u64, home_island: u32) {
    unsafe { try_install_home_info(chan, endpoint_id, home_island) }
        .expect("HomeInfo allocation failed");
}

pub unsafe fn try_install_home_info(
    chan: GcRef,
    endpoint_id: u64,
    home_island: u32,
) -> Result<(), QueueEndpointError> {
    // Safety: `chan` is a live local queue object. The mutation installs a
    // non-GC HomeInfo pointer and does not publish GC-visible references.
    let data = unsafe { QueueData::as_mut(chan) };
    debug_assert!(
        super::queue_state::backing(chan) == QueueBacking::Local,
        "install_home_info on non-LOCAL channel"
    );
    debug_assert!(data.endpoint_ptr == 0, "HomeInfo already installed");
    assert!(
        kind(chan) == QueueKind::Port,
        "install_home_info: chan cannot cross islands"
    );
    let info = try_box(HomeInfo {
        endpoint_id,
        home_island,
        peers: hashbrown::HashSet::new(),
    })
    .map_err(|_| QueueEndpointError::AllocationFailed)?;
    data.endpoint_ptr = ptr_to_slot(Box::into_raw(info) as *mut u8);
    Ok(())
}

/// Get channel metadata for cross-island transfer.
pub unsafe fn get_metadata(chan: GcRef) -> (QueueKind, u64, ValueMeta, ValueRttid, u16) {
    let data = unsafe { QueueData::as_ref(chan) };
    (
        kind(chan),
        data.cap,
        data.elem_meta,
        ValueRttid::from_raw(data.elem_rttid),
        data.elem_slots,
    )
}

/// Access channel state via closure. LOCAL backing only.
#[inline]
pub unsafe fn with_local_state<T, F: FnOnce(&mut LocalQueueState) -> T>(chan: GcRef, f: F) -> T {
    unsafe { f(local_state(chan)) }
}

#[inline]
pub unsafe fn next_remote_direct_receiver(chan: GcRef, local_island: u32) -> Option<QueueWaiter> {
    if is_remote(chan) {
        return None;
    }
    unsafe { local_state(chan) }
        .waiting_receivers
        .front()
        .filter(|receiver| {
            receiver.island_id() != local_island || receiver.endpoint_wait_key().is_some()
        })
        .cloned()
}

#[inline]
pub unsafe fn next_local_select_recv_receiver(
    chan: GcRef,
    local_island: u32,
) -> Option<QueueWaiter> {
    if is_remote(chan) {
        return None;
    }
    let state = unsafe { local_state(chan) };
    if state.is_closed() {
        return None;
    }
    state
        .waiting_receivers
        .front()
        .filter(|receiver| receiver.is_local_select_recv(local_island))
        .cloned()
}

#[inline]
pub unsafe fn next_recv_endpoint_sender(chan: GcRef) -> Option<QueueWaiter> {
    if is_remote(chan) {
        return None;
    }
    unsafe { local_state(chan) }
        .waiting_senders
        .front()
        .map(|(sender, _)| sender)
        .filter(|sender| sender.endpoint_wait_key().is_some())
        .cloned()
}

#[inline]
pub unsafe fn has_endpoint_waiters(chan: GcRef) -> bool {
    if is_remote(chan) {
        return false;
    }
    let state = unsafe { local_state(chan) };
    state
        .waiting_receivers
        .iter()
        .any(|waiter| waiter.endpoint_wait_key().is_some())
        || state
            .waiting_senders
            .iter()
            .any(|(waiter, _)| waiter.endpoint_wait_key().is_some())
}

#[inline]
pub unsafe fn len(chan: GcRef) -> usize {
    if is_remote(chan) {
        return 0;
    }
    unsafe { local_state(chan) }.len()
}
#[inline]
pub unsafe fn is_closed(chan: GcRef) -> bool {
    if is_remote(chan) {
        return unsafe { remote_proxy(chan) }.closed;
    }
    unsafe { local_state(chan) }.is_closed()
}
#[inline]
pub unsafe fn close(chan: GcRef) {
    debug_assert!(
        !is_remote(chan),
        "close called on REMOTE channel — use message passing"
    );
    unsafe { local_state(chan) }.close();
}

#[inline]
pub unsafe fn send_ready(chan: GcRef) -> bool {
    let cap = super::queue_state::capacity(chan);
    with_local_state(chan, |s| s.is_send_ready(cap))
}

#[inline]
pub unsafe fn recv_ready(chan: GcRef) -> bool {
    with_local_state(chan, |s| s.is_recv_ready())
}

#[inline]
pub unsafe fn try_send(
    chan: GcRef,
    value: impl Into<QueueMessage>,
) -> SendResult<QueueWaiter, QueueMessage> {
    let cap = super::queue_state::capacity(chan);
    with_local_state(chan, |s| s.try_send(value.into(), cap))
}

#[inline]
pub unsafe fn try_recv(chan: GcRef) -> RecvResult<QueueWaiter, QueueMessage> {
    with_local_state(chan, |s| s.try_recv())
}

#[inline]
pub unsafe fn register_sender(chan: GcRef, waiter: QueueWaiter, value: impl Into<QueueMessage>) {
    with_local_state(chan, |s| s.register_sender(waiter, value.into()))
}

#[inline]
pub unsafe fn register_receiver(chan: GcRef, waiter: QueueWaiter) {
    with_local_state(chan, |s| s.register_receiver(waiter))
}

#[inline]
pub unsafe fn restore_direct_receiver(chan: GcRef, waiter: QueueWaiter) {
    with_local_state(chan, |s| s.restore_direct_receiver(waiter))
}

#[inline]
pub unsafe fn cancel_select_waiters(chan: GcRef, fiber_key: u64, select_id: u64) {
    with_local_state(chan, |s| s.cancel_select_waiters(fiber_key, select_id))
}

#[inline]
pub unsafe fn cancel_simple_waiter(
    chan: GcRef,
    fiber_key: u64,
    kind: crate::objects::queue_state::SelectWaitKind,
) {
    with_local_state(chan, |s| s.cancel_simple_waiter(fiber_key, kind))
}

/// Atomic send: try to send, if would block, register waiter in same operation.
pub unsafe fn send_or_block(
    chan: GcRef,
    value: impl Into<QueueMessage>,
    waiter: QueueWaiter,
) -> BlockingSendResult<QueueWaiter, QueueMessage> {
    let cap = super::queue_state::capacity(chan);
    with_local_state(chan, |s| s.send_or_block(value.into(), cap, waiter))
}

#[inline]
pub unsafe fn send_or_block_resolved(
    chan: GcRef,
    value: QueueMessage,
    waiter: QueueWaiter,
    local_island: u32,
) -> ResolvedSendResult<QueueWaiter, QueueMessage> {
    let cap = super::queue_state::capacity(chan);
    with_local_state(chan, |s| {
        s.send_or_block_resolved(value, cap, waiter, local_island)
    })
}

#[inline]
pub unsafe fn try_send_or_block_resolved_with<E, F>(
    chan: GcRef,
    value: QueueMessage,
    make_waiter: F,
    local_island: u32,
) -> Result<ResolvedSendResult<QueueWaiter, QueueMessage>, E>
where
    F: FnOnce() -> Result<QueueWaiter, E>,
{
    let cap = super::queue_state::capacity(chan);
    with_local_state(chan, |state| {
        state.try_send_or_block_resolved_with(value, cap, make_waiter, local_island)
    })
}

/// Atomic recv: try to receive, if would block, register waiter in same operation.
pub unsafe fn recv_or_block(
    chan: GcRef,
    waiter: QueueWaiter,
) -> BlockingRecvResult<QueueWaiter, QueueMessage> {
    with_local_state(chan, |s| s.recv_or_block(waiter))
}

#[inline]
pub unsafe fn try_recv_or_block_with<E, F>(
    chan: GcRef,
    make_waiter: F,
) -> Result<BlockingRecvResult<QueueWaiter, QueueMessage>, E>
where
    F: FnOnce() -> Result<QueueWaiter, E>,
{
    with_local_state(chan, |state| state.try_recv_or_block_with(make_waiter))
}

/// Take all waiting receivers (for close notification).
pub unsafe fn take_waiting_receivers(chan: GcRef) -> Vec<QueueWaiter> {
    with_local_state(chan, |s| s.take_waiting_receivers())
}

/// Take all waiting senders (for close notification).
pub unsafe fn take_waiting_senders(chan: GcRef) -> Vec<(QueueWaiter, QueueMessage)> {
    with_local_state(chan, |s| s.take_waiting_senders())
}

/// # Safety
/// chan must be a valid Channel GcRef.
pub unsafe fn drop_inner(chan: GcRef) {
    // Safety: `chan` is a valid queue object owned by the GC finalization path.
    let data = unsafe { QueueData::as_mut(chan) };
    match QueueBacking::from_raw(data.backing) {
        QueueBacking::Local => {
            if data.state != 0 {
                drop(Box::from_raw(slot_to_ptr::<LocalQueueState>(data.state)));
                data.state = 0;
            }
            if data.endpoint_ptr != 0 {
                drop(Box::from_raw(data.endpoint_ptr as *mut HomeInfo));
                data.endpoint_ptr = 0;
            }
        }
        QueueBacking::Remote => {
            // REMOTE channels have no ChannelState (state == 0)
            if data.endpoint_ptr != 0 {
                drop(Box::from_raw(data.endpoint_ptr as *mut RemoteProxy));
                data.endpoint_ptr = 0;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::queue::{is_port, is_remote, remote_proxy};
    use vo_common_core::{RuntimeType, SlotType, StructMeta, ValueKind};

    #[test]
    fn explicit_queue_creation_reports_oom_without_publishing_abi_state() {
        let mut gc = Gc::with_memory_config(crate::gc::VmMemoryConfig {
            max_objects: Some(0),
            ..crate::gc::VmMemoryConfig::default()
        })
        .expect("bounded GC");
        let meta = ValueMeta::new(0, ValueKind::Int64);
        let rttid = ValueRttid::new(0, ValueKind::Int64);

        assert_eq!(
            try_create(&mut gc, QueueKind::Chan, meta, rttid, 1, 0),
            Err(MemoryError::MetadataExhausted)
        );
        assert_eq!(gc.last_memory_error(), None);
    }

    #[test]
    fn remote_proxy_is_always_a_port() {
        let mut gc = Gc::new();
        let port = create_remote_proxy_with_closed(
            &mut gc,
            7,
            9,
            4,
            ValueMeta::new(0, ValueKind::Int64),
            ValueRttid::new(0, ValueKind::Int64),
            1,
            true,
        );
        assert!(is_remote(port));
        assert!(is_port(port));
        assert!(remote_proxy(port).closed);
        assert_eq!(remote_proxy(port).endpoint_id, 7);
        assert_eq!(remote_proxy(port).home_island, 9);
    }

    #[test]
    fn create_checked_rejects_zero_capacity_port() {
        let mut gc = Gc::new();
        let result = create_checked(
            &mut gc,
            QueueKind::Port,
            ValueMeta::new(0, ValueKind::Int64),
            ValueRttid::new(0, ValueKind::Int64),
            1,
            0,
        );
        assert_eq!(result, Err(crate::objects::alloc_error::NEGATIVE_CAP));
    }

    #[test]
    fn checked_queue_capacity_rejects_host_layout_overflow_before_allocation() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(0, ValueKind::Int64);
        let rttid = ValueRttid::new(0, ValueKind::Int64);
        let cap = i64::MAX / 2 + 1;

        assert_eq!(
            try_create_checked(&mut gc, QueueKind::Chan, meta, rttid, 1, cap),
            Err(QueueCreateError::Invalid(
                crate::objects::alloc_error::OVERFLOW
            ))
        );
        assert_eq!(gc.last_memory_error(), None);
    }

    #[test]
    fn vm_queue_new_type_layout_009_rejects_meta_rttid_drift_before_create() {
        let mut module = Module::new("queue-new-meta-rttid-drift".to_string());
        module.struct_metas.push(StructMeta {
            slot_types: vec![SlotType::Value],
            fields: Vec::new(),
            field_index: Default::default(),
        });
        module.struct_metas.push(StructMeta {
            slot_types: vec![SlotType::Value, SlotType::GcRef],
            fields: Vec::new(),
            field_index: Default::default(),
        });
        module.runtime_types.push(RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 1,
        });
        let mut gc = Gc::new();

        let result = create_checked_with_module(
            &mut gc,
            QueueKind::Chan,
            ValueMeta::new(0, ValueKind::Struct),
            ValueRttid::new(0, ValueKind::Struct),
            2,
            1,
            &module,
        );

        assert_eq!(result, Err(crate::objects::alloc_error::OVERFLOW));
    }

    #[test]
    fn vm_queue_new_type_layout_009_rejects_elem_slot_drift_before_create() {
        let mut module = Module::new("queue-new-slot-drift".to_string());
        module.struct_metas.push(StructMeta {
            slot_types: vec![SlotType::Value, SlotType::GcRef],
            fields: Vec::new(),
            field_index: Default::default(),
        });
        module.runtime_types.push(RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        });
        let mut gc = Gc::new();

        let result = create_checked_with_module(
            &mut gc,
            QueueKind::Chan,
            ValueMeta::new(0, ValueKind::Struct),
            ValueRttid::new(0, ValueKind::Struct),
            1,
            1,
            &module,
        );

        assert_eq!(result, Err(crate::objects::alloc_error::OVERFLOW));
    }
}
