#![allow(clippy::missing_safety_doc)]
//! Generic queue state for channel objects.
//!
//! This module provides:
//! - Unified QueueData structure for all channel objects
//! - Unified accessors: elem_meta, elem_slots, capacity, len, close, is_closed
//! - Generic QueueState<W, M> state machine
//!
//! # Safety contract
//! Unsafe raw accessors require a canonical live queue allocation; backing-
//! specific state access must agree with `QueueData::backing`.

#[cfg(not(feature = "std"))]
use alloc::{
    boxed::Box,
    collections::{TryReserveError, VecDeque},
    vec::Vec,
};

#[cfg(feature = "std")]
use std::{
    boxed::Box,
    collections::{TryReserveError, VecDeque},
    vec::Vec,
};

use core::{
    num::NonZeroU64,
    sync::atomic::{AtomicUsize, Ordering},
};
use hashbrown::HashSet;

use crate::gc::{Gc, GcRef, MemoryError};
use crate::island::EndpointWaitKey;
use crate::slot::{slot_to_usize, Slot, SLOT_BYTES};
use vo_common_core::types::{ValueKind, ValueMeta, ValueRttid};

static QUEUE_WAIT_REGISTRATION_COUNTER: AtomicUsize = AtomicUsize::new(1);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueueWaiterError {
    RegistrationExhausted,
    ZeroQueueRef,
}

impl core::fmt::Display for QueueWaiterError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::RegistrationExhausted => {
                f.write_str("queue wait registration identity space exhausted")
            }
            Self::ZeroQueueRef => f.write_str("queue waiter requires a non-zero queue reference"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for QueueWaiterError {}

/// Allocates a process-unique, non-zero queue waiter identity.
///
/// Zero is a permanent exhausted sentinel.  Publishing it with the successful
/// allocation of `usize::MAX` prevents every later caller from reusing an old
/// identity, including when several threads race at the boundary.
fn next_queue_wait_registration_id_from(counter: &AtomicUsize) -> Result<u64, QueueWaiterError> {
    let mut current = counter.load(Ordering::Relaxed);
    loop {
        if current == 0 {
            return Err(QueueWaiterError::RegistrationExhausted);
        }
        let next = current.checked_add(1).unwrap_or(0);
        match counter.compare_exchange_weak(current, next, Ordering::Relaxed, Ordering::Relaxed) {
            Ok(_) => return Ok(current as u64),
            Err(observed) => current = observed,
        }
    }
}

fn next_queue_wait_registration_id() -> Result<NonZeroU64, QueueWaiterError> {
    let registration_id = next_queue_wait_registration_id_from(&QUEUE_WAIT_REGISTRATION_COUNTER)?;
    NonZeroU64::new(registration_id).ok_or(QueueWaiterError::RegistrationExhausted)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum QueueBacking {
    Local = 0,
    Remote = 1,
}

impl QueueBacking {
    #[inline]
    pub fn from_raw(raw: u16) -> Self {
        match raw {
            0 => Self::Local,
            1 => Self::Remote,
            other => panic!("QueueBacking::from_raw: invalid queue backing {}", other),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum QueueKind {
    Chan = 0,
    Port = 1,
}

impl QueueKind {
    #[inline]
    pub fn from_raw(raw: u16) -> Self {
        match raw {
            0 => Self::Chan,
            1 => Self::Port,
            other => panic!("QueueKind::from_raw: invalid queue kind {}", other),
        }
    }

    #[inline]
    pub fn value_kind(self) -> ValueKind {
        match self {
            Self::Chan => ValueKind::Channel,
            Self::Port => ValueKind::Port,
        }
    }

    #[inline]
    pub fn from_value_kind(kind: ValueKind) -> Self {
        match kind {
            ValueKind::Channel => Self::Chan,
            ValueKind::Port => Self::Port,
            other => panic!(
                "QueueKind::from_value_kind: expected channel/port kind, got {:?}",
                other
            ),
        }
    }
}

/// Unified data structure for all channel objects.
/// Layout: GcHeader + QueueData
#[repr(C)]
pub struct QueueData {
    pub state: Slot,
    pub cap: Slot,
    pub elem_meta: ValueMeta, // 4 bytes
    pub elem_slots: u16,      // 2 bytes
    /// Channel backing kind: BACKING_LOCAL (0) or BACKING_REMOTE (1).
    pub backing: u16, // 2 bytes
    pub kind: u16,
    pub reserved: u16,
    pub elem_rttid: u32,
    /// Pointer to HomeInfo (LOCAL) or RemoteProxy (REMOTE).
    /// 0 if unused (LOCAL channel that has never been transferred cross-island).
    pub endpoint_ptr: Slot,
}

pub const DATA_SLOTS: u16 = 5;
const _: () = assert!(core::mem::size_of::<QueueData>() == DATA_SLOTS as usize * SLOT_BYTES);

impl_gc_object!(QueueData);

// =============================================================================
// Unified accessors (capacity, elem_meta, elem_slots work for both)
// len/close/is_closed are in channel.rs
// =============================================================================

#[inline]
pub unsafe fn capacity(q: GcRef) -> usize {
    slot_to_usize(unsafe { QueueData::as_ref(q) }.cap)
}

#[inline]
pub unsafe fn elem_meta(q: GcRef) -> ValueMeta {
    unsafe { QueueData::as_ref(q) }.elem_meta
}

#[inline]
pub unsafe fn elem_slots(q: GcRef) -> u16 {
    unsafe { QueueData::as_ref(q) }.elem_slots
}

#[inline]
pub unsafe fn elem_rttid(q: GcRef) -> ValueRttid {
    ValueRttid::from_raw(unsafe { QueueData::as_ref(q) }.elem_rttid)
}

#[inline]
pub unsafe fn kind(q: GcRef) -> QueueKind {
    QueueKind::from_raw(unsafe { QueueData::as_ref(q) }.kind)
}

#[inline]
pub unsafe fn backing(q: GcRef) -> QueueBacking {
    QueueBacking::from_raw(unsafe { QueueData::as_ref(q) }.backing)
}

// =============================================================================
// Type aliases for channel states
// =============================================================================

/// Queue payload storage.
///
/// Guest sends use `Managed`: payload slots live in a runtime-backing object
/// owned and charged by the Island heap. `Owned` remains available at
/// transport/test boundaries where bytes arrive before a destination Island
/// is selected; production queue insertion copies it into `Managed` storage.
#[derive(Debug)]
pub enum QueueMessage {
    Managed { backing: GcRef, len: usize },
    Owned(Box<[u64]>),
}

impl QueueMessage {
    pub fn managed(gc: &mut Gc, slots: &[u64]) -> Result<Self, MemoryError> {
        if slots.is_empty() {
            return Ok(Self::Managed {
                backing: core::ptr::null_mut(),
                len: 0,
            });
        }
        let backing = gc.alloc_runtime_backing(slots.len());
        if backing.is_null() {
            return Err(gc
                .last_memory_error()
                .unwrap_or(MemoryError::SystemAllocationFailed));
        }
        unsafe {
            core::ptr::copy_nonoverlapping(slots.as_ptr(), backing, slots.len());
        }
        Ok(Self::Managed {
            backing,
            len: slots.len(),
        })
    }

    #[inline]
    pub fn backing_ref(&self) -> Option<GcRef> {
        match self {
            Self::Managed { backing, .. } if !backing.is_null() => Some(*backing),
            Self::Managed { .. } | Self::Owned(_) => None,
        }
    }

    pub fn into_vec(self) -> Vec<u64> {
        self.as_ref().to_vec()
    }
}

impl Clone for QueueMessage {
    fn clone(&self) -> Self {
        match self {
            Self::Managed { backing, len } => Self::Managed {
                backing: *backing,
                len: *len,
            },
            Self::Owned(slots) => Self::Owned(slots.clone()),
        }
    }
}

impl PartialEq for QueueMessage {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl Eq for QueueMessage {}

impl AsRef<[u64]> for QueueMessage {
    fn as_ref(&self) -> &[u64] {
        match self {
            Self::Managed { backing, len } => {
                if *len == 0 {
                    &[]
                } else {
                    unsafe { core::slice::from_raw_parts(*backing, *len) }
                }
            }
            Self::Owned(slots) => slots,
        }
    }
}

impl core::ops::Deref for QueueMessage {
    type Target = [u64];

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl From<Box<[u64]>> for QueueMessage {
    fn from(value: Box<[u64]>) -> Self {
        Self::Owned(value)
    }
}

impl From<Vec<u64>> for QueueMessage {
    fn from(value: Vec<u64>) -> Self {
        Self::Owned(value.into_boxed_slice())
    }
}

impl From<&[u64]> for QueueMessage {
    fn from(value: &[u64]) -> Self {
        Self::Owned(value.into())
    }
}

/// Copyable projection of a select waiter target.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SelectInfo {
    pub case_index: u16,
    pub select_id: u64,
    pub queue_ref: u64,
    pub kind: SelectWaitKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SelectWaitKind {
    Send,
    Recv,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum QueueWaitTarget {
    Queue {
        registration_id: NonZeroU64,
        queue_ref: NonZeroU64,
        kind: SelectWaitKind,
    },
    Select {
        case_index: u16,
        select_id: u64,
        queue_ref: NonZeroU64,
        kind: SelectWaitKind,
    },
    Endpoint {
        wait_id: NonZeroU64,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QueueWaiter {
    island_id: u32,
    fiber_key: u64,
    target: QueueWaitTarget,
}

impl QueueWaiter {
    #[inline]
    pub fn island_id(&self) -> u32 {
        self.island_id
    }

    #[inline]
    pub fn fiber_key(&self) -> u64 {
        self.fiber_key
    }

    #[inline]
    pub fn target(&self) -> &QueueWaitTarget {
        &self.target
    }

    #[inline]
    pub fn registration_id(&self) -> Option<NonZeroU64> {
        match self.target {
            QueueWaitTarget::Queue {
                registration_id, ..
            } => Some(registration_id),
            QueueWaitTarget::Select { .. } | QueueWaitTarget::Endpoint { .. } => None,
        }
    }

    #[inline]
    pub fn endpoint_wait_key(&self) -> Option<EndpointWaitKey> {
        match self.target {
            QueueWaitTarget::Endpoint { wait_id } => {
                Some(EndpointWaitKey::new(self.fiber_key, wait_id))
            }
            QueueWaitTarget::Queue { .. } | QueueWaitTarget::Select { .. } => None,
        }
    }

    #[inline]
    pub fn queue_identity(&self) -> Option<(u64, SelectWaitKind)> {
        match self.target {
            QueueWaitTarget::Queue {
                queue_ref, kind, ..
            }
            | QueueWaitTarget::Select {
                queue_ref, kind, ..
            } => Some((queue_ref.get(), kind)),
            QueueWaitTarget::Endpoint { .. } => None,
        }
    }

    #[inline]
    pub fn select_info(&self) -> Option<SelectInfo> {
        match self.target {
            QueueWaitTarget::Select {
                case_index,
                select_id,
                queue_ref,
                kind,
                ..
            } => Some(SelectInfo {
                case_index,
                select_id,
                queue_ref: queue_ref.get(),
                kind,
            }),
            QueueWaitTarget::Queue { .. } | QueueWaitTarget::Endpoint { .. } => None,
        }
    }

    #[inline]
    pub fn try_queue(
        island_id: u32,
        fiber_key: u64,
        queue_ref: u64,
        kind: SelectWaitKind,
    ) -> Result<Self, QueueWaiterError> {
        let queue_ref = NonZeroU64::new(queue_ref).ok_or(QueueWaiterError::ZeroQueueRef)?;
        Ok(Self {
            island_id,
            fiber_key,
            target: QueueWaitTarget::Queue {
                registration_id: next_queue_wait_registration_id()?,
                queue_ref,
                kind,
            },
        })
    }

    #[inline]
    pub fn endpoint(island_id: u32, wait_key: EndpointWaitKey) -> Self {
        Self {
            island_id,
            fiber_key: wait_key.fiber_key(),
            target: QueueWaitTarget::Endpoint {
                wait_id: wait_key.wait_id(),
            },
        }
    }

    #[inline]
    pub fn try_select(
        island_id: u32,
        fiber_key: u64,
        case_index: u16,
        select_id: u64,
        queue_ref: u64,
        kind: SelectWaitKind,
    ) -> Result<Self, QueueWaiterError> {
        let queue_ref = NonZeroU64::new(queue_ref).ok_or(QueueWaiterError::ZeroQueueRef)?;
        Ok(Self {
            island_id,
            fiber_key,
            target: QueueWaitTarget::Select {
                case_index,
                select_id,
                queue_ref,
                kind,
            },
        })
    }

    #[inline]
    pub fn is_select_for(&self, fiber_key: u64, select_id: u64) -> bool {
        self.fiber_key == fiber_key
            && self
                .select_info()
                .is_some_and(|info| info.select_id == select_id)
    }

    #[inline]
    pub fn is_local_select_recv(&self, local_island: u32) -> bool {
        self.island_id == local_island
            && self
                .select_info()
                .is_some_and(|info| info.kind == SelectWaitKind::Recv)
    }
}

pub type LocalQueueState = QueueState<QueueWaiter, QueueMessage>;

pub use crate::pack::PackedValue;

// =============================================================================
// Channel backing metadata (stored behind QueueData::endpoint_ptr)
// =============================================================================

/// Metadata for a BACKING_LOCAL channel that has been transferred cross-island.
/// Stored as Box<HomeInfo> behind endpoint_ptr on the home island.
pub struct HomeInfo {
    /// Unique endpoint ID for this channel (allocated by home island).
    pub endpoint_id: u64,
    /// Island ID of the home island (where ChannelState lives).
    pub home_island: u32,
    /// Set of island IDs that hold remote proxies to this channel.
    pub peers: HashSet<u32>,
}

/// Metadata for a BACKING_REMOTE proxy channel.
/// Stored as Box<RemoteProxy> behind endpoint_ptr on the remote island.
pub struct RemoteProxy {
    /// Endpoint ID matching the HomeInfo on the home island.
    pub endpoint_id: u64,
    /// Island ID where the real ChannelState lives.
    pub home_island: u32,
    /// Set to true when close notification received from home.
    pub closed: bool,
}

/// Result of a send operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SendResult<W, M> {
    /// Value sent directly to a waiting receiver (receiver woken).
    DirectSend { receiver: W, payload: M },
    /// Value buffered successfully.
    Buffered,
    /// Would block - buffer full, no receivers. Returns the value back.
    WouldBlock(M),
    /// Channel is closed.
    Closed,
}

/// Result of an atomic send that registers `waiter` when immediate progress is
/// unavailable. Its type excludes the impossible `WouldBlock` state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlockingSendResult<W, M> {
    DirectSend { receiver: W, payload: M },
    Buffered,
    Blocked(W),
    Closed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedSendResult<W, M> {
    Wake { receiver: W, payload: Option<M> },
    RemoteDirect { receiver: W, payload: M },
    Buffered,
    Blocked(W),
    Closed,
}

/// Result of a receive operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecvResult<W, M> {
    /// Successfully received a payload and optionally woke a sender.
    Success { woke_sender: Option<W>, payload: M },
    /// Would block - buffer empty, no senders.
    WouldBlock,
    /// Channel is closed.
    Closed,
}

/// Result of an atomic receive that registers `waiter` when immediate progress
/// is unavailable. Its type excludes the impossible `WouldBlock` state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlockingRecvResult<W, M> {
    Success { woke_sender: Option<W>, payload: M },
    Blocked(W),
    Closed,
}

/// Generic queue state for channel-like communication.
///
/// Type parameters:
/// - `W`: Waiter identifier type
/// - `M`: Message type (e.g., `Box<[u64]>` for Channel)
#[derive(Debug, Clone)]
pub struct QueueState<W, M> {
    pub buffer: VecDeque<M>,
    pub closed: bool,
    pub waiting_senders: VecDeque<(W, M)>,
    pub waiting_receivers: VecDeque<W>,
}

impl<W, M> QueueState<W, M> {
    pub fn new(cap: usize) -> Self {
        Self::try_new(cap).expect("queue capacity allocation failed")
    }

    pub fn try_new(cap: usize) -> Result<Self, TryReserveError> {
        let mut buffer = VecDeque::new();
        buffer.try_reserve_exact(cap)?;
        Ok(Self {
            buffer,
            closed: false,
            waiting_senders: VecDeque::new(),
            waiting_receivers: VecDeque::new(),
        })
    }

    pub fn is_send_ready(&self, cap: usize) -> bool {
        self.closed || !self.waiting_receivers.is_empty() || self.buffer.len() < cap
    }

    pub fn is_recv_ready(&self) -> bool {
        !self.buffer.is_empty() || !self.waiting_senders.is_empty() || self.closed
    }

    /// Try to send a value. Returns the value back if immediate progress is unavailable.
    pub fn try_send(&mut self, value: M, cap: usize) -> SendResult<W, M> {
        if self.closed {
            return SendResult::Closed;
        }
        if let Some(receiver) = self.waiting_receivers.pop_front() {
            return SendResult::DirectSend {
                receiver,
                payload: value,
            };
        }
        if self.buffer.len() < cap {
            self.buffer.push_back(value);
            return SendResult::Buffered;
        }
        SendResult::WouldBlock(value)
    }

    /// Try to receive a value.
    pub fn try_recv(&mut self) -> RecvResult<W, M> {
        if let Some(value) = self.buffer.pop_front() {
            let woke_sender = if let Some((sender, sender_value)) = self.waiting_senders.pop_front()
            {
                self.buffer.push_back(sender_value);
                Some(sender)
            } else {
                None
            };
            return RecvResult::Success {
                woke_sender,
                payload: value,
            };
        }
        if let Some((sender, value)) = self.waiting_senders.pop_front() {
            return RecvResult::Success {
                woke_sender: Some(sender),
                payload: value,
            };
        }
        if self.closed {
            RecvResult::Closed
        } else {
            RecvResult::WouldBlock
        }
    }

    /// Atomic send: try to send, then create and register a waiter only when
    /// immediate progress is unavailable.
    pub fn try_send_or_block_with<E, F>(
        &mut self,
        value: M,
        cap: usize,
        make_waiter: F,
    ) -> Result<BlockingSendResult<W, M>, E>
    where
        W: Clone,
        F: FnOnce() -> Result<W, E>,
    {
        Ok(match self.try_send(value, cap) {
            SendResult::DirectSend { receiver, payload } => {
                BlockingSendResult::DirectSend { receiver, payload }
            }
            SendResult::Buffered => BlockingSendResult::Buffered,
            SendResult::WouldBlock(value) => {
                let waiter = make_waiter()?;
                self.waiting_senders.push_back((waiter.clone(), value));
                BlockingSendResult::Blocked(waiter)
            }
            SendResult::Closed => BlockingSendResult::Closed,
        })
    }

    /// Eager compatibility wrapper for callers that already own a waiter.
    pub fn send_or_block(&mut self, value: M, cap: usize, waiter: W) -> BlockingSendResult<W, M>
    where
        W: Clone,
    {
        match self.try_send_or_block_with(value, cap, || Ok::<W, core::convert::Infallible>(waiter))
        {
            Ok(result) => result,
            Err(never) => match never {},
        }
    }

    /// Atomic receive: try to receive, then create and register a waiter only
    /// when immediate progress is unavailable.
    pub fn try_recv_or_block_with<E, F>(
        &mut self,
        make_waiter: F,
    ) -> Result<BlockingRecvResult<W, M>, E>
    where
        W: Clone,
        F: FnOnce() -> Result<W, E>,
    {
        Ok(match self.try_recv() {
            RecvResult::Success {
                woke_sender,
                payload,
            } => BlockingRecvResult::Success {
                woke_sender,
                payload,
            },
            RecvResult::WouldBlock => {
                let waiter = make_waiter()?;
                self.waiting_receivers.push_back(waiter.clone());
                BlockingRecvResult::Blocked(waiter)
            }
            RecvResult::Closed => BlockingRecvResult::Closed,
        })
    }

    /// Eager compatibility wrapper for callers that already own a waiter.
    pub fn recv_or_block(&mut self, waiter: W) -> BlockingRecvResult<W, M>
    where
        W: Clone,
    {
        match self.try_recv_or_block_with(|| Ok::<W, core::convert::Infallible>(waiter)) {
            Ok(result) => result,
            Err(never) => match never {},
        }
    }

    pub fn register_sender(&mut self, waiter: W, value: M) {
        self.waiting_senders.push_back((waiter, value));
    }

    pub fn register_receiver(&mut self, waiter: W) {
        self.waiting_receivers.push_back(waiter);
    }

    pub fn restore_direct_receiver(&mut self, waiter: W) {
        self.waiting_receivers.push_front(waiter);
    }

    pub fn close(&mut self) {
        self.closed = true;
    }

    pub fn is_closed(&self) -> bool {
        self.closed
    }

    pub(crate) fn len(&self) -> usize {
        self.buffer.len()
    }

    pub fn take_waiting_receivers(&mut self) -> Vec<W> {
        self.waiting_receivers.drain(..).collect()
    }

    pub fn take_waiting_senders(&mut self) -> Vec<(W, M)> {
        self.waiting_senders.drain(..).collect()
    }
}

// =============================================================================
// ChannelState-specific methods for select cancellation
// =============================================================================

impl<M> QueueState<QueueWaiter, M> {
    pub fn cancel_simple_waiter(&mut self, fiber_key: u64, kind: SelectWaitKind) {
        match kind {
            SelectWaitKind::Send => {
                self.waiting_senders.retain(|(waiter, _)| {
                    waiter.fiber_key() != fiber_key
                        || !matches!(
                            waiter.target(),
                            QueueWaitTarget::Queue {
                                kind: SelectWaitKind::Send,
                                ..
                            }
                        )
                });
            }
            SelectWaitKind::Recv => {
                self.waiting_receivers.retain(|waiter| {
                    waiter.fiber_key() != fiber_key
                        || !matches!(
                            waiter.target(),
                            QueueWaitTarget::Queue {
                                kind: SelectWaitKind::Recv,
                                ..
                            }
                        )
                });
            }
        }
    }

    pub fn try_send_or_block_resolved_with<E, F>(
        &mut self,
        value: M,
        cap: usize,
        make_waiter: F,
        local_island: u32,
    ) -> Result<ResolvedSendResult<QueueWaiter, M>, E>
    where
        F: FnOnce() -> Result<QueueWaiter, E>,
    {
        Ok(
            match self.try_send_or_block_with(value, cap, make_waiter)? {
                BlockingSendResult::DirectSend { receiver, payload } => {
                    if receiver.endpoint_wait_key().is_none()
                        && receiver.island_id() == local_island
                    {
                        if receiver
                            .select_info()
                            .is_some_and(|select| select.kind == SelectWaitKind::Recv)
                        {
                            ResolvedSendResult::Wake {
                                receiver,
                                payload: Some(payload),
                            }
                        } else {
                            // A simple local receiver replays the ordinary receive path,
                            // so publish the payload to the queue before waking it.
                            self.buffer.push_back(payload);
                            ResolvedSendResult::Wake {
                                receiver,
                                payload: None,
                            }
                        }
                    } else {
                        ResolvedSendResult::RemoteDirect { receiver, payload }
                    }
                }
                BlockingSendResult::Buffered => ResolvedSendResult::Buffered,
                BlockingSendResult::Blocked(waiter) => ResolvedSendResult::Blocked(waiter),
                BlockingSendResult::Closed => ResolvedSendResult::Closed,
            },
        )
    }

    pub fn send_or_block_resolved(
        &mut self,
        value: M,
        cap: usize,
        waiter: QueueWaiter,
        local_island: u32,
    ) -> ResolvedSendResult<QueueWaiter, M> {
        match self.try_send_or_block_resolved_with(
            value,
            cap,
            || Ok::<QueueWaiter, core::convert::Infallible>(waiter),
            local_island,
        ) {
            Ok(result) => result,
            Err(never) => match never {},
        }
    }

    /// Cancel all select waiters with the given select_id.
    /// Called when a select completes (one case became ready) to remove
    /// this fiber from all other channels it was waiting on.
    pub fn cancel_select_waiters(&mut self, fiber_key: u64, select_id: u64) {
        self.waiting_receivers
            .retain(|w| !w.is_select_for(fiber_key, select_id));
        self.waiting_senders
            .retain(|(w, _)| !w.is_select_for(fiber_key, select_id));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::panic::{catch_unwind, AssertUnwindSafe};
    use std::sync::Arc;

    // Simple waiter type for unit tests (avoids depending on QueueWaiter).
    type TestQueue = QueueState<u32, Vec<u64>>;

    #[test]
    fn queue_wait_registration_exhaustion_is_permanent_and_non_aliasing() {
        let counter = AtomicUsize::new(usize::MAX - 1);
        assert_eq!(
            next_queue_wait_registration_id_from(&counter),
            Ok((usize::MAX - 1) as u64)
        );
        assert_eq!(
            next_queue_wait_registration_id_from(&counter),
            Ok(usize::MAX as u64)
        );
        assert_eq!(
            next_queue_wait_registration_id_from(&counter),
            Err(QueueWaiterError::RegistrationExhausted)
        );
        assert_eq!(counter.load(Ordering::Relaxed), 0);
    }

    #[test]
    fn queue_wait_registration_boundary_race_allocates_max_once() {
        let counter = Arc::new(AtomicUsize::new(usize::MAX));
        let workers: Vec<_> = (0..8)
            .map(|_| {
                let counter = Arc::clone(&counter);
                std::thread::spawn(move || next_queue_wait_registration_id_from(&counter))
            })
            .collect();
        let results: Vec<_> = workers
            .into_iter()
            .map(|worker| worker.join().expect("allocator worker"))
            .collect();
        assert_eq!(
            results
                .iter()
                .filter(|result| **result == Ok(usize::MAX as u64))
                .count(),
            1
        );
        assert_eq!(
            results
                .iter()
                .filter(|result| **result == Err(QueueWaiterError::RegistrationExhausted))
                .count(),
            7
        );
    }

    #[test]
    fn queue_waiter_constructors_reject_zero_target_identity() {
        assert_eq!(
            QueueWaiter::try_queue(0, 1, 0, SelectWaitKind::Recv),
            Err(QueueWaiterError::ZeroQueueRef)
        );
        assert_eq!(
            QueueWaiter::try_select(0, 1, 0, 0, 0, SelectWaitKind::Recv),
            Err(QueueWaiterError::ZeroQueueRef)
        );
    }

    #[test]
    fn select_waiter_has_no_queue_registration_identity() {
        let waiter = QueueWaiter::try_select(3, 9, 1, 11, 0x1000, SelectWaitKind::Recv)
            .expect("select waiter");

        assert_eq!(waiter.registration_id(), None);
        assert!(matches!(
            waiter.target(),
            QueueWaitTarget::Select {
                case_index: 1,
                select_id: 11,
                queue_ref,
                kind: SelectWaitKind::Recv,
            } if queue_ref.get() == 0x1000
        ));
        assert!(core::mem::size_of::<QueueWaiter>() <= 40);
    }

    #[test]
    fn queue_kind_from_raw_rejects_invalid_value() {
        let result = catch_unwind(AssertUnwindSafe(|| QueueKind::from_raw(2)));
        assert!(result.is_err());
    }

    #[test]
    fn queue_backing_from_raw_rejects_invalid_value() {
        let result = catch_unwind(AssertUnwindSafe(|| QueueBacking::from_raw(2)));
        assert!(result.is_err());
    }

    #[test]
    fn is_send_ready_matches_queue_state() {
        let q = TestQueue::new(1);
        assert!(q.is_send_ready(1));

        let mut q = TestQueue::new(1);
        match q.try_send(vec![1u64], 1) {
            SendResult::Buffered => {}
            other => panic!("expected Buffered, got {:?}", other),
        }
        assert!(!q.is_send_ready(1));

        q.register_receiver(7);
        assert!(q.is_send_ready(1));

        let mut q = TestQueue::new(0);
        assert!(!q.is_send_ready(0));
        q.close();
        assert!(q.is_send_ready(0));
    }

    #[test]
    fn is_recv_ready_matches_queue_state() {
        let q = TestQueue::new(0);
        assert!(!q.is_recv_ready());

        let mut q = TestQueue::new(0);
        q.register_sender(3, vec![9u64]);
        assert!(q.is_recv_ready());

        let mut q = TestQueue::new(1);
        match q.try_send(vec![1u64], 1) {
            SendResult::Buffered => {}
            other => panic!("expected Buffered, got {:?}", other),
        }
        assert!(q.is_recv_ready());

        let mut q = TestQueue::new(0);
        q.close();
        assert!(q.is_recv_ready());
    }

    #[test]
    fn direct_send_returns_payload_without_buffer_mutation() {
        let mut q = TestQueue::new(0); // unbuffered
        q.register_receiver(99); // remote receiver waiter

        let value = vec![42u64];
        match q.send_or_block(value, 0, 1 /* sender waiter */) {
            BlockingSendResult::DirectSend { receiver, payload } => {
                assert_eq!(receiver, 99);
                assert_eq!(payload, vec![42u64]);
                assert!(q.buffer.is_empty());
            }
            other => panic!("expected DirectSend, got {:?}", other),
        }

        // Subsequent recv must block (buffer empty, no senders).
        assert_eq!(q.recv_or_block(100), BlockingRecvResult::Blocked(100));
    }

    #[test]
    fn immediate_send_and_recv_do_not_create_waiters() {
        let calls = std::cell::Cell::new(0);
        let mut q = TestQueue::new(1);

        let send = q
            .try_send_or_block_with(vec![42], 1, || {
                calls.set(calls.get() + 1);
                Ok::<u32, &'static str>(7)
            })
            .expect("buffered send");
        assert_eq!(send, BlockingSendResult::Buffered);
        assert_eq!(calls.get(), 0);

        let recv = q
            .try_recv_or_block_with(|| {
                calls.set(calls.get() + 1);
                Ok::<u32, &'static str>(8)
            })
            .expect("immediate recv");
        assert_eq!(
            recv,
            BlockingRecvResult::Success {
                woke_sender: None,
                payload: vec![42],
            }
        );
        assert_eq!(calls.get(), 0);
    }

    #[test]
    fn blocked_send_and_recv_create_one_waiter_each() {
        let send_calls = std::cell::Cell::new(0);
        let mut send_queue = TestQueue::new(0);
        let send = send_queue
            .try_send_or_block_with(vec![42], 0, || {
                send_calls.set(send_calls.get() + 1);
                Ok::<u32, &'static str>(7)
            })
            .expect("blocked send");
        assert_eq!(send, BlockingSendResult::Blocked(7));
        assert_eq!(send_calls.get(), 1);
        assert_eq!(send_queue.waiting_senders.front(), Some(&(7, vec![42])));

        let recv_calls = std::cell::Cell::new(0);
        let mut recv_queue = TestQueue::new(0);
        let recv = recv_queue
            .try_recv_or_block_with(|| {
                recv_calls.set(recv_calls.get() + 1);
                Ok::<u32, &'static str>(8)
            })
            .expect("blocked recv");
        assert_eq!(recv, BlockingRecvResult::Blocked(8));
        assert_eq!(recv_calls.get(), 1);
        assert_eq!(recv_queue.waiting_receivers.front(), Some(&8));
    }

    #[test]
    fn waiter_factory_error_leaves_queue_state_unchanged() {
        let mut send_queue = TestQueue::new(0);
        assert_eq!(
            send_queue.try_send_or_block_with(vec![42], 0, || Err::<u32, _>("exhausted")),
            Err("exhausted")
        );
        assert!(send_queue.buffer.is_empty());
        assert!(send_queue.waiting_senders.is_empty());
        assert!(send_queue.waiting_receivers.is_empty());

        let mut recv_queue = TestQueue::new(0);
        assert_eq!(
            recv_queue.try_recv_or_block_with(|| Err::<u32, _>("exhausted")),
            Err("exhausted")
        );
        assert!(recv_queue.buffer.is_empty());
        assert!(recv_queue.waiting_senders.is_empty());
        assert!(recv_queue.waiting_receivers.is_empty());
    }

    #[test]
    fn dropping_direct_send_result_cannot_leave_a_phantom_payload() {
        let mut q = TestQueue::new(0);
        q.register_receiver(99);

        let value = vec![42u64];
        match q.send_or_block(value, 0, 1) {
            BlockingSendResult::DirectSend { receiver, payload } => {
                assert_eq!(receiver, 99);
                drop(payload);
                assert!(q.buffer.is_empty());
            }
            other => panic!("expected DirectSend, got {:?}", other),
        }

        assert_eq!(q.try_recv(), RecvResult::WouldBlock);
    }

    #[test]
    fn direct_send_preserves_preexisting_buffered_values() {
        let mut q = TestQueue::new(2);
        // Pre-fill one value
        match q.try_send(vec![10u64], 2) {
            SendResult::Buffered => {}
            other => panic!("expected Buffered, got {:?}", other),
        }
        assert_eq!(q.buffer.len(), 1);

        q.register_receiver(99);

        let value = vec![20u64];
        match q.send_or_block(value, 2, 1) {
            BlockingSendResult::DirectSend { receiver, payload } => {
                assert_eq!(receiver, 99);
                assert_eq!(payload, vec![20u64]);
                assert_eq!(q.buffer.len(), 1);
                assert_eq!(q.buffer[0], vec![10u64]);
            }
            other => panic!("expected DirectSend, got {:?}", other),
        }
    }

    #[test]
    fn send_or_block_blocked_does_not_create_phantom() {
        // When send_or_block returns Blocked (no receiver, full buffer),
        // the value goes into waiting_senders, not buffer. No phantom issue.
        let mut q = TestQueue::new(0); // unbuffered, no receiver
        match q.send_or_block(vec![99u64], 0, 1) {
            BlockingSendResult::Blocked(waiter) => {
                assert_eq!(waiter, 1);
                assert_eq!(q.buffer.len(), 0);
                assert_eq!(q.waiting_senders.len(), 1);
            }
            other => panic!("expected Blocked, got {:?}", other),
        }
    }

    #[test]
    fn send_or_block_buffered_does_not_create_phantom() {
        // When send_or_block returns Buffered (has capacity), value goes
        // into buffer legitimately. No pop_back needed.
        let mut q = TestQueue::new(5);
        match q.send_or_block(vec![77u64], 5, 1) {
            BlockingSendResult::Buffered => {
                assert_eq!(q.buffer.len(), 1);
                assert_eq!(q.buffer[0], vec![77u64]);
            }
            other => panic!("expected Buffered, got {:?}", other),
        }
    }

    #[test]
    fn resolved_direct_send_to_remote_waiter_extracts_payload() {
        let mut q = LocalQueueState::new(0);
        q.register_receiver(
            QueueWaiter::try_queue(9, 99, 0x1000, SelectWaitKind::Recv).expect("receiver waiter"),
        );

        match q.send_or_block_resolved(
            vec![42u64].into_boxed_slice().into(),
            0,
            QueueWaiter::try_queue(7, 1, 0x1000, SelectWaitKind::Send).expect("sender waiter"),
            7,
        ) {
            ResolvedSendResult::RemoteDirect { receiver, payload } => {
                assert_eq!(receiver.island_id(), 9);
                assert_eq!(receiver.fiber_key(), 99);
                assert_eq!(payload.as_ref(), &[42u64]);
                assert_eq!(q.buffer.len(), 0);
            }
            other => panic!("expected RemoteDirect, got {:?}", other),
        }
    }

    #[test]
    fn same_island_endpoint_receiver_uses_endpoint_response_path() {
        let mut q = LocalQueueState::new(0);
        let wait_key = EndpointWaitKey::try_new(99, 11).unwrap();
        q.register_receiver(QueueWaiter::endpoint(7, wait_key));

        match q.send_or_block_resolved(
            vec![42u64].into_boxed_slice().into(),
            0,
            QueueWaiter::try_queue(7, 1, 0x1000, SelectWaitKind::Send).expect("sender waiter"),
            7,
        ) {
            ResolvedSendResult::RemoteDirect { receiver, payload } => {
                assert_eq!(receiver.island_id(), 7);
                assert_eq!(receiver.fiber_key(), 99);
                assert_eq!(receiver.endpoint_wait_key(), Some(wait_key));
                assert_eq!(payload.as_ref(), &[42u64]);
                assert_eq!(q.buffer.len(), 0);
            }
            other => panic!(
                "same-island endpoint receiver must use endpoint response path, got {other:?}"
            ),
        }
    }

    #[test]
    fn vm_wake_registration_002_select_cancel_keeps_other_fiber_same_select_id_waiters() {
        let mut q = LocalQueueState::new(0);
        let queue_ref = 0xfeed_u64;
        let select_id = 1;
        let first_fiber_key = 0x0000_0001_0000_0001;
        let second_fiber_key = 0x0000_0002_0000_0001;

        q.register_receiver(
            QueueWaiter::try_select(
                0,
                first_fiber_key,
                0,
                select_id,
                queue_ref,
                SelectWaitKind::Recv,
            )
            .expect("first select waiter"),
        );
        q.register_receiver(
            QueueWaiter::try_select(
                0,
                second_fiber_key,
                0,
                select_id,
                queue_ref,
                SelectWaitKind::Recv,
            )
            .expect("second select waiter"),
        );

        q.cancel_select_waiters(first_fiber_key, select_id);

        assert_eq!(q.waiting_receivers.len(), 1);
        assert_eq!(q.waiting_receivers[0].fiber_key(), second_fiber_key);
    }
}
