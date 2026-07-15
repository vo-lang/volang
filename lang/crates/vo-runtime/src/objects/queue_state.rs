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

use core::sync::atomic::{AtomicUsize, Ordering};
use hashbrown::HashSet;

use crate::gc::GcRef;
use crate::slot::{slot_to_usize, Slot, SLOT_BYTES};
use vo_common_core::types::{ValueKind, ValueMeta, ValueRttid};

static QUEUE_WAIT_REGISTRATION_COUNTER: AtomicUsize = AtomicUsize::new(1);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct QueueWaitRegistrationExhausted;

impl core::fmt::Display for QueueWaitRegistrationExhausted {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("queue wait registration identity space exhausted")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for QueueWaitRegistrationExhausted {}

/// Allocates a process-unique, non-zero queue waiter identity.
///
/// Zero is a permanent exhausted sentinel.  Publishing it with the successful
/// allocation of `usize::MAX` prevents every later caller from reusing an old
/// identity, including when several threads race at the boundary.
fn next_queue_wait_registration_id_from(
    counter: &AtomicUsize,
) -> Result<u64, QueueWaitRegistrationExhausted> {
    let mut current = counter.load(Ordering::Relaxed);
    loop {
        if current == 0 {
            return Err(QueueWaitRegistrationExhausted);
        }
        let next = current.checked_add(1).unwrap_or(0);
        match counter.compare_exchange_weak(current, next, Ordering::Relaxed, Ordering::Relaxed) {
            Ok(_) => return Ok(current as u64),
            Err(observed) => current = observed,
        }
    }
}

fn next_queue_wait_registration_id() -> Result<u64, QueueWaitRegistrationExhausted> {
    next_queue_wait_registration_id_from(&QUEUE_WAIT_REGISTRATION_COUNTER)
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
pub unsafe fn elem_kind(q: GcRef) -> ValueKind {
    elem_meta(q).value_kind()
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

#[inline]
pub unsafe fn is_port(q: GcRef) -> bool {
    kind(q) == QueueKind::Port
}

// =============================================================================
// Type aliases for channel states
// =============================================================================

pub type QueueMessage = Box<[u64]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectInfo {
    pub case_index: u16,
    pub select_id: u64,
    pub queue_ref: u64,
    pub kind: SelectWaitKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SelectWaitKind {
    Send,
    Recv,
}

impl SelectWaitKind {
    #[inline]
    pub fn to_raw(self) -> u8 {
        match self {
            Self::Send => 1,
            Self::Recv => 2,
        }
    }

    #[inline]
    pub fn from_raw(raw: u8) -> Option<Self> {
        match raw {
            1 => Some(Self::Send),
            2 => Some(Self::Recv),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QueueWaiter {
    pub island_id: u32,
    pub fiber_key: u64,
    pub registration_id: u64,
    pub endpoint_wait_id: u64,
    pub queue_ref: u64,
    pub kind: Option<SelectWaitKind>,
    pub select: Option<SelectInfo>,
}

impl QueueWaiter {
    #[inline]
    pub fn fiber_key(&self) -> u64 {
        self.fiber_key
    }

    #[inline]
    pub fn endpoint_wait_id(&self) -> u64 {
        self.endpoint_wait_id
    }

    #[inline]
    pub fn simple(island_id: u32, fiber_key: u64) -> Self {
        Self {
            island_id,
            fiber_key,
            registration_id: 0,
            endpoint_wait_id: 0,
            queue_ref: 0,
            kind: None,
            select: None,
        }
    }

    #[inline]
    pub fn simple_queue(
        island_id: u32,
        fiber_key: u64,
        queue_ref: u64,
        kind: SelectWaitKind,
    ) -> Self {
        Self::try_simple_queue(island_id, fiber_key, queue_ref, kind)
            .expect("queue wait registration identity space exhausted")
    }

    #[inline]
    pub fn try_simple_queue(
        island_id: u32,
        fiber_key: u64,
        queue_ref: u64,
        kind: SelectWaitKind,
    ) -> Result<Self, QueueWaitRegistrationExhausted> {
        Ok(Self {
            island_id,
            fiber_key,
            registration_id: next_queue_wait_registration_id()?,
            endpoint_wait_id: 0,
            queue_ref,
            kind: Some(kind),
            select: None,
        })
    }

    #[inline]
    pub fn endpoint(island_id: u32, fiber_key: u64, endpoint_wait_id: u64) -> Self {
        Self {
            island_id,
            fiber_key,
            registration_id: 0,
            endpoint_wait_id,
            queue_ref: 0,
            kind: None,
            select: None,
        }
    }

    #[inline]
    pub fn selecting(
        island_id: u32,
        fiber_key: u64,
        case_index: u16,
        select_id: u64,
        queue_ref: u64,
        kind: SelectWaitKind,
    ) -> Self {
        Self::try_selecting(island_id, fiber_key, case_index, select_id, queue_ref, kind)
            .expect("queue wait registration identity space exhausted")
    }

    #[inline]
    pub fn try_selecting(
        island_id: u32,
        fiber_key: u64,
        case_index: u16,
        select_id: u64,
        queue_ref: u64,
        kind: SelectWaitKind,
    ) -> Result<Self, QueueWaitRegistrationExhausted> {
        Ok(Self {
            island_id,
            fiber_key,
            registration_id: next_queue_wait_registration_id()?,
            endpoint_wait_id: 0,
            queue_ref,
            kind: Some(kind),
            select: Some(SelectInfo {
                case_index,
                select_id,
                queue_ref,
                kind,
            }),
        })
    }

    #[inline]
    pub fn is_select_for(&self, fiber_key: u64, select_id: u64) -> bool {
        self.fiber_key == fiber_key
            && self
                .select
                .as_ref()
                .is_some_and(|info| info.select_id == select_id)
    }

    #[inline]
    pub fn is_local_select_recv(&self, local_island: u32) -> bool {
        self.island_id == local_island
            && self
                .select
                .as_ref()
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
    Blocked,
    Closed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedSendResult<W, M> {
    Wake { receiver: W, payload: Option<M> },
    RemoteDirect { receiver: W, payload: M },
    Buffered,
    Blocked,
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
    Blocked,
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

    /// Atomic send: try to send, if would block, register waiter in same operation.
    /// This avoids TOCTOU race between try_send and register_sender.
    pub fn send_or_block(&mut self, value: M, cap: usize, waiter: W) -> BlockingSendResult<W, M> {
        match self.try_send(value, cap) {
            SendResult::DirectSend { receiver, payload } => {
                BlockingSendResult::DirectSend { receiver, payload }
            }
            SendResult::Buffered => BlockingSendResult::Buffered,
            SendResult::WouldBlock(value) => {
                self.waiting_senders.push_back((waiter, value));
                BlockingSendResult::Blocked
            }
            SendResult::Closed => BlockingSendResult::Closed,
        }
    }

    /// Atomic recv: try to receive, if would block, register waiter in same operation.
    /// This avoids TOCTOU race between try_recv and register_receiver.
    pub fn recv_or_block(&mut self, waiter: W) -> BlockingRecvResult<W, M> {
        match self.try_recv() {
            RecvResult::Success {
                woke_sender,
                payload,
            } => BlockingRecvResult::Success {
                woke_sender,
                payload,
            },
            RecvResult::WouldBlock => {
                self.waiting_receivers.push_back(waiter);
                BlockingRecvResult::Blocked
            }
            RecvResult::Closed => BlockingRecvResult::Closed,
        }
    }

    pub fn register_sender(&mut self, waiter: W, value: M) {
        self.waiting_senders.push_back((waiter, value));
    }

    pub fn register_receiver(&mut self, waiter: W) {
        self.waiting_receivers.push_back(waiter);
    }

    pub fn close(&mut self) {
        self.closed = true;
    }

    pub fn is_closed(&self) -> bool {
        self.closed
    }

    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
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
                    waiter.fiber_key != fiber_key
                        || waiter.kind != Some(SelectWaitKind::Send)
                        || waiter.select.is_some()
                });
            }
            SelectWaitKind::Recv => {
                self.waiting_receivers.retain(|waiter| {
                    waiter.fiber_key != fiber_key
                        || waiter.kind != Some(SelectWaitKind::Recv)
                        || waiter.select.is_some()
                });
            }
        }
    }

    pub fn send_or_block_resolved(
        &mut self,
        value: M,
        cap: usize,
        waiter: QueueWaiter,
        local_island: u32,
    ) -> ResolvedSendResult<QueueWaiter, M> {
        match self.send_or_block(value, cap, waiter) {
            BlockingSendResult::DirectSend { receiver, payload } => {
                if receiver.endpoint_wait_id() == 0 && receiver.island_id == local_island {
                    if receiver
                        .select
                        .as_ref()
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
            BlockingSendResult::Blocked => ResolvedSendResult::Blocked,
            BlockingSendResult::Closed => ResolvedSendResult::Closed,
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
            Err(QueueWaitRegistrationExhausted)
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
                .filter(|result| **result == Err(QueueWaitRegistrationExhausted))
                .count(),
            7
        );
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
        assert_eq!(q.recv_or_block(100), BlockingRecvResult::Blocked);
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
            BlockingSendResult::Blocked => {
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
        q.register_receiver(QueueWaiter::simple(9, 99));

        match q.send_or_block_resolved(
            vec![42u64].into_boxed_slice(),
            0,
            QueueWaiter::simple(7, 1),
            7,
        ) {
            ResolvedSendResult::RemoteDirect { receiver, payload } => {
                assert_eq!(receiver.island_id, 9);
                assert_eq!(receiver.fiber_key, 99);
                assert_eq!(payload.as_ref(), &[42u64]);
                assert_eq!(q.buffer.len(), 0);
            }
            other => panic!("expected RemoteDirect, got {:?}", other),
        }
    }

    #[test]
    fn same_island_endpoint_receiver_uses_endpoint_response_path() {
        let mut q = LocalQueueState::new(0);
        q.register_receiver(QueueWaiter::endpoint(7, 99, 11));

        match q.send_or_block_resolved(
            vec![42u64].into_boxed_slice(),
            0,
            QueueWaiter::simple(7, 1),
            7,
        ) {
            ResolvedSendResult::RemoteDirect { receiver, payload } => {
                assert_eq!(receiver.island_id, 7);
                assert_eq!(receiver.fiber_key, 99);
                assert_eq!(receiver.endpoint_wait_id(), 11);
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

        q.register_receiver(QueueWaiter::selecting(
            0,
            first_fiber_key,
            0,
            select_id,
            queue_ref,
            SelectWaitKind::Recv,
        ));
        q.register_receiver(QueueWaiter::selecting(
            0,
            second_fiber_key,
            0,
            select_id,
            queue_ref,
            SelectWaitKind::Recv,
        ));

        q.cancel_select_waiters(first_fiber_key, select_id);

        assert_eq!(q.waiting_receivers.len(), 1);
        assert_eq!(q.waiting_receivers[0].fiber_key(), second_fiber_key);
    }
}
