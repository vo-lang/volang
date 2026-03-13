//! Generic queue state for channel objects.
//!
//! This module provides:
//! - Unified QueueData structure for all channel objects
//! - Unified accessors: elem_meta, elem_slots, capacity, len, close, is_closed
//! - Generic QueueState<W, M> state machine

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, collections::VecDeque, vec::Vec};

#[cfg(feature = "std")]
use std::{collections::VecDeque, vec::Vec, boxed::Box};

use hashbrown::HashSet;

use crate::gc::GcRef;
use crate::slot::{slot_to_usize, Slot, SLOT_BYTES};
use vo_common_core::types::{ValueKind, ValueMeta, ValueRttid};

/// Channel backing kind discriminant.
/// Stored in QueueData::backing field.
pub const BACKING_LOCAL: u16 = 0;
pub const BACKING_REMOTE: u16 = 1;

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
            1 => Self::Port,
            _ => Self::Chan,
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
            other => panic!("QueueKind::from_value_kind: expected channel/port kind, got {:?}", other),
        }
    }
}

/// Unified data structure for all channel objects.
/// Layout: GcHeader + QueueData
#[repr(C)]
pub struct QueueData {
    pub state: Slot,
    pub cap: Slot,
    pub elem_meta: ValueMeta,  // 4 bytes
    pub elem_slots: u16,       // 2 bytes
    /// Channel backing kind: BACKING_LOCAL (0) or BACKING_REMOTE (1).
    pub backing: u16,          // 2 bytes
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
pub fn capacity(q: GcRef) -> usize {
    slot_to_usize(QueueData::as_ref(q).cap)
}

#[inline]
pub fn elem_meta(q: GcRef) -> ValueMeta {
    QueueData::as_ref(q).elem_meta
}

#[inline]
pub fn elem_kind(q: GcRef) -> ValueKind {
    elem_meta(q).value_kind()
}

#[inline]
pub fn elem_slots(q: GcRef) -> u16 {
    QueueData::as_ref(q).elem_slots
}

#[inline]
pub fn elem_rttid(q: GcRef) -> ValueRttid {
    ValueRttid::from_raw(QueueData::as_ref(q).elem_rttid)
}

#[inline]
pub fn kind(q: GcRef) -> QueueKind {
    QueueKind::from_raw(QueueData::as_ref(q).kind)
}

#[inline]
pub fn is_port(q: GcRef) -> bool {
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QueueWaiter {
    pub island_id: u32,
    pub fiber_id: u64,
    pub select: Option<SelectInfo>,
}

impl QueueWaiter {
    #[inline]
    pub fn fiber_id(&self) -> u64 {
        self.fiber_id
    }

    #[inline]
    pub fn simple(island_id: u32, fiber_id: u64) -> Self {
        Self {
            island_id,
            fiber_id,
            select: None,
        }
    }

    #[inline]
    pub fn selecting(island_id: u32, fiber_id: u64, case_index: u16, select_id: u64) -> Self {
        Self {
            island_id,
            fiber_id,
            select: Some(SelectInfo { case_index, select_id }),
        }
    }

    #[inline]
    pub fn is_select_with_id(&self, select_id: u64) -> bool {
        self.select.as_ref().is_some_and(|info| info.select_id == select_id)
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
    DirectSend(W),
    /// Value buffered successfully.
    Buffered,
    /// Would block - buffer full, no receivers. Returns the value back.
    WouldBlock(M),
    /// Blocked - waiter registered atomically (used by send_or_block).
    Blocked,
    /// Channel is closed.
    Closed,
}

/// Result of a receive operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecvResult<W> {
    /// Successfully received, optionally woke a sender.
    Success(Option<W>),
    /// Would block - buffer empty, no senders.
    WouldBlock,
    /// Blocked - waiter registered atomically (used by recv_or_block).
    Blocked,
    /// Channel is closed.
    Closed,
}

/// Generic queue state for channel-like communication.
///
/// Type parameters:
/// - `W`: Waiter identifier type
/// - `M`: Message type (e.g., `Box<[u64]>` for Channel)
pub struct QueueState<W, M> {
    pub buffer: VecDeque<M>,
    pub closed: bool,
    pub waiting_senders: VecDeque<(W, M)>,
    pub waiting_receivers: VecDeque<W>,
}

impl<W, M> QueueState<W, M> {
    pub fn new(cap: usize) -> Self {
        Self {
            buffer: VecDeque::with_capacity(cap),
            closed: false,
            waiting_senders: VecDeque::new(),
            waiting_receivers: VecDeque::new(),
        }
    }

    /// Try to send a value. Returns the value back if would block.
    pub fn try_send(&mut self, value: M, cap: usize) -> SendResult<W, M> {
        if self.closed {
            return SendResult::Closed;
        }
        // If there's a waiting receiver, buffer the value and wake receiver
        if let Some(receiver) = self.waiting_receivers.pop_front() {
            self.buffer.push_back(value);
            return SendResult::DirectSend(receiver);
        }
        // Buffer if capacity allows
        if self.buffer.len() < cap {
            self.buffer.push_back(value);
            return SendResult::Buffered;
        }
        SendResult::WouldBlock(value)
    }

    /// Atomic send: try to send, if would block, register waiter in same operation.
    /// This avoids TOCTOU race between try_send and register_sender.
    pub fn send_or_block(&mut self, value: M, cap: usize, waiter: W) -> SendResult<W, M> {
        if self.closed {
            return SendResult::Closed;
        }
        // If there's a waiting receiver, buffer the value and wake receiver
        if let Some(receiver) = self.waiting_receivers.pop_front() {
            self.buffer.push_back(value);
            return SendResult::DirectSend(receiver);
        }
        // Buffer if capacity allows
        if self.buffer.len() < cap {
            self.buffer.push_back(value);
            return SendResult::Buffered;
        }
        // Would block - register waiter atomically
        self.waiting_senders.push_back((waiter, value));
        SendResult::Blocked
    }

    /// Try to receive a value.
    pub fn try_recv(&mut self) -> (RecvResult<W>, Option<M>) {
        if let Some(value) = self.buffer.pop_front() {
            let woke_sender = if let Some((sender, sender_value)) = self.waiting_senders.pop_front() {
                self.buffer.push_back(sender_value);
                Some(sender)
            } else {
                None
            };
            return (RecvResult::Success(woke_sender), Some(value));
        }
        if let Some((sender, value)) = self.waiting_senders.pop_front() {
            return (RecvResult::Success(Some(sender)), Some(value));
        }
        if self.closed {
            (RecvResult::Closed, None)
        } else {
            (RecvResult::WouldBlock, None)
        }
    }

    /// Atomic recv: try to receive, if would block, register waiter in same operation.
    /// This avoids TOCTOU race between try_recv and register_receiver.
    pub fn recv_or_block(&mut self, waiter: W) -> (RecvResult<W>, Option<M>) {
        if let Some(value) = self.buffer.pop_front() {
            let woke_sender = if let Some((sender, sender_value)) = self.waiting_senders.pop_front() {
                self.buffer.push_back(sender_value);
                Some(sender)
            } else {
                None
            };
            return (RecvResult::Success(woke_sender), Some(value));
        }
        if let Some((sender, value)) = self.waiting_senders.pop_front() {
            return (RecvResult::Success(Some(sender)), Some(value));
        }
        if self.closed {
            (RecvResult::Closed, None)
        } else {
            // Would block - register waiter atomically
            self.waiting_receivers.push_back(waiter);
            (RecvResult::Blocked, None)
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
    /// Cancel all select waiters with the given select_id.
    /// Called when a select completes (one case became ready) to remove
    /// this fiber from all other channels it was waiting on.
    pub fn cancel_select_waiters(&mut self, select_id: u64) {
        self.waiting_receivers.retain(|w| !w.is_select_with_id(select_id));
        self.waiting_senders.retain(|(w, _)| !w.is_select_with_id(select_id));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Simple waiter type for unit tests (avoids depending on QueueWaiter).
    type TestQueue = QueueState<u32, Vec<u64>>;

    #[test]
    fn direct_send_pop_back_removes_phantom_entry() {
        // Simulates the BUG-1 scenario:
        //   1. A remote receiver is waiting.
        //   2. send_or_block DirectSends — pushes value into buffer.
        //   3. Caller pops the value back (for remote transport delivery).
        //   4. Buffer must be empty afterwards.
        let mut q = TestQueue::new(0); // unbuffered
        q.register_receiver(99); // remote receiver waiter

        let value = vec![42u64];
        match q.send_or_block(value, 0, 1 /* sender waiter */) {
            SendResult::DirectSend(receiver) => {
                assert_eq!(receiver, 99);
                // Buffer now has the phantom entry
                assert_eq!(q.buffer.len(), 1);
                assert_eq!(q.buffer[0], vec![42u64]);
                // Pop it back — this is what the fix does
                q.buffer.pop_back();
                assert_eq!(q.buffer.len(), 0);
            }
            other => panic!("expected DirectSend, got {:?}", other),
        }

        // Subsequent recv must block (buffer empty, no senders).
        let (result, _) = q.recv_or_block(100);
        assert_eq!(result, RecvResult::Blocked);
    }

    #[test]
    fn direct_send_without_pop_back_leaves_phantom() {
        // Shows the bug: without pop_back, buffer retains a phantom entry
        // and a subsequent recv incorrectly succeeds.
        let mut q = TestQueue::new(0);
        q.register_receiver(99);

        let value = vec![42u64];
        match q.send_or_block(value, 0, 1) {
            SendResult::DirectSend(receiver) => {
                assert_eq!(receiver, 99);
                // WITHOUT pop_back — buffer still has the phantom
                assert_eq!(q.buffer.len(), 1);
                // A subsequent recv would incorrectly succeed
                let (result, data) = q.try_recv();
                assert!(matches!(result, RecvResult::Success(None)));
                assert_eq!(data.unwrap(), vec![42u64]);
                // Now buffer is empty
                assert_eq!(q.buffer.len(), 0);
            }
            other => panic!("expected DirectSend, got {:?}", other),
        }
    }

    #[test]
    fn direct_send_buffered_channel_pop_back_correct() {
        // Buffered channel (cap=2) with a remote receiver waiting.
        // DirectSend pushes to buffer, pop_back removes it.
        // Pre-existing buffered values must remain intact.
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
            SendResult::DirectSend(receiver) => {
                assert_eq!(receiver, 99);
                // Buffer: [10, 20] — 10 was pre-existing, 20 is the DirectSend phantom
                assert_eq!(q.buffer.len(), 2);
                q.buffer.pop_back(); // remove the phantom 20
                assert_eq!(q.buffer.len(), 1);
                assert_eq!(q.buffer[0], vec![10u64]); // pre-existing value intact
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
            SendResult::Blocked => {
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
            SendResult::Buffered => {
                assert_eq!(q.buffer.len(), 1);
                assert_eq!(q.buffer[0], vec![77u64]);
            }
            other => panic!("expected Buffered, got {:?}", other),
        }
    }
}
