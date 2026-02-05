//! Generic queue state for channel-like objects (Channel, Port).
//!
//! This module provides:
//! - Unified QueueData structure (layout identical for Channel and Port)
//! - Unified accessors: elem_meta, elem_slots, capacity, len, close, is_closed
//! - Generic QueueState<W, M> state machine

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, collections::VecDeque, vec::Vec};

#[cfg(feature = "std")]
use std::{collections::VecDeque, vec::Vec, boxed::Box};

use crate::gc::GcRef;
use crate::slot::{slot_to_usize, Slot, SLOT_BYTES};
use vo_common_core::types::{ValueKind, ValueMeta};

/// Unified data structure for Channel and Port.
/// Layout: GcHeader + QueueData
#[repr(C)]
pub struct QueueData {
    pub state: Slot,
    pub cap: Slot,
    pub elem_meta: ValueMeta,
    pub elem_slots: u16,
    _pad: u16,
}

pub const DATA_SLOTS: u16 = 3;
const _: () = assert!(core::mem::size_of::<QueueData>() == DATA_SLOTS as usize * SLOT_BYTES);

impl_gc_object!(QueueData);

// =============================================================================
// Unified accessors (capacity, elem_meta, elem_slots work for both)
// len/close/is_closed are in channel.rs and port.rs separately
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

// =============================================================================
// Type aliases for Channel and Port states
// =============================================================================

pub type GoId = u64;
pub type ChannelMessage = Box<[u64]>;

/// Select waiter information - for fibers waiting in a select statement.
/// When a select blocks, it registers SelectWaiter on multiple channels.
/// When any channel becomes ready, it wakes the fiber and sets woken_case_index.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectWaiter {
    pub fiber_id: u64,
    pub case_index: u16,      // Index of this case in the select statement
    pub select_id: u64,       // Unique ID for this select instance (for cancellation)
}

/// Channel waiter - either a simple fiber ID or a select waiter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChannelWaiter {
    /// Simple send/recv operation - just the fiber ID
    Simple(GoId),
    /// Select case - includes case index and select ID for cancellation
    Select(SelectWaiter),
}

impl ChannelWaiter {
    /// Get the fiber ID regardless of waiter type.
    #[inline]
    pub fn fiber_id(&self) -> u64 {
        match self {
            ChannelWaiter::Simple(id) => *id,
            ChannelWaiter::Select(sw) => sw.fiber_id,
        }
    }
    
    /// Check if this is a select waiter with the given select_id.
    #[inline]
    pub fn is_select_with_id(&self, select_id: u64) -> bool {
        match self {
            ChannelWaiter::Simple(_) => false,
            ChannelWaiter::Select(sw) => sw.select_id == select_id,
        }
    }
}

pub type ChannelState = QueueState<ChannelWaiter, ChannelMessage>;

#[cfg(feature = "std")]
pub use crate::pack::PackedValue;

#[cfg(feature = "std")]
pub type PortState = QueueState<WaiterInfo, PackedValue>;

/// Information about a waiting fiber for cross-island wake
#[cfg(feature = "std")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WaiterInfo {
    pub island_id: u32,
    pub fiber_id: u64,
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
    /// Channel/port is closed.
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
    /// Channel/port is closed.
    Closed,
}

/// Generic queue state for channel-like communication.
///
/// Type parameters:
/// - `W`: Waiter identifier type (e.g., `GoId` for Channel, `WaiterInfo` for Port)
/// - `M`: Message type (e.g., `Box<[u64]>` for Channel, `PackedValue` for Port)
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

impl ChannelState {
    /// Cancel all select waiters with the given select_id.
    /// Called when a select completes (one case became ready) to remove
    /// this fiber from all other channels it was waiting on.
    pub fn cancel_select_waiters(&mut self, select_id: u64) {
        self.waiting_receivers.retain(|w| !w.is_select_with_id(select_id));
        self.waiting_senders.retain(|(w, _)| !w.is_select_with_id(select_id));
    }
}
