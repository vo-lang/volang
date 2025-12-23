//! Channel object operations.
//!
//! Channel layout: [state_ptr, elem_kind, capacity] (3 slots)

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, collections::VecDeque, vec::Vec};

#[cfg(feature = "std")]
use std::{boxed::Box, collections::VecDeque, vec::Vec};

use crate::gc::{Gc, GcRef};
use vo_common_core::types::ValueKind;

pub const SLOT_STATE: usize = 0;
pub const SLOT_ELEM_KIND: usize = 1;
pub const SLOT_CAP: usize = 2;
pub const SLOT_COUNT: u16 = 3;

pub type GoId = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SendResult {
    DirectSend(GoId),
    Buffered,
    WouldBlock,
    Closed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecvResult {
    Success(u64, Option<GoId>),
    WouldBlock,
    Closed,
}

#[derive(Default)]
pub struct ChannelState {
    pub buffer: VecDeque<u64>,
    pub closed: bool,
    pub waiting_senders: VecDeque<(GoId, u64)>,
    pub waiting_receivers: VecDeque<GoId>,
}

impl ChannelState {
    pub fn new(cap: usize) -> Self {
        Self {
            buffer: VecDeque::with_capacity(cap),
            closed: false,
            waiting_senders: VecDeque::new(),
            waiting_receivers: VecDeque::new(),
        }
    }

    pub fn try_send(&mut self, value: u64, cap: usize) -> SendResult {
        if self.closed {
            return SendResult::Closed;
        }
        if let Some(receiver_id) = self.waiting_receivers.pop_front() {
            self.buffer.push_back(value);
            return SendResult::DirectSend(receiver_id);
        }
        if self.buffer.len() < cap {
            self.buffer.push_back(value);
            return SendResult::Buffered;
        }
        SendResult::WouldBlock
    }

    pub fn try_recv(&mut self) -> RecvResult {
        if let Some(value) = self.buffer.pop_front() {
            let woke_sender = if let Some((sender_id, sender_value)) = self.waiting_senders.pop_front() {
                self.buffer.push_back(sender_value);
                Some(sender_id)
            } else {
                None
            };
            return RecvResult::Success(value, woke_sender);
        }
        if let Some((sender_id, value)) = self.waiting_senders.pop_front() {
            return RecvResult::Success(value, Some(sender_id));
        }
        if self.closed { RecvResult::Closed } else { RecvResult::WouldBlock }
    }

    pub fn register_sender(&mut self, go_id: GoId, value: u64) {
        self.waiting_senders.push_back((go_id, value));
    }

    pub fn register_receiver(&mut self, go_id: GoId) {
        self.waiting_receivers.push_back(go_id);
    }

    pub fn close(&mut self) { self.closed = true; }
    pub fn is_closed(&self) -> bool { self.closed }
    pub fn len(&self) -> usize { self.buffer.len() }
    pub fn is_empty(&self) -> bool { self.buffer.is_empty() }

    pub fn take_waiting_receivers(&mut self) -> Vec<GoId> {
        self.waiting_receivers.drain(..).collect()
    }

    pub fn take_waiting_senders(&mut self) -> Vec<(GoId, u64)> {
        self.waiting_senders.drain(..).collect()
    }
}

pub fn create(gc: &mut Gc, elem_kind: u8, cap: usize) -> GcRef {
    let chan = gc.alloc(ValueKind::Channel as u8, 0, SLOT_COUNT);
    let state = Box::new(ChannelState::new(cap));
    Gc::write_slot(chan, SLOT_STATE, Box::into_raw(state) as u64);
    Gc::write_slot(chan, SLOT_ELEM_KIND, elem_kind as u64);
    Gc::write_slot(chan, SLOT_CAP, cap as u64);
    chan
}

pub fn get_state(chan: GcRef) -> &'static mut ChannelState {
    let ptr = Gc::read_slot(chan, SLOT_STATE) as *mut ChannelState;
    unsafe { &mut *ptr }
}

pub fn elem_kind(chan: GcRef) -> ValueKind {
    ValueKind::from_u8(Gc::read_slot(chan, SLOT_ELEM_KIND) as u8)
}

pub fn capacity(chan: GcRef) -> usize {
    Gc::read_slot(chan, SLOT_CAP) as usize
}

pub fn len(chan: GcRef) -> usize { get_state(chan).len() }

pub fn is_closed(chan: GcRef) -> bool { get_state(chan).is_closed() }

pub fn close(chan: GcRef) { get_state(chan).close(); }

pub unsafe fn drop_inner(chan: GcRef) {
    let ptr = Gc::read_slot(chan, SLOT_STATE) as *mut ChannelState;
    if !ptr.is_null() {
        drop(Box::from_raw(ptr));
        Gc::write_slot(chan, SLOT_STATE, 0);
    }
}
