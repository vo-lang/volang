//! Channel object operations.
//!
//! Layout: GcHeader + ChannelData
//!
//! Supports multi-slot elements. Buffer stores elements consecutively,
//! each element occupies `elem_slots` slots.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, collections::VecDeque, vec::Vec};

#[cfg(feature = "std")]
use std::{boxed::Box, collections::VecDeque, vec::Vec};

use crate::gc::{Gc, GcRef};
use vo_common_core::types::{ValueKind, ValueMeta};

#[repr(C)]
pub struct ChannelData {
    pub state: *mut ChannelState,
    pub cap: usize,
    pub elem_meta: ValueMeta,
    pub elem_slots: u16,
    _pad: u16,
}

pub const DATA_SLOTS: u16 = 3;
const _: () = assert!(core::mem::size_of::<ChannelData>() == DATA_SLOTS as usize * 8);

impl ChannelData {
    #[inline]
    fn as_ref(c: GcRef) -> &'static Self {
        unsafe { &*(c as *const Self) }
    }

    #[inline]
    fn as_mut(c: GcRef) -> &'static mut Self {
        unsafe { &mut *(c as *mut Self) }
    }
}

pub type GoId = u64;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SendResult {
    DirectSend(GoId),
    Buffered,
    WouldBlock,
    Closed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecvResult {
    Success(Option<GoId>),
    WouldBlock,
    Closed,
}

pub struct ChannelState {
    pub buffer: VecDeque<Box<[u64]>>,
    pub closed: bool,
    pub waiting_senders: VecDeque<(GoId, Box<[u64]>)>,
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

    pub fn try_send(&mut self, value: Box<[u64]>, cap: usize) -> SendResult {
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

    pub fn try_recv(&mut self) -> (RecvResult, Option<Box<[u64]>>) {
        if let Some(value) = self.buffer.pop_front() {
            let woke_sender = if let Some((sender_id, sender_value)) = self.waiting_senders.pop_front() {
                self.buffer.push_back(sender_value);
                Some(sender_id)
            } else {
                None
            };
            return (RecvResult::Success(woke_sender), Some(value));
        }
        if let Some((sender_id, value)) = self.waiting_senders.pop_front() {
            return (RecvResult::Success(Some(sender_id)), Some(value));
        }
        if self.closed {
            (RecvResult::Closed, None)
        } else {
            (RecvResult::WouldBlock, None)
        }
    }

    pub fn register_sender(&mut self, go_id: GoId, value: Box<[u64]>) {
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

    pub fn take_waiting_senders(&mut self) -> Vec<(GoId, Box<[u64]>)> {
        self.waiting_senders.drain(..).collect()
    }

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
    let data = ChannelData::as_mut(chan);
    data.state = Box::into_raw(state);
    data.cap = cap;
    data.elem_meta = elem_meta;
    data.elem_slots = elem_slots;
    chan
}

#[inline]
pub fn elem_meta(chan: GcRef) -> ValueMeta { ChannelData::as_ref(chan).elem_meta }
#[inline]
pub fn elem_kind(chan: GcRef) -> ValueKind { elem_meta(chan).value_kind() }
#[inline]
pub fn elem_slots(chan: GcRef) -> u16 { ChannelData::as_ref(chan).elem_slots }
#[inline]
pub fn capacity(chan: GcRef) -> usize { ChannelData::as_ref(chan).cap }
#[inline]
pub fn get_state(chan: GcRef) -> &'static mut ChannelState {
    unsafe { &mut *ChannelData::as_ref(chan).state }
}

pub fn len(chan: GcRef) -> usize { get_state(chan).len() }
pub fn is_closed(chan: GcRef) -> bool { get_state(chan).is_closed() }
pub fn close(chan: GcRef) { get_state(chan).close(); }

/// # Safety
/// chan must be a valid Channel GcRef.
pub unsafe fn drop_inner(chan: GcRef) {
    let data = ChannelData::as_mut(chan);
    if !data.state.is_null() {
        drop(Box::from_raw(data.state));
        data.state = core::ptr::null_mut();
    }
}
