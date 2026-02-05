//! Channel instructions: ChanNew, ChanSend, ChanRecv, ChanClose

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, string::String, vec::Vec, format};
#[cfg(feature = "std")]
use std::{boxed::Box, vec::Vec};

use vo_runtime::ValueMeta;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::channel::{self, RecvResult, SendResult};
use vo_runtime::objects::queue_state;
use vo_runtime::slot::Slot;

use crate::instruction::Instruction;
use crate::vm::RuntimeTrapKind;
use crate::vm::helpers::{stack_get, stack_set};

pub enum ChanResult {
    Continue,
    Yield,
    Wake(u32),
    WakeMultiple(Vec<u32>),
    Trap(RuntimeTrapKind),
}

/// Result of exec_chan_new: Ok(()) on success, Err(msg) on invalid parameters
pub type ChanNewResult = Result<(), String>;

#[inline]
pub fn exec_chan_new(stack: *mut Slot, bp: usize, inst: &Instruction, gc: &mut Gc) -> ChanNewResult {
    let meta_raw = stack_get(stack, bp + inst.b as usize) as u32;
    let elem_meta = ValueMeta::from_raw(meta_raw);
    let cap = stack_get(stack, bp + inst.c as usize) as i64;
    let elem_slots = inst.flags as u16;
    
    // Use unified validation logic from channel::create_checked
    match channel::create_checked(gc, elem_meta, elem_slots, cap) {
        Ok(ch) => {
            stack_set(stack, bp + inst.a as usize, ch as u64);
            Ok(())
        }
        Err(_) => Err(format!("runtime error: makechan: size out of range")),
    }
}

pub fn exec_chan_send(stack: *const Slot, bp: usize, fiber_id: u32, inst: &Instruction) -> ChanResult {
    let ch = stack_get(stack, bp + inst.a as usize) as GcRef;
    if ch.is_null() {
        return ChanResult::Trap(RuntimeTrapKind::SendOnNilChannel);
    }
    let elem_slots = inst.flags as usize;
    let src_start = bp + inst.b as usize;

    let value: Box<[u64]> = (0..elem_slots).map(|i| stack_get(stack, src_start + i)).collect();
    let cap = queue_state::capacity(ch);
    let state = channel::get_state(ch);

    match state.try_send(value, cap) {
        SendResult::DirectSend(receiver_id) => ChanResult::Wake(receiver_id as u32),
        SendResult::Buffered => ChanResult::Continue,
        SendResult::WouldBlock(value) => {
            state.register_sender(fiber_id as u64, value);
            ChanResult::Yield
        }
        SendResult::Closed => ChanResult::Trap(RuntimeTrapKind::SendOnClosedChannel),
    }
}

pub fn exec_chan_recv(stack: *mut Slot, bp: usize, fiber_id: u32, inst: &Instruction) -> ChanResult {
    let ch = stack_get(stack, bp + inst.b as usize) as GcRef;
    if ch.is_null() {
        return ChanResult::Trap(RuntimeTrapKind::RecvOnNilChannel);
    }
    let elem_slots = ((inst.flags >> 1) & 0x7F) as usize;
    let has_ok = (inst.flags & 1) != 0;
    let dst_start = bp + inst.a as usize;

    let state = channel::get_state(ch);
    let (result, value) = state.try_recv();

    match result {
        RecvResult::Success(woke_sender) => {
            if let Some(val) = value {
                for (i, &v) in val.iter().enumerate() {
                    if i < elem_slots {
                        stack_set(stack, dst_start + i, v);
                    }
                }
            }
            if has_ok {
                stack_set(stack, dst_start + elem_slots, 1);
            }
            if let Some(id) = woke_sender {
                ChanResult::Wake(id as u32)
            } else {
                ChanResult::Continue
            }
        }
        RecvResult::WouldBlock => {
            state.register_receiver(fiber_id as u64);
            ChanResult::Yield
        }
        RecvResult::Closed => {
            for i in 0..elem_slots {
                stack_set(stack, dst_start + i, 0);
            }
            if has_ok {
                stack_set(stack, dst_start + elem_slots, 0);
            }
            ChanResult::Continue
        }
    }
}

#[inline]
pub fn exec_queue_get<F>(stack: *mut Slot, bp: usize, inst: &Instruction, get: F)
where F: FnOnce(GcRef) -> usize {
    let obj = stack_get(stack, bp + inst.b as usize) as GcRef;
    let val = if obj.is_null() { 0 } else { get(obj) };
    stack_set(stack, bp + inst.a as usize, val as u64);
}

#[inline]
pub fn exec_chan_close(stack: *const Slot, bp: usize, inst: &Instruction) -> ChanResult {
    let ch = stack_get(stack, bp + inst.a as usize) as GcRef;
    if ch.is_null() {
        return ChanResult::Trap(RuntimeTrapKind::CloseNilChannel);
    }
    let state = channel::get_state(ch);
    if state.is_closed() {
        return ChanResult::Trap(RuntimeTrapKind::CloseClosedChannel);
    }
    state.close();
    let mut wake_ids: Vec<u32> = state.take_waiting_receivers().into_iter().map(|id| id as u32).collect();
    wake_ids.extend(state.take_waiting_senders().into_iter().map(|(id, _)| id as u32));
    if wake_ids.is_empty() {
        ChanResult::Continue
    } else {
        ChanResult::WakeMultiple(wake_ids)
    }
}
