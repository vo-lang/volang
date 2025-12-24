//! Channel instructions: ChanNew, ChanSend, ChanRecv, ChanClose

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, vec::Vec};
#[cfg(feature = "std")]
use std::{boxed::Box, vec::Vec};

use vo_common_core::types::ValueMeta;
use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::channel::{self, RecvResult, SendResult};

use crate::fiber::Fiber;
use crate::instruction::Instruction;

pub enum ChanResult {
    Continue,
    Yield,
    Panic,
    Wake(u32),
    WakeMultiple(Vec<u32>),
}

#[inline]
pub fn exec_chan_new(fiber: &mut Fiber, inst: &Instruction, gc: &mut Gc) {
    let meta_raw = fiber.read_reg(inst.b) as u32;
    let elem_meta = ValueMeta::from_raw(meta_raw);
    let cap = fiber.read_reg(inst.c) as usize;
    let elem_slots = inst.flags as u16;
    let ch = channel::create(gc, elem_meta, elem_slots, cap);
    fiber.write_reg(inst.a, ch as u64);
}

pub fn exec_chan_send(fiber: &mut Fiber, inst: &Instruction) -> ChanResult {
    let ch = fiber.read_reg(inst.a) as GcRef;
    let elem_slots = inst.flags as usize;

    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let src_start = bp + inst.b as usize;

    let value: Box<[u64]> = fiber.stack[src_start..src_start + elem_slots].into();
    let cap = channel::capacity(ch);
    let state = channel::get_state(ch);

    match state.try_send(value.clone(), cap) {
        SendResult::DirectSend(receiver_id) => ChanResult::Wake(receiver_id as u32),
        SendResult::Buffered => ChanResult::Continue,
        SendResult::WouldBlock => {
            state.register_sender(fiber.id as u64, value);
            ChanResult::Yield
        }
        SendResult::Closed => ChanResult::Panic,
    }
}

pub fn exec_chan_recv(fiber: &mut Fiber, inst: &Instruction) -> ChanResult {
    let ch = fiber.read_reg(inst.b) as GcRef;
    let elem_slots = ((inst.flags >> 1) & 0x7F) as usize;
    let has_ok = (inst.flags & 1) != 0;

    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let dst_start = bp + inst.a as usize;

    let state = channel::get_state(ch);
    let (result, value) = state.try_recv();

    match result {
        RecvResult::Success(woke_sender) => {
            if let Some(val) = value {
                for (i, &v) in val.iter().enumerate() {
                    if i < elem_slots {
                        fiber.stack[dst_start + i] = v;
                    }
                }
            }
            if has_ok {
                fiber.stack[dst_start + elem_slots] = 1;
            }
            if let Some(id) = woke_sender {
                ChanResult::Wake(id as u32)
            } else {
                ChanResult::Continue
            }
        }
        RecvResult::WouldBlock => {
            state.register_receiver(fiber.id as u64);
            ChanResult::Yield
        }
        RecvResult::Closed => {
            for i in 0..elem_slots {
                fiber.stack[dst_start + i] = 0;
            }
            if has_ok {
                fiber.stack[dst_start + elem_slots] = 0;
            }
            ChanResult::Continue
        }
    }
}

#[inline]
pub fn exec_chan_close(fiber: &mut Fiber, inst: &Instruction) -> ChanResult {
    let ch = fiber.read_reg(inst.a) as GcRef;
    let state = channel::get_state(ch);
    state.close();
    let mut wake_ids: Vec<u32> = state.take_waiting_receivers().into_iter().map(|id| id as u32).collect();
    wake_ids.extend(state.take_waiting_senders().into_iter().map(|(id, _)| id as u32));
    if wake_ids.is_empty() {
        ChanResult::Continue
    } else {
        ChanResult::WakeMultiple(wake_ids)
    }
}
