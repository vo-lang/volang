//! Unified channel instructions: ChanNew, ChanSend, ChanRecv, ChanClose
//!
//! All queue objects (local and remote) use ValueKind::Channel.
//! Remote (cross-island) channels are dispatched via channel::is_remote().

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, string::String, vec::Vec, format};
#[cfg(feature = "std")]
use std::{boxed::Box, vec::Vec};

use vo_common_core::bytecode::StructMeta;
use vo_common_core::RuntimeType;
use vo_runtime::ValueMeta;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::channel::{self, RecvResult, SendResult};
use vo_runtime::objects::queue_state::{self, QueueWaiter};
use vo_runtime::slot::Slot;

use crate::instruction::Instruction;
use crate::vm::RuntimeTrapKind;
use crate::vm::helpers::{stack_get, stack_set};

pub type ChanResult = super::QueueAction;

pub enum ChannelRecvCoreResult {
    Success { data: Box<[u64]>, wake_sender: Option<QueueWaiter> },
    WouldBlock,
    Closed,
    Remote { endpoint_id: u64, home_island: u32 },
    Trap(RuntimeTrapKind),
}

#[cfg(feature = "std")]
pub fn decode_remote_recv_response(
    gc: &mut Gc,
    response: crate::fiber::RemoteRecvResponse,
    elem_slots: usize,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    endpoint_registry: &mut crate::vm::EndpointRegistry,
) -> Option<Box<[u64]>> {
    if response.closed {
        None
    } else {
        Some(super::transport::unpack_transport_message(
            gc,
            &response.data,
            elem_slots,
            struct_metas,
            runtime_types,
            Some(endpoint_registry),
        ))
    }
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

pub fn channel_send_core(
    ch: GcRef,
    src: &[u64],
    island_id: u32,
    fiber_id: u64,
    state: &mut crate::vm::VmState,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    module: Option<&vo_runtime::bytecode::Module>,
) -> ChanResult {
    if ch.is_null() {
        return ChanResult::Trap(RuntimeTrapKind::SendOnNilChannel);
    }

    // REMOTE channel — send via message passing
    if channel::is_remote(ch) {
        #[cfg(feature = "std")]
        {
            let proxy = channel::remote_proxy(ch);
            if proxy.closed { return ChanResult::Trap(RuntimeTrapKind::SendOnClosedChannel); }
            let elem_meta = queue_state::elem_meta(ch);
            return ChanResult::RemoteSend {
                endpoint_id: proxy.endpoint_id,
                home_island: proxy.home_island,
                data: super::transport::pack_transport_message(&state.gc, src, elem_meta, struct_metas, runtime_types),
            };
        }
        #[cfg(not(feature = "std"))]
        unreachable!("REMOTE channel in no_std");
    }

    let value: Box<[u64]> = src.iter().copied().collect();

    // Write barrier: type-aware to avoid UB on mixed-slot types.
    // Must be done before send_or_block because the value is moved into the buffer.
    let em = queue_state::elem_meta(ch);
    if em.value_kind().may_contain_gc_refs() {
        vo_runtime::gc_types::typed_write_barrier_by_meta(&mut state.gc, ch, &value, em, module);
    }

    let waiter = QueueWaiter::simple(island_id, fiber_id);
    match channel::send_or_block(ch, value, waiter) {
        SendResult::DirectSend(receiver) => {
            if receiver.island_id == island_id {
                ChanResult::Wake(receiver)
            } else {
                #[cfg(feature = "std")]
                {
                    // send_or_block pushed value into buffer for local-wake semantics,
                    // but remote receivers consume data via transport message, so pop
                    // the value back out to avoid a phantom buffered entry.
                    channel::pop_back_buffer(ch);
                    if em.value_kind().may_contain_gc_refs() {
                        super::prepare_value_chans_for_transfer(
                            src,
                            em,
                            receiver.island_id,
                            struct_metas,
                            runtime_types,
                            state,
                        );
                    }
                    let endpoint_id = channel::home_info(ch)
                        .expect("DirectSend to remote waiter requires HomeInfo")
                        .endpoint_id;
                    let data = super::transport::pack_transport_message(
                        &state.gc,
                        src,
                        em,
                        struct_metas,
                        runtime_types,
                    );
                    ChanResult::RemoteRecvData {
                        endpoint_id,
                        target_island: receiver.island_id,
                        fiber_id: receiver.fiber_id,
                        data,
                    }
                }
                #[cfg(not(feature = "std"))]
                unreachable!("cross-island DirectSend in no_std")
            }
        }
        SendResult::Buffered => ChanResult::Continue,
        SendResult::Blocked => ChanResult::Block,
        SendResult::WouldBlock(_) => unreachable!("send_or_block never returns WouldBlock"),
        SendResult::Closed => ChanResult::Trap(RuntimeTrapKind::SendOnClosedChannel),
    }
}

pub fn exec_chan_send(
    stack: *const Slot,
    bp: usize,
    island_id: u32,
    fiber_id: u32,
    inst: &Instruction,
    state: &mut crate::vm::VmState,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    module: Option<&vo_runtime::bytecode::Module>,
) -> ChanResult {
    let ch = stack_get(stack, bp + inst.a as usize) as GcRef;
    let elem_slots = inst.flags as usize;
    let src_start = bp + inst.b as usize;
    let src: Vec<u64> = (0..elem_slots).map(|i| stack_get(stack, src_start + i)).collect();
    channel_send_core(
        ch,
        &src,
        island_id,
        fiber_id as u64,
        state,
        struct_metas,
        runtime_types,
        module,
    )
}

pub fn channel_recv_core(ch: GcRef, island_id: u32, fiber_id: u64) -> ChannelRecvCoreResult {
    if ch.is_null() {
        return ChannelRecvCoreResult::Trap(RuntimeTrapKind::RecvOnNilChannel);
    }

    // REMOTE channel — recv via message passing
    if channel::is_remote(ch) {
        #[cfg(feature = "std")]
        {
            let proxy = channel::remote_proxy(ch);
            if proxy.closed {
                return ChannelRecvCoreResult::Closed;
            }
            return ChannelRecvCoreResult::Remote {
                endpoint_id: proxy.endpoint_id,
                home_island: proxy.home_island,
            };
        }
        #[cfg(not(feature = "std"))]
        unreachable!("REMOTE channel in no_std");
    }

    let waiter = QueueWaiter::simple(island_id, fiber_id);
    let (result, value_opt) = channel::recv_or_block(ch, waiter);

    match result {
        RecvResult::Success(woke_sender) => {
            let data = value_opt.expect("channel recv: success without payload");
            ChannelRecvCoreResult::Success { data, wake_sender: woke_sender }
        }
        RecvResult::Blocked => ChannelRecvCoreResult::WouldBlock,
        RecvResult::WouldBlock => unreachable!("recv_or_block never returns WouldBlock"),
        RecvResult::Closed => ChannelRecvCoreResult::Closed,
    }
}

pub fn exec_chan_recv(stack: *mut Slot, bp: usize, island_id: u32, fiber_id: u32, inst: &Instruction) -> ChanResult {
    let ch = stack_get(stack, bp + inst.b as usize) as GcRef;
    let elem_slots = ((inst.flags >> 1) & 0x7F) as usize;
    let has_ok = (inst.flags & 1) != 0;
    let dst_start = bp + inst.a as usize;

    match channel_recv_core(ch, island_id, fiber_id as u64) {
        ChannelRecvCoreResult::Success { data, wake_sender } => {
            for (i, &v) in data.iter().enumerate().take(elem_slots) {
                stack_set(stack, dst_start + i, v);
            }
            if has_ok {
                stack_set(stack, dst_start + elem_slots, 1);
            }
            match wake_sender {
                Some(sender) => ChanResult::Wake(sender),
                None => ChanResult::Continue,
            }
        }
        ChannelRecvCoreResult::WouldBlock => ChanResult::ReplayThenBlock,
        ChannelRecvCoreResult::Closed => {
            for i in 0..elem_slots {
                stack_set(stack, dst_start + i, 0);
            }
            if has_ok {
                stack_set(stack, dst_start + elem_slots, 0);
            }
            ChanResult::Continue
        }
        #[cfg(feature = "std")]
        ChannelRecvCoreResult::Remote { endpoint_id, home_island } => {
            ChanResult::RemoteRecv { endpoint_id, home_island }
        }
        #[cfg(not(feature = "std"))]
        ChannelRecvCoreResult::Remote { .. } => unreachable!("remote recv in no_std"),
        ChannelRecvCoreResult::Trap(kind) => ChanResult::Trap(kind),
    }
}

#[inline]
pub fn channel_len(ch: GcRef) -> usize {
    if ch.is_null() {
        return 0;
    }
    channel::len(ch)
}

#[inline]
pub fn exec_queue_get<F>(stack: *mut Slot, bp: usize, inst: &Instruction, get: F)
where F: FnOnce(GcRef) -> usize {
    let obj = stack_get(stack, bp + inst.b as usize) as GcRef;
    let val = if obj.is_null() { 0 } else { get(obj) };
    stack_set(stack, bp + inst.a as usize, val as u64);
}

pub fn channel_close_core(ch: GcRef) -> ChanResult {
    if ch.is_null() {
        return ChanResult::Trap(RuntimeTrapKind::CloseNilChannel);
    }

    // REMOTE channel close — send message to home island
    if channel::is_remote(ch) {
        #[cfg(feature = "std")]
        {
            let proxy = channel::remote_proxy(ch);
            if proxy.closed { return ChanResult::Continue; }
            let endpoint_id = proxy.endpoint_id;
            let home_island = proxy.home_island;
            channel::remote_proxy_mut(ch).closed = true;
            return ChanResult::RemoteClose { endpoint_id, home_island };
        }
        #[cfg(not(feature = "std"))]
        unreachable!("REMOTE channel in no_std");
    }

    if channel::is_closed(ch) {
        return ChanResult::Trap(RuntimeTrapKind::CloseClosedChannel);
    }
    channel::close(ch);
    let mut waiters: Vec<QueueWaiter> = channel::take_waiting_receivers(ch);
    waiters.extend(
        channel::take_waiting_senders(ch)
            .into_iter()
            .map(|(w, _)| w)
    );
    #[cfg(feature = "std")]
    let endpoint_id = channel::home_info(ch).map(|info| info.endpoint_id);
    #[cfg(not(feature = "std"))]
    let endpoint_id = None;
    if waiters.is_empty() && endpoint_id.is_none() {
        ChanResult::Continue
    } else {
        ChanResult::Close { waiters, endpoint_id }
    }
}

#[inline]
pub fn exec_chan_close(stack: *const Slot, bp: usize, inst: &Instruction) -> ChanResult {
    let ch = stack_get(stack, bp + inst.a as usize) as GcRef;
    channel_close_core(ch)
}
