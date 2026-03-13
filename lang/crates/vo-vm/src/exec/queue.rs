//! Unified channel instructions: ChanNew, ChanSend, ChanRecv, ChanClose
//!
//! All queue objects (local and remote) use ValueKind::Channel.
//! Remote (cross-island) channels are dispatched via channel::is_remote().
//!

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, string::String, vec::Vec, format};
#[cfg(feature = "std")]
use std::{boxed::Box, vec::Vec};

use vo_common_core::bytecode::StructMeta;
use vo_common_core::RuntimeType;
use vo_runtime::{ValueMeta, ValueRttid};
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::queue::{self, RecvResult, SendResult};
use vo_runtime::objects::queue_state::{self, QueueKind, QueueWaiter};
use vo_runtime::slot::Slot;

use crate::instruction::Instruction;
use crate::vm::RuntimeTrapKind;
use crate::vm::helpers::{stack_get, stack_set};

pub type QueueExecResult = super::QueueAction;

pub enum QueueRecvCoreResult {
    Success { data: Box<[u64]>, wake_sender: Option<QueueWaiter> },
    WouldBlock,
    Closed,
    Remote { endpoint_id: u64, home_island: u32 },
    Trap(RuntimeTrapKind),
}

pub fn decode_remote_queue_recv_response(
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
            endpoint_registry,
        ))
    }
}

pub fn write_recv_result<F>(
    data: Option<&[u64]>,
    elem_slots: usize,
    has_ok: bool,
    mut write_slot: F,
) where
    F: FnMut(usize, u64),
{
    match data {
        Some(data) => {
            for (i, &value) in data.iter().enumerate().take(elem_slots) {
                write_slot(i, value);
            }
            if has_ok {
                write_slot(elem_slots, 1);
            }
        }
        None => {
            for i in 0..elem_slots {
                write_slot(i, 0);
            }
            if has_ok {
                write_slot(elem_slots, 0);
            }
        }
    }
}

pub fn replay_remote_queue_recv_response<F>(
    gc: &mut Gc,
    response: crate::fiber::RemoteRecvResponse,
    elem_slots: usize,
    has_ok: bool,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    endpoint_registry: &mut crate::vm::EndpointRegistry,
    write_slot: F,
) where
    F: FnMut(usize, u64),
{
    let decoded = decode_remote_queue_recv_response(
        gc,
        response,
        elem_slots,
        struct_metas,
        runtime_types,
        endpoint_registry,
    );
    write_recv_result(decoded.as_deref(), elem_slots, has_ok, write_slot);
}

/// Result of exec_chan_new: Ok(()) on success, Err(msg) on invalid parameters
pub type QueueNewResult = Result<(), String>;

#[inline]
pub fn exec_chan_new(stack: *mut Slot, bp: usize, inst: &Instruction, gc: &mut Gc) -> QueueNewResult {
    exec_queue_new(stack, bp, inst, gc, QueueKind::Chan, "makechan")
}

#[inline]
pub fn exec_port_new(stack: *mut Slot, bp: usize, inst: &Instruction, gc: &mut Gc) -> QueueNewResult {
    exec_queue_new(stack, bp, inst, gc, QueueKind::Port, "makeport")
}

#[inline]
fn exec_queue_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    kind: QueueKind,
    err_name: &str,
) -> QueueNewResult {
    let packed_type = stack_get(stack, bp + inst.b as usize);
    let elem_meta = ValueMeta::from_raw(packed_type as u32);
    let elem_rttid = ValueRttid::from_raw((packed_type >> 32) as u32);
    let cap = stack_get(stack, bp + inst.c as usize) as i64;
    let elem_slots = inst.flags as u16;

    match queue::create_checked(gc, kind, elem_meta, elem_rttid, elem_slots, cap) {
        Ok(ch) => {
            stack_set(stack, bp + inst.a as usize, ch as u64);
            Ok(())
        }
        Err(_) => Err(format!("runtime error: {}: size out of range", err_name)),
    }
}

pub fn queue_send_core(
    ch: GcRef,
    src: &[u64],
    island_id: u32,
    fiber_id: u64,
    state: &mut crate::vm::VmState,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    module: Option<&vo_runtime::bytecode::Module>,
) -> QueueExecResult {
    if ch.is_null() {
        return QueueExecResult::Trap(RuntimeTrapKind::SendOnNilChannel);
    }

    // REMOTE channel — send via message passing
    if queue::is_remote(ch) {
        let proxy = queue::remote_proxy(ch);
        if proxy.closed { return QueueExecResult::Trap(RuntimeTrapKind::SendOnClosedChannel); }
        let elem_meta = queue_state::elem_meta(ch);
        return QueueExecResult::RemoteSend {
            endpoint_id: proxy.endpoint_id,
            home_island: proxy.home_island,
            data: super::transport::pack_transport_message(&state.gc, src, elem_meta, struct_metas, runtime_types),
        };
    }

    let value: Box<[u64]> = src.iter().copied().collect();

    // Write barrier: type-aware to avoid UB on mixed-slot types.
    // Must be done before send_or_block because the value is moved into the buffer.
    let em = queue_state::elem_meta(ch);
    if em.value_kind().may_contain_gc_refs() {
        vo_runtime::gc_types::typed_write_barrier_by_meta(&mut state.gc, ch, &value, em, module);
    }

    let waiter = QueueWaiter::simple(island_id, fiber_id);
    match queue::send_or_block(ch, value, waiter) {
        SendResult::DirectSend(receiver) => {
            if receiver.island_id == island_id {
                QueueExecResult::Wake(receiver)
            } else {
                queue::pop_back_buffer(ch);
                if em.value_kind().may_contain_gc_refs() {
                    super::prepare_value_queue_handles_for_transfer(
                        src,
                        em,
                        receiver.island_id,
                        struct_metas,
                        runtime_types,
                        state,
                    );
                }
                let endpoint_id = queue::home_info(ch)
                    .expect("DirectSend to remote waiter requires HomeInfo")
                    .endpoint_id;
                let data = super::transport::pack_transport_message(
                    &state.gc,
                    src,
                    em,
                    struct_metas,
                    runtime_types,
                );
                QueueExecResult::RemoteRecvData {
                    endpoint_id,
                    target_island: receiver.island_id,
                    fiber_id: receiver.fiber_id,
                    data,
                }
            }
        }
        SendResult::Buffered => QueueExecResult::Continue,
        SendResult::Blocked => QueueExecResult::Block,
        SendResult::WouldBlock(_) => unreachable!("send_or_block never returns WouldBlock"),
        SendResult::Closed => QueueExecResult::Trap(RuntimeTrapKind::SendOnClosedChannel),
    }
}

pub fn exec_queue_send(
    stack: *const Slot,
    bp: usize,
    island_id: u32,
    fiber_id: u32,
    inst: &Instruction,
    state: &mut crate::vm::VmState,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    module: Option<&vo_runtime::bytecode::Module>,
) -> QueueExecResult {
    let ch = stack_get(stack, bp + inst.a as usize) as GcRef;
    let elem_slots = inst.flags as usize;
    let src_start = bp + inst.b as usize;
    let src: Vec<u64> = (0..elem_slots).map(|i| stack_get(stack, src_start + i)).collect();
    queue_send_core(
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

pub fn queue_recv_core(ch: GcRef, island_id: u32, fiber_id: u64) -> QueueRecvCoreResult {
    if ch.is_null() {
        return QueueRecvCoreResult::Trap(RuntimeTrapKind::RecvOnNilChannel);
    }

    // REMOTE channel — recv via message passing
    if queue::is_remote(ch) {
        let proxy = queue::remote_proxy(ch);
        if proxy.closed {
            return QueueRecvCoreResult::Closed;
        }
        return QueueRecvCoreResult::Remote {
            endpoint_id: proxy.endpoint_id,
            home_island: proxy.home_island,
        };
    }

    let waiter = QueueWaiter::simple(island_id, fiber_id);
    let (result, value_opt) = queue::recv_or_block(ch, waiter);

    match result {
        RecvResult::Success(woke_sender) => {
            let data = value_opt.expect("channel recv: success without payload");
            QueueRecvCoreResult::Success { data, wake_sender: woke_sender }
        }
        RecvResult::Blocked => QueueRecvCoreResult::WouldBlock,
        RecvResult::WouldBlock => unreachable!("recv_or_block never returns WouldBlock"),
        RecvResult::Closed => QueueRecvCoreResult::Closed,
    }
}

pub fn exec_queue_recv(stack: *mut Slot, bp: usize, island_id: u32, fiber_id: u32, inst: &Instruction) -> QueueExecResult {
    let ch = stack_get(stack, bp + inst.b as usize) as GcRef;
    let elem_slots = ((inst.flags >> 1) & 0x7F) as usize;
    let has_ok = (inst.flags & 1) != 0;
    let dst_start = bp + inst.a as usize;

    match queue_recv_core(ch, island_id, fiber_id as u64) {
        QueueRecvCoreResult::Success { data, wake_sender } => {
            write_recv_result(Some(data.as_ref()), elem_slots, has_ok, |i, value| {
                stack_set(stack, dst_start + i, value);
            });
            match wake_sender {
                Some(sender) => QueueExecResult::Wake(sender),
                None => QueueExecResult::Continue,
            }
        }
        QueueRecvCoreResult::WouldBlock => QueueExecResult::ReplayThenBlock,
        QueueRecvCoreResult::Closed => {
            write_recv_result(None, elem_slots, has_ok, |i, value| {
                stack_set(stack, dst_start + i, value);
            });
            QueueExecResult::Continue
        }
        QueueRecvCoreResult::Remote { endpoint_id, home_island } => {
            QueueExecResult::RemoteRecv { endpoint_id, home_island }
        }
        QueueRecvCoreResult::Trap(kind) => QueueExecResult::Trap(kind),
    }
}

#[inline]
pub fn queue_len(ch: GcRef) -> usize {
    if ch.is_null() {
        return 0;
    }
    queue::len(ch)
}

#[inline]
pub fn exec_queue_get<F>(stack: *mut Slot, bp: usize, inst: &Instruction, get: F)
where F: FnOnce(GcRef) -> usize {
    let obj = stack_get(stack, bp + inst.b as usize) as GcRef;
    let val = if obj.is_null() { 0 } else { get(obj) };
    stack_set(stack, bp + inst.a as usize, val as u64);
}

pub fn queue_close_core(ch: GcRef) -> QueueExecResult {
    if ch.is_null() {
        return QueueExecResult::Trap(RuntimeTrapKind::CloseNilChannel);
    }

    // REMOTE channel close — send message to home island
    if queue::is_remote(ch) {
        let proxy = queue::remote_proxy(ch);
        if proxy.closed { return QueueExecResult::Continue; }
        let endpoint_id = proxy.endpoint_id;
        let home_island = proxy.home_island;
        queue::remote_proxy_mut(ch).closed = true;
        return QueueExecResult::RemoteClose { endpoint_id, home_island };
    }

    if queue::is_closed(ch) {
        return QueueExecResult::Trap(RuntimeTrapKind::CloseClosedChannel);
    }
    queue::close(ch);
    let mut waiters: Vec<QueueWaiter> = queue::take_waiting_receivers(ch);
    waiters.extend(
        queue::take_waiting_senders(ch)
            .into_iter()
            .map(|(w, _)| w)
    );
    let endpoint_id = queue::home_info(ch).map(|info| info.endpoint_id);
    if waiters.is_empty() && endpoint_id.is_none() {
        QueueExecResult::Continue
    } else {
        QueueExecResult::Close { waiters, endpoint_id }
    }
}

#[inline]
pub fn exec_queue_close(stack: *const Slot, bp: usize, inst: &Instruction) -> QueueExecResult {
    let ch = stack_get(stack, bp + inst.a as usize) as GcRef;
    queue_close_core(ch)
}
