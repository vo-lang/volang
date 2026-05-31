//! Unified queue instructions: QueueNew, QueueSend, QueueRecv, QueueClose
//!
//! All queue objects (local and remote) use ValueKind::Channel.
//! Remote (cross-island) channels are dispatched via channel::is_remote().
//!

#[cfg(not(feature = "std"))]
use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    vec::Vec,
};
#[cfg(feature = "std")]
use std::{
    boxed::Box,
    string::{String, ToString},
    vec::Vec,
};

use vo_common_core::bytecode::StructMeta;
use vo_common_core::instruction::QUEUE_KIND_PORT_FLAG;
use vo_common_core::RuntimeType;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::queue::{self, RecvResult};
use vo_runtime::objects::queue_state::{self, QueueKind, QueueWaiter};
use vo_runtime::slot::Slot;
use vo_runtime::{ValueMeta, ValueRttid};

use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};
use crate::vm::RuntimeTrapKind;

#[derive(Debug)]
pub enum QueueAction {
    Continue,
    Block,
    ReplayThenBlock,
    Wake(QueueWaiter),
    Trap(RuntimeTrapKind),
    Malformed(String),
    Close {
        waiters: Vec<QueueWaiter>,
        endpoint_id: Option<u64>,
    },
    RemoteSend {
        endpoint_id: u64,
        home_island: u32,
        data: Vec<u8>,
    },
    RemoteRecv {
        endpoint_id: u64,
        home_island: u32,
    },
    RemoteRecvData {
        endpoint_id: u64,
        target_island: u32,
        fiber_id: u64,
        data: Vec<u8>,
    },
    RemoteClose {
        endpoint_id: u64,
        home_island: u32,
    },
}

pub type QueueExecResult = QueueAction;

#[derive(Debug)]
pub enum QueueRecvCoreResult {
    Success {
        data: Box<[u64]>,
        wake_sender: Option<QueueWaiter>,
    },
    WouldBlock,
    Closed,
    Remote {
        endpoint_id: u64,
        home_island: u32,
    },
    Trap(RuntimeTrapKind),
    Malformed(String),
}

pub fn complete_queue_recv<F>(
    result: QueueRecvCoreResult,
    elem_slots: usize,
    has_ok: bool,
    write_slot: F,
) -> Result<Option<QueueWaiter>, QueueRecvCoreResult>
where
    F: FnMut(usize, u64),
{
    match result {
        QueueRecvCoreResult::Success { data, wake_sender } => {
            write_recv_result(Some(data.as_ref()), elem_slots, has_ok, write_slot);
            Ok(wake_sender)
        }
        QueueRecvCoreResult::Closed => {
            write_recv_result(None, elem_slots, has_ok, write_slot);
            Ok(None)
        }
        other => Err(other),
    }
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

#[allow(clippy::too_many_arguments)]
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

#[inline]
pub fn queue_new_kind_from_flags(flags: u8) -> QueueKind {
    if (flags & QUEUE_KIND_PORT_FLAG) != 0 {
        QueueKind::Port
    } else {
        QueueKind::Chan
    }
}

#[inline]
pub fn queue_new_trap_kind(flags: u8) -> RuntimeTrapKind {
    match queue_new_kind_from_flags(flags) {
        QueueKind::Chan => RuntimeTrapKind::MakeChan,
        QueueKind::Port => RuntimeTrapKind::MakePort,
    }
}

pub type QueueNewResult = Result<(), String>;

#[inline]
pub fn exec_queue_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
) -> QueueNewResult {
    let kind = queue_new_kind_from_flags(inst.flags);
    let packed_type = stack_get(stack, bp + inst.b as usize);
    let elem_meta = ValueMeta::from_raw(packed_type as u32);
    let elem_rttid = ValueRttid::from_raw((packed_type >> 32) as u32);
    let cap = stack_get(stack, bp + inst.c as usize) as i64;
    let elem_slots = inst.queue_new_elem_slots();

    match queue::create_checked(gc, kind, elem_meta, elem_rttid, elem_slots, cap) {
        Ok(ch) => {
            stack_set(stack, bp + inst.a as usize, ch as u64);
            Ok(())
        }
        Err(_) => Err(String::from(crate::vm::helpers::make_queue_error_message(
            queue_new_trap_kind(inst.flags),
        ))),
    }
}

#[allow(clippy::too_many_arguments)]
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
        return QueueExecResult::Block;
    }

    // REMOTE channel — send via message passing
    if queue::is_remote(ch) {
        let proxy = queue::remote_proxy(ch);
        if proxy.closed {
            return QueueExecResult::Trap(RuntimeTrapKind::SendOnClosedChannel);
        }
        if let Err(msg) =
            super::prepare_remote_send_value_if_needed(ch, src, struct_metas, runtime_types, state)
        {
            return QueueExecResult::Malformed(msg);
        }
        let elem_meta = queue_state::elem_meta(ch);
        let result = QueueExecResult::RemoteSend {
            endpoint_id: proxy.endpoint_id,
            home_island: proxy.home_island,
            data: super::transport::pack_transport_message(
                &state.gc,
                src,
                elem_meta,
                struct_metas,
                runtime_types,
            ),
        };

        return result;
    }

    let value: Box<[u64]> = src.iter().copied().collect();

    // Write barrier: type-aware to avoid UB on mixed-slot types.
    // Must be done before send_or_block because the value is moved into the buffer.
    let em = queue_state::elem_meta(ch);
    if em.value_kind().may_contain_gc_refs() {
        if let Err(err) = vo_runtime::gc_types::try_typed_write_barrier_by_meta(
            &mut state.gc,
            ch,
            &value,
            em,
            module,
        ) {
            return QueueExecResult::Malformed(err.to_string());
        }
    }

    let waiter = QueueWaiter::simple(island_id, fiber_id);
    match queue::send_or_block_resolved(ch, value, waiter, island_id) {
        queue::ResolvedSendResult::Wake(receiver) => QueueExecResult::Wake(receiver),
        queue::ResolvedSendResult::RemoteDirect {
            receiver,
            payload: value,
        } => {
            if em.value_kind().may_contain_gc_refs() {
                if let Err(msg) = super::prepare_value_queue_handles_for_transfer(
                    value.as_ref(),
                    em,
                    receiver.island_id,
                    struct_metas,
                    runtime_types,
                    state,
                ) {
                    return QueueExecResult::Malformed(msg);
                }
            }
            let Some(home_info) = queue::home_info(ch) else {
                return QueueExecResult::Malformed(format!(
                    "RemoteDirect send missing HomeInfo for local port: receiver_island={} receiver_fiber={}",
                    receiver.island_id, receiver.fiber_id
                ));
            };
            let endpoint_id = home_info.endpoint_id;
            let data = super::transport::pack_transport_message(
                &state.gc,
                value.as_ref(),
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
        queue::ResolvedSendResult::Buffered => QueueExecResult::Continue,
        queue::ResolvedSendResult::Blocked => QueueExecResult::Block,
        queue::ResolvedSendResult::Closed => {
            QueueExecResult::Trap(RuntimeTrapKind::SendOnClosedChannel)
        }
    }
}

pub fn queue_recv_core(ch: GcRef, island_id: u32, fiber_id: u64) -> QueueRecvCoreResult {
    if ch.is_null() {
        return QueueRecvCoreResult::WouldBlock;
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
            if let Some(data) = value_opt {
                QueueRecvCoreResult::Success {
                    data,
                    wake_sender: woke_sender,
                }
            } else {
                QueueRecvCoreResult::Malformed(
                    "queue recv success returned without payload".to_string(),
                )
            }
        }
        RecvResult::Blocked | RecvResult::WouldBlock => QueueRecvCoreResult::WouldBlock,
        RecvResult::Closed => QueueRecvCoreResult::Closed,
    }
}

pub fn exec_queue_recv(
    stack: *mut Slot,
    bp: usize,
    island_id: u32,
    fiber_id: u32,
    inst: &Instruction,
) -> QueueExecResult {
    let ch = stack_get(stack, bp + inst.b as usize) as GcRef;
    let elem_slots = inst.recv_elem_slots() as usize;
    let has_ok = inst.recv_has_ok();
    let dst_start = bp + inst.a as usize;

    match complete_queue_recv(
        queue_recv_core(ch, island_id, fiber_id as u64),
        elem_slots,
        has_ok,
        |i, value| stack_set(stack, dst_start + i, value),
    ) {
        Ok(Some(sender)) => QueueExecResult::Wake(sender),
        Ok(None) => QueueExecResult::Continue,
        Err(QueueRecvCoreResult::WouldBlock) => QueueExecResult::ReplayThenBlock,
        Err(QueueRecvCoreResult::Remote {
            endpoint_id,
            home_island,
        }) => QueueExecResult::RemoteRecv {
            endpoint_id,
            home_island,
        },
        Err(QueueRecvCoreResult::Trap(kind)) => QueueExecResult::Trap(kind),
        Err(QueueRecvCoreResult::Malformed(msg)) => QueueExecResult::Malformed(msg),
        Err(QueueRecvCoreResult::Success { .. } | QueueRecvCoreResult::Closed) => {
            QueueExecResult::Malformed(
                "complete_queue_recv returned terminal recv result as Err".to_string(),
            )
        }
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
where
    F: FnOnce(GcRef) -> usize,
{
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
        if proxy.closed {
            return QueueExecResult::Continue;
        }
        let endpoint_id = proxy.endpoint_id;
        let home_island = proxy.home_island;
        queue::mark_remote_closed(ch);
        return QueueExecResult::RemoteClose {
            endpoint_id,
            home_island,
        };
    }

    if queue::is_closed(ch) {
        return QueueExecResult::Trap(RuntimeTrapKind::CloseClosedChannel);
    }
    queue::close(ch);
    let mut waiters: Vec<QueueWaiter> = queue::take_waiting_receivers(ch);
    waiters.extend(queue::take_waiting_senders(ch).into_iter().map(|(w, _)| w));
    let endpoint_id = queue::home_info(ch).map(|info| info.endpoint_id);
    if waiters.is_empty() && endpoint_id.is_none() {
        QueueExecResult::Continue
    } else {
        QueueExecResult::Close {
            waiters,
            endpoint_id,
        }
    }
}

#[inline]
pub fn exec_queue_close(stack: *const Slot, bp: usize, inst: &Instruction) -> QueueExecResult {
    let ch = stack_get(stack, bp + inst.a as usize) as GcRef;
    queue_close_core(ch)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fiber::RemoteRecvResponse;
    use crate::vm::{EndpointRegistry, VmState};
    use vo_common_core::bytecode::Module;
    use vo_runtime::objects::slice;
    use vo_runtime::ValueKind;

    fn make_byte_slice(gc: &mut Gc, bytes: &[u8]) -> GcRef {
        let slice_ref = slice::create(
            gc,
            ValueMeta::new(0, ValueKind::Uint8),
            1,
            bytes.len(),
            bytes.len(),
        );
        for (i, &byte) in bytes.iter().enumerate() {
            slice::set(slice_ref, i, byte as u64, 1);
        }
        slice_ref
    }

    fn assert_byte_slice_eq(slice_ref: GcRef, expected: &[u8]) {
        assert!(!slice_ref.is_null());
        assert_eq!(slice::elem_meta(slice_ref).value_kind(), ValueKind::Uint8);
        assert_eq!(slice::len(slice_ref), expected.len());
        for (i, &byte) in expected.iter().enumerate() {
            assert_eq!(slice::get(slice_ref, i, 1), byte as u64);
        }
    }

    #[test]
    fn nil_queue_send_blocks() {
        let mut state = VmState::new();
        let result = queue_send_core(
            core::ptr::null_mut(),
            &[123],
            0,
            1,
            &mut state,
            &[],
            &[],
            None,
        );
        assert!(matches!(result, QueueExecResult::Block));
    }

    #[test]
    fn nil_queue_recv_blocks() {
        let result = queue_recv_core(core::ptr::null_mut(), 0, 1);
        assert!(matches!(result, QueueRecvCoreResult::WouldBlock));
    }

    #[test]
    fn remote_direct_send_without_home_info_is_malformed_instead_of_expect_panic() {
        let mut state = VmState::new();
        let ch = queue::create(
            &mut state.gc,
            QueueKind::Port,
            ValueMeta::new(0, ValueKind::Int64),
            ValueRttid::new(0, ValueKind::Int64),
            1,
            1,
        );
        queue::register_receiver(ch, QueueWaiter::simple(7, 99));

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            queue_send_core(ch, &[123], 0, 1, &mut state, &[], &[], None)
        }));

        match result {
            Ok(QueueExecResult::Malformed(msg)) => {
                assert!(msg.contains("RemoteDirect send missing HomeInfo"), "{msg}");
            }
            Ok(other) => {
                panic!("missing HomeInfo should be a malformed queue action, got {other:?}")
            }
            Err(_) => panic!("missing HomeInfo must not panic"),
        }
    }

    #[test]
    fn queue_send_core_reports_typed_barrier_metadata_errors_without_unwinding() {
        let mut state = VmState::new();
        let module = Module::new("queue-barrier-contract".to_string());
        let ch = queue::create(
            &mut state.gc,
            QueueKind::Chan,
            ValueMeta::new(99, ValueKind::Struct),
            ValueRttid::new(999, ValueKind::Struct),
            1,
            1,
        );

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            queue_send_core(ch, &[0], 0, 1, &mut state, &[], &[], Some(&module))
        }));

        match result {
            Ok(QueueExecResult::Malformed(msg)) => {
                assert!(msg.contains("typed_write_barrier_by_meta"), "{msg}");
                assert!(msg.contains("missing StructMeta"), "{msg}");
            }
            Ok(other) => panic!("invalid typed barrier metadata must be malformed, got {other:?}"),
            Err(_) => panic!("JIT-reachable queue_send_core must not unwind on metadata errors"),
        }
    }

    #[test]
    fn remote_queue_send_rejects_non_sendable_metadata_before_pack() {
        let mut state = VmState::new();
        let ch = queue::create_remote_proxy(
            &mut state.gc,
            QueueKind::Port,
            77,
            8,
            1,
            ValueMeta::new(0, ValueKind::Interface),
            ValueRttid::new(0, ValueKind::Interface),
            2,
        );

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            queue_send_core(ch, &[0, 0], 0, 1, &mut state, &[], &[], None)
        }));

        match result {
            Ok(QueueExecResult::Malformed(msg)) => {
                assert!(msg.contains("non-sendable"), "{msg}");
                assert!(msg.contains("Interface"), "{msg}");
            }
            Ok(other) => {
                panic!("non-sendable remote queue metadata must be malformed, got {other:?}")
            }
            Err(_) => panic!("non-sendable remote queue metadata must not panic in pack"),
        }
    }

    #[test]
    fn queue_send_core_uses_result_typed_barrier_for_jit_callback_path() {
        let source = include_str!("queue.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("queue source should contain tests section");

        assert!(
            source.contains("try_typed_write_barrier_by_meta"),
            "queue_send_core must use the Result-returning typed barrier helper"
        );
        assert!(
            !source.contains("typed_write_barrier_by_meta(&mut state.gc"),
            "queue_send_core is called from JIT callbacks and must not call the panic barrier"
        );
    }

    #[test]
    fn replay_remote_queue_recv_response_restores_closed_result() {
        let mut gc = Gc::new();
        let struct_metas = vec![];
        let runtime_types = vec![];
        let mut endpoint_registry = EndpointRegistry::new();
        let response = RemoteRecvResponse {
            data: Vec::new(),
            closed: true,
        };
        let mut dst = [99u64, 99u64];

        replay_remote_queue_recv_response(
            &mut gc,
            response,
            1,
            true,
            &struct_metas,
            &runtime_types,
            &mut endpoint_registry,
            |i, value| dst[i] = value,
        );

        assert_eq!(dst[0], 0);
        assert_eq!(dst[1], 0);
    }

    #[test]
    fn replay_remote_queue_recv_response_restores_empty_byte_slice() {
        let mut gc = Gc::new();
        let struct_metas = vec![];
        let runtime_types = vec![];
        let mut endpoint_registry = EndpointRegistry::new();
        let slice_ref = make_byte_slice(&mut gc, &[]);
        let response = RemoteRecvResponse {
            data: crate::exec::transport::pack_transport_message(
                &gc,
                &[slice_ref as u64],
                ValueMeta::new(0, ValueKind::Slice),
                &struct_metas,
                &runtime_types,
            ),
            closed: false,
        };
        let mut dst = [0u64; 2];

        replay_remote_queue_recv_response(
            &mut gc,
            response,
            1,
            true,
            &struct_metas,
            &runtime_types,
            &mut endpoint_registry,
            |i, value| dst[i] = value,
        );

        let unpacked = dst[0] as GcRef;
        assert_byte_slice_eq(unpacked, &[]);
        assert_eq!(dst[1], 1);
        assert_ne!(slice_ref, unpacked);
        assert_ne!(slice::array_ref(slice_ref), slice::array_ref(unpacked));
    }

    #[test]
    fn replay_remote_queue_recv_response_restores_non_empty_byte_slice() {
        let mut gc = Gc::new();
        let struct_metas = vec![];
        let runtime_types = vec![];
        let mut endpoint_registry = EndpointRegistry::new();
        let slice_ref = make_byte_slice(&mut gc, &[30, 1, 0, 0, 0]);
        let response = RemoteRecvResponse {
            data: crate::exec::transport::pack_transport_message(
                &gc,
                &[slice_ref as u64],
                ValueMeta::new(0, ValueKind::Slice),
                &struct_metas,
                &runtime_types,
            ),
            closed: false,
        };
        let mut dst = [0u64; 2];

        replay_remote_queue_recv_response(
            &mut gc,
            response,
            1,
            true,
            &struct_metas,
            &runtime_types,
            &mut endpoint_registry,
            |i, value| dst[i] = value,
        );

        let unpacked = dst[0] as GcRef;
        assert_byte_slice_eq(unpacked, &[30, 1, 0, 0, 0]);
        assert_eq!(dst[1], 1);
        assert_ne!(slice_ref, unpacked);
        assert_ne!(slice::array_ref(slice_ref), slice::array_ref(unpacked));
    }
}
