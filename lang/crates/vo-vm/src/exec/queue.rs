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
    vec,
    vec::Vec,
};
#[cfg(feature = "std")]
use std::{
    boxed::Box,
    string::{String, ToString},
    vec::Vec,
};

use vo_common_core::bytecode::{Module, StructMeta};
use vo_common_core::instruction::QUEUE_KIND_PORT_FLAG;
use vo_common_core::RuntimeType;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::queue::{self, BlockingRecvResult};
use vo_runtime::objects::queue_state::{
    self, QueueKind, QueueMessage, QueueWaiter, SelectWaitKind,
};
use vo_runtime::slot::Slot;
use vo_runtime::{SlotType, ValueKind, ValueMeta, ValueRttid};

use crate::fiber::SelectWokenResult;
use crate::instruction::Instruction;
use crate::runtime_boundary::IslandCommandEffect;
use crate::vm::helpers::{stack_get, stack_set};
use crate::vm::RuntimeTrapKind;

pub fn validate_queue_handle(gc: &Gc, q: GcRef, context: &str) -> Result<GcRef, String> {
    let Some(base) = gc.canonicalize_ref(q) else {
        return Err(format!("{context}: invalid queue handle"));
    };
    if base != q {
        return Err(format!("{context}: queue handle must be an object base"));
    }
    let kind = unsafe { Gc::header(base) }.kind();
    if !kind.is_queue() {
        return Err(format!("{context}: expected queue handle, got {:?}", kind));
    }
    Ok(base)
}

pub fn validate_queue_payload_slots(
    ch: GcRef,
    payload_slots: usize,
    context: &str,
) -> Result<(), String> {
    // Safety: callers validate the queue handle before payload validation.
    let expected = unsafe { queue_state::elem_slots(ch) } as usize;
    if payload_slots != expected {
        return Err(format!(
            "{context} payload slots {payload_slots} do not match queue element slots {expected}"
        ));
    }
    Ok(())
}

pub fn select_woken_recv_slot_types(
    ch: GcRef,
    module: Option<&vo_runtime::bytecode::Module>,
) -> Result<Vec<SlotType>, String> {
    // Safety: callers validate the queue handle before layout inspection.
    let elem_meta = unsafe { queue_state::elem_meta(ch) };
    let elem_slots = unsafe { queue_state::elem_slots(ch) } as usize;
    let kind = elem_meta.value_kind();
    Ok(match kind {
        ValueKind::Struct => {
            let meta_id = elem_meta.meta_id() as usize;
            module
                .and_then(|module| module.struct_metas.get(meta_id))
                .map(|meta| meta.slot_types.clone())
                .ok_or_else(|| {
                    format!(
                        "select wake recv missing StructMeta id {meta_id} for payload root scan"
                    )
                })?
        }
        ValueKind::Array => {
            let module = module.ok_or_else(|| {
                "select wake recv missing module runtime metadata for array payload root scan"
                    .to_string()
            })?;
            let rttid = if elem_meta.meta_id() != 0 {
                ValueRttid::new(elem_meta.meta_id(), ValueKind::Array)
            } else {
                unsafe { queue_state::elem_rttid(ch) }
            };
            select_woken_slot_types_for_rttid(rttid, module)?
        }
        ValueKind::Interface => vec![SlotType::Interface0, SlotType::Interface1],
        ValueKind::Float32 | ValueKind::Float64 => vec![SlotType::Float; elem_slots],
        kind if kind.may_contain_gc_refs() => vec![SlotType::GcRef; elem_slots],
        _ => vec![SlotType::Value; elem_slots],
    })
}

pub fn validate_queue_payload_layout(
    ch: GcRef,
    payload_layout: &[SlotType],
    context: &str,
    module: Option<&vo_runtime::bytecode::Module>,
) -> Result<(), String> {
    let expected = select_woken_recv_slot_types(ch, module)?;
    if payload_layout != expected.as_slice() {
        return Err(format!(
            "{context} payload layout {payload_layout:?} does not match queue element layout {expected:?}"
        ));
    }
    validate_queue_payload_slots(ch, payload_layout.len(), context)
}

pub fn preflight_island_route(
    state: &crate::vm::VmState,
    target_island: u32,
    context: &str,
) -> Result<(), String> {
    if target_island == state.current_island_id {
        return Ok(());
    }
    #[cfg(feature = "std")]
    {
        state
            .can_route_to_island(target_island)
            .map_err(|error| format!("{context}: {error}"))
    }
    #[cfg(not(feature = "std"))]
    {
        let _ = (state, target_island, context);
        Ok(())
    }
}

pub fn preflight_queue_close_routes(state: &crate::vm::VmState, ch: GcRef) -> Result<(), String> {
    if ch.is_null() {
        return Ok(());
    }
    let Ok(ch) = validate_queue_handle(&state.gc, ch, "QueueClose") else {
        return Ok(());
    };
    if unsafe { queue::is_remote(ch) } {
        let proxy = unsafe { queue::remote_proxy(ch) };
        if !proxy.closed {
            preflight_island_route(state, proxy.home_island, "QueueClose remote home route")?;
        }
        return Ok(());
    }
    if let Some(info) = unsafe { queue::home_info(ch) } {
        for peer in info.peers.iter().copied() {
            preflight_island_route(state, peer, "QueueClose endpoint peer route")?;
        }
    }
    Ok(())
}

pub fn preflight_queue_send_routes(state: &crate::vm::VmState, ch: GcRef) -> Result<(), String> {
    if ch.is_null() {
        return Ok(());
    }
    let Ok(ch) = validate_queue_handle(&state.gc, ch, "QueueSend") else {
        return Ok(());
    };
    if unsafe { queue::is_remote(ch) } {
        let proxy = unsafe { queue::remote_proxy(ch) };
        if !proxy.closed {
            preflight_island_route(state, proxy.home_island, "QueueSend remote home route")?;
        }
        return Ok(());
    }
    if let Some(receiver) =
        unsafe { queue::next_remote_direct_receiver(ch, state.current_island_id) }
    {
        if unsafe { queue::home_info(ch) }.is_none() {
            return Err(format!(
                "RemoteDirect send missing HomeInfo for local port: receiver_island={} receiver_key={}",
                receiver.island_id, receiver.fiber_key
            ));
        }
        preflight_island_route(
            state,
            receiver.island_id,
            "QueueSend remote receiver response route",
        )?;
    }
    Ok(())
}

pub fn preflight_queue_recv_routes(state: &crate::vm::VmState, ch: GcRef) -> Result<(), String> {
    if ch.is_null() {
        return Ok(());
    }
    let Ok(ch) = validate_queue_handle(&state.gc, ch, "QueueRecv") else {
        return Ok(());
    };
    if unsafe { queue::is_remote(ch) } {
        let proxy = unsafe { queue::remote_proxy(ch) };
        if !proxy.closed {
            preflight_island_route(state, proxy.home_island, "QueueRecv remote home route")?;
        }
        return Ok(());
    }
    queue_recv_endpoint_ack_preflight(ch)?;
    if let Some(sender) = unsafe { queue::next_recv_endpoint_sender(ch) } {
        preflight_island_route(
            state,
            sender.island_id,
            "QueueRecv remote sender response route",
        )?;
    }
    Ok(())
}

fn select_woken_slot_types_for_rttid(
    rttid: ValueRttid,
    module: &vo_runtime::bytecode::Module,
) -> Result<Vec<SlotType>, String> {
    module
        .runtime_type_resolver()
        .slot_layout_for_value_rttid(rttid)
        .ok_or_else(|| {
            format!(
                "select wake recv missing, cyclic, or oversized runtime slot layout for rttid {}",
                rttid.rttid()
            )
        })
}

pub(crate) fn validate_select_woken_recv_payload_width(
    payload_len: usize,
    slot_types_len: usize,
) -> Result<(), String> {
    if slot_types_len != payload_len {
        return Err(format!(
            "select wake recv payload width {} does not match slot metadata {}",
            payload_len, slot_types_len
        ));
    }
    Ok(())
}

pub(crate) fn validate_select_woken_recv_payload_contract(
    payload_len: usize,
    slot_types_len: usize,
    elem_slots: usize,
    closed: bool,
) -> Result<(), String> {
    if closed {
        if payload_len != 0 || slot_types_len != 0 {
            return Err(format!(
                "closed select wake recv carried payload width {payload_len} and slot metadata {slot_types_len}"
            ));
        }
        return Ok(());
    }
    validate_select_woken_recv_payload_width(payload_len, slot_types_len)?;
    if payload_len != elem_slots {
        return Err(format!(
            "select wake recv payload width {payload_len} does not match element slots {elem_slots}"
        ));
    }
    Ok(())
}

pub(crate) fn validate_select_woken_recv_payload_layout(
    payload_len: usize,
    slot_types: &[SlotType],
    expected_slot_types: &[SlotType],
    closed: bool,
) -> Result<(), String> {
    if closed {
        if payload_len != 0 || !slot_types.is_empty() {
            return Err(format!(
                "closed select wake recv carried payload width {payload_len} and slot metadata {}",
                slot_types.len()
            ));
        }
        return Ok(());
    }
    validate_select_woken_recv_payload_width(payload_len, slot_types.len())?;
    if slot_types != expected_slot_types {
        return Err(format!(
            "select wake recv slot metadata {:?} does not match queue element layout {:?}",
            slot_types, expected_slot_types
        ));
    }
    Ok(())
}

pub fn select_woken_recv_payload_with_slot_types(
    payload: QueueMessage,
    slot_types: Vec<SlotType>,
) -> Result<SelectWokenResult, String> {
    validate_select_woken_recv_payload_width(payload.len(), slot_types.len())?;
    Ok(SelectWokenResult::Recv {
        data: payload.into_vec(),
        slot_types,
        closed: false,
    })
}

pub fn select_woken_recv_payload(
    ch: GcRef,
    payload: QueueMessage,
    module: Option<&vo_runtime::bytecode::Module>,
) -> Result<SelectWokenResult, String> {
    let slot_types = select_woken_recv_slot_types(ch, module)?;
    select_woken_recv_payload_with_slot_types(payload, slot_types)
}

#[derive(Debug)]
pub enum QueueAction {
    Continue,
    Block {
        waiter: Option<QueueWaiter>,
    },
    ReplayThenBlock {
        waiter: Option<QueueWaiter>,
    },
    Wake {
        waiter: QueueWaiter,
        payload: Option<SelectWokenResult>,
    },
    Trap(RuntimeTrapKind),
    Malformed(String),
    Close {
        receivers: Vec<QueueWaiter>,
        senders: Vec<QueueWaiter>,
        endpoint_id: Option<u64>,
        rollback: crate::runtime_boundary::RuntimeRollback,
    },
    RemoteSend {
        endpoint_id: u64,
        home_island: u32,
        data: Vec<u8>,
        island_effects: Vec<IslandCommandEffect>,
        transfer_commit: super::QueueTransferCommit,
    },
    RemoteRecv {
        endpoint_id: u64,
        home_island: u32,
    },
    RemoteSendAck {
        endpoint_id: u64,
        target_island: u32,
        fiber_key: u64,
        wait_id: u64,
        closed: bool,
        rollback: Option<crate::runtime_boundary::RuntimeRollback>,
    },
    RemoteRecvData {
        endpoint_id: u64,
        target_island: u32,
        fiber_key: u64,
        wait_id: u64,
        data: Vec<u8>,
        island_effects: Vec<IslandCommandEffect>,
        rollback: Option<crate::runtime_boundary::RuntimeRollback>,
    },
    RemoteClose {
        endpoint_id: u64,
        home_island: u32,
        rollback: crate::runtime_boundary::RuntimeRollback,
    },
}

pub type QueueExecResult = QueueAction;

#[derive(Debug)]
pub enum QueueRecvCoreResult {
    Success {
        data: Box<[u64]>,
        wake_sender: Option<QueueWaiter>,
    },
    WouldBlock {
        waiter: Option<QueueWaiter>,
    },
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
            if data.len() != elem_slots {
                return Err(QueueRecvCoreResult::Malformed(format!(
                    "QueueRecv payload slots {} do not match queue element slots {elem_slots}",
                    data.len()
                )));
            }
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
    elem_meta: ValueMeta,
    elem_rttid: ValueRttid,
    elem_slots: usize,
    struct_metas: &[StructMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[RuntimeType],
    endpoint_registry: &mut crate::vm::EndpointRegistry,
) -> Result<Option<Box<[u64]>>, super::transport::QueueHandleValidationError> {
    if response.rejected {
        Err(super::transport::QueueHandleValidationError::EndpointRecvRejected)
    } else if response.closed {
        Ok(None)
    } else {
        super::transport::unpack_transport_message(
            gc,
            &response.data,
            elem_meta,
            elem_rttid,
            elem_slots,
            struct_metas,
            named_type_metas,
            runtime_types,
            endpoint_registry,
        )
        .map(Some)
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

pub fn stack_slot_snapshot(stack: *const Slot, start: usize, len: usize) -> Vec<(usize, Slot)> {
    (0..len)
        .map(|offset| {
            let index = start + offset;
            (index, stack_get(stack, index))
        })
        .collect()
}

#[allow(clippy::too_many_arguments)]
pub fn replay_remote_queue_recv_response<F>(
    gc: &mut Gc,
    response: crate::fiber::RemoteRecvResponse,
    elem_meta: ValueMeta,
    elem_rttid: ValueRttid,
    elem_slots: usize,
    has_ok: bool,
    struct_metas: &[StructMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[RuntimeType],
    endpoint_registry: &mut crate::vm::EndpointRegistry,
    write_slot: F,
) -> Result<(), super::transport::QueueHandleValidationError>
where
    F: FnMut(usize, u64),
{
    let decoded = decode_remote_queue_recv_response(
        gc,
        response,
        elem_meta,
        elem_rttid,
        elem_slots,
        struct_metas,
        named_type_metas,
        runtime_types,
        endpoint_registry,
    )?;
    write_recv_result(decoded.as_deref(), elem_slots, has_ok, write_slot);
    Ok(())
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

enum QueueSendPayload<'a> {
    Borrowed(&'a [u64]),
    Owned(QueueMessage),
}

impl QueueSendPayload<'_> {
    #[inline]
    fn as_slice(&self) -> &[u64] {
        match self {
            Self::Borrowed(value) => value,
            Self::Owned(value) => value,
        }
    }

    #[inline]
    fn into_owned(self) -> QueueMessage {
        match self {
            Self::Borrowed(value) => value.into(),
            Self::Owned(value) => value,
        }
    }
}

struct LocalQueueMutation {
    ch: GcRef,
    state: vo_runtime::objects::queue_state::LocalQueueState,
    endpoint_registry: crate::vm::EndpointRegistrySnapshot,
    transfer_commit: super::QueueTransferCommit,
}

impl LocalQueueMutation {
    fn snapshot(state: &crate::vm::VmState, ch: GcRef) -> Self {
        Self {
            ch,
            state: unsafe { queue::local_state(ch) }.clone(),
            endpoint_registry: state.endpoint_registry.snapshot(),
            transfer_commit: super::QueueTransferCommit::default(),
        }
    }

    fn absorb_transfer(&mut self, commit: super::QueueTransferCommit) {
        self.transfer_commit.absorb(commit);
    }

    fn rollback(self, state: &mut crate::vm::VmState) {
        self.transfer_commit
            .restore_committed_local_endpoint_state(state);
        unsafe {
            queue::with_local_state(self.ch, |local_state| {
                *local_state = self.state;
            })
        };
        state.endpoint_registry.restore(self.endpoint_registry);
        state.mark_gc_all_roots_dirty();
    }

    fn into_runtime_rollback(self) -> crate::runtime_boundary::RuntimeRollback {
        let local = crate::runtime_boundary::RuntimeRollback::local_queue_from_snapshot(
            self.ch,
            self.state,
            self.endpoint_registry,
        );
        match self.transfer_commit.into_runtime_rollback() {
            Some(transfer) => crate::runtime_boundary::RuntimeRollback::combine(local, transfer),
            None => local,
        }
    }
}

#[inline]
pub fn exec_queue_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    module: &Module,
    elem_layout: &[SlotType],
) -> QueueNewResult {
    let kind = queue_new_kind_from_flags(inst.flags);
    let packed_type = stack_get(stack, bp + inst.b as usize);
    let elem_meta = ValueMeta::from_raw(packed_type as u32);
    let elem_rttid = ValueRttid::from_raw((packed_type >> 32) as u32);
    let cap = stack_get(stack, bp + inst.c as usize) as i64;
    let elem_slots = u16::try_from(elem_layout.len())
        .map_err(|_| "QueueNew QueueLayout element slot count exceeds u16::MAX".to_string())?;

    match queue::create_checked_with_module(
        gc, kind, elem_meta, elem_rttid, elem_slots, cap, module,
    ) {
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
    fiber_key: u64,
    state: &mut crate::vm::VmState,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    module: Option<&vo_runtime::bytecode::Module>,
) -> QueueExecResult {
    queue_send_core_with_layout(
        ch,
        src,
        None,
        island_id,
        fiber_key,
        state,
        struct_metas,
        runtime_types,
        module,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn queue_send_core_with_layout(
    ch: GcRef,
    src: &[u64],
    src_layout: Option<&[SlotType]>,
    island_id: u32,
    fiber_key: u64,
    state: &mut crate::vm::VmState,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    module: Option<&vo_runtime::bytecode::Module>,
) -> QueueExecResult {
    queue_send_payload_core(
        ch,
        QueueSendPayload::Borrowed(src),
        src_layout,
        island_id,
        fiber_key,
        state,
        struct_metas,
        runtime_types,
        module,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn queue_send_owned_core_with_layout(
    ch: GcRef,
    value: QueueMessage,
    src_layout: Option<&[SlotType]>,
    island_id: u32,
    fiber_key: u64,
    state: &mut crate::vm::VmState,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    module: Option<&vo_runtime::bytecode::Module>,
) -> QueueExecResult {
    queue_send_payload_core(
        ch,
        QueueSendPayload::Owned(value),
        src_layout,
        island_id,
        fiber_key,
        state,
        struct_metas,
        runtime_types,
        module,
    )
}

#[allow(clippy::too_many_arguments)]
fn queue_send_payload_core(
    ch: GcRef,
    payload: QueueSendPayload<'_>,
    src_layout: Option<&[SlotType]>,
    island_id: u32,
    fiber_key: u64,
    state: &mut crate::vm::VmState,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    module: Option<&vo_runtime::bytecode::Module>,
) -> QueueExecResult {
    let src = payload.as_slice();
    if ch.is_null() {
        return QueueExecResult::Block { waiter: None };
    }
    let ch = match validate_queue_handle(&state.gc, ch, "QueueSend") {
        Ok(ch) => ch,
        Err(msg) => return QueueExecResult::Malformed(msg),
    };
    if let Err(msg) = validate_queue_payload_slots(ch, src.len(), "QueueSend") {
        return QueueExecResult::Malformed(msg);
    }
    if let Some(src_layout) = src_layout {
        if let Err(msg) = validate_queue_payload_layout(ch, src_layout, "QueueSend", module) {
            return QueueExecResult::Malformed(msg);
        }
    }
    if let Err(msg) = preflight_queue_send_routes(state, ch) {
        return QueueExecResult::Malformed(msg);
    }

    // REMOTE channel — send via message passing
    if unsafe { queue::is_remote(ch) } {
        let proxy = unsafe { queue::remote_proxy(ch) };
        if proxy.closed {
            return QueueExecResult::Trap(RuntimeTrapKind::SendOnClosedChannel);
        }
        let mut island_effects = Vec::new();
        let transfer_commit = match super::prepare_remote_send_value_if_needed(
            ch,
            src,
            struct_metas,
            module.map(|m| m.named_type_metas.as_slice()).unwrap_or(&[]),
            runtime_types,
            state,
            &mut island_effects,
        ) {
            Ok(commit) => commit,
            Err(msg) => return QueueExecResult::Malformed(msg),
        };
        let elem_meta = unsafe { queue_state::elem_meta(ch) };
        let data = match unsafe {
            super::transport::pack_transport_message(
                &state.gc,
                src,
                elem_meta,
                struct_metas,
                module.map(|m| m.named_type_metas.as_slice()).unwrap_or(&[]),
                runtime_types,
            )
        } {
            Ok(data) => data,
            Err(error) => {
                transfer_commit.restore_committed_local_endpoint_state(state);
                return QueueExecResult::Malformed(format!(
                    "failed to pack remote send payload: {error}"
                ));
            }
        };
        let result = QueueExecResult::RemoteSend {
            endpoint_id: proxy.endpoint_id,
            home_island: proxy.home_island,
            data,
            island_effects,
            transfer_commit,
        };

        return result;
    }

    let value = payload.into_owned();

    // Write barrier: type-aware to avoid UB on mixed-slot types.
    // Must be done before send_or_block because the value is moved into the buffer.
    let em = unsafe { queue_state::elem_meta(ch) };
    let remote_direct_receiver = unsafe { queue::next_remote_direct_receiver(ch, island_id) };
    if let Some(receiver) = remote_direct_receiver.as_ref() {
        if unsafe { queue::home_info(ch) }.is_none() {
            return QueueExecResult::Malformed(format!(
                "RemoteDirect send missing HomeInfo for local port: receiver_island={} receiver_key={}",
                receiver.island_id, receiver.fiber_key
            ));
        }
    }
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
        if remote_direct_receiver.is_some() {
            if let Err(msg) = super::validate_value_queue_handles_for_transfer(
                value.as_ref(),
                em,
                island_id,
                struct_metas,
                module.map(|m| m.named_type_metas.as_slice()).unwrap_or(&[]),
                runtime_types,
                state,
            ) {
                return QueueExecResult::Malformed(msg);
            }
        }
    }
    let select_recv_slot_types =
        if unsafe { queue::next_local_select_recv_receiver(ch, island_id) }.is_some() {
            match select_woken_recv_slot_types(ch, module).and_then(|slot_types| {
                validate_select_woken_recv_payload_width(value.len(), slot_types.len())?;
                Ok(slot_types)
            }) {
                Ok(slot_types) => Some(slot_types),
                Err(msg) => return QueueExecResult::Malformed(msg),
            }
        } else {
            None
        };

    let waiter = match QueueWaiter::try_simple_queue(
        island_id,
        fiber_key,
        ch as u64,
        SelectWaitKind::Send,
    ) {
        Ok(waiter) => waiter,
        Err(err) => return QueueExecResult::Malformed(err.to_string()),
    };
    let mut select_recv_slot_types = select_recv_slot_types;
    let mut mutation = LocalQueueMutation::snapshot(state, ch);
    match unsafe { queue::send_or_block_resolved(ch, value, waiter.clone(), island_id) } {
        queue::ResolvedSendResult::Wake { receiver, payload } => {
            let payload = match payload {
                Some(payload) => {
                    let Some(slot_types) = select_recv_slot_types.take() else {
                        mutation.rollback(state);
                        return QueueExecResult::Malformed(
                            "select wake recv payload returned without preflight".to_string(),
                        );
                    };
                    match select_woken_recv_payload_with_slot_types(payload, slot_types) {
                        Ok(payload) => Some(payload),
                        Err(msg) => {
                            mutation.rollback(state);
                            return QueueExecResult::Malformed(msg);
                        }
                    }
                }
                None => None,
            };
            QueueExecResult::Wake {
                waiter: receiver,
                payload,
            }
        }
        queue::ResolvedSendResult::RemoteDirect {
            receiver,
            payload: value,
        } => {
            let mut island_effects = Vec::new();
            let transfer_commit = match super::prepare_value_queue_handles_for_transfer_with_commit(
                value.as_ref(),
                em,
                receiver.island_id,
                struct_metas,
                module.map(|m| m.named_type_metas.as_slice()).unwrap_or(&[]),
                runtime_types,
                state,
                &mut island_effects,
            ) {
                Ok(commit) => commit,
                Err(msg) => {
                    mutation.rollback(state);
                    return QueueExecResult::Malformed(msg);
                }
            };
            mutation.absorb_transfer(transfer_commit);
            let Some(home_info) = (unsafe { queue::home_info(ch) }) else {
                mutation.rollback(state);
                return QueueExecResult::Malformed(format!(
                    "RemoteDirect send missing HomeInfo for local port: receiver_island={} receiver_key={}",
                    receiver.island_id, receiver.fiber_key
                ));
            };
            let endpoint_id = home_info.endpoint_id;
            // Safety: the validated queue metadata matches `value`, which remains
            // rooted until the transport payload is materialized.
            let data = match unsafe {
                super::transport::pack_transport_message(
                    &state.gc,
                    value.as_ref(),
                    em,
                    struct_metas,
                    module.map(|m| m.named_type_metas.as_slice()).unwrap_or(&[]),
                    runtime_types,
                )
            } {
                Ok(data) => data,
                Err(error) => {
                    mutation.rollback(state);
                    return QueueExecResult::Malformed(format!(
                        "failed to pack remote receive payload: {error}"
                    ));
                }
            };
            let rollback = mutation.into_runtime_rollback();
            QueueExecResult::RemoteRecvData {
                endpoint_id,
                target_island: receiver.island_id,
                fiber_key: receiver.fiber_key,
                wait_id: receiver.endpoint_wait_id(),
                data,
                island_effects,
                rollback: Some(rollback),
            }
        }
        queue::ResolvedSendResult::Buffered => QueueExecResult::Continue,
        queue::ResolvedSendResult::Blocked => QueueExecResult::Block {
            waiter: Some(waiter),
        },
        queue::ResolvedSendResult::Closed => {
            QueueExecResult::Trap(RuntimeTrapKind::SendOnClosedChannel)
        }
    }
}

pub fn queue_recv_core(gc: &Gc, ch: GcRef, island_id: u32, fiber_key: u64) -> QueueRecvCoreResult {
    if ch.is_null() {
        return QueueRecvCoreResult::WouldBlock { waiter: None };
    }
    let ch = match validate_queue_handle(gc, ch, "QueueRecv") {
        Ok(ch) => ch,
        Err(msg) => return QueueRecvCoreResult::Malformed(msg),
    };

    // REMOTE channel — recv via message passing
    if unsafe { queue::is_remote(ch) } {
        let proxy = unsafe { queue::remote_proxy(ch) };
        if proxy.closed {
            return QueueRecvCoreResult::Closed;
        }
        return QueueRecvCoreResult::Remote {
            endpoint_id: proxy.endpoint_id,
            home_island: proxy.home_island,
        };
    }
    if let Err(msg) = queue_recv_endpoint_ack_preflight(ch) {
        return QueueRecvCoreResult::Malformed(msg);
    }

    let waiter = match QueueWaiter::try_simple_queue(
        island_id,
        fiber_key,
        ch as u64,
        SelectWaitKind::Recv,
    ) {
        Ok(waiter) => waiter,
        Err(err) => return QueueRecvCoreResult::Malformed(err.to_string()),
    };
    match unsafe { queue::recv_or_block(ch, waiter.clone()) } {
        BlockingRecvResult::Success {
            woke_sender,
            payload,
        } => QueueRecvCoreResult::Success {
            data: payload,
            wake_sender: woke_sender,
        },
        BlockingRecvResult::Blocked => QueueRecvCoreResult::WouldBlock {
            waiter: Some(waiter),
        },
        BlockingRecvResult::Closed => QueueRecvCoreResult::Closed,
    }
}

pub fn queue_recv_endpoint_ack_preflight(ch: GcRef) -> Result<(), String> {
    if let Some(sender) = unsafe { queue::next_recv_endpoint_sender(ch) } {
        if unsafe { queue::home_info(ch) }.is_none() {
            return Err(format!(
                "remote endpoint sender missing HomeInfo: sender_island={} sender_key={}",
                sender.island_id,
                sender.fiber_key()
            ));
        }
    }
    Ok(())
}

pub fn queue_sender_ack_or_wake(
    ch: GcRef,
    sender: QueueWaiter,
    closed: bool,
    rollback: Option<crate::runtime_boundary::RuntimeRollback>,
) -> QueueExecResult {
    if sender.endpoint_wait_id() == 0 {
        return QueueExecResult::Wake {
            waiter: sender,
            payload: None,
        };
    }
    let Some(home_info) = (unsafe { queue::home_info(ch) }) else {
        return QueueExecResult::Malformed(format!(
            "remote endpoint sender missing HomeInfo: sender_island={} sender_key={}",
            sender.island_id,
            sender.fiber_key()
        ));
    };
    QueueExecResult::RemoteSendAck {
        endpoint_id: home_info.endpoint_id,
        target_island: sender.island_id,
        fiber_key: sender.fiber_key(),
        wait_id: sender.endpoint_wait_id(),
        closed,
        rollback,
    }
}

pub fn exec_queue_recv(
    stack: *mut Slot,
    bp: usize,
    island_id: u32,
    fiber_key: u64,
    inst: &Instruction,
    state: &crate::vm::VmState,
    module: Option<&vo_runtime::bytecode::Module>,
    elem_layout: Option<&[SlotType]>,
) -> QueueExecResult {
    let ch = stack_get(stack, bp + inst.b as usize) as GcRef;
    // Verified bytecode always supplies QueueLayout. The legacy flag width is
    // retained only for direct low-level callers exercising old instructions.
    let elem_slots = elem_layout
        .map(<[SlotType]>::len)
        .unwrap_or_else(|| inst.recv_legacy_elem_slots() as usize);
    let has_ok = inst.recv_has_ok();
    let dst_start = bp + inst.a as usize;

    if !ch.is_null() {
        let ch = match validate_queue_handle(&state.gc, ch, "QueueRecv") {
            Ok(ch) => ch,
            Err(msg) => return QueueExecResult::Malformed(msg),
        };
        if let Err(msg) = validate_queue_payload_slots(ch, elem_slots, "QueueRecv") {
            return QueueExecResult::Malformed(msg);
        }
        if let Some(elem_layout) = elem_layout {
            if let Err(msg) = validate_queue_payload_layout(ch, elem_layout, "QueueRecv", module) {
                return QueueExecResult::Malformed(msg);
            }
        }
    }
    if let Err(msg) = preflight_queue_recv_routes(state, ch) {
        return QueueExecResult::Malformed(msg);
    }
    let remote_sender_rollback = if !ch.is_null()
        && !unsafe { queue::is_remote(ch) }
        && unsafe { queue::next_recv_endpoint_sender(ch) }.is_some()
    {
        Some(
            crate::runtime_boundary::RuntimeRollback::local_queue_with_stack_slots(
                state,
                ch,
                stack_slot_snapshot(stack, dst_start, elem_slots + usize::from(has_ok)),
            ),
        )
    } else {
        None
    };

    match complete_queue_recv(
        queue_recv_core(&state.gc, ch, island_id, fiber_key),
        elem_slots,
        has_ok,
        |i, value| stack_set(stack, dst_start + i, value),
    ) {
        Ok(Some(sender)) => queue_sender_ack_or_wake(ch, sender, false, remote_sender_rollback),
        Ok(None) => QueueExecResult::Continue,
        Err(QueueRecvCoreResult::WouldBlock { waiter }) => {
            QueueExecResult::ReplayThenBlock { waiter }
        }
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
pub unsafe fn queue_len(ch: GcRef) -> usize {
    if ch.is_null() {
        return 0;
    }
    // Safety: callers validate non-null queue handles before using this helper.
    unsafe { queue::len(ch) }
}

#[inline]
pub fn exec_queue_get<F>(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &Gc,
    get: F,
) -> QueueExecResult
where
    F: FnOnce(GcRef) -> usize,
{
    let obj = stack_get(stack, bp + inst.b as usize) as GcRef;
    let val = if obj.is_null() {
        0
    } else {
        let obj = match validate_queue_handle(gc, obj, "QueueGet") {
            Ok(obj) => obj,
            Err(msg) => return QueueExecResult::Malformed(msg),
        };
        get(obj)
    };
    stack_set(stack, bp + inst.a as usize, val as u64);
    QueueExecResult::Continue
}

pub fn queue_close_core(state: &crate::vm::VmState, ch: GcRef) -> QueueExecResult {
    if ch.is_null() {
        return QueueExecResult::Trap(RuntimeTrapKind::CloseNilChannel);
    }
    let ch = match validate_queue_handle(&state.gc, ch, "QueueClose") {
        Ok(ch) => ch,
        Err(msg) => return QueueExecResult::Malformed(msg),
    };

    // REMOTE channel close — send message to home island
    if unsafe { queue::is_remote(ch) } {
        let proxy = unsafe { queue::remote_proxy(ch) };
        if proxy.closed {
            return QueueExecResult::Continue;
        }
        let endpoint_id = proxy.endpoint_id;
        let home_island = proxy.home_island;
        let rollback = crate::runtime_boundary::RuntimeRollback::remote_queue_proxy(state, ch);
        unsafe { queue::mark_remote_closed(ch) };
        return QueueExecResult::RemoteClose {
            endpoint_id,
            home_island,
            rollback,
        };
    }

    if unsafe { queue::is_closed(ch) } {
        return QueueExecResult::Trap(RuntimeTrapKind::CloseClosedChannel);
    }
    if unsafe { queue::has_endpoint_waiters(ch) } && unsafe { queue::home_info(ch) }.is_none() {
        return QueueExecResult::Malformed(
            "QueueClose missing HomeInfo for remote endpoint waiters".to_string(),
        );
    }
    let rollback = crate::runtime_boundary::RuntimeRollback::local_queue(state, ch);
    unsafe { queue::close(ch) };
    let receivers = unsafe { queue::take_waiting_receivers(ch) };
    let senders = unsafe { queue::take_waiting_senders(ch) }
        .into_iter()
        .map(|(w, _)| w)
        .collect::<Vec<_>>();
    let endpoint_id = unsafe { queue::home_info(ch) }.map(|info| info.endpoint_id);
    if receivers.is_empty() && senders.is_empty() && endpoint_id.is_none() {
        QueueExecResult::Continue
    } else {
        QueueExecResult::Close {
            receivers,
            senders,
            endpoint_id,
            rollback,
        }
    }
}

#[inline]
pub fn exec_queue_close(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    state: &crate::vm::VmState,
) -> QueueExecResult {
    let ch = stack_get(stack, bp + inst.a as usize) as GcRef;
    queue_close_core(state, ch)
}

#[cfg(test)]
mod tests;
