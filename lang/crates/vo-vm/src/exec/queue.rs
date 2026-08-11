//! Unified queue instructions: QueueNew, QueueSend, QueueRecv, QueueClose
//!
//! All queue objects (local and remote) use ValueKind::Channel.
//! Remote (cross-island) channels are dispatched via channel::is_remote().
//!

#[cfg(not(feature = "std"))]
use alloc::{
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
#[cfg(feature = "std")]
use std::{
    string::{String, ToString},
    vec::Vec,
};

use vo_common_core::bytecode::{Module, ModuleRuntimeMetadata, StructMeta};
use vo_common_core::instruction::QUEUE_KIND_PORT_FLAG;
use vo_common_core::RuntimeType;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::island::EndpointWaitKey;
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
    module: Option<ModuleRuntimeMetadata<'_>>,
) -> Result<Vec<SlotType>, String> {
    // Safety: callers validate the queue handle before layout inspection.
    let elem_meta = unsafe { queue_state::elem_meta(ch) };
    let elem_slots = unsafe { queue_state::elem_slots(ch) } as usize;
    let kind = elem_meta.value_kind();
    Ok(match kind {
        ValueKind::Struct => {
            let meta_id = elem_meta.meta_id() as usize;
            module
                .and_then(|metadata| metadata.module().struct_metas.get(meta_id))
                .map(|meta| meta.slot_types.clone())
                .ok_or_else(|| {
                    format!(
                        "select wake recv missing StructMeta id {meta_id} for payload root scan"
                    )
                })?
        }
        ValueKind::Array => {
            let metadata = module.ok_or_else(|| {
                "select wake recv missing module runtime metadata for array payload root scan"
                    .to_string()
            })?;
            let rttid = if elem_meta.meta_id() != 0 {
                ValueRttid::new(elem_meta.meta_id(), ValueKind::Array)
            } else {
                unsafe { queue_state::elem_rttid(ch) }
            };
            select_woken_slot_types_for_rttid(rttid, metadata.module())?
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
    module: Option<ModuleRuntimeMetadata<'_>>,
) -> Result<(), String> {
    validate_queue_payload_slots(ch, payload_layout.len(), context)?;
    let elem_meta = unsafe { queue_state::elem_meta(ch) };
    let matches = match elem_meta.value_kind() {
        ValueKind::Struct => {
            let meta_id = elem_meta.meta_id() as usize;
            let expected = module
                .and_then(|metadata| metadata.module().struct_metas.get(meta_id))
                .ok_or_else(|| {
                    format!(
                        "select wake recv missing StructMeta id {meta_id} for payload root scan"
                    )
                })?;
            payload_layout == expected.slot_types.as_slice()
        }
        ValueKind::Array => {
            let expected = select_woken_recv_slot_types(ch, module)?;
            payload_layout == expected.as_slice()
        }
        ValueKind::Interface => payload_layout == [SlotType::Interface0, SlotType::Interface1],
        ValueKind::Float32 | ValueKind::Float64 => {
            payload_layout.iter().all(|slot| *slot == SlotType::Float)
        }
        kind if kind.may_contain_gc_refs() => {
            payload_layout.iter().all(|slot| *slot == SlotType::GcRef)
        }
        _ => payload_layout.iter().all(|slot| *slot == SlotType::Value),
    };
    if !matches {
        let expected = select_woken_recv_slot_types(ch, module)?;
        return Err(format!(
            "{context} payload layout {payload_layout:?} does not match queue element layout {expected:?}"
        ));
    }
    Ok(())
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

fn preflight_queue_close_routes_validated(
    state: &crate::vm::VmState,
    ch: GcRef,
) -> Result<(), String> {
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

fn preflight_queue_send_routes_validated(
    state: &crate::vm::VmState,
    ch: GcRef,
) -> Result<(), String> {
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
                receiver.island_id(), receiver.fiber_key()
            ));
        }
        preflight_island_route(
            state,
            receiver.island_id(),
            "QueueSend remote receiver response route",
        )?;
    }
    Ok(())
}

pub(crate) fn preflight_queue_recv_routes_validated(
    state: &crate::vm::VmState,
    ch: GcRef,
) -> Result<(), String> {
    if unsafe { queue::is_remote(ch) } {
        let proxy = unsafe { queue::remote_proxy(ch) };
        if !proxy.closed {
            preflight_island_route(state, proxy.home_island, "QueueRecv remote home route")?;
        }
        return Ok(());
    }
    if let Some(sender) = unsafe { queue::next_recv_endpoint_sender(ch) } {
        if unsafe { queue::home_info(ch) }.is_none() {
            return Err(format!(
                "remote endpoint sender missing HomeInfo: sender_island={} sender_key={}",
                sender.island_id(),
                sender.fiber_key()
            ));
        }
        preflight_island_route(
            state,
            sender.island_id(),
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
        ch: GcRef,
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
        wait_key: EndpointWaitKey,
        closed: bool,
        rollback: crate::runtime_boundary::RuntimeRollback,
    },
    RemoteRecvData {
        endpoint_id: u64,
        target_island: u32,
        wait_key: EndpointWaitKey,
        data: Vec<u8>,
        island_effects: Vec<IslandCommandEffect>,
        rollback: crate::runtime_boundary::RuntimeRollback,
    },
    RemoteClose {
        endpoint_id: u64,
        home_island: u32,
        rollback: crate::runtime_boundary::RuntimeRollback,
    },
}

#[derive(Debug)]
pub enum QueueRecvCoreResult {
    Success {
        data: QueueMessage,
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
) -> Result<Option<QueueMessage>, super::transport::QueueHandleValidationError> {
    match response {
        crate::fiber::RemoteRecvResponse::Rejected => {
            Err(super::transport::QueueHandleValidationError::EndpointRecvRejected)
        }
        crate::fiber::RemoteRecvResponse::Closed => Ok(None),
        crate::fiber::RemoteRecvResponse::Data(data) => super::transport::unpack_transport_message(
            gc,
            &data,
            elem_meta,
            elem_rttid,
            elem_slots,
            struct_metas,
            named_type_metas,
            runtime_types,
            endpoint_registry,
        )
        .map(Some),
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

fn restore_direct_receiver(
    state: &mut crate::vm::VmState,
    ch: GcRef,
    receiver: QueueWaiter,
    transfer: super::QueueTransferCommit,
) {
    transfer.restore_committed_local_endpoint_state(state);
    unsafe { queue::restore_direct_receiver(ch, receiver) };
    state.mark_gc_all_roots_dirty();
}

fn direct_receiver_rollback(
    ch: GcRef,
    receiver: QueueWaiter,
    transfer: super::QueueTransferCommit,
) -> crate::runtime_boundary::RuntimeRollback {
    let receiver = crate::runtime_boundary::RuntimeRollback::direct_queue_receiver(ch, receiver);
    match transfer.into_runtime_rollback() {
        Some(transfer) => crate::runtime_boundary::RuntimeRollback::combine(receiver, transfer),
        None => receiver,
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
) -> Result<(), String> {
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

/// Copy a payload into Island-owned storage and publish every queue edge that
/// can keep it live beyond the current instruction.
pub(crate) fn prepare_local_queue_payload(
    state: &mut crate::vm::VmState,
    ch: GcRef,
    src: &[u64],
    module: Option<ModuleRuntimeMetadata<'_>>,
    context: &str,
) -> Result<(QueueMessage, ValueMeta), String> {
    let value = QueueMessage::managed(&mut state.gc, src)
        .map_err(|error| format!("{context} Island allocation failed: {error}"))?;
    if let Some(backing) = value.backing_ref() {
        state.gc.write_barrier(ch, backing);
    }
    let elem_meta = unsafe { queue_state::elem_meta(ch) };
    if elem_meta.value_kind().may_contain_gc_refs() {
        vo_runtime::gc_types::try_typed_write_barrier_by_meta(
            &mut state.gc,
            ch,
            &value,
            elem_meta,
            module,
        )
        .map_err(|error| error.to_string())?;
    }
    Ok((value, elem_meta))
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
    module: Option<ModuleRuntimeMetadata<'_>>,
) -> QueueAction {
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
    module: Option<ModuleRuntimeMetadata<'_>>,
) -> QueueAction {
    if ch.is_null() {
        return QueueAction::Block { waiter: None };
    }
    let ch = match validate_queue_handle(&state.gc, ch, "QueueSend") {
        Ok(ch) => ch,
        Err(msg) => return QueueAction::Malformed(msg),
    };
    if let Err(msg) = validate_queue_payload_slots(ch, src.len(), "QueueSend") {
        return QueueAction::Malformed(msg);
    }
    if let Some(src_layout) = src_layout {
        if let Err(msg) = validate_queue_payload_layout(ch, src_layout, "QueueSend", module) {
            return QueueAction::Malformed(msg);
        }
    }
    if unsafe { queue::is_closed(ch) } {
        return QueueAction::Trap(RuntimeTrapKind::SendOnClosedChannel);
    }
    if let Err(msg) = preflight_queue_send_routes_validated(state, ch) {
        return QueueAction::Malformed(msg);
    }

    let raw_module = module.map(ModuleRuntimeMetadata::module);

    // REMOTE channel — send via message passing
    if unsafe { queue::is_remote(ch) } {
        let proxy = unsafe { queue::remote_proxy(ch) };
        let mut island_effects = Vec::new();
        let transfer_commit = match super::prepare_remote_send_value_if_needed(
            ch,
            src,
            struct_metas,
            raw_module
                .map(|module| module.named_type_metas.as_slice())
                .unwrap_or(&[]),
            runtime_types,
            state,
            &mut island_effects,
        ) {
            Ok(commit) => commit,
            Err(msg) => return QueueAction::Malformed(msg),
        };
        let elem_meta = unsafe { queue_state::elem_meta(ch) };
        let data = match unsafe {
            super::transport::pack_transport_message(
                &state.gc,
                src,
                elem_meta,
                struct_metas,
                raw_module
                    .map(|module| module.named_type_metas.as_slice())
                    .unwrap_or(&[]),
                runtime_types,
            )
        } {
            Ok(data) => data,
            Err(error) => {
                transfer_commit.restore_committed_local_endpoint_state(state);
                return QueueAction::Malformed(format!(
                    "failed to pack remote send payload: {error}"
                ));
            }
        };
        let result = QueueAction::RemoteSend {
            endpoint_id: proxy.endpoint_id,
            home_island: proxy.home_island,
            data,
            island_effects,
            transfer_commit,
        };

        return result;
    }

    let (value, em) = match prepare_local_queue_payload(state, ch, src, module, "QueueSend") {
        Ok(prepared) => prepared,
        Err(message) => return QueueAction::Malformed(message),
    };
    let remote_direct_receiver = unsafe { queue::next_remote_direct_receiver(ch, island_id) };
    if em.value_kind().may_contain_gc_refs() && remote_direct_receiver.is_some() {
        if let Err(msg) = super::validate_value_queue_handles_for_transfer(
            value.as_ref(),
            em,
            island_id,
            struct_metas,
            raw_module
                .map(|module| module.named_type_metas.as_slice())
                .unwrap_or(&[]),
            runtime_types,
            state,
        ) {
            return QueueAction::Malformed(msg);
        }
    }
    let select_recv_slot_types =
        if unsafe { queue::next_local_select_recv_receiver(ch, island_id) }.is_some() {
            match select_woken_recv_slot_types(ch, module).and_then(|slot_types| {
                validate_select_woken_recv_payload_width(value.len(), slot_types.len())?;
                Ok(slot_types)
            }) {
                Ok(slot_types) => Some(slot_types),
                Err(msg) => return QueueAction::Malformed(msg),
            }
        } else {
            None
        };

    let mut select_recv_slot_types = select_recv_slot_types;
    let result = unsafe {
        queue::try_send_or_block_resolved_with(
            ch,
            value,
            || QueueWaiter::try_queue(island_id, fiber_key, ch as u64, SelectWaitKind::Send),
            island_id,
        )
    };
    let result = match result {
        Ok(result) => result,
        Err(err) => return QueueAction::Malformed(err.to_string()),
    };
    match result {
        queue::ResolvedSendResult::Wake { receiver, payload } => {
            let payload = match payload {
                Some(payload) => {
                    let Some(slot_types) = select_recv_slot_types.take() else {
                        unsafe { queue::restore_direct_receiver(ch, receiver) };
                        return QueueAction::Malformed(
                            "select wake recv payload returned without preflight".to_string(),
                        );
                    };
                    match select_woken_recv_payload_with_slot_types(payload, slot_types) {
                        Ok(payload) => Some(payload),
                        Err(msg) => {
                            unsafe { queue::restore_direct_receiver(ch, receiver) };
                            return QueueAction::Malformed(msg);
                        }
                    }
                }
                None => None,
            };
            QueueAction::Wake {
                waiter: receiver,
                payload,
            }
        }
        queue::ResolvedSendResult::RemoteDirect {
            receiver,
            payload: value,
        } => {
            let target_island = receiver.island_id();
            let receiver_key = receiver.fiber_key();
            let mut island_effects = Vec::new();
            let transfer_commit = match super::prepare_value_queue_handles_for_transfer_with_commit(
                value.as_ref(),
                em,
                target_island,
                struct_metas,
                raw_module
                    .map(|module| module.named_type_metas.as_slice())
                    .unwrap_or(&[]),
                runtime_types,
                state,
                &mut island_effects,
            ) {
                Ok(commit) => commit,
                Err(msg) => {
                    unsafe { queue::restore_direct_receiver(ch, receiver) };
                    return QueueAction::Malformed(msg);
                }
            };
            let Some(home_info) = (unsafe { queue::home_info(ch) }) else {
                restore_direct_receiver(state, ch, receiver, transfer_commit);
                return QueueAction::Malformed(format!(
                    "RemoteDirect send missing HomeInfo for local port: receiver_island={} receiver_key={}",
                    target_island, receiver_key
                ));
            };
            let endpoint_id = home_info.endpoint_id;
            let Some(wait_key) = receiver.endpoint_wait_key() else {
                restore_direct_receiver(state, ch, receiver, transfer_commit);
                return QueueAction::Malformed(
                    "RemoteDirect receiver missing endpoint wait identity".to_string(),
                );
            };
            // Safety: the validated queue metadata matches `value`, which remains
            // rooted until the transport payload is materialized.
            let data = match unsafe {
                super::transport::pack_transport_message(
                    &state.gc,
                    value.as_ref(),
                    em,
                    struct_metas,
                    raw_module
                        .map(|module| module.named_type_metas.as_slice())
                        .unwrap_or(&[]),
                    runtime_types,
                )
            } {
                Ok(data) => data,
                Err(error) => {
                    restore_direct_receiver(state, ch, receiver, transfer_commit);
                    return QueueAction::Malformed(format!(
                        "failed to pack remote receive payload: {error}"
                    ));
                }
            };
            let rollback = direct_receiver_rollback(ch, receiver, transfer_commit);
            QueueAction::RemoteRecvData {
                endpoint_id,
                target_island,
                wait_key,
                data,
                island_effects,
                rollback,
            }
        }
        queue::ResolvedSendResult::Buffered => QueueAction::Continue,
        queue::ResolvedSendResult::Blocked(waiter) => QueueAction::Block {
            waiter: Some(waiter),
        },
        queue::ResolvedSendResult::Closed => {
            QueueAction::Trap(RuntimeTrapKind::SendOnClosedChannel)
        }
    }
}

/// Execute receive after the caller validated every non-null queue handle.
pub(crate) unsafe fn queue_recv_validated_core(
    state: &crate::vm::VmState,
    ch: GcRef,
    island_id: u32,
    fiber_key: u64,
) -> QueueRecvCoreResult {
    if ch.is_null() {
        return QueueRecvCoreResult::WouldBlock { waiter: None };
    }
    if let Err(msg) = preflight_queue_recv_routes_validated(state, ch) {
        return QueueRecvCoreResult::Malformed(msg);
    }
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
    let result = unsafe {
        queue::try_recv_or_block_with(ch, || {
            QueueWaiter::try_queue(island_id, fiber_key, ch as u64, SelectWaitKind::Recv)
        })
    };
    match result {
        Err(err) => QueueRecvCoreResult::Malformed(err.to_string()),
        Ok(BlockingRecvResult::Success {
            woke_sender,
            payload,
        }) => QueueRecvCoreResult::Success {
            data: payload,
            wake_sender: woke_sender,
        },
        Ok(BlockingRecvResult::Blocked(waiter)) => QueueRecvCoreResult::WouldBlock {
            waiter: Some(waiter),
        },
        Ok(BlockingRecvResult::Closed) => QueueRecvCoreResult::Closed,
    }
}

pub fn queue_sender_ack_or_wake(
    ch: GcRef,
    sender: QueueWaiter,
    closed: bool,
    rollback: Option<crate::runtime_boundary::RuntimeRollback>,
) -> QueueAction {
    let Some(wait_key) = sender.endpoint_wait_key() else {
        return QueueAction::Wake {
            waiter: sender,
            payload: None,
        };
    };
    let Some(home_info) = (unsafe { queue::home_info(ch) }) else {
        return QueueAction::Malformed(format!(
            "remote endpoint sender missing HomeInfo: sender_island={} sender_key={}",
            sender.island_id(),
            sender.fiber_key()
        ));
    };
    let Some(rollback) = rollback else {
        return QueueAction::Malformed("remote endpoint sender missing queue rollback".to_string());
    };
    QueueAction::RemoteSendAck {
        endpoint_id: home_info.endpoint_id,
        target_island: sender.island_id(),
        wait_key,
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
    module: Option<ModuleRuntimeMetadata<'_>>,
    elem_layout: &[SlotType],
) -> QueueAction {
    let ch = stack_get(stack, bp + inst.b as usize) as GcRef;
    let elem_slots = elem_layout.len();
    let has_ok = inst.recv_has_ok();
    let dst_start = bp + inst.a as usize;

    if !ch.is_null() {
        let ch = match validate_queue_handle(&state.gc, ch, "QueueRecv") {
            Ok(ch) => ch,
            Err(msg) => return QueueAction::Malformed(msg),
        };
        if let Err(msg) = validate_queue_payload_slots(ch, elem_slots, "QueueRecv") {
            return QueueAction::Malformed(msg);
        }
        if let Err(msg) = validate_queue_payload_layout(ch, elem_layout, "QueueRecv", module) {
            return QueueAction::Malformed(msg);
        }
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
        unsafe { queue_recv_validated_core(state, ch, island_id, fiber_key) },
        elem_slots,
        has_ok,
        |i, value| stack_set(stack, dst_start + i, value),
    ) {
        Ok(Some(sender)) => queue_sender_ack_or_wake(ch, sender, false, remote_sender_rollback),
        Ok(None) => QueueAction::Continue,
        Err(QueueRecvCoreResult::WouldBlock { waiter }) => QueueAction::ReplayThenBlock { waiter },
        Err(QueueRecvCoreResult::Remote {
            endpoint_id,
            home_island,
        }) => QueueAction::RemoteRecv {
            endpoint_id,
            home_island,
        },
        Err(QueueRecvCoreResult::Trap(kind)) => QueueAction::Trap(kind),
        Err(QueueRecvCoreResult::Malformed(msg)) => QueueAction::Malformed(msg),
        Err(QueueRecvCoreResult::Success { .. } | QueueRecvCoreResult::Closed) => {
            QueueAction::Malformed(
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
) -> QueueAction
where
    F: FnOnce(GcRef) -> usize,
{
    let obj = stack_get(stack, bp + inst.b as usize) as GcRef;
    let val = if obj.is_null() {
        0
    } else {
        let obj = match validate_queue_handle(gc, obj, "QueueGet") {
            Ok(obj) => obj,
            Err(msg) => return QueueAction::Malformed(msg),
        };
        get(obj)
    };
    stack_set(stack, bp + inst.a as usize, val as u64);
    QueueAction::Continue
}

pub fn queue_close_core(state: &crate::vm::VmState, ch: GcRef) -> QueueAction {
    if ch.is_null() {
        return QueueAction::Trap(RuntimeTrapKind::CloseNilChannel);
    }
    let ch = match validate_queue_handle(&state.gc, ch, "QueueClose") {
        Ok(ch) => ch,
        Err(msg) => return QueueAction::Malformed(msg),
    };
    if let Err(msg) = preflight_queue_close_routes_validated(state, ch) {
        return QueueAction::Malformed(msg);
    }

    // REMOTE channel close — send message to home island
    if unsafe { queue::is_remote(ch) } {
        let proxy = unsafe { queue::remote_proxy(ch) };
        if proxy.closed {
            return QueueAction::Continue;
        }
        let endpoint_id = proxy.endpoint_id;
        let home_island = proxy.home_island;
        let rollback = crate::runtime_boundary::RuntimeRollback::remote_queue_proxy(state, ch);
        unsafe { queue::mark_remote_closed(ch) };
        return QueueAction::RemoteClose {
            endpoint_id,
            home_island,
            rollback,
        };
    }

    if unsafe { queue::is_closed(ch) } {
        return QueueAction::Trap(RuntimeTrapKind::CloseClosedChannel);
    }
    if unsafe { queue::has_endpoint_waiters(ch) } && unsafe { queue::home_info(ch) }.is_none() {
        return QueueAction::Malformed(
            "QueueClose missing HomeInfo for remote endpoint waiters".to_string(),
        );
    }
    let rollback = crate::runtime_boundary::RuntimeRollback::local_queue_close(ch);
    unsafe { queue::close(ch) };
    let local = unsafe { queue::local_state(ch) };
    let receivers: Vec<QueueWaiter> = local.waiting_receivers.iter().cloned().collect();
    let senders: Vec<QueueWaiter> = local
        .waiting_senders
        .iter()
        .map(|(waiter, _)| waiter.clone())
        .collect();
    let endpoint_id = unsafe { queue::home_info(ch) }.map(|info| info.endpoint_id);
    if receivers.is_empty() && senders.is_empty() && endpoint_id.is_none() {
        QueueAction::Continue
    } else {
        QueueAction::Close {
            ch,
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
) -> QueueAction {
    let ch = stack_get(stack, bp + inst.a as usize) as GcRef;
    queue_close_core(state, ch)
}

#[cfg(test)]
mod tests;
