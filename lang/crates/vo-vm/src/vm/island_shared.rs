#[cfg(not(feature = "std"))]
use alloc::{
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
#[cfg(feature = "std")]
use std::{string::String, vec, vec::Vec};

use core::fmt;
use vo_runtime::gc::GcRef;
use vo_runtime::island::{EndpointRequestKind, EndpointResponseKind, EndpointWaitKey};
use vo_runtime::island_msg;
use vo_runtime::objects::queue;
use vo_runtime::objects::queue_state::{
    BlockingRecvResult, LocalQueueState, QueueBacking, QueueMessage, QueueWaitTarget, QueueWaiter,
    ResolvedSendResult,
};

use crate::bytecode::ModuleRuntimeMetadata;
use crate::runtime_boundary::{
    validate_canonical_fiber_key, EndpointTombstone, IslandCommandEffect, ResumePolicy,
    RuntimeBoundary, RuntimeCommand, RuntimeCommandOutcome, RuntimeTransition, WakeCommand,
};
use crate::vm::GcRootEffect;

use super::types::{EndpointEntry, VmError};
use super::{helpers, Vm};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SpawnFiberError {
    message: String,
}

impl SpawnFiberError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for SpawnFiberError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

pub(crate) fn handle_spawn_fiber(vm: &mut Vm, data: &[u8]) -> Result<(), SpawnFiberError> {
    let payload = island_msg::decode_spawn_header(data).map_err(|err| {
        SpawnFiberError::new(format!(
            "GoIsland spawn payload header decode failed: {err}"
        ))
    })?;
    let requested_capture_count = if payload.raw_capture_slots == 0 {
        payload.num_captures as usize
    } else {
        payload.raw_capture_slots as usize
    };
    if requested_capture_count > vo_runtime::objects::closure::MAX_CAPTURE_SLOTS {
        let error = vo_runtime::objects::closure::ClosureCreateError::CaptureCountTooLarge {
            capture_count: requested_capture_count,
            max_capture_slots: vo_runtime::objects::closure::MAX_CAPTURE_SLOTS,
        };
        return Err(SpawnFiberError::new(format!(
            "GoIsland spawn closure allocation failed for func_id {}: {error:?}",
            payload.func_id
        )));
    }
    let module = vm
        .module
        .as_ref()
        .cloned()
        .ok_or_else(|| SpawnFiberError::new("GoIsland spawn requires a loaded module"))?;
    let (capture_types, param_types) = {
        let func_idx = payload.func_id as usize;
        let func_def = module.functions.get(func_idx).ok_or_else(|| {
            SpawnFiberError::new(format!(
                "GoIsland spawn payload references missing func_id {}",
                payload.func_id
            ))
        })?;
        let capture_types = if payload.raw_capture_slots > 0 {
            let plan = crate::exec::direct_method_receiver_transfer_plan(
                &module,
                payload.func_id,
                func_def,
                payload.raw_capture_slots,
            )
            .map_err(|msg| {
                SpawnFiberError::new(format!(
                    "GoIsland spawn payload receiver metadata error for func_id {}: {msg}",
                    payload.func_id
                ))
            })?;
            vec![plan.transfer_type]
        } else if func_def.capture_types.is_empty()
            && func_def.recv_slots > 0
            && payload.num_captures > 0
        {
            let plan = crate::exec::direct_method_receiver_transfer_plan(
                &module,
                payload.func_id,
                func_def,
                func_def.recv_slots,
            )
            .map_err(|msg| {
                SpawnFiberError::new(format!(
                    "GoIsland spawn payload receiver metadata error for func_id {}: {msg}",
                    payload.func_id
                ))
            })?;
            if plan.raw_capture_slots != 0 {
                return Err(SpawnFiberError::new(format!(
                    "GoIsland spawn payload receiver metadata error for func_id {}: boxed receiver plan produced raw slots {}",
                    payload.func_id, plan.raw_capture_slots
                )));
            }
            vec![plan.transfer_type]
        } else {
            func_def.capture_types.clone()
        };
        let param_types = crate::exec::go_island_payload_param_transfer_types(
            &module,
            payload.func_id,
            func_def,
            payload.num_args as usize,
        )
        .map_err(|msg| {
            SpawnFiberError::new(format!(
                "GoIsland spawn payload param metadata error for func_id {}: {msg}",
                payload.func_id
            ))
        })?;
        (capture_types, param_types)
    };

    let mut endpoint_insertions = crate::exec::EndpointRegistryInsertions::default();
    let (unpacked_args, closure_ref) = {
        let (gc, endpoint_registry) = (&mut vm.state.gc, &mut vm.state.endpoint_registry);
        let mut handle_error = None;
        let unpack_result = island_msg::unpack_spawn_payload(
            gc,
            data,
            &payload,
            &capture_types,
            &param_types,
            &module.struct_metas,
            &module.named_type_metas,
            &module.runtime_types,
            |gc, handle| match crate::exec::try_resolve_unpacked_queue_handle_transactional(
                gc,
                handle,
                endpoint_registry,
                &mut endpoint_insertions,
            ) {
                Ok(queue_ref) => queue_ref,
                Err(err) => {
                    if handle_error.is_none() {
                        handle_error = Some(err);
                    }
                    core::ptr::null_mut()
                }
            },
        );
        let (unpacked_captures, unpacked_args) = match unpack_result {
            Ok(value) => value,
            Err(err) => {
                endpoint_insertions.rollback(endpoint_registry);
                vm.state.mark_gc_all_roots_dirty();
                return Err(SpawnFiberError::new(format!(
                    "GoIsland spawn payload unpack failed for func_id {}: {err}",
                    payload.func_id
                )));
            }
        };
        if let Some(err) = handle_error {
            endpoint_insertions.rollback(endpoint_registry);
            vm.state.mark_gc_all_roots_dirty();
            return Err(SpawnFiberError::new(format!(
                "GoIsland spawn payload queue handle resolution failed for func_id {}: {err}",
                payload.func_id
            )));
        }

        let capture_count = if payload.raw_capture_slots == 0 {
            payload.num_captures as usize
        } else {
            unpacked_captures.len()
        };
        let closure_ref =
            match vo_runtime::objects::closure::try_create(gc, payload.func_id, capture_count) {
                Ok(closure_ref) => closure_ref,
                Err(err) => {
                    endpoint_insertions.rollback(endpoint_registry);
                    vm.state.mark_gc_all_roots_dirty();
                    return Err(SpawnFiberError::new(format!(
                        "GoIsland spawn closure allocation failed for func_id {}: {err:?}",
                        payload.func_id
                    )));
                }
            };

        for (i, &slot) in unpacked_captures.iter().enumerate() {
            // Safety: `closure_ref` is freshly allocated; it is marked for
            // scanning after all captures are initialized.
            unsafe { vo_runtime::objects::closure::set_capture(closure_ref, i, slot) };
        }
        gc.mark_allocated_for_scan(closure_ref);
        (unpacked_args, closure_ref)
    };
    vm.mark_gc_all_roots_dirty();

    let unpacked_arg_slots = match u32::try_from(unpacked_args.len()) {
        Ok(unpacked_arg_slots) => unpacked_arg_slots,
        Err(_) => {
            endpoint_insertions.rollback(&mut vm.state.endpoint_registry);
            vm.mark_gc_all_roots_dirty();
            return Err(SpawnFiberError::new(format!(
                "GoIsland spawn argument slot count {} exceeds u32::MAX",
                unpacked_args.len()
            )));
        }
    };
    let spawn = match unsafe {
        helpers::try_build_closure_pending_spawn_from_args_ptr(
            &vm.state.gc,
            &module,
            closure_ref as u64,
            unpacked_args.as_ptr(),
            unpacked_arg_slots,
        )
    } {
        Ok(fiber) => fiber,
        Err(err) => {
            endpoint_insertions.rollback(&mut vm.state.endpoint_registry);
            vm.mark_gc_all_roots_dirty();
            return Err(SpawnFiberError::new(format!(
                "GoIsland spawn closure fiber build failed for func_id {}: {err:?}",
                payload.func_id
            )));
        }
    };
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::AllRootsDirty,
    );
    transition.spawns.push(spawn);
    if let Err(err) = vm.apply_runtime_transition(None, transition) {
        endpoint_insertions.rollback(&mut vm.state.endpoint_registry);
        vm.mark_gc_all_roots_dirty();
        return Err(SpawnFiberError::new(format!(
            "GoIsland spawn transition failed: {err:?}"
        )));
    }
    Ok(())
}

enum EndpointQueueResponse {
    SendAck { closed: bool },
    RecvData { data: Vec<u8>, closed: bool },
    RecvError,
}

impl EndpointQueueResponse {
    fn with_wait_key(self, wait_key: EndpointWaitKey) -> EndpointResponseKind {
        match self {
            Self::SendAck { closed } => EndpointResponseKind::SendAck { closed, wait_key },
            Self::RecvData { data, closed } => EndpointResponseKind::RecvData {
                data,
                closed,
                wait_key,
            },
            Self::RecvError => EndpointResponseKind::RecvError { wait_key },
        }
    }
}

fn endpoint_send_ack(closed: bool) -> EndpointQueueResponse {
    EndpointQueueResponse::SendAck { closed }
}

fn endpoint_recv_closed() -> EndpointQueueResponse {
    EndpointQueueResponse::RecvData {
        data: Vec::new(),
        closed: true,
    }
}

fn endpoint_recv_error() -> EndpointQueueResponse {
    EndpointQueueResponse::RecvError
}

fn endpoint_recv_data(data: Vec<u8>) -> EndpointQueueResponse {
    EndpointQueueResponse::RecvData {
        data,
        closed: false,
    }
}

fn endpoint_request_authorized(vm: &Vm, ch: GcRef, from_island: u32) -> Result<bool, VmError> {
    if unsafe { vo_runtime::objects::queue_state::backing(ch) } != QueueBacking::Local {
        return Err(VmError::Jit(
            "EndpointRequest resolved to a non-local queue".to_string(),
        ));
    }
    Ok(unsafe { queue::home_info(ch) }.is_some_and(|info| {
        from_island == vm.state.current_island_id || info.peers.contains(&from_island)
    }))
}

fn preflight_endpoint_response_route(
    vm: &Vm,
    target: &QueueWaiter,
    context: &str,
) -> Result<(), VmError> {
    match *target.target() {
        QueueWaitTarget::Endpoint { .. } => {
            crate::exec::preflight_island_route(&vm.state, target.island_id(), context)
                .map_err(VmError::Jit)
        }
        QueueWaitTarget::Queue { .. } | QueueWaitTarget::Select { .. }
            if target.island_id() == vm.state.current_island_id =>
        {
            Ok(())
        }
        QueueWaitTarget::Queue { .. } | QueueWaitTarget::Select { .. } => Err(VmError::Jit(
            format!("{context}: remote queue waiter missing endpoint wait identity"),
        )),
    }
}

fn preflight_endpoint_peer_route(vm: &Vm, peer: u32, context: &str) -> Result<(), VmError> {
    crate::exec::preflight_island_route(&vm.state, peer, context).map_err(VmError::Jit)
}

fn preflight_endpoint_request_routes(
    vm: &Vm,
    ch: GcRef,
    kind: &EndpointRequestKind,
    from: Option<&QueueWaiter>,
    from_island: u32,
) -> Result<(), VmError> {
    match (kind, from) {
        (EndpointRequestKind::Send { .. }, Some(from)) => {
            preflight_endpoint_response_route(vm, from, "EndpointRequest send requester route")?;
            if let Some(receiver) =
                unsafe { queue::next_remote_direct_receiver(ch, vm.state.current_island_id) }
            {
                preflight_endpoint_response_route(
                    vm,
                    &receiver,
                    "EndpointRequest send receiver route",
                )?;
            }
        }
        (EndpointRequestKind::Recv { .. }, Some(from)) => {
            preflight_endpoint_response_route(vm, from, "EndpointRequest recv requester route")?;
            if let Some(sender) = unsafe { queue::next_recv_endpoint_sender(ch) } {
                preflight_endpoint_response_route(
                    vm,
                    &sender,
                    "EndpointRequest recv sender route",
                )?;
            }
        }
        (EndpointRequestKind::Close, _) => {
            if let Some(info) = unsafe { queue::home_info(ch) } {
                for peer in info.peers.iter().copied() {
                    if peer != from_island {
                        preflight_endpoint_peer_route(
                            vm,
                            peer,
                            "EndpointRequest close peer route",
                        )?;
                    }
                }
            }
            let state = unsafe { queue::local_state(ch) };
            for receiver in &state.waiting_receivers {
                preflight_endpoint_response_route(
                    vm,
                    receiver,
                    "EndpointRequest close receiver route",
                )?;
            }
            for (sender, _) in state.waiting_senders.iter() {
                preflight_endpoint_response_route(
                    vm,
                    sender,
                    "EndpointRequest close sender route",
                )?;
            }
        }
        (EndpointRequestKind::Transfer { .. }, _) => {}
        (EndpointRequestKind::Send { .. } | EndpointRequestKind::Recv { .. }, None) => {
            return Err(VmError::Jit(
                "endpoint request missing response waiter".to_string(),
            ));
        }
    }
    Ok(())
}

fn reject_endpoint_request_kind(kind: &EndpointRequestKind) -> Option<EndpointQueueResponse> {
    match kind {
        EndpointRequestKind::Send { .. } => Some(endpoint_send_ack(true)),
        EndpointRequestKind::Recv { .. } => Some(endpoint_recv_error()),
        EndpointRequestKind::Close | EndpointRequestKind::Transfer { .. } => None,
    }
}

fn reject_endpoint_request(
    vm: &mut Vm,
    endpoint_id: u64,
    kind: &EndpointRequestKind,
    from_island: u32,
) -> Result<(), VmError> {
    let (Some(response), Some(wait_key)) = (reject_endpoint_request_kind(kind), kind.wait_key())
    else {
        return Ok(());
    };
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            from_island,
            endpoint_id,
            response.with_wait_key(wait_key),
        ));
    vm.apply_runtime_transition(None, transition)?;
    Ok(())
}

pub(crate) fn preflight_endpoint_request_command(
    vm: &Vm,
    endpoint_id: u64,
    kind: &EndpointRequestKind,
    from_island: u32,
) -> Result<(), VmError> {
    if matches!(kind, EndpointRequestKind::Transfer { .. }) {
        return Ok(());
    }
    let from = endpoint_request_waiter(kind, from_island)?;

    match vm.state.endpoint_registry.entry(endpoint_id) {
        Some(EndpointEntry::Live(ch)) => {
            if !endpoint_request_authorized(vm, ch, from_island)? {
                if let Some(from) = from.as_ref() {
                    preflight_endpoint_response_route(
                        vm,
                        from,
                        "EndpointRequest reject requester route",
                    )?;
                }
                return Ok(());
            }
            preflight_endpoint_request_routes(vm, ch, kind, from.as_ref(), from_island)
        }
        Some(EndpointEntry::Tombstone { .. }) | None => from.as_ref().map_or(Ok(()), |from| {
            preflight_endpoint_response_route(
                vm,
                from,
                "EndpointRequest missing endpoint requester route",
            )
        }),
    }
}

pub(crate) fn handle_endpoint_request_command(
    vm: &mut Vm,
    endpoint_id: u64,
    kind: EndpointRequestKind,
    from_island: u32,
) -> Result<(), VmError> {
    let from = endpoint_request_waiter(&kind, from_island)?;
    let is_close = matches!(&kind, EndpointRequestKind::Close);

    if let EndpointRequestKind::Transfer { new_peer } = &kind {
        if let Some(ch) = vm.state.endpoint_registry.get_live(endpoint_id) {
            if endpoint_request_authorized(vm, ch, from_island)? {
                unsafe { queue::add_home_peer(ch, *new_peer) }.map_err(|_| {
                    VmError::Jit("EndpointRequest transfer target lost its HomeInfo".to_string())
                })?;
            }
        }
        return Ok(());
    }

    match vm.state.endpoint_registry.entry(endpoint_id) {
        Some(EndpointEntry::Live(ch)) => {
            if !endpoint_request_authorized(vm, ch, from_island)? {
                reject_endpoint_request(vm, endpoint_id, &kind, from_island)?;
                return Ok(());
            }
            let cap = unsafe { vo_runtime::objects::queue_state::capacity(ch) };
            let elem_meta = unsafe { vo_runtime::objects::queue_state::elem_meta(ch) };
            let elem_rttid = unsafe { vo_runtime::objects::queue_state::elem_rttid(ch) };
            let elem_slots = unsafe { vo_runtime::objects::queue_state::elem_slots(ch) } as usize;
            let home_island = vm.state.current_island_id;
            let module =
                vm.module.as_ref().cloned().ok_or_else(|| {
                    VmError::Jit("EndpointRequest requires a loaded module".into())
                })?;

            let mut responses: Vec<(u32, EndpointResponseKind)> = Vec::new();
            let mut local_wakes: Vec<WakeCommand> = Vec::new();
            let mut island_effects = Vec::new();

            preflight_endpoint_request_routes(vm, ch, &kind, from.as_ref(), from_island)?;
            let mut queue_rollback = if is_close {
                EndpointQueueRollback::CloseFlag(unsafe { queue::is_closed(ch) })
            } else {
                EndpointQueueRollback::None
            };
            let mut endpoint_insertions = crate::exec::EndpointRegistryInsertions::default();
            let mut transfer_commit = crate::exec::QueueTransferCommit::default();

            let ctx = EndpointRequestCtx {
                ch,
                cap,
                home_island,
                elem_meta,
                elem_rttid,
                elem_slots,
                struct_metas: &module.struct_metas,
                runtime_types: &module.runtime_types,
                module: module.runtime_metadata(),
            };
            let inner_result = unsafe {
                queue::with_local_state(ch, |state| {
                    handle_endpoint_request_inner(
                        &ctx,
                        state,
                        kind,
                        from,
                        &mut vm.state,
                        &mut responses,
                        &mut local_wakes,
                        &mut queue_rollback,
                        &mut endpoint_insertions,
                        &mut transfer_commit,
                        &mut island_effects,
                    )
                })
            };
            if let Err(error) = inner_result {
                let rollback_error = rollback_endpoint_request(
                    vm,
                    ch,
                    &mut queue_rollback,
                    &mut endpoint_insertions,
                    &mut transfer_commit,
                );
                vm.mark_gc_all_roots_dirty();
                let rollback_error = rollback_error
                    .err()
                    .map(|rollback| format!("; rollback failed: {rollback}"))
                    .unwrap_or_default();
                return Err(VmError::Jit(format!(
                    "EndpointRequest {endpoint_id} failed: {error}{rollback_error}"
                )));
            }

            if is_close
                || !island_effects.is_empty()
                || !responses.is_empty()
                || !local_wakes.is_empty()
            {
                let mut transition = RuntimeTransition::new(
                    RuntimeBoundary::Continue,
                    ResumePolicy::PreserveFramePc,
                    GcRootEffect::AllRootsDirty,
                );
                transition.island_commands.append(&mut island_effects);
                if is_close {
                    transition.prepare_queue_close(ch);
                    append_closed_home_endpoint_effects(
                        &vm.state,
                        endpoint_id,
                        Some(from_island),
                        &mut transition,
                    );
                }
                transition.island_commands.extend(responses.into_iter().map(
                    |(target_island, resp_kind)| {
                        IslandCommandEffect::endpoint_response(
                            target_island,
                            endpoint_id,
                            resp_kind,
                        )
                    },
                ));
                for wake in local_wakes.drain(..) {
                    if wake.is_queue_close_wake() {
                        transition.push_queue_close_wake(wake);
                    } else {
                        transition.wakes.push(wake);
                    }
                }
                if let Err(err) = vm.apply_runtime_transition(None, transition) {
                    let rollback_error = rollback_endpoint_request(
                        vm,
                        ch,
                        &mut queue_rollback,
                        &mut endpoint_insertions,
                        &mut transfer_commit,
                    );
                    vm.mark_gc_all_roots_dirty();
                    return match rollback_error {
                        Ok(()) => Err(err),
                        Err(rollback) => Err(VmError::Jit(format!(
                            "{err:?}; endpoint queue rollback failed: {rollback}"
                        ))),
                    };
                }
            }
        }
        Some(EndpointEntry::Tombstone { .. }) | None => {
            let (resp, wait_key) = match kind {
                EndpointRequestKind::Send { wait_key, .. } => (endpoint_send_ack(true), wait_key),
                EndpointRequestKind::Recv { wait_key } => (endpoint_recv_closed(), wait_key),
                EndpointRequestKind::Close | EndpointRequestKind::Transfer { .. } => return Ok(()),
            };
            let mut transition = RuntimeTransition::new(
                RuntimeBoundary::Continue,
                ResumePolicy::PreserveFramePc,
                GcRootEffect::None,
            );
            transition
                .island_commands
                .push(IslandCommandEffect::endpoint_response(
                    from_island,
                    endpoint_id,
                    resp.with_wait_key(wait_key),
                ));
            vm.apply_runtime_transition(None, transition)?;
        }
    }
    Ok(())
}

fn endpoint_request_waiter(
    kind: &EndpointRequestKind,
    from_island: u32,
) -> Result<Option<QueueWaiter>, VmError> {
    let Some(wait_key) = kind.wait_key() else {
        return Ok(None);
    };
    validate_canonical_fiber_key(wait_key.fiber_key(), "endpoint request response identity")
        .map_err(VmError::Jit)?;
    Ok(Some(QueueWaiter::endpoint(from_island, wait_key)))
}

struct EndpointRequestCtx<'a> {
    ch: GcRef,
    cap: usize,
    home_island: u32,
    elem_meta: vo_runtime::ValueMeta,
    elem_rttid: vo_runtime::ValueRttid,
    elem_slots: usize,
    struct_metas: &'a [vo_common_core::bytecode::StructMeta],
    runtime_types: &'a [vo_common_core::RuntimeType],
    module: ModuleRuntimeMetadata<'a>,
}

#[derive(Debug)]
enum EndpointQueueRollback {
    None,
    SendBuffered,
    SendBlocked(QueueWaiter),
    SendDirect {
        receiver: QueueWaiter,
        replayed_to_buffer: bool,
    },
    RecvBlocked(QueueWaiter),
    RecvSuccess {
        from_buffer: bool,
        payload: QueueMessage,
        woke_sender: Option<QueueWaiter>,
    },
    CloseFlag(bool),
}

impl EndpointQueueRollback {
    fn rollback(&mut self, state: &mut LocalQueueState) -> Result<(), String> {
        match core::mem::replace(self, Self::None) {
            Self::None => Ok(()),
            Self::SendBuffered => state
                .buffer
                .pop_back()
                .map(|_| ())
                .ok_or_else(|| "buffered endpoint send rollback found an empty buffer".into()),
            Self::SendBlocked(waiter) => {
                let Some((registered, payload)) = state.waiting_senders.pop_back() else {
                    return Err("blocked endpoint send rollback found no sender".into());
                };
                if registered == waiter {
                    Ok(())
                } else {
                    state.waiting_senders.push_back((registered, payload));
                    Err("blocked endpoint send rollback found a different sender".into())
                }
            }
            Self::SendDirect {
                receiver,
                replayed_to_buffer,
            } => {
                let replay_error = replayed_to_buffer && state.buffer.pop_back().is_none();
                state.waiting_receivers.push_front(receiver);
                if replay_error {
                    Err("direct endpoint send rollback found no replayed payload".into())
                } else {
                    Ok(())
                }
            }
            Self::RecvBlocked(waiter) => {
                let Some(registered) = state.waiting_receivers.pop_back() else {
                    return Err("blocked endpoint recv rollback found no receiver".into());
                };
                if registered == waiter {
                    Ok(())
                } else {
                    state.waiting_receivers.push_back(registered);
                    Err("blocked endpoint recv rollback found a different receiver".into())
                }
            }
            Self::RecvSuccess {
                from_buffer,
                payload,
                woke_sender,
            } => {
                if from_buffer {
                    if let Some(sender) = woke_sender {
                        let Some(sender_payload) = state.buffer.pop_back() else {
                            state.buffer.push_front(payload);
                            return Err(
                                "endpoint recv rollback found no promoted sender payload".into()
                            );
                        };
                        state.waiting_senders.push_front((sender, sender_payload));
                    }
                    state.buffer.push_front(payload);
                    Ok(())
                } else if let Some(sender) = woke_sender {
                    state.waiting_senders.push_front((sender, payload));
                    Ok(())
                } else {
                    state.buffer.push_front(payload);
                    Err("endpoint recv rollback lost its source sender".into())
                }
            }
            Self::CloseFlag(closed) => {
                state.closed = closed;
                Ok(())
            }
        }
    }

    fn recv_payload(&self) -> Option<&QueueMessage> {
        match self {
            Self::RecvSuccess { payload, .. } => Some(payload),
            _ => None,
        }
    }
}

fn rollback_endpoint_request(
    vm: &mut Vm,
    ch: GcRef,
    queue_rollback: &mut EndpointQueueRollback,
    endpoint_insertions: &mut crate::exec::EndpointRegistryInsertions,
    transfer_commit: &mut crate::exec::QueueTransferCommit,
) -> Result<(), String> {
    core::mem::take(transfer_commit).restore_committed_local_endpoint_state(&mut vm.state);
    endpoint_insertions.rollback(&mut vm.state.endpoint_registry);
    unsafe { queue::with_local_state(ch, |state| queue_rollback.rollback(state)) }
}

fn handle_endpoint_request_inner(
    ctx: &EndpointRequestCtx<'_>,
    state: &mut vo_runtime::objects::queue_state::LocalQueueState,
    req: EndpointRequestKind,
    from: Option<QueueWaiter>,
    vm_state: &mut crate::vm::VmState,
    responses: &mut Vec<(u32, EndpointResponseKind)>,
    local_wakes: &mut Vec<WakeCommand>,
    queue_rollback: &mut EndpointQueueRollback,
    endpoint_insertions: &mut crate::exec::EndpointRegistryInsertions,
    transfer_commit: &mut crate::exec::QueueTransferCommit,
    island_effects: &mut Vec<IslandCommandEffect>,
) -> Result<(), String> {
    let home_island = ctx.home_island;
    match (req, from) {
        (EndpointRequestKind::Send { data, .. }, Some(from)) => {
            let requester = from.clone();
            if state.is_closed() {
                dispatch_response(
                    requester,
                    home_island,
                    endpoint_send_ack(true),
                    responses,
                    local_wakes,
                )?;
                return Ok(());
            }
            let value = match crate::exec::unpack_transport_message_transactional(
                &mut vm_state.gc,
                &data,
                ctx.elem_meta,
                ctx.elem_rttid,
                ctx.elem_slots,
                ctx.struct_metas,
                &ctx.module.named_type_metas,
                ctx.runtime_types,
                &mut vm_state.endpoint_registry,
            ) {
                Ok((value, insertions)) => {
                    endpoint_insertions.absorb(insertions);
                    value
                }
                Err(_) => {
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_send_ack(true),
                        responses,
                        local_wakes,
                    )?;
                    return Ok(());
                }
            };
            vm_state.mark_gc_all_roots_dirty();
            let value = match crate::exec::queue::prepare_local_queue_payload(
                vm_state,
                ctx.ch,
                value.as_ref(),
                Some(ctx.module),
                "EndpointSend",
            ) {
                Ok((value, _)) => value,
                Err(_) => {
                    endpoint_insertions.rollback(&mut vm_state.endpoint_registry);
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_send_ack(true),
                        responses,
                        local_wakes,
                    )?;
                    return Ok(());
                }
            };
            let direct_receiver = state.waiting_receivers.front().cloned();
            if let Some(receiver) = direct_receiver.as_ref() {
                if validate_endpoint_recv_payload_for_waiter(ctx, receiver, &value, vm_state)
                    .is_err()
                {
                    endpoint_insertions.rollback(&mut vm_state.endpoint_registry);
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_send_ack(true),
                        responses,
                        local_wakes,
                    )?;
                    return Ok(());
                }
            }
            let select_recv_slot_types = if !state.is_closed()
                && state
                    .waiting_receivers
                    .front()
                    .is_some_and(|receiver| receiver.is_local_select_recv(home_island))
            {
                match crate::exec::queue::select_woken_recv_slot_types(ctx.ch, Some(ctx.module))
                    .and_then(|slot_types| {
                        crate::exec::queue::validate_select_woken_recv_payload_width(
                            value.len(),
                            slot_types.len(),
                        )?;
                        Ok(slot_types)
                    }) {
                    Ok(slot_types) => Some(slot_types),
                    Err(_) => {
                        endpoint_insertions.rollback(&mut vm_state.endpoint_registry);
                        dispatch_response(
                            requester,
                            home_island,
                            endpoint_send_ack(true),
                            responses,
                            local_wakes,
                        )?;
                        return Ok(());
                    }
                }
            } else {
                None
            };
            let mut select_recv_slot_types = select_recv_slot_types;
            match state.send_or_block_resolved(value, ctx.cap, from, home_island) {
                ResolvedSendResult::Wake { receiver, payload } => {
                    *queue_rollback = EndpointQueueRollback::SendDirect {
                        receiver: receiver.clone(),
                        replayed_to_buffer: payload.is_none(),
                    };
                    let wake = match payload {
                        Some(payload) => {
                            let slot_types = select_recv_slot_types.take().ok_or_else(|| {
                                "endpoint send produced select payload without slot preflight"
                                    .to_string()
                            })?;
                            let payload =
                                crate::exec::queue::select_woken_recv_payload_with_slot_types(
                                    payload, slot_types,
                                )?;
                            WakeCommand::queue_waiter_with_result(receiver, payload)
                        }
                        None => WakeCommand::queue_waiter(receiver),
                    };
                    local_wakes.push(wake);
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_send_ack(false),
                        responses,
                        local_wakes,
                    )?;
                }
                ResolvedSendResult::RemoteDirect {
                    receiver,
                    payload: value,
                } => {
                    *queue_rollback = EndpointQueueRollback::SendDirect {
                        receiver: receiver.clone(),
                        replayed_to_buffer: false,
                    };
                    let (recv_kind, commit) = pack_recv_data_for_waiter(
                        ctx,
                        &receiver,
                        &value,
                        vm_state,
                        island_effects,
                    )?;
                    transfer_commit.absorb(commit);
                    dispatch_response(receiver, home_island, recv_kind, responses, local_wakes)?;
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_send_ack(false),
                        responses,
                        local_wakes,
                    )?;
                }
                ResolvedSendResult::Buffered => {
                    *queue_rollback = EndpointQueueRollback::SendBuffered;
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_send_ack(false),
                        responses,
                        local_wakes,
                    )?;
                }
                ResolvedSendResult::Blocked(waiter) => {
                    *queue_rollback = EndpointQueueRollback::SendBlocked(waiter);
                }
                ResolvedSendResult::Closed => {
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_send_ack(true),
                        responses,
                        local_wakes,
                    )?;
                }
            }
        }
        (EndpointRequestKind::Recv { .. }, Some(from)) => {
            let requester = from.clone();
            if preflight_endpoint_recv_value_for_waiter(ctx, state, &requester, vm_state).is_err() {
                dispatch_response(
                    requester,
                    home_island,
                    endpoint_recv_error(),
                    responses,
                    local_wakes,
                )?;
                return Ok(());
            }
            let from_buffer = !state.buffer.is_empty();
            match state.recv_or_block(from) {
                BlockingRecvResult::Success {
                    woke_sender,
                    payload: value,
                } => {
                    *queue_rollback = EndpointQueueRollback::RecvSuccess {
                        from_buffer,
                        payload: value,
                        woke_sender: woke_sender.clone(),
                    };
                    let value = queue_rollback.recv_payload().ok_or_else(|| {
                        "endpoint recv rollback lost its received payload".to_string()
                    })?;
                    let (recv_kind, commit) = pack_recv_data_for_waiter(
                        ctx,
                        &requester,
                        value,
                        vm_state,
                        island_effects,
                    )?;
                    transfer_commit.absorb(commit);
                    dispatch_response(requester, home_island, recv_kind, responses, local_wakes)?;
                    if let Some(sender) = woke_sender {
                        dispatch_response(
                            sender,
                            home_island,
                            endpoint_send_ack(false),
                            responses,
                            local_wakes,
                        )?;
                    }
                }
                BlockingRecvResult::Blocked(waiter) => {
                    *queue_rollback = EndpointQueueRollback::RecvBlocked(waiter);
                }
                BlockingRecvResult::Closed => {
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_recv_closed(),
                        responses,
                        local_wakes,
                    )?;
                }
            }
        }
        (EndpointRequestKind::Close, _) => {
            state.close();
            for receiver in state.waiting_receivers.iter().cloned() {
                dispatch_response(
                    receiver,
                    home_island,
                    endpoint_recv_closed(),
                    responses,
                    local_wakes,
                )?;
            }
            for sender in state
                .waiting_senders
                .iter()
                .map(|(waiter, _)| waiter.clone())
            {
                dispatch_response(
                    sender,
                    home_island,
                    endpoint_send_ack(true),
                    responses,
                    local_wakes,
                )?;
            }
        }
        (EndpointRequestKind::Transfer { .. }, _) => {
            return Err("Transfer reached endpoint request queue mutation path".to_string());
        }
        (EndpointRequestKind::Send { .. } | EndpointRequestKind::Recv { .. }, None) => {
            return Err("endpoint request missing response waiter".to_string());
        }
    }
    Ok(())
}

fn preflight_endpoint_recv_value_for_waiter(
    ctx: &EndpointRequestCtx<'_>,
    state: &vo_runtime::objects::queue_state::LocalQueueState,
    target: &QueueWaiter,
    vm_state: &mut crate::vm::VmState,
) -> Result<(), String> {
    let value = state
        .buffer
        .front()
        .map(|value| value.as_ref())
        .or_else(|| {
            state
                .waiting_senders
                .front()
                .map(|(_, value)| value.as_ref())
        });
    let Some(value) = value else {
        return Ok(());
    };
    validate_endpoint_recv_payload_for_waiter(ctx, target, value, vm_state)
}

fn validate_endpoint_recv_payload_for_waiter(
    ctx: &EndpointRequestCtx<'_>,
    target: &QueueWaiter,
    value: &[u64],
    vm_state: &mut crate::vm::VmState,
) -> Result<(), String> {
    if !endpoint_recv_response_serializes(ctx, target)
        || !ctx.elem_meta.value_kind().may_contain_gc_refs()
    {
        return Ok(());
    }
    crate::exec::validate_value_queue_handles_for_transfer(
        value,
        ctx.elem_meta,
        target.island_id(),
        ctx.struct_metas,
        &ctx.module.named_type_metas,
        ctx.runtime_types,
        vm_state,
    )
}

fn endpoint_recv_response_serializes(ctx: &EndpointRequestCtx<'_>, target: &QueueWaiter) -> bool {
    target.endpoint_wait_key().is_some() || target.island_id() != ctx.home_island
}

fn prepare_endpoint_recv_payload_for_waiter(
    ctx: &EndpointRequestCtx<'_>,
    target: &QueueWaiter,
    value: &[u64],
    vm_state: &mut crate::vm::VmState,
    island_effects: &mut Vec<IslandCommandEffect>,
) -> Result<crate::exec::QueueTransferCommit, String> {
    if !endpoint_recv_response_serializes(ctx, target)
        || !ctx.elem_meta.value_kind().may_contain_gc_refs()
    {
        return Ok(crate::exec::QueueTransferCommit::default());
    }
    crate::exec::prepare_value_queue_handles_for_transfer_with_commit(
        value,
        ctx.elem_meta,
        target.island_id(),
        ctx.struct_metas,
        &ctx.module.named_type_metas,
        ctx.runtime_types,
        vm_state,
        island_effects,
    )
}

fn pack_recv_data_for_waiter(
    ctx: &EndpointRequestCtx<'_>,
    target: &QueueWaiter,
    value: &[u64],
    vm_state: &mut crate::vm::VmState,
    island_effects: &mut Vec<IslandCommandEffect>,
) -> Result<(EndpointQueueResponse, crate::exec::QueueTransferCommit), String> {
    let commit =
        prepare_endpoint_recv_payload_for_waiter(ctx, target, value, vm_state, island_effects)?;
    // Safety: endpoint preflight validated the element layout and the queued
    // value remains rooted until packing completes.
    let data = match unsafe {
        crate::exec::pack_transport_message(
            &vm_state.gc,
            value,
            ctx.elem_meta,
            ctx.struct_metas,
            &ctx.module.named_type_metas,
            ctx.runtime_types,
        )
    } {
        Ok(data) => data,
        Err(error) => {
            commit.restore_committed_local_endpoint_state(vm_state);
            return Err(format!("failed to pack endpoint receive payload: {error}"));
        }
    };
    Ok((endpoint_recv_data(data), commit))
}

fn dispatch_response(
    target: QueueWaiter,
    home_island: u32,
    response: EndpointQueueResponse,
    responses: &mut Vec<(u32, EndpointResponseKind)>,
    local_wakes: &mut Vec<WakeCommand>,
) -> Result<(), String> {
    match *target.target() {
        QueueWaitTarget::Queue { .. } | QueueWaitTarget::Select { .. } => {
            if target.island_id() != home_island {
                return Err("remote queue waiter missing endpoint wait identity".to_string());
            }
            match response {
                EndpointQueueResponse::RecvData { closed: true, .. }
                | EndpointQueueResponse::RecvError => {
                    local_wakes.push(WakeCommand::queue_closed_receiver(target, None));
                }
                EndpointQueueResponse::SendAck { closed: true } => {
                    local_wakes.push(WakeCommand::queue_closed_sender(target, None));
                }
                EndpointQueueResponse::SendAck { closed: false }
                | EndpointQueueResponse::RecvData { closed: false, .. } => {
                    local_wakes.push(WakeCommand::queue_waiter(target));
                }
            }
        }
        QueueWaitTarget::Endpoint { .. } => {
            responses.push((
                target.island_id(),
                response.with_wait_key(
                    target
                        .endpoint_wait_key()
                        .expect("endpoint waiter must carry endpoint identity"),
                ),
            ));
        }
    }
    Ok(())
}

pub(crate) fn append_closed_home_endpoint_effects(
    state: &crate::vm::VmState,
    endpoint_id: u64,
    exclude_peer: Option<u32>,
    transition: &mut RuntimeTransition,
) {
    let peers = state
        .endpoint_registry
        .get_live(endpoint_id)
        .and_then(|ch| unsafe { queue::home_info(ch) })
        .map(|info| info.peers.iter().copied().collect::<Vec<_>>())
        .unwrap_or_default();

    for peer in peers {
        if Some(peer) == exclude_peer {
            continue;
        }
        transition
            .island_commands
            .push(IslandCommandEffect::endpoint_response(
                peer,
                endpoint_id,
                EndpointResponseKind::Closed,
            ));
    }
    transition
        .endpoint_tombstones
        .push(EndpointTombstone::with_response_source(
            endpoint_id,
            state.current_island_id,
        ));
}

fn mark_remote_endpoint_closed(vm: &mut Vm, endpoint_id: u64) {
    if let Some(ch) = vm.state.endpoint_registry.get_live(endpoint_id) {
        if unsafe { queue::is_remote(ch) } {
            unsafe { queue::mark_remote_closed(ch) };
        }
    }
}

pub(crate) fn endpoint_response_from_authorized_source(
    vm: &Vm,
    endpoint_id: u64,
    from_island: u32,
) -> bool {
    match vm.state.endpoint_registry.entry(endpoint_id) {
        Some(EndpointEntry::Live(ch)) if unsafe { queue::is_remote(ch) } => {
            unsafe { queue::remote_proxy(ch) }.home_island == from_island
        }
        Some(EndpointEntry::Live(_)) => from_island == vm.state.current_island_id,
        None => false,
        Some(EndpointEntry::Tombstone {
            response_source: Some(source),
        }) => source == from_island,
        Some(EndpointEntry::Tombstone {
            response_source: None,
        }) => from_island == vm.state.current_island_id,
    }
}

fn resume_endpoint_response(
    vm: &mut Vm,
    endpoint_id: u64,
    from_island: u32,
    kind: EndpointResponseKind,
) -> RuntimeCommandOutcome {
    vm.apply_runtime_command(RuntimeCommand::endpoint_response(
        endpoint_id,
        from_island,
        kind,
    ))
}

pub(crate) fn handle_endpoint_response_command(
    vm: &mut Vm,
    endpoint_id: u64,
    kind: EndpointResponseKind,
    from_island: u32,
) -> Result<(), VmError> {
    if !endpoint_response_from_authorized_source(vm, endpoint_id, from_island) {
        return Ok(());
    }
    let (outcome, closes_endpoint, context) = match kind {
        EndpointResponseKind::Closed => (
            vm.apply_runtime_command(RuntimeCommand::endpoint_closed_response(
                endpoint_id,
                from_island,
            )),
            true,
            "closed",
        ),
        kind @ EndpointResponseKind::SendAck { closed, .. } => (
            resume_endpoint_response(vm, endpoint_id, from_island, kind),
            closed,
            "send",
        ),
        kind @ EndpointResponseKind::RecvData { closed, .. } => (
            resume_endpoint_response(vm, endpoint_id, from_island, kind),
            closed,
            "recv",
        ),
        kind @ EndpointResponseKind::RecvError { .. } => (
            resume_endpoint_response(vm, endpoint_id, from_island, kind),
            false,
            "recv error",
        ),
    };
    if closes_endpoint && outcome.payload_accepted {
        mark_remote_endpoint_closed(vm, endpoint_id);
    }
    if !outcome.applied || !outcome.payload_accepted {
        return Err(VmError::Jit(format!(
            "endpoint {context} response command was rejected by the VM"
        )));
    }
    Ok(())
}

#[cfg(test)]
mod tests;
