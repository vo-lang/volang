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
use vo_runtime::island::{EndpointRequestKind, EndpointResponseKind};
use vo_runtime::island_msg;
use vo_runtime::objects::queue;
use vo_runtime::objects::queue_state::{QueueBacking, QueueWaiter};

use crate::bytecode::Module;
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

    let endpoint_registry_snapshot = vm.state.endpoint_registry.snapshot();
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
            |gc, handle| match crate::exec::try_resolve_unpacked_queue_handle(
                gc,
                handle,
                endpoint_registry,
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
                endpoint_registry.restore(endpoint_registry_snapshot);
                vm.state.mark_gc_all_roots_dirty();
                return Err(SpawnFiberError::new(format!(
                    "GoIsland spawn payload unpack failed for func_id {}: {err}",
                    payload.func_id
                )));
            }
        };
        if let Some(err) = handle_error {
            endpoint_registry.restore(endpoint_registry_snapshot);
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
                    endpoint_registry.restore(endpoint_registry_snapshot);
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
            vm.state
                .endpoint_registry
                .restore(endpoint_registry_snapshot);
            vm.mark_gc_all_roots_dirty();
            return Err(SpawnFiberError::new(format!(
                "GoIsland spawn argument slot count {} exceeds u32::MAX",
                unpacked_args.len()
            )));
        }
    };
    let next_fiber_id = match vm.scheduler.next_spawn_identity_hint() {
        Ok(next_fiber_id) => next_fiber_id,
        Err(err) => {
            vm.state
                .endpoint_registry
                .restore(endpoint_registry_snapshot);
            vm.mark_gc_all_roots_dirty();
            return Err(SpawnFiberError::new(format!(
                "GoIsland spawn identity allocation failed: {err}"
            )));
        }
    };
    let new_fiber = match unsafe {
        helpers::try_build_closure_fiber_from_args_ptr(
            &vm.state.gc,
            &module,
            next_fiber_id,
            closure_ref as u64,
            unpacked_args.as_ptr(),
            unpacked_arg_slots,
        )
    } {
        Ok(fiber) => fiber,
        Err(err) => {
            vm.state
                .endpoint_registry
                .restore(endpoint_registry_snapshot);
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
    transition.spawns.push(new_fiber);
    if let Err(err) = vm.apply_runtime_transition(None, transition) {
        vm.state
            .endpoint_registry
            .restore(endpoint_registry_snapshot);
        vm.mark_gc_all_roots_dirty();
        return Err(SpawnFiberError::new(format!(
            "GoIsland spawn transition failed: {err:?}"
        )));
    }
    Ok(())
}

fn endpoint_send_ack(closed: bool) -> EndpointResponseKind {
    EndpointResponseKind::SendAck { closed }
}

fn endpoint_recv_closed() -> EndpointResponseKind {
    EndpointResponseKind::RecvData {
        data: Vec::new(),
        closed: true,
    }
}

fn endpoint_recv_error() -> EndpointResponseKind {
    EndpointResponseKind::RecvError
}

fn endpoint_recv_data(data: Vec<u8>) -> EndpointResponseKind {
    EndpointResponseKind::RecvData {
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
    target: QueueWaiter,
    context: &str,
) -> Result<(), VmError> {
    if target.endpoint_wait_id() == 0 && target.island_id == vm.state.current_island_id {
        return Ok(());
    }
    crate::exec::preflight_island_route(&vm.state, target.island_id, context).map_err(VmError::Jit)
}

fn preflight_endpoint_peer_route(vm: &Vm, peer: u32, context: &str) -> Result<(), VmError> {
    crate::exec::preflight_island_route(&vm.state, peer, context).map_err(VmError::Jit)
}

fn preflight_endpoint_request_routes(
    vm: &Vm,
    ch: GcRef,
    kind: &EndpointRequestKind,
    from: QueueWaiter,
) -> Result<(), VmError> {
    match kind {
        EndpointRequestKind::Send { .. } => {
            preflight_endpoint_response_route(vm, from, "EndpointRequest send requester route")?;
            if let Some(receiver) =
                unsafe { queue::next_remote_direct_receiver(ch, vm.state.current_island_id) }
            {
                preflight_endpoint_response_route(
                    vm,
                    receiver,
                    "EndpointRequest send receiver route",
                )?;
            }
        }
        EndpointRequestKind::Recv => {
            preflight_endpoint_response_route(vm, from, "EndpointRequest recv requester route")?;
            if let Some(sender) = unsafe { queue::next_recv_endpoint_sender(ch) } {
                preflight_endpoint_response_route(vm, sender, "EndpointRequest recv sender route")?;
            }
        }
        EndpointRequestKind::Close => {
            if let Some(info) = unsafe { queue::home_info(ch) } {
                for peer in info.peers.iter().copied() {
                    if peer != from.island_id {
                        preflight_endpoint_peer_route(
                            vm,
                            peer,
                            "EndpointRequest close peer route",
                        )?;
                    }
                }
            }
            let state = unsafe { queue::local_state(ch) };
            for receiver in state.waiting_receivers.iter().cloned() {
                preflight_endpoint_response_route(
                    vm,
                    receiver,
                    "EndpointRequest close receiver route",
                )?;
            }
            for (sender, _) in state.waiting_senders.iter() {
                preflight_endpoint_response_route(
                    vm,
                    sender.clone(),
                    "EndpointRequest close sender route",
                )?;
            }
        }
        EndpointRequestKind::Transfer { .. } => {}
    }
    Ok(())
}

fn reject_endpoint_request_kind(kind: &EndpointRequestKind) -> Option<EndpointResponseKind> {
    match kind {
        EndpointRequestKind::Send { .. } => Some(endpoint_send_ack(true)),
        EndpointRequestKind::Recv => Some(endpoint_recv_error()),
        EndpointRequestKind::Close | EndpointRequestKind::Transfer { .. } => None,
    }
}

fn reject_endpoint_request(
    vm: &mut Vm,
    endpoint_id: u64,
    kind: &EndpointRequestKind,
    from_island: u32,
    fiber_key: u64,
    wait_id: u64,
) -> Result<(), VmError> {
    if fiber_key == 0 || wait_id == 0 {
        return Ok(());
    }
    let Some(response) = reject_endpoint_request_kind(kind) else {
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
            vm.state.current_island_id,
            endpoint_id,
            response,
            fiber_key,
            wait_id,
        ));
    vm.apply_runtime_transition(None, transition)?;
    Ok(())
}

pub(crate) fn preflight_endpoint_request_command(
    vm: &Vm,
    endpoint_id: u64,
    kind: &EndpointRequestKind,
    from_island: u32,
    fiber_key: u64,
    wait_id: u64,
) -> Result<(), VmError> {
    let from = QueueWaiter::endpoint(from_island, fiber_key, wait_id);

    if matches!(kind, EndpointRequestKind::Transfer { .. }) {
        return Ok(());
    }
    preflight_endpoint_request_response_identity(kind, fiber_key, wait_id)?;

    match vm.state.endpoint_registry.entry(endpoint_id) {
        Some(EndpointEntry::Live(ch)) => {
            if !endpoint_request_authorized(vm, ch, from_island)? {
                if fiber_key != 0 && wait_id != 0 && reject_endpoint_request_kind(kind).is_some() {
                    preflight_endpoint_response_route(
                        vm,
                        from,
                        "EndpointRequest reject requester route",
                    )?;
                }
                return Ok(());
            }
            preflight_endpoint_request_routes(vm, ch, kind, from)
        }
        Some(EndpointEntry::Tombstone { .. }) | None => match kind {
            EndpointRequestKind::Send { .. } | EndpointRequestKind::Recv => {
                preflight_endpoint_response_route(
                    vm,
                    from,
                    "EndpointRequest missing endpoint requester route",
                )
            }
            EndpointRequestKind::Close | EndpointRequestKind::Transfer { .. } => Ok(()),
        },
    }
}

pub(crate) fn handle_endpoint_request_command(
    vm: &mut Vm,
    endpoint_id: u64,
    kind: EndpointRequestKind,
    from_island: u32,
    fiber_key: u64,
    wait_id: u64,
) -> Result<(), VmError> {
    preflight_endpoint_request_response_identity(&kind, fiber_key, wait_id)?;
    let from = QueueWaiter::endpoint(from_island, fiber_key, wait_id);
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
                reject_endpoint_request(vm, endpoint_id, &kind, from_island, fiber_key, wait_id)?;
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

            let mut responses: Vec<(u32, EndpointResponseKind, u64, u64)> = Vec::new();
            let mut local_wakes: Vec<WakeCommand> = Vec::new();
            let mut island_effects = Vec::new();

            preflight_endpoint_request_routes(vm, ch, &kind, from.clone())?;
            let queue_snapshot = unsafe { queue::local_state(ch) }.clone();
            let endpoint_registry_snapshot = vm.state.endpoint_registry.snapshot();
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
                module: &module,
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
                        &mut transfer_commit,
                        &mut island_effects,
                    )
                })
            };
            if let Err(error) = inner_result {
                transfer_commit.restore_committed_local_endpoint_state(&mut vm.state);
                unsafe {
                    queue::with_local_state(ch, |state| {
                        *state = queue_snapshot.clone();
                    })
                };
                vm.state
                    .endpoint_registry
                    .restore(endpoint_registry_snapshot.clone());
                vm.mark_gc_all_roots_dirty();
                return Err(VmError::Jit(format!(
                    "EndpointRequest {endpoint_id} failed: {error}"
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
                    append_closed_home_endpoint_effects(
                        vm,
                        endpoint_id,
                        Some(from_island),
                        &mut transition,
                    );
                }
                transition.island_commands.extend(responses.into_iter().map(
                    |(target_island, resp_kind, resp_fiber, resp_wait_id)| {
                        IslandCommandEffect::endpoint_response(
                            target_island,
                            home_island,
                            endpoint_id,
                            resp_kind,
                            resp_fiber,
                            resp_wait_id,
                        )
                    },
                ));
                transition.wakes.append(&mut local_wakes);
                if let Err(err) = vm.apply_runtime_transition(None, transition) {
                    transfer_commit.restore_committed_local_endpoint_state(&mut vm.state);
                    unsafe {
                        queue::with_local_state(ch, |state| {
                            *state = queue_snapshot;
                        })
                    };
                    vm.state
                        .endpoint_registry
                        .restore(endpoint_registry_snapshot);
                    vm.mark_gc_all_roots_dirty();
                    return Err(err);
                }
            }
        }
        Some(EndpointEntry::Tombstone { .. }) | None => {
            let resp = match kind {
                EndpointRequestKind::Send { .. } => endpoint_send_ack(true),
                EndpointRequestKind::Recv => endpoint_recv_closed(),
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
                    vm.state.current_island_id,
                    endpoint_id,
                    resp,
                    fiber_key,
                    wait_id,
                ));
            vm.apply_runtime_transition(None, transition)?;
        }
    }
    Ok(())
}

fn preflight_endpoint_request_response_identity(
    kind: &EndpointRequestKind,
    fiber_key: u64,
    wait_id: u64,
) -> Result<(), VmError> {
    if !matches!(
        kind,
        EndpointRequestKind::Send { .. } | EndpointRequestKind::Recv
    ) {
        return Ok(());
    }
    if fiber_key == 0 || wait_id == 0 {
        return Err(VmError::Jit(
            "endpoint request missing response wait identity".to_string(),
        ));
    }
    validate_canonical_fiber_key(fiber_key, "endpoint request response identity")
        .map_err(VmError::Jit)
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
    module: &'a Module,
}

fn handle_endpoint_request_inner(
    ctx: &EndpointRequestCtx<'_>,
    state: &mut vo_runtime::objects::queue_state::LocalQueueState,
    req: EndpointRequestKind,
    from: QueueWaiter,
    vm_state: &mut crate::vm::VmState,
    responses: &mut Vec<(u32, EndpointResponseKind, u64, u64)>,
    local_wakes: &mut Vec<WakeCommand>,
    transfer_commit: &mut crate::exec::QueueTransferCommit,
    island_effects: &mut Vec<IslandCommandEffect>,
) -> Result<(), String> {
    let home_island = ctx.home_island;
    match req {
        EndpointRequestKind::Send { data } => {
            let requester = from.clone();
            let registry_snapshot = vm_state.endpoint_registry.snapshot();
            let value = match crate::exec::unpack_transport_message(
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
                Ok(value) => value,
                Err(_) => {
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_send_ack(true),
                        responses,
                        local_wakes,
                    );
                    return Ok(());
                }
            };
            vm_state.mark_gc_all_roots_dirty();
            if ctx.elem_meta.value_kind().may_contain_gc_refs()
                && vo_runtime::gc_types::try_typed_write_barrier_by_meta(
                    &mut vm_state.gc,
                    ctx.ch,
                    &value,
                    ctx.elem_meta,
                    Some(ctx.module),
                )
                .is_err()
            {
                vm_state.endpoint_registry.restore(registry_snapshot);
                dispatch_response(
                    requester,
                    home_island,
                    endpoint_send_ack(true),
                    responses,
                    local_wakes,
                );
                return Ok(());
            }
            let direct_receiver = state.waiting_receivers.front().cloned();
            if let Some(receiver) = direct_receiver.as_ref() {
                if validate_endpoint_recv_payload_for_waiter(ctx, receiver, &value, vm_state)
                    .is_err()
                {
                    vm_state.endpoint_registry.restore(registry_snapshot);
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_send_ack(true),
                        responses,
                        local_wakes,
                    );
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
                        vm_state.endpoint_registry.restore(registry_snapshot);
                        dispatch_response(
                            requester,
                            home_island,
                            endpoint_send_ack(true),
                            responses,
                            local_wakes,
                        );
                        return Ok(());
                    }
                }
            } else {
                None
            };
            let mut select_recv_slot_types = select_recv_slot_types;
            match state.send_or_block_resolved(value, ctx.cap, from, home_island) {
                vo_runtime::objects::queue_state::ResolvedSendResult::Wake {
                    receiver,
                    payload,
                } => {
                    let wake = match payload {
                        Some(payload) => {
                            let Some(slot_types) = select_recv_slot_types.take() else {
                                dispatch_response(
                                    requester,
                                    home_island,
                                    endpoint_send_ack(true),
                                    responses,
                                    local_wakes,
                                );
                                return Ok(());
                            };
                            match crate::exec::queue::select_woken_recv_payload_with_slot_types(
                                payload, slot_types,
                            ) {
                                Ok(payload) => {
                                    WakeCommand::queue_waiter_with_result(receiver, payload)
                                }
                                Err(_) => {
                                    dispatch_response(
                                        requester,
                                        home_island,
                                        endpoint_send_ack(true),
                                        responses,
                                        local_wakes,
                                    );
                                    return Ok(());
                                }
                            }
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
                    );
                }
                vo_runtime::objects::queue_state::ResolvedSendResult::RemoteDirect {
                    receiver,
                    payload: value,
                } => {
                    let (recv_kind, commit) = pack_recv_data_for_waiter(
                        ctx,
                        &receiver,
                        &value,
                        vm_state,
                        island_effects,
                    )?;
                    transfer_commit.absorb(commit);
                    dispatch_response(receiver, home_island, recv_kind, responses, local_wakes);
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_send_ack(false),
                        responses,
                        local_wakes,
                    );
                }
                vo_runtime::objects::queue_state::ResolvedSendResult::Buffered => {
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_send_ack(false),
                        responses,
                        local_wakes,
                    );
                }
                vo_runtime::objects::queue_state::ResolvedSendResult::Blocked => {}
                vo_runtime::objects::queue_state::ResolvedSendResult::Closed => {
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_send_ack(true),
                        responses,
                        local_wakes,
                    );
                }
            }
        }
        EndpointRequestKind::Recv => {
            let requester = from.clone();
            if preflight_endpoint_recv_value_for_waiter(ctx, state, &requester, vm_state).is_err() {
                dispatch_response(
                    requester,
                    home_island,
                    endpoint_recv_error(),
                    responses,
                    local_wakes,
                );
                return Ok(());
            }
            match state.recv_or_block(from) {
                vo_runtime::objects::queue_state::BlockingRecvResult::Success {
                    woke_sender,
                    payload: value,
                } => {
                    let (recv_kind, commit) = pack_recv_data_for_waiter(
                        ctx,
                        &requester,
                        &value,
                        vm_state,
                        island_effects,
                    )?;
                    transfer_commit.absorb(commit);
                    dispatch_response(requester, home_island, recv_kind, responses, local_wakes);
                    if let Some(sender) = woke_sender {
                        dispatch_response(
                            sender,
                            home_island,
                            endpoint_send_ack(false),
                            responses,
                            local_wakes,
                        );
                    }
                }
                vo_runtime::objects::queue_state::BlockingRecvResult::Blocked => {}
                vo_runtime::objects::queue_state::BlockingRecvResult::Closed => {
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_recv_closed(),
                        responses,
                        local_wakes,
                    );
                }
            }
        }
        EndpointRequestKind::Close => {
            state.close();
            for receiver in state.take_waiting_receivers() {
                dispatch_response(
                    receiver,
                    home_island,
                    endpoint_recv_closed(),
                    responses,
                    local_wakes,
                );
            }
            for (sender, _) in state.take_waiting_senders() {
                dispatch_response(
                    sender,
                    home_island,
                    endpoint_send_ack(true),
                    responses,
                    local_wakes,
                );
            }
        }
        EndpointRequestKind::Transfer { .. } => {
            return Err("Transfer reached endpoint request queue mutation path".to_string());
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
        target.island_id,
        ctx.struct_metas,
        &ctx.module.named_type_metas,
        ctx.runtime_types,
        vm_state,
    )
}

fn endpoint_recv_response_serializes(ctx: &EndpointRequestCtx<'_>, target: &QueueWaiter) -> bool {
    target.endpoint_wait_id() != 0 || target.island_id != ctx.home_island
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
        target.island_id,
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
) -> Result<(EndpointResponseKind, crate::exec::QueueTransferCommit), String> {
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
    kind: EndpointResponseKind,
    responses: &mut Vec<(u32, EndpointResponseKind, u64, u64)>,
    local_wakes: &mut Vec<WakeCommand>,
) {
    if target.endpoint_wait_id() == 0 && target.island_id == home_island {
        match kind {
            EndpointResponseKind::RecvData { closed: true, .. }
            | EndpointResponseKind::RecvError
            | EndpointResponseKind::Closed => {
                crate::runtime_boundary::push_queue_close_wake(
                    local_wakes,
                    WakeCommand::queue_closed_receiver(target, None),
                );
            }
            EndpointResponseKind::SendAck { closed: true } => {
                crate::runtime_boundary::push_queue_close_wake(
                    local_wakes,
                    WakeCommand::queue_closed_sender(target, None),
                );
            }
            EndpointResponseKind::SendAck { closed: false }
            | EndpointResponseKind::RecvData { closed: false, .. } => {
                local_wakes.push(WakeCommand::queue_waiter(target));
            }
        }
        return;
    }
    responses.push((
        target.island_id,
        kind,
        target.fiber_key(),
        target.endpoint_wait_id(),
    ));
}

pub(crate) fn append_closed_home_endpoint_effects(
    vm: &Vm,
    endpoint_id: u64,
    exclude_peer: Option<u32>,
    transition: &mut RuntimeTransition,
) {
    let peers = vm
        .state
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
                vm.state.current_island_id,
                endpoint_id,
                EndpointResponseKind::Closed,
                0,
                0,
            ));
    }
    transition
        .endpoint_tombstones
        .push(EndpointTombstone::with_response_source(
            endpoint_id,
            vm.state.current_island_id,
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
    fiber_key: u64,
    wait_id: u64,
    kind: EndpointResponseKind,
) -> RuntimeCommandOutcome {
    vm.apply_runtime_command(RuntimeCommand::endpoint_response(
        endpoint_id,
        from_island,
        fiber_key,
        wait_id,
        kind,
    ))
}

pub(crate) fn handle_endpoint_response_command(
    vm: &mut Vm,
    endpoint_id: u64,
    kind: EndpointResponseKind,
    from_island: u32,
    fiber_key: u64,
    wait_id: u64,
) -> Result<(), VmError> {
    if !endpoint_response_from_authorized_source(vm, endpoint_id, from_island) {
        return Ok(());
    }
    match kind {
        EndpointResponseKind::Closed => {
            let outcome = vm.apply_runtime_command(RuntimeCommand::endpoint_closed_response(
                endpoint_id,
                from_island,
            ));
            if !outcome.applied || !outcome.payload_accepted {
                return Err(VmError::Jit(String::from(
                    "endpoint closed response command was rejected by the VM",
                )));
            }
        }
        EndpointResponseKind::SendAck { closed } => {
            let outcome = resume_endpoint_response(
                vm,
                endpoint_id,
                from_island,
                fiber_key,
                wait_id,
                EndpointResponseKind::SendAck { closed },
            );
            if closed && outcome.payload_accepted {
                mark_remote_endpoint_closed(vm, endpoint_id);
            }
            if !outcome.applied || !outcome.payload_accepted {
                return Err(VmError::Jit(String::from(
                    "endpoint send response command was rejected by the VM",
                )));
            }
        }
        EndpointResponseKind::RecvData { data, closed } => {
            let outcome = resume_endpoint_response(
                vm,
                endpoint_id,
                from_island,
                fiber_key,
                wait_id,
                EndpointResponseKind::RecvData { data, closed },
            );
            if closed && outcome.payload_accepted {
                mark_remote_endpoint_closed(vm, endpoint_id);
            }
            if !outcome.applied || !outcome.payload_accepted {
                return Err(VmError::Jit(String::from(
                    "endpoint recv response command was rejected by the VM",
                )));
            }
        }
        EndpointResponseKind::RecvError => {
            let outcome = resume_endpoint_response(
                vm,
                endpoint_id,
                from_island,
                fiber_key,
                wait_id,
                EndpointResponseKind::RecvError,
            );
            if !outcome.applied || !outcome.payload_accepted {
                return Err(VmError::Jit(String::from(
                    "endpoint recv error response command was rejected by the VM",
                )));
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests;
