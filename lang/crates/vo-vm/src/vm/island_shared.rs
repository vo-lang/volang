#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::vec::Vec;

use vo_runtime::gc::GcRef;
use vo_runtime::island::{EndpointRequestKind, EndpointResponseKind};
use vo_runtime::island_msg;
use vo_runtime::objects::queue;
use vo_runtime::objects::queue_state::{QueueBacking, QueueWaiter};

use crate::bytecode::Module;

use super::types::EndpointEntry;
use super::{helpers, Vm};

pub(crate) fn handle_spawn_fiber(vm: &mut Vm, data: &[u8]) {
    let payload = island_msg::decode_spawn_header(data);
    let (capture_types, param_types, struct_metas, runtime_types) = {
        let module = vm.module().expect("module loaded");
        let func_idx = payload.func_id as usize;
        assert!(func_idx < module.functions.len(), "island spawn: invalid func_id {}", payload.func_id);
        let func_def = &module.functions[func_idx];
        (
            func_def.capture_types.clone(),
            func_def.param_types.clone(),
            module.struct_metas.clone(),
            module.runtime_types.clone(),
        )
    };

    let (gc, endpoint_registry) = (&mut vm.state.gc, &mut vm.state.endpoint_registry);
    let (unpacked_captures, unpacked_args) = island_msg::unpack_spawn_payload(
        gc,
        data,
        &payload,
        &capture_types,
        &param_types,
        &struct_metas,
        &runtime_types,
        |gc, handle| crate::exec::resolve_unpacked_queue_handle(gc, handle, endpoint_registry),
    );

    let closure_ref = vo_runtime::objects::closure::create(
        gc, payload.func_id, payload.num_captures as usize,
    );

    for (i, &cap_ref) in unpacked_captures.iter().enumerate() {
        vo_runtime::objects::closure::set_capture(closure_ref, i, cap_ref as u64);
    }

    let new_fiber = unsafe {
        helpers::build_closure_fiber_from_args_ptr(
            &vm.module().expect("module loaded").functions,
            vm.scheduler.fibers.len() as u32,
            closure_ref as u64,
            unpacked_args.as_ptr(),
            unpacked_args.len() as u32,
        )
    };
    vm.scheduler.spawn(new_fiber);
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

fn endpoint_recv_data(data: Vec<u8>) -> EndpointResponseKind {
    EndpointResponseKind::RecvData { data, closed: false }
}

pub(crate) fn handle_endpoint_request_command(
    vm: &mut Vm,
    endpoint_id: u64,
    kind: EndpointRequestKind,
    from_island: u32,
    fiber_id: u64,
) {
    let from = QueueWaiter::simple(from_island, fiber_id);
    let is_close = matches!(kind, EndpointRequestKind::Close);

    if let EndpointRequestKind::Transfer { new_peer } = &kind {
        if let Some(ch) = vm.state.endpoint_registry.get_live(endpoint_id) {
            queue::add_home_peer(ch, *new_peer);
        }
        return;
    }

    match vm.state.endpoint_registry.entries.get(&endpoint_id) {
        Some(EndpointEntry::Live(ch_ref)) => {
            let ch = *ch_ref;
            debug_assert!(
                vo_runtime::objects::queue_state::backing(ch) == QueueBacking::Local,
                "EndpointRequest arrived at non-LOCAL channel"
            );

            let cap = vo_runtime::objects::queue_state::capacity(ch);
            let elem_meta = vo_runtime::objects::queue_state::elem_meta(ch);
            let elem_slots = vo_runtime::objects::queue_state::elem_slots(ch) as usize;
            let home_island = vm.state.current_island_id;
            let module = vm.module.as_ref().expect("module loaded");

            let mut responses: Vec<(u32, EndpointResponseKind, u64)> = Vec::new();
            let mut local_wakes: Vec<QueueWaiter> = Vec::new();

            let ctx = EndpointRequestCtx {
                ch,
                cap,
                home_island,
                elem_meta,
                elem_slots,
                struct_metas: &module.struct_metas,
                runtime_types: &module.runtime_types,
                module,
            };
            queue::with_local_state(ch, |state| {
                handle_endpoint_request_inner(
                    &ctx,
                    state,
                    kind,
                    from,
                    &mut vm.state,
                    &mut responses,
                    &mut local_wakes,
                );
            });

            if is_close {
                finalize_closed_home_endpoint(vm, endpoint_id, Some(from_island));
            }

            for (target_island, resp_kind, resp_fiber) in responses {
                vm.state.send_endpoint_response(target_island, endpoint_id, resp_kind, resp_fiber);
            }

            for waiter in local_wakes {
                vm.scheduler.wake_queue_waiter(&waiter);
            }
        }
        Some(EndpointEntry::Tombstone) | None => {
            let resp = match kind {
                EndpointRequestKind::Send { .. } => endpoint_send_ack(true),
                EndpointRequestKind::Recv => endpoint_recv_closed(),
                EndpointRequestKind::Close | EndpointRequestKind::Transfer { .. } => return,
            };
            vm.state.send_endpoint_response(from_island, endpoint_id, resp, fiber_id);
        }
    }
}

struct EndpointRequestCtx<'a> {
    ch: GcRef,
    cap: usize,
    home_island: u32,
    elem_meta: vo_runtime::ValueMeta,
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
    responses: &mut Vec<(u32, EndpointResponseKind, u64)>,
    local_wakes: &mut Vec<QueueWaiter>,
) {
    let home_island = ctx.home_island;
    match req {
        EndpointRequestKind::Send { data } => {
            let requester = from.clone();
            let value = crate::exec::unpack_transport_message(
                &mut vm_state.gc,
                &data,
                ctx.elem_slots,
                ctx.struct_metas,
                ctx.runtime_types,
                &mut vm_state.endpoint_registry,
            );
            if ctx.elem_meta.value_kind().may_contain_gc_refs() {
                vo_runtime::gc_types::typed_write_barrier_by_meta(
                    &mut vm_state.gc,
                    ctx.ch,
                    &value,
                    ctx.elem_meta,
                    Some(ctx.module),
                );
            }
            match state.send_or_block_resolved(value, ctx.cap, from, home_island) {
                vo_runtime::objects::queue_state::ResolvedSendResult::Wake(receiver) => {
                    local_wakes.push(receiver);
                    dispatch_response(
                        requester,
                        home_island,
                        endpoint_send_ack(false),
                        responses,
                        local_wakes,
                    );
                }
                vo_runtime::objects::queue_state::ResolvedSendResult::RemoteDirect { receiver, payload: value } => {
                    let recv_kind = pack_recv_data_for_waiter(ctx, &receiver, &value, vm_state);
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
            let (result, value) = state.recv_or_block(from);
            match result {
                vo_runtime::objects::queue_state::RecvResult::Success(woke_sender) => {
                    let value = value.expect("recv_or_block success without payload");
                    let recv_kind = pack_recv_data_for_waiter(ctx, &requester, &value, vm_state);
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
                vo_runtime::objects::queue_state::RecvResult::Blocked => {}
                vo_runtime::objects::queue_state::RecvResult::WouldBlock => {
                    unreachable!("recv_or_block never returns WouldBlock")
                }
                vo_runtime::objects::queue_state::RecvResult::Closed => {
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
                dispatch_response(receiver, home_island, endpoint_recv_closed(), responses, local_wakes);
            }
            for (sender, _) in state.take_waiting_senders() {
                dispatch_response(sender, home_island, endpoint_send_ack(true), responses, local_wakes);
            }
        }
        EndpointRequestKind::Transfer { .. } => {
            unreachable!("Transfer is handled by caller before entering handle_endpoint_request_inner")
        }
    }
}

fn pack_recv_data_for_waiter(
    ctx: &EndpointRequestCtx<'_>,
    target: &QueueWaiter,
    value: &[u64],
    vm_state: &mut crate::vm::VmState,
) -> EndpointResponseKind {
    if target.island_id != ctx.home_island && ctx.elem_meta.value_kind().may_contain_gc_refs() {
        crate::exec::prepare_value_queue_handles_for_transfer(
            value,
            ctx.elem_meta,
            target.island_id,
            ctx.struct_metas,
            ctx.runtime_types,
            vm_state,
        );
    }
    let data = crate::exec::pack_transport_message(
        &vm_state.gc,
        value,
        ctx.elem_meta,
        ctx.struct_metas,
        ctx.runtime_types,
    );
    endpoint_recv_data(data)
}

fn dispatch_response(
    target: QueueWaiter,
    home_island: u32,
    kind: EndpointResponseKind,
    responses: &mut Vec<(u32, EndpointResponseKind, u64)>,
    local_wakes: &mut Vec<QueueWaiter>,
) {
    if target.island_id == home_island {
        local_wakes.push(target);
    } else {
        responses.push((target.island_id, kind, target.fiber_id));
    }
}

pub(crate) fn finalize_closed_home_endpoint(vm: &mut Vm, endpoint_id: u64, exclude_peer: Option<u32>) {
    let peers = vm
        .state
        .endpoint_registry
        .get_live(endpoint_id)
        .and_then(|ch| queue::home_info(ch))
        .map(|info| info.peers.iter().copied().collect::<Vec<_>>())
        .unwrap_or_default();

    for peer in peers {
        if Some(peer) == exclude_peer {
            continue;
        }
        vm.state
            .send_endpoint_response(peer, endpoint_id, EndpointResponseKind::Closed, 0);
    }

    vm.state.endpoint_registry.mark_tombstone(endpoint_id);
}

fn mark_remote_endpoint_closed(vm: &mut Vm, endpoint_id: u64) {
    if let Some(ch) = vm.state.endpoint_registry.get_live(endpoint_id) {
        if queue::is_remote(ch) {
            queue::mark_remote_closed(ch);
        }
    }
}

fn resume_endpoint_response(vm: &mut Vm, fiber_id: u64, kind: &EndpointResponseKind) {
    vm.state.pending_island_responses = vm.state.pending_island_responses.saturating_sub(1);
    let fid = crate::scheduler::FiberId::from_raw(fiber_id as u32);
    vm.scheduler.get_fiber_mut(fid).apply_endpoint_response(kind);
    vm.scheduler.wake_fiber(fid);
}

pub(crate) fn handle_endpoint_response_command(
    vm: &mut Vm,
    endpoint_id: u64,
    kind: EndpointResponseKind,
    fiber_id: u64,
) {
    match &kind {
        EndpointResponseKind::Closed => {
            mark_remote_endpoint_closed(vm, endpoint_id);
            vm.state.endpoint_registry.mark_tombstone(endpoint_id);
        }
        EndpointResponseKind::SendAck { closed } => {
            if *closed {
                mark_remote_endpoint_closed(vm, endpoint_id);
            }
            resume_endpoint_response(vm, fiber_id, &kind);
        }
        EndpointResponseKind::RecvData { closed, .. } => {
            if *closed {
                mark_remote_endpoint_closed(vm, endpoint_id);
            }
            resume_endpoint_response(vm, fiber_id, &kind);
        }
    }
}
