//! Island thread execution - runs a VM instance for an island.

use std::sync::Arc;

use vo_runtime::gc::GcRef;
use vo_runtime::island::IslandCommand;
use vo_runtime::island_msg;
use vo_runtime::island_transport::IslandTransport;
use vo_runtime::objects::channel;

use crate::bytecode::Module;
use super::{helpers, Vm};
pub use super::types::IslandRegistry;

/// Run an island thread - processes commands and executes fibers.
#[cfg(feature = "jit")]
pub fn run_island_thread(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
    jit_config: Option<super::JitConfig>,
) {
    let mut vm = match jit_config {
        Some(config) => Vm::with_jit_config(config),
        None => Vm::new(),
    };
    run_island_vm(island_id, module, transport, island_registry, &mut vm);
}

#[cfg(not(feature = "jit"))]
pub fn run_island_thread(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
) {
    let mut vm = Vm::new();
    run_island_vm(island_id, module, transport, island_registry, &mut vm);
}

fn run_island_vm(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
    vm: &mut Vm,
) {
    vm.load((*module).clone());
    vm.state.island_registry = Some(island_registry);
    vm.state.current_island_id = island_id;
    run_island_loop(vm, &transport);
}

fn run_island_loop(vm: &mut Vm, transport: &dyn IslandTransport) {
    loop {
        // 1. Process all pending commands first
        loop {
            match transport.try_recv() {
                Ok(Some(cmd)) => {
                    if handle_command(vm, cmd) { return; }
                }
                Ok(None) => break,
                Err(_) => return,
            }
        }
        
        // 2. Run scheduler if there's work
        if vm.scheduler.has_work() {
            let _ = vm.run_scheduled();
            continue; // Check for new commands after running
        }
        
        // 3. No runnable fibers - decide how to wait for next event
        let has_waiters = vm.scheduler.has_io_waiters() || vm.scheduler.has_blocked();
        
        if has_waiters {
            // Has pending I/O or blocked fibers - use timeout to allow periodic polling
            match transport.recv_timeout(std::time::Duration::from_millis(10)) {
                Ok(cmd) => {
                    if handle_command(vm, cmd) { return; }
                }
                Err(vo_runtime::island_transport::TransportError::Timeout) => {
                    // Poll I/O to check for completions
                    vm.scheduler.poll_io(&mut vm.state.io);
                }
                Err(_) => return,
            }
        } else {
            // Completely idle - block until command arrives
            match transport.recv() {
                Ok(cmd) => {
                    if handle_command(vm, cmd) { return; }
                }
                Err(_) => return,
            }
        }
    }
}

/// Returns true if should exit loop.
fn handle_command(vm: &mut Vm, cmd: IslandCommand) -> bool {
    match cmd {
        IslandCommand::Shutdown => true,
        IslandCommand::SpawnFiber { closure_data } => {
            handle_spawn_fiber(vm, closure_data.data());
            false
        }
        IslandCommand::WakeFiber { fiber_id } => {
            vm.scheduler.wake_fiber(crate::scheduler::FiberId::from_raw(fiber_id));
            let _ = vm.run_scheduled();
            false
        }
        IslandCommand::ChanRequest { endpoint_id, kind, from_island, fiber_id } => {
            handle_chan_request_command(vm, endpoint_id, kind, from_island, fiber_id);
            false
        }
        IslandCommand::ChanResponse { endpoint_id, kind, fiber_id } => {
            handle_chan_response_command(vm, endpoint_id, kind, fiber_id);
            false
        }
    }
}

fn handle_spawn_fiber(vm: &mut Vm, data: &[u8]) {
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
        |gc, handle| crate::exec::resolve_unpacked_chan_handle(gc, handle, endpoint_registry),
    );

    let closure_ref = vo_runtime::objects::closure::create(
        gc, payload.func_id, payload.num_captures as usize
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

    let _ = vm.run_scheduled();
}

// =============================================================================
// Cross-island channel request/response handlers
// =============================================================================

use vo_runtime::island::{ChanRequestKind, ChanResponseKind};

/// Handle incoming ChanRequest on the home island.
/// This is where the channel's ChannelState lives — all queue operations happen here.
pub fn handle_chan_request_command(
    vm: &mut Vm,
    endpoint_id: u64,
    kind: ChanRequestKind,
    from_island: u32,
    fiber_id: u64,
) {
    use vo_runtime::objects::queue_state::{QueueData, HomeInfo, QueueWaiter, BACKING_LOCAL};
    use super::types::EndpointEntry;

    let from = QueueWaiter::simple(from_island, fiber_id);
    let is_close = matches!(kind, ChanRequestKind::Close);

    // Handle Transfer separately (no ChannelState access needed)
    if let ChanRequestKind::Transfer { new_peer } = &kind {
        if let Some(ch) = vm.state.endpoint_registry.get_live(endpoint_id) {
            let data = QueueData::as_ref(ch);
            if data.endpoint_ptr != 0 {
                let info = unsafe { &mut *(data.endpoint_ptr as *mut HomeInfo) };
                info.peers.insert(*new_peer);
            }
        }
        return;
    }

    // Lookup home channel
    match vm.state.endpoint_registry.entries.get(&endpoint_id) {
        Some(EndpointEntry::Live(ch_ref)) => {
            let ch = *ch_ref;
            let data = QueueData::as_ref(ch);
            debug_assert!(data.backing == BACKING_LOCAL, "ChanRequest arrived at non-LOCAL channel");

            let cap = data.cap as usize;
            let elem_meta = data.elem_meta;
            let elem_slots = data.elem_slots as usize;
            let home_island = vm.state.current_island_id;
            let module = vm.module.as_ref().expect("module loaded");

            let mut responses: Vec<(u32, ChanResponseKind, u64)> = Vec::new();
            let mut local_wakes: Vec<QueueWaiter> = Vec::new();

            let ctx = ChanRequestCtx {
                ch, cap, home_island, elem_meta, elem_slots,
                struct_metas: &module.struct_metas,
                runtime_types: &module.runtime_types,
                module: Some(module),
            };
            channel::with_state(ch, |state| {
                handle_chan_request_inner(
                    &ctx, state, kind, from,
                    &mut vm.state,
                    &mut responses, &mut local_wakes,
                );
            });

            if is_close {
                finalize_closed_home_endpoint(vm, endpoint_id, Some(from_island));
            }

            // Send responses to remote islands
            for (target_island, resp_kind, resp_fiber) in responses {
                vm.state.send_chan_response(target_island, endpoint_id, resp_kind, resp_fiber);
            }

            // Wake local fibers
            for waiter in local_wakes {
                vm.scheduler.wake_queue_waiter(&waiter);
            }
            if vm.scheduler.has_work() {
                let _ = vm.run_scheduled();
            }
        }
        Some(EndpointEntry::Tombstone) | None => {
            // Stale request — respond with closed
            let resp = match kind {
                ChanRequestKind::Send { .. } =>
                    ChanResponseKind::SendAck { closed: true },
                ChanRequestKind::Recv =>
                    ChanResponseKind::RecvData { data: vec![], closed: true },
                ChanRequestKind::Close | ChanRequestKind::Transfer { .. } => return,
            };
            vm.state.send_chan_response(from_island, endpoint_id, resp, fiber_id);
        }
    }
}

/// Static channel metadata extracted before entering the critical section.
struct ChanRequestCtx<'a> {
    ch: GcRef,
    cap: usize,
    home_island: u32,
    elem_meta: vo_runtime::ValueMeta,
    elem_slots: usize,
    struct_metas: &'a [vo_common_core::bytecode::StructMeta],
    runtime_types: &'a [vo_common_core::RuntimeType],
    module: Option<&'a crate::bytecode::Module>,
}

/// Core channel request handler — operates on ChannelState.
/// Shared by local exec layer and remote command handler.
fn handle_chan_request_inner(
    ctx: &ChanRequestCtx<'_>,
    state: &mut vo_runtime::objects::queue_state::ChannelState,
    req: ChanRequestKind,
    from: vo_runtime::objects::queue_state::QueueWaiter,
    vm_state: &mut crate::vm::VmState,
    responses: &mut Vec<(u32, ChanResponseKind, u64)>,
    local_wakes: &mut Vec<vo_runtime::objects::queue_state::QueueWaiter>,
) {
    let home_island = ctx.home_island;
    match req {
        ChanRequestKind::Send { data } => {
            let requester = from.clone();
            let value = crate::exec::unpack_transport_message(
                &mut vm_state.gc,
                &data,
                ctx.elem_slots,
                ctx.struct_metas,
                ctx.runtime_types,
                Some(&mut vm_state.endpoint_registry),
            );
            if ctx.elem_meta.value_kind().may_contain_gc_refs() {
                vo_runtime::gc_types::typed_write_barrier_by_meta(
                    &mut vm_state.gc, ctx.ch, &value, ctx.elem_meta, ctx.module,
                );
            }
            match state.send_or_block(value, ctx.cap, from) {
                vo_runtime::objects::queue_state::SendResult::DirectSend(receiver) => {
                    if vm_state.is_local_waiter(&receiver) {
                        local_wakes.push(receiver);
                    } else {
                        let value = state.buffer.pop_back()
                            .expect("remote direct send must leave payload in buffer");
                        let recv_kind = pack_recv_data_for_waiter(ctx, &receiver, &value, vm_state);
                        dispatch_response(receiver, home_island, recv_kind, responses, local_wakes);
                    }
                    dispatch_response(
                        requester,
                        home_island,
                        ChanResponseKind::SendAck { closed: false },
                        responses,
                        local_wakes,
                    );
                }
                vo_runtime::objects::queue_state::SendResult::Buffered => {
                    dispatch_response(
                        requester,
                        home_island,
                        ChanResponseKind::SendAck { closed: false },
                        responses,
                        local_wakes,
                    );
                }
                vo_runtime::objects::queue_state::SendResult::Blocked => {}
                vo_runtime::objects::queue_state::SendResult::WouldBlock(_) => {
                    unreachable!("send_or_block never returns WouldBlock")
                }
                vo_runtime::objects::queue_state::SendResult::Closed => {
                    dispatch_response(
                        requester,
                        home_island,
                        ChanResponseKind::SendAck { closed: true },
                        responses,
                        local_wakes,
                    );
                }
            }
        }

        ChanRequestKind::Recv => {
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
                            ChanResponseKind::SendAck { closed: false },
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
                        ChanResponseKind::RecvData { data: vec![], closed: true },
                        responses,
                        local_wakes,
                    );
                }
            }
        }

        ChanRequestKind::Close => {
            state.close();
            for receiver in state.take_waiting_receivers() {
                dispatch_response(receiver, home_island,
                    ChanResponseKind::RecvData { data: vec![], closed: true },
                    responses, local_wakes);
            }
            for (sender, _) in state.take_waiting_senders() {
                dispatch_response(sender, home_island,
                    ChanResponseKind::SendAck { closed: true },
                    responses, local_wakes);
            }
        }

        ChanRequestKind::Transfer { .. } => {
            unreachable!("Transfer is handled by caller before entering handle_chan_request_inner")
        }
    }
}

fn pack_recv_data_for_waiter(
    ctx: &ChanRequestCtx<'_>,
    target: &vo_runtime::objects::queue_state::QueueWaiter,
    value: &[u64],
    vm_state: &mut crate::vm::VmState,
) -> ChanResponseKind {
    if target.island_id != ctx.home_island && ctx.elem_meta.value_kind().may_contain_gc_refs() {
        crate::exec::prepare_value_chans_for_transfer(
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
    ChanResponseKind::RecvData { data, closed: false }
}

/// Route a response to the correct target (remote message or local wake).
fn dispatch_response(
    target: vo_runtime::objects::queue_state::QueueWaiter,
    home_island: u32,
    kind: ChanResponseKind,
    responses: &mut Vec<(u32, ChanResponseKind, u64)>,
    local_wakes: &mut Vec<vo_runtime::objects::queue_state::QueueWaiter>,
) {
    if target.island_id == home_island {
        local_wakes.push(target);
    } else {
        responses.push((target.island_id, kind, target.fiber_id));
    }
}

/// Apply a channel response to a local fiber before waking it.
/// Writes data to fiber fields so the ChanRecv/ChanSend handler can consume it on retry.
fn apply_local_response(vm: &mut Vm, fiber_id: u64, kind: &ChanResponseKind) {
    let fid = crate::scheduler::FiberId::from_raw(fiber_id as u32);
    let fiber = vm.scheduler.get_fiber_mut(fid);
    match kind {
        ChanResponseKind::SendAck { closed } => {
            if *closed {
                fiber.remote_send_closed = true;
                if let Some(frame) = fiber.current_frame_mut() {
                    frame.pc -= 1; // Roll back to ChanSend instruction
                }
            }
            // Non-closed: fiber resumes at next instruction (no action needed)
        }
        ChanResponseKind::RecvData { data, .. } => {
            fiber.remote_recv_response = Some(crate::fiber::RemoteRecvResponse {
                data: data.clone(),
                closed: matches!(kind, ChanResponseKind::RecvData { closed: true, .. }),
            });
        }
        ChanResponseKind::Closed => {
            // Broadcast close — no fiber to wake (fiber_id=0)
        }
    }
}

pub(crate) fn finalize_closed_home_endpoint(vm: &mut Vm, endpoint_id: u64, exclude_peer: Option<u32>) {
    let peers = vm.state.endpoint_registry.get_live(endpoint_id)
        .and_then(|ch| channel::home_info(ch))
        .map(|info| info.peers.iter().copied().collect::<Vec<_>>())
        .unwrap_or_default();

    for peer in peers {
        if Some(peer) == exclude_peer {
            continue;
        }
        vm.state.send_chan_response(peer, endpoint_id, ChanResponseKind::Closed, 0);
    }

    vm.state.endpoint_registry.mark_tombstone(endpoint_id);
}

/// Handle incoming channel response on a REMOTE proxy island.
/// For SendAck/RecvData: writes data to fiber fields, wakes fiber, runs scheduler.
/// For Closed: marks proxy as closed.
pub fn handle_chan_response_command(
    vm: &mut Vm,
    endpoint_id: u64,
    kind: ChanResponseKind,
    fiber_id: u64,
) {
    match &kind {
        ChanResponseKind::Closed => {
            // Broadcast close notification — mark proxy as closed, no fiber to wake
            if let Some(ch) = vm.state.endpoint_registry.get_live(endpoint_id) {
                if channel::is_remote(ch) {
                    channel::remote_proxy_mut(ch).closed = true;
                }
            }
            vm.state.endpoint_registry.mark_tombstone(endpoint_id);
        }
        _ => {
            // SendAck or RecvData — write to fiber fields, wake, and run
            apply_local_response(vm, fiber_id, &kind);
            let fid = crate::scheduler::FiberId::from_raw(fiber_id as u32);
            vm.scheduler.wake_fiber(fid);
            let _ = vm.run_scheduled();
        }
    }
}

