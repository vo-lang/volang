//! JIT callbacks for channel operations.
//!
//! These callbacks are called from JIT-compiled code when channel operations
//! cannot be inlined. They handle the full channel protocol including blocking
//! and waking of fibers.

use vo_runtime::gc::GcRef;
use vo_runtime::jit_api::{
    set_jit_infra_error, set_jit_infra_error_with_message, JitContext, JitResult,
    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
};

use crate::exec::QueueAction;
use crate::runtime_boundary::{
    IslandCommandEffect, PendingTransitionTerminalPolicy, ResumePolicy, RuntimeBoundary,
    RuntimeTransition, WakeCommand,
};
use crate::vm::helpers;
use crate::vm::{GcRootEffect, RuntimeTrapKind};

use super::helpers::{
    extract_context, queue_layout_for_current_pc, set_jit_trap, validate_callback_raw_slot_span,
    validate_callback_raw_slots, validate_queue_layout_slot_count,
};

const JIT_QUEUE_CLOSE_UNEXPECTED_ACTION: u64 = 1;
const JIT_QUEUE_SEND_UNEXPECTED_ACTION: u64 = 2;
const JIT_QUEUE_RECV_UNEXPECTED_RESULT: u64 = 3;
const JIT_QUEUE_LEN_INVALID_HANDLE: u64 = 4;
const JIT_QUEUE_INVALID_SEND_BUFFER: u64 = 5;
const JIT_QUEUE_INVALID_RECV_BUFFER: u64 = 6;
const JIT_QUEUE_CAP_INVALID_HANDLE: u64 = 7;
const JIT_QUEUE_INVALID_GET_OUTPUT: u64 = 8;
const JIT_QUEUE_RECV_INVALID_HANDLE: u64 = 9;
const JIT_QUEUE_ROUTE_UNAVAILABLE: u64 = 10;

fn set_queue_trap(
    gc: &mut vo_runtime::gc::Gc,
    fiber: &mut crate::fiber::Fiber,
    kind: RuntimeTrapKind,
) -> JitResult {
    set_jit_trap(gc, fiber, kind, helpers::runtime_trap_message(kind))
}

fn commit_queue_wakes(
    ctx: *mut JitContext,
    vm: &mut crate::vm::Vm,
    wakes: Vec<WakeCommand>,
) -> JitResult {
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition.wakes = wakes;
    commit_queue_transition(ctx, vm, transition)
}

fn commit_queue_transition(
    ctx: *mut JitContext,
    vm: &mut crate::vm::Vm,
    mut transition: RuntimeTransition,
) -> JitResult {
    let _ = ctx;
    transition.set_pending_terminal_policy(PendingTransitionTerminalPolicy::CommitOnAnyTerminal);
    vm.push_pending_runtime_transition(transition);
    JitResult::Ok
}

fn push_pending_queue_transition_with_policy(
    vm: &mut crate::vm::Vm,
    mut transition: RuntimeTransition,
    terminal_policy: PendingTransitionTerminalPolicy,
) {
    transition.set_pending_terminal_policy(terminal_policy);
    vm.push_pending_runtime_transition(transition);
}

fn push_pending_queue_transition(vm: &mut crate::vm::Vm, transition: RuntimeTransition) {
    push_pending_queue_transition_with_policy(
        vm,
        transition,
        PendingTransitionTerminalPolicy::DiscardOnTerminal,
    );
}

fn fiber_stack_slot_snapshot(
    fiber: &crate::fiber::Fiber,
    dst_ptr: *const u64,
    slots: usize,
) -> Option<Vec<(usize, u64)>> {
    if slots == 0 {
        return Some(Vec::new());
    }

    let slot_size = core::mem::size_of::<u64>();
    let stack_base = fiber.stack.as_ptr() as usize;
    let stack_bytes = fiber.stack.len().checked_mul(slot_size)?;
    let stack_end = stack_base.checked_add(stack_bytes)?;
    let dst = dst_ptr as usize;
    let dst_bytes = slots.checked_mul(slot_size)?;
    let dst_end = dst.checked_add(dst_bytes)?;
    if dst < stack_base || dst_end > stack_end {
        return None;
    }

    let byte_offset = dst.checked_sub(stack_base)?;
    if byte_offset % slot_size != 0 {
        return None;
    }
    let start = byte_offset / slot_size;
    start
        .checked_add(slots)
        .filter(|end| *end <= fiber.stack.len())?;
    Some(crate::exec::stack_slot_snapshot(
        fiber.stack.as_ptr(),
        start,
        slots,
    ))
}

fn preflight_jit_queue_route(
    ctx: *mut JitContext,
    result: Result<(), String>,
) -> Result<(), JitResult> {
    result.map_err(|msg| {
        set_jit_infra_error_with_message(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_QUEUE_ROUTE_UNAVAILABLE,
            msg,
        )
    })
}

fn jit_queue_get(
    ctx: *mut JitContext,
    chan: u64,
    out: *mut u64,
    get: unsafe fn(GcRef) -> usize,
    invalid_handle_detail: u64,
) -> JitResult {
    if let Err(result) = validate_callback_raw_slot_span(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_GET_OUTPUT,
        out,
        1,
    ) {
        return result;
    }

    let (vm, _) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;
    let value = if ch.is_null() {
        0
    } else {
        let ch = match crate::exec::validate_queue_handle(&vm.state.gc, ch, "QueueGet") {
            Ok(ch) => ch,
            Err(msg) => {
                return set_jit_infra_error_with_message(
                    ctx,
                    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    invalid_handle_detail,
                    msg,
                )
            }
        };
        // Safety: validate_queue_handle proved that ch is a live queue object.
        unsafe { get(ch) as u64 }
    };
    unsafe {
        *out = value;
    }
    JitResult::Ok
}

/// Read a channel length through the same checked queue handle contract as the interpreter.
pub extern "C" fn jit_queue_len(ctx: *mut JitContext, chan: u64, out: *mut u64) -> JitResult {
    jit_queue_get(
        ctx,
        chan,
        out,
        crate::exec::queue_len,
        JIT_QUEUE_LEN_INVALID_HANDLE,
    )
}

/// Read a channel capacity through the same checked queue handle contract as the interpreter.
pub extern "C" fn jit_queue_cap(ctx: *mut JitContext, chan: u64, out: *mut u64) -> JitResult {
    jit_queue_get(
        ctx,
        chan,
        out,
        vo_runtime::objects::queue_state::capacity,
        JIT_QUEUE_CAP_INVALID_HANDLE,
    )
}

/// Close a channel.
pub extern "C" fn jit_queue_close(ctx: *mut JitContext, chan: u64) -> JitResult {
    use crate::exec::{queue_close_core, QueueAction};

    let (vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;
    if let Err(result) = preflight_jit_queue_route(
        ctx,
        crate::exec::preflight_queue_close_routes(&vm.state, ch),
    ) {
        return result;
    }
    match queue_close_core(&vm.state, ch) {
        QueueAction::Continue => JitResult::Ok,
        QueueAction::Close {
            receivers,
            senders,
            endpoint_id,
            rollback,
        } => {
            let mut transition = RuntimeTransition::new(
                RuntimeBoundary::Continue,
                ResumePolicy::PreserveFramePc,
                GcRootEffect::CurrentFiberDirty,
            );
            transition.set_rollback(rollback);
            for waiter in receivers {
                transition
                    .push_queue_close_wake(WakeCommand::queue_closed_receiver(waiter, endpoint_id));
            }
            for waiter in senders {
                transition
                    .push_queue_close_wake(WakeCommand::queue_closed_sender(waiter, endpoint_id));
            }
            if let Some(endpoint_id) = endpoint_id {
                crate::vm::island_shared::append_closed_home_endpoint_effects(
                    vm,
                    endpoint_id,
                    None,
                    &mut transition,
                );
            }
            commit_queue_transition(ctx, vm, transition)
        }
        QueueAction::RemoteClose {
            endpoint_id,
            home_island,
            rollback,
        } => {
            let mut transition = RuntimeTransition::new(
                RuntimeBoundary::Continue,
                ResumePolicy::PreserveFramePc,
                GcRootEffect::AllRootsDirty,
            );
            transition.set_rollback(rollback);
            transition
                .island_commands
                .push(IslandCommandEffect::endpoint_close_request(
                    home_island,
                    endpoint_id,
                    vm.state.current_island_id,
                ));
            transition.endpoint_tombstones.push(
                crate::runtime_boundary::EndpointTombstone::with_response_source(
                    endpoint_id,
                    home_island,
                ),
            );
            commit_queue_transition(ctx, vm, transition)
        }
        QueueAction::Trap(kind) => set_queue_trap(&mut vm.state.gc, fiber, kind),
        QueueAction::Malformed(_) => set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_QUEUE_CLOSE_UNEXPECTED_ACTION,
        ),
        // channel_close_core never produces these variants
        QueueAction::Block { .. }
        | QueueAction::ReplayThenBlock { .. }
        | QueueAction::Wake { .. } => set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_QUEUE_CLOSE_UNEXPECTED_ACTION,
        ),
        QueueAction::RemoteSend { .. }
        | QueueAction::RemoteRecv { .. }
        | QueueAction::RemoteSendAck { .. }
        | QueueAction::RemoteRecvData { .. } => set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_QUEUE_CLOSE_UNEXPECTED_ACTION,
        ),
    }
}

/// Send on a channel. Returns WaitQueue if would block.
pub extern "C" fn jit_queue_send(
    ctx: *mut JitContext,
    chan: u64,
    val_ptr: *const u64,
    val_slots: u32,
) -> JitResult {
    use crate::exec::{queue_send_core_with_layout, QueueAction};

    let module = unsafe { &*((*ctx).module) };
    let elem_layout = match queue_layout_for_current_pc(unsafe { &*ctx }, module) {
        Ok(layout) => layout,
        Err(msg) => {
            return set_jit_infra_error_with_message(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_INVALID_SEND_BUFFER,
                msg,
            )
        }
    };
    if let Err(result) = validate_queue_layout_slot_count(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_SEND_BUFFER,
        elem_layout,
        val_slots as usize,
    ) {
        return result;
    }
    let (vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;
    let val_slots = match validate_callback_raw_slots(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_SEND_BUFFER,
        val_ptr,
        val_slots,
    ) {
        Ok(val_slots) => val_slots,
        Err(result) => return result,
    };
    if !ch.is_null() {
        let ch = match crate::exec::validate_queue_handle(&vm.state.gc, ch, "QueueSend") {
            Ok(ch) => ch,
            Err(msg) => {
                return set_jit_infra_error_with_message(
                    ctx,
                    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    JIT_QUEUE_SEND_UNEXPECTED_ACTION,
                    msg,
                )
            }
        };
        // Safety: validate_queue_handle proved that ch is a live queue object.
        let queue_elem_slots = unsafe { vo_runtime::objects::queue_state::elem_slots(ch) } as usize;
        if val_slots != queue_elem_slots {
            return set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_INVALID_SEND_BUFFER,
            );
        }
    }
    if fiber.consume_remote_send_closed() {
        return set_jit_trap(
            &mut vm.state.gc,
            fiber,
            RuntimeTrapKind::SendOnClosedChannel,
            helpers::ERR_SEND_ON_CLOSED,
        );
    }
    if let Err(result) =
        preflight_jit_queue_route(ctx, crate::exec::preflight_queue_send_routes(&vm.state, ch))
    {
        return result;
    }
    // Safety: callback ABI validation above established a readable payload span
    // that remains live until the native helper returns.
    let src = unsafe { core::slice::from_raw_parts(val_ptr, val_slots) };

    match queue_send_core_with_layout(
        ch,
        src,
        elem_layout,
        vm.state.current_island_id,
        fiber.wake_key_packed(),
        &mut vm.state,
        &module.struct_metas,
        &module.runtime_types,
        Some(module),
    ) {
        QueueAction::Wake { waiter, payload } => commit_queue_wakes(
            ctx,
            vm,
            vec![match payload {
                Some(payload) => WakeCommand::queue_waiter_with_result(waiter, payload),
                None => WakeCommand::queue_waiter(waiter),
            }],
        ),
        QueueAction::Continue => JitResult::Ok,
        QueueAction::Block { waiter } => {
            if let Some(waiter) = waiter.as_ref() {
                fiber.begin_queue_wait(waiter);
            } else {
                fiber.clear_queue_wait();
            }
            JitResult::WaitQueue
        }
        QueueAction::RemoteSend {
            endpoint_id,
            home_island,
            data,
            mut island_effects,
            transfer_commit,
        } => {
            fiber.clear_queue_wait();
            let fiber_key = fiber.endpoint_response_key();
            let wait_id = fiber.begin_remote_endpoint_send_wait(endpoint_id);
            let mut transition = RuntimeTransition::new(
                RuntimeBoundary::Continue,
                ResumePolicy::PreserveFramePc,
                GcRootEffect::CurrentFiberDirty,
            );
            transition.island_commands.append(&mut island_effects);
            transition
                .island_commands
                .push(IslandCommandEffect::endpoint_send_request(
                    home_island,
                    endpoint_id,
                    data,
                    vm.state.current_island_id,
                    fiber_key,
                    wait_id,
                ));
            let terminal_policy = if transfer_commit.requires_terminal_commit() {
                PendingTransitionTerminalPolicy::CommitOnAnyTerminal
            } else {
                PendingTransitionTerminalPolicy::DiscardOnTerminal
            };
            if let Some(rollback) = transfer_commit.into_runtime_rollback() {
                transition.set_rollback(rollback);
            }
            push_pending_queue_transition_with_policy(vm, transition, terminal_policy);
            JitResult::WaitQueue
        }
        QueueAction::Trap(kind) => set_queue_trap(&mut vm.state.gc, fiber, kind),
        QueueAction::Malformed(_) => set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_QUEUE_SEND_UNEXPECTED_ACTION,
        ),
        QueueAction::RemoteRecvData {
            endpoint_id,
            target_island,
            fiber_key,
            wait_id,
            data,
            mut island_effects,
            rollback,
        } => {
            let mut transition = RuntimeTransition::new(
                RuntimeBoundary::Continue,
                ResumePolicy::PreserveFramePc,
                GcRootEffect::CurrentFiberDirty,
            );
            transition.island_commands.append(&mut island_effects);
            transition
                .island_commands
                .push(IslandCommandEffect::endpoint_recv_data_response(
                    target_island,
                    vm.state.current_island_id,
                    endpoint_id,
                    data,
                    fiber_key,
                    wait_id,
                ));
            if let Some(rollback) = rollback {
                transition.set_rollback(rollback);
            }
            commit_queue_transition(ctx, vm, transition)
        }
        // channel_send_core never produces these variants
        QueueAction::ReplayThenBlock { .. } | QueueAction::Close { .. } => set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_QUEUE_SEND_UNEXPECTED_ACTION,
        ),
        QueueAction::RemoteRecv { .. }
        | QueueAction::RemoteSendAck { .. }
        | QueueAction::RemoteClose { .. } => set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_QUEUE_SEND_UNEXPECTED_ACTION,
        ),
    }
}

/// Receive from a channel. Returns WaitQueue if would block.
/// Writes received value to dst_ptr. If has_ok, writes ok flag after value.
pub extern "C" fn jit_queue_recv(
    ctx: *mut JitContext,
    chan: u64,
    dst_ptr: *mut u64,
    elem_slots: u32,
    has_ok: u32,
) -> JitResult {
    use crate::exec::{complete_queue_recv, queue_recv_core, QueueRecvCoreResult};

    let module = unsafe { &*((*ctx).module) };
    let elem_layout = match queue_layout_for_current_pc(unsafe { &*ctx }, module) {
        Ok(layout) => layout,
        Err(msg) => {
            return set_jit_infra_error_with_message(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_INVALID_RECV_BUFFER,
                msg,
            )
        }
    };
    let (vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;
    let has_ok = has_ok != 0;
    let elem_slots_u16 = match super::helpers::validate_callback_slot_count(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_RECV_BUFFER,
        elem_slots,
    ) {
        Ok(elem_slots) => elem_slots,
        Err(result) => return result,
    };
    let elem_slots = usize::from(elem_slots_u16);
    if let Err(result) = validate_queue_layout_slot_count(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_RECV_BUFFER,
        elem_layout,
        elem_slots,
    ) {
        return result;
    }
    let Some(dst_slots) = elem_slots.checked_add(usize::from(has_ok)) else {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_QUEUE_INVALID_RECV_BUFFER,
        );
    };
    if let Err(result) = validate_callback_raw_slot_span(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_RECV_BUFFER,
        dst_ptr,
        dst_slots,
    ) {
        return result;
    }

    let ch = if ch.is_null() {
        if fiber.remote_recv_response.is_some() {
            return set_jit_infra_error_with_message(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_RECV_INVALID_HANDLE,
                "QueueRecv replay requires a queue handle",
            );
        }
        ch
    } else {
        let ch = match crate::exec::validate_queue_handle(&vm.state.gc, ch, "QueueRecv") {
            Ok(ch) => ch,
            Err(msg) => {
                return set_jit_infra_error_with_message(
                    ctx,
                    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    JIT_QUEUE_RECV_INVALID_HANDLE,
                    msg,
                )
            }
        };
        // Safety: validate_queue_handle proved that ch is a live queue object.
        let queue_elem_slots = unsafe { vo_runtime::objects::queue_state::elem_slots(ch) } as usize;
        if elem_slots != queue_elem_slots {
            return set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_INVALID_RECV_BUFFER,
            );
        }
        if let Some(elem_layout) = elem_layout {
            if let Err(msg) = crate::exec::validate_queue_payload_layout(
                ch,
                elem_layout,
                "QueueRecv",
                Some(module),
            ) {
                return set_jit_infra_error_with_message(
                    ctx,
                    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    JIT_QUEUE_INVALID_RECV_BUFFER,
                    msg,
                );
            }
        }
        ch
    };

    if let Some(recv_response) = fiber.remote_recv_response.clone() {
        // Safety: the non-null handle was validated above and remains rooted by the fiber wait.
        let (elem_meta, elem_rttid) = unsafe {
            (
                vo_runtime::objects::queue_state::elem_meta(ch),
                vo_runtime::objects::queue_state::elem_rttid(ch),
            )
        };
        if crate::exec::replay_remote_queue_recv_response(
            &mut vm.state.gc,
            recv_response,
            elem_meta,
            elem_rttid,
            elem_slots,
            has_ok,
            &module.struct_metas,
            &module.named_type_metas,
            &module.runtime_types,
            &mut vm.state.endpoint_registry,
            |i, value| unsafe { *dst_ptr.add(i) = value },
        )
        .is_err()
        {
            return set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_RECV_UNEXPECTED_RESULT,
            );
        }
        fiber.remote_recv_response = None;
        vm.mark_gc_all_roots_dirty();
        return JitResult::Ok;
    }
    if let Err(result) =
        preflight_jit_queue_route(ctx, crate::exec::preflight_queue_recv_routes(&vm.state, ch))
    {
        return result;
    }
    // Safety: every non-null handle reached here through validate_queue_handle.
    let has_local_endpoint_sender = !ch.is_null()
        && unsafe {
            !vo_runtime::objects::queue::is_remote(ch)
                && vo_runtime::objects::queue::next_recv_endpoint_sender(ch).is_some()
        };
    let remote_sender_rollback = if has_local_endpoint_sender {
        let Some(stack_slots) = fiber_stack_slot_snapshot(fiber, dst_ptr, dst_slots) else {
            return set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_INVALID_RECV_BUFFER,
            );
        };
        Some(
            crate::runtime_boundary::RuntimeRollback::local_queue_with_stack_slots(
                &vm.state,
                ch,
                stack_slots,
            ),
        )
    } else {
        None
    };
    match complete_queue_recv(
        queue_recv_core(
            &vm.state.gc,
            ch,
            vm.state.current_island_id,
            fiber.wake_key_packed(),
        ),
        elem_slots,
        has_ok,
        |i, value| unsafe { *dst_ptr.add(i) = value },
    ) {
        Ok(Some(sender)) => {
            match crate::exec::queue_sender_ack_or_wake(ch, sender, false, remote_sender_rollback) {
                QueueAction::Wake { waiter, payload } => commit_queue_wakes(
                    ctx,
                    vm,
                    vec![match payload {
                        Some(payload) => WakeCommand::queue_waiter_with_result(waiter, payload),
                        None => WakeCommand::queue_waiter(waiter),
                    }],
                ),
                QueueAction::RemoteSendAck {
                    endpoint_id,
                    target_island,
                    fiber_key,
                    wait_id,
                    closed,
                    rollback,
                } => {
                    let mut transition = RuntimeTransition::new(
                        RuntimeBoundary::Continue,
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::CurrentFiberDirty,
                    );
                    transition
                        .island_commands
                        .push(IslandCommandEffect::endpoint_response(
                            target_island,
                            vm.state.current_island_id,
                            endpoint_id,
                            vo_runtime::island::EndpointResponseKind::SendAck { closed },
                            fiber_key,
                            wait_id,
                        ));
                    if let Some(rollback) = rollback {
                        transition.set_rollback(rollback);
                    }
                    commit_queue_transition(ctx, vm, transition)
                }
                QueueAction::Malformed(_) => set_jit_infra_error(
                    ctx,
                    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    JIT_QUEUE_RECV_UNEXPECTED_RESULT,
                ),
                _ => set_jit_infra_error(
                    ctx,
                    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    JIT_QUEUE_RECV_UNEXPECTED_RESULT,
                ),
            }
        }
        Ok(None) => {
            vm.mark_gc_fiber_roots_dirty(crate::scheduler::FiberId::from_raw(fiber.id));
            JitResult::Ok
        }
        Err(QueueRecvCoreResult::WouldBlock { waiter }) => {
            if let Some(waiter) = waiter.as_ref() {
                fiber.begin_queue_wait(waiter);
            } else {
                fiber.clear_queue_wait();
            }
            JitResult::WaitQueue
        }
        Err(QueueRecvCoreResult::Remote {
            endpoint_id,
            home_island,
        }) => {
            fiber.clear_queue_wait();
            let fiber_key = fiber.endpoint_response_key();
            let wait_id = fiber.begin_remote_endpoint_recv_wait(endpoint_id);
            let mut transition = RuntimeTransition::new(
                RuntimeBoundary::Continue,
                ResumePolicy::PreserveFramePc,
                GcRootEffect::CurrentFiberDirty,
            );
            transition
                .island_commands
                .push(IslandCommandEffect::endpoint_recv_request(
                    home_island,
                    endpoint_id,
                    vm.state.current_island_id,
                    fiber_key,
                    wait_id,
                ));
            push_pending_queue_transition(vm, transition);
            JitResult::WaitQueue
        }
        Err(QueueRecvCoreResult::Trap(kind)) => set_queue_trap(&mut vm.state.gc, fiber, kind),
        Err(QueueRecvCoreResult::Malformed(_)) => set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_QUEUE_RECV_UNEXPECTED_RESULT,
        ),
        Err(QueueRecvCoreResult::Success { .. } | QueueRecvCoreResult::Closed) => {
            set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_RECV_UNEXPECTED_RESULT,
            )
        }
    }
}

#[cfg(test)]
mod tests;
