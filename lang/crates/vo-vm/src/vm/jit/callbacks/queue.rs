//! JIT callbacks for channel operations.
//!
//! These callbacks are called from JIT-compiled code when channel operations
//! cannot be inlined. They handle the full channel protocol including blocking
//! and waking of fibers.

use vo_runtime::gc::GcRef;
use vo_runtime::jit_api::{
    set_jit_infra_error, JitContext, JitResult, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
};

use crate::vm::helpers;
use crate::vm::RuntimeTrapKind;

use super::helpers::{extract_context, set_jit_trap};

const JIT_QUEUE_CLOSE_UNEXPECTED_ACTION: u64 = 1;
const JIT_QUEUE_SEND_UNEXPECTED_ACTION: u64 = 2;
const JIT_QUEUE_RECV_UNEXPECTED_RESULT: u64 = 3;

fn set_queue_trap(
    gc: &mut vo_runtime::gc::Gc,
    fiber: &mut crate::fiber::Fiber,
    kind: RuntimeTrapKind,
) -> JitResult {
    set_jit_trap(gc, fiber, kind, helpers::runtime_trap_message(kind))
}

/// Close a channel.
pub extern "C" fn jit_queue_close(ctx: *mut JitContext, chan: u64) -> JitResult {
    use crate::exec::{queue_close_core, QueueAction};

    let (vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;
    match queue_close_core(ch) {
        QueueAction::Continue => JitResult::Ok,
        QueueAction::Close {
            waiters,
            endpoint_id,
        } => {
            for waiter in &waiters {
                vm.state.wake_waiter(waiter, &mut vm.scheduler);
            }
            if let Some(endpoint_id) = endpoint_id {
                crate::vm::island_shared::finalize_closed_home_endpoint(vm, endpoint_id, None);
            }
            JitResult::Ok
        }
        QueueAction::RemoteClose {
            endpoint_id,
            home_island,
        } => {
            vm.state
                .send_endpoint_close_request(home_island, endpoint_id);
            vm.mark_gc_all_roots_dirty();
            vm.state.endpoint_registry.mark_tombstone(endpoint_id);
            JitResult::Ok
        }
        QueueAction::Trap(kind) => set_queue_trap(&mut vm.state.gc, fiber, kind),
        QueueAction::Malformed(_) => set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_QUEUE_CLOSE_UNEXPECTED_ACTION,
        ),
        // channel_close_core never produces these variants
        QueueAction::Block | QueueAction::ReplayThenBlock | QueueAction::Wake(_) => {
            set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_CLOSE_UNEXPECTED_ACTION,
            )
        }
        QueueAction::RemoteSend { .. }
        | QueueAction::RemoteRecv { .. }
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
    use crate::exec::{queue_send_core, QueueAction};

    let module = unsafe { &*((*ctx).module) };
    let (vm, fiber) = unsafe { extract_context(ctx) };
    if fiber.consume_remote_send_closed() {
        return set_jit_trap(
            &mut vm.state.gc,
            fiber,
            RuntimeTrapKind::SendOnClosedChannel,
            helpers::ERR_SEND_ON_CLOSED,
        );
    }
    let ch = chan as GcRef;
    let src: Vec<u64> = (0..val_slots as usize)
        .map(|i| unsafe { *val_ptr.add(i) })
        .collect();

    match queue_send_core(
        ch,
        &src,
        vm.state.current_island_id,
        fiber.id as u64,
        &mut vm.state,
        &module.struct_metas,
        &module.runtime_types,
        Some(module),
    ) {
        QueueAction::Wake(receiver) => {
            vm.state.wake_waiter(&receiver, &mut vm.scheduler);
            JitResult::Ok
        }
        QueueAction::Continue => JitResult::Ok,
        QueueAction::Block => JitResult::WaitQueue,
        QueueAction::RemoteSend {
            endpoint_id,
            home_island,
            data,
        } => {
            vm.state
                .send_endpoint_send_request(home_island, endpoint_id, data, fiber.id as u64);
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
            fiber_id,
            data,
        } => {
            vm.state
                .send_endpoint_recv_data_response(target_island, endpoint_id, data, fiber_id);
            JitResult::Ok
        }
        // channel_send_core never produces these variants
        QueueAction::ReplayThenBlock | QueueAction::Close { .. } => set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_QUEUE_SEND_UNEXPECTED_ACTION,
        ),
        QueueAction::RemoteRecv { .. } | QueueAction::RemoteClose { .. } => set_jit_infra_error(
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
    let (vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;
    let has_ok = has_ok != 0;

    if let Some(recv_response) = fiber.take_remote_recv_response() {
        crate::exec::replay_remote_queue_recv_response(
            &mut vm.state.gc,
            recv_response,
            elem_slots as usize,
            has_ok,
            &module.struct_metas,
            &module.runtime_types,
            &mut vm.state.endpoint_registry,
            |i, value| unsafe { *dst_ptr.add(i) = value },
        );
        vm.mark_gc_all_roots_dirty();
        return JitResult::Ok;
    }
    match complete_queue_recv(
        queue_recv_core(ch, vm.state.current_island_id, fiber.id as u64),
        elem_slots as usize,
        has_ok,
        |i, value| unsafe { *dst_ptr.add(i) = value },
    ) {
        Ok(Some(sender)) => {
            vm.state.wake_waiter(&sender, &mut vm.scheduler);
            JitResult::Ok
        }
        Ok(None) => JitResult::Ok,
        Err(QueueRecvCoreResult::WouldBlock) => JitResult::WaitQueue,
        Err(QueueRecvCoreResult::Remote {
            endpoint_id,
            home_island,
        }) => {
            vm.state
                .send_endpoint_recv_request(home_island, endpoint_id, fiber.id as u64);
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
mod tests {
    #[test]
    fn queue_jit_callbacks_return_jit_error_instead_of_unreachable_panics() {
        let src = include_str!("queue.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("queue callback source should contain production section");
        assert!(
            !src.contains("unreachable!("),
            "JIT queue callbacks must surface impossible queue states as JitError"
        );
    }
}
