//! JIT callbacks for channel operations.
//!
//! These callbacks are called from JIT-compiled code when channel operations
//! cannot be inlined. They handle the full channel protocol including blocking
//! and waking of fibers.

use vo_runtime::gc::GcRef;
use vo_runtime::jit_api::{JitContext, JitResult};

use crate::vm::helpers;
use crate::vm::RuntimeTrapKind;

use super::helpers::{extract_context, set_jit_panic};

/// Close a channel.
pub extern "C" fn jit_queue_close(ctx: *mut JitContext, chan: u64) -> JitResult {
    use crate::exec::{QueueAction, queue_close_core};

    let (vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;
    match queue_close_core(ch) {
        QueueAction::Continue => JitResult::Ok,
        QueueAction::Close { waiters, endpoint_id } => {
            for waiter in &waiters {
                vm.state.wake_waiter(waiter, &mut vm.scheduler);
            }
            if let Some(endpoint_id) = endpoint_id {
                crate::vm::island_shared::finalize_closed_home_endpoint(vm, endpoint_id, None);
            }
            JitResult::Ok
        }
        QueueAction::RemoteClose { endpoint_id, home_island } => {
            vm.state.send_endpoint_close_request(home_island, endpoint_id);
            vm.state.endpoint_registry.mark_tombstone(endpoint_id);
            JitResult::Ok
        }
        QueueAction::Trap(RuntimeTrapKind::CloseNilChannel) => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_CLOSE_NIL_CHANNEL)
        }
        QueueAction::Trap(RuntimeTrapKind::CloseClosedChannel) => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_CLOSE_CLOSED_CHANNEL)
        }
        QueueAction::Trap(_) => JitResult::Panic,
        // channel_close_core never produces these variants
        QueueAction::Block | QueueAction::ReplayThenBlock | QueueAction::Wake(_) => unreachable!("close"),
        QueueAction::RemoteSend { .. } | QueueAction::RemoteRecv { .. } | QueueAction::RemoteRecvData { .. } => unreachable!("close"),
    }
}

/// Send on a channel. Returns WaitQueue if would block.
pub extern "C" fn jit_queue_send(
    ctx: *mut JitContext,
    chan: u64,
    val_ptr: *const u64,
    val_slots: u32,
) -> JitResult {
    use crate::exec::{QueueAction, queue_send_core};

    let module = unsafe { &*((*ctx).module as *const vo_runtime::bytecode::Module) };
    let (vm, fiber) = unsafe { extract_context(ctx) };
    if fiber.consume_remote_send_closed() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_CLOSED);
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
        QueueAction::RemoteSend { endpoint_id, home_island, data } => {
            vm.state.send_endpoint_send_request(home_island, endpoint_id, data, fiber.id as u64);
            JitResult::WaitQueue
        }
        QueueAction::Trap(RuntimeTrapKind::SendOnNilChannel) => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_NIL)
        }
        QueueAction::Trap(RuntimeTrapKind::SendOnClosedChannel) => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_CLOSED)
        }
        QueueAction::Trap(_) => JitResult::Panic,
        QueueAction::RemoteRecvData { endpoint_id, target_island, fiber_id, data } => {
            vm.state.send_endpoint_recv_data_response(target_island, endpoint_id, data, fiber_id);
            JitResult::Ok
        }
        // channel_send_core never produces these variants
        QueueAction::ReplayThenBlock | QueueAction::Close { .. } => unreachable!("send"),
        QueueAction::RemoteRecv { .. } | QueueAction::RemoteClose { .. } => unreachable!("send"),
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
    use crate::exec::{QueueRecvCoreResult, complete_queue_recv, queue_recv_core};

    let module = unsafe { &*((*ctx).module as *const vo_runtime::bytecode::Module) };
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
        Err(QueueRecvCoreResult::Remote { endpoint_id, home_island }) => {
            vm.state.send_endpoint_recv_request(home_island, endpoint_id, fiber.id as u64);
            JitResult::WaitQueue
        }
        Err(QueueRecvCoreResult::Trap(RuntimeTrapKind::RecvOnNilChannel)) => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_RECV_ON_NIL)
        }
        Err(QueueRecvCoreResult::Trap(_)) => JitResult::Panic,
        Err(QueueRecvCoreResult::Success { .. } | QueueRecvCoreResult::Closed) => {
            unreachable!("complete_queue_recv returned terminal recv result as Err")
        }
    }
}
