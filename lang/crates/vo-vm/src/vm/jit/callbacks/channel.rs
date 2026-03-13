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
pub extern "C" fn jit_chan_close(ctx: *mut JitContext, chan: u64) -> JitResult {
    use crate::exec::{QueueAction, channel_close_core};

    let (vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;
    match channel_close_core(ch) {
        QueueAction::Continue => JitResult::Ok,
        QueueAction::Close { waiters, endpoint_id } => {
            for waiter in &waiters {
                vm.state.wake_waiter(waiter, &mut vm.scheduler);
            }
            #[cfg(feature = "std")]
            if let Some(endpoint_id) = endpoint_id {
                crate::vm::island_thread::finalize_closed_home_endpoint(vm, endpoint_id, None);
            }
            JitResult::Ok
        }
        QueueAction::RemoteClose { endpoint_id, home_island } => {
            #[cfg(feature = "std")]
            {
                vm.state.send_chan_close_request(home_island, endpoint_id);
                vm.state.endpoint_registry.mark_tombstone(endpoint_id);
            }
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
        #[cfg(feature = "std")]
        QueueAction::RemoteSend { .. } | QueueAction::RemoteRecv { .. } | QueueAction::RemoteRecvData { .. } => unreachable!("close"),
    }
}

/// Send on a channel. Returns WaitQueue if would block.
pub extern "C" fn jit_chan_send(
    ctx: *mut JitContext,
    chan: u64,
    val_ptr: *const u64,
    val_slots: u32,
) -> JitResult {
    use crate::exec::{QueueAction, channel_send_core};

    let module = unsafe { &*((*ctx).module as *const vo_runtime::bytecode::Module) };
    let (vm, fiber) = unsafe { extract_context(ctx) };
    #[cfg(feature = "std")]
    if fiber.consume_remote_send_closed() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_CLOSED);
    }
    let ch = chan as GcRef;
    let src: Vec<u64> = (0..val_slots as usize)
        .map(|i| unsafe { *val_ptr.add(i) })
        .collect();

    // Prepare nested channels in the send value for cross-island transfer.
    #[cfg(feature = "std")]
    crate::exec::prepare_remote_send_value_if_needed(
        ch,
        &src,
        &module.struct_metas,
        &module.runtime_types,
        &mut vm.state,
    );

    match channel_send_core(
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
            #[cfg(feature = "std")]
            {
                vm.state.send_chan_send_request(home_island, endpoint_id, data, fiber.id as u64);
            }
            JitResult::WaitQueue
        }
        QueueAction::Trap(RuntimeTrapKind::SendOnNilChannel) => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_NIL)
        }
        QueueAction::Trap(RuntimeTrapKind::SendOnClosedChannel) => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_CLOSED)
        }
        QueueAction::Trap(_) => JitResult::Panic,
        #[cfg(feature = "std")]
        QueueAction::RemoteRecvData { endpoint_id, target_island, fiber_id, data } => {
            vm.state.send_chan_recv_data_response(target_island, endpoint_id, data, fiber_id);
            JitResult::Ok
        }
        // channel_send_core never produces these variants
        QueueAction::ReplayThenBlock | QueueAction::Close { .. } => unreachable!("send"),
        #[cfg(feature = "std")]
        QueueAction::RemoteRecv { .. } | QueueAction::RemoteClose { .. } => unreachable!("send"),
    }
}

/// Receive from a channel. Returns WaitQueue if would block.
/// Writes received value to dst_ptr. If has_ok, writes ok flag after value.
pub extern "C" fn jit_chan_recv(
    ctx: *mut JitContext,
    chan: u64,
    dst_ptr: *mut u64,
    elem_slots: u32,
    has_ok: u32,
) -> JitResult {
    use crate::exec::{ChannelRecvCoreResult, channel_recv_core};

    let module = unsafe { &*((*ctx).module as *const vo_runtime::bytecode::Module) };
    let (vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;
    let has_ok = has_ok != 0;
    #[cfg(feature = "std")]
    if let Some(recv_response) = fiber.take_remote_recv_response() {
        crate::exec::replay_remote_recv_response(
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
    match channel_recv_core(ch, vm.state.current_island_id, fiber.id as u64) {
        ChannelRecvCoreResult::Success { data, wake_sender } => {
            crate::exec::write_recv_result(Some(data.as_ref()), elem_slots as usize, has_ok, |i, value| unsafe {
                *dst_ptr.add(i) = value
            });
            if let Some(sender) = wake_sender {
                vm.state.wake_waiter(&sender, &mut vm.scheduler);
            }
            JitResult::Ok
        }
        ChannelRecvCoreResult::WouldBlock => JitResult::WaitQueue,
        ChannelRecvCoreResult::Remote { endpoint_id, home_island } => {
            #[cfg(feature = "std")]
            {
                vm.state.send_chan_recv_request(home_island, endpoint_id, fiber.id as u64);
            }
            JitResult::WaitQueue
        }
        ChannelRecvCoreResult::Closed => {
            crate::exec::write_recv_result(None, elem_slots as usize, has_ok, |i, value| unsafe {
                *dst_ptr.add(i) = value
            });
            JitResult::Ok
        }
        ChannelRecvCoreResult::Trap(RuntimeTrapKind::RecvOnNilChannel) => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_RECV_ON_NIL)
        }
        ChannelRecvCoreResult::Trap(_) => JitResult::Panic,
    }
}
