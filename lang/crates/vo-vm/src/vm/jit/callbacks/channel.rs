//! JIT callbacks for channel operations.
//!
//! These callbacks are called from JIT-compiled code when channel operations
//! cannot be inlined. They handle the full channel protocol including blocking
//! and waking of fibers.

use vo_runtime::gc::GcRef;
use vo_runtime::jit_api::{JitContext, JitResult};
use vo_runtime::objects::channel::{self, RecvResult, SendResult};
use vo_runtime::objects::queue_state::{self, ChannelWaiter};

use crate::vm::helpers;

use super::helpers::{extract_context, set_jit_panic};

/// Close a channel.
pub extern "C" fn jit_chan_close(ctx: *mut JitContext, chan: u64) -> JitResult {
    let (vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;

    if ch.is_null() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_CLOSE_NIL_CHANNEL);
    }

    let state = channel::get_state(ch);
    if state.is_closed() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_CLOSE_CLOSED_CHANNEL);
    }

    state.close();

    // Wake all waiting fibers - they need to retry to see closed state
    for waiter in state.take_waiting_receivers() {
        vm.scheduler.wake_channel_waiter(&waiter);
    }
    for (waiter, _) in state.take_waiting_senders() {
        vm.scheduler.wake_channel_waiter(&waiter);
    }

    JitResult::Ok
}

/// Send on a channel. Returns WaitQueue if would block.
pub extern "C" fn jit_chan_send(
    ctx: *mut JitContext,
    chan: u64,
    val_ptr: *const u64,
    val_slots: u32,
) -> JitResult {
    let (vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;

    if ch.is_null() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_NIL);
    }

    let value: Box<[u64]> = (0..val_slots as usize)
        .map(|i| unsafe { *val_ptr.add(i) })
        .collect();

    // Write barrier: type-aware to avoid UB on mixed-slot types.
    let em = queue_state::elem_meta(ch);
    if em.value_kind().may_contain_gc_refs() {
        let module = vm.module.as_ref();
        vo_runtime::gc_types::typed_write_barrier_by_meta(&mut vm.state.gc, ch, &value, em, module);
    }

    let cap = queue_state::capacity(ch);
    let state = channel::get_state(ch);

    match state.try_send(value, cap) {
        SendResult::DirectSend(receiver) => {
            vm.scheduler.wake_channel_waiter(&receiver);
            JitResult::Ok
        }
        SendResult::Buffered => JitResult::Ok,
        SendResult::WouldBlock(value) => {
            state.register_sender(ChannelWaiter::Simple(fiber.id as u64), value);
            JitResult::WaitQueue
        }
        SendResult::Blocked => unreachable!("try_send never returns Blocked"),
        SendResult::Closed => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_CLOSED)
        }
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
    let (vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;
    let has_ok = has_ok != 0;

    if ch.is_null() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_RECV_ON_NIL);
    }

    let state = channel::get_state(ch);
    let (result, value) = state.try_recv();

    match result {
        RecvResult::Success(woke_sender) => {
            // Write value
            if let Some(val) = value {
                for (i, &v) in val.iter().enumerate().take(elem_slots as usize) {
                    unsafe { *dst_ptr.add(i) = v; }
                }
            }
            if has_ok {
                unsafe { *dst_ptr.add(elem_slots as usize) = 1; }
            }
            if let Some(sender) = woke_sender {
                vm.scheduler.wake_channel_waiter(&sender);
            }
            JitResult::Ok
        }
        RecvResult::WouldBlock => {
            state.register_receiver(ChannelWaiter::Simple(fiber.id as u64));
            JitResult::WaitQueue
        }
        RecvResult::Blocked => unreachable!("try_recv never returns Blocked"),
        RecvResult::Closed => {
            // Zero value, ok=false
            for i in 0..elem_slots as usize {
                unsafe { *dst_ptr.add(i) = 0; }
            }
            if has_ok {
                unsafe { *dst_ptr.add(elem_slots as usize) = 0; }
            }
            JitResult::Ok
        }
    }
}
