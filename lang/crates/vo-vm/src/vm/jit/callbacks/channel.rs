//! JIT callbacks for channel operations.

use vo_runtime::jit_api::{JitContext, JitResult};

use crate::fiber::Fiber;
use crate::vm::{helpers, Vm};

use super::helpers::set_jit_panic;

/// JIT callback to close a channel.
/// Returns JitResult::Ok on success, JitResult::Panic on nil/closed channel.
pub extern "C" fn jit_chan_close(ctx: *mut JitContext, chan: u64) -> JitResult {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::channel;
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    let ch = chan as GcRef;
    
    if ch.is_null() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_CLOSE_NIL_CHANNEL);
    }
    
    let state = channel::get_state(ch);
    if state.is_closed() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_CLOSE_CLOSED_CHANNEL);
    }
    
    state.close();
    
    // Wake all waiting fibers
    let mut wake_ids: Vec<u32> = state.take_waiting_receivers().into_iter().map(|id| id as u32).collect();
    wake_ids.extend(state.take_waiting_senders().into_iter().map(|(id, _)| id as u32));
    
    for id in wake_ids {
        let fiber_id = crate::scheduler::FiberId::from_raw(id);
        vm.scheduler.wake_fiber(fiber_id);
    }
    
    JitResult::Ok
}

/// JIT callback to send on a channel.
/// Returns JitResult::Ok on success, JitResult::Panic on nil/closed channel,
/// or JitResult::WaitIo if the send would block.
pub extern "C" fn jit_chan_send(
    ctx: *mut JitContext,
    chan: u64,
    val_ptr: *const u64,
    val_slots: u32,
) -> JitResult {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::channel::{self, SendResult};
    use vo_runtime::objects::queue_state;
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    let ch = chan as GcRef;
    
    if ch.is_null() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_NIL);
    }
    
    // Read value slots from val_ptr
    let value: Box<[u64]> = (0..val_slots as usize)
        .map(|i| unsafe { *val_ptr.add(i) })
        .collect();
    
    let cap = queue_state::capacity(ch);
    let state = channel::get_state(ch);
    
    match state.try_send(value, cap) {
        SendResult::DirectSend(receiver_id) => {
            // Wake the receiver
            let fiber_id = crate::scheduler::FiberId::from_raw(receiver_id as u32);
            vm.scheduler.wake_fiber(fiber_id);
            JitResult::Ok
        }
        SendResult::Buffered => JitResult::Ok,
        SendResult::WouldBlock(value) => {
            // Register sender and block
            let fiber_id = fiber.id as u64;
            state.register_sender(fiber_id, value);
            JitResult::WaitIo
        }
        SendResult::Closed => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_CLOSED)
        }
    }
}

/// JIT callback to receive from a channel.
/// Returns JitResult::Ok on success (including closed channel case),
/// JitResult::Panic on nil channel, or JitResult::WaitIo if would block.
/// 
/// dst_ptr points to where the received value should be written.
/// If has_ok is true, writes 1/0 to dst_ptr[elem_slots] indicating success.
pub extern "C" fn jit_chan_recv(
    ctx: *mut JitContext,
    chan: u64,
    dst_ptr: *mut u64,
    elem_slots: u32,
    has_ok: u32,
) -> JitResult {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::channel::{self, RecvResult};
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    let ch = chan as GcRef;
    let has_ok = has_ok != 0;
    
    if ch.is_null() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_RECV_ON_NIL);
    }
    
    let state = channel::get_state(ch);
    let (result, value) = state.try_recv();
    
    match result {
        RecvResult::Success(woke_sender) => {
            // Write received value to dst_ptr
            if let Some(val) = value {
                for (i, &v) in val.iter().enumerate() {
                    if i < elem_slots as usize {
                        unsafe { *dst_ptr.add(i) = v; }
                    }
                }
            }
            if has_ok {
                unsafe { *dst_ptr.add(elem_slots as usize) = 1; }
            }
            // Wake sender if any
            if let Some(id) = woke_sender {
                let fiber_id = crate::scheduler::FiberId::from_raw(id as u32);
                vm.scheduler.wake_fiber(fiber_id);
            }
            JitResult::Ok
        }
        RecvResult::WouldBlock => {
            // Register receiver and block
            let fiber_id = fiber.id as u64;
            state.register_receiver(fiber_id);
            JitResult::WaitIo
        }
        RecvResult::Closed => {
            // Zero out the destination and set ok=false
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
