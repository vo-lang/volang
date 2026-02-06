//! JIT callbacks for port operations.

use vo_runtime::jit_api::{JitContext, JitResult};

use crate::fiber::Fiber;
use crate::vm::{helpers, Vm};

use super::helpers::set_jit_panic;

/// JIT callback to close a port.
/// Returns JitResult::Ok on success, JitResult::Panic on nil port.
#[cfg(feature = "std")]
pub extern "C" fn jit_port_close(ctx: *mut JitContext, port: u64) -> JitResult {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::port;
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    let p = port as GcRef;
    
    if p.is_null() {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_CLOSE_NIL_CHANNEL);
    }
    
    // Port doesn't panic on double close
    if port::is_closed(p) {
        return JitResult::Ok;
    }
    
    port::close(p);
    
    // Wake all waiting fibers (remote waiters)
    let mut waiters = port::take_waiting_receivers(p);
    for (sender, _) in port::take_waiting_senders(p) {
        waiters.push(sender);
    }
    
    for waiter in &waiters {
        vm.state.wake_waiter(waiter, &mut vm.scheduler);
    }
    
    JitResult::Ok
}

#[cfg(not(feature = "std"))]
pub extern "C" fn jit_port_close(_ctx: *mut JitContext, _port: u64) -> JitResult {
    // Ports not supported in no_std
    JitResult::Ok
}

/// JIT callback to send to a port.
/// Returns JitResult (Ok, Panic, or WaitQueue).
#[cfg(feature = "std")]
pub extern "C" fn jit_port_send(
    ctx: *mut JitContext,
    port: u64,
    val_ptr: *const u64,
    val_slots: u32,
) -> JitResult {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::port;
    use crate::exec::{PortResult, port_send_core};
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    let module = unsafe { &*(ctx.module as *const vo_runtime::bytecode::Module) };
    let p = port as GcRef;
    
    if port::is_closed(p) {
        return set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_CLOSED);
    }
    
    // Read value slots from val_ptr
    let src: Vec<u64> = (0..val_slots as usize)
        .map(|i| unsafe { *val_ptr.add(i) })
        .collect();
    
    let island_id = vm.state.current_island_id;
    let fiber_id = fiber.id as u64;
    
    match port_send_core(p, &src, island_id, fiber_id, &vm.state.gc, &module.struct_metas, &module.runtime_types) {
        PortResult::WakeRemote(receiver) => {
            vm.state.wake_waiter(&receiver, &mut vm.scheduler);
            JitResult::Ok
        }
        PortResult::Continue => JitResult::Ok,
        PortResult::Yield => JitResult::WaitQueue,
        PortResult::SendOnClosed => set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_CLOSED),
        _ => JitResult::Ok,
    }
}

#[cfg(not(feature = "std"))]
pub extern "C" fn jit_port_send(ctx: *mut JitContext, _port: u64, _val_ptr: *const u64, _val_slots: u32) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let gc = unsafe { &mut *ctx.gc };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    set_jit_panic(gc, fiber, "runtime error: port not supported in no_std mode")
}

/// JIT callback to receive from a port.
/// Returns JitResult::Ok on success (including closed port),
/// or JitResult::WaitQueue if would block.
#[cfg(feature = "std")]
pub extern "C" fn jit_port_recv(
    ctx: *mut JitContext,
    port: u64,
    dst_ptr: *mut u64,
    elem_slots: u32,
    has_ok: u32,
) -> JitResult {
    use vo_runtime::gc::GcRef;
    use crate::exec::{port_recv_core, PortRecvCoreResult};
    
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    let module = unsafe { &*(ctx.module as *const vo_runtime::bytecode::Module) };
    let p = port as GcRef;
    let has_ok = has_ok != 0;
    
    let island_id = vm.state.current_island_id;
    let fiber_id = fiber.id as u64;
    
    match port_recv_core(p, elem_slots as usize, island_id, fiber_id, &mut vm.state.gc, &module.struct_metas, &module.runtime_types) {
        PortRecvCoreResult::Success { data, wake_sender } => {
            for i in 0..elem_slots as usize {
                unsafe { *dst_ptr.add(i) = data[i]; }
            }
            if has_ok {
                unsafe { *dst_ptr.add(elem_slots as usize) = 1; }
            }
            if let Some(sender) = wake_sender {
                vm.state.wake_waiter(&sender, &mut vm.scheduler);
            }
            JitResult::Ok
        }
        PortRecvCoreResult::WouldBlock => JitResult::WaitQueue,
        PortRecvCoreResult::Closed => {
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

#[cfg(not(feature = "std"))]
pub extern "C" fn jit_port_recv(ctx: *mut JitContext, _port: u64, _dst_ptr: *mut u64, _elem_slots: u32, _has_ok: u32) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let gc = unsafe { &mut *ctx.gc };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    set_jit_panic(gc, fiber, "runtime error: port not supported in no_std mode")
}
