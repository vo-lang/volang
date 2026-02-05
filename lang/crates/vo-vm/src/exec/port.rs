//! Port instructions: PortNew, PortSend, PortRecv, PortClose

#[cfg(not(feature = "std"))]
use alloc::{format, string::String};

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::port::{self, RecvResult, SendResult, WaiterInfo};
#[cfg(feature = "std")]
use vo_runtime::objects::queue_state;
#[cfg(feature = "std")]
use vo_runtime::pack::{pack_slots, unpack_slots};
use vo_runtime::slot::Slot;
use vo_runtime::ValueMeta;
#[cfg(feature = "std")]
use vo_common_core::bytecode::StructMeta;
#[cfg(feature = "std")]
use vo_common_core::RuntimeType;

use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

pub enum PortResult {
    Continue,
    Yield,
    /// Wake fiber on another island
    WakeRemote(WaiterInfo),
    /// Send on closed port - panic
    SendOnClosed,
    /// Close nil port - panic
    CloseNil,
    /// Port closed - wake these waiters
    #[cfg(feature = "std")]
    Closed(Vec<WaiterInfo>),
    /// Port not supported (no_std)
    #[cfg(not(feature = "std"))]
    NotSupported,
}

pub type PortNewResult = Result<(), String>;

#[inline]
pub fn exec_port_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
) -> PortNewResult {
    let meta_raw = stack_get(stack, bp + inst.b as usize) as u32;
    let elem_meta = ValueMeta::from_raw(meta_raw);
    let cap = stack_get(stack, bp + inst.c as usize) as i64;
    let elem_slots = inst.flags as u16;

    // Use unified validation logic from port::create_checked
    match port::create_checked(gc, elem_meta, elem_slots, cap) {
        Ok(p) => {
            stack_set(stack, bp + inst.a as usize, p as u64);
            Ok(())
        }
        Err(_) => Err(format!("runtime error: makeport: size out of range")),
    }
}

/// Core port send logic - shared by VM interpreter and JIT callbacks.
#[cfg(feature = "std")]
pub fn port_send_core(
    p: GcRef,
    src: &[u64],
    island_id: u32,
    fiber_id: u64,
    gc: &Gc,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> PortResult {
    let elem_meta = queue_state::elem_meta(p);
    
    if !src.is_empty() && elem_meta.value_kind() == vo_runtime::ValueKind::Void {
        panic!(
            "port send: elem_slots={} but elem_meta is Void (raw={})",
            src.len(),
            elem_meta.to_raw()
        );
    }

    // Pack the value for cross-island transfer
    let packed = pack_slots(gc, src, elem_meta, struct_metas, runtime_types);
    match port::try_send(p, packed) {
        SendResult::DirectSend(receiver) => PortResult::WakeRemote(receiver),
        SendResult::Buffered => PortResult::Continue,
        SendResult::WouldBlock(value) => {
            let waiter = WaiterInfo { island_id, fiber_id };
            port::register_sender(p, waiter, value);
            PortResult::Yield
        }
        SendResult::Closed => PortResult::SendOnClosed,
    }
}

#[cfg(feature = "std")]
pub fn exec_port_send(
    stack: *const Slot,
    bp: usize,
    island_id: u32,
    fiber_id: u64,
    inst: &Instruction,
    gc: &Gc,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> PortResult {
    let p = stack_get(stack, bp + inst.a as usize) as GcRef;
    let elem_slots = inst.flags as usize;
    let src_start = bp + inst.b as usize;
    let src: Vec<u64> = (0..elem_slots).map(|i| stack_get(stack, src_start + i)).collect();
    
    port_send_core(p, &src, island_id, fiber_id, gc, struct_metas, runtime_types)
}

#[cfg(not(feature = "std"))]
pub fn exec_port_send(
    _stack: *const Slot,
    _bp: usize,
    _island_id: u32,
    _fiber_id: u64,
    _inst: &Instruction,
    _gc: &Gc,
) -> PortResult {
    PortResult::NotSupported
}

/// Port recv result for core function.
#[cfg(feature = "std")]
pub enum PortRecvCoreResult {
    /// Success with unpacked data and optional sender to wake
    Success { data: Vec<u64>, wake_sender: Option<WaiterInfo> },
    /// Would block - waiter registered
    WouldBlock,
    /// Port closed
    Closed,
}

/// Core port recv logic - shared by VM interpreter and JIT callbacks.
#[cfg(feature = "std")]
pub fn port_recv_core(
    p: GcRef,
    elem_slots: usize,
    island_id: u32,
    fiber_id: u64,
    gc: &mut Gc,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> PortRecvCoreResult {
    let (result, packed_opt) = port::try_recv(p);

    match result {
        RecvResult::Success(woke_sender) => {
            if packed_opt.is_none() {
                panic!("port recv: success without payload");
            }
            let mut dst: Vec<u64> = vec![0; elem_slots];
            if let Some(packed) = packed_opt {
                if elem_slots != 0 && packed.data().first().copied() == Some(vo_runtime::ValueKind::Void as u8) {
                    let elem_meta = queue_state::elem_meta(p);
                    panic!(
                        "port recv: elem_slots={} but packed tag is Void (port elem_meta raw={})",
                        elem_slots,
                        elem_meta.to_raw()
                    );
                }
                unpack_slots(gc, &packed, &mut dst, struct_metas, runtime_types);
            }
            PortRecvCoreResult::Success { data: dst, wake_sender: woke_sender }
        }
        RecvResult::WouldBlock => {
            let waiter = WaiterInfo { island_id, fiber_id };
            port::register_receiver(p, waiter);
            PortRecvCoreResult::WouldBlock
        }
        RecvResult::Closed => PortRecvCoreResult::Closed,
    }
}

#[cfg(feature = "std")]
pub fn exec_port_recv(
    stack: *mut Slot,
    bp: usize,
    island_id: u32,
    fiber_id: u64,
    inst: &Instruction,
    gc: &mut Gc,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> PortResult {
    let p = stack_get(stack, bp + inst.b as usize) as GcRef;
    let elem_slots = ((inst.flags >> 1) & 0x7F) as usize;
    let has_ok = (inst.flags & 1) != 0;
    let dst_start = bp + inst.a as usize;

    match port_recv_core(p, elem_slots, island_id, fiber_id, gc, struct_metas, runtime_types) {
        PortRecvCoreResult::Success { data, wake_sender } => {
            for i in 0..elem_slots {
                stack_set(stack, dst_start + i, data[i]);
            }
            if has_ok {
                stack_set(stack, dst_start + elem_slots, 1);
            }
            match wake_sender {
                Some(sender) => PortResult::WakeRemote(sender),
                None => PortResult::Continue,
            }
        }
        PortRecvCoreResult::WouldBlock => PortResult::Yield,
        PortRecvCoreResult::Closed => {
            for i in 0..elem_slots {
                stack_set(stack, dst_start + i, 0);
            }
            if has_ok {
                stack_set(stack, dst_start + elem_slots, 0);
            }
            PortResult::Continue
        }
    }
}

#[cfg(not(feature = "std"))]
pub fn exec_port_recv(
    _stack: *mut Slot,
    _bp: usize,
    _island_id: u32,
    _fiber_id: u64,
    _inst: &Instruction,
    _gc: &mut Gc,
) -> PortResult {
    PortResult::NotSupported
}

pub fn exec_port_close(stack: *const Slot, bp: usize, inst: &Instruction) -> PortResult {
    let p = stack_get(stack, bp + inst.a as usize) as GcRef;

    if p.is_null() {
        return PortResult::CloseNil;
    }

    if port::is_closed(p) {
        // Already closed - just return, don't panic like channel
        return PortResult::Continue;
    }

    port::close(p);

    // Collect all waiters that need to be woken
    #[cfg(feature = "std")]
    {
        let mut waiters = port::take_waiting_receivers(p);
        for (sender, _) in port::take_waiting_senders(p) {
            waiters.push(sender);
        }
        
        if waiters.is_empty() {
            PortResult::Continue
        } else {
            PortResult::Closed(waiters)
        }
    }
    
    #[cfg(not(feature = "std"))]
    PortResult::Continue
}
