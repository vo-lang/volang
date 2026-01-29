//! Port instructions: PortNew, PortSend, PortRecv, PortClose

#[cfg(not(feature = "std"))]
use alloc::{format, string::String};

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::port::{self, RecvResult, SendResult, WaiterInfo};
#[cfg(feature = "std")]
use vo_runtime::objects::queue_state;
#[cfg(feature = "std")]
use vo_runtime::pack::{pack_slots, unpack_slots};
use vo_runtime::ValueMeta;
#[cfg(feature = "std")]
use vo_common_core::bytecode::StructMeta;
#[cfg(feature = "std")]
use vo_common_core::RuntimeType;

use crate::instruction::Instruction;

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
}

pub type PortNewResult = Result<(), String>;

#[inline]
pub fn exec_port_new(
    stack: &mut [u64],
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
) -> PortNewResult {
    let meta_raw = stack[bp + inst.b as usize] as u32;
    let elem_meta = ValueMeta::from_raw(meta_raw);

    let cap_i64 = stack[bp + inst.c as usize] as i64;
    if cap_i64 < 0 {
        return Err(format!("runtime error: make port: size out of range"));
    }

    let cap = cap_i64 as usize;
    let elem_slots = inst.flags as u16;
    let p = port::create(gc, elem_meta, elem_slots, cap);
    stack[bp + inst.a as usize] = p as u64;
    Ok(())
}

#[cfg(feature = "std")]
pub fn exec_port_send(
    stack: &[u64],
    bp: usize,
    island_id: u32,
    fiber_id: u64,
    inst: &Instruction,
    gc: &Gc,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> PortResult {
    let p = stack[bp + inst.a as usize] as GcRef;
    let elem_slots = inst.flags as usize;
    let src_start = bp + inst.b as usize;

    let elem_meta = queue_state::elem_meta(p);
    let src = &stack[src_start..src_start + elem_slots];

    if elem_slots != 0 && elem_meta.value_kind() == vo_runtime::ValueKind::Void {
        panic!(
            "port send: elem_slots={} but elem_meta is Void (raw={})",
            elem_slots,
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

#[cfg(not(feature = "std"))]
pub fn exec_port_send(
    _stack: &[u64],
    _bp: usize,
    _island_id: u32,
    _fiber_id: u64,
    _inst: &Instruction,
    _gc: &Gc,
) -> PortResult {
    panic!("Port not supported in no_std mode")
}

#[cfg(feature = "std")]
pub fn exec_port_recv(
    stack: &mut [u64],
    bp: usize,
    island_id: u32,
    fiber_id: u64,
    inst: &Instruction,
    gc: &mut Gc,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> PortResult {
    let p = stack[bp + inst.b as usize] as GcRef;
    let elem_slots = ((inst.flags >> 1) & 0x7F) as usize;
    let has_ok = (inst.flags & 1) != 0;
    let dst_start = bp + inst.a as usize;

    let (result, packed_opt) = port::try_recv(p);

    match result {
        RecvResult::Success(woke_sender) => {
            if packed_opt.is_none() {
                panic!("port recv: success without payload");
            }
            if let Some(packed) = packed_opt {
                if elem_slots != 0 && packed.data().first().copied() == Some(vo_runtime::ValueKind::Void as u8) {
                    let elem_meta = queue_state::elem_meta(p);
                    panic!(
                        "port recv: elem_slots={} but packed tag is Void (port elem_meta raw={})",
                        elem_slots,
                        elem_meta.to_raw()
                    );
                }
                // Unpack the value into destination island's heap
                let dst = &mut stack[dst_start..dst_start + elem_slots];
                unpack_slots(gc, &packed, dst, struct_metas, runtime_types);
            }
            if has_ok {
                stack[dst_start + elem_slots] = 1; // ok = true
            }
            match woke_sender {
                Some(sender) => PortResult::WakeRemote(sender),
                None => PortResult::Continue,
            }
        }
        RecvResult::WouldBlock => {
            let waiter = WaiterInfo { island_id, fiber_id };
            port::register_receiver(p, waiter);
            PortResult::Yield
        }
        RecvResult::Closed => {
            // Zero out destination slots
            for i in 0..elem_slots {
                stack[dst_start + i] = 0;
            }
            if has_ok {
                stack[dst_start + elem_slots] = 0; // ok = false
            }
            PortResult::Continue
        }
    }
}

#[cfg(not(feature = "std"))]
pub fn exec_port_recv(
    _stack: &mut [u64],
    _bp: usize,
    _island_id: u32,
    _fiber_id: u64,
    _inst: &Instruction,
    _gc: &mut Gc,
) -> PortResult {
    panic!("Port not supported in no_std mode")
}

pub fn exec_port_close(stack: &[u64], bp: usize, inst: &Instruction) -> PortResult {
    let p = stack[bp + inst.a as usize] as GcRef;

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
