//! Island message encoding/decoding - single source of truth for cross-island protocol.
//!
//! This module defines the binary format for spawning fibers across islands.
//! Uses pack_slots/unpack_slots for proper deep copying of all sendable types.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::{Gc, GcRef};
use crate::objects::port;
use crate::pack::{pack_slots, unpack_slots, PackedValue};
use crate::slot::Slot;
use crate::ValueKind;
use crate::ValueMeta;
use vo_common_core::bytecode::StructMeta;
use vo_common_core::RuntimeType;

/// Read little-endian integers from byte slice at offset.
#[inline]
fn read_u16(data: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes(data[offset..offset + 2].try_into().unwrap())
}

#[inline]
fn read_u32(data: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes(data[offset..offset + 4].try_into().unwrap())
}

#[inline]
fn read_u64(data: &[u8], offset: usize) -> u64 {
    u64::from_le_bytes(data[offset..offset + 8].try_into().unwrap())
}

/// Port wire format for cross-island transfer.
/// Contains all info needed to recreate a port handle on another island.
#[derive(Debug, Clone)]
pub struct PortWire {
    /// Index in capture or arg array
    pub idx: u16,
    /// Whether port is inside a box (escaped variable)
    pub is_boxed: bool,
    /// Whether this is an arg (true) or capture (false)
    pub is_arg: bool,
    /// Raw pointer to shared PortState (Arc)
    pub state_ptr: u64,
    /// Port capacity
    pub cap: u64,
    /// Element metadata (raw u32)
    pub meta_raw: u32,
    /// Element slot count
    pub elem_slots: u16,
}

impl PortWire {
    pub const WIRE_SIZE: usize = 26; // 2 + 1 + 1 + 8 + 8 + 4 + 2

    pub fn encode(&self, buf: &mut Vec<u8>) {
        buf.extend_from_slice(&self.idx.to_le_bytes());
        buf.push(self.is_boxed as u8);
        buf.push(self.is_arg as u8);
        buf.extend_from_slice(&self.state_ptr.to_le_bytes());
        buf.extend_from_slice(&self.cap.to_le_bytes());
        buf.extend_from_slice(&self.meta_raw.to_le_bytes());
        buf.extend_from_slice(&self.elem_slots.to_le_bytes());
    }

    pub fn decode(data: &[u8], offset: &mut usize) -> Self {
        let idx = read_u16(data, *offset);
        let is_boxed = data[*offset + 2] != 0;
        let is_arg = data[*offset + 3] != 0;
        let state_ptr = read_u64(data, *offset + 4);
        let cap = read_u64(data, *offset + 12);
        let meta_raw = read_u32(data, *offset + 20);
        let elem_slots = read_u16(data, *offset + 24);
        *offset += Self::WIRE_SIZE;

        Self { idx, is_boxed, is_arg, state_ptr, cap, meta_raw, elem_slots }
    }
}

/// Header size for spawn fiber message.
/// Format: func_id(4) + num_captures(2) + num_args(2) + num_ports(2)
pub const HEADER_SIZE: usize = 10;

/// Encode spawn fiber payload with proper type-aware serialization.
///
/// Format:
/// - Header: func_id(4) + num_captures(2) + num_args(2) + num_ports(2)
/// - Port infos: num_ports × PortWire (26 bytes each)
/// - Captures: packed data for each capture (length-prefixed)
/// - Args: packed data for each arg (length-prefixed)
///
/// Captures are GcRefs to boxed variables. We serialize the boxed content.
/// Args are direct values. We serialize them using param_types.
pub fn encode_spawn_payload(
    gc: &Gc,
    func_id: u32,
    captures: &[Slot],
    capture_types: &[(u32, u16)], // (ValueMeta raw, slots) from FunctionDef
    args: &[Slot],
    param_types: &[(u32, u16)], // (ValueMeta raw, slots) from FunctionDef
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> Vec<u8> {
    let mut buf = Vec::new();

    // Detect ports in captures and args using type info for safe detection
    let mut port_wires = Vec::new();
    
    // For captures, use capture_types to know if the boxed value is a port
    for (i, &slot) in captures.iter().enumerate() {
        if slot == 0 || i >= capture_types.len() {
            continue;
        }
        let (meta_raw, _) = capture_types[i];
        let vk = ValueMeta::from_raw(meta_raw).value_kind();
        if let Some(wire) = detect_port_typed(slot, i as u16, false, vk) {
            port_wires.push(wire);
        }
    }
    
    // For args, use param_types to determine which slots could be ports
    let mut arg_slot_idx = 0usize;
    for (param_idx, &(meta_raw, slots)) in param_types.iter().enumerate() {
        let vk = ValueMeta::from_raw(meta_raw).value_kind();
        if arg_slot_idx < args.len() {
            if let Some(wire) = detect_port_typed(args[arg_slot_idx], param_idx as u16, true, vk) {
                port_wires.push(wire);
            }
        }
        arg_slot_idx += slots as usize;
    }

    // Header: func_id(4) + num_captures(2) + num_args(2) + num_ports(2)
    buf.extend_from_slice(&func_id.to_le_bytes());
    buf.extend_from_slice(&(captures.len() as u16).to_le_bytes());
    buf.extend_from_slice(&(param_types.len() as u16).to_le_bytes()); // actual param count
    buf.extend_from_slice(&(port_wires.len() as u16).to_le_bytes());

    // Port infos
    for wire in &port_wires {
        wire.encode(&mut buf);
    }

    // Pack captures (each capture is a GcRef to a boxed variable)
    for (i, &slot) in captures.iter().enumerate() {
        if slot == 0 {
            // Null capture - write empty packed value
            buf.extend_from_slice(&0u32.to_le_bytes()); // length = 0
            continue;
        }
        
        let gcref = slot as GcRef;
        let header = Gc::header(gcref);
        
        // Check if this is a port (handled separately)
        if header.value_kind() == ValueKind::Port {
            buf.extend_from_slice(&0u32.to_le_bytes());
            continue;
        }
        
        // Get the type info for this capture
        if i < capture_types.len() {
            let (meta_raw, _expected_slots) = capture_types[i];
            let value_meta = ValueMeta::from_raw(meta_raw);
            let actual_slots = header.slots as usize;
            
            // Read the boxed value content
            let mut value_slots = vec![0u64; actual_slots];
            for j in 0..actual_slots {
                value_slots[j] = unsafe { Gc::read_slot(gcref, j) };
            }
            
            // Pack the value
            let packed = pack_slots(gc, &value_slots, value_meta, struct_metas, runtime_types);
            let packed_data = packed.data();
            buf.extend_from_slice(&(packed_data.len() as u32).to_le_bytes());
            buf.extend_from_slice(packed_data);
        }
    }

    // Pack args
    let mut arg_offset = 0usize;
    for &(meta_raw, slots) in param_types {
        let value_meta = ValueMeta::from_raw(meta_raw);
        let vk = value_meta.value_kind();
        
        // Check if this arg is a port (only safe to call is_port_slot on GcRef types)
        if vk == ValueKind::Port && arg_offset < args.len() {
            buf.extend_from_slice(&0u32.to_le_bytes()); // length = 0, port handled separately
            arg_offset += slots as usize;
            continue;
        }
        
        let slots_usize = slots as usize;
        
        if arg_offset + slots_usize <= args.len() {
            let value_slots = &args[arg_offset..arg_offset + slots_usize];
            let packed = pack_slots(gc, value_slots, value_meta, struct_metas, runtime_types);
            let packed_data = packed.data();
            buf.extend_from_slice(&(packed_data.len() as u32).to_le_bytes());
            buf.extend_from_slice(packed_data);
        } else {
            buf.extend_from_slice(&0u32.to_le_bytes());
        }
        arg_offset += slots_usize;
    }

    buf
}

/// Type-aware port detection. Uses known ValueKind to safely detect ports.
fn detect_port_typed(slot: Slot, idx: u16, is_arg: bool, value_kind: ValueKind) -> Option<PortWire> {
    if slot == 0 || value_kind != ValueKind::Port {
        return None;
    }

    let gcref = slot as GcRef;
    let header = Gc::header(gcref);
    
    // Determine if direct port or boxed port, get the actual port GcRef
    let (port_ref, is_boxed) = if header.value_kind() == ValueKind::Port {
        (gcref, false)
    } else if header.value_kind() == ValueKind::Struct && header.slots == 1 {
        let inner = unsafe { Gc::read_slot(gcref, 0) };
        if inner == 0 { return None; }
        (inner as GcRef, true)
    } else {
        return None;
    };

    let (cap, elem_meta, elem_slots) = port::get_metadata(port_ref);
    let state_ptr = port::get_state_ptr(port_ref);
    Some(PortWire {
        idx, is_boxed, is_arg, state_ptr, cap,
        meta_raw: elem_meta.to_raw(),
        elem_slots,
    })
}

/// Decoded spawn fiber payload.
pub struct SpawnPayload {
    pub func_id: u32,
    pub num_captures: u16,
    pub num_args: u16,
    pub port_wires: Vec<PortWire>,
    pub data_offset: usize, // offset to packed capture/arg data
}

/// Decode spawn fiber header and port infos.
pub fn decode_spawn_header(data: &[u8]) -> SpawnPayload {
    assert!(data.len() >= HEADER_SIZE, "island spawn: invalid data length");

    let func_id = read_u32(data, 0);
    let num_captures = read_u16(data, 4);
    let num_args = read_u16(data, 6);
    let num_ports = read_u16(data, 8) as usize;

    let mut offset = HEADER_SIZE;
    let mut port_wires = Vec::with_capacity(num_ports);
    for _ in 0..num_ports {
        assert!(
            offset + PortWire::WIRE_SIZE <= data.len(),
            "island spawn: truncated port info"
        );
        port_wires.push(PortWire::decode(data, &mut offset));
    }

    SpawnPayload {
        func_id,
        num_captures,
        num_args,
        port_wires,
        data_offset: offset,
    }
}

/// Unpack captures from payload.
/// Returns Vec of boxed GcRefs - each capture gets its own heap box.
pub fn unpack_captures(
    gc: &mut Gc,
    data: &[u8],
    data_offset: usize,
    num_captures: u16,
    capture_types: &[(u32, u16)],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> (Vec<GcRef>, usize) {
    let mut captures = Vec::with_capacity(num_captures as usize);
    let mut offset = data_offset;

    for i in 0..num_captures as usize {
        let len = read_u32(data, offset) as usize;
        offset += 4;

        if len == 0 {
            // Null or port (handled separately)
            captures.push(0 as GcRef);
            continue;
        }

        let (meta_raw, slots) = capture_types.get(i).copied().unwrap_or((0, 1));
        let value_meta = ValueMeta::from_raw(meta_raw);

        // Unpack the value
        let packed = PackedValue::from_data(data[offset..offset + len].to_vec());
        let mut value_slots = vec![0u64; slots as usize];
        unpack_slots(gc, &packed, &mut value_slots, struct_metas, runtime_types);
        offset += len;

        // Allocate a box for the capture
        let box_ref = gc.alloc(value_meta, slots);
        for j in 0..slots as usize {
            unsafe { Gc::write_slot(box_ref, j, value_slots[j]); }
        }
        captures.push(box_ref);
    }

    (captures, offset)
}

/// Unpack args from payload.
/// Returns flattened arg slots.
pub fn unpack_args(
    gc: &mut Gc,
    data: &[u8],
    data_offset: usize,
    num_args: u16,
    param_types: &[(u32, u16)],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> Vec<u64> {
    let mut args = Vec::new();
    let mut offset = data_offset;

    for i in 0..num_args as usize {
        let len = read_u32(data, offset) as usize;
        offset += 4;

        let (_, slots) = param_types.get(i).copied().unwrap_or((0, 1));

        if len == 0 {
            args.resize(args.len() + slots as usize, 0);
            continue;
        }

        // Unpack the value
        let packed = PackedValue::from_data(data[offset..offset + len].to_vec());
        let mut value_slots = vec![0u64; slots as usize];
        unpack_slots(gc, &packed, &mut value_slots, struct_metas, runtime_types);
        offset += len;

        args.extend_from_slice(&value_slots);
    }

    args
}
