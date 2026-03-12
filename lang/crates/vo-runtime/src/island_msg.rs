//! Island message encoding/decoding - single source of truth for cross-island protocol.
//!
//! This module defines the binary format for spawning fibers across islands.
//! Uses pack_slots/unpack_slots for proper deep copying of all sendable types.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::vec;

use crate::gc::{Gc, GcRef};
use crate::pack::{
    pack_slots, unpack_slots_with_chan_resolver_and_cache, ChanHandleInfo, PackedValue,
};
use crate::slot::Slot;
use crate::ValueMeta;
use crate::ValueKind;
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

pub const HEADER_SIZE: usize = 8;

/// Encode spawn fiber payload with proper type-aware serialization.
///
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

    buf.extend_from_slice(&func_id.to_le_bytes());
    buf.extend_from_slice(&(captures.len() as u16).to_le_bytes());
    buf.extend_from_slice(&(param_types.len() as u16).to_le_bytes());

    for (i, &slot) in captures.iter().enumerate() {
        let Some(&(meta_raw, slots)) = capture_types.get(i) else {
            buf.extend_from_slice(&0u32.to_le_bytes());
            continue;
        };
        if slot == 0 {
            buf.extend_from_slice(&0u32.to_le_bytes());
            continue;
        }
        let value_meta = ValueMeta::from_raw(meta_raw);
        let gcref = slot as GcRef;
        let mut value_slots = vec![0u64; slots as usize];
        for j in 0..slots as usize {
            value_slots[j] = unsafe { Gc::read_slot(gcref, j) };
        }
        let packed = pack_slots(gc, &value_slots, value_meta, struct_metas, runtime_types);
        let packed_data = packed.data();
        buf.extend_from_slice(&(packed_data.len() as u32).to_le_bytes());
        buf.extend_from_slice(packed_data);
    }

    let mut arg_offset = 0usize;
    for &(meta_raw, slots) in param_types {
        let slots_usize = slots as usize;
        if arg_offset + slots_usize <= args.len() {
            let value_meta = ValueMeta::from_raw(meta_raw);
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

/// Decoded spawn fiber payload.
pub struct SpawnPayload {
    pub func_id: u32,
    pub num_captures: u16,
    pub num_args: u16,
    pub data_offset: usize,
}

pub fn decode_spawn_header(data: &[u8]) -> SpawnPayload {
    assert!(data.len() >= HEADER_SIZE, "island spawn: invalid data length");

    let func_id = read_u32(data, 0);
    let num_captures = read_u16(data, 4);
    let num_args = read_u16(data, 6);

    SpawnPayload {
        func_id,
        num_captures,
        num_args,
        data_offset: HEADER_SIZE,
    }
}

pub fn unpack_spawn_payload<F>(
    gc: &mut Gc,
    data: &[u8],
    payload: &SpawnPayload,
    capture_types: &[(u32, u16)],
    param_types: &[(u32, u16)],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    mut resolve_chan: F,
) -> (Vec<GcRef>, Vec<u64>)
where
    F: FnMut(&mut Gc, ChanHandleInfo) -> GcRef,
{
    let mut captures = Vec::with_capacity(payload.num_captures as usize);
    let mut args = Vec::new();
    let mut offset = payload.data_offset;
    let mut chan_cache = Default::default();

    for i in 0..payload.num_captures as usize {
        let len = read_u32(data, offset) as usize;
        offset += 4;

        if len == 0 {
            captures.push(0 as GcRef);
            continue;
        }

        let (meta_raw, slots) = capture_types.get(i).copied().unwrap_or((0, 1));
        let value_meta = ValueMeta::from_raw(meta_raw);
        let packed = PackedValue::from_data(data[offset..offset + len].to_vec());
        let mut value_slots = vec![0u64; slots as usize];
        let mut cursor = 0;
        unpack_slots_with_chan_resolver_and_cache(
            gc,
            packed.data(),
            &mut cursor,
            &mut value_slots,
            struct_metas,
            runtime_types,
            &mut chan_cache,
            &mut resolve_chan,
        );
        offset += len;

        let box_vk = value_meta.value_kind();
        let box_meta = if box_vk == ValueKind::Array || box_vk == ValueKind::Map
            || box_vk == ValueKind::Channel || box_vk == ValueKind::Slice
            || box_vk == ValueKind::String || box_vk == ValueKind::Closure
            || box_vk == ValueKind::Island
        {
            ValueMeta::new(0, ValueKind::Struct)
        } else {
            value_meta
        };
        let box_ref = gc.alloc(box_meta, slots);
        for j in 0..slots as usize {
            unsafe { Gc::write_slot(box_ref, j, value_slots[j]); }
        }
        captures.push(box_ref);
    }

    for i in 0..payload.num_args as usize {
        let len = read_u32(data, offset) as usize;
        offset += 4;

        let (_, slots) = param_types.get(i).copied().unwrap_or((0, 1));

        if len == 0 {
            args.resize(args.len() + slots as usize, 0);
            continue;
        }

        let packed = PackedValue::from_data(data[offset..offset + len].to_vec());
        let mut value_slots = vec![0u64; slots as usize];
        let mut cursor = 0;
        unpack_slots_with_chan_resolver_and_cache(
            gc,
            packed.data(),
            &mut cursor,
            &mut value_slots,
            struct_metas,
            runtime_types,
            &mut chan_cache,
            &mut resolve_chan,
        );
        offset += len;

        args.extend_from_slice(&value_slots);
    }

    (captures, args)
}
