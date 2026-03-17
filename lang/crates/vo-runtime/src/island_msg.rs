//! Island message encoding/decoding - single source of truth for cross-island protocol.
//!
//! This module defines the binary format for spawning fibers across islands.
//! Uses pack_slots/unpack_slots for proper deep copying of all sendable types.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::vec;

use crate::gc::{Gc, GcRef};
use crate::island::{EndpointRequestKind, EndpointResponseKind, IslandCommand};
use crate::pack::{
    pack_slots, unpack_slots_with_queue_handle_resolver_and_cache, PackedValue, QueueHandleInfo,
};
use crate::slot::Slot;
use crate::ValueMeta;
use crate::ValueKind;
use vo_common_core::bytecode::StructMeta;
use vo_common_core::TransferType;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IslandCommandDecodeError {
    UnexpectedEof,
    InvalidTag,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IslandTransportFrameDecodeError {
    UnexpectedEof,
    InvalidCommand(IslandCommandDecodeError),
}

fn read_byte(data: &[u8], offset: &mut usize) -> Result<u8, IslandCommandDecodeError> {
    let Some(value) = data.get(*offset).copied() else {
        return Err(IslandCommandDecodeError::UnexpectedEof);
    };
    *offset += 1;
    Ok(value)
}

fn read_bool(data: &[u8], offset: &mut usize) -> Result<bool, IslandCommandDecodeError> {
    match read_byte(data, offset)? {
        0 => Ok(false),
        1 => Ok(true),
        _ => Err(IslandCommandDecodeError::InvalidTag),
    }
}

fn read_u32_checked(data: &[u8], offset: &mut usize) -> Result<u32, IslandCommandDecodeError> {
    if data.len() < *offset + 4 {
        return Err(IslandCommandDecodeError::UnexpectedEof);
    }
    let value = read_u32(data, *offset);
    *offset += 4;
    Ok(value)
}

fn read_u64_checked(data: &[u8], offset: &mut usize) -> Result<u64, IslandCommandDecodeError> {
    if data.len() < *offset + 8 {
        return Err(IslandCommandDecodeError::UnexpectedEof);
    }
    let value = read_u64(data, *offset);
    *offset += 8;
    Ok(value)
}

fn read_bytes(data: &[u8], offset: &mut usize, len: usize) -> Result<Vec<u8>, IslandCommandDecodeError> {
    if data.len() < *offset + len {
        return Err(IslandCommandDecodeError::UnexpectedEof);
    }
    let out = data[*offset..*offset + len].to_vec();
    *offset += len;
    Ok(out)
}

pub const HEADER_SIZE: usize = 8;

/// Determine the GC allocation meta for a boxed capture value.
/// Reference-like value kinds (arrays, maps, queues, slices, strings, closures, islands)
/// are allocated as opaque Struct so the GC does not misinterpret the inner layout.
#[inline]
fn capture_box_meta(value_meta: ValueMeta) -> ValueMeta {
    let vk = value_meta.value_kind();
    if vk.is_queue()
        || matches!(
            vk,
            ValueKind::Array
                | ValueKind::Map
                | ValueKind::Pointer
                | ValueKind::Slice
                | ValueKind::String
                | ValueKind::Closure
                | ValueKind::Island
        )
    {
        ValueMeta::new(0, ValueKind::Struct)
    } else {
        value_meta
    }
}

/// Encode spawn fiber payload with proper type-aware serialization.
///
pub fn encode_spawn_payload(
    gc: &Gc,
    func_id: u32,
    captures: &[Slot],
    capture_types: &[TransferType],
    args: &[Slot],
    param_types: &[TransferType],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> Vec<u8> {
    let mut buf = Vec::new();

    buf.extend_from_slice(&func_id.to_le_bytes());
    buf.extend_from_slice(&(captures.len() as u16).to_le_bytes());
    buf.extend_from_slice(&(param_types.len() as u16).to_le_bytes());

    for (i, &slot) in captures.iter().enumerate() {
        let Some(transfer_type) = capture_types.get(i) else {
            buf.extend_from_slice(&0u32.to_le_bytes());
            continue;
        };
        if slot == 0 {
            buf.extend_from_slice(&0u32.to_le_bytes());
            continue;
        }
        let value_meta = ValueMeta::from_raw(transfer_type.meta_raw);
        let gcref = slot as GcRef;
        let mut value_slots = vec![0u64; transfer_type.slots as usize];
        for j in 0..transfer_type.slots as usize {
            value_slots[j] = unsafe { Gc::read_slot(gcref, j) };
        }
        let packed = pack_slots(gc, &value_slots, value_meta, struct_metas, runtime_types);
        let packed_data = packed.data();
        buf.extend_from_slice(&(packed_data.len() as u32).to_le_bytes());
        buf.extend_from_slice(packed_data);
    }

    let mut arg_offset = 0usize;
    for transfer_type in param_types {
        let slots_usize = transfer_type.slots as usize;
        if arg_offset + slots_usize <= args.len() {
            let value_meta = ValueMeta::from_raw(transfer_type.meta_raw);
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
    capture_types: &[TransferType],
    param_types: &[TransferType],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    mut resolve_queue_handle: F,
) -> (Vec<GcRef>, Vec<u64>)
where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let mut captures = Vec::with_capacity(payload.num_captures as usize);
    let mut args = Vec::new();
    let mut offset = payload.data_offset;
    let mut queue_handle_cache = Default::default();

    for i in 0..payload.num_captures as usize {
        let len = read_u32(data, offset) as usize;
        offset += 4;

        if len == 0 {
            captures.push(0 as GcRef);
            continue;
        }

        let transfer_type = capture_types.get(i).copied().unwrap_or(TransferType {
            meta_raw: 0,
            rttid_raw: 0,
            slots: 1,
        });
        let value_meta = ValueMeta::from_raw(transfer_type.meta_raw);
        let packed = PackedValue::from_data(data[offset..offset + len].to_vec());
        let mut value_slots = vec![0u64; transfer_type.slots as usize];
        let mut cursor = 0;
        unpack_slots_with_queue_handle_resolver_and_cache(
            gc,
            packed.data(),
            &mut cursor,
            &mut value_slots,
            struct_metas,
            runtime_types,
            &mut queue_handle_cache,
            &mut resolve_queue_handle,
        );
        offset += len;

        let box_ref = gc.alloc(capture_box_meta(value_meta), transfer_type.slots);
        for j in 0..transfer_type.slots as usize {
            unsafe { Gc::write_slot(box_ref, j, value_slots[j]); }
        }
        captures.push(box_ref);
    }

    for i in 0..payload.num_args as usize {
        let len = read_u32(data, offset) as usize;
        offset += 4;

        let transfer_type = param_types.get(i).copied().unwrap_or(TransferType {
            meta_raw: 0,
            rttid_raw: 0,
            slots: 1,
        });

        if len == 0 {
            args.resize(args.len() + transfer_type.slots as usize, 0);
            continue;
        }

        let packed = PackedValue::from_data(data[offset..offset + len].to_vec());
        let mut value_slots = vec![0u64; transfer_type.slots as usize];
        let mut cursor = 0;
        unpack_slots_with_queue_handle_resolver_and_cache(
            gc,
            packed.data(),
            &mut cursor,
            &mut value_slots,
            struct_metas,
            runtime_types,
            &mut queue_handle_cache,
            &mut resolve_queue_handle,
        );
        offset += len;

        args.extend_from_slice(&value_slots);
    }

    (captures, args)
}

pub fn encode_island_command(cmd: &IslandCommand) -> Vec<u8> {
    let mut buf = Vec::new();
    match cmd {
        IslandCommand::SpawnFiber { closure_data } => {
            buf.push(0);
            let data = closure_data.data();
            buf.extend_from_slice(&(data.len() as u32).to_le_bytes());
            buf.extend_from_slice(data);
        }
        IslandCommand::WakeFiber { fiber_id } => {
            buf.push(1);
            buf.extend_from_slice(&fiber_id.to_le_bytes());
        }
        IslandCommand::Shutdown => {
            buf.push(2);
        }
        IslandCommand::EndpointRequest { endpoint_id, kind, from_island, fiber_id } => {
            buf.push(3);
            buf.extend_from_slice(&endpoint_id.to_le_bytes());
            encode_endpoint_request_kind(&mut buf, kind);
            buf.extend_from_slice(&from_island.to_le_bytes());
            buf.extend_from_slice(&fiber_id.to_le_bytes());
        }
        IslandCommand::EndpointResponse { endpoint_id, kind, fiber_id } => {
            buf.push(4);
            buf.extend_from_slice(&endpoint_id.to_le_bytes());
            encode_endpoint_response_kind(&mut buf, kind);
            buf.extend_from_slice(&fiber_id.to_le_bytes());
        }
    }
    buf
}

pub fn encode_island_transport_frame(target_island_id: u32, cmd: &IslandCommand) -> Vec<u8> {
    let payload = encode_island_command(cmd);
    let mut buf = Vec::with_capacity(4 + payload.len());
    buf.extend_from_slice(&target_island_id.to_le_bytes());
    buf.extend_from_slice(&payload);
    buf
}

fn encode_endpoint_request_kind(buf: &mut Vec<u8>, kind: &EndpointRequestKind) {
    match kind {
        EndpointRequestKind::Send { data } => {
            buf.push(0);
            buf.extend_from_slice(&(data.len() as u32).to_le_bytes());
            buf.extend_from_slice(data);
        }
        EndpointRequestKind::Recv => buf.push(1),
        EndpointRequestKind::Close => buf.push(2),
        EndpointRequestKind::Transfer { new_peer } => {
            buf.push(3);
            buf.extend_from_slice(&new_peer.to_le_bytes());
        }
    }
}

fn encode_endpoint_response_kind(buf: &mut Vec<u8>, kind: &EndpointResponseKind) {
    match kind {
        EndpointResponseKind::SendAck { closed } => {
            buf.push(0);
            buf.push(u8::from(*closed));
        }
        EndpointResponseKind::RecvData { data, closed } => {
            buf.push(1);
            buf.push(u8::from(*closed));
            buf.extend_from_slice(&(data.len() as u32).to_le_bytes());
            buf.extend_from_slice(data);
        }
        EndpointResponseKind::Closed => buf.push(2),
    }
}

pub fn decode_island_command(data: &[u8]) -> Result<IslandCommand, IslandCommandDecodeError> {
    let mut offset = 0usize;
    let tag = read_byte(data, &mut offset)?;
    match tag {
        0 => {
            let len = read_u32_checked(data, &mut offset)? as usize;
            let payload = read_bytes(data, &mut offset, len)?;
            Ok(IslandCommand::SpawnFiber {
                closure_data: PackedValue::from_data(payload),
            })
        }
        1 => Ok(IslandCommand::WakeFiber {
            fiber_id: read_u32_checked(data, &mut offset)?,
        }),
        2 => Ok(IslandCommand::Shutdown),
        3 => {
            let endpoint_id = read_u64_checked(data, &mut offset)?;
            let kind = decode_endpoint_request_kind(data, &mut offset)?;
            let from_island = read_u32_checked(data, &mut offset)?;
            let fiber_id = read_u64_checked(data, &mut offset)?;
            Ok(IslandCommand::EndpointRequest {
                endpoint_id,
                kind,
                from_island,
                fiber_id,
            })
        }
        4 => {
            let endpoint_id = read_u64_checked(data, &mut offset)?;
            let kind = decode_endpoint_response_kind(data, &mut offset)?;
            let fiber_id = read_u64_checked(data, &mut offset)?;
            Ok(IslandCommand::EndpointResponse {
                endpoint_id,
                kind,
                fiber_id,
            })
        }
        _ => Err(IslandCommandDecodeError::InvalidTag),
    }
}

pub fn decode_island_transport_frame(
    data: &[u8],
) -> Result<(u32, IslandCommand), IslandTransportFrameDecodeError> {
    if data.len() < 4 {
        return Err(IslandTransportFrameDecodeError::UnexpectedEof);
    }
    let target_island_id = read_u32(data, 0);
    let cmd = decode_island_command(&data[4..])
        .map_err(IslandTransportFrameDecodeError::InvalidCommand)?;
    Ok((target_island_id, cmd))
}

fn decode_endpoint_request_kind(
    data: &[u8],
    offset: &mut usize,
) -> Result<EndpointRequestKind, IslandCommandDecodeError> {
    match read_byte(data, offset)? {
        0 => {
            let len = read_u32_checked(data, offset)? as usize;
            let data = read_bytes(data, offset, len)?;
            Ok(EndpointRequestKind::Send { data })
        }
        1 => Ok(EndpointRequestKind::Recv),
        2 => Ok(EndpointRequestKind::Close),
        3 => Ok(EndpointRequestKind::Transfer {
            new_peer: read_u32_checked(data, offset)?,
        }),
        _ => Err(IslandCommandDecodeError::InvalidTag),
    }
}

fn decode_endpoint_response_kind(
    data: &[u8],
    offset: &mut usize,
) -> Result<EndpointResponseKind, IslandCommandDecodeError> {
    match read_byte(data, offset)? {
        0 => Ok(EndpointResponseKind::SendAck {
            closed: read_bool(data, offset)?,
        }),
        1 => {
            let closed = read_bool(data, offset)?;
            let len = read_u32_checked(data, offset)? as usize;
            let data = read_bytes(data, offset, len)?;
            Ok(EndpointResponseKind::RecvData { data, closed })
        }
        2 => Ok(EndpointResponseKind::Closed),
        _ => Err(IslandCommandDecodeError::InvalidTag),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn roundtrip(cmd: IslandCommand) {
        let encoded = encode_island_command(&cmd);
        let decoded = decode_island_command(&encoded).unwrap();
        match (cmd, decoded) {
            (
                IslandCommand::SpawnFiber { closure_data: a },
                IslandCommand::SpawnFiber { closure_data: b },
            ) => assert_eq!(a.data(), b.data()),
            (IslandCommand::WakeFiber { fiber_id: a }, IslandCommand::WakeFiber { fiber_id: b }) => assert_eq!(a, b),
            (IslandCommand::Shutdown, IslandCommand::Shutdown) => {}
            (
                IslandCommand::EndpointRequest { endpoint_id: ae, kind: ak, from_island: af, fiber_id: afi },
                IslandCommand::EndpointRequest { endpoint_id: be, kind: bk, from_island: bf, fiber_id: bfi },
            ) => {
                assert_eq!(ae, be);
                assert_eq!(af, bf);
                assert_eq!(afi, bfi);
                assert_request_kind_eq(&ak, &bk);
            }
            (
                IslandCommand::EndpointResponse { endpoint_id: ae, kind: ak, fiber_id: afi },
                IslandCommand::EndpointResponse { endpoint_id: be, kind: bk, fiber_id: bfi },
            ) => {
                assert_eq!(ae, be);
                assert_eq!(afi, bfi);
                assert_response_kind_eq(&ak, &bk);
            }
            _ => panic!("decoded command variant mismatch"),
        }
    }

    fn roundtrip_frame(target_island_id: u32, cmd: IslandCommand) {
        let encoded = encode_island_transport_frame(target_island_id, &cmd);
        let (decoded_target, decoded_cmd) = decode_island_transport_frame(&encoded).unwrap();
        assert_eq!(decoded_target, target_island_id);
        match (cmd, decoded_cmd) {
            (
                IslandCommand::SpawnFiber { closure_data: a },
                IslandCommand::SpawnFiber { closure_data: b },
            ) => assert_eq!(a.data(), b.data()),
            (IslandCommand::WakeFiber { fiber_id: a }, IslandCommand::WakeFiber { fiber_id: b }) => assert_eq!(a, b),
            (IslandCommand::Shutdown, IslandCommand::Shutdown) => {}
            (
                IslandCommand::EndpointRequest { endpoint_id: ae, kind: ak, from_island: af, fiber_id: afi },
                IslandCommand::EndpointRequest { endpoint_id: be, kind: bk, from_island: bf, fiber_id: bfi },
            ) => {
                assert_eq!(ae, be);
                assert_eq!(af, bf);
                assert_eq!(afi, bfi);
                assert_request_kind_eq(&ak, &bk);
            }
            (
                IslandCommand::EndpointResponse { endpoint_id: ae, kind: ak, fiber_id: afi },
                IslandCommand::EndpointResponse { endpoint_id: be, kind: bk, fiber_id: bfi },
            ) => {
                assert_eq!(ae, be);
                assert_eq!(afi, bfi);
                assert_response_kind_eq(&ak, &bk);
            }
            _ => panic!("decoded frame command variant mismatch"),
        }
    }

    fn assert_request_kind_eq(a: &EndpointRequestKind, b: &EndpointRequestKind) {
        match (a, b) {
            (EndpointRequestKind::Send { data: ad }, EndpointRequestKind::Send { data: bd }) => assert_eq!(ad, bd),
            (EndpointRequestKind::Recv, EndpointRequestKind::Recv) => {}
            (EndpointRequestKind::Close, EndpointRequestKind::Close) => {}
            (EndpointRequestKind::Transfer { new_peer: a }, EndpointRequestKind::Transfer { new_peer: b }) => assert_eq!(a, b),
            _ => panic!("request kind mismatch"),
        }
    }

    fn assert_response_kind_eq(a: &EndpointResponseKind, b: &EndpointResponseKind) {
        match (a, b) {
            (EndpointResponseKind::SendAck { closed: a }, EndpointResponseKind::SendAck { closed: b }) => assert_eq!(a, b),
            (
                EndpointResponseKind::RecvData { data: ad, closed: ac },
                EndpointResponseKind::RecvData { data: bd, closed: bc },
            ) => {
                assert_eq!(ad, bd);
                assert_eq!(ac, bc);
            }
            (EndpointResponseKind::Closed, EndpointResponseKind::Closed) => {}
            _ => panic!("response kind mismatch"),
        }
    }

    #[test]
    fn island_command_codec_roundtrips_all_variants() {
        roundtrip(IslandCommand::SpawnFiber {
            closure_data: PackedValue::from_data(vec![1, 2, 3, 4]),
        });
        roundtrip(IslandCommand::WakeFiber { fiber_id: 42 });
        roundtrip(IslandCommand::Shutdown);
        roundtrip(IslandCommand::EndpointRequest {
            endpoint_id: 99,
            kind: EndpointRequestKind::Send { data: vec![7, 8, 9] },
            from_island: 3,
            fiber_id: 1234,
        });
        roundtrip(IslandCommand::EndpointRequest {
            endpoint_id: 100,
            kind: EndpointRequestKind::Recv,
            from_island: 4,
            fiber_id: 1235,
        });
        roundtrip(IslandCommand::EndpointRequest {
            endpoint_id: 101,
            kind: EndpointRequestKind::Close,
            from_island: 5,
            fiber_id: 1236,
        });
        roundtrip(IslandCommand::EndpointRequest {
            endpoint_id: 102,
            kind: EndpointRequestKind::Transfer { new_peer: 8 },
            from_island: 6,
            fiber_id: 1237,
        });
        roundtrip(IslandCommand::EndpointResponse {
            endpoint_id: 103,
            kind: EndpointResponseKind::SendAck { closed: true },
            fiber_id: 2234,
        });
        roundtrip(IslandCommand::EndpointResponse {
            endpoint_id: 104,
            kind: EndpointResponseKind::RecvData { data: vec![10, 11], closed: false },
            fiber_id: 2235,
        });
        roundtrip(IslandCommand::EndpointResponse {
            endpoint_id: 105,
            kind: EndpointResponseKind::Closed,
            fiber_id: 2236,
        });
    }

    #[test]
    fn island_transport_frame_roundtrips_target_and_command() {
        roundtrip_frame(
            7,
            IslandCommand::SpawnFiber {
                closure_data: PackedValue::from_data(vec![9, 8, 7]),
            },
        );
        roundtrip_frame(
            11,
            IslandCommand::EndpointRequest {
                endpoint_id: 44,
                kind: EndpointRequestKind::Transfer { new_peer: 12 },
                from_island: 3,
                fiber_id: 99,
            },
        );
    }

    #[test]
    fn island_command_codec_rejects_truncated_payload() {
        let encoded = encode_island_command(&IslandCommand::WakeFiber { fiber_id: 7 });
        let truncated = &encoded[..encoded.len() - 1];
        match decode_island_command(truncated) {
            Err(IslandCommandDecodeError::UnexpectedEof) => {}
            other => panic!("expected UnexpectedEof, got {:?}", other),
        }
    }

    #[test]
    fn island_transport_frame_rejects_truncated_header() {
        match decode_island_transport_frame(&[1, 2, 3]) {
            Err(IslandTransportFrameDecodeError::UnexpectedEof) => {}
            other => panic!("expected UnexpectedEof, got {:?}", other),
        }
    }
}
