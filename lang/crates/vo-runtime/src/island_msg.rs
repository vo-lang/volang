//! Island message encoding/decoding - single source of truth for cross-island protocol.
//!
//! This module defines the binary format for spawning fibers across islands.
//! Uses pack_slots/unpack_slots for proper deep copying of all sendable types.

#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::{Gc, GcRef};
use crate::island::{EndpointRequestKind, EndpointResponseKind, IslandCommand};
use crate::objects::queue_state::{QueueWaiter, SelectInfo, SelectWaitKind};
use crate::pack::{
    pack_slots_with_named_type_metas, unpack_slots_expected_with_queue_handle_resolver_and_cache,
    validate_packed_slots_expected_with_named_type_metas, PackTypeContext, PackedValue,
    QueueHandleInfo,
};
use crate::slot::Slot;
use crate::ValueKind;
use crate::ValueMeta;
use crate::ValueRttid;
use vo_common_core::bytecode::{NamedTypeMeta, StructMeta};
use vo_common_core::RuntimeType;
use vo_common_core::TransferType;

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
    TrailingBytes,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IslandTransportFrameDecodeError {
    UnexpectedEof,
    InvalidCommand(IslandCommandDecodeError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpawnPayloadUnpackError {
    UnexpectedEof,
    TypeCountMismatch,
    LayoutMismatch,
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

fn read_u16_checked(data: &[u8], offset: &mut usize) -> Result<u16, IslandCommandDecodeError> {
    if data.len() < *offset + 2 {
        return Err(IslandCommandDecodeError::UnexpectedEof);
    }
    let value = read_u16(data, *offset);
    *offset += 2;
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

fn encode_queue_waiter(buf: &mut Vec<u8>, waiter: &QueueWaiter) {
    buf.extend_from_slice(&waiter.island_id.to_le_bytes());
    buf.extend_from_slice(&waiter.fiber_key.to_le_bytes());
    buf.extend_from_slice(&waiter.registration_id.to_le_bytes());
    buf.extend_from_slice(&waiter.endpoint_wait_id.to_le_bytes());
    buf.extend_from_slice(&waiter.queue_ref.to_le_bytes());
    match waiter.kind {
        Some(kind) => {
            buf.push(1);
            buf.push(kind.to_raw());
        }
        None => buf.push(0),
    }
    match waiter.select.as_ref() {
        Some(select) => {
            buf.push(1);
            buf.extend_from_slice(&select.case_index.to_le_bytes());
            buf.extend_from_slice(&select.select_id.to_le_bytes());
            buf.extend_from_slice(&select.queue_ref.to_le_bytes());
            buf.push(select.kind.to_raw());
        }
        None => buf.push(0),
    }
}

fn decode_queue_waiter(
    data: &[u8],
    offset: &mut usize,
) -> Result<QueueWaiter, IslandCommandDecodeError> {
    let island_id = read_u32_checked(data, offset)?;
    let fiber_key = read_u64_checked(data, offset)?;
    let registration_id = read_u64_checked(data, offset)?;
    let endpoint_wait_id = read_u64_checked(data, offset)?;
    let queue_ref = read_u64_checked(data, offset)?;
    let kind = match read_byte(data, offset)? {
        0 => None,
        1 => Some(
            SelectWaitKind::from_raw(read_byte(data, offset)?)
                .ok_or(IslandCommandDecodeError::InvalidTag)?,
        ),
        _ => return Err(IslandCommandDecodeError::InvalidTag),
    };
    let select = match read_byte(data, offset)? {
        0 => None,
        1 => Some(SelectInfo {
            case_index: read_u16_checked(data, offset)?,
            select_id: read_u64_checked(data, offset)?,
            queue_ref: read_u64_checked(data, offset)?,
            kind: SelectWaitKind::from_raw(read_byte(data, offset)?)
                .ok_or(IslandCommandDecodeError::InvalidTag)?,
        }),
        _ => return Err(IslandCommandDecodeError::InvalidTag),
    };
    Ok(QueueWaiter {
        island_id,
        fiber_key,
        registration_id,
        endpoint_wait_id,
        queue_ref,
        kind,
        select,
    })
}

fn read_bytes(
    data: &[u8],
    offset: &mut usize,
    len: usize,
) -> Result<Vec<u8>, IslandCommandDecodeError> {
    if data.len() < *offset + len {
        return Err(IslandCommandDecodeError::UnexpectedEof);
    }
    let out = data[*offset..*offset + len].to_vec();
    *offset += len;
    Ok(out)
}

pub const HEADER_SIZE: usize = 10;

/// Determine the GC allocation meta for a boxed capture value.
/// Reference-like value kinds (arrays, maps, queues, slices, strings, closures, islands)
/// are allocated as opaque Struct so the GC does not misinterpret the inner layout.
#[inline]
pub fn capture_box_meta(value_meta: ValueMeta) -> ValueMeta {
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

#[inline]
pub fn alloc_capture_box(gc: &mut Gc, value_meta: ValueMeta, slots: u16) -> GcRef {
    gc.alloc_value_slots(value_meta, slots)
}

/// Encode spawn fiber payload with proper type-aware serialization.
///
#[allow(clippy::too_many_arguments)]
pub fn encode_spawn_payload_from_capture_values(
    gc: &Gc,
    func_id: u32,
    capture_values: &[Option<Vec<Slot>>],
    capture_types: &[TransferType],
    args: &[Slot],
    param_types: &[TransferType],
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Vec<u8> {
    let mut buf = Vec::new();

    buf.extend_from_slice(&func_id.to_le_bytes());
    buf.extend_from_slice(&(capture_values.len() as u16).to_le_bytes());
    buf.extend_from_slice(&(param_types.len() as u16).to_le_bytes());
    buf.extend_from_slice(&0u16.to_le_bytes());

    for (i, value_slots) in capture_values.iter().enumerate() {
        let Some(transfer_type) = capture_types.get(i) else {
            buf.extend_from_slice(&0u32.to_le_bytes());
            continue;
        };
        let Some(value_slots) = value_slots.as_ref() else {
            buf.extend_from_slice(&0u32.to_le_bytes());
            continue;
        };
        let value_meta = ValueMeta::from_raw(transfer_type.meta_raw);
        let packed = pack_slots_with_named_type_metas(
            gc,
            value_slots,
            value_meta,
            struct_metas,
            named_type_metas,
            runtime_types,
        );
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
            let packed = pack_slots_with_named_type_metas(
                gc,
                value_slots,
                value_meta,
                struct_metas,
                named_type_metas,
                runtime_types,
            );
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

/// Encode a direct method-value closure payload.
///
/// Direct method-value closures store receiver slots directly in closure captures
/// so the destination must recreate those raw captures, not capture boxes.
#[allow(clippy::too_many_arguments)]
pub fn encode_spawn_payload_from_raw_capture_slots(
    gc: &Gc,
    func_id: u32,
    raw_capture_slots: &[Slot],
    capture_type: TransferType,
    args: &[Slot],
    param_types: &[TransferType],
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Vec<u8> {
    let mut buf = Vec::new();

    buf.extend_from_slice(&func_id.to_le_bytes());
    buf.extend_from_slice(&1u16.to_le_bytes());
    buf.extend_from_slice(&(param_types.len() as u16).to_le_bytes());
    buf.extend_from_slice(&(raw_capture_slots.len() as u16).to_le_bytes());

    let value_meta = ValueMeta::from_raw(capture_type.meta_raw);
    let packed = pack_slots_with_named_type_metas(
        gc,
        raw_capture_slots,
        value_meta,
        struct_metas,
        named_type_metas,
        runtime_types,
    );
    let packed_data = packed.data();
    buf.extend_from_slice(&(packed_data.len() as u32).to_le_bytes());
    buf.extend_from_slice(packed_data);

    let mut arg_offset = 0usize;
    for transfer_type in param_types {
        let slots_usize = transfer_type.slots as usize;
        if arg_offset + slots_usize <= args.len() {
            let value_meta = ValueMeta::from_raw(transfer_type.meta_raw);
            let value_slots = &args[arg_offset..arg_offset + slots_usize];
            let packed = pack_slots_with_named_type_metas(
                gc,
                value_slots,
                value_meta,
                struct_metas,
                named_type_metas,
                runtime_types,
            );
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
    pub raw_capture_slots: u16,
    pub data_offset: usize,
}

pub fn decode_spawn_header(data: &[u8]) -> Result<SpawnPayload, SpawnPayloadUnpackError> {
    if data.len() < HEADER_SIZE {
        return Err(SpawnPayloadUnpackError::UnexpectedEof);
    }

    let func_id = read_u32(data, 0);
    let num_captures = read_u16(data, 4);
    let num_args = read_u16(data, 6);
    let raw_capture_slots = read_u16(data, 8);

    Ok(SpawnPayload {
        func_id,
        num_captures,
        num_args,
        raw_capture_slots,
        data_offset: HEADER_SIZE,
    })
}

fn read_spawn_packed_chunk<'a>(
    data: &'a [u8],
    offset: &mut usize,
    transfer_type: TransferType,
) -> Result<Option<&'a [u8]>, SpawnPayloadUnpackError> {
    let len = read_u32_checked(data, offset).map_err(|_| SpawnPayloadUnpackError::UnexpectedEof)?
        as usize;
    if len == 0 {
        return if transfer_type.slots == 0 {
            Ok(None)
        } else {
            Err(SpawnPayloadUnpackError::LayoutMismatch)
        };
    }
    let end = offset
        .checked_add(len)
        .ok_or(SpawnPayloadUnpackError::UnexpectedEof)?;
    let packed_data = data
        .get(*offset..end)
        .ok_or(SpawnPayloadUnpackError::UnexpectedEof)?;
    *offset = end;
    Ok(Some(packed_data))
}

#[allow(clippy::too_many_arguments)]
pub fn unpack_spawn_payload<F>(
    gc: &mut Gc,
    data: &[u8],
    payload: &SpawnPayload,
    capture_types: &[TransferType],
    param_types: &[TransferType],
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
    mut resolve_queue_handle: F,
) -> Result<(Vec<u64>, Vec<u64>), SpawnPayloadUnpackError>
where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    if payload.num_args as usize != param_types.len() {
        return Err(SpawnPayloadUnpackError::TypeCountMismatch);
    }
    if payload.raw_capture_slots == 0 {
        if payload.num_captures as usize != capture_types.len() {
            return Err(SpawnPayloadUnpackError::TypeCountMismatch);
        }
    } else if payload.num_captures != 1 || capture_types.len() != 1 {
        return Err(SpawnPayloadUnpackError::TypeCountMismatch);
    }

    let mut captures = Vec::with_capacity(payload.num_captures as usize);
    let mut args = Vec::new();
    let mut offset = payload.data_offset;
    let mut queue_handle_cache = Default::default();

    if payload.raw_capture_slots > 0 {
        let transfer_type = capture_types[0];
        if transfer_type.slots != payload.raw_capture_slots {
            return Err(SpawnPayloadUnpackError::LayoutMismatch);
        }
        let Some(packed_data) = read_spawn_packed_chunk(data, &mut offset, transfer_type)? else {
            return Err(SpawnPayloadUnpackError::LayoutMismatch);
        };
        let value_meta = ValueMeta::from_raw(transfer_type.meta_raw);
        let value_rttid = ValueRttid::from_raw(transfer_type.rttid_raw);
        validate_packed_slots_expected_with_named_type_metas(
            packed_data,
            value_meta,
            value_rttid,
            struct_metas,
            named_type_metas,
            runtime_types,
        )
        .map_err(|_| SpawnPayloadUnpackError::LayoutMismatch)?;
        let packed = PackedValue::from_data(packed_data.to_vec());
        let mut value_slots = vec![0u64; transfer_type.slots as usize];
        let mut cursor = 0;
        unpack_slots_expected_with_queue_handle_resolver_and_cache(
            gc,
            packed.data(),
            &mut cursor,
            &mut value_slots,
            value_meta,
            PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types),
            &mut queue_handle_cache,
            &mut resolve_queue_handle,
        );
        captures.extend_from_slice(&value_slots);
    } else {
        for i in 0..payload.num_captures as usize {
            let transfer_type = capture_types[i];
            let Some(packed_data) = read_spawn_packed_chunk(data, &mut offset, transfer_type)?
            else {
                captures.push(0);
                continue;
            };
            let value_meta = ValueMeta::from_raw(transfer_type.meta_raw);
            let value_rttid = ValueRttid::from_raw(transfer_type.rttid_raw);
            validate_packed_slots_expected_with_named_type_metas(
                packed_data,
                value_meta,
                value_rttid,
                struct_metas,
                named_type_metas,
                runtime_types,
            )
            .map_err(|_| SpawnPayloadUnpackError::LayoutMismatch)?;
            let packed = PackedValue::from_data(packed_data.to_vec());
            let mut value_slots = vec![0u64; transfer_type.slots as usize];
            let mut cursor = 0;
            unpack_slots_expected_with_queue_handle_resolver_and_cache(
                gc,
                packed.data(),
                &mut cursor,
                &mut value_slots,
                value_meta,
                PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types),
                &mut queue_handle_cache,
                &mut resolve_queue_handle,
            );

            let box_ref = alloc_capture_box(gc, value_meta, transfer_type.slots);
            for (j, &slot) in value_slots.iter().enumerate() {
                unsafe {
                    Gc::write_slot(box_ref, j, slot);
                }
            }
            gc.mark_allocated_for_scan(box_ref);
            captures.push(box_ref as u64);
        }
    }

    for i in 0..payload.num_args as usize {
        let transfer_type = param_types[i];
        let value_meta = ValueMeta::from_raw(transfer_type.meta_raw);

        let Some(packed_data) = read_spawn_packed_chunk(data, &mut offset, transfer_type)? else {
            args.resize(args.len() + transfer_type.slots as usize, 0);
            continue;
        };
        let value_rttid = ValueRttid::from_raw(transfer_type.rttid_raw);
        validate_packed_slots_expected_with_named_type_metas(
            packed_data,
            value_meta,
            value_rttid,
            struct_metas,
            named_type_metas,
            runtime_types,
        )
        .map_err(|_| SpawnPayloadUnpackError::LayoutMismatch)?;
        let packed = PackedValue::from_data(packed_data.to_vec());
        let mut value_slots = vec![0u64; transfer_type.slots as usize];
        let mut cursor = 0;
        unpack_slots_expected_with_queue_handle_resolver_and_cache(
            gc,
            packed.data(),
            &mut cursor,
            &mut value_slots,
            value_meta,
            PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types),
            &mut queue_handle_cache,
            &mut resolve_queue_handle,
        );

        args.extend_from_slice(&value_slots);
    }

    if offset != data.len() {
        return Err(SpawnPayloadUnpackError::LayoutMismatch);
    }

    Ok((captures, args))
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
        IslandCommand::WakeFiber { waiter } => {
            buf.push(1);
            encode_queue_waiter(&mut buf, waiter);
        }
        IslandCommand::Shutdown => {
            buf.push(2);
        }
        IslandCommand::EndpointRequest {
            endpoint_id,
            kind,
            from_island,
            fiber_key,
            wait_id,
        } => {
            buf.push(3);
            buf.extend_from_slice(&endpoint_id.to_le_bytes());
            encode_endpoint_request_kind(&mut buf, kind);
            buf.extend_from_slice(&from_island.to_le_bytes());
            buf.extend_from_slice(&fiber_key.to_le_bytes());
            buf.extend_from_slice(&wait_id.to_le_bytes());
        }
        IslandCommand::EndpointResponse {
            endpoint_id,
            kind,
            from_island,
            fiber_key,
            wait_id,
        } => {
            buf.push(4);
            buf.extend_from_slice(&endpoint_id.to_le_bytes());
            encode_endpoint_response_kind(&mut buf, kind);
            buf.extend_from_slice(&from_island.to_le_bytes());
            buf.extend_from_slice(&fiber_key.to_le_bytes());
            buf.extend_from_slice(&wait_id.to_le_bytes());
        }
    }
    buf
}

pub fn encode_island_transport_frame(
    target_island_id: u32,
    source_island_id: u32,
    cmd: &IslandCommand,
) -> Vec<u8> {
    let payload = encode_island_command(cmd);
    let mut buf = Vec::with_capacity(8 + payload.len());
    buf.extend_from_slice(&target_island_id.to_le_bytes());
    buf.extend_from_slice(&source_island_id.to_le_bytes());
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
        EndpointResponseKind::RecvError => buf.push(3),
    }
}

pub fn decode_island_command(data: &[u8]) -> Result<IslandCommand, IslandCommandDecodeError> {
    let mut offset = 0usize;
    let tag = read_byte(data, &mut offset)?;
    let command = match tag {
        0 => {
            let len = read_u32_checked(data, &mut offset)? as usize;
            let payload = read_bytes(data, &mut offset, len)?;
            Ok(IslandCommand::SpawnFiber {
                closure_data: PackedValue::from_data(payload),
            })
        }
        1 => Ok(IslandCommand::WakeFiber {
            waiter: decode_queue_waiter(data, &mut offset)?,
        }),
        2 => Ok(IslandCommand::Shutdown),
        3 => {
            let endpoint_id = read_u64_checked(data, &mut offset)?;
            let kind = decode_endpoint_request_kind(data, &mut offset)?;
            let from_island = read_u32_checked(data, &mut offset)?;
            let fiber_key = read_u64_checked(data, &mut offset)?;
            let wait_id = read_u64_checked(data, &mut offset)?;
            Ok(IslandCommand::EndpointRequest {
                endpoint_id,
                kind,
                from_island,
                fiber_key,
                wait_id,
            })
        }
        4 => {
            let endpoint_id = read_u64_checked(data, &mut offset)?;
            let kind = decode_endpoint_response_kind(data, &mut offset)?;
            let from_island = read_u32_checked(data, &mut offset)?;
            let fiber_key = read_u64_checked(data, &mut offset)?;
            let wait_id = read_u64_checked(data, &mut offset)?;
            Ok(IslandCommand::EndpointResponse {
                endpoint_id,
                kind,
                from_island,
                fiber_key,
                wait_id,
            })
        }
        _ => Err(IslandCommandDecodeError::InvalidTag),
    }?;
    if offset != data.len() {
        return Err(IslandCommandDecodeError::TrailingBytes);
    }
    Ok(command)
}

pub fn decode_island_transport_frame(
    data: &[u8],
) -> Result<(u32, u32, IslandCommand), IslandTransportFrameDecodeError> {
    if data.len() < 8 {
        return Err(IslandTransportFrameDecodeError::UnexpectedEof);
    }
    let target_island_id = read_u32(data, 0);
    let source_island_id = read_u32(data, 4);
    let cmd = decode_island_command(&data[8..])
        .map_err(IslandTransportFrameDecodeError::InvalidCommand)?;
    Ok((target_island_id, source_island_id, cmd))
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
        3 => Ok(EndpointResponseKind::RecvError),
        _ => Err(IslandCommandDecodeError::InvalidTag),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc_types::{trace_object_children_with_context, ClosureScanLayout, GcScanContext};
    use crate::objects::string;

    fn roundtrip(cmd: IslandCommand) {
        let encoded = encode_island_command(&cmd);
        let decoded = decode_island_command(&encoded).unwrap();
        match (cmd, decoded) {
            (
                IslandCommand::SpawnFiber { closure_data: a },
                IslandCommand::SpawnFiber { closure_data: b },
            ) => assert_eq!(a.data(), b.data()),
            (IslandCommand::WakeFiber { waiter: a }, IslandCommand::WakeFiber { waiter: b }) => {
                assert_eq!(a, b)
            }
            (IslandCommand::Shutdown, IslandCommand::Shutdown) => {}
            (
                IslandCommand::EndpointRequest {
                    endpoint_id: ae,
                    kind: ak,
                    from_island: af,
                    fiber_key: afi,
                    wait_id: aw,
                },
                IslandCommand::EndpointRequest {
                    endpoint_id: be,
                    kind: bk,
                    from_island: bf,
                    fiber_key: bfi,
                    wait_id: bw,
                },
            ) => {
                assert_eq!(ae, be);
                assert_eq!(af, bf);
                assert_eq!(afi, bfi);
                assert_eq!(aw, bw);
                assert_request_kind_eq(&ak, &bk);
            }
            (
                IslandCommand::EndpointResponse {
                    endpoint_id: ae,
                    kind: ak,
                    from_island: af,
                    fiber_key: afi,
                    wait_id: aw,
                },
                IslandCommand::EndpointResponse {
                    endpoint_id: be,
                    kind: bk,
                    from_island: bf,
                    fiber_key: bfi,
                    wait_id: bw,
                },
            ) => {
                assert_eq!(ae, be);
                assert_eq!(af, bf);
                assert_eq!(afi, bfi);
                assert_eq!(aw, bw);
                assert_response_kind_eq(&ak, &bk);
            }
            _ => panic!("decoded command variant mismatch"),
        }
    }

    #[test]
    fn vm_island_spawn_capture_root_005_unpack_allocates_value_slot_capture_box() {
        let mut gc = Gc::new();
        let left = string::create(&mut gc, b"left");
        let right = string::create(&mut gc, b"right");
        let runtime_types = vec![
            RuntimeType::Basic(ValueKind::String),
            RuntimeType::Array {
                len: 2,
                elem: ValueRttid::new(0, ValueKind::String),
            },
        ];
        let capture_type = TransferType {
            meta_raw: ValueMeta::new(1, ValueKind::Array).to_raw(),
            rttid_raw: ValueRttid::new(1, ValueKind::Array).to_raw(),
            slots: 2,
        };
        let encoded = encode_spawn_payload_from_capture_values(
            &gc,
            7,
            &[Some(vec![left as u64, right as u64])],
            &[capture_type],
            &[],
            &[],
            &[],
            &[],
            &runtime_types,
        );
        let payload = decode_spawn_header(&encoded).expect("spawn header");
        let (captures, args) = unpack_spawn_payload(
            &mut gc,
            &encoded,
            &payload,
            &[capture_type],
            &[],
            &[],
            &[],
            &runtime_types,
            |_, _| core::ptr::null_mut(),
        )
        .expect("spawn payload");

        assert!(args.is_empty());
        let capture_box = captures[0] as GcRef;
        let header = Gc::header(capture_box);
        assert!(header.is_value_slots_object());
        assert_eq!(header.value_meta(), ValueMeta::new(1, ValueKind::Array));
        assert_eq!(header.slots, 2);

        let mut visited = Vec::new();
        trace_object_children_with_context(
            capture_box,
            GcScanContext::from_module_parts(&[], &[], &runtime_types),
            &|_| ClosureScanLayout::default(),
            |child| visited.push(child),
        );
        assert_eq!(
            visited.len(),
            2,
            "spawn unpack capture box must scan every array element root"
        );
        assert!(visited.iter().all(|&child| {
            Gc::header(child).value_meta() == ValueMeta::new(0, ValueKind::String)
        }));
    }

    #[test]
    fn vm_direct_method_capture_protocol_006_unpack_preserves_raw_receiver_slots() {
        let mut gc = Gc::new();
        let left = string::create(&mut gc, b"left");
        let right = string::create(&mut gc, b"right");
        let runtime_types = vec![
            RuntimeType::Basic(ValueKind::String),
            RuntimeType::Array {
                len: 2,
                elem: ValueRttid::new(0, ValueKind::String),
            },
        ];
        let receiver_type = TransferType {
            meta_raw: ValueMeta::new(1, ValueKind::Array).to_raw(),
            rttid_raw: ValueRttid::new(1, ValueKind::Array).to_raw(),
            slots: 2,
        };
        let encoded = encode_spawn_payload_from_raw_capture_slots(
            &gc,
            7,
            &[left as u64, right as u64],
            receiver_type,
            &[],
            &[],
            &[],
            &[],
            &runtime_types,
        );
        let payload = decode_spawn_header(&encoded).expect("spawn header");
        assert_eq!(payload.num_captures, 1);
        assert_eq!(payload.raw_capture_slots, 2);

        let (captures, args) = unpack_spawn_payload(
            &mut gc,
            &encoded,
            &payload,
            &[receiver_type],
            &[],
            &[],
            &[],
            &runtime_types,
            |_, _| core::ptr::null_mut(),
        )
        .expect("raw receiver spawn payload");

        assert!(args.is_empty());
        assert_eq!(captures.len(), 2);
        assert_eq!(Gc::header(captures[0] as GcRef).kind(), ValueKind::String);
        assert_eq!(Gc::header(captures[1] as GcRef).kind(), ValueKind::String);
        assert_eq!(string::as_bytes(captures[0] as GcRef), b"left");
        assert_eq!(string::as_bytes(captures[1] as GcRef), b"right");
    }

    fn roundtrip_frame(target_island_id: u32, cmd: IslandCommand) {
        let source_island_id = 3;
        let encoded = encode_island_transport_frame(target_island_id, source_island_id, &cmd);
        let (decoded_target, decoded_source, decoded_cmd) =
            decode_island_transport_frame(&encoded).unwrap();
        assert_eq!(decoded_target, target_island_id);
        assert_eq!(decoded_source, source_island_id);
        match (cmd, decoded_cmd) {
            (
                IslandCommand::SpawnFiber { closure_data: a },
                IslandCommand::SpawnFiber { closure_data: b },
            ) => assert_eq!(a.data(), b.data()),
            (IslandCommand::WakeFiber { waiter: a }, IslandCommand::WakeFiber { waiter: b }) => {
                assert_eq!(a, b)
            }
            (IslandCommand::Shutdown, IslandCommand::Shutdown) => {}
            (
                IslandCommand::EndpointRequest {
                    endpoint_id: ae,
                    kind: ak,
                    from_island: af,
                    fiber_key: afi,
                    wait_id: aw,
                },
                IslandCommand::EndpointRequest {
                    endpoint_id: be,
                    kind: bk,
                    from_island: bf,
                    fiber_key: bfi,
                    wait_id: bw,
                },
            ) => {
                assert_eq!(ae, be);
                assert_eq!(af, bf);
                assert_eq!(afi, bfi);
                assert_eq!(aw, bw);
                assert_request_kind_eq(&ak, &bk);
            }
            (
                IslandCommand::EndpointResponse {
                    endpoint_id: ae,
                    kind: ak,
                    from_island: af,
                    fiber_key: afi,
                    wait_id: aw,
                },
                IslandCommand::EndpointResponse {
                    endpoint_id: be,
                    kind: bk,
                    from_island: bf,
                    fiber_key: bfi,
                    wait_id: bw,
                },
            ) => {
                assert_eq!(ae, be);
                assert_eq!(af, bf);
                assert_eq!(afi, bfi);
                assert_eq!(aw, bw);
                assert_response_kind_eq(&ak, &bk);
            }
            _ => panic!("decoded frame command variant mismatch"),
        }
    }

    fn assert_request_kind_eq(a: &EndpointRequestKind, b: &EndpointRequestKind) {
        match (a, b) {
            (EndpointRequestKind::Send { data: ad }, EndpointRequestKind::Send { data: bd }) => {
                assert_eq!(ad, bd)
            }
            (EndpointRequestKind::Recv, EndpointRequestKind::Recv) => {}
            (EndpointRequestKind::Close, EndpointRequestKind::Close) => {}
            (
                EndpointRequestKind::Transfer { new_peer: a },
                EndpointRequestKind::Transfer { new_peer: b },
            ) => assert_eq!(a, b),
            _ => panic!("request kind mismatch"),
        }
    }

    fn assert_response_kind_eq(a: &EndpointResponseKind, b: &EndpointResponseKind) {
        match (a, b) {
            (
                EndpointResponseKind::SendAck { closed: a },
                EndpointResponseKind::SendAck { closed: b },
            ) => assert_eq!(a, b),
            (
                EndpointResponseKind::RecvData {
                    data: ad,
                    closed: ac,
                },
                EndpointResponseKind::RecvData {
                    data: bd,
                    closed: bc,
                },
            ) => {
                assert_eq!(ad, bd);
                assert_eq!(ac, bc);
            }
            (EndpointResponseKind::Closed, EndpointResponseKind::Closed) => {}
            (EndpointResponseKind::RecvError, EndpointResponseKind::RecvError) => {}
            _ => panic!("response kind mismatch"),
        }
    }

    #[test]
    fn island_command_codec_roundtrips_all_variants() {
        roundtrip(IslandCommand::SpawnFiber {
            closure_data: PackedValue::from_data(vec![1, 2, 3, 4]),
        });
        roundtrip(IslandCommand::WakeFiber {
            waiter: QueueWaiter::simple(2, 0x0000_0002_0000_002a),
        });
        roundtrip(IslandCommand::WakeFiber {
            waiter: QueueWaiter::selecting(
                2,
                0x0000_0003_0000_002b,
                4,
                55,
                0x0000_00aa_0000_00bb,
                SelectWaitKind::Recv,
            ),
        });
        roundtrip(IslandCommand::Shutdown);
        roundtrip(IslandCommand::EndpointRequest {
            endpoint_id: 99,
            kind: EndpointRequestKind::Send {
                data: vec![7, 8, 9],
            },
            from_island: 3,
            fiber_key: 1234,
            wait_id: 1,
        });
        roundtrip(IslandCommand::EndpointRequest {
            endpoint_id: 100,
            kind: EndpointRequestKind::Recv,
            from_island: 4,
            fiber_key: 1235,
            wait_id: 2,
        });
        roundtrip(IslandCommand::EndpointRequest {
            endpoint_id: 101,
            kind: EndpointRequestKind::Close,
            from_island: 5,
            fiber_key: 1236,
            wait_id: 0,
        });
        roundtrip(IslandCommand::EndpointRequest {
            endpoint_id: 102,
            kind: EndpointRequestKind::Transfer { new_peer: 8 },
            from_island: 6,
            fiber_key: 1237,
            wait_id: 0,
        });
        roundtrip(IslandCommand::EndpointResponse {
            endpoint_id: 103,
            kind: EndpointResponseKind::SendAck { closed: true },
            from_island: 6,
            fiber_key: 2234,
            wait_id: 3,
        });
        roundtrip(IslandCommand::EndpointResponse {
            endpoint_id: 104,
            kind: EndpointResponseKind::RecvData {
                data: vec![10, 11],
                closed: false,
            },
            from_island: 6,
            fiber_key: 2235,
            wait_id: 4,
        });
        roundtrip(IslandCommand::EndpointResponse {
            endpoint_id: 105,
            kind: EndpointResponseKind::Closed,
            from_island: 6,
            fiber_key: 2236,
            wait_id: 0,
        });
        roundtrip(IslandCommand::EndpointResponse {
            endpoint_id: 106,
            kind: EndpointResponseKind::RecvError,
            from_island: 6,
            fiber_key: 2237,
            wait_id: 5,
        });
    }

    #[test]
    fn island_command_codec_rejects_trailing_bytes_and_preserves_queue_identity_060() {
        let waiter = QueueWaiter::simple_queue(
            2,
            0x0000_0002_0000_002a,
            0x0000_00aa_0000_00bb,
            SelectWaitKind::Recv,
        );
        let encoded = encode_island_command(&IslandCommand::WakeFiber {
            waiter: waiter.clone(),
        });
        let decoded = decode_island_command(&encoded).expect("decode simple queue wake");
        let IslandCommand::WakeFiber {
            waiter: decoded_waiter,
        } = decoded
        else {
            panic!("expected wake command");
        };
        assert_eq!(decoded_waiter.registration_id, waiter.registration_id);
        assert_eq!(decoded_waiter.queue_ref, waiter.queue_ref);
        assert_eq!(decoded_waiter.kind, waiter.kind);

        let commands = [
            IslandCommand::SpawnFiber {
                closure_data: PackedValue::from_data(vec![1, 2, 3, 4]),
            },
            IslandCommand::WakeFiber { waiter },
            IslandCommand::Shutdown,
            IslandCommand::EndpointRequest {
                endpoint_id: 99,
                kind: EndpointRequestKind::Send {
                    data: vec![7, 8, 9],
                },
                from_island: 3,
                fiber_key: 1234,
                wait_id: 1,
            },
            IslandCommand::EndpointResponse {
                endpoint_id: 104,
                kind: EndpointResponseKind::RecvData {
                    data: vec![10, 11],
                    closed: false,
                },
                from_island: 6,
                fiber_key: 2235,
                wait_id: 4,
            },
        ];

        for command in commands {
            let mut encoded = encode_island_command(&command);
            encoded.push(0xff);
            assert!(matches!(
                decode_island_command(&encoded),
                Err(IslandCommandDecodeError::TrailingBytes)
            ));

            let mut frame = encode_island_transport_frame(7, 3, &command);
            frame.push(0xff);
            assert!(matches!(
                decode_island_transport_frame(&frame),
                Err(IslandTransportFrameDecodeError::InvalidCommand(
                    IslandCommandDecodeError::TrailingBytes
                ))
            ));
        }
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
                fiber_key: 99,
                wait_id: 0,
            },
        );
    }

    #[test]
    fn island_command_codec_rejects_truncated_payload() {
        let encoded = encode_island_command(&IslandCommand::WakeFiber {
            waiter: QueueWaiter::simple(1, 7),
        });
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
