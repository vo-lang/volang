//! Island message encoding/decoding - single source of truth for cross-island protocol.
//!
//! This module defines the binary format for spawning fibers across islands.
//! Uses pack_slots/unpack_slots for proper deep copying of all sendable types.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::{Gc, GcRef};
use crate::gc_types::try_typed_write_barrier;
use crate::island::{EndpointRequestKind, EndpointResponseKind, IslandCommand};
use crate::objects::array;
use crate::objects::queue_state::{QueueWaiter, SelectInfo, SelectWaitKind};
use crate::pack::{
    pack_slots_with_named_type_metas_and_cache_limited,
    unpack_slots_expected_with_queue_handle_resolver_and_object_cache,
    validate_packed_slots_expected_with_named_type_metas_and_cache, PackObjectGraph,
    PackOutputError, PackTypeContext, PackedValue, QueueHandleInfo, UnpackObjectCache,
    ValidateObjectCache,
};
use crate::slot::{Slot, SLOT_BYTES};
use crate::ValueKind;
use crate::ValueMeta;
use crate::ValueRttid;
use vo_common_core::bytecode::{NamedTypeMeta, RuntimeTypeResolver, StructMeta};
use vo_common_core::RuntimeType;
use vo_common_core::SlotType;
use vo_common_core::TransferType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IslandMessageEncodeError {
    LengthOverflow {
        field: &'static str,
        len: usize,
        max: usize,
    },
    LayoutMismatch {
        field: &'static str,
        expected: usize,
        actual: usize,
    },
    InvalidLayout {
        field: &'static str,
    },
    SizeOverflow {
        field: &'static str,
    },
    AllocationFailed {
        field: &'static str,
        requested: usize,
    },
}

impl core::fmt::Display for IslandMessageEncodeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::LengthOverflow { field, len, max } => write!(
                f,
                "{field} length {len} exceeds the wire-format maximum {max}"
            ),
            Self::LayoutMismatch {
                field,
                expected,
                actual,
            } => write!(
                f,
                "{field} layout requires {expected} entries or slots, got {actual}"
            ),
            Self::InvalidLayout { field } => write!(f, "{field} has an invalid wire layout"),
            Self::SizeOverflow { field } => {
                write!(f, "{field} encoded size exceeds addressable memory")
            }
            Self::AllocationFailed { field, requested } => write!(
                f,
                "failed to reserve {requested} capacity units for {field} encoding"
            ),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for IslandMessageEncodeError {}

/// Read little-endian integers from byte slice at offset.
#[inline]
fn read_u16(data: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes(data[offset..offset + 2].try_into().unwrap())
}

#[inline]
fn read_u32(data: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes(data[offset..offset + 4].try_into().unwrap())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IslandCommandDecodeError {
    UnexpectedEof,
    InvalidTag,
    TrailingBytes,
    AllocationFailed { requested: usize },
}

impl core::fmt::Display for IslandCommandDecodeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::UnexpectedEof => f.write_str("unexpected end of island command"),
            Self::InvalidTag => f.write_str("invalid or reserved island command tag"),
            Self::TrailingBytes => f.write_str("trailing bytes after island command"),
            Self::AllocationFailed { requested } => write!(
                f,
                "failed to reserve {requested} bytes while decoding island command"
            ),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for IslandCommandDecodeError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IslandTransportFrameDecodeError {
    UnexpectedEof,
    InvalidCommand(IslandCommandDecodeError),
}

impl core::fmt::Display for IslandTransportFrameDecodeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::UnexpectedEof => f.write_str("unexpected end of island transport frame"),
            Self::InvalidCommand(error) => write!(f, "invalid island command: {error}"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for IslandTransportFrameDecodeError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpawnPayloadUnpackError {
    UnexpectedEof,
    TypeCountMismatch,
    LayoutMismatch,
    HeaderMismatch,
    TrailingBytes,
    SizeOverflow,
    AllocationFailed {
        field: &'static str,
        requested: usize,
    },
}

impl core::fmt::Display for SpawnPayloadUnpackError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::UnexpectedEof => f.write_str("unexpected end of spawn payload"),
            Self::TypeCountMismatch => f.write_str("spawn payload type count mismatch"),
            Self::LayoutMismatch => f.write_str("spawn payload type or slot layout mismatch"),
            Self::HeaderMismatch => f.write_str("spawn payload header does not match its bytes"),
            Self::TrailingBytes => f.write_str("trailing bytes after spawn payload"),
            Self::SizeOverflow => f.write_str("spawn payload slot count exceeds addressable memory"),
            Self::AllocationFailed { field, requested } => write!(
                f,
                "failed to reserve {requested} capacity units for {field} while unpacking spawn payload"
            ),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for SpawnPayloadUnpackError {}

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
    let end = offset
        .checked_add(4)
        .ok_or(IslandCommandDecodeError::UnexpectedEof)?;
    let bytes = data
        .get(*offset..end)
        .ok_or(IslandCommandDecodeError::UnexpectedEof)?;
    let value = u32::from_le_bytes(
        bytes
            .try_into()
            .map_err(|_| IslandCommandDecodeError::UnexpectedEof)?,
    );
    *offset = end;
    Ok(value)
}

fn read_u16_checked(data: &[u8], offset: &mut usize) -> Result<u16, IslandCommandDecodeError> {
    let end = offset
        .checked_add(2)
        .ok_or(IslandCommandDecodeError::UnexpectedEof)?;
    let bytes = data
        .get(*offset..end)
        .ok_or(IslandCommandDecodeError::UnexpectedEof)?;
    let value = u16::from_le_bytes(
        bytes
            .try_into()
            .map_err(|_| IslandCommandDecodeError::UnexpectedEof)?,
    );
    *offset = end;
    Ok(value)
}

fn read_u64_checked(data: &[u8], offset: &mut usize) -> Result<u64, IslandCommandDecodeError> {
    let end = offset
        .checked_add(8)
        .ok_or(IslandCommandDecodeError::UnexpectedEof)?;
    let bytes = data
        .get(*offset..end)
        .ok_or(IslandCommandDecodeError::UnexpectedEof)?;
    let value = u64::from_le_bytes(
        bytes
            .try_into()
            .map_err(|_| IslandCommandDecodeError::UnexpectedEof)?,
    );
    *offset = end;
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
    let end = offset
        .checked_add(len)
        .ok_or(IslandCommandDecodeError::UnexpectedEof)?;
    let bytes = data
        .get(*offset..end)
        .ok_or(IslandCommandDecodeError::UnexpectedEof)?;
    let mut out = Vec::new();
    out.try_reserve_exact(len)
        .map_err(|_| IslandCommandDecodeError::AllocationFailed { requested: len })?;
    out.extend_from_slice(bytes);
    *offset = end;
    Ok(out)
}

fn validate_variable_payload_extent(
    data: &[u8],
    offset: usize,
    payload_len: usize,
    trailing_len: usize,
) -> Result<(), IslandCommandDecodeError> {
    let payload_end = offset
        .checked_add(payload_len)
        .ok_or(IslandCommandDecodeError::UnexpectedEof)?;
    let command_end = payload_end
        .checked_add(trailing_len)
        .ok_or(IslandCommandDecodeError::UnexpectedEof)?;
    match command_end.cmp(&data.len()) {
        core::cmp::Ordering::Less => Err(IslandCommandDecodeError::TrailingBytes),
        core::cmp::Ordering::Equal => Ok(()),
        core::cmp::Ordering::Greater => Err(IslandCommandDecodeError::UnexpectedEof),
    }
}

pub const HEADER_SIZE: usize = 10;

#[inline]
fn wire_len_u16(len: usize, field: &'static str) -> Result<u16, IslandMessageEncodeError> {
    u16::try_from(len).map_err(|_| IslandMessageEncodeError::LengthOverflow {
        field,
        len,
        max: u16::MAX as usize,
    })
}

#[inline]
fn wire_len_u32(len: usize, field: &'static str) -> Result<u32, IslandMessageEncodeError> {
    u32::try_from(len).map_err(|_| IslandMessageEncodeError::LengthOverflow {
        field,
        len,
        max: u32::MAX as usize,
    })
}

#[inline]
fn checked_encoded_size(
    current: usize,
    additional: usize,
    field: &'static str,
) -> Result<usize, IslandMessageEncodeError> {
    current
        .checked_add(additional)
        .ok_or(IslandMessageEncodeError::SizeOverflow { field })
}

fn try_vec_with_capacity(
    capacity: usize,
    field: &'static str,
) -> Result<Vec<u8>, IslandMessageEncodeError> {
    let mut buf = Vec::new();
    buf.try_reserve_exact(capacity)
        .map_err(|_| IslandMessageEncodeError::AllocationFailed {
            field,
            requested: capacity,
        })?;
    Ok(buf)
}

fn reserve_append(
    buf: &mut Vec<u8>,
    additional: usize,
    field: &'static str,
) -> Result<(), IslandMessageEncodeError> {
    let requested = checked_encoded_size(buf.len(), additional, field)?;
    if buf.capacity().saturating_sub(buf.len()) >= additional {
        return Ok(());
    }
    let geometric = buf.capacity().checked_mul(2).unwrap_or(requested);
    let target_capacity = geometric.max(requested);
    let reserve = target_capacity
        .checked_sub(buf.len())
        .ok_or(IslandMessageEncodeError::SizeOverflow { field })?;
    buf.try_reserve_exact(reserve)
        .map_err(|_| IslandMessageEncodeError::AllocationFailed {
            field,
            requested: target_capacity,
        })
}

fn append_packed_chunk(
    buf: &mut Vec<u8>,
    packed_data: &[u8],
    field: &'static str,
) -> Result<(), IslandMessageEncodeError> {
    let len = wire_len_u32(packed_data.len(), field)?;
    let additional = checked_encoded_size(4, packed_data.len(), field)?;
    reserve_append(buf, additional, field)?;
    buf.extend_from_slice(&len.to_le_bytes());
    buf.extend_from_slice(packed_data);
    Ok(())
}

fn map_pack_output_error(error: PackOutputError, field: &'static str) -> IslandMessageEncodeError {
    match error {
        PackOutputError::LengthOverflow { attempted: len, .. } => {
            IslandMessageEncodeError::LengthOverflow {
                field,
                len,
                max: u32::MAX as usize,
            }
        }
        PackOutputError::AllocationFailed { requested } => {
            IslandMessageEncodeError::AllocationFailed { field, requested }
        }
    }
}

fn expected_argument_slots(
    param_types: &[TransferType],
) -> Result<usize, IslandMessageEncodeError> {
    param_types.iter().try_fold(0usize, |total, transfer_type| {
        checked_encoded_size(
            total,
            transfer_type.slots as usize,
            "spawn argument slot layout",
        )
    })
}

fn checked_encode_transfer_meta(
    transfer_type: TransferType,
    field: &'static str,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<ValueMeta, IslandMessageEncodeError> {
    let value_meta = ValueMeta::try_from_raw(transfer_type.meta_raw)
        .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
    let value_rttid = ValueRttid::try_from_raw(transfer_type.rttid_raw)
        .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
    if value_meta.try_value_kind() != value_rttid.try_value_kind() {
        return Err(IslandMessageEncodeError::InvalidLayout { field });
    }
    if matches!(
        value_meta.try_value_kind(),
        Some(ValueKind::Channel | ValueKind::Closure | ValueKind::Interface | ValueKind::Island)
    ) {
        return Err(IslandMessageEncodeError::InvalidLayout { field });
    }
    let resolver = RuntimeTypeResolver::new(struct_metas, named_type_metas, runtime_types);
    if resolver.canonical_value_meta_for_value_rttid(value_rttid) != Some(value_meta)
        || resolver.slot_count_for_value_rttid(value_rttid) != Some(transfer_type.slots as usize)
    {
        return Err(IslandMessageEncodeError::InvalidLayout { field });
    }
    validate_sendable_type_graph(value_rttid, field, struct_metas, runtime_types, resolver)?;
    Ok(value_meta)
}

fn checked_encode_transfer_metas(
    transfer_types: &[TransferType],
    field: &'static str,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<Vec<ValueMeta>, IslandMessageEncodeError> {
    let mut value_metas = Vec::new();
    value_metas
        .try_reserve_exact(transfer_types.len())
        .map_err(|_| IslandMessageEncodeError::AllocationFailed {
            field,
            requested: transfer_types.len(),
        })?;
    for &transfer_type in transfer_types {
        value_metas.push(checked_encode_transfer_meta(
            transfer_type,
            field,
            struct_metas,
            named_type_metas,
            runtime_types,
        )?);
    }
    Ok(value_metas)
}

fn validate_sendable_type_graph(
    root: ValueRttid,
    field: &'static str,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    resolver: RuntimeTypeResolver<'_>,
) -> Result<(), IslandMessageEncodeError> {
    fn enqueue(
        value_rttid: ValueRttid,
        field: &'static str,
        runtime_types: &[RuntimeType],
        seen: &mut [bool],
        pending: &mut Vec<ValueRttid>,
    ) -> Result<(), IslandMessageEncodeError> {
        let value_rttid = ValueRttid::try_from_raw(value_rttid.to_raw())
            .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
        let index = value_rttid.rttid() as usize;
        runtime_types
            .get(index)
            .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
        if !seen[index] {
            seen[index] = true;
            pending.push(value_rttid);
        }
        Ok(())
    }

    let type_count = runtime_types.len();
    let mut seen = Vec::new();
    seen.try_reserve_exact(type_count)
        .map_err(|_| IslandMessageEncodeError::AllocationFailed {
            field,
            requested: type_count,
        })?;
    seen.resize(type_count, false);
    let mut pending = Vec::new();
    pending.try_reserve_exact(type_count).map_err(|_| {
        IslandMessageEncodeError::AllocationFailed {
            field,
            requested: type_count,
        }
    })?;
    enqueue(root, field, runtime_types, &mut seen, &mut pending)?;

    while let Some(value_rttid) = pending.pop() {
        if resolver
            .canonical_value_meta_for_value_rttid(value_rttid)
            .is_none()
            || resolver.slot_count_for_value_rttid(value_rttid).is_none()
        {
            return Err(IslandMessageEncodeError::InvalidLayout { field });
        }
        let (resolved_rttid, runtime_type) = resolver
            .resolve_value_rttid(value_rttid)
            .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
        let resolved_index = resolved_rttid.rttid() as usize;
        if let Some(resolved_seen) = seen.get_mut(resolved_index) {
            *resolved_seen = true;
        } else {
            return Err(IslandMessageEncodeError::InvalidLayout { field });
        }

        let mut push = |nested| enqueue(nested, field, runtime_types, &mut seen, &mut pending);
        match runtime_type {
            RuntimeType::Basic(kind) => {
                if matches!(
                    kind,
                    ValueKind::Channel
                        | ValueKind::Closure
                        | ValueKind::Interface
                        | ValueKind::Island
                ) {
                    return Err(IslandMessageEncodeError::InvalidLayout { field });
                }
            }
            RuntimeType::Pointer(elem)
            | RuntimeType::Slice(elem)
            | RuntimeType::Array { elem, .. } => push(*elem)?,
            RuntimeType::Port { dir, elem } => {
                if *dir != vo_common_core::ChanDir::Send {
                    return Err(IslandMessageEncodeError::InvalidLayout { field });
                }
                push(*elem)?;
            }
            RuntimeType::Map { key, val } => {
                push(*key)?;
                push(*val)?;
            }
            RuntimeType::Struct { fields, meta_id } => {
                let physical = struct_metas
                    .get(*meta_id as usize)
                    .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
                if fields.len() != physical.fields.len() {
                    return Err(IslandMessageEncodeError::InvalidLayout { field });
                }
                let mut expected_offset = 0usize;
                for (identity_field, physical_field) in fields.iter().zip(&physical.fields) {
                    if identity_field.name != physical_field.name
                        || identity_field.typ != physical_field.type_info
                        || identity_field.embedded != physical_field.embedded
                        || identity_field.tag != physical_field.tag.as_deref().unwrap_or("")
                    {
                        return Err(IslandMessageEncodeError::InvalidLayout { field });
                    }
                    let field_layout = resolver
                        .slot_layout_for_value_rttid(physical_field.type_info)
                        .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
                    if physical_field.offset as usize != expected_offset
                        || physical_field.slot_count as usize != field_layout.len()
                    {
                        return Err(IslandMessageEncodeError::InvalidLayout { field });
                    }
                    let field_end = expected_offset
                        .checked_add(field_layout.len())
                        .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
                    if physical.slot_types.get(expected_offset..field_end)
                        != Some(field_layout.as_slice())
                    {
                        return Err(IslandMessageEncodeError::InvalidLayout { field });
                    }
                    expected_offset = field_end;
                    push(identity_field.typ)?;
                }
                let zero_size_workaround = expected_offset == 0
                    && physical.slot_types.as_slice() == [vo_common_core::SlotType::Value]
                    && !physical.fields.is_empty();
                if !physical.fields.is_empty()
                    && expected_offset != physical.slot_types.len()
                    && !zero_size_workaround
                {
                    return Err(IslandMessageEncodeError::InvalidLayout { field });
                }
            }
            RuntimeType::Chan { .. }
            | RuntimeType::Func { .. }
            | RuntimeType::Interface { .. }
            | RuntimeType::Tuple(_)
            | RuntimeType::Island
            | RuntimeType::Named { .. } => {
                return Err(IslandMessageEncodeError::InvalidLayout { field });
            }
        }
    }
    Ok(())
}

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

/// Physical storage expected by the destination closure for one logical
/// capture value.
///
/// Fixed arrays need this distinction because ordinary captured array
/// variables use canonical heap-array objects, while method-value wrappers
/// capture a private box containing the receiver's flattened slots.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum SpawnCaptureStorage {
    ValueSlots = 0,
    HeapArray = 1,
}

/// Borrowed capture descriptor used by the spawn encoder.
#[derive(Debug, Clone, Copy)]
pub struct SpawnCaptureValue<'a> {
    pub slots: Option<&'a [Slot]>,
    pub storage: SpawnCaptureStorage,
}

impl<'a> SpawnCaptureValue<'a> {
    #[inline]
    pub const fn value_slots(slots: Option<&'a [Slot]>) -> Self {
        Self {
            slots,
            storage: SpawnCaptureStorage::ValueSlots,
        }
    }

    #[inline]
    pub const fn heap_array(slots: &'a [Slot]) -> Self {
        Self {
            slots: Some(slots),
            storage: SpawnCaptureStorage::HeapArray,
        }
    }
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
) -> Result<Vec<u8>, IslandMessageEncodeError> {
    if capture_values.len() != capture_types.len() {
        return Err(IslandMessageEncodeError::LayoutMismatch {
            field: "spawn capture type count",
            expected: capture_values.len(),
            actual: capture_types.len(),
        });
    }
    let mut descriptors = Vec::new();
    descriptors
        .try_reserve_exact(capture_values.len())
        .map_err(|_| IslandMessageEncodeError::AllocationFailed {
            field: "spawn capture descriptors",
            requested: capture_values.len(),
        })?;
    for (value_slots, transfer_type) in capture_values.iter().zip(capture_types) {
        let storage = if value_slots.is_some()
            && ValueMeta::try_from_raw(transfer_type.meta_raw)
                .is_some_and(|meta| meta.value_kind() == ValueKind::Array)
        {
            SpawnCaptureStorage::HeapArray
        } else {
            SpawnCaptureStorage::ValueSlots
        };
        descriptors.push(SpawnCaptureValue {
            slots: value_slots.as_deref(),
            storage,
        });
    }
    encode_spawn_payload_from_capture_descriptors(
        gc,
        func_id,
        &descriptors,
        capture_types,
        args,
        param_types,
        struct_metas,
        named_type_metas,
        runtime_types,
    )
}

/// Encode spawn captures while preserving their physical destination storage.
#[allow(clippy::too_many_arguments)]
pub fn encode_spawn_payload_from_capture_descriptors(
    gc: &Gc,
    func_id: u32,
    capture_values: &[SpawnCaptureValue<'_>],
    capture_types: &[TransferType],
    args: &[Slot],
    param_types: &[TransferType],
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<Vec<u8>, IslandMessageEncodeError> {
    let capture_count = wire_len_u16(capture_values.len(), "spawn capture count")?;
    if capture_values.len() != capture_types.len() {
        return Err(IslandMessageEncodeError::LayoutMismatch {
            field: "spawn capture type count",
            expected: capture_values.len(),
            actual: capture_types.len(),
        });
    }
    let argument_count = wire_len_u16(param_types.len(), "spawn argument count")?;
    let expected_arg_slots = expected_argument_slots(param_types)?;
    if args.len() != expected_arg_slots {
        return Err(IslandMessageEncodeError::LayoutMismatch {
            field: "spawn argument slots",
            expected: expected_arg_slots,
            actual: args.len(),
        });
    }
    for (capture, &transfer_type) in capture_values.iter().zip(capture_types) {
        if let Some(value_slots) = capture.slots {
            let expected = transfer_type.slots as usize;
            if value_slots.len() != expected {
                return Err(IslandMessageEncodeError::LayoutMismatch {
                    field: "spawn capture slots",
                    expected,
                    actual: value_slots.len(),
                });
            }
        }
    }
    let capture_metas = checked_encode_transfer_metas(
        capture_types,
        "spawn capture transfer metadata",
        struct_metas,
        named_type_metas,
        runtime_types,
    )?;
    let argument_metas = checked_encode_transfer_metas(
        param_types,
        "spawn argument transfer metadata",
        struct_metas,
        named_type_metas,
        runtime_types,
    )?;
    for (capture, &value_meta) in capture_values.iter().zip(&capture_metas) {
        if capture.storage == SpawnCaptureStorage::HeapArray
            && (capture.slots.is_none() || value_meta.value_kind() != ValueKind::Array)
        {
            return Err(IslandMessageEncodeError::InvalidLayout {
                field: "spawn capture storage",
            });
        }
    }

    let mut buf = try_vec_with_capacity(HEADER_SIZE, "spawn payload")?;
    let mut object_graph = PackObjectGraph::default();

    buf.extend_from_slice(&func_id.to_le_bytes());
    buf.extend_from_slice(&capture_count.to_le_bytes());
    buf.extend_from_slice(&argument_count.to_le_bytes());
    buf.extend_from_slice(&0u16.to_le_bytes());

    for (capture, &value_meta) in capture_values.iter().zip(&capture_metas) {
        reserve_append(&mut buf, 1, "spawn capture storage")?;
        buf.push(capture.storage as u8);
        let Some(value_slots) = capture.slots else {
            reserve_append(&mut buf, 4, "nil packed capture")?;
            buf.extend_from_slice(&0u32.to_le_bytes());
            continue;
        };
        // Safety: capture slots and metadata are produced by verified bytecode.
        let packed = unsafe {
            pack_slots_with_named_type_metas_and_cache_limited(
                gc,
                value_slots,
                value_meta,
                struct_metas,
                named_type_metas,
                runtime_types,
                &mut object_graph,
                u32::MAX as usize,
            )
        }
        .map_err(|error| map_pack_output_error(error, "packed capture"))?;
        let packed_data = packed.data();
        append_packed_chunk(&mut buf, packed_data, "packed capture")?;
    }

    let mut arg_offset = 0usize;
    for (&transfer_type, &value_meta) in param_types.iter().zip(&argument_metas) {
        let slots_usize = transfer_type.slots as usize;
        let arg_end = checked_encoded_size(arg_offset, slots_usize, "spawn argument slots")?;
        let value_slots = &args[arg_offset..arg_end];
        // Safety: parameter slots and metadata come from the verified call layout.
        let packed = unsafe {
            pack_slots_with_named_type_metas_and_cache_limited(
                gc,
                value_slots,
                value_meta,
                struct_metas,
                named_type_metas,
                runtime_types,
                &mut object_graph,
                u32::MAX as usize,
            )
        }
        .map_err(|error| map_pack_output_error(error, "packed argument"))?;
        append_packed_chunk(&mut buf, packed.data(), "packed argument")?;
        arg_offset = arg_end;
    }

    Ok(buf)
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
) -> Result<Vec<u8>, IslandMessageEncodeError> {
    let expected_capture_slots = capture_type.slots as usize;
    if expected_capture_slots == 0 {
        return Err(IslandMessageEncodeError::InvalidLayout {
            field: "raw receiver capture",
        });
    }
    if raw_capture_slots.len() != expected_capture_slots {
        return Err(IslandMessageEncodeError::LayoutMismatch {
            field: "raw receiver capture slots",
            expected: expected_capture_slots,
            actual: raw_capture_slots.len(),
        });
    }
    let raw_capture_count = wire_len_u16(raw_capture_slots.len(), "raw receiver capture slots")?;
    let argument_count = wire_len_u16(param_types.len(), "spawn argument count")?;
    let expected_arg_slots = expected_argument_slots(param_types)?;
    if args.len() != expected_arg_slots {
        return Err(IslandMessageEncodeError::LayoutMismatch {
            field: "spawn argument slots",
            expected: expected_arg_slots,
            actual: args.len(),
        });
    }
    let capture_meta = checked_encode_transfer_meta(
        capture_type,
        "raw receiver capture transfer metadata",
        struct_metas,
        named_type_metas,
        runtime_types,
    )?;
    let argument_metas = checked_encode_transfer_metas(
        param_types,
        "spawn argument transfer metadata",
        struct_metas,
        named_type_metas,
        runtime_types,
    )?;

    let mut buf = try_vec_with_capacity(HEADER_SIZE, "spawn payload")?;
    let mut object_graph = PackObjectGraph::default();

    buf.extend_from_slice(&func_id.to_le_bytes());
    buf.extend_from_slice(&1u16.to_le_bytes());
    buf.extend_from_slice(&argument_count.to_le_bytes());
    buf.extend_from_slice(&raw_capture_count.to_le_bytes());

    // Safety: direct receiver captures are checked against `capture_type` above.
    let packed = unsafe {
        pack_slots_with_named_type_metas_and_cache_limited(
            gc,
            raw_capture_slots,
            capture_meta,
            struct_metas,
            named_type_metas,
            runtime_types,
            &mut object_graph,
            u32::MAX as usize,
        )
    }
    .map_err(|error| map_pack_output_error(error, "packed receiver"))?;
    append_packed_chunk(&mut buf, packed.data(), "packed receiver")?;

    let mut arg_offset = 0usize;
    for (&transfer_type, &value_meta) in param_types.iter().zip(&argument_metas) {
        let slots_usize = transfer_type.slots as usize;
        let arg_end = checked_encoded_size(arg_offset, slots_usize, "spawn argument slots")?;
        let value_slots = &args[arg_offset..arg_end];
        // Safety: parameter slots and metadata come from the verified call layout.
        let packed = unsafe {
            pack_slots_with_named_type_metas_and_cache_limited(
                gc,
                value_slots,
                value_meta,
                struct_metas,
                named_type_metas,
                runtime_types,
                &mut object_graph,
                u32::MAX as usize,
            )
        }
        .map_err(|error| map_pack_output_error(error, "packed argument"))?;
        append_packed_chunk(&mut buf, packed.data(), "packed argument")?;
        arg_offset = arg_end;
    }

    Ok(buf)
}

/// Decoded spawn fiber payload.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
) -> Result<Option<&'a [u8]>, SpawnPayloadUnpackError> {
    let len = read_u32_checked(data, offset).map_err(|_| SpawnPayloadUnpackError::UnexpectedEof)?
        as usize;
    if len == 0 {
        return Ok(None);
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

fn read_spawn_capture_storage(
    data: &[u8],
    offset: &mut usize,
    value_meta: ValueMeta,
) -> Result<SpawnCaptureStorage, SpawnPayloadUnpackError> {
    let tag = *data
        .get(*offset)
        .ok_or(SpawnPayloadUnpackError::UnexpectedEof)?;
    *offset = (*offset)
        .checked_add(1)
        .ok_or(SpawnPayloadUnpackError::UnexpectedEof)?;
    match tag {
        tag if tag == SpawnCaptureStorage::ValueSlots as u8 => Ok(SpawnCaptureStorage::ValueSlots),
        tag if tag == SpawnCaptureStorage::HeapArray as u8
            && value_meta.value_kind() == ValueKind::Array =>
        {
            Ok(SpawnCaptureStorage::HeapArray)
        }
        _ => Err(SpawnPayloadUnpackError::LayoutMismatch),
    }
}

fn checked_transfer_metadata(
    transfer_type: TransferType,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<(ValueMeta, ValueRttid), SpawnPayloadUnpackError> {
    let value_meta = ValueMeta::try_from_raw(transfer_type.meta_raw)
        .ok_or(SpawnPayloadUnpackError::LayoutMismatch)?;
    let value_rttid = ValueRttid::try_from_raw(transfer_type.rttid_raw)
        .ok_or(SpawnPayloadUnpackError::LayoutMismatch)?;
    if value_meta.try_value_kind() != value_rttid.try_value_kind() {
        return Err(SpawnPayloadUnpackError::LayoutMismatch);
    }
    let resolver = RuntimeTypeResolver::new(struct_metas, named_type_metas, runtime_types);
    if resolver.canonical_value_meta_for_value_rttid(value_rttid) != Some(value_meta)
        || resolver.slot_count_for_value_rttid(value_rttid) != Some(transfer_type.slots as usize)
    {
        return Err(SpawnPayloadUnpackError::LayoutMismatch);
    }
    validate_sendable_type_graph(
        value_rttid,
        "spawn transfer metadata",
        struct_metas,
        runtime_types,
        resolver,
    )
    .map_err(|error| match error {
        IslandMessageEncodeError::AllocationFailed { requested, .. } => {
            SpawnPayloadUnpackError::AllocationFailed {
                field: "spawn transfer type validation",
                requested,
            }
        }
        IslandMessageEncodeError::SizeOverflow { .. } => SpawnPayloadUnpackError::SizeOverflow,
        IslandMessageEncodeError::LengthOverflow { .. }
        | IslandMessageEncodeError::LayoutMismatch { .. }
        | IslandMessageEncodeError::InvalidLayout { .. } => SpawnPayloadUnpackError::LayoutMismatch,
    })?;
    Ok((value_meta, value_rttid))
}

#[allow(clippy::too_many_arguments)]
fn validate_spawn_packed_chunk(
    packed_data: &[u8],
    value_meta: ValueMeta,
    value_rttid: ValueRttid,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
    validate_object_cache: &mut ValidateObjectCache,
) -> Result<(), SpawnPayloadUnpackError> {
    validate_packed_slots_expected_with_named_type_metas_and_cache(
        packed_data,
        value_meta,
        value_rttid,
        struct_metas,
        named_type_metas,
        runtime_types,
        validate_object_cache,
    )
    .map_err(|_| SpawnPayloadUnpackError::LayoutMismatch)?;
    Ok(())
}

struct ValidatedSpawnChunk<'a> {
    data: &'a [u8],
    value_meta: ValueMeta,
    slot_start: usize,
    slot_end: usize,
}

struct ValidatedBoxedCaptureChunk<'a> {
    data: Option<&'a [u8]>,
    value_meta: ValueMeta,
    storage: SpawnCaptureStorage,
}

enum ValidatedCapturePlan<'a> {
    Raw {
        data: &'a [u8],
        value_meta: ValueMeta,
    },
    Boxed(Vec<ValidatedBoxedCaptureChunk<'a>>),
}

struct ValidatedSpawnPlan<'a> {
    captures: ValidatedCapturePlan<'a>,
    args: Vec<ValidatedSpawnChunk<'a>>,
    total_arg_slots: usize,
}

fn try_reserve_spawn_plan<T>(
    values: &mut Vec<T>,
    len: usize,
    field: &'static str,
) -> Result<(), SpawnPayloadUnpackError> {
    values
        .try_reserve_exact(len)
        .map_err(|_| SpawnPayloadUnpackError::AllocationFailed {
            field,
            requested: len,
        })
}

#[allow(clippy::too_many_arguments)]
fn validate_spawn_payload_chunks<'a>(
    data: &'a [u8],
    payload: &SpawnPayload,
    capture_types: &[TransferType],
    param_types: &[TransferType],
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<ValidatedSpawnPlan<'a>, SpawnPayloadUnpackError> {
    let mut offset = payload.data_offset;
    let mut validate_object_cache = ValidateObjectCache::default();

    let captures = if payload.raw_capture_slots > 0 {
        let transfer_type = capture_types[0];
        let packed_data = read_spawn_packed_chunk(data, &mut offset)?
            .ok_or(SpawnPayloadUnpackError::LayoutMismatch)?;
        let (value_meta, value_rttid) = checked_transfer_metadata(
            transfer_type,
            struct_metas,
            named_type_metas,
            runtime_types,
        )?;
        validate_spawn_packed_chunk(
            packed_data,
            value_meta,
            value_rttid,
            struct_metas,
            named_type_metas,
            runtime_types,
            &mut validate_object_cache,
        )?;
        ValidatedCapturePlan::Raw {
            data: packed_data,
            value_meta,
        }
    } else {
        let mut chunks = Vec::new();
        try_reserve_spawn_plan(&mut chunks, capture_types.len(), "spawn capture plan")?;
        for &transfer_type in capture_types {
            let (value_meta, value_rttid) = checked_transfer_metadata(
                transfer_type,
                struct_metas,
                named_type_metas,
                runtime_types,
            )?;
            let storage = read_spawn_capture_storage(data, &mut offset, value_meta)?;
            let data = read_spawn_packed_chunk(data, &mut offset)?;
            if data.is_none() && storage != SpawnCaptureStorage::ValueSlots {
                return Err(SpawnPayloadUnpackError::LayoutMismatch);
            }
            if let Some(packed_data) = data {
                validate_spawn_packed_chunk(
                    packed_data,
                    value_meta,
                    value_rttid,
                    struct_metas,
                    named_type_metas,
                    runtime_types,
                    &mut validate_object_cache,
                )?;
            }
            chunks.push(ValidatedBoxedCaptureChunk {
                data,
                value_meta,
                storage,
            });
        }
        ValidatedCapturePlan::Boxed(chunks)
    };

    let mut args = Vec::new();
    try_reserve_spawn_plan(&mut args, param_types.len(), "spawn argument plan")?;
    let mut arg_slot_offset = 0usize;
    for &transfer_type in param_types {
        let (value_meta, value_rttid) = checked_transfer_metadata(
            transfer_type,
            struct_metas,
            named_type_metas,
            runtime_types,
        )?;
        let packed_data = read_spawn_packed_chunk(data, &mut offset)?
            .ok_or(SpawnPayloadUnpackError::LayoutMismatch)?;
        validate_spawn_packed_chunk(
            packed_data,
            value_meta,
            value_rttid,
            struct_metas,
            named_type_metas,
            runtime_types,
            &mut validate_object_cache,
        )?;
        let arg_slot_end = arg_slot_offset
            .checked_add(transfer_type.slots as usize)
            .ok_or(SpawnPayloadUnpackError::SizeOverflow)?;
        args.push(ValidatedSpawnChunk {
            data: packed_data,
            value_meta,
            slot_start: arg_slot_offset,
            slot_end: arg_slot_end,
        });
        arg_slot_offset = arg_slot_end;
    }

    if offset != data.len() {
        return Err(SpawnPayloadUnpackError::TrailingBytes);
    }
    Ok(ValidatedSpawnPlan {
        captures,
        args,
        total_arg_slots: arg_slot_offset,
    })
}

fn try_zeroed_slots(len: usize, field: &'static str) -> Result<Vec<u64>, SpawnPayloadUnpackError> {
    let mut slots = Vec::new();
    slots
        .try_reserve_exact(len)
        .map_err(|_| SpawnPayloadUnpackError::AllocationFailed {
            field,
            requested: len,
        })?;
    slots.resize(len, 0);
    Ok(slots)
}

fn try_reserve_slots(
    slots: &mut Vec<u64>,
    len: usize,
    field: &'static str,
) -> Result<(), SpawnPayloadUnpackError> {
    slots
        .try_reserve_exact(len)
        .map_err(|_| SpawnPayloadUnpackError::AllocationFailed {
            field,
            requested: len,
        })
}

struct BoxedArrayCaptureLayout {
    len: usize,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    elem_slots: usize,
    elem_slot_types: Vec<SlotType>,
}

fn canonical_heap_element_bytes(
    elem_meta: ValueMeta,
    elem_slots: usize,
) -> Result<usize, SpawnPayloadUnpackError> {
    let packed_bytes = match elem_meta.value_kind() {
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => Some(1),
        ValueKind::Int16 | ValueKind::Uint16 => Some(2),
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => Some(4),
        _ => None,
    };
    if let Some(elem_bytes) = packed_bytes {
        return (elem_slots == 1)
            .then_some(elem_bytes)
            .ok_or(SpawnPayloadUnpackError::LayoutMismatch);
    }
    elem_slots
        .checked_mul(SLOT_BYTES)
        .ok_or(SpawnPayloadUnpackError::SizeOverflow)
}

fn boxed_array_capture_layout(
    transfer_type: TransferType,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<Option<BoxedArrayCaptureLayout>, SpawnPayloadUnpackError> {
    let value_meta = ValueMeta::try_from_raw(transfer_type.meta_raw)
        .ok_or(SpawnPayloadUnpackError::LayoutMismatch)?;
    if value_meta.value_kind() != ValueKind::Array {
        return Ok(None);
    }
    let value_rttid = ValueRttid::try_from_raw(transfer_type.rttid_raw)
        .ok_or(SpawnPayloadUnpackError::LayoutMismatch)?;
    let resolver = RuntimeTypeResolver::new(struct_metas, named_type_metas, runtime_types);
    let (_, runtime_type) = resolver
        .resolve_value_rttid(value_rttid)
        .ok_or(SpawnPayloadUnpackError::LayoutMismatch)?;
    let RuntimeType::Array {
        len,
        elem: elem_rttid,
    } = runtime_type
    else {
        return Err(SpawnPayloadUnpackError::LayoutMismatch);
    };
    let len = usize::try_from(*len).map_err(|_| SpawnPayloadUnpackError::SizeOverflow)?;
    if len > isize::MAX as usize {
        return Err(SpawnPayloadUnpackError::SizeOverflow);
    }
    let elem_meta = resolver
        .canonical_value_meta_for_value_rttid(*elem_rttid)
        .ok_or(SpawnPayloadUnpackError::LayoutMismatch)?;
    let elem_slots = resolver
        .slot_count_for_value_rttid(*elem_rttid)
        .ok_or(SpawnPayloadUnpackError::LayoutMismatch)?;
    let total_slots = len
        .checked_mul(elem_slots)
        .ok_or(SpawnPayloadUnpackError::SizeOverflow)?;
    if total_slots != transfer_type.slots as usize {
        return Err(SpawnPayloadUnpackError::LayoutMismatch);
    }
    let elem_slot_types = resolver
        .slot_layout_for_value_rttid(*elem_rttid)
        .ok_or(SpawnPayloadUnpackError::LayoutMismatch)?;
    if elem_slot_types.len() != elem_slots {
        return Err(SpawnPayloadUnpackError::LayoutMismatch);
    }
    let elem_bytes = canonical_heap_element_bytes(elem_meta, elem_slots)?;
    if u32::try_from(elem_bytes).is_err() {
        return Err(SpawnPayloadUnpackError::SizeOverflow);
    }
    Ok(Some(BoxedArrayCaptureLayout {
        len,
        elem_meta,
        elem_bytes,
        elem_slots,
        elem_slot_types,
    }))
}

fn capture_slot_layout(
    transfer_type: TransferType,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<Vec<SlotType>, SpawnPayloadUnpackError> {
    let value_rttid = ValueRttid::try_from_raw(transfer_type.rttid_raw)
        .ok_or(SpawnPayloadUnpackError::LayoutMismatch)?;
    let layout = RuntimeTypeResolver::new(struct_metas, named_type_metas, runtime_types)
        .slot_layout_for_value_rttid(value_rttid)
        .ok_or(SpawnPayloadUnpackError::LayoutMismatch)?;
    if layout.len() != transfer_type.slots as usize {
        return Err(SpawnPayloadUnpackError::LayoutMismatch);
    }
    Ok(layout)
}

fn materialize_boxed_array_capture(
    gc: &mut Gc,
    value_slots: &[u64],
    layout: BoxedArrayCaptureLayout,
) -> Result<GcRef, SpawnPayloadUnpackError> {
    let expected_slots = layout
        .len
        .checked_mul(layout.elem_slots)
        .ok_or(SpawnPayloadUnpackError::SizeOverflow)?;
    if value_slots.len() != expected_slots {
        return Err(SpawnPayloadUnpackError::LayoutMismatch);
    }
    let requested = layout
        .len
        .checked_mul(layout.elem_bytes)
        .and_then(|bytes| bytes.checked_add(array::HEADER_SLOTS * SLOT_BYTES))
        .ok_or(SpawnPayloadUnpackError::SizeOverflow)?;
    let array_ref = array::create(gc, layout.elem_meta, layout.elem_bytes, layout.len);
    if array_ref.is_null() {
        return Err(SpawnPayloadUnpackError::AllocationFailed {
            field: "boxed Array capture",
            requested,
        });
    }
    if layout.elem_slots != 0 {
        for elem in value_slots.chunks_exact(layout.elem_slots) {
            try_typed_write_barrier(gc, array_ref, elem, &layout.elem_slot_types)
                .map_err(|_| SpawnPayloadUnpackError::LayoutMismatch)?;
        }
    }
    unsafe { array::write_value_flat(array_ref, value_slots) }
        .map_err(|_| SpawnPayloadUnpackError::LayoutMismatch)?;
    gc.mark_allocated_for_scan(array_ref);
    Ok(array_ref)
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
    let decoded_payload = decode_spawn_header(data)?;
    if decoded_payload != *payload {
        return Err(SpawnPayloadUnpackError::HeaderMismatch);
    }
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

    if payload.raw_capture_slots > 0 && capture_types[0].slots != payload.raw_capture_slots {
        return Err(SpawnPayloadUnpackError::LayoutMismatch);
    }

    // Validate every chunk and the exact end of the payload before allocating
    // destination objects or invoking the queue-handle resolver.
    let validated_plan = validate_spawn_payload_chunks(
        data,
        payload,
        capture_types,
        param_types,
        struct_metas,
        named_type_metas,
        runtime_types,
    )?;
    let ValidatedSpawnPlan {
        captures: validated_captures,
        args: validated_args,
        total_arg_slots,
    } = validated_plan;

    let capture_slots = if payload.raw_capture_slots > 0 {
        payload.raw_capture_slots as usize
    } else {
        payload.num_captures as usize
    };
    let mut captures = Vec::new();
    try_reserve_slots(&mut captures, capture_slots, "spawn captures")?;
    let mut args = Vec::new();
    try_reserve_slots(&mut args, total_arg_slots, "spawn arguments")?;
    args.resize(total_arg_slots, 0);
    let max_boxed_capture_slots = if payload.raw_capture_slots == 0 {
        capture_types
            .iter()
            .map(|transfer_type| transfer_type.slots as usize)
            .max()
            .unwrap_or(0)
    } else {
        0
    };
    let mut capture_scratch =
        try_zeroed_slots(max_boxed_capture_slots, "boxed spawn capture scratch")?;
    let mut queue_handle_cache = Default::default();
    let mut unpack_object_cache = UnpackObjectCache::default();

    match validated_captures {
        ValidatedCapturePlan::Raw {
            data: packed_data,
            value_meta,
        } => {
            captures.resize(payload.raw_capture_slots as usize, 0);
            let mut cursor = 0;
            // Safety: phase one validated the complete payload, this chunk,
            // and the exact destination width before any GC allocation.
            unsafe {
                unpack_slots_expected_with_queue_handle_resolver_and_object_cache(
                    gc,
                    packed_data,
                    &mut cursor,
                    &mut captures,
                    value_meta,
                    PackTypeContext::with_named_types(
                        struct_metas,
                        named_type_metas,
                        runtime_types,
                    ),
                    &mut queue_handle_cache,
                    &mut unpack_object_cache,
                    &mut resolve_queue_handle,
                )
            };
        }
        ValidatedCapturePlan::Boxed(chunks) => {
            for (&transfer_type, chunk) in capture_types.iter().zip(chunks) {
                let Some(packed_data) = chunk.data else {
                    captures.push(0);
                    continue;
                };
                let value_slots = &mut capture_scratch[..transfer_type.slots as usize];
                value_slots.fill(0);
                let mut cursor = 0;
                // Safety: phase one validated the complete payload, this chunk,
                // and the exact destination width before any GC allocation.
                unsafe {
                    unpack_slots_expected_with_queue_handle_resolver_and_object_cache(
                        gc,
                        packed_data,
                        &mut cursor,
                        value_slots,
                        chunk.value_meta,
                        PackTypeContext::with_named_types(
                            struct_metas,
                            named_type_metas,
                            runtime_types,
                        ),
                        &mut queue_handle_cache,
                        &mut unpack_object_cache,
                        &mut resolve_queue_handle,
                    )
                };

                let heap_array_layout = if chunk.storage == SpawnCaptureStorage::HeapArray {
                    Some(
                        boxed_array_capture_layout(
                            transfer_type,
                            struct_metas,
                            named_type_metas,
                            runtime_types,
                        )?
                        .ok_or(SpawnPayloadUnpackError::LayoutMismatch)?,
                    )
                } else {
                    None
                };
                if let Some(layout) = heap_array_layout {
                    let array_ref = materialize_boxed_array_capture(gc, value_slots, layout)?;
                    captures.push(array_ref as u64);
                    continue;
                }

                let slot_layout = capture_slot_layout(
                    transfer_type,
                    struct_metas,
                    named_type_metas,
                    runtime_types,
                )?;
                let box_ref = alloc_capture_box(gc, chunk.value_meta, transfer_type.slots);
                if box_ref.is_null() {
                    return Err(SpawnPayloadUnpackError::AllocationFailed {
                        field: "boxed spawn capture",
                        requested: value_slots.len(),
                    });
                }
                try_typed_write_barrier(gc, box_ref, value_slots, &slot_layout)
                    .map_err(|_| SpawnPayloadUnpackError::LayoutMismatch)?;
                for (j, &slot) in value_slots.iter().enumerate() {
                    unsafe { Gc::write_slot(box_ref, j, slot) };
                }
                gc.mark_allocated_for_scan(box_ref);
                captures.push(box_ref as u64);
            }
        }
    }

    for chunk in validated_args {
        let value_slots = &mut args[chunk.slot_start..chunk.slot_end];
        let mut cursor = 0;
        // Safety: the packed chunk and destination layout were validated above.
        unsafe {
            unpack_slots_expected_with_queue_handle_resolver_and_object_cache(
                gc,
                chunk.data,
                &mut cursor,
                value_slots,
                chunk.value_meta,
                PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types),
                &mut queue_handle_cache,
                &mut unpack_object_cache,
                &mut resolve_queue_handle,
            )
        };
    }

    Ok((captures, args))
}

fn queue_waiter_encoded_len(waiter: &QueueWaiter) -> usize {
    // Five identity fields, two option tags, and the optional payloads.
    36 + 1 + usize::from(waiter.kind.is_some()) + 1 + if waiter.select.is_some() { 19 } else { 0 }
}

fn island_command_encoded_len(cmd: &IslandCommand) -> Result<usize, IslandMessageEncodeError> {
    let size = match cmd {
        IslandCommand::SpawnFiber { closure_data } => {
            wire_len_u32(closure_data.data().len(), "spawn command payload")?;
            checked_encoded_size(5, closure_data.data().len(), "island command")?
        }
        IslandCommand::WakeFiber { waiter } => {
            checked_encoded_size(1, queue_waiter_encoded_len(waiter), "island command")?
        }
        IslandCommand::Shutdown => 1,
        IslandCommand::EndpointRequest { kind, .. } => {
            let kind_len = match kind {
                EndpointRequestKind::Send { data } => {
                    wire_len_u32(data.len(), "endpoint send payload")?;
                    checked_encoded_size(5, data.len(), "endpoint request")?
                }
                EndpointRequestKind::Recv | EndpointRequestKind::Close => 1,
                EndpointRequestKind::Transfer { .. } => 5,
            };
            checked_encoded_size(29, kind_len, "island command")?
        }
        IslandCommand::EndpointResponse { kind, .. } => {
            let kind_len = match kind {
                EndpointResponseKind::SendAck { .. } => 2,
                EndpointResponseKind::RecvData { data, .. } => {
                    wire_len_u32(data.len(), "endpoint receive payload")?;
                    checked_encoded_size(6, data.len(), "endpoint response")?
                }
                EndpointResponseKind::Closed | EndpointResponseKind::RecvError => 1,
            };
            checked_encoded_size(29, kind_len, "island command")?
        }
    };
    Ok(size)
}

pub fn encode_island_command(cmd: &IslandCommand) -> Result<Vec<u8>, IslandMessageEncodeError> {
    let encoded_len = island_command_encoded_len(cmd)?;
    let mut buf = try_vec_with_capacity(encoded_len, "island command")?;
    encode_island_command_into(&mut buf, cmd)?;
    debug_assert_eq!(buf.len(), encoded_len);
    Ok(buf)
}

fn encode_island_command_into(
    buf: &mut Vec<u8>,
    cmd: &IslandCommand,
) -> Result<(), IslandMessageEncodeError> {
    match cmd {
        IslandCommand::SpawnFiber { closure_data } => {
            buf.push(0);
            let data = closure_data.data();
            buf.extend_from_slice(
                &wire_len_u32(data.len(), "spawn command payload")?.to_le_bytes(),
            );
            buf.extend_from_slice(data);
        }
        IslandCommand::WakeFiber { waiter } => {
            buf.push(1);
            encode_queue_waiter(buf, waiter);
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
            encode_endpoint_request_kind(buf, kind)?;
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
            encode_endpoint_response_kind(buf, kind)?;
            buf.extend_from_slice(&from_island.to_le_bytes());
            buf.extend_from_slice(&fiber_key.to_le_bytes());
            buf.extend_from_slice(&wait_id.to_le_bytes());
        }
    }
    Ok(())
}

pub fn encode_island_transport_frame(
    target_island_id: u32,
    source_island_id: u32,
    cmd: &IslandCommand,
) -> Result<Vec<u8>, IslandMessageEncodeError> {
    let command_len = island_command_encoded_len(cmd)?;
    let encoded_len = checked_encoded_size(8, command_len, "island transport frame")?;
    let mut buf = try_vec_with_capacity(encoded_len, "island transport frame")?;
    buf.extend_from_slice(&target_island_id.to_le_bytes());
    buf.extend_from_slice(&source_island_id.to_le_bytes());
    encode_island_command_into(&mut buf, cmd)?;
    debug_assert_eq!(buf.len(), encoded_len);
    Ok(buf)
}

fn encode_endpoint_request_kind(
    buf: &mut Vec<u8>,
    kind: &EndpointRequestKind,
) -> Result<(), IslandMessageEncodeError> {
    match kind {
        EndpointRequestKind::Send { data } => {
            buf.push(0);
            buf.extend_from_slice(
                &wire_len_u32(data.len(), "endpoint send payload")?.to_le_bytes(),
            );
            buf.extend_from_slice(data);
        }
        EndpointRequestKind::Recv => buf.push(1),
        EndpointRequestKind::Close => buf.push(2),
        EndpointRequestKind::Transfer { new_peer } => {
            buf.push(3);
            buf.extend_from_slice(&new_peer.to_le_bytes());
        }
    }
    Ok(())
}

fn encode_endpoint_response_kind(
    buf: &mut Vec<u8>,
    kind: &EndpointResponseKind,
) -> Result<(), IslandMessageEncodeError> {
    match kind {
        EndpointResponseKind::SendAck { closed } => {
            buf.push(0);
            buf.push(u8::from(*closed));
        }
        EndpointResponseKind::RecvData { data, closed } => {
            buf.push(1);
            buf.push(u8::from(*closed));
            buf.extend_from_slice(
                &wire_len_u32(data.len(), "endpoint receive payload")?.to_le_bytes(),
            );
            buf.extend_from_slice(data);
        }
        EndpointResponseKind::Closed => buf.push(2),
        EndpointResponseKind::RecvError => buf.push(3),
    }
    Ok(())
}

pub fn decode_island_command(data: &[u8]) -> Result<IslandCommand, IslandCommandDecodeError> {
    let mut offset = 0usize;
    let tag = read_byte(data, &mut offset)?;
    let command = match tag {
        0 => {
            let len = read_u32_checked(data, &mut offset)? as usize;
            validate_variable_payload_extent(data, offset, len, 0)?;
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
            validate_variable_payload_extent(data, *offset, len, 20)?;
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
            validate_variable_payload_extent(data, *offset, len, 20)?;
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
    use crate::gc_types::{ClosureScanLayout, GcScanContext};
    use crate::objects::{array, queue, string};
    use crate::test_support::trace_object_children_with_context;

    #[test]
    fn checked_readers_reject_offset_arithmetic_overflow() {
        let mut offset = usize::MAX;
        assert_eq!(
            read_u16_checked(&[], &mut offset),
            Err(IslandCommandDecodeError::UnexpectedEof)
        );
        assert_eq!(offset, usize::MAX);

        assert_eq!(
            read_u32_checked(&[], &mut offset),
            Err(IslandCommandDecodeError::UnexpectedEof)
        );
        assert_eq!(offset, usize::MAX);

        assert_eq!(
            read_u64_checked(&[], &mut offset),
            Err(IslandCommandDecodeError::UnexpectedEof)
        );
        assert_eq!(offset, usize::MAX);

        assert_eq!(
            read_bytes(&[], &mut offset, 1),
            Err(IslandCommandDecodeError::UnexpectedEof)
        );
        assert_eq!(offset, usize::MAX);
    }

    #[test]
    fn wire_lengths_reject_values_outside_the_encoded_domain() {
        assert_eq!(wire_len_u16(u16::MAX as usize, "test"), Ok(u16::MAX));
        assert!(matches!(
            wire_len_u16(u16::MAX as usize + 1, "test"),
            Err(IslandMessageEncodeError::LengthOverflow {
                field: "test",
                len,
                max
            }) if len == u16::MAX as usize + 1 && max == u16::MAX as usize
        ));

        assert_eq!(wire_len_u32(u32::MAX as usize, "test"), Ok(u32::MAX));
        #[cfg(target_pointer_width = "64")]
        assert!(matches!(
            wire_len_u32(u32::MAX as usize + 1, "test"),
            Err(IslandMessageEncodeError::LengthOverflow {
                field: "test",
                len,
                max
            }) if len == u32::MAX as usize + 1 && max == u32::MAX as usize
        ));
    }

    #[test]
    fn spawn_encoder_rejects_noncanonical_metadata_and_layout_without_panicking() {
        let gc = Gc::new();
        let runtime_types = vec![RuntimeType::Basic(ValueKind::Int64)];
        let int_type = TransferType {
            meta_raw: ValueMeta::new(0, ValueKind::Int64).to_raw(),
            rttid_raw: ValueRttid::new(0, ValueKind::Int64).to_raw(),
            slots: 1,
        };

        assert!(matches!(
            encode_spawn_payload_from_capture_values(
                &gc,
                1,
                &[Some(vec![7])],
                &[],
                &[],
                &[],
                &[],
                &[],
                &runtime_types,
            ),
            Err(IslandMessageEncodeError::LayoutMismatch {
                field: "spawn capture type count",
                ..
            })
        ));
        assert!(matches!(
            encode_spawn_payload_from_capture_values(
                &gc,
                1,
                &[],
                &[],
                &[],
                &[int_type],
                &[],
                &[],
                &runtime_types,
            ),
            Err(IslandMessageEncodeError::LayoutMismatch {
                field: "spawn argument slots",
                ..
            })
        ));

        let invalid_kind = TransferType {
            meta_raw: 0xff,
            rttid_raw: 0xff,
            slots: 1,
        };
        assert!(matches!(
            encode_spawn_payload_from_capture_values(
                &gc,
                1,
                &[Some(vec![7])],
                &[invalid_kind],
                &[],
                &[],
                &[],
                &[],
                &runtime_types,
            ),
            Err(IslandMessageEncodeError::InvalidLayout {
                field: "spawn capture transfer metadata"
            })
        ));

        let reserved_meta = TransferType {
            meta_raw: 0xffff_ff00 | ValueKind::Int64 as u32,
            ..int_type
        };
        assert!(matches!(
            encode_spawn_payload_from_capture_values(
                &gc,
                1,
                &[Some(vec![7])],
                &[reserved_meta],
                &[],
                &[],
                &[],
                &[],
                &runtime_types,
            ),
            Err(IslandMessageEncodeError::InvalidLayout { .. })
        ));

        let wrong_slots = TransferType {
            slots: 2,
            ..int_type
        };
        assert!(matches!(
            encode_spawn_payload_from_capture_values(
                &gc,
                1,
                &[Some(vec![7, 8])],
                &[wrong_slots],
                &[],
                &[],
                &[],
                &[],
                &runtime_types,
            ),
            Err(IslandMessageEncodeError::InvalidLayout { .. })
        ));

        let mut object_graph = PackObjectGraph::default();
        let limited = unsafe {
            pack_slots_with_named_type_metas_and_cache_limited(
                &gc,
                &[7],
                ValueMeta::new(0, ValueKind::Int64),
                &[],
                &[],
                &runtime_types,
                &mut object_graph,
                4,
            )
        };
        assert!(matches!(
            limited,
            Err(PackOutputError::LengthOverflow {
                limit: 4,
                attempted: 9
            })
        ));

        let channel_rttid = ValueRttid::new(1, ValueKind::Channel);
        let nested_non_sendable_runtime_types = vec![
            RuntimeType::Basic(ValueKind::Int64),
            RuntimeType::Chan {
                dir: vo_common_core::ChanDir::Both,
                elem: ValueRttid::new(0, ValueKind::Int64),
            },
            RuntimeType::Array {
                len: 1,
                elem: channel_rttid,
            },
        ];
        let nested_channel_array = TransferType {
            meta_raw: ValueMeta::new(2, ValueKind::Array).to_raw(),
            rttid_raw: ValueRttid::new(2, ValueKind::Array).to_raw(),
            slots: 1,
        };
        assert!(matches!(
            encode_spawn_payload_from_capture_values(
                &gc,
                1,
                &[Some(vec![0])],
                &[nested_channel_array],
                &[],
                &[],
                &[],
                &[],
                &nested_non_sendable_runtime_types,
            ),
            Err(IslandMessageEncodeError::InvalidLayout { .. })
        ));

        for dir in [vo_common_core::ChanDir::Both, vo_common_core::ChanDir::Recv] {
            let runtime_types = vec![
                RuntimeType::Basic(ValueKind::Int64),
                RuntimeType::Port {
                    dir,
                    elem: ValueRttid::new(0, ValueKind::Int64),
                },
            ];
            let local_port = TransferType {
                meta_raw: ValueMeta::new(0, ValueKind::Port).to_raw(),
                rttid_raw: ValueRttid::new(1, ValueKind::Port).to_raw(),
                slots: 1,
            };
            assert!(matches!(
                encode_spawn_payload_from_capture_values(
                    &gc,
                    1,
                    &[Some(vec![u64::MAX])],
                    &[local_port],
                    &[],
                    &[],
                    &[],
                    &[],
                    &runtime_types,
                ),
                Err(IslandMessageEncodeError::InvalidLayout { .. })
            ));
        }
    }

    #[test]
    fn spawn_unpack_preflights_all_chunks_before_allocating_or_resolving_queues() {
        let elem_meta = ValueMeta::new(0, ValueKind::Int64);
        let elem_rttid = ValueRttid::new(0, ValueKind::Int64);
        let port_meta = ValueMeta::new(0, ValueKind::Port);
        let port_rttid = ValueRttid::new(1, ValueKind::Port);
        let runtime_types = vec![
            RuntimeType::Basic(ValueKind::Int64),
            RuntimeType::Port {
                dir: vo_common_core::ChanDir::Send,
                elem: elem_rttid,
            },
        ];
        let port_type = TransferType {
            meta_raw: port_meta.to_raw(),
            rttid_raw: port_rttid.to_raw(),
            slots: 1,
        };
        let int_type = TransferType {
            meta_raw: elem_meta.to_raw(),
            rttid_raw: elem_rttid.to_raw(),
            slots: 1,
        };
        let mut source_gc = Gc::new();
        let port = queue::create_remote_proxy(&mut source_gc, 41, 7, 3, elem_meta, elem_rttid, 1);
        let mut encoded = encode_spawn_payload_from_capture_values(
            &source_gc,
            9,
            &[],
            &[],
            &[port as u64, 42],
            &[port_type, int_type],
            &[],
            &[],
            &runtime_types,
        )
        .expect("encode two-chunk payload");
        let mut destination_gc = Gc::new();
        let object_count_before = destination_gc.object_count();
        let mut resolver_calls = 0usize;

        let mut trailing = encoded.clone();
        trailing.push(0xff);
        let trailing_payload = decode_spawn_header(&trailing).expect("spawn header");
        let result = unpack_spawn_payload(
            &mut destination_gc,
            &trailing,
            &trailing_payload,
            &[],
            &[port_type, int_type],
            &[],
            &[],
            &runtime_types,
            |_, _| {
                resolver_calls += 1;
                core::ptr::null_mut()
            },
        );
        assert_eq!(result, Err(SpawnPayloadUnpackError::TrailingBytes));
        assert_eq!(resolver_calls, 0);
        assert_eq!(destination_gc.object_count(), object_count_before);

        let mut mismatched_payload = decode_spawn_header(&encoded).expect("spawn header");
        mismatched_payload.func_id ^= 1;
        let result = unpack_spawn_payload(
            &mut destination_gc,
            &encoded,
            &mismatched_payload,
            &[],
            &[port_type, int_type],
            &[],
            &[],
            &runtime_types,
            |_, _| {
                resolver_calls += 1;
                core::ptr::null_mut()
            },
        );
        assert_eq!(result, Err(SpawnPayloadUnpackError::HeaderMismatch));
        assert_eq!(resolver_calls, 0);
        assert_eq!(destination_gc.object_count(), object_count_before);

        let payload = decode_spawn_header(&encoded).expect("spawn header");
        let result = unpack_spawn_payload(
            &mut destination_gc,
            &encoded,
            &payload,
            &[],
            &[port_type],
            &[],
            &[],
            &runtime_types,
            |_, _| {
                resolver_calls += 1;
                core::ptr::null_mut()
            },
        );
        assert_eq!(result, Err(SpawnPayloadUnpackError::TypeCountMismatch));
        assert_eq!(resolver_calls, 0);
        assert_eq!(destination_gc.object_count(), object_count_before);

        encoded.pop();
        let payload = decode_spawn_header(&encoded).expect("spawn header");
        let result = unpack_spawn_payload(
            &mut destination_gc,
            &encoded,
            &payload,
            &[],
            &[port_type, int_type],
            &[],
            &[],
            &runtime_types,
            |_, _| {
                resolver_calls += 1;
                core::ptr::null_mut()
            },
        );

        assert_eq!(result, Err(SpawnPayloadUnpackError::UnexpectedEof));
        assert_eq!(resolver_calls, 0);
        assert_eq!(destination_gc.object_count(), object_count_before);
    }

    #[test]
    fn spawn_unpack_rejects_noncanonical_transfer_metadata_before_allocation() {
        let runtime_types = vec![RuntimeType::Basic(ValueKind::Int64)];
        let int_type = TransferType {
            meta_raw: ValueMeta::new(0, ValueKind::Int64).to_raw(),
            rttid_raw: ValueRttid::new(0, ValueKind::Int64).to_raw(),
            slots: 1,
        };
        let source_gc = Gc::new();
        let encoded = encode_spawn_payload_from_capture_values(
            &source_gc,
            3,
            &[],
            &[],
            &[11],
            &[int_type],
            &[],
            &[],
            &runtime_types,
        )
        .expect("encode valid spawn payload");
        let payload = decode_spawn_header(&encoded).expect("spawn header");
        let invalid_type = TransferType {
            meta_raw: 0xffff_ff00 | ValueKind::Int64 as u32,
            ..int_type
        };
        let mut destination_gc = Gc::new();
        let before = destination_gc.object_count();
        let mut resolver_calls = 0;

        let result = unpack_spawn_payload(
            &mut destination_gc,
            &encoded,
            &payload,
            &[],
            &[invalid_type],
            &[],
            &[],
            &runtime_types,
            |_, _| {
                resolver_calls += 1;
                core::ptr::null_mut()
            },
        );

        assert_eq!(result, Err(SpawnPayloadUnpackError::LayoutMismatch));
        assert_eq!(resolver_calls, 0);
        assert_eq!(destination_gc.object_count(), before);

        let nested_runtime_types = vec![
            RuntimeType::Basic(ValueKind::Int64),
            RuntimeType::Chan {
                dir: vo_common_core::ChanDir::Both,
                elem: ValueRttid::new(0, ValueKind::Int64),
            },
            RuntimeType::Array {
                len: 1,
                elem: ValueRttid::new(1, ValueKind::Channel),
            },
        ];
        let nested_channel_array = TransferType {
            meta_raw: ValueMeta::new(2, ValueKind::Array).to_raw(),
            rttid_raw: ValueRttid::new(2, ValueKind::Array).to_raw(),
            slots: 1,
        };
        let result = unpack_spawn_payload(
            &mut destination_gc,
            &encoded,
            &payload,
            &[],
            &[nested_channel_array],
            &[],
            &[],
            &nested_runtime_types,
            |_, _| {
                resolver_calls += 1;
                core::ptr::null_mut()
            },
        );

        assert_eq!(result, Err(SpawnPayloadUnpackError::LayoutMismatch));
        assert_eq!(resolver_calls, 0);
        assert_eq!(destination_gc.object_count(), before);
    }

    #[test]
    fn spawn_unpack_rejects_empty_argument_chunks_even_for_zero_width_values() {
        let struct_metas = vec![StructMeta {
            slot_types: Vec::new(),
            fields: Vec::new(),
            field_index: Default::default(),
        }];
        let runtime_types = vec![RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        }];
        let zero_width_type = TransferType {
            meta_raw: ValueMeta::new(0, ValueKind::Struct).to_raw(),
            rttid_raw: ValueRttid::new(0, ValueKind::Struct).to_raw(),
            slots: 0,
        };
        let source_gc = Gc::new();
        let mut encoded = encode_spawn_payload_from_capture_values(
            &source_gc,
            3,
            &[],
            &[],
            &[],
            &[zero_width_type],
            &struct_metas,
            &[],
            &runtime_types,
        )
        .expect("encode zero-width argument");
        encoded[HEADER_SIZE..HEADER_SIZE + 4].copy_from_slice(&0u32.to_le_bytes());
        encoded.truncate(HEADER_SIZE + 4);
        let payload = decode_spawn_header(&encoded).expect("spawn header");
        let mut destination_gc = Gc::new();

        let result = unpack_spawn_payload(
            &mut destination_gc,
            &encoded,
            &payload,
            &[],
            &[zero_width_type],
            &struct_metas,
            &[],
            &runtime_types,
            |_, _| core::ptr::null_mut(),
        );

        assert_eq!(result, Err(SpawnPayloadUnpackError::LayoutMismatch));
        assert_eq!(destination_gc.object_count(), 0);
    }

    #[test]
    fn nil_capture_chunk_roundtrips_as_a_nil_capture_box() {
        let mut gc = Gc::new();
        let transfer_type = TransferType {
            meta_raw: ValueMeta::new(0, ValueKind::Int64).to_raw(),
            rttid_raw: ValueRttid::new(0, ValueKind::Int64).to_raw(),
            slots: 1,
        };
        let runtime_types = vec![RuntimeType::Basic(ValueKind::Int64)];
        let encoded = encode_spawn_payload_from_capture_values(
            &gc,
            7,
            &[None],
            &[transfer_type],
            &[],
            &[],
            &[],
            &[],
            &runtime_types,
        )
        .expect("encode nil capture payload");
        let payload = decode_spawn_header(&encoded).expect("spawn header");
        let (captures, args) = unpack_spawn_payload(
            &mut gc,
            &encoded,
            &payload,
            &[transfer_type],
            &[],
            &[],
            &[],
            &runtime_types,
            |_, _| core::ptr::null_mut(),
        )
        .expect("nil capture payload");

        assert_eq!(captures, vec![0]);
        assert!(args.is_empty());
    }

    fn roundtrip(cmd: IslandCommand) {
        let encoded = encode_island_command(&cmd).expect("encode island command");
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
    fn vm_island_spawn_capture_root_005_unpack_allocates_canonical_array_capture() {
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
        )
        .expect("encode capture payload");
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
        let header = unsafe { Gc::header(capture_box) };
        assert!(!header.is_value_slots_object());
        assert_eq!(header.value_meta(), ValueMeta::new(0, ValueKind::Array));
        assert_eq!(unsafe { array::len(capture_box) }, 2);
        assert_eq!(
            unsafe { array::elem_meta(capture_box) },
            ValueMeta::new(0, ValueKind::String)
        );
        assert_eq!(unsafe { array::elem_bytes(capture_box) }, SLOT_BYTES);
        let mut flattened = [0_u64; 2];
        unsafe { array::read_value_flat(capture_box, &mut flattened) }
            .expect("canonical String array capture layout");
        assert_eq!(unsafe { string::to_bytes(flattened[0] as GcRef) }, b"left");
        assert_eq!(unsafe { string::to_bytes(flattened[1] as GcRef) }, b"right");

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
            "spawn unpack canonical Array must scan every element root"
        );
        assert!(visited.iter().all(|&child| {
            unsafe { Gc::header(child) }.value_meta() == ValueMeta::new(0, ValueKind::String)
        }));
    }

    #[test]
    fn array_capture_value_slot_storage_roundtrips_without_array_header_aliasing() {
        let mut gc = Gc::new();
        let runtime_types = vec![
            RuntimeType::Basic(ValueKind::Int8),
            RuntimeType::Array {
                len: 4,
                elem: ValueRttid::new(0, ValueKind::Int8),
            },
        ];
        let capture_type = TransferType {
            meta_raw: ValueMeta::new(1, ValueKind::Array).to_raw(),
            rttid_raw: ValueRttid::new(1, ValueKind::Array).to_raw(),
            slots: 4,
        };
        let source = [(-2_i64) as u64, (-1_i64) as u64, 3, 4];
        let descriptors = [SpawnCaptureValue::value_slots(Some(&source))];
        let encoded = encode_spawn_payload_from_capture_descriptors(
            &gc,
            7,
            &descriptors,
            &[capture_type],
            &[],
            &[],
            &[],
            &[],
            &runtime_types,
        )
        .expect("encode value-slot Array capture");
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
        let header = unsafe { Gc::header(capture_box) };
        assert!(header.is_value_slots_object());
        assert_eq!(header.value_meta(), ValueMeta::new(1, ValueKind::Array));
        let restored: Vec<_> = (0..4)
            .map(|index| unsafe { Gc::read_slot(capture_box, index) })
            .collect();
        assert_eq!(restored, source);

        let mut invalid_storage = encoded;
        invalid_storage[HEADER_SIZE] = u8::MAX;
        assert_eq!(
            unpack_spawn_payload(
                &mut gc,
                &invalid_storage,
                &payload,
                &[capture_type],
                &[],
                &[],
                &[],
                &runtime_types,
                |_, _| core::ptr::null_mut(),
            ),
            Err(SpawnPayloadUnpackError::LayoutMismatch)
        );
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
        )
        .expect("encode raw capture payload");
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
        assert_eq!(
            unsafe { Gc::header(captures[0] as GcRef) }.kind(),
            ValueKind::String
        );
        assert_eq!(
            unsafe { Gc::header(captures[1] as GcRef) }.kind(),
            ValueKind::String
        );
        // Safety: unpacking produced live string captures in `gc`.
        assert_eq!(unsafe { string::to_bytes(captures[0] as GcRef) }, b"left");
        assert_eq!(unsafe { string::to_bytes(captures[1] as GcRef) }, b"right");
    }

    fn roundtrip_frame(target_island_id: u32, cmd: IslandCommand) {
        let source_island_id = 3;
        let encoded = encode_island_transport_frame(target_island_id, source_island_id, &cmd)
            .expect("encode island frame");
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
        })
        .expect("encode queue wake");
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
            let mut encoded = encode_island_command(&command).expect("encode island command");
            encoded.push(0xff);
            assert!(matches!(
                decode_island_command(&encoded),
                Err(IslandCommandDecodeError::TrailingBytes)
            ));

            let mut frame =
                encode_island_transport_frame(7, 3, &command).expect("encode island frame");
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
        })
        .expect("encode queue wake");
        let truncated = &encoded[..encoded.len() - 1];
        match decode_island_command(truncated) {
            Err(IslandCommandDecodeError::UnexpectedEof) => {}
            other => panic!("expected UnexpectedEof, got {:?}", other),
        }
    }

    #[test]
    fn island_command_codec_rejects_reserved_tags_and_oversized_declared_chunks() {
        assert!(matches!(
            decode_island_command(&[0xff]),
            Err(IslandCommandDecodeError::InvalidTag)
        ));

        let mut invalid_waiter_option = vec![1];
        invalid_waiter_option.extend_from_slice(&[0; 36]);
        invalid_waiter_option.push(2);
        assert!(matches!(
            decode_island_command(&invalid_waiter_option),
            Err(IslandCommandDecodeError::InvalidTag)
        ));

        let mut invalid_request_kind = vec![3];
        invalid_request_kind.extend_from_slice(&0u64.to_le_bytes());
        invalid_request_kind.push(0xff);
        assert!(matches!(
            decode_island_command(&invalid_request_kind),
            Err(IslandCommandDecodeError::InvalidTag)
        ));

        let mut invalid_response_bool = vec![4];
        invalid_response_bool.extend_from_slice(&0u64.to_le_bytes());
        invalid_response_bool.push(0);
        invalid_response_bool.push(2);
        assert!(matches!(
            decode_island_command(&invalid_response_bool),
            Err(IslandCommandDecodeError::InvalidTag)
        ));

        let mut oversized_declared_chunk = vec![0];
        oversized_declared_chunk.extend_from_slice(&u32::MAX.to_le_bytes());
        oversized_declared_chunk.extend_from_slice(&[1, 2, 3]);
        assert!(matches!(
            decode_island_command(&oversized_declared_chunk),
            Err(IslandCommandDecodeError::UnexpectedEof)
        ));
    }

    #[test]
    fn island_transport_frame_rejects_truncated_header() {
        match decode_island_transport_frame(&[1, 2, 3]) {
            Err(IslandTransportFrameDecodeError::UnexpectedEof) => {}
            other => panic!("expected UnexpectedEof, got {:?}", other),
        }
    }
}
