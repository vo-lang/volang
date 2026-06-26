//! Pack/Unpack for cross-island value transfer.
//!
//! All sendable values are deep-copied when crossing island boundaries.
//! Pack converts values to an island-independent representation.
//! Unpack reconstructs values in the destination island's heap.

#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};

use hashbrown::HashMap;

use crate::gc::{Gc, GcRef};
use crate::objects::queue_state::QueueKind;
use crate::objects::{array, map, queue, slice, string};
use crate::slot::SLOT_BYTES;
use vo_common_core::bytecode::{Module, NamedTypeMeta, StructMeta};
use vo_common_core::types::{ValueKind, ValueMeta, ValueRttid};
use vo_common_core::RuntimeType;

/// Packed representation of a sendable value.
/// Contains serialized bytes that can be transferred across islands.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackedValue {
    /// Serialized data
    data: Vec<u8>,
}

impl PackedValue {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn from_data(data: Vec<u8>) -> Self {
        Self { data }
    }

    pub fn data(&self) -> &[u8] {
        &self.data
    }

    pub fn into_data(self) -> Vec<u8> {
        self.data
    }
}

impl Default for PackedValue {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct CachedQueueHandle {
    handle: QueueHandleInfo,
    chan_ref: GcRef,
}

pub(crate) type UnpackQueueHandleCache = HashMap<u64, CachedQueueHandle>;

const SEQUENCE_ENCODING_ELEMENTS: u8 = 0;
const SEQUENCE_ENCODING_RAW_BYTES: u8 = 1;
const ARRAY_VALUE_INLINE_MARKER: u8 = 2;

#[derive(Clone, Copy)]
pub(crate) struct PackTypeContext<'a> {
    pub(crate) struct_metas: &'a [StructMeta],
    pub(crate) named_type_metas: &'a [NamedTypeMeta],
    pub(crate) runtime_types: &'a [RuntimeType],
}

impl<'a> PackTypeContext<'a> {
    pub(crate) const fn new(
        struct_metas: &'a [StructMeta],
        runtime_types: &'a [RuntimeType],
    ) -> Self {
        Self {
            struct_metas,
            named_type_metas: &[],
            runtime_types,
        }
    }

    pub(crate) const fn with_named_types(
        struct_metas: &'a [StructMeta],
        named_type_metas: &'a [NamedTypeMeta],
        runtime_types: &'a [RuntimeType],
    ) -> Self {
        Self {
            struct_metas,
            named_type_metas,
            runtime_types,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct QueueHandleInfo {
    pub kind: QueueKind,
    pub endpoint_id: u64,
    pub home_island: u32,
    pub cap: u64,
    pub elem_meta: ValueMeta,
    pub elem_rttid: ValueRttid,
    pub elem_slots: u16,
    pub closed: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PackedLayoutError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SequenceElementLayout {
    pub logical_slots: usize,
    pub physical_bytes: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SequenceLayoutError {
    pub expected_bytes: usize,
    pub actual_bytes: usize,
}

pub fn sequence_element_layout(
    elem_meta: ValueMeta,
    elem_bytes: usize,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<SequenceElementLayout, SequenceLayoutError> {
    sequence_elem_layout_checked(
        elem_meta,
        elem_bytes,
        PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types),
    )
}

/// Pack slots into a PackedValue.
///
/// # Arguments
/// - `gc`: Source GC (for reading heap objects)
/// - `src`: Source slots
/// - `value_meta`: Type metadata for the value
/// - `struct_metas`: Struct metadata for recursive packing
/// - `runtime_types`: Runtime type info for looking up nested struct meta_ids
///
/// # Returns
/// PackedValue containing the serialized data
pub fn pack_slots(
    gc: &Gc,
    src: &[u64],
    value_meta: ValueMeta,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> PackedValue {
    pack_slots_with_context(
        gc,
        src,
        value_meta,
        PackTypeContext::new(struct_metas, runtime_types),
    )
}

pub fn pack_slots_with_named_type_metas(
    gc: &Gc,
    src: &[u64],
    value_meta: ValueMeta,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> PackedValue {
    pack_slots_with_context(
        gc,
        src,
        value_meta,
        PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types),
    )
}

fn pack_slots_with_context(
    gc: &Gc,
    src: &[u64],
    value_meta: ValueMeta,
    context: PackTypeContext<'_>,
) -> PackedValue {
    let mut packed = PackedValue::new();
    pack_value(&mut packed, gc, src, value_meta, context);
    packed
}

/// Unpack a PackedValue into slots.
///
/// # Arguments
/// - `gc`: Destination GC (for allocating heap objects)
/// - `packed`: The packed value to unpack
/// - `dst`: Destination slots
/// - `struct_metas`: Struct metadata for recursive unpacking
/// - `runtime_types`: Runtime type info for looking up nested struct meta_ids
pub fn unpack_slots(
    gc: &mut Gc,
    packed: &PackedValue,
    dst: &mut [u64],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) {
    unpack_slots_with_queue_handle_resolver(
        gc,
        packed,
        dst,
        struct_metas,
        runtime_types,
        default_unpack_queue_handle,
    );
}

pub fn unpack_slots_with_named_type_metas(
    gc: &mut Gc,
    packed: &PackedValue,
    dst: &mut [u64],
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) {
    unpack_slots_with_queue_handle_resolver_and_named_type_metas(
        gc,
        packed,
        dst,
        struct_metas,
        named_type_metas,
        runtime_types,
        default_unpack_queue_handle,
    );
}

pub fn unpack_slots_with_queue_handle_resolver<F>(
    gc: &mut Gc,
    packed: &PackedValue,
    dst: &mut [u64],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    mut resolve_queue_handle: F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let mut cursor = 0;
    let mut queue_handle_cache = UnpackQueueHandleCache::default();
    unpack_slots_with_queue_handle_resolver_and_cache(
        gc,
        &packed.data,
        &mut cursor,
        dst,
        PackTypeContext::new(struct_metas, runtime_types),
        &mut queue_handle_cache,
        &mut resolve_queue_handle,
    );
}

pub fn unpack_slots_with_queue_handle_resolver_and_named_type_metas<F>(
    gc: &mut Gc,
    packed: &PackedValue,
    dst: &mut [u64],
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
    mut resolve_queue_handle: F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let mut cursor = 0;
    let mut queue_handle_cache = UnpackQueueHandleCache::default();
    unpack_slots_with_queue_handle_resolver_and_cache(
        gc,
        &packed.data,
        &mut cursor,
        dst,
        PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types),
        &mut queue_handle_cache,
        &mut resolve_queue_handle,
    );
}

#[allow(clippy::too_many_arguments)]
pub fn unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas<F>(
    gc: &mut Gc,
    packed: &PackedValue,
    dst: &mut [u64],
    expected_meta: ValueMeta,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
    mut resolve_queue_handle: F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let mut cursor = 0;
    let mut queue_handle_cache = UnpackQueueHandleCache::default();
    unpack_slots_expected_with_queue_handle_resolver_and_cache(
        gc,
        &packed.data,
        &mut cursor,
        dst,
        expected_meta,
        PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types),
        &mut queue_handle_cache,
        &mut resolve_queue_handle,
    );
}

pub fn validate_packed_slots_expected_with_named_type_metas(
    data: &[u8],
    expected_meta: ValueMeta,
    expected_rttid: ValueRttid,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<(), PackedLayoutError> {
    let context = PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types);
    let mut cursor = 0;
    validate_packed_value(
        data,
        &mut cursor,
        expected_meta,
        Some(expected_rttid),
        context,
    )?;
    if cursor == data.len() {
        Ok(())
    } else {
        Err(PackedLayoutError)
    }
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn unpack_slots_with_queue_handle_resolver_and_cache<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    unpack_value(
        gc,
        data,
        cursor,
        dst,
        None,
        context,
        queue_handle_cache,
        resolve_queue_handle,
    );
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn unpack_slots_expected_with_queue_handle_resolver_and_cache<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    expected_meta: ValueMeta,
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    unpack_value(
        gc,
        data,
        cursor,
        dst,
        Some(expected_meta),
        context,
        queue_handle_cache,
        resolve_queue_handle,
    );
}

// =============================================================================
// Internal Pack Implementation
// =============================================================================

fn array_value_layout(
    value_meta: ValueMeta,
    context: PackTypeContext<'_>,
) -> Option<ArrayValueLayout> {
    if value_meta.value_kind() != ValueKind::Array {
        return None;
    }
    let array_rttid = ValueRttid::new(value_meta.meta_id(), ValueKind::Array);
    let (len, elem_rttid) = resolve_array_runtime_type(array_rttid, context, 0)?;
    let elem_slots = value_slot_count_for_rttid(elem_rttid, context, 0)?;
    let elem_meta = value_meta_for_rttid(elem_rttid, context, 0)?;
    Some(ArrayValueLayout {
        len: len as usize,
        elem_meta,
        elem_slots,
    })
}

fn resolve_array_runtime_type(
    rttid: ValueRttid,
    context: PackTypeContext<'_>,
    depth: usize,
) -> Option<(u64, ValueRttid)> {
    if depth > context.runtime_types.len() + context.named_type_metas.len() + 1 {
        return None;
    }
    match context.runtime_types.get(rttid.rttid() as usize)? {
        RuntimeType::Array { len, elem } => Some((*len, *elem)),
        RuntimeType::Named { id, .. } => {
            let named = context.named_type_metas.get(*id as usize)?;
            resolve_array_runtime_type(named.underlying_rttid, context, depth + 1)
        }
        _ => None,
    }
}

fn value_slot_count_for_rttid(
    rttid: ValueRttid,
    context: PackTypeContext<'_>,
    depth: usize,
) -> Option<usize> {
    if depth > context.runtime_types.len() + context.named_type_metas.len() + 1 {
        return None;
    }
    match context.runtime_types.get(rttid.rttid() as usize)? {
        RuntimeType::Basic(_) | RuntimeType::Pointer(_) | RuntimeType::Slice(_) => Some(1),
        RuntimeType::Map { .. }
        | RuntimeType::Chan { .. }
        | RuntimeType::Port { .. }
        | RuntimeType::Func { .. }
        | RuntimeType::Island => Some(1),
        RuntimeType::Interface { .. } => Some(2),
        RuntimeType::Struct { meta_id, .. } => context
            .struct_metas
            .get(*meta_id as usize)
            .and_then(checked_struct_slot_count),
        RuntimeType::Array { len, elem } => {
            let elem_slots = value_slot_count_for_rttid(*elem, context, depth + 1)?;
            (*len as usize).checked_mul(elem_slots)
        }
        RuntimeType::Named { id, .. } => {
            let named = context.named_type_metas.get(*id as usize)?;
            value_slot_count_for_rttid(named.underlying_rttid, context, depth + 1)
        }
        RuntimeType::Tuple(elems) => elems.iter().try_fold(0usize, |acc, elem| {
            acc.checked_add(value_slot_count_for_rttid(*elem, context, depth + 1)?)
        }),
    }
}

fn checked_struct_slot_count(meta: &StructMeta) -> Option<usize> {
    u16::try_from(meta.slot_types.len()).ok().map(usize::from)
}

fn struct_slot_count(meta: &StructMeta, context: &str) -> usize {
    checked_struct_slot_count(meta).unwrap_or_else(|| {
        panic!(
            "{context}: struct slot count {} exceeds u16::MAX",
            meta.slot_types.len()
        )
    })
}

fn value_meta_for_rttid(
    rttid: ValueRttid,
    context: PackTypeContext<'_>,
    depth: usize,
) -> Option<ValueMeta> {
    if depth > context.runtime_types.len() + context.named_type_metas.len() + 1 {
        return None;
    }
    match context.runtime_types.get(rttid.rttid() as usize)? {
        RuntimeType::Basic(kind) => Some(ValueMeta::new(0, *kind)),
        RuntimeType::Struct { meta_id, .. } => Some(ValueMeta::new(*meta_id, ValueKind::Struct)),
        RuntimeType::Pointer(inner) => pointer_target_struct_meta_id(*inner, context, depth + 1)
            .map(|meta_id| ValueMeta::new(meta_id, ValueKind::Pointer)),
        RuntimeType::Array { .. } => Some(ValueMeta::new(rttid.rttid(), ValueKind::Array)),
        RuntimeType::Slice(_) => Some(ValueMeta::new(0, ValueKind::Slice)),
        RuntimeType::Map { .. } => Some(ValueMeta::new(0, ValueKind::Map)),
        RuntimeType::Chan { .. } => Some(ValueMeta::new(0, ValueKind::Channel)),
        RuntimeType::Port { .. } => Some(ValueMeta::new(0, ValueKind::Port)),
        RuntimeType::Interface { meta_id, .. } => {
            Some(ValueMeta::new(*meta_id, ValueKind::Interface))
        }
        RuntimeType::Func { .. } => Some(ValueMeta::new(0, ValueKind::Closure)),
        RuntimeType::Island => Some(ValueMeta::new(0, ValueKind::Island)),
        RuntimeType::Named { id, .. } => {
            let named = context.named_type_metas.get(*id as usize)?;
            if named.underlying_meta.value_kind() == ValueKind::Array {
                Some(ValueMeta::new(rttid.rttid(), ValueKind::Array))
            } else {
                Some(named.underlying_meta)
            }
        }
        RuntimeType::Tuple(_) => None,
    }
}

fn expected_meta_for_rttid(rttid: ValueRttid, context: PackTypeContext<'_>) -> ValueMeta {
    value_meta_for_rttid(rttid, context, 0).unwrap_or_else(|| {
        let kind = rttid.value_kind();
        let meta_id = match kind {
            ValueKind::Array | ValueKind::Struct | ValueKind::Interface => rttid.rttid(),
            _ => 0,
        };
        ValueMeta::new(meta_id, kind)
    })
}

fn runtime_type_for_rttid<'a>(
    rttid: ValueRttid,
    context: PackTypeContext<'a>,
    depth: usize,
) -> Option<&'a RuntimeType> {
    if depth > context.runtime_types.len() + context.named_type_metas.len() + 1 {
        return None;
    }
    let runtime_type = context.runtime_types.get(rttid.rttid() as usize)?;
    if let RuntimeType::Named { id, .. } = runtime_type {
        let named = context.named_type_metas.get(*id as usize)?;
        return runtime_type_for_rttid(named.underlying_rttid, context, depth + 1);
    }
    Some(runtime_type)
}

fn pointer_target_struct_meta_id(
    rttid: ValueRttid,
    context: PackTypeContext<'_>,
    depth: usize,
) -> Option<u32> {
    if depth > context.runtime_types.len() + context.named_type_metas.len() + 1 {
        return None;
    }
    match context.runtime_types.get(rttid.rttid() as usize)? {
        RuntimeType::Struct { meta_id, .. } => Some(*meta_id),
        RuntimeType::Named { id, .. } => {
            let named = context.named_type_metas.get(*id as usize)?;
            if named.underlying_meta.value_kind() == ValueKind::Struct {
                Some(named.underlying_meta.meta_id())
            } else {
                pointer_target_struct_meta_id(named.underlying_rttid, context, depth + 1)
            }
        }
        _ => None,
    }
}

fn pack_value(
    packed: &mut PackedValue,
    gc: &Gc,
    src: &[u64],
    value_meta: ValueMeta,
    context: PackTypeContext<'_>,
) {
    let vk = value_meta.value_kind();

    // Write type tag
    packed.data.push(vk as u8);

    if vk == ValueKind::Channel {
        panic!("Cannot pack non-sendable type: {:?}", vk);
    }

    if vk.is_queue() {
        let chan_ref = src[0] as GcRef;
        pack_queue_handle(packed, chan_ref);
    } else {
        match vk {
            // Scalars: direct copy
            ValueKind::Void => {}
            ValueKind::Bool
            | ValueKind::Int
            | ValueKind::Int8
            | ValueKind::Int16
            | ValueKind::Int32
            | ValueKind::Int64
            | ValueKind::Uint
            | ValueKind::Uint8
            | ValueKind::Uint16
            | ValueKind::Uint32
            | ValueKind::Uint64
            | ValueKind::Float32
            | ValueKind::Float64 => {
                packed.data.extend_from_slice(&src[0].to_le_bytes());
            }

            // String: copy bytes
            ValueKind::String => {
                let str_ref = src[0] as GcRef;
                pack_string(packed, str_ref);
            }

            // Slice: recursively pack elements
            ValueKind::Slice => {
                let slice_ref = src[0] as GcRef;
                pack_slice(packed, gc, slice_ref, context);
            }

            // Array values are flattened into slots. Heap array objects used by
            // slices are encoded by pack_array_object instead.
            ValueKind::Array => {
                if let Some(layout) = array_value_layout(value_meta, context) {
                    pack_array_value_inline(packed, gc, src, layout, context);
                } else {
                    let arr_ref = src[0] as GcRef;
                    pack_array(packed, gc, arr_ref, context);
                }
            }

            // Struct: recursively pack fields
            ValueKind::Struct => {
                let meta_id = value_meta.meta_id() as usize;
                pack_struct_inline(packed, gc, src, meta_id, context);
            }

            // Pointer: pack pointed object (deep copy)
            ValueKind::Pointer => {
                let ptr_ref = src[0] as GcRef;
                pack_pointer(packed, gc, ptr_ref, value_meta, context);
            }

            // Map: iterate and pack entries
            ValueKind::Map => {
                let map_ref = src[0] as GcRef;
                pack_map(packed, gc, map_ref, context);
            }

            // Not sendable - caught at compile time or runtime-checked
            ValueKind::Island | ValueKind::Closure | ValueKind::Interface => {
                panic!("Cannot pack non-sendable type: {:?}", vk);
            }
            ValueKind::Channel | ValueKind::Port => unreachable!("queue kinds handled above"),
        }
    }
}

fn pack_string(packed: &mut PackedValue, str_ref: GcRef) {
    if str_ref.is_null() {
        // Null string: length = 0
        packed.data.extend_from_slice(&0u64.to_le_bytes());
    } else {
        let bytes = string::as_bytes(str_ref);
        packed
            .data
            .extend_from_slice(&(bytes.len() as u64).to_le_bytes());
        packed.data.extend_from_slice(bytes);
    }
}

fn pack_slice(packed: &mut PackedValue, gc: &Gc, slice_ref: GcRef, context: PackTypeContext<'_>) {
    // Explicit null marker for all reference types
    if slice_ref.is_null() {
        packed.data.push(0); // null marker
        return;
    }
    packed.data.push(1); // non-null marker

    let length = slice::len(slice_ref);
    let elem_meta = slice::elem_meta(slice_ref);
    let arr_ref = slice::array_ref(slice_ref);
    let elem_bytes = array::elem_bytes(arr_ref);
    validate_sequence_elem_layout(elem_meta, elem_bytes, context);

    packed
        .data
        .extend_from_slice(&(length as u64).to_le_bytes());
    packed
        .data
        .extend_from_slice(&elem_meta.to_raw().to_le_bytes());
    packed
        .data
        .extend_from_slice(&(elem_bytes as u32).to_le_bytes());

    // Zero-size elements (e.g., struct{}) - just write count, no element data
    if elem_bytes == 0 {
        return;
    }
    if can_pack_sequence_as_raw_bytes(elem_meta, elem_bytes) {
        packed.data.push(SEQUENCE_ENCODING_RAW_BYTES);
        pack_raw_sequence_bytes(packed, slice::data_ptr(slice_ref), length, elem_bytes);
        return;
    }
    packed.data.push(SEQUENCE_ENCODING_ELEMENTS);

    // For struct elements, exact StructMeta is the layout authority.
    let elem_slots = sequence_elem_slots(elem_meta, elem_bytes, context);
    let mut elem_buf = vec![0u64; elem_slots];
    let data_ptr = slice::data_ptr(slice_ref);

    for i in 0..length {
        read_element(data_ptr, i, elem_bytes, elem_meta, &mut elem_buf);
        pack_value(packed, gc, &elem_buf, elem_meta, context);
    }
}

fn pack_array(packed: &mut PackedValue, gc: &Gc, arr_ref: GcRef, context: PackTypeContext<'_>) {
    // Explicit null marker for all reference types
    if arr_ref.is_null() {
        packed.data.push(0); // null marker
        return;
    }
    packed.data.push(1); // non-null marker

    let length = array::len(arr_ref);
    let elem_meta = array::elem_meta(arr_ref);
    let elem_bytes = array::elem_bytes(arr_ref);
    validate_sequence_elem_layout(elem_meta, elem_bytes, context);

    packed
        .data
        .extend_from_slice(&(length as u64).to_le_bytes());
    packed
        .data
        .extend_from_slice(&elem_meta.to_raw().to_le_bytes());
    packed
        .data
        .extend_from_slice(&(elem_bytes as u32).to_le_bytes());

    // Zero-size elements (e.g., struct{}) - just write count, no element data
    if elem_bytes == 0 {
        return;
    }
    if can_pack_sequence_as_raw_bytes(elem_meta, elem_bytes) {
        packed.data.push(SEQUENCE_ENCODING_RAW_BYTES);
        pack_raw_sequence_bytes(packed, array::data_ptr_bytes(arr_ref), length, elem_bytes);
        return;
    }
    packed.data.push(SEQUENCE_ENCODING_ELEMENTS);

    // For struct elements, exact StructMeta is the layout authority.
    let elem_slots = sequence_elem_slots(elem_meta, elem_bytes, context);
    let mut elem_buf = vec![0u64; elem_slots];
    let data_ptr = array::data_ptr_bytes(arr_ref);

    for i in 0..length {
        read_element(data_ptr, i, elem_bytes, elem_meta, &mut elem_buf);
        pack_value(packed, gc, &elem_buf, elem_meta, context);
    }
}

#[derive(Clone, Copy)]
struct ArrayValueLayout {
    len: usize,
    elem_meta: ValueMeta,
    elem_slots: usize,
}

fn pack_array_value_inline(
    packed: &mut PackedValue,
    gc: &Gc,
    src: &[u64],
    layout: ArrayValueLayout,
    context: PackTypeContext<'_>,
) {
    let expected_slots = layout
        .len
        .checked_mul(layout.elem_slots)
        .expect("pack array value slot length overflow");
    if src.len() != expected_slots {
        panic!(
            "pack array value slot length mismatch: expected exactly {expected_slots}, got {}",
            src.len()
        );
    }

    packed.data.push(ARRAY_VALUE_INLINE_MARKER);
    packed
        .data
        .extend_from_slice(&(layout.len as u64).to_le_bytes());
    packed
        .data
        .extend_from_slice(&layout.elem_meta.to_raw().to_le_bytes());
    packed
        .data
        .extend_from_slice(&(layout.elem_slots as u64).to_le_bytes());

    if layout.elem_slots == 0 {
        for _ in 0..layout.len {
            pack_value(packed, gc, &[], layout.elem_meta, context);
        }
    } else {
        for elem in src[..expected_slots]
            .chunks(layout.elem_slots)
            .take(layout.len)
        {
            pack_value(packed, gc, elem, layout.elem_meta, context);
        }
    }
}

fn pack_struct_inline(
    packed: &mut PackedValue,
    gc: &Gc,
    src: &[u64],
    meta_id: usize,
    context: PackTypeContext<'_>,
) {
    if meta_id >= context.struct_metas.len() {
        panic!("Invalid struct meta_id: {}", meta_id);
    }

    let meta = &context.struct_metas[meta_id];
    let slot_count = struct_slot_count(meta, "pack_struct_inline");

    // Write meta_id for reconstruction
    packed
        .data
        .extend_from_slice(&(meta_id as u32).to_le_bytes());
    packed
        .data
        .extend_from_slice(&(slot_count as u32).to_le_bytes());

    // Pack each field based on fields metadata
    for field in &meta.fields {
        let field_slots = field.slot_count as usize;
        let slot_idx = field.offset as usize;

        let field_meta = value_meta_for_rttid(field.type_info, context, 0).unwrap_or_else(|| {
            panic!(
                "Invalid runtime type metadata for struct field {} rttid {}",
                field.name,
                field.type_info.rttid()
            )
        });
        let field_src = &src[slot_idx..slot_idx + field_slots];
        pack_value(packed, gc, field_src, field_meta, context);
    }
}

fn pack_pointer(
    packed: &mut PackedValue,
    gc: &Gc,
    ptr_ref: GcRef,
    value_meta: ValueMeta,
    context: PackTypeContext<'_>,
) {
    if ptr_ref.is_null() {
        packed.data.push(0); // null marker
        return;
    }

    packed.data.push(1); // non-null marker

    let Some((_, offset_bytes, data_bytes)) = gc.ref_data_range(ptr_ref) else {
        panic!("pack_pointer: invalid pointer {:p}", ptr_ref);
    };
    let meta_id = value_meta.meta_id() as usize;
    if meta_id >= context.struct_metas.len() {
        panic!("pack_pointer: invalid pointee meta_id {}", meta_id);
    }
    let obj_meta = ValueMeta::new(value_meta.meta_id(), ValueKind::Struct);
    let slots = struct_slot_count(&context.struct_metas[meta_id], "pack_pointer");
    if offset_bytes % SLOT_BYTES != 0 {
        panic!("pack_pointer: pointer {:p} is not slot-aligned", ptr_ref);
    }
    let byte_width = slots
        .checked_mul(SLOT_BYTES)
        .expect("pack_pointer: pointee byte width overflow");
    let end = offset_bytes
        .checked_add(byte_width)
        .expect("pack_pointer: pointee range overflow");
    if end > data_bytes {
        panic!(
            "pack_pointer: pointee layout exceeds allocation: offset {offset_bytes}, width {byte_width}, allocation {data_bytes}"
        );
    }

    packed
        .data
        .extend_from_slice(&obj_meta.to_raw().to_le_bytes());
    packed.data.extend_from_slice(&(slots as u32).to_le_bytes());

    // Read and pack the pointed object
    // Note: pack_value will write another ValueKind tag, which is redundant with obj_meta above.
    // This is intentional for simplicity - unpack_pointer reads both consistently.
    let mut obj_slots = vec![0u64; slots];
    for (i, slot) in obj_slots.iter_mut().enumerate() {
        *slot = unsafe { Gc::read_slot(ptr_ref, i) };
    }
    pack_value(packed, gc, &obj_slots, obj_meta, context);
}

fn pack_map(packed: &mut PackedValue, gc: &Gc, map_ref: GcRef, context: PackTypeContext<'_>) {
    // Explicit null marker for all reference types
    if map_ref.is_null() {
        packed.data.push(0); // null marker
        return;
    }
    packed.data.push(1); // non-null marker

    let length = map::len(map_ref);
    let key_meta = map::key_meta(map_ref);
    let val_meta = map::val_meta(map_ref);
    let key_slots = map::key_slots(map_ref) as usize;
    let val_slots = map::val_slots(map_ref) as usize;
    let key_rttid = map::key_rttid(map_ref);

    packed
        .data
        .extend_from_slice(&(length as u64).to_le_bytes());
    packed
        .data
        .extend_from_slice(&key_meta.to_raw().to_le_bytes());
    packed
        .data
        .extend_from_slice(&val_meta.to_raw().to_le_bytes());
    packed
        .data
        .extend_from_slice(&(key_slots as u16).to_le_bytes());
    packed
        .data
        .extend_from_slice(&(val_slots as u16).to_le_bytes());
    packed.data.extend_from_slice(&key_rttid.to_le_bytes());

    // Iterate and pack entries
    let mut iter = map::iter_init(map_ref);
    while let Some((k, v)) = map::iter_next(&mut iter) {
        pack_value(packed, gc, k, key_meta, context);
        pack_value(packed, gc, v, val_meta, context);
    }
}

fn map_key_context_module(key_meta: ValueMeta, context: PackTypeContext<'_>) -> Option<Module> {
    match key_meta.value_kind() {
        ValueKind::Struct | ValueKind::Interface => {
            let mut module = Module::new("pack-map-key-context".into());
            module.struct_metas = context.struct_metas.to_vec();
            module.named_type_metas = context.named_type_metas.to_vec();
            module.runtime_types = context.runtime_types.to_vec();
            Some(module)
        }
        _ => None,
    }
}

// =============================================================================
// Internal Unpack Implementation
// =============================================================================

fn read_exact<'a>(
    data: &'a [u8],
    cursor: &mut usize,
    len: usize,
) -> Result<&'a [u8], PackedLayoutError> {
    let end = cursor.checked_add(len).ok_or(PackedLayoutError)?;
    let bytes = data.get(*cursor..end).ok_or(PackedLayoutError)?;
    *cursor = end;
    Ok(bytes)
}

fn validate_read_u64(data: &[u8], cursor: &mut usize) -> Result<u64, PackedLayoutError> {
    let bytes: [u8; 8] = read_exact(data, cursor, 8)?
        .try_into()
        .map_err(|_| PackedLayoutError)?;
    Ok(u64::from_le_bytes(bytes))
}

fn validate_read_u32(data: &[u8], cursor: &mut usize) -> Result<u32, PackedLayoutError> {
    let bytes: [u8; 4] = read_exact(data, cursor, 4)?
        .try_into()
        .map_err(|_| PackedLayoutError)?;
    Ok(u32::from_le_bytes(bytes))
}

fn validate_read_u16(data: &[u8], cursor: &mut usize) -> Result<u16, PackedLayoutError> {
    let bytes: [u8; 2] = read_exact(data, cursor, 2)?
        .try_into()
        .map_err(|_| PackedLayoutError)?;
    Ok(u16::from_le_bytes(bytes))
}

fn validate_reference_marker(data: &[u8], cursor: &mut usize) -> Result<bool, PackedLayoutError> {
    match *read_exact(data, cursor, 1)?
        .first()
        .ok_or(PackedLayoutError)?
    {
        0 => Ok(false),
        1 => Ok(true),
        _ => Err(PackedLayoutError),
    }
}

fn validate_packed_queue_handle(
    data: &[u8],
    cursor: &mut usize,
    expected_kind: ValueKind,
    expected_rttid: Option<ValueRttid>,
    context: PackTypeContext<'_>,
) -> Result<(), PackedLayoutError> {
    if !validate_reference_marker(data, cursor)? {
        return Ok(());
    }
    let kind = ValueKind::from_u8(
        *read_exact(data, cursor, 1)?
            .first()
            .ok_or(PackedLayoutError)?,
    );
    if kind != expected_kind {
        return Err(PackedLayoutError);
    }
    let _endpoint_id = validate_read_u64(data, cursor)?;
    let _home_island = validate_read_u32(data, cursor)?;
    let _cap = validate_read_u64(data, cursor)?;
    let elem_meta = ValueMeta::from_raw(validate_read_u32(data, cursor)?);
    let elem_rttid = ValueRttid::from_raw(validate_read_u32(data, cursor)?);
    let elem_slots = validate_read_u16(data, cursor)?;
    let expected_elem_rttid =
        expected_rttid.and_then(|rttid| match runtime_type_for_rttid(rttid, context, 0)? {
            RuntimeType::Chan { elem, .. } | RuntimeType::Port { elem, .. } => Some(*elem),
            _ => None,
        });
    if let Some(expected_elem_rttid) = expected_elem_rttid {
        if elem_rttid != expected_elem_rttid {
            return Err(PackedLayoutError);
        }
        let expected_elem_meta = expected_meta_for_rttid(expected_elem_rttid, context);
        if elem_meta != expected_elem_meta {
            return Err(PackedLayoutError);
        }
        let expected_elem_slots =
            value_slot_count_for_rttid(expected_elem_rttid, context, 0).ok_or(PackedLayoutError)?;
        if usize::from(elem_slots) != expected_elem_slots {
            return Err(PackedLayoutError);
        }
    }
    let _closed = read_exact(data, cursor, 1)?;
    Ok(())
}

fn validate_packed_sequence(
    data: &[u8],
    cursor: &mut usize,
    elem_meta: ValueMeta,
    elem_rttid: Option<ValueRttid>,
    elem_bytes: usize,
    length: usize,
    context: PackTypeContext<'_>,
) -> Result<(), PackedLayoutError> {
    let elem_layout = sequence_elem_layout_checked(elem_meta, elem_bytes, context)
        .map_err(|_| PackedLayoutError)?;
    if elem_bytes == 0 {
        return Ok(());
    }
    let encoding = *read_exact(data, cursor, 1)?
        .first()
        .ok_or(PackedLayoutError)?;
    if encoding == SEQUENCE_ENCODING_RAW_BYTES {
        if !can_pack_sequence_as_raw_bytes(elem_meta, elem_bytes) {
            return Err(PackedLayoutError);
        }
        let byte_len = length.checked_mul(elem_bytes).ok_or(PackedLayoutError)?;
        read_exact(data, cursor, byte_len)?;
        return Ok(());
    }
    if encoding != SEQUENCE_ENCODING_ELEMENTS {
        return Err(PackedLayoutError);
    }
    for _ in 0..length {
        let _ = elem_layout.logical_slots;
        validate_packed_value(data, cursor, elem_meta, elem_rttid, context)?;
    }
    Ok(())
}

fn validate_packed_value(
    data: &[u8],
    cursor: &mut usize,
    expected_meta: ValueMeta,
    expected_rttid: Option<ValueRttid>,
    context: PackTypeContext<'_>,
) -> Result<(), PackedLayoutError> {
    let vk = ValueKind::from_u8(
        *read_exact(data, cursor, 1)?
            .first()
            .ok_or(PackedLayoutError)?,
    );
    let expected_kind = expected_meta.value_kind();
    if vk != expected_kind {
        return Err(PackedLayoutError);
    }
    if vk.is_queue() {
        return validate_packed_queue_handle(data, cursor, expected_kind, expected_rttid, context);
    }
    match vk {
        ValueKind::Void => Ok(()),
        ValueKind::Bool
        | ValueKind::Int
        | ValueKind::Int8
        | ValueKind::Int16
        | ValueKind::Int32
        | ValueKind::Int64
        | ValueKind::Uint
        | ValueKind::Uint8
        | ValueKind::Uint16
        | ValueKind::Uint32
        | ValueKind::Uint64
        | ValueKind::Float32
        | ValueKind::Float64 => {
            read_exact(data, cursor, 8)?;
            Ok(())
        }
        ValueKind::String => {
            let len = validate_read_u64(data, cursor)? as usize;
            read_exact(data, cursor, len)?;
            Ok(())
        }
        ValueKind::Slice => {
            if !validate_reference_marker(data, cursor)? {
                return Ok(());
            }
            let length = validate_read_u64(data, cursor)? as usize;
            let elem_meta = ValueMeta::from_raw(validate_read_u32(data, cursor)?);
            let elem_bytes = validate_read_u32(data, cursor)? as usize;
            let elem_rttid =
                expected_rttid.and_then(|rttid| match runtime_type_for_rttid(rttid, context, 0)? {
                    RuntimeType::Slice(elem) => Some(*elem),
                    _ => None,
                });
            if let Some(elem_rttid) = elem_rttid {
                let expected_elem_meta = expected_meta_for_rttid(elem_rttid, context);
                if elem_meta != expected_elem_meta {
                    return Err(PackedLayoutError);
                }
            }
            validate_packed_sequence(
                data, cursor, elem_meta, elem_rttid, elem_bytes, length, context,
            )
        }
        ValueKind::Array => {
            let marker = *read_exact(data, cursor, 1)?
                .first()
                .ok_or(PackedLayoutError)?;
            let expected_inline_layout = array_value_layout(expected_meta, context);
            if expected_inline_layout.is_some() && marker != ARRAY_VALUE_INLINE_MARKER {
                return Err(PackedLayoutError);
            }
            if marker == ARRAY_VALUE_INLINE_MARKER {
                let length = validate_read_u64(data, cursor)? as usize;
                let elem_meta = ValueMeta::from_raw(validate_read_u32(data, cursor)?);
                let elem_slots = validate_read_u64(data, cursor)? as usize;
                let elem_rttid = expected_rttid.and_then(|rttid| {
                    match runtime_type_for_rttid(rttid, context, 0)? {
                        RuntimeType::Array { elem, .. } => Some(*elem),
                        _ => None,
                    }
                });
                if let Some(layout) = expected_inline_layout {
                    if length != layout.len
                        || elem_meta != layout.elem_meta
                        || elem_slots != layout.elem_slots
                    {
                        return Err(PackedLayoutError);
                    }
                }
                for _ in 0..length {
                    validate_packed_value(data, cursor, elem_meta, elem_rttid, context)?;
                }
                Ok(())
            } else {
                if marker == 0 {
                    return Ok(());
                }
                if marker != 1 {
                    return Err(PackedLayoutError);
                }
                let length = validate_read_u64(data, cursor)? as usize;
                let elem_meta = ValueMeta::from_raw(validate_read_u32(data, cursor)?);
                let elem_bytes = validate_read_u32(data, cursor)? as usize;
                let elem_rttid = expected_rttid.and_then(|rttid| {
                    match runtime_type_for_rttid(rttid, context, 0)? {
                        RuntimeType::Array { elem, .. } => Some(*elem),
                        _ => None,
                    }
                });
                if let Some(elem_rttid) = elem_rttid {
                    let expected_elem_meta = expected_meta_for_rttid(elem_rttid, context);
                    if elem_meta != expected_elem_meta {
                        return Err(PackedLayoutError);
                    }
                }
                validate_packed_sequence(
                    data, cursor, elem_meta, elem_rttid, elem_bytes, length, context,
                )
            }
        }
        ValueKind::Struct => {
            let meta_id = validate_read_u32(data, cursor)? as usize;
            let slot_count = validate_read_u32(data, cursor)? as usize;
            if slot_count > u16::MAX as usize {
                return Err(PackedLayoutError);
            }
            if meta_id != expected_meta.meta_id() as usize {
                return Err(PackedLayoutError);
            }
            let meta = context.struct_metas.get(meta_id).ok_or(PackedLayoutError)?;
            let expected_slots = checked_struct_slot_count(meta).ok_or(PackedLayoutError)?;
            if slot_count != expected_slots {
                return Err(PackedLayoutError);
            }
            for field in &meta.fields {
                let field_meta = expected_meta_for_rttid(field.type_info, context);
                validate_packed_value(data, cursor, field_meta, Some(field.type_info), context)?;
            }
            Ok(())
        }
        ValueKind::Pointer => {
            if !validate_reference_marker(data, cursor)? {
                return Ok(());
            }
            let obj_meta = ValueMeta::from_raw(validate_read_u32(data, cursor)?);
            let slots = validate_read_u32(data, cursor)? as usize;
            if slots > u16::MAX as usize {
                return Err(PackedLayoutError);
            }
            let inner_rttid =
                expected_rttid.and_then(|rttid| match runtime_type_for_rttid(rttid, context, 0)? {
                    RuntimeType::Pointer(inner) => Some(*inner),
                    _ => None,
                });
            if let Some(inner_rttid) = inner_rttid {
                let expected_obj_meta = expected_meta_for_rttid(inner_rttid, context);
                if obj_meta != expected_obj_meta {
                    return Err(PackedLayoutError);
                }
                let expected_slots =
                    value_slot_count_for_rttid(inner_rttid, context, 0).ok_or(PackedLayoutError)?;
                if slots != expected_slots {
                    return Err(PackedLayoutError);
                }
            }
            validate_packed_value(data, cursor, obj_meta, inner_rttid, context)
        }
        ValueKind::Map => {
            if !validate_reference_marker(data, cursor)? {
                return Ok(());
            }
            let length = validate_read_u64(data, cursor)? as usize;
            let key_meta = ValueMeta::from_raw(validate_read_u32(data, cursor)?);
            let val_meta = ValueMeta::from_raw(validate_read_u32(data, cursor)?);
            let key_slots = validate_read_u16(data, cursor)? as usize;
            let val_slots = validate_read_u16(data, cursor)? as usize;
            let _key_rttid = validate_read_u32(data, cursor)?;
            let (key_rttid, val_rttid) = expected_rttid
                .and_then(|rttid| match runtime_type_for_rttid(rttid, context, 0)? {
                    RuntimeType::Map { key, val } => Some((*key, *val)),
                    _ => None,
                })
                .map_or((None, None), |(key, val)| (Some(key), Some(val)));
            if let Some(key_rttid) = key_rttid {
                let expected_key_meta = expected_meta_for_rttid(key_rttid, context);
                if key_meta != expected_key_meta {
                    return Err(PackedLayoutError);
                }
                let expected_key_slots =
                    value_slot_count_for_rttid(key_rttid, context, 0).ok_or(PackedLayoutError)?;
                if key_slots != expected_key_slots {
                    return Err(PackedLayoutError);
                }
            }
            if let Some(val_rttid) = val_rttid {
                let expected_val_meta = expected_meta_for_rttid(val_rttid, context);
                if val_meta != expected_val_meta {
                    return Err(PackedLayoutError);
                }
                let expected_val_slots =
                    value_slot_count_for_rttid(val_rttid, context, 0).ok_or(PackedLayoutError)?;
                if val_slots != expected_val_slots {
                    return Err(PackedLayoutError);
                }
            }
            for _ in 0..length {
                validate_packed_value(data, cursor, key_meta, key_rttid, context)?;
                validate_packed_value(data, cursor, val_meta, val_rttid, context)?;
            }
            Ok(())
        }
        ValueKind::Channel | ValueKind::Port => unreachable!("queue kinds handled above"),
        ValueKind::Closure | ValueKind::Island | ValueKind::Interface => Err(PackedLayoutError),
    }
}

#[allow(clippy::too_many_arguments)]
fn unpack_value<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    expected_meta: Option<ValueMeta>,
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let vk = ValueKind::from_u8(data[*cursor]);
    *cursor += 1;
    if let Some(expected_meta) = expected_meta {
        let expected_kind = expected_meta.value_kind();
        if vk != expected_kind {
            panic!("pack: unpack value kind mismatch: expected {expected_kind:?}, got {vk:?}");
        }
    }

    if vk.is_queue() {
        dst[0] =
            unpack_queue_handle(gc, data, cursor, queue_handle_cache, resolve_queue_handle) as u64;
    } else {
        match vk {
            ValueKind::Void => {}
            ValueKind::Bool
            | ValueKind::Int
            | ValueKind::Int8
            | ValueKind::Int16
            | ValueKind::Int32
            | ValueKind::Int64
            | ValueKind::Uint
            | ValueKind::Uint8
            | ValueKind::Uint16
            | ValueKind::Uint32
            | ValueKind::Uint64
            | ValueKind::Float32
            | ValueKind::Float64 => {
                dst[0] = read_u64(data, cursor);
            }

            ValueKind::String => {
                dst[0] = unpack_string(gc, data, cursor) as u64;
            }

            ValueKind::Slice => {
                dst[0] = unpack_slice(
                    gc,
                    data,
                    cursor,
                    context,
                    queue_handle_cache,
                    resolve_queue_handle,
                ) as u64;
            }

            ValueKind::Array => {
                let marker = data[*cursor];
                if expected_meta
                    .and_then(|meta| array_value_layout(meta, context))
                    .is_some()
                    && marker != ARRAY_VALUE_INLINE_MARKER
                {
                    panic!("pack: fixed array value requires inline array encoding");
                }
                if marker == ARRAY_VALUE_INLINE_MARKER {
                    *cursor += 1;
                    unpack_array_value_inline(
                        gc,
                        data,
                        cursor,
                        dst,
                        expected_meta,
                        context,
                        queue_handle_cache,
                        resolve_queue_handle,
                    );
                } else {
                    dst[0] = unpack_array(
                        gc,
                        data,
                        cursor,
                        context,
                        queue_handle_cache,
                        resolve_queue_handle,
                    ) as u64;
                }
            }

            ValueKind::Struct => {
                unpack_struct_inline(
                    gc,
                    data,
                    cursor,
                    dst,
                    expected_meta,
                    context,
                    queue_handle_cache,
                    resolve_queue_handle,
                );
            }

            ValueKind::Pointer => {
                dst[0] = unpack_pointer(
                    gc,
                    data,
                    cursor,
                    context,
                    queue_handle_cache,
                    resolve_queue_handle,
                ) as u64;
            }

            ValueKind::Map => {
                dst[0] = unpack_map(
                    gc,
                    data,
                    cursor,
                    context,
                    queue_handle_cache,
                    resolve_queue_handle,
                ) as u64;
            }

            ValueKind::Channel | ValueKind::Port => unreachable!("queue kinds handled above"),
            _ => panic!("Cannot unpack non-sendable type: {:?}", vk),
        }
    }
}

fn unpack_string(gc: &mut Gc, data: &[u8], cursor: &mut usize) -> GcRef {
    let len = read_u64(data, cursor) as usize;
    let bytes = &data[*cursor..*cursor + len];
    *cursor += len;
    string::create(gc, bytes)
}

fn unpack_slice<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) -> GcRef
where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    if !read_reference_marker(data, cursor, "slice") {
        return core::ptr::null_mut();
    }

    let length = read_u64(data, cursor) as usize;
    let elem_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let elem_bytes = read_u32(data, cursor) as usize;
    validate_sequence_elem_layout(elem_meta, elem_bytes, context);

    // Create slice with capacity = length
    let new_slice = slice::create(gc, elem_meta, elem_bytes, length, length);

    // Zero-size elements (e.g., struct{}) - no element data to read
    if elem_bytes == 0 {
        return new_slice;
    }

    let data_ptr = slice::data_ptr(new_slice);
    let encoding = data[*cursor];
    *cursor += 1;
    if encoding == SEQUENCE_ENCODING_RAW_BYTES {
        let byte_len = length
            .checked_mul(elem_bytes)
            .expect("unpack raw slice byte length overflow");
        unpack_raw_sequence_bytes(data, cursor, data_ptr, byte_len);
        return new_slice;
    }
    assert_eq!(
        encoding, SEQUENCE_ENCODING_ELEMENTS,
        "pack: invalid slice sequence encoding {encoding}"
    );

    // For struct elements, exact StructMeta is the layout authority.
    let elem_slots = sequence_elem_slots(elem_meta, elem_bytes, context);
    let mut elem_buf = vec![0u64; elem_slots];

    for i in 0..length {
        unpack_value(
            gc,
            data,
            cursor,
            &mut elem_buf,
            Some(elem_meta),
            context,
            queue_handle_cache,
            resolve_queue_handle,
        );
        write_element(data_ptr, i, elem_bytes, &elem_buf);
    }
    if elem_meta.value_kind().may_contain_gc_refs() {
        gc.mark_allocated_for_scan(slice::array_ref(new_slice));
    }

    new_slice
}

fn unpack_array<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) -> GcRef
where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    if !read_reference_marker(data, cursor, "array") {
        return core::ptr::null_mut();
    }

    let length = read_u64(data, cursor) as usize;
    let elem_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let elem_bytes = read_u32(data, cursor) as usize;
    validate_sequence_elem_layout(elem_meta, elem_bytes, context);

    let new_arr = array::create(gc, elem_meta, elem_bytes, length);

    // Zero-size elements (e.g., struct{}) - no element data to read
    if elem_bytes == 0 {
        return new_arr;
    }

    let data_ptr = array::data_ptr_bytes(new_arr);
    let encoding = data[*cursor];
    *cursor += 1;
    if encoding == SEQUENCE_ENCODING_RAW_BYTES {
        let byte_len = length
            .checked_mul(elem_bytes)
            .expect("unpack raw array byte length overflow");
        unpack_raw_sequence_bytes(data, cursor, data_ptr, byte_len);
        return new_arr;
    }
    assert_eq!(
        encoding, SEQUENCE_ENCODING_ELEMENTS,
        "pack: invalid array sequence encoding {encoding}"
    );

    // For struct elements, exact StructMeta is the layout authority.
    let elem_slots = sequence_elem_slots(elem_meta, elem_bytes, context);
    let mut elem_buf = vec![0u64; elem_slots];

    for i in 0..length {
        unpack_value(
            gc,
            data,
            cursor,
            &mut elem_buf,
            Some(elem_meta),
            context,
            queue_handle_cache,
            resolve_queue_handle,
        );
        write_element(data_ptr, i, elem_bytes, &elem_buf);
    }
    if elem_meta.value_kind().may_contain_gc_refs() {
        gc.mark_allocated_for_scan(new_arr);
    }

    new_arr
}

fn unpack_array_value_inline<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    expected_meta: Option<ValueMeta>,
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let length = read_u64(data, cursor) as usize;
    let elem_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let elem_slots = read_u64(data, cursor) as usize;
    if let Some(layout) = expected_meta.and_then(|meta| array_value_layout(meta, context)) {
        if length != layout.len || elem_meta != layout.elem_meta || elem_slots != layout.elem_slots
        {
            panic!(
                "unpack array value layout mismatch: expected len {} meta {:?} elem_slots {}, got len {length} meta {:?} elem_slots {elem_slots}",
                layout.len,
                layout.elem_meta,
                layout.elem_slots,
                elem_meta
            );
        }
    }
    let expected_slots = length
        .checked_mul(elem_slots)
        .expect("unpack array value slot length overflow");
    if dst.len() != expected_slots {
        panic!(
            "unpack array value slot length mismatch: expected exactly {expected_slots}, got {}",
            dst.len()
        );
    }

    if elem_slots == 0 {
        for _ in 0..length {
            unpack_value(
                gc,
                data,
                cursor,
                &mut [],
                Some(elem_meta),
                context,
                queue_handle_cache,
                resolve_queue_handle,
            );
        }
        return;
    }

    for elem_dst in dst[..expected_slots].chunks_mut(elem_slots).take(length) {
        unpack_value(
            gc,
            data,
            cursor,
            elem_dst,
            Some(elem_meta),
            context,
            queue_handle_cache,
            resolve_queue_handle,
        );
    }
}

#[allow(clippy::too_many_arguments)]
fn unpack_struct_inline<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    expected_meta: Option<ValueMeta>,
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let meta_id = read_u32(data, cursor) as usize;
    let slot_count = read_u32(data, cursor) as usize;
    if let Some(expected_meta) = expected_meta {
        let expected_meta_id = expected_meta.meta_id() as usize;
        if meta_id != expected_meta_id {
            panic!("unpack struct meta_id mismatch: expected {expected_meta_id}, got {meta_id}");
        }
    }

    if meta_id >= context.struct_metas.len() {
        panic!("Invalid struct meta_id during unpack: {}", meta_id);
    }

    let meta = &context.struct_metas[meta_id];
    if slot_count != meta.slot_types.len() || dst.len() != meta.slot_types.len() {
        panic!(
            "unpack struct slot count mismatch: expected {}, wire {}, dst {}",
            meta.slot_types.len(),
            slot_count,
            dst.len()
        );
    }

    // Unpack each field
    for field in &meta.fields {
        let field_slots = field.slot_count as usize;
        let slot_idx = field.offset as usize;
        let field_dst = &mut dst[slot_idx..slot_idx + field_slots];
        let field_meta = expected_meta_for_rttid(field.type_info, context);
        unpack_value(
            gc,
            data,
            cursor,
            field_dst,
            Some(field_meta),
            context,
            queue_handle_cache,
            resolve_queue_handle,
        );
    }
}

fn unpack_pointer<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) -> GcRef
where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    if !read_reference_marker(data, cursor, "pointer") {
        return core::ptr::null_mut();
    }

    let obj_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let slots = read_u32(data, cursor) as usize;
    let slots_u16 =
        u16::try_from(slots).expect("unpack_pointer: pointer slot count exceeds u16::MAX");

    // Allocate new object
    let new_obj = gc.alloc(obj_meta, slots_u16);
    let mut obj_slots = vec![0u64; slots];

    unpack_value(
        gc,
        data,
        cursor,
        &mut obj_slots,
        Some(obj_meta),
        context,
        queue_handle_cache,
        resolve_queue_handle,
    );

    // Write slots to new object
    for (i, &val) in obj_slots.iter().enumerate() {
        unsafe { Gc::write_slot(new_obj, i, val) };
    }

    gc.mark_allocated_for_scan(new_obj);
    new_obj
}

fn unpack_map<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) -> GcRef
where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    if !read_reference_marker(data, cursor, "map") {
        return core::ptr::null_mut();
    }

    let length = read_u64(data, cursor) as usize;
    let key_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let val_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let key_slots = read_u16(data, cursor);
    let val_slots = read_u16(data, cursor);
    let key_rttid = read_u32(data, cursor);

    // Create new map
    let new_map = map::create(gc, key_meta, val_meta, key_slots, val_slots, key_rttid);

    let mut key_buf = vec![0u64; key_slots as usize];
    let mut val_buf = vec![0u64; val_slots as usize];
    let key_context_module = map_key_context_module(key_meta, context);

    for _ in 0..length {
        unpack_value(
            gc,
            data,
            cursor,
            &mut key_buf,
            Some(key_meta),
            context,
            queue_handle_cache,
            resolve_queue_handle,
        );
        unpack_value(
            gc,
            data,
            cursor,
            &mut val_buf,
            Some(val_meta),
            context,
            queue_handle_cache,
            resolve_queue_handle,
        );
        unsafe {
            // SAFETY: unpack initializes a fresh map and marks it for scan before publication below.
            map::set_checked(new_map, &key_buf, &val_buf, key_context_module.as_ref())
        }
        .expect("packed map keys must be hashable");
    }
    if key_meta.value_kind().may_contain_gc_refs() || val_meta.value_kind().may_contain_gc_refs() {
        gc.mark_allocated_for_scan(new_map);
    }

    new_map
}

// =============================================================================
// Channel Handle Pack/Unpack
// =============================================================================

/// Pack a channel handle for cross-island transfer.
/// Serializes: endpoint_id(8) + home_island(4) + cap(8) + meta_raw(4) + elem_slots(2) + closed(1)
/// LOCAL channels MUST have HomeInfo installed before packing (call prepare_chans first).
fn pack_queue_handle(packed: &mut PackedValue, chan_ref: GcRef) {
    if chan_ref.is_null() {
        packed.data.push(0); // null marker
        return;
    }
    packed.data.push(1); // non-null marker
    pack_queue_handle_inner(packed, chan_ref);
}

fn pack_queue_handle_inner(packed: &mut PackedValue, chan_ref: GcRef) {
    let (kind, cap, elem_meta, elem_rttid, elem_slots) = queue::get_metadata(chan_ref);
    if kind != QueueKind::Port {
        panic!("pack_queue_handle: {:?} cannot cross islands", kind);
    }

    let (endpoint_id, home_island, closed) = if queue::is_remote(chan_ref) {
        let proxy = queue::remote_proxy(chan_ref);
        (proxy.endpoint_id, proxy.home_island, proxy.closed)
    } else {
        match queue::home_info(chan_ref) {
            Some(info) => (info.endpoint_id, info.home_island, queue::is_closed(chan_ref) && queue::len(chan_ref) == 0),
            None => panic!("pack_chan_handle: LOCAL channel without HomeInfo — call prepare_value_chans_for_transfer first"),
        }
    };

    packed.data.push(kind.value_kind() as u8);
    packed.data.extend_from_slice(&endpoint_id.to_le_bytes());
    packed.data.extend_from_slice(&home_island.to_le_bytes());
    packed.data.extend_from_slice(&cap.to_le_bytes());
    packed
        .data
        .extend_from_slice(&elem_meta.to_raw().to_le_bytes());
    packed
        .data
        .extend_from_slice(&elem_rttid.to_raw().to_le_bytes());
    packed.data.extend_from_slice(&elem_slots.to_le_bytes());
    packed.data.push(closed as u8);
}

/// Unpack a channel handle — creates a REMOTE proxy on the destination island.
fn default_unpack_queue_handle(gc: &mut Gc, handle: QueueHandleInfo) -> GcRef {
    queue::create_remote_proxy_with_closed(
        gc,
        handle.kind,
        handle.endpoint_id,
        handle.home_island,
        handle.cap,
        handle.elem_meta,
        handle.elem_rttid,
        handle.elem_slots,
        handle.closed,
    )
}

fn unpack_queue_handle<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) -> GcRef
where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    if !read_reference_marker(data, cursor, "queue") {
        return core::ptr::null_mut();
    }

    let kind = QueueKind::from_value_kind(ValueKind::from_u8(data[*cursor]));
    *cursor += 1;
    let endpoint_id = read_u64(data, cursor);
    let home_island = read_u32(data, cursor);
    let cap = read_u64(data, cursor);
    let meta_raw = read_u32(data, cursor);
    let rttid_raw = read_u32(data, cursor);
    let elem_slots = read_u16(data, cursor);
    let closed = data[*cursor] != 0;
    *cursor += 1;

    let handle = QueueHandleInfo {
        kind,
        endpoint_id,
        home_island,
        cap,
        elem_meta: ValueMeta::from_raw(meta_raw),
        elem_rttid: ValueRttid::from_raw(rttid_raw),
        elem_slots,
        closed,
    };

    if let Some(existing) = queue_handle_cache.get(&handle.endpoint_id) {
        if existing.handle == handle {
            return existing.chan_ref;
        }
    }
    let chan_ref = resolve_queue_handle(gc, handle);
    queue_handle_cache.insert(handle.endpoint_id, CachedQueueHandle { handle, chan_ref });
    chan_ref
}

// =============================================================================
// Helper Functions
// =============================================================================

fn can_pack_sequence_as_raw_bytes(elem_meta: ValueMeta, elem_bytes: usize) -> bool {
    if elem_bytes != 1 {
        return false;
    }
    matches!(
        elem_meta.value_kind(),
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8
    )
}

fn sequence_elem_slots(
    elem_meta: ValueMeta,
    elem_bytes: usize,
    context: PackTypeContext<'_>,
) -> usize {
    match sequence_elem_layout_checked(elem_meta, elem_bytes, context) {
        Ok(layout) => layout.logical_slots,
        Err(err) => panic!(
            "pack: sequence {:?} element byte width mismatch: expected {}, got {}",
            elem_meta.value_kind(),
            err.expected_bytes,
            err.actual_bytes
        ),
    }
}

fn validate_sequence_elem_layout(
    elem_meta: ValueMeta,
    elem_bytes: usize,
    context: PackTypeContext<'_>,
) {
    let _ = sequence_elem_slots(elem_meta, elem_bytes, context);
}

fn sequence_elem_layout_checked(
    elem_meta: ValueMeta,
    elem_bytes: usize,
    context: PackTypeContext<'_>,
) -> Result<SequenceElementLayout, SequenceLayoutError> {
    let layout = match elem_meta.value_kind() {
        ValueKind::Void => SequenceElementLayout {
            logical_slots: 0,
            physical_bytes: 0,
        },
        ValueKind::Struct => {
            let meta_id = elem_meta.meta_id() as usize;
            let Some(meta) = context.struct_metas.get(meta_id) else {
                return Err(SequenceLayoutError {
                    expected_bytes: 0,
                    actual_bytes: elem_bytes,
                });
            };
            let Some(logical_slots) = checked_struct_slot_count(meta) else {
                return Err(SequenceLayoutError {
                    expected_bytes: 0,
                    actual_bytes: elem_bytes,
                });
            };
            let physical_bytes = if meta.fields.is_empty() {
                0
            } else {
                logical_slots
                    .checked_mul(SLOT_BYTES)
                    .ok_or(SequenceLayoutError {
                        expected_bytes: 0,
                        actual_bytes: elem_bytes,
                    })?
            };
            SequenceElementLayout {
                logical_slots,
                physical_bytes,
            }
        }
        ValueKind::Array => {
            let Some(layout) = array_value_layout(elem_meta, context) else {
                return Err(SequenceLayoutError {
                    expected_bytes: 0,
                    actual_bytes: elem_bytes,
                });
            };
            let elem_slots =
                layout
                    .len
                    .checked_mul(layout.elem_slots)
                    .ok_or(SequenceLayoutError {
                        expected_bytes: 0,
                        actual_bytes: elem_bytes,
                    })?;
            SequenceElementLayout {
                logical_slots: elem_slots,
                physical_bytes: elem_slots
                    .checked_mul(SLOT_BYTES)
                    .ok_or(SequenceLayoutError {
                        expected_bytes: 0,
                        actual_bytes: elem_bytes,
                    })?,
            }
        }
        ValueKind::Interface => SequenceElementLayout {
            logical_slots: 2,
            physical_bytes: 2 * SLOT_BYTES,
        },
        kind if kind.is_queue()
            || matches!(
                kind,
                ValueKind::String
                    | ValueKind::Slice
                    | ValueKind::Map
                    | ValueKind::Pointer
                    | ValueKind::Closure
                    | ValueKind::Island
            ) =>
        {
            SequenceElementLayout {
                logical_slots: 1,
                physical_bytes: SLOT_BYTES,
            }
        }
        kind => {
            let bytes = kind.elem_bytes();
            SequenceElementLayout {
                logical_slots: bytes.div_ceil(SLOT_BYTES),
                physical_bytes: bytes,
            }
        }
    };
    if elem_bytes != layout.physical_bytes {
        return Err(SequenceLayoutError {
            expected_bytes: layout.physical_bytes,
            actual_bytes: elem_bytes,
        });
    }
    Ok(layout)
}

fn pack_raw_sequence_bytes(
    packed: &mut PackedValue,
    data_ptr: *mut u8,
    length: usize,
    elem_bytes: usize,
) {
    let byte_len = length
        .checked_mul(elem_bytes)
        .expect("pack raw sequence byte length overflow");
    if byte_len == 0 {
        return;
    }
    let bytes = unsafe { core::slice::from_raw_parts(data_ptr as *const u8, byte_len) };
    packed.data.extend_from_slice(bytes);
}

fn unpack_raw_sequence_bytes(data: &[u8], cursor: &mut usize, dst: *mut u8, byte_len: usize) {
    if byte_len == 0 {
        return;
    }
    let end = *cursor + byte_len;
    unsafe {
        core::ptr::copy_nonoverlapping(data[*cursor..end].as_ptr(), dst, byte_len);
    }
    *cursor = end;
}

fn read_u64(data: &[u8], cursor: &mut usize) -> u64 {
    let bytes: [u8; 8] = data[*cursor..*cursor + 8]
        .try_into()
        .expect("pack: insufficient data for u64");
    *cursor += 8;
    u64::from_le_bytes(bytes)
}

fn read_u32(data: &[u8], cursor: &mut usize) -> u32 {
    let bytes: [u8; 4] = data[*cursor..*cursor + 4]
        .try_into()
        .expect("pack: insufficient data for u32");
    *cursor += 4;
    u32::from_le_bytes(bytes)
}

fn read_u16(data: &[u8], cursor: &mut usize) -> u16 {
    let bytes: [u8; 2] = data[*cursor..*cursor + 2]
        .try_into()
        .expect("pack: insufficient data for u16");
    *cursor += 2;
    u16::from_le_bytes(bytes)
}

fn read_reference_marker(data: &[u8], cursor: &mut usize, kind: &str) -> bool {
    let marker = *data
        .get(*cursor)
        .unwrap_or_else(|| panic!("pack: insufficient data for {kind} reference marker"));
    *cursor += 1;
    match marker {
        0 => false,
        1 => true,
        _ => panic!("pack: invalid {kind} reference marker {marker}"),
    }
}

fn read_element(
    base_ptr: *mut u8,
    idx: usize,
    elem_bytes: usize,
    elem_meta: ValueMeta,
    dst: &mut [u64],
) {
    let ptr = unsafe { base_ptr.add(idx * elem_bytes) };
    let vk = elem_meta.value_kind();

    match elem_bytes {
        1 => {
            let v = unsafe { *ptr };
            // Sign extend for Int8
            dst[0] = if vk == ValueKind::Int8 {
                (v as i8) as i64 as u64
            } else {
                v as u64
            };
        }
        2 => {
            let v = unsafe { *(ptr as *const u16) };
            // Sign extend for Int16
            dst[0] = if vk == ValueKind::Int16 {
                (v as i16) as i64 as u64
            } else {
                v as u64
            };
        }
        4 => {
            let v = unsafe { *(ptr as *const u32) };
            // Sign extend for Int32, keep bits for Float32
            dst[0] = if vk == ValueKind::Int32 {
                (v as i32) as i64 as u64
            } else {
                v as u64
            };
        }
        8 => dst[0] = unsafe { *(ptr as *const u64) },
        _ => {
            let dst_bytes = dst.as_mut_ptr() as *mut u8;
            unsafe { core::ptr::copy_nonoverlapping(ptr, dst_bytes, elem_bytes) };
        }
    }
}

fn write_element(base_ptr: *mut u8, idx: usize, elem_bytes: usize, src: &[u64]) {
    let ptr = unsafe { base_ptr.add(idx * elem_bytes) };
    match elem_bytes {
        1 => unsafe { *ptr = src[0] as u8 },
        2 => unsafe { *(ptr as *mut u16) = src[0] as u16 },
        4 => unsafe { *(ptr as *mut u32) = src[0] as u32 },
        8 => unsafe { *(ptr as *mut u64) = src[0] },
        _ => {
            let src_bytes = src.as_ptr() as *const u8;
            unsafe { core::ptr::copy_nonoverlapping(src_bytes, ptr, elem_bytes) };
        }
    }
}

#[cfg(test)]
mod tests;
