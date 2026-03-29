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
use vo_common_core::bytecode::StructMeta;
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

type UnpackQueueHandleCache = HashMap<u64, GcRef>;

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
    let mut packed = PackedValue::new();
    pack_value(
        &mut packed,
        gc,
        src,
        value_meta,
        struct_metas,
        runtime_types,
    );
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
        struct_metas,
        runtime_types,
        &mut queue_handle_cache,
        &mut resolve_queue_handle,
    );
}

pub(crate) fn unpack_slots_with_queue_handle_resolver_and_cache<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
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
        struct_metas,
        runtime_types,
        queue_handle_cache,
        resolve_queue_handle,
    );
}

// =============================================================================
// Internal Pack Implementation
// =============================================================================

/// Look up struct meta_id from rttid using runtime_types.
/// For struct types, the rttid indexes into runtime_types which contains the meta_id.
fn lookup_struct_meta_id(rttid: u32, runtime_types: &[RuntimeType]) -> u32 {
    if let Some(rt) = runtime_types.get(rttid as usize) {
        if let RuntimeType::Struct { meta_id, .. } = rt {
            return *meta_id;
        }
        if let RuntimeType::Named { struct_meta_id, .. } = rt {
            if let Some(id) = struct_meta_id {
                return *id;
            }
        }
    }
    panic!("Cannot find struct meta_id for rttid {}", rttid);
}

fn pack_value(
    packed: &mut PackedValue,
    gc: &Gc,
    src: &[u64],
    value_meta: ValueMeta,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) {
    let vk = value_meta.value_kind();

    // Write type tag
    packed.data.push(vk as u8);

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
                pack_slice(packed, gc, slice_ref, struct_metas, runtime_types);
            }

            // Array: recursively pack elements (boxed array on heap)
            ValueKind::Array => {
                let arr_ref = src[0] as GcRef;
                pack_array(packed, gc, arr_ref, struct_metas, runtime_types);
            }

            // Struct: recursively pack fields
            ValueKind::Struct => {
                let meta_id = value_meta.meta_id() as usize;
                pack_struct_inline(packed, gc, src, meta_id, struct_metas, runtime_types);
            }

            // Pointer: pack pointed object (deep copy)
            ValueKind::Pointer => {
                let ptr_ref = src[0] as GcRef;
                pack_pointer(packed, gc, ptr_ref, value_meta, struct_metas, runtime_types);
            }

            // Map: iterate and pack entries
            ValueKind::Map => {
                let map_ref = src[0] as GcRef;
                pack_map(packed, gc, map_ref, struct_metas, runtime_types);
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

fn pack_slice(
    packed: &mut PackedValue,
    gc: &Gc,
    slice_ref: GcRef,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) {
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

    // For struct elements, use actual slot_count from metadata (may be > elem_bytes/8 for packed structs)
    let elem_slots = if elem_meta.value_kind() == ValueKind::Struct {
        let meta_id = elem_meta.meta_id() as usize;
        if meta_id < struct_metas.len() {
            struct_metas[meta_id].slot_types.len()
        } else {
            (elem_bytes + SLOT_BYTES - 1) / SLOT_BYTES
        }
    } else {
        (elem_bytes + SLOT_BYTES - 1) / SLOT_BYTES
    };
    let mut elem_buf = vec![0u64; elem_slots];
    let data_ptr = slice::data_ptr(slice_ref);

    for i in 0..length {
        read_element(data_ptr, i, elem_bytes, elem_meta, &mut elem_buf);
        pack_value(
            packed,
            gc,
            &elem_buf,
            elem_meta,
            struct_metas,
            runtime_types,
        );
    }
}

fn pack_array(
    packed: &mut PackedValue,
    gc: &Gc,
    arr_ref: GcRef,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) {
    // Explicit null marker for all reference types
    if arr_ref.is_null() {
        packed.data.push(0); // null marker
        return;
    }
    packed.data.push(1); // non-null marker

    let length = array::len(arr_ref);
    let elem_meta = array::elem_meta(arr_ref);
    let elem_bytes = array::elem_bytes(arr_ref);

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

    // For struct elements, use actual slot_count from metadata (may be > elem_bytes/8 for packed structs)
    let elem_slots = if elem_meta.value_kind() == ValueKind::Struct {
        let meta_id = elem_meta.meta_id() as usize;
        if meta_id < struct_metas.len() {
            struct_metas[meta_id].slot_types.len()
        } else {
            (elem_bytes + SLOT_BYTES - 1) / SLOT_BYTES
        }
    } else {
        (elem_bytes + SLOT_BYTES - 1) / SLOT_BYTES
    };
    let mut elem_buf = vec![0u64; elem_slots];
    let data_ptr = array::data_ptr_bytes(arr_ref);

    for i in 0..length {
        read_element(data_ptr, i, elem_bytes, elem_meta, &mut elem_buf);
        pack_value(
            packed,
            gc,
            &elem_buf,
            elem_meta,
            struct_metas,
            runtime_types,
        );
    }
}

fn pack_struct_inline(
    packed: &mut PackedValue,
    gc: &Gc,
    src: &[u64],
    meta_id: usize,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) {
    if meta_id >= struct_metas.len() {
        panic!("Invalid struct meta_id: {}", meta_id);
    }

    let meta = &struct_metas[meta_id];
    let slot_count = meta.slot_types.len();

    // Write meta_id for reconstruction
    packed
        .data
        .extend_from_slice(&(meta_id as u32).to_le_bytes());
    packed
        .data
        .extend_from_slice(&(slot_count as u32).to_le_bytes());

    // Pack each field based on fields metadata
    for field in &meta.fields {
        let field_vk = field.type_info.value_kind();
        let field_slots = field.slot_count as usize;
        let slot_idx = field.offset as usize;

        // For nested structs, look up the correct meta_id from runtime_types.
        // For other types, meta_id is not used (containers read metadata from GC object).
        let field_meta = if field_vk == ValueKind::Struct {
            let nested_meta_id = lookup_struct_meta_id(field.type_info.rttid(), runtime_types);
            ValueMeta::new(nested_meta_id, field_vk)
        } else {
            ValueMeta::new(0, field_vk)
        };
        let field_src = &src[slot_idx..slot_idx + field_slots];
        pack_value(
            packed,
            gc,
            field_src,
            field_meta,
            struct_metas,
            runtime_types,
        );
    }
}

fn pack_pointer(
    packed: &mut PackedValue,
    gc: &Gc,
    ptr_ref: GcRef,
    value_meta: ValueMeta,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) {
    if ptr_ref.is_null() {
        packed.data.push(0); // null marker
        return;
    }

    packed.data.push(1); // non-null marker

    let Some(_) = gc.canonicalize_ref(ptr_ref) else {
        panic!("pack_pointer: invalid pointer {:p}", ptr_ref);
    };
    let meta_id = value_meta.meta_id() as usize;
    if meta_id >= struct_metas.len() {
        panic!("pack_pointer: invalid pointee meta_id {}", meta_id);
    }
    let obj_meta = ValueMeta::new(value_meta.meta_id(), ValueKind::Struct);
    let slots = struct_metas[meta_id].slot_types.len();

    packed
        .data
        .extend_from_slice(&obj_meta.to_raw().to_le_bytes());
    packed.data.extend_from_slice(&(slots as u32).to_le_bytes());

    // Read and pack the pointed object
    // Note: pack_value will write another ValueKind tag, which is redundant with obj_meta above.
    // This is intentional for simplicity - unpack_pointer reads both consistently.
    let mut obj_slots = vec![0u64; slots];
    for i in 0..slots {
        obj_slots[i] = unsafe { Gc::read_slot(ptr_ref, i) };
    }
    pack_value(
        packed,
        gc,
        &obj_slots,
        obj_meta,
        struct_metas,
        runtime_types,
    );
}

fn pack_map(
    packed: &mut PackedValue,
    gc: &Gc,
    map_ref: GcRef,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) {
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
        pack_value(packed, gc, k, key_meta, struct_metas, runtime_types);
        pack_value(packed, gc, v, val_meta, struct_metas, runtime_types);
    }
}

// =============================================================================
// Internal Unpack Implementation
// =============================================================================

fn unpack_value<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let vk = ValueKind::from_u8(data[*cursor]);
    *cursor += 1;

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
                    struct_metas,
                    runtime_types,
                    queue_handle_cache,
                    resolve_queue_handle,
                ) as u64;
            }

            ValueKind::Array => {
                dst[0] = unpack_array(
                    gc,
                    data,
                    cursor,
                    struct_metas,
                    runtime_types,
                    queue_handle_cache,
                    resolve_queue_handle,
                ) as u64;
            }

            ValueKind::Struct => {
                unpack_struct_inline(
                    gc,
                    data,
                    cursor,
                    dst,
                    struct_metas,
                    runtime_types,
                    queue_handle_cache,
                    resolve_queue_handle,
                );
            }

            ValueKind::Pointer => {
                dst[0] = unpack_pointer(
                    gc,
                    data,
                    cursor,
                    struct_metas,
                    runtime_types,
                    queue_handle_cache,
                    resolve_queue_handle,
                ) as u64;
            }

            ValueKind::Map => {
                dst[0] = unpack_map(
                    gc,
                    data,
                    cursor,
                    struct_metas,
                    runtime_types,
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
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) -> GcRef
where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    // Read explicit null marker
    let is_non_null = data[*cursor];
    *cursor += 1;
    if is_non_null == 0 {
        return core::ptr::null_mut();
    }

    let length = read_u64(data, cursor) as usize;
    let elem_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let elem_bytes = read_u32(data, cursor) as usize;

    // Create slice with capacity = length
    let new_slice = slice::create(gc, elem_meta, elem_bytes, length, length);

    // Zero-size elements (e.g., struct{}) - no element data to read
    if elem_bytes == 0 {
        return new_slice;
    }

    let data_ptr = slice::data_ptr(new_slice);
    // For struct elements, use actual slot_count from metadata (may be > elem_bytes/8 for packed structs)
    let elem_slots = if elem_meta.value_kind() == ValueKind::Struct {
        let meta_id = elem_meta.meta_id() as usize;
        if meta_id < struct_metas.len() {
            struct_metas[meta_id].slot_types.len()
        } else {
            (elem_bytes + SLOT_BYTES - 1) / SLOT_BYTES
        }
    } else {
        (elem_bytes + SLOT_BYTES - 1) / SLOT_BYTES
    };
    let mut elem_buf = vec![0u64; elem_slots];

    for i in 0..length {
        unpack_value(
            gc,
            data,
            cursor,
            &mut elem_buf,
            struct_metas,
            runtime_types,
            queue_handle_cache,
            resolve_queue_handle,
        );
        write_element(data_ptr, i, elem_bytes, &elem_buf);
    }

    new_slice
}

fn unpack_array<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) -> GcRef
where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    // Read explicit null marker
    let is_non_null = data[*cursor];
    *cursor += 1;
    if is_non_null == 0 {
        return core::ptr::null_mut();
    }

    let length = read_u64(data, cursor) as usize;
    let elem_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let elem_bytes = read_u32(data, cursor) as usize;

    let new_arr = array::create(gc, elem_meta, elem_bytes, length);

    // Zero-size elements (e.g., struct{}) - no element data to read
    if elem_bytes == 0 {
        return new_arr;
    }

    let data_ptr = array::data_ptr_bytes(new_arr);
    // For struct elements, use actual slot_count from metadata (may be > elem_bytes/8 for packed structs)
    let elem_slots = if elem_meta.value_kind() == ValueKind::Struct {
        let meta_id = elem_meta.meta_id() as usize;
        if meta_id < struct_metas.len() {
            struct_metas[meta_id].slot_types.len()
        } else {
            (elem_bytes + SLOT_BYTES - 1) / SLOT_BYTES
        }
    } else {
        (elem_bytes + SLOT_BYTES - 1) / SLOT_BYTES
    };
    let mut elem_buf = vec![0u64; elem_slots];

    for i in 0..length {
        unpack_value(
            gc,
            data,
            cursor,
            &mut elem_buf,
            struct_metas,
            runtime_types,
            queue_handle_cache,
            resolve_queue_handle,
        );
        write_element(data_ptr, i, elem_bytes, &elem_buf);
    }

    new_arr
}

fn unpack_struct_inline<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let meta_id = read_u32(data, cursor) as usize;
    let _slot_count = read_u32(data, cursor) as usize;

    if meta_id >= struct_metas.len() {
        panic!("Invalid struct meta_id during unpack: {}", meta_id);
    }

    let meta = &struct_metas[meta_id];

    // Unpack each field
    for field in &meta.fields {
        let field_slots = field.slot_count as usize;
        let slot_idx = field.offset as usize;
        let field_dst = &mut dst[slot_idx..slot_idx + field_slots];
        unpack_value(
            gc,
            data,
            cursor,
            field_dst,
            struct_metas,
            runtime_types,
            queue_handle_cache,
            resolve_queue_handle,
        );
    }
}

fn unpack_pointer<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) -> GcRef
where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let is_non_null = data[*cursor];
    *cursor += 1;

    if is_non_null == 0 {
        return core::ptr::null_mut();
    }

    let obj_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let slots = read_u32(data, cursor) as usize;

    // Allocate new object
    let new_obj = gc.alloc(obj_meta, slots as u16);
    let mut obj_slots = vec![0u64; slots];

    unpack_value(
        gc,
        data,
        cursor,
        &mut obj_slots,
        struct_metas,
        runtime_types,
        queue_handle_cache,
        resolve_queue_handle,
    );

    // Write slots to new object
    for (i, &val) in obj_slots.iter().enumerate() {
        unsafe { Gc::write_slot(new_obj, i, val) };
    }

    new_obj
}

fn unpack_map<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) -> GcRef
where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    // Read explicit null marker
    let is_non_null = data[*cursor];
    *cursor += 1;
    if is_non_null == 0 {
        return core::ptr::null_mut();
    }

    let length = read_u64(data, cursor) as usize;
    let key_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let val_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let key_slots = read_u16(data, cursor) as u16;
    let val_slots = read_u16(data, cursor) as u16;
    let key_rttid = read_u32(data, cursor);

    // Create new map
    let new_map = map::create(gc, key_meta, val_meta, key_slots, val_slots, key_rttid);

    let mut key_buf = vec![0u64; key_slots as usize];
    let mut val_buf = vec![0u64; val_slots as usize];

    for _ in 0..length {
        unpack_value(
            gc,
            data,
            cursor,
            &mut key_buf,
            struct_metas,
            runtime_types,
            queue_handle_cache,
            resolve_queue_handle,
        );
        unpack_value(
            gc,
            data,
            cursor,
            &mut val_buf,
            struct_metas,
            runtime_types,
            queue_handle_cache,
            resolve_queue_handle,
        );
        map::set(new_map, &key_buf, &val_buf, None);
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
    let is_non_null = data[*cursor];
    *cursor += 1;
    if is_non_null == 0 {
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

    if let Some(&existing) = queue_handle_cache.get(&handle.endpoint_id) {
        return existing;
    }
    let chan_ref = resolve_queue_handle(gc, handle);
    queue_handle_cache.insert(handle.endpoint_id, chan_ref);
    chan_ref
}

// =============================================================================
// Helper Functions
// =============================================================================

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
mod tests {
    use super::*;
    use crate::objects::{queue, queue_state::QueueKind, slice};
    use std::panic::{catch_unwind, AssertUnwindSafe};

    fn make_byte_slice(gc: &mut Gc, bytes: &[u8]) -> GcRef {
        let slice_ref = slice::create(
            gc,
            ValueMeta::new(0, ValueKind::Uint8),
            1,
            bytes.len(),
            bytes.len(),
        );
        for (i, &byte) in bytes.iter().enumerate() {
            slice::set(slice_ref, i, byte as u64, 1);
        }
        slice_ref
    }

    fn assert_byte_slice_eq(slice_ref: GcRef, expected: &[u8]) {
        assert!(!slice_ref.is_null());
        assert_eq!(slice::elem_meta(slice_ref).value_kind(), ValueKind::Uint8);
        assert_eq!(slice::len(slice_ref), expected.len());
        for (i, &byte) in expected.iter().enumerate() {
            assert_eq!(slice::get(slice_ref, i, 1), byte as u64);
        }
    }

    #[test]
    fn test_pack_unpack_scalar() {
        let mut gc = Gc::new();
        let struct_metas = vec![];
        let runtime_types = vec![];

        // Test integer
        let src = [42u64];
        let packed = pack_slots(
            &gc,
            &src,
            ValueMeta::new(0, ValueKind::Int64),
            &struct_metas,
            &runtime_types,
        );

        let mut dst = [0u64];
        unpack_slots(&mut gc, &packed, &mut dst, &struct_metas, &runtime_types);

        assert_eq!(dst[0], 42);
    }

    #[test]
    fn test_pack_unpack_string() {
        let mut gc = Gc::new();
        let struct_metas = vec![];
        let runtime_types = vec![];

        // Create a string
        let str_ref = string::create(&mut gc, b"hello");
        let src = [str_ref as u64];

        let packed = pack_slots(
            &gc,
            &src,
            ValueMeta::new(0, ValueKind::String),
            &struct_metas,
            &runtime_types,
        );

        let mut dst = [0u64];
        unpack_slots(&mut gc, &packed, &mut dst, &struct_metas, &runtime_types);

        let unpacked_str = dst[0] as GcRef;
        assert_eq!(string::as_str(unpacked_str), "hello");
        // Verify it's a different GcRef (deep copy)
        assert_ne!(str_ref, unpacked_str);
    }

    #[test]
    fn test_pack_unpack_nil_byte_slice() {
        let mut gc = Gc::new();
        let struct_metas = vec![];
        let runtime_types = vec![];

        let src = [0u64];
        let packed = pack_slots(
            &gc,
            &src,
            ValueMeta::new(0, ValueKind::Slice),
            &struct_metas,
            &runtime_types,
        );

        let mut dst = [1u64];
        unpack_slots(&mut gc, &packed, &mut dst, &struct_metas, &runtime_types);

        assert_eq!(dst[0], 0);
    }

    #[test]
    fn test_pack_unpack_empty_byte_slice() {
        let mut gc = Gc::new();
        let struct_metas = vec![];
        let runtime_types = vec![];

        let slice_ref = make_byte_slice(&mut gc, &[]);
        let src = [slice_ref as u64];

        let packed = pack_slots(
            &gc,
            &src,
            ValueMeta::new(0, ValueKind::Slice),
            &struct_metas,
            &runtime_types,
        );

        let mut dst = [0u64];
        unpack_slots(&mut gc, &packed, &mut dst, &struct_metas, &runtime_types);

        let unpacked = dst[0] as GcRef;
        assert_byte_slice_eq(unpacked, &[]);
        assert_ne!(slice_ref, unpacked);
        assert_ne!(slice::array_ref(slice_ref), slice::array_ref(unpacked));
    }

    #[test]
    fn test_pack_unpack_non_empty_byte_slice() {
        let mut gc = Gc::new();
        let struct_metas = vec![];
        let runtime_types = vec![];

        let slice_ref = make_byte_slice(&mut gc, &[30, 1, 0, 0, 0]);
        let src = [slice_ref as u64];

        let packed = pack_slots(
            &gc,
            &src,
            ValueMeta::new(0, ValueKind::Slice),
            &struct_metas,
            &runtime_types,
        );

        let mut dst = [0u64];
        unpack_slots(&mut gc, &packed, &mut dst, &struct_metas, &runtime_types);

        let unpacked = dst[0] as GcRef;
        assert_byte_slice_eq(unpacked, &[30, 1, 0, 0, 0]);
        assert_ne!(slice_ref, unpacked);
        assert_ne!(slice::array_ref(slice_ref), slice::array_ref(unpacked));
    }

    #[test]
    fn test_pack_unpack_port_handle_roundtrip() {
        let mut gc = Gc::new();
        let struct_metas = vec![];
        let runtime_types = vec![];

        let port = queue::create(
            &mut gc,
            QueueKind::Port,
            ValueMeta::new(0, ValueKind::Int64),
            ValueRttid::new(0, ValueKind::Int64),
            1,
            4,
        );
        queue::install_home_info(port, 77, 12);

        let src = [port as u64];
        let packed = pack_slots(
            &gc,
            &src,
            ValueMeta::new(0, ValueKind::Port),
            &struct_metas,
            &runtime_types,
        );

        let mut dst = [0u64];
        unpack_slots(&mut gc, &packed, &mut dst, &struct_metas, &runtime_types);

        let unpacked = dst[0] as GcRef;
        assert!(queue::is_remote(unpacked));
        assert!(queue::is_port(unpacked));
        assert_eq!(queue::remote_proxy(unpacked).endpoint_id, 77);
        assert_eq!(queue::remote_proxy(unpacked).home_island, 12);
    }

    #[test]
    fn test_pack_queue_handle_rejects_chan() {
        let mut gc = Gc::new();
        let struct_metas = vec![];
        let runtime_types = vec![];

        let chan = queue::create(
            &mut gc,
            QueueKind::Chan,
            ValueMeta::new(0, ValueKind::Int64),
            ValueRttid::new(0, ValueKind::Int64),
            1,
            4,
        );

        let result = catch_unwind(AssertUnwindSafe(|| {
            let src = [chan as u64];
            pack_slots(
                &gc,
                &src,
                ValueMeta::new(0, ValueKind::Channel),
                &struct_metas,
                &runtime_types,
            )
        }));
        assert!(result.is_err());
    }
}
