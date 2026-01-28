//! Pack/Unpack for cross-island value transfer.
//!
//! All sendable values are deep-copied when crossing island boundaries.
//! Pack converts values to an island-independent representation.
//! Unpack reconstructs values in the destination island's heap.

#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};

use crate::gc::{Gc, GcRef};
use crate::objects::{array, map, slice, string};
use crate::slot::SLOT_BYTES;
use vo_common_core::bytecode::StructMeta;
use vo_common_core::types::{ValueKind, ValueMeta};
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
    pack_value(&mut packed, gc, src, value_meta, struct_metas, runtime_types);
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
    let mut cursor = 0;
    unpack_value(gc, &packed.data, &mut cursor, dst, struct_metas, runtime_types);
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

        // Not sendable - these should be caught at compile time
        ValueKind::Channel
        | ValueKind::Port
        | ValueKind::Island
        | ValueKind::Closure
        | ValueKind::Interface => {
            panic!("Cannot pack non-sendable type: {:?}", vk);
        }
    }
}

fn pack_string(packed: &mut PackedValue, str_ref: GcRef) {
    if str_ref.is_null() {
        // Null string: length = 0
        packed.data.extend_from_slice(&0u64.to_le_bytes());
    } else {
        let bytes = string::as_bytes(str_ref);
        packed.data.extend_from_slice(&(bytes.len() as u64).to_le_bytes());
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
    if slice_ref.is_null() {
        // Null slice
        packed.data.extend_from_slice(&0u64.to_le_bytes()); // length
        packed.data.extend_from_slice(&0u32.to_le_bytes()); // elem_meta
        packed.data.extend_from_slice(&0u32.to_le_bytes()); // elem_bytes
        return;
    }

    let length = slice::len(slice_ref);
    let elem_meta = slice::elem_meta(slice_ref);
    let arr_ref = slice::array_ref(slice_ref);
    let elem_bytes = array::elem_bytes(arr_ref);

    packed.data.extend_from_slice(&(length as u64).to_le_bytes());
    packed.data.extend_from_slice(&elem_meta.to_raw().to_le_bytes());
    packed.data.extend_from_slice(&(elem_bytes as u32).to_le_bytes());

    // Pack each element
    let elem_slots = (elem_bytes + SLOT_BYTES - 1) / SLOT_BYTES;
    let mut elem_buf = vec![0u64; elem_slots];
    let data_ptr = slice::data_ptr(slice_ref);

    for i in 0..length {
        // Read element from slice
        read_element(data_ptr, i, elem_bytes, elem_meta, &mut elem_buf);
        pack_value(packed, gc, &elem_buf, elem_meta, struct_metas, runtime_types);
    }
}

fn pack_array(
    packed: &mut PackedValue,
    gc: &Gc,
    arr_ref: GcRef,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) {
    if arr_ref.is_null() {
        packed.data.extend_from_slice(&0u64.to_le_bytes()); // length
        packed.data.extend_from_slice(&0u32.to_le_bytes()); // elem_meta
        packed.data.extend_from_slice(&0u32.to_le_bytes()); // elem_bytes
        return;
    }

    let length = array::len(arr_ref);
    let elem_meta = array::elem_meta(arr_ref);
    let elem_bytes = array::elem_bytes(arr_ref);

    packed.data.extend_from_slice(&(length as u64).to_le_bytes());
    packed.data.extend_from_slice(&elem_meta.to_raw().to_le_bytes());
    packed.data.extend_from_slice(&(elem_bytes as u32).to_le_bytes());

    // Pack each element
    let elem_slots = (elem_bytes + SLOT_BYTES - 1) / SLOT_BYTES;
    let mut elem_buf = vec![0u64; elem_slots];
    let data_ptr = array::data_ptr_bytes(arr_ref);

    for i in 0..length {
        read_element(data_ptr, i, elem_bytes, elem_meta, &mut elem_buf);
        pack_value(packed, gc, &elem_buf, elem_meta, struct_metas, runtime_types);
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
    packed.data.extend_from_slice(&(meta_id as u32).to_le_bytes());
    packed.data.extend_from_slice(&(slot_count as u32).to_le_bytes());

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
        pack_value(packed, gc, field_src, field_meta, struct_metas, runtime_types);
    }
}

fn pack_pointer(
    packed: &mut PackedValue,
    gc: &Gc,
    ptr_ref: GcRef,
    _value_meta: ValueMeta,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) {
    if ptr_ref.is_null() {
        packed.data.push(0); // null marker
        return;
    }

    packed.data.push(1); // non-null marker

    // Get pointed object metadata
    let header = Gc::header(ptr_ref);
    let obj_meta = header.value_meta();
    let slots = header.slots as usize;

    packed.data.extend_from_slice(&obj_meta.to_raw().to_le_bytes());
    packed.data.extend_from_slice(&(slots as u32).to_le_bytes());

    // Read and pack the pointed object
    // Note: pack_value will write another ValueKind tag, which is redundant with obj_meta above.
    // This is intentional for simplicity - unpack_pointer reads both consistently.
    let mut obj_slots = vec![0u64; slots];
    for i in 0..slots {
        obj_slots[i] = unsafe { Gc::read_slot(ptr_ref, i) };
    }
    pack_value(packed, gc, &obj_slots, obj_meta, struct_metas, runtime_types);
}

fn pack_map(
    packed: &mut PackedValue,
    gc: &Gc,
    map_ref: GcRef,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) {
    if map_ref.is_null() {
        packed.data.extend_from_slice(&0u64.to_le_bytes()); // length
        return;
    }

    let length = map::len(map_ref);
    let key_meta = map::key_meta(map_ref);
    let val_meta = map::val_meta(map_ref);
    let key_slots = map::key_slots(map_ref) as usize;
    let val_slots = map::val_slots(map_ref) as usize;
    let key_rttid = map::key_rttid(map_ref);

    packed.data.extend_from_slice(&(length as u64).to_le_bytes());
    packed.data.extend_from_slice(&key_meta.to_raw().to_le_bytes());
    packed.data.extend_from_slice(&val_meta.to_raw().to_le_bytes());
    packed.data.extend_from_slice(&(key_slots as u16).to_le_bytes());
    packed.data.extend_from_slice(&(val_slots as u16).to_le_bytes());
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

fn unpack_value(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) {
    let vk = ValueKind::from_u8(data[*cursor]);
    *cursor += 1;

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
            dst[0] = unpack_slice(gc, data, cursor, struct_metas, runtime_types) as u64;
        }

        ValueKind::Array => {
            dst[0] = unpack_array(gc, data, cursor, struct_metas, runtime_types) as u64;
        }

        ValueKind::Struct => {
            unpack_struct_inline(gc, data, cursor, dst, struct_metas, runtime_types);
        }

        ValueKind::Pointer => {
            dst[0] = unpack_pointer(gc, data, cursor, struct_metas, runtime_types) as u64;
        }

        ValueKind::Map => {
            dst[0] = unpack_map(gc, data, cursor, struct_metas, runtime_types) as u64;
        }

        _ => panic!("Cannot unpack non-sendable type: {:?}", vk),
    }
}

fn unpack_string(gc: &mut Gc, data: &[u8], cursor: &mut usize) -> GcRef {
    let len = read_u64(data, cursor) as usize;
    let bytes = &data[*cursor..*cursor + len];
    *cursor += len;
    string::create(gc, bytes)
}

fn unpack_slice(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> GcRef {
    let length = read_u64(data, cursor) as usize;
    let elem_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let elem_bytes = read_u32(data, cursor) as usize;

    // Create slice with capacity = length
    let new_slice = slice::create(gc, elem_meta, elem_bytes, length, length);
    let data_ptr = slice::data_ptr(new_slice);
    let elem_slots = (elem_bytes + SLOT_BYTES - 1) / SLOT_BYTES;
    let mut elem_buf = vec![0u64; elem_slots];

    for i in 0..length {
        unpack_value(gc, data, cursor, &mut elem_buf, struct_metas, runtime_types);
        write_element(data_ptr, i, elem_bytes, &elem_buf);
    }

    new_slice
}

fn unpack_array(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> GcRef {
    let length = read_u64(data, cursor) as usize;
    let elem_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let elem_bytes = read_u32(data, cursor) as usize;

    let new_arr = array::create(gc, elem_meta, elem_bytes, length);
    let data_ptr = array::data_ptr_bytes(new_arr);
    let elem_slots = (elem_bytes + SLOT_BYTES - 1) / SLOT_BYTES;
    let mut elem_buf = vec![0u64; elem_slots];

    for i in 0..length {
        unpack_value(gc, data, cursor, &mut elem_buf, struct_metas, runtime_types);
        write_element(data_ptr, i, elem_bytes, &elem_buf);
    }

    new_arr
}

fn unpack_struct_inline(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) {
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
        unpack_value(gc, data, cursor, field_dst, struct_metas, runtime_types);
    }
}

fn unpack_pointer(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> GcRef {
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
    
    unpack_value(gc, data, cursor, &mut obj_slots, struct_metas, runtime_types);
    
    // Write slots to new object
    for (i, &val) in obj_slots.iter().enumerate() {
        unsafe { Gc::write_slot(new_obj, i, val) };
    }

    new_obj
}

fn unpack_map(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> GcRef {
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
        unpack_value(gc, data, cursor, &mut key_buf, struct_metas, runtime_types);
        unpack_value(gc, data, cursor, &mut val_buf, struct_metas, runtime_types);
        map::set(new_map, &key_buf, &val_buf, None);
    }

    new_map
}

// =============================================================================
// Helper Functions
// =============================================================================

fn read_u64(data: &[u8], cursor: &mut usize) -> u64 {
    let bytes: [u8; 8] = data[*cursor..*cursor + 8].try_into().expect("pack: insufficient data for u64");
    *cursor += 8;
    u64::from_le_bytes(bytes)
}

fn read_u32(data: &[u8], cursor: &mut usize) -> u32 {
    let bytes: [u8; 4] = data[*cursor..*cursor + 4].try_into().expect("pack: insufficient data for u32");
    *cursor += 4;
    u32::from_le_bytes(bytes)
}

fn read_u16(data: &[u8], cursor: &mut usize) -> u16 {
    let bytes: [u8; 2] = data[*cursor..*cursor + 2].try_into().expect("pack: insufficient data for u16");
    *cursor += 2;
    u16::from_le_bytes(bytes)
}

fn read_element(base_ptr: *mut u8, idx: usize, elem_bytes: usize, elem_meta: ValueMeta, dst: &mut [u64]) {
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

    #[test]
    fn test_pack_unpack_scalar() {
        let mut gc = Gc::new();
        let struct_metas = vec![];
        let runtime_types = vec![];

        // Test integer
        let src = [42u64];
        let packed = pack_slots(&gc, &src, ValueMeta::new(0, ValueKind::Int64), &struct_metas, &runtime_types);
        
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
        
        let packed = pack_slots(&gc, &src, ValueMeta::new(0, ValueKind::String), &struct_metas, &runtime_types);
        
        let mut dst = [0u64];
        unpack_slots(&mut gc, &packed, &mut dst, &struct_metas, &runtime_types);
        
        let unpacked_str = dst[0] as GcRef;
        assert_eq!(string::as_str(unpacked_str), "hello");
        // Verify it's a different GcRef (deep copy)
        assert_ne!(str_ref, unpacked_str);
    }
}
