//! Deep comparison for interface values.
//!
//! Shared logic used by both VM (exec_iface_eq) and JIT (vo_iface_eq).

use crate::gc::GcRef;
use crate::objects::{array, string};
use crate::slot::{slots_for_bytes, SLOT_BYTES};
use crate::{RuntimeType, SlotType, ValueKind};
use vo_common_core::bytecode::Module;

pub const HASH_K: u64 = 0xf1357aea2e62a9c5;
pub const HASH_SEED: u64 = 0x517cc1b727220a95;

/// Find the actual ValueKind for a slot, recursively descending into nested structs.
/// Returns the ValueKind of the leaf field at the given slot offset.
fn find_slot_value_kind(
    struct_meta: &vo_common_core::bytecode::StructMeta,
    slot_offset: u16,
    module: &Module,
) -> ValueKind {
    for field in &struct_meta.fields {
        let field_end = field.offset + field.slot_count;
        if slot_offset >= field.offset && slot_offset < field_end {
            let field_vk = field.type_info.value_kind();
            if field_vk == ValueKind::Struct {
                // Nested struct: recursively find the actual slot type
                let inner_rttid = field.type_info.rttid();
                let inner_meta_id = match module.runtime_types.get(inner_rttid as usize) {
                    Some(RuntimeType::Named { struct_meta_id: Some(id), .. }) => *id,
                    Some(RuntimeType::Struct { meta_id, .. }) => *meta_id,
                    _ => return ValueKind::Void,
                };
                if let Some(inner_meta) = module.struct_metas.get(inner_meta_id as usize) {
                    let inner_offset = slot_offset - field.offset;
                    return find_slot_value_kind(inner_meta, inner_offset, module);
                }
            }
            return field_vk;
        }
    }
    ValueKind::Void
}

/// Deep hash of inline struct data (key slots), considering string fields by content.
/// For map keys with struct type containing string fields.
/// `key` is the struct field data laid out directly in the key slots.
pub fn deep_hash_struct_inline(key: &[u64], rttid: u32, module: &Module) -> u64 {
    let struct_meta = match get_struct_meta(rttid, module) {
        Some(m) => m,
        None => return shallow_hash_inline(key),
    };
    
    let slot_types = &struct_meta.slot_types;
    let mut h = HASH_SEED;
    let mut i = 0;
    
    while i < slot_types.len() && i < key.len() {
        let val = key[i];
        
        match slot_types[i] {
            SlotType::Value | SlotType::Float => {
                h = h.wrapping_add(val).wrapping_mul(HASH_K);
            }
            SlotType::GcRef => {
                // Find actual slot type, recursively descending into nested structs
                let slot_vk = find_slot_value_kind(struct_meta, i as u16, module);
                
                if slot_vk == ValueKind::String {
                    // Hash string by content
                    if val == 0 {
                        h = h.wrapping_add(0).wrapping_mul(HASH_K);
                    } else {
                        let s = string::as_bytes(val as GcRef);
                        for &b in s {
                            h = h.wrapping_add(b as u64).wrapping_mul(HASH_K);
                        }
                    }
                } else {
                    // Other GcRef: hash by pointer (must be same object to be equal)
                    h = h.wrapping_add(val).wrapping_mul(HASH_K);
                }
            }
            SlotType::Interface0 => {
                // Hash both slots of interface
                h = h.wrapping_add(val).wrapping_mul(HASH_K);
                if i + 1 < key.len() {
                    let slot1 = key[i + 1];
                    h = h.wrapping_add(slot1).wrapping_mul(HASH_K);
                }
                i += 1; // Skip Interface1
            }
            SlotType::Interface1 => {}
        }
        i += 1;
    }
    h.rotate_left(5)
}

/// Simple hash of inline key slots.
fn shallow_hash_inline(key: &[u64]) -> u64 {
    let mut h = HASH_SEED;
    for &val in key {
        h = h.wrapping_add(val).wrapping_mul(HASH_K);
    }
    h.rotate_left(5)
}

/// Compare two GcRef values with early-out for same pointer or null.
/// Returns Some(result) if comparison is done, None if deep comparison needed.
#[inline]
fn try_shallow_eq(a: u64, b: u64) -> Option<bool> {
    if a == b { Some(true) }
    else if a == 0 || b == 0 { Some(false) }
    else { None }
}

/// Compare two interface values for equality.
/// Returns: 0 = not equal, 1 = equal, 2 = panic (uncomparable type)
pub fn iface_eq(b_slot0: u64, b_slot1: u64, c_slot0: u64, c_slot1: u64, module: &Module) -> u64 {
    // slot0 format: [itab_id:32 | rttid:24 | vk:8]
    // Compare only rttid + vk (low 32 bits), NOT itab_id
    let b_type = (b_slot0 & 0xFFFFFFFF) as u32;
    let c_type = (c_slot0 & 0xFFFFFFFF) as u32;
    
    if b_type != c_type {
        return 0;
    }
    
    let vk = ValueKind::from_u8((b_slot0 & 0xFF) as u8);
    let rttid = ((b_slot0 >> 8) & 0xFFFFFF) as u32;
    
    // Check for uncomparable types - return 2 to signal panic
    // Note: Port and Island ARE comparable (identity comparison like Pointer)
    match vk {
        ValueKind::Slice | ValueKind::Map | ValueKind::Closure => {
            return 2;
        }
        _ => {}
    }
    
    let eq = match vk {
        ValueKind::String => try_shallow_eq(b_slot1, c_slot1).unwrap_or_else(|| {
            string::as_str(b_slot1 as GcRef) == string::as_str(c_slot1 as GcRef)
        }),
        ValueKind::Struct => try_shallow_eq(b_slot1, c_slot1).unwrap_or_else(|| {
            deep_eq_struct(b_slot1 as GcRef, c_slot1 as GcRef, rttid, module)
        }),
        ValueKind::Array => try_shallow_eq(b_slot1, c_slot1).unwrap_or_else(|| {
            deep_eq_array(b_slot1 as GcRef, c_slot1 as GcRef, rttid, module)
        }),
        _ => b_slot1 == c_slot1,
    };
    
    eq as u64
}

/// Core struct comparison logic. Returns true if equal.
/// `get_slot` returns (a_val, b_val, a_next, b_next) for the given index.
fn deep_eq_struct_core<F>(
    struct_meta: &vo_common_core::bytecode::StructMeta,
    len: usize,
    module: &Module,
    mut get_slot: F,
) -> bool
where
    F: FnMut(usize) -> (u64, u64, u64, u64),
{
    let slot_types = &struct_meta.slot_types;
    let mut i = 0;
    while i < slot_types.len() && i < len {
        let (a_val, b_val, a_next, b_next) = get_slot(i);
        
        match slot_types[i] {
            SlotType::Value | SlotType::Float => {
                if a_val != b_val {
                    return false;
                }
            }
            SlotType::GcRef => {
                if a_val != b_val {
                    let slot_vk = find_slot_value_kind(struct_meta, i as u16, module);
                    
                    if slot_vk == ValueKind::String {
                        if a_val == 0 || b_val == 0 {
                            return false;
                        }
                        let a_str = string::as_str(a_val as GcRef);
                        let b_str = string::as_str(b_val as GcRef);
                        if a_str != b_str {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
            }
            SlotType::Interface0 => {
                let result = iface_eq(a_val, a_next, b_val, b_next, module);
                if result != 1 {
                    return false;
                }
                i += 1;
            }
            SlotType::Interface1 => {}
        }
        i += 1;
    }
    true
}

/// Resolve rttid to StructMeta, returning None if not found.
fn get_struct_meta(rttid: u32, module: &Module) -> Option<&vo_common_core::bytecode::StructMeta> {
    let struct_meta_id = match module.runtime_types.get(rttid as usize) {
        Some(RuntimeType::Named { struct_meta_id: Some(id), .. }) => *id,
        Some(RuntimeType::Struct { meta_id, .. }) => *meta_id,
        _ => return None,
    };
    module.struct_metas.get(struct_meta_id as usize)
}

/// Deep comparison of two inline struct key data.
pub fn deep_eq_struct_inline(a: &[u64], b: &[u64], rttid: u32, module: &Module) -> bool {
    if a.len() != b.len() {
        return false;
    }
    
    let struct_meta = match get_struct_meta(rttid, module) {
        Some(m) => m,
        None => return a == b,
    };
    
    deep_eq_struct_core(struct_meta, a.len(), module, |i| {
        let a_next = if i + 1 < a.len() { a[i + 1] } else { 0 };
        let b_next = if i + 1 < b.len() { b[i + 1] } else { 0 };
        (a[i], b[i], a_next, b_next)
    })
}

/// Deep comparison of two struct values on heap.
pub fn deep_eq_struct(a: GcRef, b: GcRef, rttid: u32, module: &Module) -> bool {
    let struct_meta = match get_struct_meta(rttid, module) {
        Some(m) => m,
        None => return a == b,
    };
    
    let len = struct_meta.slot_types.len();
    deep_eq_struct_core(struct_meta, len, module, |i| {
        let a_val = unsafe { *a.add(i) };
        let b_val = unsafe { *b.add(i) };
        let a_next = unsafe { *a.add(i + 1) };
        let b_next = unsafe { *b.add(i + 1) };
        (a_val, b_val, a_next, b_next)
    })
}

/// Deep comparison of two array values.
/// Hash an interface value for use as map key.
/// Uses content-based hashing for comparable types (string, struct, array, primitives).
pub fn iface_hash(slot0: u64, slot1: u64, module: &Module) -> u64 {
    let vk = ValueKind::from_u8((slot0 & 0xFF) as u8);
    let rttid = ((slot0 >> 8) & 0xFFFFFF) as u32;
    
    let mut h = HASH_SEED;
    // Include type info in hash
    h = h.wrapping_add(slot0 & 0xFFFFFFFF).wrapping_mul(HASH_K);
    
    match vk {
        ValueKind::String => {
            if slot1 != 0 {
                let s = string::as_bytes(slot1 as GcRef);
                for &b in s {
                    h = h.wrapping_add(b as u64).wrapping_mul(HASH_K);
                }
            }
        }
        ValueKind::Struct => {
            if slot1 != 0 {
                if let Some(struct_meta) = get_struct_meta(rttid, module) {
                    let ptr = slot1 as GcRef;
                    for i in 0..struct_meta.slot_types.len() {
                        let val = unsafe { *ptr.add(i) };
                        h = h.wrapping_add(val).wrapping_mul(HASH_K);
                    }
                } else {
                    h = h.wrapping_add(slot1).wrapping_mul(HASH_K);
                }
            }
        }
        ValueKind::Array => {
            if slot1 != 0 {
                let arr = slot1 as GcRef;
                let len = array::len(arr);
                let elem_bytes = array::elem_bytes(arr);
                // Hash by raw data slots (handles both packed and slot-aligned arrays)
                let total_bytes = len * elem_bytes;
                let total_slots = slots_for_bytes(total_bytes);
                let data = array::data_ptr_bytes(arr) as *const u64;
                for i in 0..total_slots {
                    let val = unsafe { *data.add(i) };
                    h = h.wrapping_add(val).wrapping_mul(HASH_K);
                }
            }
        }
        _ => {
            // Primitives: hash the value directly
            h = h.wrapping_add(slot1).wrapping_mul(HASH_K);
        }
    }
    
    h.rotate_left(5)
}

pub fn deep_eq_array(a: GcRef, b: GcRef, rttid: u32, module: &Module) -> bool {
    let a_len = array::len(a);
    let b_len = array::len(b);
    
    if a_len != b_len {
        return false;
    }
    
    let len = a_len;
    let elem_vk = array::elem_kind(a);
    let elem_bytes = array::elem_bytes(a);
    
    // For reference types (string, struct, array, etc.), elements are slot-aligned
    // For primitives, compare raw bytes
    let needs_deep_compare = matches!(
        elem_vk,
        ValueKind::String | ValueKind::Struct | ValueKind::Array
    );
    
    if !needs_deep_compare {
        // Primitives: compare raw data bytes
        let total_bytes = len * elem_bytes;
        let a_ptr = array::data_ptr_bytes(a);
        let b_ptr = array::data_ptr_bytes(b);
        return unsafe {
            core::slice::from_raw_parts(a_ptr, total_bytes)
                == core::slice::from_raw_parts(b_ptr, total_bytes)
        };
    }
    
    // Reference types: need element-by-element deep comparison
    let elem_slots = elem_bytes / SLOT_BYTES;
    let elem_rttid = match module.runtime_types.get(rttid as usize) {
        Some(RuntimeType::Array { elem, .. }) => elem.rttid(),
        Some(RuntimeType::Named { id, .. }) => {
            module.named_type_metas.get(*id as usize)
                .and_then(|meta| {
                    let underlying_rttid = meta.underlying_meta.meta_id();
                    match module.runtime_types.get(underlying_rttid as usize) {
                        Some(RuntimeType::Array { elem, .. }) => Some(elem.rttid()),
                        _ => None,
                    }
                })
                .unwrap_or(0)
        }
        _ => 0,
    };
    
    let a_data = array::data_ptr_bytes(a) as *const u64;
    let b_data = array::data_ptr_bytes(b) as *const u64;
    
    for i in 0..len {
        let offset = i * elem_slots;
        let a_val = unsafe { *a_data.add(offset) };
        let b_val = unsafe { *b_data.add(offset) };
        
        let eq = match elem_vk {
            ValueKind::String => try_shallow_eq(a_val, b_val).unwrap_or_else(|| {
                string::as_str(a_val as GcRef) == string::as_str(b_val as GcRef)
            }),
            ValueKind::Struct => try_shallow_eq(a_val, b_val).unwrap_or_else(|| {
                deep_eq_struct(a_val as GcRef, b_val as GcRef, elem_rttid, module)
            }),
            ValueKind::Array => try_shallow_eq(a_val, b_val).unwrap_or_else(|| {
                deep_eq_array(a_val as GcRef, b_val as GcRef, elem_rttid, module)
            }),
            _ => unreachable!(),
        };
        if !eq {
            return false;
        }
    }
    true
}
