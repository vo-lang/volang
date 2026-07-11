#![allow(clippy::missing_safety_doc, clippy::not_unsafe_ptr_arg_deref)]
//! Deep comparison for interface values.
//!
//! Shared logic used by both VM (exec_iface_eq) and JIT (vo_iface_eq).
//!
//! # Safety contract
//! Unsafe comparisons require canonical live objects matching the supplied
//! runtime type and metadata graph for the duration of recursive traversal.

use crate::gc::GcRef;
use crate::objects::{array, string};
use crate::slot::{slots_for_bytes, SLOT_BYTES};
use crate::{RuntimeType, SlotType, ValueKind, ValueRttid};
use vo_common_core::bytecode::Module;

pub const HASH_K: u64 = 0xf1357aea2e62a9c5;
pub const HASH_SEED: u64 = 0x517cc1b727220a95;
pub const UNCOMPARABLE_INTERFACE_ERROR: &str =
    "runtime error: comparing uncomparable type in interface value";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnhashableType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EqResult {
    NotEqual,
    Equal,
    Uncomparable,
}

impl EqResult {
    #[inline]
    fn to_iface_code(self) -> u64 {
        match self {
            EqResult::NotEqual => 0,
            EqResult::Equal => 1,
            EqResult::Uncomparable => 2,
        }
    }
}

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
                    Some(RuntimeType::Named {
                        struct_meta_id: Some(id),
                        ..
                    }) => *id,
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

fn array_elem_rttid(rttid: u32, module: &Module) -> Option<ValueRttid> {
    match module.runtime_types.get(rttid as usize)? {
        RuntimeType::Array { elem, .. } => Some(*elem),
        RuntimeType::Named { id, .. } => {
            let underlying = module.named_type_metas.get(*id as usize)?.underlying_rttid;
            match module.runtime_types.get(underlying.rttid() as usize)? {
                RuntimeType::Array { elem, .. } => Some(*elem),
                _ => None,
            }
        }
        _ => None,
    }
}

fn value_rttid_is_comparable(value: ValueRttid, module: &Module, depth: usize) -> bool {
    value_type_is_comparable(value.rttid(), value.value_kind(), module, depth)
}

fn value_type_is_comparable(rttid: u32, vk: ValueKind, module: &Module, depth: usize) -> bool {
    if depth
        > module
            .runtime_types
            .len()
            .saturating_add(module.struct_metas.len())
            .max(32)
    {
        return false;
    }

    match vk {
        ValueKind::Slice | ValueKind::Map | ValueKind::Closure => false,
        ValueKind::Struct => get_struct_meta(rttid, module)
            .map(|meta| {
                meta.fields
                    .iter()
                    .all(|field| value_rttid_is_comparable(field.type_info, module, depth + 1))
            })
            .unwrap_or(true),
        ValueKind::Array => array_elem_rttid(rttid, module)
            .map(|elem| value_rttid_is_comparable(elem, module, depth + 1))
            .unwrap_or(true),
        _ => true,
    }
}

/// Return whether a runtime type can participate in equality/hash operations.
///
/// Interface values need this static check first, then recursive dynamic checks
/// for interface-typed fields/elements during equality and hashing.
pub fn value_is_comparable(rttid: u32, vk: ValueKind, module: &Module) -> bool {
    value_type_is_comparable(rttid, vk, module, 0)
}

pub fn iface_is_hashable(slot0: u64, module: &Module) -> bool {
    let vk = ValueKind::from_u8((slot0 & 0xFF) as u8);
    let rttid = ((slot0 >> 8) & 0xFFFFFF) as u32;
    value_is_comparable(rttid, vk, module)
}

/// Deep hash of inline struct data (key slots), considering string fields by content.
/// For map keys with struct type containing string fields.
/// `key` is the struct field data laid out directly in the key slots.
pub unsafe fn deep_hash_struct_inline_checked(
    key: &[u64],
    rttid: u32,
    module: &Module,
) -> Result<u64, UnhashableType> {
    if !value_is_comparable(rttid, ValueKind::Struct, module) {
        return Err(UnhashableType);
    }

    let struct_meta = match get_struct_meta(rttid, module) {
        Some(m) => m,
        None => return Ok(shallow_hash_inline(key)),
    };

    deep_hash_struct_slots_checked(struct_meta, key, module)
}

/// Deep hash of inline struct data (key slots), considering string fields by content.
/// For map keys with struct type containing string fields.
/// `key` is the struct field data laid out directly in the key slots.
pub unsafe fn deep_hash_struct_inline(key: &[u64], rttid: u32, module: &Module) -> u64 {
    deep_hash_struct_inline_checked(key, rttid, module).unwrap_or_else(|_| shallow_hash_inline(key))
}

unsafe fn deep_hash_struct_ref_checked(
    ptr: GcRef,
    rttid: u32,
    module: &Module,
) -> Result<u64, UnhashableType> {
    if ptr.is_null() {
        return Ok(HASH_SEED.rotate_left(5));
    }

    if !value_is_comparable(rttid, ValueKind::Struct, module) {
        return Err(UnhashableType);
    }

    let struct_meta = match get_struct_meta(rttid, module) {
        Some(m) => m,
        None => return Ok((HASH_SEED.wrapping_add(ptr as u64).wrapping_mul(HASH_K)).rotate_left(5)),
    };
    let len = struct_meta.slot_types.len();
    let slots = unsafe { core::slice::from_raw_parts(ptr, len) };
    deep_hash_struct_slots_checked(struct_meta, slots, module)
}

unsafe fn deep_hash_struct_slots_checked(
    struct_meta: &vo_common_core::bytecode::StructMeta,
    key: &[u64],
    module: &Module,
) -> Result<u64, UnhashableType> {
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
                        let s = unsafe { string::bytes_unchecked(val as GcRef) };
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
                let slot1 = if i + 1 < key.len() { key[i + 1] } else { 0 };
                let iface_hash = iface_hash_checked(val, slot1, module)?;
                h = h.wrapping_add(iface_hash).wrapping_mul(HASH_K);
                i += 1;
            }
            SlotType::Interface1 => {}
        }
        i += 1;
    }
    Ok(h.rotate_left(5))
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
    if a == b {
        Some(true)
    } else if a == 0 || b == 0 {
        Some(false)
    } else {
        None
    }
}

/// Compare two interface values for equality.
/// Returns: 0 = not equal, 1 = equal, 2 = panic (uncomparable type)
pub unsafe fn iface_eq(
    b_slot0: u64,
    b_slot1: u64,
    c_slot0: u64,
    c_slot1: u64,
    module: &Module,
) -> u64 {
    // slot0 format: [itab_id:32 | rttid:24 | vk:8]
    // Compare only rttid + vk (low 32 bits), NOT itab_id
    let b_type = (b_slot0 & 0xFFFFFFFF) as u32;
    let c_type = (c_slot0 & 0xFFFFFFFF) as u32;

    if b_type != c_type {
        return 0;
    }

    let vk = ValueKind::from_u8((b_slot0 & 0xFF) as u8);
    let rttid = ((b_slot0 >> 8) & 0xFFFFFF) as u32;

    if !value_is_comparable(rttid, vk, module) {
        return 2;
    }

    let result = match vk {
        ValueKind::String => {
            if try_shallow_eq(b_slot1, c_slot1).unwrap_or_else(|| unsafe {
                string::bytes_unchecked(b_slot1 as GcRef)
                    == string::bytes_unchecked(c_slot1 as GcRef)
            }) {
                EqResult::Equal
            } else {
                EqResult::NotEqual
            }
        }
        ValueKind::Struct => match try_shallow_eq(b_slot1, c_slot1) {
            Some(true) => EqResult::Equal,
            Some(false) => EqResult::NotEqual,
            None => deep_eq_struct_result(b_slot1 as GcRef, c_slot1 as GcRef, rttid, module),
        },
        ValueKind::Array => match try_shallow_eq(b_slot1, c_slot1) {
            Some(true) => EqResult::Equal,
            Some(false) => EqResult::NotEqual,
            None => deep_eq_array_result(b_slot1 as GcRef, c_slot1 as GcRef, rttid, module),
        },
        _ => {
            if b_slot1 == c_slot1 {
                EqResult::Equal
            } else {
                EqResult::NotEqual
            }
        }
    };

    result.to_iface_code()
}

/// Core struct comparison logic. Returns true if equal.
/// `get_slot` returns (a_val, b_val, a_next, b_next) for the given index.
unsafe fn deep_eq_struct_core<F>(
    struct_meta: &vo_common_core::bytecode::StructMeta,
    len: usize,
    module: &Module,
    mut get_slot: F,
) -> EqResult
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
                    return EqResult::NotEqual;
                }
            }
            SlotType::GcRef => {
                if a_val != b_val {
                    let slot_vk = find_slot_value_kind(struct_meta, i as u16, module);

                    if slot_vk == ValueKind::String {
                        if a_val == 0 || b_val == 0 {
                            return EqResult::NotEqual;
                        }
                        let a_str = unsafe { string::bytes_unchecked(a_val as GcRef) };
                        let b_str = unsafe { string::bytes_unchecked(b_val as GcRef) };
                        if a_str != b_str {
                            return EqResult::NotEqual;
                        }
                    } else {
                        return EqResult::NotEqual;
                    }
                }
            }
            SlotType::Interface0 => {
                let result = iface_eq(a_val, a_next, b_val, b_next, module);
                match result {
                    1 => {}
                    2 => return EqResult::Uncomparable,
                    _ => return EqResult::NotEqual,
                }
                i += 1;
            }
            SlotType::Interface1 => {}
        }
        i += 1;
    }
    EqResult::Equal
}

/// Resolve rttid to StructMeta, returning None if not found.
fn get_struct_meta(rttid: u32, module: &Module) -> Option<&vo_common_core::bytecode::StructMeta> {
    let struct_meta_id = match module.runtime_types.get(rttid as usize) {
        Some(RuntimeType::Named {
            struct_meta_id: Some(id),
            ..
        }) => *id,
        Some(RuntimeType::Struct { meta_id, .. }) => *meta_id,
        _ => return None,
    };
    module.struct_metas.get(struct_meta_id as usize)
}

/// Deep comparison of two inline struct key data.
pub unsafe fn deep_eq_struct_inline(a: &[u64], b: &[u64], rttid: u32, module: &Module) -> bool {
    if a.len() != b.len() {
        return false;
    }

    let struct_meta = match get_struct_meta(rttid, module) {
        Some(m) => m,
        None => return a == b,
    };

    matches!(
        deep_eq_struct_core(struct_meta, a.len(), module, |i| {
            let a_next = if i + 1 < a.len() { a[i + 1] } else { 0 };
            let b_next = if i + 1 < b.len() { b[i + 1] } else { 0 };
            (a[i], b[i], a_next, b_next)
        }),
        EqResult::Equal
    )
}

unsafe fn deep_eq_struct_result(a: GcRef, b: GcRef, rttid: u32, module: &Module) -> EqResult {
    let struct_meta = match get_struct_meta(rttid, module) {
        Some(m) => m,
        None => {
            return if a == b {
                EqResult::Equal
            } else {
                EqResult::NotEqual
            }
        }
    };

    let len = struct_meta.slot_types.len();
    deep_eq_struct_core(struct_meta, len, module, |i| {
        let a_val = unsafe { *a.add(i) };
        let b_val = unsafe { *b.add(i) };
        let a_next = if i + 1 < len {
            unsafe { *a.add(i + 1) }
        } else {
            0
        };
        let b_next = if i + 1 < len {
            unsafe { *b.add(i + 1) }
        } else {
            0
        };
        (a_val, b_val, a_next, b_next)
    })
}

/// Deep comparison of two struct values on heap.
pub unsafe fn deep_eq_struct(a: GcRef, b: GcRef, rttid: u32, module: &Module) -> bool {
    matches!(deep_eq_struct_result(a, b, rttid, module), EqResult::Equal)
}

/// Deep comparison of two array values.
/// Hash an interface value for use as map key.
/// Uses content-based hashing for comparable types (string, struct, array, primitives).
pub unsafe fn iface_hash_checked(
    slot0: u64,
    slot1: u64,
    module: &Module,
) -> Result<u64, UnhashableType> {
    let vk = ValueKind::from_u8((slot0 & 0xFF) as u8);
    let rttid = ((slot0 >> 8) & 0xFFFFFF) as u32;
    if !value_is_comparable(rttid, vk, module) {
        return Err(UnhashableType);
    }

    let mut h = HASH_SEED;
    // Include type info in hash
    h = h.wrapping_add(slot0 & 0xFFFFFFFF).wrapping_mul(HASH_K);

    match vk {
        ValueKind::String => {
            if slot1 != 0 {
                let s = unsafe { string::bytes_unchecked(slot1 as GcRef) };
                for &b in s {
                    h = h.wrapping_add(b as u64).wrapping_mul(HASH_K);
                }
            }
        }
        ValueKind::Struct => {
            if slot1 != 0 {
                h = h
                    .wrapping_add(deep_hash_struct_ref_checked(slot1 as GcRef, rttid, module)?)
                    .wrapping_mul(HASH_K);
            }
        }
        ValueKind::Array => {
            h = h
                .wrapping_add(deep_hash_array_checked(slot1 as GcRef, rttid, module)?)
                .wrapping_mul(HASH_K);
        }
        _ => {
            // Primitives: hash the value directly
            h = h.wrapping_add(slot1).wrapping_mul(HASH_K);
        }
    }

    Ok(h.rotate_left(5))
}

pub unsafe fn iface_hash(slot0: u64, slot1: u64, module: &Module) -> u64 {
    iface_hash_checked(slot0, slot1, module)
        .unwrap_or_else(|_| shallow_hash_inline(&[slot0, slot1]))
}

unsafe fn deep_hash_array_checked(
    arr: GcRef,
    rttid: u32,
    module: &Module,
) -> Result<u64, UnhashableType> {
    if arr.is_null() {
        return Ok(HASH_SEED.rotate_left(5));
    }
    if !value_is_comparable(rttid, ValueKind::Array, module) {
        return Err(UnhashableType);
    }

    let len = array::len(arr);
    let elem_vk = array::elem_kind(arr);
    let elem_bytes = array::elem_bytes(arr);
    let total_bytes = len * elem_bytes;
    let total_slots = slots_for_bytes(total_bytes);
    let data = array::data_ptr_bytes(arr) as *const u64;
    let elem_slots = slots_for_bytes(elem_bytes);
    let elem_rttid = array_elem_rttid(rttid, module)
        .map(|elem| elem.rttid())
        .unwrap_or(0);

    let mut h = HASH_SEED;
    match elem_vk {
        ValueKind::String => {
            for i in 0..len {
                let val = unsafe { *data.add(i * elem_slots) };
                if val != 0 {
                    for &b in unsafe { string::bytes_unchecked(val as GcRef) } {
                        h = h.wrapping_add(b as u64).wrapping_mul(HASH_K);
                    }
                }
            }
        }
        ValueKind::Interface => {
            for i in 0..len {
                let offset = i * elem_slots;
                let slot0 = unsafe { *data.add(offset) };
                let slot1 = unsafe { *data.add(offset + 1) };
                h = h
                    .wrapping_add(iface_hash_checked(slot0, slot1, module)?)
                    .wrapping_mul(HASH_K);
            }
        }
        ValueKind::Struct => {
            for i in 0..len {
                let offset = i * elem_slots;
                let slots = unsafe { core::slice::from_raw_parts(data.add(offset), elem_slots) };
                h = h
                    .wrapping_add(deep_hash_struct_inline_checked(slots, elem_rttid, module)?)
                    .wrapping_mul(HASH_K);
            }
        }
        _ => {
            for i in 0..total_slots {
                let val = unsafe { *data.add(i) };
                h = h.wrapping_add(val).wrapping_mul(HASH_K);
            }
        }
    }
    Ok(h.rotate_left(5))
}

unsafe fn deep_eq_array_result(a: GcRef, b: GcRef, rttid: u32, module: &Module) -> EqResult {
    let a_len = array::len(a);
    let b_len = array::len(b);

    if a_len != b_len {
        return EqResult::NotEqual;
    }

    let len = a_len;
    let elem_vk = array::elem_kind(a);
    let elem_bytes = array::elem_bytes(a);

    // For reference types (string, struct, array, etc.), elements are slot-aligned
    // For primitives, compare raw bytes
    let needs_deep_compare = matches!(
        elem_vk,
        ValueKind::String | ValueKind::Struct | ValueKind::Array | ValueKind::Interface
    );

    if !needs_deep_compare {
        // Primitives: compare raw data bytes
        let total_bytes = len * elem_bytes;
        let a_ptr = array::data_ptr_bytes(a);
        let b_ptr = array::data_ptr_bytes(b);
        return if unsafe {
            core::slice::from_raw_parts(a_ptr, total_bytes)
                == core::slice::from_raw_parts(b_ptr, total_bytes)
        } {
            EqResult::Equal
        } else {
            EqResult::NotEqual
        };
    }

    // Reference types: need element-by-element deep comparison
    let elem_slots = elem_bytes / SLOT_BYTES;
    let elem_rttid = array_elem_rttid(rttid, module)
        .map(|elem| elem.rttid())
        .unwrap_or(0);

    let a_data = array::data_ptr_bytes(a) as *const u64;
    let b_data = array::data_ptr_bytes(b) as *const u64;

    for i in 0..len {
        let offset = i * elem_slots;
        let a_val = unsafe { *a_data.add(offset) };
        let b_val = unsafe { *b_data.add(offset) };
        let a_next = if elem_slots > 1 {
            unsafe { *a_data.add(offset + 1) }
        } else {
            0
        };
        let b_next = if elem_slots > 1 {
            unsafe { *b_data.add(offset + 1) }
        } else {
            0
        };

        let eq = match elem_vk {
            ValueKind::String => {
                if try_shallow_eq(a_val, b_val).unwrap_or_else(|| unsafe {
                    string::bytes_unchecked(a_val as GcRef)
                        == string::bytes_unchecked(b_val as GcRef)
                }) {
                    EqResult::Equal
                } else {
                    EqResult::NotEqual
                }
            }
            ValueKind::Struct => {
                let slots = elem_slots;
                let a_slots = unsafe { core::slice::from_raw_parts(a_data.add(offset), slots) };
                let b_slots = unsafe { core::slice::from_raw_parts(b_data.add(offset), slots) };
                if deep_eq_struct_inline(a_slots, b_slots, elem_rttid, module) {
                    EqResult::Equal
                } else {
                    EqResult::NotEqual
                }
            }
            ValueKind::Array => match try_shallow_eq(a_val, b_val) {
                Some(true) => EqResult::Equal,
                Some(false) => EqResult::NotEqual,
                None => deep_eq_array_result(a_val as GcRef, b_val as GcRef, elem_rttid, module),
            },
            ValueKind::Interface => match iface_eq(a_val, a_next, b_val, b_next, module) {
                1 => EqResult::Equal,
                2 => EqResult::Uncomparable,
                _ => EqResult::NotEqual,
            },
            _ => unreachable!(),
        };
        if eq != EqResult::Equal {
            return eq;
        }
    }
    EqResult::Equal
}

pub unsafe fn deep_eq_array(a: GcRef, b: GcRef, rttid: u32, module: &Module) -> bool {
    matches!(deep_eq_array_result(a, b, rttid, module), EqResult::Equal)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::objects::interface;
    use crate::{RuntimeType, StructMeta, ValueRttid};
    use std::collections::{BTreeMap, HashMap};
    use vo_common_core::bytecode::{FieldMeta, NamedTypeMeta};

    fn module_with_nested_unhashable_struct() -> (Module, u32, u32) {
        let mut module = Module::new("compare-test".to_string());
        let int_rttid = module.runtime_types.len() as u32;
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::Int));

        let slice_rttid = module.runtime_types.len() as u32;
        module
            .runtime_types
            .push(RuntimeType::Slice(ValueRttid::new(
                int_rttid,
                ValueKind::Int,
            )));

        let struct_meta_id = module.struct_metas.len() as u32;
        module.struct_metas.push(StructMeta {
            slot_types: vec![SlotType::GcRef],
            fields: vec![FieldMeta {
                name: "xs".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(slice_rttid, ValueKind::Slice),
                embedded: false,
                tag: None,
            }],
            field_index: HashMap::from_iter([("xs".to_string(), 0)]),
        });

        let struct_rttid = module.runtime_types.len() as u32;
        module.runtime_types.push(RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: struct_meta_id,
        });

        let array_rttid = module.runtime_types.len() as u32;
        module.runtime_types.push(RuntimeType::Array {
            len: 1,
            elem: ValueRttid::new(slice_rttid, ValueKind::Slice),
        });

        (module, struct_rttid, array_rttid)
    }

    #[test]
    fn iface_comparable_rejects_nested_uncomparable_struct_and_array() {
        let (module, struct_rttid, array_rttid) = module_with_nested_unhashable_struct();

        assert!(!value_is_comparable(
            struct_rttid,
            ValueKind::Struct,
            &module
        ));
        assert!(!value_is_comparable(array_rttid, ValueKind::Array, &module));

        let struct_slot0 = interface::pack_slot0(0, struct_rttid, ValueKind::Struct);
        assert_eq!(
            unsafe { iface_eq(struct_slot0, 0, struct_slot0, 0, &module) },
            2
        );
    }

    #[test]
    fn iface_hashable_rejects_nested_unhashable_struct_and_array() {
        let (module, struct_rttid, array_rttid) = module_with_nested_unhashable_struct();

        let struct_slot0 = interface::pack_slot0(0, struct_rttid, ValueKind::Struct);
        let array_slot0 = interface::pack_slot0(0, array_rttid, ValueKind::Array);
        assert!(!iface_is_hashable(struct_slot0, &module));
        assert!(!iface_is_hashable(array_slot0, &module));
        assert!(unsafe { iface_hash_checked(struct_slot0, 0, &module) }.is_err());
        assert!(unsafe { iface_hash_checked(array_slot0, 0, &module) }.is_err());
    }

    #[test]
    fn named_struct_uses_underlying_comparability() {
        let (mut module, struct_rttid, _) = module_with_nested_unhashable_struct();
        let named_id = module.named_type_metas.len() as u32;
        module.named_type_metas.push(NamedTypeMeta {
            name: "NamedSliceStruct".to_string(),
            underlying_meta: vo_common_core::types::ValueMeta::new(0, ValueKind::Struct),
            underlying_rttid: ValueRttid::new(struct_rttid, ValueKind::Struct),
            methods: BTreeMap::new(),
        });
        let named_rttid = module.runtime_types.len() as u32;
        module.runtime_types.push(RuntimeType::Named {
            id: named_id,
            struct_meta_id: Some(0),
        });

        assert!(!value_is_comparable(
            named_rttid,
            ValueKind::Struct,
            &module
        ));
    }
}
