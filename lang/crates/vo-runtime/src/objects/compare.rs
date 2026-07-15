#![allow(clippy::missing_safety_doc, clippy::not_unsafe_ptr_arg_deref)]
//! Deep comparison for interface values.
//!
//! Shared logic used by both VM (exec_iface_eq) and JIT (vo_iface_eq).
//!
//! # Safety contract
//! Unsafe comparisons require canonical live objects matching the supplied
//! runtime type and metadata graph for the duration of explicit work-stack traversal.

use crate::gc::GcRef;
use crate::objects::{array, string};
use crate::{RuntimeType, ValueKind, ValueRttid};
use hashbrown::HashSet;
use vo_common_core::bytecode::Module;

#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};
#[cfg(feature = "std")]
use std::vec::Vec;

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

#[inline]
pub(crate) fn float_slot_eq(kind: ValueKind, a: u64, b: u64) -> bool {
    match kind {
        ValueKind::Float32 => f32::from_bits(a as u32) == f32::from_bits(b as u32),
        ValueKind::Float64 => f64::from_bits(a) == f64::from_bits(b),
        _ => a == b,
    }
}

/// Canonical bits for hashing a floating-point map/interface key.
///
/// IEEE equality makes both signed zero encodings equal, so they must share a
/// hash. NaNs remain payload-sensitive; equality rejects every NaN regardless
/// of payload, and unequal values are allowed to collide or hash differently.
#[inline]
pub(crate) fn canonical_float_hash_bits(kind: ValueKind, bits: u64) -> u64 {
    match kind {
        ValueKind::Float32 => {
            let bits = bits as u32;
            if bits & 0x7fff_ffff == 0 {
                0
            } else {
                u64::from(bits)
            }
        }
        ValueKind::Float64 => {
            if bits & 0x7fff_ffff_ffff_ffff == 0 {
                0
            } else {
                bits
            }
        }
        _ => bits,
    }
}

#[inline]
pub(crate) fn float_key_hash(kind: ValueKind, bits: u64) -> u64 {
    HASH_SEED
        .wrapping_add(canonical_float_hash_bits(kind, bits))
        .wrapping_mul(HASH_K)
        .rotate_left(5)
}

fn array_elem_rttid(rttid: u32, module: &Module) -> Option<ValueRttid> {
    array_type_info(rttid, module).map(|(_, elem)| elem)
}

fn array_type_info(rttid: u32, module: &Module) -> Option<(u64, ValueRttid)> {
    let (_, runtime_type) = module
        .runtime_type_resolver()
        .resolve_value_rttid(ValueRttid::new(rttid, ValueKind::Array))?;
    let RuntimeType::Array { len, elem } = runtime_type else {
        return None;
    };
    Some((*len, *elem))
}

fn value_rttid_is_comparable(value: ValueRttid, module: &Module) -> bool {
    enum Task {
        Visit(ValueRttid),
        Exit(u32),
    }

    let mut active = HashSet::new();
    let mut complete = HashSet::new();
    let mut pending = vec![Task::Visit(value)];
    while let Some(task) = pending.pop() {
        match task {
            Task::Visit(value) => match value.value_kind() {
                ValueKind::Slice | ValueKind::Map | ValueKind::Closure => return false,
                ValueKind::Struct | ValueKind::Array => {
                    let key = value.to_raw();
                    if complete.contains(&key) {
                        continue;
                    }
                    if !active.insert(key) {
                        return false;
                    }
                    pending.push(Task::Exit(key));
                    match value.value_kind() {
                        ValueKind::Struct => {
                            let Some(meta) = get_struct_meta(value.rttid(), module) else {
                                return false;
                            };
                            pending.extend(
                                meta.fields
                                    .iter()
                                    .rev()
                                    .map(|field| Task::Visit(field.type_info)),
                            );
                        }
                        ValueKind::Array => {
                            let Some(elem) = array_elem_rttid(value.rttid(), module) else {
                                return false;
                            };
                            pending.push(Task::Visit(elem));
                        }
                        _ => unreachable!(),
                    }
                }
                _ => {}
            },
            Task::Exit(key) => {
                if !active.remove(&key) {
                    return false;
                }
                complete.insert(key);
            }
        }
    }
    true
}

/// Return whether a runtime type can participate in equality/hash operations.
///
/// Interface values need this static check first, then nested dynamic checks
/// for interface-typed fields/elements during equality and hashing.
pub fn value_is_comparable(rttid: u32, vk: ValueKind, module: &Module) -> bool {
    value_rttid_is_comparable(ValueRttid::new(rttid, vk), module)
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
    unsafe {
        deep_hash_value_inline_checked(key, ValueRttid::new(rttid, ValueKind::Struct), module)
    }
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
    unsafe {
        deep_hash_value_inline_checked(slots, ValueRttid::new(rttid, ValueKind::Struct), module)
    }
}

pub(crate) unsafe fn deep_hash_value_inline_checked(
    key: &[u64],
    value: ValueRttid,
    module: &Module,
) -> Result<u64, UnhashableType> {
    enum HashTask<'a> {
        Value {
            key: &'a [u64],
            value: ValueRttid,
            layout_validated: bool,
        },
        StructField {
            key: &'a [u64],
            meta: &'a vo_common_core::bytecode::StructMeta,
            index: usize,
            hash: u64,
        },
        StructCombine {
            key: &'a [u64],
            meta: &'a vo_common_core::bytecode::StructMeta,
            next_index: usize,
            hash: u64,
        },
        ArrayElement {
            key: &'a [u64],
            elem: ValueRttid,
            elem_slots: usize,
            index: usize,
            len: usize,
            hash: u64,
        },
        ArrayCombine {
            key: &'a [u64],
            elem: ValueRttid,
            elem_slots: usize,
            next_index: usize,
            len: usize,
            hash: u64,
        },
    }

    if !value_rttid_is_comparable(value, module) {
        return Err(UnhashableType);
    }

    let resolver = module.runtime_type_resolver();
    let mut pending = vec![HashTask::Value {
        key,
        value,
        layout_validated: false,
    }];
    let mut results = Vec::new();
    while let Some(task) = pending.pop() {
        match task {
            HashTask::Value {
                key,
                value,
                layout_validated,
            } => {
                let first = key.first().copied().unwrap_or_default();
                match value.value_kind() {
                    ValueKind::Float32 | ValueKind::Float64 => {
                        results.push(float_key_hash(value.value_kind(), first));
                    }
                    ValueKind::String => {
                        let mut hash = HASH_SEED;
                        if first != 0 {
                            for &byte in unsafe { string::bytes_unchecked(first as GcRef) } {
                                hash = hash.wrapping_add(u64::from(byte)).wrapping_mul(HASH_K);
                            }
                        }
                        results.push(hash.rotate_left(5));
                    }
                    ValueKind::Struct => {
                        let Some(meta) = get_struct_meta(value.rttid(), module) else {
                            return Err(UnhashableType);
                        };
                        if key.len() != meta.slot_types.len() {
                            return Err(UnhashableType);
                        }
                        pending.push(HashTask::StructField {
                            key,
                            meta,
                            index: 0,
                            hash: HASH_SEED,
                        });
                    }
                    ValueKind::Array => {
                        let Some((len, elem)) = array_type_info(value.rttid(), module) else {
                            return Err(UnhashableType);
                        };
                        if !layout_validated
                            && resolver.slot_count_for_value_rttid(value) != Some(key.len())
                        {
                            return Err(UnhashableType);
                        }
                        let len = usize::try_from(len).map_err(|_| UnhashableType)?;
                        let elem_slots = if len == 0 {
                            if !key.is_empty() {
                                return Err(UnhashableType);
                            }
                            0
                        } else {
                            if !key.len().is_multiple_of(len) {
                                return Err(UnhashableType);
                            }
                            key.len() / len
                        };
                        if elem_slots == 0 {
                            results
                                .push(repeat_hash_fold(HASH_SEED, HASH_SEED, len).rotate_left(5));
                        } else {
                            pending.push(HashTask::ArrayElement {
                                key,
                                elem,
                                elem_slots,
                                index: 0,
                                len,
                                hash: HASH_SEED,
                            });
                        }
                    }
                    ValueKind::Interface => {
                        let slot1 = key.get(1).copied().unwrap_or_default();
                        results.push(unsafe { iface_hash_checked(first, slot1, module)? });
                    }
                    ValueKind::Slice | ValueKind::Map | ValueKind::Closure => {
                        return Err(UnhashableType);
                    }
                    _ => results.push(shallow_hash_inline(key)),
                }
            }
            HashTask::StructField {
                key,
                meta,
                index,
                hash,
            } => {
                let Some(field) = meta.fields.get(index) else {
                    results.push(hash.rotate_left(5));
                    continue;
                };
                let start = usize::from(field.offset);
                let end = start
                    .checked_add(usize::from(field.slot_count))
                    .ok_or(UnhashableType)?;
                let field_slots = key.get(start..end).ok_or(UnhashableType)?;
                pending.push(HashTask::StructCombine {
                    key,
                    meta,
                    next_index: index + 1,
                    hash,
                });
                pending.push(HashTask::Value {
                    key: field_slots,
                    value: field.type_info,
                    layout_validated: false,
                });
            }
            HashTask::StructCombine {
                key,
                meta,
                next_index,
                hash,
            } => {
                let field_hash = results.pop().ok_or(UnhashableType)?;
                pending.push(HashTask::StructField {
                    key,
                    meta,
                    index: next_index,
                    hash: hash.wrapping_add(field_hash).wrapping_mul(HASH_K),
                });
            }
            HashTask::ArrayElement {
                key,
                elem,
                elem_slots,
                index,
                len,
                hash,
            } => {
                if index >= len {
                    results.push(hash.rotate_left(5));
                    continue;
                }
                let start = index.checked_mul(elem_slots).ok_or(UnhashableType)?;
                let end = start.checked_add(elem_slots).ok_or(UnhashableType)?;
                let elem_value = key.get(start..end).ok_or(UnhashableType)?;
                pending.push(HashTask::ArrayCombine {
                    key,
                    elem,
                    elem_slots,
                    next_index: index + 1,
                    len,
                    hash,
                });
                pending.push(HashTask::Value {
                    key: elem_value,
                    value: elem,
                    layout_validated: true,
                });
            }
            HashTask::ArrayCombine {
                key,
                elem,
                elem_slots,
                next_index,
                len,
                hash,
            } => {
                let elem_hash = results.pop().ok_or(UnhashableType)?;
                pending.push(HashTask::ArrayElement {
                    key,
                    elem,
                    elem_slots,
                    index: next_index,
                    len,
                    hash: hash.wrapping_add(elem_hash).wrapping_mul(HASH_K),
                });
            }
        }
    }
    match results.as_slice() {
        [hash] => Ok(*hash),
        _ => Err(UnhashableType),
    }
}

/// Apply `hash = (hash + value) * HASH_K` `count` times in O(log count).
fn repeat_hash_fold(mut hash: u64, value: u64, mut count: usize) -> u64 {
    let mut multiplier = HASH_K;
    let mut addend = value.wrapping_mul(HASH_K);
    while count != 0 {
        if count & 1 != 0 {
            hash = hash.wrapping_mul(multiplier).wrapping_add(addend);
        }
        addend = addend.wrapping_mul(multiplier).wrapping_add(addend);
        multiplier = multiplier.wrapping_mul(multiplier);
        count >>= 1;
    }
    hash
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
        ValueKind::Float32 | ValueKind::Float64 => {
            if float_slot_eq(vk, b_slot1, c_slot1) {
                EqResult::Equal
            } else {
                EqResult::NotEqual
            }
        }
        // Structs and arrays are value aggregates. Shared boxing identity does
        // not imply equality: a NaN leaf remains unequal to itself, and an
        // interface leaf with an uncomparable dynamic value must still panic.
        ValueKind::Struct => {
            deep_eq_struct_result(b_slot1 as GcRef, c_slot1 as GcRef, rttid, module)
        }
        ValueKind::Array => deep_eq_array_result(b_slot1 as GcRef, c_slot1 as GcRef, rttid, module),
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

/// Resolve rttid to StructMeta, returning None if not found.
fn get_struct_meta(rttid: u32, module: &Module) -> Option<&vo_common_core::bytecode::StructMeta> {
    let meta = module
        .runtime_type_resolver()
        .canonical_value_meta_for_value_rttid(ValueRttid::new(rttid, ValueKind::Struct))?;
    module.struct_metas.get(meta.meta_id() as usize)
}

unsafe fn deep_eq_value_inline_result(
    a: &[u64],
    b: &[u64],
    value: ValueRttid,
    module: &Module,
) -> EqResult {
    if a.len() != b.len() {
        return EqResult::NotEqual;
    }
    if !value_rttid_is_comparable(value, module) {
        return EqResult::Uncomparable;
    }

    enum EqTask<'a> {
        Value {
            a: &'a [u64],
            b: &'a [u64],
            value: ValueRttid,
            layout_validated: bool,
        },
        StructField {
            a: &'a [u64],
            b: &'a [u64],
            meta: &'a vo_common_core::bytecode::StructMeta,
            index: usize,
        },
        ArrayElement {
            a: &'a [u64],
            b: &'a [u64],
            elem: ValueRttid,
            elem_slots: usize,
            index: usize,
            len: usize,
        },
    }

    let resolver = module.runtime_type_resolver();
    let mut pending = vec![EqTask::Value {
        a,
        b,
        value,
        layout_validated: false,
    }];
    while let Some(task) = pending.pop() {
        match task {
            EqTask::Value {
                a,
                b,
                value,
                layout_validated,
            } => {
                if a.len() != b.len() {
                    return EqResult::NotEqual;
                }
                let a_first = a.first().copied().unwrap_or_default();
                let b_first = b.first().copied().unwrap_or_default();
                match value.value_kind() {
                    ValueKind::Float32 | ValueKind::Float64 => {
                        if !float_slot_eq(value.value_kind(), a_first, b_first) {
                            return EqResult::NotEqual;
                        }
                    }
                    ValueKind::String => {
                        if !try_shallow_eq(a_first, b_first).unwrap_or_else(|| unsafe {
                            string::bytes_unchecked(a_first as GcRef)
                                == string::bytes_unchecked(b_first as GcRef)
                        }) {
                            return EqResult::NotEqual;
                        }
                    }
                    ValueKind::Struct => {
                        let Some(meta) = get_struct_meta(value.rttid(), module) else {
                            return EqResult::Uncomparable;
                        };
                        if a.len() != meta.slot_types.len() {
                            return EqResult::Uncomparable;
                        }
                        pending.push(EqTask::StructField {
                            a,
                            b,
                            meta,
                            index: 0,
                        });
                    }
                    ValueKind::Array => {
                        let Some((len, elem)) = array_type_info(value.rttid(), module) else {
                            return EqResult::Uncomparable;
                        };
                        if !layout_validated
                            && resolver.slot_count_for_value_rttid(value) != Some(a.len())
                        {
                            return EqResult::Uncomparable;
                        }
                        let Ok(len) = usize::try_from(len) else {
                            return EqResult::Uncomparable;
                        };
                        let elem_slots = if len == 0 {
                            if !a.is_empty() {
                                return EqResult::Uncomparable;
                            }
                            0
                        } else {
                            if !a.len().is_multiple_of(len) {
                                return EqResult::Uncomparable;
                            }
                            a.len() / len
                        };
                        if elem_slots != 0 {
                            pending.push(EqTask::ArrayElement {
                                a,
                                b,
                                elem,
                                elem_slots,
                                index: 0,
                                len,
                            });
                        }
                    }
                    ValueKind::Interface => match unsafe {
                        iface_eq(
                            a_first,
                            a.get(1).copied().unwrap_or_default(),
                            b_first,
                            b.get(1).copied().unwrap_or_default(),
                            module,
                        )
                    } {
                        1 => {}
                        2 => return EqResult::Uncomparable,
                        _ => return EqResult::NotEqual,
                    },
                    ValueKind::Slice | ValueKind::Map | ValueKind::Closure => {
                        return EqResult::Uncomparable;
                    }
                    _ => {
                        if a != b {
                            return EqResult::NotEqual;
                        }
                    }
                }
            }
            EqTask::StructField { a, b, meta, index } => {
                let Some(field) = meta.fields.get(index) else {
                    continue;
                };
                let start = usize::from(field.offset);
                let Some(end) = start.checked_add(usize::from(field.slot_count)) else {
                    return EqResult::Uncomparable;
                };
                let (Some(a_field), Some(b_field)) = (a.get(start..end), b.get(start..end)) else {
                    return EqResult::Uncomparable;
                };
                pending.push(EqTask::StructField {
                    a,
                    b,
                    meta,
                    index: index + 1,
                });
                pending.push(EqTask::Value {
                    a: a_field,
                    b: b_field,
                    value: field.type_info,
                    layout_validated: false,
                });
            }
            EqTask::ArrayElement {
                a,
                b,
                elem,
                elem_slots,
                index,
                len,
            } => {
                if index >= len {
                    continue;
                }
                let Some(start) = index.checked_mul(elem_slots) else {
                    return EqResult::Uncomparable;
                };
                let Some(end) = start.checked_add(elem_slots) else {
                    return EqResult::Uncomparable;
                };
                let (Some(a_elem), Some(b_elem)) = (a.get(start..end), b.get(start..end)) else {
                    return EqResult::Uncomparable;
                };
                pending.push(EqTask::ArrayElement {
                    a,
                    b,
                    elem,
                    elem_slots,
                    index: index + 1,
                    len,
                });
                pending.push(EqTask::Value {
                    a: a_elem,
                    b: b_elem,
                    value: elem,
                    layout_validated: true,
                });
            }
        }
    }
    EqResult::Equal
}

pub(crate) unsafe fn deep_eq_value_inline(
    a: &[u64],
    b: &[u64],
    value: ValueRttid,
    module: &Module,
) -> bool {
    matches!(
        unsafe { deep_eq_value_inline_result(a, b, value, module) },
        EqResult::Equal
    )
}

/// Deep comparison of two inline struct key data.
pub unsafe fn deep_eq_struct_inline(a: &[u64], b: &[u64], rttid: u32, module: &Module) -> bool {
    unsafe { deep_eq_value_inline(a, b, ValueRttid::new(rttid, ValueKind::Struct), module) }
}

unsafe fn deep_eq_struct_result(a: GcRef, b: GcRef, rttid: u32, module: &Module) -> EqResult {
    if a.is_null() || b.is_null() {
        return if a == b {
            EqResult::Equal
        } else {
            EqResult::NotEqual
        };
    }
    let Some(struct_meta) = get_struct_meta(rttid, module) else {
        return if a == b {
            EqResult::Equal
        } else {
            EqResult::NotEqual
        };
    };
    let len = struct_meta.slot_types.len();
    let a_slots = unsafe { core::slice::from_raw_parts(a, len) };
    let b_slots = unsafe { core::slice::from_raw_parts(b, len) };
    unsafe {
        deep_eq_value_inline_result(
            a_slots,
            b_slots,
            ValueRttid::new(rttid, ValueKind::Struct),
            module,
        )
    }
}

/// Deep comparison of two struct values on heap.
pub unsafe fn deep_eq_struct(a: GcRef, b: GcRef, rttid: u32, module: &Module) -> bool {
    matches!(
        unsafe { deep_eq_struct_result(a, b, rttid, module) },
        EqResult::Equal
    )
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
        ValueKind::Float32 | ValueKind::Float64 => {
            h = h
                .wrapping_add(canonical_float_hash_bits(vk, slot1))
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
    let Some((declared_len, elem)) = array_type_info(rttid, module) else {
        return Err(UnhashableType);
    };
    if usize::try_from(declared_len).ok() != Some(len) {
        return Err(UnhashableType);
    }
    let Some(elem_meta) = module.canonical_value_meta_for_value_rttid(elem) else {
        return Err(UnhashableType);
    };
    if array::elem_meta(arr) != elem_meta {
        return Err(UnhashableType);
    }
    let elem_bytes = array::elem_bytes(arr);
    let Ok(elem_layout) = crate::pack::sequence_element_layout(
        elem_meta,
        elem_bytes,
        &module.struct_metas,
        &module.named_type_metas,
        &module.runtime_types,
    ) else {
        return Err(UnhashableType);
    };
    let logical_elem_slots = elem_layout.logical_slots;

    let mut h = HASH_SEED;
    if logical_elem_slots == 0 {
        return Ok(repeat_hash_fold(h, HASH_SEED, len).rotate_left(5));
    }
    if elem_bytes < logical_elem_slots.saturating_mul(core::mem::size_of::<u64>()) {
        if logical_elem_slots != 1 || elem_bytes > core::mem::size_of::<u64>() {
            return Err(UnhashableType);
        }
        for index in 0..len {
            let value = [unsafe { array::get(arr, index, elem_bytes) }];
            let elem_hash = unsafe { deep_hash_value_inline_checked(&value, elem, module)? };
            h = h.wrapping_add(elem_hash).wrapping_mul(HASH_K);
        }
    } else {
        let data = array::data_ptr_bytes(arr);
        for index in 0..len {
            let byte_offset = index.checked_mul(elem_bytes).ok_or(UnhashableType)?;
            let elem_ptr = unsafe { data.add(byte_offset) } as *const u64;
            let value = unsafe { core::slice::from_raw_parts(elem_ptr, logical_elem_slots) };
            let elem_hash = unsafe { deep_hash_value_inline_checked(value, elem, module)? };
            h = h.wrapping_add(elem_hash).wrapping_mul(HASH_K);
        }
    }
    Ok(h.rotate_left(5))
}

unsafe fn deep_eq_array_result(a: GcRef, b: GcRef, rttid: u32, module: &Module) -> EqResult {
    if a.is_null() || b.is_null() {
        return if a == b {
            EqResult::Equal
        } else {
            EqResult::NotEqual
        };
    }
    let a_len = array::len(a);
    let b_len = array::len(b);

    if a_len != b_len {
        return EqResult::NotEqual;
    }

    let Some((declared_len, elem)) = array_type_info(rttid, module) else {
        return EqResult::Uncomparable;
    };
    if usize::try_from(declared_len).ok() != Some(a_len) {
        return EqResult::Uncomparable;
    }
    let Some(elem_meta) = module.canonical_value_meta_for_value_rttid(elem) else {
        return EqResult::Uncomparable;
    };
    if array::elem_meta(a) != elem_meta || array::elem_meta(b) != elem_meta {
        return EqResult::NotEqual;
    }
    if array::elem_bytes(a) != array::elem_bytes(b) {
        return EqResult::NotEqual;
    }
    let elem_bytes = array::elem_bytes(a);
    let Ok(elem_layout) = crate::pack::sequence_element_layout(
        elem_meta,
        elem_bytes,
        &module.struct_metas,
        &module.named_type_metas,
        &module.runtime_types,
    ) else {
        return EqResult::Uncomparable;
    };
    let logical_elem_slots = elem_layout.logical_slots;
    if logical_elem_slots == 0 {
        return EqResult::Equal;
    }
    let packed = elem_bytes < logical_elem_slots.saturating_mul(core::mem::size_of::<u64>());
    if packed && (logical_elem_slots != 1 || elem_bytes > core::mem::size_of::<u64>()) {
        return EqResult::Uncomparable;
    }
    let a_data = array::data_ptr_bytes(a);
    let b_data = array::data_ptr_bytes(b);

    for index in 0..a_len {
        let result = if packed {
            let a_value = [unsafe { array::get(a, index, elem_bytes) }];
            let b_value = [unsafe { array::get(b, index, elem_bytes) }];
            unsafe { deep_eq_value_inline_result(&a_value, &b_value, elem, module) }
        } else {
            let Some(byte_offset) = index.checked_mul(elem_bytes) else {
                return EqResult::Uncomparable;
            };
            let a_ptr = unsafe { a_data.add(byte_offset) } as *const u64;
            let b_ptr = unsafe { b_data.add(byte_offset) } as *const u64;
            let a_value = unsafe { core::slice::from_raw_parts(a_ptr, logical_elem_slots) };
            let b_value = unsafe { core::slice::from_raw_parts(b_ptr, logical_elem_slots) };
            unsafe { deep_eq_value_inline_result(a_value, b_value, elem, module) }
        };
        if result != EqResult::Equal {
            return result;
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
    use crate::{gc::Gc, RuntimeType, SlotType, StructMeta, ValueMeta, ValueRttid};
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

    fn module_with_float_aggregates() -> (Module, u32, u32, u32, u32) {
        let mut module = Module::new("float-compare".to_string());
        let f32_rttid = module.runtime_types.len() as u32;
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::Float32));
        let f64_rttid = module.runtime_types.len() as u32;
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::Float64));
        let inner_array_rttid = module.runtime_types.len() as u32;
        module.runtime_types.push(RuntimeType::Array {
            len: 2,
            elem: ValueRttid::new(f32_rttid, ValueKind::Float32),
        });
        let outer_array_rttid = module.runtime_types.len() as u32;
        module.runtime_types.push(RuntimeType::Array {
            len: 2,
            elem: ValueRttid::new(inner_array_rttid, ValueKind::Array),
        });
        let struct_meta_id = module.struct_metas.len() as u32;
        module.struct_metas.push(StructMeta {
            slot_types: vec![
                SlotType::Float,
                SlotType::Float,
                SlotType::Float,
                SlotType::Float,
                SlotType::Float,
                SlotType::Float,
            ],
            fields: vec![
                FieldMeta {
                    name: "f32".to_string(),
                    offset: 0,
                    slot_count: 1,
                    type_info: ValueRttid::new(f32_rttid, ValueKind::Float32),
                    embedded: false,
                    tag: None,
                },
                FieldMeta {
                    name: "f64".to_string(),
                    offset: 1,
                    slot_count: 1,
                    type_info: ValueRttid::new(f64_rttid, ValueKind::Float64),
                    embedded: false,
                    tag: None,
                },
                FieldMeta {
                    name: "nested".to_string(),
                    offset: 2,
                    slot_count: 4,
                    type_info: ValueRttid::new(outer_array_rttid, ValueKind::Array),
                    embedded: false,
                    tag: None,
                },
            ],
            field_index: HashMap::from_iter([
                ("f32".to_string(), 0),
                ("f64".to_string(), 1),
                ("nested".to_string(), 2),
            ]),
        });
        let struct_rttid = module.runtime_types.len() as u32;
        module.runtime_types.push(RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: struct_meta_id,
        });
        (
            module,
            f32_rttid,
            f64_rttid,
            outer_array_rttid,
            struct_rttid,
        )
    }

    #[test]
    fn interface_float_equality_and_hash_follow_numeric_semantics() {
        let (module, f32_rttid, f64_rttid, _, _) = module_with_float_aggregates();
        for (kind, rttid, positive_zero, negative_zero, nan) in [
            (
                ValueKind::Float32,
                f32_rttid,
                u64::from(0.0_f32.to_bits()),
                u64::from((-0.0_f32).to_bits()),
                u64::from(f32::NAN.to_bits()),
            ),
            (
                ValueKind::Float64,
                f64_rttid,
                0.0_f64.to_bits(),
                (-0.0_f64).to_bits(),
                f64::NAN.to_bits(),
            ),
        ] {
            let slot0 = interface::pack_slot0(0, rttid, kind);
            assert_eq!(
                unsafe { iface_eq(slot0, positive_zero, slot0, negative_zero, &module) },
                1,
                "{kind:?}"
            );
            assert_eq!(
                unsafe { iface_hash(slot0, positive_zero, &module) },
                unsafe { iface_hash(slot0, negative_zero, &module) },
                "{kind:?}"
            );
            assert_eq!(
                unsafe { iface_eq(slot0, nan, slot0, nan, &module) },
                0,
                "{kind:?}"
            );
        }

        let f32_slot0 = interface::pack_slot0(0, f32_rttid, ValueKind::Float32);
        let one = u64::from(1.0_f32.to_bits());
        let polluted = one | 0xfeed_beef_0000_0000;
        assert_eq!(
            unsafe { iface_eq(f32_slot0, one, f32_slot0, polluted, &module) },
            1
        );
        assert_eq!(unsafe { iface_hash(f32_slot0, one, &module) }, unsafe {
            iface_hash(f32_slot0, polluted, &module)
        });
    }

    #[test]
    fn repeated_zero_width_array_hash_fold_matches_element_order_semantics() {
        for count in [0, 1, 2, 3, 31, 256, 4_096] {
            let mut expected = HASH_SEED;
            for _ in 0..count {
                expected = expected.wrapping_add(HASH_SEED).wrapping_mul(HASH_K);
            }
            assert_eq!(repeat_hash_fold(HASH_SEED, HASH_SEED, count), expected);
        }
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn huge_zero_width_heap_array_hash_is_logarithmic() {
        let len = 1_usize << 40;
        let mut module = Module::new("zero-width-array-hash".to_string());
        module.struct_metas.push(StructMeta {
            slot_types: Vec::new(),
            fields: Vec::new(),
            field_index: HashMap::new(),
        });
        module.runtime_types.push(RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        });
        module.runtime_types.push(RuntimeType::Array {
            len: len as u64,
            elem: ValueRttid::new(0, ValueKind::Struct),
        });

        let mut gc = Gc::new();
        let arr = array::create(&mut gc, ValueMeta::new(0, ValueKind::Struct), 0, len);
        assert!(!arr.is_null());
        assert_eq!(
            unsafe { deep_hash_array_checked(arr, 1, &module) },
            Ok(repeat_hash_fold(HASH_SEED, HASH_SEED, len).rotate_left(5))
        );
    }

    #[test]
    fn recursive_struct_and_inline_array_float_comparison_is_numeric() {
        let (module, _, _, outer_array_rttid, struct_rttid) = module_with_float_aggregates();
        let positive = [0_u64; 6];
        let negative = [
            u64::from((-0.0_f32).to_bits()),
            (-0.0_f64).to_bits(),
            u64::from((-0.0_f32).to_bits()),
            0,
            u64::from((-0.0_f32).to_bits()),
            0,
        ];

        assert!(unsafe { deep_eq_struct_inline(&positive, &negative, struct_rttid, &module) });
        assert_eq!(
            unsafe { deep_hash_struct_inline(&positive, struct_rttid, &module) },
            unsafe { deep_hash_struct_inline(&negative, struct_rttid, &module) }
        );
        assert!(unsafe {
            deep_eq_value_inline(
                &positive[2..],
                &negative[2..],
                ValueRttid::new(outer_array_rttid, ValueKind::Array),
                &module,
            )
        });

        let mut nan = positive;
        nan[1] = f64::NAN.to_bits();
        assert!(!unsafe { deep_eq_struct_inline(&nan, &nan, struct_rttid, &module) });
        let slot0 = interface::pack_slot0(0, struct_rttid, ValueKind::Struct);
        let ptr = nan.as_ptr() as u64;
        assert_eq!(unsafe { iface_eq(slot0, ptr, slot0, ptr, &module) }, 0);
    }

    #[test]
    fn canonical_packed_float_array_comparison_and_hash_are_numeric() {
        let (module, _, _, _, _) = module_with_float_aggregates();
        let array_rttid = 2;
        let mut gc = Gc::new();
        let elem_meta = ValueMeta::new(0, ValueKind::Float32);
        let positive = array::create(&mut gc, elem_meta, 4, 2);
        let negative = array::create(&mut gc, elem_meta, 4, 2);
        unsafe {
            array::set(positive, 0, u64::from(0.0_f32.to_bits()), 4);
            array::set(positive, 1, u64::from(1.0_f32.to_bits()), 4);
            array::set(negative, 0, u64::from((-0.0_f32).to_bits()), 4);
            array::set(negative, 1, u64::from(1.0_f32.to_bits()), 4);
        }
        let slot0 = interface::pack_slot0(0, array_rttid, ValueKind::Array);
        assert_eq!(
            unsafe { iface_eq(slot0, positive as u64, slot0, negative as u64, &module) },
            1
        );
        assert_eq!(
            unsafe { iface_hash(slot0, positive as u64, &module) },
            unsafe { iface_hash(slot0, negative as u64, &module) }
        );

        unsafe { array::set(positive, 0, u64::from(f32::NAN.to_bits()), 4) };
        assert_eq!(
            unsafe { iface_eq(slot0, positive as u64, slot0, positive as u64, &module) },
            0
        );
    }

    #[test]
    fn heap_array_comparison_rejects_runtime_header_layout_drift() {
        let (module, _, _, _, _) = module_with_float_aggregates();
        let array_rttid = 2;
        let slot0 = interface::pack_slot0(0, array_rttid, ValueKind::Array);
        let mut gc = Gc::new();
        let elem_meta = ValueMeta::new(0, ValueKind::Float32);

        let wrong_len = array::create(&mut gc, elem_meta, 4, 3);
        assert!(unsafe { iface_hash_checked(slot0, wrong_len as u64, &module) }.is_err());
        assert_eq!(
            unsafe { iface_eq(slot0, wrong_len as u64, slot0, wrong_len as u64, &module) },
            2
        );

        let wrong_width = array::create(&mut gc, elem_meta, 8, 2);
        assert!(unsafe { iface_hash_checked(slot0, wrong_width as u64, &module) }.is_err());
        assert_eq!(
            unsafe {
                iface_eq(
                    slot0,
                    wrong_width as u64,
                    slot0,
                    wrong_width as u64,
                    &module,
                )
            },
            2
        );
    }

    fn module_with_deep_single_slot_arrays(depth: usize) -> Module {
        let mut module = Module::new("deep-array-compare".to_string());
        for index in 0..depth {
            let elem = if index + 1 == depth {
                ValueRttid::new(depth as u32, ValueKind::Int64)
            } else {
                ValueRttid::new((index + 1) as u32, ValueKind::Array)
            };
            module
                .runtime_types
                .push(RuntimeType::Array { len: 1, elem });
        }
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::Int64));
        module
    }

    fn module_with_deep_single_slot_structs(depth: usize) -> Module {
        let mut module = Module::new("deep-struct-compare".to_string());
        for index in 0..depth {
            let field_type = if index + 1 == depth {
                ValueRttid::new(depth as u32, ValueKind::Int64)
            } else {
                ValueRttid::new((index + 1) as u32, ValueKind::Struct)
            };
            module.struct_metas.push(StructMeta {
                slot_types: vec![SlotType::Value],
                fields: vec![FieldMeta {
                    name: "value".to_string(),
                    offset: 0,
                    slot_count: 1,
                    type_info: field_type,
                    embedded: false,
                    tag: None,
                }],
                field_index: HashMap::from_iter([("value".to_string(), 0)]),
            });
            module.runtime_types.push(RuntimeType::Struct {
                fields: Vec::new(),
                meta_id: index as u32,
            });
        }
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::Int64));
        module
    }

    #[test]
    fn deep_array_comparability_hash_and_equality_use_explicit_work_stacks() {
        const DEPTH: usize = 2_048;
        let module = module_with_deep_single_slot_arrays(DEPTH);
        let root = ValueRttid::new(0, ValueKind::Array);
        let equal = [17_u64];
        let different = [18_u64];

        assert!(value_is_comparable(0, ValueKind::Array, &module));
        assert!(unsafe { deep_eq_value_inline(&equal, &equal, root, &module) });
        assert!(!unsafe { deep_eq_value_inline(&equal, &different, root, &module) });
        assert_eq!(
            unsafe { deep_hash_value_inline_checked(&equal, root, &module) }.unwrap(),
            unsafe { deep_hash_value_inline_checked(&equal, root, &module) }.unwrap()
        );
        assert_ne!(
            unsafe { deep_hash_value_inline_checked(&equal, root, &module) }.unwrap(),
            unsafe { deep_hash_value_inline_checked(&different, root, &module) }.unwrap()
        );
    }

    #[test]
    fn deep_struct_comparability_hash_and_equality_use_explicit_work_stacks() {
        const DEPTH: usize = 2_048;
        let module = module_with_deep_single_slot_structs(DEPTH);
        let root = ValueRttid::new(0, ValueKind::Struct);
        let equal = [41_u64];
        let different = [42_u64];

        assert!(value_is_comparable(0, ValueKind::Struct, &module));
        assert!(unsafe { deep_eq_value_inline(&equal, &equal, root, &module) });
        assert!(!unsafe { deep_eq_value_inline(&equal, &different, root, &module) });
        assert_eq!(
            unsafe { deep_hash_value_inline_checked(&equal, root, &module) }.unwrap(),
            unsafe { deep_hash_value_inline_checked(&equal, root, &module) }.unwrap()
        );
        assert_ne!(
            unsafe { deep_hash_value_inline_checked(&equal, root, &module) }.unwrap(),
            unsafe { deep_hash_value_inline_checked(&different, root, &module) }.unwrap()
        );
    }

    #[test]
    fn deep_named_array_chain_resolves_without_metadata_recursion() {
        const DEPTH: usize = 4_096;
        let mut module = Module::new("deep-named-array-compare".to_string());
        for index in 0..DEPTH {
            module.named_type_metas.push(NamedTypeMeta {
                name: format!("N{index}"),
                underlying_meta: ValueMeta::new((index + 1) as u32, ValueKind::Array),
                underlying_rttid: ValueRttid::new((index + 1) as u32, ValueKind::Array),
                methods: BTreeMap::new(),
            });
            module.runtime_types.push(RuntimeType::Named {
                id: index as u32,
                struct_meta_id: None,
            });
        }
        module.runtime_types.push(RuntimeType::Array {
            len: 1,
            elem: ValueRttid::new((DEPTH + 1) as u32, ValueKind::Int64),
        });
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::Int64));

        let root = ValueRttid::new(0, ValueKind::Array);
        assert!(value_is_comparable(0, ValueKind::Array, &module));
        assert!(unsafe { deep_eq_value_inline(&[7], &[7], root, &module) });
        assert!(unsafe { deep_hash_value_inline_checked(&[7], root, &module) }.is_ok());
    }

    #[test]
    fn cyclic_aggregate_metadata_is_uncomparable_without_looping() {
        let mut module = Module::new("cyclic-aggregate-compare".to_string());
        module.struct_metas.push(StructMeta {
            slot_types: vec![SlotType::Value],
            fields: vec![FieldMeta {
                name: "self".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(0, ValueKind::Struct),
                embedded: false,
                tag: None,
            }],
            field_index: HashMap::from_iter([("self".to_string(), 0)]),
        });
        module.runtime_types.push(RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        });

        assert!(!value_is_comparable(0, ValueKind::Struct, &module));
        assert!(unsafe {
            deep_hash_value_inline_checked(&[0], ValueRttid::new(0, ValueKind::Struct), &module)
        }
        .is_err());
        assert!(!unsafe {
            deep_eq_value_inline(&[0], &[0], ValueRttid::new(0, ValueKind::Struct), &module)
        });
    }
}
