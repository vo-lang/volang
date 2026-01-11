//! Deep comparison for interface values.
//!
//! Shared logic used by both VM (exec_iface_eq) and JIT (vo_iface_eq).

use crate::gc::GcRef;
use crate::objects::{array, string};
use crate::{RuntimeType, SlotType, ValueKind};
use vo_common_core::bytecode::Module;

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
    match vk {
        ValueKind::Slice | ValueKind::Map | ValueKind::Closure | ValueKind::Channel => {
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

/// Find field by slot offset.
fn find_field_at_offset(fields: &[vo_common_core::bytecode::FieldMeta], offset: u16) -> Option<&vo_common_core::bytecode::FieldMeta> {
    fields.iter().find(|f| f.offset == offset)
}

/// Deep comparison of two struct values.
fn deep_eq_struct(a: GcRef, b: GcRef, rttid: u32, module: &Module) -> bool {
    let struct_meta_id = match module.runtime_types.get(rttid as usize) {
        Some(RuntimeType::Named { struct_meta_id: Some(id), .. }) => *id,
        Some(RuntimeType::Struct { meta_id, .. }) => *meta_id,
        _ => return a == b,
    };
    
    let struct_meta = match module.struct_metas.get(struct_meta_id as usize) {
        Some(m) => m,
        None => return a == b,
    };
    
    let slot_types = &struct_meta.slot_types;
    let mut i = 0;
    while i < slot_types.len() {
        let a_val = unsafe { *a.add(i) };
        let b_val = unsafe { *b.add(i) };
        
        match slot_types[i] {
            SlotType::Value => {
                if a_val != b_val {
                    return false;
                }
            }
            SlotType::GcRef => {
                if a_val != b_val {
                    // Find field by offset to get correct type info
                    let field_vk = find_field_at_offset(&struct_meta.fields, i as u16)
                        .map(|f| f.type_info.value_kind())
                        .unwrap_or(ValueKind::Void);
                    
                    if field_vk == ValueKind::String {
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
                let a_slot1 = unsafe { *a.add(i + 1) };
                let b_slot1 = unsafe { *b.add(i + 1) };
                
                // Recursive interface comparison
                let result = iface_eq(a_val, a_slot1, b_val, b_slot1, module);
                if result != 1 {
                    return false;
                }
                i += 1; // Skip Interface1 slot
            }
            SlotType::Interface1 => {}
        }
        i += 1;
    }
    true
}

/// Deep comparison of two array values.
fn deep_eq_array(a: GcRef, b: GcRef, rttid: u32, module: &Module) -> bool {
    let a_len = array::len(a);
    let b_len = array::len(b);
    
    if a_len != b_len {
        return false;
    }
    
    let len = a_len;
    let elem_vk = array::elem_kind(a);
    let elem_bytes = array::elem_bytes(a);
    let elem_slots = (elem_bytes / 8).max(1);
    
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
    
    let a_data = unsafe { a.add(array::HEADER_SLOTS) };
    let b_data = unsafe { b.add(array::HEADER_SLOTS) };
    
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
            _ => {
                // Multi-slot primitive comparison
                (0..elem_slots).all(|j| unsafe {
                    *a_data.add(offset + j) == *b_data.add(offset + j)
                })
            }
        };
        if !eq {
            return false;
        }
    }
    true
}
