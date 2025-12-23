//! GC type information and unified object scanning.
//!
//! This module provides:
//! - Static struct slot_types table (initialized once at module load)
//! - Unified scan_object function shared by VM and JIT

use alloc::boxed::Box;
use alloc::vec::Vec;
use once_cell::sync::OnceCell;
use vo_common_core::{ValueKind, SlotType};

use crate::gc::{Gc, GcRef};

#[cfg(feature = "std")]
use crate::objects::map;

// =============================================================================
// Static Struct SlotType Table
// =============================================================================

/// Struct slot_types, indexed by type_id (starting from 0).
/// Each entry describes how GC should scan each slot.
static STRUCT_SLOT_TYPES: OnceCell<Box<[Box<[SlotType]>]>> = OnceCell::new();

/// Initialize the struct slot_types table.
/// Called once at module load time by VM or JIT.
pub fn init_struct_slot_types(slot_types: Vec<Vec<SlotType>>) {
    let boxed: Box<[Box<[SlotType]>]> = slot_types
        .into_iter()
        .map(|v| v.into_boxed_slice())
        .collect();
    let _ = STRUCT_SLOT_TYPES.set(boxed);
}

/// Get slot_types for a struct type by its type_id.
/// Returns None if type_id is out of range.
#[inline]
pub fn get_struct_slot_types(type_id: u16) -> Option<&'static [SlotType]> {
    STRUCT_SLOT_TYPES.get()?.get(type_id as usize).map(|b| b.as_ref())
}

// =============================================================================
// Unified Object Scanning
// =============================================================================

/// Scan a GC object for internal references.
/// This is the unified scan function used by both VM and JIT.
pub fn scan_object(gc: &mut Gc, obj: GcRef) {
    if obj.is_null() {
        return;
    }
    
    let header = unsafe { (*obj).header };
    let kind = ValueKind::from_u8(header.value_kind);
    
    // Struct: use slot_types for dynamic scanning
    if kind == ValueKind::Struct {
        if let Some(slot_types) = get_struct_slot_types(header.type_id) {
            scan_with_slot_types(gc, obj, slot_types, 0);
        }
        return;
    }
    
    // Built-in types: fixed layouts
    match kind {
        ValueKind::String | ValueKind::Slice => {
            // String: [array_ref, start, len]
            // Slice: [array_ref, start, len, cap]
            let val = Gc::read_slot(obj, 0);
            if val != 0 {
                gc.mark_gray(val as GcRef);
            }
        }
        ValueKind::Array => scan_array(gc, obj),
        #[cfg(feature = "std")]
        ValueKind::Map => scan_map(gc, obj),
        ValueKind::Channel => scan_channel(gc, obj),
        ValueKind::Closure => scan_closure(gc, obj),
        _ => {}
    }
}

/// Scan array elements.
fn scan_array(gc: &mut Gc, obj: GcRef) {
    // Array: [elem_kind:u8, elem_type_id:u16, elem_bytes, len, data...]
    let elem_kind = ValueKind::from_u8(Gc::read_slot(obj, 0) as u8);
    let elem_type_id = Gc::read_slot(obj, 1) as u16;
    let len = Gc::read_slot(obj, 3) as usize;
    
    if !elem_kind.needs_gc() {
        return;
    }
    
    if elem_kind == ValueKind::Struct {
        // Struct elements: use slot_types for each element
        if let Some(slot_types) = get_struct_slot_types(elem_type_id) {
            let slots_per_elem = slot_types.len().max(1);
            for i in 0..len {
                let base = 4 + i * slots_per_elem;
                scan_with_slot_types(gc, obj, slot_types, base);
            }
        }
    } else {
        // Built-in reference elements: each element is a GcRef
        for i in 0..len {
            let val = Gc::read_slot(obj, 4 + i);
            if val != 0 {
                gc.mark_gray(val as GcRef);
            }
        }
    }
}

/// Scan map entries.
/// Note: Map keys must be comparable types (no slices, maps, funcs), so no GC refs.
#[cfg(feature = "std")]
fn scan_map(gc: &mut Gc, obj: GcRef) {
    // Map: [map_ptr, key_kind, val_kind]
    let val_kind = ValueKind::from_u8(Gc::read_slot(obj, 2) as u8);
    
    if !val_kind.needs_gc() {
        return;
    }
    
    // Only scan values (keys are comparable types, no GC refs)
    let len = map::len(obj);
    for idx in 0..len {
        if let Some((_, val)) = map::iter_at(obj, idx) {
            if val != 0 {
                gc.mark_gray(val as GcRef);
            }
        }
    }
}

/// Scan channel buffer and waiting senders.
#[cfg(feature = "std")]
fn scan_channel(gc: &mut Gc, obj: GcRef) {
    use crate::objects::channel;
    
    // Channel: [chan_ptr, elem_kind, cap]
    let elem_kind = ValueKind::from_u8(Gc::read_slot(obj, 1) as u8);
    
    if !elem_kind.needs_gc() {
        return;
    }
    
    let state = channel::get_state(obj);
    
    // Scan buffer
    for &val in &state.buffer {
        if val != 0 {
            gc.mark_gray(val as GcRef);
        }
    }
    
    // Scan waiting_senders values
    for &(_, val) in &state.waiting_senders {
        if val != 0 {
            gc.mark_gray(val as GcRef);
        }
    }
}

#[cfg(not(feature = "std"))]
fn scan_channel(_gc: &mut Gc, _obj: GcRef) {
    // Channel not supported in no_std
}

/// Scan closure upvalues.
fn scan_closure(gc: &mut Gc, obj: GcRef) {
    // Closure: [func_id, count, upval0, upval1, ...]
    let count = Gc::read_slot(obj, 1) as usize;
    for i in 0..count.min(256) {
        let val = Gc::read_slot(obj, 2 + i);
        if val != 0 {
            gc.mark_gray(val as GcRef);
        }
    }
}

/// Scan slots using slot_types for dynamic interface handling.
/// This is the core function that handles Interface1 slots dynamically.
fn scan_with_slot_types(gc: &mut Gc, obj: GcRef, slot_types: &[SlotType], base_offset: usize) {
    let mut i = 0;
    while i < slot_types.len() {
        match slot_types[i] {
            SlotType::Value => {
                // Non-pointer, skip
            }
            SlotType::GcRef => {
                let val = Gc::read_slot(obj, base_offset + i);
                if val != 0 {
                    gc.mark_gray(val as GcRef);
                }
            }
            SlotType::Interface0 => {
                // Interface first slot is packed type info, not a pointer
                // Next slot (Interface1) needs dynamic check
            }
            SlotType::Interface1 => {
                // Dynamic check: use unpack_value_kind from interface module
                if i > 0 {
                    let packed = Gc::read_slot(obj, base_offset + i - 1);
                    let value_kind = crate::objects::interface::unpack_value_kind(packed);
                    if value_kind.needs_gc() {
                        let val = Gc::read_slot(obj, base_offset + i);
                        if val != 0 {
                            gc.mark_gray(val as GcRef);
                        }
                    }
                }
            }
        }
        i += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_init_and_get_slot_types() {
        // Note: OnceCell can only be set once, so this test may conflict with others
        let slot_types = vec![
            vec![SlotType::GcRef, SlotType::Value, SlotType::GcRef],  // type_id 0
            vec![SlotType::Value, SlotType::GcRef],                   // type_id 1
        ];
        init_struct_slot_types(slot_types);
        
        // Should return slot_types for struct types (starting from 0)
        assert_eq!(get_struct_slot_types(0), Some(&[SlotType::GcRef, SlotType::Value, SlotType::GcRef][..]));
        assert_eq!(get_struct_slot_types(1), Some(&[SlotType::Value, SlotType::GcRef][..]));
        assert!(get_struct_slot_types(2).is_none());
    }
}
