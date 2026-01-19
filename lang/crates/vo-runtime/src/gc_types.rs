//! GC object scanning by type.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::{scan_slots_by_types, Gc, GcRef};
use crate::objects::{array, channel, closure, interface, map, slice};
use crate::slot::{byte_offset_for_slots, slot_to_ptr, Slot, SLOT_BYTES};
use vo_common_core::bytecode::StructMeta;
use vo_common_core::types::{SlotType, ValueKind};


/// Scan a GC object and mark its children.
pub fn scan_object(gc: &mut Gc, obj: GcRef, struct_metas: &[StructMeta]) {
    let gc_header = Gc::header(obj);
    
    match gc_header.kind() {
        ValueKind::Array => scan_array(gc, obj, struct_metas),
        ValueKind::String => {
            let arr = slice::array_ref(obj);
            if !arr.is_null() { gc.mark_gray(arr); }
        }

        ValueKind::Slice => {
            let arr = slice::array_ref(obj);
            if !arr.is_null() { gc.mark_gray(arr); }
        }

        ValueKind::Struct | ValueKind::Pointer => {
            scan_struct(gc, obj, gc_header.meta_id() as usize, struct_metas);
        }

        ValueKind::Closure => {
            for i in 0..closure::capture_count(obj) {
                let cap = closure::get_capture(obj, i);
                if cap != 0 { gc.mark_gray(cap as GcRef); }
            }
        }

        ValueKind::Map => {
            scan_map(gc, obj, struct_metas);
        }

        ValueKind::Channel => {
            scan_channel(gc, obj, struct_metas);
        }

        _ => {}
    }
}

fn scan_array(gc: &mut Gc, obj: GcRef, struct_metas: &[StructMeta]) {
    let elem_meta = array::elem_meta(obj);
    let elem_kind = elem_meta.value_kind();
    
    // Packed types (bool, int8-32, float32) don't contain GcRefs
    if !elem_kind.may_contain_gc_refs() { return; }
    
    let len = array::len(obj);
    let elem_bytes = array::elem_bytes(obj);
    let elem_slots = elem_bytes / SLOT_BYTES;
    
    // For struct/pointer elements, use slot_types from struct_metas
    if matches!(elem_kind, ValueKind::Struct | ValueKind::Pointer) {
        let meta_id = elem_meta.meta_id() as usize;
        if meta_id < struct_metas.len() {
            let slot_types = &struct_metas[meta_id].slot_types;
            for idx in 0..len {
                scan_array_struct_elem(gc, obj, idx, elem_bytes, slot_types);
            }
            return;
        }
    }
    
    // For reference types (slice, map, string, etc.), each element is a single GcRef
    for idx in 0..len {
        for slot in 0..elem_slots {
            let byte_off = idx * elem_bytes + slot * SLOT_BYTES;
            let base_off = byte_offset_for_slots(array::HEADER_SLOTS);
            let ptr = unsafe { (obj as *const u8).add(base_off + byte_off) as *const Slot };
            let child = unsafe { *ptr };
            if child != 0 { gc.mark_gray(slot_to_ptr(child)); }
        }
    }
}

fn scan_array_struct_elem(gc: &mut Gc, obj: GcRef, idx: usize, elem_bytes: usize, slot_types: &[SlotType]) {
    let base_off = byte_offset_for_slots(array::HEADER_SLOTS) + idx * elem_bytes;
    let mut i = 0;
    while i < slot_types.len() {
        let st = slot_types[i];
        if st == SlotType::GcRef {
            let ptr = unsafe { (obj as *const u8).add(base_off + i * SLOT_BYTES) as *const Slot };
            let child = unsafe { *ptr };
            if child != 0 { gc.mark_gray(slot_to_ptr(child)); }
        } else if st == SlotType::Interface0 {
            let header_ptr = unsafe { (obj as *const u8).add(base_off + i * SLOT_BYTES) as *const Slot };
            let header_slot = unsafe { *header_ptr };
            if interface::data_is_gc_ref(header_slot) {
                let data_ptr = unsafe { (obj as *const u8).add(base_off + (i + 1) * SLOT_BYTES) as *const Slot };
                let child = unsafe { *data_ptr };
                if child != 0 { gc.mark_gray(slot_to_ptr(child)); }
            }
            i += 1;
        }
        i += 1;
    }
}

fn scan_channel(gc: &mut Gc, obj: GcRef, struct_metas: &[StructMeta]) {
    let elem_meta = channel::elem_meta(obj);
    let elem_kind = elem_meta.value_kind();
    if !elem_kind.may_contain_gc_refs() { return; }
    
    let elem_slot_types = if matches!(elem_kind, ValueKind::Struct | ValueKind::Pointer) {
        let meta_id = elem_meta.meta_id() as usize;
        if meta_id < struct_metas.len() { Some(&struct_metas[meta_id].slot_types) } else { None }
    } else { None };
    
    let state = channel::get_state(obj);
    for elem in state.iter_buffer() {
        scan_slots_with_types(gc, elem, elem_slot_types);
    }
    for elem in state.iter_waiting_values() {
        scan_slots_with_types(gc, elem, elem_slot_types);
    }
}

fn scan_map(gc: &mut Gc, obj: GcRef, struct_metas: &[StructMeta]) {
    let key_meta = map::key_meta(obj);
    let val_meta = map::val_meta(obj);
    let key_kind = key_meta.value_kind();
    let val_kind = val_meta.value_kind();
    
    let scan_key = key_kind.may_contain_gc_refs();
    let scan_val = val_kind.may_contain_gc_refs();
    if !scan_key && !scan_val { return; }
    
    // Get slot_types for struct keys/values
    let key_slot_types = if matches!(key_kind, ValueKind::Struct | ValueKind::Pointer) {
        let meta_id = key_meta.meta_id() as usize;
        if meta_id < struct_metas.len() { Some(&struct_metas[meta_id].slot_types) } else { None }
    } else { None };
    
    let val_slot_types = if matches!(val_kind, ValueKind::Struct | ValueKind::Pointer) {
        let meta_id = val_meta.meta_id() as usize;
        if meta_id < struct_metas.len() { Some(&struct_metas[meta_id].slot_types) } else { None }
    } else { None };
    
    let mut iter = map::iter_init(obj);
    while let Some((k, v)) = map::iter_next(&mut iter) {
        if scan_key {
            scan_slots_with_types(gc, k, key_slot_types);
        }
        if scan_val {
            scan_slots_with_types(gc, v, val_slot_types);
        }
    }
}

/// Scan slots, using slot_types if available (for structs), otherwise treat all as GcRefs.
fn scan_slots_with_types(gc: &mut Gc, slots: &[u64], slot_types: Option<&Vec<SlotType>>) {
    if let Some(types) = slot_types {
        scan_slots_by_types(gc, slots, types);
    } else {
        // Reference types: all slots are GcRefs
        for &slot in slots {
            if slot != 0 { gc.mark_gray(slot as GcRef); }
        }
    }
}

fn scan_struct(gc: &mut Gc, obj: GcRef, meta_id: usize, struct_metas: &[StructMeta]) {
    if meta_id >= struct_metas.len() { return; }
    
    let meta = &struct_metas[meta_id];
    let mut i = 0;
    while i < meta.slot_types.len() {
        let st = meta.slot_types[i];
        if st == SlotType::GcRef {
            let child = unsafe { Gc::read_slot(obj, i) };
            if child != 0 { gc.mark_gray(child as GcRef); }
        } else if st == SlotType::Interface0 {
            let header_slot = unsafe { Gc::read_slot(obj, i) };
            if interface::data_is_gc_ref(header_slot) {
                let child = unsafe { Gc::read_slot(obj, i + 1) };
                if child != 0 { gc.mark_gray(child as GcRef); }
            }
            i += 1;
        }
        i += 1;
    }
}

/// Finalize a GC object before deallocation.
/// Releases native resources (Box, etc.) not managed by GC.
pub fn finalize_object(obj: GcRef) {
    let header = Gc::header(obj);
    match header.kind() {
        ValueKind::Channel => unsafe { channel::drop_inner(obj); }
        ValueKind::Map => unsafe { map::drop_inner(obj); }
        _ => {}
    }
}