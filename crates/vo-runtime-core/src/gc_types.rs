//! GC object scanning by type.

use crate::gc::{Gc, GcRef};
use crate::objects::{array, channel, closure, interface, map, slice, string};
use crate::types::StructMeta;
use vo_common_core::types::{SlotType, ValueKind};

/// Scan a GC object and mark its children.
pub fn scan_object(gc: &mut Gc, obj: GcRef, struct_metas: &[StructMeta]) {
    let gc_header = Gc::header(obj);
    
    if gc_header.is_array() {
        scan_array(gc, obj);
        return;
    }
    
    match gc_header.kind() {
        ValueKind::String => {
            let arr = string::array_ref(obj);
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
            for i in 0..closure::upval_count(obj) {
                let uv = closure::get_upvalue(obj, i);
                if uv != 0 { gc.mark_gray(uv as GcRef); }
            }
        }

        ValueKind::Map => {
            if map::val_kind(obj).is_ref_type() {
                for i in 0..map::len(obj) {
                    if let Some((_, v)) = map::iter_at(obj, i) {
                        if v != 0 { gc.mark_gray(v as GcRef); }
                    }
                }
            }
        }

        ValueKind::Channel => {
            if channel::elem_kind(obj).is_ref_type() {
                let state = channel::get_state(obj);
                for &val in &state.buffer {
                    if val != 0 { gc.mark_gray(val as GcRef); }
                }
                for &(_, val) in &state.waiting_senders {
                    if val != 0 { gc.mark_gray(val as GcRef); }
                }
            }
        }

        _ => {}
    }
}

fn scan_array(gc: &mut Gc, obj: GcRef) {
    let gc_header = Gc::header(obj);
    if !gc_header.kind().may_contain_gc_refs() { return; }
    
    let length = array::len(obj);
    let es = array::elem_slots(obj) as usize;
    for i in 0..length {
        if es == 1 {
            let child = array::get(obj, i);
            if child != 0 { gc.mark_gray(child as GcRef); }
        } else {
            for j in 0..es {
                let child = array::get(obj, i * es + j);
                if child != 0 { gc.mark_gray(child as GcRef); }
            }
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
            let child = Gc::read_slot(obj, i);
            if child != 0 { gc.mark_gray(child as GcRef); }
        } else if st == SlotType::Interface0 {
            let header_slot = Gc::read_slot(obj, i);
            if interface::unpack_value_kind(header_slot).is_ref_type() {
                let child = Gc::read_slot(obj, i + 1);
                if child != 0 { gc.mark_gray(child as GcRef); }
            }
            i += 1;
        }
        i += 1;
    }
}
