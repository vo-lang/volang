//! GC object scanning by type.

use crate::gc::{Gc, GcRef};
use crate::objects::{array, closure, interface, slice, string};
use crate::types::StructMeta;
use vo_common_core::types::{SlotType, ValueKind};

/// Scan a GC object and mark its children.
pub fn scan_object(gc: &mut Gc, obj: GcRef, struct_metas: &[StructMeta]) {
    let gc_header = Gc::header(obj);
    
    // Check if this is an Array (is_array flag set)
    if gc_header.is_array() {
        let elem_kind = gc_header.kind();
        if elem_kind.may_contain_gc_refs() {
            let arr_header = Gc::read_slot(obj, 0);
            let length = array::unpack_len(arr_header);
            let elem_slots = array::unpack_elem_slots(arr_header) as usize;
            for i in 0..length {
                if elem_slots == 1 {
                    let child = Gc::read_slot(obj, array::DATA_START + i);
                    if child != 0 {
                        gc.mark_gray(child as GcRef);
                    }
                } else {
                    for j in 0..elem_slots {
                        let child = Gc::read_slot(obj, array::DATA_START + i * elem_slots + j);
                        if child != 0 {
                            gc.mark_gray(child as GcRef);
                        }
                    }
                }
            }
        }
        return;
    }
    
    let vk = gc_header.kind();

    match vk {
        ValueKind::String => {
            let array_ref = Gc::read_slot(obj, string::SLOT_ARRAY);
            if array_ref != 0 {
                gc.mark_gray(array_ref as GcRef);
            }
        }

        ValueKind::Slice => {
            let array_ref = Gc::read_slot(obj, slice::SLOT_ARRAY);
            if array_ref != 0 {
                gc.mark_gray(array_ref as GcRef);
            }
        }

        ValueKind::Struct => {
            let meta_id = gc_header.meta_id() as usize;
            if meta_id < struct_metas.len() {
                let meta = &struct_metas[meta_id];
                let mut i = 0;
                while i < meta.slot_types.len() {
                    let st = meta.slot_types[i];
                    if st == SlotType::GcRef {
                        let child = Gc::read_slot(obj, i);
                        if child != 0 {
                            gc.mark_gray(child as GcRef);
                        }
                    } else if st == SlotType::Interface0 {
                        let header_slot = Gc::read_slot(obj, i);
                        let value_kind = interface::unpack_value_kind(header_slot);
                        if value_kind.is_ref_type() {
                            let child = Gc::read_slot(obj, i + 1);
                            if child != 0 {
                                gc.mark_gray(child as GcRef);
                            }
                        }
                        i += 1;
                    }
                    i += 1;
                }
            }
        }

        ValueKind::Closure => {
            let cap_count = closure::upval_count(obj);
            for i in 0..cap_count {
                let uv = closure::get_upvalue(obj, i);
                if uv != 0 {
                    gc.mark_gray(uv as GcRef);
                }
            }
        }

        ValueKind::Map => {
            // Map inner HashMap scanning is handled separately
            // Key/value GC refs need to be scanned via iterator
        }

        ValueKind::Channel => {
            // Channel state managed externally
        }

        ValueKind::Pointer => {
            // Pointer points to escaped value, but the escaped value itself
            // has its own GcHeader with the original type - it will be scanned separately
        }

        _ => {}
    }
}
