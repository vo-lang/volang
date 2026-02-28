//! GC object scanning by type.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::{scan_slots_by_types, Gc, GcRef};
use crate::objects::{array, channel, closure, interface, map, port, queue_state, slice};
use crate::slot::{byte_offset_for_slots, slot_to_ptr, Slot, SLOT_BYTES};
use vo_common_core::bytecode::StructMeta;
use vo_common_core::types::{SlotType, ValueKind, ValueMeta};


/// Type-safe write barrier for mixed-slot values.
///
/// Only barriers slots that are actually GcRefs (SlotType::GcRef) or
/// interface data slots (SlotType::Interface0 + data_is_gc_ref check).
/// Avoids UB from passing non-pointer values (int, float, slot0 metadata)
/// to write_barrier, which would dereference them as GcHeader pointers.
///
/// Used by MapSet, ChanSend, and any operation writing mixed-type values into heap objects.
pub fn typed_write_barrier(gc: &mut Gc, parent: GcRef, vals: &[u64], slot_types: &[SlotType]) {
    let mut i = 0;
    while i < slot_types.len() && i < vals.len() {
        match slot_types[i] {
            SlotType::GcRef => {
                if vals[i] != 0 {
                    gc.write_barrier(parent, vals[i] as GcRef);
                }
            }
            SlotType::Interface0 => {
                if i + 1 < vals.len()
                    && interface::data_is_gc_ref(vals[i])
                    && vals[i + 1] != 0
                {
                    gc.write_barrier(parent, vals[i + 1] as GcRef);
                }
                i += 1; // skip data slot (Interface1)
            }
            _ => {}
        }
        i += 1;
    }
}

/// Type-safe write barrier driven by ValueMeta (for JIT paths that don't have slot_types directly).
/// Resolves struct slot_types from Module when needed. For simple reference types, barriers directly.
pub fn typed_write_barrier_by_meta(
    gc: &mut Gc,
    parent: GcRef,
    vals: &[u64],
    meta: vo_common_core::types::ValueMeta,
    module: Option<&vo_common_core::bytecode::Module>,
) {
    use vo_common_core::types::ValueKind;
    let vk = meta.value_kind();
    match vk {
        // Single-slot reference types: the entire value is a GcRef
        ValueKind::String | ValueKind::Slice | ValueKind::Map | ValueKind::Closure |
        ValueKind::Channel | ValueKind::Pointer | ValueKind::Port | ValueKind::Island => {
            if !vals.is_empty() && vals[0] != 0 {
                gc.write_barrier(parent, vals[0] as GcRef);
            }
        }
        // Struct/Array with mixed slots: need slot_types from struct_metas
        ValueKind::Struct | ValueKind::Array => {
            if let Some(module) = module {
                let meta_id = meta.meta_id() as usize;
                if meta_id < module.struct_metas.len() {
                    typed_write_barrier(gc, parent, vals, &module.struct_metas[meta_id].slot_types);
                }
            }
        }
        // Interface: 2 slots (slot0=header, slot1=data). Only barrier data if it's a GcRef.
        ValueKind::Interface => {
            if vals.len() >= 2 {
                if interface::data_is_gc_ref(vals[0]) && vals[1] != 0 {
                    gc.write_barrier(parent, vals[1] as GcRef);
                }
            }
        }
        // Primitive types: no GcRefs
        _ => {}
    }
}

/// Scan a GC object and mark its children.
///
/// `func_slot_types`: indexed by func_id, each entry is the slot_types for that function.
/// Used to scan closure captures with correct types (Interface0/Interface1 vs GcRef).
pub fn scan_object(gc: &mut Gc, obj: GcRef, struct_metas: &[StructMeta], func_slot_types: &[&[SlotType]]) {
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
            scan_closure(gc, obj, func_slot_types);
        }

        ValueKind::Map => {
            // PtrNew creates heap-boxed map variables with kind=Map but slots=1
            // (single GcRef to the real map). Only scan as map if it's a real
            // MapData object (DATA_SLOTS=3). Otherwise scan slots as GcRefs.
            if gc_header.slots == map::DATA_SLOTS {
                scan_map(gc, obj, struct_metas);
            } else {
                scan_slots_as_gcrefs(gc, obj, gc_header.slots);
            }
        }

        ValueKind::Channel => {
            if gc_header.slots == queue_state::DATA_SLOTS {
                scan_channel(gc, obj, struct_metas);
            } else {
                scan_slots_as_gcrefs(gc, obj, gc_header.slots);
            }
        }

        // Port: state is Arc<Mutex<>> on Rust heap, not a GC object.
        // Elements live in QueueState's Vecs, also Rust heap. No GC refs to scan.
        // Island: id only, no GC refs.
        // Primitives: no GC refs.
        _ => {}
    }
}

/// Scan closure captures using the wrapper function's slot_types.
///
/// Closure layout: [ClosureHeader (1 slot)] [capture_0] [capture_1] ...
/// For regular closures, all captures are GcRef (pointers to heap-boxed escaped vars).
/// For method value closures, captures may include interface data (itab + data).
/// The wrapper function's slot_types[0..capture_count] describes the capture layout:
/// slot 0 = closure ref (GcRef), so captures start at slot_types[1..].
fn scan_closure(gc: &mut Gc, obj: GcRef, func_slot_types: &[&[SlotType]]) {
    let func_id = closure::func_id(obj);
    let cap_count = closure::capture_count(obj);
    if cap_count == 0 { return; }

    let slot_types = func_slot_types.get(func_id as usize)
        .unwrap_or_else(|| panic!("scan_closure: missing slot_types for func_id {}", func_id));
    
    // Wrapper function slot_types: [closure_ref(GcRef), params..., locals...]
    // Captures are stored at closure object offsets [0..cap_count),
    // but their types correspond to what ClosureGet reads into the function body.
    // For closure functions, slot 0 is the closure ref itself.
    // The captures are read by ClosureGet instructions and their types are
    // determined by how the wrapper uses them.
    //
    // If we have slot_types and the function is a closure (slot_types[0] = GcRef for closure ref),
    // then captures correspond to what ClosureGet reads. For method value wrappers with
    // interface receivers, capture 0 = Interface0, capture 1 = Interface1.
    //
    if cap_count + 1 > slot_types.len() {
        panic!(
            "scan_closure: capture layout overflow for func_id {} (captures={}, slot_types={})",
            func_id,
            cap_count,
            slot_types.len(),
        );
    }
    
    // Use slot_types[1..1+cap_count] for capture types (slot 0 = closure ref)
    let capture_slots = unsafe {
        core::slice::from_raw_parts(
            (obj as *const u64).add(closure::HEADER_SLOTS),
            cap_count,
        )
    };
    let capture_types = &slot_types[1..1 + cap_count];
    scan_slots_by_types(gc, capture_slots, capture_types);
}

/// Scan all slots of a GC object as potential GcRefs.
/// Used for PtrNew pointer-to-T objects (e.g., heap-return for `*map[K]V`)
/// where slots < DATA_SLOTS and each slot holds a reference to the actual object.
#[inline]
fn scan_slots_as_gcrefs(gc: &mut Gc, obj: GcRef, slots: u16) {
    for i in 0..slots as usize {
        let slot = unsafe { Gc::read_slot(obj, i) };
        if slot != 0 { gc.mark_gray(slot_to_ptr(slot)); }
    }
}

fn scan_array(gc: &mut Gc, obj: GcRef, struct_metas: &[StructMeta]) {
    let elem_meta = array::elem_meta(obj);
    let elem_kind = elem_meta.value_kind();
    
    // Packed types (bool, int8-32, float32) don't contain GcRefs
    if !elem_kind.may_contain_gc_refs() { return; }
    
    let len = array::len(obj);
    let elem_bytes = array::elem_bytes(obj);
    
    // For INLINE struct elements (ValueKind::Struct), use field slot_types from struct_metas.
    // Pointer elements (ValueKind::Pointer) are a single GcRef slot each — using the
    // pointed-to struct's slot_types here would read beyond each 1-slot element into adjacent
    // array slots and past the array end, causing mark_gray to be called on invalid addresses.
    if elem_kind == ValueKind::Struct {
        let meta_id = elem_meta.meta_id() as usize;
        if meta_id < struct_metas.len() {
            let slot_types = &struct_metas[meta_id].slot_types;
            for idx in 0..len {
                scan_array_struct_elem(gc, obj, idx, elem_bytes, slot_types);
            }
        }
        // No struct_meta available — skip scanning (matches scan_struct behavior).
        // Falling through to the generic "all GcRef" path would be wrong for structs
        // with mixed Value/GcRef fields.
        return;
    }

    // Interface elements: 2 slots per element (slot0=itab, slot1=data).
    // The generic handler below treats every slot as a GcRef, but slot0 (itab) is a
    // static code pointer — NOT a GcRef. In release builds itab addresses are >= 4096
    // and pass all mark_gray guards, causing mark_gray to read from the code section
    // as a GcHeader → SIGSEGV. Handle Interface explicitly using data_is_gc_ref.
    if elem_kind == ValueKind::Interface {
        let base_off = byte_offset_for_slots(array::HEADER_SLOTS);
        for idx in 0..len {
            let elem_off = base_off + idx * elem_bytes;
            let itab_slot = unsafe { *((obj as *const u8).add(elem_off) as *const Slot) };
            if interface::data_is_gc_ref(itab_slot) {
                let data_slot = unsafe { *((obj as *const u8).add(elem_off + SLOT_BYTES) as *const Slot) };
                if data_slot != 0 { gc.mark_gray(slot_to_ptr(data_slot)); }
            }
        }
        return;
    }
    
    // For reference types (slice, map, string, etc.), each element is a single GcRef
    let base_off = byte_offset_for_slots(array::HEADER_SLOTS);
    for idx in 0..len {
        let ptr = unsafe { (obj as *const u8).add(base_off + idx * elem_bytes) as *const Slot };
        let child = unsafe { *ptr };
        if child != 0 { gc.mark_gray(slot_to_ptr(child)); }
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
    let elem_meta = queue_state::elem_meta(obj);
    let elem_kind = elem_meta.value_kind();
    if !elem_kind.may_contain_gc_refs() { return; }
    
    let elem_scan = resolve_elem_scan(elem_kind, elem_meta, true, struct_metas);
    if matches!(elem_scan, ElemScan::Skip) { return; }
    
    let state = channel::get_state(obj);
    for elem in state.iter_buffer() {
        scan_elem(gc, elem, &elem_scan);
    }
    for elem in state.iter_waiting_values() {
        scan_elem(gc, elem, &elem_scan);
    }
}

/// Strategy for scanning a composite element (map key/value, channel element).
enum ElemScan<'a> {
    Skip,
    GcRefs,
    Interface,
    Typed(&'a [SlotType]),
}

fn resolve_elem_scan<'a>(kind: ValueKind, meta: ValueMeta, should_scan: bool, struct_metas: &'a [StructMeta]) -> ElemScan<'a> {
    if !should_scan { return ElemScan::Skip; }
    match kind {
        ValueKind::Struct => {
            let meta_id = meta.meta_id() as usize;
            if meta_id < struct_metas.len() {
                ElemScan::Typed(&struct_metas[meta_id].slot_types)
            } else {
                ElemScan::Skip
            }
        }
        // Pointer is a single GcRef slot (address of heap struct), NOT an inline struct.
        // Using Typed(struct_slot_types) here would interpret the pointer address bytes
        // as struct fields — completely wrong.
        ValueKind::Pointer => ElemScan::GcRefs,
        ValueKind::Interface => ElemScan::Interface,
        _ => ElemScan::GcRefs,
    }
}

fn scan_elem(gc: &mut Gc, slots: &[u64], scan: &ElemScan) {
    match scan {
        ElemScan::Skip => {}
        ElemScan::GcRefs => {
            for &slot in slots {
                if slot != 0 { gc.mark_gray(slot as GcRef); }
            }
        }
        ElemScan::Interface => {
            if slots.len() >= 2 {
                if interface::data_is_gc_ref(slots[0]) && slots[1] != 0 {
                    gc.mark_gray(slots[1] as GcRef);
                }
            }
        }
        ElemScan::Typed(types) => {
            scan_slots_by_types(gc, slots, types);
        }
    }
}

fn scan_map(gc: &mut Gc, obj: GcRef, struct_metas: &[StructMeta]) {
    let key_meta = map::key_meta(obj);
    let val_meta = map::val_meta(obj);
    let key_kind = key_meta.value_kind();
    let val_kind = val_meta.value_kind();
    
    if !key_kind.may_contain_gc_refs() && !val_kind.may_contain_gc_refs() { return; }
    
    let key_scan = resolve_elem_scan(key_kind, key_meta, key_kind.may_contain_gc_refs(), struct_metas);
    let val_scan = resolve_elem_scan(val_kind, val_meta, val_kind.may_contain_gc_refs(), struct_metas);
    
    if matches!(key_scan, ElemScan::Skip) && matches!(val_scan, ElemScan::Skip) { return; }
    
    let mut iter = map::iter_init(obj);
    while let Some((k, v)) = map::iter_next(&mut iter) {
        scan_elem(gc, k, &key_scan);
        scan_elem(gc, v, &val_scan);
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
        ValueKind::Channel => {
            // Only finalize real channel objects (DATA_SLOTS=3), not heap-boxed
            // pointer-to-channel (1 slot) created by PtrNew.
            if header.slots == queue_state::DATA_SLOTS {
                unsafe { channel::drop_inner(obj); }
            }
        }
        ValueKind::Map => {
            if header.slots == map::DATA_SLOTS {
                unsafe { map::drop_inner(obj); }
            }
        }
        ValueKind::Port => {
            if header.slots == queue_state::DATA_SLOTS {
                unsafe { port::drop_inner(obj); }
            }
        }
        // Island has no native resources to finalize (channels managed by VM)
        _ => {}
    }
}