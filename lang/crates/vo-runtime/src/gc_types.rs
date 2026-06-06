#![allow(clippy::not_unsafe_ptr_arg_deref)]
//! GC object scanning by type.

#[cfg(not(feature = "std"))]
use alloc::vec;

use crate::gc::{trace_slots_by_types, Gc, GcRef};
use crate::objects::{array, closure, interface, map, queue, queue_state, slice};
use crate::slot::{byte_offset_for_slots, slot_to_ptr, Slot, SLOT_BYTES};
use vo_common_core::bytecode::StructMeta;
use vo_common_core::types::{SlotType, ValueKind, ValueMeta};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypedWriteBarrierByMetaError {
    AllocationFailed,
    MissingModuleMetadata,
    MissingStructMeta { meta_id: usize },
    SlotWidthMismatch { vals: usize, slot_types: usize },
    InterfacePairTruncated { slot: usize },
    InterfacePairMalformed { slot: usize },
}

impl core::fmt::Display for TypedWriteBarrierByMetaError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::AllocationFailed => write!(f, "typed_write_barrier_by_meta: allocation failed"),
            Self::MissingModuleMetadata => {
                write!(f, "typed_write_barrier_by_meta: missing module metadata")
            }
            Self::MissingStructMeta { meta_id } => {
                write!(
                    f,
                    "typed_write_barrier_by_meta: missing StructMeta id {meta_id}"
                )
            }
            Self::SlotWidthMismatch { vals, slot_types } => write!(
                f,
                "typed_write_barrier: vals length {vals} != slot_types length {slot_types}"
            ),
            Self::InterfacePairTruncated { slot } => write!(
                f,
                "typed_write_barrier: Interface0 at slot {slot} missing Interface1 data slot"
            ),
            Self::InterfacePairMalformed { slot } => write!(
                f,
                "typed_write_barrier: Interface0 at slot {slot} must be followed by Interface1"
            ),
        }
    }
}

/// GC layout metadata needed to scan closure captures for a function.
#[derive(Clone, Copy, Debug, Default)]
pub struct ClosureScanLayout<'a> {
    /// Explicit capture slot layout for codegen-created closures and wrappers.
    pub capture_slot_types: &'a [SlotType],
    /// Capture slot layout for runtime-created direct method closures.
    pub runtime_capture_slot_types: &'a [SlotType],
}

impl<'a> ClosureScanLayout<'a> {
    #[inline]
    pub const fn new(
        capture_slot_types: &'a [SlotType],
        runtime_capture_slot_types: &'a [SlotType],
    ) -> Self {
        Self {
            capture_slot_types,
            runtime_capture_slot_types,
        }
    }
}

/// Type-safe write barrier for mixed-slot values.
///
/// Only barriers slots that are actually GcRefs (SlotType::GcRef) or
/// interface data slots (SlotType::Interface0 + data_is_gc_ref check).
/// Avoids UB from passing non-pointer values (int, float, slot0 metadata)
/// to write_barrier, which would dereference them as GcHeader pointers.
///
/// Used by MapSet, ChanSend, and any operation writing mixed-type values into heap objects.
pub fn typed_write_barrier(gc: &mut Gc, parent: GcRef, vals: &[u64], slot_types: &[SlotType]) {
    try_typed_write_barrier(gc, parent, vals, slot_types).unwrap_or_else(|err| panic!("{err}"));
}

pub fn try_typed_write_barrier(
    gc: &mut Gc,
    parent: GcRef,
    vals: &[u64],
    slot_types: &[SlotType],
) -> Result<(), TypedWriteBarrierByMetaError> {
    if vals.len() != slot_types.len() {
        return Err(TypedWriteBarrierByMetaError::SlotWidthMismatch {
            vals: vals.len(),
            slot_types: slot_types.len(),
        });
    }

    let mut i = 0;
    while i < slot_types.len() {
        match slot_types[i] {
            SlotType::GcRef => {
                if vals[i] != 0 {
                    gc.write_barrier(parent, vals[i] as GcRef);
                }
            }
            SlotType::Interface0 => {
                if i + 1 >= vals.len() {
                    return Err(TypedWriteBarrierByMetaError::InterfacePairTruncated { slot: i });
                }
                if slot_types[i + 1] != SlotType::Interface1 {
                    return Err(TypedWriteBarrierByMetaError::InterfacePairMalformed { slot: i });
                }
                if interface::data_is_gc_ref(vals[i]) && vals[i + 1] != 0 {
                    gc.write_barrier(parent, vals[i + 1] as GcRef);
                }
                i += 1; // skip data slot (Interface1)
            }
            _ => {}
        }
        i += 1;
    }
    Ok(())
}

#[inline]
fn slot_types_may_contain_gc_refs(slot_types: &[SlotType]) -> bool {
    slot_types
        .iter()
        .any(|slot_type| matches!(slot_type, SlotType::GcRef | SlotType::Interface0))
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
    try_typed_write_barrier_by_meta(gc, parent, vals, meta, module)
        .unwrap_or_else(|err| panic!("{err}"));
}

pub fn try_typed_write_barrier_by_meta(
    gc: &mut Gc,
    parent: GcRef,
    vals: &[u64],
    meta: vo_common_core::types::ValueMeta,
    module: Option<&vo_common_core::bytecode::Module>,
) -> Result<(), TypedWriteBarrierByMetaError> {
    use vo_common_core::types::ValueKind;
    let vk = meta.value_kind();
    match vk {
        // Single-slot reference types: the entire value is a GcRef
        ValueKind::String
        | ValueKind::Slice
        | ValueKind::Map
        | ValueKind::Closure
        | ValueKind::Channel
        | ValueKind::Port
        | ValueKind::Pointer
        | ValueKind::Island => {
            if !vals.is_empty() && vals[0] != 0 {
                gc.write_barrier(parent, vals[0] as GcRef);
            }
        }
        // Struct with mixed slots: need slot_types from struct_metas.
        ValueKind::Struct => {
            let module = module.ok_or(TypedWriteBarrierByMetaError::MissingModuleMetadata)?;
            let meta_id = meta.meta_id() as usize;
            let struct_meta = module
                .struct_metas
                .get(meta_id)
                .ok_or(TypedWriteBarrierByMetaError::MissingStructMeta { meta_id })?;
            if !slot_types_may_contain_gc_refs(&struct_meta.slot_types) {
                return Ok(());
            }
            try_typed_write_barrier(gc, parent, vals, &struct_meta.slot_types)?;
        }
        // Fixed arrays are flattened in value storage. ValueMeta does not carry the
        // array element layout, so use write_barrier's child validation instead of
        // pretending meta_id is a StructMeta index.
        ValueKind::Array => {
            for &slot in vals {
                if slot != 0 {
                    gc.write_barrier(parent, slot as GcRef);
                }
            }
        }
        // Interface: 2 slots (slot0=header, slot1=data). Only barrier data if it's a GcRef.
        ValueKind::Interface => {
            if vals.len() >= 2 && interface::data_is_gc_ref(vals[0]) && vals[1] != 0 {
                gc.write_barrier(parent, vals[1] as GcRef);
            }
        }
        // Primitive types: no GcRefs
        _ => {}
    }
    Ok(())
}

/// Apply typed write barriers for a contiguous range of elements just written
/// into an existing heap container.
pub fn typed_write_barrier_range_by_meta(
    gc: &mut Gc,
    parent: GcRef,
    base_ptr: *const u8,
    count: usize,
    elem_bytes: usize,
    meta: vo_common_core::types::ValueMeta,
    module: Option<&vo_common_core::bytecode::Module>,
) {
    if count == 0 || !meta.value_kind().may_contain_gc_refs() {
        return;
    }

    let elem_slots = elem_bytes.div_ceil(SLOT_BYTES);
    let mut vals = vec![0u64; elem_slots];
    for idx in 0..count {
        let elem_ptr = unsafe { base_ptr.add(idx * elem_bytes) };
        vals.fill(0);
        unsafe {
            core::ptr::copy_nonoverlapping(elem_ptr, vals.as_mut_ptr() as *mut u8, elem_bytes);
        }
        typed_write_barrier_by_meta(gc, parent, &vals, meta, module);
    }
}

/// Scan a GC object and mark its children.
///
/// `func_closure_scan_layout`: returns closure capture GC metadata for a function id.
/// Used to scan closure captures with correct types (Interface0/Interface1 vs GcRef).
pub fn scan_object<'a, F>(
    gc: &mut Gc,
    obj: GcRef,
    struct_metas: &[StructMeta],
    func_closure_scan_layout: &F,
) where
    F: Fn(u32) -> ClosureScanLayout<'a> + ?Sized,
{
    trace_object_children(obj, struct_metas, func_closure_scan_layout, |child| {
        gc.mark_gray(child)
    });
}

/// Visit a GC object's children through the same precise metadata rules used by collection.
pub fn trace_object_children<'a, F, V>(
    obj: GcRef,
    struct_metas: &[StructMeta],
    func_closure_scan_layout: &F,
    mut visit: V,
) where
    F: Fn(u32) -> ClosureScanLayout<'a> + ?Sized,
    V: FnMut(GcRef),
{
    let gc_header = Gc::header(obj);

    // Large arrays use GcHeader.slots == 0 and store the real size in ArrayHeader.
    // Any other ValueKind::Array object with slots < HEADER_SLOTS is a codegen bug.
    match gc_header.kind() {
        ValueKind::Array => {
            assert!(
                gc_header.slots == 0 || gc_header.slots >= array::HEADER_SLOTS as u16,
                "scan_object: Array object {:p} has invalid slots={} (expected 0 for large array or >= HEADER_SLOTS={}) — codegen bug (PtrNew box should use Struct)",
                obj, gc_header.slots, array::HEADER_SLOTS
            );
            trace_array_children(obj, struct_metas, &mut visit);
        }
        ValueKind::String => {
            let arr = slice::array_ref(obj);
            if !arr.is_null() {
                visit(arr);
            }
        }

        ValueKind::Slice => {
            let arr = slice::array_ref(obj);
            if !arr.is_null() {
                visit(arr);
            }
        }

        ValueKind::Struct | ValueKind::Pointer => {
            trace_struct_children(obj, gc_header.meta_id() as usize, struct_metas, &mut visit);
        }

        ValueKind::Closure => {
            trace_closure_children(obj, func_closure_scan_layout, &mut visit);
        }

        ValueKind::Map => {
            // Real maps: created by map::create with DATA_SLOTS=3 (MapData layout).
            // PtrNew heap-boxed map variables: kind=Struct (fixed by get_boxing_meta),
            // so they never reach this branch. Any Map with wrong slots is a bug.
            assert!(
                gc_header.slots == map::DATA_SLOTS,
                "scan_object: Map object {:p} has slots={} != DATA_SLOTS={} — codegen bug",
                obj,
                gc_header.slots,
                map::DATA_SLOTS
            );
            trace_map_children(obj, struct_metas, &mut visit);
        }

        kind if kind.is_queue() => {
            assert!(
                gc_header.slots == queue_state::DATA_SLOTS,
                "scan_object: Queue object {:p} has slots={} != DATA_SLOTS={} — codegen bug",
                obj,
                gc_header.slots,
                queue_state::DATA_SLOTS
            );
            trace_queue_children(obj, struct_metas, &mut visit);
        }

        // Remote channel proxy: state lives on home island, not locally.
        // LOCAL channels are scanned via scan_channel. No GC refs to scan here.
        // Island: id only, no GC refs.
        // Void: used for defer args storage — scanned precisely by scan_defer_entry.
        // Primitives: no GC refs.
        _ => {}
    }
}

/// Scan closure captures using the function's capture GC layout.
///
/// Closure layout: [ClosureHeader (1 slot)] [capture_0] [capture_1] ...
/// For regular closures, all captures are GcRef (pointers to heap-boxed escaped vars).
/// For method value closures, captures may include interface data (itab + data).
/// Runtime-created direct method closures capture receiver slot1 for a method function.
fn trace_closure_children<'a, F, V>(obj: GcRef, func_capture_slot_types: &F, visit: &mut V)
where
    F: Fn(u32) -> ClosureScanLayout<'a> + ?Sized,
    V: FnMut(GcRef),
{
    let func_id = closure::func_id(obj);
    let cap_count = closure::capture_count(obj);
    if cap_count == 0 {
        return;
    }

    let capture_slots = unsafe {
        core::slice::from_raw_parts((obj as *const u64).add(closure::HEADER_SLOTS), cap_count)
    };

    let layout = func_capture_slot_types(func_id);
    let capture_types = layout.capture_slot_types;
    if !capture_types.is_empty() {
        trace_slots_by_types(capture_slots, capture_types, visit);
        return;
    }

    let runtime_capture_types = layout.runtime_capture_slot_types;
    if !runtime_capture_types.is_empty() {
        if runtime_capture_types.len() == cap_count {
            trace_slots_by_types(capture_slots, runtime_capture_types, visit);
            return;
        }
        panic!(
            "scan_closure: func_id={} has {} captures but runtime capture layout has {} slots",
            func_id,
            cap_count,
            runtime_capture_types.len()
        );
    }

    panic!(
        "scan_closure: func_id={} has {} captures but empty capture_slot_types — codegen must set capture_slot_types for all closures with captures",
        func_id, cap_count
    );
}

fn trace_array_children<V>(obj: GcRef, struct_metas: &[StructMeta], visit: &mut V)
where
    V: FnMut(GcRef),
{
    let elem_meta = array::elem_meta(obj);
    let elem_kind = elem_meta.value_kind();

    // Packed types (bool, int8-32, float32) don't contain GcRefs
    if !elem_kind.may_contain_gc_refs() {
        return;
    }

    let len = array::len(obj);
    let elem_bytes = array::elem_bytes(obj);

    // For INLINE struct elements (ValueKind::Struct), use field slot_types from struct_metas.
    // Pointer elements (ValueKind::Pointer) are a single GcRef slot each — using the
    // pointed-to struct's slot_types here would read beyond each 1-slot element into adjacent
    // array slots and past the array end, causing mark_gray to be called on invalid addresses.
    if elem_kind == ValueKind::Struct {
        let meta_id = elem_meta.meta_id() as usize;
        let slot_types = &struct_metas
            .get(meta_id)
            .unwrap_or_else(|| panic!("scan_array: missing StructMeta id {meta_id}"))
            .slot_types;
        for idx in 0..len {
            trace_array_struct_elem(obj, idx, elem_bytes, slot_types, visit);
        }
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
                let data_slot =
                    unsafe { *((obj as *const u8).add(elem_off + SLOT_BYTES) as *const Slot) };
                if data_slot != 0 {
                    visit(slot_to_ptr(data_slot));
                }
            }
        }
        return;
    }

    // For reference types (slice, map, string, etc.), each element is a single GcRef
    let base_off = byte_offset_for_slots(array::HEADER_SLOTS);
    for idx in 0..len {
        let ptr = unsafe { (obj as *const u8).add(base_off + idx * elem_bytes) as *const Slot };
        let child = unsafe { *ptr };
        if child != 0 {
            visit(slot_to_ptr(child));
        }
    }
}

fn trace_array_struct_elem<V>(
    obj: GcRef,
    idx: usize,
    elem_bytes: usize,
    slot_types: &[SlotType],
    visit: &mut V,
) where
    V: FnMut(GcRef),
{
    let base_off = byte_offset_for_slots(array::HEADER_SLOTS) + idx * elem_bytes;
    let mut i = 0;
    while i < slot_types.len() {
        let st = slot_types[i];
        if st == SlotType::GcRef {
            let ptr = unsafe { (obj as *const u8).add(base_off + i * SLOT_BYTES) as *const Slot };
            let child = unsafe { *ptr };
            if child != 0 {
                visit(slot_to_ptr(child));
            }
        } else if st == SlotType::Interface0 {
            let header_ptr =
                unsafe { (obj as *const u8).add(base_off + i * SLOT_BYTES) as *const Slot };
            let header_slot = unsafe { *header_ptr };
            if interface::data_is_gc_ref(header_slot) {
                let data_ptr = unsafe {
                    (obj as *const u8).add(base_off + (i + 1) * SLOT_BYTES) as *const Slot
                };
                let child = unsafe { *data_ptr };
                if child != 0 {
                    visit(slot_to_ptr(child));
                }
            }
            i += 1;
        }
        i += 1;
    }
}

fn trace_queue_children<V>(obj: GcRef, struct_metas: &[StructMeta], visit: &mut V)
where
    V: FnMut(GcRef),
{
    // REMOTE channels have no local state — elements live on the home island
    if queue::is_remote(obj) {
        return;
    }

    let elem_meta = queue_state::elem_meta(obj);
    let elem_kind = elem_meta.value_kind();
    if !elem_kind.may_contain_gc_refs() {
        return;
    }

    let elem_scan = resolve_elem_scan(elem_kind, elem_meta, true, struct_metas);
    if matches!(elem_scan, ElemScan::NoRefs) {
        return;
    }

    let state = queue::local_state(obj);
    for elem in state.iter_buffer() {
        trace_elem(elem, &elem_scan, visit);
    }
    for elem in state.iter_waiting_values() {
        trace_elem(elem, &elem_scan, visit);
    }
}

/// Strategy for scanning a composite element (map key/value, channel element).
enum ElemScan<'a> {
    NoRefs,
    GcRefs,
    Interface,
    Typed(&'a [SlotType]),
}

fn resolve_elem_scan<'a>(
    kind: ValueKind,
    meta: ValueMeta,
    should_scan: bool,
    struct_metas: &'a [StructMeta],
) -> ElemScan<'a> {
    if !should_scan {
        return ElemScan::NoRefs;
    }
    match kind {
        ValueKind::Struct => {
            let meta_id = meta.meta_id() as usize;
            ElemScan::Typed(
                &struct_metas
                    .get(meta_id)
                    .unwrap_or_else(|| panic!("resolve_elem_scan: missing StructMeta id {meta_id}"))
                    .slot_types,
            )
        }
        // Pointer is a single GcRef slot (address of heap struct), NOT an inline struct.
        // Using Typed(struct_slot_types) here would interpret the pointer address bytes
        // as struct fields — completely wrong.
        ValueKind::Pointer => ElemScan::GcRefs,
        ValueKind::Interface => ElemScan::Interface,
        _ => ElemScan::GcRefs,
    }
}

fn trace_elem<V>(slots: &[u64], scan: &ElemScan, visit: &mut V)
where
    V: FnMut(GcRef),
{
    match scan {
        ElemScan::NoRefs => {}
        ElemScan::GcRefs => {
            for &slot in slots {
                if slot != 0 {
                    visit(slot as GcRef);
                }
            }
        }
        ElemScan::Interface => {
            if slots.len() >= 2 && interface::data_is_gc_ref(slots[0]) && slots[1] != 0 {
                visit(slots[1] as GcRef);
            }
        }
        ElemScan::Typed(types) => {
            trace_slots_by_types(slots, types, visit);
        }
    }
}

fn trace_map_children<V>(obj: GcRef, struct_metas: &[StructMeta], visit: &mut V)
where
    V: FnMut(GcRef),
{
    let key_meta = map::key_meta(obj);
    let val_meta = map::val_meta(obj);
    let key_kind = key_meta.value_kind();
    let val_kind = val_meta.value_kind();

    if !key_kind.may_contain_gc_refs() && !val_kind.may_contain_gc_refs() {
        return;
    }

    let key_scan = resolve_elem_scan(
        key_kind,
        key_meta,
        key_kind.may_contain_gc_refs(),
        struct_metas,
    );
    let val_scan = resolve_elem_scan(
        val_kind,
        val_meta,
        val_kind.may_contain_gc_refs(),
        struct_metas,
    );

    if matches!(key_scan, ElemScan::NoRefs) && matches!(val_scan, ElemScan::NoRefs) {
        return;
    }

    let mut iter = map::iter_init(obj);
    while let Some((k, v)) = map::iter_next(&mut iter) {
        trace_elem(k, &key_scan, visit);
        trace_elem(v, &val_scan, visit);
    }
}

fn trace_struct_children<V>(obj: GcRef, meta_id: usize, struct_metas: &[StructMeta], visit: &mut V)
where
    V: FnMut(GcRef),
{
    let meta = struct_metas
        .get(meta_id)
        .unwrap_or_else(|| panic!("scan_struct: missing StructMeta id {meta_id}"));
    let mut i = 0;
    while i < meta.slot_types.len() {
        let st = meta.slot_types[i];
        if st == SlotType::GcRef {
            let child = unsafe { Gc::read_slot(obj, i) };
            if child != 0 {
                visit(child as GcRef);
            }
        } else if st == SlotType::Interface0 {
            let header_slot = unsafe { Gc::read_slot(obj, i) };
            if interface::data_is_gc_ref(header_slot) {
                let child = unsafe { Gc::read_slot(obj, i + 1) };
                if child != 0 {
                    visit(child as GcRef);
                }
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
        kind if kind.is_queue() => {
            // Only finalize real channel objects (DATA_SLOTS=3), not heap-boxed
            // pointer-to-channel (1 slot) created by PtrNew.
            if header.slots == queue_state::DATA_SLOTS {
                unsafe {
                    queue::drop_inner(obj);
                }
            }
        }
        ValueKind::Map => {
            if header.slots == map::DATA_SLOTS {
                unsafe {
                    map::drop_inner(obj);
                }
            }
        }
        // Island has no native resources to finalize (channels managed by VM)
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn section<'a>(src: &'a str, start: &str, end: &str) -> &'a str {
        let start = src.find(start).expect("section start");
        let end = src[start..]
            .find(end)
            .map(|offset| start + offset)
            .expect("section end");
        &src[start..end]
    }

    #[test]
    fn gc_layout_metadata_drift_has_no_release_skip_or_legacy_fallback() {
        let src = include_str!("gc_types.rs");

        let barrier = section(
            src,
            "pub fn typed_write_barrier_by_meta",
            "/// Apply typed write barriers",
        );
        assert!(
            !barrier.contains("conservative"),
            "typed write barriers must not conservatively continue when struct metadata is missing"
        );

        let scan_object = section(src, "pub fn scan_object", "/// Scan closure captures");
        assert!(
            !scan_object.contains("debug_assert"),
            "GC object layout validation must run in release builds"
        );
        assert!(
            !scan_object.contains("skip scanning"),
            "invalid GC object layouts must fail fast instead of skipping scans"
        );

        let scan_closure = section(src, "fn trace_closure_children", "fn trace_array_children");
        assert!(
            !scan_closure.contains("debug_assert"),
            "closure capture layout validation must run in release builds"
        );
        assert!(
            !scan_closure.contains("Compatibility fallback"),
            "closure capture layout drift must not use legacy all-GcRef fallback"
        );

        let scan_array = section(src, "fn trace_array_children", "fn trace_array_struct_elem");
        assert!(
            !scan_array.contains("No struct_meta available"),
            "array struct-element metadata drift must fail fast instead of skipping scans"
        );

        let elem_scan = section(src, "fn resolve_elem_scan", "fn trace_elem");
        assert!(
            !elem_scan.contains("ElemScan::Skip"),
            "container element scan resolution must not use missing-metadata skip"
        );

        let scan_struct = section(src, "fn trace_struct_children", "/// Finalize");
        assert!(
            !scan_struct.contains("return;"),
            "struct metadata drift must fail fast instead of returning without scanning"
        );
    }

    #[test]
    #[should_panic(expected = "typed_write_barrier: vals length 1 != slot_types length 2")]
    fn typed_write_barrier_rejects_non_exact_width() {
        let mut gc = Gc::new();
        let parent = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
        typed_write_barrier(&mut gc, parent, &[0], &[SlotType::GcRef, SlotType::Value]);
    }

    #[test]
    #[should_panic(
        expected = "typed_write_barrier: Interface0 at slot 0 missing Interface1 data slot"
    )]
    fn typed_write_barrier_rejects_truncated_interface_pair() {
        let mut gc = Gc::new();
        let parent = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
        typed_write_barrier(&mut gc, parent, &[0], &[SlotType::Interface0]);
    }

    #[test]
    #[should_panic(
        expected = "typed_write_barrier: Interface0 at slot 0 must be followed by Interface1"
    )]
    fn typed_write_barrier_rejects_malformed_interface_pair() {
        let mut gc = Gc::new();
        let parent = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 2);
        typed_write_barrier(
            &mut gc,
            parent,
            &[0, 0],
            &[SlotType::Interface0, SlotType::Value],
        );
    }

    #[test]
    fn typed_write_barrier_by_meta_accepts_zero_width_no_ref_struct() {
        let mut module = vo_common_core::bytecode::Module::new("test".to_string());
        module
            .struct_metas
            .push(vo_common_core::bytecode::StructMeta {
                slot_types: vec![SlotType::Value],
                fields: Vec::new(),
                field_index: std::collections::HashMap::new(),
            });
        let mut gc = Gc::new();
        let parent = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);

        try_typed_write_barrier_by_meta(
            &mut gc,
            parent,
            &[],
            ValueMeta::new(0, ValueKind::Struct),
            Some(&module),
        )
        .expect("zero-width no-ref struct barrier should be a no-op");
    }
}
