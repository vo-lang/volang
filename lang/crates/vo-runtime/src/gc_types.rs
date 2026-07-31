#![allow(clippy::missing_safety_doc, clippy::not_unsafe_ptr_arg_deref)]
//! GC object scanning by type.
//!
//! # Safety contract
//! Unsafe scanners require canonical live GC objects whose allocation layout
//! matches the supplied runtime metadata for the complete call.

#[cfg(not(feature = "std"))]
use alloc::vec;

use crate::gc::{trace_slots_by_types, Gc, GcObjectScanChunk, GcRef, GcTraceCursor};
#[cfg(test)]
use crate::objects::string;
use crate::objects::{array, closure, interface, map, queue, queue_state, slice};
use crate::slot::{byte_offset_for_slots, SLOT_BYTES};
use vo_common_core::bytecode::{NamedTypeMeta, RuntimeTypeResolver, StructMeta};
use vo_common_core::runtime_type::RuntimeType;
use vo_common_core::types::{SlotType, ValueKind, ValueMeta, ValueRttid};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypedWriteBarrierByMetaError {
    AllocationFailed,
    MissingModuleMetadata,
    MissingStructMeta {
        meta_id: usize,
    },
    MissingRuntimeType {
        rttid: u32,
    },
    MissingNamedTypeMeta {
        id: u32,
    },
    RuntimeTypeKindMismatch {
        rttid: u32,
        expected: ValueKind,
        actual: ValueKind,
    },
    ArraySlotWidthMismatch {
        expected: usize,
        actual: usize,
    },
    ArraySlotWidthOverflow,
    SlotWidthMismatch {
        vals: usize,
        slot_types: usize,
    },
    InterfacePairTruncated {
        slot: usize,
    },
    InterfacePairMalformed {
        slot: usize,
    },
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
            Self::MissingRuntimeType { rttid } => {
                write!(f, "typed_write_barrier_by_meta: missing runtime type {rttid}")
            }
            Self::MissingNamedTypeMeta { id } => {
                write!(
                    f,
                    "typed_write_barrier_by_meta: missing named type metadata {id}"
                )
            }
            Self::RuntimeTypeKindMismatch {
                rttid,
                expected,
                actual,
            } => write!(
                f,
                "typed_write_barrier_by_meta: runtime type {rttid} has kind {actual:?}, expected {expected:?}"
            ),
            Self::ArraySlotWidthMismatch { expected, actual } => write!(
                f,
                "typed_write_barrier_by_meta: array value slots length {actual} != expected {expected}"
            ),
            Self::ArraySlotWidthOverflow => write!(
                f,
                "typed_write_barrier_by_meta: array value slot width overflow"
            ),
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

/// Metadata tables needed for precise heap-object scanning.
///
/// `ValueMeta` can identify structs and interfaces directly, but array values
/// need their `ValueRttid` to recover nested element layout. Keep the full
/// module-side type tables together so every heap container uses the same
/// recursive slot interpretation.
#[derive(Clone, Copy, Debug)]
pub struct GcScanContext<'a> {
    pub struct_metas: &'a [StructMeta],
    pub named_type_metas: &'a [NamedTypeMeta],
    pub runtime_types: &'a [RuntimeType],
}

impl<'a> GcScanContext<'a> {
    #[inline]
    pub const fn new(struct_metas: &'a [StructMeta]) -> Self {
        Self {
            struct_metas,
            named_type_metas: &[],
            runtime_types: &[],
        }
    }

    #[inline]
    pub const fn from_module_parts(
        struct_metas: &'a [StructMeta],
        named_type_metas: &'a [NamedTypeMeta],
        runtime_types: &'a [RuntimeType],
    ) -> Self {
        Self {
            struct_metas,
            named_type_metas,
            runtime_types,
        }
    }

    #[inline]
    fn has_runtime_types(self) -> bool {
        !self.runtime_types.is_empty()
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
                let Some(value_kind) = interface::try_unpack_value_kind(vals[i]) else {
                    return Err(TypedWriteBarrierByMetaError::InterfacePairMalformed { slot: i });
                };
                if value_kind.may_contain_gc_refs() && vals[i + 1] != 0 {
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
            require_meta_slot_width(vals, 1)?;
            if vals[0] != 0 {
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
        // Fixed arrays are flattened in value storage. Their precise layout is
        // recovered from the array rttid stored in ValueMeta::meta_id.
        ValueKind::Array => {
            let module = module.ok_or(TypedWriteBarrierByMetaError::MissingModuleMetadata)?;
            let ctx = GcScanContext::from_module_parts(
                &module.struct_metas,
                &module.named_type_metas,
                &module.runtime_types,
            );
            trace_value_slots_by_meta(vals, meta, ctx, &mut |child| {
                gc.write_barrier(parent, child)
            })?;
        }
        // Interface: 2 slots (slot0=header, slot1=data). Only barrier data if it's a GcRef.
        ValueKind::Interface => {
            require_meta_slot_width(vals, 2)?;
            let Some(value_kind) = interface::try_unpack_value_kind(vals[0]) else {
                return Err(TypedWriteBarrierByMetaError::InterfacePairMalformed { slot: 0 });
            };
            if value_kind.may_contain_gc_refs() && vals[1] != 0 {
                gc.write_barrier(parent, vals[1] as GcRef);
            }
        }
        // Primitive types: no GcRefs
        _ => {}
    }
    Ok(())
}

fn require_meta_slot_width(
    vals: &[u64],
    expected: usize,
) -> Result<(), TypedWriteBarrierByMetaError> {
    if vals.len() == expected {
        return Ok(());
    }
    Err(TypedWriteBarrierByMetaError::SlotWidthMismatch {
        vals: vals.len(),
        slot_types: expected,
    })
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
pub unsafe fn scan_object<'a, F>(
    gc: &mut Gc,
    obj: GcRef,
    struct_metas: &[StructMeta],
    func_closure_scan_layout: &F,
) where
    F: Fn(u32) -> ClosureScanLayout<'a> + ?Sized,
{
    scan_object_with_context(
        gc,
        obj,
        GcScanContext::new(struct_metas),
        func_closure_scan_layout,
    );
}

pub unsafe fn scan_object_with_context<'a, F>(
    gc: &mut Gc,
    obj: GcRef,
    context: GcScanContext<'_>,
    func_closure_scan_layout: &F,
) where
    F: Fn(u32) -> ClosureScanLayout<'a> + ?Sized,
{
    trace_object_children_with_context(obj, context, func_closure_scan_layout, |child| {
        gc.mark_gray(child)
    });
}

/// Scan at most `limit_bytes` of one heap object's trace layout.
///
/// The cursor is owned by the collector and survives scheduler boundaries.
/// One work unit is one source slot inspected. Composite array values are
/// resolved by flat slot index, so deeply nested layouts require no temporary
/// allocation or native recursion.
pub unsafe fn scan_object_chunk_with_context<'a, F>(
    gc: &mut Gc,
    obj: GcRef,
    context: GcScanContext<'_>,
    func_closure_scan_layout: &F,
    cursor: &mut GcTraceCursor,
    limit_bytes: usize,
) -> GcObjectScanChunk
where
    F: Fn(u32) -> ClosureScanLayout<'a> + ?Sized,
{
    let slot_budget = limit_bytes / SLOT_BYTES;
    if slot_budget == 0 {
        return GcObjectScanChunk::pending(0);
    }

    let gc_header = unsafe { Gc::header(obj) };
    if gc_header.is_value_slots_object() {
        let slots =
            unsafe { core::slice::from_raw_parts(obj as *const u64, gc_header.slots as usize) };
        return scan_value_slots_chunk(
            gc,
            slots,
            gc_header.value_meta(),
            context,
            cursor,
            slot_budget,
            "scan_value_slots_object",
        );
    }

    match gc_header.kind() {
        ValueKind::Array => {
            assert!(
                gc_header.slots == 0 || gc_header.slots >= array::HEADER_SLOTS as u16,
                "scan_object_chunk: Array object {:p} has invalid slots={}",
                obj,
                gc_header.slots
            );
            unsafe { scan_array_chunk(gc, obj, context, cursor, slot_budget) }
        }
        ValueKind::String | ValueKind::Slice => {
            if cursor.reference_index == 0 {
                let owner = unsafe { slice::owner_ref(obj) };
                if !owner.is_null() {
                    gc.mark_gray(owner);
                }
                cursor.reference_index = 1;
                GcObjectScanChunk::complete(SLOT_BYTES)
            } else {
                GcObjectScanChunk::complete(0)
            }
        }
        ValueKind::Struct | ValueKind::Pointer => {
            let meta_id = gc_header.meta_id() as usize;
            let slot_types = &context
                .struct_metas
                .get(meta_id)
                .unwrap_or_else(|| panic!("scan_struct: missing StructMeta id {meta_id}"))
                .slot_types;
            let slots = unsafe { core::slice::from_raw_parts(obj as *const u64, slot_types.len()) };
            scan_slot_types_chunk(gc, slots, slot_types, cursor, slot_budget)
        }
        ValueKind::Closure => unsafe {
            scan_closure_chunk(gc, obj, func_closure_scan_layout, cursor, slot_budget)
        },
        ValueKind::Map => {
            assert!(
                gc_header.slots == map::DATA_SLOTS,
                "scan_object_chunk: Map object {:p} has slots={} != DATA_SLOTS={}",
                obj,
                gc_header.slots,
                map::DATA_SLOTS
            );
            unsafe { scan_map_chunk(gc, obj, context, cursor, slot_budget) }
        }
        kind if kind.is_queue() => {
            assert!(
                gc_header.slots == queue_state::DATA_SLOTS,
                "scan_object_chunk: Queue object {:p} has slots={} != DATA_SLOTS={}",
                obj,
                gc_header.slots,
                queue_state::DATA_SLOTS
            );
            unsafe { scan_queue_chunk(gc, obj, context, cursor, slot_budget) }
        }
        _ => GcObjectScanChunk::complete(0),
    }
}

fn scan_value_slots_chunk(
    gc: &mut Gc,
    slots: &[u64],
    meta: ValueMeta,
    context: GcScanContext<'_>,
    cursor: &mut GcTraceCursor,
    slot_budget: usize,
    label: &str,
) -> GcObjectScanChunk {
    let start = cursor.reference_index.min(slots.len());
    let end = start.saturating_add(slot_budget).min(slots.len());
    for flat_index in start..end {
        trace_flat_value_slot(slots, meta, context, flat_index, &mut |child| {
            gc.mark_gray(child)
        })
        .unwrap_or_else(|err| panic!("{label}: {err}"));
    }
    cursor.reference_index = end;
    let work_bytes = (end - start) * SLOT_BYTES;
    if end == slots.len() {
        GcObjectScanChunk::complete(work_bytes)
    } else {
        GcObjectScanChunk::pending(work_bytes)
    }
}

fn scan_slot_types_chunk(
    gc: &mut Gc,
    slots: &[u64],
    slot_types: &[SlotType],
    cursor: &mut GcTraceCursor,
    slot_budget: usize,
) -> GcObjectScanChunk {
    assert_eq!(
        slots.len(),
        slot_types.len(),
        "scan_slot_types_chunk: slots length {} != slot_types length {}",
        slots.len(),
        slot_types.len()
    );
    let start = cursor.reference_index.min(slots.len());
    let end = start.saturating_add(slot_budget).min(slots.len());
    for index in start..end {
        trace_flat_slot_by_types(slots, slot_types, index, &mut |child| gc.mark_gray(child));
    }
    cursor.reference_index = end;
    let work_bytes = (end - start) * SLOT_BYTES;
    if end == slots.len() {
        GcObjectScanChunk::complete(work_bytes)
    } else {
        GcObjectScanChunk::pending(work_bytes)
    }
}

fn trace_flat_slot_by_types<V>(slots: &[u64], slot_types: &[SlotType], index: usize, visit: &mut V)
where
    V: FnMut(GcRef),
{
    match slot_types.get(index).copied() {
        Some(SlotType::GcRef) if slots[index] != 0 => visit(slots[index] as GcRef),
        Some(SlotType::Interface1)
            if index > 0
                && slot_types[index - 1] == SlotType::Interface0
                && interface::data_is_gc_ref(slots[index - 1])
                && slots[index] != 0 =>
        {
            visit(slots[index] as GcRef)
        }
        _ => {}
    }
}

fn trace_flat_value_slot<V>(
    mut slots: &[u64],
    mut meta: ValueMeta,
    context: GcScanContext<'_>,
    mut flat_index: usize,
    visit: &mut V,
) -> Result<(), TypedWriteBarrierByMetaError>
where
    V: FnMut(GcRef),
{
    loop {
        match meta.value_kind() {
            ValueKind::Array => {
                if !context.has_runtime_types() {
                    return Ok(());
                }
                let resolver = RuntimeTypeResolver::new(
                    context.struct_metas,
                    context.named_type_metas,
                    context.runtime_types,
                );
                let array_rttid = ValueRttid::new(meta.meta_id(), ValueKind::Array);
                let Some((_, RuntimeType::Array { len, elem })) =
                    resolver.resolve_value_rttid(array_rttid)
                else {
                    return Err(TypedWriteBarrierByMetaError::MissingRuntimeType {
                        rttid: array_rttid.rttid(),
                    });
                };
                let expected = resolver
                    .slot_count_for_value_rttid(array_rttid)
                    .ok_or(TypedWriteBarrierByMetaError::ArraySlotWidthOverflow)?;
                if slots.len() != expected {
                    return Err(TypedWriteBarrierByMetaError::ArraySlotWidthMismatch {
                        expected,
                        actual: slots.len(),
                    });
                }
                let len = usize::try_from(*len)
                    .map_err(|_| TypedWriteBarrierByMetaError::ArraySlotWidthOverflow)?;
                if len == 0 {
                    return Ok(());
                }
                let elem_slots = slots.len() / len;
                if elem_slots == 0 || !slots.len().is_multiple_of(len) {
                    return Err(TypedWriteBarrierByMetaError::ArraySlotWidthMismatch {
                        expected: len,
                        actual: slots.len(),
                    });
                }
                let element_index = flat_index / elem_slots;
                if element_index >= len {
                    return Ok(());
                }
                let start = element_index
                    .checked_mul(elem_slots)
                    .ok_or(TypedWriteBarrierByMetaError::ArraySlotWidthOverflow)?;
                slots = &slots[start..start + elem_slots];
                flat_index %= elem_slots;
                meta = resolver.canonical_value_meta_for_value_rttid(*elem).ok_or(
                    TypedWriteBarrierByMetaError::MissingRuntimeType {
                        rttid: elem.rttid(),
                    },
                )?;
            }
            ValueKind::Struct => {
                let meta_id = meta.meta_id() as usize;
                let slot_types = &context
                    .struct_metas
                    .get(meta_id)
                    .ok_or(TypedWriteBarrierByMetaError::MissingStructMeta { meta_id })?
                    .slot_types;
                if slots.len() != slot_types.len() {
                    return Err(TypedWriteBarrierByMetaError::SlotWidthMismatch {
                        vals: slots.len(),
                        slot_types: slot_types.len(),
                    });
                }
                trace_flat_slot_by_types(slots, slot_types, flat_index, visit);
                return Ok(());
            }
            ValueKind::Interface => {
                if flat_index == 1
                    && slots.len() >= 2
                    && interface::data_is_gc_ref(slots[0])
                    && slots[1] != 0
                {
                    visit(slots[1] as GcRef);
                }
                return Ok(());
            }
            ValueKind::String
            | ValueKind::Slice
            | ValueKind::Map
            | ValueKind::Channel
            | ValueKind::Closure
            | ValueKind::Pointer
            | ValueKind::Port
            | ValueKind::Island => {
                if flat_index == 0 && slots.first().copied().unwrap_or(0) != 0 {
                    visit(slots[0] as GcRef);
                }
                return Ok(());
            }
            _ => return Ok(()),
        }
    }
}

/// Visit a GC object's children through the same precise metadata rules used by collection.
pub unsafe fn trace_object_children<'a, F, V>(
    obj: GcRef,
    struct_metas: &[StructMeta],
    func_closure_scan_layout: &F,
    mut visit: V,
) where
    F: Fn(u32) -> ClosureScanLayout<'a> + ?Sized,
    V: FnMut(GcRef),
{
    trace_object_children_with_context(
        obj,
        GcScanContext::new(struct_metas),
        func_closure_scan_layout,
        &mut visit,
    );
}

pub unsafe fn trace_object_children_with_context<'a, F, V>(
    obj: GcRef,
    context: GcScanContext<'_>,
    func_closure_scan_layout: &F,
    mut visit: V,
) where
    F: Fn(u32) -> ClosureScanLayout<'a> + ?Sized,
    V: FnMut(GcRef),
{
    let gc_header = unsafe { Gc::header(obj) };

    if gc_header.is_value_slots_object() {
        let slots =
            unsafe { core::slice::from_raw_parts(obj as *const u64, gc_header.slots as usize) };
        trace_value_slots_by_meta(slots, gc_header.value_meta(), context, &mut visit)
            .unwrap_or_else(|err| panic!("scan_value_slots_object: {err}"));
        return;
    }

    // Large arrays use GcHeader.slots == 0 and store the real size in ArrayHeader.
    // Any other ValueKind::Array object with slots < HEADER_SLOTS is a codegen bug.
    match gc_header.kind() {
        ValueKind::Array => {
            assert!(
                gc_header.slots == 0 || gc_header.slots >= array::HEADER_SLOTS as u16,
                "scan_object: Array object {:p} has invalid slots={} (expected 0 for large array or >= HEADER_SLOTS={}) — codegen bug (PtrNew box should use Struct)",
                obj, gc_header.slots, array::HEADER_SLOTS
            );
            trace_array_children(obj, context, &mut visit);
        }
        ValueKind::String => {
            let owner = slice::owner_ref(obj);
            if !owner.is_null() {
                visit(owner);
            }
        }

        ValueKind::Slice => {
            let owner = slice::owner_ref(obj);
            if !owner.is_null() {
                visit(owner);
            }
        }

        ValueKind::Struct | ValueKind::Pointer => {
            trace_struct_children(
                obj,
                gc_header.meta_id() as usize,
                context.struct_metas,
                &mut visit,
            );
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
            trace_map_children(obj, context, &mut visit);
        }

        kind if kind.is_queue() => {
            assert!(
                gc_header.slots == queue_state::DATA_SLOTS,
                "scan_object: Queue object {:p} has slots={} != DATA_SLOTS={} — codegen bug",
                obj,
                gc_header.slots,
                queue_state::DATA_SLOTS
            );
            trace_queue_children(obj, context, &mut visit);
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
unsafe fn trace_closure_children<'a, F, V>(obj: GcRef, func_capture_slot_types: &F, visit: &mut V)
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

unsafe fn scan_closure_chunk<'a, F>(
    gc: &mut Gc,
    obj: GcRef,
    func_capture_slot_types: &F,
    cursor: &mut GcTraceCursor,
    slot_budget: usize,
) -> GcObjectScanChunk
where
    F: Fn(u32) -> ClosureScanLayout<'a> + ?Sized,
{
    let func_id = unsafe { closure::func_id(obj) };
    let cap_count = unsafe { closure::capture_count(obj) };
    if cap_count == 0 {
        return GcObjectScanChunk::complete(0);
    }
    let capture_slots = unsafe {
        core::slice::from_raw_parts((obj as *const u64).add(closure::HEADER_SLOTS), cap_count)
    };
    let layout = func_capture_slot_types(func_id);
    let capture_types = if !layout.capture_slot_types.is_empty() {
        layout.capture_slot_types
    } else if layout.runtime_capture_slot_types.len() == cap_count {
        layout.runtime_capture_slot_types
    } else {
        panic!(
            "scan_closure_chunk: func_id={} has {} captures and no matching capture layout",
            func_id, cap_count
        );
    };
    scan_slot_types_chunk(gc, capture_slots, capture_types, cursor, slot_budget)
}

unsafe fn scan_array_chunk(
    gc: &mut Gc,
    obj: GcRef,
    context: GcScanContext<'_>,
    cursor: &mut GcTraceCursor,
    slot_budget: usize,
) -> GcObjectScanChunk {
    let elem_meta = unsafe { array::elem_meta(obj) };
    if !elem_meta.value_kind().may_contain_gc_refs() {
        return GcObjectScanChunk::complete(0);
    }
    let len = unsafe { array::len(obj) };
    let elem_bytes = unsafe { array::elem_bytes(obj) };
    assert!(
        elem_bytes.is_multiple_of(SLOT_BYTES),
        "scan_array_chunk: GC-containing element has non-slot-aligned elem_bytes={elem_bytes}"
    );
    let elem_slots = elem_bytes / SLOT_BYTES;
    if elem_slots == 0 {
        return GcObjectScanChunk::complete(0);
    }
    let total_slots = len
        .checked_mul(elem_slots)
        .expect("scan_array_chunk: element slot count overflow");
    let start = cursor.reference_index.min(total_slots);
    let end = start.saturating_add(slot_budget).min(total_slots);
    let base_off = byte_offset_for_slots(array::HEADER_SLOTS);
    for flat in start..end {
        let element_index = flat / elem_slots;
        let element_slot = flat % elem_slots;
        let elem_ptr =
            unsafe { (obj as *const u8).add(base_off + element_index * elem_bytes) as *const u64 };
        let slots = unsafe { core::slice::from_raw_parts(elem_ptr, elem_slots) };
        trace_flat_value_slot(slots, elem_meta, context, element_slot, &mut |child| {
            gc.mark_gray(child)
        })
        .unwrap_or_else(|err| panic!("scan_array_chunk: {err}"));
    }
    cursor.reference_index = end;
    let work_bytes = (end - start) * SLOT_BYTES;
    if end == total_slots {
        GcObjectScanChunk::complete(work_bytes)
    } else {
        GcObjectScanChunk::pending(work_bytes)
    }
}

unsafe fn scan_map_chunk(
    gc: &mut Gc,
    obj: GcRef,
    context: GcScanContext<'_>,
    cursor: &mut GcTraceCursor,
    slot_budget: usize,
) -> GcObjectScanChunk {
    let key_meta = unsafe { map::key_meta(obj) };
    let val_meta = unsafe { map::val_meta(obj) };
    let backing = unsafe { map::backing_ref(obj) };
    if backing.is_null() {
        return GcObjectScanChunk::complete(0);
    }

    let mut work_slots = 0usize;
    if cursor.auxiliary == 0 && work_slots < slot_budget {
        gc.mark_gray(backing);
        cursor.auxiliary = 1;
        work_slots += 1;
    }
    while work_slots < slot_budget {
        let mut empty = false;
        let mut total_slots = 0usize;
        let mut traced_reference = false;
        let bucket = unsafe {
            map::with_bucket_at(obj, cursor.element_index, |entry| {
                if let Some((key, val)) = entry {
                    total_slots = key.len().saturating_add(val.len());
                    if cursor.reference_index > 0 {
                        let reference_index = cursor.reference_index - 1;
                        if reference_index < key.len() {
                            trace_flat_value_slot(
                                key,
                                key_meta,
                                context,
                                reference_index,
                                &mut |child| gc.mark_gray(child),
                            )
                            .unwrap_or_else(|err| panic!("scan_map_chunk key: {err}"));
                        } else {
                            trace_flat_value_slot(
                                val,
                                val_meta,
                                context,
                                reference_index - key.len(),
                                &mut |child| gc.mark_gray(child),
                            )
                            .unwrap_or_else(|err| panic!("scan_map_chunk value: {err}"));
                        }
                        traced_reference = true;
                    }
                    true
                } else {
                    empty = true;
                    false
                }
            })
        };
        let Some(occupied) = bucket else {
            break;
        };
        if cursor.reference_index == 0 {
            cursor.reference_index = 1;
            work_slots += 1;
            if empty {
                cursor.element_index += 1;
                cursor.reference_index = 0;
                continue;
            }
            if !occupied || total_slots == 0 {
                cursor.element_index += 1;
                cursor.reference_index = 0;
                continue;
            }
            continue;
        }
        assert!(
            traced_reference,
            "scan_map_chunk must inspect one occupied key/value slot per work unit"
        );
        cursor.reference_index += 1;
        work_slots += 1;
        if cursor.reference_index - 1 == total_slots {
            cursor.element_index += 1;
            cursor.reference_index = 0;
        }
    }
    if unsafe { map::with_bucket_at(obj, cursor.element_index, |_| ()) }.is_none() {
        GcObjectScanChunk::complete(work_slots * SLOT_BYTES)
    } else {
        GcObjectScanChunk::pending(work_slots * SLOT_BYTES)
    }
}

unsafe fn scan_queue_chunk(
    gc: &mut Gc,
    obj: GcRef,
    context: GcScanContext<'_>,
    cursor: &mut GcTraceCursor,
    slot_budget: usize,
) -> GcObjectScanChunk {
    if unsafe { queue::is_remote(obj) } {
        return GcObjectScanChunk::complete(0);
    }
    let elem_meta = unsafe { queue_state::elem_meta(obj) };
    let elem_rttid = unsafe { queue_state::elem_rttid(obj) };
    let scan_meta = if elem_meta.value_kind() == ValueKind::Array && elem_meta.meta_id() == 0 {
        ValueMeta::new(elem_rttid.rttid(), ValueKind::Array)
    } else {
        elem_meta
    };
    let state = unsafe { queue::local_state_ref(obj) };
    let mut work_slots = 0usize;

    while work_slots < slot_budget {
        let message = match cursor.auxiliary {
            0 => state.buffer.get(cursor.element_index),
            1 => state
                .waiting_senders
                .get(cursor.element_index)
                .map(|(_, message)| message),
            _ => return GcObjectScanChunk::complete(work_slots * SLOT_BYTES),
        };
        let Some(message) = message else {
            if cursor.auxiliary == 0 {
                cursor.auxiliary = 1;
                cursor.element_index = 0;
                cursor.reference_index = 0;
                continue;
            }
            return GcObjectScanChunk::complete(work_slots * SLOT_BYTES);
        };
        if cursor.reference_index == 0 {
            if let Some(backing) = message.backing_ref() {
                gc.mark_gray(backing);
            }
            cursor.reference_index = 1;
            work_slots += 1;
            if work_slots >= slot_budget {
                continue;
            }
        }
        let payload_index = cursor.reference_index - 1;
        if payload_index >= message.len() {
            cursor.element_index += 1;
            cursor.reference_index = 0;
            continue;
        }
        if elem_meta.value_kind().may_contain_gc_refs() {
            trace_flat_value_slot(message, scan_meta, context, payload_index, &mut |child| {
                gc.mark_gray(child)
            })
            .unwrap_or_else(|err| panic!("scan_queue_chunk: {err}"));
        }
        cursor.reference_index += 1;
        work_slots += 1;
    }

    GcObjectScanChunk::pending(work_slots * SLOT_BYTES)
}

unsafe fn trace_array_children<V>(obj: GcRef, context: GcScanContext<'_>, visit: &mut V)
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
    assert!(
        elem_bytes.is_multiple_of(SLOT_BYTES),
        "scan_array: GC-containing element kind {elem_kind:?} has non-slot-aligned elem_bytes={elem_bytes}"
    );
    if elem_bytes == 0 {
        return;
    }
    let elem_slots = if elem_kind == ValueKind::Struct {
        let meta_id = elem_meta.meta_id() as usize;
        let slot_count = context
            .struct_metas
            .get(meta_id)
            .unwrap_or_else(|| panic!("scan_array: missing struct element metadata {meta_id}"))
            .slot_types
            .len();
        assert!(
            slot_count * SLOT_BYTES <= elem_bytes,
            "scan_array: struct element metadata has {} slots but elem_bytes={elem_bytes}",
            slot_count
        );
        slot_count
    } else {
        elem_bytes / SLOT_BYTES
    };
    let base_off = byte_offset_for_slots(array::HEADER_SLOTS);
    for idx in 0..len {
        let elem_ptr = unsafe { (obj as *const u8).add(base_off + idx * elem_bytes) as *const u64 };
        let slots = unsafe { core::slice::from_raw_parts(elem_ptr, elem_slots) };
        trace_value_slots_by_meta(slots, elem_meta, context, visit)
            .unwrap_or_else(|err| panic!("scan_array: {err}"));
    }
}

unsafe fn trace_queue_children<V>(obj: GcRef, context: GcScanContext<'_>, visit: &mut V)
where
    V: FnMut(GcRef),
{
    // REMOTE channels have no local state — elements live on the home island
    if queue::is_remote(obj) {
        return;
    }

    let elem_meta = queue_state::elem_meta(obj);
    let state = unsafe { queue::local_state(obj) };
    for message in state.buffer.iter() {
        if let Some(backing) = message.backing_ref() {
            visit(backing);
        }
        if elem_meta.value_kind().may_contain_gc_refs() {
            trace_queue_elem(
                message,
                elem_meta,
                queue_state::elem_rttid(obj),
                context,
                visit,
            );
        }
    }
    for (_, message) in state.waiting_senders.iter() {
        if let Some(backing) = message.backing_ref() {
            visit(backing);
        }
        if elem_meta.value_kind().may_contain_gc_refs() {
            trace_queue_elem(
                message,
                elem_meta,
                queue_state::elem_rttid(obj),
                context,
                visit,
            );
        }
    }
}

fn trace_queue_elem<V>(
    slots: &[u64],
    meta: ValueMeta,
    rttid: ValueRttid,
    context: GcScanContext<'_>,
    visit: &mut V,
) where
    V: FnMut(GcRef),
{
    let scan_meta = if meta.value_kind() == ValueKind::Array && meta.meta_id() == 0 {
        ValueMeta::new(rttid.rttid(), ValueKind::Array)
    } else {
        meta
    };
    trace_value_slots_by_meta(slots, scan_meta, context, visit)
        .unwrap_or_else(|err| panic!("scan_queue: {err}"));
}

fn trace_value_slots_by_meta<V>(
    slots: &[u64],
    meta: ValueMeta,
    context: GcScanContext<'_>,
    visit: &mut V,
) -> Result<(), TypedWriteBarrierByMetaError>
where
    V: FnMut(GcRef),
{
    match meta.value_kind() {
        ValueKind::Struct => {
            let meta_id = meta.meta_id() as usize;
            let slot_types = &context
                .struct_metas
                .get(meta_id)
                .ok_or(TypedWriteBarrierByMetaError::MissingStructMeta { meta_id })?
                .slot_types;
            trace_slots_by_types(slots, slot_types, visit);
        }
        ValueKind::Array => {
            trace_array_value_slots(slots, meta, context, visit)?;
        }
        ValueKind::Interface => {
            if slots.len() >= 2 && interface::data_is_gc_ref(slots[0]) && slots[1] != 0 {
                visit(slots[1] as GcRef);
            }
        }
        ValueKind::String
        | ValueKind::Slice
        | ValueKind::Map
        | ValueKind::Channel
        | ValueKind::Closure
        | ValueKind::Pointer
        | ValueKind::Port
        | ValueKind::Island => {
            if let Some(&slot) = slots.first() {
                if slot != 0 {
                    visit(slot as GcRef);
                }
            }
        }
        _ => {}
    }
    Ok(())
}

fn trace_array_value_slots<V>(
    slots: &[u64],
    meta: ValueMeta,
    context: GcScanContext<'_>,
    visit: &mut V,
) -> Result<(), TypedWriteBarrierByMetaError>
where
    V: FnMut(GcRef),
{
    // Runtime-only GC tests can call the scanner without a Module. VM paths
    // always provide the module-backed layout.
    if !context.has_runtime_types() {
        return Ok(());
    }
    for flat_index in 0..slots.len() {
        trace_flat_value_slot(slots, meta, context, flat_index, visit)?;
    }
    Ok(())
}

unsafe fn trace_map_children<V>(obj: GcRef, context: GcScanContext<'_>, visit: &mut V)
where
    V: FnMut(GcRef),
{
    let backing = map::backing_ref(obj);
    if !backing.is_null() {
        visit(backing);
    }
    let key_meta = map::key_meta(obj);
    let val_meta = map::val_meta(obj);
    let key_kind = key_meta.value_kind();
    let val_kind = val_meta.value_kind();

    if !key_kind.may_contain_gc_refs() && !val_kind.may_contain_gc_refs() {
        return;
    }

    let mut iter = map::iter_init(obj);
    while map::with_next(&mut iter, |entry| {
        let Some((key, val)) = entry else {
            return false;
        };
        if key_kind.may_contain_gc_refs() {
            trace_value_slots_by_meta(key, key_meta, context, visit)
                .unwrap_or_else(|err| panic!("scan_map key: {err}"));
        }
        if val_kind.may_contain_gc_refs() {
            trace_value_slots_by_meta(val, val_meta, context, visit)
                .unwrap_or_else(|err| panic!("scan_map value: {err}"));
        }
        true
    }) {}
}

unsafe fn trace_struct_children<V>(
    obj: GcRef,
    meta_id: usize,
    struct_metas: &[StructMeta],
    visit: &mut V,
) where
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
pub unsafe fn finalize_object(obj: GcRef) {
    let header = unsafe { Gc::header(obj) };
    if header.is_value_slots_object() {
        return;
    }
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
    use crate::test_support::{queue, scan_object, trace_object_children_with_context};

    fn section<'a>(src: &'a str, start: &str, end: &str) -> &'a str {
        let start = src.find(start).expect("section start");
        let end = src[start..]
            .find(end)
            .map(|offset| start + offset)
            .expect("section end");
        &src[start..end]
    }

    #[test]
    fn gc_layout_metadata_drift_has_no_release_skip_or_all_gcref_substitute() {
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
            "closure capture layout drift must not use all-GcRef substitute"
        );

        let scan_array = section(src, "fn trace_array_children", "fn trace_queue_children");
        assert!(
            !scan_array.contains("No struct_meta available"),
            "array struct-element metadata drift must fail fast instead of skipping scans"
        );

        let elem_scan = section(
            src,
            "fn trace_value_slots_by_meta",
            "fn trace_array_value_slots",
        );
        assert!(
            !elem_scan.contains("ElemScan::Skip"),
            "container element scan resolution must not use missing-metadata skip"
        );
        assert!(
            elem_scan.contains("trace_array_value_slots"),
            "container array values must use recursive array layout instead of all-GcRef scanning"
        );

        let scan_struct = section(src, "fn trace_struct_children", "/// Finalize");
        assert!(
            !scan_struct.contains("return;"),
            "struct metadata drift must fail fast instead of returning without scanning"
        );
    }

    #[test]
    fn gc_scan_queue_array_value_elements_as_precise_values() {
        let mut gc = Gc::new();
        let ch = queue::create(
            &mut gc,
            queue_state::QueueKind::Chan,
            ValueMeta::new(0, ValueKind::Array),
            vo_common_core::types::ValueRttid::new(0, ValueKind::Array),
            2,
            1,
        );

        match queue::try_send(
            ch,
            vec![0x1111_2222_3333_4444, 0x5555_6666_7777_8888].into_boxed_slice(),
        ) {
            queue::SendResult::Buffered => {}
            other => panic!("expected buffered array value send, got {other:?}"),
        }

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            scan_object(&mut gc, ch, &[], &|_| ClosureScanLayout::default());
        }));

        assert!(
            result.is_ok(),
            "queue element scan must not treat array value slots as GcRefs"
        );
    }

    #[test]
    fn gc_queue_managed_payload_keeps_runtime_backing_visible() {
        let mut gc = Gc::new();
        let ch = queue::create(
            &mut gc,
            queue_state::QueueKind::Chan,
            ValueMeta::new(0, ValueKind::Uint64),
            ValueRttid::new(0, ValueKind::Uint64),
            1,
            1,
        );
        let message = queue_state::QueueMessage::managed(&mut gc, &[41])
            .expect("managed queue payload allocation");
        let backing = message.backing_ref().expect("managed payload backing");
        match queue::try_send(ch, message) {
            queue::SendResult::Buffered => {}
            other => panic!("expected buffered managed payload, got {other:?}"),
        }

        let mut visited = Vec::new();
        trace_object_children_with_context(
            ch,
            GcScanContext::from_module_parts(&[], &[], &[]),
            &|_| ClosureScanLayout::default(),
            |child| visited.push(child),
        );

        assert!(visited.contains(&backing));
        assert!(gc.memory_stats().runtime_backing_bytes > 0);
    }

    #[test]
    fn gc_queue_managed_payload_scan_is_resumable_per_work_unit() {
        let mut gc = Gc::new();
        let ch = queue::create(
            &mut gc,
            queue_state::QueueKind::Chan,
            ValueMeta::new(0, ValueKind::Uint64),
            ValueRttid::new(0, ValueKind::Uint64),
            1,
            1,
        );
        let message = queue_state::QueueMessage::managed(&mut gc, &[1, 2, 3])
            .expect("managed queue payload allocation");
        let backing = message.backing_ref().expect("managed payload backing");
        match queue::try_send(ch, message) {
            queue::SendResult::Buffered => {}
            other => panic!("expected buffered managed payload, got {other:?}"),
        }

        let mut cursor = GcTraceCursor::default();
        let mut calls = 0usize;
        loop {
            let chunk = unsafe {
                scan_object_chunk_with_context(
                    &mut gc,
                    ch,
                    GcScanContext::from_module_parts(&[], &[], &[]),
                    &|_| ClosureScanLayout::default(),
                    &mut cursor,
                    SLOT_BYTES,
                )
            };
            assert!(chunk.work_bytes <= SLOT_BYTES);
            calls += 1;
            if chunk.done {
                break;
            }
            assert!(calls < 16, "bounded queue scan did not converge");
        }

        assert!(calls >= 4);
        assert!(unsafe { Gc::header(backing) }.is_gray());
    }

    #[test]
    fn vm_gc_queue_payload_root_003_buffered_array_payload_roots_survive_scan() {
        let mut gc = Gc::new();
        let left = string::create(&mut gc, b"left");
        let right = string::create(&mut gc, b"right");
        let ch = queue::create(
            &mut gc,
            queue_state::QueueKind::Chan,
            ValueMeta::new(1, ValueKind::Array),
            ValueRttid::new(1, ValueKind::Array),
            2,
            1,
        );
        match queue::try_send(ch, vec![left as u64, right as u64].into_boxed_slice()) {
            queue::SendResult::Buffered => {}
            other => panic!("expected buffered array value send, got {other:?}"),
        }

        let runtime_types = vec![
            RuntimeType::Basic(ValueKind::String),
            RuntimeType::Array {
                len: 2,
                elem: ValueRttid::new(0, ValueKind::String),
            },
        ];
        let mut visited = Vec::new();
        trace_object_children_with_context(
            ch,
            GcScanContext::from_module_parts(&[], &[], &runtime_types),
            &|_| ClosureScanLayout::default(),
            |child| visited.push(child),
        );

        assert!(
            visited.contains(&left) && visited.contains(&right),
            "buffered queue array payload must keep every nested root visible"
        );
    }

    #[test]
    fn vm_gc_queue_payload_root_003_waiting_sender_array_payload_roots_survive_scan() {
        let mut gc = Gc::new();
        let left = string::create(&mut gc, b"left");
        let right = string::create(&mut gc, b"right");
        let ch = queue::create(
            &mut gc,
            queue_state::QueueKind::Chan,
            ValueMeta::new(1, ValueKind::Array),
            ValueRttid::new(1, ValueKind::Array),
            2,
            0,
        );
        let waiter = queue_state::QueueWaiter::simple_queue(
            4,
            9,
            ch as u64,
            queue_state::SelectWaitKind::Send,
        );
        match queue::send_or_block(
            ch,
            vec![left as u64, right as u64].into_boxed_slice(),
            waiter,
        ) {
            queue::BlockingSendResult::Blocked => {}
            other => panic!("expected waiting sender array value, got {other:?}"),
        }

        let runtime_types = vec![
            RuntimeType::Basic(ValueKind::String),
            RuntimeType::Array {
                len: 2,
                elem: ValueRttid::new(0, ValueKind::String),
            },
        ];
        let mut visited = Vec::new();
        trace_object_children_with_context(
            ch,
            GcScanContext::from_module_parts(&[], &[], &runtime_types),
            &|_| ClosureScanLayout::default(),
            |child| visited.push(child),
        );

        assert!(
            visited.contains(&left) && visited.contains(&right),
            "waiting sender queue array payload must keep every nested root visible"
        );
    }

    #[test]
    fn vm_gc_map_string_key_value_slots_are_traceable_roots_060() {
        let mut gc = Gc::new();
        let key = string::create(&mut gc, b"key");
        let value = string::create(&mut gc, b"value");
        let map_ref = map::create(
            &mut gc,
            ValueMeta::new(0, ValueKind::String),
            ValueMeta::new(0, ValueKind::String),
            1,
            1,
            0,
        );
        unsafe {
            map::set_checked(&mut gc, map_ref, &[key as u64], &[value as u64], None)
                .expect("string map entry should be hashable");
        }

        let mut visited = Vec::new();
        trace_object_children_with_context(
            map_ref,
            GcScanContext::from_module_parts(&[], &[], &[]),
            &|_| ClosureScanLayout::default(),
            |child| visited.push(child),
        );

        assert!(ValueKind::String.may_contain_gc_refs());
        assert!(visited.contains(&key), "string map key must be traced");
        assert!(visited.contains(&value), "string map value must be traced");
    }

    #[test]
    fn gc_map_backing_and_empty_buckets_scan_one_work_unit_at_a_time() {
        let mut gc = Gc::new();
        let key = string::create(&mut gc, b"bounded-key");
        let value = string::create(&mut gc, b"bounded-value");
        let map_ref = map::create(
            &mut gc,
            ValueMeta::new(0, ValueKind::String),
            ValueMeta::new(0, ValueKind::String),
            1,
            1,
            0,
        );
        unsafe {
            map::set_checked(&mut gc, map_ref, &[key as u64], &[value as u64], None)
                .expect("string map entry");
        }
        let backing = unsafe { map::backing_ref(map_ref) };
        let mut cursor = GcTraceCursor::default();
        let mut calls = 0usize;
        loop {
            let chunk = unsafe {
                scan_object_chunk_with_context(
                    &mut gc,
                    map_ref,
                    GcScanContext::from_module_parts(&[], &[], &[]),
                    &|_| ClosureScanLayout::default(),
                    &mut cursor,
                    SLOT_BYTES,
                )
            };
            assert!(chunk.work_bytes <= SLOT_BYTES);
            calls += 1;
            if chunk.done {
                break;
            }
            assert!(calls < 64, "bounded map scan did not converge");
        }

        assert!(calls > 8, "empty buckets must remain visible work units");
        assert!(unsafe { Gc::header(backing) }.is_gray());
        assert!(unsafe { Gc::header(key) }.is_gray());
        assert!(unsafe { Gc::header(value) }.is_gray());
    }

    #[test]
    fn vm_island_spawn_capture_root_005_array_value_slot_box_scans_all_slots() {
        let mut gc = Gc::new();
        let left = string::create(&mut gc, b"left");
        let right = string::create(&mut gc, b"right");
        let capture_box = gc.alloc_value_slots(ValueMeta::new(1, ValueKind::Array), 2);
        unsafe {
            Gc::write_slot(capture_box, 0, left as u64);
            Gc::write_slot(capture_box, 1, right as u64);
        }

        let runtime_types = vec![
            RuntimeType::Basic(ValueKind::String),
            RuntimeType::Array {
                len: 2,
                elem: ValueRttid::new(0, ValueKind::String),
            },
        ];
        let mut visited = Vec::new();
        trace_object_children_with_context(
            capture_box,
            GcScanContext::from_module_parts(&[], &[], &runtime_types),
            &|_| ClosureScanLayout::default(),
            |child| visited.push(child),
        );

        assert!(
            visited.contains(&left) && visited.contains(&right),
            "spawn capture value-slot boxes must scan every array element root"
        );
    }

    #[test]
    fn gc_array_value_scan_handles_thousands_of_nested_array_types() {
        const DEPTH: usize = 2_048;
        let mut gc = Gc::new();
        let leaf = string::create(&mut gc, b"deep-root");
        let mut runtime_types = Vec::with_capacity(DEPTH + 1);
        for index in 0..DEPTH {
            let elem = if index + 1 == DEPTH {
                ValueRttid::new(DEPTH as u32, ValueKind::String)
            } else {
                ValueRttid::new((index + 1) as u32, ValueKind::Array)
            };
            runtime_types.push(RuntimeType::Array { len: 1, elem });
        }
        runtime_types.push(RuntimeType::Basic(ValueKind::String));

        let mut visited = Vec::new();
        trace_value_slots_by_meta(
            &[leaf as u64],
            ValueMeta::new(0, ValueKind::Array),
            GcScanContext::from_module_parts(&[], &[], &runtime_types),
            &mut |child| visited.push(child),
        )
        .expect("deep nested Array metadata must scan through an explicit work stack");

        assert_eq!(visited, [leaf]);
    }

    #[test]
    fn gc_scan_zero_byte_struct_array_elements_do_not_use_synthetic_slots() {
        let mut gc = Gc::new();
        let arr = array::create(&mut gc, ValueMeta::new(0, ValueKind::Struct), 0, 5);
        let struct_metas = vec![StructMeta {
            slot_types: vec![SlotType::Value],
            fields: Vec::new(),
            field_index: std::collections::HashMap::new(),
        }];

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            scan_object(&mut gc, arr, &struct_metas, &|_| {
                ClosureScanLayout::default()
            });
        }));

        assert!(
            result.is_ok(),
            "zero-byte struct array elements should not scan synthetic value slots"
        );
    }

    #[test]
    fn gc_scan_struct_array_elements_uses_metadata_width_not_physical_stride() {
        let mut gc = Gc::new();
        let child = gc.alloc(ValueMeta::new(0, ValueKind::String), 1);
        let arr = array::create(&mut gc, ValueMeta::new(0, ValueKind::Struct), 24, 1);
        unsafe { array::set(arr, 0, child as u64, 8) };
        let struct_metas = vec![StructMeta {
            slot_types: vec![SlotType::GcRef],
            fields: Vec::new(),
            field_index: std::collections::HashMap::new(),
        }];

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            scan_object(&mut gc, arr, &struct_metas, &|_| {
                ClosureScanLayout::default()
            });
        }));

        assert!(
            result.is_ok(),
            "struct array scanning should use StructMeta slot width with elem_bytes as stride"
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

    #[test]
    fn typed_write_barrier_by_meta_rejects_ref_and_interface_width_drift_052() {
        let mut gc = Gc::new();
        let parent = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);

        assert_eq!(
            try_typed_write_barrier_by_meta(
                &mut gc,
                parent,
                &[],
                ValueMeta::new(0, ValueKind::String),
                None,
            ),
            Err(TypedWriteBarrierByMetaError::SlotWidthMismatch {
                vals: 0,
                slot_types: 1,
            }),
        );
        assert_eq!(
            try_typed_write_barrier_by_meta(
                &mut gc,
                parent,
                &[0],
                ValueMeta::new(0, ValueKind::Map),
                None,
            ),
            Ok(()),
        );
        assert_eq!(
            try_typed_write_barrier_by_meta(
                &mut gc,
                parent,
                &[0],
                ValueMeta::new(0, ValueKind::Interface),
                None,
            ),
            Err(TypedWriteBarrierByMetaError::SlotWidthMismatch {
                vals: 1,
                slot_types: 2,
            }),
        );
    }
}
