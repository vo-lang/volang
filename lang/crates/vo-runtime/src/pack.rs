//! Pack/Unpack for cross-island value transfer.
//!
//! All sendable values are deep-copied when crossing island boundaries.
//! Pack converts values to an island-independent representation.
//! Unpack reconstructs values in the destination island's heap.
//!
//! # Safety contract
//! Unsafe transfer helpers require rooted live objects matching the supplied
//! canonical metadata until serialization or reconstruction completes.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, vec, vec::Vec};

use hashbrown::HashMap;

use crate::gc::{Gc, GcRef};
use crate::objects::queue_state::QueueKind;
use crate::objects::{array, map, queue, slice, string};
use crate::slot::SLOT_BYTES;
use vo_common_core::bytecode::{Module, NamedTypeMeta, RuntimeTypeResolver, StructMeta};
use vo_common_core::types::{ValueKind, ValueMeta, ValueRttid};
use vo_common_core::RuntimeType;

/// Packed representation of a sendable value.
/// Contains serialized bytes that can be transferred across islands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PackOutputError {
    LengthOverflow { limit: usize, attempted: usize },
    AllocationFailed { requested: usize },
}

impl core::fmt::Display for PackOutputError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::LengthOverflow { limit, attempted } => write!(
                f,
                "packed output length {attempted} exceeds the limit of {limit} bytes"
            ),
            Self::AllocationFailed { requested } => {
                write!(f, "packed output allocation for {requested} bytes failed")
            }
        }
    }
}

impl core::error::Error for PackOutputError {}

#[derive(Debug, Clone)]
pub struct PackedValue {
    /// Serialized data
    data: Vec<u8>,
    output_limit: usize,
    output_error: Option<PackOutputError>,
}

impl PackedValue {
    pub fn new() -> Self {
        Self::with_output_limit(usize::MAX)
    }

    pub fn from_data(data: Vec<u8>) -> Self {
        Self {
            data,
            output_limit: usize::MAX,
            output_error: None,
        }
    }

    pub fn data(&self) -> &[u8] {
        &self.data
    }

    pub fn into_data(self) -> Vec<u8> {
        self.data
    }

    fn with_output_limit(output_limit: usize) -> Self {
        Self {
            data: Vec::new(),
            output_limit,
            output_error: None,
        }
    }

    fn push_encoded(&mut self, value: u8) {
        self.extend_encoded(&[value]);
    }

    fn extend_encoded(&mut self, bytes: &[u8]) {
        if self.output_error.is_some() {
            return;
        }
        let Some(requested) = self.data.len().checked_add(bytes.len()) else {
            self.output_error = Some(PackOutputError::LengthOverflow {
                limit: self.output_limit,
                attempted: usize::MAX,
            });
            return;
        };
        if requested > self.output_limit {
            self.output_error = Some(PackOutputError::LengthOverflow {
                limit: self.output_limit,
                attempted: requested,
            });
            return;
        }
        if self.data.capacity().saturating_sub(self.data.len()) < bytes.len() {
            let geometric = self.data.capacity().saturating_mul(2).max(8);
            let target_capacity = geometric.max(requested).min(self.output_limit);
            let additional = target_capacity.saturating_sub(self.data.len());
            if self.data.try_reserve_exact(additional).is_err() {
                self.output_error = Some(PackOutputError::AllocationFailed { requested });
                return;
            }
        }
        self.data.extend_from_slice(bytes);
    }

    fn finish_output(mut self) -> Result<Self, PackOutputError> {
        if let Some(error) = self.output_error.take() {
            return Err(error);
        }
        self.output_limit = usize::MAX;
        Ok(self)
    }
}

impl PartialEq for PackedValue {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl Eq for PackedValue {}

impl Default for PackedValue {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct CachedQueueHandle {
    handle: QueueHandleInfo,
    chan_ref: GcRef,
}

pub(crate) type UnpackQueueHandleCache = HashMap<u64, CachedQueueHandle>;

#[derive(Default)]
pub(crate) struct PackObjectGraph {
    allocation_ids: HashMap<usize, u64>,
    slice_backing_ids: HashMap<SliceBackingKey, u64>,
    map_ids: HashMap<usize, u64>,
    next_id: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SliceBackingKey {
    owner: usize,
    data: usize,
    len: usize,
    elem_meta: u32,
    elem_bytes: usize,
    storage_stride: usize,
    flat_storage: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PackedPointerRef {
    Null,
    Definition { id: u64, offset: usize },
    BackReference { id: u64, offset: usize },
}

#[derive(Debug, Clone, Copy)]
enum UnpackedAllocationKind {
    Struct,
    Array {
        elem_meta: ValueMeta,
        elem_bytes: usize,
    },
}

#[derive(Debug, Clone, Copy)]
struct UnpackedAllocation {
    base: GcRef,
    data_bytes: usize,
    kind: UnpackedAllocationKind,
}

#[derive(Default)]
pub(crate) struct UnpackObjectCache {
    allocations: HashMap<u64, UnpackedAllocation>,
    maps: HashMap<u64, GcRef>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValidatedAllocationKind {
    Struct {
        meta: ValueMeta,
        slots: usize,
    },
    Array {
        elem_meta: ValueMeta,
        elem_bytes: usize,
        len: usize,
    },
}

#[derive(Debug, Clone, Copy)]
struct ValidatedAllocation {
    data_bytes: usize,
    kind: ValidatedAllocationKind,
}

#[derive(Clone, Default)]
pub(crate) struct ValidateObjectCache {
    allocations: HashMap<u64, ValidatedAllocation>,
    maps: HashMap<u64, (ValueMeta, ValueMeta, usize, usize)>,
}

type UnpackPointerCache = UnpackObjectCache;
type ValidatePointerCache = ValidateObjectCache;

const SEQUENCE_ENCODING_ELEMENTS: u8 = 0;
const SEQUENCE_ENCODING_RAW_BYTES: u8 = 1;
const ARRAY_VALUE_INLINE_MARKER: u8 = 2;
const ALLOCATION_KIND_STRUCT: u8 = 0;
const ALLOCATION_KIND_ARRAY: u8 = 1;
const SLICE_BACKING_DEFINITION: u8 = 1;
const SLICE_BACKING_BACK_REFERENCE: u8 = 2;
const SLICE_OWNER_DEFINITION: u8 = 3;
const SLICE_OWNER_BACK_REFERENCE: u8 = 4;

#[derive(Debug, Clone, Copy)]
struct PackedSequenceSpec {
    elem_meta: ValueMeta,
    elem_rttid: Option<ValueRttid>,
    elem_bytes: usize,
    length: usize,
}

#[derive(Clone, Copy)]
pub(crate) struct PackTypeContext<'a> {
    pub(crate) struct_metas: &'a [StructMeta],
    pub(crate) named_type_metas: &'a [NamedTypeMeta],
    pub(crate) runtime_types: &'a [RuntimeType],
}

impl<'a> PackTypeContext<'a> {
    pub(crate) const fn new(
        struct_metas: &'a [StructMeta],
        runtime_types: &'a [RuntimeType],
    ) -> Self {
        Self {
            struct_metas,
            named_type_metas: &[],
            runtime_types,
        }
    }

    pub(crate) const fn with_named_types(
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

    fn resolver(self) -> RuntimeTypeResolver<'a> {
        RuntimeTypeResolver::new(self.struct_metas, self.named_type_metas, self.runtime_types)
    }

    fn resolve_runtime_type(self, rttid: ValueRttid) -> Option<&'a RuntimeType> {
        self.resolver()
            .resolve_value_rttid(rttid)
            .map(|(_, runtime_type)| runtime_type)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct QueueHandleInfo {
    pub kind: QueueKind,
    pub endpoint_id: u64,
    pub home_island: u32,
    pub cap: u64,
    pub elem_meta: ValueMeta,
    pub elem_rttid: ValueRttid,
    pub elem_slots: u16,
    pub closed: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PackedLayoutError;

impl core::fmt::Display for PackedLayoutError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("packed value does not match the required canonical layout")
    }
}

impl core::error::Error for PackedLayoutError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SequenceElementLayout {
    pub logical_slots: usize,
    pub physical_bytes: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SequenceLayoutError {
    pub expected_bytes: usize,
    pub actual_bytes: usize,
}

impl core::fmt::Display for SequenceLayoutError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "sequence element uses {} physical bytes; canonical metadata requires {}",
            self.actual_bytes, self.expected_bytes
        )
    }
}

impl core::error::Error for SequenceLayoutError {}

pub fn sequence_element_layout(
    elem_meta: ValueMeta,
    elem_bytes: usize,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<SequenceElementLayout, SequenceLayoutError> {
    let mut layout_cache = RuntimeLayoutCache::default();
    sequence_elem_layout_checked(
        elem_meta,
        elem_bytes,
        PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types),
        &mut layout_cache,
    )
}

/// Fallibly serializes a rooted value without an implementation output cap.
///
/// # Safety
///
/// `src` must contain the complete slot representation described by
/// `value_meta`; every embedded `GcRef` must be live, owned by `gc`, and remain
/// rooted for the duration of the call. The metadata tables must be canonical
/// for those objects.
pub unsafe fn try_pack_slots(
    gc: &Gc,
    src: &[u64],
    value_meta: ValueMeta,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> Result<PackedValue, PackOutputError> {
    try_pack_slots_with_context(
        gc,
        src,
        value_meta,
        PackTypeContext::new(struct_metas, runtime_types),
    )
}

/// Fallibly serializes a rooted value using canonical named-type metadata.
///
/// # Safety
///
/// `src` must contain the complete slot representation described by
/// `value_meta`; every embedded `GcRef` must be live, owned by `gc`, and remain
/// rooted for the duration of the call. All metadata tables must be canonical
/// for those objects.
pub unsafe fn try_pack_slots_with_named_type_metas(
    gc: &Gc,
    src: &[u64],
    value_meta: ValueMeta,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<PackedValue, PackOutputError> {
    try_pack_slots_with_context(
        gc,
        src,
        value_meta,
        PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types),
    )
}

unsafe fn try_pack_slots_with_context(
    gc: &Gc,
    src: &[u64],
    value_meta: ValueMeta,
    context: PackTypeContext<'_>,
) -> Result<PackedValue, PackOutputError> {
    let mut object_graph = PackObjectGraph::default();
    let mut packed = PackedValue::with_output_limit(usize::MAX);
    pack_value(&mut packed, gc, src, value_meta, context, &mut object_graph);
    packed.finish_output()
}

#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn pack_slots_with_named_type_metas_and_cache_limited(
    gc: &Gc,
    src: &[u64],
    value_meta: ValueMeta,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
    object_graph: &mut PackObjectGraph,
    max_output_len: usize,
) -> Result<PackedValue, PackOutputError> {
    let mut packed = PackedValue::with_output_limit(max_output_len);
    pack_value(
        &mut packed,
        gc,
        src,
        value_meta,
        PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types),
        object_graph,
    );
    packed.finish_output()
}

/// Unpack a PackedValue into slots.
///
/// # Arguments
/// - `gc`: Destination GC (for allocating heap objects)
/// - `packed`: The packed value to unpack
/// - `dst`: Destination slots
/// - `struct_metas`: Struct metadata for recursive unpacking
/// - `runtime_types`: Runtime type info for looking up nested struct meta_ids
#[cfg(test)]
pub(crate) unsafe fn unpack_slots(
    gc: &mut Gc,
    packed: &PackedValue,
    dst: &mut [u64],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) {
    unpack_slots_with_queue_handle_resolver(
        gc,
        packed,
        dst,
        struct_metas,
        runtime_types,
        default_unpack_queue_handle,
    );
}

#[cfg(test)]
pub(crate) unsafe fn unpack_slots_with_named_type_metas(
    gc: &mut Gc,
    packed: &PackedValue,
    dst: &mut [u64],
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) {
    unpack_slots_with_queue_handle_resolver_and_named_type_metas(
        gc,
        packed,
        dst,
        struct_metas,
        named_type_metas,
        runtime_types,
        default_unpack_queue_handle,
    );
}

#[cfg(test)]
pub(crate) unsafe fn unpack_slots_with_queue_handle_resolver<F>(
    gc: &mut Gc,
    packed: &PackedValue,
    dst: &mut [u64],
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    mut resolve_queue_handle: F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let mut cursor = 0;
    let mut queue_handle_cache = UnpackQueueHandleCache::default();
    unpack_slots_with_queue_handle_resolver_and_cache(
        gc,
        &packed.data,
        &mut cursor,
        dst,
        PackTypeContext::new(struct_metas, runtime_types),
        &mut queue_handle_cache,
        &mut resolve_queue_handle,
    );
}

#[cfg(test)]
pub(crate) unsafe fn unpack_slots_with_queue_handle_resolver_and_named_type_metas<F>(
    gc: &mut Gc,
    packed: &PackedValue,
    dst: &mut [u64],
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
    mut resolve_queue_handle: F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let mut cursor = 0;
    let mut queue_handle_cache = UnpackQueueHandleCache::default();
    unpack_slots_with_queue_handle_resolver_and_cache(
        gc,
        &packed.data,
        &mut cursor,
        dst,
        PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types),
        &mut queue_handle_cache,
        &mut resolve_queue_handle,
    );
}

#[allow(clippy::too_many_arguments)]
#[cfg(test)]
pub(crate) unsafe fn unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas<F>(
    gc: &mut Gc,
    packed: &PackedValue,
    dst: &mut [u64],
    expected_meta: ValueMeta,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
    mut resolve_queue_handle: F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let mut cursor = 0;
    let mut queue_handle_cache = UnpackQueueHandleCache::default();
    unpack_slots_expected_with_queue_handle_resolver_and_cache(
        gc,
        &packed.data,
        &mut cursor,
        dst,
        expected_meta,
        PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types),
        &mut queue_handle_cache,
        &mut resolve_queue_handle,
    );
}

pub fn validate_packed_slots_expected_with_named_type_metas(
    data: &[u8],
    expected_meta: ValueMeta,
    expected_rttid: ValueRttid,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<(), PackedLayoutError> {
    let mut pointer_cache = ValidatePointerCache::default();
    validate_packed_slots_expected_with_named_type_metas_and_cache(
        data,
        expected_meta,
        expected_rttid,
        struct_metas,
        named_type_metas,
        runtime_types,
        &mut pointer_cache,
    )
}

/// Validates a packed value and reconstructs it using the runtime's default
/// queue-handle resolver.
///
/// The destination must provide at least the canonical slot width of
/// `expected_rttid`. Malformed payloads, metadata drift, and short destination
/// buffers are rejected before `gc` or `dst` is mutated.
#[allow(clippy::too_many_arguments)]
pub fn validate_and_unpack_slots_expected_with_named_type_metas(
    gc: &mut Gc,
    data: &[u8],
    dst: &mut [u64],
    expected_meta: ValueMeta,
    expected_rttid: ValueRttid,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<(), PackedLayoutError> {
    // Safety: the default resolver creates queue handles in `gc`; all wire and
    // destination layout invariants are established inside the callee.
    unsafe {
        validate_and_unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas(
            gc,
            data,
            dst,
            expected_meta,
            expected_rttid,
            struct_metas,
            named_type_metas,
            runtime_types,
            default_unpack_queue_handle,
        )
    }
}

/// Validates a packed value and reconstructs it with a caller-supplied queue
/// handle resolver, keeping validation and unchecked decoding in one API.
///
/// Malformed payloads, metadata drift, and short destination buffers are
/// rejected before `gc`, `dst`, or the resolver is touched.
///
/// # Safety
///
/// Every non-null `GcRef` returned by `resolve_queue_handle` must identify a
/// live queue object owned by `gc` with the exact contract described by the
/// supplied [`QueueHandleInfo`]. It must remain live through reconstruction.
#[allow(clippy::too_many_arguments)]
pub unsafe fn validate_and_unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas<
    F,
>(
    gc: &mut Gc,
    data: &[u8],
    dst: &mut [u64],
    expected_meta: ValueMeta,
    expected_rttid: ValueRttid,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
    mut resolve_queue_handle: F,
) -> Result<(), PackedLayoutError>
where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    validate_packed_slots_expected_with_named_type_metas(
        data,
        expected_meta,
        expected_rttid,
        struct_metas,
        named_type_metas,
        runtime_types,
    )?;
    let context = PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types);
    let expected_slots = context
        .resolver()
        .slot_count_for_value_rttid(expected_rttid)
        .ok_or(PackedLayoutError)?;
    if dst.len() < expected_slots {
        return Err(PackedLayoutError);
    }

    let mut cursor = 0;
    let mut queue_handle_cache = UnpackQueueHandleCache::default();
    let mut pointer_cache = UnpackPointerCache::default();
    unpack_slots_expected_with_queue_handle_resolver_and_object_cache(
        gc,
        data,
        &mut cursor,
        dst,
        expected_meta,
        context,
        &mut queue_handle_cache,
        &mut pointer_cache,
        &mut resolve_queue_handle,
    );
    debug_assert_eq!(cursor, data.len());
    Ok(())
}

pub(crate) fn validate_packed_slots_expected_with_named_type_metas_and_cache(
    data: &[u8],
    expected_meta: ValueMeta,
    expected_rttid: ValueRttid,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
    pointer_cache: &mut ValidatePointerCache,
) -> Result<(), PackedLayoutError> {
    let context = PackTypeContext::with_named_types(struct_metas, named_type_metas, runtime_types);
    let mut cursor = 0;
    // A definition must be visible while its recursive payload is validated so
    // cycles can resolve back-references. Stage those registrations and publish
    // them only after the complete chunk, including its extent, is valid.
    let mut staged_cache = pointer_cache.clone();
    validate_packed_value(
        data,
        &mut cursor,
        expected_meta,
        Some(expected_rttid),
        context,
        &mut staged_cache,
    )?;
    if cursor == data.len() {
        *pointer_cache = staged_cache;
        Ok(())
    } else {
        Err(PackedLayoutError)
    }
}

#[allow(clippy::too_many_arguments)]
#[cfg(test)]
pub(crate) unsafe fn unpack_slots_with_queue_handle_resolver_and_cache<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let mut pointer_cache = UnpackPointerCache::default();
    unpack_value(
        gc,
        data,
        cursor,
        dst,
        None,
        context,
        queue_handle_cache,
        &mut pointer_cache,
        resolve_queue_handle,
    );
}

#[allow(clippy::too_many_arguments)]
#[cfg(test)]
pub(crate) unsafe fn unpack_slots_expected_with_queue_handle_resolver_and_cache<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    expected_meta: ValueMeta,
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let mut pointer_cache = UnpackPointerCache::default();
    unpack_slots_expected_with_queue_handle_resolver_and_object_cache(
        gc,
        data,
        cursor,
        dst,
        expected_meta,
        context,
        queue_handle_cache,
        &mut pointer_cache,
        resolve_queue_handle,
    );
}

#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn unpack_slots_expected_with_queue_handle_resolver_and_object_cache<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    expected_meta: ValueMeta,
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    pointer_cache: &mut UnpackPointerCache,
    resolve_queue_handle: &mut F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    unpack_value(
        gc,
        data,
        cursor,
        dst,
        Some(expected_meta),
        context,
        queue_handle_cache,
        pointer_cache,
        resolve_queue_handle,
    );
}

// =============================================================================
// Internal Pack Implementation
// =============================================================================

#[derive(Clone, Copy)]
struct ArrayValueLayout {
    len: usize,
    elem_meta: ValueMeta,
    elem_slots: usize,
}

#[derive(Default)]
struct RuntimeLayoutCache {
    slot_counts: HashMap<ValueRttid, Option<usize>>,
    array_layouts: HashMap<u32, Option<ArrayValueLayout>>,
    #[cfg(test)]
    slot_count_resolutions: usize,
    #[cfg(test)]
    array_layout_resolutions: usize,
}

impl RuntimeLayoutCache {
    fn slot_count(
        &mut self,
        value_rttid: ValueRttid,
        context: PackTypeContext<'_>,
    ) -> Option<usize> {
        if let Some(cached) = self.slot_counts.get(&value_rttid) {
            return *cached;
        }
        #[cfg(test)]
        {
            self.slot_count_resolutions += 1;
        }
        let resolved = context.resolver().slot_count_for_value_rttid(value_rttid);
        self.slot_counts.insert(value_rttid, resolved);
        resolved
    }

    fn array_value_layout(
        &mut self,
        value_meta: ValueMeta,
        context: PackTypeContext<'_>,
    ) -> Option<ArrayValueLayout> {
        let cache_key = value_meta.to_raw();
        if let Some(cached) = self.array_layouts.get(&cache_key) {
            return *cached;
        }
        #[cfg(test)]
        {
            self.array_layout_resolutions += 1;
        }
        let resolved = self.compute_array_value_layout(value_meta, context);
        self.array_layouts.insert(cache_key, resolved);
        resolved
    }

    fn compute_array_value_layout(
        &mut self,
        value_meta: ValueMeta,
        context: PackTypeContext<'_>,
    ) -> Option<ArrayValueLayout> {
        if value_meta.value_kind() != ValueKind::Array {
            return None;
        }
        let array_rttid = ValueRttid::new(value_meta.meta_id(), ValueKind::Array);
        let resolver = context.resolver();
        let (_, runtime_type) = resolver.resolve_value_rttid(array_rttid)?;
        let RuntimeType::Array {
            len,
            elem: elem_rttid,
        } = runtime_type
        else {
            return None;
        };
        let len = usize::try_from(*len).ok()?;
        let elem_slots = match self.slot_counts.get(elem_rttid).copied() {
            Some(Some(elem_slots)) => elem_slots,
            Some(None) => return None,
            None => {
                if len == 0 {
                    // A zero-length array occupies no slots, but its element
                    // layout remains part of the canonical wire metadata and
                    // must still be validated and preserved.
                    self.slot_count(*elem_rttid, context)?
                } else {
                    let total_slots = self.slot_count(array_rttid, context)?;
                    let elem_slots = total_slots.checked_div(len)?;
                    (elem_slots.checked_mul(len) == Some(total_slots)).then_some(elem_slots)?
                }
            }
        };
        let total_slots = len.checked_mul(elem_slots)?;
        if total_slots > RuntimeTypeResolver::MAX_LAYOUT_SLOTS {
            return None;
        }
        match self.slot_counts.get(&array_rttid) {
            Some(Some(cached)) if *cached != total_slots => return None,
            Some(None) => return None,
            Some(Some(_)) => {}
            None => {
                self.slot_counts.insert(array_rttid, Some(total_slots));
            }
        }
        if len != 0 && !self.slot_counts.contains_key(elem_rttid) {
            self.slot_counts.insert(*elem_rttid, Some(elem_slots));
        }
        let elem_meta = resolver.canonical_value_meta_for_value_rttid(*elem_rttid)?;
        Some(ArrayValueLayout {
            len,
            elem_meta,
            elem_slots,
        })
    }
}

fn array_value_layout(
    value_meta: ValueMeta,
    context: PackTypeContext<'_>,
    layout_cache: &mut RuntimeLayoutCache,
) -> Option<ArrayValueLayout> {
    layout_cache.array_value_layout(value_meta, context)
}

fn checked_struct_slot_count(meta: &StructMeta) -> Option<usize> {
    u16::try_from(meta.slot_types.len()).ok().map(usize::from)
}

fn struct_slot_count(meta: &StructMeta, context: &str) -> usize {
    checked_struct_slot_count(meta).unwrap_or_else(|| {
        panic!(
            "{context}: struct slot count {} exceeds u16::MAX",
            meta.slot_types.len()
        )
    })
}

fn expected_meta_for_rttid(rttid: ValueRttid, context: PackTypeContext<'_>) -> ValueMeta {
    context
        .resolver()
        .canonical_value_meta_for_value_rttid(rttid)
        .unwrap_or_else(|| {
            let kind = rttid.value_kind();
            let meta_id = match kind {
                ValueKind::Array | ValueKind::Struct | ValueKind::Interface => rttid.rttid(),
                _ => 0,
            };
            ValueMeta::new(meta_id, kind)
        })
}

fn checked_expected_meta_for_rttid(
    rttid: ValueRttid,
    context: PackTypeContext<'_>,
) -> Result<ValueMeta, PackedLayoutError> {
    context
        .resolver()
        .canonical_value_meta_for_value_rttid(rttid)
        .ok_or(PackedLayoutError)
}

enum PackTask {
    Value {
        src: Box<[u64]>,
        value_meta: ValueMeta,
    },
    SequenceElements {
        data_ptr: *mut u8,
        length: usize,
        index: usize,
        elem_meta: ValueMeta,
        elem_bytes: usize,
        storage_stride: usize,
        flat_storage: bool,
    },
    InlineArrayElements {
        src: Box<[u64]>,
        layout: ArrayValueLayout,
        index: usize,
    },
    StructFields {
        src: Box<[u64]>,
        meta_id: usize,
        field_index: usize,
    },
    MapEntries {
        iter: map::MapIterator,
        key_meta: ValueMeta,
        val_meta: ValueMeta,
    },
    MapValue {
        iter: map::MapIterator,
        val: Box<[u64]>,
        key_meta: ValueMeta,
        val_meta: ValueMeta,
    },
}

unsafe fn pack_value(
    packed: &mut PackedValue,
    gc: &Gc,
    src: &[u64],
    value_meta: ValueMeta,
    context: PackTypeContext<'_>,
    object_graph: &mut PackObjectGraph,
) {
    let mut layout_cache = RuntimeLayoutCache::default();
    let mut tasks = vec![PackTask::Value {
        src: src.to_vec().into_boxed_slice(),
        value_meta,
    }];
    while packed.output_error.is_none() {
        let Some(task) = tasks.pop() else {
            break;
        };
        match task {
            PackTask::Value { src, value_meta } => {
                pack_value_inner(
                    packed,
                    gc,
                    &src,
                    value_meta,
                    context,
                    object_graph,
                    &mut tasks,
                    &mut layout_cache,
                );
            }
            PackTask::SequenceElements {
                data_ptr,
                length,
                index,
                elem_meta,
                elem_bytes,
                storage_stride,
                flat_storage,
            } => {
                if index >= length {
                    continue;
                }
                let elem_slots =
                    sequence_elem_slots(elem_meta, elem_bytes, context, &mut layout_cache);
                let mut elem_buf = vec![0u64; elem_slots];
                if flat_storage {
                    assert_eq!(
                        storage_stride,
                        elem_slots * SLOT_BYTES,
                        "pack_slice: flat element stride does not match logical slot width"
                    );
                    core::ptr::copy_nonoverlapping(
                        data_ptr.add(index * storage_stride),
                        elem_buf.as_mut_ptr() as *mut u8,
                        storage_stride,
                    );
                } else {
                    read_element(data_ptr, index, elem_bytes, elem_meta, &mut elem_buf);
                }
                tasks.push(PackTask::SequenceElements {
                    data_ptr,
                    length,
                    index: index + 1,
                    elem_meta,
                    elem_bytes,
                    storage_stride,
                    flat_storage,
                });
                tasks.push(PackTask::Value {
                    src: elem_buf.into_boxed_slice(),
                    value_meta: elem_meta,
                });
            }
            PackTask::InlineArrayElements { src, layout, index } => {
                if index >= layout.len {
                    continue;
                }
                let elem_src = if layout.elem_slots == 0 {
                    Box::default()
                } else {
                    let start = index * layout.elem_slots;
                    src[start..start + layout.elem_slots]
                        .to_vec()
                        .into_boxed_slice()
                };
                tasks.push(PackTask::InlineArrayElements {
                    src,
                    layout,
                    index: index + 1,
                });
                tasks.push(PackTask::Value {
                    src: elem_src,
                    value_meta: layout.elem_meta,
                });
            }
            PackTask::StructFields {
                src,
                meta_id,
                field_index,
            } => {
                let meta = &context.struct_metas[meta_id];
                let Some(field) = meta.fields.get(field_index) else {
                    continue;
                };
                let field_slots = field.slot_count as usize;
                let slot_idx = field.offset as usize;
                let field_meta = context
                    .resolver()
                    .canonical_value_meta_for_value_rttid(field.type_info)
                    .unwrap_or_else(|| {
                        panic!(
                            "Invalid runtime type metadata for struct field {} rttid {}",
                            field.name,
                            field.type_info.rttid()
                        )
                    });
                let field_src = src[slot_idx..slot_idx + field_slots]
                    .to_vec()
                    .into_boxed_slice();
                tasks.push(PackTask::StructFields {
                    src,
                    meta_id,
                    field_index: field_index + 1,
                });
                tasks.push(PackTask::Value {
                    src: field_src,
                    value_meta: field_meta,
                });
            }
            PackTask::MapEntries {
                mut iter,
                key_meta,
                val_meta,
            } => {
                if let Some((key, val)) = map::iter_next(&mut iter) {
                    tasks.push(PackTask::MapValue {
                        iter,
                        val,
                        key_meta,
                        val_meta,
                    });
                    tasks.push(PackTask::Value {
                        src: key,
                        value_meta: key_meta,
                    });
                }
            }
            PackTask::MapValue {
                iter,
                val,
                key_meta,
                val_meta,
            } => {
                tasks.push(PackTask::MapEntries {
                    iter,
                    key_meta,
                    val_meta,
                });
                tasks.push(PackTask::Value {
                    src: val,
                    value_meta: val_meta,
                });
            }
        }
    }
}

#[inline]
fn canonical_scalar_slot(kind: ValueKind, raw: u64) -> Option<u64> {
    match kind {
        ValueKind::Bool => matches!(raw, 0 | 1).then_some(raw),
        ValueKind::Int8 => Some((raw as u8 as i8 as i64) as u64),
        ValueKind::Int16 => Some((raw as u16 as i16 as i64) as u64),
        ValueKind::Int32 => Some((raw as u32 as i32 as i64) as u64),
        ValueKind::Uint8 => Some(u64::from(raw as u8)),
        ValueKind::Uint16 => Some(u64::from(raw as u16)),
        ValueKind::Uint32 => Some(u64::from(raw as u32)),
        ValueKind::Int
        | ValueKind::Int64
        | ValueKind::Uint
        | ValueKind::Uint64
        | ValueKind::Float32
        | ValueKind::Float64 => Some(raw),
        _ => None,
    }
}

#[allow(clippy::too_many_arguments)]
unsafe fn pack_value_inner(
    packed: &mut PackedValue,
    gc: &Gc,
    src: &[u64],
    value_meta: ValueMeta,
    context: PackTypeContext<'_>,
    object_graph: &mut PackObjectGraph,
    tasks: &mut Vec<PackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    let vk = value_meta.value_kind();

    // Write type tag
    packed.push_encoded(vk as u8);

    if vk == ValueKind::Channel {
        panic!("Cannot pack non-sendable type: {:?}", vk);
    }

    if vk.is_queue() {
        let chan_ref = src[0] as GcRef;
        pack_queue_handle(packed, chan_ref);
    } else {
        match vk {
            // Scalars use one canonical logical-slot representation on the
            // wire. This matters for packed integer array values whose source
            // slots may contain a narrow storage bit-pattern such as `0xfe`
            // even though the language value is signed `-2`.
            ValueKind::Void => {}
            ValueKind::Bool
            | ValueKind::Int
            | ValueKind::Int8
            | ValueKind::Int16
            | ValueKind::Int32
            | ValueKind::Int64
            | ValueKind::Uint
            | ValueKind::Uint8
            | ValueKind::Uint16
            | ValueKind::Uint32
            | ValueKind::Uint64
            | ValueKind::Float32
            | ValueKind::Float64 => {
                let value = canonical_scalar_slot(vk, src[0])
                    .expect("pack: noncanonical boolean scalar value");
                packed.extend_encoded(&value.to_le_bytes());
            }

            // String: copy bytes
            ValueKind::String => {
                let str_ref = src[0] as GcRef;
                pack_string(packed, str_ref);
            }

            // Slice: recursively pack elements
            ValueKind::Slice => {
                let slice_ref = src[0] as GcRef;
                pack_slice(
                    packed,
                    gc,
                    slice_ref,
                    context,
                    object_graph,
                    tasks,
                    layout_cache,
                );
            }

            // Array values are flattened into slots. Heap array objects used by
            // slices are encoded by pack_array_object instead.
            ValueKind::Array => {
                if let Some(layout) = array_value_layout(value_meta, context, layout_cache) {
                    pack_array_value_inline(packed, src, layout, tasks);
                } else {
                    let arr_ref = src[0] as GcRef;
                    pack_array(packed, arr_ref, context, tasks, layout_cache);
                }
            }

            // Struct: recursively pack fields
            ValueKind::Struct => {
                let meta_id = value_meta.meta_id() as usize;
                pack_struct_inline(packed, src, meta_id, context, tasks);
            }

            // Pointer: pack pointed object (deep copy)
            ValueKind::Pointer => {
                let ptr_ref = src[0] as GcRef;
                pack_pointer(
                    packed,
                    gc,
                    ptr_ref,
                    value_meta,
                    context,
                    object_graph,
                    tasks,
                    layout_cache,
                );
            }

            // Map: iterate and pack entries
            ValueKind::Map => {
                let map_ref = src[0] as GcRef;
                pack_map(packed, map_ref, object_graph, tasks);
            }

            // Not sendable - caught at compile time or runtime-checked
            ValueKind::Island | ValueKind::Closure | ValueKind::Interface => {
                panic!("Cannot pack non-sendable type: {:?}", vk);
            }
            ValueKind::Channel | ValueKind::Port => unreachable!("queue kinds handled above"),
        }
    }
}

unsafe fn pack_string(packed: &mut PackedValue, str_ref: GcRef) {
    if str_ref.is_null() {
        // Null string: length = 0
        packed.extend_encoded(&0u64.to_le_bytes());
    } else {
        let bytes = unsafe { string::bytes_unchecked(str_ref) };
        packed.extend_encoded(&(bytes.len() as u64).to_le_bytes());
        packed.extend_encoded(bytes);
    }
}

unsafe fn pack_slice(
    packed: &mut PackedValue,
    gc: &Gc,
    slice_ref: GcRef,
    context: PackTypeContext<'_>,
    object_graph: &mut PackObjectGraph,
    tasks: &mut Vec<PackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    // Explicit null marker for all reference types
    if slice_ref.is_null() {
        packed.push_encoded(0); // null marker
        return;
    }
    packed.push_encoded(1); // non-null marker

    let length = slice::len(slice_ref);
    let capacity = slice::cap(slice_ref);
    let elem_meta = slice::elem_meta(slice_ref);
    let elem_bytes = slice::elem_bytes(slice_ref);
    let backing_len = slice::backing_len(slice_ref);
    let backing_ptr = slice::backing_ptr(slice_ref);
    let storage_stride = slice::storage_stride(slice_ref);
    let flat_storage = slice::uses_flat_slot_storage(slice_ref);
    validate_sequence_elem_layout(elem_meta, elem_bytes, context, layout_cache);

    let start = slice::start_offset(slice_ref);
    assert!(
        start <= backing_len && capacity <= backing_len - start,
        "pack_slice: slice range exceeds backing array"
    );

    packed.extend_encoded(&(length as u64).to_le_bytes());
    packed.extend_encoded(&(capacity as u64).to_le_bytes());
    packed.extend_encoded(&(start as u64).to_le_bytes());
    packed.extend_encoded(&elem_meta.to_raw().to_le_bytes());
    packed.extend_encoded(&(elem_bytes as u32).to_le_bytes());

    let owner = slice::owner_ref(slice_ref);
    let owner = if owner.is_null() {
        None
    } else {
        Some(
            gc.canonicalize_ref(owner)
                .expect("pack_slice: owner is not a GC allocation"),
        )
    };
    let key = SliceBackingKey {
        owner: owner.map_or(0, |owner| owner as usize),
        data: backing_ptr as usize,
        len: backing_len,
        elem_meta: elem_meta.to_raw(),
        elem_bytes,
        storage_stride,
        flat_storage,
    };
    if let Some(owner) = owner {
        pack_slice_owner_view(
            packed,
            gc,
            owner,
            backing_ptr,
            backing_len,
            storage_stride,
            flat_storage,
            context,
            object_graph,
            tasks,
            layout_cache,
        );
    } else {
        pack_slice_backing_view(
            packed,
            key,
            backing_ptr,
            backing_len,
            elem_meta,
            elem_bytes,
            storage_stride,
            flat_storage,
            context,
            object_graph,
            tasks,
            layout_cache,
        );
    }
}

#[allow(clippy::too_many_arguments)]
unsafe fn pack_slice_owner_view(
    packed: &mut PackedValue,
    gc: &Gc,
    owner: GcRef,
    backing_ptr: *mut u8,
    backing_len: usize,
    storage_stride: usize,
    flat_storage: bool,
    context: PackTypeContext<'_>,
    object_graph: &mut PackObjectGraph,
    tasks: &mut Vec<PackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    let (base, owner_offset, owner_bytes) = gc
        .ref_data_range(owner)
        .expect("pack_slice: owner is not a GC allocation");
    assert_eq!(owner_offset, 0, "pack_slice: owner must be canonical");
    let backing_offset = (backing_ptr as usize)
        .checked_sub(base as usize)
        .expect("pack_slice: backing precedes owner allocation");
    let backing_bytes = backing_len
        .checked_mul(storage_stride)
        .expect("pack_slice: backing byte width overflow");
    assert!(
        backing_offset
            .checked_add(backing_bytes)
            .is_some_and(|end| end <= owner_bytes),
        "pack_slice: backing exceeds owner allocation"
    );

    let (id, is_definition) = if let Some(&id) = object_graph.allocation_ids.get(&(base as usize)) {
        (id, false)
    } else {
        object_graph.next_id = object_graph
            .next_id
            .checked_add(1)
            .expect("pack_slice: object id overflow");
        let id = object_graph.next_id;
        object_graph.allocation_ids.insert(base as usize, id);
        (id, true)
    };
    packed.push_encoded(if is_definition {
        SLICE_OWNER_DEFINITION
    } else {
        SLICE_OWNER_BACK_REFERENCE
    });
    packed.extend_encoded(&id.to_le_bytes());
    packed.extend_encoded(&(backing_offset as u64).to_le_bytes());
    packed.extend_encoded(&(backing_len as u64).to_le_bytes());
    packed.extend_encoded(&(storage_stride as u64).to_le_bytes());
    packed.push_encoded(u8::from(flat_storage));
    if is_definition {
        pack_allocation_payload(packed, base, owner_bytes, context, tasks, layout_cache);
    }
}

#[allow(clippy::too_many_arguments)]
unsafe fn pack_slice_backing_view(
    packed: &mut PackedValue,
    key: SliceBackingKey,
    backing_ptr: *mut u8,
    backing_len: usize,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    storage_stride: usize,
    flat_storage: bool,
    context: PackTypeContext<'_>,
    object_graph: &mut PackObjectGraph,
    tasks: &mut Vec<PackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    if let Some(&id) = object_graph.slice_backing_ids.get(&key) {
        packed.push_encoded(SLICE_BACKING_BACK_REFERENCE);
        packed.extend_encoded(&id.to_le_bytes());
        return;
    }

    object_graph.next_id = object_graph
        .next_id
        .checked_add(1)
        .expect("pack_slice: object id overflow");
    let id = object_graph.next_id;
    object_graph.slice_backing_ids.insert(key, id);
    packed.push_encoded(SLICE_BACKING_DEFINITION);
    packed.extend_encoded(&id.to_le_bytes());

    pack_sequence_allocation_payload(
        packed,
        backing_ptr,
        backing_len,
        elem_meta,
        elem_bytes,
        storage_stride,
        flat_storage,
        context,
        tasks,
        layout_cache,
    );
}

unsafe fn pack_array_allocation_payload(
    packed: &mut PackedValue,
    arr_ref: GcRef,
    context: PackTypeContext<'_>,
    tasks: &mut Vec<PackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    let length = array::len(arr_ref);
    let elem_meta = array::elem_meta(arr_ref);
    let elem_bytes = array::elem_bytes(arr_ref);
    let data_ptr = array::data_ptr_bytes(arr_ref);
    pack_sequence_allocation_payload(
        packed,
        data_ptr,
        length,
        elem_meta,
        elem_bytes,
        elem_bytes,
        false,
        context,
        tasks,
        layout_cache,
    );
}

#[allow(clippy::too_many_arguments)]
unsafe fn pack_sequence_allocation_payload(
    packed: &mut PackedValue,
    data_ptr: *mut u8,
    length: usize,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    storage_stride: usize,
    flat_storage: bool,
    context: PackTypeContext<'_>,
    tasks: &mut Vec<PackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    validate_sequence_elem_layout(elem_meta, elem_bytes, context, layout_cache);
    packed.extend_encoded(&(length as u64).to_le_bytes());
    packed.extend_encoded(&elem_meta.to_raw().to_le_bytes());
    packed.extend_encoded(&(elem_bytes as u32).to_le_bytes());

    if elem_bytes == 0 {
        return;
    }
    if !flat_storage && can_pack_sequence_as_raw_bytes(elem_meta, elem_bytes) {
        packed.push_encoded(SEQUENCE_ENCODING_RAW_BYTES);
        pack_raw_sequence_bytes(packed, data_ptr, length, elem_bytes);
        return;
    }
    packed.push_encoded(SEQUENCE_ENCODING_ELEMENTS);
    tasks.push(PackTask::SequenceElements {
        data_ptr,
        length,
        index: 0,
        elem_meta,
        elem_bytes,
        storage_stride,
        flat_storage,
    });
}

unsafe fn pack_array(
    packed: &mut PackedValue,
    arr_ref: GcRef,
    context: PackTypeContext<'_>,
    tasks: &mut Vec<PackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    // Explicit null marker for all reference types
    if arr_ref.is_null() {
        packed.push_encoded(0); // null marker
        return;
    }
    packed.push_encoded(1); // non-null marker

    let length = array::len(arr_ref);
    let elem_meta = array::elem_meta(arr_ref);
    let elem_bytes = array::elem_bytes(arr_ref);
    validate_sequence_elem_layout(elem_meta, elem_bytes, context, layout_cache);

    packed.extend_encoded(&(length as u64).to_le_bytes());
    packed.extend_encoded(&elem_meta.to_raw().to_le_bytes());
    packed.extend_encoded(&(elem_bytes as u32).to_le_bytes());

    // Zero-size elements (e.g., struct{}) - just write count, no element data
    if elem_bytes == 0 {
        return;
    }
    if can_pack_sequence_as_raw_bytes(elem_meta, elem_bytes) {
        packed.push_encoded(SEQUENCE_ENCODING_RAW_BYTES);
        pack_raw_sequence_bytes(packed, array::data_ptr_bytes(arr_ref), length, elem_bytes);
        return;
    }
    packed.push_encoded(SEQUENCE_ENCODING_ELEMENTS);

    tasks.push(PackTask::SequenceElements {
        data_ptr: array::data_ptr_bytes(arr_ref),
        length,
        index: 0,
        elem_meta,
        elem_bytes,
        storage_stride: elem_bytes,
        flat_storage: false,
    });
}

unsafe fn pack_array_value_inline(
    packed: &mut PackedValue,
    src: &[u64],
    layout: ArrayValueLayout,
    tasks: &mut Vec<PackTask>,
) {
    let expected_slots = layout
        .len
        .checked_mul(layout.elem_slots)
        .expect("pack array value slot length overflow");
    if src.len() != expected_slots {
        panic!(
            "pack array value slot length mismatch: expected exactly {expected_slots}, got {}",
            src.len()
        );
    }

    packed.push_encoded(ARRAY_VALUE_INLINE_MARKER);
    packed.extend_encoded(&(layout.len as u64).to_le_bytes());
    packed.extend_encoded(&layout.elem_meta.to_raw().to_le_bytes());
    packed.extend_encoded(&(layout.elem_slots as u64).to_le_bytes());

    tasks.push(PackTask::InlineArrayElements {
        src: src.to_vec().into_boxed_slice(),
        layout,
        index: 0,
    });
}

unsafe fn pack_struct_inline(
    packed: &mut PackedValue,
    src: &[u64],
    meta_id: usize,
    context: PackTypeContext<'_>,
    tasks: &mut Vec<PackTask>,
) {
    if meta_id >= context.struct_metas.len() {
        panic!("Invalid struct meta_id: {}", meta_id);
    }

    let meta = &context.struct_metas[meta_id];
    let slot_count = struct_slot_count(meta, "pack_struct_inline");

    // Write meta_id for reconstruction
    packed.extend_encoded(&(meta_id as u32).to_le_bytes());
    packed.extend_encoded(&(slot_count as u32).to_le_bytes());

    tasks.push(PackTask::StructFields {
        src: src.to_vec().into_boxed_slice(),
        meta_id,
        field_index: 0,
    });
}

#[allow(clippy::too_many_arguments)]
unsafe fn pack_allocation_payload(
    packed: &mut PackedValue,
    base: GcRef,
    data_bytes: usize,
    context: PackTypeContext<'_>,
    tasks: &mut Vec<PackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    let header = unsafe { Gc::header(base) };
    if header.kind() == ValueKind::Array {
        packed.push_encoded(ALLOCATION_KIND_ARRAY);
        unsafe { pack_array_allocation_payload(packed, base, context, tasks, layout_cache) };
        return;
    }

    if !matches!(header.kind(), ValueKind::Struct | ValueKind::Pointer) {
        panic!(
            "pack allocation: unsupported owning allocation kind {:?}",
            header.kind()
        );
    }
    let allocation_meta = ValueMeta::new(header.meta_id(), ValueKind::Struct);
    let allocation_slots = struct_slot_count(
        context
            .struct_metas
            .get(allocation_meta.meta_id() as usize)
            .unwrap_or_else(|| panic!("pack allocation: invalid struct metadata")),
        "pack allocation",
    );
    assert!(
        allocation_slots
            .checked_mul(SLOT_BYTES)
            .is_some_and(|bytes| bytes <= data_bytes),
        "pack allocation: struct layout exceeds allocation"
    );
    packed.push_encoded(ALLOCATION_KIND_STRUCT);
    packed.extend_encoded(&allocation_meta.to_raw().to_le_bytes());
    packed.extend_encoded(&(allocation_slots as u32).to_le_bytes());
    let mut allocation_data = vec![0u64; allocation_slots];
    for (index, slot) in allocation_data.iter_mut().enumerate() {
        *slot = unsafe { Gc::read_slot(base, index) };
    }
    tasks.push(PackTask::Value {
        src: allocation_data.into_boxed_slice(),
        value_meta: allocation_meta,
    });
}

#[allow(clippy::too_many_arguments)]
unsafe fn pack_pointer(
    packed: &mut PackedValue,
    gc: &Gc,
    ptr_ref: GcRef,
    value_meta: ValueMeta,
    context: PackTypeContext<'_>,
    object_graph: &mut PackObjectGraph,
    tasks: &mut Vec<PackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    let allocation = if ptr_ref.is_null() {
        None
    } else {
        gc.ref_data_range(ptr_ref)
    };
    let pointer_ref = if ptr_ref.is_null() {
        PackedPointerRef::Null
    } else {
        let (base, offset, _) =
            allocation.unwrap_or_else(|| panic!("pack_pointer: invalid pointer {:p}", ptr_ref));
        if let Some(&id) = object_graph.allocation_ids.get(&(base as usize)) {
            PackedPointerRef::BackReference { id, offset }
        } else {
            object_graph.next_id = object_graph
                .next_id
                .checked_add(1)
                .expect("pack_pointer: object id overflow");
            let id = object_graph.next_id;
            object_graph.allocation_ids.insert(base as usize, id);
            PackedPointerRef::Definition { id, offset }
        }
    };

    match pointer_ref {
        PackedPointerRef::Null => {
            packed.push_encoded(0);
            return;
        }
        PackedPointerRef::Definition { id, offset } => {
            packed.push_encoded(1);
            packed.extend_encoded(&id.to_le_bytes());
            packed.extend_encoded(&(offset as u64).to_le_bytes());
        }
        PackedPointerRef::BackReference { id, offset } => {
            packed.push_encoded(2);
            packed.extend_encoded(&id.to_le_bytes());
            packed.extend_encoded(&(offset as u64).to_le_bytes());
            return;
        }
    }

    let (base, offset_bytes, data_bytes) = allocation.expect("non-null pointer allocation");
    let meta_id = value_meta.meta_id() as usize;
    if meta_id >= context.struct_metas.len() {
        panic!("pack_pointer: invalid pointee meta_id {}", meta_id);
    }
    let slots = struct_slot_count(&context.struct_metas[meta_id], "pack_pointer");
    if offset_bytes % SLOT_BYTES != 0 {
        panic!("pack_pointer: pointer {:p} is not slot-aligned", ptr_ref);
    }
    let byte_width = slots
        .checked_mul(SLOT_BYTES)
        .expect("pack_pointer: pointee byte width overflow");
    let end = offset_bytes
        .checked_add(byte_width)
        .expect("pack_pointer: pointee range overflow");
    if end > data_bytes {
        panic!(
            "pack_pointer: pointee layout exceeds allocation: offset {offset_bytes}, width {byte_width}, allocation {data_bytes}"
        );
    }

    unsafe { pack_allocation_payload(packed, base, data_bytes, context, tasks, layout_cache) };
}

unsafe fn pack_map(
    packed: &mut PackedValue,
    map_ref: GcRef,
    object_graph: &mut PackObjectGraph,
    tasks: &mut Vec<PackTask>,
) {
    // Explicit null marker for all reference types
    if map_ref.is_null() {
        packed.push_encoded(0); // null marker
        return;
    }
    if let Some(&id) = object_graph.map_ids.get(&(map_ref as usize)) {
        packed.push_encoded(2);
        packed.extend_encoded(&id.to_le_bytes());
        return;
    }
    object_graph.next_id = object_graph
        .next_id
        .checked_add(1)
        .expect("pack_map: object id overflow");
    let object_id = object_graph.next_id;
    object_graph.map_ids.insert(map_ref as usize, object_id);
    packed.push_encoded(1);
    packed.extend_encoded(&object_id.to_le_bytes());

    let length = map::len(map_ref);
    let key_meta = map::key_meta(map_ref);
    let val_meta = map::val_meta(map_ref);
    let key_slots = map::key_slots(map_ref) as usize;
    let val_slots = map::val_slots(map_ref) as usize;
    let key_rttid = map::key_rttid(map_ref);

    packed.extend_encoded(&(length as u64).to_le_bytes());
    packed.extend_encoded(&key_meta.to_raw().to_le_bytes());
    packed.extend_encoded(&val_meta.to_raw().to_le_bytes());
    packed.extend_encoded(&(key_slots as u16).to_le_bytes());
    packed.extend_encoded(&(val_slots as u16).to_le_bytes());
    packed.extend_encoded(&key_rttid.to_le_bytes());

    tasks.push(PackTask::MapEntries {
        iter: map::iter_init(map_ref),
        key_meta,
        val_meta,
    });
}

fn map_key_context_module(key_meta: ValueMeta, context: PackTypeContext<'_>) -> Option<Module> {
    match key_meta.value_kind() {
        ValueKind::Struct | ValueKind::Interface => {
            let mut module = Module::new("pack-map-key-context".into());
            module.struct_metas = context.struct_metas.to_vec();
            module.named_type_metas = context.named_type_metas.to_vec();
            module.runtime_types = context.runtime_types.to_vec();
            Some(module)
        }
        _ => None,
    }
}

// =============================================================================
// Internal Unpack Implementation
// =============================================================================

fn read_exact<'a>(
    data: &'a [u8],
    cursor: &mut usize,
    len: usize,
) -> Result<&'a [u8], PackedLayoutError> {
    let end = cursor.checked_add(len).ok_or(PackedLayoutError)?;
    let bytes = data.get(*cursor..end).ok_or(PackedLayoutError)?;
    *cursor = end;
    Ok(bytes)
}

fn validate_read_u64(data: &[u8], cursor: &mut usize) -> Result<u64, PackedLayoutError> {
    let bytes: [u8; 8] = read_exact(data, cursor, 8)?
        .try_into()
        .map_err(|_| PackedLayoutError)?;
    Ok(u64::from_le_bytes(bytes))
}

fn validate_read_usize(data: &[u8], cursor: &mut usize) -> Result<usize, PackedLayoutError> {
    usize::try_from(validate_read_u64(data, cursor)?).map_err(|_| PackedLayoutError)
}

fn validate_read_u32(data: &[u8], cursor: &mut usize) -> Result<u32, PackedLayoutError> {
    let bytes: [u8; 4] = read_exact(data, cursor, 4)?
        .try_into()
        .map_err(|_| PackedLayoutError)?;
    Ok(u32::from_le_bytes(bytes))
}

fn validate_read_u16(data: &[u8], cursor: &mut usize) -> Result<u16, PackedLayoutError> {
    let bytes: [u8; 2] = read_exact(data, cursor, 2)?
        .try_into()
        .map_err(|_| PackedLayoutError)?;
    Ok(u16::from_le_bytes(bytes))
}

fn validate_value_kind(raw: u8) -> Result<ValueKind, PackedLayoutError> {
    ValueKind::try_from(raw).map_err(|_| PackedLayoutError)
}

fn validate_read_value_meta(
    data: &[u8],
    cursor: &mut usize,
) -> Result<ValueMeta, PackedLayoutError> {
    let raw = validate_read_u32(data, cursor)?;
    ValueMeta::try_from_raw(raw).ok_or(PackedLayoutError)
}

fn validate_read_value_rttid(
    data: &[u8],
    cursor: &mut usize,
) -> Result<ValueRttid, PackedLayoutError> {
    let raw = validate_read_u32(data, cursor)?;
    ValueRttid::try_from_raw(raw).ok_or(PackedLayoutError)
}

fn validate_reference_marker(data: &[u8], cursor: &mut usize) -> Result<bool, PackedLayoutError> {
    match *read_exact(data, cursor, 1)?
        .first()
        .ok_or(PackedLayoutError)?
    {
        0 => Ok(false),
        1 => Ok(true),
        _ => Err(PackedLayoutError),
    }
}

fn validate_packed_pointer_ref(
    data: &[u8],
    cursor: &mut usize,
) -> Result<PackedPointerRef, PackedLayoutError> {
    match *read_exact(data, cursor, 1)?
        .first()
        .ok_or(PackedLayoutError)?
    {
        0 => Ok(PackedPointerRef::Null),
        1 => Ok(PackedPointerRef::Definition {
            id: validate_read_u64(data, cursor)?,
            offset: usize::try_from(validate_read_u64(data, cursor)?)
                .map_err(|_| PackedLayoutError)?,
        }),
        2 => Ok(PackedPointerRef::BackReference {
            id: validate_read_u64(data, cursor)?,
            offset: usize::try_from(validate_read_u64(data, cursor)?)
                .map_err(|_| PackedLayoutError)?,
        }),
        _ => Err(PackedLayoutError),
    }
}

fn validate_packed_queue_handle(
    data: &[u8],
    cursor: &mut usize,
    expected_kind: ValueKind,
    expected_rttid: Option<ValueRttid>,
    context: PackTypeContext<'_>,
    layout_cache: &mut RuntimeLayoutCache,
) -> Result<(), PackedLayoutError> {
    if !validate_reference_marker(data, cursor)? {
        return Ok(());
    }
    let kind = validate_value_kind(
        *read_exact(data, cursor, 1)?
            .first()
            .ok_or(PackedLayoutError)?,
    )?;
    if kind != expected_kind {
        return Err(PackedLayoutError);
    }
    let _endpoint_id = validate_read_u64(data, cursor)?;
    let _home_island = validate_read_u32(data, cursor)?;
    let _cap = validate_read_u64(data, cursor)?;
    let elem_meta = validate_read_value_meta(data, cursor)?;
    let elem_rttid = validate_read_value_rttid(data, cursor)?;
    let elem_slots = validate_read_u16(data, cursor)?;
    let expected_elem_rttid =
        expected_rttid.and_then(|rttid| match context.resolve_runtime_type(rttid)? {
            RuntimeType::Chan { elem, .. } | RuntimeType::Port { elem, .. } => Some(*elem),
            _ => None,
        });
    if let Some(expected_elem_rttid) = expected_elem_rttid {
        if elem_rttid != expected_elem_rttid {
            return Err(PackedLayoutError);
        }
        let expected_elem_meta = expected_meta_for_rttid(expected_elem_rttid, context);
        if elem_meta != expected_elem_meta {
            return Err(PackedLayoutError);
        }
        let expected_elem_slots = layout_cache
            .slot_count(expected_elem_rttid, context)
            .ok_or(PackedLayoutError)?;
        if usize::from(elem_slots) != expected_elem_slots {
            return Err(PackedLayoutError);
        }
    }
    let _closed = read_exact(data, cursor, 1)?;
    Ok(())
}

enum ValidateTask {
    Value {
        expected_meta: ValueMeta,
        expected_rttid: Option<ValueRttid>,
    },
    SequenceElements {
        remaining: usize,
        elem_meta: ValueMeta,
        elem_rttid: Option<ValueRttid>,
    },
    StructFields {
        meta_id: usize,
        field_index: usize,
    },
    MapEntries {
        remaining: usize,
        key_meta: ValueMeta,
        key_rttid: Option<ValueRttid>,
        val_meta: ValueMeta,
        val_rttid: Option<ValueRttid>,
    },
    MapValue {
        remaining: usize,
        key_meta: ValueMeta,
        key_rttid: Option<ValueRttid>,
        val_meta: ValueMeta,
        val_rttid: Option<ValueRttid>,
    },
}

fn validate_packed_sequence(
    data: &[u8],
    cursor: &mut usize,
    sequence: PackedSequenceSpec,
    context: PackTypeContext<'_>,
    tasks: &mut Vec<ValidateTask>,
    layout_cache: &mut RuntimeLayoutCache,
) -> Result<(), PackedLayoutError> {
    if sequence.length > isize::MAX as usize {
        return Err(PackedLayoutError);
    }
    let elem_layout = sequence_elem_layout_checked(
        sequence.elem_meta,
        sequence.elem_bytes,
        context,
        layout_cache,
    )
    .map_err(|_| PackedLayoutError)?;
    if sequence.elem_bytes == 0 {
        return Ok(());
    }
    let encoding = *read_exact(data, cursor, 1)?
        .first()
        .ok_or(PackedLayoutError)?;
    if encoding == SEQUENCE_ENCODING_RAW_BYTES {
        if !can_pack_sequence_as_raw_bytes(sequence.elem_meta, sequence.elem_bytes) {
            return Err(PackedLayoutError);
        }
        let byte_len = sequence
            .length
            .checked_mul(sequence.elem_bytes)
            .ok_or(PackedLayoutError)?;
        read_exact(data, cursor, byte_len)?;
        return Ok(());
    }
    if encoding != SEQUENCE_ENCODING_ELEMENTS {
        return Err(PackedLayoutError);
    }
    let _ = elem_layout.logical_slots;
    tasks.push(ValidateTask::SequenceElements {
        remaining: sequence.length,
        elem_meta: sequence.elem_meta,
        elem_rttid: sequence.elem_rttid,
    });
    Ok(())
}

fn checked_array_allocation_data_bytes(len: usize, elem_bytes: usize) -> Option<usize> {
    let element_data = len.checked_mul(elem_bytes)?;
    let element_slots = element_data.checked_add(SLOT_BYTES - 1)? / SLOT_BYTES;
    array::HEADER_SLOTS
        .checked_add(element_slots)?
        .checked_mul(SLOT_BYTES)
}

fn validate_pointer_into_allocation(
    allocation: ValidatedAllocation,
    offset: usize,
    expected_obj_meta: ValueMeta,
    expected_slots: usize,
    context: PackTypeContext<'_>,
    layout_cache: &mut RuntimeLayoutCache,
) -> Result<(), PackedLayoutError> {
    if !offset.is_multiple_of(SLOT_BYTES) {
        return Err(PackedLayoutError);
    }
    if matches!(allocation.kind, ValidatedAllocationKind::Array { .. })
        && offset < array::HEADER_SLOTS * SLOT_BYTES
    {
        return Err(PackedLayoutError);
    }
    let width = expected_slots
        .checked_mul(SLOT_BYTES)
        .ok_or(PackedLayoutError)?;
    if offset
        .checked_add(width)
        .filter(|end| *end <= allocation.data_bytes)
        .is_none()
    {
        return Err(PackedLayoutError);
    }
    if !allocation_contains_pointer_target(
        allocation,
        offset,
        expected_obj_meta,
        expected_slots,
        context,
        layout_cache,
    ) {
        return Err(PackedLayoutError);
    }
    Ok(())
}

fn allocation_contains_pointer_target(
    allocation: ValidatedAllocation,
    offset: usize,
    expected_meta: ValueMeta,
    expected_slots: usize,
    context: PackTypeContext<'_>,
    layout_cache: &mut RuntimeLayoutCache,
) -> bool {
    let depth = context
        .runtime_types
        .len()
        .saturating_add(context.struct_metas.len())
        .saturating_add(1);
    match allocation.kind {
        ValidatedAllocationKind::Struct { meta, .. } => value_layout_contains_pointer_target(
            meta,
            None,
            0,
            offset / SLOT_BYTES,
            expected_meta,
            expected_slots,
            context,
            layout_cache,
            depth,
        ),
        ValidatedAllocationKind::Array {
            elem_meta,
            elem_bytes,
            len,
        } => {
            let data_offset = array::HEADER_SLOTS * SLOT_BYTES;
            let Some(relative) = offset.checked_sub(data_offset) else {
                return false;
            };
            let Ok(elem_layout) =
                sequence_elem_layout_checked(elem_meta, elem_bytes, context, layout_cache)
            else {
                return false;
            };
            if elem_layout.logical_slots == 0 {
                if len == 0 || relative != 0 || expected_slots != 0 {
                    return false;
                }
                return value_layout_contains_pointer_target(
                    elem_meta,
                    inferred_rttid_for_meta(elem_meta),
                    array::HEADER_SLOTS,
                    array::HEADER_SLOTS,
                    expected_meta,
                    expected_slots,
                    context,
                    layout_cache,
                    depth,
                );
            }
            let Some(flat_elem_bytes) = elem_layout.logical_slots.checked_mul(SLOT_BYTES) else {
                return false;
            };
            if elem_bytes != flat_elem_bytes {
                return false;
            }
            let elem_index = relative / elem_bytes;
            if elem_index >= len {
                return false;
            }
            let Some(elem_base) = elem_index
                .checked_mul(elem_layout.logical_slots)
                .and_then(|offset| array::HEADER_SLOTS.checked_add(offset))
            else {
                return false;
            };
            value_layout_contains_pointer_target(
                elem_meta,
                inferred_rttid_for_meta(elem_meta),
                elem_base,
                offset / SLOT_BYTES,
                expected_meta,
                expected_slots,
                context,
                layout_cache,
                depth,
            )
        }
    }
}

fn inferred_rttid_for_meta(meta: ValueMeta) -> Option<ValueRttid> {
    matches!(
        meta.value_kind(),
        ValueKind::Array | ValueKind::Struct | ValueKind::Interface
    )
    .then(|| ValueRttid::new(meta.meta_id(), meta.value_kind()))
}

fn allocation_contains_flat_slice_backing(
    allocation: ValidatedAllocation,
    backing_offset: usize,
    backing_len: usize,
    elem_meta: ValueMeta,
    storage_stride: usize,
    context: PackTypeContext<'_>,
    layout_cache: &mut RuntimeLayoutCache,
) -> bool {
    let depth = context
        .runtime_types
        .len()
        .saturating_add(context.struct_metas.len())
        .saturating_add(1);
    let target_slot = backing_offset / SLOT_BYTES;
    match allocation.kind {
        ValidatedAllocationKind::Struct { meta, .. } => value_layout_contains_flat_array_backing(
            meta,
            None,
            0,
            target_slot,
            backing_len,
            elem_meta,
            storage_stride,
            context,
            layout_cache,
            depth,
        ),
        ValidatedAllocationKind::Array {
            elem_meta: owner_elem_meta,
            elem_bytes: owner_elem_bytes,
            len: owner_len,
        } => {
            let Ok(owner_elem_layout) = sequence_elem_layout_checked(
                owner_elem_meta,
                owner_elem_bytes,
                context,
                layout_cache,
            ) else {
                return false;
            };
            let data_base = array::HEADER_SLOTS;
            if owner_elem_layout.logical_slots == 0 {
                if owner_len == 0 || target_slot != data_base {
                    return false;
                }
                return value_layout_contains_flat_array_backing(
                    owner_elem_meta,
                    inferred_rttid_for_meta(owner_elem_meta),
                    data_base,
                    target_slot,
                    backing_len,
                    elem_meta,
                    storage_stride,
                    context,
                    layout_cache,
                    depth,
                );
            }
            let Some(flat_owner_elem_bytes) =
                owner_elem_layout.logical_slots.checked_mul(SLOT_BYTES)
            else {
                return false;
            };
            if owner_elem_bytes != flat_owner_elem_bytes || target_slot < data_base {
                return false;
            }
            let relative = target_slot - data_base;
            let elem_index = relative / owner_elem_layout.logical_slots;
            if elem_index >= owner_len {
                return false;
            }
            let Some(elem_base) = elem_index
                .checked_mul(owner_elem_layout.logical_slots)
                .and_then(|offset| data_base.checked_add(offset))
            else {
                return false;
            };
            value_layout_contains_flat_array_backing(
                owner_elem_meta,
                inferred_rttid_for_meta(owner_elem_meta),
                elem_base,
                target_slot,
                backing_len,
                elem_meta,
                storage_stride,
                context,
                layout_cache,
                depth,
            )
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn value_layout_contains_flat_array_backing(
    actual_meta: ValueMeta,
    actual_rttid: Option<ValueRttid>,
    actual_base_slot: usize,
    target_slot: usize,
    backing_len: usize,
    expected_elem_meta: ValueMeta,
    storage_stride: usize,
    context: PackTypeContext<'_>,
    layout_cache: &mut RuntimeLayoutCache,
    depth: usize,
) -> bool {
    if depth == 0 || target_slot < actual_base_slot {
        return false;
    }
    match actual_meta.value_kind() {
        ValueKind::Struct => {
            let Some(meta) = context.struct_metas.get(actual_meta.meta_id() as usize) else {
                return false;
            };
            for field in &meta.fields {
                let Some(field_base) = actual_base_slot.checked_add(field.offset as usize) else {
                    return false;
                };
                let field_slots = field.slot_count as usize;
                if target_slot < field_base || target_slot > field_base.saturating_add(field_slots)
                {
                    continue;
                }
                if value_layout_contains_flat_array_backing(
                    expected_meta_for_rttid(field.type_info, context),
                    Some(field.type_info),
                    field_base,
                    target_slot,
                    backing_len,
                    expected_elem_meta,
                    storage_stride,
                    context,
                    layout_cache,
                    depth - 1,
                ) {
                    return true;
                }
            }
            false
        }
        ValueKind::Array => {
            let Some(layout) = array_value_layout(actual_meta, context, layout_cache) else {
                return false;
            };
            let array_rttid = actual_rttid.or_else(|| inferred_rttid_for_meta(actual_meta));
            let Some((elem_rttid, nested_elem_meta)) = array_rttid.and_then(|array_rttid| {
                let (_, runtime_type) = context.resolver().resolve_value_rttid(array_rttid)?;
                let RuntimeType::Array { elem, .. } = runtime_type else {
                    return None;
                };
                Some((*elem, expected_meta_for_rttid(*elem, context)))
            }) else {
                return false;
            };
            let Some(expected_stride) = layout.elem_slots.checked_mul(SLOT_BYTES) else {
                return false;
            };
            if actual_base_slot == target_slot
                && layout.len == backing_len
                && layout.elem_meta == expected_elem_meta
                && expected_stride == storage_stride
            {
                return true;
            }
            if layout.elem_slots == 0 {
                if layout.len == 0 || target_slot != actual_base_slot {
                    return false;
                }
                return value_layout_contains_flat_array_backing(
                    nested_elem_meta,
                    Some(elem_rttid),
                    actual_base_slot,
                    target_slot,
                    backing_len,
                    expected_elem_meta,
                    storage_stride,
                    context,
                    layout_cache,
                    depth - 1,
                );
            }
            let relative = target_slot - actual_base_slot;
            let elem_index = relative / layout.elem_slots;
            if elem_index >= layout.len {
                return false;
            }
            let Some(elem_base) = elem_index
                .checked_mul(layout.elem_slots)
                .and_then(|offset| actual_base_slot.checked_add(offset))
            else {
                return false;
            };
            value_layout_contains_flat_array_backing(
                nested_elem_meta,
                Some(elem_rttid),
                elem_base,
                target_slot,
                backing_len,
                expected_elem_meta,
                storage_stride,
                context,
                layout_cache,
                depth - 1,
            )
        }
        _ => false,
    }
}

#[allow(clippy::too_many_arguments)]
fn value_layout_contains_pointer_target(
    actual_meta: ValueMeta,
    actual_rttid: Option<ValueRttid>,
    actual_base_slot: usize,
    target_slot: usize,
    expected_meta: ValueMeta,
    expected_slots: usize,
    context: PackTypeContext<'_>,
    layout_cache: &mut RuntimeLayoutCache,
    depth: usize,
) -> bool {
    if depth == 0 || target_slot < actual_base_slot {
        return false;
    }
    let actual_slots = match actual_meta.value_kind() {
        ValueKind::Void => Some(0),
        ValueKind::Interface => Some(2),
        ValueKind::Struct => context
            .struct_metas
            .get(actual_meta.meta_id() as usize)
            .and_then(checked_struct_slot_count),
        ValueKind::Array => array_value_layout(actual_meta, context, layout_cache)
            .and_then(|layout| layout.len.checked_mul(layout.elem_slots)),
        _ => Some(1),
    };
    let Some(actual_slots) = actual_slots else {
        return false;
    };
    if target_slot == actual_base_slot
        && actual_meta == expected_meta
        && actual_slots == expected_slots
    {
        return true;
    }
    if target_slot
        .checked_add(expected_slots)
        .is_none_or(|end| end > actual_base_slot.saturating_add(actual_slots))
    {
        return false;
    }

    match actual_meta.value_kind() {
        ValueKind::Struct => {
            let Some(meta) = context.struct_metas.get(actual_meta.meta_id() as usize) else {
                return false;
            };
            for field in &meta.fields {
                let Some(field_base) = actual_base_slot.checked_add(field.offset as usize) else {
                    return false;
                };
                let field_slots = field.slot_count as usize;
                if target_slot < field_base
                    || target_slot
                        .checked_add(expected_slots)
                        .is_none_or(|end| end > field_base.saturating_add(field_slots))
                {
                    continue;
                }
                let field_meta = expected_meta_for_rttid(field.type_info, context);
                if value_layout_contains_pointer_target(
                    field_meta,
                    Some(field.type_info),
                    field_base,
                    target_slot,
                    expected_meta,
                    expected_slots,
                    context,
                    layout_cache,
                    depth - 1,
                ) {
                    return true;
                }
            }
            false
        }
        ValueKind::Array => {
            let Some(layout) = array_value_layout(actual_meta, context, layout_cache) else {
                return false;
            };
            let array_rttid = actual_rttid.or_else(|| inferred_rttid_for_meta(actual_meta));
            let Some((elem_rttid, elem_meta)) = array_rttid.and_then(|array_rttid| {
                let (_, runtime_type) = context.resolver().resolve_value_rttid(array_rttid)?;
                let RuntimeType::Array { elem, .. } = runtime_type else {
                    return None;
                };
                Some((*elem, expected_meta_for_rttid(*elem, context)))
            }) else {
                return false;
            };
            if layout.elem_slots == 0 {
                if layout.len == 0 || target_slot != actual_base_slot || expected_slots != 0 {
                    return false;
                }
                return value_layout_contains_pointer_target(
                    elem_meta,
                    Some(elem_rttid),
                    actual_base_slot,
                    target_slot,
                    expected_meta,
                    expected_slots,
                    context,
                    layout_cache,
                    depth - 1,
                );
            }
            let relative = target_slot - actual_base_slot;
            let elem_index = relative / layout.elem_slots;
            if elem_index >= layout.len {
                return false;
            }
            let Some(elem_base) = elem_index
                .checked_mul(layout.elem_slots)
                .and_then(|offset| actual_base_slot.checked_add(offset))
            else {
                return false;
            };
            value_layout_contains_pointer_target(
                elem_meta,
                Some(elem_rttid),
                elem_base,
                target_slot,
                expected_meta,
                expected_slots,
                context,
                layout_cache,
                depth - 1,
            )
        }
        _ => false,
    }
}

#[allow(clippy::too_many_arguments)]
fn validate_and_register_allocation_definition(
    data: &[u8],
    cursor: &mut usize,
    object_id: u64,
    context: PackTypeContext<'_>,
    object_cache: &mut ValidatePointerCache,
    tasks: &mut Vec<ValidateTask>,
    layout_cache: &mut RuntimeLayoutCache,
) -> Result<ValidatedAllocation, PackedLayoutError> {
    if object_id == 0 || object_cache.allocations.contains_key(&object_id) {
        return Err(PackedLayoutError);
    }
    let allocation_kind = *read_exact(data, cursor, 1)?
        .first()
        .ok_or(PackedLayoutError)?;
    match allocation_kind {
        ALLOCATION_KIND_STRUCT => {
            let allocation_meta = validate_read_value_meta(data, cursor)?;
            let slots = validate_read_u32(data, cursor)? as usize;
            if slots > u16::MAX as usize || allocation_meta.value_kind() != ValueKind::Struct {
                return Err(PackedLayoutError);
            }
            let canonical_slots = context
                .struct_metas
                .get(allocation_meta.meta_id() as usize)
                .and_then(checked_struct_slot_count)
                .ok_or(PackedLayoutError)?;
            if slots != canonical_slots {
                return Err(PackedLayoutError);
            }
            let allocation = ValidatedAllocation {
                data_bytes: slots.checked_mul(SLOT_BYTES).ok_or(PackedLayoutError)?,
                kind: ValidatedAllocationKind::Struct {
                    meta: allocation_meta,
                    slots,
                },
            };
            object_cache.allocations.insert(object_id, allocation);
            tasks.push(ValidateTask::Value {
                expected_meta: allocation_meta,
                expected_rttid: None,
            });
            Ok(allocation)
        }
        ALLOCATION_KIND_ARRAY => {
            let length = validate_read_usize(data, cursor)?;
            let elem_meta = validate_read_value_meta(data, cursor)?;
            let elem_bytes = validate_read_u32(data, cursor)? as usize;
            sequence_elem_layout_checked(elem_meta, elem_bytes, context, layout_cache)
                .map_err(|_| PackedLayoutError)?;
            let allocation = ValidatedAllocation {
                data_bytes: checked_array_allocation_data_bytes(length, elem_bytes)
                    .ok_or(PackedLayoutError)?,
                kind: ValidatedAllocationKind::Array {
                    elem_meta,
                    elem_bytes,
                    len: length,
                },
            };
            object_cache.allocations.insert(object_id, allocation);
            validate_packed_sequence(
                data,
                cursor,
                PackedSequenceSpec {
                    elem_meta,
                    elem_rttid: None,
                    elem_bytes,
                    length,
                },
                context,
                tasks,
                layout_cache,
            )?;
            Ok(allocation)
        }
        _ => Err(PackedLayoutError),
    }
}

#[allow(clippy::too_many_arguments)]
fn validate_slice_view_in_allocation(
    allocation: ValidatedAllocation,
    backing_offset: usize,
    backing_len: usize,
    storage_stride: usize,
    flat_storage: bool,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    context: PackTypeContext<'_>,
    layout_cache: &mut RuntimeLayoutCache,
) -> Result<(), PackedLayoutError> {
    if backing_len > isize::MAX as usize {
        return Err(PackedLayoutError);
    }
    let layout = sequence_elem_layout_checked(elem_meta, elem_bytes, context, layout_cache)
        .map_err(|_| PackedLayoutError)?;
    let expected_stride = if flat_storage {
        layout
            .logical_slots
            .checked_mul(SLOT_BYTES)
            .ok_or(PackedLayoutError)?
    } else {
        elem_bytes
    };
    if storage_stride != expected_stride {
        return Err(PackedLayoutError);
    }
    if flat_storage && !backing_offset.is_multiple_of(SLOT_BYTES) {
        return Err(PackedLayoutError);
    }
    if flat_storage
        && !allocation_contains_flat_slice_backing(
            allocation,
            backing_offset,
            backing_len,
            elem_meta,
            storage_stride,
            context,
            layout_cache,
        )
    {
        return Err(PackedLayoutError);
    }
    let backing_bytes = backing_len
        .checked_mul(storage_stride)
        .ok_or(PackedLayoutError)?;
    if backing_offset
        .checked_add(backing_bytes)
        .filter(|end| *end <= allocation.data_bytes)
        .is_none()
    {
        return Err(PackedLayoutError);
    }
    match allocation.kind {
        ValidatedAllocationKind::Struct { .. } if !flat_storage => Err(PackedLayoutError),
        ValidatedAllocationKind::Array { .. }
            if flat_storage && backing_offset < array::HEADER_SLOTS * SLOT_BYTES =>
        {
            Err(PackedLayoutError)
        }
        ValidatedAllocationKind::Array {
            elem_meta: owner_elem_meta,
            elem_bytes: owner_elem_bytes,
            len: owner_len,
        } if !flat_storage => {
            if backing_offset != array::HEADER_SLOTS * SLOT_BYTES
                || backing_len != owner_len
                || elem_meta != owner_elem_meta
                || elem_bytes != owner_elem_bytes
            {
                return Err(PackedLayoutError);
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

#[allow(clippy::too_many_arguments)]
fn validate_packed_slice_backing(
    data: &[u8],
    cursor: &mut usize,
    elem_meta: ValueMeta,
    elem_rttid: Option<ValueRttid>,
    elem_bytes: usize,
    start: usize,
    capacity: usize,
    context: PackTypeContext<'_>,
    object_cache: &mut ValidatePointerCache,
    tasks: &mut Vec<ValidateTask>,
    layout_cache: &mut RuntimeLayoutCache,
) -> Result<(), PackedLayoutError> {
    let marker = *read_exact(data, cursor, 1)?
        .first()
        .ok_or(PackedLayoutError)?;
    let object_id = validate_read_u64(data, cursor)?;
    if object_id == 0 {
        return Err(PackedLayoutError);
    }
    if marker == SLICE_BACKING_BACK_REFERENCE {
        let Some(allocation) = object_cache.allocations.get(&object_id).copied() else {
            return Err(PackedLayoutError);
        };
        let ValidatedAllocationKind::Array {
            elem_meta: cached_meta,
            elem_bytes: cached_bytes,
            len: cached_len,
        } = allocation.kind
        else {
            return Err(PackedLayoutError);
        };
        if cached_meta != elem_meta
            || cached_bytes != elem_bytes
            || start > cached_len
            || capacity > cached_len - start
        {
            return Err(PackedLayoutError);
        }
        return Ok(());
    }
    if marker == SLICE_BACKING_DEFINITION {
        if object_cache.allocations.contains_key(&object_id) {
            return Err(PackedLayoutError);
        }
        let backing_len = validate_read_usize(data, cursor)?;
        let backing_meta = validate_read_value_meta(data, cursor)?;
        let backing_bytes = validate_read_u32(data, cursor)? as usize;
        if backing_meta != elem_meta
            || backing_bytes != elem_bytes
            || start > backing_len
            || capacity > backing_len - start
        {
            return Err(PackedLayoutError);
        }
        let data_bytes = checked_array_allocation_data_bytes(backing_len, backing_bytes)
            .ok_or(PackedLayoutError)?;
        object_cache.allocations.insert(
            object_id,
            ValidatedAllocation {
                data_bytes,
                kind: ValidatedAllocationKind::Array {
                    elem_meta: backing_meta,
                    elem_bytes: backing_bytes,
                    len: backing_len,
                },
            },
        );
        return validate_packed_sequence(
            data,
            cursor,
            PackedSequenceSpec {
                elem_meta: backing_meta,
                elem_rttid,
                elem_bytes: backing_bytes,
                length: backing_len,
            },
            context,
            tasks,
            layout_cache,
        );
    }

    if !matches!(marker, SLICE_OWNER_DEFINITION | SLICE_OWNER_BACK_REFERENCE) {
        return Err(PackedLayoutError);
    }
    let backing_offset = validate_read_usize(data, cursor)?;
    let backing_len = validate_read_usize(data, cursor)?;
    let storage_stride = validate_read_usize(data, cursor)?;
    let flat_storage = match *read_exact(data, cursor, 1)?
        .first()
        .ok_or(PackedLayoutError)?
    {
        0 => false,
        1 => true,
        _ => return Err(PackedLayoutError),
    };
    if start > backing_len || capacity > backing_len - start {
        return Err(PackedLayoutError);
    }
    let allocation = if marker == SLICE_OWNER_DEFINITION {
        validate_and_register_allocation_definition(
            data,
            cursor,
            object_id,
            context,
            object_cache,
            tasks,
            layout_cache,
        )?
    } else {
        object_cache
            .allocations
            .get(&object_id)
            .copied()
            .ok_or(PackedLayoutError)?
    };
    validate_slice_view_in_allocation(
        allocation,
        backing_offset,
        backing_len,
        storage_stride,
        flat_storage,
        elem_meta,
        elem_bytes,
        context,
        layout_cache,
    )
}

fn validate_packed_value(
    data: &[u8],
    cursor: &mut usize,
    expected_meta: ValueMeta,
    expected_rttid: Option<ValueRttid>,
    context: PackTypeContext<'_>,
    object_cache: &mut ValidatePointerCache,
) -> Result<(), PackedLayoutError> {
    let mut layout_cache = RuntimeLayoutCache::default();
    let mut tasks = vec![ValidateTask::Value {
        expected_meta,
        expected_rttid,
    }];
    while let Some(task) = tasks.pop() {
        match task {
            ValidateTask::Value {
                expected_meta,
                expected_rttid,
            } => validate_packed_value_inner(
                data,
                cursor,
                expected_meta,
                expected_rttid,
                context,
                object_cache,
                &mut tasks,
                &mut layout_cache,
            )?,
            ValidateTask::SequenceElements {
                remaining,
                elem_meta,
                elem_rttid,
            } => {
                if remaining == 0 {
                    continue;
                }
                tasks.push(ValidateTask::SequenceElements {
                    remaining: remaining - 1,
                    elem_meta,
                    elem_rttid,
                });
                tasks.push(ValidateTask::Value {
                    expected_meta: elem_meta,
                    expected_rttid: elem_rttid,
                });
            }
            ValidateTask::StructFields {
                meta_id,
                field_index,
            } => {
                let meta = &context.struct_metas[meta_id];
                let Some(field) = meta.fields.get(field_index) else {
                    continue;
                };
                tasks.push(ValidateTask::StructFields {
                    meta_id,
                    field_index: field_index + 1,
                });
                tasks.push(ValidateTask::Value {
                    expected_meta: expected_meta_for_rttid(field.type_info, context),
                    expected_rttid: Some(field.type_info),
                });
            }
            ValidateTask::MapEntries {
                remaining,
                key_meta,
                key_rttid,
                val_meta,
                val_rttid,
            } => {
                if remaining == 0 {
                    continue;
                }
                tasks.push(ValidateTask::MapValue {
                    remaining,
                    key_meta,
                    key_rttid,
                    val_meta,
                    val_rttid,
                });
                tasks.push(ValidateTask::Value {
                    expected_meta: key_meta,
                    expected_rttid: key_rttid,
                });
            }
            ValidateTask::MapValue {
                remaining,
                key_meta,
                key_rttid,
                val_meta,
                val_rttid,
            } => {
                tasks.push(ValidateTask::MapEntries {
                    remaining: remaining - 1,
                    key_meta,
                    key_rttid,
                    val_meta,
                    val_rttid,
                });
                tasks.push(ValidateTask::Value {
                    expected_meta: val_meta,
                    expected_rttid: val_rttid,
                });
            }
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn validate_packed_value_inner(
    data: &[u8],
    cursor: &mut usize,
    expected_meta: ValueMeta,
    expected_rttid: Option<ValueRttid>,
    context: PackTypeContext<'_>,
    pointer_cache: &mut ValidatePointerCache,
    tasks: &mut Vec<ValidateTask>,
    layout_cache: &mut RuntimeLayoutCache,
) -> Result<(), PackedLayoutError> {
    if let Some(expected_rttid) = expected_rttid {
        if checked_expected_meta_for_rttid(expected_rttid, context)? != expected_meta
            || layout_cache.slot_count(expected_rttid, context).is_none()
        {
            return Err(PackedLayoutError);
        }
    }
    let vk = validate_value_kind(
        *read_exact(data, cursor, 1)?
            .first()
            .ok_or(PackedLayoutError)?,
    )?;
    let expected_kind = expected_meta.value_kind();
    if vk != expected_kind {
        return Err(PackedLayoutError);
    }
    if vk.is_queue() {
        return validate_packed_queue_handle(
            data,
            cursor,
            expected_kind,
            expected_rttid,
            context,
            layout_cache,
        );
    }
    match vk {
        ValueKind::Void => Ok(()),
        ValueKind::Bool
        | ValueKind::Int
        | ValueKind::Int8
        | ValueKind::Int16
        | ValueKind::Int32
        | ValueKind::Int64
        | ValueKind::Uint
        | ValueKind::Uint8
        | ValueKind::Uint16
        | ValueKind::Uint32
        | ValueKind::Uint64
        | ValueKind::Float32
        | ValueKind::Float64 => {
            let raw = validate_read_u64(data, cursor)?;
            if canonical_scalar_slot(vk, raw) == Some(raw) {
                Ok(())
            } else {
                Err(PackedLayoutError)
            }
        }
        ValueKind::String => {
            let len = validate_read_usize(data, cursor)?;
            read_exact(data, cursor, len)?;
            Ok(())
        }
        ValueKind::Slice => {
            if !validate_reference_marker(data, cursor)? {
                return Ok(());
            }
            let length = validate_read_usize(data, cursor)?;
            let capacity = validate_read_usize(data, cursor)?;
            let start = validate_read_usize(data, cursor)?;
            if length > isize::MAX as usize
                || capacity > isize::MAX as usize
                || start > isize::MAX as usize
                || length > capacity
            {
                return Err(PackedLayoutError);
            }
            let elem_meta = validate_read_value_meta(data, cursor)?;
            let elem_bytes = validate_read_u32(data, cursor)? as usize;
            let elem_rttid =
                expected_rttid.and_then(|rttid| match context.resolve_runtime_type(rttid)? {
                    RuntimeType::Slice(elem) => Some(*elem),
                    _ => None,
                });
            if let Some(elem_rttid) = elem_rttid {
                let expected_elem_meta = checked_expected_meta_for_rttid(elem_rttid, context)?;
                if elem_meta != expected_elem_meta {
                    return Err(PackedLayoutError);
                }
            }
            validate_packed_slice_backing(
                data,
                cursor,
                elem_meta,
                elem_rttid,
                elem_bytes,
                start,
                capacity,
                context,
                pointer_cache,
                tasks,
                layout_cache,
            )
        }
        ValueKind::Array => {
            let marker = *read_exact(data, cursor, 1)?
                .first()
                .ok_or(PackedLayoutError)?;
            let expected_inline_layout = array_value_layout(expected_meta, context, layout_cache);
            if expected_inline_layout.is_some() && marker != ARRAY_VALUE_INLINE_MARKER {
                return Err(PackedLayoutError);
            }
            if marker == ARRAY_VALUE_INLINE_MARKER {
                let length = validate_read_usize(data, cursor)?;
                let elem_meta = validate_read_value_meta(data, cursor)?;
                let elem_slots = validate_read_usize(data, cursor)?;
                let elem_rttid =
                    expected_rttid.and_then(|rttid| match context.resolve_runtime_type(rttid)? {
                        RuntimeType::Array { elem, .. } => Some(*elem),
                        _ => None,
                    });
                if let Some(layout) = expected_inline_layout {
                    if length != layout.len
                        || elem_meta != layout.elem_meta
                        || elem_slots != layout.elem_slots
                    {
                        return Err(PackedLayoutError);
                    }
                }
                tasks.push(ValidateTask::SequenceElements {
                    remaining: length,
                    elem_meta,
                    elem_rttid,
                });
                Ok(())
            } else {
                if marker == 0 {
                    return Ok(());
                }
                if marker != 1 {
                    return Err(PackedLayoutError);
                }
                let length = validate_read_usize(data, cursor)?;
                let elem_meta = validate_read_value_meta(data, cursor)?;
                let elem_bytes = validate_read_u32(data, cursor)? as usize;
                let elem_rttid =
                    expected_rttid.and_then(|rttid| match context.resolve_runtime_type(rttid)? {
                        RuntimeType::Array { elem, .. } => Some(*elem),
                        _ => None,
                    });
                if let Some(elem_rttid) = elem_rttid {
                    let expected_elem_meta = checked_expected_meta_for_rttid(elem_rttid, context)?;
                    if elem_meta != expected_elem_meta {
                        return Err(PackedLayoutError);
                    }
                }
                validate_packed_sequence(
                    data,
                    cursor,
                    PackedSequenceSpec {
                        elem_meta,
                        elem_rttid,
                        elem_bytes,
                        length,
                    },
                    context,
                    tasks,
                    layout_cache,
                )
            }
        }
        ValueKind::Struct => {
            let meta_id = validate_read_u32(data, cursor)? as usize;
            let slot_count = validate_read_u32(data, cursor)? as usize;
            if slot_count > u16::MAX as usize {
                return Err(PackedLayoutError);
            }
            if meta_id != expected_meta.meta_id() as usize {
                return Err(PackedLayoutError);
            }
            let meta = context.struct_metas.get(meta_id).ok_or(PackedLayoutError)?;
            let expected_slots = checked_struct_slot_count(meta).ok_or(PackedLayoutError)?;
            if slot_count != expected_slots {
                return Err(PackedLayoutError);
            }
            tasks.push(ValidateTask::StructFields {
                meta_id,
                field_index: 0,
            });
            Ok(())
        }
        ValueKind::Pointer => {
            let pointer_ref = validate_packed_pointer_ref(data, cursor)?;
            let inner_rttid =
                expected_rttid.and_then(|rttid| match context.resolve_runtime_type(rttid)? {
                    RuntimeType::Pointer(inner) => Some(*inner),
                    _ => None,
                });
            let expected_obj_meta = inner_rttid
                .map(|inner| expected_meta_for_rttid(inner, context))
                .unwrap_or_else(|| ValueMeta::new(expected_meta.meta_id(), ValueKind::Struct));

            let expected_slots = inner_rttid
                .and_then(|inner| layout_cache.slot_count(inner, context))
                .or_else(|| {
                    context
                        .struct_metas
                        .get(expected_obj_meta.meta_id() as usize)
                        .and_then(checked_struct_slot_count)
                })
                .ok_or(PackedLayoutError)?;

            let (object_id, offset) = match pointer_ref {
                PackedPointerRef::Null => return Ok(()),
                PackedPointerRef::BackReference { id, offset } => {
                    let allocation = pointer_cache
                        .allocations
                        .get(&id)
                        .copied()
                        .ok_or(PackedLayoutError)?;
                    return validate_pointer_into_allocation(
                        allocation,
                        offset,
                        expected_obj_meta,
                        expected_slots,
                        context,
                        layout_cache,
                    );
                }
                PackedPointerRef::Definition { id, offset } => (id, offset),
            };
            let allocation = validate_and_register_allocation_definition(
                data,
                cursor,
                object_id,
                context,
                pointer_cache,
                tasks,
                layout_cache,
            )?;
            validate_pointer_into_allocation(
                allocation,
                offset,
                expected_obj_meta,
                expected_slots,
                context,
                layout_cache,
            )
        }
        ValueKind::Map => {
            let marker = *read_exact(data, cursor, 1)?
                .first()
                .ok_or(PackedLayoutError)?;
            if marker == 0 {
                return Ok(());
            }
            let object_id = validate_read_u64(data, cursor)?;
            if object_id == 0 {
                return Err(PackedLayoutError);
            }
            let (key_rttid, val_rttid) = expected_rttid
                .and_then(|rttid| match context.resolve_runtime_type(rttid)? {
                    RuntimeType::Map { key, val } => Some((*key, *val)),
                    _ => None,
                })
                .map_or((None, None), |(key, val)| (Some(key), Some(val)));

            if marker == 2 {
                let Some(&(key_meta, val_meta, key_slots, val_slots)) =
                    pointer_cache.maps.get(&object_id)
                else {
                    return Err(PackedLayoutError);
                };
                if let Some(key_rttid) = key_rttid {
                    if key_meta != expected_meta_for_rttid(key_rttid, context)
                        || key_slots
                            != layout_cache
                                .slot_count(key_rttid, context)
                                .ok_or(PackedLayoutError)?
                    {
                        return Err(PackedLayoutError);
                    }
                }
                if let Some(val_rttid) = val_rttid {
                    if val_meta != expected_meta_for_rttid(val_rttid, context)
                        || val_slots
                            != layout_cache
                                .slot_count(val_rttid, context)
                                .ok_or(PackedLayoutError)?
                    {
                        return Err(PackedLayoutError);
                    }
                }
                return Ok(());
            }
            if marker != 1 || pointer_cache.maps.contains_key(&object_id) {
                return Err(PackedLayoutError);
            }

            let length = validate_read_usize(data, cursor)?;
            let key_meta = validate_read_value_meta(data, cursor)?;
            let val_meta = validate_read_value_meta(data, cursor)?;
            let key_slots = validate_read_u16(data, cursor)? as usize;
            let val_slots = validate_read_u16(data, cursor)? as usize;
            let _key_rttid = validate_read_u32(data, cursor)?;
            if let Some(key_rttid) = key_rttid {
                let expected_key_meta = expected_meta_for_rttid(key_rttid, context);
                if key_meta != expected_key_meta {
                    return Err(PackedLayoutError);
                }
                let expected_key_slots = layout_cache
                    .slot_count(key_rttid, context)
                    .ok_or(PackedLayoutError)?;
                if key_slots != expected_key_slots {
                    return Err(PackedLayoutError);
                }
            }
            if let Some(val_rttid) = val_rttid {
                let expected_val_meta = expected_meta_for_rttid(val_rttid, context);
                if val_meta != expected_val_meta {
                    return Err(PackedLayoutError);
                }
                let expected_val_slots = layout_cache
                    .slot_count(val_rttid, context)
                    .ok_or(PackedLayoutError)?;
                if val_slots != expected_val_slots {
                    return Err(PackedLayoutError);
                }
            }
            pointer_cache
                .maps
                .insert(object_id, (key_meta, val_meta, key_slots, val_slots));
            tasks.push(ValidateTask::MapEntries {
                remaining: length,
                key_meta,
                key_rttid,
                val_meta,
                val_rttid,
            });
            Ok(())
        }
        ValueKind::Channel | ValueKind::Port => unreachable!("queue kinds handled above"),
        ValueKind::Closure | ValueKind::Island | ValueKind::Interface => Err(PackedLayoutError),
    }
}

enum UnpackSequenceFinish {
    Array {
        dst: *mut u64,
    },
    Slice {
        dst: *mut u64,
        start: usize,
        length: usize,
        capacity: usize,
    },
    SliceView {
        object_id: u64,
        view: UnpackSliceView,
    },
    Pointer {
        dst: *mut u64,
        object_id: u64,
        offset: usize,
        expected_slots: usize,
    },
}

#[derive(Clone, Copy)]
struct UnpackSliceView {
    dst: *mut u64,
    backing_offset: usize,
    backing_len: usize,
    start: usize,
    length: usize,
    capacity: usize,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    storage_stride: usize,
    flat_storage: bool,
}

struct UnpackSequenceState {
    allocation: GcRef,
    data_ptr: *mut u8,
    length: usize,
    index: usize,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    mark_for_scan: bool,
    finish: UnpackSequenceFinish,
}

struct UnpackMapState {
    dst: *mut u64,
    map_ref: GcRef,
    remaining: usize,
    key_meta: ValueMeta,
    val_meta: ValueMeta,
    key_buf: Box<[u64]>,
    val_buf: Box<[u64]>,
    key_context_module: Option<Module>,
}

enum UnpackTask {
    Value {
        dst: *mut u64,
        dst_len: usize,
        expected_meta: Option<ValueMeta>,
    },
    SequenceNext(Box<UnpackSequenceState>),
    SequenceCommit {
        state: Box<UnpackSequenceState>,
        elem_buf: Box<[u64]>,
    },
    InlineArrayElements {
        dst: *mut u64,
        length: usize,
        index: usize,
        elem_meta: ValueMeta,
        elem_slots: usize,
    },
    StructFields {
        dst: *mut u64,
        dst_len: usize,
        meta_id: usize,
        field_index: usize,
    },
    PointerStructCommit {
        dst: *mut u64,
        object_id: u64,
        offset: usize,
        expected_slots: usize,
        allocation: GcRef,
        slots: Box<[u64]>,
    },
    SliceStructCommit {
        object_id: u64,
        allocation: GcRef,
        slots: Box<[u64]>,
        view: UnpackSliceView,
    },
    MapKey(Box<UnpackMapState>),
    MapValue(Box<UnpackMapState>),
    MapCommit(Box<UnpackMapState>),
}

#[allow(clippy::too_many_arguments)]
unsafe fn unpack_value<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    expected_meta: Option<ValueMeta>,
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    object_cache: &mut UnpackPointerCache,
    resolve_queue_handle: &mut F,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let mut layout_cache = RuntimeLayoutCache::default();
    let mut tasks = vec![UnpackTask::Value {
        dst: dst.as_mut_ptr(),
        dst_len: dst.len(),
        expected_meta,
    }];
    while let Some(task) = tasks.pop() {
        match task {
            UnpackTask::Value {
                dst,
                dst_len,
                expected_meta,
            } => {
                let dst = core::slice::from_raw_parts_mut(dst, dst_len);
                unpack_value_inner(
                    gc,
                    data,
                    cursor,
                    dst,
                    expected_meta,
                    context,
                    queue_handle_cache,
                    object_cache,
                    resolve_queue_handle,
                    &mut tasks,
                    &mut layout_cache,
                );
            }
            UnpackTask::SequenceNext(state) => {
                if state.index >= state.length {
                    if state.mark_for_scan {
                        gc.mark_allocated_for_scan(state.allocation);
                    }
                    finish_unpacked_sequence(gc, object_cache, state.allocation, state.finish);
                    continue;
                }
                let elem_slots = sequence_elem_slots(
                    state.elem_meta,
                    state.elem_bytes,
                    context,
                    &mut layout_cache,
                );
                let mut elem_buf = vec![0u64; elem_slots].into_boxed_slice();
                let elem_dst = elem_buf.as_mut_ptr();
                let elem_meta = state.elem_meta;
                tasks.push(UnpackTask::SequenceCommit { state, elem_buf });
                tasks.push(UnpackTask::Value {
                    dst: elem_dst,
                    dst_len: elem_slots,
                    expected_meta: Some(elem_meta),
                });
            }
            UnpackTask::SequenceCommit {
                mut state,
                elem_buf,
            } => {
                write_element(state.data_ptr, state.index, state.elem_bytes, &elem_buf);
                state.index += 1;
                tasks.push(UnpackTask::SequenceNext(state));
            }
            UnpackTask::InlineArrayElements {
                dst,
                length,
                index,
                elem_meta,
                elem_slots,
            } => {
                if index >= length {
                    continue;
                }
                tasks.push(UnpackTask::InlineArrayElements {
                    dst,
                    length,
                    index: index + 1,
                    elem_meta,
                    elem_slots,
                });
                tasks.push(UnpackTask::Value {
                    dst: dst.add(index * elem_slots),
                    dst_len: elem_slots,
                    expected_meta: Some(elem_meta),
                });
            }
            UnpackTask::StructFields {
                dst,
                dst_len,
                meta_id,
                field_index,
            } => {
                let meta = &context.struct_metas[meta_id];
                let Some(field) = meta.fields.get(field_index) else {
                    continue;
                };
                let field_slots = field.slot_count as usize;
                let field_start = field.offset as usize;
                let field_end = field_start
                    .checked_add(field_slots)
                    .filter(|end| *end <= dst_len)
                    .expect("unpack struct field range exceeds destination layout");
                tasks.push(UnpackTask::StructFields {
                    dst,
                    dst_len,
                    meta_id,
                    field_index: field_index + 1,
                });
                tasks.push(UnpackTask::Value {
                    dst: dst.add(field_start),
                    dst_len: field_end - field_start,
                    expected_meta: Some(expected_meta_for_rttid(field.type_info, context)),
                });
            }
            UnpackTask::PointerStructCommit {
                dst,
                object_id,
                offset,
                expected_slots,
                allocation,
                slots,
            } => {
                for (index, value) in slots.iter().copied().enumerate() {
                    Gc::write_slot(allocation, index, value);
                }
                gc.mark_allocated_for_scan(allocation);
                *dst =
                    unpacked_pointer_target(object_cache, object_id, offset, expected_slots) as u64;
            }
            UnpackTask::SliceStructCommit {
                object_id,
                allocation,
                slots,
                view,
            } => {
                for (index, value) in slots.iter().copied().enumerate() {
                    Gc::write_slot(allocation, index, value);
                }
                gc.mark_allocated_for_scan(allocation);
                *view.dst = unpacked_slice_view(gc, object_cache, object_id, view) as u64;
            }
            UnpackTask::MapKey(mut state) => {
                if state.remaining == 0 {
                    if state.key_meta.value_kind().may_contain_gc_refs()
                        || state.val_meta.value_kind().may_contain_gc_refs()
                    {
                        gc.mark_allocated_for_scan(state.map_ref);
                    }
                    *state.dst = state.map_ref as u64;
                    continue;
                }
                state.key_buf.fill(0);
                let key_dst = state.key_buf.as_mut_ptr();
                let key_slots = state.key_buf.len();
                let key_meta = state.key_meta;
                tasks.push(UnpackTask::MapValue(state));
                tasks.push(UnpackTask::Value {
                    dst: key_dst,
                    dst_len: key_slots,
                    expected_meta: Some(key_meta),
                });
            }
            UnpackTask::MapValue(mut state) => {
                state.val_buf.fill(0);
                let val_dst = state.val_buf.as_mut_ptr();
                let val_slots = state.val_buf.len();
                let val_meta = state.val_meta;
                tasks.push(UnpackTask::MapCommit(state));
                tasks.push(UnpackTask::Value {
                    dst: val_dst,
                    dst_len: val_slots,
                    expected_meta: Some(val_meta),
                });
            }
            UnpackTask::MapCommit(mut state) => {
                map::set_checked(
                    state.map_ref,
                    &state.key_buf,
                    &state.val_buf,
                    state.key_context_module.as_ref(),
                )
                .expect("packed map keys must be hashable");
                state.remaining -= 1;
                tasks.push(UnpackTask::MapKey(state));
            }
        }
    }
}

unsafe fn finish_unpacked_sequence(
    gc: &mut Gc,
    object_cache: &UnpackPointerCache,
    allocation: GcRef,
    finish: UnpackSequenceFinish,
) {
    match finish {
        UnpackSequenceFinish::Array { dst } => *dst = allocation as u64,
        UnpackSequenceFinish::Slice {
            dst,
            start,
            length,
            capacity,
        } => {
            *dst = slice::from_array_range_with_cap(gc, allocation, start, length, capacity) as u64;
        }
        UnpackSequenceFinish::SliceView { object_id, view } => {
            let cached = object_cache
                .allocations
                .get(&object_id)
                .expect("pack: missing unpacked slice owner allocation");
            assert_eq!(
                cached.base, allocation,
                "pack: slice owner allocation mismatch"
            );
            *view.dst = unpacked_slice_view(gc, object_cache, object_id, view) as u64;
        }
        UnpackSequenceFinish::Pointer {
            dst,
            object_id,
            offset,
            expected_slots,
        } => {
            *dst = unpacked_pointer_target(object_cache, object_id, offset, expected_slots) as u64;
        }
    }
}

unsafe fn unpacked_slice_view(
    gc: &mut Gc,
    object_cache: &UnpackPointerCache,
    object_id: u64,
    view: UnpackSliceView,
) -> GcRef {
    let allocation = object_cache
        .allocations
        .get(&object_id)
        .copied()
        .unwrap_or_else(|| panic!("pack: unknown slice owner allocation id {object_id}"));
    let backing_bytes = view
        .backing_len
        .checked_mul(view.storage_stride)
        .expect("pack: unpacked slice backing byte width overflow");
    assert!(
        view.backing_offset
            .checked_add(backing_bytes)
            .is_some_and(|end| end <= allocation.data_bytes),
        "pack: unpacked slice backing exceeds owner allocation"
    );
    let backing_ptr = if view.backing_offset == 0 {
        allocation.base as *mut u8
    } else {
        (allocation.base as *mut u8).add(view.backing_offset)
    };
    let slice_ref = slice::from_allocation_view_range_with_cap(
        gc,
        allocation.base,
        backing_ptr,
        view.backing_len,
        view.start,
        view.length,
        view.capacity,
        view.elem_meta,
        view.elem_bytes,
        view.storage_stride,
        view.flat_storage,
    );
    assert!(
        !slice_ref.is_null(),
        "pack: validated slice view could not be reconstructed"
    );
    slice_ref
}

#[allow(clippy::too_many_arguments)]
unsafe fn unpack_value_inner<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    expected_meta: Option<ValueMeta>,
    context: PackTypeContext<'_>,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    pointer_cache: &mut UnpackPointerCache,
    resolve_queue_handle: &mut F,
    tasks: &mut Vec<UnpackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    let vk = ValueKind::try_from(data[*cursor])
        .expect("unpack requires a previously validated packed value kind");
    *cursor += 1;
    if let Some(expected_meta) = expected_meta {
        let expected_kind = expected_meta.value_kind();
        if vk != expected_kind {
            panic!("pack: unpack value kind mismatch: expected {expected_kind:?}, got {vk:?}");
        }
    }

    if vk.is_queue() {
        dst[0] =
            unpack_queue_handle(gc, data, cursor, queue_handle_cache, resolve_queue_handle) as u64;
    } else {
        match vk {
            ValueKind::Void => {}
            ValueKind::Bool
            | ValueKind::Int
            | ValueKind::Int8
            | ValueKind::Int16
            | ValueKind::Int32
            | ValueKind::Int64
            | ValueKind::Uint
            | ValueKind::Uint8
            | ValueKind::Uint16
            | ValueKind::Uint32
            | ValueKind::Uint64
            | ValueKind::Float32
            | ValueKind::Float64 => {
                let raw = read_u64(data, cursor);
                dst[0] = canonical_scalar_slot(vk, raw)
                    .expect("unpack requires a canonical packed scalar value");
            }

            ValueKind::String => {
                dst[0] = unpack_string(gc, data, cursor) as u64;
            }

            ValueKind::Slice => {
                unpack_slice(
                    gc,
                    data,
                    cursor,
                    dst.as_mut_ptr(),
                    context,
                    pointer_cache,
                    tasks,
                    layout_cache,
                );
            }

            ValueKind::Array => {
                let marker = data[*cursor];
                if expected_meta
                    .and_then(|meta| array_value_layout(meta, context, layout_cache))
                    .is_some()
                    && marker != ARRAY_VALUE_INLINE_MARKER
                {
                    panic!("pack: fixed array value requires inline array encoding");
                }
                if marker == ARRAY_VALUE_INLINE_MARKER {
                    *cursor += 1;
                    unpack_array_value_inline(
                        data,
                        cursor,
                        dst,
                        expected_meta,
                        context,
                        tasks,
                        layout_cache,
                    );
                } else {
                    unpack_array(
                        gc,
                        data,
                        cursor,
                        dst.as_mut_ptr(),
                        context,
                        tasks,
                        layout_cache,
                    );
                }
            }

            ValueKind::Struct => {
                unpack_struct_inline(data, cursor, dst, expected_meta, context, tasks);
            }

            ValueKind::Pointer => {
                unpack_pointer(
                    gc,
                    data,
                    cursor,
                    dst.as_mut_ptr(),
                    expected_meta,
                    context,
                    pointer_cache,
                    tasks,
                    layout_cache,
                );
            }

            ValueKind::Map => {
                unpack_map(
                    gc,
                    data,
                    cursor,
                    dst.as_mut_ptr(),
                    context,
                    pointer_cache,
                    tasks,
                );
            }

            ValueKind::Channel | ValueKind::Port => unreachable!("queue kinds handled above"),
            _ => panic!("Cannot unpack non-sendable type: {:?}", vk),
        }
    }
}

fn unpack_string(gc: &mut Gc, data: &[u8], cursor: &mut usize) -> GcRef {
    let len = read_u64(data, cursor) as usize;
    let bytes = &data[*cursor..*cursor + len];
    *cursor += len;
    string::create(gc, bytes)
}

#[allow(clippy::too_many_arguments)]
unsafe fn unpack_slice(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: *mut u64,
    context: PackTypeContext<'_>,
    object_cache: &mut UnpackPointerCache,
    tasks: &mut Vec<UnpackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    if !read_reference_marker(data, cursor, "slice") {
        *dst = 0;
        return;
    }

    let length = read_u64(data, cursor) as usize;
    let capacity = read_u64(data, cursor) as usize;
    let start = read_u64(data, cursor) as usize;
    assert!(length <= capacity, "pack: slice length exceeds capacity");
    let elem_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let elem_bytes = read_u32(data, cursor) as usize;
    validate_sequence_elem_layout(elem_meta, elem_bytes, context, layout_cache);
    let marker = data[*cursor];
    *cursor += 1;
    let object_id = read_u64(data, cursor);
    if marker == SLICE_BACKING_DEFINITION {
        assert!(
            object_id != 0 && !object_cache.allocations.contains_key(&object_id),
            "pack: duplicate or invalid slice backing definition id {object_id}"
        );
        let backing = unpack_array_allocation_payload(
            gc,
            data,
            cursor,
            object_id,
            Some((elem_meta, elem_bytes)),
            context,
            object_cache,
            UnpackSequenceFinish::Slice {
                dst,
                start,
                length,
                capacity,
            },
            tasks,
            layout_cache,
        );
        let backing_len = array::len(backing);
        assert!(
            start <= backing_len && capacity <= backing_len - start,
            "pack: slice range exceeds backing array"
        );
        return;
    }

    if marker == SLICE_BACKING_BACK_REFERENCE {
        let allocation = *object_cache
            .allocations
            .get(&object_id)
            .unwrap_or_else(|| panic!("pack: unknown slice backing back-reference id {object_id}"));
        let UnpackedAllocationKind::Array {
            elem_meta: cached_elem_meta,
            elem_bytes: cached_elem_bytes,
        } = allocation.kind
        else {
            panic!("pack: slice backing back-reference is not an array")
        };
        assert_eq!(cached_elem_meta, elem_meta);
        assert_eq!(cached_elem_bytes, elem_bytes);
        let backing_len = array::len(allocation.base);
        assert!(
            start <= backing_len && capacity <= backing_len - start,
            "pack: slice range exceeds backing array"
        );
        *dst =
            slice::from_array_range_with_cap(gc, allocation.base, start, length, capacity) as u64;
        return;
    }

    assert!(
        matches!(marker, SLICE_OWNER_DEFINITION | SLICE_OWNER_BACK_REFERENCE),
        "pack: invalid slice backing marker {marker}"
    );
    let backing_offset = read_u64(data, cursor) as usize;
    let backing_len = read_u64(data, cursor) as usize;
    let storage_stride = read_u64(data, cursor) as usize;
    let flat_storage = match data[*cursor] {
        0 => false,
        1 => true,
        marker => panic!("pack: invalid slice storage mode marker {marker}"),
    };
    *cursor += 1;
    assert!(
        start <= backing_len && capacity <= backing_len - start,
        "pack: slice range exceeds owner backing"
    );
    let view = UnpackSliceView {
        dst,
        backing_offset,
        backing_len,
        start,
        length,
        capacity,
        elem_meta,
        elem_bytes,
        storage_stride,
        flat_storage,
    };
    if marker == SLICE_OWNER_BACK_REFERENCE {
        *dst = unpacked_slice_view(gc, object_cache, object_id, view) as u64;
        return;
    }

    assert!(
        object_id != 0 && !object_cache.allocations.contains_key(&object_id),
        "pack: duplicate or invalid slice owner definition id {object_id}"
    );
    let allocation_kind = data[*cursor];
    *cursor += 1;
    match allocation_kind {
        ALLOCATION_KIND_STRUCT => {
            let allocation_meta = ValueMeta::from_raw(read_u32(data, cursor));
            let slots = read_u32(data, cursor) as usize;
            let slots_u16 = u16::try_from(slots)
                .expect("unpack_slice: owner allocation slot count exceeds u16::MAX");
            let allocation = gc.alloc(allocation_meta, slots_u16);
            let data_bytes = gc
                .allocated_data_size_bytes(allocation)
                .expect("unpacked slice owner allocation size");
            object_cache.allocations.insert(
                object_id,
                UnpackedAllocation {
                    base: allocation,
                    data_bytes,
                    kind: UnpackedAllocationKind::Struct,
                },
            );
            let mut slots = vec![0u64; slots].into_boxed_slice();
            let slots_dst = slots.as_mut_ptr();
            let slots_len = slots.len();
            tasks.push(UnpackTask::SliceStructCommit {
                object_id,
                allocation,
                slots,
                view,
            });
            tasks.push(UnpackTask::Value {
                dst: slots_dst,
                dst_len: slots_len,
                expected_meta: Some(allocation_meta),
            });
        }
        ALLOCATION_KIND_ARRAY => {
            unpack_array_allocation_payload(
                gc,
                data,
                cursor,
                object_id,
                None,
                context,
                object_cache,
                UnpackSequenceFinish::SliceView { object_id, view },
                tasks,
                layout_cache,
            );
        }
        _ => panic!("pack: invalid slice owner allocation kind {allocation_kind}"),
    }
}

#[allow(clippy::too_many_arguments)]
unsafe fn unpack_array_allocation_payload(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    object_id: u64,
    expected_layout: Option<(ValueMeta, usize)>,
    context: PackTypeContext<'_>,
    object_cache: &mut UnpackPointerCache,
    finish: UnpackSequenceFinish,
    tasks: &mut Vec<UnpackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) -> GcRef {
    let backing_len = read_u64(data, cursor) as usize;
    let elem_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let elem_bytes = read_u32(data, cursor) as usize;
    if let Some((expected_elem_meta, expected_elem_bytes)) = expected_layout {
        assert_eq!(
            elem_meta, expected_elem_meta,
            "pack: slice backing metadata mismatch"
        );
        assert_eq!(
            elem_bytes, expected_elem_bytes,
            "pack: slice backing element width mismatch"
        );
    }
    validate_sequence_elem_layout(elem_meta, elem_bytes, context, layout_cache);

    let backing = array::create(gc, elem_meta, elem_bytes, backing_len);
    let data_bytes = gc
        .allocated_data_size_bytes(backing)
        .expect("unpacked array allocation size");
    object_cache.allocations.insert(
        object_id,
        UnpackedAllocation {
            base: backing,
            data_bytes,
            kind: UnpackedAllocationKind::Array {
                elem_meta,
                elem_bytes,
            },
        },
    );
    schedule_unpack_sequence(
        data,
        cursor,
        backing,
        backing_len,
        elem_meta,
        elem_bytes,
        finish,
        tasks,
    );
    backing
}

#[allow(clippy::too_many_arguments)]
unsafe fn schedule_unpack_sequence(
    data: &[u8],
    cursor: &mut usize,
    allocation: GcRef,
    length: usize,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    finish: UnpackSequenceFinish,
    tasks: &mut Vec<UnpackTask>,
) {
    let data_ptr = array::data_ptr_bytes(allocation);
    if elem_bytes == 0 {
        tasks.push(UnpackTask::SequenceNext(Box::new(UnpackSequenceState {
            allocation,
            data_ptr,
            length,
            index: length,
            elem_meta,
            elem_bytes,
            mark_for_scan: false,
            finish,
        })));
        return;
    }

    let encoding = data[*cursor];
    *cursor += 1;
    if encoding == SEQUENCE_ENCODING_RAW_BYTES {
        let byte_len = length
            .checked_mul(elem_bytes)
            .expect("unpack raw sequence byte length overflow");
        unpack_raw_sequence_bytes(data, cursor, data_ptr, byte_len);
        tasks.push(UnpackTask::SequenceNext(Box::new(UnpackSequenceState {
            allocation,
            data_ptr,
            length,
            index: length,
            elem_meta,
            elem_bytes,
            mark_for_scan: false,
            finish,
        })));
        return;
    }
    assert_eq!(
        encoding, SEQUENCE_ENCODING_ELEMENTS,
        "pack: invalid sequence encoding {encoding}"
    );
    tasks.push(UnpackTask::SequenceNext(Box::new(UnpackSequenceState {
        allocation,
        data_ptr,
        length,
        index: 0,
        elem_meta,
        elem_bytes,
        mark_for_scan: elem_meta.value_kind().may_contain_gc_refs(),
        finish,
    })));
}

unsafe fn unpack_array(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: *mut u64,
    context: PackTypeContext<'_>,
    tasks: &mut Vec<UnpackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    if !read_reference_marker(data, cursor, "array") {
        *dst = 0;
        return;
    }

    let length = read_u64(data, cursor) as usize;
    let elem_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let elem_bytes = read_u32(data, cursor) as usize;
    validate_sequence_elem_layout(elem_meta, elem_bytes, context, layout_cache);

    let new_arr = array::create(gc, elem_meta, elem_bytes, length);
    schedule_unpack_sequence(
        data,
        cursor,
        new_arr,
        length,
        elem_meta,
        elem_bytes,
        UnpackSequenceFinish::Array { dst },
        tasks,
    );
}

#[allow(clippy::too_many_arguments)]
unsafe fn unpack_array_value_inline(
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    expected_meta: Option<ValueMeta>,
    context: PackTypeContext<'_>,
    tasks: &mut Vec<UnpackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    let length = read_u64(data, cursor) as usize;
    let elem_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let elem_slots = read_u64(data, cursor) as usize;
    if let Some(layout) =
        expected_meta.and_then(|meta| array_value_layout(meta, context, layout_cache))
    {
        if length != layout.len || elem_meta != layout.elem_meta || elem_slots != layout.elem_slots
        {
            panic!(
                "unpack array value layout mismatch: expected len {} meta {:?} elem_slots {}, got len {length} meta {:?} elem_slots {elem_slots}",
                layout.len,
                layout.elem_meta,
                layout.elem_slots,
                elem_meta
            );
        }
    }
    let expected_slots = length
        .checked_mul(elem_slots)
        .expect("unpack array value slot length overflow");
    if dst.len() != expected_slots {
        panic!(
            "unpack array value slot length mismatch: expected exactly {expected_slots}, got {}",
            dst.len()
        );
    }

    tasks.push(UnpackTask::InlineArrayElements {
        dst: dst.as_mut_ptr(),
        length,
        index: 0,
        elem_meta,
        elem_slots,
    });
}

#[allow(clippy::too_many_arguments)]
unsafe fn unpack_struct_inline(
    data: &[u8],
    cursor: &mut usize,
    dst: &mut [u64],
    expected_meta: Option<ValueMeta>,
    context: PackTypeContext<'_>,
    tasks: &mut Vec<UnpackTask>,
) {
    let meta_id = read_u32(data, cursor) as usize;
    let slot_count = read_u32(data, cursor) as usize;
    if let Some(expected_meta) = expected_meta {
        let expected_meta_id = expected_meta.meta_id() as usize;
        if meta_id != expected_meta_id {
            panic!("unpack struct meta_id mismatch: expected {expected_meta_id}, got {meta_id}");
        }
    }

    if meta_id >= context.struct_metas.len() {
        panic!("Invalid struct meta_id during unpack: {}", meta_id);
    }

    let meta = &context.struct_metas[meta_id];
    if slot_count != meta.slot_types.len() || dst.len() != meta.slot_types.len() {
        panic!(
            "unpack struct slot count mismatch: expected {}, wire {}, dst {}",
            meta.slot_types.len(),
            slot_count,
            dst.len()
        );
    }

    tasks.push(UnpackTask::StructFields {
        dst: dst.as_mut_ptr(),
        dst_len: dst.len(),
        meta_id,
        field_index: 0,
    });
}

#[allow(clippy::too_many_arguments)]
unsafe fn unpack_pointer(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: *mut u64,
    expected_meta: Option<ValueMeta>,
    context: PackTypeContext<'_>,
    pointer_cache: &mut UnpackPointerCache,
    tasks: &mut Vec<UnpackTask>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    let expected_slots = expected_meta
        .and_then(|meta| context.struct_metas.get(meta.meta_id() as usize))
        .map(|meta| meta.slot_types.len())
        .unwrap_or(0);
    let (object_id, offset, is_definition) = match read_packed_pointer_ref(data, cursor) {
        PackedPointerRef::Null => {
            *dst = 0;
            return;
        }
        PackedPointerRef::BackReference { id, offset } => (id, offset, false),
        PackedPointerRef::Definition { id, offset } => (id, offset, true),
    };
    if object_id == 0 {
        panic!("pack: duplicate or invalid pointer definition id {object_id}");
    }

    if is_definition {
        assert!(
            !pointer_cache.allocations.contains_key(&object_id),
            "pack: duplicate pointer allocation definition id {object_id}"
        );
        let allocation_kind = data[*cursor];
        *cursor += 1;
        match allocation_kind {
            ALLOCATION_KIND_STRUCT => {
                let allocation_meta = ValueMeta::from_raw(read_u32(data, cursor));
                let slots = read_u32(data, cursor) as usize;
                let slots_u16 = u16::try_from(slots)
                    .expect("unpack_pointer: allocation slot count exceeds u16::MAX");
                let new_obj = gc.alloc(allocation_meta, slots_u16);
                let data_bytes = gc
                    .allocated_data_size_bytes(new_obj)
                    .expect("unpacked pointer allocation size");
                pointer_cache.allocations.insert(
                    object_id,
                    UnpackedAllocation {
                        base: new_obj,
                        data_bytes,
                        kind: UnpackedAllocationKind::Struct,
                    },
                );
                let mut slots = vec![0u64; slots].into_boxed_slice();
                let slots_dst = slots.as_mut_ptr();
                let slots_len = slots.len();
                tasks.push(UnpackTask::PointerStructCommit {
                    dst,
                    object_id,
                    offset,
                    expected_slots,
                    allocation: new_obj,
                    slots,
                });
                tasks.push(UnpackTask::Value {
                    dst: slots_dst,
                    dst_len: slots_len,
                    expected_meta: Some(allocation_meta),
                });
                return;
            }
            ALLOCATION_KIND_ARRAY => {
                unpack_array_allocation_payload(
                    gc,
                    data,
                    cursor,
                    object_id,
                    None,
                    context,
                    pointer_cache,
                    UnpackSequenceFinish::Pointer {
                        dst,
                        object_id,
                        offset,
                        expected_slots,
                    },
                    tasks,
                    layout_cache,
                );
                return;
            }
            _ => panic!("pack: invalid pointer allocation kind {allocation_kind}"),
        }
    }

    *dst = unpacked_pointer_target(pointer_cache, object_id, offset, expected_slots) as u64;
}

unsafe fn unpacked_pointer_target(
    pointer_cache: &UnpackPointerCache,
    object_id: u64,
    offset: usize,
    expected_slots: usize,
) -> GcRef {
    let allocation = pointer_cache
        .allocations
        .get(&object_id)
        .copied()
        .unwrap_or_else(|| panic!("pack: unknown pointer allocation id {object_id}"));
    assert_eq!(
        offset % SLOT_BYTES,
        0,
        "pack: pointer offset is not aligned"
    );
    if matches!(allocation.kind, UnpackedAllocationKind::Array { .. }) {
        assert!(
            offset >= array::HEADER_SLOTS * SLOT_BYTES,
            "pack: pointer target overlaps array metadata"
        );
    }
    let width = expected_slots
        .checked_mul(SLOT_BYTES)
        .expect("pack: pointer target width overflow");
    assert!(
        offset
            .checked_add(width)
            .is_some_and(|end| end <= allocation.data_bytes),
        "pack: pointer target exceeds owning allocation"
    );
    unsafe { (allocation.base as *mut u8).add(offset) as GcRef }
}

unsafe fn unpack_map(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    dst: *mut u64,
    context: PackTypeContext<'_>,
    pointer_cache: &mut UnpackPointerCache,
    tasks: &mut Vec<UnpackTask>,
) {
    let marker = data[*cursor];
    *cursor += 1;
    if marker == 0 {
        *dst = 0;
        return;
    }
    let object_id = read_u64(data, cursor);
    if marker == 2 {
        *dst = *pointer_cache
            .maps
            .get(&object_id)
            .unwrap_or_else(|| panic!("pack: unknown map back-reference id {object_id}"))
            as u64;
        return;
    }
    assert_eq!(marker, 1, "pack: invalid map reference marker {marker}");
    assert!(
        object_id != 0 && !pointer_cache.maps.contains_key(&object_id),
        "pack: duplicate or invalid map definition id {object_id}"
    );

    let length = read_u64(data, cursor) as usize;
    let key_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let val_meta = ValueMeta::from_raw(read_u32(data, cursor));
    let key_slots = read_u16(data, cursor);
    let val_slots = read_u16(data, cursor);
    let key_rttid = read_u32(data, cursor);

    // Create new map
    let new_map = map::create(gc, key_meta, val_meta, key_slots, val_slots, key_rttid);
    pointer_cache.maps.insert(object_id, new_map);

    tasks.push(UnpackTask::MapKey(Box::new(UnpackMapState {
        dst,
        map_ref: new_map,
        remaining: length,
        key_meta,
        val_meta,
        key_buf: vec![0u64; key_slots as usize].into_boxed_slice(),
        val_buf: vec![0u64; val_slots as usize].into_boxed_slice(),
        key_context_module: map_key_context_module(key_meta, context),
    })));
}

// =============================================================================
// Channel Handle Pack/Unpack
// =============================================================================

/// Pack a channel handle for cross-island transfer.
/// Serializes: endpoint_id(8) + home_island(4) + cap(8) + meta_raw(4) + elem_slots(2) + closed(1)
/// LOCAL channels MUST have HomeInfo installed before packing (call prepare_chans first).
unsafe fn pack_queue_handle(packed: &mut PackedValue, chan_ref: GcRef) {
    if chan_ref.is_null() {
        packed.push_encoded(0); // null marker
        return;
    }
    packed.push_encoded(1); // non-null marker
    pack_queue_handle_inner(packed, chan_ref);
}

unsafe fn pack_queue_handle_inner(packed: &mut PackedValue, chan_ref: GcRef) {
    let (kind, cap, elem_meta, elem_rttid, elem_slots) = queue::get_metadata(chan_ref);
    if kind != QueueKind::Port {
        panic!("pack_queue_handle: {:?} cannot cross islands", kind);
    }

    let (endpoint_id, home_island, closed) = if queue::is_remote(chan_ref) {
        let proxy = unsafe { queue::remote_proxy(chan_ref) };
        (proxy.endpoint_id, proxy.home_island, proxy.closed)
    } else {
        match unsafe { queue::home_info(chan_ref) } {
            Some(info) => (info.endpoint_id, info.home_island, queue::is_closed(chan_ref) && queue::len(chan_ref) == 0),
            None => panic!("pack_chan_handle: LOCAL channel without HomeInfo — call prepare_value_chans_for_transfer first"),
        }
    };

    packed.push_encoded(kind.value_kind() as u8);
    packed.extend_encoded(&endpoint_id.to_le_bytes());
    packed.extend_encoded(&home_island.to_le_bytes());
    packed.extend_encoded(&cap.to_le_bytes());
    packed.extend_encoded(&elem_meta.to_raw().to_le_bytes());
    packed.extend_encoded(&elem_rttid.to_raw().to_le_bytes());
    packed.extend_encoded(&elem_slots.to_le_bytes());
    packed.push_encoded(closed as u8);
}

/// Unpack a channel handle — creates a REMOTE proxy on the destination island.
fn default_unpack_queue_handle(gc: &mut Gc, handle: QueueHandleInfo) -> GcRef {
    if handle.kind != QueueKind::Port {
        return core::ptr::null_mut();
    }
    queue::create_remote_proxy_with_closed(
        gc,
        handle.endpoint_id,
        handle.home_island,
        handle.cap,
        handle.elem_meta,
        handle.elem_rttid,
        handle.elem_slots,
        handle.closed,
    )
}

fn unpack_queue_handle<F>(
    gc: &mut Gc,
    data: &[u8],
    cursor: &mut usize,
    queue_handle_cache: &mut UnpackQueueHandleCache,
    resolve_queue_handle: &mut F,
) -> GcRef
where
    F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
{
    if !read_reference_marker(data, cursor, "queue") {
        return core::ptr::null_mut();
    }

    let value_kind = ValueKind::try_from(data[*cursor])
        .expect("unpack requires a previously validated packed queue kind");
    let kind = QueueKind::from_value_kind(value_kind);
    *cursor += 1;
    let endpoint_id = read_u64(data, cursor);
    let home_island = read_u32(data, cursor);
    let cap = read_u64(data, cursor);
    let meta_raw = read_u32(data, cursor);
    let rttid_raw = read_u32(data, cursor);
    let elem_slots = read_u16(data, cursor);
    let closed = data[*cursor] != 0;
    *cursor += 1;

    let handle = QueueHandleInfo {
        kind,
        endpoint_id,
        home_island,
        cap,
        elem_meta: ValueMeta::from_raw(meta_raw),
        elem_rttid: ValueRttid::from_raw(rttid_raw),
        elem_slots,
        closed,
    };

    if let Some(existing) = queue_handle_cache.get(&handle.endpoint_id) {
        if existing.handle == handle {
            return existing.chan_ref;
        }
    }
    let chan_ref = resolve_queue_handle(gc, handle);
    queue_handle_cache.insert(handle.endpoint_id, CachedQueueHandle { handle, chan_ref });
    chan_ref
}

// =============================================================================
// Helper Functions
// =============================================================================

fn can_pack_sequence_as_raw_bytes(elem_meta: ValueMeta, elem_bytes: usize) -> bool {
    if elem_bytes != 1 {
        return false;
    }
    matches!(
        elem_meta.value_kind(),
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8
    )
}

fn sequence_elem_slots(
    elem_meta: ValueMeta,
    elem_bytes: usize,
    context: PackTypeContext<'_>,
    layout_cache: &mut RuntimeLayoutCache,
) -> usize {
    match sequence_elem_layout_checked(elem_meta, elem_bytes, context, layout_cache) {
        Ok(layout) => layout.logical_slots,
        Err(err) => panic!(
            "pack: sequence {:?} element byte width mismatch: expected {}, got {}",
            elem_meta.value_kind(),
            err.expected_bytes,
            err.actual_bytes
        ),
    }
}

fn validate_sequence_elem_layout(
    elem_meta: ValueMeta,
    elem_bytes: usize,
    context: PackTypeContext<'_>,
    layout_cache: &mut RuntimeLayoutCache,
) {
    let _ = sequence_elem_slots(elem_meta, elem_bytes, context, layout_cache);
}

fn sequence_elem_layout_checked(
    elem_meta: ValueMeta,
    elem_bytes: usize,
    context: PackTypeContext<'_>,
    layout_cache: &mut RuntimeLayoutCache,
) -> Result<SequenceElementLayout, SequenceLayoutError> {
    let layout = match elem_meta.value_kind() {
        ValueKind::Void => SequenceElementLayout {
            logical_slots: 0,
            physical_bytes: 0,
        },
        ValueKind::Struct => {
            let meta_id = elem_meta.meta_id() as usize;
            let Some(meta) = context.struct_metas.get(meta_id) else {
                return Err(SequenceLayoutError {
                    expected_bytes: 0,
                    actual_bytes: elem_bytes,
                });
            };
            let Some(logical_slots) = checked_struct_slot_count(meta) else {
                return Err(SequenceLayoutError {
                    expected_bytes: 0,
                    actual_bytes: elem_bytes,
                });
            };
            let physical_bytes = if meta.fields.is_empty() {
                0
            } else {
                logical_slots
                    .checked_mul(SLOT_BYTES)
                    .ok_or(SequenceLayoutError {
                        expected_bytes: 0,
                        actual_bytes: elem_bytes,
                    })?
            };
            SequenceElementLayout {
                logical_slots,
                physical_bytes,
            }
        }
        ValueKind::Array => {
            let Some(layout) = array_value_layout(elem_meta, context, layout_cache) else {
                return Err(SequenceLayoutError {
                    expected_bytes: 0,
                    actual_bytes: elem_bytes,
                });
            };
            let elem_slots =
                layout
                    .len
                    .checked_mul(layout.elem_slots)
                    .ok_or(SequenceLayoutError {
                        expected_bytes: 0,
                        actual_bytes: elem_bytes,
                    })?;
            SequenceElementLayout {
                logical_slots: elem_slots,
                physical_bytes: elem_slots
                    .checked_mul(SLOT_BYTES)
                    .ok_or(SequenceLayoutError {
                        expected_bytes: 0,
                        actual_bytes: elem_bytes,
                    })?,
            }
        }
        ValueKind::Interface => SequenceElementLayout {
            logical_slots: 2,
            physical_bytes: 2 * SLOT_BYTES,
        },
        kind if kind.is_queue()
            || matches!(
                kind,
                ValueKind::String
                    | ValueKind::Slice
                    | ValueKind::Map
                    | ValueKind::Pointer
                    | ValueKind::Closure
                    | ValueKind::Island
            ) =>
        {
            SequenceElementLayout {
                logical_slots: 1,
                physical_bytes: SLOT_BYTES,
            }
        }
        kind => {
            let bytes = kind.elem_bytes();
            SequenceElementLayout {
                logical_slots: bytes.div_ceil(SLOT_BYTES),
                physical_bytes: bytes,
            }
        }
    };
    if elem_bytes != layout.physical_bytes {
        return Err(SequenceLayoutError {
            expected_bytes: layout.physical_bytes,
            actual_bytes: elem_bytes,
        });
    }
    Ok(layout)
}

fn pack_raw_sequence_bytes(
    packed: &mut PackedValue,
    data_ptr: *mut u8,
    length: usize,
    elem_bytes: usize,
) {
    let byte_len = length
        .checked_mul(elem_bytes)
        .expect("pack raw sequence byte length overflow");
    if byte_len == 0 {
        return;
    }
    let bytes = unsafe { core::slice::from_raw_parts(data_ptr as *const u8, byte_len) };
    packed.extend_encoded(bytes);
}

fn unpack_raw_sequence_bytes(data: &[u8], cursor: &mut usize, dst: *mut u8, byte_len: usize) {
    if byte_len == 0 {
        return;
    }
    let end = *cursor + byte_len;
    unsafe {
        core::ptr::copy_nonoverlapping(data[*cursor..end].as_ptr(), dst, byte_len);
    }
    *cursor = end;
}

fn read_u64(data: &[u8], cursor: &mut usize) -> u64 {
    let bytes: [u8; 8] = data[*cursor..*cursor + 8]
        .try_into()
        .expect("pack: insufficient data for u64");
    *cursor += 8;
    u64::from_le_bytes(bytes)
}

fn read_u32(data: &[u8], cursor: &mut usize) -> u32 {
    let bytes: [u8; 4] = data[*cursor..*cursor + 4]
        .try_into()
        .expect("pack: insufficient data for u32");
    *cursor += 4;
    u32::from_le_bytes(bytes)
}

fn read_u16(data: &[u8], cursor: &mut usize) -> u16 {
    let bytes: [u8; 2] = data[*cursor..*cursor + 2]
        .try_into()
        .expect("pack: insufficient data for u16");
    *cursor += 2;
    u16::from_le_bytes(bytes)
}

fn read_reference_marker(data: &[u8], cursor: &mut usize, kind: &str) -> bool {
    let marker = *data
        .get(*cursor)
        .unwrap_or_else(|| panic!("pack: insufficient data for {kind} reference marker"));
    *cursor += 1;
    match marker {
        0 => false,
        1 => true,
        _ => panic!("pack: invalid {kind} reference marker {marker}"),
    }
}

fn read_packed_pointer_ref(data: &[u8], cursor: &mut usize) -> PackedPointerRef {
    let marker = *data
        .get(*cursor)
        .unwrap_or_else(|| panic!("pack: insufficient data for pointer reference marker"));
    *cursor += 1;
    match marker {
        0 => PackedPointerRef::Null,
        1 => PackedPointerRef::Definition {
            id: read_u64(data, cursor),
            offset: usize::try_from(read_u64(data, cursor))
                .expect("pack: pointer offset exceeds target address space"),
        },
        2 => PackedPointerRef::BackReference {
            id: read_u64(data, cursor),
            offset: usize::try_from(read_u64(data, cursor))
                .expect("pack: pointer offset exceeds target address space"),
        },
        _ => panic!("pack: invalid pointer reference marker {marker}"),
    }
}

fn read_element(
    base_ptr: *mut u8,
    idx: usize,
    elem_bytes: usize,
    elem_meta: ValueMeta,
    dst: &mut [u64],
) {
    let ptr = unsafe { base_ptr.add(idx * elem_bytes) };
    let vk = elem_meta.value_kind();

    match elem_bytes {
        1 => {
            let v = unsafe { *ptr };
            // Sign extend for Int8
            dst[0] = if vk == ValueKind::Int8 {
                (v as i8) as i64 as u64
            } else {
                v as u64
            };
        }
        2 => {
            let v = unsafe { *(ptr as *const u16) };
            // Sign extend for Int16
            dst[0] = if vk == ValueKind::Int16 {
                (v as i16) as i64 as u64
            } else {
                v as u64
            };
        }
        4 => {
            let v = unsafe { *(ptr as *const u32) };
            // Sign extend for Int32, keep bits for Float32
            dst[0] = if vk == ValueKind::Int32 {
                (v as i32) as i64 as u64
            } else {
                v as u64
            };
        }
        8 => dst[0] = unsafe { *(ptr as *const u64) },
        _ => {
            let dst_bytes = dst.as_mut_ptr() as *mut u8;
            unsafe { core::ptr::copy_nonoverlapping(ptr, dst_bytes, elem_bytes) };
        }
    }
}

fn write_element(base_ptr: *mut u8, idx: usize, elem_bytes: usize, src: &[u64]) {
    let ptr = unsafe { base_ptr.add(idx * elem_bytes) };
    match elem_bytes {
        1 => unsafe { *ptr = src[0] as u8 },
        2 => unsafe { *(ptr as *mut u16) = src[0] as u16 },
        4 => unsafe { *(ptr as *mut u32) = src[0] as u32 },
        8 => unsafe { *(ptr as *mut u64) = src[0] },
        _ => {
            let src_bytes = src.as_ptr() as *const u8;
            unsafe { core::ptr::copy_nonoverlapping(src_bytes, ptr, elem_bytes) };
        }
    }
}

#[cfg(test)]
mod tests;
