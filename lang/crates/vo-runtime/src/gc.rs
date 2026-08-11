//! Garbage collector core.
#![allow(clippy::items_after_test_module)]

mod heap;

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use heap::{HeapError, HeapStats, SpanHeap};

use crate::slot::{Slot, SLOT_BYTES};
use vo_common_core::types::{ValueKind, ValueMeta};

/// Host-owned operations used by the native-extension GC facade.
///
/// Native extensions receive a private, allocation-free `Gc` proxy. Any
/// operation that can mutate collector-owned storage is dispatched back into
/// the host so Rust allocations are always created and destroyed by the same
/// allocator image.
#[derive(Clone, Copy)]
pub(crate) struct GcOwnerDispatch {
    pub state: *mut core::ffi::c_void,
    pub alloc: unsafe extern "C" fn(
        state: *mut core::ffi::c_void,
        value_meta: u32,
        allocation_kind: u8,
        header_slots: u16,
        total_slots: usize,
    ) -> GcRef,
    pub canonicalize: unsafe extern "C" fn(state: *mut core::ffi::c_void, obj: GcRef) -> GcRef,
    pub mark_gray: unsafe extern "C" fn(state: *mut core::ffi::c_void, obj: GcRef),
    pub mark_allocated_for_scan: unsafe extern "C" fn(state: *mut core::ffi::c_void, obj: GcRef),
    pub write_barrier:
        unsafe extern "C" fn(state: *mut core::ffi::c_void, parent: GcRef, child: GcRef),
}

pub(crate) const GC_OWNER_ALLOC_OBJECT: u8 = 0;
pub(crate) const GC_OWNER_ALLOC_ARRAY: u8 = 1;
pub(crate) const GC_OWNER_ALLOC_VALUE_SLOTS: u8 = 2;

/// GC object header - 8 bytes.
/// Layout: [marked:8 | reserved:8 | slots:16 | ValueMeta:32]
///
/// marked field bit layout (Lua style):
///   bit 0-2: age (for generational GC)
///   bit 3: WHITE0
///   bit 4: WHITE1
///   bit 5: BLACK
///   bit 6-7: reserved
///
/// ValueMeta contains:
/// - meta_id (24 bits): meaning depends on value_kind
/// - value_kind (8 bits): ValueKind enum
///
/// meta_id meaning depends on kind:
/// - Struct: struct_metas[] index (for field layout / GC scan)
/// - Pointer: struct_metas[] index of the *pointee* struct (for PtrNew-created
///   objects that hold the full struct data). For heap-boxed pointer variables
///   (1-slot GcRef container), meta_id = 0 (ref box).
/// - Array: element's ValueMeta (elem_kind + elem_meta_id)
/// - Interface: interface_metas[] index
/// - Others: 0
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct GcHeader {
    pub marked: u8,
    pub _reserved: u8,
    pub slots: u16,
    pub value_meta: ValueMeta,
}

// Marked field bit positions
pub(crate) const AGE_MASK: u8 = 0x07; // bits 0-2
pub(crate) const WHITE0_BIT: u8 = 1 << 3; // bit 3
pub(crate) const WHITE1_BIT: u8 = 1 << 4; // bit 4
pub(crate) const BLACK_BIT: u8 = 1 << 5; // bit 5
pub(crate) const WHITE_BITS: u8 = WHITE0_BIT | WHITE1_BIT;
pub(crate) const VALUE_SLOTS_OBJECT_BIT: u8 = 1 << 0;
pub(crate) const RUNTIME_BACKING_OBJECT_BIT: u8 = 1 << 1;
const REMEMBERED_FORGET_PENDING_BIT: u8 = 1 << 2;

// Age values (for generational GC)
pub const G_YOUNG: u8 = 0;
pub const G_SURVIVAL: u8 = 1;
pub const G_OLD: u8 = 2;
pub const G_TOUCHED: u8 = 3;

/// Collector scheduling policy. Both modes use the same heap, object layout,
/// barrier ABI, and resumable mark/sweep engine.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcMode {
    Generational = 0,
    Incremental = 1,
}

/// Kind of the cycle currently running or most recently completed.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcCycleKind {
    Minor = 0,
    Major = 1,
}

/// Allocation-failure behavior selected by the Island host.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OomPolicy {
    CollectThenTerminateIsland = 0,
    TerminateIsland = 1,
}

/// Memory configuration applied when an Island collector is created.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VmMemoryConfig {
    pub initial_reserve_bytes: usize,
    pub hard_limit_bytes: Option<usize>,
    pub gc_mode: GcMode,
    pub automatic_gc: bool,
    pub oom_policy: OomPolicy,
    pub growth_allowed: bool,
    pub allocation_allowed: bool,
    pub max_objects: Option<usize>,
    pub max_leases: Option<usize>,
}

impl Default for VmMemoryConfig {
    fn default() -> Self {
        Self {
            initial_reserve_bytes: 0,
            hard_limit_bytes: None,
            gc_mode: GcMode::Generational,
            automatic_gc: true,
            oom_policy: OomPolicy::CollectThenTerminateIsland,
            growth_allowed: true,
            allocation_allowed: true,
            max_objects: None,
            max_leases: None,
        }
    }
}

/// Stable host root for a GC object retained across FFI calls.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GcLease {
    pub index: u32,
    pub generation: u32,
}

#[derive(Debug, Clone, Copy)]
struct GcLeaseEntry {
    root: GcRef,
    generation: u32,
    retired: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct AllocationExtent {
    data_size: usize,
    remembered_index: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryError {
    AllocationForbidden,
    GrowthDisabled,
    HardLimitExceeded,
    MetadataExhausted,
    SystemAllocationFailed,
    InvalidPointer,
    CollectorBusy,
}

impl core::fmt::Display for MemoryError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let message = match self {
            Self::AllocationForbidden => "managed allocation is disabled",
            Self::GrowthDisabled => "Island heap growth is disabled",
            Self::HardLimitExceeded => "Island memory hard limit exceeded",
            Self::MetadataExhausted => "reserved Island heap metadata exhausted",
            Self::SystemAllocationFailed => "system allocation failed",
            Self::InvalidPointer => "pointer does not belong to the Island heap",
            Self::CollectorBusy => "collector state does not allow this operation",
        };
        f.write_str(message)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for MemoryError {}

impl From<HeapError> for MemoryError {
    fn from(value: HeapError) -> Self {
        match value {
            HeapError::AllocationForbidden => Self::AllocationForbidden,
            HeapError::GrowthDisabled => Self::GrowthDisabled,
            HeapError::HardLimitExceeded => Self::HardLimitExceeded,
            HeapError::SystemAllocationFailed => Self::SystemAllocationFailed,
            HeapError::InvalidPointer => Self::InvalidPointer,
        }
    }
}

/// Platform-independent Island memory counters.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MemoryStats {
    pub managed_reserved_bytes: usize,
    pub managed_committed_bytes: usize,
    pub managed_live_bytes: usize,
    pub allocated_span_bytes: usize,
    pub pending_reclaim_bytes: usize,
    pub segment_count: usize,
    pub block_count: usize,
    pub free_blocks: usize,
    pub object_count: usize,
    pub young_live_bytes: usize,
    pub old_live_bytes: usize,
    pub large_live_bytes: usize,
    pub runtime_backing_bytes: usize,
    pub external_reported_bytes: usize,
    pub unknown_external_provider_count: usize,
    pub partial_span_bytes: usize,
    pub fragmentation_bytes: usize,
    pub wasm_current_pages: u64,
    pub wasm_maximum_pages: Option<u64>,
    pub allocation_bytes_total: u64,
    pub allocation_failures: u64,
    pub cycle_id: u64,
    pub minor_cycles: u64,
    pub major_cycles: u64,
    pub work_units_total: u64,
    pub last_step_work_units: usize,
    pub max_step_work_units: usize,
    /// Number of remembered old parents. The legacy field name is preserved
    /// in the public runtime/mem stats ABI.
    pub dirty_cards: usize,
    pub dirty_root_domains: usize,
    pub remark_rounds: u64,
    pub active_leases: usize,
    pub reclaim_backlog_bytes: usize,
    pub growth_allowed: bool,
    pub allocation_allowed: bool,
    pub hard_limit_bytes: Option<usize>,
    pub gc_mode: GcMode,
    pub automatic_gc: bool,
    pub gc_state: GcState,
}

impl Default for MemoryStats {
    fn default() -> Self {
        Self {
            managed_reserved_bytes: 0,
            managed_committed_bytes: 0,
            managed_live_bytes: 0,
            allocated_span_bytes: 0,
            pending_reclaim_bytes: 0,
            segment_count: 0,
            block_count: 0,
            free_blocks: 0,
            object_count: 0,
            young_live_bytes: 0,
            old_live_bytes: 0,
            large_live_bytes: 0,
            runtime_backing_bytes: 0,
            external_reported_bytes: 0,
            unknown_external_provider_count: 0,
            partial_span_bytes: 0,
            fragmentation_bytes: 0,
            wasm_current_pages: 0,
            wasm_maximum_pages: None,
            allocation_bytes_total: 0,
            allocation_failures: 0,
            cycle_id: 0,
            minor_cycles: 0,
            major_cycles: 0,
            work_units_total: 0,
            last_step_work_units: 0,
            max_step_work_units: 0,
            dirty_cards: 0,
            dirty_root_domains: 0,
            remark_rounds: 0,
            active_leases: 0,
            reclaim_backlog_bytes: 0,
            growth_allowed: true,
            allocation_allowed: true,
            hard_limit_bytes: None,
            gc_mode: GcMode::Generational,
            automatic_gc: true,
            gc_state: GcState::Pause,
        }
    }
}

/// GC state machine states.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcState {
    Pause = 0,     // Idle, waiting for trigger
    Propagate = 1, // Incremental marking (interruptible)
    Atomic = 2,    // Resumable remark/fixed-point marking
    Sweep = 3,     // Sweeping dead objects
    Reclaim = 4,   // Incrementally publishing reclaimed large-span blocks
}

/// Caller-provided root-set freshness for one incremental GC step.
///
/// `Gc::step` always uses `MayHaveChanged`, which is the conservative and safe
/// default. `StableSinceLastScan` may only be used when the caller can prove
/// that no root slot has changed since the previous root scan performed by this
/// GC instance. Heap write barriers and new allocations are still processed
/// through the gray queues; this flag controls only whether sweep must rescan
/// all roots before freeing the next chunk.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcRootState {
    MayHaveChanged = 0,
    StableSinceLastScan = 1,
}

/// Root scan pass currently requested by the collector.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcRootScanKind {
    StartCycle = 0,
    Atomic = 1,
    Sweep = 2,
}

/// Result of one bounded root scan chunk.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GcRootScanChunk {
    pub done: bool,
    pub work_bytes: usize,
}

/// Persistent cursor for one heap object's generated trace layout.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct GcTraceCursor {
    pub element_index: usize,
    pub reference_index: usize,
    pub auxiliary: usize,
}

/// Result of one bounded heap-object scan chunk.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GcObjectScanChunk {
    pub done: bool,
    pub work_bytes: usize,
}

impl GcObjectScanChunk {
    #[inline]
    pub fn complete(work_bytes: usize) -> Self {
        Self {
            done: true,
            work_bytes,
        }
    }

    #[inline]
    pub fn pending(work_bytes: usize) -> Self {
        Self {
            done: false,
            work_bytes,
        }
    }
}

impl GcRootScanChunk {
    #[inline]
    pub fn complete(work_bytes: usize) -> Self {
        Self {
            done: true,
            work_bytes,
        }
    }

    #[inline]
    pub fn pending(work_bytes: usize) -> Self {
        Self {
            done: false,
            work_bytes,
        }
    }
}

/// Platform-independent telemetry for the most recent incremental GC step.
///
/// Durations are intentionally not recorded here because `vo-runtime` is
/// no_std-capable. Hosts such as the VM, Studio, or the perf harness should
/// measure wall-clock time around `Gc::step`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GcStepStats {
    pub phase_before: GcState,
    pub phase_after: GcState,
    pub cycle_kind: GcCycleKind,
    pub root_state: GcRootState,
    pub root_scan_calls: usize,
    pub root_scan_skips: usize,
    pub root_scan_work_bytes: usize,
    pub object_scans: usize,
    pub finalized_objects: usize,
    pub sweep_freed_bytes: usize,
    pub propagate_work_bytes: usize,
    pub sweep_work_bytes: usize,
    pub total_work_bytes: usize,
    pub heap_bytes_before: usize,
    pub heap_bytes_after: usize,
    pub debt_before: i64,
    pub debt_after: i64,
    pub gray_len_before: usize,
    pub gray_len_after: usize,
    pub remembered_len_before: usize,
    pub remembered_len_after: usize,
    pub cycle_started: bool,
    pub cycle_finished: bool,
}

impl Default for GcStepStats {
    fn default() -> Self {
        Self {
            phase_before: GcState::Pause,
            phase_after: GcState::Pause,
            cycle_kind: GcCycleKind::Major,
            root_state: GcRootState::MayHaveChanged,
            root_scan_calls: 0,
            root_scan_skips: 0,
            root_scan_work_bytes: 0,
            object_scans: 0,
            finalized_objects: 0,
            sweep_freed_bytes: 0,
            propagate_work_bytes: 0,
            sweep_work_bytes: 0,
            total_work_bytes: 0,
            heap_bytes_before: 0,
            heap_bytes_after: 0,
            debt_before: 0,
            debt_after: 0,
            gray_len_before: 0,
            gray_len_after: 0,
            remembered_len_before: 0,
            remembered_len_after: 0,
            cycle_started: false,
            cycle_finished: false,
        }
    }
}

impl GcHeader {
    pub const SIZE: usize = SLOT_BYTES;

    pub fn new(value_meta: ValueMeta, slots: u16) -> Self {
        Self::new_with_white(value_meta, slots, WHITE0_BIT)
    }

    pub fn new_with_white(value_meta: ValueMeta, slots: u16, white_bit: u8) -> Self {
        Self {
            marked: white_bit | G_YOUNG,
            _reserved: 0,
            slots,
            value_meta,
        }
    }

    // ========== Color methods ==========

    #[inline]
    pub fn is_white(&self) -> bool {
        (self.marked & WHITE_BITS) != 0
    }

    #[inline]
    pub fn is_black(&self) -> bool {
        (self.marked & BLACK_BIT) != 0
    }

    #[inline]
    pub fn is_gray(&self) -> bool {
        !self.is_white() && !self.is_black()
    }

    #[inline]
    pub fn set_black(&mut self) {
        self.marked = (self.marked & !(WHITE_BITS)) | BLACK_BIT;
    }

    #[inline]
    pub fn set_white(&mut self, current_white: u8) {
        self.marked = (self.marked & !(WHITE_BITS | BLACK_BIT)) | current_white;
    }

    #[inline]
    pub fn set_gray(&mut self) {
        self.marked &= !(WHITE_BITS | BLACK_BIT);
    }

    // ========== Age methods ==========

    #[inline]
    pub fn age(&self) -> u8 {
        self.marked & AGE_MASK
    }

    #[inline]
    pub fn set_age(&mut self, age: u8) {
        self.marked = (self.marked & !AGE_MASK) | (age & AGE_MASK);
    }

    // ========== ValueMeta methods ==========

    #[inline]
    pub fn meta_id(&self) -> u32 {
        self.value_meta.meta_id()
    }

    #[inline]
    pub fn set_meta_id(&mut self, meta_id: u32) {
        self.value_meta = ValueMeta::new(meta_id, self.value_meta.value_kind());
    }

    #[inline]
    pub fn kind(&self) -> ValueKind {
        self.value_meta.value_kind()
    }

    #[inline]
    pub fn value_meta(&self) -> ValueMeta {
        self.value_meta
    }

    #[inline]
    pub fn is_value_slots_object(&self) -> bool {
        (self._reserved & VALUE_SLOTS_OBJECT_BIT) != 0
    }

    #[inline]
    pub fn set_value_slots_object(&mut self) {
        self._reserved |= VALUE_SLOTS_OBJECT_BIT;
    }

    #[inline]
    pub fn is_runtime_backing_object(&self) -> bool {
        (self._reserved & RUNTIME_BACKING_OBJECT_BIT) != 0
    }

    #[inline]
    fn set_runtime_backing_object(&mut self) {
        self._reserved |= RUNTIME_BACKING_OBJECT_BIT;
    }

    #[inline]
    fn remembered_forget_pending(&self) -> bool {
        (self._reserved & REMEMBERED_FORGET_PENDING_BIT) != 0
    }

    #[inline]
    fn set_remembered_forget_pending(&mut self, pending: bool) {
        if pending {
            self._reserved |= REMEMBERED_FORGET_PENDING_BIT;
        } else {
            self._reserved &= !REMEMBERED_FORGET_PENDING_BIT;
        }
    }
}

/// GC reference - pointer to GcObject data (after header).
pub type GcRef = *mut Slot;

/// Garbage collector.
#[repr(C)]
pub struct Gc {
    /// Present only in the lightweight GC facade constructed inside a native
    /// extension trampoline. Host collectors always keep this as `None`.
    owner_dispatch: Option<GcOwnerDispatch>,

    // ========== Island Heap ==========
    heap: SpanHeap,
    initial_reserve_bytes: usize,
    gc_mode: GcMode,
    automatic_gc: bool,
    oom_policy: OomPolicy,
    max_objects: Option<usize>,
    max_objects_explicit: bool,
    max_leases: Option<usize>,
    max_leases_explicit: bool,
    allocation_failures: u64,
    last_memory_error: Option<MemoryError>,
    allocation_bytes_total: u64,
    external_reported_bytes: usize,
    unknown_external_provider_count: usize,
    wasm_current_pages: u64,
    wasm_maximum_pages: Option<u64>,

    // ========== Object Storage ==========
    all_objects: Vec<GcRef>,
    all_object_data_sizes: Vec<usize>,
    allocation_extents: hashbrown::HashMap<usize, AllocationExtent>,
    leases: Vec<GcLeaseEntry>,
    free_lease_indices: Vec<u32>,

    // ========== Mark Queues ==========
    gray: Vec<GcRef>,
    remembered: Vec<GcRef>,
    remembered_forget: Vec<GcRef>,
    pending_object_scan: Option<GcRef>,
    pending_trace_cursor: GcTraceCursor,
    pending_remembered_parent: Option<GcRef>,
    pending_remembered_has_young: bool,
    remembered_scan_pos: usize,
    remembered_scan_end: usize,

    // ========== State ==========
    state: GcState,
    current_white: u8,      // Current white bit (WHITE0_BIT or WHITE1_BIT)
    sweep_pos: usize,       // Read position in sweep phase
    sweep_write_pos: usize, // Write position for live objects in sweep phase

    // ========== Memory Stats ==========
    total_bytes: usize, // Total allocated bytes
    live_object_count: usize,
    young_live_bytes: usize,
    old_live_bytes: usize,
    large_live_bytes: usize,
    runtime_backing_bytes: usize,
    active_lease_count: usize,
    estimate: usize, // Estimated live bytes after last GC
    debt: i64,       // Work debt (triggers GC when > 0)

    // ========== Parameters ==========
    pause: u16,      // Pause multiplier (default 200 = 2x)
    stepmul: u16,    // Step multiplier (default 100)
    stepsize: usize, // Bytes per step (default 8KB)

    // ========== Phase Budget ==========
    /// Fixed per-step budget for sweep phase, snapshotted at sweep start.
    /// Using a constant prevents convergence issues (total_bytes shrinks as dead
    /// objects are freed, causing dynamically-computed limits to shrink too).
    sweep_budget: usize,
    /// In-progress root scan for callers that provide a bounded root scanner.
    pending_root_scan: Option<GcRootScanKind>,
    root_lease_scan_kind: Option<GcRootScanKind>,
    root_lease_scan_cursor: usize,
    atomic_root_scan_complete: bool,
    sweep_root_scan_complete: bool,
    cycle_kind: GcCycleKind,
    force_major_cycle: bool,
    minor_cycles_since_major: u8,
    cycle_id: u64,
    minor_cycles: u64,
    major_cycles: u64,
    remark_rounds: u64,
    work_units_total: u64,
    max_step_work_units: usize,

    // ========== Diagnostics ==========
    /// Stress mode for GC correctness testing. When enabled, every scheduler
    /// boundary runs a GC step even when there is no allocation debt, forcing
    /// mark/sweep interleavings that are otherwise rare.
    stress_every_step: bool,
    last_step_stats: GcStepStats,
}

impl Gc {
    // Default parameters
    const DEFAULT_PAUSE: u16 = 200; // Trigger at 2x estimated live size
    const DEFAULT_STEPMUL: u16 = 100; // Work multiplier
    const DEFAULT_STEPSIZE: usize = 8192; // 8KB per step
    const NO_GROWTH_LEASES_PER_BLOCK: usize = 64;
    const MAX_LEASE_SLOTS: usize = u32::MAX as usize;

    pub fn new() -> Self {
        Self::with_memory_config(VmMemoryConfig::default())
            .expect("default GC memory configuration must be constructible")
    }

    pub fn with_memory_config(config: VmMemoryConfig) -> Result<Self, MemoryError> {
        if config
            .max_leases
            .is_some_and(|max_leases| max_leases > Self::MAX_LEASE_SLOTS)
        {
            return Err(MemoryError::MetadataExhausted);
        }
        let mut heap = SpanHeap::new(config.hard_limit_bytes);
        if config.initial_reserve_bytes > 0 {
            heap.reserve(config.initial_reserve_bytes)?;
        }
        heap.set_growth_allowed(config.growth_allowed);
        heap.set_allocation_allowed(config.allocation_allowed);
        let max_objects = config
            .max_objects
            .or_else(|| (!config.growth_allowed).then(|| heap.max_min_cell_allocations()));
        let max_leases = config.max_leases.or_else(|| {
            (!config.growth_allowed).then(|| {
                heap.committed_block_count()
                    .saturating_mul(Self::NO_GROWTH_LEASES_PER_BLOCK)
            })
        });
        let mut all_objects = Vec::new();
        let mut all_object_data_sizes = Vec::new();
        let mut allocation_extents = hashbrown::HashMap::new();
        let mut leases = Vec::new();
        let mut free_lease_indices = Vec::new();
        let mut gray = Vec::new();
        let mut remembered = Vec::new();
        let mut remembered_forget = Vec::new();
        if let Some(max_objects) = max_objects {
            all_objects
                .try_reserve_exact(max_objects)
                .map_err(|_| MemoryError::SystemAllocationFailed)?;
            all_object_data_sizes
                .try_reserve_exact(max_objects)
                .map_err(|_| MemoryError::SystemAllocationFailed)?;
            allocation_extents
                .try_reserve(max_objects)
                .map_err(|_| MemoryError::SystemAllocationFailed)?;
            gray.try_reserve_exact(max_objects)
                .map_err(|_| MemoryError::SystemAllocationFailed)?;
            remembered
                .try_reserve_exact(max_objects)
                .map_err(|_| MemoryError::SystemAllocationFailed)?;
            remembered_forget
                .try_reserve_exact(max_objects)
                .map_err(|_| MemoryError::SystemAllocationFailed)?;
        }
        if let Some(max_leases) = max_leases {
            leases
                .try_reserve_exact(max_leases)
                .map_err(|_| MemoryError::SystemAllocationFailed)?;
            free_lease_indices
                .try_reserve_exact(max_leases)
                .map_err(|_| MemoryError::SystemAllocationFailed)?;
        }
        Ok(Self {
            owner_dispatch: None,
            heap,
            initial_reserve_bytes: config.initial_reserve_bytes,
            gc_mode: config.gc_mode,
            automatic_gc: config.automatic_gc,
            oom_policy: config.oom_policy,
            max_objects,
            max_objects_explicit: config.max_objects.is_some(),
            max_leases,
            max_leases_explicit: config.max_leases.is_some(),
            allocation_failures: 0,
            last_memory_error: None,
            allocation_bytes_total: 0,
            external_reported_bytes: 0,
            unknown_external_provider_count: 0,
            wasm_current_pages: 0,
            wasm_maximum_pages: None,
            all_objects,
            all_object_data_sizes,
            allocation_extents,
            leases,
            free_lease_indices,
            gray,
            remembered,
            remembered_forget,
            pending_object_scan: None,
            pending_trace_cursor: GcTraceCursor::default(),
            pending_remembered_parent: None,
            pending_remembered_has_young: false,
            remembered_scan_pos: 0,
            remembered_scan_end: 0,
            state: GcState::Pause,
            current_white: WHITE0_BIT,
            sweep_pos: 0,
            sweep_write_pos: 0,
            total_bytes: 0,
            live_object_count: 0,
            young_live_bytes: 0,
            old_live_bytes: 0,
            large_live_bytes: 0,
            runtime_backing_bytes: 0,
            active_lease_count: 0,
            estimate: 0,
            debt: 0,
            pause: Self::DEFAULT_PAUSE,
            stepmul: Self::DEFAULT_STEPMUL,
            stepsize: Self::DEFAULT_STEPSIZE,
            sweep_budget: 0,
            pending_root_scan: None,
            root_lease_scan_kind: None,
            root_lease_scan_cursor: 0,
            atomic_root_scan_complete: false,
            sweep_root_scan_complete: false,
            cycle_kind: GcCycleKind::Major,
            force_major_cycle: false,
            minor_cycles_since_major: 0,
            cycle_id: 0,
            minor_cycles: 0,
            major_cycles: 0,
            remark_rounds: 0,
            work_units_total: 0,
            max_step_work_units: 0,
            stress_every_step: false,
            last_step_stats: GcStepStats::default(),
        })
    }

    /// Construct the lightweight collector facade used by ABI-v10 extensions.
    ///
    /// The facade owns no VM allocations. Its empty Rust collections remain
    /// extension-local, while all collector mutations dispatch to the host.
    pub(crate) fn with_owner_dispatch(owner_dispatch: GcOwnerDispatch) -> Self {
        let mut gc = Self::new();
        gc.owner_dispatch = Some(owner_dispatch);
        gc
    }

    #[track_caller]
    fn reject_owner_proxy_api(&self, api: &str) {
        if self.owner_dispatch.is_some() {
            panic!(
                "native extension GC facade does not expose collector API `{api}`; use an allocator-neutral ExternCallContext helper"
            );
        }
    }

    /// Get current GC state.
    #[inline]
    pub fn state(&self) -> GcState {
        self.reject_owner_proxy_api("state");
        self.state
    }

    pub fn memory_reserve(&mut self, bytes: usize) -> Result<MemoryStats, MemoryError> {
        self.reject_owner_proxy_api("memory_reserve");
        self.heap.reserve(bytes)?;
        if let Some(max_objects) = self.max_objects {
            if self.all_objects.capacity() < max_objects {
                self.all_objects
                    .try_reserve_exact(max_objects.saturating_sub(self.all_objects.len()))
                    .map_err(|_| MemoryError::SystemAllocationFailed)?;
            }
            if self.all_object_data_sizes.capacity() < max_objects {
                self.all_object_data_sizes
                    .try_reserve_exact(max_objects.saturating_sub(self.all_object_data_sizes.len()))
                    .map_err(|_| MemoryError::SystemAllocationFailed)?;
            }
            let extent_remaining = max_objects.saturating_sub(self.allocation_extents.len());
            if extent_remaining > 0 {
                self.allocation_extents
                    .try_reserve(extent_remaining)
                    .map_err(|_| MemoryError::SystemAllocationFailed)?;
            }
            self.reserve_object_work_queues(max_objects)?;
        }
        if let Some(max_leases) = self.max_leases {
            if self.leases.capacity() < max_leases {
                self.leases
                    .try_reserve_exact(max_leases.saturating_sub(self.leases.len()))
                    .map_err(|_| MemoryError::SystemAllocationFailed)?;
            }
            if self.free_lease_indices.capacity() < max_leases {
                self.free_lease_indices
                    .try_reserve_exact(max_leases.saturating_sub(self.free_lease_indices.len()))
                    .map_err(|_| MemoryError::SystemAllocationFailed)?;
            }
        }
        Ok(self.memory_stats())
    }

    /// Keep every object-indexed collector queue large enough for the complete
    /// live-object set. Queue insertion can then remain infallible after an
    /// object color or remembered-set bit has changed, and a bounded GC slice
    /// never hides a whole-Vec reallocation in one work unit.
    fn reserve_object_work_queues(&mut self, object_capacity: usize) -> Result<(), MemoryError> {
        for queue in [
            &mut self.gray,
            &mut self.remembered,
            &mut self.remembered_forget,
        ] {
            if queue.capacity() < object_capacity {
                queue
                    .try_reserve_exact(object_capacity.saturating_sub(queue.len()))
                    .map_err(|_| MemoryError::SystemAllocationFailed)?;
            }
        }
        Ok(())
    }

    #[inline]
    pub fn memory_set_growth_allowed(&mut self, allowed: bool) -> Result<(), MemoryError> {
        self.reject_owner_proxy_api("memory_set_growth_allowed");
        if !allowed && self.heap.growth_allowed() {
            let max_objects = self
                .max_objects
                .unwrap_or_else(|| self.heap.max_min_cell_allocations());
            if self.all_objects.capacity() < max_objects {
                self.all_objects
                    .try_reserve_exact(max_objects.saturating_sub(self.all_objects.len()))
                    .map_err(|_| MemoryError::SystemAllocationFailed)?;
            }
            if self.all_object_data_sizes.capacity() < max_objects {
                self.all_object_data_sizes
                    .try_reserve_exact(max_objects.saturating_sub(self.all_object_data_sizes.len()))
                    .map_err(|_| MemoryError::SystemAllocationFailed)?;
            }
            let extent_additional = max_objects.saturating_sub(self.allocation_extents.len());
            if extent_additional > 0 {
                self.allocation_extents
                    .try_reserve(extent_additional)
                    .map_err(|_| MemoryError::SystemAllocationFailed)?;
            }
            self.reserve_object_work_queues(max_objects)?;
            self.max_objects = Some(max_objects);

            let max_leases = self.max_leases.unwrap_or_else(|| {
                self.heap
                    .committed_block_count()
                    .saturating_mul(Self::NO_GROWTH_LEASES_PER_BLOCK)
            });
            if self.leases.capacity() < max_leases {
                self.leases
                    .try_reserve_exact(max_leases.saturating_sub(self.leases.len()))
                    .map_err(|_| MemoryError::SystemAllocationFailed)?;
            }
            if self.free_lease_indices.capacity() < max_leases {
                self.free_lease_indices
                    .try_reserve_exact(max_leases.saturating_sub(self.free_lease_indices.len()))
                    .map_err(|_| MemoryError::SystemAllocationFailed)?;
            }
            self.max_leases = Some(max_leases);
        } else if allowed && !self.heap.growth_allowed() {
            if !self.max_objects_explicit {
                self.max_objects = None;
            }
            if !self.max_leases_explicit {
                self.max_leases = None;
            }
        }
        self.heap.set_growth_allowed(allowed);
        Ok(())
    }

    #[inline]
    pub fn memory_set_allocation_allowed(&mut self, allowed: bool) {
        self.reject_owner_proxy_api("memory_set_allocation_allowed");
        self.heap.set_allocation_allowed(allowed);
    }

    pub fn memory_set_hard_limit_bytes(&mut self, limit: Option<usize>) -> Result<(), MemoryError> {
        self.reject_owner_proxy_api("memory_set_hard_limit_bytes");
        self.heap.set_hard_limit_bytes(limit)?;
        Ok(())
    }

    /// Publish host/provider memory counters alongside managed-heap telemetry.
    pub fn memory_set_external_reported(&mut self, bytes: usize, unknown_provider_count: usize) {
        self.reject_owner_proxy_api("memory_set_external_reported");
        self.external_reported_bytes = bytes;
        self.unknown_external_provider_count = unknown_provider_count;
    }

    /// Publish WebAssembly linear-memory admission state for unified telemetry.
    pub fn memory_set_wasm_pages(&mut self, current: u64, maximum: Option<u64>) {
        self.reject_owner_proxy_api("memory_set_wasm_pages");
        self.wasm_current_pages = current;
        self.wasm_maximum_pages = maximum;
    }

    pub fn gc_set_mode(&mut self, mode: GcMode) -> Result<(), MemoryError> {
        self.reject_owner_proxy_api("gc_set_mode");
        if self.state != GcState::Pause {
            return Err(MemoryError::CollectorBusy);
        }
        self.gc_mode = mode;
        if mode == GcMode::Incremental {
            self.force_major_cycle = true;
        }
        Ok(())
    }

    /// Request that the next cycle trace and sweep every generation.
    #[inline]
    pub fn gc_request_major(&mut self) {
        self.reject_owner_proxy_api("gc_request_major");
        self.force_major_cycle = true;
        self.debt = self.debt.max(1);
    }

    /// Request ordinary cycle scheduling without changing the selected mode.
    #[inline]
    pub fn gc_request_cycle(&mut self) {
        self.reject_owner_proxy_api("gc_request_cycle");
        self.debt = self.debt.max(1);
    }

    /// Notify the collector that a root changed after a completed remark or
    /// sweep-rescue scan.
    #[inline]
    pub fn roots_changed(&mut self) {
        self.reject_owner_proxy_api("roots_changed");
        match self.state {
            GcState::Atomic => self.atomic_root_scan_complete = false,
            GcState::Sweep => self.sweep_root_scan_complete = false,
            _ => {}
        }
    }

    #[inline]
    pub fn gc_mode(&self) -> GcMode {
        self.reject_owner_proxy_api("gc_mode");
        self.gc_mode
    }

    #[inline]
    pub fn gc_stop(&mut self) {
        self.reject_owner_proxy_api("gc_stop");
        self.automatic_gc = false;
    }

    #[inline]
    pub fn gc_restart(&mut self) {
        self.reject_owner_proxy_api("gc_restart");
        self.automatic_gc = true;
    }

    #[inline]
    pub fn automatic_gc(&self) -> bool {
        self.reject_owner_proxy_api("automatic_gc");
        self.automatic_gc
    }

    #[inline]
    pub fn oom_policy(&self) -> OomPolicy {
        self.reject_owner_proxy_api("oom_policy");
        self.oom_policy
    }

    /// Snapshot the policy inherited by a newly-created child Island.
    ///
    /// Runtime telemetry and live heap occupancy stay Island-local. The
    /// snapshot carries only admission and collector policy.
    pub fn memory_config_snapshot(&self) -> VmMemoryConfig {
        self.reject_owner_proxy_api("memory_config_snapshot");
        VmMemoryConfig {
            initial_reserve_bytes: self.initial_reserve_bytes,
            hard_limit_bytes: self.heap.hard_limit_bytes(),
            gc_mode: self.gc_mode,
            automatic_gc: self.automatic_gc,
            oom_policy: self.oom_policy,
            growth_allowed: self.heap.growth_allowed(),
            allocation_allowed: self.heap.allocation_allowed(),
            max_objects: self
                .max_objects_explicit
                .then_some(self.max_objects)
                .flatten(),
            max_leases: self
                .max_leases_explicit
                .then_some(self.max_leases)
                .flatten(),
        }
    }

    #[inline]
    pub fn last_memory_error(&self) -> Option<MemoryError> {
        self.reject_owner_proxy_api("last_memory_error");
        self.last_memory_error
    }

    #[inline]
    pub fn take_last_memory_error(&mut self) -> Option<MemoryError> {
        self.reject_owner_proxy_api("take_last_memory_error");
        self.last_memory_error.take()
    }

    pub fn gc_lease(&mut self, obj: GcRef) -> Result<GcLease, MemoryError> {
        self.reject_owner_proxy_api("gc_lease");
        let obj = self
            .canonicalize_ref(obj)
            .filter(|obj| !obj.is_null())
            .ok_or(MemoryError::InvalidPointer)?;
        if let Some(index) = self.free_lease_indices.pop() {
            let entry = &mut self.leases[index as usize];
            debug_assert!(entry.root.is_null() && !entry.retired);
            entry.root = obj;
            let generation = entry.generation;
            self.active_lease_count += 1;
            self.mark_gray(obj);
            self.roots_changed();
            return Ok(GcLease { index, generation });
        }
        if self
            .max_leases
            .is_some_and(|max_leases| self.leases.len() >= max_leases)
            || self.leases.len() >= Self::MAX_LEASE_SLOTS
        {
            return Err(MemoryError::MetadataExhausted);
        }
        if self.leases.len() == self.leases.capacity() {
            if !self.heap.growth_allowed() {
                return Err(MemoryError::MetadataExhausted);
            }
            self.leases
                .try_reserve(1)
                .map_err(|_| MemoryError::SystemAllocationFailed)?;
        }
        let index = self.leases.len();
        self.leases.push(GcLeaseEntry {
            root: obj,
            generation: 1,
            retired: false,
        });
        self.active_lease_count += 1;
        self.mark_gray(obj);
        self.roots_changed();
        Ok(GcLease {
            index: index as u32,
            generation: 1,
        })
    }

    pub fn gc_lease_root(&self, lease: GcLease) -> Result<GcRef, MemoryError> {
        self.reject_owner_proxy_api("gc_lease_root");
        let entry = self
            .leases
            .get(lease.index as usize)
            .filter(|entry| {
                !entry.retired && !entry.root.is_null() && entry.generation == lease.generation
            })
            .ok_or(MemoryError::InvalidPointer)?;
        Ok(entry.root)
    }

    pub fn gc_release_lease(&mut self, lease: GcLease) -> Result<(), MemoryError> {
        self.reject_owner_proxy_api("gc_release_lease");
        let entry = self
            .leases
            .get_mut(lease.index as usize)
            .filter(|entry| {
                !entry.retired && !entry.root.is_null() && entry.generation == lease.generation
            })
            .ok_or(MemoryError::InvalidPointer)?;
        if entry.generation != u32::MAX
            && self.free_lease_indices.len() == self.free_lease_indices.capacity()
        {
            if !self.heap.growth_allowed() {
                return Err(MemoryError::MetadataExhausted);
            }
            self.free_lease_indices
                .try_reserve(1)
                .map_err(|_| MemoryError::SystemAllocationFailed)?;
        }
        entry.root = core::ptr::null_mut();
        self.active_lease_count -= 1;
        if let Some(next) = entry.generation.checked_add(1) {
            entry.generation = next;
            self.free_lease_indices.push(lease.index);
        } else {
            entry.retired = true;
        }
        Ok(())
    }

    pub fn memory_stats(&self) -> MemoryStats {
        self.reject_owner_proxy_api("memory_stats");
        let HeapStats {
            committed_bytes,
            allocated_span_bytes,
            pending_reclaim_bytes,
            segment_count,
            block_count,
            free_blocks,
        } = self.heap.stats();
        let fragmentation_bytes = allocated_span_bytes.saturating_sub(self.total_bytes);
        MemoryStats {
            managed_reserved_bytes: committed_bytes,
            managed_committed_bytes: committed_bytes,
            managed_live_bytes: self.total_bytes,
            allocated_span_bytes,
            pending_reclaim_bytes,
            segment_count,
            block_count,
            free_blocks,
            object_count: self.live_object_count,
            young_live_bytes: self.young_live_bytes,
            old_live_bytes: self.old_live_bytes,
            large_live_bytes: self.large_live_bytes,
            runtime_backing_bytes: self.runtime_backing_bytes,
            external_reported_bytes: self.external_reported_bytes,
            unknown_external_provider_count: self.unknown_external_provider_count,
            partial_span_bytes: fragmentation_bytes,
            fragmentation_bytes,
            wasm_current_pages: self.wasm_current_pages,
            wasm_maximum_pages: self.wasm_maximum_pages,
            allocation_bytes_total: self.allocation_bytes_total,
            allocation_failures: self.allocation_failures,
            cycle_id: self.cycle_id,
            minor_cycles: self.minor_cycles,
            major_cycles: self.major_cycles,
            work_units_total: self.work_units_total,
            last_step_work_units: self.last_step_stats.total_work_bytes / SLOT_BYTES,
            max_step_work_units: self.max_step_work_units,
            dirty_cards: self.remembered.len(),
            dirty_root_domains: usize::from(
                self.pending_root_scan.is_some()
                    || !self.atomic_root_scan_complete
                    || !self.sweep_root_scan_complete,
            ),
            remark_rounds: self.remark_rounds,
            active_leases: self.active_lease_count,
            reclaim_backlog_bytes: pending_reclaim_bytes,
            growth_allowed: self.heap.growth_allowed(),
            allocation_allowed: self.heap.allocation_allowed(),
            hard_limit_bytes: self.heap.hard_limit_bytes(),
            gc_mode: self.gc_mode,
            automatic_gc: self.automatic_gc,
            gc_state: self.state,
        }
    }

    /// Get current white bit for new allocations.
    #[inline]
    pub fn current_white(&self) -> u8 {
        self.reject_owner_proxy_api("current_white");
        self.current_white
    }

    /// Telemetry for the most recent incremental GC step.
    #[inline]
    pub fn last_step_stats(&self) -> GcStepStats {
        self.reject_owner_proxy_api("last_step_stats");
        self.last_step_stats
    }

    /// Get the "other" white bit (for checking dead objects).
    #[inline]
    fn other_white(&self) -> u8 {
        self.current_white ^ WHITE_BITS
    }

    /// Check if object is dead-white for the current cycle.
    #[inline]
    pub fn is_dead_white(&self, obj: GcRef) -> bool {
        self.reject_owner_proxy_api("is_dead_white");
        let Some(obj) = self.canonicalize_ref(obj) else {
            return false;
        };
        let header = unsafe { Self::header(obj) };
        (header.marked & WHITE_BITS) == self.other_white()
    }

    /// Allocate a new GC object.
    pub fn alloc(&mut self, value_meta: ValueMeta, slots: u16) -> GcRef {
        self.alloc_inner(value_meta, GC_OWNER_ALLOC_OBJECT, slots, slots as usize)
    }

    /// Allocate a heap object whose payload is a bare value-slot sequence.
    ///
    /// The header `ValueMeta` describes the payload slots directly, not a
    /// runtime object layout such as ArrayHeader or MapData.
    pub fn alloc_value_slots(&mut self, value_meta: ValueMeta, slots: u16) -> GcRef {
        if let Some(dispatch) = self.owner_dispatch {
            return unsafe {
                (dispatch.alloc)(
                    dispatch.state,
                    value_meta.to_raw(),
                    GC_OWNER_ALLOC_VALUE_SLOTS,
                    slots,
                    usize::from(slots),
                )
            };
        }
        let obj = self.alloc(value_meta, slots);
        if !obj.is_null() {
            unsafe { Self::header_mut(obj) }.set_value_slots_object();
        }
        obj
    }

    /// Allocate a large array. For arrays with total_slots > u16::MAX,
    /// GcHeader.slots is set to 0, and the actual size is read from ArrayHeader.
    pub fn alloc_array(&mut self, value_meta: ValueMeta, total_slots: usize) -> GcRef {
        let header_slots = if total_slots > u16::MAX as usize {
            0
        } else {
            total_slots as u16
        };
        self.alloc_inner(value_meta, GC_OWNER_ALLOC_ARRAY, header_slots, total_slots)
    }

    /// Allocate scalar runtime-container backing inside the Island heap.
    ///
    /// The allocation participates in the ordinary object table, hard limit,
    /// no-growth policy, incremental sweep, and telemetry. Container scanners
    /// retain it explicitly and trace any logical child references using the
    /// container's element metadata.
    pub fn alloc_runtime_backing(&mut self, total_slots: usize) -> GcRef {
        let backing = self.alloc_array(ValueMeta::new(0, ValueKind::Uint64), total_slots);
        if !backing.is_null() {
            unsafe { Self::header_mut(backing) }.set_runtime_backing_object();
            if self.owner_dispatch.is_none() {
                self.runtime_backing_bytes = self.runtime_backing_bytes.saturating_add(
                    GcHeader::SIZE.saturating_add(total_slots.saturating_mul(SLOT_BYTES)),
                );
            }
        }
        backing
    }

    fn alloc_inner(
        &mut self,
        value_meta: ValueMeta,
        allocation_kind: u8,
        header_slots: u16,
        slots: usize,
    ) -> GcRef {
        if let Some(dispatch) = self.owner_dispatch {
            // These object kinds install allocator-owning Rust payloads outside
            // the GC allocation itself (for example MapInner and queue state).
            // Extension code must use the corresponding context helper, whose
            // complete construction runs inside the host callback.
            match value_meta.value_kind() {
                ValueKind::Map | ValueKind::Channel | ValueKind::Port | ValueKind::Island => {
                    panic!(
                        "native extension cannot construct {:?} through ctx.gc(); use an allocator-neutral host capability",
                        value_meta.value_kind()
                    );
                }
                _ => {}
            }
            return unsafe {
                (dispatch.alloc)(
                    dispatch.state,
                    value_meta.to_raw(),
                    allocation_kind,
                    header_slots,
                    slots,
                )
            };
        }

        let header_size = GcHeader::SIZE;
        let data_size = match slots.checked_mul(SLOT_BYTES) {
            Some(s) => s,
            None => {
                // Overflow - allocation too large
                #[cfg(feature = "std")]
                eprintln!("GC allocation overflow: slots={}", slots);
                return core::ptr::null_mut();
            }
        };
        let total_size = match header_size.checked_add(data_size) {
            Some(s) => s,
            None => {
                #[cfg(feature = "std")]
                eprintln!(
                    "GC allocation overflow: header_size={}, data_size={}",
                    header_size, data_size
                );
                return core::ptr::null_mut();
            }
        };

        if self
            .max_objects
            .is_some_and(|max_objects| self.all_objects.len() >= max_objects)
        {
            self.record_allocation_failure(MemoryError::MetadataExhausted);
            return core::ptr::null_mut();
        }
        if self.all_objects.len() == self.all_objects.capacity()
            || self.all_object_data_sizes.len() == self.all_object_data_sizes.capacity()
            || self.allocation_extents.len() == self.allocation_extents.capacity()
        {
            if !self.heap.growth_allowed() {
                self.record_allocation_failure(MemoryError::MetadataExhausted);
                return core::ptr::null_mut();
            }
            if self.all_objects.try_reserve(1).is_err()
                || self.all_object_data_sizes.try_reserve(1).is_err()
                || self.allocation_extents.try_reserve(1).is_err()
            {
                self.record_allocation_failure(MemoryError::SystemAllocationFailed);
                return core::ptr::null_mut();
            }
        }
        if let Err(error) = self.reserve_object_work_queues(self.all_objects.capacity()) {
            self.record_allocation_failure(error);
            return core::ptr::null_mut();
        }

        let allocation = match self.heap.allocate(total_size) {
            Ok(allocation) => allocation,
            Err(error) => {
                self.record_allocation_failure(error.into());
                return core::ptr::null_mut();
            }
        };
        debug_assert!(allocation.capacity >= total_size);
        let ptr = allocation.raw;

        // New object gets current white color. During marking, queue it gray so
        // its initialized slots are scanned before the cycle reaches sweep.
        let header = GcHeader::new_with_white(value_meta, header_slots, self.current_white);
        unsafe {
            core::ptr::write(ptr as *mut GcHeader, header);
        }

        let data_ptr = unsafe { ptr.add(header_size) as GcRef };

        self.all_objects.push(data_ptr);
        self.all_object_data_sizes.push(data_size);
        let replaced_extent = self.allocation_extents.insert(
            data_ptr as usize,
            AllocationExtent {
                data_size,
                remembered_index: None,
            },
        );
        assert!(
            replaced_extent.is_none(),
            "new allocation must not replace a live allocation extent"
        );
        self.total_bytes += total_size;
        self.live_object_count += 1;
        self.young_live_bytes += total_size;
        if total_size > (1usize << 15) {
            self.large_live_bytes += total_size;
        }
        self.allocation_bytes_total = self
            .allocation_bytes_total
            .saturating_add(total_size as u64);
        self.debt += total_size as i64;
        if matches!(self.state, GcState::Propagate | GcState::Atomic) {
            unsafe { Self::header_mut(data_ptr) }.set_gray();
            debug_assert!(self.gray.len() < self.gray.capacity());
            self.gray.push(data_ptr);
        }

        #[cfg(feature = "gc-debug")]
        crate::gc_debug::on_alloc(data_ptr);

        data_ptr
    }

    #[inline]
    fn record_allocation_failure(&mut self, error: MemoryError) {
        self.allocation_failures = self.allocation_failures.saturating_add(1);
        self.last_memory_error = Some(error);
    }

    /// Record fallible host-side runtime metadata allocation owned by this
    /// Island. Container/transport helpers use this to preserve the same sticky
    /// OOM path as managed allocations.
    pub(crate) fn record_system_allocation_failure(&mut self) {
        self.record_allocation_failure(MemoryError::SystemAllocationFailed);
    }

    /// Read a slot from a GC object.
    /// # Safety
    /// obj must be a valid GcRef and idx must be within bounds.
    #[inline]
    pub unsafe fn read_slot(obj: GcRef, idx: usize) -> u64 {
        *obj.add(idx)
    }

    /// Write a slot to a GC object.
    /// # Safety
    /// obj must be a valid GcRef and idx must be within bounds.
    #[inline]
    pub unsafe fn write_slot(obj: GcRef, idx: usize, val: u64) {
        *obj.add(idx) = val
    }

    /// Get the header of a GC object.
    ///
    /// # Safety
    /// `obj` must be the base address of a live allocation owned by a `Gc`.
    /// The allocation must outlive the returned borrow.
    #[inline]
    pub unsafe fn header<'a>(obj: GcRef) -> &'a GcHeader {
        unsafe { &*((obj as *const u8).sub(GcHeader::SIZE) as *const GcHeader) }
    }

    /// Get mutable header of a GC object.
    ///
    /// # Safety
    /// `obj` must be the base address of a live allocation owned by a `Gc`, and
    /// the caller must hold exclusive access to its header for the returned
    /// borrow.
    #[inline]
    pub unsafe fn header_mut<'a>(obj: GcRef) -> &'a mut GcHeader {
        unsafe { &mut *((obj as *mut u8).sub(GcHeader::SIZE) as *mut GcHeader) }
    }

    /// Compute object size in bytes from header.
    /// For large arrays (slots == 0), reads actual size from ArrayHeader.
    #[inline]
    fn object_size_bytes(obj: GcRef) -> usize {
        use crate::objects::array;
        let header = unsafe { Self::header(obj) };
        let slots = if header.is_value_slots_object() {
            header.slots as usize
        } else if header.slots == 0 && header.kind() == ValueKind::Array {
            // Safety: `obj` was allocated by this collector and its header
            // identifies the array layout used below.
            unsafe { array::total_slots(obj) }
        } else {
            header.slots as usize
        };
        GcHeader::SIZE + slots * SLOT_BYTES
    }

    fn allocated_data_size_bytes_for_base(&self, obj: GcRef) -> Option<usize> {
        self.allocation_extents
            .get(&(obj as usize))
            .map(|extent| extent.data_size)
    }

    pub fn allocated_data_size_bytes(&self, obj: GcRef) -> Option<usize> {
        self.reject_owner_proxy_api("allocated_data_size_bytes");
        if obj.is_null() {
            return Some(0);
        }
        let base = self.canonicalize_ref(obj)?;
        if base != obj {
            return None;
        }
        self.allocated_data_size_bytes_for_base(base)
    }

    pub fn ref_data_range(&self, obj: GcRef) -> Option<(GcRef, usize, usize)> {
        self.reject_owner_proxy_api("ref_data_range");
        if obj.is_null() {
            return Some((obj, 0, 0));
        }
        let base = self.canonicalize_ref(obj)?;
        let data_size = self.allocated_data_size_bytes_for_base(base)?;
        let offset = (obj as usize).checked_sub(base as usize)?;
        if offset > data_size {
            return None;
        }
        Some((base, offset, data_size))
    }

    // The owner-dispatch callback receives an opaque handle. It validates the
    // handle in the owning collector before any dereference.
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn canonicalize_ref(&self, obj: GcRef) -> Option<GcRef> {
        if let Some(dispatch) = self.owner_dispatch {
            if obj.is_null() {
                return Some(obj);
            }
            let canonical = unsafe { (dispatch.canonicalize)(dispatch.state, obj) };
            return (!canonical.is_null()).then_some(canonical);
        }

        if obj.is_null() {
            return Some(obj);
        }

        let addr = obj as usize;
        if (addr & (SLOT_BYTES - 1)) != 0 || addr < 4096 {
            return None;
        }

        let located = self.heap.locate(addr, GcHeader::SIZE)?;
        let base = unsafe { located.raw.add(GcHeader::SIZE) as GcRef };
        let data_size = self.allocated_data_size_bytes_for_base(base)?;
        if GcHeader::SIZE.saturating_add(data_size) > located.capacity {
            return None;
        }
        let data_end = (base as usize).checked_add(data_size)?;
        (addr == base as usize || addr < data_end).then_some(base)
    }

    /// Mark an object as gray (pending scan).
    #[inline]
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn mark_gray(&mut self, obj: GcRef) {
        if let Some(dispatch) = self.owner_dispatch {
            unsafe { (dispatch.mark_gray)(dispatch.state, obj) };
            return;
        }
        if obj.is_null() {
            return;
        }
        let Some(obj) = self.canonicalize_ref(obj) else {
            self.mark_gray_fail(obj);
        };
        if self.pending_remembered_parent.is_some() && unsafe { Self::header(obj) }.age() < G_OLD {
            self.pending_remembered_has_young = true;
        }
        if self.state == GcState::Sweep {
            self.mark_dead_white_gray(obj);
            return;
        }
        let header = unsafe { Self::header_mut(obj) };
        if header.is_white() {
            header.set_gray();
            debug_assert!(self.gray.len() < self.gray.capacity());
            self.gray.push(obj);
        }
    }

    #[inline]
    fn mark_dead_white_gray(&mut self, obj: GcRef) {
        let dead_white = self.other_white();
        let header = unsafe { Self::header_mut(obj) };
        if header.marked & WHITE_BITS == dead_white {
            header.set_gray();
            debug_assert!(self.gray.len() < self.gray.capacity());
            self.gray.push(obj);
        }
    }

    /// Queue a fully-initialized object allocated during sweep for scanning.
    ///
    /// Sweep treats current-white objects as live, but a newly allocated wrapper
    /// can contain copied references to old-white objects. Those children must be
    /// traced before sweep can free them. Call this only after the object's slots
    /// have been initialized.
    #[inline]
    #[track_caller]
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn mark_allocated_for_scan(&mut self, obj: GcRef) {
        if let Some(dispatch) = self.owner_dispatch {
            unsafe { (dispatch.mark_allocated_for_scan)(dispatch.state, obj) };
            return;
        }
        if self.state != GcState::Sweep || obj.is_null() {
            return;
        }
        let Some(obj) = self.canonicalize_ref(obj) else {
            self.mark_gray_fail(obj);
        };
        let header = unsafe { Self::header_mut(obj) };
        if header.marked & WHITE_BITS == self.current_white {
            header.set_gray();
            debug_assert!(self.gray.len() < self.gray.capacity());
            self.gray.push(obj);
        }
    }

    #[cold]
    #[track_caller]
    #[inline(never)]
    fn mark_gray_fail(&self, obj: GcRef) -> ! {
        let loc = core::panic::Location::caller();
        panic!(
            "mark_gray: invalid GcRef {:p} (raw={:#x}) — non-GcRef value in GcRef-typed slot caller={}:{}",
            obj,
            obj as usize,
            loc.file(),
            loc.line(),
        );
    }

    fn remember_parent(&mut self, parent: GcRef) {
        let key = parent as usize;
        let already_remembered = self
            .allocation_extents
            .get(&key)
            .and_then(|extent| extent.remembered_index)
            .is_some();
        if already_remembered {
            unsafe { Self::header_mut(parent) }.set_remembered_forget_pending(false);
            return;
        }
        let index = self.remembered.len();
        debug_assert!(self.remembered.len() < self.remembered.capacity());
        self.remembered.push(parent);
        self.allocation_extents
            .get_mut(&key)
            .expect("live parent must have allocation metadata")
            .remembered_index = Some(index);
    }

    fn forget_remembered_parent(&mut self, parent: GcRef) {
        unsafe { Self::header_mut(parent) }.set_remembered_forget_pending(false);
        let key = parent as usize;
        let Some(index) = self
            .allocation_extents
            .get_mut(&key)
            .and_then(|extent| extent.remembered_index.take())
        else {
            return;
        };
        let moved = self.remembered.swap_remove(index);
        debug_assert_eq!(moved, parent);
        if index < self.remembered.len() {
            let replacement = self.remembered[index];
            self.allocation_extents
                .get_mut(&(replacement as usize))
                .expect("remembered parent must have allocation metadata")
                .remembered_index = Some(index);
        }
    }

    /// Typed new-value barrier shared by the interpreter, JIT, stdlib, and FFI.
    ///
    /// The barrier shades a newly stored white child when the parent has
    /// already been scanned. In generational mode it also remembers an
    /// old-to-young edge. Callers invoke this before publishing the store so a
    /// validation failure cannot leave a partially-mutated object.
    #[track_caller]
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn write_barrier(&mut self, parent: GcRef, child: GcRef) {
        if parent.is_null() || child.is_null() {
            return;
        }
        let Some(parent) = self.canonicalize_ref(parent) else {
            self.write_barrier_parent_fail(parent, child);
        };
        let Some(child) = self.canonicalize_ref(child) else {
            return;
        };
        self.write_barrier_canonicalized(parent, child);
    }

    /// Fail-closed barrier entry for runtime ABIs that cannot surface an
    /// invalid-parent panic. Both references are canonicalized exactly once in
    /// the owning collector's common case.
    pub(crate) fn write_barrier_if_valid(&mut self, parent: GcRef, child: GcRef) {
        if parent.is_null() || child.is_null() {
            return;
        }
        let Some(parent) = self.canonicalize_ref(parent) else {
            return;
        };
        let Some(child) = self.canonicalize_ref(child) else {
            return;
        };
        self.write_barrier_canonicalized(parent, child);
    }

    fn write_barrier_canonicalized(&mut self, parent: GcRef, child: GcRef) {
        if let Some(dispatch) = self.owner_dispatch {
            unsafe { (dispatch.write_barrier)(dispatch.state, parent, child) };
            return;
        }
        #[cfg(feature = "gc-debug")]
        crate::gc_debug::on_barrier(parent, 0, child as u64);

        let parent_age = unsafe { Self::header(parent) }.age();
        let child_age = unsafe { Self::header(child) }.age();
        if self.gc_mode == GcMode::Generational && parent_age >= G_OLD && child_age < G_OLD {
            self.remember_parent(parent);
            if self.cycle_kind == GcCycleKind::Minor
                && matches!(self.state, GcState::Propagate | GcState::Atomic)
            {
                self.mark_gray(child);
            }
        }

        match self.state {
            GcState::Propagate | GcState::Atomic => {
                let p_header = unsafe { Self::header(parent) };
                let c_header = unsafe { Self::header(child) };
                // Incremental-update barrier: preserve the strong tri-color
                // invariant by shading the new child.
                if (p_header.is_black() || p_header.is_gray()) && c_header.is_white() {
                    self.mark_gray(child);
                }
            }
            GcState::Sweep => {
                // During sweep an old-white object may become reachable again
                // through a root or heap write. It must be rescanned, not just
                // blackened, because composite values such as strings/slices
                // own backing arrays that would otherwise still be swept.
                self.mark_dead_white_gray(parent);
                self.mark_dead_white_gray(child);
            }
            GcState::Pause | GcState::Reclaim => {}
        }
    }

    #[cold]
    #[track_caller]
    #[inline(never)]
    fn write_barrier_parent_fail(&self, parent: GcRef, child: GcRef) -> ! {
        let loc = core::panic::Location::caller();
        panic!(
            "write_barrier: invalid parent {:p} (raw={:#x}) child={:p} child_raw={:#x} state={:?} caller={}:{}",
            parent,
            parent as usize,
            child,
            child as usize,
            self.state,
            loc.file(),
            loc.line(),
        );
    }

    /// Check if object is black (for gc-debug)
    #[inline]
    pub fn is_black(&self, obj: GcRef) -> bool {
        if obj.is_null() {
            return false;
        }
        self.canonicalize_ref(obj)
            .map(|base| !base.is_null() && unsafe { Self::header(base) }.is_black())
            .unwrap_or(false)
    }

    /// Check if object is white (for gc-debug)
    #[inline]
    pub fn is_white(&self, obj: GcRef) -> bool {
        if obj.is_null() {
            return false;
        }
        self.canonicalize_ref(obj)
            .map(|base| !base.is_null() && unsafe { Self::header(base) }.is_white())
            .unwrap_or(false)
    }

    /// Enable or disable GC stress mode.
    #[inline]
    pub fn set_stress_every_step(&mut self, enabled: bool) {
        self.reject_owner_proxy_api("set_stress_every_step");
        self.stress_every_step = enabled;
    }

    /// Returns whether GC stress mode is enabled.
    #[inline]
    pub fn stress_every_step(&self) -> bool {
        self.reject_owner_proxy_api("stress_every_step");
        self.stress_every_step
    }

    /// Check if GC should run.
    ///
    /// Debt starts a new cycle. Once a cycle has started, keep advancing it at
    /// every scheduler boundary so mark/sweep work is amortized across frames
    /// instead of bunching up behind future allocations. Stress mode forces a
    /// step at every scheduler boundary to expose write-barrier bugs.
    #[inline]
    pub fn should_step(&self) -> bool {
        self.reject_owner_proxy_api("should_step");
        self.stress_every_step
            || (self.automatic_gc && (self.debt > 0 || self.state != GcState::Pause))
    }

    /// Incremental GC step. Returns work done (bytes processed).
    /// Call this when should_step() is true, passing scan_roots and scan_object callbacks.
    ///
    /// # Safety
    ///
    /// The caller must own a VM or test boundary where all precise roots are
    /// stable and the supplied scanner covers every live root for the duration
    /// of the step.
    pub unsafe fn step<R, S, F>(
        &mut self,
        scan_roots: R,
        scan_object: S,
        finalize_object: F,
    ) -> usize
    where
        R: FnMut(&mut Gc),
        S: FnMut(&mut Gc, GcRef),
        F: FnMut(GcRef),
    {
        self.reject_owner_proxy_api("step");
        unsafe {
            self.step_with_root_state(
                GcRootState::MayHaveChanged,
                scan_roots,
                scan_object,
                finalize_object,
            )
        }
    }

    /// Incremental GC step with an explicit root-set freshness contract.
    ///
    /// The safe default is `GcRootState::MayHaveChanged`. Passing
    /// `StableSinceLastScan` is correct only when the caller controls all roots
    /// and can prove none changed since this collector last ran `scan_roots`.
    /// # Safety
    ///
    /// The caller must control the root-set freshness described by
    /// `root_state`, and the supplied callbacks must cover every live root and
    /// object layout reachable at this VM/test boundary.
    pub unsafe fn step_with_root_state<R, S, F>(
        &mut self,
        root_state: GcRootState,
        mut scan_roots: R,
        mut scan_object: S,
        mut finalize_object: F,
    ) -> usize
    where
        R: FnMut(&mut Gc),
        S: FnMut(&mut Gc, GcRef),
        F: FnMut(GcRef),
    {
        self.reject_owner_proxy_api("step_with_root_state");
        unsafe {
            self.step_with_root_scanner(
                root_state,
                |gc, _kind, _limit| {
                    scan_roots(gc);
                    GcRootScanChunk::complete(0)
                },
                &mut scan_object,
                &mut finalize_object,
            )
        }
    }

    /// Incremental GC step with a bounded root scanner and an atomic
    /// heap-object scanner retained for compatibility.
    ///
    /// Production hosts should use [`Gc::step_with_scanners`] so large objects
    /// can pause at reference-slot boundaries.
    ///
    /// # Safety
    ///
    /// The caller must control the freshness of `root_state`; `scan_roots`
    /// must cover every live root, and `scan_object` must trace every managed
    /// reference in each object it receives.
    pub unsafe fn step_with_root_scanner<R, S, F>(
        &mut self,
        root_state: GcRootState,
        scan_roots: R,
        mut scan_object: S,
        finalize_object: F,
    ) -> usize
    where
        R: FnMut(&mut Gc, GcRootScanKind, usize) -> GcRootScanChunk,
        S: FnMut(&mut Gc, GcRef),
        F: FnMut(GcRef),
    {
        unsafe {
            self.step_with_scanners(
                root_state,
                scan_roots,
                |gc, obj, _cursor, limit| {
                    scan_object(gc, obj);
                    GcObjectScanChunk::complete(Self::object_size_bytes(obj).min(limit))
                },
                finalize_object,
            )
        }
    }

    /// Incremental GC step with bounded root and heap-object scanners.
    ///
    /// This API is for hosts with very large root sets. The scanner may process
    /// up to `limit_bytes` worth of root work and return `pending`; the collector
    /// will resume the same `GcRootScanKind` on the next step.
    ///
    /// Correctness contract: while a root scan pass is pending, the caller must
    /// either keep the scanned root set stable or use its own dirty/restart
    /// protocol so roots changed behind the cursor are not lost. `Gc::step` and
    /// `step_with_root_state` remain the conservative default for callers that
    /// cannot provide that proof.
    /// # Safety
    ///
    /// The caller must prove root-set stability or dirty-restart ownership for
    /// pending chunked scans, and the callbacks must remain valid while the
    /// collector advances through mark/sweep work.
    pub unsafe fn step_with_scanners<R, S, F>(
        &mut self,
        root_state: GcRootState,
        scan_roots: R,
        scan_object: S,
        finalize_object: F,
    ) -> usize
    where
        R: FnMut(&mut Gc, GcRootScanKind, usize) -> GcRootScanChunk,
        S: FnMut(&mut Gc, GcRef, &mut GcTraceCursor, usize) -> GcObjectScanChunk,
        F: FnMut(GcRef),
    {
        unsafe {
            self.step_with_scanners_budget(
                root_state,
                usize::MAX / SLOT_BYTES,
                scan_roots,
                scan_object,
                finalize_object,
            )
        }
    }

    /// Variant of [`Gc::step_with_scanners`] with a strict work-unit ceiling.
    ///
    /// A work unit is one root slot, trace slot, object-table entry, swept
    /// allocation, or reclaimed heap block. A zero budget observes no heap
    /// state and performs no collector transition.
    ///
    /// # Safety
    ///
    /// The caller must preserve the root-set and object-scanner ownership
    /// requirements of [`Gc::step_with_scanners`] for every bounded call and
    /// across every resumed cursor.
    pub unsafe fn step_with_scanners_budget<R, S, F>(
        &mut self,
        root_state: GcRootState,
        work_unit_limit: usize,
        mut scan_roots: R,
        mut scan_object: S,
        mut finalize_object: F,
    ) -> usize
    where
        R: FnMut(&mut Gc, GcRootScanKind, usize) -> GcRootScanChunk,
        S: FnMut(&mut Gc, GcRef, &mut GcTraceCursor, usize) -> GcObjectScanChunk,
        F: FnMut(GcRef),
    {
        self.reject_owner_proxy_api("step_with_scanners");
        if work_unit_limit == 0 {
            return 0;
        }
        let mut work = 0usize;
        let requested_limit = work_unit_limit.saturating_mul(SLOT_BYTES);
        let base = self.stepsize * self.stepmul as usize / 100;
        let mut stats = GcStepStats {
            phase_before: self.state,
            cycle_kind: self.cycle_kind,
            root_state,
            heap_bytes_before: self.total_bytes,
            debt_before: self.debt,
            gray_len_before: self.gray.len(),
            remembered_len_before: self.remembered.len(),
            ..GcStepStats::default()
        };

        // work_limit: allocation-proportional budget for DEBT TRACKING only.
        // This controls how much debt is repaid per step; it does NOT control
        // how much work a step actually does (that's phase_limit's job).
        let work_limit = base.max(self.debt.max(0) as usize);

        // Target frame count for each GC phase (Propagate/Sweep).
        //
        // Phase acceleration is needed because allocation-proportional debt alone
        // can make large mostly-live heaps take thousands of frames to finish a
        // cycle. Cap one incremental slice at 1 MiB so the collector can keep
        // pace with allocation-heavy interactive workloads without turning one
        // scheduler boundary into an unbounded full-heap scan. The old 8 KiB cap
        // limited collection to roughly 160 KiB/s at a 20 Hz game tick, allowing
        // ordinary presentation allocations to outrun the collector indefinitely.
        const TARGET_PHASE_FRAMES: usize = 128;
        const MAX_PHASE_STEP_BYTES: usize = 1024 * 1024;

        loop {
            let phase_limit = (self.total_bytes / TARGET_PHASE_FRAMES)
                .max(base)
                .min(MAX_PHASE_STEP_BYTES)
                .min(requested_limit);
            // Scanner work is accounted in whole slots. A heap-derived phase
            // budget can have a 1..SLOT_BYTES-1 remainder; returning that
            // remainder to the next scheduler boundary prevents a zero-work
            // spin in Propagate or Sweep.
            if phase_limit.saturating_sub(work) < SLOT_BYTES {
                break;
            }

            if let Some(kind) = self.pending_root_scan {
                if self.root_lease_scan_kind != Some(kind) {
                    self.root_lease_scan_kind = Some(kind);
                    self.root_lease_scan_cursor = 0;
                }
                while self.root_lease_scan_cursor < self.leases.len() && work < phase_limit {
                    let root = self.leases[self.root_lease_scan_cursor].root;
                    self.root_lease_scan_cursor += 1;
                    work += SLOT_BYTES;
                    stats.root_scan_work_bytes += SLOT_BYTES;
                    if !root.is_null() {
                        self.mark_gray(root);
                    }
                }
                if self.root_lease_scan_cursor < self.leases.len() || work >= phase_limit {
                    break;
                }
                let limit = phase_limit - work;
                stats.root_scan_calls += 1;
                let chunk = scan_roots(self, kind, limit);
                debug_assert!(
                    chunk.done || chunk.work_bytes > 0,
                    "bounded GC root scanner returned pending without progress"
                );
                stats.root_scan_work_bytes += chunk.work_bytes;
                work += chunk.work_bytes;

                if !chunk.done {
                    break;
                }

                self.pending_root_scan = None;
                self.root_lease_scan_kind = None;
                self.root_lease_scan_cursor = 0;
                if kind == GcRootScanKind::Atomic {
                    self.atomic_root_scan_complete = true;
                    self.remark_rounds = self.remark_rounds.saturating_add(1);
                }
                if kind == GcRootScanKind::Sweep {
                    self.sweep_root_scan_complete = true;
                }
                if work >= phase_limit {
                    break;
                }
            }

            match self.state {
                GcState::Pause => {
                    // Start new cycle
                    stats.cycle_started = true;
                    const MINOR_CYCLES_PER_MAJOR: u8 = 8;
                    self.cycle_kind = if self.gc_mode == GcMode::Generational
                        && !self.force_major_cycle
                        && self.minor_cycles_since_major < MINOR_CYCLES_PER_MAJOR
                    {
                        GcCycleKind::Minor
                    } else {
                        GcCycleKind::Major
                    };
                    stats.cycle_kind = self.cycle_kind;
                    self.force_major_cycle = false;
                    self.cycle_id = self.cycle_id.saturating_add(1);
                    self.current_white ^= WHITE_BITS;
                    debug_assert!(self.remembered_forget.is_empty());
                    self.remembered_scan_pos = 0;
                    // Snapshot the stable remembered-parent prefix. New
                    // old-to-young stores shade their child immediately and
                    // join the next minor cycle's prefix.
                    self.remembered_scan_end = self.remembered.len();
                    self.atomic_root_scan_complete = false;
                    self.sweep_root_scan_complete = false;
                    self.pending_root_scan = Some(GcRootScanKind::StartCycle);
                    self.state = GcState::Propagate;
                }

                GcState::Propagate => {
                    // phase_limit for Propagate: total_bytes / TARGET.
                    // total_bytes only increases during Propagate (new allocs, no frees),
                    // so this is stable/increasing across steps — no convergence issue.
                    // Use .max(base) NOT .max(work_limit) to avoid first-cycle spikes
                    // where debt = total_bytes would make phase_limit = entire heap.
                    let remembered_work = if self.cycle_kind == GcCycleKind::Minor {
                        self.remembered_step(phase_limit.saturating_sub(work))
                    } else {
                        self.remembered_scan_pos = self.remembered_scan_end;
                        0
                    };
                    stats.propagate_work_bytes += remembered_work;
                    work += remembered_work;
                    if work >= phase_limit || self.remembered_scan_pos < self.remembered_scan_end {
                        break;
                    }
                    let propagate_work = {
                        let mut counted_scan_object =
                            |gc: &mut Gc, obj: GcRef, cursor: &mut GcTraceCursor, limit: usize| {
                                if *cursor == GcTraceCursor::default() {
                                    stats.object_scans += 1;
                                }
                                scan_object(gc, obj, cursor, limit)
                            };
                        self.propagate_step(
                            &mut counted_scan_object,
                            phase_limit.saturating_sub(work),
                        )
                    };
                    stats.propagate_work_bytes += propagate_work;
                    work += propagate_work;

                    let forget_work = self.remembered_forget_step(phase_limit.saturating_sub(work));
                    stats.propagate_work_bytes += forget_work;
                    work += forget_work;

                    if self.gray.is_empty()
                        && self.pending_object_scan.is_none()
                        && self.remembered_scan_pos >= self.remembered_scan_end
                        && self.remembered_forget.is_empty()
                    {
                        self.state = GcState::Atomic;
                        break;
                    } else if work >= phase_limit {
                        break;
                    }
                }

                GcState::Atomic => {
                    // Roots are mutable during incremental marking. A stack slot or global
                    // can start pointing at an old-white object after start_cycle() has
                    // already scanned roots, so rescan roots at the atomic boundary before
                    // finalizing the mark set.
                    if !self.atomic_root_scan_complete {
                        self.pending_root_scan = Some(GcRootScanKind::Atomic);
                        continue;
                    }
                    let propagate_work = {
                        let mut counted_scan_object =
                            |gc: &mut Gc, obj: GcRef, cursor: &mut GcTraceCursor, limit: usize| {
                                if *cursor == GcTraceCursor::default() {
                                    stats.object_scans += 1;
                                }
                                scan_object(gc, obj, cursor, limit)
                            };
                        self.propagate_step(
                            &mut counted_scan_object,
                            phase_limit.saturating_sub(work),
                        )
                    };
                    stats.propagate_work_bytes += propagate_work;
                    work += propagate_work;
                    if !self.gray.is_empty() || self.pending_object_scan.is_some() {
                        break;
                    }
                    self.state = GcState::Sweep;
                    self.sweep_pos = 0;
                    self.sweep_write_pos = 0;
                    // Snapshot sweep budget at sweep start. total_bytes here includes
                    // all objects (alive + dead). Using a fixed budget prevents the
                    // convergence problem: if we recomputed total_bytes/TARGET each step,
                    // freed dead bytes would shrink total_bytes, shrinking the budget,
                    // causing exponential decay instead of linear progress (99% dead heap
                    // would need ~130 steps instead of 32).
                    self.sweep_budget = (self.total_bytes / TARGET_PHASE_FRAMES)
                        .max(base)
                        .min(MAX_PHASE_STEP_BYTES);
                    break;
                }

                GcState::Sweep => {
                    let sweep_limit = self.sweep_budget.min(phase_limit);
                    // Mutator roots can change while sweep is incremental. Rescue
                    // any newly reachable old-white graph before sweeping the next
                    // chunk; mark_gray() is sweep-aware and ignores current-white
                    // objects that have already survived this cycle.
                    if root_state == GcRootState::MayHaveChanged {
                        if !self.sweep_root_scan_complete {
                            self.pending_root_scan = Some(GcRootScanKind::Sweep);
                            continue;
                        }
                    } else if !self.sweep_root_scan_complete {
                        stats.root_scan_skips += 1;
                    }
                    let rescue_work = {
                        let mut counted_scan_object =
                            |gc: &mut Gc, obj: GcRef, cursor: &mut GcTraceCursor, limit: usize| {
                                if *cursor == GcTraceCursor::default() {
                                    stats.object_scans += 1;
                                }
                                scan_object(gc, obj, cursor, limit)
                            };
                        self.propagate_step(
                            &mut counted_scan_object,
                            sweep_limit.saturating_sub(work),
                        )
                    };
                    stats.propagate_work_bytes += rescue_work;
                    work += rescue_work;
                    if !self.gray.is_empty() || self.pending_object_scan.is_some() {
                        break;
                    }
                    let sweep_work = self.sweep_step_counted(
                        &mut finalize_object,
                        sweep_limit.saturating_sub(work),
                        &mut stats,
                    );
                    stats.sweep_work_bytes += sweep_work;
                    work += sweep_work;
                    self.sweep_root_scan_complete = false;

                    if self.sweep_pos >= self.all_objects.len() {
                        if self.heap.stats().pending_reclaim_bytes == 0 {
                            self.finish_cycle();
                            stats.cycle_finished = true;
                        } else {
                            self.state = GcState::Reclaim;
                        }
                        break;
                    } else if work >= sweep_limit {
                        break;
                    }
                }

                GcState::Reclaim => {
                    let remaining = phase_limit.saturating_sub(work);
                    let block_budget = remaining / SLOT_BYTES;
                    let (blocks, done) = self.heap.reclaim_step(block_budget);
                    let reclaim_work = blocks.saturating_mul(SLOT_BYTES);
                    stats.sweep_work_bytes = stats.sweep_work_bytes.saturating_add(reclaim_work);
                    work = work.saturating_add(reclaim_work);
                    if done {
                        self.finish_cycle();
                        stats.cycle_finished = true;
                    }
                    break;
                }
            }
        }

        // Debt tracks allocation-proportional work. Phase-accelerated work (done to
        // finish within TARGET_PHASE_FRAMES) may far exceed the allocation budget;
        // crediting all of it would make debt hugely negative and delay the next cycle.
        self.debt -= (work as i64).min(work_limit as i64);
        stats.phase_after = self.state;
        stats.total_work_bytes = work;
        stats.heap_bytes_after = self.total_bytes;
        stats.debt_after = self.debt;
        stats.gray_len_after = self.gray.len();
        stats.remembered_len_after = self.remembered.len();
        let step_units = work.div_ceil(SLOT_BYTES);
        self.work_units_total = self.work_units_total.saturating_add(step_units as u64);
        self.max_step_work_units = self.max_step_work_units.max(step_units);
        self.last_step_stats = stats;
        work
    }

    /// Queue the stable prefix of old parents known to contain young edges.
    fn remembered_step(&mut self, limit: usize) -> usize {
        let entry_budget = limit / SLOT_BYTES;
        if entry_budget == 0 {
            return 0;
        }
        let start = self.remembered_scan_pos;
        let end = self
            .remembered_scan_pos
            .saturating_add(entry_budget)
            .min(self.remembered_scan_end);
        while self.remembered_scan_pos < end {
            let obj = self.remembered[self.remembered_scan_pos];
            self.remembered_scan_pos += 1;
            let header = unsafe { Self::header(obj) };
            debug_assert!(header.age() >= G_OLD);
            if header.is_white() {
                unsafe { Self::header_mut(obj) }.set_gray();
                debug_assert!(self.gray.len() < self.gray.capacity());
                self.gray.push(obj);
            }
        }
        (end - start) * SLOT_BYTES
    }

    /// Retire remembered parents after the stable prefix has been queued.
    /// Delaying swap-removal keeps the snapshot cursor valid across small
    /// incremental budgets.
    fn remembered_forget_step(&mut self, limit: usize) -> usize {
        let count = (limit / SLOT_BYTES).min(self.remembered_forget.len());
        for _ in 0..count {
            let parent = self
                .remembered_forget
                .pop()
                .expect("bounded remembered retirement count must be available");
            if unsafe { Self::header(parent) }.remembered_forget_pending() {
                self.forget_remembered_parent(parent);
            }
        }
        count * SLOT_BYTES
    }

    /// Propagate marking incrementally. Returns work done.
    fn propagate_step<S: FnMut(&mut Gc, GcRef, &mut GcTraceCursor, usize) -> GcObjectScanChunk>(
        &mut self,
        scan_object: &mut S,
        limit: usize,
    ) -> usize {
        let mut work = 0usize;

        while work < limit {
            let obj = if let Some(obj) = self.pending_object_scan {
                obj
            } else {
                let Some(obj) = self.gray.pop() else {
                    break;
                };
                self.pending_trace_cursor = GcTraceCursor::default();
                self.pending_object_scan = Some(obj);
                obj
            };

            debug_assert!(
                !obj.is_null() && (obj as usize) & (SLOT_BYTES - 1) == 0 && (obj as usize) >= 4096,
                "propagate_step: invalid GcRef {:p} in gray queue",
                obj
            );
            if unsafe { Self::header(obj) }.is_black() {
                if self.pending_remembered_parent == Some(obj) {
                    self.pending_remembered_parent = None;
                    self.pending_remembered_has_young = false;
                }
                self.pending_object_scan = None;
                self.pending_trace_cursor = GcTraceCursor::default();
                continue;
            }

            if self.pending_remembered_parent.is_none()
                && self.cycle_kind == GcCycleKind::Minor
                && self
                    .allocation_extents
                    .get(&(obj as usize))
                    .and_then(|extent| extent.remembered_index)
                    .is_some()
            {
                self.pending_remembered_parent = Some(obj);
                self.pending_remembered_has_young = false;
            }

            let remaining = limit.saturating_sub(work);
            if remaining < SLOT_BYTES {
                break;
            }
            let mut cursor = self.pending_trace_cursor;
            let chunk = scan_object(self, obj, &mut cursor, remaining);
            debug_assert!(
                chunk.done || chunk.work_bytes > 0,
                "bounded GC object scanner returned pending without progress"
            );
            debug_assert!(
                chunk.work_bytes <= remaining,
                "bounded GC object scanner exceeded its work limit"
            );
            self.pending_trace_cursor = cursor;
            work = work.saturating_add(chunk.work_bytes);
            if chunk.done {
                unsafe { Self::header_mut(obj) }.set_black();
                if self.pending_remembered_parent == Some(obj) {
                    self.pending_remembered_parent = None;
                    if !core::mem::take(&mut self.pending_remembered_has_young) {
                        unsafe { Self::header_mut(obj) }.set_remembered_forget_pending(true);
                        debug_assert!(
                            self.remembered_forget.len() < self.remembered_forget.capacity()
                        );
                        self.remembered_forget.push(obj);
                    }
                }
                self.pending_object_scan = None;
                self.pending_trace_cursor = GcTraceCursor::default();
            } else {
                break;
            }
        }

        work
    }

    #[cfg(test)]
    fn atomic_phase<S: FnMut(&mut Gc, GcRef)>(&mut self, scan_object: &mut S) {
        let mut atomic_scanner =
            |gc: &mut Gc, obj: GcRef, _cursor: &mut GcTraceCursor, limit: usize| {
                scan_object(gc, obj);
                GcObjectScanChunk::complete(Self::object_size_bytes(obj).min(limit))
            };
        while self.pending_object_scan.is_some() || !self.gray.is_empty() {
            let work = self.propagate_step(&mut atomic_scanner, usize::MAX);
            assert!(work > 0, "test atomic drain must make progress");
        }
    }

    /// Sweep dead objects incrementally. Returns work done.
    #[cfg(test)]
    fn sweep_step<F: FnMut(GcRef)>(&mut self, finalize_object: &mut F, limit: usize) -> usize {
        let mut stats = GcStepStats::default();
        self.sweep_step_counted(finalize_object, limit, &mut stats)
    }

    /// Sweep dead objects incrementally and record telemetry. Returns work done.
    fn sweep_step_counted<F: FnMut(GcRef)>(
        &mut self,
        finalize_object: &mut F,
        limit: usize,
        stats: &mut GcStepStats,
    ) -> usize {
        let mut work = 0;
        let dead_white = self.other_white();

        while self.sweep_pos < self.all_objects.len() && work < limit {
            let obj = self.all_objects[self.sweep_pos];
            let header = unsafe { Self::header(obj) };
            let obj_white = header.marked & WHITE_BITS;
            let age = header.age();
            let runtime_backing = header.is_runtime_backing_object();
            let data_size = self.all_object_data_sizes[self.sweep_pos];

            // Gray objects should never exist during sweep — atomic phase processes all of them.
            // If one leaks here, it would be silently dropped (memory leak). Catch it early.
            debug_assert!(
                header.is_black() || obj_white != 0,
                "sweep_step: gray object {:p} found during sweep (neither white nor black)",
                obj
            );

            // Compute size once — both alive and dead branches need it for work accounting.
            let size_bytes = GcHeader::SIZE + data_size;

            let retained_old = self.cycle_kind == GcCycleKind::Minor && age >= G_OLD;
            if header.is_black() || obj_white == self.current_white || retained_old {
                // Alive: reset to current white
                let mut promoted_to_old = false;
                let header = unsafe { Self::header_mut(obj) };
                if self.gc_mode == GcMode::Generational && age < G_OLD {
                    let next_age = if age == G_YOUNG { G_SURVIVAL } else { G_OLD };
                    header.set_age(next_age);
                    promoted_to_old = next_age >= G_OLD;
                }
                header.set_white(self.current_white);
                if promoted_to_old {
                    self.young_live_bytes -= size_bytes;
                    self.old_live_bytes += size_bytes;
                    self.remember_parent(obj);
                }
                self.all_objects[self.sweep_write_pos] = obj;
                self.all_object_data_sizes[self.sweep_write_pos] = data_size;
                self.sweep_write_pos += 1;
                // Sweeping one allocation is one bounded metadata operation.
                work += SLOT_BYTES;
            } else if obj_white == dead_white {
                // Dead: free it
                #[cfg(feature = "gc-debug")]
                crate::gc_debug::on_free(obj);

                finalize_object(obj);
                stats.finalized_objects += 1;
                stats.sweep_freed_bytes += size_bytes;
                self.all_objects[self.sweep_pos] = core::ptr::null_mut();
                self.all_object_data_sizes[self.sweep_pos] = 0;
                self.total_bytes -= size_bytes;
                self.live_object_count -= 1;
                if age >= G_OLD {
                    self.old_live_bytes -= size_bytes;
                } else {
                    self.young_live_bytes -= size_bytes;
                }
                if size_bytes > (1usize << 15) {
                    self.large_live_bytes -= size_bytes;
                }
                if runtime_backing {
                    self.runtime_backing_bytes -= size_bytes;
                }
                work += SLOT_BYTES;

                self.forget_remembered_parent(obj);
                let removed_extent = self.allocation_extents.remove(&(obj as usize));
                assert_eq!(
                    removed_extent,
                    Some(AllocationExtent {
                        data_size,
                        remembered_index: None,
                    }),
                    "sweep must remove the exact allocation extent for every live object record"
                );
                let raw_ptr = unsafe { (obj as *mut u8).sub(GcHeader::SIZE) };
                self.heap
                    .free(raw_ptr)
                    .expect("sweep must release an allocation owned by the Island heap");
            }

            self.sweep_pos += 1;
        }

        // If sweep complete, truncate the vector
        if self.sweep_pos >= self.all_objects.len() {
            self.all_objects.truncate(self.sweep_write_pos);
            self.all_object_data_sizes.truncate(self.sweep_write_pos);
        }

        work
    }

    /// Finish GC cycle.
    fn finish_cycle(&mut self) {
        match self.cycle_kind {
            GcCycleKind::Minor => {
                self.minor_cycles = self.minor_cycles.saturating_add(1);
                self.minor_cycles_since_major = self.minor_cycles_since_major.saturating_add(1);
            }
            GcCycleKind::Major => {
                self.major_cycles = self.major_cycles.saturating_add(1);
                self.minor_cycles_since_major = 0;
            }
        }
        self.estimate = self.total_bytes;
        self.state = GcState::Pause;
        self.pending_root_scan = None;

        // `pause` is the next-heap multiplier: 200 means start the next cycle
        // when the heap reaches 2x the live size. Debt counts bytes allocated
        // after this cycle, so the allocation threshold is only the growth
        // portion of that target. Using the full multiplier here delayed a
        // pause=200 collector until 3x live size and produced large sawtooth
        // heaps in allocation-heavy browser games.
        let growth_percent = self.pause.saturating_sub(100).max(1);
        let threshold = (self.estimate as u64 * growth_percent as u64 / 100) as i64;
        self.debt = -threshold.max(1024);
    }

    pub fn total_bytes(&self) -> usize {
        self.reject_owner_proxy_api("total_bytes");
        self.total_bytes
    }

    pub fn object_count(&self) -> usize {
        self.reject_owner_proxy_api("object_count");
        self.live_object_count
    }

    pub fn objects(&self) -> impl Iterator<Item = GcRef> + '_ {
        self.reject_owner_proxy_api("objects");
        self.all_objects
            .iter()
            .copied()
            .filter(|obj| !obj.is_null())
    }

    pub fn debt(&self) -> i64 {
        self.reject_owner_proxy_api("debt");
        self.debt
    }

    pub fn estimate(&self) -> usize {
        self.reject_owner_proxy_api("estimate");
        self.estimate
    }

    /// Deep copy (clone) a heap object.
    /// Allocates new object with same value_meta and copies all slots.
    /// Used by PtrClone instruction and interface assignment (value semantics).
    /// # Safety
    /// src must be a valid GcRef or null.
    pub unsafe fn ptr_clone(&mut self, src: GcRef) -> GcRef {
        use crate::objects::array;

        if src.is_null() {
            return src;
        }
        let header = unsafe { Self::header(src) };
        let value_meta = header.value_meta;

        // For large arrays, slots == 0, read actual size from ArrayHeader.
        // Value-slot objects with zero slots are not ArrayHeader-backed arrays.
        let actual_slots = if header.is_value_slots_object() {
            header.slots as usize
        } else if header.slots == 0 {
            if value_meta.value_kind() != ValueKind::Array {
                panic!("slots == 0 but value_kind is not Array");
            }
            array::total_slots(src)
        } else {
            header.slots as usize
        };

        let allocation_kind = if header.is_value_slots_object() {
            GC_OWNER_ALLOC_VALUE_SLOTS
        } else if value_meta.value_kind() == ValueKind::Array {
            GC_OWNER_ALLOC_ARRAY
        } else {
            GC_OWNER_ALLOC_OBJECT
        };
        let owner_dispatched = self.owner_dispatch.is_some();
        let dst = self.alloc_inner(value_meta, allocation_kind, header.slots, actual_slots);
        if header.is_value_slots_object() && !owner_dispatched && !dst.is_null() {
            unsafe { Self::header_mut(dst) }.set_value_slots_object();
        }

        for i in 0..actual_slots {
            let val = unsafe { Self::read_slot(src, i) };
            unsafe { Self::write_slot(dst, i, val) };
        }

        self.mark_allocated_for_scan(dst);
        dst
    }
}

impl Default for Gc {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;

/// Scan a slice of values using SlotTypes for GC marking.
///
/// This is the unified scanning function used by both VM root scanning
/// and heap object scanning.
#[inline]
pub fn scan_slots_by_types(gc: &mut Gc, slots: &[u64], slot_types: &[crate::SlotType]) {
    trace_slots_by_types(slots, slot_types, |child| gc.mark_gray(child));
}

/// Visit all GC references in a slot slice using precise SlotType metadata.
#[inline]
pub fn trace_slots_by_types<F>(slots: &[u64], slot_types: &[crate::SlotType], mut visit: F)
where
    F: FnMut(GcRef),
{
    use crate::objects::interface;
    use crate::SlotType;

    assert_eq!(
        slots.len(),
        slot_types.len(),
        "scan_slots_by_types: slots length {} != slot_types length {}",
        slots.len(),
        slot_types.len()
    );

    let mut i = 0;
    while i < slot_types.len() {
        match slot_types[i] {
            SlotType::GcRef => {
                if slots[i] != 0 {
                    visit(slots[i] as GcRef);
                }
            }
            SlotType::Interface0 => {
                assert!(
                    i + 1 < slots.len(),
                    "scan_slots_by_types: Interface0 at slot {i} missing Interface1 data slot"
                );
                assert!(
                    slot_types[i + 1] == SlotType::Interface1,
                    "scan_slots_by_types: Interface0 at slot {i} must be followed by Interface1"
                );
                // Interface header slot - check if data slot contains GcRef
                if interface::data_is_gc_ref(slots[i]) && slots[i + 1] != 0 {
                    visit(slots[i + 1] as GcRef);
                }
                i += 1;
            }
            _ => {}
        }
        i += 1;
    }
}
