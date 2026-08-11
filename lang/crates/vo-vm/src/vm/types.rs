//! VM types and state definitions.

#[cfg(not(feature = "std"))]
use alloc::collections::VecDeque;
#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::sync::Arc;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::{
    Gc, GcCycleKind, GcRef, GcRootScanKind, GcStepStats, MemoryError, MemoryStats, VmMemoryConfig,
};
use vo_runtime::output::{default_sink, OutputSink};
use vo_runtime::SentinelErrorCache;

use crate::runtime_boundary::RuntimeTransition;
use crate::scheduler::FiberWakeKey;
use vo_runtime::ffi::{ExternRegistry, RuntimeMemRequests};
#[cfg(feature = "std")]
use vo_runtime::island::IslandCommand;
use vo_runtime::island::IslandCommandEnvelope;
use vo_runtime::itab::ItabCache;

use hashbrown::HashMap as HbHashMap;
#[cfg(feature = "std")]
use std::collections::{HashMap as StdHashMap, VecDeque};
#[cfg(feature = "std")]
use std::sync::atomic::{AtomicBool, Ordering};
#[cfg(feature = "std")]
use std::sync::mpsc::Receiver;
#[cfg(feature = "std")]
use std::sync::{Arc, Mutex};
#[cfg(feature = "std")]
use std::thread::JoinHandle;
#[cfg(feature = "std")]
use vo_runtime::island_transport::{
    IslandSendReservation, IslandSender, IslandTransport, TransportError,
};

/// Shared registry of island senders.
/// Island VMs use this shared map as their command-routing source.
#[cfg(feature = "std")]
pub type IslandRegistry = Arc<Mutex<StdHashMap<u32, Arc<dyn IslandSender>>>>;

#[cfg(feature = "std")]
#[derive(Debug)]
pub(crate) enum IslandRouteError {
    MissingSender {
        island_id: u32,
    },
    RegistryPoisoned {
        island_id: u32,
    },
    Transport {
        island_id: u32,
        error: TransportError,
    },
}

#[cfg(feature = "std")]
impl core::fmt::Display for IslandRouteError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::MissingSender { island_id } => {
                write!(f, "missing island sender for island {island_id}")
            }
            Self::RegistryPoisoned { island_id } => {
                write!(f, "island sender registry poisoned for island {island_id}")
            }
            Self::Transport { island_id, error } => {
                write!(
                    f,
                    "island transport failed for island {island_id}: {error:?}"
                )
            }
        }
    }
}

// =============================================================================
// EndpointRegistry
// =============================================================================

/// Entry in the endpoint registry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EndpointEntry {
    /// Channel is live — GcRef to the channel object.
    Live(GcRef),
    /// Channel was closed/collected. The response source is retained until
    /// pending obligations quiesce so in-flight targeted responses can settle.
    Tombstone { response_source: Option<u32> },
}

/// Registry mapping endpoint IDs to local channel GcRefs.
/// Used on both home islands (LOCAL channels) and remote islands (REMOTE proxies)
/// to route incoming ChanRequest/ChanResponse commands.
pub struct EndpointRegistry {
    entries: HbHashMap<u64, EndpointEntry>,
    tombstone_count: usize,
    live_roots: Vec<(u64, GcRef)>,
    live_root_indices: HbHashMap<u64, usize>,
}

#[derive(Debug, Default)]
pub(crate) struct EndpointRegistryUndo {
    first: Option<(u64, Option<EndpointEntry>)>,
    entries: HbHashMap<u64, Option<EndpointEntry>>,
}

impl Default for EndpointRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl EndpointRegistry {
    pub fn new() -> Self {
        Self {
            entries: HbHashMap::new(),
            tombstone_count: 0,
            live_roots: Vec::new(),
            live_root_indices: HbHashMap::new(),
        }
    }

    pub(crate) fn try_reserve_live(&mut self, additional: usize) -> Result<(), &'static str> {
        self.entries
            .try_reserve(additional)
            .map_err(|_| "endpoint registry allocation failed")?;
        self.live_roots
            .try_reserve(additional)
            .map_err(|_| "endpoint root allocation failed")?;
        self.live_root_indices
            .try_reserve(additional)
            .map_err(|_| "endpoint root index allocation failed")
    }

    fn upsert_live_root(&mut self, endpoint_id: u64, ch: GcRef) {
        if let Some(&index) = self.live_root_indices.get(&endpoint_id) {
            self.live_roots[index].1 = ch;
            return;
        }
        let index = self.live_roots.len();
        self.live_roots.push((endpoint_id, ch));
        self.live_root_indices.insert(endpoint_id, index);
    }

    fn remove_live_root(&mut self, endpoint_id: u64) {
        let Some(index) = self.live_root_indices.remove(&endpoint_id) else {
            return;
        };
        self.live_roots.swap_remove(index);
        if let Some((moved_endpoint, _)) = self.live_roots.get(index).copied() {
            self.live_root_indices.insert(moved_endpoint, index);
        }
    }

    fn replace_entry(&mut self, endpoint_id: u64, entry: Option<EndpointEntry>) {
        match self.entries.remove(&endpoint_id) {
            Some(EndpointEntry::Live(_)) => self.remove_live_root(endpoint_id),
            Some(EndpointEntry::Tombstone { .. }) => self.tombstone_count -= 1,
            None => {}
        }
        match entry {
            Some(EndpointEntry::Live(ch)) => {
                self.entries.insert(endpoint_id, EndpointEntry::Live(ch));
                self.upsert_live_root(endpoint_id, ch);
            }
            Some(EndpointEntry::Tombstone { response_source }) => {
                self.entries
                    .insert(endpoint_id, EndpointEntry::Tombstone { response_source });
                self.tombstone_count += 1;
            }
            None => {}
        }
    }

    /// Register or update a live channel for an endpoint.
    pub fn register_live(&mut self, endpoint_id: u64, ch: GcRef) {
        let old = self.entries.insert(endpoint_id, EndpointEntry::Live(ch));
        if matches!(old, Some(EndpointEntry::Tombstone { .. })) {
            self.tombstone_count -= 1;
        }
        self.upsert_live_root(endpoint_id, ch);
    }

    pub(crate) fn rollback_live_insertion(&mut self, endpoint_id: u64, ch: GcRef) {
        if self.entries.get(&endpoint_id) != Some(&EndpointEntry::Live(ch)) {
            return;
        }
        self.entries.remove(&endpoint_id);
        self.remove_live_root(endpoint_id);
    }

    /// Ensure endpoint is registered as live (idempotent).
    pub fn ensure_live(&mut self, endpoint_id: u64, ch: GcRef) {
        if self.entries.contains_key(&endpoint_id) {
            return;
        }
        self.entries.insert(endpoint_id, EndpointEntry::Live(ch));
        self.upsert_live_root(endpoint_id, ch);
    }

    /// Get a live channel by endpoint ID. Returns None for tombstones and missing.
    pub fn get_live(&self, endpoint_id: u64) -> Option<GcRef> {
        match self.entries.get(&endpoint_id) {
            Some(EndpointEntry::Live(ch)) => Some(*ch),
            _ => None,
        }
    }

    pub(crate) fn entry(&self, endpoint_id: u64) -> Option<EndpointEntry> {
        self.entries.get(&endpoint_id).copied()
    }

    /// Mark an endpoint as tombstoned (channel closed or collected).
    pub fn mark_tombstone(&mut self, endpoint_id: u64) {
        self.mark_tombstone_with_response_source(endpoint_id, None);
    }

    pub fn mark_tombstone_with_response_source(
        &mut self,
        endpoint_id: u64,
        response_source: Option<u32>,
    ) {
        let old_response_source = match self.entries.get(&endpoint_id) {
            Some(EndpointEntry::Tombstone { response_source }) => *response_source,
            _ => None,
        };
        let old = self.entries.insert(
            endpoint_id,
            EndpointEntry::Tombstone {
                response_source: response_source.or(old_response_source),
            },
        );
        if !matches!(old, Some(EndpointEntry::Tombstone { .. })) {
            self.tombstone_count += 1;
        }
        if matches!(old, Some(EndpointEntry::Live(_))) {
            self.remove_live_root(endpoint_id);
        }
    }

    /// Check if an endpoint is tombstoned.
    pub fn is_tombstone(&self, endpoint_id: u64) -> bool {
        matches!(
            self.entries.get(&endpoint_id),
            Some(EndpointEntry::Tombstone { .. })
        )
    }

    pub(crate) fn tombstone_response_source(&self, endpoint_id: u64) -> Option<Option<u32>> {
        match self.entries.get(&endpoint_id) {
            Some(EndpointEntry::Tombstone { response_source }) => Some(*response_source),
            _ => None,
        }
    }

    /// Clear all tombstones (called periodically or on shutdown).
    pub fn clear_tombstones(&mut self) {
        if self.tombstone_count == 0 {
            return;
        }
        self.entries
            .retain(|_, v| matches!(v, EndpointEntry::Live(_)));
        self.tombstone_count = 0;
    }

    pub fn has_tombstones(&self) -> bool {
        self.tombstone_count != 0
    }

    /// Check if there are any live (non-tombstoned) endpoints.
    /// Used to detect active cross-island communication.
    pub fn has_live(&self) -> bool {
        self.entries.len() > self.tombstone_count
    }

    /// Iterate all live GcRefs for GC root scanning.
    pub fn live_handles(&self) -> impl Iterator<Item = GcRef> + '_ {
        self.live_roots.iter().map(|(_, ch)| *ch)
    }

    pub(crate) fn live_handle_at(&self, index: usize) -> Option<GcRef> {
        self.live_roots.get(index).map(|(_, ch)| *ch)
    }
}

impl EndpointRegistryUndo {
    pub(crate) fn try_reserve(&mut self, additional: usize) -> Result<(), &'static str> {
        self.entries
            .try_reserve(additional.saturating_sub(usize::from(self.first.is_none())))
            .map_err(|_| "endpoint rollback allocation failed")
    }

    pub(crate) fn record(&mut self, registry: &EndpointRegistry, endpoint_id: u64) {
        if self
            .first
            .is_some_and(|(existing, _)| existing == endpoint_id)
        {
            return;
        }
        if self.first.is_none() {
            self.first = Some((endpoint_id, registry.entry(endpoint_id)));
            return;
        }
        self.entries
            .entry(endpoint_id)
            .or_insert_with(|| registry.entry(endpoint_id));
    }

    pub(crate) fn absorb(&mut self, mut other: Self) {
        if self.first.is_none() {
            self.first = other.first.take();
        } else if let Some((endpoint_id, entry)) = other.first.take() {
            if self
                .first
                .is_none_or(|(existing, _)| existing != endpoint_id)
            {
                self.entries.entry(endpoint_id).or_insert(entry);
            }
        }
        for (endpoint_id, entry) in other.entries {
            if self
                .first
                .is_some_and(|(existing, _)| existing == endpoint_id)
            {
                continue;
            }
            self.entries.entry(endpoint_id).or_insert(entry);
        }
    }

    pub(crate) fn restore(self, registry: &mut EndpointRegistry) {
        if let Some((endpoint_id, entry)) = self.first {
            registry.replace_entry(endpoint_id, entry);
        }
        for (endpoint_id, entry) in self.entries {
            registry.replace_entry(endpoint_id, entry);
        }
    }
}

/// Time slice: number of instructions before forced yield check.
/// VM executes at most TIME_SLICE instructions per fiber before yielding to scheduler.
pub const TIME_SLICE: u32 = vo_runtime::EXECUTION_TIMESLICE_INSTRUCTIONS;

/// VM execution result - drives scheduler state transitions.
///
/// Variants visible to the scheduling loop include TimesliceExpired, Block,
/// Panic, Exit, Interrupted, JitError, Transition, and Done.
/// Internal variants (FrameChanged, CallClosure): consumed inside run_fiber,
/// never reach the scheduling loop.
#[derive(Debug)]
pub enum ExecResult {
    /// Runtime boundary work to apply after the active fiber lease is released.
    Transition(RuntimeTransition),
    /// Call/return changed frames, refetch locals. Internal to run_fiber.
    FrameChanged,
    /// Time slice expired, yield to scheduler.
    TimesliceExpired,
    Interrupted,
    /// Block on external event.
    Block(crate::fiber::BlockReason),
    /// Panic, unwind or kill.
    Panic,
    /// Fatal JIT infrastructure error. This is not recoverable by user code.
    JitError(String),
    /// Entire VM process requested immediate termination through `os.Exit`.
    Exit(i32),
    /// Fiber finished.
    Done,
    /// Extern function requests closure execution. Internal to run_fiber.
    /// VM pushes the closure frame, sets replay depth, and re-executes the extern on return.
    CallClosure {
        closure_ref: vo_runtime::gc::GcRef,
        args: Vec<u64>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcRootEffect {
    None,
    CurrentFiberDirty,
    AllRootsDirty,
}

/// Runtime error location for debug info lookup.
#[derive(Debug, Clone, Copy)]
pub struct ErrorLocation {
    pub func_id: u32,
    pub pc: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeTrapKind {
    NilPointerDereference,
    NilMapWrite,
    UnhashableType,
    UncomparableType,
    NegativeShift,
    NilFuncCall,
    TypeAssertionFailed,
    DivisionByZero,
    IndexOutOfBounds,
    SliceBoundsOutOfRange,
    MakeSlice,
    MakeChan,
    MakePort,
    SendOnClosedChannel,
    SendOnNilChannel,
    RecvOnNilChannel,
    CloseNilChannel,
    CloseClosedChannel,
    StackOverflow,
}

/// Scheduling loop outcome - separates scheduling from deadlock handling.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SchedulingOutcome {
    /// All fibers completed normally.
    Completed,
    /// Program requested immediate process termination with this status code.
    Exited(i32),
    /// Reached iteration limit, suspended for later continuation.
    Suspended,
    /// All fibers blocked, no progress possible.
    /// Caller decides whether this is a deadlock or expected (e.g., trampoline context).
    Blocked,
    /// All runnable fibers are done; some fibers are waiting for host-side events.
    /// The async run loop should await those events then call wake_host_event + resume.
    SuspendedForHostEvents,
    /// A fiber panicked.
    Panicked,
}

#[derive(Debug)]
pub enum VmError {
    NoEntryFunction,
    InvalidFunctionId(u32),
    StackOverflow,
    StackUnderflow,
    InvalidOpcode(u8),
    Interrupted,
    RuntimeTrap {
        kind: RuntimeTrapKind,
        msg: String,
        loc: Option<ErrorLocation>,
    },
    PanicUnwound {
        msg: Option<String>,
        loc: Option<ErrorLocation>,
    },
    /// The current Island reached a terminal managed-memory failure.
    IslandMemory(MemoryError),
    Resource(VmResourceError),
    Deadlock(String),
    Jit(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmResourceError {
    Limit {
        resource: &'static str,
        required: usize,
        limit: usize,
    },
    Allocation {
        resource: &'static str,
    },
}

impl core::fmt::Display for VmResourceError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Limit {
                resource,
                required,
                limit,
            } => write!(
                f,
                "VM {resource} resource limit exceeded: required {required}, limit {limit}"
            ),
            Self::Allocation { resource } => {
                write!(f, "VM host allocation failed for {resource}")
            }
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for VmResourceError {}

#[derive(Debug)]
pub enum VmConstructionError {
    #[cfg(feature = "std")]
    Io(std::io::Error),
    #[cfg(feature = "jit")]
    Jit(vo_jit::JitError),
    Memory(MemoryError),
    /// Keeps the error type explicitly uninhabited when VM state construction
    /// is infallible in `no_std` builds.
    #[cfg(not(feature = "std"))]
    #[doc(hidden)]
    Infallible(core::convert::Infallible),
}

/// A VM-scoped host-service owner can only change before execution and while
/// no in-thread child island exists. This keeps every fiber and island in one
/// VM process on the same immutable service generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HostServicesUpdateError {
    ExecutionStarted,
    ActiveChildIslands { count: usize },
    InvalidV2Caller,
    InvalidV2(vo_runtime::host_services_v2::HostServicesV2ValidationError),
}

impl core::fmt::Display for HostServicesUpdateError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::ExecutionStarted => write!(
                f,
                "host services cannot change after VM execution has started"
            ),
            Self::ActiveChildIslands { count } => write!(
                f,
                "host services cannot change while {count} child island thread(s) are owned by the VM"
            ),
            Self::InvalidV2Caller => {
                write!(f, "HostServices V2 caller endpoint is invalid")
            }
            Self::InvalidV2(error) => {
                write!(f, "HostServices V2 table validation failed: {error:?}")
            }
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for HostServicesUpdateError {}

impl core::fmt::Display for VmConstructionError {
    fn fmt(&self, _f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            #[cfg(feature = "std")]
            Self::Io(error) => write!(_f, "VM I/O runtime initialization failed: {error}"),
            #[cfg(feature = "jit")]
            Self::Jit(error) => write!(_f, "VM JIT initialization failed: {error}"),
            Self::Memory(error) => write!(_f, "VM memory initialization failed: {error:?}"),
            #[cfg(not(feature = "std"))]
            Self::Infallible(error) => match *error {},
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for VmConstructionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(error) => Some(error),
            #[cfg(feature = "jit")]
            Self::Jit(error) => Some(error),
            Self::Memory(_) => None,
        }
    }
}

#[cfg(feature = "std")]
impl From<std::io::Error> for VmConstructionError {
    fn from(error: std::io::Error) -> Self {
        Self::Io(error)
    }
}

#[cfg(feature = "jit")]
impl From<vo_jit::JitError> for VmConstructionError {
    fn from(error: vo_jit::JitError) -> Self {
        Self::Jit(error)
    }
}

/// Lifecycle events emitted by an island worker thread.
#[cfg(feature = "std")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IslandThreadEvent {
    Ready,
    EntryRunning { launch_token: u64 },
    EntryFailed { launch_token: u64, error: String },
    Failed(String),
    GuestExited(i32),
    Exited,
}

#[cfg(feature = "std")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntryIslandEvent {
    Running {
        launch_token: u64,
        island_id: u32,
    },
    Failed {
        launch_token: u64,
        island_id: u32,
        error: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmIdentityExhausted {
    Island,
    Endpoint,
}

impl core::fmt::Display for VmIdentityExhausted {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Island => f.write_str("VM island identity space exhausted"),
            Self::Endpoint => f.write_str("VM endpoint identity space exhausted"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for VmIdentityExhausted {}

/// Active island thread info.
#[cfg(feature = "std")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IslandThreadLifecycle {
    /// Startup completed and the worker is part of the running guest.
    Running,
    /// Startup failed or timed out. The VM retains ownership until the worker
    /// acknowledges cancellation and can be joined safely.
    Stopping,
}

/// Active island thread info.
#[cfg(feature = "std")]
pub struct IslandThread {
    pub island_id: u32,
    pub join_handle: Option<JoinHandle<()>>,
    pub events: Receiver<IslandThreadEvent>,
    pub interrupt_flag: Arc<AtomicBool>,
    pub lifecycle: IslandThreadLifecycle,
}

/// VM mutable state that can be borrowed independently from scheduler.
pub struct VmState {
    pub gc: Gc,
    /// Per-VM tie breaker for selecting among simultaneously ready cases.
    /// Keeping the generator in VM state avoids a hidden `std` thread-local
    /// dependency in alloc-only hosts.
    pub(crate) select_rng: fastrand::Rng,
    pub globals: Vec<u64>,
    pub itab_cache: ItabCache,
    /// One exact entry per verified interface-call site, shared by the
    /// interpreter and JIT tiers for this VM.
    pub(crate) dynamic_call_ic: Vec<vo_runtime::DynCallIC>,
    pub extern_registry: Arc<ExternRegistry>,
    pub program_args: Vec<Vec<u8>>,
    /// Output sink for fmt.Print / println. Defaults to StdoutSink (std) or
    /// GlobalBufferSink (WASM). Replace with CaptureSink to capture output.
    pub output: Arc<dyn OutputSink>,
    #[cfg(feature = "std")]
    pub io: vo_runtime::io::IoRuntime,
    /// Generic byte output channel (FFI → Host). Written by extern functions
    /// via `ctx.set_host_output()`; read by host via `Vm::take_host_output()`.
    pub host_output: Option<Vec<u8>>,
    pub(crate) host_services_v2: Option<vo_runtime::host_services_v2::HostServicesV2Binding>,
    pub(crate) runtime_mem_requests: RuntimeMemRequests,
    /// Executor notification used by process-local island transport. The
    /// callback only signals readiness; VM work stays on the owning thread.
    #[cfg(feature = "std")]
    pub(crate) runtime_waker: Option<Arc<dyn Fn() + Send + Sync>>,
    /// Per-VM sentinel error cache (reset on each module load).
    pub sentinel_errors: SentinelErrorCache,
    /// Next island ID to assign
    pub(crate) next_island_id: Option<u32>,
    /// Active island threads (index = island_id - 1, since main island is 0)
    #[cfg(feature = "std")]
    pub island_threads: Vec<IslandThread>,
    #[cfg(feature = "std")]
    pub entry_island_events: VecDeque<EntryIslandEvent>,
    /// Shared registry used by island VMs for in-thread command routing.
    #[cfg(feature = "std")]
    pub island_registry: Option<IslandRegistry>,
    /// Current island ID (0 for main island)
    pub current_island_id: u32,
    /// Main island's command receiver.
    #[cfg(feature = "std")]
    pub main_transport: Option<Box<dyn IslandTransport>>,
    /// Per-island sender map. Key = island_id, Value = sender trait object.
    /// Single-owner per island thread — no Mutex wrapper needed.
    #[cfg(feature = "std")]
    pub island_senders: StdHashMap<u32, Arc<dyn IslandSender>>,
    #[cfg(feature = "std")]
    pub interrupt_flag: Option<Arc<AtomicBool>>,
    #[cfg(feature = "std")]
    pub external_island_transport: bool,
    /// Next endpoint ID counter for this island.
    pub(crate) next_endpoint_id: Option<u32>,
    /// Endpoint registry — maps endpoint IDs to local channel GcRefs.
    pub endpoint_registry: EndpointRegistry,
    pub command_queue: VecDeque<IslandCommandEnvelope>,
    pub(crate) outbound_commands: VecDeque<(u32, IslandCommandEnvelope)>,
    #[cfg(feature = "jit")]
    pub(crate) jit_osr_borrow_lease_depth: u32,
    pub(crate) pending_island_responses: u32,
    /// Conservative root-dirty marker for incremental GC sweep. Set when host
    /// command/I/O paths may have changed roots outside the currently running
    /// fiber.
    pub gc_roots_dirty_all: bool,
    /// Fibers whose root set may have changed since the last full or dirty root
    /// scan. Used to avoid rescanning every fiber on each sweep slice.
    pub gc_dirty_fibers: Vec<u32>,
    gc_dirty_fiber_marks: Vec<bool>,
    /// Monotonic root mutation epoch. Incremented on every dirty-root event so a
    /// bounded root snapshot can detect changes that happened while it was being
    /// scanned across scheduler boundaries.
    pub gc_dirty_epoch: u64,
    pub gc_root_scan: Option<VmRootScanSnapshot>,
    pub last_gc_step_stats: VmGcStepStats,
    pub gc_verify_after_step: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmRootScanMode {
    Full,
    DirtyFibers,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmRootScanStage {
    Globals,
    Fibers,
    IoStaging,
    SentinelErrors,
    Endpoints,
    Done,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmFiberRootScanStage {
    Defers,
    UnwindDefers,
    ReturnValues,
    UnwindPanics,
    Panic,
    ClosureReplay,
    JitSuspend,
    SelectQueues,
    SelectResult,
    QueueWait,
    JitPanic,
    Done,
}

#[derive(Debug)]
pub struct VmRootScanSnapshot {
    pub kind: GcRootScanKind,
    pub mode: VmRootScanMode,
    pub dirty_epoch: u64,
    pub stage: VmRootScanStage,
    pub global_def_cursor: usize,
    pub global_base_cursor: usize,
    pub global_slot_cursor: usize,
    pub fiber_source_cursor: usize,
    pub fiber_frame_cursor: usize,
    pub fiber_slot_cursor: usize,
    pub fiber_aux_stage: VmFiberRootScanStage,
    pub fiber_aux_outer_cursor: usize,
    pub fiber_aux_inner_cursor: usize,
    pub fiber_aux_slot_cursor: usize,
    pub io_staging_cursor: usize,
    pub sentinel_cursor: usize,
    pub endpoint_cursor: usize,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct VmGcStepStats {
    pub gc: GcStepStats,
    pub dirty_all_before: bool,
    pub dirty_fiber_count: usize,
    pub full_roots_scanned: bool,
    pub dirty_roots_scanned: bool,
    pub stable_roots_skipped: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct VmGcStepReport {
    pub requested_work_units: usize,
    pub completed_work_units: usize,
    pub stats: VmGcStepStats,
    pub memory: MemoryStats,
}

#[derive(Debug, Clone, Copy)]
pub struct VmGcCycleReport {
    pub cycle_id: u64,
    pub cycle_kind: GcCycleKind,
    pub steps: usize,
    pub completed_work_units: u64,
    pub reclaimed_live_bytes: usize,
    pub memory: MemoryStats,
}

#[cfg(feature = "std")]
fn select_rng_seed() -> u64 {
    fastrand::u64(..)
}

#[cfg(not(feature = "std"))]
const fn select_rng_seed() -> u64 {
    // Alloc-only hosts may have no entropy service. A fixed nonzero seed still
    // yields uniform pseudo-random tie breaking and deterministic replay.
    0xbb67_ae85_84ca_a73b
}

impl VmState {
    #[cfg(feature = "std")]
    pub fn try_new() -> std::io::Result<Self> {
        let io = vo_runtime::io::IoRuntime::new()?;
        Ok(Self::from_runtime_parts(io))
    }

    #[cfg(not(feature = "std"))]
    pub fn try_new() -> Result<Self, core::convert::Infallible> {
        Ok(Self::from_runtime_parts())
    }

    pub fn new() -> Self {
        Self::try_new().expect("VM I/O runtime initialization failed")
    }

    #[cfg(feature = "std")]
    pub fn try_new_with_memory_config(config: VmMemoryConfig) -> Result<Self, VmConstructionError> {
        let io = vo_runtime::io::IoRuntime::new().map_err(VmConstructionError::Io)?;
        let gc = Gc::with_memory_config(config).map_err(VmConstructionError::Memory)?;
        Ok(Self::from_runtime_parts_with_gc(io, gc))
    }

    #[cfg(not(feature = "std"))]
    pub fn try_new_with_memory_config(config: VmMemoryConfig) -> Result<Self, VmConstructionError> {
        let gc = Gc::with_memory_config(config).map_err(VmConstructionError::Memory)?;
        Ok(Self::from_runtime_parts_with_gc(gc))
    }

    /// Stop and join every island thread owned by this VM.
    ///
    /// The operation is idempotent so terminal guest shutdown and `Drop` can
    /// share one implementation without retaining stale senders or transports.
    #[cfg(feature = "std")]
    pub(crate) fn shutdown_island_threads(&mut self) {
        let island_ids = self
            .island_threads
            .iter()
            .map(|island| island.island_id)
            .collect::<Vec<_>>();

        for island in &self.island_threads {
            island.interrupt_flag.store(true, Ordering::SeqCst);
        }
        for island_id in &island_ids {
            let _ = self.try_send_to_island(*island_id, IslandCommand::Shutdown);
        }
        for island in &mut self.island_threads {
            if let Some(handle) = island.join_handle.take() {
                let _ = handle.join();
            }
        }
        self.island_threads.clear();

        if let Some(registry) = self.island_registry.take() {
            if let Ok(mut registry) = registry.lock() {
                if self.current_island_id == 0 {
                    registry.clear();
                } else {
                    registry.remove(&self.current_island_id);
                    for island_id in &island_ids {
                        registry.remove(island_id);
                    }
                }
            }
        }
        self.island_senders.clear();
        self.main_transport = None;
    }

    fn from_runtime_parts(#[cfg(feature = "std")] io: vo_runtime::io::IoRuntime) -> Self {
        Self::from_runtime_parts_with_gc(
            #[cfg(feature = "std")]
            io,
            Gc::new(),
        )
    }

    fn from_runtime_parts_with_gc(
        #[cfg(feature = "std")] io: vo_runtime::io::IoRuntime,
        gc: Gc,
    ) -> Self {
        Self {
            gc,
            select_rng: fastrand::Rng::with_seed(select_rng_seed()),
            globals: Vec::new(),
            itab_cache: ItabCache::new(),
            dynamic_call_ic: Vec::new(),
            extern_registry: Arc::new(ExternRegistry::new()),
            program_args: Vec::new(),
            output: default_sink(),
            #[cfg(feature = "std")]
            io,
            host_output: None,
            host_services_v2: None,
            runtime_mem_requests: RuntimeMemRequests::default(),
            #[cfg(feature = "std")]
            runtime_waker: None,
            sentinel_errors: SentinelErrorCache::new(),
            next_island_id: Some(1), // 0 is main island
            #[cfg(feature = "std")]
            island_threads: Vec::new(),
            #[cfg(feature = "std")]
            entry_island_events: VecDeque::new(),
            #[cfg(feature = "std")]
            island_registry: None,
            current_island_id: 0,
            #[cfg(feature = "std")]
            main_transport: None,
            #[cfg(feature = "std")]
            island_senders: StdHashMap::new(),
            #[cfg(feature = "std")]
            interrupt_flag: None,
            #[cfg(feature = "std")]
            external_island_transport: false,
            next_endpoint_id: Some(1), // 0 is reserved
            endpoint_registry: EndpointRegistry::new(),
            command_queue: VecDeque::new(),
            outbound_commands: VecDeque::new(),
            #[cfg(feature = "jit")]
            jit_osr_borrow_lease_depth: 0,
            pending_island_responses: 0,
            gc_roots_dirty_all: true,
            gc_dirty_fibers: Vec::new(),
            gc_dirty_fiber_marks: Vec::new(),
            gc_dirty_epoch: 0,
            gc_root_scan: None,
            last_gc_step_stats: VmGcStepStats::default(),
            gc_verify_after_step: false,
        }
    }

    /// Send any command to an island.
    /// Main VM uses `island_senders`; island VMs use the shared in-thread registry.
    #[cfg(feature = "std")]
    pub(crate) fn try_send_to_island(
        &self,
        island_id: u32,
        cmd: IslandCommand,
    ) -> Result<(), IslandRouteError> {
        let reservation = self.reserve_send_to_island(island_id)?;
        reservation.send(self.current_island_id, cmd);
        Ok(())
    }

    #[cfg(feature = "std")]
    pub(crate) fn reserve_send_to_island(
        &self,
        island_id: u32,
    ) -> Result<Box<dyn IslandSendReservation>, IslandRouteError> {
        if let Some(sender) = self.island_senders.get(&island_id) {
            return sender
                .reserve_send_command()
                .map_err(|error| IslandRouteError::Transport { island_id, error });
        }
        if let Some(ref registry) = self.island_registry {
            let guard = registry
                .lock()
                .map_err(|_| IslandRouteError::RegistryPoisoned { island_id })?;
            if let Some(sender) = guard.get(&island_id) {
                return sender
                    .reserve_send_command()
                    .map_err(|error| IslandRouteError::Transport { island_id, error });
            }
        }
        Err(IslandRouteError::MissingSender { island_id })
    }

    #[cfg(feature = "std")]
    pub(crate) fn can_route_to_island(&self, island_id: u32) -> Result<(), IslandRouteError> {
        if self.external_island_transport {
            return Ok(());
        }
        if let Some(sender) = self.island_senders.get(&island_id) {
            return sender
                .preflight_send_command()
                .map_err(|error| IslandRouteError::Transport { island_id, error });
        }
        if let Some(ref registry) = self.island_registry {
            let guard = registry
                .lock()
                .map_err(|_| IslandRouteError::RegistryPoisoned { island_id })?;
            if let Some(sender) = guard.get(&island_id) {
                return sender
                    .preflight_send_command()
                    .map_err(|error| IslandRouteError::Transport { island_id, error });
            }
        }
        Err(IslandRouteError::MissingSender { island_id })
    }

    /// Clear endpoint tombstones when no island responses are in flight.
    /// Safe to call at scheduling maintenance boundaries.
    pub fn clear_endpoint_tombstones_if_quiescent(&mut self) {
        if self.pending_island_responses == 0 && self.endpoint_registry.has_tombstones() {
            self.endpoint_registry.clear_tombstones();
        }
    }

    /// Conservatively record that the VM root set changed outside the current
    /// fiber's ordinary stack mutation path.
    #[inline]
    pub(crate) fn bump_gc_dirty_epoch_or_restart_scan(&mut self) {
        if let Some(next) = self.gc_dirty_epoch.checked_add(1) {
            self.gc_dirty_epoch = next;
            return;
        }

        // Equality against the snapshot epoch protects mutations that happen
        // during a bounded root scan. Reusing zero while an old zero snapshot
        // survives would defeat that guard, so overflow discards the snapshot
        // and forces the next slice to restart with every root dirty.
        self.gc_dirty_epoch = 0;
        self.gc_root_scan = None;
        self.gc_roots_dirty_all = true;
        self.clear_gc_dirty_fibers();
    }

    #[inline]
    pub fn mark_gc_all_roots_dirty(&mut self) {
        if self.gc_root_scan.is_some() || !self.gc_roots_dirty_all {
            self.gc.roots_changed();
            self.bump_gc_dirty_epoch_or_restart_scan();
        }
        self.gc_roots_dirty_all = true;
        self.clear_gc_dirty_fibers();
    }

    #[inline]
    pub(crate) fn clear_gc_dirty_fibers(&mut self) {
        for raw in self.gc_dirty_fibers.drain(..) {
            if let Some(mark) = self.gc_dirty_fiber_marks.get_mut(raw as usize) {
                *mark = false;
            }
        }
    }

    #[inline]
    pub(crate) fn record_gc_dirty_fiber_raw(&mut self, raw: u32) -> bool {
        let index = raw as usize;
        if index >= self.gc_dirty_fiber_marks.len() {
            let additional = index + 1 - self.gc_dirty_fiber_marks.len();
            if self
                .gc_dirty_fiber_marks
                .try_reserve_exact(additional)
                .is_err()
            {
                self.mark_gc_all_roots_dirty();
                return false;
            }
            self.gc_dirty_fiber_marks.resize(index + 1, false);
        }
        if self.gc_dirty_fiber_marks[index] {
            return false;
        }
        if self.gc_dirty_fibers.len() == self.gc_dirty_fibers.capacity()
            && self.gc_dirty_fibers.try_reserve(1).is_err()
        {
            self.mark_gc_all_roots_dirty();
            return false;
        }
        self.gc_dirty_fiber_marks[index] = true;
        self.gc_dirty_fibers.push(raw);
        true
    }

    #[inline]
    pub fn mark_gc_fiber_roots_dirty(&mut self, raw: u32) {
        let already_dirty = self.gc_roots_dirty_all
            || self
                .gc_dirty_fiber_marks
                .get(raw as usize)
                .copied()
                .unwrap_or(false);
        if self.gc_root_scan.is_some() || !already_dirty {
            self.gc.roots_changed();
            self.bump_gc_dirty_epoch_or_restart_scan();
        }
        if !self.gc_roots_dirty_all {
            self.record_gc_dirty_fiber_raw(raw);
        }
    }

    /// Whether the latest completed VM root scan still covers the current root set.
    ///
    /// A bounded scan may span scheduler boundaries. Roots that its cursor has not
    /// reached remain legitimately white until a later slice, so color verification
    /// must wait while either the snapshot or a newer dirty-root set is pending.
    #[inline]
    pub(crate) fn gc_root_colors_are_verifiable(&self) -> bool {
        self.gc_root_scan.is_none() && !self.gc_roots_dirty_all && self.gc_dirty_fibers.is_empty()
    }

    /// Allocate a new endpoint ID for this island.
    /// Format: high 32 bits = island_id, low 32 bits = counter.
    pub fn allocate_endpoint_id(&mut self) -> Result<u64, VmIdentityExhausted> {
        let counter = self.next_endpoint_id.ok_or(VmIdentityExhausted::Endpoint)?;
        self.next_endpoint_id = counter.checked_add(1);
        Ok(((self.current_island_id as u64) << 32) | u64::from(counter))
    }

    /// Allocate a VM-wide island ID. Every value is issued at most once;
    /// `None` permanently records exhaustion after `u32::MAX` is consumed.
    pub fn allocate_island_id(&mut self) -> Result<u32, VmIdentityExhausted> {
        let id = self.next_island_id.ok_or(VmIdentityExhausted::Island)?;
        self.next_island_id = id.checked_add(1);
        Ok(id)
    }

    /// Check if waiter is on current island.
    #[inline]
    pub fn is_local_waiter(&self, waiter: &vo_runtime::objects::queue_state::QueueWaiter) -> bool {
        waiter.island_id() == self.current_island_id
    }

    /// Wake a waiter (local or remote). No PC modification - blocker sets resume PC.
    pub(crate) fn wake_waiter(
        &mut self,
        waiter: &vo_runtime::objects::queue_state::QueueWaiter,
        select_result: Option<crate::fiber::SelectWokenResult>,
        scheduler: &mut crate::scheduler::Scheduler,
    ) -> Result<bool, String> {
        if waiter.endpoint_wait_key().is_some() {
            return Ok(false);
        }
        if waiter.island_id() == self.current_island_id {
            let wake_key = FiberWakeKey::from_packed(waiter.fiber_key());
            if scheduler
                .try_get_fiber(wake_key.fiber_id())
                .is_some_and(|fiber| {
                    fiber.generation == wake_key.generation && fiber.remote_endpoint_wait.is_some()
                })
            {
                return Ok(false);
            }
            self.mark_gc_all_roots_dirty();
            Ok(scheduler.wake_queue_waiter_with_result(waiter, select_result))
        } else {
            Err(String::from("remote waiter wake bypassed runtime boundary"))
        }
    }

    /// Wake a receiver after a local home queue has been closed.
    ///
    /// Local waiters resume through the ordinary queue wake path. Remote
    /// waiters must be split into endpoint-response commands by the runtime
    /// boundary before this local wake applier runs.
    pub(crate) fn wake_closed_receiver(
        &mut self,
        waiter: &vo_runtime::objects::queue_state::QueueWaiter,
        endpoint_id: Option<u64>,
        scheduler: &mut crate::scheduler::Scheduler,
    ) -> Result<bool, String> {
        if waiter.island_id() == self.current_island_id {
            self.mark_gc_all_roots_dirty();
            Ok(scheduler.wake_queue_waiter(waiter))
        } else {
            let _ = endpoint_id;
            Err(String::from(
                "remote closed receiver wake bypassed runtime boundary",
            ))
        }
    }

    /// Wake a sender after a local home queue has been closed.
    pub(crate) fn wake_closed_sender(
        &mut self,
        waiter: &vo_runtime::objects::queue_state::QueueWaiter,
        endpoint_id: Option<u64>,
        scheduler: &mut crate::scheduler::Scheduler,
    ) -> Result<bool, String> {
        if waiter.island_id() == self.current_island_id {
            let woke = scheduler.wake_queue_sender_closed(waiter)?;
            if woke {
                self.mark_gc_all_roots_dirty();
            }
            Ok(woke)
        } else {
            let _ = endpoint_id;
            Err(String::from(
                "remote closed sender wake bypassed runtime boundary",
            ))
        }
    }
}

impl Default for VmState {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "std")]
impl Drop for VmState {
    fn drop(&mut self) {
        self.shutdown_island_threads();
    }
}

#[cfg(test)]
mod tests {
    use super::{EndpointRegistry, VmIdentityExhausted, VmState};
    use crate::scheduler::Scheduler;
    use crate::test_support::endpoint_waiter;
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::queue_state::{QueueWaiter, SelectWaitKind};

    #[test]
    fn endpoint_identity_uses_exact_low_32_bits_and_exhausts_without_aliasing() {
        let mut state = VmState::new();
        state.current_island_id = 0x89ab_cdef;
        state.next_endpoint_id = Some(u32::MAX);

        assert_eq!(state.allocate_endpoint_id(), Ok(0x89ab_cdef_ffff_ffff));
        assert_eq!(
            state.allocate_endpoint_id(),
            Err(VmIdentityExhausted::Endpoint)
        );
    }

    #[test]
    fn island_identity_allocates_max_once_then_stays_exhausted() {
        let mut state = VmState::new();
        state.next_island_id = Some(u32::MAX);

        assert_eq!(state.allocate_island_id(), Ok(u32::MAX));
        assert_eq!(state.allocate_island_id(), Err(VmIdentityExhausted::Island));
    }

    #[test]
    fn endpoint_registry_tombstone_count_tracks_correctly() {
        let mut reg = EndpointRegistry::new();
        let ch = 1usize as GcRef;

        reg.register_live(7, ch);
        reg.mark_tombstone(7);
        // Double tombstone on same id should not double-count.
        reg.mark_tombstone(7);

        assert!(reg.has_tombstones());
        assert!(reg.is_tombstone(7));

        reg.clear_tombstones();

        assert!(!reg.has_tombstones());
        assert!(!reg.is_tombstone(7));
        assert_eq!(reg.get_live(7), None);
    }

    #[test]
    fn register_live_over_tombstone_decrements_count() {
        let mut reg = EndpointRegistry::new();
        let ch = 1usize as GcRef;

        reg.mark_tombstone(9);
        assert!(reg.has_tombstones());

        reg.register_live(9, ch);

        assert!(!reg.has_tombstones());
        assert_eq!(reg.get_live(9), Some(ch));
    }

    #[test]
    fn clear_tombstones_noop_when_empty() {
        let mut reg = EndpointRegistry::new();
        let ch = 1usize as GcRef;
        reg.register_live(1, ch);
        // No tombstones — clear should be a no-op.
        reg.clear_tombstones();
        assert_eq!(reg.get_live(1), Some(ch));
        assert!(!reg.has_tombstones());
    }

    #[test]
    fn closed_remote_sender_rejects_direct_endpoint_response_bypass_062() {
        let mut state = super::VmState::new();
        state.external_island_transport = true;
        let mut scheduler = Scheduler::new();
        let waiter = endpoint_waiter(7, 0x0000_0002_0000_0003, 11);

        let err = state
            .wake_closed_sender(&waiter, Some(42), &mut scheduler)
            .expect_err("remote closed sender must be routed by runtime boundary");

        assert!(
            err.contains("runtime boundary"),
            "unexpected error message: {err}"
        );
        assert!(state.outbound_commands.is_empty());
    }

    #[test]
    fn remote_waiter_rejects_direct_wakefiber_bypass_062() {
        let mut state = super::VmState::new();
        state.external_island_transport = true;
        let mut scheduler = Scheduler::new();
        let waiter =
            QueueWaiter::try_queue(7, 0x0000_0006_0000_0007, 31, SelectWaitKind::Recv).unwrap();

        let err = state
            .wake_waiter(&waiter, None, &mut scheduler)
            .expect_err("remote waiter wake must be routed by runtime boundary");

        assert!(
            err.contains("runtime boundary"),
            "unexpected error message: {err}"
        );
        assert!(state.outbound_commands.is_empty());
    }

    #[test]
    fn closed_remote_receiver_rejects_direct_endpoint_response_bypass_062() {
        let mut state = super::VmState::new();
        state.external_island_transport = true;
        let mut scheduler = Scheduler::new();
        let waiter = endpoint_waiter(7, 0x0000_0004_0000_0005, 12);

        let err = state
            .wake_closed_receiver(&waiter, Some(43), &mut scheduler)
            .expect_err("remote closed receiver must be routed by runtime boundary");

        assert!(
            err.contains("runtime boundary"),
            "unexpected error message: {err}"
        );
        assert!(state.outbound_commands.is_empty());
    }
}
