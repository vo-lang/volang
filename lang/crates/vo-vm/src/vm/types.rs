//! VM types and state definitions.

#[cfg(not(feature = "std"))]
use alloc::collections::VecDeque;
#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::sync::Arc;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::{Gc, GcRef, GcRootScanKind, GcStepStats};
use vo_runtime::output::{default_sink, OutputSink};
use vo_runtime::SentinelErrorCache;

use crate::runtime_boundary::RuntimeTransition;
use crate::scheduler::FiberWakeKey;
use vo_runtime::bytecode::ResolvedExternTable;
use vo_runtime::ffi::ExternRegistry;
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

#[derive(Debug, Clone)]
pub(crate) struct EndpointRegistrySnapshot {
    entries: HbHashMap<u64, EndpointEntry>,
    tombstone_count: usize,
    live_roots: Vec<(u64, GcRef)>,
    live_root_indices: HbHashMap<u64, usize>,
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

    pub(crate) fn snapshot(&self) -> EndpointRegistrySnapshot {
        EndpointRegistrySnapshot {
            entries: self.entries.clone(),
            tombstone_count: self.tombstone_count,
            live_roots: self.live_roots.clone(),
            live_root_indices: self.live_root_indices.clone(),
        }
    }

    pub(crate) fn restore(&mut self, snapshot: EndpointRegistrySnapshot) {
        self.entries = snapshot.entries;
        self.tombstone_count = snapshot.tombstone_count;
        self.live_roots = snapshot.live_roots;
        self.live_root_indices = snapshot.live_root_indices;
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

    /// Register or update a live channel for an endpoint.
    pub fn register_live(&mut self, endpoint_id: u64, ch: GcRef) {
        let old = self.entries.insert(endpoint_id, EndpointEntry::Live(ch));
        if matches!(old, Some(EndpointEntry::Tombstone { .. })) {
            self.tombstone_count -= 1;
        }
        self.upsert_live_root(endpoint_id, ch);
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
    Deadlock(String),
    Jit(String),
}

#[derive(Debug)]
pub enum VmConstructionError {
    #[cfg(feature = "std")]
    Io(std::io::Error),
    #[cfg(feature = "jit")]
    Jit(vo_jit::JitError),
    /// Keeps the error type explicitly uninhabited when VM state construction
    /// is infallible in `no_std` builds.
    #[cfg(not(feature = "std"))]
    #[doc(hidden)]
    Infallible(core::convert::Infallible),
}

/// A VM-scoped host-service owner can only change before execution and while
/// no in-thread child island exists. This keeps every fiber and island in one
/// VM process on the same immutable service generation.
#[cfg(feature = "std")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HostServicesUpdateError {
    ExecutionStarted,
    ActiveChildIslands { count: usize },
}

#[cfg(feature = "std")]
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
    Failed(String),
    GuestExited(i32),
    Exited,
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
    pub extern_registry: ExternRegistry,
    pub resolved_externs: ResolvedExternTable,
    pub program_args: Vec<Vec<u8>>,
    /// Output sink for fmt.Print / println. Defaults to StdoutSink (std) or
    /// GlobalBufferSink (WASM). Replace with CaptureSink to capture output.
    pub output: Arc<dyn OutputSink>,
    #[cfg(feature = "std")]
    pub io: vo_runtime::io::IoRuntime,
    /// Generic byte output channel (FFI → Host). Written by extern functions
    /// via `ctx.set_host_output()`; read by host via `Vm::take_host_output()`.
    pub host_output: Option<Vec<u8>>,
    /// VM-scoped host services used by native extensions. Child islands clone
    /// this owner before their worker thread starts.
    #[cfg(feature = "std")]
    pub(crate) host_services: Option<vo_runtime::host_services::SharedHostServices>,
    /// Per-VM sentinel error cache (reset on each module load).
    pub sentinel_errors: SentinelErrorCache,
    /// Next island ID to assign
    pub(crate) next_island_id: Option<u32>,
    /// Active island threads (index = island_id - 1, since main island is 0)
    #[cfg(feature = "std")]
    pub island_threads: Vec<IslandThread>,
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
    pub(crate) pending_runtime_transitions: Vec<RuntimeTransition>,
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
    pub dirty_fibers: Vec<u32>,
    /// Small pending-root buffer for non-stack fiber state and VM-owned caches.
    /// Global and frame slots are enumerated directly through the cursors below.
    pub roots: Vec<GcRef>,
    pub cursor: usize,
    pub stage: VmRootScanStage,
    pub global_def_cursor: usize,
    pub global_base_cursor: usize,
    pub global_slot_cursor: usize,
    pub fiber_source_cursor: usize,
    pub fiber_frame_cursor: usize,
    pub fiber_slot_cursor: usize,
    pub fiber_aux_stage: VmFiberRootScanStage,
    pub fiber_aux_outer_cursor: usize,
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
        Self {
            gc: Gc::new(),
            select_rng: fastrand::Rng::with_seed(select_rng_seed()),
            globals: Vec::new(),
            itab_cache: ItabCache::new(),
            extern_registry: ExternRegistry::new(),
            resolved_externs: ResolvedExternTable::empty(),
            program_args: Vec::new(),
            output: default_sink(),
            #[cfg(feature = "std")]
            io,
            host_output: None,
            #[cfg(feature = "std")]
            host_services: None,
            sentinel_errors: SentinelErrorCache::new(),
            next_island_id: Some(1), // 0 is main island
            #[cfg(feature = "std")]
            island_threads: Vec::new(),
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
            pending_runtime_transitions: Vec::new(),
            #[cfg(feature = "jit")]
            jit_osr_borrow_lease_depth: 0,
            pending_island_responses: 0,
            gc_roots_dirty_all: true,
            gc_dirty_fibers: Vec::new(),
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
        self.gc_dirty_fibers.clear();
    }

    #[inline]
    pub fn mark_gc_all_roots_dirty(&mut self) {
        if self.gc_root_scan.is_some() || !self.gc_roots_dirty_all {
            self.bump_gc_dirty_epoch_or_restart_scan();
        }
        self.gc_roots_dirty_all = true;
        self.gc_dirty_fibers.clear();
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
        waiter.island_id == self.current_island_id
    }

    /// Wake a waiter (local or remote). No PC modification - blocker sets resume PC.
    pub(crate) fn wake_waiter(
        &mut self,
        waiter: &vo_runtime::objects::queue_state::QueueWaiter,
        select_result: Option<crate::fiber::SelectWokenResult>,
        scheduler: &mut crate::scheduler::Scheduler,
    ) -> Result<bool, String> {
        if waiter.endpoint_wait_id() != 0 {
            return Ok(false);
        }
        if waiter.island_id == self.current_island_id {
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
        if waiter.island_id == self.current_island_id {
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
        if waiter.island_id == self.current_island_id {
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
    fn vm_endpoint_request_publication_013_request_publishers_are_boundary_owned() {
        let src = crate::source_contract::production_source_without_test_modules(include_str!(
            "types.rs"
        ));
        let vm_src =
            crate::source_contract::production_source_without_test_modules(include_str!("mod.rs"));
        let scheduler_src = crate::source_contract::production_source_without_test_modules(
            include_str!("../scheduler.rs"),
        );
        let web_island_src = include_str!("../../../vo-web/src/island.rs");
        let web_vm_src = include_str!("../../../vo-web/src/vm.rs");
        let web_async_src = include_str!("../../../vo-web/src/async_runner.rs");
        let app_session_src = include_str!("../../../vo-app-runtime/src/session.rs");
        let app_native_src = include_str!("../../../vo-app-runtime/src/native.rs");
        let engine_run_src = include_str!("../../../vo-engine/src/run.rs");

        for removed_helper in [
            "fn send_endpoint_request",
            "fn send_endpoint_send_request",
            "fn send_endpoint_recv_request",
            "fn send_endpoint_close_request",
        ] {
            assert!(
                !src.contains(removed_helper),
                "{removed_helper} bypasses RuntimeTransition endpoint request ownership"
            );
        }
        for raw_publication_surface in [
            "pub fn try_send_to_island",
            "pub fn send_to_island",
            "pub outbound_commands",
            "pub pending_island_responses",
            "fn send_wake_to_island",
            "pub fn send_spawn_fiber_to_island",
            "pub fn send_endpoint_response",
            "pub fn send_endpoint_recv_data_response",
            "pub fn wake_waiter",
            "pub fn wake_closed_receiver",
            "pub fn wake_closed_sender",
            "pub fn set_island_id",
        ] {
            assert!(
                !src.contains(raw_publication_surface),
                "{raw_publication_surface} exposes raw island publication outside the runtime boundary"
            );
        }
        for raw_scheduler_surface in [
            "pub fn host_event_key",
            "pub fn wake_host_event(",
            "pub fn wake_host_event_legacy_timer_token",
            "pub fn wake_host_event_legacy_replay_token",
            "pub fn wake_host_event_with_data",
            "pub fn take_pending_host_events",
            "pub fn has_host_event_waiters",
            "pub fn poll_io_ready_tokens",
            "pub fn io_wait_key",
            "pub fn wake_io(",
            "pub fn wake_io_token",
            "pub fn has_io_waiters",
            "pub fn wake_queue_waiter(",
            "pub fn wake_queue_waiter_with_result",
            "pub fn wake_queue_sender_closed",
        ] {
            assert!(
                !scheduler_src.contains(raw_scheduler_surface),
                "{raw_scheduler_surface} exposes raw scheduler wake application outside Vm runtime-command ownership"
            );
        }
        for leaked_vm_field in [
            "pub module: Option<Module>",
            "pub scheduler: Scheduler",
            "pub state: VmState",
        ] {
            assert!(
                !vm_src.contains(leaked_vm_field),
                "{leaked_vm_field} exposes raw VM internals outside semantic Vm APIs"
            );
        }
        assert!(
            vm_src.contains("pub fn has_outbound_commands(&self) -> bool"),
            "Vm must expose a semantic outbound-command predicate instead of leaking VmState queues"
        );
        assert!(
            vm_src.contains("pub fn push_targeted_island_command("),
            "Vm must own targeted island identity adoption instead of leaking a raw island-id setter"
        );
        assert!(
            vm_src.contains("pub fn take_pending_host_events(&mut self)"),
            "Vm must expose semantic host-event polling instead of leaking Scheduler"
        );
        assert!(
            vm_src.contains("pub fn host_event_key("),
            "Vm must expose semantic host-event key lookup instead of leaking Scheduler"
        );
        assert!(
            vm_src.contains("pub fn extern_registry_mut(")
                && vm_src.contains("Result<&mut vo_runtime::ExternRegistry, VmError>"),
            "Vm must expose fallible pre-load extern registration without leaking VmState"
        );
        assert!(
            vm_src.contains("pub fn set_output_sink("),
            "Vm must expose output-sink configuration without leaking VmState"
        );
        assert!(
            !web_island_src.contains(".state.outbound_commands"),
            "vo-web must use Vm::has_outbound_commands instead of reaching into VmState raw queues"
        );
        for external_src in [
            web_island_src,
            web_vm_src,
            web_async_src,
            app_session_src,
            app_native_src,
            engine_run_src,
        ] {
            assert!(
                !external_src.contains(".scheduler"),
                "external crates must use Vm host-event APIs instead of reaching into Scheduler"
            );
            assert!(
                !external_src.contains(".state"),
                "external crates must use semantic Vm APIs instead of reaching into VmState"
            );
            assert!(
                !external_src.contains(".set_island_id"),
                "external crates must use Vm::push_targeted_island_command instead of raw island-id mutation"
            );
        }
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
        let waiter = QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11);

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
        let waiter = QueueWaiter::simple_queue(7, 0x0000_0006_0000_0007, 31, SelectWaitKind::Recv);

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
        let waiter = QueueWaiter::endpoint(7, 0x0000_0004_0000_0005, 12);

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
