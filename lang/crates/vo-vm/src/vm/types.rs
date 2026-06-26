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
use std::sync::atomic::AtomicBool;
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
    pub entries: HbHashMap<u64, EndpointEntry>,
    tombstone_count: usize,
}

#[derive(Debug, Clone)]
pub(crate) struct EndpointRegistrySnapshot {
    entries: HbHashMap<u64, EndpointEntry>,
    tombstone_count: usize,
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
        }
    }

    pub(crate) fn snapshot(&self) -> EndpointRegistrySnapshot {
        EndpointRegistrySnapshot {
            entries: self.entries.clone(),
            tombstone_count: self.tombstone_count,
        }
    }

    pub(crate) fn restore(&mut self, snapshot: EndpointRegistrySnapshot) {
        self.entries = snapshot.entries;
        self.tombstone_count = snapshot.tombstone_count;
    }

    /// Register or update a live channel for an endpoint.
    pub fn register_live(&mut self, endpoint_id: u64, ch: GcRef) {
        if matches!(
            self.entries.insert(endpoint_id, EndpointEntry::Live(ch)),
            Some(EndpointEntry::Tombstone { .. })
        ) {
            self.tombstone_count -= 1;
        }
    }

    /// Ensure endpoint is registered as live (idempotent).
    pub fn ensure_live(&mut self, endpoint_id: u64, ch: GcRef) {
        self.entries
            .entry(endpoint_id)
            .or_insert(EndpointEntry::Live(ch));
    }

    /// Get a live channel by endpoint ID. Returns None for tombstones and missing.
    pub fn get_live(&self, endpoint_id: u64) -> Option<GcRef> {
        match self.entries.get(&endpoint_id) {
            Some(EndpointEntry::Live(ch)) => Some(*ch),
            _ => None,
        }
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
        self.entries.values().filter_map(|e| match e {
            EndpointEntry::Live(ch) => Some(*ch),
            EndpointEntry::Tombstone { .. } => None,
        })
    }
}

/// Time slice: number of instructions before forced yield check.
/// VM executes at most TIME_SLICE instructions per fiber before yielding to scheduler.
pub const TIME_SLICE: u32 = 1000;

/// VM execution result - drives scheduler state transitions.
///
/// Variants visible to the scheduling loop: TimesliceExpired, Block, Panic, Done.
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

/// Active island thread info.
#[cfg(feature = "std")]
pub struct IslandThread {
    pub island_id: u32,
    pub join_handle: Option<JoinHandle<()>>,
}

/// VM mutable state that can be borrowed independently from scheduler.
pub struct VmState {
    pub gc: Gc,
    pub globals: Vec<u64>,
    pub itab_cache: ItabCache,
    pub extern_registry: ExternRegistry,
    pub resolved_externs: ResolvedExternTable,
    pub program_args: Vec<String>,
    /// Output sink for fmt.Print / println. Defaults to StdoutSink (std) or
    /// GlobalBufferSink (WASM). Replace with CaptureSink to capture output.
    pub output: Arc<dyn OutputSink>,
    #[cfg(feature = "std")]
    pub io: vo_runtime::io::IoRuntime,
    /// Generic byte output channel (FFI → Host). Written by extern functions
    /// via `ctx.set_host_output()`; read by host via `Vm::take_host_output()`.
    pub host_output: Option<Vec<u8>>,
    /// Per-VM sentinel error cache (reset on each module load).
    pub sentinel_errors: SentinelErrorCache,
    /// Next island ID to assign
    pub next_island_id: u32,
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
    pub next_endpoint_id: u64,
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

#[derive(Debug)]
pub struct VmRootScanSnapshot {
    pub kind: GcRootScanKind,
    pub mode: VmRootScanMode,
    pub dirty_epoch: u64,
    pub dirty_fibers: Vec<u32>,
    pub roots: Vec<GcRef>,
    pub cursor: usize,
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

impl VmState {
    pub fn new() -> Self {
        Self {
            gc: Gc::new(),
            globals: Vec::new(),
            itab_cache: ItabCache::new(),
            extern_registry: ExternRegistry::new(),
            resolved_externs: ResolvedExternTable::empty(),
            program_args: Vec::new(),
            output: default_sink(),
            #[cfg(feature = "std")]
            io: vo_runtime::io::IoRuntime::new()
                .unwrap_or_else(|e| panic!("IoRuntime::new failed: {}", e)),
            host_output: None,
            sentinel_errors: SentinelErrorCache::new(),
            next_island_id: 1, // 0 is main island
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
            next_endpoint_id: 1, // 0 is reserved
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
    pub fn mark_gc_all_roots_dirty(&mut self) {
        if self.gc_root_scan.is_some() || !self.gc_roots_dirty_all {
            self.gc_dirty_epoch = self.gc_dirty_epoch.wrapping_add(1);
        }
        self.gc_roots_dirty_all = true;
        self.gc_dirty_fibers.clear();
    }

    /// Allocate a new endpoint ID for this island.
    /// Format: high 32 bits = island_id, low 32 bits = counter.
    pub fn allocate_endpoint_id(&mut self) -> u64 {
        let id = ((self.current_island_id as u64) << 32) | self.next_endpoint_id;
        self.next_endpoint_id += 1;
        id
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
        for island in &self.island_threads {
            let _ = self.try_send_to_island(island.island_id, IslandCommand::Shutdown);
        }

        for island in &mut self.island_threads {
            if let Some(handle) = island.join_handle.take() {
                let _ = handle.join();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::EndpointRegistry;
    use crate::scheduler::Scheduler;
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::queue_state::{QueueWaiter, SelectWaitKind};

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
            vm_src.contains("pub fn extern_registry_mut(&mut self)"),
            "Vm must expose extern registration without leaking VmState"
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
