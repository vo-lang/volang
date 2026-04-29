//! VM types and state definitions.

#[cfg(not(feature = "std"))]
use alloc::collections::VecDeque;
#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::sync::Arc;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::Gc;
use vo_runtime::gc::GcRef;
use vo_runtime::output::{default_sink, OutputSink};
use vo_runtime::SentinelErrorCache;

use vo_runtime::ffi::ExternRegistry;
use vo_runtime::island::{EndpointRequestKind, EndpointResponseKind, IslandCommand};
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
use vo_runtime::island_transport::{IslandSender, IslandTransport, TransportError};

/// Shared registry of island senders.
/// Island VMs use this shared map as their command-routing source.
#[cfg(feature = "std")]
pub type IslandRegistry = Arc<Mutex<StdHashMap<u32, Arc<dyn IslandSender>>>>;

#[cfg(feature = "std")]
#[derive(Debug)]
pub enum IslandRouteError {
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

// =============================================================================
// EndpointRegistry
// =============================================================================

/// Entry in the endpoint registry.
#[derive(Debug)]
pub enum EndpointEntry {
    /// Channel is live — GcRef to the channel object.
    Live(GcRef),
    /// Channel was closed/collected — stale messages get idempotent closed responses.
    Tombstone,
}

/// Registry mapping endpoint IDs to local channel GcRefs.
/// Used on both home islands (LOCAL channels) and remote islands (REMOTE proxies)
/// to route incoming ChanRequest/ChanResponse commands.
pub struct EndpointRegistry {
    pub entries: HbHashMap<u64, EndpointEntry>,
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

    /// Register or update a live channel for an endpoint.
    pub fn register_live(&mut self, endpoint_id: u64, ch: GcRef) {
        if matches!(
            self.entries.insert(endpoint_id, EndpointEntry::Live(ch)),
            Some(EndpointEntry::Tombstone)
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
        if !matches!(
            self.entries.insert(endpoint_id, EndpointEntry::Tombstone),
            Some(EndpointEntry::Tombstone)
        ) {
            self.tombstone_count += 1;
        }
    }

    /// Check if an endpoint is tombstoned.
    pub fn is_tombstone(&self, endpoint_id: u64) -> bool {
        matches!(
            self.entries.get(&endpoint_id),
            Some(EndpointEntry::Tombstone)
        )
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
            EndpointEntry::Tombstone => None,
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExecResult {
    /// Call/return changed frames, refetch locals. Internal to run_fiber.
    FrameChanged,
    /// Time slice expired, yield to scheduler.
    TimesliceExpired,
    Interrupted,
    /// Block on external event.
    Block(crate::fiber::BlockReason),
    /// Panic, unwind or kill.
    Panic,
    /// Fiber finished.
    Done,
    /// Extern function requests closure execution. Internal to run_fiber.
    /// VM pushes the closure frame, sets replay depth, and re-executes the extern on return.
    CallClosure {
        closure_ref: vo_runtime::gc::GcRef,
        args: Vec<u64>,
    },
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
    pub command_queue: VecDeque<IslandCommand>,
    pub outbound_commands: VecDeque<(u32, IslandCommand)>,
    pub pending_island_responses: u32,
}

impl VmState {
    pub fn new() -> Self {
        Self {
            gc: Gc::new(),
            globals: Vec::new(),
            itab_cache: ItabCache::new(),
            extern_registry: ExternRegistry::new(),
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
            pending_island_responses: 0,
        }
    }

    /// Send wake command to an island.
    /// Delegates to send_to_island so both main VM and island threads work.
    pub fn send_wake_to_island(&mut self, island_id: u32, fiber_id: u32) {
        self.send_to_island(island_id, IslandCommand::WakeFiber { fiber_id })
    }

    /// Send any command to an island.
    /// Main VM uses `island_senders`; island VMs use the shared in-thread registry.
    #[cfg(feature = "std")]
    pub fn try_send_to_island(
        &self,
        island_id: u32,
        cmd: IslandCommand,
    ) -> Result<(), IslandRouteError> {
        if let Some(sender) = self.island_senders.get(&island_id) {
            return sender
                .send_command(cmd)
                .map_err(|error| IslandRouteError::Transport { island_id, error });
        }
        if let Some(ref registry) = self.island_registry {
            let guard = registry
                .lock()
                .map_err(|_| IslandRouteError::RegistryPoisoned { island_id })?;
            if let Some(sender) = guard.get(&island_id) {
                return sender
                    .send_command(cmd)
                    .map_err(|error| IslandRouteError::Transport { island_id, error });
            }
        }
        Err(IslandRouteError::MissingSender { island_id })
    }

    /// Send any command to an island.
    /// Operational paths are fail-fast; teardown paths should use try_send_to_island.
    pub fn send_to_island(&mut self, island_id: u32, cmd: IslandCommand) {
        #[cfg(feature = "std")]
        {
            if self.external_island_transport {
                self.outbound_commands.push_back((island_id, cmd));
                return;
            }
            self.try_send_to_island(island_id, cmd)
                .unwrap_or_else(|error| {
                    panic!(
                        "send_to_island failed for island {}: {:?}",
                        island_id, error
                    )
                });
        }
        #[cfg(not(feature = "std"))]
        {
            self.outbound_commands.push_back((island_id, cmd));
        }
    }

    pub fn send_spawn_fiber_to_island(
        &mut self,
        island_id: u32,
        closure_data: vo_runtime::pack::PackedValue,
    ) {
        self.send_to_island(island_id, IslandCommand::SpawnFiber { closure_data });
    }

    pub fn send_endpoint_request(
        &mut self,
        island_id: u32,
        endpoint_id: u64,
        kind: EndpointRequestKind,
        from_island: u32,
        fiber_id: u64,
    ) {
        self.send_to_island(
            island_id,
            IslandCommand::EndpointRequest {
                endpoint_id,
                kind,
                from_island,
                fiber_id,
            },
        );
    }

    pub fn send_endpoint_send_request(
        &mut self,
        island_id: u32,
        endpoint_id: u64,
        data: Vec<u8>,
        fiber_id: u64,
    ) {
        self.pending_island_responses += 1;
        self.send_endpoint_request(
            island_id,
            endpoint_id,
            EndpointRequestKind::Send { data },
            self.current_island_id,
            fiber_id,
        );
    }

    pub fn send_endpoint_recv_request(&mut self, island_id: u32, endpoint_id: u64, fiber_id: u64) {
        self.pending_island_responses += 1;
        self.send_endpoint_request(
            island_id,
            endpoint_id,
            EndpointRequestKind::Recv,
            self.current_island_id,
            fiber_id,
        );
    }

    pub fn send_endpoint_close_request(&mut self, island_id: u32, endpoint_id: u64) {
        self.send_endpoint_request(
            island_id,
            endpoint_id,
            EndpointRequestKind::Close,
            self.current_island_id,
            0,
        );
    }

    pub fn send_endpoint_response(
        &mut self,
        island_id: u32,
        endpoint_id: u64,
        kind: EndpointResponseKind,
        fiber_id: u64,
    ) {
        self.send_to_island(
            island_id,
            IslandCommand::EndpointResponse {
                endpoint_id,
                kind,
                fiber_id,
            },
        );
    }

    pub fn send_endpoint_recv_data_response(
        &mut self,
        island_id: u32,
        endpoint_id: u64,
        data: Vec<u8>,
        fiber_id: u64,
    ) {
        self.send_endpoint_response(
            island_id,
            endpoint_id,
            EndpointResponseKind::RecvData {
                data,
                closed: false,
            },
            fiber_id,
        );
    }

    /// Clear endpoint tombstones when no island responses are in flight.
    /// Safe to call at scheduling maintenance boundaries.
    pub fn clear_endpoint_tombstones_if_quiescent(&mut self) {
        if self.pending_island_responses == 0 && self.endpoint_registry.has_tombstones() {
            self.endpoint_registry.clear_tombstones();
        }
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
    pub fn wake_waiter(
        &mut self,
        waiter: &vo_runtime::objects::queue_state::QueueWaiter,
        scheduler: &mut crate::scheduler::Scheduler,
    ) {
        if waiter.island_id == self.current_island_id {
            scheduler.wake_queue_waiter(waiter);
        } else {
            self.send_wake_to_island(waiter.island_id, waiter.fiber_id as u32);
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
    use vo_runtime::gc::GcRef;

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
}
