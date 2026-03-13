//! VM types and state definitions.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::sync::Arc;

use vo_runtime::gc::Gc;
#[cfg(feature = "std")]
use vo_runtime::gc::GcRef;
use vo_runtime::output::{OutputSink, default_sink};
use vo_runtime::SentinelErrorCache;

use vo_runtime::ffi::ExternRegistry;
use vo_runtime::itab::ItabCache;

#[cfg(feature = "std")]
use std::thread::JoinHandle;
#[cfg(feature = "std")]
use std::sync::{Arc, Mutex};
#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(feature = "std")]
use vo_runtime::island::{IslandCommand, ChanRequestKind, ChanResponseKind};
#[cfg(feature = "std")]
use vo_runtime::island_transport::{IslandSender, IslandTransport, TransportError};

/// Shared registry of island senders.
/// Island VMs use this shared map as their command-routing source.
#[cfg(feature = "std")]
pub type IslandRegistry = Arc<Mutex<HashMap<u32, Arc<dyn IslandSender>>>>;

#[cfg(feature = "std")]
#[derive(Debug)]
pub enum IslandRouteError {
    MissingSender { island_id: u32 },
    RegistryPoisoned { island_id: u32 },
    Transport { island_id: u32, error: TransportError },
}

// =============================================================================
// EndpointRegistry
// =============================================================================

/// Entry in the endpoint registry.
#[cfg(feature = "std")]
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
#[cfg(feature = "std")]
pub struct EndpointRegistry {
    pub entries: HashMap<u64, EndpointEntry>,
}

#[cfg(feature = "std")]
impl EndpointRegistry {
    pub fn new() -> Self {
        Self { entries: HashMap::new() }
    }

    /// Register or update a live channel for an endpoint.
    pub fn register_live(&mut self, endpoint_id: u64, ch: GcRef) {
        self.entries.insert(endpoint_id, EndpointEntry::Live(ch));
    }

    /// Ensure endpoint is registered as live (idempotent).
    pub fn ensure_live(&mut self, endpoint_id: u64, ch: GcRef) {
        self.entries.entry(endpoint_id).or_insert(EndpointEntry::Live(ch));
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
        self.entries.insert(endpoint_id, EndpointEntry::Tombstone);
    }

    /// Check if an endpoint is tombstoned.
    pub fn is_tombstone(&self, endpoint_id: u64) -> bool {
        matches!(self.entries.get(&endpoint_id), Some(EndpointEntry::Tombstone))
    }

    /// Clear all tombstones (called periodically or on shutdown).
    pub fn clear_tombstones(&mut self) {
        self.entries.retain(|_, v| matches!(v, EndpointEntry::Live(_)));
    }

    /// Iterate all live GcRefs for GC root scanning.
    pub fn live_channels(&self) -> impl Iterator<Item = GcRef> + '_ {
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
     SendOnClosedChannel,
     SendOnNilChannel,
     RecvOnNilChannel,
     CloseNilChannel,
     CloseClosedChannel,
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
    RuntimeTrap {
        kind: RuntimeTrapKind,
        msg: String,
        loc: Option<ErrorLocation>,
    },
    PanicUnwound { msg: Option<String>, loc: Option<ErrorLocation> },
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
    pub island_senders: HashMap<u32, Arc<dyn IslandSender>>,
    /// Next endpoint ID counter for this island.
    pub next_endpoint_id: u64,
    /// Endpoint registry — maps endpoint IDs to local channel GcRefs.
    #[cfg(feature = "std")]
    pub endpoint_registry: EndpointRegistry,
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
            island_senders: HashMap::new(),
            next_endpoint_id: 1, // 0 is reserved
            #[cfg(feature = "std")]
            endpoint_registry: EndpointRegistry::new(),
        }
    }
    
    /// Send wake command to an island.
    /// Delegates to send_to_island so both main VM and island threads work.
    #[cfg(feature = "std")]
    pub fn send_wake_to_island(&self, island_id: u32, fiber_id: u32) {
        self.send_to_island(island_id, IslandCommand::WakeFiber { fiber_id })
    }

    /// Send any command to an island.
    /// Main VM uses `island_senders`; island VMs use the shared in-thread registry.
    #[cfg(feature = "std")]
    pub fn try_send_to_island(&self, island_id: u32, cmd: IslandCommand) -> Result<(), IslandRouteError> {
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
    #[cfg(feature = "std")]
    pub fn send_to_island(&self, island_id: u32, cmd: IslandCommand) {
        self.try_send_to_island(island_id, cmd).unwrap_or_else(|error| {
            panic!("send_to_island failed for island {}: {:?}", island_id, error)
        });
    }

    #[cfg(feature = "std")]
    pub fn send_spawn_fiber_to_island(&self, island_id: u32, closure_data: vo_runtime::pack::PackedValue) {
        self.send_to_island(island_id, IslandCommand::SpawnFiber { closure_data });
    }

    #[cfg(feature = "std")]
    pub fn send_chan_request(
        &self,
        island_id: u32,
        endpoint_id: u64,
        kind: ChanRequestKind,
        from_island: u32,
        fiber_id: u64,
    ) {
        self.send_to_island(island_id, IslandCommand::ChanRequest {
            endpoint_id,
            kind,
            from_island,
            fiber_id,
        });
    }

    #[cfg(feature = "std")]
    pub fn send_chan_send_request(
        &self,
        island_id: u32,
        endpoint_id: u64,
        data: Vec<u8>,
        fiber_id: u64,
    ) {
        self.send_chan_request(
            island_id,
            endpoint_id,
            ChanRequestKind::Send { data },
            self.current_island_id,
            fiber_id,
        );
    }

    #[cfg(feature = "std")]
    pub fn send_chan_recv_request(&self, island_id: u32, endpoint_id: u64, fiber_id: u64) {
        self.send_chan_request(
            island_id,
            endpoint_id,
            ChanRequestKind::Recv,
            self.current_island_id,
            fiber_id,
        );
    }

    #[cfg(feature = "std")]
    pub fn send_chan_close_request(&self, island_id: u32, endpoint_id: u64) {
        self.send_chan_request(
            island_id,
            endpoint_id,
            ChanRequestKind::Close,
            self.current_island_id,
            0,
        );
    }

    #[cfg(feature = "std")]
    pub fn send_chan_response(
        &self,
        island_id: u32,
        endpoint_id: u64,
        kind: ChanResponseKind,
        fiber_id: u64,
    ) {
        self.send_to_island(island_id, IslandCommand::ChanResponse {
            endpoint_id,
            kind,
            fiber_id,
        });
    }

    #[cfg(feature = "std")]
    pub fn send_chan_recv_data_response(
        &self,
        island_id: u32,
        endpoint_id: u64,
        data: Vec<u8>,
        fiber_id: u64,
    ) {
        self.send_chan_response(
            island_id,
            endpoint_id,
            ChanResponseKind::RecvData { data, closed: false },
            fiber_id,
        );
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
    pub fn wake_waiter(&self, waiter: &vo_runtime::objects::queue_state::QueueWaiter, scheduler: &mut crate::scheduler::Scheduler) {
        if waiter.island_id == self.current_island_id {
            scheduler.wake_queue_waiter(waiter);
        } else {
            #[cfg(feature = "std")]
            { self.send_wake_to_island(waiter.island_id, waiter.fiber_id as u32); }
            // no_std: remote wakes are delivered by the host through transport commands
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
