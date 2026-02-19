//! VM types and state definitions.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::SentinelErrorCache;

use vo_runtime::ffi::ExternRegistry;
use vo_runtime::itab::ItabCache;

#[cfg(feature = "std")]
use std::sync::mpsc::{Sender, Receiver};
#[cfg(feature = "std")]
use std::thread::JoinHandle;
#[cfg(feature = "std")]
use std::sync::{Arc, Mutex};
#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(feature = "std")]
use vo_runtime::island::IslandCommand;

/// Shared registry of island command senders.
#[cfg(feature = "std")]
pub type IslandRegistry = Arc<Mutex<HashMap<u32, Sender<IslandCommand>>>>;

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
     MakePort,
     PortNotSupported,
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
    /// All runnable fibers are done; some fibers are waiting for external callbacks.
    /// The async run loop should await those callbacks then call wake_callback + resume.
    SuspendedForCallbacks,
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
    pub handle: GcRef,
    pub command_tx: Sender<IslandCommand>,
    pub join_handle: Option<JoinHandle<()>>,
}

/// VM mutable state that can be borrowed independently from scheduler.
pub struct VmState {
    pub gc: Gc,
    pub globals: Vec<u64>,
    pub itab_cache: ItabCache,
    pub extern_registry: ExternRegistry,
    pub program_args: Vec<String>,
    #[cfg(feature = "std")]
    pub io: vo_runtime::io::IoRuntime,
    /// Per-VM sentinel error cache (reset on each module load).
    pub sentinel_errors: SentinelErrorCache,
    /// Next island ID to assign
    pub next_island_id: u32,
    /// Active island threads (index = island_id - 1, since main island is 0)
    #[cfg(feature = "std")]
    pub island_threads: Vec<IslandThread>,
    /// Shared registry for cross-island wake (used by island VMs)
    #[cfg(feature = "std")]
    pub island_registry: Option<IslandRegistry>,
    /// Current island ID (0 for main island)
    #[cfg(feature = "std")]
    pub current_island_id: u32,
    /// Main island's command receiver (for wake commands from other islands)
    #[cfg(feature = "std")]
    pub main_cmd_rx: Option<Receiver<IslandCommand>>,
}

impl VmState {
    pub fn new() -> Self {
        Self {
            gc: Gc::new(),
            globals: Vec::new(),
            itab_cache: ItabCache::new(),
            extern_registry: ExternRegistry::new(),
            program_args: Vec::new(),
            #[cfg(feature = "std")]
            io: vo_runtime::io::IoRuntime::new()
                .unwrap_or_else(|e| panic!("IoRuntime::new failed: {}", e)),
            sentinel_errors: SentinelErrorCache::new(),
            next_island_id: 1, // 0 is main island
            #[cfg(feature = "std")]
            island_threads: Vec::new(),
            #[cfg(feature = "std")]
            island_registry: None,
            #[cfg(feature = "std")]
            current_island_id: 0,
            #[cfg(feature = "std")]
            main_cmd_rx: None,
        }
    }
    
    /// Send wake command to an island via shared registry.
    #[cfg(feature = "std")]
    pub fn send_wake_to_island(&self, island_id: u32, fiber_id: u32) -> bool {
        if let Some(ref registry) = self.island_registry {
            if let Ok(guard) = registry.lock() {
                if let Some(tx) = guard.get(&island_id) {
                    return tx.send(IslandCommand::WakeFiber { fiber_id }).is_ok();
                }
            }
        }
        false
    }

    /// Check if waiter is on current island.
    #[cfg(feature = "std")]
    #[inline]
    pub fn is_local_waiter(&self, waiter: &vo_runtime::objects::port::WaiterInfo) -> bool {
        waiter.island_id == self.current_island_id
    }

    /// Wake a waiter (local or remote). No PC modification - blocker sets resume PC.
    #[cfg(feature = "std")]
    pub fn wake_waiter(&self, waiter: &vo_runtime::objects::port::WaiterInfo, scheduler: &mut crate::scheduler::Scheduler) {
        if waiter.island_id == self.current_island_id {
            let fiber_id = crate::scheduler::FiberId::from_raw(waiter.fiber_id as u32);
            scheduler.wake_fiber(fiber_id);
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
        // Shutdown all island threads and wait for them to complete
        for island in &mut self.island_threads {
            // Send shutdown command
            let _ = island.command_tx.send(IslandCommand::Shutdown);
        }
        
        // Wait for all threads to finish
        for island in &mut self.island_threads {
            if let Some(handle) = island.join_handle.take() {
                let _ = handle.join();
            }
        }
    }
}
