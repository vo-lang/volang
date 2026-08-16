//! Fiber (coroutine) and related structures.

use core::num::NonZeroU64;
use core::sync::atomic::{AtomicUsize, Ordering};

#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::sync::Arc;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::sync::Arc;

use vo_runtime::ffi::HostEventReplaySource;
use vo_runtime::gc::GcRef;
#[cfg(feature = "std")]
use vo_runtime::io::IoToken;
use vo_runtime::island::{EndpointResponseKind, EndpointWaitKey};
use vo_runtime::objects::interface::InterfaceSlot;

use crate::vm::RuntimeTrapKind;

#[derive(Debug, Clone)]
pub enum RemoteRecvResponse {
    Data(Vec<u8>),
    Closed,
    Rejected,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RemoteEndpointWait {
    Send {
        endpoint_id: u64,
        wait_id: NonZeroU64,
    },
    Recv {
        endpoint_id: u64,
        wait_id: NonZeroU64,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct QueueWaitState {
    pub queue_ref: GcRef,
    pub kind: vo_runtime::objects::queue_state::SelectWaitKind,
    pub registration_id: u64,
}

/// VM-owned slot payload that may survive to a GC-visible boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedSlotPayload {
    pub values: Vec<u64>,
    pub slot_types: Vec<vo_runtime::SlotType>,
}

impl TypedSlotPayload {
    pub fn try_new(
        values: Vec<u64>,
        slot_types: Vec<vo_runtime::SlotType>,
    ) -> Result<Self, String> {
        if values.len() != slot_types.len() {
            return Err(format!(
                "typed slot payload width mismatch: values={} slot_types={}",
                values.len(),
                slot_types.len()
            ));
        }
        Ok(Self { values, slot_types })
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallFrame {
    pub func_id: u32,
    pub pc: usize,
    pub bp: usize,
    pub sp_restore: usize,
    pub ret_reg: u16,
    pub ret_count: u16,
}

impl CallFrame {
    #[inline]
    pub fn new(func_id: u32, bp: usize, sp_restore: usize, ret_reg: u16, ret_count: u16) -> Self {
        Self {
            func_id,
            pc: 0,
            bp,
            sp_restore,
            ret_reg,
            ret_count,
        }
    }
}

/// Proof that one call-frame slot and its stack window were admitted together.
/// The token is consumed when the frame becomes visible to GC and unwinding.
#[derive(Debug)]
#[must_use = "a reserved call window must be committed or deliberately abandoned"]
pub(crate) struct ReservedCallWindow {
    bp: usize,
    sp: usize,
}

/// Result of completing a verifier-authorized stack return without entering
/// the defer/replay state machine.
#[derive(Debug, Clone, Copy)]
pub(crate) enum CompletedStackReturn {
    Done,
    Resume(CallFrame),
}

#[derive(Debug, Clone)]
pub struct DeferArgLayout {
    pub slot_types: Vec<vo_runtime::SlotType>,
}

impl DeferArgLayout {
    pub fn try_from_caller_slot_types(
        caller_slot_types: &[vo_runtime::SlotType],
        caller_func_id: u32,
        caller_pc: u32,
        arg_start: u16,
        arg_slots: u16,
    ) -> Result<Self, String> {
        let start = arg_start as usize;
        let count = arg_slots as usize;
        let end = start.saturating_add(count);
        if end > caller_slot_types.len() {
            return Err(format!(
                "DeferArgLayout metadata missing: func_id={} pc={} slot range {}..{} expected {} slots actual slot_types={}",
                caller_func_id,
                caller_pc,
                start,
                end,
                count,
                caller_slot_types.len()
            ));
        }
        Ok(Self {
            slot_types: caller_slot_types[start..end].to_vec(),
        })
    }

    #[inline]
    pub fn arg_slots(&self) -> u16 {
        self.slot_types.len() as u16
    }
}

#[derive(Debug, Clone)]
pub struct DeferEntry {
    pub frame_depth: usize,
    pub func_id: u32,
    pub closure: GcRef,
    pub args: GcRef,
    pub arg_layout: DeferArgLayout,
    pub is_closure: bool,
    pub is_errdefer: bool,
    /// The panic generation when this defer was registered.
    /// A defer can recover a panic only if registered_at < current panic_generation.
    pub registered_at_generation: u64,
}

/// How return values are stored while defers execute.
#[derive(Debug, Clone)]
pub enum ReturnValues {
    /// Return values copied from stack before frame was popped.
    Stack {
        vals: Vec<u64>,
        /// SlotTypes for GC scanning during defer execution.
        slot_types: Vec<vo_runtime::SlotType>,
    },
    /// Escaped named returns: GcRefs to dereference after all defers complete.
    /// The actual values are read from heap at the end, so defers can modify them.
    Heap {
        gcrefs: Vec<u64>,
        /// Slot count for each GcRef (parallel array).
        slots_per_ref: Vec<usize>,
    },
}

/// Unwinding mode: Return (normal) or Panic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnwindingMode {
    /// Normal return with pending defers.
    Return,
    /// Panic unwinding - execute defers, check for recover().
    Panic,
}

/// Unified state for defer execution during return or panic unwinding.
///
/// Lifecycle:
/// 1. Return/panic triggers unwinding → UnwindingState created
/// 2. Each defer executes and returns → next defer called
/// 3. For Return: all defers done → write return values, clear state
/// 4. For Panic: if recover() called → switch to Return mode, resume normal
/// 5. For Panic: no recover, no more defers → unwind to parent frame
/// 6. For Panic: no more frames → return ExecResult::Panic
#[derive(Debug, Clone)]
pub struct UnwindingState {
    /// Defers remaining to execute in registration order (`last()` runs next).
    ///
    /// Keeping the next defer at the tail makes each unwind step an O(1) pop and
    /// lets defers registered by a running defer append without moving the older
    /// pending tail.
    pub pending: Vec<DeferEntry>,
    /// Frame depth after the unwinding function was popped.
    /// Defer functions run at depth = target_depth + 1.
    pub target_depth: usize,
    /// Unwinding mode: Return or Panic.
    pub mode: UnwindingMode,
    /// The generation of the currently executing defer.
    /// Used with Fiber.panic_generation to check if recover() should work.
    pub current_defer_generation: u64,
    /// Panic owned by this unwind operation. Older operations retain their own
    /// context while a nested call unwinds a newer panic.
    pub panic_context: Option<PanicContext>,
    /// Return values to write after all defers complete.
    /// None for void functions. For panic, may contain heap return values for recover().
    pub return_values: Option<ReturnValues>,
    /// Function whose return metadata applies to `return_values`.
    pub return_func_id: u32,
    /// PC in the returning function when return/unwind started.
    pub return_pc: usize,
    /// Where to write return values in caller's frame.
    pub caller_ret_reg: u16,
    /// How many slots caller expects.
    pub caller_ret_count: usize,
    /// The state represents a panic raised by the currently executing defer.
    /// Once recovered, the defer frame has already been removed and the
    /// suspended parent unwind must continue directly.
    pub resume_parent_after_recovery: bool,
    /// True when this is a closure-for-extern-replay return.
    /// When set, return_values=None means "skip writing return values" (handled by replay).
    /// When false, return_values=None means "write zeroed return values" (panic/recover).
    pub is_closure_replay: bool,
}

impl UnwindingState {
    /// Check if we're at the defer boundary (defer function just returned).
    #[inline]
    pub fn at_defer_boundary(&self, frame_count: usize) -> bool {
        frame_count == self.target_depth + 1
    }

    /// Switch from Panic to Return mode after successful recover().
    /// Filters out errdefers since function is now returning normally.
    pub fn switch_to_return_mode(&mut self) {
        self.pending.retain(|d| !d.is_errdefer);
        self.mode = UnwindingMode::Return;
        self.panic_context = None;
    }
}

/// Nested unwind operations, ordered from the oldest suspended operation to
/// the operation that currently owns the executing defer frame.
///
/// A defer may call an ordinary function which starts its own deferred return.
/// Keeping every operation here prevents the nested return from overwriting the
/// suspended caller's pending defers and return values.
#[derive(Debug, Clone, Default)]
pub struct UnwindingStack {
    states: Vec<UnwindingState>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnwindingStackOrderError {
    pub parent_target_depth: Option<usize>,
    pub child_target_depth: usize,
    pub child_mode: UnwindingMode,
    pub resume_parent_after_recovery: bool,
}

impl core::fmt::Display for UnwindingStackOrderError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "invalid nested unwind order: parent_depth={:?} child_depth={} child_mode={:?} resume_parent_after_recovery={}",
            self.parent_target_depth,
            self.child_target_depth,
            self.child_mode,
            self.resume_parent_after_recovery
        )
    }
}

#[cfg(feature = "std")]
impl std::error::Error for UnwindingStackOrderError {}

impl UnwindingStack {
    #[inline]
    pub fn as_ref(&self) -> Option<&UnwindingState> {
        self.states.last()
    }

    #[inline]
    pub fn as_mut(&mut self) -> Option<&mut UnwindingState> {
        self.states.last_mut()
    }

    #[inline]
    pub fn is_none(&self) -> bool {
        self.is_empty()
    }

    #[inline]
    pub fn is_some(&self) -> bool {
        !self.is_empty()
    }

    #[inline]
    pub fn try_push(&mut self, state: UnwindingState) -> Result<(), UnwindingStackOrderError> {
        let valid = match self.states.last() {
            None => !state.resume_parent_after_recovery,
            Some(parent) if state.resume_parent_after_recovery => {
                state.target_depth == parent.target_depth && state.mode == UnwindingMode::Panic
            }
            Some(parent) => state.target_depth > parent.target_depth,
        };
        if !valid {
            return Err(UnwindingStackOrderError {
                parent_target_depth: self.states.last().map(|state| state.target_depth),
                child_target_depth: state.target_depth,
                child_mode: state.mode,
                resume_parent_after_recovery: state.resume_parent_after_recovery,
            });
        }
        self.states.push(state);
        Ok(())
    }

    #[cfg(test)]
    #[inline]
    pub fn push(&mut self, state: UnwindingState) {
        self.try_push(state)
            .expect("test constructed an invalid nested unwind state");
    }

    #[inline]
    pub fn pop(&mut self) -> Option<UnwindingState> {
        self.states.pop()
    }

    #[inline]
    pub fn clear(&mut self) {
        self.states.clear();
    }

    #[inline]
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &UnwindingState> {
        self.states.iter()
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<&UnwindingState> {
        self.states.get(index)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.states.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.states.is_empty()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SelectCaseKind {
    Send,
    Recv,
}

impl SelectCaseKind {
    #[inline]
    pub fn wait_kind(self) -> vo_runtime::objects::queue_state::SelectWaitKind {
        match self {
            Self::Send => vo_runtime::objects::queue_state::SelectWaitKind::Send,
            Self::Recv => vo_runtime::objects::queue_state::SelectWaitKind::Recv,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SelectCase {
    pub kind: SelectCaseKind,
    pub result_index: u16,
    pub queue_reg: u16,
    pub val_reg: u16,
    pub elem_slots: u16,
    pub elem_layout: Option<Vec<vo_runtime::SlotType>>,
    pub has_ok: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SelectRegisteredQueue {
    pub case_index: u16,
    pub queue: vo_runtime::gc::GcRef,
    pub kind: SelectCaseKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SelectWokenResult {
    SendAccepted,
    Recv {
        data: Vec<u64>,
        slot_types: Vec<vo_runtime::SlotType>,
        closed: bool,
    },
}

#[derive(Debug, Clone)]
pub struct SelectState {
    pub cases: Vec<SelectCase>,
    pub expected_cases: u16,
    pub has_default: bool,
    pub woken_index: Option<usize>,
    pub woken_result: Option<SelectWokenResult>,
    /// Unique ID for this select instance, used for cancellation.
    /// When one case becomes ready, we cancel waiters on other channels using this ID.
    pub select_id: u64,
    /// Channels we've registered waiters on (for cancellation when woken).
    pub registered_queues: Vec<SelectRegisteredQueue>,
}

/// Fiber lifecycle state - single source of truth.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FiberState {
    /// In ready_queue, waiting to be scheduled.
    Runnable,
    /// Currently being executed.
    Running,
    /// Blocked waiting for external event.
    Blocked(BlockReason),
    /// Finished, slot can be recycled.
    Dead,
}

impl FiberState {
    #[inline]
    pub fn is_runnable(&self) -> bool {
        matches!(self, FiberState::Runnable)
    }

    #[inline]
    pub fn is_running(&self) -> bool {
        matches!(self, FiberState::Running)
    }

    #[inline]
    pub fn is_blocked(&self) -> bool {
        matches!(self, FiberState::Blocked(_))
    }

    #[inline]
    pub fn is_dead(&self) -> bool {
        matches!(self, FiberState::Dead)
    }
}

/// Reason why a fiber is blocked.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlockReason {
    /// Waiting for channel send/recv (queue-like primitives).
    Queue,
    /// Waiting for I/O completion.
    #[cfg(feature = "std")]
    Io(vo_runtime::io::IoToken),
    /// Waiting for a host-side event (e.g. setTimeout, platform timer).
    /// Fiber resumes at next instruction after wake.
    /// `delay_ms` is a hint to the platform (e.g. setTimeout ms); 0 = no hint.
    HostEvent { token: u64, delay_ms: u32 },
    /// Waiting for a host-side async op that produces a result (e.g. fetch Promise).
    /// Fiber re-executes the extern on wake (PC was undone before blocking).
    HostEventReplay {
        token: u64,
        source: HostEventReplaySource,
    },
}

#[cfg(feature = "jit")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JitExternSuspend {
    Exit {
        code: i32,
    },
    Yield {
        resume_pc: u32,
    },
    QueueBlock {
        resume_pc: u32,
    },
    #[cfg(feature = "std")]
    WaitIo {
        token: vo_runtime::io::IoToken,
        replay_pc: u32,
        staged_io_roots_added: bool,
    },
    HostWait {
        token: u64,
        delay_ms: u32,
        resume_pc: u32,
    },
    HostReplay {
        token: u64,
        source: HostEventReplaySource,
        replay_pc: u32,
    },
    CallClosure {
        closure_ref: GcRef,
        args: TypedSlotPayload,
        replay_pc: u32,
    },
}

/// Unified panic state for both recoverable and fatal panics.
#[derive(Debug, Clone, Copy)]
pub enum PanicState {
    /// Recoverable panic (user code panic, runtime errors like bounds check).
    /// Can be caught by recover() in a defer.
    /// Stores full interface{} value as InterfaceSlot.
    Recoverable(InterfaceSlot),
    /// Fatal panic (internal runtime errors that cannot be recovered).
    /// Examples: unsupported operation.
    Fatal,
}

/// Complete identity and diagnostics for one recoverable panic generation.
/// Stored in unwind states so a nested panic can be recovered without erasing
/// an older panic that is suspended below it.
#[derive(Debug, Clone, Copy)]
pub struct PanicContext {
    pub state: PanicState,
    pub trap_kind: Option<RuntimeTrapKind>,
    pub source_loc: Option<(u32, u32)>,
    pub generation: u64,
}

impl PanicState {
    /// Extract human-readable message from panic value.
    pub fn message(&self) -> String {
        match self {
            PanicState::Fatal => "fatal error".to_string(),
            PanicState::Recoverable(val) => {
                if val.is_string() && !val.as_ref().is_null() {
                    return val.to_display_string();
                }
                "panic".to_string()
            }
        }
    }
}

/// State for extern closure callback suspend/replay.
///
/// When an extern function requests a closure call (ExternResult::CallClosure),
/// the VM pushes the closure frame, executes it, caches the return values here,
/// then replays the extern with cached results.
#[derive(Debug, Clone)]
pub struct ClosureReplayState {
    /// Accumulated closure call results for extern replay.
    /// Each entry is (return_values, slot_types) from one closure call.
    /// slot_types are needed for GC scanning — without them, non-GcRef values
    /// (int, float, interface slot0 metadata) would be dereferenced as pointers.
    /// On extern replay, results are consumed in order within the active
    /// extern scope. The scope is cleared when that extern finally returns a
    /// terminal result, but parent scopes survive nested extern calls.
    pub results: Vec<(Vec<u64>, Vec<vo_runtime::SlotType>)>,
    /// Consumption index during extern replay.
    /// Tracks how many cached results have been consumed in the current replay.
    /// Reset to 0 at the start of each CallExtern execution.
    pub index: usize,
    /// Nested closure-replay boundaries, ordered outermost to innermost.
    /// Depth and replay PC form one transition record so they cannot drift.
    boundaries: Vec<ClosureReplayBoundary>,
    /// Original panic message captured when the replayed closure unwound.
    /// Preserved so the replayed extern can report the true root cause.
    /// `is_some()` also serves as the "panicked" flag.
    pub panic_message: Option<String>,
    /// Active extern replay scope.
    pub extern_scope: Option<ClosureReplayExternScope>,
    /// Saved parent extern scopes for nested extern calls.
    pub extern_scope_stack: Vec<ClosureReplayExternScope>,
    /// Saved parent panic messages for nested extern calls.
    pub panic_message_stack: Vec<Option<String>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ClosureReplayBoundary {
    pub frame_depth: usize,
    pub replay_pc: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ClosureReplayExternScope {
    pub result_start: usize,
    pub frame_depth: usize,
}

impl Default for ClosureReplayState {
    fn default() -> Self {
        Self::new()
    }
}

impl ClosureReplayState {
    pub fn new() -> Self {
        Self {
            results: Vec::new(),
            index: 0,
            boundaries: Vec::new(),
            panic_message: None,
            extern_scope: None,
            extern_scope_stack: Vec::new(),
            panic_message_stack: Vec::new(),
        }
    }

    pub fn reset(&mut self) {
        self.results.clear();
        self.index = 0;
        self.boundaries.clear();
        self.panic_message = None;
        self.extern_scope = None;
        self.extern_scope_stack.clear();
        self.panic_message_stack.clear();
    }

    /// Prepare a replay snapshot for a new CallExtern execution.
    ///
    /// The fiber keeps the authoritative typed replay log until the extern
    /// returns a terminal result. That lets multi-step replay re-run from the
    /// beginning and keeps cached GC references visible to VM root scanning.
    pub fn snapshot_for_extern(
        &mut self,
        frame_depth: usize,
    ) -> (Vec<vo_runtime::ffi::ExternReplayResult>, Option<String>) {
        self.begin_extern_scope(frame_depth);
        let result_start = self
            .extern_scope
            .map(|scope| scope.result_start.min(self.results.len()))
            .unwrap_or(0);
        let results = self
            .results
            .iter()
            .skip(result_start)
            .map(|(vals, slot_types)| {
                vo_runtime::ffi::ExternReplayResult::new(vals.clone(), slot_types.clone())
            })
            .collect();
        let panic_message = self.panic_message.clone();
        self.index = result_start;
        (results, panic_message)
    }

    fn begin_extern_scope(&mut self, frame_depth: usize) {
        match self.extern_scope {
            Some(scope) if scope.frame_depth == frame_depth => {}
            Some(scope) => {
                self.extern_scope_stack.push(scope);
                self.panic_message_stack.push(self.panic_message.take());
                self.extern_scope = Some(ClosureReplayExternScope {
                    result_start: self.results.len(),
                    frame_depth,
                });
            }
            None => {
                self.extern_scope = Some(ClosureReplayExternScope {
                    result_start: self.results.len(),
                    frame_depth,
                });
            }
        }
    }

    /// Finish a terminal extern replay result.
    ///
    /// Nested externs can run while an outer extern's closure replay is still
    /// pending. A terminal inner extern must discard only the replay results it
    /// produced and then restore the parent replay scope.
    pub fn finish_extern_terminal(&mut self) {
        let Some(scope) = self.extern_scope.take() else {
            self.reset();
            return;
        };
        self.results
            .truncate(scope.result_start.min(self.results.len()));
        self.index = scope.result_start.min(self.results.len());
        self.panic_message = self.panic_message_stack.pop().flatten();
        self.extern_scope = self.extern_scope_stack.pop();
        if self.extern_scope.is_none() {
            self.index = 0;
        }
    }

    /// Prepare a full unscoped replay snapshot for tests and root scanning.
    #[cfg(test)]
    pub fn snapshot_all_for_test(
        &mut self,
    ) -> (Vec<vo_runtime::ffi::ExternReplayResult>, Option<String>) {
        let results = self
            .results
            .iter()
            .map(|(vals, slot_types)| {
                vo_runtime::ffi::ExternReplayResult::new(vals.clone(), slot_types.clone())
            })
            .collect();
        let panic_message = self.panic_message.clone();
        self.index = 0;
        (results, panic_message)
    }

    /// Publish the boundary owned by a newly pushed replay closure frame.
    pub fn push_boundary(&mut self, frame_depth: usize, replay_pc: usize) {
        self.boundaries.push(ClosureReplayBoundary {
            frame_depth,
            replay_pc,
        });
    }

    /// Retire the innermost replay boundary after return or intercepted panic.
    pub fn pop_boundary(&mut self) -> Option<ClosureReplayBoundary> {
        self.boundaries.pop()
    }

    #[inline]
    pub fn active_boundary(&self) -> Option<ClosureReplayBoundary> {
        self.boundaries.last().copied()
    }

    #[inline]
    pub fn boundary_count(&self) -> usize {
        self.boundaries.len()
    }

    /// Check if current frame is at the closure replay boundary.
    #[inline]
    pub fn at_replay_boundary(&self, frame_count: usize) -> bool {
        self.active_boundary()
            .is_some_and(|boundary| frame_count == boundary.frame_depth)
    }

    /// Check if panic should be intercepted at closure replay boundary.
    #[inline]
    pub fn should_intercept_panic(&self, frame_count: usize) -> bool {
        self.active_boundary()
            .is_some_and(|boundary| frame_count <= boundary.frame_depth)
    }
}

/// Minimum stack size after the first non-empty reservation (2 KiB).
///
/// This matches the JIT's maximum SSA-local budget, keeps fresh fibers cheap,
/// and still lets larger stacks grow geometrically.
pub(crate) const MIN_STACK_SLOTS: usize = 256;
/// Largest completed-fiber stack retained across an idle boundary (32 KiB).
///
/// Active scheduling bursts can reuse larger stacks until the VM next waits,
/// while idle VMs keep only a modest warm cache per reusable slot.
pub(crate) const MAX_RETAINED_STACK_SLOTS: usize = 1 << 12;
/// Maximum stack capacity per fiber in slots (8 MiB).
///
/// Without a VM-owned limit, runaway recursion keeps doubling the Rust Vec
/// until wasm memory allocation fails as a raw `memory access out of bounds`.
/// Keep the failure at the VM stack boundary so the reported error points at
/// the actual execution problem.
pub const MAX_STACK_CAPACITY: usize = 1 << 20;
/// Maximum slot budget for JIT direct-call shadow stack chains.
///
/// JIT-to-JIT direct calls reserve unmaterialized windows in `fiber.stack` and
/// materialize real frames only on side exits. Keep a separate bound so deep
/// native call chains fail at the language stack boundary.
pub const MAX_JIT_DIRECT_STACK_SLOTS: usize = 1 << 15;
/// Maximum nested direct JIT call depth before converting recursion into a
/// recoverable Vo stack overflow.
pub const MAX_JIT_CALL_DEPTH: usize = 512;
/// Maximum call frames per fiber.
///
/// Small-frame recursion can keep reusing the same stack slots while growing
/// only `frames`. Without a VM-owned frame limit, wasm eventually reports a raw
/// `memory access out of bounds` from `RawVec::grow_one` in call-frame push.
const MAX_CALL_FRAMES: usize = 1 << 15;
/// Largest completed-fiber call-frame cache retained for slot reuse.
///
/// The direct JIT call-depth limit also bounds the idle materialized-frame cache.
pub(crate) const MAX_RETAINED_CALL_FRAMES: usize = MAX_JIT_CALL_DEPTH;

/// VM-owned limits for native Fiber and scheduler storage.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VmResourceLimits {
    pub max_fibers: usize,
    pub max_total_fiber_storage_bytes: usize,
    pub max_stack_slots_per_fiber: usize,
    pub max_call_frames_per_fiber: usize,
}

impl Default for VmResourceLimits {
    fn default() -> Self {
        Self {
            max_fibers: 16 * 1024,
            max_total_fiber_storage_bytes: 512 * 1024 * 1024,
            max_stack_slots_per_fiber: MAX_STACK_CAPACITY,
            max_call_frames_per_fiber: MAX_CALL_FRAMES,
        }
    }
}

#[derive(Debug)]
pub(crate) struct FiberStorageBudget {
    limit_bytes: usize,
    used_bytes: AtomicUsize,
}

impl FiberStorageBudget {
    pub(crate) fn new(limit_bytes: usize) -> Self {
        Self {
            limit_bytes,
            used_bytes: AtomicUsize::new(0),
        }
    }

    fn try_charge(&self, bytes: usize) -> bool {
        let mut used = self.used_bytes.load(Ordering::Relaxed);
        loop {
            let Some(next) = used.checked_add(bytes) else {
                return false;
            };
            if next > self.limit_bytes {
                return false;
            }
            match self.used_bytes.compare_exchange_weak(
                used,
                next,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => return true,
                Err(actual) => used = actual,
            }
        }
    }

    fn release(&self, bytes: usize) {
        let previous = self.used_bytes.fetch_sub(bytes, Ordering::Relaxed);
        debug_assert!(previous >= bytes);
    }

    pub(crate) fn used_bytes(&self) -> usize {
        self.used_bytes.load(Ordering::Relaxed)
    }

    pub(crate) fn limit_bytes(&self) -> usize {
        self.limit_bytes
    }
}

/// Resume point for JIT call chain suspension.
///
/// When JIT returns `Call` or `WaitIo`, this captures the minimal state
/// needed to resume execution after the VM handles the request.
#[derive(Debug, Clone, Copy)]
pub struct ResumePoint {
    /// Function id (callee).
    pub func_id: u32,
    /// Bytecode PC to resume from (caller's resume_pc).
    pub resume_pc: u32,
    /// Base pointer for this frame (callee's bp).
    pub bp: usize,
    /// Caller's base pointer (needed for jit_pop_frame to restore ctx.jit_bp).
    pub caller_bp: usize,
    /// Return register in caller's frame where return values should go.
    pub ret_reg: u16,
    /// Return slots expected.
    pub ret_slots: u16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FiberCapacityError {
    StackSlots {
        required: usize,
        limit: usize,
    },
    CallFrames {
        required: usize,
        limit: usize,
    },
    HostStorage {
        resource: &'static str,
        requested_bytes: usize,
        limit_bytes: usize,
    },
    HostAllocation {
        resource: &'static str,
    },
}

impl FiberCapacityError {
    pub fn message(&self) -> String {
        match self {
            FiberCapacityError::StackSlots { required, limit } => format!(
                "runtime error: stack overflow: required {} slots exceeds limit {}",
                required, limit
            ),
            FiberCapacityError::CallFrames { required, limit } => format!(
                "runtime error: stack overflow: required {} call frames exceeds limit {}",
                required, limit
            ),
            FiberCapacityError::HostStorage {
                resource,
                requested_bytes,
                limit_bytes,
            } => format!(
                "runtime error: VM {resource} storage limit exceeded: requested {requested_bytes} bytes, limit {limit_bytes} bytes"
            ),
            FiberCapacityError::HostAllocation { resource } => {
                format!("runtime error: VM {resource} host allocation failed")
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FiberIdentityExhausted {
    Select,
    RemoteEndpointWait,
    HostAllocation(&'static str),
}

impl core::fmt::Display for FiberIdentityExhausted {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Select => f.write_str("fiber select identity space exhausted"),
            Self::RemoteEndpointWait => {
                f.write_str("fiber remote endpoint wait identity space exhausted")
            }
            Self::HostAllocation(resource) => {
                write!(f, "fiber host allocation failed for {resource}")
            }
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for FiberIdentityExhausted {}

#[derive(Debug)]
pub struct Fiber {
    pub id: u32,
    /// Generation for protocols that carry opaque fiber handles across turns.
    /// Incremented whenever a scheduler slot is reused so stale responses cannot
    /// target a new fiber that happens to occupy the same slot.
    pub generation: u32,
    /// Unified state machine (single source of truth).
    pub state: FiberState,
    pub stack: Vec<u64>,
    /// Stack pointer - current stack top. stack[0..sp] is in use.
    pub sp: usize,
    pub frames: Vec<CallFrame>,
    resource_limits: VmResourceLimits,
    storage_budget: Arc<FiberStorageBudget>,
    accounted_storage_bytes: usize,
    pub defer_stack: Vec<DeferEntry>,
    pub unwinding: UnwindingStack,
    pub queue_wait_state: Option<QueueWaitState>,
    pub select_state: Option<SelectState>,
    /// Next unique select ID within this fiber; `None` permanently records
    /// exhaustion until the scheduler safely resets the fiber generation.
    next_select_id: Option<u64>,
    pub panic_state: Option<PanicState>,
    pub panic_trap_kind: Option<RuntimeTrapKind>,
    /// Incremented each time a new panic starts. Used to determine which defers can recover.
    /// A defer registered at generation N can only recover panics with generation > N.
    pub panic_generation: u64,
    /// Generation of `panic_state`; differs from `panic_generation` after a
    /// nested panic is recovered and an older panic becomes active again.
    pub active_panic_generation: Option<u64>,
    /// Source location (func_id, pc) captured at panic initiation, before frames are unwound.
    /// Used by kill_current() to report accurate error locations.
    pub panic_source_loc: Option<(u32, u32)>,
    #[cfg(feature = "std")]
    pub resume_io_token: Option<IoToken>,
    /// Host event token set when fiber wakes via `HostEventWaitAndReplay`.
    /// Read by extern on re-invocation via `take_resume_host_event_token()`.
    pub resume_host_event_token: Option<u64>,
    /// Opaque data attached by host via `wake_host_event_with_data`.
    /// Read by extern on re-invocation via `take_resume_host_event_data()`.
    pub resume_host_event_data: Option<Vec<u8>>,
    /// JIT resume stack for suspended call chains.
    /// When JIT returns Call/WaitIo, resume points are pushed here.
    /// On resume, they are popped and converted to VM frames.
    #[cfg(feature = "jit")]
    pub resume_stack: Vec<ResumePoint>,
    #[cfg(feature = "jit")]
    pub jit_extern_suspend: Option<JitExternSuspend>,
    /// Closure callback suspend/replay state for extern functions.
    pub closure_replay: ClosureReplayState,
    /// Instructions still available in the current scheduler turn.
    /// Shared by interpreter execution, loop OSR, and nested full-function JIT calls.
    pub(crate) execution_budget: u32,
    /// Reused operand/result storage for interpreter map instructions.
    pub(crate) map_scratch: crate::exec::MapScratch,
    /// JIT panic flag — set by JIT code when a runtime error occurs (nil deref, bounds check).
    /// Replaces the per-call Box<JitOwnedState> allocation.
    #[cfg(feature = "jit")]
    pub jit_panic_flag: bool,
    /// JIT user panic flag — true when panic is from explicit `panic()` call, false for runtime errors.
    #[cfg(feature = "jit")]
    pub jit_is_user_panic: bool,
    /// JIT panic message — the interface{} value passed to panic().
    #[cfg(feature = "jit")]
    pub jit_panic_msg: InterfaceSlot,
    /// JIT infrastructure diagnostic message published by runtime callbacks.
    #[cfg(feature = "jit")]
    pub jit_infra_error_message: String,
    /// Reused wide-result storage for VM-to-JIT calls.
    #[cfg(feature = "jit")]
    pub(crate) jit_return_scratch: Vec<u64>,
    /// Reused wide argument/result frame for JIT-to-extern callbacks.
    #[cfg(feature = "jit")]
    pub(crate) jit_extern_scratch: Vec<u64>,
    /// Pending remote recv response data from home island.
    /// Set by handle_chan_response_command before waking fiber.
    /// Consumed by ChanRecv handler on retry.
    pub remote_recv_response: Option<RemoteRecvResponse>,
    /// Flag indicating REMOTE send was on a closed channel.
    /// Set by handle_chan_response_command(SendAck{closed:true}) before waking fiber.
    /// Consumed by ChanSend handler on retry.
    pub remote_send_closed: bool,
    /// Source-specific identity for an outstanding remote endpoint wait.
    pub remote_endpoint_wait: Option<RemoteEndpointWait>,
    next_remote_endpoint_wait_id: Option<u64>,
}

/// Validated entry state for a fiber whose scheduler identity is assigned at commit.
///
/// Keeping only the initialized frame prefix avoids growing a Fiber stack while
/// a runtime transition is still pending.
#[derive(Debug)]
pub(crate) struct PendingSpawn {
    func_id: u32,
    local_slots: u16,
    ret_slots: u16,
    entry_slots: Vec<u64>,
}

impl PendingSpawn {
    pub(crate) fn try_new(
        func_id: u32,
        local_slots: u16,
        ret_slots: u16,
        entry_slots: Vec<u64>,
    ) -> Result<Self, FiberCapacityError> {
        let initialized_slots = entry_slots.len();
        if initialized_slots > local_slots as usize {
            return Err(FiberCapacityError::StackSlots {
                required: initialized_slots,
                limit: local_slots as usize,
            });
        }
        Ok(Self {
            func_id,
            local_slots,
            ret_slots,
            entry_slots,
        })
    }

    pub(crate) fn initialize(self, fiber: &mut Fiber) -> Result<(), FiberCapacityError> {
        debug_assert_eq!(fiber.sp, 0);
        debug_assert!(fiber.frames.is_empty());
        let bp = fiber.try_push_frame(self.func_id, self.local_slots, 0, self.ret_slots)?;
        fiber.zero_slots_at(bp, usize::from(self.local_slots));
        fiber.copy_slots_from_slice(bp, &self.entry_slots);
        Ok(())
    }

    /// Reserve every fallible Fiber allocation used by `initialize` without
    /// publishing the spawn or changing its execution state.
    pub(crate) fn preflight(&self, fiber: &mut Fiber) -> Result<(), FiberCapacityError> {
        fiber.try_ensure_capacity(self.local_slots as usize)?;
        fiber.try_reserve_call_frames(1)
    }

    #[cfg(test)]
    pub(crate) fn for_test(func_id: u32) -> Self {
        Self::try_new(func_id, 0, 0, Vec::new()).expect("empty test spawn")
    }
}

impl Fiber {
    pub fn new(id: u32) -> Self {
        let limits = VmResourceLimits::default();
        Self::new_with_resources(
            id,
            limits,
            Arc::new(FiberStorageBudget::new(
                limits.max_total_fiber_storage_bytes,
            )),
        )
    }

    pub(crate) fn new_with_resources(
        id: u32,
        resource_limits: VmResourceLimits,
        storage_budget: Arc<FiberStorageBudget>,
    ) -> Self {
        Self {
            id,
            generation: 1,
            state: FiberState::Runnable,
            stack: Vec::new(),
            sp: 0,
            frames: Vec::new(),
            resource_limits,
            storage_budget,
            accounted_storage_bytes: 0,
            defer_stack: Vec::new(),
            unwinding: UnwindingStack::default(),
            queue_wait_state: None,
            select_state: None,
            next_select_id: Some(0),
            panic_state: None,
            panic_trap_kind: None,
            panic_generation: 0,
            active_panic_generation: None,
            panic_source_loc: None,
            #[cfg(feature = "std")]
            resume_io_token: None,
            resume_host_event_token: None,
            resume_host_event_data: None,
            #[cfg(feature = "jit")]
            resume_stack: Vec::new(), // Lazy: only allocates on first push (Call/WaitIo)
            #[cfg(feature = "jit")]
            jit_extern_suspend: None,
            closure_replay: ClosureReplayState::new(),
            execution_budget: 0,
            map_scratch: crate::exec::MapScratch::default(),
            #[cfg(feature = "jit")]
            jit_panic_flag: false,
            #[cfg(feature = "jit")]
            jit_is_user_panic: false,
            #[cfg(feature = "jit")]
            jit_panic_msg: InterfaceSlot::default(),
            #[cfg(feature = "jit")]
            jit_infra_error_message: String::new(),
            #[cfg(feature = "jit")]
            jit_return_scratch: Vec::new(),
            #[cfg(feature = "jit")]
            jit_extern_scratch: Vec::new(),
            remote_recv_response: None,
            remote_send_closed: false,
            remote_endpoint_wait: None,
            next_remote_endpoint_wait_id: Some(1),
        }
    }

    pub fn consume_remote_send_closed(&mut self) -> bool {
        let closed = self.remote_send_closed;
        self.remote_send_closed = false;
        closed
    }

    pub fn wake_key_packed(&self) -> u64 {
        ((self.generation as u64) << 32) | self.id as u64
    }

    pub fn endpoint_response_key(&self) -> u64 {
        self.wake_key_packed()
    }

    pub fn begin_queue_wait(&mut self, waiter: &vo_runtime::objects::queue_state::QueueWaiter) {
        self.queue_wait_state = waiter.queue_identity().zip(waiter.registration_id()).map(
            |((queue_ref, kind), registration_id)| QueueWaitState {
                queue_ref: queue_ref as GcRef,
                kind,
                registration_id: registration_id.get(),
            },
        );
    }

    pub fn clear_queue_wait(&mut self) {
        self.queue_wait_state = None;
    }

    pub fn queue_wait_matches(
        &self,
        waiter: &vo_runtime::objects::queue_state::QueueWaiter,
    ) -> bool {
        match (
            self.queue_wait_state,
            waiter.queue_identity(),
            waiter.registration_id(),
        ) {
            (Some(state), Some((queue_ref, kind)), Some(registration_id)) => {
                state.queue_ref as u64 == queue_ref
                    && state.kind == kind
                    && state.registration_id == registration_id.get()
            }
            (None, None, None) => true,
            _ => false,
        }
    }

    pub fn try_alloc_select_id(&mut self) -> Result<u64, FiberIdentityExhausted> {
        let select_id = self.next_select_id.ok_or(FiberIdentityExhausted::Select)?;
        self.next_select_id = select_id.checked_add(1);
        Ok(select_id)
    }

    fn try_alloc_remote_endpoint_wait_id(&mut self) -> Result<NonZeroU64, FiberIdentityExhausted> {
        let raw_wait_id = self
            .next_remote_endpoint_wait_id
            .ok_or(FiberIdentityExhausted::RemoteEndpointWait)?;
        self.next_remote_endpoint_wait_id = raw_wait_id.checked_add(1);
        NonZeroU64::new(raw_wait_id).ok_or(FiberIdentityExhausted::RemoteEndpointWait)
    }

    pub fn begin_remote_endpoint_send_wait(&mut self, endpoint_id: u64) -> EndpointWaitKey {
        self.try_begin_remote_endpoint_send_wait(endpoint_id)
            .expect("fiber remote endpoint wait identity space exhausted")
    }

    pub fn try_begin_remote_endpoint_send_wait(
        &mut self,
        endpoint_id: u64,
    ) -> Result<EndpointWaitKey, FiberIdentityExhausted> {
        let wait_id = self.try_alloc_remote_endpoint_wait_id()?;
        self.remote_endpoint_wait = Some(RemoteEndpointWait::Send {
            endpoint_id,
            wait_id,
        });
        Ok(EndpointWaitKey::new(self.endpoint_response_key(), wait_id))
    }

    pub fn begin_remote_endpoint_recv_wait(&mut self, endpoint_id: u64) -> EndpointWaitKey {
        self.try_begin_remote_endpoint_recv_wait(endpoint_id)
            .expect("fiber remote endpoint wait identity space exhausted")
    }

    pub fn try_begin_remote_endpoint_recv_wait(
        &mut self,
        endpoint_id: u64,
    ) -> Result<EndpointWaitKey, FiberIdentityExhausted> {
        let wait_id = self.try_alloc_remote_endpoint_wait_id()?;
        self.remote_endpoint_wait = Some(RemoteEndpointWait::Recv {
            endpoint_id,
            wait_id,
        });
        Ok(EndpointWaitKey::new(self.endpoint_response_key(), wait_id))
    }

    pub fn apply_endpoint_response(
        &mut self,
        endpoint_id: u64,
        kind: EndpointResponseKind,
    ) -> bool {
        match (self.remote_endpoint_wait, kind) {
            (
                Some(RemoteEndpointWait::Send {
                    endpoint_id: expected,
                    wait_id: expected_wait_id,
                }),
                EndpointResponseKind::SendAck { closed, wait_key },
            ) if expected == endpoint_id && expected_wait_id == wait_key.wait_id() => {
                if closed {
                    self.remote_send_closed = true;
                }
                self.remote_endpoint_wait = None;
                true
            }
            (
                Some(RemoteEndpointWait::Recv {
                    endpoint_id: expected,
                    wait_id: expected_wait_id,
                }),
                EndpointResponseKind::RecvData {
                    data,
                    closed,
                    wait_key,
                },
            ) if expected == endpoint_id && expected_wait_id == wait_key.wait_id() => {
                self.remote_recv_response = Some(if closed {
                    RemoteRecvResponse::Closed
                } else {
                    RemoteRecvResponse::Data(data)
                });
                self.remote_endpoint_wait = None;
                true
            }
            (
                Some(RemoteEndpointWait::Recv {
                    endpoint_id: expected,
                    wait_id: expected_wait_id,
                }),
                EndpointResponseKind::RecvError { wait_key },
            ) if expected == endpoint_id && expected_wait_id == wait_key.wait_id() => {
                self.remote_recv_response = Some(RemoteRecvResponse::Rejected);
                self.remote_endpoint_wait = None;
                true
            }
            _ => false,
        }
    }

    pub fn can_apply_endpoint_response(
        &self,
        endpoint_id: u64,
        kind: &EndpointResponseKind,
    ) -> bool {
        match (self.remote_endpoint_wait, kind) {
            (
                Some(RemoteEndpointWait::Send {
                    endpoint_id: expected,
                    wait_id: expected_wait_id,
                }),
                EndpointResponseKind::SendAck { wait_key, .. },
            )
            | (
                Some(RemoteEndpointWait::Recv {
                    endpoint_id: expected,
                    wait_id: expected_wait_id,
                }),
                EndpointResponseKind::RecvData { wait_key, .. },
            )
            | (
                Some(RemoteEndpointWait::Recv {
                    endpoint_id: expected,
                    wait_id: expected_wait_id,
                }),
                EndpointResponseKind::RecvError { wait_key },
            ) => expected == endpoint_id && expected_wait_id == wait_key.wait_id(),
            _ => false,
        }
    }

    /// Reset fiber for reuse.
    pub fn reset(&mut self) {
        self.state = FiberState::Runnable;
        self.sp = 0;
        self.frames.clear();
        self.defer_stack.clear();
        self.unwinding.clear();
        self.queue_wait_state = None;
        self.select_state = None;
        self.next_select_id = Some(0);
        self.panic_state = None;
        self.panic_trap_kind = None;
        self.panic_generation = 0;
        self.active_panic_generation = None;
        self.panic_source_loc = None;
        #[cfg(feature = "std")]
        {
            self.resume_io_token = None;
        }
        self.resume_host_event_token = None;
        self.resume_host_event_data = None;
        #[cfg(feature = "jit")]
        self.resume_stack.clear();
        #[cfg(feature = "jit")]
        {
            self.jit_extern_suspend = None;
        }
        self.closure_replay.reset();
        self.execution_budget = 0;
        #[cfg(feature = "jit")]
        {
            self.jit_panic_flag = false;
            self.jit_is_user_panic = false;
            self.jit_panic_msg = InterfaceSlot::default();
            self.jit_infra_error_message.clear();
        }
        self.remote_recv_response = None;
        self.remote_send_closed = false;
        self.remote_endpoint_wait = None;
        self.next_remote_endpoint_wait_id = Some(1);
    }

    #[inline]
    pub(crate) fn has_oversized_storage(&self) -> bool {
        self.stack.capacity() > MAX_RETAINED_STACK_SLOTS
            || self.frames.capacity() > MAX_RETAINED_CALL_FRAMES
    }

    /// Shed exceptional high-water storage while preserving scheduler identity.
    pub(crate) fn release_oversized_storage(&mut self) {
        debug_assert!(self.state.is_dead());
        if !self.has_oversized_storage() {
            return;
        }
        let mut fresh = Self::new_with_resources(
            self.id,
            self.resource_limits,
            Arc::clone(&self.storage_budget),
        );
        fresh.generation = self.generation;
        *self = fresh;
        self.state = FiberState::Dead;
    }

    /// Check if current panic is recoverable and return the interface{} value if so.
    /// Used by recover() to consume the panic value.
    pub fn take_recoverable_panic(&mut self) -> Option<InterfaceSlot> {
        match self.panic_state.take() {
            Some(PanicState::Recoverable(val)) => {
                self.panic_trap_kind = None;
                self.panic_source_loc = None;
                self.active_panic_generation = None;
                Some(val)
            }
            other => {
                self.panic_state = other; // Put it back if not recoverable
                None
            }
        }
    }

    /// Set a fatal (non-recoverable) panic.
    pub fn set_fatal_panic(&mut self) {
        self.panic_state = Some(PanicState::Fatal);
        self.panic_trap_kind = None;
        self.active_panic_generation = None;
    }

    /// Set a recoverable panic with full interface{} value (InterfaceSlot).
    /// Also increments panic_generation so we can track which defers can recover.
    pub fn set_recoverable_panic(&mut self, msg: InterfaceSlot) {
        let Some(generation) = self.panic_generation.checked_add(1) else {
            // A wrapped generation could let a defer registered for an ancient
            // panic recover a new one. At this unreachable boundary, preserve
            // identity safety by escalating the new panic to fatal.
            self.set_fatal_panic();
            return;
        };
        self.panic_generation = generation;
        self.panic_state = Some(PanicState::Recoverable(msg));
        self.panic_trap_kind = None;
        self.active_panic_generation = Some(generation);
    }

    /// Set a recoverable runtime trap (typed runtime panic).
    pub fn set_recoverable_trap(&mut self, kind: RuntimeTrapKind, msg: InterfaceSlot) {
        let Some(generation) = self.panic_generation.checked_add(1) else {
            self.set_fatal_panic();
            return;
        };
        self.panic_generation = generation;
        self.panic_state = Some(PanicState::Recoverable(msg));
        self.panic_trap_kind = Some(kind);
        self.active_panic_generation = Some(generation);
    }

    #[inline]
    pub fn panic_context(&self) -> Option<PanicContext> {
        let state = self.panic_state?;
        Some(PanicContext {
            state,
            trap_kind: self.panic_trap_kind,
            source_loc: self.panic_source_loc,
            generation: self
                .active_panic_generation
                .unwrap_or(self.panic_generation),
        })
    }

    #[inline]
    pub fn restore_panic_context(&mut self, context: Option<PanicContext>) {
        if let Some(context) = context {
            self.panic_state = Some(context.state);
            self.panic_trap_kind = context.trap_kind;
            self.panic_source_loc = context.source_loc;
            self.active_panic_generation = Some(context.generation);
        } else {
            self.panic_state = None;
            self.panic_trap_kind = None;
            self.panic_source_loc = None;
            self.active_panic_generation = None;
        }
    }

    /// Get panic message for error reporting.
    pub fn panic_message(&self) -> Option<String> {
        self.panic_state.as_ref().map(|s| s.message())
    }

    /// Check if we're at the defer boundary (defer function just returned).
    #[inline]
    pub fn at_defer_boundary(&self) -> bool {
        self.unwinding
            .as_ref()
            .is_some_and(|s| s.at_defer_boundary(self.frames.len()))
    }

    /// Check if we're in panic unwinding mode AND directly in the defer function
    /// (not in a nested call from the defer function).
    /// Per Go semantics, recover() only works when called directly from defer.
    /// Defer functions run at depth = target_depth + 1.
    /// Additionally, the defer must have been registered before the current panic started.
    #[inline]
    pub fn is_direct_defer_context(&self) -> bool {
        match self.unwinding.as_ref() {
            Some(state) if state.mode == UnwindingMode::Panic => {
                // Must be at defer execution depth
                if !state.at_defer_boundary(self.frames.len()) {
                    return false;
                }
                // Defer must have been registered before the current panic
                state
                    .panic_context
                    .is_some_and(|context| state.current_defer_generation < context.generation)
            }
            _ => false,
        }
    }

    /// Switch unwinding mode from Panic to Return after successful recover().
    /// This prevents nested calls within the defer function from triggering panic_unwind.
    pub fn switch_panic_to_return_mode(&mut self) {
        if let Some(state) = self.unwinding.as_mut() {
            if state.mode == UnwindingMode::Panic {
                state.switch_to_return_mode();
            }
        }
    }

    /// Get the effective generation for registering a new defer.
    /// During panic unwinding, returns the current_defer_generation so nested defers
    /// can recover the same panic as their parent defer.
    /// Outside panic unwinding, returns panic_generation (current value before any panic).
    #[inline]
    pub fn effective_defer_generation(&self) -> u64 {
        match self.unwinding.as_ref() {
            Some(state) if state.mode == UnwindingMode::Panic => state.current_defer_generation,
            _ => self.panic_generation,
        }
    }

    /// Get raw pointer to stack for fast access.
    #[inline(always)]
    pub fn stack_ptr(&mut self) -> *mut u64 {
        self.stack.as_mut_ptr()
    }

    /// Ensure stack has capacity for at least `required` slots.
    /// Grows by doubling if needed. Only call when sp might exceed capacity.
    #[inline]
    pub fn try_ensure_capacity(&mut self, required: usize) -> Result<(), FiberCapacityError> {
        let stack_limit = self
            .resource_limits
            .max_stack_slots_per_fiber
            .min(MAX_STACK_CAPACITY);
        if required > stack_limit {
            return Err(FiberCapacityError::StackSlots {
                required,
                limit: stack_limit,
            });
        }
        if required > self.stack.len() {
            let new_cap = self
                .stack
                .len()
                .max(MIN_STACK_SLOTS)
                .max(required)
                .next_power_of_two()
                .min(stack_limit);
            let additional_bytes = new_cap
                .saturating_sub(self.stack.len())
                .saturating_mul(core::mem::size_of::<u64>());
            if !self.storage_budget.try_charge(additional_bytes) {
                return Err(FiberCapacityError::HostStorage {
                    resource: "fiber stack",
                    requested_bytes: self
                        .storage_budget
                        .used_bytes()
                        .saturating_add(additional_bytes),
                    limit_bytes: self.storage_budget.limit_bytes(),
                });
            }
            if self
                .stack
                .try_reserve_exact(new_cap.saturating_sub(self.stack.len()))
                .is_err()
            {
                self.storage_budget.release(additional_bytes);
                return Err(FiberCapacityError::HostAllocation {
                    resource: "fiber stack",
                });
            }
            self.stack.resize(new_cap, 0);
            self.accounted_storage_bytes = self
                .accounted_storage_bytes
                .saturating_add(additional_bytes);
        }
        Ok(())
    }

    #[inline]
    pub fn ensure_capacity(&mut self, required: usize) {
        self.try_ensure_capacity(required)
            .unwrap_or_else(|err| panic!("{}", err.message()));
    }

    #[inline]
    pub fn try_reserve_slots_at(
        &mut self,
        bp: usize,
        slot_count: usize,
    ) -> Result<usize, FiberCapacityError> {
        let new_sp = bp
            .checked_add(slot_count)
            .ok_or(FiberCapacityError::StackSlots {
                required: usize::MAX,
                limit: MAX_STACK_CAPACITY,
            })?;
        self.try_ensure_capacity(new_sp)?;
        self.sp = new_sp;
        Ok(new_sp)
    }

    #[inline]
    pub(crate) fn try_reserve_call_window(
        &mut self,
        bp: usize,
        slot_count: usize,
    ) -> Result<ReservedCallWindow, FiberCapacityError> {
        let sp = bp
            .checked_add(slot_count)
            .ok_or(FiberCapacityError::StackSlots {
                required: usize::MAX,
                limit: MAX_STACK_CAPACITY,
            })?;
        if self.frames.len() < self.frames.capacity() && sp <= self.stack.len() {
            self.sp = sp;
            return Ok(ReservedCallWindow { bp, sp });
        }
        self.try_reserve_call_window_slow(bp, slot_count)
    }

    #[cold]
    fn try_reserve_call_window_slow(
        &mut self,
        bp: usize,
        slot_count: usize,
    ) -> Result<ReservedCallWindow, FiberCapacityError> {
        self.try_reserve_call_frame()?;
        let sp = self.try_reserve_slots_at(bp, slot_count)?;
        Ok(ReservedCallWindow { bp, sp })
    }

    #[inline]
    pub fn reserve_slots_at(&mut self, bp: usize, slot_count: usize) -> usize {
        self.try_reserve_slots_at(bp, slot_count)
            .unwrap_or_else(|err| panic!("{}", err.message()))
    }

    #[inline]
    pub fn try_reserve_call_frames(&mut self, additional: usize) -> Result<(), FiberCapacityError> {
        let required =
            self.frames
                .len()
                .checked_add(additional)
                .ok_or(FiberCapacityError::CallFrames {
                    required: usize::MAX,
                    limit: self.resource_limits.max_call_frames_per_fiber,
                })?;
        let frame_limit = self
            .resource_limits
            .max_call_frames_per_fiber
            .min(MAX_CALL_FRAMES);
        if required > frame_limit {
            return Err(FiberCapacityError::CallFrames {
                required,
                limit: frame_limit,
            });
        }
        if required > self.frames.capacity() {
            let new_cap = (if self.frames.capacity() == 0 {
                4
            } else {
                self.frames.capacity().saturating_mul(2)
            })
            .max(required)
            .min(frame_limit);
            let additional_frames = new_cap.saturating_sub(self.frames.capacity());
            let additional_bytes =
                additional_frames.saturating_mul(core::mem::size_of::<CallFrame>());
            if !self.storage_budget.try_charge(additional_bytes) {
                return Err(FiberCapacityError::HostStorage {
                    resource: "fiber call frames",
                    requested_bytes: self
                        .storage_budget
                        .used_bytes()
                        .saturating_add(additional_bytes),
                    limit_bytes: self.storage_budget.limit_bytes(),
                });
            }
            if self.frames.try_reserve_exact(additional_frames).is_err() {
                self.storage_budget.release(additional_bytes);
                return Err(FiberCapacityError::HostAllocation {
                    resource: "fiber call frames",
                });
            }
            self.accounted_storage_bytes = self
                .accounted_storage_bytes
                .saturating_add(additional_bytes);
        }
        Ok(())
    }

    #[inline]
    fn try_reserve_call_frame(&mut self) -> Result<(), FiberCapacityError> {
        self.try_reserve_call_frames(1)
    }

    #[inline]
    pub fn zero_slots_at(&mut self, bp: usize, slot_count: usize) {
        self.stack[bp..bp + slot_count].fill(0);
    }

    /// Establish valid zero values for root-shaped locals before an
    /// interpreted frame becomes observable. Parameter cells already contain
    /// the caller-provided arguments and are intentionally preserved.
    #[inline]
    pub fn zero_frame_root_locals_at(
        &mut self,
        bp: usize,
        param_slots: u16,
        roots: vo_common_core::FrameRootSet<'_>,
    ) {
        let param_slots = usize::from(param_slots);
        for &slot in roots.direct {
            let slot = usize::from(slot);
            if slot >= param_slots {
                self.stack[bp + slot] = 0;
            }
        }
        for &header in roots.conditional {
            let header = usize::from(header);
            if header >= param_slots {
                self.stack[bp + header] = 0;
                self.stack[bp + header + 1] = 0;
            }
        }
    }

    /// Test and defensive-runtime fallback when verified loaded facts are not
    /// available. Production interpreter dispatch uses the precomputed root
    /// set above, keeping scalar-only calls free of frame initialization work.
    #[inline]
    pub fn zero_function_root_locals_at(
        &mut self,
        bp: usize,
        func: &vo_runtime::bytecode::FunctionDef,
    ) {
        for (slot, ty) in func
            .slot_types
            .iter()
            .copied()
            .enumerate()
            .skip(usize::from(func.param_slots))
        {
            if matches!(
                ty,
                vo_runtime::SlotType::GcRef
                    | vo_runtime::SlotType::Interface0
                    | vo_runtime::SlotType::Interface1
            ) {
                self.stack[bp + slot] = 0;
            }
        }
    }

    #[inline]
    pub fn copy_stack_slots(&mut self, dst: usize, src: usize, slot_count: usize) {
        if slot_count > 0 {
            self.stack.copy_within(src..src + slot_count, dst);
        }
    }

    #[inline]
    pub fn copy_slots_from_slice(&mut self, dst: usize, values: &[u64]) {
        if !values.is_empty() {
            self.stack[dst..dst + values.len()].copy_from_slice(values);
        }
    }

    pub fn push_call_frame(&mut self, func_id: u32, bp: usize, ret_reg: u16, ret_count: u16) {
        self.push_call_frame_extended(func_id, bp, bp, ret_reg, ret_count);
    }

    pub fn try_push_call_frame(
        &mut self,
        func_id: u32,
        bp: usize,
        ret_reg: u16,
        ret_count: u16,
    ) -> Result<(), FiberCapacityError> {
        self.try_push_call_frame_extended(func_id, bp, bp, ret_reg, ret_count)
    }

    pub fn push_call_frame_extended(
        &mut self,
        func_id: u32,
        bp: usize,
        sp_restore: usize,
        ret_reg: u16,
        ret_count: u16,
    ) {
        self.try_push_call_frame_extended(func_id, bp, sp_restore, ret_reg, ret_count)
            .unwrap_or_else(|err| panic!("{}", err.message()));
    }

    pub fn try_push_call_frame_extended(
        &mut self,
        func_id: u32,
        bp: usize,
        sp_restore: usize,
        ret_reg: u16,
        ret_count: u16,
    ) -> Result<(), FiberCapacityError> {
        self.try_reserve_call_frame()?;
        self.push_reserved_call_frame_extended(func_id, bp, sp_restore, ret_reg, ret_count);
        Ok(())
    }

    /// Publish a frame after its single capacity admission has succeeded.
    #[inline]
    fn push_reserved_call_frame_extended(
        &mut self,
        func_id: u32,
        bp: usize,
        sp_restore: usize,
        ret_reg: u16,
        ret_count: u16,
    ) {
        debug_assert!(self.frames.len() < self.frames.capacity());
        self.frames
            .push(CallFrame::new(func_id, bp, sp_restore, ret_reg, ret_count));
    }

    /// Commit a frame using capacity owned by `reservation`.
    #[inline]
    pub(crate) fn commit_reserved_call_frame(
        &mut self,
        reservation: ReservedCallWindow,
        func_id: u32,
        sp_restore: usize,
        ret_reg: u16,
        ret_count: u16,
    ) {
        debug_assert_eq!(self.sp, reservation.sp);
        self.push_reserved_call_frame_extended(
            func_id,
            reservation.bp,
            sp_restore,
            ret_reg,
            ret_count,
        );
    }

    pub fn push_borrowed_call_frame(
        &mut self,
        func_id: u32,
        borrowed_start: u16,
        ret_reg: u16,
        ret_count: u16,
        local_slots: u16,
    ) -> usize {
        self.try_push_borrowed_call_frame(func_id, borrowed_start, ret_reg, ret_count, local_slots)
            .unwrap_or_else(|err| panic!("{}", err.message()))
    }

    pub fn try_push_borrowed_call_frame(
        &mut self,
        func_id: u32,
        borrowed_start: u16,
        ret_reg: u16,
        ret_count: u16,
        local_slots: u16,
    ) -> Result<usize, FiberCapacityError> {
        let caller_frame = self
            .frames
            .last()
            .expect("push_borrowed_call_frame: missing caller frame");
        let caller_bp = caller_frame.bp;
        let caller_sp = self.sp;

        let bp = caller_bp + borrowed_start as usize;
        let reservation = self.try_reserve_call_window(bp, local_slots as usize)?;
        self.commit_reserved_call_frame(reservation, func_id, caller_sp, ret_reg, ret_count);
        Ok(bp)
    }

    pub fn push_frame(
        &mut self,
        func_id: u32,
        local_slots: u16,
        ret_reg: u16,
        ret_count: u16,
    ) -> usize {
        self.try_push_frame(func_id, local_slots, ret_reg, ret_count)
            .unwrap_or_else(|err| panic!("{}", err.message()))
    }

    pub fn try_push_frame(
        &mut self,
        func_id: u32,
        local_slots: u16,
        ret_reg: u16,
        ret_count: u16,
    ) -> Result<usize, FiberCapacityError> {
        let bp = self.sp;
        let reservation = self.try_reserve_call_window(bp, local_slots as usize)?;
        self.commit_reserved_call_frame(reservation, func_id, bp, ret_reg, ret_count);
        Ok(bp)
    }

    pub fn pop_frame(&mut self) -> Option<CallFrame> {
        if let Some(frame) = self.frames.pop() {
            self.sp = frame.sp_restore;
            Some(frame)
        } else {
            None
        }
    }

    /// Pop the active frame and copy its ordinary stack return into the caller.
    ///
    /// The common verifier has already proved both source and destination
    /// windows. Callers must establish that no defer, replay, unwind, or heap
    /// return protocol applies before using this transition.
    #[inline]
    pub(crate) fn complete_verified_stack_return(
        &mut self,
        ret_start: u16,
        ret_count: u16,
    ) -> CompletedStackReturn {
        let Some(frame) = self.pop_frame() else {
            return CompletedStackReturn::Done;
        };
        let write_count = usize::from(frame.ret_count.min(ret_count));
        let src = frame.bp + usize::from(ret_start);

        let Some(caller) = self.frames.last().copied() else {
            self.copy_stack_slots(0, src, write_count);
            self.sp = write_count;
            return CompletedStackReturn::Done;
        };

        let dst = caller.bp + usize::from(frame.ret_reg);
        debug_assert!(dst + write_count <= self.stack.len());
        self.copy_stack_slots(dst, src, write_count);
        CompletedStackReturn::Resume(caller)
    }

    /// Whether the active return can bypass every extended return protocol.
    #[inline]
    pub(crate) fn can_complete_verified_stack_return(
        &self,
        func_has_defer: bool,
        has_heap_returns: bool,
    ) -> bool {
        !func_has_defer
            && !has_heap_returns
            && self.unwinding.is_none()
            && !self.closure_replay.at_replay_boundary(self.frames.len())
    }

    /// Take a recoverable panic while preserving typed runtime trap metadata.
    pub fn take_recoverable_panic_with_kind(
        &mut self,
    ) -> Option<(Option<RuntimeTrapKind>, InterfaceSlot)> {
        match self.panic_state.take() {
            Some(PanicState::Recoverable(val)) => {
                let kind = self.panic_trap_kind.take();
                self.panic_source_loc = None;
                self.active_panic_generation = None;
                Some((kind, val))
            }
            other => {
                self.panic_state = other;
                None
            }
        }
    }

    /// Capture the current frame as the source location for a new panic.
    /// Call this before any frame unwinding begins. Uses pc-1 because the VM loop
    /// increments pc before dispatching each instruction.
    #[inline]
    pub fn capture_panic_source_loc(&mut self) {
        self.panic_source_loc = self
            .frames
            .last()
            .map(|f| (f.func_id, f.pc.saturating_sub(1) as u32));
    }

    #[inline]
    pub fn current_frame(&self) -> Option<&CallFrame> {
        self.frames.last()
    }

    #[inline]
    pub fn current_frame_mut(&mut self) -> Option<&mut CallFrame> {
        self.frames.last_mut()
    }

    #[inline]
    pub fn read_reg(&self, reg: u16) -> u64 {
        let frame = self.frames.last().expect("no active frame");
        self.stack[frame.bp + reg as usize]
    }

    #[inline]
    pub fn write_reg(&mut self, reg: u16, val: u64) {
        let frame = self.frames.last().expect("no active frame");
        self.stack[frame.bp + reg as usize] = val;
    }

    #[inline]
    pub fn read_reg_abs(&self, idx: usize) -> u64 {
        self.stack[idx]
    }

    #[inline]
    pub fn write_reg_abs(&mut self, idx: usize, val: u64) {
        self.stack[idx] = val;
    }
}

impl Drop for Fiber {
    fn drop(&mut self) {
        if self.accounted_storage_bytes != 0 {
            self.storage_budget.release(self.accounted_storage_bytes);
            self.accounted_storage_bytes = 0;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        DeferArgLayout, Fiber, FiberCapacityError, FiberIdentityExhausted, FiberState, PanicState,
        PendingSpawn, UnwindingMode, UnwindingStack, UnwindingState, MAX_CALL_FRAMES,
        MAX_RETAINED_CALL_FRAMES, MAX_RETAINED_STACK_SLOTS, MAX_STACK_CAPACITY, MIN_STACK_SLOTS,
    };
    use crate::test_support::queue as test_queue;
    use vo_runtime::island::{EndpointResponseKind, EndpointWaitKey};
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{InterfaceSlot, RuntimeType, SlotType, ValueKind, ValueMeta, ValueRttid};

    fn unwind_state(
        target_depth: usize,
        mode: UnwindingMode,
        resume_parent_after_recovery: bool,
    ) -> UnwindingState {
        UnwindingState {
            pending: Vec::new(),
            target_depth,
            mode,
            current_defer_generation: 0,
            panic_context: None,
            return_values: None,
            return_func_id: 0,
            return_pc: 0,
            caller_ret_reg: 0,
            caller_ret_count: 0,
            resume_parent_after_recovery,
            is_closure_replay: false,
        }
    }

    #[test]
    fn unwind_stack_allows_only_the_explicit_same_depth_defer_panic_scope() {
        let mut stack = UnwindingStack::default();
        assert!(stack.is_empty());
        stack
            .try_push(unwind_state(3, UnwindingMode::Panic, false))
            .unwrap();
        stack
            .try_push(unwind_state(3, UnwindingMode::Panic, true))
            .unwrap();
        assert_eq!(stack.len(), 2);
        assert!(!stack.is_empty());
    }

    #[test]
    fn unwind_stack_rejects_unrelated_same_depth_or_backward_states() {
        for child in [
            unwind_state(3, UnwindingMode::Return, false),
            unwind_state(3, UnwindingMode::Panic, false),
            unwind_state(2, UnwindingMode::Panic, true),
            unwind_state(4, UnwindingMode::Panic, true),
        ] {
            let mut stack = UnwindingStack::default();
            stack
                .try_push(unwind_state(3, UnwindingMode::Panic, false))
                .unwrap();
            assert!(stack.try_push(child).is_err());
            assert_eq!(stack.len(), 1);
        }

        let mut stack = UnwindingStack::default();
        assert!(stack
            .try_push(unwind_state(3, UnwindingMode::Panic, true))
            .is_err());
        assert!(stack.is_none());
    }

    #[test]
    fn fresh_fiber_stack_is_lazy() {
        let fiber = Fiber::new(1);

        assert_eq!(fiber.stack.len(), 0);
        assert_eq!(fiber.stack.capacity(), 0);
    }

    #[test]
    fn ensure_capacity_uses_small_power_of_two_floor() {
        let mut fiber = Fiber::new(1);

        fiber.ensure_capacity(1);
        assert_eq!(fiber.stack.len(), MIN_STACK_SLOTS);

        fiber.ensure_capacity(MIN_STACK_SLOTS + 1);
        assert_eq!(fiber.stack.len(), MIN_STACK_SLOTS * 2);
    }

    #[test]
    fn release_keeps_bounded_execution_storage_for_reuse() {
        let mut fiber = Fiber::new(1);
        fiber.generation = 7;
        fiber.ensure_capacity(MAX_RETAINED_STACK_SLOTS);
        for _ in 0..MAX_RETAINED_CALL_FRAMES {
            fiber.try_push_call_frame_extended(0, 0, 0, 0, 0).unwrap();
        }
        let stack_ptr = fiber.stack.as_ptr();
        let stack_capacity = fiber.stack.capacity();
        let frames_ptr = fiber.frames.as_ptr();
        let frames_capacity = fiber.frames.capacity();

        fiber.state = FiberState::Dead;
        fiber.release_oversized_storage();

        assert!(fiber.state.is_dead());
        assert_eq!((fiber.id, fiber.generation), (1, 7));
        assert_eq!(
            (fiber.stack.as_ptr(), fiber.stack.capacity()),
            (stack_ptr, stack_capacity)
        );
        assert_eq!(
            (fiber.frames.as_ptr(), fiber.frames.capacity()),
            (frames_ptr, frames_capacity)
        );
    }

    #[test]
    fn release_drops_oversized_stack_or_call_frames() {
        let mut stack_heavy = Fiber::new(2);
        stack_heavy.generation = u32::MAX;
        stack_heavy.ensure_capacity(MAX_RETAINED_STACK_SLOTS + 1);
        stack_heavy.state = FiberState::Dead;
        stack_heavy.release_oversized_storage();
        assert!(stack_heavy.state.is_dead());
        assert_eq!((stack_heavy.id, stack_heavy.generation), (2, u32::MAX));
        assert_eq!(
            (stack_heavy.stack.len(), stack_heavy.stack.capacity()),
            (0, 0)
        );

        let mut frame_heavy = Fiber::new(3);
        for _ in 0..=MAX_RETAINED_CALL_FRAMES {
            frame_heavy
                .try_push_call_frame_extended(0, 0, 0, 0, 0)
                .unwrap();
        }
        frame_heavy.state = FiberState::Dead;
        frame_heavy.release_oversized_storage();
        assert!(frame_heavy.state.is_dead());
        assert_eq!(
            (frame_heavy.frames.len(), frame_heavy.frames.capacity()),
            (0, 0)
        );
    }

    #[test]
    fn try_ensure_capacity_rejects_stack_overflow() {
        let mut fiber = Fiber::new(1);

        assert_eq!(
            fiber.try_ensure_capacity(MAX_STACK_CAPACITY + 1),
            Err(FiberCapacityError::StackSlots {
                required: MAX_STACK_CAPACITY + 1,
                limit: MAX_STACK_CAPACITY,
            })
        );
    }

    #[test]
    fn pending_spawn_rejects_entry_prefix_past_frame_capacity() {
        assert!(matches!(
            PendingSpawn::try_new(0, 1, 0, vec![11, 22]),
            Err(FiberCapacityError::StackSlots {
                required: 2,
                limit: 1,
            })
        ));
    }

    #[test]
    fn try_push_call_frame_rejects_call_frame_overflow() {
        let mut fiber = Fiber::new(1);

        for _ in 0..MAX_CALL_FRAMES {
            fiber.try_push_call_frame_extended(0, 0, 0, 0, 0).unwrap();
        }

        assert_eq!(
            fiber.try_push_call_frame_extended(0, 0, 0, 0, 0),
            Err(FiberCapacityError::CallFrames {
                required: MAX_CALL_FRAMES + 1,
                limit: MAX_CALL_FRAMES,
            })
        );
    }

    #[test]
    fn endpoint_response_replay_is_bound_to_a_specific_wait_turn() {
        let mut fiber = Fiber::new(1);

        let first_wait_key = fiber.begin_remote_endpoint_send_wait(42);
        assert!(fiber.apply_endpoint_response(
            42,
            EndpointResponseKind::SendAck {
                closed: false,
                wait_key: first_wait_key,
            },
        ));

        let second_wait_key = fiber.begin_remote_endpoint_send_wait(42);
        assert!(
            !fiber.apply_endpoint_response(
                42,
                EndpointResponseKind::SendAck {
                    closed: false,
                    wait_key: first_wait_key,
                },
            ),
            "a response accepted for one wait turn must not be accepted again for the next wait"
        );
        assert!(fiber.apply_endpoint_response(
            42,
            EndpointResponseKind::SendAck {
                closed: false,
                wait_key: second_wait_key,
            },
        ));
    }

    #[test]
    fn per_fiber_identity_spaces_exhaust_without_aliasing() {
        let mut fiber = Fiber::new(1);

        fiber.next_select_id = Some(u64::MAX);
        assert_eq!(fiber.try_alloc_select_id(), Ok(u64::MAX));
        assert_eq!(
            fiber.try_alloc_select_id(),
            Err(FiberIdentityExhausted::Select)
        );

        fiber.next_remote_endpoint_wait_id = Some(u64::MAX);
        let expected_wait_key =
            EndpointWaitKey::try_new(fiber.endpoint_response_key(), u64::MAX).unwrap();
        assert_eq!(
            fiber.try_begin_remote_endpoint_send_wait(42),
            Ok(expected_wait_key)
        );
        let established_wait = fiber.remote_endpoint_wait;
        assert_eq!(
            fiber.try_begin_remote_endpoint_recv_wait(43),
            Err(FiberIdentityExhausted::RemoteEndpointWait)
        );
        assert_eq!(fiber.remote_endpoint_wait, established_wait);
    }

    #[test]
    fn remote_send_identity_exhaustion_restores_committed_endpoint_transfer() {
        const TARGET_ISLAND: u32 = 7;

        let mut vm = crate::vm::Vm::new();
        vm.state.external_island_transport = true;
        let runtime_types = [
            RuntimeType::Port {
                dir: vo_common_core::ChanDir::Both,
                elem: ValueRttid::new(1, ValueKind::Int64),
            },
            RuntimeType::Basic(ValueKind::Int64),
        ];
        let port = test_queue::create(
            &mut vm.state.gc,
            QueueKind::Port,
            ValueMeta::new(0, ValueKind::Int64),
            ValueRttid::new(1, ValueKind::Int64),
            1,
            0,
        );
        assert!(test_queue::home_info(port).is_none());
        assert!(!vm.state.endpoint_registry.has_live());

        let mut island_effects = Vec::new();
        let transfer_commit = crate::exec::prepare_value_queue_handles_for_transfer_with_commit(
            &[port as u64],
            ValueMeta::new(0, ValueKind::Port),
            TARGET_ISLAND,
            &[],
            &[],
            &runtime_types,
            &mut vm.state,
            &mut island_effects,
        )
        .expect("local port transfer must commit endpoint state");
        assert!(transfer_commit.requires_terminal_commit());
        assert!(island_effects.is_empty());
        let endpoint_id = {
            let home_info = test_queue::home_info(port).expect("committed HomeInfo");
            assert!(home_info.peers.contains(&TARGET_ISLAND));
            home_info.endpoint_id
        };
        assert_eq!(vm.state.endpoint_registry.get_live(endpoint_id), Some(port));

        vm.state.gc_roots_dirty_all = false;
        vm.state.clear_gc_dirty_fibers();
        let mut fiber = Fiber::new(1);
        fiber.next_remote_endpoint_wait_id = None;
        let error = match crate::vm::prepare_queue_action(
            &mut vm.state,
            &mut fiber,
            crate::exec::QueueAction::RemoteSend {
                endpoint_id: 42,
                home_island: TARGET_ISLAND,
                data: vec![1, 2, 3],
                island_effects,
                transfer_commit,
            },
        ) {
            Err(error) => error,
            Ok(_) => panic!("exhausted remote wait identity must reject queue preparation"),
        };

        assert!(
            error.contains("remote endpoint wait identity space exhausted"),
            "{error}"
        );
        assert!(fiber.remote_endpoint_wait.is_none());
        assert!(test_queue::home_info(port).is_none());
        assert_eq!(vm.state.endpoint_registry.get_live(endpoint_id), None);
        assert!(!vm.state.endpoint_registry.has_live());
        assert!(vm.state.gc_roots_dirty_all);
    }

    #[test]
    fn resetting_a_reused_fiber_reopens_identity_spaces_under_new_generation() {
        let mut fiber = Fiber::new(1);
        fiber.next_select_id = None;
        fiber.next_remote_endpoint_wait_id = None;

        fiber.reset();

        assert_eq!(fiber.try_alloc_select_id(), Ok(0));
        let expected_wait_key = EndpointWaitKey::try_new(fiber.endpoint_response_key(), 1).unwrap();
        assert_eq!(
            fiber.try_begin_remote_endpoint_recv_wait(7),
            Ok(expected_wait_key)
        );
    }

    #[test]
    fn panic_generation_exhaustion_escalates_without_wrapping_recovery_identity() {
        let mut fiber = Fiber::new(1);
        fiber.panic_generation = u64::MAX;

        fiber.set_recoverable_panic(InterfaceSlot::nil());

        assert_eq!(fiber.panic_generation, u64::MAX);
        assert!(matches!(fiber.panic_state, Some(PanicState::Fatal)));
        assert_eq!(fiber.active_panic_generation, None);
    }

    #[test]
    fn vm_panic_recover_loc_001_recover_clears_consumed_panic_source_loc() {
        let mut fiber = Fiber::new(1);
        fiber.push_frame(7, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 12;
        fiber.set_recoverable_panic(InterfaceSlot::nil());
        fiber.capture_panic_source_loc();
        assert_eq!(fiber.panic_source_loc, Some((7, 11)));

        assert!(fiber.take_recoverable_panic().is_some());
        assert!(
            fiber.panic_source_loc.is_none(),
            "recover must clear the consumed panic source location"
        );

        let frame = fiber.current_frame_mut().unwrap();
        frame.func_id = 9;
        frame.pc = 21;
        fiber.set_recoverable_panic(InterfaceSlot::nil());
        fiber.capture_panic_source_loc();

        assert_eq!(
            fiber.panic_source_loc,
            Some((9, 20)),
            "a later independent panic must report its own source location"
        );
    }

    #[test]
    fn closure_replay_snapshot_keeps_fiber_owned_typed_log() {
        let mut replay = super::ClosureReplayState::new();
        let (empty, _) = replay.snapshot_for_extern(1);
        assert!(empty.is_empty());

        replay.results.push((vec![11], vec![SlotType::GcRef]));

        let (first, panic) = replay.snapshot_for_extern(1);
        assert!(panic.is_none());
        assert_eq!(first.len(), 1);
        assert_eq!(first[0].values, vec![11]);
        assert_eq!(first[0].slot_types, vec![SlotType::GcRef]);
        assert_eq!(replay.results.len(), 1);

        replay.results.push((vec![22], vec![SlotType::Value]));
        let (second, _) = replay.snapshot_for_extern(1);
        assert_eq!(second.len(), 2);
        assert_eq!(second[0].values, vec![11]);
        assert_eq!(second[1].values, vec![22]);
        assert_eq!(replay.results.len(), 2);
    }

    #[test]
    fn nested_extern_replay_scope_discards_inner_results_only() {
        let mut replay = super::ClosureReplayState::new();

        let (outer_empty, _) = replay.snapshot_for_extern(1);
        assert!(outer_empty.is_empty());

        replay.results.push((vec![11], vec![SlotType::Value]));
        let (outer_first, _) = replay.snapshot_for_extern(1);
        assert_eq!(outer_first.len(), 1);
        assert_eq!(outer_first[0].values, vec![11]);

        let (inner_empty, _) = replay.snapshot_for_extern(2);
        assert!(inner_empty.is_empty());

        replay.results.push((vec![99], vec![SlotType::Value]));
        let (inner_replay, _) = replay.snapshot_for_extern(2);
        assert_eq!(inner_replay.len(), 1);
        assert_eq!(inner_replay[0].values, vec![99]);

        replay.finish_extern_terminal();
        assert_eq!(replay.results.len(), 1);
        assert_eq!(replay.results[0].0, vec![11]);

        replay.results.push((vec![22], vec![SlotType::GcRef]));
        let (outer_second, _) = replay.snapshot_for_extern(1);
        assert_eq!(outer_second.len(), 2);
        assert_eq!(outer_second[0].values, vec![11]);
        assert_eq!(outer_second[1].values, vec![22]);

        replay.finish_extern_terminal();
        assert!(replay.results.is_empty());
        assert!(replay.extern_scope.is_none());
    }

    #[test]
    fn closure_replay_boundaries_keep_depth_and_pc_atomic() {
        let mut replay = super::ClosureReplayState::new();
        replay.push_boundary(2, 7);
        replay.push_boundary(5, 19);

        assert_eq!(
            replay.active_boundary(),
            Some(super::ClosureReplayBoundary {
                frame_depth: 5,
                replay_pc: 19,
            })
        );
        assert_eq!(
            replay.pop_boundary(),
            Some(super::ClosureReplayBoundary {
                frame_depth: 5,
                replay_pc: 19,
            })
        );
        assert_eq!(
            replay.pop_boundary(),
            Some(super::ClosureReplayBoundary {
                frame_depth: 2,
                replay_pc: 7,
            })
        );
        assert!(replay.active_boundary().is_none());
    }

    #[cfg(feature = "jit")]
    #[test]
    fn vm_fiber_reset_clears_jit_extern_suspend_roots_059() {
        let mut fiber = Fiber::new(1);
        fiber.jit_extern_suspend = Some(super::JitExternSuspend::CallClosure {
            closure_ref: 0x1000 as vo_runtime::gc::GcRef,
            args: super::TypedSlotPayload::try_new(vec![0x2000], vec![SlotType::GcRef])
                .expect("typed payload"),
            replay_pc: 7,
        });

        fiber.reset();

        assert!(
            fiber.jit_extern_suspend.is_none(),
            "fiber reuse must not preserve stale GC-visible JIT extern suspend roots"
        );
    }

    #[test]
    fn failed_borrowed_call_frame_setup_is_transactional() {
        let mut fiber = Fiber::new(1);
        for _ in 0..MAX_CALL_FRAMES {
            fiber.try_push_call_frame_extended(0, 0, 4, 0, 0).unwrap();
        }
        fiber.sp = 4;
        fiber.stack.resize(8, 0);
        fiber.stack[1..4].copy_from_slice(&[11, 22, 33]);

        let old_sp = fiber.sp;
        let old_stack = fiber.stack.clone();
        let result = fiber.try_push_borrowed_call_frame(1, 1, 1, 0, 4);

        assert_eq!(
            result,
            Err(FiberCapacityError::CallFrames {
                required: MAX_CALL_FRAMES + 1,
                limit: MAX_CALL_FRAMES,
            })
        );
        assert_eq!(fiber.sp, old_sp);
        assert_eq!(fiber.stack, old_stack);
    }

    #[test]
    fn borrowed_call_frame_keeps_dead_local_contents_for_exact_root_scanning() {
        let mut fiber = Fiber::new(1);
        fiber.push_frame(0, 4, 0, 0);
        fiber.stack[1..3].copy_from_slice(&[0xfeed, 0xbeef]);

        let bp = fiber
            .try_push_borrowed_call_frame(1, 1, 1, 0, 2)
            .expect("borrowed frame");

        assert_eq!(bp, 1);
        assert_eq!(&fiber.stack[bp..bp + 2], &[0xfeed, 0xbeef]);
        assert_eq!(fiber.frames.len(), 2);
    }

    #[test]
    fn verified_stack_return_copies_results_and_restores_the_caller_window() {
        let mut fiber = Fiber::new(1);
        fiber.push_frame(7, 8, 0, 0);
        let caller_sp = fiber.sp;
        let callee_bp = fiber.push_borrowed_call_frame(9, 2, 4, 2, 4);
        fiber.stack[callee_bp..callee_bp + 2].copy_from_slice(&[41, 42]);

        let completed = fiber.complete_verified_stack_return(0, 2);

        let super::CompletedStackReturn::Resume(caller) = completed else {
            panic!("borrowed callee must resume its caller");
        };
        assert_eq!(caller.func_id, 7);
        assert_eq!(fiber.sp, caller_sp);
        assert_eq!(&fiber.stack[4..6], &[41, 42]);
        assert_eq!(fiber.frames.len(), 1);
    }

    #[test]
    fn verified_terminal_stack_return_publishes_entry_results() {
        let mut fiber = Fiber::new(1);
        fiber.push_frame(7, 4, 0, 2);
        fiber.stack[1..3].copy_from_slice(&[51, 52]);

        assert!(matches!(
            fiber.complete_verified_stack_return(1, 2),
            super::CompletedStackReturn::Done
        ));
        assert!(fiber.frames.is_empty());
        assert_eq!(fiber.sp, 2);
        assert_eq!(&fiber.stack[..2], &[51, 52]);
    }

    #[test]
    fn defer_arg_layout_rejects_missing_slot_metadata() {
        let err = DeferArgLayout::try_from_caller_slot_types(&[SlotType::Value], 7, 11, 1, 2)
            .expect_err("missing metadata must fail fast");

        assert!(err.contains("func_id=7"));
        assert!(err.contains("pc=11"));
        assert!(err.contains("slot range 1..3"));
        assert!(err.contains("actual slot_types=1"));
    }
}
