//! Fiber (coroutine) and related structures.

#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::ffi::HostEventReplaySource;
use vo_runtime::gc::GcRef;
#[cfg(feature = "std")]
use vo_runtime::io::IoToken;
use vo_runtime::objects::interface::InterfaceSlot;

use crate::vm::RuntimeTrapKind;

#[derive(Debug, Clone)]
pub struct RemoteRecvResponse {
    pub data: Vec<u8>,
    pub closed: bool,
    pub rejected: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RemoteEndpointWait {
    Send { endpoint_id: u64, wait_id: u64 },
    Recv { endpoint_id: u64, wait_id: u64 },
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
    pub scan_slots: u16,
    pub caller_scan_slots_restore: Option<u16>,
    pub caller_zero_start: u16,
    pub caller_zero_end: u16,
}

impl CallFrame {
    #[inline]
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        func_id: u32,
        bp: usize,
        sp_restore: usize,
        ret_reg: u16,
        ret_count: u16,
        scan_slots: u16,
        caller_scan_slots_restore: Option<u16>,
        caller_zero_start: u16,
        caller_zero_end: u16,
    ) -> Self {
        Self {
            func_id,
            pc: 0,
            bp,
            sp_restore,
            ret_reg,
            ret_count,
            scan_slots,
            caller_scan_slots_restore,
            caller_zero_start,
            caller_zero_end,
        }
    }
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
    /// Defers remaining to execute (LIFO order, first = next to run).
    pub pending: Vec<DeferEntry>,
    /// Frame depth after the unwinding function was popped.
    /// Defer functions run at depth = target_depth + 1.
    pub target_depth: usize,
    /// Unwinding mode: Return or Panic.
    pub mode: UnwindingMode,
    /// The generation of the currently executing defer.
    /// Used with Fiber.panic_generation to check if recover() should work.
    pub current_defer_generation: u64,
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
    pub elem_slots: u8,
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

impl PanicState {
    /// Extract human-readable message from panic value.
    pub fn message(&self) -> String {
        match self {
            PanicState::Fatal => "fatal error".to_string(),
            PanicState::Recoverable(val) => {
                if val.is_string() && !val.as_ref().is_null() {
                    return val.to_rust_string();
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
    /// Frame depth at which a closure-for-extern-replay was pushed.
    /// When a Return pops down to this depth, the return values are
    /// appended to `results` and the extern is replayed.
    /// 0 = no pending closure replay.
    pub depth: usize,
    /// Original panic message captured when the replayed closure unwound.
    /// Preserved so the replayed extern can report the true root cause.
    /// `is_some()` also serves as the "panicked" flag.
    pub panic_message: Option<String>,
    /// Stack of saved `depth` values for nested replay cycles.
    pub depth_stack: Vec<usize>,
    /// Active extern replay scope.
    pub extern_scope: Option<ClosureReplayExternScope>,
    /// Saved parent extern scopes for nested extern calls.
    pub extern_scope_stack: Vec<ClosureReplayExternScope>,
    /// Saved parent panic messages for nested extern calls.
    pub panic_message_stack: Vec<Option<String>>,
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
            depth: 0,
            panic_message: None,
            depth_stack: Vec::new(),
            extern_scope: None,
            extern_scope_stack: Vec::new(),
            panic_message_stack: Vec::new(),
        }
    }

    pub fn reset(&mut self) {
        self.results.clear();
        self.index = 0;
        self.depth = 0;
        self.panic_message = None;
        self.depth_stack.clear();
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

    /// Push a closure frame: save current depth, set new depth.
    pub fn push_depth(&mut self, frame_count: usize) {
        self.depth_stack.push(self.depth);
        self.depth = frame_count;
    }

    /// Pop closure frame depth after return or panic.
    pub fn pop_depth(&mut self) {
        self.depth = self.depth_stack.pop().unwrap_or(0);
    }

    /// Check if current frame is at the closure replay boundary.
    #[inline]
    pub fn at_replay_boundary(&self, frame_count: usize) -> bool {
        self.depth > 0 && frame_count == self.depth
    }

    /// Check if panic should be intercepted at closure replay boundary.
    #[inline]
    pub fn should_intercept_panic(&self, frame_count: usize) -> bool {
        self.depth > 0 && frame_count <= self.depth
    }
}

/// Initial stack capacity in slots (64KB = 8192 slots).
const INITIAL_STACK_CAPACITY: usize = 8192;
/// Maximum stack capacity per fiber in slots (8 MiB).
///
/// Without a VM-owned limit, runaway recursion keeps doubling the Rust Vec
/// until wasm memory allocation fails as a raw `memory access out of bounds`.
/// Keep the failure at the VM stack boundary so the reported error points at
/// the actual execution problem.
pub const MAX_STACK_CAPACITY: usize = 1 << 20;
/// Maximum slot budget for JIT direct-call native stack chains.
///
/// JIT-to-JIT direct calls use native stack slots for locals before materializing
/// frames back to `fiber.stack` on side exits. Keep this limit lower than the VM
/// stack limit so large-frame recursion trips a Vo panic before host stack
/// exhaustion.
pub const MAX_JIT_NATIVE_STACK_SLOTS: usize = 1 << 15;
/// Maximum nested direct JIT call depth before converting recursion into a
/// recoverable Vo stack overflow.
pub const MAX_JIT_CALL_DEPTH: usize = 512;
/// Maximum call frames per fiber.
///
/// Small-frame recursion can keep reusing the same stack slots while growing
/// only `frames`. Without a VM-owned frame limit, wasm eventually reports a raw
/// `memory access out of bounds` from `RawVec::grow_one` in call-frame push.
const MAX_CALL_FRAMES: usize = 1 << 15;

const CALL_IFACE_IC_TABLE_SIZE: usize = 64;
const CALL_IFACE_IC_TABLE_MASK: u32 = (CALL_IFACE_IC_TABLE_SIZE - 1) as u32;

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

#[derive(Debug, Clone, Copy, Default)]
pub struct CallIfaceICEntry {
    pub caller_func_id: u32,
    pub callsite_pc: u32,
    pub itab_id: u32,
    pub method_idx: u8,
    pub valid: bool,
    pub local_slots: u16,
    pub gc_scan_slots: u16,
    pub func_id: u32,
}

impl CallIfaceICEntry {
    #[inline]
    pub fn matches(
        &self,
        caller_func_id: u32,
        callsite_pc: u32,
        itab_id: u32,
        method_idx: u8,
    ) -> bool {
        self.valid
            && self.caller_func_id == caller_func_id
            && self.callsite_pc == callsite_pc
            && self.itab_id == itab_id
            && self.method_idx == method_idx
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FiberCapacityError {
    StackSlots { required: usize, limit: usize },
    CallFrames { required: usize, limit: usize },
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
        }
    }
}

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
    pub defer_stack: Vec<DeferEntry>,
    pub unwinding: Option<UnwindingState>,
    pub queue_wait_state: Option<QueueWaitState>,
    pub select_state: Option<SelectState>,
    /// Counter for generating unique select IDs within this fiber.
    pub next_select_id: u64,
    pub panic_state: Option<PanicState>,
    pub panic_trap_kind: Option<RuntimeTrapKind>,
    /// Incremented each time a new panic starts. Used to determine which defers can recover.
    /// A defer registered at generation N can only recover panics with generation > N.
    pub panic_generation: u64,
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
    pub call_iface_ic_table: Vec<CallIfaceICEntry>,
    /// Closure callback suspend/replay state for extern functions.
    pub closure_replay: ClosureReplayState,
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
    next_remote_endpoint_wait_id: u64,
}

impl Fiber {
    pub fn new(id: u32) -> Self {
        Self {
            id,
            generation: 1,
            state: FiberState::Runnable,
            stack: Vec::with_capacity(INITIAL_STACK_CAPACITY),
            sp: 0,
            frames: Vec::new(),
            defer_stack: Vec::new(),
            unwinding: None,
            queue_wait_state: None,
            select_state: None,
            next_select_id: 0,
            panic_state: None,
            panic_trap_kind: None,
            panic_generation: 0,
            panic_source_loc: None,
            #[cfg(feature = "std")]
            resume_io_token: None,
            resume_host_event_token: None,
            resume_host_event_data: None,
            #[cfg(feature = "jit")]
            resume_stack: Vec::new(), // Lazy: only allocates on first push (Call/WaitIo)
            #[cfg(feature = "jit")]
            jit_extern_suspend: None,
            call_iface_ic_table: Vec::new(),
            closure_replay: ClosureReplayState::new(),
            #[cfg(feature = "jit")]
            jit_panic_flag: false,
            #[cfg(feature = "jit")]
            jit_is_user_panic: false,
            #[cfg(feature = "jit")]
            jit_panic_msg: InterfaceSlot::default(),
            #[cfg(feature = "jit")]
            jit_infra_error_message: String::new(),
            remote_recv_response: None,
            remote_send_closed: false,
            remote_endpoint_wait: None,
            next_remote_endpoint_wait_id: 1,
        }
    }

    pub fn take_remote_recv_response(&mut self) -> Option<RemoteRecvResponse> {
        self.remote_recv_response.take()
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
        self.queue_wait_state = waiter.kind.map(|kind| QueueWaitState {
            queue_ref: waiter.queue_ref as GcRef,
            kind,
            registration_id: waiter.registration_id,
        });
    }

    pub fn clear_queue_wait(&mut self) {
        self.queue_wait_state = None;
    }

    pub fn queue_wait_matches(
        &self,
        waiter: &vo_runtime::objects::queue_state::QueueWaiter,
    ) -> bool {
        match (self.queue_wait_state, waiter.kind) {
            (Some(state), Some(kind)) => {
                state.queue_ref as u64 == waiter.queue_ref
                    && state.kind == kind
                    && state.registration_id != 0
                    && state.registration_id == waiter.registration_id
            }
            (None, None) => true,
            _ => false,
        }
    }

    fn alloc_remote_endpoint_wait_id(&mut self) -> u64 {
        let wait_id = self.next_remote_endpoint_wait_id.max(1);
        self.next_remote_endpoint_wait_id = wait_id.wrapping_add(1).max(1);
        wait_id
    }

    pub fn begin_remote_endpoint_send_wait(&mut self, endpoint_id: u64) -> u64 {
        let wait_id = self.alloc_remote_endpoint_wait_id();
        self.remote_endpoint_wait = Some(RemoteEndpointWait::Send {
            endpoint_id,
            wait_id,
        });
        wait_id
    }

    pub fn begin_remote_endpoint_recv_wait(&mut self, endpoint_id: u64) -> u64 {
        let wait_id = self.alloc_remote_endpoint_wait_id();
        self.remote_endpoint_wait = Some(RemoteEndpointWait::Recv {
            endpoint_id,
            wait_id,
        });
        wait_id
    }

    pub fn apply_endpoint_response(
        &mut self,
        endpoint_id: u64,
        wait_id: u64,
        kind: &vo_runtime::island::EndpointResponseKind,
    ) -> bool {
        match (self.remote_endpoint_wait, kind) {
            (
                Some(RemoteEndpointWait::Send {
                    endpoint_id: expected,
                    wait_id: expected_wait_id,
                }),
                vo_runtime::island::EndpointResponseKind::SendAck { closed },
            ) if expected == endpoint_id && expected_wait_id == wait_id => {
                if *closed {
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
                vo_runtime::island::EndpointResponseKind::RecvData { data, closed },
            ) if expected == endpoint_id && expected_wait_id == wait_id => {
                self.remote_recv_response = Some(RemoteRecvResponse {
                    data: data.clone(),
                    closed: *closed,
                    rejected: false,
                });
                self.remote_endpoint_wait = None;
                true
            }
            (
                Some(RemoteEndpointWait::Recv {
                    endpoint_id: expected,
                    wait_id: expected_wait_id,
                }),
                vo_runtime::island::EndpointResponseKind::RecvError,
            ) if expected == endpoint_id && expected_wait_id == wait_id => {
                self.remote_recv_response = Some(RemoteRecvResponse {
                    data: Vec::new(),
                    closed: false,
                    rejected: true,
                });
                self.remote_endpoint_wait = None;
                true
            }
            _ => false,
        }
    }

    pub fn can_apply_endpoint_response(
        &self,
        endpoint_id: u64,
        wait_id: u64,
        kind: &vo_runtime::island::EndpointResponseKind,
    ) -> bool {
        matches!(
            (self.remote_endpoint_wait, kind),
            (
                Some(RemoteEndpointWait::Send {
                    endpoint_id: expected,
                    wait_id: expected_wait_id,
                }),
                vo_runtime::island::EndpointResponseKind::SendAck { .. },
            ) if expected == endpoint_id && expected_wait_id == wait_id
        ) || matches!(
            (self.remote_endpoint_wait, kind),
            (
                Some(RemoteEndpointWait::Recv {
                    endpoint_id: expected,
                    wait_id: expected_wait_id,
                }),
                vo_runtime::island::EndpointResponseKind::RecvData { .. }
                    | vo_runtime::island::EndpointResponseKind::RecvError,
            ) if expected == endpoint_id && expected_wait_id == wait_id
        )
    }

    /// Reset fiber for reuse.
    pub fn reset(&mut self) {
        self.state = FiberState::Runnable;
        self.sp = 0;
        self.frames.clear();
        self.defer_stack.clear();
        self.unwinding = None;
        self.queue_wait_state = None;
        self.select_state = None;
        self.next_select_id = 0;
        self.panic_state = None;
        self.panic_trap_kind = None;
        self.panic_generation = 0;
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
        if !self.call_iface_ic_table.is_empty() {
            unsafe {
                core::ptr::write_bytes(
                    self.call_iface_ic_table.as_mut_ptr(),
                    0,
                    self.call_iface_ic_table.len(),
                );
            }
        }
        self.closure_replay.reset();
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
        self.next_remote_endpoint_wait_id = 1;
    }

    #[inline]
    pub fn call_iface_ic_index(caller_func_id: u32, callsite_pc: u32) -> usize {
        ((caller_func_id.wrapping_mul(97)).wrapping_add(callsite_pc) & CALL_IFACE_IC_TABLE_MASK)
            as usize
    }

    pub fn ensure_call_iface_ic_table(&mut self) -> &mut [CallIfaceICEntry] {
        if self.call_iface_ic_table.is_empty() {
            self.call_iface_ic_table = Vec::with_capacity(CALL_IFACE_IC_TABLE_SIZE);
            self.call_iface_ic_table
                .resize(CALL_IFACE_IC_TABLE_SIZE, CallIfaceICEntry::default());
        }
        self.call_iface_ic_table.as_mut_slice()
    }

    /// Check if current panic is recoverable and return the interface{} value if so.
    /// Used by recover() to consume the panic value.
    pub fn take_recoverable_panic(&mut self) -> Option<InterfaceSlot> {
        match self.panic_state.take() {
            Some(PanicState::Recoverable(val)) => {
                self.panic_trap_kind = None;
                self.panic_source_loc = None;
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
    }

    /// Set a recoverable panic with full interface{} value (InterfaceSlot).
    /// Also increments panic_generation so we can track which defers can recover.
    pub fn set_recoverable_panic(&mut self, msg: InterfaceSlot) {
        self.panic_generation += 1;
        self.panic_state = Some(PanicState::Recoverable(msg));
        self.panic_trap_kind = None;
    }

    /// Set a recoverable runtime trap (typed runtime panic).
    pub fn set_recoverable_trap(&mut self, kind: RuntimeTrapKind, msg: InterfaceSlot) {
        self.panic_generation += 1;
        self.panic_state = Some(PanicState::Recoverable(msg));
        self.panic_trap_kind = Some(kind);
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
        match &self.unwinding {
            Some(state) if state.mode == UnwindingMode::Panic => {
                // Must be at defer execution depth
                if !state.at_defer_boundary(self.frames.len()) {
                    return false;
                }
                // Defer must have been registered before the current panic
                state.current_defer_generation < self.panic_generation
            }
            _ => false,
        }
    }

    /// Switch unwinding mode from Panic to Return after successful recover().
    /// This prevents nested calls within the defer function from triggering panic_unwind.
    pub fn switch_panic_to_return_mode(&mut self) {
        if let Some(ref mut state) = self.unwinding {
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
        match &self.unwinding {
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
        if required > MAX_STACK_CAPACITY {
            return Err(FiberCapacityError::StackSlots {
                required,
                limit: MAX_STACK_CAPACITY,
            });
        }
        if required > self.stack.len() {
            let new_cap = self
                .stack
                .len()
                .max(INITIAL_STACK_CAPACITY)
                .max(required)
                .next_power_of_two()
                .min(MAX_STACK_CAPACITY);
            self.stack.resize(new_cap, 0);
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
    pub fn try_reserve_call_window(
        &mut self,
        bp: usize,
        slot_count: usize,
    ) -> Result<usize, FiberCapacityError> {
        self.try_reserve_call_frame()?;
        self.try_reserve_slots_at(bp, slot_count)
    }

    #[inline]
    pub fn reserve_slots_at(&mut self, bp: usize, slot_count: usize) -> usize {
        self.try_reserve_slots_at(bp, slot_count)
            .unwrap_or_else(|err| panic!("{}", err.message()))
    }

    #[inline]
    pub fn try_reserve_call_frames(&self, additional: usize) -> Result<(), FiberCapacityError> {
        let required =
            self.frames
                .len()
                .checked_add(additional)
                .ok_or(FiberCapacityError::CallFrames {
                    required: usize::MAX,
                    limit: MAX_CALL_FRAMES,
                })?;
        if required > MAX_CALL_FRAMES {
            return Err(FiberCapacityError::CallFrames {
                required,
                limit: MAX_CALL_FRAMES,
            });
        }
        Ok(())
    }

    #[inline]
    fn try_reserve_call_frame(&self) -> Result<(), FiberCapacityError> {
        self.try_reserve_call_frames(1)
    }

    #[inline]
    pub fn zero_slots_at(&mut self, bp: usize, slot_count: usize) {
        self.stack[bp..bp + slot_count].fill(0);
    }

    #[inline]
    pub fn zero_slots_tail_at(&mut self, bp: usize, slot_count: usize, initialized_prefix: usize) {
        let zero_start = initialized_prefix.min(slot_count);
        if zero_start < slot_count {
            self.zero_slots_at(bp + zero_start, slot_count - zero_start);
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

    pub fn push_call_frame(
        &mut self,
        func_id: u32,
        bp: usize,
        ret_reg: u16,
        ret_count: u16,
        scan_slots: u16,
    ) {
        self.push_call_frame_extended(func_id, bp, bp, ret_reg, ret_count, scan_slots, None, 0, 0);
    }

    pub fn try_push_call_frame(
        &mut self,
        func_id: u32,
        bp: usize,
        ret_reg: u16,
        ret_count: u16,
        scan_slots: u16,
    ) -> Result<(), FiberCapacityError> {
        self.try_push_call_frame_extended(
            func_id, bp, bp, ret_reg, ret_count, scan_slots, None, 0, 0,
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub fn push_call_frame_extended(
        &mut self,
        func_id: u32,
        bp: usize,
        sp_restore: usize,
        ret_reg: u16,
        ret_count: u16,
        scan_slots: u16,
        caller_scan_slots_restore: Option<u16>,
        caller_zero_start: u16,
        caller_zero_end: u16,
    ) {
        self.try_push_call_frame_extended(
            func_id,
            bp,
            sp_restore,
            ret_reg,
            ret_count,
            scan_slots,
            caller_scan_slots_restore,
            caller_zero_start,
            caller_zero_end,
        )
        .unwrap_or_else(|err| panic!("{}", err.message()));
    }

    #[allow(clippy::too_many_arguments)]
    pub fn try_push_call_frame_extended(
        &mut self,
        func_id: u32,
        bp: usize,
        sp_restore: usize,
        ret_reg: u16,
        ret_count: u16,
        scan_slots: u16,
        caller_scan_slots_restore: Option<u16>,
        caller_zero_start: u16,
        caller_zero_end: u16,
    ) -> Result<(), FiberCapacityError> {
        self.try_reserve_call_frame()?;
        self.frames.push(CallFrame::new(
            func_id,
            bp,
            sp_restore,
            ret_reg,
            ret_count,
            scan_slots,
            caller_scan_slots_restore,
            caller_zero_start,
            caller_zero_end,
        ));
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub fn push_borrowed_call_frame(
        &mut self,
        func_id: u32,
        borrowed_start: u16,
        ret_reg: u16,
        ret_count: u16,
        caller_scan_slots: u16,
        local_slots: u16,
        scan_slots: u16,
    ) -> usize {
        self.try_push_borrowed_call_frame(
            func_id,
            borrowed_start,
            ret_reg,
            ret_count,
            caller_scan_slots,
            local_slots,
            scan_slots,
        )
        .unwrap_or_else(|err| panic!("{}", err.message()))
    }

    #[allow(clippy::too_many_arguments)]
    pub fn try_push_borrowed_call_frame(
        &mut self,
        func_id: u32,
        borrowed_start: u16,
        ret_reg: u16,
        ret_count: u16,
        caller_scan_slots: u16,
        local_slots: u16,
        scan_slots: u16,
    ) -> Result<usize, FiberCapacityError> {
        if scan_slots > local_slots {
            return Err(FiberCapacityError::StackSlots {
                required: scan_slots as usize,
                limit: local_slots as usize,
            });
        }
        let caller_frame = self
            .frames
            .last()
            .expect("push_borrowed_call_frame: missing caller frame");
        let caller_bp = caller_frame.bp;
        let caller_sp = self.sp;
        let previous_scan_slots = caller_frame.scan_slots;
        let caller_scan_slots_restore = if previous_scan_slots != caller_scan_slots {
            Some(previous_scan_slots)
        } else {
            None
        };
        let (caller_zero_start, caller_zero_end) = if borrowed_start < previous_scan_slots {
            (borrowed_start, previous_scan_slots)
        } else {
            (0, 0)
        };

        let bp = caller_bp + borrowed_start as usize;
        self.try_reserve_call_frame()?;
        self.try_reserve_slots_at(bp, local_slots as usize)?;
        self.frames
            .last_mut()
            .expect("push_borrowed_call_frame: missing caller frame")
            .scan_slots = caller_scan_slots;
        self.try_push_call_frame_extended(
            func_id,
            bp,
            caller_sp,
            ret_reg,
            ret_count,
            scan_slots,
            caller_scan_slots_restore,
            caller_zero_start,
            caller_zero_end,
        )?;
        Ok(bp)
    }

    pub fn push_frame(
        &mut self,
        func_id: u32,
        local_slots: u16,
        scan_slots: u16,
        ret_reg: u16,
        ret_count: u16,
    ) -> usize {
        self.try_push_frame(func_id, local_slots, scan_slots, ret_reg, ret_count)
            .unwrap_or_else(|err| panic!("{}", err.message()))
    }

    pub fn try_push_frame(
        &mut self,
        func_id: u32,
        local_slots: u16,
        scan_slots: u16,
        ret_reg: u16,
        ret_count: u16,
    ) -> Result<usize, FiberCapacityError> {
        if scan_slots > local_slots {
            return Err(FiberCapacityError::StackSlots {
                required: scan_slots as usize,
                limit: local_slots as usize,
            });
        }
        let bp = self.sp;
        self.try_reserve_call_window(bp, local_slots as usize)?;
        // Zero the new frame's slots. ensure_capacity zeros newly-allocated memory, but
        // previously-used slots (from prior calls that shared this stack region) contain
        // stale values. GC root scanning uses slot_types to determine which slots hold
        // GcRefs — a stale integer in a GcRef-typed slot causes mark_gray to segfault.
        // This zero-fill is the canonical fix (same approach as JVM/CLR).
        // Safety: ensure_capacity guarantees stack[bp..new_sp] is valid.
        self.zero_slots_at(bp, scan_slots as usize);
        self.try_push_call_frame(func_id, bp, ret_reg, ret_count, scan_slots)?;
        Ok(bp)
    }

    pub fn pop_frame(&mut self) -> Option<CallFrame> {
        if let Some(frame) = self.frames.pop() {
            self.sp = frame.sp_restore;
            if let Some(scan_slots_restore) = frame.caller_scan_slots_restore {
                let parent_idx = self.frames.len().checked_sub(1);
                if let Some(parent_idx) = parent_idx {
                    self.frames[parent_idx].scan_slots = scan_slots_restore;
                }
            }
            Some(frame)
        } else {
            None
        }
    }

    /// Take a recoverable panic while preserving typed runtime trap metadata.
    pub fn take_recoverable_panic_with_kind(
        &mut self,
    ) -> Option<(Option<RuntimeTrapKind>, InterfaceSlot)> {
        match self.panic_state.take() {
            Some(PanicState::Recoverable(val)) => {
                let kind = self.panic_trap_kind.take();
                self.panic_source_loc = None;
                Some((kind, val))
            }
            other => {
                self.panic_state = other;
                None
            }
        }
    }

    #[inline]
    pub fn clear_parent_borrowed_slots(
        &mut self,
        frame: &CallFrame,
        preserved_start: usize,
        preserved_len: usize,
    ) {
        if frame.caller_scan_slots_restore.is_none() || self.frames.is_empty() {
            return;
        }

        let parent_bp = self.frames.last().unwrap().bp;
        let zero_start = frame.caller_zero_start as usize;
        let zero_end = frame.caller_zero_end as usize;
        if zero_start >= zero_end {
            return;
        }

        let keep_start = preserved_start.max(zero_start).min(zero_end);
        let keep_end = preserved_start
            .saturating_add(preserved_len)
            .max(keep_start)
            .min(zero_end);

        if zero_start < keep_start {
            self.zero_slots_at(parent_bp + zero_start, keep_start - zero_start);
        }
        if keep_end < zero_end {
            self.zero_slots_at(parent_bp + keep_end, zero_end - keep_end);
        }
    }

    /// Capture the current frame as the panic source location, if not already set.
    /// Call this before any frame unwinding begins. Uses pc-1 because the VM loop
    /// increments pc before dispatching each instruction.
    #[inline]
    pub fn capture_panic_source_loc(&mut self) {
        if self.panic_source_loc.is_none() {
            self.panic_source_loc = self
                .frames
                .last()
                .map(|f| (f.func_id, f.pc.saturating_sub(1) as u32));
        }
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

#[cfg(test)]
mod tests {
    use super::{
        DeferArgLayout, Fiber, FiberCapacityError, INITIAL_STACK_CAPACITY, MAX_CALL_FRAMES,
        MAX_STACK_CAPACITY,
    };
    use vo_runtime::island::EndpointResponseKind;
    use vo_runtime::InterfaceSlot;
    use vo_runtime::SlotType;

    #[test]
    fn ensure_capacity_grows_stack_within_limit() {
        let mut fiber = Fiber::new(1);

        fiber.ensure_capacity(INITIAL_STACK_CAPACITY + 1);

        assert!(fiber.stack.len() > INITIAL_STACK_CAPACITY);
        assert!(fiber.stack.len() <= MAX_STACK_CAPACITY);
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
    fn try_push_call_frame_rejects_call_frame_overflow() {
        let mut fiber = Fiber::new(1);

        for _ in 0..MAX_CALL_FRAMES {
            fiber
                .try_push_call_frame_extended(0, 0, 0, 0, 0, 0, None, 0, 0)
                .unwrap();
        }

        assert_eq!(
            fiber.try_push_call_frame_extended(0, 0, 0, 0, 0, 0, None, 0, 0),
            Err(FiberCapacityError::CallFrames {
                required: MAX_CALL_FRAMES + 1,
                limit: MAX_CALL_FRAMES,
            })
        );
    }

    #[test]
    fn endpoint_response_replay_is_not_bound_to_a_specific_wait_turn() {
        let mut fiber = Fiber::new(1);
        let response = EndpointResponseKind::SendAck { closed: false };

        let first_wait_id = fiber.begin_remote_endpoint_send_wait(42);
        assert!(fiber.apply_endpoint_response(42, first_wait_id, &response));

        let second_wait_id = fiber.begin_remote_endpoint_send_wait(42);
        assert!(
            !fiber.apply_endpoint_response(42, first_wait_id, &response),
            "a response accepted for one wait turn must not be accepted again for the next wait"
        );
        assert!(fiber.apply_endpoint_response(42, second_wait_id, &response));
    }

    #[test]
    fn vm_panic_recover_loc_001_recover_clears_consumed_panic_source_loc() {
        let mut fiber = Fiber::new(1);
        fiber.push_frame(7, 0, 0, 0, 0);
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
    fn failed_borrowed_call_frame_setup_leaves_stack_and_caller_scan_unchanged() {
        let mut fiber = Fiber::new(1);
        for _ in 0..MAX_CALL_FRAMES {
            fiber
                .try_push_call_frame_extended(0, 0, 4, 0, 0, 4, None, 0, 0)
                .unwrap();
        }
        fiber.sp = 4;
        fiber.stack.resize(8, 0);
        fiber.frames.last_mut().unwrap().scan_slots = 4;

        let old_sp = fiber.sp;
        let old_scan = fiber.frames.last().unwrap().scan_slots;
        let result = fiber.try_push_borrowed_call_frame(1, 1, 1, 0, 1, 4, 4);

        assert_eq!(
            result,
            Err(FiberCapacityError::CallFrames {
                required: MAX_CALL_FRAMES + 1,
                limit: MAX_CALL_FRAMES,
            })
        );
        assert_eq!(fiber.sp, old_sp);
        assert_eq!(fiber.frames.last().unwrap().scan_slots, old_scan);
    }

    #[test]
    fn borrowed_call_frame_rejects_scan_slots_beyond_locals_without_panic_062() {
        let mut fiber = Fiber::new(1);
        fiber.try_push_call_frame(0, 4, 1, 0, 0).unwrap();
        let old_frame_count = fiber.frames.len();
        let old_sp = fiber.sp;
        let old_scan = fiber.frames.last().unwrap().scan_slots;

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            fiber.try_push_borrowed_call_frame(1, 1, 1, 0, 1, 1, 2)
        }));

        let err = result
            .expect("borrowed frame scan/local drift must return an error, not panic")
            .expect_err("borrowed frame scan/local drift must be rejected");
        assert_eq!(
            err,
            FiberCapacityError::StackSlots {
                required: 2,
                limit: 1,
            }
        );
        assert_eq!(fiber.frames.len(), old_frame_count);
        assert_eq!(fiber.sp, old_sp);
        assert_eq!(fiber.frames.last().unwrap().scan_slots, old_scan);
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
