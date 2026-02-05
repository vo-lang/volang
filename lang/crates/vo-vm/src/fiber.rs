//! Fiber (coroutine) and related structures.

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::objects::interface::InterfaceSlot;
#[cfg(feature = "std")]
use vo_runtime::io::IoToken;

use vo_runtime::gc::GcRef;

use crate::vm::RuntimeTrapKind;

#[derive(Debug, Clone, Copy)]
pub struct CallFrame {
    pub func_id: u32,
    pub pc: usize,
    pub bp: usize,
    pub ret_reg: u16,
    pub ret_count: u16,
}

impl CallFrame {
    #[inline]
    pub fn new(func_id: u32, bp: usize, ret_reg: u16, ret_count: u16) -> Self {
        Self {
            func_id,
            pc: 0,
            bp,
            ret_reg,
            ret_count,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DeferEntry {
    pub frame_depth: usize,
    pub func_id: u32,
    pub closure: GcRef,
    pub args: GcRef,
    pub arg_slots: u16,
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
    /// Where to write return values in caller's frame.
    pub caller_ret_reg: u16,
    /// How many slots caller expects.
    pub caller_ret_count: usize,
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

#[derive(Debug, Clone)]
pub struct SelectCase {
    pub kind: SelectCaseKind,
    pub chan_reg: u16,
    pub val_reg: u16,
    pub elem_slots: u8,
    pub has_ok: bool,
}

#[derive(Debug, Clone)]
pub struct SelectState {
    pub cases: Vec<SelectCase>,
    pub has_default: bool,
    pub woken_index: Option<usize>,
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
    /// Waiting for channel/port send/recv (queue-like primitives).
    Queue,
    /// Waiting for I/O completion.
    #[cfg(feature = "std")]
    Io(vo_runtime::io::IoToken),
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
                    return val.as_str().to_string();
                }
                "panic".to_string()
            }
        }
    }
}

/// Initial stack capacity in slots (64KB = 8192 slots).
const INITIAL_STACK_CAPACITY: usize = 8192;

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

#[derive(Debug)]
pub struct Fiber {
    pub id: u32,
    /// Unified state machine (single source of truth).
    pub state: FiberState,
    pub stack: Vec<u64>,
    /// Stack pointer - current stack top. stack[0..sp] is in use.
    pub sp: usize,
    pub frames: Vec<CallFrame>,
    pub defer_stack: Vec<DeferEntry>,
    pub unwinding: Option<UnwindingState>,
    pub select_state: Option<SelectState>,
    pub panic_state: Option<PanicState>,
    pub panic_trap_kind: Option<RuntimeTrapKind>,
    /// Incremented each time a new panic starts. Used to determine which defers can recover.
    /// A defer registered at generation N can only recover panics with generation > N.
    pub panic_generation: u64,
    #[cfg(feature = "std")]
    pub resume_io_token: Option<IoToken>,
    /// JIT resume stack for suspended call chains.
    /// When JIT returns Call/WaitIo, resume points are pushed here.
    /// On resume, they are popped and converted to VM frames.
    #[cfg(feature = "jit")]
    pub resume_stack: Vec<ResumePoint>,
}

impl Fiber {
    pub fn new(id: u32) -> Self {
        Self {
            id,
            state: FiberState::Runnable,
            stack: Vec::with_capacity(INITIAL_STACK_CAPACITY),
            sp: 0,
            frames: Vec::new(),
            defer_stack: Vec::new(),
            unwinding: None,
            select_state: None,
            panic_state: None,
            panic_trap_kind: None,
            panic_generation: 0,
            #[cfg(feature = "std")]
            resume_io_token: None,
            #[cfg(feature = "jit")]
            resume_stack: Vec::new(),  // Lazy: only allocates on first push (Call/WaitIo)
        }
    }
    
    /// Reset fiber for reuse.
    pub fn reset(&mut self) {
        self.state = FiberState::Running;
        self.sp = 0;
        self.frames.clear();
        self.defer_stack.clear();
        self.unwinding = None;
        self.select_state = None;
        self.panic_state = None;
        self.panic_trap_kind = None;
        self.panic_generation = 0;
        #[cfg(feature = "std")]
        {
            self.resume_io_token = None;
        }
        #[cfg(feature = "jit")]
        self.resume_stack.clear();
    }
    
    /// Check if current panic is recoverable and return the interface{} value if so.
    /// Used by recover() to consume the panic value.
    pub fn take_recoverable_panic(&mut self) -> Option<InterfaceSlot> {
        match self.panic_state.take() {
            Some(PanicState::Recoverable(val)) => {
                self.panic_trap_kind = None;
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
        self.unwinding.as_ref()
            .map_or(false, |s| s.at_defer_boundary(self.frames.len()))
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
    pub fn ensure_capacity(&mut self, required: usize) {
        if required > self.stack.len() {
            let new_cap = self.stack.len().max(INITIAL_STACK_CAPACITY).max(required).next_power_of_two();
            self.stack.resize(new_cap, 0);
        }
    }

    pub fn push_frame(&mut self, func_id: u32, local_slots: u16, ret_reg: u16, ret_count: u16) {
        let bp = self.sp;
        let new_sp = bp + local_slots as usize;
        self.ensure_capacity(new_sp);
        self.sp = new_sp;
        self.frames.push(CallFrame::new(func_id, bp, ret_reg, ret_count));
    }

    pub fn pop_frame(&mut self) -> Option<CallFrame> {
        if let Some(frame) = self.frames.pop() {
            self.sp = frame.bp;
            Some(frame)
        } else {
            None
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
