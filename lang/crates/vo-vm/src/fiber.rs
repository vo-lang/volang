//! Fiber (coroutine) and related structures.

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::objects::interface::InterfaceSlot;
#[cfg(feature = "std")]
use vo_runtime::io::IoToken;

use vo_runtime::gc::GcRef;

#[derive(Debug, Clone, Copy)]
pub struct CallFrame {
    pub func_id: u32,
    pub pc: usize,
    pub bp: usize,
    pub ret_reg: u16,
    pub ret_count: u16,
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
#[derive(Debug, Clone, Default)]
pub enum PendingReturnKind {
    #[default]
    /// No return values (void function or recovered panic without named returns).
    None,
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

/// What kind of unwinding is in progress.
#[derive(Debug, Clone)]
pub enum UnwindingKind {
    /// Normal return with pending defers.
    Return {
        return_kind: PendingReturnKind,
        caller_ret_reg: u16,
        caller_ret_count: usize,
    },
    /// Panic unwinding - execute defers, check for recover().
    /// Preserves return values so they can be restored if recover() succeeds.
    Panic {
        /// Saved return values from the original return (before panic).
        /// Used to restore return values when recover() succeeds.
        saved_return_kind: PendingReturnKind,
        caller_ret_reg: u16,
        caller_ret_count: usize,
    },
}

/// Defines return semantics - whether this is a normal or error return.
/// This is the single source of truth for errdefer behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReturnSemantics {
    /// Normal return: errdefers don't run, preserve named return values.
    Normal,
    /// Error return (fail stmt, non-nil error, or unrecovered panic): errdefers run.
    Error,
}

impl ReturnSemantics {
    /// Whether errdefers should be included in defer execution.
    #[inline]
    pub fn should_run_errdefers(&self) -> bool {
        matches!(self, ReturnSemantics::Error)
    }
}

/// Unified state for defer execution during return or panic unwinding.
///
/// Lifecycle:
/// 1. Return/panic triggers unwinding → UnwindingState created
/// 2. Each defer executes and returns → next defer called
/// 3. For Return: all defers done → write return values, clear state
/// 4. For Panic: if recover() called → clear state, resume normal
/// 5. For Panic: no recover, no more defers → unwind to parent frame
/// 6. For Panic: no more frames → return ExecResult::Panic
#[derive(Debug, Clone)]
pub struct UnwindingState {
    /// Defers remaining to execute (LIFO order, first = next to run).
    pub pending: Vec<DeferEntry>,
    /// Frame depth after the unwinding function was popped.
    /// Defer functions run at depth = target_depth + 1.
    pub target_depth: usize,
    /// What kind of unwinding is in progress.
    pub kind: UnwindingKind,
    /// The generation of the currently executing defer.
    /// Used with Fiber.panic_generation to check if recover() should work.
    pub current_defer_generation: u64,
}

impl UnwindingState {
    /// Transition to Return mode. This is the ONLY place where errdefers are filtered out.
    /// Call this when:
    /// - Panic is recovered (panic -> normal return)
    /// - Continuing with defers after initial return collection
    pub fn transition_to_return(
        &mut self,
        return_kind: PendingReturnKind,
        caller_ret_reg: u16,
        caller_ret_count: usize,
    ) {
        // Filter out errdefers - function is returning normally (not with an error)
        self.pending.retain(|d| !d.is_errdefer);
        self.kind = UnwindingKind::Return {
            return_kind,
            caller_ret_reg,
            caller_ret_count,
        };
    }
    
    /// Transition to Panic mode. Keeps all defers including errdefers.
    /// Call this when continuing panic unwinding.
    pub fn transition_to_panic(
        &mut self,
        saved_return_kind: PendingReturnKind,
        caller_ret_reg: u16,
        caller_ret_count: usize,
    ) {
        // Keep errdefers - panic unwinding is an error path
        self.kind = UnwindingKind::Panic {
            saved_return_kind,
            caller_ret_reg,
            caller_ret_count,
        };
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FiberStatus {
    Running,
    Suspended,
    Dead,
}

/// I/O wait kind for non-blocking I/O.
#[cfg(feature = "std")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IoKind {
    Read,
    Write,
}

/// Reason why a fiber is parked (blocked waiting for something).
#[derive(Debug, Clone)]
pub enum ParkReason {
    /// Waiting on channel operation (send or receive).
    Channel,
    /// Waiting on I/O operation (std only).
    #[cfg(feature = "std")]
    Io { token: IoToken },
}

/// Unified panic state for both recoverable and fatal panics.
#[derive(Debug, Clone, Copy)]
pub enum PanicState {
    /// Recoverable panic (user code panic, runtime errors like bounds check).
    /// Can be caught by recover() in a defer.
    /// Stores full interface{} value as InterfaceSlot.
    Recoverable(InterfaceSlot),
    /// Fatal panic (internal VM/JIT errors that cannot be recovered).
    /// Examples: blocking operation in JIT, unsupported operation.
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

#[derive(Debug)]
pub struct Fiber {
    pub id: u32,
    pub status: FiberStatus,
    pub stack: Vec<u64>,
    pub frames: Vec<CallFrame>,
    pub defer_stack: Vec<DeferEntry>,
    pub unwinding: Option<UnwindingState>,
    pub select_state: Option<SelectState>,
    pub panic_state: Option<PanicState>,
    /// Incremented each time a new panic starts. Used to determine which defers can recover.
    /// A defer registered at generation N can only recover panics with generation > N.
    pub panic_generation: u64,
    /// Reason why this fiber is parked, if any.
    pub park_reason: Option<ParkReason>,
    #[cfg(feature = "std")]
    pub resume_io_token: Option<IoToken>,
}

impl Fiber {
    pub fn new(id: u32) -> Self {
        Self {
            id,
            status: FiberStatus::Suspended,
            stack: Vec::new(),
            frames: Vec::new(),
            defer_stack: Vec::new(),
            unwinding: None,
            select_state: None,
            panic_state: None,
            panic_generation: 0,
            park_reason: None,
            #[cfg(feature = "std")]
            resume_io_token: None,
        }
    }
    
    /// Reset fiber for reuse (trampoline fiber pool).
    pub fn reset(&mut self) {
        self.status = FiberStatus::Running;
        self.stack.clear();
        self.frames.clear();
        self.defer_stack.clear();
        self.unwinding = None;
        self.select_state = None;
        self.panic_state = None;
        self.panic_generation = 0;
        self.park_reason = None;
        #[cfg(feature = "std")]
        {
            self.resume_io_token = None;
        }
    }
    
    /// Check if current panic is recoverable and return the interface{} value if so.
    /// Used by recover() to consume the panic value.
    pub fn take_recoverable_panic(&mut self) -> Option<InterfaceSlot> {
        match self.panic_state.take() {
            Some(PanicState::Recoverable(val)) => Some(val),
            other => {
                self.panic_state = other; // Put it back if not recoverable
                None
            }
        }
    }
    
    /// Set a fatal (non-recoverable) panic.
    pub fn set_fatal_panic(&mut self) {
        self.panic_state = Some(PanicState::Fatal);
    }
    
    /// Set a recoverable panic with full interface{} value (InterfaceSlot).
    /// Also increments panic_generation so we can track which defers can recover.
    pub fn set_recoverable_panic(&mut self, msg: InterfaceSlot) {
        self.panic_generation += 1;
        self.panic_state = Some(PanicState::Recoverable(msg));
    }
    
    /// Get panic message for error reporting.
    pub fn panic_message(&self) -> Option<String> {
        self.panic_state.as_ref().map(|s| s.message())
    }
    
    /// Check if we're in panic unwinding mode AND directly in the defer function
    /// (not in a nested call from the defer function).
    /// Per Go semantics, recover() only works when called directly from defer.
    /// Defer functions run at depth = target_depth + 1.
    /// Additionally, the defer must have been registered before the current panic started.
    #[inline]
    pub fn is_direct_defer_context(&self) -> bool {
        match &self.unwinding {
            Some(UnwindingState { 
                kind: UnwindingKind::Panic { .. }, 
                target_depth, 
                current_defer_generation,
                ..
            }) => {
                // Must be at defer execution depth
                if self.frames.len() != *target_depth + 1 {
                    return false;
                }
                // Defer must have been registered before the current panic
                // (registered_at < panic_generation means it was registered before this panic)
                *current_defer_generation < self.panic_generation
            }
            _ => false,
        }
    }
    
    /// Switch unwinding mode from Panic to Return after successful recover().
    /// This prevents nested calls within the defer function from triggering panic_unwind.
    /// Delegates to UnwindingState::transition_to_return which handles errdefer filtering.
    pub fn switch_panic_to_return_mode(&mut self) {
        let Some(ref mut state) = self.unwinding else { return };
        let UnwindingKind::Panic { saved_return_kind, caller_ret_reg, caller_ret_count } = &mut state.kind else { return };
        
        // Use transition method - it handles errdefer filtering
        let return_kind = core::mem::take(saved_return_kind);
        let reg = *caller_ret_reg;
        let count = *caller_ret_count;
        state.transition_to_return(return_kind, reg, count);
    }
    
    /// Get the effective generation for registering a new defer.
    /// During panic unwinding, returns the current_defer_generation so nested defers
    /// can recover the same panic as their parent defer.
    /// Outside panic unwinding, returns panic_generation (current value before any panic).
    #[inline]
    pub fn effective_defer_generation(&self) -> u64 {
        match &self.unwinding {
            Some(UnwindingState { kind: UnwindingKind::Panic { .. }, current_defer_generation, .. }) => {
                *current_defer_generation
            }
            _ => self.panic_generation,
        }
    }

    pub fn push_frame(&mut self, func_id: u32, local_slots: u16, ret_reg: u16, ret_count: u16) {
        let bp = self.stack.len();
        self.stack.resize(bp + local_slots as usize, 0);
        self.frames.push(CallFrame {
            func_id,
            pc: 0,
            bp,
            ret_reg,
            ret_count,
        });
    }

    pub fn pop_frame(&mut self) -> Option<CallFrame> {
        if let Some(frame) = self.frames.pop() {
            self.stack.truncate(frame.bp);
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
