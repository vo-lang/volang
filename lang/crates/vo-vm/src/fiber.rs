//! Fiber (coroutine) and related structures.

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::GcRef;
use vo_runtime::InterfaceSlot;

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
}

/// How return values are stored while defers execute.
#[derive(Debug, Clone)]
pub enum PendingReturnKind {
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
    /// If function has heap-allocated named returns, stores info to return them after recovery.
    Panic {
        /// Heap return info for recovery (reuses PendingReturnKind::Heap format).
        /// None if no heap returns or ret_slots is 0.
        heap_gcrefs: Option<Vec<u64>>,
        /// Slot count for each GcRef (parallel array).
        slots_per_ref: Vec<usize>,
        caller_ret_reg: u16,
        caller_ret_count: usize,
    },
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
    pub fn set_recoverable_panic(&mut self, msg: InterfaceSlot) {
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
    #[inline]
    pub fn is_direct_defer_context(&self) -> bool {
        match &self.unwinding {
            Some(UnwindingState { kind: UnwindingKind::Panic { .. }, target_depth, .. }) => {
                self.frames.len() == *target_depth + 1
            }
            _ => false,
        }
    }
    
    /// Switch unwinding mode from Panic to Return after successful recover().
    /// This prevents nested calls within the defer function from triggering panic_unwind.
    pub fn switch_panic_to_return_mode(&mut self) {
        let Some(ref mut state) = self.unwinding else { return };
        let UnwindingKind::Panic { heap_gcrefs, slots_per_ref, caller_ret_reg, caller_ret_count } = &mut state.kind else { return };
        
        let return_kind = match heap_gcrefs.take() {
            Some(gcrefs) => PendingReturnKind::Heap { gcrefs, slots_per_ref: core::mem::take(slots_per_ref) },
            None => PendingReturnKind::None,
        };
        state.kind = UnwindingKind::Return {
            return_kind,
            caller_ret_reg: *caller_ret_reg,
            caller_ret_count: *caller_ret_count,
        };
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
