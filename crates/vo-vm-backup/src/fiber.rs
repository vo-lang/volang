//! Fiber (goroutine) implementation.

use alloc::{vec, vec::Vec};

use crate::gc::GcRef;

/// Fiber ID.
pub type FiberId = u32;

/// Fiber status.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FiberStatus {
    Ready,
    Running,
    Blocked,
    Dead,
}

/// Block reason for a fiber.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BlockReason {
    None,
    ChanSend(GcRef),
    ChanRecv(GcRef),
}

// SAFETY: BlockReason contains GcRef (raw pointers) but these are just
// identifiers for GC objects. Thread safety is ensured at the GC level.
unsafe impl Send for BlockReason {}
unsafe impl Sync for BlockReason {}

/// Call frame.
#[derive(Clone, Debug)]
pub struct CallFrame {
    pub func_id: u32,
    pub pc: usize,
    pub bp: usize,
    pub ret_reg: u16,
    pub ret_count: u8,
    pub is_extern: bool,
}

impl CallFrame {
    pub fn new(func_id: u32, bp: usize, ret_reg: u16, ret_count: u8) -> Self {
        Self {
            func_id,
            pc: 0,
            bp,
            ret_reg,
            ret_count,
            is_extern: false,
        }
    }
    
    pub fn extern_call(func_id: u32, bp: usize, ret_reg: u16, ret_count: u8) -> Self {
        Self {
            func_id,
            pc: 0,
            bp,
            ret_reg,
            ret_count,
            is_extern: true,
        }
    }
}

/// Iterator state for range loops.
#[derive(Clone, Debug)]
pub enum IterState {
    Slice {
        slice_ref: GcRef,
        elem_size: usize,
        index: usize,
        len: usize,
    },
    Map {
        map_ref: GcRef,
        index: usize,
    },
    String {
        str_ref: GcRef,
        byte_pos: usize,
    },
    IntRange {
        current: i64,
        end: i64,
        step: i64,
    },
}

impl IterState {
    pub fn container_ref(&self) -> Option<GcRef> {
        match self {
            IterState::Slice { slice_ref, .. } => Some(*slice_ref),
            IterState::Map { map_ref, .. } => Some(*map_ref),
            IterState::String { str_ref, .. } => Some(*str_ref),
            IterState::IntRange { .. } => None,
        }
    }
}

// SAFETY: IterState contains GcRef (raw pointers) but these are just
// identifiers for GC objects. Thread safety is ensured at the GC level.
unsafe impl Send for IterState {}
unsafe impl Sync for IterState {}

/// A select case (send or recv).
#[derive(Clone, Debug)]
pub enum SelectCase {
    /// Send case: channel ref, value to send
    Send { chan: GcRef, value: u64 },
    /// Recv case: channel ref, dest register, ok register
    Recv { chan: GcRef, dest: u16, ok_dest: u16 },
}

/// State for building a select statement.
#[derive(Clone, Debug, Default)]
pub struct SelectState {
    pub cases: Vec<SelectCase>,
    pub has_default: bool,
}

/// Defer entry.
#[derive(Clone, Debug)]
pub struct DeferEntry {
    pub frame_depth: usize,
    pub func_id: u32,
    pub arg_count: u8,
    pub args: [u64; 8],
    /// Whether this is an errdefer (only runs on error return).
    pub is_errdefer: bool,
}

impl DeferEntry {
    pub fn new(frame_depth: usize, func_id: u32) -> Self {
        Self {
            frame_depth,
            func_id,
            arg_count: 0,
            args: [0; 8],
            is_errdefer: false,
        }
    }
}

/// State for executing pending defers during return.
#[derive(Clone, Debug)]
pub struct DeferState {
    /// Pending defers to execute (in order).
    pub pending: Vec<DeferEntry>,
    /// Return values to pass to caller after all defers complete.
    pub ret_vals: Vec<u64>,
    /// Caller's return register.
    pub caller_ret_reg: u16,
    /// Caller's expected return count.
    pub caller_ret_count: usize,
    /// Whether this is an error return (for errdefer filtering).
    pub is_error_return: bool,
}

impl DeferState {
    pub fn new(
        pending: Vec<DeferEntry>,
        ret_vals: Vec<u64>,
        caller_ret_reg: u16,
        caller_ret_count: usize,
        is_error_return: bool,
    ) -> Self {
        Self {
            pending,
            ret_vals,
            caller_ret_reg,
            caller_ret_count,
            is_error_return,
        }
    }
}

/// A fiber (lightweight thread / goroutine).
pub struct Fiber {
    pub id: FiberId,
    pub status: FiberStatus,
    pub block_reason: BlockReason,
    
    // Execution state
    pub stack: Vec<u64>,
    pub frames: Vec<CallFrame>,
    
    // Iterator stack (for range loops)
    pub iter_stack: Vec<IterState>,
    
    // Defer stack
    pub defer_stack: Vec<DeferEntry>,
    
    // Pending defer execution state (for multi-defer returns)
    pub defer_state: Option<DeferState>,
    
    // Panic state
    pub panic_value: Option<u64>,
    pub recovering: bool,
    
    // Assert state
    pub assert_failed: bool,
    pub assert_line: u16,
    
    // Select state (for building select statement)
    pub select_state: Option<SelectState>,
}

impl Fiber {
    pub fn new(id: FiberId, stack_size: usize) -> Self {
        Self {
            id,
            status: FiberStatus::Ready,
            block_reason: BlockReason::None,
            stack: vec![0; stack_size],
            frames: Vec::new(),
            iter_stack: Vec::new(),
            defer_stack: Vec::new(),
            defer_state: None,
            panic_value: None,
            recovering: false,
            assert_failed: false,
            assert_line: 0,
            select_state: None,
        }
    }
    
    /// Get current call frame.
    #[inline]
    pub fn frame(&self) -> Option<&CallFrame> {
        self.frames.last()
    }
    
    /// Get current call frame mutably.
    #[inline]
    pub fn frame_mut(&mut self) -> Option<&mut CallFrame> {
        self.frames.last_mut()
    }
    
    /// Get base pointer of current frame.
    #[inline]
    pub fn bp(&self) -> usize {
        self.frames.last().map(|f| f.bp).unwrap_or(0)
    }
    
    /// Read a register (stack slot relative to bp).
    #[inline]
    pub fn read_reg(&self, reg: u16) -> u64 {
        self.stack[self.bp() + reg as usize]
    }
    
    /// Write a register.
    #[inline]
    pub fn write_reg(&mut self, reg: u16, val: u64) {
        let idx = self.bp() + reg as usize;
        self.stack[idx] = val;
    }
    
    /// Read multiple registers as a slice.
    pub fn read_regs(&self, start: u16, count: usize) -> &[u64] {
        let bp = self.bp();
        &self.stack[bp + start as usize..bp + start as usize + count]
    }
    
    /// Write multiple registers.
    pub fn write_regs(&mut self, start: u16, values: &[u64]) {
        let bp = self.bp();
        let dest = &mut self.stack[bp + start as usize..bp + start as usize + values.len()];
        dest.copy_from_slice(values);
    }
    
    /// Ensure stack has capacity for given number of slots from current bp.
    pub fn ensure_stack(&mut self, slots: usize) {
        let required = self.bp() + slots;
        if self.stack.len() < required {
            self.stack.resize(required.max(self.stack.len() * 2), 0);
        }
    }
    
    /// Push a call frame for a Vo function.
    pub fn push_frame(&mut self, func_id: u32, arg_start: u16, local_slots: usize, ret_reg: u16, ret_count: u8) {
        let new_bp = self.bp() + arg_start as usize;
        self.ensure_stack(local_slots + 64); // Extra space for nested calls
        self.frames.push(CallFrame::new(func_id, new_bp, ret_reg, ret_count));
    }
    
    /// Push a call frame for an extern function.
    pub fn push_extern_frame(&mut self, func_id: u32, arg_start: u16, ret_reg: u16, ret_count: u8) {
        let new_bp = self.bp() + arg_start as usize;
        self.frames.push(CallFrame::extern_call(func_id, new_bp, ret_reg, ret_count));
    }
    
    /// Pop a call frame.
    pub fn pop_frame(&mut self) -> Option<CallFrame> {
        self.frames.pop()
    }
    
    /// Check if fiber is done (no more frames).
    pub fn is_done(&self) -> bool {
        self.frames.is_empty()
    }
    
    /// Get defers for current frame depth.
    pub fn get_frame_defers(&self) -> Vec<DeferEntry> {
        let depth = self.frames.len();
        self.defer_stack
            .iter()
            .filter(|d| d.frame_depth == depth)
            .cloned()
            .collect()
    }
    
    /// Pop defers for current frame depth.
    pub fn pop_frame_defers(&mut self) -> Vec<DeferEntry> {
        let depth = self.frames.len();
        let mut defers = Vec::new();
        while let Some(d) = self.defer_stack.last() {
            if d.frame_depth == depth {
                defers.push(self.defer_stack.pop().unwrap());
            } else {
                break;
            }
        }
        defers
    }
}

/// Scheduler for fibers.
#[derive(Default)]
pub struct Scheduler {
    fibers: Vec<Fiber>,
    ready_queue: alloc::collections::VecDeque<FiberId>,
    pub current: Option<FiberId>,
    next_id: FiberId,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            fibers: Vec::new(),
            ready_queue: alloc::collections::VecDeque::new(),
            current: None,
            next_id: 0,
        }
    }
    
    /// Spawn a new fiber.
    pub fn spawn(&mut self, stack_size: usize) -> FiberId {
        let id = self.next_id;
        self.next_id += 1;
        
        let fiber = Fiber::new(id, stack_size);
        self.fibers.push(fiber);
        self.ready_queue.push_back(id);
        
        id
    }
    
    /// Get fiber by ID.
    pub fn get(&self, id: FiberId) -> Option<&Fiber> {
        self.fibers.iter().find(|f| f.id == id)
    }
    
    /// Get fiber by ID mutably.
    pub fn get_mut(&mut self, id: FiberId) -> Option<&mut Fiber> {
        self.fibers.iter_mut().find(|f| f.id == id)
    }
    
    /// Get current fiber ID.
    pub fn current_id(&self) -> Option<FiberId> {
        self.current
    }
    
    /// Get current fiber.
    pub fn current(&self) -> Option<&Fiber> {
        self.current.and_then(|id| self.get(id))
    }
    
    /// Get current fiber mutably.
    pub fn current_mut(&mut self) -> Option<&mut Fiber> {
        if let Some(id) = self.current {
            self.fibers.iter_mut().find(|f| f.id == id)
        } else {
            None
        }
    }
    
    /// Schedule next ready fiber.
    pub fn schedule(&mut self) -> Option<FiberId> {
        // If current fiber is still running, don't switch
        if let Some(id) = self.current {
            if let Some(fiber) = self.get(id) {
                if fiber.status == FiberStatus::Running {
                    return Some(id);
                }
            }
        }
        
        // Find next ready fiber
        while let Some(id) = self.ready_queue.pop_front() {
            if let Some(fiber) = self.get_mut(id) {
                if fiber.status == FiberStatus::Ready || fiber.status == FiberStatus::Running {
                    fiber.status = FiberStatus::Running;
                    self.current = Some(id);
                    return Some(id);
                }
            }
        }
        
        self.current = None;
        None
    }
    
    /// Yield current fiber (put back in ready queue).
    pub fn yield_current(&mut self) {
        if let Some(id) = self.current {
            if let Some(fiber) = self.get_mut(id) {
                fiber.status = FiberStatus::Ready;
            }
            self.ready_queue.push_back(id);
        }
        self.current = None;
    }
    
    /// Block current fiber.
    pub fn block_current(&mut self, reason: BlockReason) {
        if let Some(id) = self.current {
            if let Some(fiber) = self.get_mut(id) {
                fiber.status = FiberStatus::Blocked;
                fiber.block_reason = reason;
            }
        }
        self.current = None;
    }
    
    /// Unblock a fiber and put it in ready queue.
    pub fn unblock(&mut self, id: FiberId) {
        if let Some(fiber) = self.get_mut(id) {
            if fiber.status == FiberStatus::Blocked {
                fiber.status = FiberStatus::Ready;
                fiber.block_reason = BlockReason::None;
                self.ready_queue.push_back(id);
            }
        }
    }
    
    /// Mark fiber as dead.
    pub fn kill(&mut self, id: FiberId) {
        if let Some(fiber) = self.get_mut(id) {
            fiber.status = FiberStatus::Dead;
        }
        if self.current == Some(id) {
            self.current = None;
        }
    }
    
    /// Check if all fibers are done.
    pub fn all_done(&self) -> bool {
        self.fibers.iter().all(|f| f.status == FiberStatus::Dead)
    }
    
    /// Get fibers blocked on a channel for sending.
    pub fn get_blocked_senders(&self, chan: GcRef) -> Vec<FiberId> {
        self.fibers
            .iter()
            .filter(|f| matches!(f.block_reason, BlockReason::ChanSend(c) if c == chan))
            .map(|f| f.id)
            .collect()
    }
    
    /// Get fibers blocked on a channel for receiving.
    pub fn get_blocked_receivers(&self, chan: GcRef) -> Vec<FiberId> {
        self.fibers
            .iter()
            .filter(|f| matches!(f.block_reason, BlockReason::ChanRecv(c) if c == chan))
            .map(|f| f.id)
            .collect()
    }
    
    /// Iterate over all fibers (for GC root scanning).
    pub fn iter_fibers(&self) -> impl Iterator<Item = &Fiber> {
        self.fibers.iter()
    }
}
