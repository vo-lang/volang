//! Fiber (coroutine) and related structures.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

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
}

#[derive(Debug, Clone)]
pub struct DeferState {
    pub pending: Vec<DeferEntry>,
    pub ret_vals: Vec<u64>,
    /// SlotTypes for ret_vals (for GC scanning)
    pub ret_slot_types: Vec<vo_runtime::SlotType>,
    pub caller_ret_reg: u16,
    pub caller_ret_count: usize,
    pub is_error_return: bool,
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

#[derive(Debug)]
pub struct Fiber {
    pub id: u32,
    pub status: FiberStatus,
    pub stack: Vec<u64>,
    pub frames: Vec<CallFrame>,
    pub defer_stack: Vec<DeferEntry>,
    pub defer_state: Option<DeferState>,
    pub select_state: Option<SelectState>,
    pub panic_value: Option<GcRef>,
}

impl Fiber {
    pub fn new(id: u32) -> Self {
        Self {
            id,
            status: FiberStatus::Suspended,
            stack: Vec::new(),
            frames: Vec::new(),
            defer_stack: Vec::new(),
            defer_state: None,
            select_state: None,
            panic_value: None,
        }
    }
    
    /// Reset fiber for reuse (trampoline fiber pool).
    pub fn reset(&mut self) {
        self.status = FiberStatus::Running;
        self.stack.clear();
        self.frames.clear();
        self.defer_stack.clear();
        self.defer_state = None;
        self.select_state = None;
        self.panic_value = None;
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
