//! Fiber scheduler for cooperative multitasking.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, collections::VecDeque, string::String, vec::Vec};
#[cfg(feature = "std")]
use std::collections::{HashMap, VecDeque};

use crate::fiber::{Fiber, FiberStatus, ParkReason};
#[cfg(feature = "std")]
use vo_runtime::io::{IoRuntime, IoToken};

/// Type-safe fiber ID that distinguishes regular fibers from trampoline fibers.
/// This prevents accidental indexing of the wrong fiber array.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FiberId {
    /// Regular fiber - index into scheduler.fibers
    Regular(u32),
    /// Trampoline fiber for JIT->VM calls - index into scheduler.trampoline_fibers
    Trampoline(u32),
}

impl FiberId {
    /// Convert to raw u32 for storage (e.g., in channel wait queues).
    /// Regular: index, Trampoline: 0x80000000 | index
    #[inline]
    pub fn to_raw(self) -> u32 {
        match self {
            FiberId::Regular(id) => id,
            FiberId::Trampoline(id) => TRAMPOLINE_FIBER_FLAG | id,
        }
    }
    
    /// Create from raw u32.
    #[inline]
    pub fn from_raw(raw: u32) -> Self {
        if raw & TRAMPOLINE_FIBER_FLAG != 0 {
            FiberId::Trampoline(raw & !TRAMPOLINE_FIBER_FLAG)
        } else {
            FiberId::Regular(raw)
        }
    }
    
    /// Check if this is a trampoline fiber.
    #[inline]
    pub fn is_trampoline(self) -> bool {
        matches!(self, FiberId::Trampoline(_))
    }
}

/// High bit flag to distinguish trampoline fibers from regular fibers.
pub const TRAMPOLINE_FIBER_FLAG: u32 = 0x8000_0000;

#[derive(Debug)]
pub struct Scheduler {
    /// Fibers indexed by id (id == index).
    /// Box<Fiber> ensures stable addresses - Vec reallocation won't invalidate fiber pointers.
    pub fibers: Vec<Box<Fiber>>,
    /// Free slots from dead fibers, available for reuse.
    free_slots: Vec<u32>,
    pub ready_queue: VecDeque<u32>,
    pub current: Option<u32>,
    
    /// Trampoline fibers for JIT->VM calls (separate ID space with high bit set).
    /// Box<Fiber> ensures stable addresses.
    pub trampoline_fibers: Vec<Box<Fiber>>,
    /// Free slots in trampoline_fibers pool.
    trampoline_free_slots: Vec<u32>,

    /// Map from I/O token to waiting fiber.
    #[cfg(feature = "std")]
    io_waiters: HashMap<IoToken, FiberId>,
}

impl Scheduler {
    pub fn new() -> Self {
        Scheduler {
            fibers: Vec::new(),
            free_slots: Vec::new(),
            ready_queue: VecDeque::new(),
            current: None,
            trampoline_fibers: Vec::new(),
            trampoline_free_slots: Vec::new(),
            #[cfg(feature = "std")]
            io_waiters: HashMap::new(),
        }
    }
    
    /// Acquire a trampoline fiber for JIT->VM calls.
    pub fn acquire_trampoline_fiber(&mut self) -> FiberId {
        let index = if let Some(slot) = self.trampoline_free_slots.pop() {
            // Reuse existing fiber
            self.trampoline_fibers[slot as usize].reset();
            slot
        } else {
            // Create new fiber
            let index = self.trampoline_fibers.len() as u32;
            let mut fiber = Fiber::new(FiberId::Trampoline(index).to_raw());
            fiber.status = FiberStatus::Running;
            self.trampoline_fibers.push(Box::new(fiber));
            index
        };

        FiberId::Trampoline(index)
    }
    
    /// Release a trampoline fiber back to the pool.
    pub fn release_trampoline_fiber(&mut self, id: FiberId) {
        match id {
            FiberId::Trampoline(index) => self.trampoline_free_slots.push(index),
            FiberId::Regular(_) => panic!("release_trampoline_fiber called with regular FiberId"),
        }
    }
    
    /// Get trampoline fiber by ID.
    #[inline]
    pub fn trampoline_fiber(&self, id: FiberId) -> &Fiber {
        match id {
            FiberId::Trampoline(index) => &*self.trampoline_fibers[index as usize],
            FiberId::Regular(_) => panic!("trampoline_fiber called with regular FiberId"),
        }
    }
    
    /// Get mutable trampoline fiber by ID.
    #[inline]
    pub fn trampoline_fiber_mut(&mut self, id: FiberId) -> &mut Fiber {
        match id {
            FiberId::Trampoline(index) => &mut *self.trampoline_fibers[index as usize],
            FiberId::Regular(_) => panic!("trampoline_fiber_mut called with regular FiberId"),
        }
    }

    /// Spawn a new fiber, returns its id.
    /// Reuses dead fiber slots when available.
    pub fn spawn(&mut self, mut fiber: Fiber) -> u32 {
        let id = if let Some(slot) = self.free_slots.pop() {
            // Reuse dead fiber slot
            fiber.id = slot;
            *self.fibers[slot as usize] = fiber;
            slot
        } else {
            // Allocate new slot
            let id = self.fibers.len() as u32;
            fiber.id = id;
            self.fibers.push(Box::new(fiber));
            id
        };
        self.ready_queue.push_back(id);
        id
    }

    /// Get fiber by id (O(1) index access).
    /// Note: Does NOT handle trampoline fibers. Use `get_fiber(FiberId)` for unified access.
    #[inline]
    pub fn fiber(&self, id: u32) -> &Fiber {
        &*self.fibers[id as usize]
    }

    /// Get mutable fiber by id (O(1) index access).
    /// Note: Does NOT handle trampoline fibers. Use `get_fiber_mut(FiberId)` for unified access.
    #[inline]
    pub fn fiber_mut(&mut self, id: u32) -> &mut Fiber {
        &mut *self.fibers[id as usize]
    }
    
    /// Get fiber by FiberId (type-safe version).
    #[inline]
    pub fn get_fiber(&self, id: FiberId) -> &Fiber {
        match id {
            FiberId::Regular(idx) => &*self.fibers[idx as usize],
            FiberId::Trampoline(idx) => &*self.trampoline_fibers[idx as usize],
        }
    }
    
    /// Get mutable fiber by FiberId (type-safe version).
    #[inline]
    pub fn get_fiber_mut(&mut self, id: FiberId) -> &mut Fiber {
        match id {
            FiberId::Regular(idx) => &mut *self.fibers[idx as usize],
            FiberId::Trampoline(idx) => &mut *self.trampoline_fibers[idx as usize],
        }
    }
    
    /// Wake a fiber by FiberId (type-safe version).
    pub fn wake_fiber(&mut self, id: FiberId) {
        match id {
            FiberId::Regular(idx) => {
                let fiber = &mut self.fibers[idx as usize];
                if fiber.status == FiberStatus::Suspended {
                    if !self.ready_queue.contains(&idx) {
                        self.ready_queue.push_back(idx);
                    }
                }
            }
            FiberId::Trampoline(idx) => {
                let fiber = &mut self.trampoline_fibers[idx as usize];
                if fiber.status == FiberStatus::Suspended {
                    fiber.status = FiberStatus::Running;
                }
            }
        }
    }

    /// Get current fiber reference.
    #[inline]
    pub fn current_fiber(&self) -> Option<&Fiber> {
        self.current.map(|id| &*self.fibers[id as usize])
    }

    /// Get current fiber mutable reference.
    #[inline]
    pub fn current_fiber_mut(&mut self) -> Option<&mut Fiber> {
        self.current.map(|id| &mut *self.fibers[id as usize])
    }

    pub fn suspend_current(&mut self) {
        if let Some(id) = self.current {
            self.fibers[id as usize].status = FiberStatus::Suspended;
            if !self.ready_queue.contains(&id) {
                self.ready_queue.push_back(id);
            }
            self.current = None;
        }
    }

    pub fn block_current(&mut self) {
        if let Some(id) = self.current {
            self.fibers[id as usize].status = FiberStatus::Suspended;
            self.current = None;
        }
    }

    pub fn schedule_next(&mut self) -> Option<u32> {
        while let Some(id) = self.ready_queue.pop_front() {
            let fiber = &mut self.fibers[id as usize];
            if fiber.status != FiberStatus::Dead {
                fiber.status = FiberStatus::Running;
                self.current = Some(id);
                return Some(id);
            }
        }
        self.current = None;
        None
    }

    /// Kill current fiber and return (panic_msg, error_location).
    /// error_location is (func_id, pc) from the current frame if available.
    pub fn kill_current(&mut self) -> (Option<String>, Option<(u32, u32)>) {
        if let Some(id) = self.current {
            let fiber = &mut self.fibers[id as usize];
            let msg = fiber.panic_message();
            let loc = fiber.current_frame().map(|f| (f.func_id, f.pc as u32));
            fiber.status = FiberStatus::Dead;
            self.free_slots.push(id);
            self.current = None;
            (msg, loc)
        } else {
            (None, None)
        }
    }

    pub fn has_runnable(&self) -> bool {
        if !self.ready_queue.is_empty() {
            return true;
        }
        if let Some(id) = self.current {
            return self.fibers[id as usize].status == FiberStatus::Running;
        }
        false
    }
    
    /// Check if there are blocked/suspended fibers waiting to be woken.
    pub fn has_blocked(&self) -> bool {
        // If we have fibers but none runnable, some must be blocked/suspended
        !self.fibers.is_empty() && !self.has_runnable() && 
            self.fibers.iter().any(|f| f.status == FiberStatus::Suspended)
    }
    
    /// Park current fiber with a reason. The fiber will be blocked until unparked.
    #[cfg(feature = "std")]
    pub fn park_current_for_io(&mut self, token: IoToken) {
        if let Some(id) = self.current {
            let fiber = &mut self.fibers[id as usize];
            fiber.status = FiberStatus::Suspended;
            fiber.park_reason = Some(ParkReason::Io { token });

            self.io_waiters.insert(token, FiberId::Regular(id));

            self.current = None;
        }
    }
    
    /// Poll I/O and wake any fibers that are ready.
    /// Returns number of fibers woken.
    #[cfg(feature = "std")]
    pub fn poll_io(&mut self, io: &mut IoRuntime) -> usize {
        let mut woken = 0;

        for completion in io.poll() {
            let token = completion.token;
            let fiber_id = match self.io_waiters.remove(&token) {
                Some(id) => id,
                None => continue,
            };

            match fiber_id {
                FiberId::Regular(id) => {
                    let fiber = &mut self.fibers[id as usize];
                    fiber.park_reason = None;
                    fiber.resume_io_token = Some(token);
                    if fiber.status == FiberStatus::Suspended {
                        if !self.ready_queue.contains(&id) {
                            self.ready_queue.push_back(id);
                        }
                        woken += 1;
                    }
                }
                FiberId::Trampoline(_) => {
                    panic!("I/O wait token woke trampoline fiber: token={}", token);
                }
            }
        }
        woken
    }
    
    /// Check if there are fibers waiting on I/O.
    #[cfg(feature = "std")]
    pub fn has_io_waiters(&self) -> bool {
        !self.io_waiters.is_empty()
    }

}

impl Default for Scheduler {
    fn default() -> Self {
        Self::new()
    }
}
