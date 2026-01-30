//! Fiber scheduler for cooperative multitasking.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, collections::VecDeque, string::String, vec::Vec};
#[cfg(feature = "std")]
use std::collections::{HashMap, VecDeque};

use crate::fiber::{BlockReason, Fiber, FiberState};
use crate::vm::RuntimeTrapKind;
#[cfg(feature = "std")]
use vo_runtime::io::{IoRuntime, IoToken};

/// Type-safe fiber ID.
/// Keeps enum form for type safety - all fiber access MUST go through scheduler.get_fiber(id).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FiberId {
    /// Regular fiber - index into scheduler.fibers
    Regular(u32),
}

impl FiberId {
    /// Convert to raw u32 for storage (e.g., in channel wait queues).
    #[inline]
    pub fn to_raw(self) -> u32 {
        match self {
            FiberId::Regular(id) => id,
        }
    }
    
    /// Create from raw u32.
    #[inline]
    pub fn from_raw(raw: u32) -> Self {
        FiberId::Regular(raw)
    }
    
    /// Get the index.
    #[inline]
    pub fn index(self) -> u32 {
        match self {
            FiberId::Regular(id) => id,
        }
    }
}

#[derive(Debug)]
pub struct Scheduler {
    /// Fibers indexed by id (id == index).
    /// Box<Fiber> ensures stable addresses - Vec reallocation won't invalidate fiber pointers.
    pub fibers: Vec<Box<Fiber>>,
    /// Free slots from dead fibers, available for reuse.
    free_slots: Vec<u32>,
    pub ready_queue: VecDeque<u32>,
    pub current: Option<u32>,

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
            #[cfg(feature = "std")]
            io_waiters: HashMap::new(),
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
        }
    }
    
    /// Get mutable fiber by FiberId (type-safe version).
    #[inline]
    pub fn get_fiber_mut(&mut self, id: FiberId) -> &mut Fiber {
        match id {
            FiberId::Regular(idx) => &mut *self.fibers[idx as usize],
        }
    }
    
    /// Wake a blocked fiber by FiberId.
    /// Blocked(*) -> Runnable (added to ready_queue).
    pub fn wake_fiber(&mut self, id: FiberId) {
        match id {
            FiberId::Regular(idx) => {
                let fiber = &mut self.fibers[idx as usize];
                if fiber.state.is_blocked() {
                    fiber.state = FiberState::Runnable;
                    if !self.ready_queue.contains(&idx) {
                        self.ready_queue.push_back(idx);
                    }
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

    /// Current fiber yields CPU, remains runnable.
    /// Running -> Runnable (back to ready_queue).
    pub fn yield_current(&mut self) {
        if let Some(id) = self.current {
            let fiber = &mut self.fibers[id as usize];
            fiber.state = FiberState::Runnable;
            self.ready_queue.push_back(id);
            self.current = None;
        }
    }

    /// Current fiber blocks on queue (channel/port).
    /// Running -> Blocked(Queue).
    pub fn block_for_queue(&mut self) {
        if let Some(id) = self.current {
            let fiber = &mut self.fibers[id as usize];
            fiber.state = FiberState::Blocked(BlockReason::Queue);
            self.current = None;
        }
    }

    /// Pick next runnable fiber and set it to Running.
    /// Runnable -> Running.
    pub fn schedule_next(&mut self) -> Option<u32> {
        while let Some(id) = self.ready_queue.pop_front() {
            let fiber = &mut self.fibers[id as usize];
            if fiber.state.is_runnable() {
                fiber.state = FiberState::Running;
                self.current = Some(id);
                return Some(id);
            }
            // Skip dead/blocked fibers that shouldn't be in queue
        }
        self.current = None;
        None
    }

    /// Kill current fiber and return (trap_kind, panic_msg, error_location).
    /// error_location is (func_id, pc) from the current frame if available.
    /// * -> Dead.
    pub fn kill_current(&mut self) -> (Option<RuntimeTrapKind>, Option<String>, Option<(u32, u32)>) {
        if let Some(id) = self.current {
            let fiber = &mut self.fibers[id as usize];
            let trap_kind = fiber.panic_trap_kind.take();
            let msg = fiber.panic_message();
            let loc = fiber.current_frame().map(|f| (f.func_id, f.pc as u32));
            fiber.state = FiberState::Dead;
            self.free_slots.push(id);
            self.current = None;
            (trap_kind, msg, loc)
        } else {
            (None, None, None)
        }
    }

    /// Check if scheduler has work to do (can make forward progress).
    /// True if either:
    /// - ready_queue is not empty (there are Runnable fibers waiting), OR
    /// - there is a currently Running fiber.
    pub fn has_work(&self) -> bool {
        if !self.ready_queue.is_empty() {
            return true;
        }
        if let Some(id) = self.current {
            return self.fibers[id as usize].state.is_running();
        }
        false
    }
    /// Check if there are blocked fibers (potential deadlock detection).
    pub fn has_blocked(&self) -> bool {
        self.fibers.iter().any(|f| f.state.is_blocked())
    }
    
    /// Current fiber blocks on I/O.
    /// Running -> Blocked(Io(token)).
    /// ONLY for regular fibers. Trampoline calling this -> panic.
    #[cfg(feature = "std")]
    pub fn block_for_io(&mut self, token: IoToken) {
        if let Some(id) = self.current {
            let fiber = &mut self.fibers[id as usize];
            fiber.state = FiberState::Blocked(BlockReason::Io(token));
            self.io_waiters.insert(token, FiberId::Regular(id));
            self.current = None;
        }
    }
    /// Poll I/O and wake any fibers that are ready.
    /// Returns number of fibers woken.
    #[cfg(feature = "std")]
    pub fn poll_io(&mut self, io: &mut IoRuntime) -> usize {
        let mut woken = 0;

        for token in io.poll() {
            let fiber_id = match self.io_waiters.remove(&token) {
                Some(id) => id,
                None => continue,
            };

            match fiber_id {
                FiberId::Regular(id) => {
                    let fiber = &mut self.fibers[id as usize];
                    fiber.resume_io_token = Some(token);
                    if fiber.state.is_blocked() {
                        fiber.state = FiberState::Runnable;
                        self.ready_queue.push_back(id);
                        woken += 1;
                    }
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
