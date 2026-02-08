//! Fiber scheduler for cooperative multitasking.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, collections::VecDeque, string::String, vec::Vec};
#[cfg(feature = "std")]
use std::collections::{HashMap, VecDeque};

use crate::fiber::{BlockReason, Fiber, FiberState};
use crate::vm::RuntimeTrapKind;
#[cfg(feature = "std")]
use vo_runtime::io::{IoRuntime, IoToken};

/// Type-safe fiber ID (newtype over u32 index into scheduler.fibers).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FiberId(u32);

impl FiberId {
    /// Convert to raw u32 for storage (e.g., in channel wait queues).
    #[inline]
    pub fn to_raw(self) -> u32 {
        self.0
    }
    
    /// Create from raw u32.
    #[inline]
    pub fn from_raw(raw: u32) -> Self {
        FiberId(raw)
    }
    
    /// Get the index.
    #[inline]
    pub fn index(self) -> u32 {
        self.0
    }
}

#[derive(Debug)]
pub struct Scheduler {
    /// Fibers indexed by id (id == index).
    /// Box<Fiber> ensures stable addresses - Vec reallocation won't invalidate fiber pointers.
    pub fibers: Vec<Box<Fiber>>,
    /// Free slots from dead fibers, available for reuse.
    free_slots: Vec<u32>,
    pub ready_queue: VecDeque<FiberId>,
    pub current: Option<FiberId>,
    /// Number of fibers currently in Blocked state (O(1) has_blocked).
    blocked_count: u32,

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
            blocked_count: 0,
            #[cfg(feature = "std")]
            io_waiters: HashMap::new(),
        }
    }

    /// Spawn a new fiber, returns its FiberId.
    /// Reuses dead fiber slots when available.
    pub fn spawn(&mut self, fiber: Fiber) -> FiberId {
        let id = self.spawn_not_ready(fiber);
        self.ready_queue.push_back(id);
        id
    }
    
    /// Spawn a new fiber without adding to ready_queue.
    fn spawn_not_ready(&mut self, mut fiber: Fiber) -> FiberId {
        if let Some(slot) = self.free_slots.pop() {
            fiber.id = slot;
            *self.fibers[slot as usize] = fiber;
            FiberId(slot)
        } else {
            let id = self.fibers.len() as u32;
            fiber.id = id;
            self.fibers.push(Box::new(fiber));
            FiberId(id)
        }
    }

    /// Get fiber by FiberId (O(1) index access).
    #[inline]
    pub fn get_fiber(&self, id: FiberId) -> &Fiber {
        &*self.fibers[id.0 as usize]
    }
    
    /// Get mutable fiber by FiberId (O(1) index access).
    #[inline]
    pub fn get_fiber_mut(&mut self, id: FiberId) -> &mut Fiber {
        &mut *self.fibers[id.0 as usize]
    }
    
    /// Wake a blocked fiber.
    /// Blocked(*) -> Runnable (added to ready_queue).
    pub fn wake_fiber(&mut self, id: FiberId) {
        let fiber = &mut self.fibers[id.0 as usize];
        if fiber.state.is_blocked() {
            fiber.state = FiberState::Runnable;
            self.blocked_count -= 1;
            self.ready_queue.push_back(id);
        }
    }

    /// Get current fiber reference.
    #[inline]
    pub fn current_fiber(&self) -> Option<&Fiber> {
        self.current.map(|id| &*self.fibers[id.0 as usize])
    }

    /// Get current fiber mutable reference.
    #[inline]
    pub fn current_fiber_mut(&mut self) -> Option<&mut Fiber> {
        self.current.map(|id| &mut *self.fibers[id.0 as usize])
    }

    /// Current fiber yields CPU, remains runnable.
    /// Running -> Runnable (back to ready_queue).
    pub fn yield_current(&mut self) {
        if let Some(id) = self.current.take() {
            let fiber = &mut self.fibers[id.0 as usize];
            fiber.state = FiberState::Runnable;
            self.ready_queue.push_back(id);
        }
    }

    /// Current fiber blocks on queue (channel/port).
    /// Running -> Blocked(Queue).
    pub fn block_for_queue(&mut self) {
        if let Some(id) = self.current.take() {
            let fiber = &mut self.fibers[id.0 as usize];
            fiber.state = FiberState::Blocked(BlockReason::Queue);
            self.blocked_count += 1;
        }
    }

    /// Pick next runnable fiber and set it to Running.
    /// Runnable -> Running.
    pub fn schedule_next(&mut self) -> Option<FiberId> {
        while let Some(id) = self.ready_queue.pop_front() {
            let fiber = &mut self.fibers[id.0 as usize];
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
        if let Some(id) = self.current.take() {
            let fiber = &mut self.fibers[id.0 as usize];
            let trap_kind = fiber.panic_trap_kind.take();
            let msg = fiber.panic_message();
            let loc = fiber.current_frame().map(|f| (f.func_id, f.pc as u32));
            fiber.state = FiberState::Dead;
            self.free_slots.push(id.0);
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
            return self.fibers[id.0 as usize].state.is_running();
        }
        false
    }

    /// Check if there are blocked fibers (potential deadlock detection). O(1).
    pub fn has_blocked(&self) -> bool {
        self.blocked_count > 0
    }
    
    /// Current fiber blocks on I/O.
    /// Running -> Blocked(Io(token)).
    #[cfg(feature = "std")]
    pub fn block_for_io(&mut self, token: IoToken) {
        if let Some(id) = self.current.take() {
            let fiber = &mut self.fibers[id.0 as usize];
            fiber.state = FiberState::Blocked(BlockReason::Io(token));
            self.blocked_count += 1;
            self.io_waiters.insert(token, id);
        }
    }
    /// Poll I/O and wake any fibers that are ready.
    /// Returns number of fibers woken.
    #[cfg(feature = "std")]
    pub fn poll_io(&mut self, io: &mut IoRuntime) -> usize {
        let mut woken = 0;

        for token in io.poll() {
            let id = match self.io_waiters.remove(&token) {
                Some(id) => id,
                None => continue,
            };

            let fiber = &mut self.fibers[id.0 as usize];
            fiber.resume_io_token = Some(token);
            if fiber.state.is_blocked() {
                fiber.state = FiberState::Runnable;
                self.blocked_count -= 1;
                self.ready_queue.push_back(id);
                woken += 1;
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
