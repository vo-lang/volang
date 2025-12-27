//! Fiber scheduler for cooperative multitasking.

#[cfg(not(feature = "std"))]
use alloc::{collections::VecDeque, vec::Vec};
#[cfg(feature = "std")]
use std::collections::VecDeque;

use crate::fiber::{Fiber, FiberStatus};

#[derive(Debug)]
pub struct Scheduler {
    /// Fibers indexed by id (id == index).
    pub fibers: Vec<Fiber>,
    /// Free slots from dead fibers, available for reuse.
    free_slots: Vec<u32>,
    pub ready_queue: VecDeque<u32>,
    pub current: Option<u32>,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            fibers: Vec::new(),
            free_slots: Vec::new(),
            ready_queue: VecDeque::new(),
            current: None,
        }
    }

    /// Spawn a new fiber, returns its id.
    /// Reuses dead fiber slots when available.
    pub fn spawn(&mut self, mut fiber: Fiber) -> u32 {
        let id = if let Some(slot) = self.free_slots.pop() {
            // Reuse dead fiber slot
            fiber.id = slot;
            self.fibers[slot as usize] = fiber;
            slot
        } else {
            // Allocate new slot
            let id = self.fibers.len() as u32;
            fiber.id = id;
            self.fibers.push(fiber);
            id
        };
        self.ready_queue.push_back(id);
        id
    }

    /// Get fiber by id (O(1) index access).
    #[inline]
    pub fn fiber(&self, id: u32) -> &Fiber {
        &self.fibers[id as usize]
    }

    /// Get mutable fiber by id (O(1) index access).
    #[inline]
    pub fn fiber_mut(&mut self, id: u32) -> &mut Fiber {
        &mut self.fibers[id as usize]
    }

    /// Get current fiber reference.
    #[inline]
    pub fn current_fiber(&self) -> Option<&Fiber> {
        self.current.map(|id| &self.fibers[id as usize])
    }

    /// Get current fiber mutable reference.
    #[inline]
    pub fn current_fiber_mut(&mut self) -> Option<&mut Fiber> {
        self.current.map(|id| &mut self.fibers[id as usize])
    }

    pub fn wake(&mut self, id: u32) {
        let fiber = &mut self.fibers[id as usize];
        if fiber.status == FiberStatus::Suspended {
            if !self.ready_queue.contains(&id) {
                self.ready_queue.push_back(id);
            }
        }
    }

    pub fn suspend_current(&mut self) {
        if let Some(id) = self.current {
            self.fibers[id as usize].status = FiberStatus::Suspended;
            if !self.ready_queue.contains(&id) {
                self.ready_queue.push_back(id);
            }
        }
    }

    pub fn block_current(&mut self) {
        if let Some(id) = self.current {
            self.fibers[id as usize].status = FiberStatus::Suspended;
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

    pub fn kill_current(&mut self) {
        if let Some(id) = self.current {
            self.fibers[id as usize].status = FiberStatus::Dead;
            self.free_slots.push(id);
        }
    }

    pub fn has_runnable(&self) -> bool {
        !self.ready_queue.is_empty() || self.current.is_some()
    }

}

impl Default for Scheduler {
    fn default() -> Self {
        Self::new()
    }
}
