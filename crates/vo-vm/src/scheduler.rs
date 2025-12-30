//! Fiber scheduler for cooperative multitasking.

#[cfg(not(feature = "std"))]
use alloc::{collections::VecDeque, vec::Vec};
#[cfg(feature = "std")]
use std::collections::VecDeque;

use crate::fiber::{Fiber, FiberStatus};

/// High bit flag to distinguish trampoline fibers from regular fibers.
pub const TRAMPOLINE_FIBER_FLAG: u32 = 0x8000_0000;

/// Check if a fiber ID is a trampoline fiber.
#[inline]
pub fn is_trampoline_fiber(id: u32) -> bool {
    id & TRAMPOLINE_FIBER_FLAG != 0
}

#[derive(Debug)]
pub struct Scheduler {
    /// Fibers indexed by id (id == index).
    pub fibers: Vec<Fiber>,
    /// Free slots from dead fibers, available for reuse.
    free_slots: Vec<u32>,
    pub ready_queue: VecDeque<u32>,
    pub current: Option<u32>,
    
    /// Trampoline fibers for JIT->VM calls (separate ID space with high bit set).
    pub trampoline_fibers: Vec<Fiber>,
    /// Free slots in trampoline_fibers pool.
    trampoline_free_slots: Vec<u32>,
}

impl Scheduler {
    /// Initial capacity for trampoline fibers.
    const INITIAL_TRAMPOLINE_CAPACITY: usize = 8;
    
    pub fn new() -> Self {
        Scheduler {
            fibers: Vec::new(),
            free_slots: Vec::new(),
            ready_queue: VecDeque::new(),
            current: None,
            trampoline_fibers: Vec::with_capacity(Self::INITIAL_TRAMPOLINE_CAPACITY),
            trampoline_free_slots: Vec::new(),
        }
    }
    
    /// Acquire a trampoline fiber for JIT->VM calls.
    /// Returns fiber ID with high bit set.
    /// 
    /// IMPORTANT: This function must NOT cause Vec reallocation because run_fiber
    /// caches raw pointers to fibers. We ensure this by reserving capacity before push.
    pub fn acquire_trampoline_fiber(&mut self) -> u32 {
        let index = if let Some(slot) = self.trampoline_free_slots.pop() {
            // Reuse existing fiber
            let fiber = &mut self.trampoline_fibers[slot as usize];
            fiber.reset();
            slot
        } else {
            // Create new fiber - MUST reserve before push to prevent reallocation
            let index = self.trampoline_fibers.len() as u32;
            if self.trampoline_fibers.len() >= self.trampoline_fibers.capacity() {
                // Double capacity or add 16, whichever is larger
                let additional = self.trampoline_fibers.capacity().max(16);
                self.trampoline_fibers.reserve(additional);
            }
            let mut fiber = Fiber::new(TRAMPOLINE_FIBER_FLAG | index);
            fiber.status = FiberStatus::Running;
            self.trampoline_fibers.push(fiber);
            index
        };
        TRAMPOLINE_FIBER_FLAG | index
    }
    
    /// Release a trampoline fiber back to the pool.
    pub fn release_trampoline_fiber(&mut self, id: u32) {
        debug_assert!(is_trampoline_fiber(id));
        let index = id & !TRAMPOLINE_FIBER_FLAG;
        self.trampoline_free_slots.push(index);
    }
    
    /// Get trampoline fiber by ID.
    #[inline]
    pub fn trampoline_fiber(&self, id: u32) -> &Fiber {
        debug_assert!(is_trampoline_fiber(id));
        let index = (id & !TRAMPOLINE_FIBER_FLAG) as usize;
        &self.trampoline_fibers[index]
    }
    
    /// Get mutable trampoline fiber by ID.
    #[inline]
    pub fn trampoline_fiber_mut(&mut self, id: u32) -> &mut Fiber {
        debug_assert!(is_trampoline_fiber(id));
        let index = (id & !TRAMPOLINE_FIBER_FLAG) as usize;
        &mut self.trampoline_fibers[index]
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
