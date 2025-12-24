//! Fiber scheduler for cooperative multitasking.

#[cfg(not(feature = "std"))]
use alloc::{collections::VecDeque, vec::Vec};
#[cfg(feature = "std")]
use std::collections::VecDeque;

use crate::fiber::{Fiber, FiberStatus};

#[derive(Debug)]
pub struct Scheduler {
    /// Fibers indexed by id (id == index). Never removed, only marked dead.
    pub fibers: Vec<Fiber>,
    pub ready_queue: VecDeque<u32>,
    pub current: Option<u32>,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            fibers: Vec::new(),
            ready_queue: VecDeque::new(),
            current: None,
        }
    }

    /// Spawn a new fiber, returns its id (which equals its index in fibers vec).
    pub fn spawn(&mut self, mut fiber: Fiber) -> u32 {
        let id = self.fibers.len() as u32;
        fiber.id = id;
        self.fibers.push(fiber);
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
        }
    }

    pub fn has_runnable(&self) -> bool {
        !self.ready_queue.is_empty() || self.current.is_some()
    }

    /// Compact dead fibers when too many accumulate.
    /// Returns true if compaction happened.
    pub fn maybe_compact(&mut self) -> bool {
        let dead_count = self.fibers.iter().filter(|f| f.status == FiberStatus::Dead).count();
        if dead_count > 64 && dead_count > self.fibers.len() / 2 {
            self.compact();
            true
        } else {
            false
        }
    }

    /// Remove dead fibers and rebuild indices.
    fn compact(&mut self) {
        // Build old_id -> new_id mapping
        let mut new_fibers = Vec::new();
        let mut id_map = vec![u32::MAX; self.fibers.len()];
        
        for (old_id, fiber) in self.fibers.drain(..).enumerate() {
            if fiber.status != FiberStatus::Dead {
                let new_id = new_fibers.len() as u32;
                id_map[old_id] = new_id;
                let mut f = fiber;
                f.id = new_id;
                new_fibers.push(f);
            }
        }
        
        self.fibers = new_fibers;
        
        // Update ready_queue
        self.ready_queue.retain(|id| id_map[*id as usize] != u32::MAX);
        for id in self.ready_queue.iter_mut() {
            *id = id_map[*id as usize];
        }
        
        // Update current
        if let Some(id) = self.current {
            let new_id = id_map[id as usize];
            self.current = if new_id != u32::MAX { Some(new_id) } else { None };
        }
    }
}

impl Default for Scheduler {
    fn default() -> Self {
        Self::new()
    }
}
