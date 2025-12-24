//! Fiber scheduler for cooperative multitasking.

#[cfg(not(feature = "std"))]
use alloc::{collections::VecDeque, vec::Vec};
#[cfg(feature = "std")]
use std::collections::VecDeque;

use crate::fiber::{Fiber, FiberStatus};

#[derive(Debug)]
pub struct Scheduler {
    pub fibers: Vec<Fiber>,
    pub ready_queue: VecDeque<u32>,
    pub current: Option<u32>,
    next_id: u32,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            fibers: Vec::new(),
            ready_queue: VecDeque::new(),
            current: None,
            next_id: 0,
        }
    }

    pub fn next_fiber_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    pub fn spawn(&mut self, fiber: Fiber) -> u32 {
        let id = fiber.id;
        self.fibers.push(fiber);
        self.ready_queue.push_back(id);
        id
    }

    pub fn get_fiber(&self, id: u32) -> Option<&Fiber> {
        self.fibers.iter().find(|f| f.id == id)
    }

    pub fn get_fiber_mut(&mut self, id: u32) -> Option<&mut Fiber> {
        self.fibers.iter_mut().find(|f| f.id == id)
    }

    pub fn current_fiber(&self) -> Option<&Fiber> {
        self.current.and_then(|id| self.get_fiber(id))
    }

    pub fn current_fiber_mut(&mut self) -> Option<&mut Fiber> {
        self.current.and_then(|id| self.fibers.iter_mut().find(|f| f.id == id))
    }

    pub fn wake(&mut self, id: u32) {
        if let Some(fiber) = self.get_fiber_mut(id) {
            if fiber.status == FiberStatus::Suspended {
                fiber.status = FiberStatus::Suspended;
                if !self.ready_queue.contains(&id) {
                    self.ready_queue.push_back(id);
                }
            }
        }
    }

    pub fn suspend_current(&mut self) {
        if let Some(id) = self.current {
            if let Some(fiber) = self.get_fiber_mut(id) {
                fiber.status = FiberStatus::Suspended;
            }
            if !self.ready_queue.contains(&id) {
                self.ready_queue.push_back(id);
            }
        }
    }

    pub fn schedule_next(&mut self) -> Option<u32> {
        if let Some(id) = self.ready_queue.pop_front() {
            if let Some(fiber) = self.get_fiber_mut(id) {
                if fiber.status != FiberStatus::Dead {
                    fiber.status = FiberStatus::Running;
                    self.current = Some(id);
                    return Some(id);
                }
            }
            self.schedule_next()
        } else {
            self.current = None;
            None
        }
    }

    pub fn kill_current(&mut self) {
        if let Some(id) = self.current {
            if let Some(fiber) = self.get_fiber_mut(id) {
                fiber.status = FiberStatus::Dead;
            }
        }
    }

    pub fn has_runnable(&self) -> bool {
        !self.ready_queue.is_empty() || self.current.is_some()
    }

    pub fn remove_dead_fibers(&mut self) {
        self.fibers.retain(|f| f.status != FiberStatus::Dead);
    }
}

impl Default for Scheduler {
    fn default() -> Self {
        Self::new()
    }
}
