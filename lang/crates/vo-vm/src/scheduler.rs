//! Fiber scheduler for cooperative multitasking.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, collections::VecDeque, string::String, vec::Vec};
#[cfg(feature = "std")]
use std::collections::{HashMap, VecDeque};

use crate::fiber::{BlockReason, Fiber, FiberState};
use crate::vm::RuntimeTrapKind;
use vo_runtime::objects::queue_state::ChannelWaiter;
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

/// Internal state for a fiber waiting on a host-side event.
#[derive(Debug)]
struct HostEventWaiter {
    token: u64,
    delay_ms: u32,
    replay: bool,
    fiber_id: FiberId,
}

/// Public view of a pending host event for the async run loop.
#[derive(Debug, Clone)]
pub struct PendingHostEvent {
    pub token: u64,
    pub delay_ms: u32,
    pub replay: bool,
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

    /// Fibers waiting for host-side events (timers, fetch Promises).
    host_event_waiters: Vec<HostEventWaiter>,
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
            host_event_waiters: Vec::new(),
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

    /// Reuse a dead fiber (keeping its stack allocation) or create a new one.
    /// Returns the FiberId. The fiber is reset and added to the ready queue.
    /// Caller should set up the fiber's stack, sp, and frames after this call.
    pub fn reuse_or_spawn(&mut self) -> FiberId {
        if let Some(slot) = self.free_slots.pop() {
            let fiber = &mut *self.fibers[slot as usize];
            fiber.reset();
            fiber.id = slot;
            let id = FiberId(slot);
            self.ready_queue.push_back(id);
            id
        } else {
            let id = self.fibers.len() as u32;
            let fiber = Fiber::new(id);
            self.fibers.push(Box::new(fiber));
            let fid = FiberId(id);
            self.ready_queue.push_back(fid);
            fid
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
    /// error_location is (func_id, pc) captured at panic initiation (before frame unwind).
    /// * -> Dead.
    pub fn kill_current(&mut self) -> (Option<RuntimeTrapKind>, Option<String>, Option<(u32, u32)>) {
        if let Some(id) = self.current.take() {
            let fiber = &mut self.fibers[id.0 as usize];
            let trap_kind = fiber.panic_trap_kind.take();
            let msg = fiber.panic_message();
            let loc = fiber.panic_source_loc.take()
                .or_else(|| fiber.current_frame().map(|f| (f.func_id, f.pc as u32)));
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

    /// Block current fiber waiting for a host-side event (e.g. setTimeout).
    /// Running -> Blocked(HostEvent { token, delay_ms }).
    pub fn block_for_host_event(&mut self, token: u64, delay_ms: u32) {
        if let Some(id) = self.current.take() {
            let fiber = &mut self.fibers[id.0 as usize];
            fiber.state = FiberState::Blocked(BlockReason::HostEvent { token, delay_ms });
            self.blocked_count += 1;
            self.host_event_waiters.push(HostEventWaiter { token, delay_ms, replay: false, fiber_id: id });
        }
    }

    /// Block current fiber waiting for a host-side async op result (e.g. fetch Promise).
    /// The extern's PC was already undone by caller; on wake the extern re-executes.
    /// Running -> Blocked(HostEventReplay(token)).
    pub fn block_for_host_event_replay(&mut self, token: u64) {
        if let Some(id) = self.current.take() {
            let fiber = &mut self.fibers[id.0 as usize];
            fiber.state = FiberState::Blocked(BlockReason::HostEventReplay(token));
            self.blocked_count += 1;
            self.host_event_waiters.push(HostEventWaiter { token, delay_ms: 0, replay: true, fiber_id: id });
        }
    }

    /// Wake the fiber waiting for the given host event token.
    /// For replay-style events, sets `resume_host_event_token` on the fiber
    /// so the extern knows it is being re-invoked.
    pub fn wake_host_event(&mut self, token: u64) {
        if let Some(pos) = self.host_event_waiters.iter().position(|w| w.token == token) {
            let waiter = self.host_event_waiters.remove(pos);
            if waiter.replay {
                self.fibers[waiter.fiber_id.0 as usize].resume_host_event_token = Some(token);
            }
            self.wake_fiber(waiter.fiber_id);
        }
    }

    /// Return all pending host event waiters for the async run loop.
    /// Entries remain in the list until individually consumed by `wake_host_event`.
    pub fn take_pending_host_events(&mut self) -> Vec<PendingHostEvent> {
        self.host_event_waiters.iter().map(|w| PendingHostEvent {
            token: w.token,
            delay_ms: w.delay_ms,
            replay: w.replay,
        }).collect()
    }

    /// Check if there are fibers waiting for host-side events.
    pub fn has_host_event_waiters(&self) -> bool {
        !self.host_event_waiters.is_empty()
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

    /// Wake a channel waiter (simple or select).
    ///
    /// For select waiters, sets `woken_index` on the fiber before waking.
    /// Unified entry point used by both VM interpreter and JIT callbacks.
    pub fn wake_channel_waiter(&mut self, waiter: &ChannelWaiter) {
        let fiber_id = FiberId::from_raw(waiter.fiber_id() as u32);
        if let ChannelWaiter::Select(ref sw) = waiter {
            if let Some(ref mut select_state) = self.get_fiber_mut(fiber_id).select_state {
                select_state.woken_index = Some(sw.case_index as usize);
            }
        }
        self.wake_fiber(fiber_id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn block_for_host_event_sets_state_and_records_waiter() {
        let mut scheduler = Scheduler::new();
        let fid = scheduler.spawn(Fiber::new(0));
        scheduler.schedule_next().expect("expected runnable fiber");

        scheduler.block_for_host_event(77, 123);

        assert!(scheduler.current.is_none());
        assert_eq!(scheduler.blocked_count, 1);
        assert!(scheduler.get_fiber(fid).state == FiberState::Blocked(BlockReason::HostEvent { token: 77, delay_ms: 123 }));
        assert_eq!(scheduler.host_event_waiters.len(), 1);
        let waiter = &scheduler.host_event_waiters[0];
        assert_eq!(waiter.token, 77);
        assert_eq!(waiter.delay_ms, 123);
        assert!(!waiter.replay);
    }

    #[test]
    fn block_for_host_event_replay_sets_state_and_records_waiter() {
        let mut scheduler = Scheduler::new();
        let fid = scheduler.spawn(Fiber::new(0));
        scheduler.schedule_next().expect("expected runnable fiber");

        scheduler.block_for_host_event_replay(42);

        assert!(scheduler.current.is_none());
        assert_eq!(scheduler.blocked_count, 1);
        assert!(scheduler.get_fiber(fid).state == FiberState::Blocked(BlockReason::HostEventReplay(42)));
        assert_eq!(scheduler.host_event_waiters.len(), 1);
        assert!(scheduler.host_event_waiters[0].replay);
    }

    #[test]
    fn wake_host_event_transitions_to_runnable() {
        let mut scheduler = Scheduler::new();
        let fid = scheduler.spawn(Fiber::new(0));
        scheduler.schedule_next();

        scheduler.block_for_host_event(99, 0);
        assert_eq!(scheduler.blocked_count, 1);
        assert!(scheduler.ready_queue.is_empty());

        scheduler.wake_host_event(99);
        assert_eq!(scheduler.blocked_count, 0);
        assert!(scheduler.get_fiber(fid).state.is_runnable());
        assert_eq!(scheduler.ready_queue.len(), 1);
    }

    #[test]
    fn wake_host_event_replay_sets_resume_token() {
        let mut scheduler = Scheduler::new();
        let fid = scheduler.spawn(Fiber::new(0));
        scheduler.schedule_next();

        scheduler.block_for_host_event_replay(55);
        scheduler.wake_host_event(55);

        assert_eq!(scheduler.get_fiber(fid).resume_host_event_token, Some(55));
    }
}

impl Default for Scheduler {
    fn default() -> Self {
        Self::new()
    }
}
