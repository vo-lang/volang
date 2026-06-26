//! Fiber scheduler for cooperative multitasking.
#![allow(clippy::items_after_test_module)]

#[cfg(not(feature = "std"))]
use alloc::{
    boxed::Box,
    collections::VecDeque,
    format,
    string::{String, ToString},
    vec::Vec,
};
#[cfg(feature = "std")]
use std::collections::{HashMap, VecDeque};

use crate::fiber::{BlockReason, Fiber, FiberState, SelectState, SelectWokenResult};
use crate::vm::RuntimeTrapKind;
use vo_runtime::ffi::HostEventReplaySource;
#[cfg(feature = "std")]
use vo_runtime::io::{IoRuntime, IoToken};
use vo_runtime::objects::queue_state::QueueWaiter;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FiberWakeKey {
    pub slot: u32,
    pub generation: u32,
}

impl FiberWakeKey {
    #[inline]
    pub fn new(slot: u32, generation: u32) -> Self {
        Self { slot, generation }
    }

    #[inline]
    pub fn from_packed(key: u64) -> Self {
        Self {
            slot: key as u32,
            generation: (key >> 32) as u32,
        }
    }

    #[inline]
    pub fn as_packed(self) -> u64 {
        ((self.generation as u64) << 32) | self.slot as u64
    }

    #[inline]
    pub fn fiber_id(self) -> FiberId {
        FiberId::from_raw(self.slot)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WaitSource {
    Queue,
    Select,
    Io,
    HostEvent,
    HostEventReplay,
    IslandEndpoint,
    IslandWake,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct WaitRegistrationKey {
    pub token: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum HostWaitSource {
    Timer,
    Replay(HostEventReplaySource),
}

impl HostWaitSource {
    #[inline]
    pub fn replay(source: HostEventReplaySource) -> Self {
        Self::Replay(source)
    }

    #[inline]
    pub fn is_replay(self) -> bool {
        matches!(self, Self::Replay(_))
    }

    #[inline]
    pub fn is_gui_event_replay(self) -> bool {
        matches!(self, Self::Replay(source) if source.is_gui_event())
    }

    #[inline]
    pub fn wait_source(self) -> WaitSource {
        match self {
            Self::Timer => WaitSource::HostEvent,
            Self::Replay(_) => WaitSource::HostEventReplay,
        }
    }

    #[inline]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Timer => "timer",
            Self::Replay(source) => match source {
                HostEventReplaySource::GuiEvent => "replay-gui-event",
                HostEventReplaySource::Fetch => "replay-fetch",
                HostEventReplaySource::Extension => "replay-extension",
            },
        }
    }

    pub fn parse(value: &str) -> Option<Self> {
        match value {
            "timer" => Some(Self::Timer),
            "replay" | "replay-gui-event" => Some(Self::Replay(HostEventReplaySource::GuiEvent)),
            "replay-fetch" => Some(Self::Replay(HostEventReplaySource::Fetch)),
            "replay-extension" => Some(Self::Replay(HostEventReplaySource::Extension)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HostWaitKey {
    pub source: HostWaitSource,
    pub token: u64,
    pub wake_key: FiberWakeKey,
    pub registration: WaitRegistrationKey,
}

impl HostWaitKey {
    pub fn encode(self) -> String {
        format!(
            "{}:{}:{}:{}",
            self.source.as_str(),
            self.token,
            self.wake_key.as_packed(),
            self.registration.token
        )
    }

    pub fn decode(encoded: &str) -> Result<Self, String> {
        let mut parts = encoded.split(':');
        let Some(source) = parts.next().and_then(HostWaitSource::parse) else {
            return Err("invalid host wait key source".to_string());
        };
        let Some(token) = parts.next().and_then(|part| part.parse::<u64>().ok()) else {
            return Err("invalid host wait key token".to_string());
        };
        let Some(wake_key) = parts
            .next()
            .and_then(|part| part.parse::<u64>().ok())
            .map(FiberWakeKey::from_packed)
        else {
            return Err("invalid host wait key fiber key".to_string());
        };
        let Some(registration) = parts.next().and_then(|part| part.parse::<u64>().ok()) else {
            return Err("invalid host wait key registration".to_string());
        };
        if parts.next().is_some() {
            return Err("invalid host wait key: too many fields".to_string());
        }
        Ok(Self {
            source,
            token,
            wake_key,
            registration: WaitRegistrationKey {
                token: registration,
            },
        })
    }
}

#[cfg(feature = "std")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct IoWaitKey {
    pub token: IoToken,
    pub wake_key: FiberWakeKey,
    pub registration: WaitRegistrationKey,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WaitRegistration {
    pub source: WaitSource,
    pub wake_key: FiberWakeKey,
    pub registration_key: WaitRegistrationKey,
}

pub(crate) struct FiberLease<'a> {
    #[allow(dead_code)]
    pub(crate) key: FiberWakeKey,
    fiber: &'a mut Fiber,
}

impl<'a> FiberLease<'a> {
    #[inline]
    #[allow(dead_code)]
    pub(crate) fn fiber(&self) -> &Fiber {
        self.fiber
    }

    #[inline]
    pub(crate) fn fiber_mut(&mut self) -> &mut Fiber {
        self.fiber
    }
}

/// Internal state for a fiber waiting on a host-side event.
#[derive(Debug)]
struct HostEventWaiter {
    key: HostWaitKey,
    delay_ms: u32,
}

/// Public view of a pending host event for the async run loop.
#[derive(Debug, Clone)]
pub struct PendingHostEvent {
    pub key: HostWaitKey,
    pub source: HostWaitSource,
    pub token: u64,
    pub delay_ms: u32,
    pub replay: bool,
}

#[derive(Debug)]
pub(crate) struct Scheduler {
    /// Fibers indexed by id (id == index).
    /// Box<Fiber> ensures stable addresses - Vec reallocation won't invalidate fiber pointers.
    pub(crate) fibers: Vec<Box<Fiber>>,
    /// Free slots from dead fibers, available for reuse.
    free_slots: Vec<u32>,
    pub(crate) ready_queue: VecDeque<FiberId>,
    pub(crate) current: Option<FiberId>,
    /// Number of fibers currently in Blocked state (O(1) has_blocked).
    blocked_count: u32,

    /// Map from I/O token to waiting fiber registration.
    #[cfg(feature = "std")]
    io_waiters: HashMap<IoToken, WaitRegistration>,

    /// Fibers waiting for host-side events (timers, fetch Promises).
    host_event_waiters: Vec<HostEventWaiter>,
    next_wait_registration_token: u64,
}

impl Scheduler {
    pub(crate) fn new() -> Self {
        Scheduler {
            fibers: Vec::new(),
            free_slots: Vec::new(),
            ready_queue: VecDeque::new(),
            current: None,
            blocked_count: 0,
            #[cfg(feature = "std")]
            io_waiters: HashMap::new(),
            host_event_waiters: Vec::new(),
            next_wait_registration_token: 1,
        }
    }

    /// Spawn a new fiber, returns its FiberId.
    /// Reuses dead fiber slots when available.
    pub(crate) fn spawn(&mut self, fiber: Fiber) -> FiberId {
        let id = self.spawn_not_ready(fiber);
        self.ready_queue.push_back(id);
        id
    }

    /// Spawn a new fiber without adding to ready_queue.
    fn spawn_not_ready(&mut self, mut fiber: Fiber) -> FiberId {
        if let Some(slot) = self.free_slots.pop() {
            let generation = Self::next_fiber_generation(self.fibers[slot as usize].generation);
            fiber.id = slot;
            fiber.generation = generation;
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
    pub(crate) fn reuse_or_spawn(&mut self) -> FiberId {
        if let Some(slot) = self.free_slots.pop() {
            let fiber = &mut *self.fibers[slot as usize];
            let generation = Self::next_fiber_generation(fiber.generation);
            fiber.reset();
            fiber.id = slot;
            fiber.generation = generation;
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

    #[inline]
    fn next_fiber_generation(current: u32) -> u32 {
        match current.wrapping_add(1) {
            0 => 1,
            next => next,
        }
    }

    /// Get fiber by FiberId (O(1) index access).
    #[inline]
    #[allow(dead_code)]
    pub(crate) fn get_fiber(&self, id: FiberId) -> &Fiber {
        &self.fibers[id.0 as usize]
    }

    /// Try to get fiber by FiberId. Use at host/island command boundaries
    /// where a raw id may be stale or malformed.
    #[inline]
    pub(crate) fn try_get_fiber(&self, id: FiberId) -> Option<&Fiber> {
        self.fibers.get(id.0 as usize).map(|fiber| &**fiber)
    }

    /// Get mutable fiber by FiberId (O(1) index access).
    #[inline]
    pub(crate) fn get_fiber_mut(&mut self, id: FiberId) -> &mut Fiber {
        &mut self.fibers[id.0 as usize]
    }

    /// Try to get mutable fiber by FiberId. Use at host/island command
    /// boundaries where a raw id may be stale or malformed.
    #[inline]
    pub(crate) fn try_get_fiber_mut(&mut self, id: FiberId) -> Option<&mut Fiber> {
        self.fibers.get_mut(id.0 as usize).map(Box::as_mut)
    }

    pub(crate) fn lease_fiber(&mut self, id: FiberId) -> Option<FiberLease<'_>> {
        let fiber = self.fibers.get_mut(id.0 as usize)?;
        let key = FiberWakeKey::new(id.to_raw(), fiber.generation);
        Some(FiberLease { key, fiber })
    }

    /// Try to get a fiber by an opaque endpoint-response key.
    ///
    /// The low 32 bits are the scheduler slot and the high 32 bits are the
    /// fiber generation. This keeps stale cross-island endpoint responses from
    /// waking a different fiber after slot reuse.
    #[inline]
    pub(crate) fn try_get_fiber_mut_by_endpoint_response_key(
        &mut self,
        key: u64,
    ) -> Option<&mut Fiber> {
        self.try_get_fiber_mut_by_wake_key(FiberWakeKey::from_packed(key))
    }

    #[inline]
    pub(crate) fn try_get_fiber_by_endpoint_response_key(&self, key: u64) -> Option<&Fiber> {
        self.try_get_fiber_by_wake_key(FiberWakeKey::from_packed(key))
    }

    #[inline]
    pub(crate) fn try_get_fiber_by_wake_key(&self, key: FiberWakeKey) -> Option<&Fiber> {
        let fiber = self.fibers.get(key.slot as usize)?;
        if fiber.generation == key.generation {
            Some(&**fiber)
        } else {
            None
        }
    }

    #[inline]
    pub(crate) fn try_get_fiber_mut_by_wake_key(
        &mut self,
        key: FiberWakeKey,
    ) -> Option<&mut Fiber> {
        let fiber = self.fibers.get_mut(key.slot as usize)?;
        if fiber.generation == key.generation {
            Some(&mut **fiber)
        } else {
            None
        }
    }

    /// Try to wake a blocked fiber. Returns false if the id is unknown or the
    /// fiber was not blocked.
    pub(crate) fn try_wake_fiber(&mut self, id: FiberId) -> bool {
        let Some(fiber) = self.fibers.get_mut(id.0 as usize) else {
            return false;
        };
        if fiber.state.is_blocked() {
            fiber.state = FiberState::Runnable;
            self.blocked_count -= 1;
            self.ready_queue.push_back(id);
            return true;
        }
        false
    }

    /// Get current fiber mutable reference.
    #[inline]
    #[allow(dead_code)]
    pub(crate) fn current_fiber_mut(&mut self) -> Option<&mut Fiber> {
        self.current.map(|id| &mut *self.fibers[id.0 as usize])
    }

    /// Current fiber yields CPU, remains runnable.
    /// Running -> Runnable (back to ready_queue).
    pub(crate) fn yield_current(&mut self) {
        if let Some(id) = self.current.take() {
            let fiber = &mut self.fibers[id.0 as usize];
            fiber.state = FiberState::Runnable;
            self.ready_queue.push_back(id);
        }
    }

    /// Current fiber blocks on queue (channel send/recv).
    /// Running -> Blocked(Queue).
    pub(crate) fn block_for_queue(&mut self) {
        if let Some(id) = self.current.take() {
            let fiber = &mut self.fibers[id.0 as usize];
            fiber.state = FiberState::Blocked(BlockReason::Queue);
            self.blocked_count += 1;
        }
    }

    /// Pick next runnable fiber and set it to Running.
    /// Runnable -> Running.
    pub(crate) fn schedule_next(&mut self) -> Option<FiberId> {
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
    pub(crate) fn kill_current(
        &mut self,
    ) -> (Option<RuntimeTrapKind>, Option<String>, Option<(u32, u32)>) {
        if let Some(id) = self.current.take() {
            let fiber = &mut self.fibers[id.0 as usize];
            let trap_kind = fiber.panic_trap_kind.take();
            let msg = fiber.panic_message();
            let loc = fiber
                .panic_source_loc
                .take()
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
    pub(crate) fn has_work(&self) -> bool {
        if !self.ready_queue.is_empty() {
            return true;
        }
        if let Some(id) = self.current {
            return self.fibers[id.0 as usize].state.is_running();
        }
        false
    }

    /// Check if there are blocked fibers (potential deadlock detection). O(1).
    pub(crate) fn has_blocked(&self) -> bool {
        self.blocked_count > 0
    }

    /// Block current fiber waiting for a host-side event (e.g. setTimeout).
    /// Running -> Blocked(HostEvent { token, delay_ms }).
    pub(crate) fn block_for_host_event(&mut self, token: u64, delay_ms: u32) {
        if let Some(id) = self.current.take() {
            let fiber = &mut self.fibers[id.0 as usize];
            fiber.state = FiberState::Blocked(BlockReason::HostEvent { token, delay_ms });
            self.blocked_count += 1;
            let generation = fiber.generation;
            let registration = self.next_wait_registration(id, generation, WaitSource::HostEvent);
            self.host_event_waiters.push(HostEventWaiter {
                key: HostWaitKey {
                    source: HostWaitSource::Timer,
                    token,
                    wake_key: registration.wake_key,
                    registration: registration.registration_key,
                },
                delay_ms,
            });
        }
    }

    /// Block current fiber waiting for a source-specific host-side async op result.
    /// The extern's PC was already undone by caller; on wake the extern re-executes.
    /// Running -> Blocked(HostEventReplay { token, source }).
    pub(crate) fn block_for_host_event_replay(
        &mut self,
        token: u64,
        source: HostEventReplaySource,
    ) {
        if let Some(id) = self.current.take() {
            let fiber = &mut self.fibers[id.0 as usize];
            fiber.state = FiberState::Blocked(BlockReason::HostEventReplay { token, source });
            self.blocked_count += 1;
            let generation = fiber.generation;
            let registration =
                self.next_wait_registration(id, generation, WaitSource::HostEventReplay);
            self.host_event_waiters.push(HostEventWaiter {
                key: HostWaitKey {
                    source: HostWaitSource::Replay(source),
                    token,
                    wake_key: registration.wake_key,
                    registration: registration.registration_key,
                },
                delay_ms: 0,
            });
        }
    }

    /// Find the complete wait key for a source-specific host token.
    pub(crate) fn host_event_key(&self, source: HostWaitSource, token: u64) -> Option<HostWaitKey> {
        self.host_event_waiters
            .iter()
            .find(|waiter| waiter.key.source == source && waiter.key.token == token)
            .map(|waiter| waiter.key)
    }

    /// Wake the fiber waiting for the given complete host wait key.
    pub(crate) fn wake_host_event(&mut self, key: HostWaitKey) -> bool {
        if let Some(pos) = self
            .host_event_waiters
            .iter()
            .position(|w| w.key == key && self.host_event_waiter_matches(w, false))
        {
            let waiter = self.host_event_waiters.remove(pos);
            return self.apply_host_event_wake(waiter, None);
        }
        false
    }

    /// Wake the fiber waiting for the given host event key, attaching opaque data.
    /// The FFI function reads the data on replay via `ctx.take_resume_host_event_data()`.
    pub(crate) fn wake_host_event_with_data(&mut self, key: HostWaitKey, data: Vec<u8>) -> bool {
        if let Some(pos) = self
            .host_event_waiters
            .iter()
            .position(|w| w.key == key && self.host_event_waiter_matches(w, true))
        {
            let waiter = self.host_event_waiters.remove(pos);
            return self.apply_host_event_wake(waiter, Some(data));
        }
        false
    }

    fn next_wait_registration(
        &mut self,
        id: FiberId,
        generation: u32,
        source: WaitSource,
    ) -> WaitRegistration {
        let token = match self.next_wait_registration_token {
            0 => 1,
            token => token,
        };
        self.next_wait_registration_token = match token.wrapping_add(1) {
            0 => 1,
            next => next,
        };
        WaitRegistration {
            source,
            wake_key: FiberWakeKey::new(id.to_raw(), generation),
            registration_key: WaitRegistrationKey { token },
        }
    }

    fn host_event_waiter_matches(&self, waiter: &HostEventWaiter, data_wake: bool) -> bool {
        let id = waiter.key.wake_key.fiber_id();
        let Some(fiber) = self.fibers.get(id.0 as usize) else {
            return false;
        };
        if fiber.generation != waiter.key.wake_key.generation {
            return false;
        }
        if data_wake && !waiter.key.source.is_replay() {
            return false;
        }
        match (&fiber.state, waiter.key.source) {
            (FiberState::Blocked(BlockReason::HostEvent { token, .. }), HostWaitSource::Timer) => {
                *token == waiter.key.token
            }
            (
                FiberState::Blocked(BlockReason::HostEventReplay { token, source }),
                HostWaitSource::Replay(wait_source),
            ) => *token == waiter.key.token && *source == wait_source,
            _ => false,
        }
    }

    fn apply_host_event_wake(&mut self, waiter: HostEventWaiter, data: Option<Vec<u8>>) -> bool {
        let data_wake = data.is_some();
        let id = waiter.key.wake_key.fiber_id();
        let Some(fiber) = self.fibers.get_mut(id.0 as usize) else {
            return false;
        };
        if fiber.generation != waiter.key.wake_key.generation {
            return false;
        }
        let source_matches = match (&fiber.state, waiter.key.source) {
            (FiberState::Blocked(BlockReason::HostEvent { token, .. }), HostWaitSource::Timer) => {
                *token == waiter.key.token
            }
            (
                FiberState::Blocked(BlockReason::HostEventReplay { token, source }),
                HostWaitSource::Replay(wait_source),
            ) => *token == waiter.key.token && *source == wait_source,
            _ => false,
        };
        if !source_matches {
            return false;
        }

        if waiter.key.source.is_replay() {
            fiber.resume_host_event_token = Some(waiter.key.token);
            if let Some(data) = data {
                fiber.resume_host_event_data = Some(data);
            }
        }
        fiber.state = FiberState::Runnable;
        self.blocked_count -= 1;
        self.ready_queue.push_back(id);

        if data_wake {
            waiter.key.source.is_replay()
        } else {
            true
        }
    }

    /// Return all pending host event waiters for the async run loop.
    /// Entries remain in the list until individually consumed by `wake_host_event`.
    pub(crate) fn take_pending_host_events(&mut self) -> Vec<PendingHostEvent> {
        self.host_event_waiters
            .iter()
            .map(|w| PendingHostEvent {
                key: w.key,
                source: w.key.source,
                token: w.key.token,
                delay_ms: w.delay_ms,
                replay: w.key.source.is_replay(),
            })
            .collect()
    }

    /// Check if there are fibers waiting for host-side events.
    pub(crate) fn has_host_event_waiters(&self) -> bool {
        !self.host_event_waiters.is_empty()
    }

    /// Current fiber blocks on I/O.
    /// Running -> Blocked(Io(token)).
    #[cfg(feature = "std")]
    pub(crate) fn block_for_io(&mut self, token: IoToken) {
        if let Some(id) = self.current.take() {
            let fiber = &mut self.fibers[id.0 as usize];
            fiber.state = FiberState::Blocked(BlockReason::Io(token));
            self.blocked_count += 1;
            let generation = fiber.generation;
            let registration = self.next_wait_registration(id, generation, WaitSource::Io);
            self.io_waiters.insert(token, registration);
        }
    }
    /// Poll I/O and return ready tokens. The runtime command bridge owns
    /// consuming registrations and waking fibers.
    #[cfg(feature = "std")]
    pub(crate) fn poll_io_ready_tokens(&mut self, io: &mut IoRuntime) -> Vec<IoToken> {
        io.poll()
    }

    #[cfg(feature = "std")]
    pub(crate) fn io_wait_key(&self, token: IoToken) -> Option<IoWaitKey> {
        let registration = self.io_waiters.get(&token)?;
        if registration.source != WaitSource::Io {
            return None;
        }
        Some(IoWaitKey {
            token,
            wake_key: registration.wake_key,
            registration: registration.registration_key,
        })
    }

    /// Consume an I/O wait registration and wake the matching fiber.
    #[cfg(feature = "std")]
    pub(crate) fn wake_io(&mut self, key: IoWaitKey) -> bool {
        let token = key.token;
        let Some(registration) = self.io_waiters.get(&token).copied() else {
            return false;
        };
        if registration.source != WaitSource::Io
            || registration.wake_key != key.wake_key
            || registration.registration_key != key.registration
        {
            return false;
        }

        let id = key.wake_key.fiber_id();
        let Some(fiber) = self.try_get_fiber_mut_by_wake_key(key.wake_key) else {
            return false;
        };
        if !matches!(fiber.state, FiberState::Blocked(BlockReason::Io(wait_token)) if wait_token == token)
        {
            return false;
        }

        fiber.resume_io_token = Some(token);
        fiber.state = FiberState::Runnable;
        self.io_waiters.remove(&token);
        self.blocked_count -= 1;
        self.ready_queue.push_back(id);
        true
    }

    /// Compatibility helper for tests and probes that only have the raw token.
    #[cfg(feature = "std")]
    #[cfg(test)]
    pub(crate) fn wake_io_token(&mut self, token: IoToken) -> bool {
        self.io_wait_key(token).is_some_and(|key| self.wake_io(key))
    }

    /// Check if there are fibers waiting on I/O.
    #[cfg(feature = "std")]
    pub(crate) fn has_io_waiters(&self) -> bool {
        !self.io_waiters.is_empty()
    }

    /// Wake a channel waiter (simple or select).
    ///
    /// For select waiters, sets `woken_index` on the fiber before waking.
    /// Unified entry point used by both VM interpreter and JIT callbacks.
    pub(crate) fn wake_queue_waiter(&mut self, waiter: &QueueWaiter) -> bool {
        self.wake_queue_waiter_with_result(waiter, None)
    }

    pub(crate) fn can_wake_queue_waiter_with_result(
        &self,
        waiter: &QueueWaiter,
        select_result: Option<&SelectWokenResult>,
    ) -> bool {
        let key = FiberWakeKey::from_packed(waiter.fiber_key());
        let Some(fiber) = self.try_get_fiber_by_wake_key(key) else {
            return false;
        };
        if !matches!(fiber.state, FiberState::Blocked(BlockReason::Queue)) {
            return false;
        }
        if let Some(select) = waiter.select.as_ref() {
            let Some(ref select_state) = fiber.select_state else {
                return false;
            };
            waiter.queue_ref == select.queue_ref
                && waiter.kind == Some(select.kind)
                && Self::select_waiter_matches(select_state, select)
        } else {
            fiber.select_state.is_none()
                && select_result.is_none()
                && fiber.queue_wait_matches(waiter)
        }
    }

    pub(crate) fn wake_queue_waiter_with_result(
        &mut self,
        waiter: &QueueWaiter,
        select_result: Option<SelectWokenResult>,
    ) -> bool {
        let key = FiberWakeKey::from_packed(waiter.fiber_key());
        let fiber_id = key.fiber_id();
        let was_blocked = {
            let Some(fiber) = self.try_get_fiber_mut_by_wake_key(key) else {
                return false;
            };
            if !matches!(fiber.state, FiberState::Blocked(BlockReason::Queue)) {
                return false;
            }
            if let Some(select) = waiter.select.as_ref() {
                let Some(ref mut select_state) = fiber.select_state else {
                    return false;
                };
                if waiter.queue_ref != select.queue_ref || waiter.kind != Some(select.kind) {
                    return false;
                }
                if !Self::select_waiter_matches(select_state, select) {
                    return false;
                }
                select_state.woken_index = Some(select.case_index as usize);
                select_state.woken_result = select_result;
            } else if fiber.select_state.is_some() {
                return false;
            } else if select_result.is_some() {
                return false;
            } else if !fiber.queue_wait_matches(waiter) {
                return false;
            } else {
                fiber.clear_queue_wait();
            }
            fiber.state = FiberState::Runnable;
            true
        };
        if was_blocked {
            self.blocked_count -= 1;
            self.ready_queue.push_back(fiber_id);
        }
        was_blocked
    }

    /// Wake a blocked sender because its channel was closed.
    ///
    /// Simple sends mark `remote_send_closed`; the VM runtime boundary owns
    /// replaying QueueSend after the wake is accepted. Select sends resume
    /// SelectExec and validate the selected case against the closed queue.
    pub(crate) fn can_wake_queue_sender_closed(&self, waiter: &QueueWaiter) -> bool {
        let key = FiberWakeKey::from_packed(waiter.fiber_key());
        let Some(fiber) = self.try_get_fiber_by_wake_key(key) else {
            return false;
        };
        if !matches!(fiber.state, FiberState::Blocked(BlockReason::Queue)) {
            return false;
        }
        if let Some(select) = waiter.select.as_ref() {
            let Some(ref select_state) = fiber.select_state else {
                return false;
            };
            waiter.queue_ref == select.queue_ref
                && waiter.kind == Some(select.kind)
                && Self::select_waiter_matches(select_state, select)
        } else {
            fiber.select_state.is_none() && fiber.queue_wait_matches(waiter)
        }
    }

    pub(crate) fn wake_queue_sender_closed(
        &mut self,
        waiter: &QueueWaiter,
    ) -> Result<bool, String> {
        let key = FiberWakeKey::from_packed(waiter.fiber_key());
        let fiber_id = key.fiber_id();
        let was_blocked = {
            let Some(fiber) = self.try_get_fiber_mut_by_wake_key(key) else {
                return Ok(false);
            };
            if !matches!(fiber.state, FiberState::Blocked(BlockReason::Queue)) {
                return Ok(false);
            }
            if let Some(select) = waiter.select.as_ref() {
                let Some(ref mut select_state) = fiber.select_state else {
                    return Ok(false);
                };
                if waiter.queue_ref != select.queue_ref || waiter.kind != Some(select.kind) {
                    return Ok(false);
                }
                if !Self::select_waiter_matches(select_state, select) {
                    return Ok(false);
                }
                select_state.woken_index = Some(select.case_index as usize);
            } else if fiber.select_state.is_some() {
                return Ok(false);
            } else if !fiber.queue_wait_matches(waiter) {
                return Ok(false);
            } else {
                fiber.remote_send_closed = true;
                fiber.clear_queue_wait();
            }
            fiber.state = FiberState::Runnable;
            true
        };
        if !was_blocked {
            return Ok(false);
        }
        self.blocked_count -= 1;
        self.ready_queue.push_back(fiber_id);
        Ok(true)
    }

    fn select_waiter_matches(
        select_state: &SelectState,
        select: &vo_runtime::objects::queue_state::SelectInfo,
    ) -> bool {
        if select_state.select_id != select.select_id {
            return false;
        }
        let Some(case) = select_state.cases.get(select.case_index as usize) else {
            return false;
        };
        if case.kind.wait_kind() != select.kind {
            return false;
        }
        select_state.registered_queues.iter().any(|registered| {
            registered.case_index == select.case_index
                && registered.queue as u64 == select.queue_ref
                && registered.kind == case.kind
        })
    }
}

#[cfg(test)]
mod tests;

impl Default for Scheduler {
    fn default() -> Self {
        Self::new()
    }
}
