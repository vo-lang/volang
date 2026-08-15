use crate::fiber::{BlockReason, Fiber, PendingSpawn, SelectState, SelectWokenResult};
#[cfg(feature = "std")]
use crate::scheduler::IoWaitKey;
use crate::scheduler::{FiberId, FiberWakeKey, HostWaitKey};
use crate::vm::{
    scheduler_error_to_vm_error, EndpointRegistryUndo, ExecResult, GcRootEffect, SchedulingOutcome,
    Vm, VmError, VmState,
};
use hashbrown::{HashMap, HashSet};
use vo_runtime::gc::GcRef;
use vo_runtime::island::{
    EndpointRequestKind, EndpointResponseKind, EndpointWaitKey, IslandCommand,
};
#[cfg(feature = "std")]
use vo_runtime::island_transport::IslandSendReservation;
use vo_runtime::objects::{
    queue,
    queue_state::{QueueMessage, QueueWaitTarget, QueueWaiter, SelectWaitKind},
};

#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(test)]
use std::collections::VecDeque;
#[cfg(feature = "std")]
use std::string::String;
#[cfg(feature = "std")]
use std::vec::Vec;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResumePolicy {
    PreserveFramePc,
    NextInstruction { pc: u32 },
    ReplayCurrentInstruction { pc: u32 },
    MaterializeAt { pc: u32 },
}

impl ResumePolicy {
    pub(crate) fn next_after(fetched_pc: u32, context: &str) -> Result<Self, String> {
        let pc = fetched_pc
            .checked_add(1)
            .ok_or_else(|| format!("{context} next-instruction pc overflow at {fetched_pc}"))?;
        Ok(Self::NextInstruction { pc })
    }

    pub(crate) fn replay_current(fetched_pc: u32) -> Self {
        Self::ReplayCurrentInstruction { pc: fetched_pc }
    }

    pub(crate) fn requires_frame(self) -> bool {
        !matches!(self, Self::PreserveFramePc)
    }

    fn target_pc(self) -> Option<u32> {
        match self {
            Self::PreserveFramePc => None,
            Self::NextInstruction { pc }
            | Self::ReplayCurrentInstruction { pc }
            | Self::MaterializeAt { pc } => Some(pc),
        }
    }
}

pub(crate) fn frame_index_for_resume(
    fiber: &Fiber,
    resume: ResumePolicy,
    context: &str,
) -> Result<Option<usize>, String> {
    if resume.target_pc().is_none() {
        return Ok(None);
    }
    fiber
        .frames
        .len()
        .checked_sub(1)
        .map(Some)
        .ok_or_else(|| format!("{context} requested without active frame"))
}

pub(crate) fn set_frame_pc_for_resume(
    fiber: &mut Fiber,
    frame_index: Option<usize>,
    resume: ResumePolicy,
    context: &str,
) -> Result<(), String> {
    let Some(pc) = resume.target_pc() else {
        return Ok(());
    };
    let Some(index) = frame_index else {
        return Err(format!("{context} requested without active frame"));
    };
    let Some(frame) = fiber.frames.get_mut(index) else {
        return Err(format!("{context} requested without active frame"));
    };
    frame.pc = pc as usize;
    Ok(())
}

pub(crate) fn set_current_frame_pc_for_resume(
    fiber: &mut Fiber,
    resume: ResumePolicy,
    context: &str,
) -> Result<(), String> {
    let frame_index = frame_index_for_resume(fiber, resume, context)?;
    set_frame_pc_for_resume(fiber, frame_index, resume, context)
}

pub(crate) fn replay_current_instruction_policy(
    fiber: &Fiber,
    context: &str,
) -> Result<ResumePolicy, String> {
    let Some(frame) = fiber.current_frame() else {
        return Err(format!("{context} requested without active frame"));
    };
    let pc = frame
        .pc
        .checked_sub(1)
        .ok_or_else(|| format!("{context} cannot replay from pc 0"))?;
    let pc = u32::try_from(pc).map_err(|_| format!("{context} replay pc {pc} exceeds u32::MAX"))?;
    Ok(ResumePolicy::ReplayCurrentInstruction { pc })
}

#[derive(Debug)]
pub struct RuntimeTransition {
    pub boundary: RuntimeBoundary,
    pub resume: ResumePolicy,
    pub wakes: Vec<WakeCommand>,
    pub gc_roots: GcRootEffect,
    pub island_commands: Vec<IslandCommandEffect>,
    pub endpoint_tombstones: Vec<EndpointTombstone>,
    pub(crate) spawns: Vec<PendingSpawn>,
    queue_closes: Vec<GcRef>,
    queue_close_handles: HashSet<usize>,
    rollback: Option<RuntimeRollback>,
    queue_close_wake_keys: Option<HashSet<SelectActivationWakeKey>>,
    #[cfg(feature = "jit")]
    pub pending_terminal_policy: PendingTransitionTerminalPolicy,
}

/// Fully validated transition with every cross-Island send reservation staged.
/// Constructing this value is the last fallible phase before local mutation.
struct PreparedRuntimeTransition {
    boundary: RuntimeBoundary,
    resume: ResumePolicy,
    wakes: Vec<WakeCommand>,
    gc_roots: GcRootEffect,
    island_commands: Vec<IslandCommandEffect>,
    remote_island_commands: Vec<RemoteIslandCommandCommit>,
    endpoint_tombstones: Vec<EndpointTombstone>,
    spawns: Vec<PendingSpawn>,
    queue_closes: Vec<GcRef>,
}

#[derive(Debug)]
pub(crate) enum RuntimeRollback {
    LocalQueueClose {
        ch: GcRef,
        closed: bool,
    },
    #[cfg(test)]
    LocalQueue {
        ch: GcRef,
        closed: bool,
        waiting_senders: VecDeque<(QueueWaiter, QueueMessage)>,
        waiting_receivers: VecDeque<QueueWaiter>,
        stack_slots: Vec<(usize, u64)>,
        select_state: Option<Option<SelectState>>,
    },
    LocalQueueRecv {
        ch: GcRef,
        buffered_payload: Option<QueueMessage>,
        sender: (QueueWaiter, QueueMessage),
        stack_slots: Vec<(usize, u64)>,
        select_state: Option<Option<SelectState>>,
    },
    RemoteQueueProxy {
        ch: GcRef,
        endpoint_id: u64,
        home_island: u32,
        closed: bool,
    },
    EndpointTransfer {
        endpoint_registry: EndpointRegistryUndo,
        home_infos: Vec<(GcRef, queue::HomeInfoUndo)>,
    },
    DirectQueueReceiver {
        ch: GcRef,
        waiter: QueueWaiter,
        stack_slots: Vec<(usize, u64)>,
        select_state: Option<Option<SelectState>>,
    },
    #[cfg(feature = "jit")]
    SelectWaiters {
        fiber_key: u64,
        select_state: Option<SelectState>,
        queues: Vec<SelectQueueWaiterUndo>,
    },
    Composite(Vec<RuntimeRollback>),
}

#[cfg(feature = "jit")]
#[derive(Debug)]
pub(crate) struct SelectQueueWaiterUndo {
    ch: GcRef,
    senders: Vec<(usize, QueueWaiter, QueueMessage)>,
    receivers: Vec<(usize, QueueWaiter)>,
}

impl RuntimeRollback {
    pub(crate) fn combine(first: Self, second: Self) -> Self {
        let mut combined = Vec::new();
        match first {
            Self::Composite(mut rollbacks) => combined.append(&mut rollbacks),
            rollback => combined.push(rollback),
        }
        match second {
            Self::Composite(mut rollbacks) => combined.append(&mut rollbacks),
            rollback => combined.push(rollback),
        }
        Self::Composite(combined)
    }

    #[cfg(test)]
    pub(crate) fn local_queue(_vm_state: &VmState, ch: GcRef) -> Self {
        let state = unsafe { queue::local_state(ch) };
        Self::LocalQueue {
            ch,
            closed: state.closed,
            waiting_senders: state.waiting_senders.clone(),
            waiting_receivers: state.waiting_receivers.clone(),
            stack_slots: Vec::new(),
            select_state: None,
        }
    }

    pub(crate) fn local_queue_close(ch: GcRef) -> Self {
        Self::LocalQueueClose {
            ch,
            closed: unsafe { queue::is_closed(ch) },
        }
    }

    pub(crate) fn remote_queue_proxy(_vm_state: &VmState, ch: GcRef) -> Self {
        let proxy = unsafe { queue::remote_proxy(ch) };
        Self::RemoteQueueProxy {
            ch,
            endpoint_id: proxy.endpoint_id,
            home_island: proxy.home_island,
            closed: proxy.closed,
        }
    }

    pub(crate) fn endpoint_transfer(
        endpoint_registry: EndpointRegistryUndo,
        home_infos: Vec<(GcRef, queue::HomeInfoUndo)>,
    ) -> Self {
        Self::EndpointTransfer {
            endpoint_registry,
            home_infos,
        }
    }

    pub(crate) fn direct_queue_receiver(ch: GcRef, waiter: QueueWaiter) -> Self {
        Self::DirectQueueReceiver {
            ch,
            waiter,
            stack_slots: Vec::new(),
            select_state: None,
        }
    }

    pub(crate) fn local_queue_with_stack_slots(
        _vm_state: &VmState,
        ch: GcRef,
        stack_slots: Vec<(usize, u64)>,
    ) -> Self {
        let state = unsafe { queue::local_state(ch) };
        let sender = state
            .waiting_senders
            .front()
            .cloned()
            .expect("queue recv rollback requires a pending endpoint sender");
        Self::LocalQueueRecv {
            ch,
            buffered_payload: state.buffer.front().cloned(),
            sender,
            stack_slots,
            select_state: None,
        }
    }

    pub(crate) fn push_stack_slot(&mut self, index: usize, value: u64) {
        match self {
            Self::LocalQueueClose { .. } => {}
            #[cfg(test)]
            Self::LocalQueue { stack_slots, .. } => {
                stack_slots.push((index, value));
            }
            Self::LocalQueueRecv { stack_slots, .. } => {
                stack_slots.push((index, value));
            }
            Self::RemoteQueueProxy { .. } => {}
            Self::EndpointTransfer { .. } => {}
            Self::DirectQueueReceiver { stack_slots, .. } => {
                stack_slots.push((index, value));
            }
            #[cfg(feature = "jit")]
            Self::SelectWaiters { .. } => {}
            Self::Composite(rollbacks) => {
                for rollback in rollbacks {
                    rollback.push_stack_slot(index, value);
                }
            }
        }
    }

    pub(crate) fn set_select_state(&mut self, state: Option<SelectState>) {
        match self {
            Self::LocalQueueClose { .. } => {}
            #[cfg(test)]
            Self::LocalQueue { select_state, .. } => *select_state = Some(state),
            Self::LocalQueueRecv { select_state, .. } => *select_state = Some(state),
            Self::RemoteQueueProxy { .. } => {}
            Self::EndpointTransfer { .. } => {}
            Self::DirectQueueReceiver { select_state, .. } => {
                *select_state = Some(state);
            }
            #[cfg(feature = "jit")]
            Self::SelectWaiters { .. } => {}
            Self::Composite(rollbacks) => {
                for rollback in rollbacks {
                    rollback.set_select_state(state.clone());
                }
            }
        }
    }

    #[cfg(feature = "jit")]
    fn select_waiters(
        fiber_key: u64,
        select_state: Option<SelectState>,
        queues: Vec<SelectQueueWaiterUndo>,
    ) -> Self {
        Self::SelectWaiters {
            fiber_key,
            select_state,
            queues,
        }
    }

    fn restore(
        self,
        vm_state: &mut VmState,
        scheduler: &mut crate::scheduler::Scheduler,
        current_fiber: Option<FiberId>,
    ) {
        match self {
            Self::LocalQueueClose { ch, closed } => unsafe {
                queue::with_local_state(ch, |state| state.closed = closed)
            },
            #[cfg(test)]
            Self::LocalQueue {
                ch,
                closed,
                waiting_senders,
                waiting_receivers,
                stack_slots,
                select_state,
            } => {
                // Safety: rollback retains the live queue handle captured at mutation time.
                unsafe {
                    queue::with_local_state(ch, |local_state| {
                        local_state.closed = closed;
                        local_state.waiting_senders = waiting_senders;
                        local_state.waiting_receivers = waiting_receivers;
                    })
                };
                if let Some(fiber) = current_fiber.and_then(|fid| scheduler.try_get_fiber_mut(fid))
                {
                    for (index, value) in stack_slots {
                        if let Some(slot) = fiber.stack.get_mut(index) {
                            *slot = value;
                        }
                    }
                    if let Some(select_state) = select_state {
                        fiber.select_state = select_state;
                    }
                }
            }
            Self::LocalQueueRecv {
                ch,
                buffered_payload,
                sender,
                stack_slots,
                select_state,
            } => {
                // Safety: rollback retains the live queue handle captured at mutation time.
                unsafe {
                    queue::with_local_state(ch, |local_state| {
                        if let Some(payload) = buffered_payload {
                            let _promoted_sender_payload = local_state.buffer.pop_back();
                            local_state.buffer.push_front(payload);
                        }
                        local_state.waiting_senders.push_front(sender);
                    })
                };
                if let Some(fiber) = current_fiber.and_then(|fid| scheduler.try_get_fiber_mut(fid))
                {
                    for (index, value) in stack_slots {
                        if let Some(slot) = fiber.stack.get_mut(index) {
                            *slot = value;
                        }
                    }
                    if let Some(select_state) = select_state {
                        fiber.select_state = select_state;
                    }
                }
            }
            Self::RemoteQueueProxy {
                ch,
                endpoint_id,
                home_island,
                closed,
            } => {
                let proxy = unsafe { queue::remote_proxy_mut(ch) };
                proxy.endpoint_id = endpoint_id;
                proxy.home_island = home_island;
                proxy.closed = closed;
            }
            Self::EndpointTransfer {
                endpoint_registry,
                home_infos,
            } => {
                for (ch, undo) in home_infos {
                    unsafe { queue::restore_home_info_undo(ch, undo) };
                }
                endpoint_registry.restore(&mut vm_state.endpoint_registry);
            }
            Self::DirectQueueReceiver {
                ch,
                waiter,
                stack_slots,
                select_state,
            } => {
                unsafe { queue::restore_direct_receiver(ch, waiter) };
                if let Some(fiber) = current_fiber.and_then(|fid| scheduler.try_get_fiber_mut(fid))
                {
                    for (index, value) in stack_slots {
                        if let Some(slot) = fiber.stack.get_mut(index) {
                            *slot = value;
                        }
                    }
                    if let Some(select_state) = select_state {
                        fiber.select_state = select_state;
                    }
                }
            }
            #[cfg(feature = "jit")]
            Self::SelectWaiters {
                fiber_key,
                select_state,
                queues,
            } => {
                for queue_undo in queues {
                    unsafe {
                        queue::with_local_state(queue_undo.ch, |local_state| {
                            for (index, waiter, message) in queue_undo.senders {
                                if !local_state
                                    .waiting_senders
                                    .iter()
                                    .any(|(queued, _)| queued == &waiter)
                                {
                                    local_state.waiting_senders.insert(index, (waiter, message));
                                }
                            }
                            for (index, waiter) in queue_undo.receivers {
                                if !local_state.waiting_receivers.contains(&waiter) {
                                    local_state.waiting_receivers.insert(index, waiter);
                                }
                            }
                        })
                    };
                }
                let key = FiberWakeKey::from_packed(fiber_key);
                if let Some(fiber) = scheduler.try_get_fiber_mut_by_wake_key(key) {
                    fiber.select_state = select_state;
                }
            }
            Self::Composite(rollbacks) => {
                for rollback in rollbacks.into_iter().rev() {
                    rollback.restore(vm_state, scheduler, current_fiber);
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EndpointTombstone {
    pub endpoint_id: u64,
    pub response_source: Option<u32>,
}

impl EndpointTombstone {
    pub fn new(endpoint_id: u64) -> Self {
        Self {
            endpoint_id,
            response_source: None,
        }
    }

    pub fn with_response_source(endpoint_id: u64, response_source: u32) -> Self {
        Self {
            endpoint_id,
            response_source: Some(response_source),
        }
    }
}

#[cfg(feature = "jit")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PendingTransitionTerminalPolicy {
    CommitOnLanguagePanic,
    CommitOnAnyTerminal,
    DiscardOnTerminal,
}

impl RuntimeTransition {
    pub fn new(boundary: RuntimeBoundary, resume: ResumePolicy, gc_roots: GcRootEffect) -> Self {
        Self {
            boundary,
            resume,
            wakes: Vec::new(),
            gc_roots,
            island_commands: Vec::new(),
            endpoint_tombstones: Vec::new(),
            spawns: Vec::new(),
            queue_closes: Vec::new(),
            queue_close_handles: HashSet::new(),
            rollback: None,
            queue_close_wake_keys: None,
            #[cfg(feature = "jit")]
            pending_terminal_policy: PendingTransitionTerminalPolicy::CommitOnLanguagePanic,
        }
    }

    pub fn continue_with_gc_roots(gc_roots: GcRootEffect) -> Self {
        Self::new(
            RuntimeBoundary::Continue,
            ResumePolicy::PreserveFramePc,
            gc_roots,
        )
    }

    pub fn fatal_infra(message: impl Into<String>) -> Self {
        Self::new(
            RuntimeBoundary::FatalInfra(message.into()),
            ResumePolicy::PreserveFramePc,
            GcRootEffect::None,
        )
    }

    pub(crate) fn push_queue_close_wake(&mut self, wake: WakeCommand) {
        debug_assert!(wake.is_queue_close_wake());
        if let Some(key) = wake.select_activation_key() {
            if !self
                .queue_close_wake_keys
                .get_or_insert_with(HashSet::new)
                .insert(key)
            {
                return;
            }
        }
        self.wakes.push(wake);
    }

    pub(crate) fn set_rollback(&mut self, rollback: RuntimeRollback) {
        self.rollback = Some(match self.rollback.take() {
            Some(existing) => RuntimeRollback::combine(existing, rollback),
            None => rollback,
        });
    }

    pub(crate) fn prepare_queue_close(&mut self, ch: GcRef) {
        let key = ch as usize;
        if self.queue_close_handles.contains(&key) {
            return;
        }
        if self.queue_close_handles.try_reserve(1).is_ok() {
            self.queue_close_handles.insert(key);
            self.queue_closes.push(ch);
        } else if !self.queue_closes.contains(&ch) {
            self.queue_closes.push(ch);
        }
    }

    #[cfg(feature = "jit")]
    fn merge_effect(&mut self, effect: GcRootEffect) {
        self.gc_roots = merge_gc_root_effects(self.gc_roots, effect);
    }

    #[cfg(feature = "jit")]
    pub fn with_pending_terminal_policy(mut self, policy: PendingTransitionTerminalPolicy) -> Self {
        self.pending_terminal_policy = policy;
        self
    }

    #[cfg(feature = "jit")]
    pub fn set_pending_terminal_policy(&mut self, policy: PendingTransitionTerminalPolicy) {
        self.pending_terminal_policy = policy;
    }

    #[cfg(feature = "jit")]
    fn merge_side_effects_from(&mut self, mut other: RuntimeTransition) {
        self.merge_effect(other.gc_roots);
        for wake in other.wakes.drain(..) {
            if wake.is_queue_close_wake() {
                self.push_queue_close_wake(wake);
            } else {
                self.wakes.push(wake);
            }
        }
        merge_vec(&mut self.island_commands, &mut other.island_commands);
        merge_vec(
            &mut self.endpoint_tombstones,
            &mut other.endpoint_tombstones,
        );
        merge_vec(&mut self.spawns, &mut other.spawns);
        for ch in other.queue_closes.drain(..) {
            self.prepare_queue_close(ch);
        }
        if let Some(rollback) = other.rollback.take() {
            self.set_rollback(rollback);
        }
    }

    #[cfg(feature = "jit")]
    pub(crate) fn discard_response_awaiting_island_commands(&mut self) {
        self.island_commands
            .retain(|effect| !effect.expects_response());
    }
}

#[cfg(feature = "jit")]
fn merge_vec<T>(target: &mut Vec<T>, source: &mut Vec<T>) {
    if target.is_empty() {
        core::mem::swap(target, source);
    } else {
        target.append(source);
    }
}

fn merge_gc_root_effects(left: GcRootEffect, right: GcRootEffect) -> GcRootEffect {
    match (left, right) {
        (GcRootEffect::AllRootsDirty, _) | (_, GcRootEffect::AllRootsDirty) => {
            GcRootEffect::AllRootsDirty
        }
        (GcRootEffect::CurrentFiberDirty, _) | (_, GcRootEffect::CurrentFiberDirty) => {
            GcRootEffect::CurrentFiberDirty
        }
        (GcRootEffect::None, GcRootEffect::None) => GcRootEffect::None,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeBoundary {
    Continue,
    Done,
    Yield,
    Block(BlockReason),
    Panic(String),
    FatalInfra(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WakeCommand {
    Waiter {
        waiter: QueueWaiter,
        select_result: Option<SelectWokenResult>,
    },
    ClosedReceiver {
        waiter: QueueWaiter,
        endpoint_id: Option<u64>,
    },
    ClosedSender {
        waiter: QueueWaiter,
        endpoint_id: Option<u64>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SelectActivationWakeKey {
    island_id: u32,
    fiber_key: u64,
    select_id: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct QueueActivationWakeKey {
    wake_key: FiberWakeKey,
    island_id: u32,
    target: QueueWaitTarget,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct EndpointActivationKey {
    endpoint_id: u64,
    wait_key: EndpointWaitKey,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct EndpointResponseAuthorizationSource {
    endpoint_id: u64,
    from_island: u32,
    target_island: u32,
}

#[inline]
fn island_command_expects_response(command: &IslandCommand) -> bool {
    matches!(
        command,
        IslandCommand::EndpointRequest { kind, .. }
            if kind.wait_key().is_some()
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum WakeActivationKey {
    Select(SelectActivationWakeKey),
    Queue(QueueActivationWakeKey),
}

#[derive(Debug)]
pub struct IslandCommandEffect {
    pub island_id: u32,
    pub command: IslandCommand,
}

impl IslandCommandEffect {
    #[inline]
    fn expects_response(&self) -> bool {
        island_command_expects_response(&self.command)
    }

    pub fn spawn_fiber(island_id: u32, closure_data: vo_runtime::pack::PackedValue) -> Self {
        Self {
            island_id,
            command: IslandCommand::SpawnFiber { closure_data },
        }
    }

    pub fn endpoint_send_request(
        island_id: u32,
        endpoint_id: u64,
        data: Vec<u8>,
        wait_key: EndpointWaitKey,
    ) -> Self {
        Self {
            island_id,
            command: IslandCommand::EndpointRequest {
                endpoint_id,
                kind: EndpointRequestKind::Send { data, wait_key },
            },
        }
    }

    pub fn endpoint_recv_request(
        island_id: u32,
        endpoint_id: u64,
        wait_key: EndpointWaitKey,
    ) -> Self {
        Self {
            island_id,
            command: IslandCommand::EndpointRequest {
                endpoint_id,
                kind: EndpointRequestKind::Recv { wait_key },
            },
        }
    }

    pub fn endpoint_transfer_request(island_id: u32, endpoint_id: u64, new_peer: u32) -> Self {
        Self {
            island_id,
            command: IslandCommand::EndpointRequest {
                endpoint_id,
                kind: EndpointRequestKind::Transfer { new_peer },
            },
        }
    }

    pub fn endpoint_close_request(island_id: u32, endpoint_id: u64) -> Self {
        Self {
            island_id,
            command: IslandCommand::EndpointRequest {
                endpoint_id,
                kind: EndpointRequestKind::Close,
            },
        }
    }

    pub fn endpoint_response(island_id: u32, endpoint_id: u64, kind: EndpointResponseKind) -> Self {
        Self {
            island_id,
            command: IslandCommand::EndpointResponse { endpoint_id, kind },
        }
    }

    pub fn endpoint_recv_data_response(
        island_id: u32,
        endpoint_id: u64,
        data: Vec<u8>,
        wait_key: EndpointWaitKey,
    ) -> Self {
        Self {
            island_id,
            command: IslandCommand::EndpointResponse {
                endpoint_id,
                kind: EndpointResponseKind::RecvData {
                    data,
                    closed: false,
                    wait_key,
                },
            },
        }
    }
}

struct RemoteIslandCommandCommit {
    island_id: u32,
    command: IslandCommand,
    #[cfg(feature = "std")]
    reservation: Option<Box<dyn IslandSendReservation>>,
}

impl WakeCommand {
    pub fn queue_waiter(waiter: QueueWaiter) -> Self {
        let select_result = match waiter.select_info().map(|select| select.kind) {
            Some(SelectWaitKind::Send) => Some(SelectWokenResult::SendAccepted),
            _ => None,
        };
        Self::Waiter {
            waiter,
            select_result,
        }
    }

    pub fn queue_waiter_with_result(waiter: QueueWaiter, select_result: SelectWokenResult) -> Self {
        Self::Waiter {
            waiter,
            select_result: Some(select_result),
        }
    }

    pub fn queue_closed_receiver(waiter: QueueWaiter, endpoint_id: Option<u64>) -> Self {
        Self::ClosedReceiver {
            waiter,
            endpoint_id,
        }
    }

    pub fn queue_closed_sender(waiter: QueueWaiter, endpoint_id: Option<u64>) -> Self {
        Self::ClosedSender {
            waiter,
            endpoint_id,
        }
    }

    fn select_activation_key(&self) -> Option<SelectActivationWakeKey> {
        let waiter = self.waiter();
        let select = waiter.select_info()?;
        Some(SelectActivationWakeKey {
            island_id: waiter.island_id(),
            fiber_key: waiter.fiber_key(),
            select_id: select.select_id,
        })
    }

    fn activation_key(&self) -> WakeActivationKey {
        if let Some(select_key) = self.select_activation_key() {
            return WakeActivationKey::Select(select_key);
        }
        let waiter = self.waiter();
        WakeActivationKey::Queue(QueueActivationWakeKey {
            wake_key: FiberWakeKey::from_packed(waiter.fiber_key()),
            island_id: waiter.island_id(),
            target: *waiter.target(),
        })
    }

    fn waiter(&self) -> &QueueWaiter {
        match self {
            Self::Waiter { waiter, .. }
            | Self::ClosedReceiver { waiter, .. }
            | Self::ClosedSender { waiter, .. } => waiter,
        }
    }

    pub(crate) fn is_queue_close_wake(&self) -> bool {
        matches!(
            self,
            Self::ClosedReceiver { .. } | Self::ClosedSender { .. }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeCommand {
    HostEvent {
        key: HostWaitKey,
        data: Option<Vec<u8>>,
    },
    #[cfg(feature = "std")]
    IoReady {
        key: IoWaitKey,
    },
    EndpointResponse {
        endpoint_id: u64,
        from_island: u32,
        kind: EndpointResponseKind,
    },
    EndpointClosed {
        endpoint_id: u64,
        from_island: u32,
    },
}

impl RuntimeCommand {
    pub fn host_event_wake(key: HostWaitKey) -> Self {
        Self::HostEvent { key, data: None }
    }

    pub fn host_event_wake_with_data(key: HostWaitKey, data: Vec<u8>) -> Self {
        Self::HostEvent {
            key,
            data: Some(data),
        }
    }

    #[cfg(feature = "std")]
    pub fn io_ready(key: IoWaitKey) -> Self {
        Self::IoReady { key }
    }

    pub fn endpoint_response(
        endpoint_id: u64,
        from_island: u32,
        kind: EndpointResponseKind,
    ) -> Self {
        Self::EndpointResponse {
            endpoint_id,
            from_island,
            kind,
        }
    }

    pub fn endpoint_closed_response(endpoint_id: u64, from_island: u32) -> Self {
        Self::EndpointClosed {
            endpoint_id,
            from_island,
        }
    }
}

pub(crate) fn validate_canonical_fiber_key(key: u64, context: &str) -> Result<(), String> {
    if FiberWakeKey::from_packed(key).generation == 0 {
        return Err(format!("{context} used raw fiber slot identity"));
    }
    Ok(())
}

fn endpoint_response_kind_is_closed(kind: &EndpointResponseKind) -> bool {
    matches!(
        kind,
        EndpointResponseKind::Closed
            | EndpointResponseKind::SendAck { closed: true, .. }
            | EndpointResponseKind::RecvData { closed: true, .. }
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RuntimeCommandOutcome {
    pub applied: bool,
    pub payload_accepted: bool,
}

impl Vm {
    #[cfg(feature = "jit")]
    pub(crate) fn push_pending_runtime_transition(&mut self, mut transition: RuntimeTransition) {
        if let Some(rollback) = self.select_waiter_rollback_for_pending_transition(&transition) {
            transition.set_rollback(rollback);
        }
        self.cancel_select_sibling_waiters_for_transition(&transition);
        self.pending_runtime_transitions.push(transition);
    }

    #[cfg(feature = "jit")]
    pub(crate) fn discard_response_awaiting_island_commands_from_pending_transitions(&mut self) {
        for pending in &mut self.pending_runtime_transitions {
            pending.discard_response_awaiting_island_commands();
        }
    }

    #[cfg(feature = "jit")]
    fn restore_pending_runtime_transition_rollback(&mut self, mut pending: RuntimeTransition) {
        if let Some(rollback) = pending.rollback.take() {
            self.restore_runtime_rollback(self.scheduler.current, rollback);
        }
    }

    #[cfg(feature = "jit")]
    fn discard_pending_runtime_transitions(&mut self) {
        let mut pending_transitions = core::mem::take(&mut self.pending_runtime_transitions);
        for pending in pending_transitions.drain(..) {
            self.restore_pending_runtime_transition_rollback(pending);
        }
        debug_assert!(self.pending_runtime_transitions.is_empty());
        self.pending_runtime_transitions = pending_transitions;
    }

    #[cfg(feature = "jit")]
    fn drain_pending_runtime_transitions_into(
        &mut self,
        transition: &mut RuntimeTransition,
        should_commit: impl Fn(PendingTransitionTerminalPolicy) -> bool,
    ) -> bool {
        let mut committed_any = false;
        let mut pending_transitions = core::mem::take(&mut self.pending_runtime_transitions);
        for pending in pending_transitions.drain(..) {
            if should_commit(pending.pending_terminal_policy) {
                transition.merge_side_effects_from(pending);
                committed_any = true;
            } else {
                self.restore_pending_runtime_transition_rollback(pending);
            }
        }
        debug_assert!(self.pending_runtime_transitions.is_empty());
        self.pending_runtime_transitions = pending_transitions;
        committed_any
    }

    #[cfg(feature = "jit")]
    pub(crate) fn attach_pending_runtime_transitions(&mut self, result: ExecResult) -> ExecResult {
        if self.pending_runtime_transitions.is_empty() {
            return result;
        }
        match result {
            ExecResult::Transition(mut transition) => {
                self.drain_pending_runtime_transitions_into(&mut transition, |_| true);
                ExecResult::Transition(transition)
            }
            ExecResult::FrameChanged => {
                let mut transition = RuntimeTransition::new(
                    RuntimeBoundary::Yield,
                    ResumePolicy::PreserveFramePc,
                    GcRootEffect::None,
                );
                self.drain_pending_runtime_transitions_into(&mut transition, |_| true);
                ExecResult::Transition(transition)
            }
            ExecResult::TimesliceExpired => self.pending_transitions_for_boundary(
                RuntimeBoundary::Yield,
                ResumePolicy::PreserveFramePc,
            ),
            ExecResult::Block(reason) => self.pending_transitions_for_boundary(
                RuntimeBoundary::Block(reason),
                ResumePolicy::PreserveFramePc,
            ),
            ExecResult::Done => self.pending_transitions_for_boundary(
                RuntimeBoundary::Done,
                ResumePolicy::PreserveFramePc,
            ),
            ExecResult::JitError(message) => {
                let mut transition = RuntimeTransition::fatal_infra(message);
                self.drain_pending_runtime_transitions_into(&mut transition, |policy| {
                    matches!(policy, PendingTransitionTerminalPolicy::CommitOnAnyTerminal)
                });
                ExecResult::Transition(transition)
            }
            ExecResult::MemoryError(error) => {
                self.discard_pending_runtime_transitions();
                ExecResult::MemoryError(error)
            }
            ExecResult::Panic => {
                let mut transition = RuntimeTransition::continue_with_gc_roots(GcRootEffect::None);
                let committed_any =
                    self.drain_pending_runtime_transitions_into(&mut transition, |policy| {
                        matches!(
                            policy,
                            PendingTransitionTerminalPolicy::CommitOnAnyTerminal
                                | PendingTransitionTerminalPolicy::CommitOnLanguagePanic
                        )
                    });
                if !committed_any {
                    return ExecResult::Panic;
                }
                match self.apply_runtime_transition(self.scheduler.current, transition) {
                    Ok(_) => ExecResult::Panic,
                    Err(err) => ExecResult::JitError(format!("{err:?}")),
                }
            }
            ExecResult::Interrupted => {
                let mut transition = RuntimeTransition::continue_with_gc_roots(GcRootEffect::None);
                let committed_any = self
                    .drain_pending_runtime_transitions_into(&mut transition, |policy| {
                        matches!(policy, PendingTransitionTerminalPolicy::CommitOnAnyTerminal)
                    });
                if !committed_any {
                    return ExecResult::Interrupted;
                }
                match self.apply_runtime_transition(self.scheduler.current, transition) {
                    Ok(_) => ExecResult::Interrupted,
                    Err(err) => ExecResult::JitError(format!("{err:?}")),
                }
            }
            ExecResult::Exit(code) => {
                let mut transition = RuntimeTransition::continue_with_gc_roots(GcRootEffect::None);
                let committed_any = self
                    .drain_pending_runtime_transitions_into(&mut transition, |policy| {
                        matches!(policy, PendingTransitionTerminalPolicy::CommitOnAnyTerminal)
                    });
                if !committed_any {
                    return ExecResult::Exit(code);
                }
                match self.apply_runtime_transition(self.scheduler.current, transition) {
                    Ok(_) => ExecResult::Exit(code),
                    Err(err) => ExecResult::JitError(format!("{err:?}")),
                }
            }
            ExecResult::CallClosure { .. } => {
                self.discard_pending_runtime_transitions();
                result
            }
        }
    }

    #[cfg(feature = "jit")]
    fn pending_transitions_for_boundary(
        &mut self,
        boundary: RuntimeBoundary,
        resume: ResumePolicy,
    ) -> ExecResult {
        if self.pending_runtime_transitions.len() == 1 {
            let mut transition = self
                .pending_runtime_transitions
                .pop()
                .expect("single pending transition disappeared");
            transition.boundary = boundary;
            transition.resume = resume;
            return ExecResult::Transition(transition);
        }
        let mut transition = RuntimeTransition::new(boundary, resume, GcRootEffect::None);
        self.drain_pending_runtime_transitions_into(&mut transition, |_| true);
        ExecResult::Transition(transition)
    }

    pub(crate) fn apply_runtime_transition(
        &mut self,
        current_fiber: Option<FiberId>,
        mut transition: RuntimeTransition,
    ) -> Result<Option<SchedulingOutcome>, VmError> {
        #[cfg(feature = "jit")]
        if self.state.jit_osr_borrow_lease_depth != 0 {
            return self.reject_runtime_transition_before_commit(
                current_fiber,
                &mut transition,
                VmError::Jit(
                    "runtime transition attempted while OSR borrow lease is active".to_string(),
                ),
            );
        }

        if matches!(&transition.boundary, RuntimeBoundary::Continue)
            && transition.resume == ResumePolicy::PreserveFramePc
            && transition.wakes.is_empty()
            && transition.island_commands.is_empty()
            && transition.endpoint_tombstones.is_empty()
            && transition.spawns.is_empty()
            && transition.queue_closes.is_empty()
            && transition.rollback.is_none()
        {
            self.apply_gc_root_effect(transition.gc_roots, current_fiber);
            return Ok(None);
        }

        if let Err(err) = self.preflight_runtime_transition(current_fiber, &transition) {
            return self.reject_runtime_transition_before_commit(
                current_fiber,
                &mut transition,
                err,
            );
        }

        let PreparedRuntimeTransition {
            boundary,
            resume,
            wakes,
            gc_roots,
            island_commands,
            remote_island_commands,
            endpoint_tombstones,
            spawns,
            queue_closes,
        } = self.prepare_runtime_transition_after_preflight(current_fiber, transition)?;

        self.apply_resume_policy(current_fiber, resume, "runtime transition")
            .expect("prepared runtime transition resume policy must remain valid");

        for ch in queue_closes {
            debug_assert!(unsafe { queue::is_closed(ch) });
            drop(unsafe { queue::take_waiting_receivers(ch) });
            drop(unsafe { queue::take_waiting_senders(ch) });
        }

        for wake in wakes {
            self.apply_runtime_wake(wake)
                .expect("prepared runtime transition wake must remain applicable");
        }
        self.apply_gc_root_effect(gc_roots, current_fiber);
        for tombstone in endpoint_tombstones {
            self.state
                .endpoint_registry
                .mark_tombstone_with_response_source(
                    tombstone.endpoint_id,
                    tombstone.response_source,
                );
        }
        if let RuntimeBoundary::Block(reason) = &boundary {
            self.apply_block_boundary(reason.clone())
                .expect("prepared runtime transition block registration must remain available");
        }
        for command in island_commands {
            self.apply_island_command_effect(command)
                .expect("prepared local island command must remain applicable");
        }
        self.apply_pending_spawns(spawns)
            .expect("prepared runtime transition spawn capacity must remain available");
        self.commit_remote_island_commands(remote_island_commands);
        match boundary {
            RuntimeBoundary::Continue => Ok(None),
            RuntimeBoundary::Done => {
                let _ = self.scheduler.kill_current();
                Ok(None)
            }
            RuntimeBoundary::Yield => {
                self.scheduler.yield_current();
                Ok(None)
            }
            RuntimeBoundary::Block(_) => Ok(None),
            RuntimeBoundary::Panic(message) => Err(VmError::PanicUnwound {
                msg: Some(message),
                loc: None,
            }),
            RuntimeBoundary::FatalInfra(message) => {
                let _ = self.scheduler.kill_current();
                Err(VmError::Jit(message))
            }
        }
    }

    fn prepare_runtime_transition_after_preflight(
        &mut self,
        current_fiber: Option<FiberId>,
        transition: RuntimeTransition,
    ) -> Result<PreparedRuntimeTransition, VmError> {
        let RuntimeTransition {
            boundary,
            resume,
            wakes,
            gc_roots,
            island_commands,
            endpoint_tombstones,
            spawns,
            queue_closes,
            queue_close_handles: _,
            rollback,
            queue_close_wake_keys: _,
            #[cfg(feature = "jit")]
                pending_terminal_policy: _,
        } = transition;
        let mut rollback = rollback;
        let endpoint_capacity = endpoint_tombstones
            .len()
            .saturating_add(wakes.len())
            .saturating_add(island_commands.len());
        if self
            .state
            .endpoint_registry
            .try_reserve_live(endpoint_capacity)
            .is_err()
        {
            if let Some(rollback) = rollback.take() {
                self.restore_runtime_rollback(current_fiber, rollback);
            }
            return Err(VmError::Jit(
                "runtime transition endpoint capacity allocation failed".into(),
            ));
        }
        let (wakes, remote_wake_commands) =
            match self.split_remote_wake_commands_before_commit(wakes) {
                Ok(staged) => staged,
                Err(err) => {
                    if let Some(rollback) = rollback.take() {
                        self.restore_runtime_rollback(current_fiber, rollback);
                    }
                    return Err(err);
                }
            };
        let mut island_commands = island_commands;
        if island_commands
            .try_reserve(remote_wake_commands.len())
            .is_err()
        {
            if let Some(rollback) = rollback.take() {
                self.restore_runtime_rollback(current_fiber, rollback);
            }
            return Err(VmError::Jit(
                "runtime transition command plan allocation failed".into(),
            ));
        }
        island_commands.extend(remote_wake_commands);
        let (island_commands, remote_island_commands) =
            match self.stage_remote_island_commands_before_commit(current_fiber, island_commands) {
                Ok(staged) => staged,
                Err(err) => {
                    if let Some(rollback) = rollback.take() {
                        self.restore_runtime_rollback(current_fiber, rollback);
                    }
                    return Err(err);
                }
            };
        let gc_roots = if spawns.is_empty() && endpoint_tombstones.is_empty() && wakes.is_empty() {
            gc_roots
        } else {
            merge_gc_root_effects(gc_roots, GcRootEffect::AllRootsDirty)
        };
        Ok(PreparedRuntimeTransition {
            boundary,
            resume,
            wakes,
            gc_roots,
            island_commands,
            remote_island_commands,
            endpoint_tombstones,
            spawns,
            queue_closes,
        })
    }

    fn reject_runtime_transition_before_commit(
        &mut self,
        current_fiber: Option<FiberId>,
        transition: &mut RuntimeTransition,
        err: VmError,
    ) -> Result<Option<SchedulingOutcome>, VmError> {
        if let Some(rollback) = transition.rollback.take() {
            self.restore_runtime_rollback(current_fiber, rollback);
        }
        self.rollback_current_remote_endpoint_wait(current_fiber);
        Err(err)
    }

    fn apply_block_boundary(&mut self, reason: BlockReason) -> Result<(), VmError> {
        match reason {
            BlockReason::Queue => self.scheduler.block_for_queue(),
            #[cfg(feature = "std")]
            BlockReason::Io(token) => self
                .scheduler
                .try_block_for_io(token)
                .map_err(|err| VmError::Jit(err.to_string()))?,
            BlockReason::HostEvent { token, delay_ms } => self
                .scheduler
                .try_block_for_host_event(token, delay_ms)
                .map_err(|err| VmError::Jit(err.to_string()))?,
            BlockReason::HostEventReplay { token, source } => self
                .scheduler
                .try_block_for_host_event_replay(token, source)
                .map_err(|err| VmError::Jit(err.to_string()))?,
        }
        Ok(())
    }

    fn apply_resume_policy(
        &mut self,
        current_fiber: Option<FiberId>,
        resume: ResumePolicy,
        context: &str,
    ) -> Result<(), VmError> {
        if !resume.requires_frame() {
            return Ok(());
        }
        let Some(fid) = current_fiber else {
            return Err(VmError::Jit(format!(
                "{context} resume policy {resume:?} without current fiber"
            )));
        };
        let Some(fiber) = self.scheduler.try_get_fiber_mut(fid) else {
            return Err(VmError::Jit(format!(
                "{context} resume policy {resume:?} for missing fiber {:?}",
                fid
            )));
        };
        set_current_frame_pc_for_resume(fiber, resume, context).map_err(VmError::Jit)
    }

    fn validate_resume_policy(
        &self,
        current_fiber: Option<FiberId>,
        resume: ResumePolicy,
        context: &str,
    ) -> Result<(), VmError> {
        if !resume.requires_frame() {
            return Ok(());
        }
        let Some(fid) = current_fiber else {
            return Err(VmError::Jit(format!(
                "{context} resume policy {resume:?} without current fiber"
            )));
        };
        let Some(fiber) = self.scheduler.try_get_fiber(fid) else {
            return Err(VmError::Jit(format!(
                "{context} resume policy {resume:?} for missing fiber {:?}",
                fid
            )));
        };
        if fiber.current_frame().is_none() {
            return Err(VmError::Jit(format!(
                "{context} resume policy {resume:?} without active frame"
            )));
        }
        Ok(())
    }

    pub(crate) fn runtime_boundary_for_exec_result(result: &ExecResult) -> RuntimeBoundary {
        match result {
            ExecResult::Transition(transition) => transition.boundary.clone(),
            ExecResult::TimesliceExpired | ExecResult::Interrupted => RuntimeBoundary::Yield,
            ExecResult::Block(reason) => RuntimeBoundary::Block(reason.clone()),
            ExecResult::Panic => RuntimeBoundary::Panic("fiber panic".to_string()),
            ExecResult::MemoryError(error) => {
                RuntimeBoundary::FatalInfra(format!("Island managed-memory failure: {error}"))
            }
            ExecResult::JitError(message) => RuntimeBoundary::FatalInfra(message.clone()),
            ExecResult::Done | ExecResult::Exit(_) => RuntimeBoundary::Done,
            ExecResult::FrameChanged | ExecResult::CallClosure { .. } => RuntimeBoundary::Continue,
        }
    }

    pub(crate) fn apply_runtime_command(
        &mut self,
        command: RuntimeCommand,
    ) -> RuntimeCommandOutcome {
        match command {
            RuntimeCommand::HostEvent { key, data } => match data {
                None => {
                    self.apply_gc_root_effect(GcRootEffect::AllRootsDirty, None);
                    let applied = self.scheduler.wake_host_event(key);
                    RuntimeCommandOutcome {
                        applied,
                        payload_accepted: false,
                    }
                }
                Some(data) => {
                    self.apply_gc_root_effect(GcRootEffect::AllRootsDirty, None);
                    let payload_accepted = self.scheduler.wake_host_event_with_data(key, data);
                    RuntimeCommandOutcome {
                        applied: payload_accepted,
                        payload_accepted,
                    }
                }
            },
            #[cfg(feature = "std")]
            RuntimeCommand::IoReady { key } => {
                self.apply_gc_root_effect(GcRootEffect::AllRootsDirty, None);
                let applied = self.scheduler.wake_io(key);
                RuntimeCommandOutcome {
                    applied,
                    payload_accepted: applied,
                }
            }
            RuntimeCommand::EndpointResponse {
                endpoint_id,
                from_island,
                kind,
            } => {
                let Some(wait_key) = kind.wait_key() else {
                    return RuntimeCommandOutcome {
                        applied: false,
                        payload_accepted: false,
                    };
                };
                let fiber_key = wait_key.fiber_key();
                if self.state.pending_island_responses == 0 {
                    return RuntimeCommandOutcome {
                        applied: false,
                        payload_accepted: false,
                    };
                }
                if !crate::vm::endpoint_response_from_authorized_source(
                    self,
                    endpoint_id,
                    from_island,
                ) {
                    return RuntimeCommandOutcome {
                        applied: false,
                        payload_accepted: false,
                    };
                }
                // Rejected endpoint responses do not satisfy the live
                // obligation, so the pending count changes only after all
                // source, generation, endpoint, and operation checks pass.
                let Some(fid) = ({
                    let Some(fiber) = self
                        .scheduler
                        .try_get_fiber_mut_by_endpoint_response_key(fiber_key)
                    else {
                        return RuntimeCommandOutcome {
                            applied: false,
                            payload_accepted: false,
                        };
                    };
                    if !matches!(
                        fiber.state,
                        crate::fiber::FiberState::Blocked(BlockReason::Queue)
                    ) {
                        return RuntimeCommandOutcome {
                            applied: false,
                            payload_accepted: false,
                        };
                    }
                    let replay_closed_send = match &kind {
                        EndpointResponseKind::SendAck { closed, .. } => *closed,
                        _ => false,
                    };
                    let resume = if replay_closed_send {
                        match replay_current_instruction_policy(
                            fiber,
                            "endpoint closed send response",
                        ) {
                            Ok(resume) => Some(resume),
                            Err(_) => {
                                return RuntimeCommandOutcome {
                                    applied: false,
                                    payload_accepted: false,
                                };
                            }
                        }
                    } else {
                        None
                    };
                    if !fiber.apply_endpoint_response(endpoint_id, kind) {
                        return RuntimeCommandOutcome {
                            applied: false,
                            payload_accepted: false,
                        };
                    }
                    if let Some(resume) = resume {
                        if set_current_frame_pc_for_resume(
                            fiber,
                            resume,
                            "endpoint closed send response",
                        )
                        .is_err()
                        {
                            return RuntimeCommandOutcome {
                                applied: false,
                                payload_accepted: false,
                            };
                        }
                    }
                    Some(FiberId::from_raw(fiber.id))
                }) else {
                    return RuntimeCommandOutcome {
                        applied: false,
                        payload_accepted: false,
                    };
                };
                self.apply_gc_root_effect(GcRootEffect::AllRootsDirty, None);
                self.state.pending_island_responses -= 1;
                let applied = self.scheduler.try_wake_fiber(fid);
                RuntimeCommandOutcome {
                    applied,
                    payload_accepted: applied,
                }
            }
            RuntimeCommand::EndpointClosed {
                endpoint_id,
                from_island,
            } => {
                if !crate::vm::endpoint_response_from_authorized_source(
                    self,
                    endpoint_id,
                    from_island,
                ) {
                    return RuntimeCommandOutcome {
                        applied: false,
                        payload_accepted: false,
                    };
                }
                if let Some(ch) = self.state.endpoint_registry.get_live(endpoint_id) {
                    if unsafe { queue::is_remote(ch) } {
                        unsafe { queue::mark_remote_closed(ch) };
                    }
                }
                self.apply_gc_root_effect(GcRootEffect::AllRootsDirty, None);
                self.state
                    .endpoint_registry
                    .mark_tombstone_with_response_source(endpoint_id, Some(from_island));
                RuntimeCommandOutcome {
                    applied: true,
                    payload_accepted: true,
                }
            }
        }
    }

    fn preflight_runtime_transition(
        &mut self,
        current_fiber: Option<FiberId>,
        transition: &RuntimeTransition,
    ) -> Result<(), VmError> {
        let requires_wait_registration = match &transition.boundary {
            RuntimeBoundary::Block(
                BlockReason::HostEvent { .. } | BlockReason::HostEventReplay { .. },
            ) => true,
            #[cfg(feature = "std")]
            RuntimeBoundary::Block(BlockReason::Io(_)) => true,
            _ => false,
        };
        if requires_wait_registration && !self.scheduler.has_wait_registration_capacity() {
            return Err(VmError::Jit(
                "scheduler wait registration identity space exhausted".to_string(),
            ));
        }
        self.scheduler
            .try_preflight_spawns(&transition.spawns)
            .map_err(scheduler_error_to_vm_error)?;
        self.validate_resume_policy(current_fiber, transition.resume, "runtime transition")?;
        self.preflight_unique_wake_activations(transition)?;
        self.preflight_unique_endpoint_response_activations(transition)?;
        self.preflight_endpoint_response_capacity(transition)?;
        self.preflight_pending_island_response_capacity(transition)?;
        self.preflight_endpoint_response_authorization_stability(transition)?;
        self.preflight_unique_endpoint_request_activations(transition)?;
        for wake in &transition.wakes {
            self.preflight_runtime_wake(wake)?;
        }
        for command in &transition.island_commands {
            self.preflight_island_command_effect(command)?;
        }
        Ok(())
    }

    fn preflight_pending_island_response_capacity(
        &self,
        transition: &RuntimeTransition,
    ) -> Result<(), VmError> {
        let additions = transition
            .island_commands
            .iter()
            .filter(|effect| effect.expects_response())
            .count();
        let additions = u32::try_from(additions).map_err(|_| {
            VmError::Jit(
                "runtime transition pending island response count exceeds u32 capacity".to_string(),
            )
        })?;
        self.state
            .pending_island_responses
            .checked_add(additions)
            .ok_or_else(|| {
                VmError::Jit(
                    "pending island response identity space exhausted before transition commit"
                        .to_string(),
                )
            })?;
        Ok(())
    }

    fn preflight_unique_wake_activations(
        &self,
        transition: &RuntimeTransition,
    ) -> Result<(), VmError> {
        let mut seen = HashSet::new();
        seen.try_reserve(transition.wakes.len())
            .map_err(|_| VmError::Jit("wake activation plan allocation failed".into()))?;
        for wake in &transition.wakes {
            let key = wake.activation_key();
            if !seen.insert(key) {
                return Err(VmError::Jit(
                    "runtime transition contains duplicate wake activation".to_string(),
                ));
            }
        }
        Ok(())
    }

    fn preflight_unique_endpoint_response_activations(
        &self,
        transition: &RuntimeTransition,
    ) -> Result<(), VmError> {
        let mut seen = HashSet::new();
        seen.try_reserve(
            transition
                .wakes
                .len()
                .saturating_add(transition.island_commands.len()),
        )
        .map_err(|_| VmError::Jit("endpoint activation plan allocation failed".into()))?;
        for wake in &transition.wakes {
            let Some(key) = self.endpoint_response_activation_key_for_wake(wake) else {
                continue;
            };
            if !seen.insert(key) {
                return Err(VmError::Jit(
                    "runtime transition contains duplicate endpoint response activation"
                        .to_string(),
                ));
            }
        }
        for command in &transition.island_commands {
            let Some(key) = self.endpoint_response_activation_key_for_island_command(command)
            else {
                continue;
            };
            if !seen.insert(key) {
                return Err(VmError::Jit(
                    "runtime transition contains duplicate endpoint response activation"
                        .to_string(),
                ));
            }
        }
        Ok(())
    }

    fn preflight_endpoint_response_capacity(
        &self,
        transition: &RuntimeTransition,
    ) -> Result<(), VmError> {
        let mut response_count = 0usize;
        for wake in &transition.wakes {
            if self
                .local_endpoint_response_consumption_key_for_wake(wake)
                .is_some()
            {
                response_count += 1;
            }
        }
        for command in &transition.island_commands {
            if self
                .local_endpoint_response_consumption_key_for_island_command(command)
                .is_some()
            {
                response_count += 1;
            }
        }
        if response_count > self.state.pending_island_responses as usize {
            return Err(VmError::Jit(format!(
                "runtime transition endpoint responses exceed pending response count: responses={} pending={}",
                response_count,
                self.state.pending_island_responses
            )));
        }
        Ok(())
    }

    fn endpoint_response_activation_key_for_wake(
        &self,
        wake: &WakeCommand,
    ) -> Option<EndpointActivationKey> {
        let (WakeCommand::ClosedReceiver {
            waiter,
            endpoint_id,
        }
        | WakeCommand::ClosedSender {
            waiter,
            endpoint_id,
        }) = wake
        else {
            return None;
        };
        Some(EndpointActivationKey {
            endpoint_id: (*endpoint_id)?,
            wait_key: waiter.endpoint_wait_key()?,
        })
    }

    fn endpoint_response_activation_key_for_island_command(
        &self,
        effect: &IslandCommandEffect,
    ) -> Option<EndpointActivationKey> {
        let IslandCommand::EndpointResponse { endpoint_id, kind } = &effect.command else {
            return None;
        };
        let wait_key = kind.wait_key()?;
        if effect.island_id != self.state.current_island_id
            && !crate::vm::endpoint_response_from_authorized_source(
                self,
                *endpoint_id,
                self.state.current_island_id,
            )
        {
            return None;
        }
        Some(EndpointActivationKey {
            endpoint_id: *endpoint_id,
            wait_key,
        })
    }

    fn local_endpoint_response_consumption_key_for_wake(
        &self,
        wake: &WakeCommand,
    ) -> Option<EndpointActivationKey> {
        let key = self.endpoint_response_activation_key_for_wake(wake)?;
        match wake {
            WakeCommand::ClosedReceiver { waiter, .. }
            | WakeCommand::ClosedSender { waiter, .. }
                if waiter.island_id() == self.state.current_island_id =>
            {
                Some(key)
            }
            _ => None,
        }
    }

    fn local_endpoint_response_consumption_key_for_island_command(
        &self,
        effect: &IslandCommandEffect,
    ) -> Option<EndpointActivationKey> {
        let key = self.endpoint_response_activation_key_for_island_command(effect)?;
        let IslandCommand::EndpointResponse {
            endpoint_id, kind, ..
        } = &effect.command
        else {
            return None;
        };
        if effect.island_id != self.state.current_island_id
            || matches!(kind, EndpointResponseKind::Closed)
            || !crate::vm::endpoint_response_from_authorized_source(
                self,
                *endpoint_id,
                self.state.current_island_id,
            )
        {
            return None;
        }
        Some(key)
    }

    fn preflight_endpoint_response_authorization_stability(
        &self,
        transition: &RuntimeTransition,
    ) -> Result<(), VmError> {
        let mut sources = HashSet::new();
        sources
            .try_reserve(
                transition
                    .wakes
                    .len()
                    .saturating_add(transition.island_commands.len()),
            )
            .map_err(|_| VmError::Jit("endpoint authorization plan allocation failed".into()))?;
        for wake in &transition.wakes {
            let Some(source) = self.endpoint_response_authorization_source_for_wake(wake) else {
                continue;
            };
            sources.insert(source);
        }
        for effect in &transition.island_commands {
            let Some(source) =
                self.endpoint_response_authorization_source_for_island_command(effect)
            else {
                continue;
            };
            sources.insert(source);
        }
        if sources.is_empty() {
            return Ok(());
        }

        let mut local_closed_wakes = HashSet::new();
        local_closed_wakes
            .try_reserve(transition.wakes.len())
            .map_err(|_| VmError::Jit("endpoint authorization plan allocation failed".into()))?;
        for wake in &transition.wakes {
            let (WakeCommand::ClosedReceiver {
                waiter,
                endpoint_id,
            }
            | WakeCommand::ClosedSender {
                waiter,
                endpoint_id,
            }) = wake
            else {
                continue;
            };
            if waiter.island_id() == self.state.current_island_id
                && waiter.endpoint_wait_key().is_some()
            {
                if let Some(endpoint_id) = endpoint_id {
                    local_closed_wakes.insert(*endpoint_id);
                }
            }
        }

        let mut close_handoffs = HashSet::new();
        close_handoffs
            .try_reserve(transition.island_commands.len())
            .map_err(|_| VmError::Jit("endpoint authorization plan allocation failed".into()))?;
        for effect in &transition.island_commands {
            if let IslandCommand::EndpointRequest {
                endpoint_id,
                kind: EndpointRequestKind::Close,
            } = effect.command
            {
                close_handoffs.insert((endpoint_id, effect.island_id));
            }
        }

        let mut tombstones = HashMap::<u64, Vec<Option<u32>>>::new();
        tombstones
            .try_reserve(transition.endpoint_tombstones.len())
            .map_err(|_| VmError::Jit("endpoint authorization plan allocation failed".into()))?;
        for tombstone in &transition.endpoint_tombstones {
            let entries = tombstones.entry(tombstone.endpoint_id).or_default();
            entries.try_reserve(1).map_err(|_| {
                VmError::Jit("endpoint authorization plan allocation failed".into())
            })?;
            entries.push(tombstone.response_source);
        }

        for source in sources {
            let before = crate::vm::endpoint_response_from_authorized_source(
                self,
                source.endpoint_id,
                source.from_island,
            );
            let mut projected = self
                .state
                .endpoint_registry
                .tombstone_response_source(source.endpoint_id);
            if local_closed_wakes.contains(&source.endpoint_id) {
                projected = Some(Some(self.state.current_island_id));
            }
            if let Some(entries) = tombstones.get(&source.endpoint_id) {
                for response_source in entries {
                    let is_close_handoff = source.from_island == self.state.current_island_id
                        && *response_source == Some(source.target_island)
                        && close_handoffs.contains(&(source.endpoint_id, source.target_island));
                    if is_close_handoff {
                        continue;
                    }
                    projected = Some(response_source.or(projected.flatten()));
                }
            }
            let after = match projected {
                Some(Some(owner)) => owner == source.from_island,
                Some(None) => source.from_island == self.state.current_island_id,
                None => before,
            };
            if before != after {
                return Err(VmError::Jit(
                    "runtime transition endpoint response authorization drift".to_string(),
                ));
            }
        }
        Ok(())
    }

    fn endpoint_response_authorization_source_for_wake(
        &self,
        wake: &WakeCommand,
    ) -> Option<EndpointResponseAuthorizationSource> {
        let key = self.endpoint_response_activation_key_for_wake(wake)?;
        match wake {
            WakeCommand::ClosedReceiver { waiter, .. }
            | WakeCommand::ClosedSender { waiter, .. }
                if waiter.island_id() != self.state.current_island_id =>
            {
                Some(EndpointResponseAuthorizationSource {
                    endpoint_id: key.endpoint_id,
                    from_island: self.state.current_island_id,
                    target_island: waiter.island_id(),
                })
            }
            _ => None,
        }
    }

    fn endpoint_response_authorization_source_for_island_command(
        &self,
        effect: &IslandCommandEffect,
    ) -> Option<EndpointResponseAuthorizationSource> {
        let IslandCommand::EndpointResponse { endpoint_id, .. } = &effect.command else {
            return None;
        };
        Some(EndpointResponseAuthorizationSource {
            endpoint_id: *endpoint_id,
            from_island: self.state.current_island_id,
            target_island: effect.island_id,
        })
    }

    fn preflight_unique_endpoint_request_activations(
        &self,
        transition: &RuntimeTransition,
    ) -> Result<(), VmError> {
        let mut seen = HashSet::new();
        seen.try_reserve(transition.island_commands.len())
            .map_err(|_| VmError::Jit("endpoint request plan allocation failed".into()))?;
        for command in &transition.island_commands {
            let Some(key) = self.endpoint_request_activation_key_for_island_command(command)?
            else {
                continue;
            };
            if !seen.insert(key) {
                return Err(VmError::Jit(
                    "runtime transition contains duplicate endpoint request activation".to_string(),
                ));
            }
        }
        Ok(())
    }

    fn endpoint_request_activation_key_for_island_command(
        &self,
        effect: &IslandCommandEffect,
    ) -> Result<Option<EndpointActivationKey>, VmError> {
        let IslandCommand::EndpointRequest { endpoint_id, kind } = &effect.command else {
            return Ok(None);
        };
        let Some(wait_key) = kind.wait_key() else {
            return Ok(None);
        };
        validate_canonical_fiber_key(wait_key.fiber_key(), "endpoint request activation")
            .map_err(VmError::Jit)?;
        Ok(Some(EndpointActivationKey {
            endpoint_id: *endpoint_id,
            wait_key,
        }))
    }

    fn preflight_runtime_wake(&self, wake: &WakeCommand) -> Result<(), VmError> {
        self.validate_queue_wake_payload(wake)
            .map_err(VmError::Jit)?;
        match wake {
            WakeCommand::Waiter {
                waiter,
                select_result,
            } => self.preflight_queue_waiter_wake(waiter, select_result.as_ref()),
            WakeCommand::ClosedReceiver {
                waiter,
                endpoint_id,
            } => self.preflight_closed_receiver_wake(waiter, *endpoint_id),
            WakeCommand::ClosedSender {
                waiter,
                endpoint_id,
            } => self.preflight_closed_sender_wake(waiter, *endpoint_id),
        }
    }

    fn preflight_queue_waiter_wake(
        &self,
        waiter: &QueueWaiter,
        select_result: Option<&SelectWokenResult>,
    ) -> Result<(), VmError> {
        if waiter.endpoint_wait_key().is_some() {
            return Err(VmError::Jit(
                "runtime queue waiter wake was rejected".to_string(),
            ));
        }
        if waiter.island_id() == self.state.current_island_id {
            if self
                .scheduler
                .can_wake_queue_waiter_with_result(waiter, select_result)
            {
                return Ok(());
            }
            return Err(VmError::Jit(
                "runtime queue waiter wake was rejected".to_string(),
            ));
        }
        Err(VmError::Jit(
            "remote queue waiter wake must use an endpoint response".to_string(),
        ))
    }

    fn preflight_remote_select_wake_shape(&self, waiter: &QueueWaiter) -> Result<(), VmError> {
        if waiter.select_info().is_some() {
            return Err(VmError::Jit(
                "remote select wake cannot be represented without select payload".to_string(),
            ));
        }
        validate_canonical_fiber_key(waiter.fiber_key(), "remote queue waiter wake")
            .map_err(VmError::Jit)?;
        self.preflight_island_route(waiter.island_id())
    }

    fn preflight_local_closed_queue_waiter(
        &self,
        waiter: &QueueWaiter,
        context: &str,
    ) -> Result<(), VmError> {
        let Some((queue_ref, _)) = waiter.queue_identity() else {
            return Err(VmError::Jit(format!("{context} missing queue identity")));
        };
        let ch = crate::exec::validate_queue_handle(&self.state.gc, queue_ref as GcRef, context)
            .map_err(VmError::Jit)?;
        if unsafe { queue::is_closed(ch) } {
            return Ok(());
        }
        Err(VmError::Jit(format!("{context} referenced open queue")))
    }

    fn preflight_closed_receiver_wake(
        &self,
        waiter: &QueueWaiter,
        endpoint_id: Option<u64>,
    ) -> Result<(), VmError> {
        if let Some(wait_key) = waiter.endpoint_wait_key() {
            let Some(endpoint_id) = endpoint_id else {
                return Err(VmError::Jit(
                    "closed endpoint receiver wake missing endpoint id".to_string(),
                ));
            };
            return self.preflight_endpoint_response_for_waiter(
                waiter,
                endpoint_id,
                &EndpointResponseKind::RecvData {
                    data: Vec::new(),
                    closed: true,
                    wait_key,
                },
            );
        }
        if waiter.island_id() == self.state.current_island_id {
            self.preflight_local_closed_queue_waiter(waiter, "closed receiver wake")?;
            if self
                .scheduler
                .can_wake_queue_waiter_with_result(waiter, None)
            {
                return Ok(());
            }
            return Err(VmError::Jit(
                "runtime closed receiver wake was rejected".to_string(),
            ));
        }
        Err(VmError::Jit(
            "remote closed receiver wake cannot be represented without endpoint wait".to_string(),
        ))
    }

    fn preflight_closed_sender_wake(
        &self,
        waiter: &QueueWaiter,
        endpoint_id: Option<u64>,
    ) -> Result<(), VmError> {
        if let Some(wait_key) = waiter.endpoint_wait_key() {
            let Some(endpoint_id) = endpoint_id else {
                return Err(VmError::Jit(
                    "closed endpoint sender wake missing endpoint id".to_string(),
                ));
            };
            return self.preflight_endpoint_response_for_waiter(
                waiter,
                endpoint_id,
                &EndpointResponseKind::SendAck {
                    closed: true,
                    wait_key,
                },
            );
        }
        if waiter.island_id() == self.state.current_island_id {
            self.preflight_local_closed_queue_waiter(waiter, "closed sender wake")?;
            if self.scheduler.can_wake_queue_sender_closed(waiter) {
                if waiter.select_info().is_none() {
                    let key = FiberWakeKey::from_packed(waiter.fiber_key());
                    let Some(fiber) = self.scheduler.try_get_fiber_by_wake_key(key) else {
                        return Err(VmError::Jit(
                            "runtime closed sender wake was rejected".to_string(),
                        ));
                    };
                    replay_current_instruction_policy(fiber, "closed queue sender wake")
                        .map_err(VmError::Jit)?;
                }
                return Ok(());
            }
            return Err(VmError::Jit(
                "runtime closed sender wake was rejected".to_string(),
            ));
        }
        Err(VmError::Jit(
            "remote closed sender wake cannot be represented without endpoint wait".to_string(),
        ))
    }

    fn preflight_endpoint_response_for_waiter(
        &self,
        waiter: &QueueWaiter,
        endpoint_id: u64,
        kind: &EndpointResponseKind,
    ) -> Result<(), VmError> {
        if waiter.island_id() != self.state.current_island_id {
            if !crate::vm::endpoint_response_from_authorized_source(
                self,
                endpoint_id,
                self.state.current_island_id,
            ) {
                return Err(VmError::Jit(
                    "remote endpoint wake response source was rejected".to_string(),
                ));
            }
            validate_canonical_fiber_key(waiter.fiber_key(), "remote endpoint wake response")
                .map_err(VmError::Jit)?;
            return self.preflight_island_route(waiter.island_id());
        }
        self.preflight_same_island_endpoint_wake_response_source(endpoint_id, kind)?;
        if self.can_accept_endpoint_response(endpoint_id, kind) {
            return Ok(());
        }
        Err(VmError::Jit(
            "same-island endpoint wake response was rejected".to_string(),
        ))
    }

    fn preflight_same_island_endpoint_wake_response_source(
        &self,
        endpoint_id: u64,
        kind: &EndpointResponseKind,
    ) -> Result<(), VmError> {
        let from_island = self.state.current_island_id;
        if crate::vm::endpoint_response_from_authorized_source(self, endpoint_id, from_island) {
            return Ok(());
        }
        if !endpoint_response_kind_is_closed(kind) {
            return Err(VmError::Jit(
                "same-island endpoint wake response source was rejected".to_string(),
            ));
        }
        let foreign_live_remote = self
            .state
            .endpoint_registry
            .get_live(endpoint_id)
            .is_some_and(|ch| {
                (unsafe { queue::is_remote(ch) })
                    && unsafe { queue::remote_proxy(ch) }.home_island != from_island
            });
        let foreign_tombstone = matches!(
            self.state
                .endpoint_registry
                .tombstone_response_source(endpoint_id),
            Some(Some(source)) if source != from_island
        );
        if foreign_live_remote || foreign_tombstone {
            return Err(VmError::Jit(
                "same-island endpoint wake response source was rejected".to_string(),
            ));
        }
        Ok(())
    }

    fn can_accept_endpoint_response(&self, endpoint_id: u64, kind: &EndpointResponseKind) -> bool {
        if self.state.pending_island_responses == 0 {
            return false;
        }
        let Some(wait_key) = kind.wait_key() else {
            return false;
        };
        let Some(fiber) = self
            .scheduler
            .try_get_fiber_by_endpoint_response_key(wait_key.fiber_key())
        else {
            return false;
        };
        if !matches!(
            fiber.state,
            crate::fiber::FiberState::Blocked(BlockReason::Queue)
        ) {
            return false;
        }
        if matches!(kind, EndpointResponseKind::SendAck { closed: true, .. })
            && replay_current_instruction_policy(fiber, "endpoint closed send response").is_err()
        {
            return false;
        }
        fiber.can_apply_endpoint_response(endpoint_id, kind)
    }

    fn preflight_same_island_endpoint_response_command(
        &self,
        endpoint_id: u64,
        kind: &EndpointResponseKind,
    ) -> Result<(), VmError> {
        if !crate::vm::endpoint_response_from_authorized_source(
            self,
            endpoint_id,
            self.state.current_island_id,
        ) {
            return Ok(());
        }
        if matches!(kind, EndpointResponseKind::Closed) {
            return Ok(());
        }
        if self.can_accept_endpoint_response(endpoint_id, kind) {
            return Ok(());
        }
        Err(VmError::Jit(
            "same-island endpoint wake response was rejected".to_string(),
        ))
    }

    fn preflight_island_command_effect(&self, effect: &IslandCommandEffect) -> Result<(), VmError> {
        if effect.island_id == self.state.current_island_id {
            match &effect.command {
                IslandCommand::SpawnFiber { .. } => {
                    return Err(VmError::Jit(
                        "same-island SpawnFiber commands must use transition spawns".to_string(),
                    ));
                }
                IslandCommand::StartEntry { .. } => {
                    return Err(VmError::Jit(
                        "same-island StartEntry commands must use the entry factory API"
                            .to_string(),
                    ));
                }
                IslandCommand::WakeHostEvent { .. } => {
                    return Err(VmError::Jit(
                        "same-island WakeHostEvent commands must use the host wake API".to_string(),
                    ));
                }
                IslandCommand::EndpointRequest { endpoint_id, kind } => {
                    self.preflight_endpoint_request_command(
                        *endpoint_id,
                        kind,
                        self.state.current_island_id,
                    )?;
                }
                IslandCommand::EndpointResponse { endpoint_id, kind } => {
                    self.preflight_same_island_endpoint_response_command(*endpoint_id, kind)?;
                }
                IslandCommand::Shutdown => {}
            }
            return Ok(());
        }
        self.preflight_remote_island_command_effect(effect)?;
        self.preflight_island_route(effect.island_id)
    }

    fn preflight_remote_island_command_effect(
        &self,
        effect: &IslandCommandEffect,
    ) -> Result<(), VmError> {
        match &effect.command {
            IslandCommand::SpawnFiber { .. }
            | IslandCommand::StartEntry { .. }
            | IslandCommand::WakeHostEvent { .. }
            | IslandCommand::Shutdown
            | IslandCommand::EndpointRequest { .. } => {}
            IslandCommand::EndpointResponse { endpoint_id, kind } => {
                if (kind.wait_key().is_some_and(|wait_key| {
                    validate_canonical_fiber_key(
                        wait_key.fiber_key(),
                        "remote EndpointResponse command",
                    )
                    .is_err()
                })) || !crate::vm::endpoint_response_from_authorized_source(
                    self,
                    *endpoint_id,
                    self.state.current_island_id,
                ) {
                    return Err(VmError::Jit(
                        "remote EndpointResponse command was rejected".to_string(),
                    ));
                }
            }
        }
        Ok(())
    }

    fn preflight_island_route(&self, island_id: u32) -> Result<(), VmError> {
        #[cfg(feature = "std")]
        {
            self.state
                .can_route_to_island(island_id)
                .map_err(|error| VmError::Jit(error.to_string()))
        }
        #[cfg(not(feature = "std"))]
        {
            let _ = island_id;
            Ok(())
        }
    }

    fn split_remote_wake_commands_before_commit(
        &self,
        wakes: Vec<WakeCommand>,
    ) -> Result<(Vec<WakeCommand>, Vec<IslandCommandEffect>), VmError> {
        let mut local_wakes = Vec::new();
        let mut remote_commands = Vec::new();
        local_wakes
            .try_reserve(wakes.len())
            .map_err(|_| VmError::Jit("runtime wake plan allocation failed".into()))?;
        remote_commands
            .try_reserve(wakes.len())
            .map_err(|_| VmError::Jit("runtime wake plan allocation failed".into()))?;
        for wake in wakes {
            match self.remote_wake_command(&wake)? {
                Some(command) => remote_commands.push(command),
                None => local_wakes.push(wake),
            }
        }
        Ok((local_wakes, remote_commands))
    }

    fn remote_wake_command(
        &self,
        wake: &WakeCommand,
    ) -> Result<Option<IslandCommandEffect>, VmError> {
        let waiter = wake.waiter();
        if waiter.island_id() == self.state.current_island_id {
            return Ok(None);
        }

        let effect = match wake {
            WakeCommand::Waiter { waiter, .. } => {
                self.preflight_remote_select_wake_shape(waiter)?;
                return Err(VmError::Jit(
                    "remote queue waiter wake must use an endpoint response".to_string(),
                ));
            }
            WakeCommand::ClosedReceiver {
                waiter,
                endpoint_id,
            } => {
                let Some(endpoint_id) = *endpoint_id else {
                    return Err(VmError::Jit(
                        "closed endpoint receiver wake missing endpoint id".to_string(),
                    ));
                };
                let Some(wait_key) = waiter.endpoint_wait_key() else {
                    return Err(VmError::Jit(
                        "closed endpoint receiver wake missing wait identity".to_string(),
                    ));
                };
                IslandCommandEffect::endpoint_response(
                    waiter.island_id(),
                    endpoint_id,
                    EndpointResponseKind::RecvData {
                        data: Vec::new(),
                        closed: true,
                        wait_key,
                    },
                )
            }
            WakeCommand::ClosedSender {
                waiter,
                endpoint_id,
            } => {
                let Some(endpoint_id) = *endpoint_id else {
                    return Err(VmError::Jit(
                        "closed endpoint sender wake missing endpoint id".to_string(),
                    ));
                };
                let Some(wait_key) = waiter.endpoint_wait_key() else {
                    return Err(VmError::Jit(
                        "closed endpoint sender wake missing wait identity".to_string(),
                    ));
                };
                IslandCommandEffect::endpoint_response(
                    waiter.island_id(),
                    endpoint_id,
                    EndpointResponseKind::SendAck {
                        closed: true,
                        wait_key,
                    },
                )
            }
        };
        Ok(Some(effect))
    }

    #[cfg(feature = "jit")]
    fn select_waiter_rollback_for_pending_transition(
        &self,
        transition: &RuntimeTransition,
    ) -> Option<RuntimeRollback> {
        let mut rollbacks = Vec::new();
        for wake in &transition.wakes {
            if let Some(rollback) = self.select_waiter_rollback_for_pending_wake(wake.waiter()) {
                rollbacks.push(rollback);
            }
        }
        let mut rollbacks = rollbacks.into_iter();
        let first = rollbacks.next()?;
        Some(rollbacks.fold(first, RuntimeRollback::combine))
    }

    #[cfg(feature = "jit")]
    fn select_waiter_rollback_for_pending_wake(
        &self,
        waiter: &QueueWaiter,
    ) -> Option<RuntimeRollback> {
        let select = waiter.select_info()?;
        if waiter.island_id() != self.state.current_island_id {
            return None;
        }
        let wake_key = FiberWakeKey::from_packed(waiter.fiber_key());
        let fiber = self.scheduler.try_get_fiber_by_wake_key(wake_key)?;
        let select_state = fiber.select_state.as_ref()?;
        if select_state.select_id != select.select_id {
            return None;
        }
        let fiber_key = waiter.fiber_key();
        let mut seen_queues = HashSet::new();
        let mut queues = Vec::new();
        for registered in &select_state.registered_queues {
            if registered.queue.is_null() || !seen_queues.insert(registered.queue as usize) {
                continue;
            }
            // Safety: select registration owns a rooted live local queue until cancellation.
            let state = unsafe { queue::local_state_ref(registered.queue) };
            let senders = state
                .waiting_senders
                .iter()
                .enumerate()
                .filter(|(_, (queued, _))| queued.is_select_for(fiber_key, select.select_id))
                .map(|(index, (waiter, message))| (index, waiter.clone(), message.clone()))
                .collect();
            let receivers = state
                .waiting_receivers
                .iter()
                .enumerate()
                .filter(|(_, queued)| queued.is_select_for(fiber_key, select.select_id))
                .map(|(index, queued)| (index, queued.clone()))
                .collect();
            queues.push(SelectQueueWaiterUndo {
                ch: registered.queue,
                senders,
                receivers,
            });
        }
        Some(RuntimeRollback::select_waiters(
            waiter.fiber_key(),
            fiber.select_state.clone(),
            queues,
        ))
    }

    #[cfg(feature = "jit")]
    fn cancel_select_sibling_waiters_for_transition(&mut self, transition: &RuntimeTransition) {
        for wake in &transition.wakes {
            self.cancel_select_sibling_waiters_for_pending_wake(wake.waiter());
        }
    }

    #[cfg(feature = "jit")]
    fn cancel_select_sibling_waiters_for_pending_wake(&mut self, waiter: &QueueWaiter) {
        let Some(select) = waiter.select_info() else {
            return;
        };
        if waiter.island_id() != self.state.current_island_id {
            return;
        }
        let wake_key = FiberWakeKey::from_packed(waiter.fiber_key());
        let Some(fiber) = self.scheduler.try_get_fiber_mut_by_wake_key(wake_key) else {
            return;
        };
        let Some(select_state) = fiber.select_state.as_mut() else {
            return;
        };
        if select_state.select_id != select.select_id {
            return;
        }
        let mut registered_queues = core::mem::take(&mut select_state.registered_queues);
        registered_queues.sort_unstable_by_key(|registered| registered.queue as usize);
        let mut selected = Vec::new();
        let mut cancelled_queue = None;
        for registered in registered_queues {
            let is_selected = registered.case_index == select.case_index
                && registered.queue as u64 == select.queue_ref
                && registered.kind.wait_kind() == select.kind;
            if is_selected {
                selected.push(registered);
            } else if !registered.queue.is_null()
                && cancelled_queue != Some(registered.queue as usize)
            {
                // Safety: the select state keeps every registered queue rooted and live.
                unsafe {
                    queue::cancel_select_waiters(
                        registered.queue,
                        waiter.fiber_key(),
                        select.select_id,
                    );
                }
                cancelled_queue = Some(registered.queue as usize);
            }
        }
        select_state.registered_queues = selected;
    }

    fn cancel_select_sibling_waiters_for_wake(&mut self, waiter: &QueueWaiter) {
        let Some(select) = waiter.select_info() else {
            return;
        };
        if waiter.island_id() != self.state.current_island_id {
            return;
        }
        let wake_key = FiberWakeKey::from_packed(waiter.fiber_key());
        let Some(fiber) = self.scheduler.try_get_fiber_mut_by_wake_key(wake_key) else {
            return;
        };
        let Some(select_state) = fiber.select_state.as_mut() else {
            return;
        };
        if select_state.select_id == select.select_id {
            crate::exec::cancel_select_waiters(select_state, waiter.fiber_key());
        }
    }

    fn apply_runtime_wake(&mut self, wake: WakeCommand) -> Result<(), VmError> {
        self.validate_queue_wake_payload(&wake)
            .map_err(VmError::Jit)?;
        match wake {
            WakeCommand::Waiter {
                waiter,
                select_result,
            } => {
                if !self
                    .state
                    .wake_waiter(&waiter, select_result, &mut self.scheduler)
                    .map_err(VmError::Jit)?
                {
                    return Err(VmError::Jit(
                        "runtime queue waiter wake was rejected".to_string(),
                    ));
                }
                self.cancel_select_sibling_waiters_for_wake(&waiter);
                Ok(())
            }
            WakeCommand::ClosedReceiver {
                waiter,
                endpoint_id,
            } => {
                if let Some(wait_key) = waiter.endpoint_wait_key() {
                    let Some(endpoint_id) = endpoint_id else {
                        return Err(VmError::Jit(
                            "closed endpoint receiver wake missing endpoint id".to_string(),
                        ));
                    };
                    self.apply_endpoint_response_for_waiter(
                        &waiter,
                        endpoint_id,
                        EndpointResponseKind::RecvData {
                            data: Vec::new(),
                            closed: true,
                            wait_key,
                        },
                    )?;
                    return Ok(());
                }
                if !self
                    .state
                    .wake_closed_receiver(&waiter, endpoint_id, &mut self.scheduler)
                    .map_err(VmError::Jit)?
                {
                    return Err(VmError::Jit(
                        "runtime closed receiver wake was rejected".to_string(),
                    ));
                }
                self.cancel_select_sibling_waiters_for_wake(&waiter);
                Ok(())
            }
            WakeCommand::ClosedSender {
                waiter,
                endpoint_id,
            } => {
                if let Some(wait_key) = waiter.endpoint_wait_key() {
                    let Some(endpoint_id) = endpoint_id else {
                        return Err(VmError::Jit(
                            "closed endpoint sender wake missing endpoint id".to_string(),
                        ));
                    };
                    self.apply_endpoint_response_for_waiter(
                        &waiter,
                        endpoint_id,
                        EndpointResponseKind::SendAck {
                            closed: true,
                            wait_key,
                        },
                    )?;
                    return Ok(());
                }
                let local_simple_sender = waiter.island_id() == self.state.current_island_id
                    && waiter.select_info().is_none();
                let woke = self
                    .state
                    .wake_closed_sender(&waiter, endpoint_id, &mut self.scheduler)
                    .map_err(VmError::Jit)?;
                if !woke {
                    return Err(VmError::Jit(
                        "runtime closed sender wake was rejected".to_string(),
                    ));
                }
                self.cancel_select_sibling_waiters_for_wake(&waiter);
                if woke && local_simple_sender {
                    let key = FiberWakeKey::from_packed(waiter.fiber_key());
                    let Some(fiber) = self.scheduler.try_get_fiber_mut_by_wake_key(key) else {
                        return Err(VmError::Jit(
                            "closed queue sender wake lost accepted fiber".to_string(),
                        ));
                    };
                    let resume =
                        replay_current_instruction_policy(fiber, "closed queue sender wake")
                            .map_err(VmError::Jit)?;
                    set_current_frame_pc_for_resume(fiber, resume, "closed queue sender wake")
                        .map_err(VmError::Jit)?;
                }
                Ok(())
            }
        }
    }

    fn apply_endpoint_response_for_waiter(
        &mut self,
        waiter: &QueueWaiter,
        endpoint_id: u64,
        kind: EndpointResponseKind,
    ) -> Result<(), VmError> {
        if waiter.island_id() == self.state.current_island_id {
            let from_island = self.state.current_island_id;
            let endpoint_registry_undo = if endpoint_response_kind_is_closed(&kind)
                && !crate::vm::endpoint_response_from_authorized_source(
                    self,
                    endpoint_id,
                    from_island,
                ) {
                if !self.can_accept_endpoint_response(endpoint_id, &kind) {
                    return Err(VmError::Jit(
                        "same-island endpoint wake response was rejected".to_string(),
                    ));
                }
                let foreign_live_remote = self
                    .state
                    .endpoint_registry
                    .get_live(endpoint_id)
                    .is_some_and(|ch| {
                        (unsafe { queue::is_remote(ch) })
                            && unsafe { queue::remote_proxy(ch) }.home_island != from_island
                    });
                let foreign_tombstone = matches!(
                    self.state
                        .endpoint_registry
                        .tombstone_response_source(endpoint_id),
                    Some(Some(source)) if source != from_island
                );
                if !foreign_live_remote && !foreign_tombstone {
                    let mut undo = EndpointRegistryUndo::default();
                    undo.try_reserve(1).map_err(|_| {
                        VmError::Jit("endpoint response rollback allocation failed".into())
                    })?;
                    self.state
                        .endpoint_registry
                        .try_reserve_live(1)
                        .map_err(|_| {
                            VmError::Jit("endpoint response registry allocation failed".into())
                        })?;
                    undo.record(&self.state.endpoint_registry, endpoint_id);
                    self.state
                        .endpoint_registry
                        .mark_tombstone_with_response_source(endpoint_id, Some(from_island));
                    Some(undo)
                } else {
                    None
                }
            } else {
                None
            };
            let outcome = self.apply_runtime_command(RuntimeCommand::endpoint_response(
                endpoint_id,
                from_island,
                kind,
            ));
            if !outcome.applied || !outcome.payload_accepted {
                if let Some(undo) = endpoint_registry_undo {
                    undo.restore(&mut self.state.endpoint_registry);
                }
                return Err(VmError::Jit(
                    "same-island endpoint wake response was rejected".to_string(),
                ));
            }
            return Ok(());
        }
        self.apply_island_command_effect(IslandCommandEffect::endpoint_response(
            waiter.island_id(),
            endpoint_id,
            kind,
        ))
    }

    fn validate_queue_wake_payload(&self, wake: &WakeCommand) -> Result<(), String> {
        let WakeCommand::Waiter {
            waiter,
            select_result,
        } = wake
        else {
            return Ok(());
        };
        match (waiter.select_info(), select_result) {
            (None, None) => Ok(()),
            (None, Some(_)) => Err("select wake payload attached to non-select waiter".to_string()),
            (Some(select), None) if select.kind == SelectWaitKind::Recv => {
                Err("select recv wake missing payload".to_string())
            }
            (Some(select), None) if select.kind == SelectWaitKind::Send => {
                Err("select send wake missing payload".to_string())
            }
            (Some(select), Some(SelectWokenResult::SendAccepted))
                if select.kind == SelectWaitKind::Send =>
            {
                Ok(())
            }
            (
                Some(select),
                Some(SelectWokenResult::Recv {
                    data,
                    slot_types,
                    closed,
                }),
            ) if select.kind == SelectWaitKind::Recv => {
                let ch = crate::exec::validate_queue_handle(
                    &self.state.gc,
                    select.queue_ref as vo_runtime::gc::GcRef,
                    "select wake recv payload",
                )?;
                let expected_slot_types = crate::exec::queue::select_woken_recv_slot_types(
                    ch,
                    self.module_runtime_metadata(),
                )?;
                crate::exec::queue::validate_select_woken_recv_payload_layout(
                    data.len(),
                    slot_types,
                    &expected_slot_types,
                    *closed,
                )
            }
            (Some(_), Some(SelectWokenResult::SendAccepted)) => {
                Err("select send wake payload attached to non-send waiter".to_string())
            }
            (Some(_), Some(SelectWokenResult::Recv { .. })) => {
                Err("select recv wake payload attached to non-recv waiter".to_string())
            }
            (Some(_), None) => Err("select wake missing payload".to_string()),
        }
    }

    fn apply_island_command_effect(&mut self, effect: IslandCommandEffect) -> Result<(), VmError> {
        let expects_response = effect.expects_response();
        if effect.island_id == self.state.current_island_id {
            if expects_response {
                self.state.pending_island_responses = self
                    .state
                    .pending_island_responses
                    .checked_add(1)
                    .ok_or_else(|| {
                        VmError::Jit(
                            "pending island response identity space exhausted during local commit"
                                .to_string(),
                        )
                    })?;
            }
            let result = self.dispatch_island_command(effect.command);
            if result.is_err() && expects_response {
                self.state.pending_island_responses =
                    self.state.pending_island_responses.saturating_sub(1);
            }
            return result;
        }

        #[cfg(feature = "std")]
        {
            if self.state.external_island_transport {
                self.state.outbound_commands.push_back((
                    effect.island_id,
                    vo_runtime::island::IslandCommandEnvelope::new(
                        self.state.current_island_id,
                        effect.command,
                    ),
                ));
            } else {
                self.state
                    .try_send_to_island(effect.island_id, effect.command)
                    .map_err(|error| VmError::Jit(error.to_string()))?;
            }
        }
        #[cfg(not(feature = "std"))]
        {
            self.state.outbound_commands.push_back((
                effect.island_id,
                vo_runtime::island::IslandCommandEnvelope::new(
                    self.state.current_island_id,
                    effect.command,
                ),
            ));
        }
        if expects_response {
            self.state.pending_island_responses = self
                .state
                .pending_island_responses
                .checked_add(1)
                .ok_or_else(|| {
                    VmError::Jit(
                        "pending island response identity space exhausted during direct commit"
                            .to_string(),
                    )
                })?;
        }
        Ok(())
    }

    fn stage_remote_island_commands_before_commit(
        &mut self,
        _current_fiber: Option<FiberId>,
        island_commands: Vec<IslandCommandEffect>,
    ) -> Result<(Vec<IslandCommandEffect>, Vec<RemoteIslandCommandCommit>), VmError> {
        let mut local_commands = Vec::new();
        let mut remote_commands = Vec::new();
        local_commands
            .try_reserve(island_commands.len())
            .map_err(|_| VmError::Jit("runtime command plan allocation failed".into()))?;
        remote_commands
            .try_reserve(island_commands.len())
            .map_err(|_| VmError::Jit("runtime command plan allocation failed".into()))?;
        for effect in island_commands {
            if effect.island_id == self.state.current_island_id {
                local_commands.push(effect);
                continue;
            }

            #[cfg(feature = "std")]
            let reservation = if self.state.external_island_transport {
                None
            } else {
                match self.state.reserve_send_to_island(effect.island_id) {
                    Ok(reservation) => Some(reservation),
                    Err(error) => {
                        self.rollback_current_remote_endpoint_wait(_current_fiber);
                        return Err(VmError::Jit(error.to_string()));
                    }
                }
            };

            remote_commands.push(RemoteIslandCommandCommit {
                island_id: effect.island_id,
                command: effect.command,
                #[cfg(feature = "std")]
                reservation,
            });
        }
        Ok((local_commands, remote_commands))
    }

    fn commit_remote_island_commands(&mut self, remote_commands: Vec<RemoteIslandCommandCommit>) {
        for effect in remote_commands {
            let expects_response = island_command_expects_response(&effect.command);
            #[cfg(feature = "std")]
            {
                if let Some(reservation) = effect.reservation {
                    reservation.send(self.state.current_island_id, effect.command);
                } else {
                    self.state.outbound_commands.push_back((
                        effect.island_id,
                        vo_runtime::island::IslandCommandEnvelope::new(
                            self.state.current_island_id,
                            effect.command,
                        ),
                    ));
                }
            }
            #[cfg(not(feature = "std"))]
            {
                self.state.outbound_commands.push_back((
                    effect.island_id,
                    vo_runtime::island::IslandCommandEnvelope::new(
                        self.state.current_island_id,
                        effect.command,
                    ),
                ));
            }
            if expects_response {
                self.state.pending_island_responses = self
                    .state
                    .pending_island_responses
                    .checked_add(1)
                    .expect("pending island response capacity was preflighted before commit");
            }
        }
    }

    fn rollback_current_remote_endpoint_wait(&mut self, current_fiber: Option<FiberId>) {
        let Some(current_fiber) = current_fiber else {
            return;
        };
        if let Some(fiber) = self.scheduler.try_get_fiber_mut(current_fiber) {
            fiber.remote_endpoint_wait = None;
        }
    }

    fn restore_runtime_rollback(
        &mut self,
        current_fiber: Option<FiberId>,
        rollback: RuntimeRollback,
    ) {
        rollback.restore(&mut self.state, &mut self.scheduler, current_fiber);
        self.mark_gc_all_roots_dirty();
    }

    fn apply_pending_spawns(&mut self, spawns: Vec<PendingSpawn>) -> Result<(), VmError> {
        for spawn in spawns {
            self.scheduler
                .try_spawn_pending(spawn)
                .map_err(scheduler_error_to_vm_error)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests;
