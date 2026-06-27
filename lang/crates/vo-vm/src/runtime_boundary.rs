use crate::fiber::{BlockReason, Fiber, SelectState, SelectWokenResult};
#[cfg(feature = "std")]
use crate::scheduler::IoWaitKey;
use crate::scheduler::{FiberId, FiberWakeKey, HostWaitKey, WaitRegistrationKey, WaitSource};
use crate::vm::{
    EndpointRegistrySnapshot, ExecResult, GcRootEffect, SchedulingOutcome, Vm, VmError, VmState,
};
use vo_runtime::gc::GcRef;
use vo_runtime::island::{EndpointRequestKind, EndpointResponseKind, IslandCommand};
#[cfg(feature = "std")]
use vo_runtime::island_transport::IslandSendReservation;
use vo_runtime::objects::{
    queue,
    queue_state::{LocalQueueState, QueueWaiter, SelectWaitKind},
};

#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
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
    Ok(ResumePolicy::ReplayCurrentInstruction { pc: pc as u32 })
}

#[derive(Debug)]
pub struct RuntimeTransition {
    pub boundary: RuntimeBoundary,
    pub resume: ResumePolicy,
    pub wakes: Vec<WakeCommand>,
    pub gc_roots: GcRootEffect,
    pub island_commands: Vec<IslandCommandEffect>,
    pub endpoint_tombstones: Vec<EndpointTombstone>,
    pub spawns: Vec<Fiber>,
    rollback: Option<RuntimeRollback>,
    #[cfg(feature = "jit")]
    pub pending_terminal_policy: PendingTransitionTerminalPolicy,
}

#[derive(Debug)]
pub(crate) enum RuntimeRollback {
    LocalQueue {
        ch: GcRef,
        state: LocalQueueState,
        endpoint_registry: EndpointRegistrySnapshot,
        stack_slots: Vec<(usize, u64)>,
        select_state: Option<Option<SelectState>>,
    },
    RemoteQueueProxy {
        ch: GcRef,
        endpoint_id: u64,
        home_island: u32,
        closed: bool,
        endpoint_registry: EndpointRegistrySnapshot,
    },
    EndpointTransfer {
        endpoint_registry: EndpointRegistrySnapshot,
        home_infos: Vec<(GcRef, Option<queue::HomeInfoSnapshot>)>,
    },
    #[cfg(feature = "jit")]
    SelectWaiters {
        fiber_key: u64,
        select_state: Option<SelectState>,
        queues: Vec<(GcRef, LocalQueueState)>,
    },
    Composite(Vec<RuntimeRollback>),
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

    pub(crate) fn local_queue(vm_state: &VmState, ch: GcRef) -> Self {
        Self::local_queue_with_stack_slots(vm_state, ch, Vec::new())
    }

    pub(crate) fn remote_queue_proxy(vm_state: &VmState, ch: GcRef) -> Self {
        let proxy = queue::remote_proxy(ch);
        Self::RemoteQueueProxy {
            ch,
            endpoint_id: proxy.endpoint_id,
            home_island: proxy.home_island,
            closed: proxy.closed,
            endpoint_registry: vm_state.endpoint_registry.snapshot(),
        }
    }

    pub(crate) fn endpoint_transfer(
        endpoint_registry: EndpointRegistrySnapshot,
        home_infos: Vec<(GcRef, Option<queue::HomeInfoSnapshot>)>,
    ) -> Self {
        Self::EndpointTransfer {
            endpoint_registry,
            home_infos,
        }
    }

    pub(crate) fn local_queue_with_stack_slots(
        vm_state: &VmState,
        ch: GcRef,
        stack_slots: Vec<(usize, u64)>,
    ) -> Self {
        Self::LocalQueue {
            ch,
            state: queue::local_state(ch).clone(),
            endpoint_registry: vm_state.endpoint_registry.snapshot(),
            stack_slots,
            select_state: None,
        }
    }

    pub(crate) fn push_stack_slot(&mut self, index: usize, value: u64) {
        match self {
            Self::LocalQueue { stack_slots, .. } => stack_slots.push((index, value)),
            Self::RemoteQueueProxy { .. } => {}
            Self::EndpointTransfer { .. } => {}
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
            Self::LocalQueue { select_state, .. } => *select_state = Some(state),
            Self::RemoteQueueProxy { .. } => {}
            Self::EndpointTransfer { .. } => {}
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
        queues: Vec<(GcRef, LocalQueueState)>,
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
            Self::LocalQueue {
                ch,
                state,
                endpoint_registry,
                stack_slots,
                select_state,
            } => {
                queue::with_local_state(ch, |local_state| {
                    *local_state = state;
                });
                vm_state.endpoint_registry.restore(endpoint_registry);
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
                endpoint_registry,
            } => {
                let proxy = queue::remote_proxy_mut(ch);
                proxy.endpoint_id = endpoint_id;
                proxy.home_island = home_island;
                proxy.closed = closed;
                vm_state.endpoint_registry.restore(endpoint_registry);
            }
            Self::EndpointTransfer {
                endpoint_registry,
                home_infos,
            } => {
                for (ch, snapshot) in home_infos {
                    queue::restore_home_info_snapshot(ch, snapshot);
                }
                vm_state.endpoint_registry.restore(endpoint_registry);
            }
            #[cfg(feature = "jit")]
            Self::SelectWaiters {
                fiber_key,
                select_state,
                queues,
            } => {
                for (ch, state) in queues {
                    queue::with_local_state(ch, |local_state| {
                        *local_state = state;
                    });
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
            rollback: None,
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
        push_queue_close_wake(&mut self.wakes, wake);
    }

    pub(crate) fn set_rollback(&mut self, rollback: RuntimeRollback) {
        self.rollback = Some(match self.rollback.take() {
            Some(existing) => RuntimeRollback::combine(existing, rollback),
            None => rollback,
        });
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
        self.island_commands.append(&mut other.island_commands);
        self.endpoint_tombstones
            .append(&mut other.endpoint_tombstones);
        self.spawns.append(&mut other.spawns);
        if let Some(rollback) = other.rollback.take() {
            self.set_rollback(rollback);
        }
    }

    #[cfg(feature = "jit")]
    pub(crate) fn discard_pending_response_island_commands(&mut self) {
        self.island_commands
            .retain(|effect| !effect.pending_response);
    }
}

pub(crate) fn push_queue_close_wake(wakes: &mut Vec<WakeCommand>, wake: WakeCommand) {
    debug_assert!(matches!(
        wake.payload,
        RuntimePayload::QueueWake(
            QueueRuntimeWake::ClosedReceiver { .. } | QueueRuntimeWake::ClosedSender { .. }
        )
    ));
    if let Some(key) = wake.select_activation_key() {
        if wakes
            .iter()
            .any(|existing| existing.select_activation_key() == Some(key))
        {
            return;
        }
    }
    wakes.push(wake);
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
pub struct WakeCommand {
    pub source: WaitSource,
    pub wake_key: FiberWakeKey,
    pub registration: WaitRegistrationKey,
    pub payload: RuntimePayload,
    pub gc_roots: GcRootEffect,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SelectActivationWakeKey {
    island_id: u32,
    fiber_key: u64,
    endpoint_wait_id: u64,
    select_id: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct QueueActivationWakeKey {
    source: WaitSource,
    wake_key: FiberWakeKey,
    island_id: u32,
    registration_id: u64,
    endpoint_wait_id: u64,
    queue_ref: u64,
    kind: Option<SelectWaitKind>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct EndpointResponseActivationKey {
    endpoint_id: u64,
    fiber_key: u64,
    wait_id: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct EndpointResponseAuthorizationSource {
    endpoint_id: u64,
    from_island: u32,
    target_island: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct EndpointRequestActivationKey {
    endpoint_id: u64,
    fiber_key: u64,
    wait_id: u64,
}

fn endpoint_request_expects_pending_response(kind: &EndpointRequestKind) -> bool {
    matches!(
        kind,
        EndpointRequestKind::Send { .. } | EndpointRequestKind::Recv
    )
}

fn validate_endpoint_request_pending_response(
    kind: &EndpointRequestKind,
    pending_response: bool,
) -> Result<(), VmError> {
    if pending_response == endpoint_request_expects_pending_response(kind) {
        return Ok(());
    }
    Err(VmError::Jit(
        "EndpointRequest pending-response contract was rejected".to_string(),
    ))
}

fn validate_non_request_pending_response(
    command_name: &str,
    pending_response: bool,
) -> Result<(), VmError> {
    if !pending_response {
        return Ok(());
    }
    Err(VmError::Jit(format!(
        "{command_name} pending-response contract was rejected"
    )))
}

fn validate_same_island_endpoint_request_source(
    current_island: u32,
    from_island: u32,
) -> Result<(), VmError> {
    if from_island == current_island {
        return Ok(());
    }
    Err(VmError::Jit(
        "same-island EndpointRequest source was rejected".to_string(),
    ))
}

fn validate_same_island_endpoint_response_source(
    current_island: u32,
    from_island: u32,
) -> Result<(), VmError> {
    if from_island == current_island {
        return Ok(());
    }
    Err(VmError::Jit(
        "same-island EndpointResponse source was rejected".to_string(),
    ))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WakeActivationKey {
    Select(SelectActivationWakeKey),
    Queue(QueueActivationWakeKey),
}

#[derive(Debug)]
pub struct IslandCommandEffect {
    pub island_id: u32,
    pub command: IslandCommand,
    pub pending_response: bool,
}

impl IslandCommandEffect {
    pub fn spawn_fiber(island_id: u32, closure_data: vo_runtime::pack::PackedValue) -> Self {
        Self {
            island_id,
            command: IslandCommand::SpawnFiber { closure_data },
            pending_response: false,
        }
    }

    pub fn endpoint_send_request(
        island_id: u32,
        endpoint_id: u64,
        data: Vec<u8>,
        from_island: u32,
        fiber_key: u64,
        wait_id: u64,
    ) -> Self {
        Self {
            island_id,
            command: IslandCommand::EndpointRequest {
                endpoint_id,
                kind: EndpointRequestKind::Send { data },
                from_island,
                fiber_key,
                wait_id,
            },
            pending_response: true,
        }
    }

    pub fn endpoint_recv_request(
        island_id: u32,
        endpoint_id: u64,
        from_island: u32,
        fiber_key: u64,
        wait_id: u64,
    ) -> Self {
        Self {
            island_id,
            command: IslandCommand::EndpointRequest {
                endpoint_id,
                kind: EndpointRequestKind::Recv,
                from_island,
                fiber_key,
                wait_id,
            },
            pending_response: true,
        }
    }

    pub fn endpoint_transfer_request(
        island_id: u32,
        endpoint_id: u64,
        new_peer: u32,
        from_island: u32,
    ) -> Self {
        Self {
            island_id,
            command: IslandCommand::EndpointRequest {
                endpoint_id,
                kind: EndpointRequestKind::Transfer { new_peer },
                from_island,
                fiber_key: 0,
                wait_id: 0,
            },
            pending_response: false,
        }
    }

    pub fn endpoint_close_request(island_id: u32, endpoint_id: u64, from_island: u32) -> Self {
        Self {
            island_id,
            command: IslandCommand::EndpointRequest {
                endpoint_id,
                kind: EndpointRequestKind::Close,
                from_island,
                fiber_key: 0,
                wait_id: 0,
            },
            pending_response: false,
        }
    }

    pub fn endpoint_response(
        island_id: u32,
        from_island: u32,
        endpoint_id: u64,
        kind: EndpointResponseKind,
        fiber_key: u64,
        wait_id: u64,
    ) -> Self {
        Self {
            island_id,
            command: IslandCommand::EndpointResponse {
                endpoint_id,
                kind,
                from_island,
                fiber_key,
                wait_id,
            },
            pending_response: false,
        }
    }

    pub fn endpoint_recv_data_response(
        island_id: u32,
        from_island: u32,
        endpoint_id: u64,
        data: Vec<u8>,
        fiber_key: u64,
        wait_id: u64,
    ) -> Self {
        Self {
            island_id,
            command: IslandCommand::EndpointResponse {
                endpoint_id,
                kind: EndpointResponseKind::RecvData {
                    data,
                    closed: false,
                },
                from_island,
                fiber_key,
                wait_id,
            },
            pending_response: false,
        }
    }
}

struct RemoteIslandCommandCommit {
    island_id: u32,
    command: IslandCommand,
    pending_response: bool,
    #[cfg(feature = "std")]
    reservation: Option<Box<dyn IslandSendReservation>>,
}

impl WakeCommand {
    pub fn queue_waiter(waiter: QueueWaiter) -> Self {
        let select_result = match waiter.select.as_ref().map(|select| select.kind) {
            Some(SelectWaitKind::Send) => Some(SelectWokenResult::SendAccepted),
            _ => None,
        };
        Self::queue(QueueRuntimeWake::Waiter {
            waiter,
            select_result,
        })
    }

    pub fn queue_waiter_with_result(waiter: QueueWaiter, select_result: SelectWokenResult) -> Self {
        Self::queue(QueueRuntimeWake::Waiter {
            waiter,
            select_result: Some(select_result),
        })
    }

    pub fn queue_closed_receiver(waiter: QueueWaiter, endpoint_id: Option<u64>) -> Self {
        Self::queue(QueueRuntimeWake::ClosedReceiver {
            waiter,
            endpoint_id,
        })
    }

    pub fn queue_closed_sender(waiter: QueueWaiter, endpoint_id: Option<u64>) -> Self {
        Self::queue(QueueRuntimeWake::ClosedSender {
            waiter,
            endpoint_id,
        })
    }

    fn queue(wake: QueueRuntimeWake) -> Self {
        let source = wake.source();
        let wake_key = FiberWakeKey::from_packed(wake.waiter().fiber_key());
        let registration = queue_wake_registration(source, &wake);
        Self {
            source,
            wake_key,
            registration,
            payload: RuntimePayload::QueueWake(wake),
            gc_roots: GcRootEffect::AllRootsDirty,
        }
    }

    fn select_activation_key(&self) -> Option<SelectActivationWakeKey> {
        match &self.payload {
            RuntimePayload::QueueWake(wake) => wake.select_activation_key(),
            _ => None,
        }
    }

    fn activation_key(&self) -> Option<WakeActivationKey> {
        if let Some(select_key) = self.select_activation_key() {
            return Some(WakeActivationKey::Select(select_key));
        }
        match &self.payload {
            RuntimePayload::QueueWake(wake) => {
                let waiter = wake.waiter();
                Some(WakeActivationKey::Queue(QueueActivationWakeKey {
                    source: self.source,
                    wake_key: self.wake_key,
                    island_id: waiter.island_id,
                    registration_id: waiter.registration_id,
                    endpoint_wait_id: waiter.endpoint_wait_id(),
                    queue_ref: waiter.queue_ref,
                    kind: waiter.kind,
                }))
            }
            _ => None,
        }
    }

    #[cfg(feature = "jit")]
    fn is_queue_close_wake(&self) -> bool {
        matches!(
            self.payload,
            RuntimePayload::QueueWake(
                QueueRuntimeWake::ClosedReceiver { .. } | QueueRuntimeWake::ClosedSender { .. }
            )
        )
    }

    fn validate_queue_identity(&self, wake: &QueueRuntimeWake) -> Result<(), String> {
        let expected_source = wake.source();
        let expected_wake_key = FiberWakeKey::from_packed(wake.waiter().fiber_key());
        let expected_registration = queue_wake_registration(expected_source, wake);
        validate_queue_waiter_identity(wake.waiter())?;
        if self.source != expected_source {
            return Err(format!(
                "queue wake source mismatch: got {:?}, expected {:?}",
                self.source, expected_source
            ));
        }
        if self.wake_key != expected_wake_key {
            return Err("queue wake fiber key mismatch".to_string());
        }
        if self.registration != expected_registration || self.registration.token == 0 {
            return Err("queue wake registration mismatch".to_string());
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeCommand {
    pub source: WaitSource,
    pub target: RuntimeCommandTarget,
    pub wake_key: Option<FiberWakeKey>,
    pub registration: Option<WaitRegistrationKey>,
    pub source_token: SourceWakeToken,
    pub resume: ResumePolicy,
    pub payload: RuntimePayload,
    pub gc_roots: GcRootEffect,
}

impl RuntimeCommand {
    pub fn host_event_wake(key: HostWaitKey) -> Self {
        Self {
            source: key.source.wait_source(),
            target: RuntimeCommandTarget::HostEvent { key },
            wake_key: Some(key.wake_key),
            registration: Some(key.registration),
            source_token: SourceWakeToken::HostEvent(key),
            resume: ResumePolicy::PreserveFramePc,
            payload: RuntimePayload::None,
            gc_roots: GcRootEffect::AllRootsDirty,
        }
    }

    pub fn host_event_replay_wake(key: HostWaitKey) -> Self {
        Self {
            source: key.source.wait_source(),
            target: RuntimeCommandTarget::HostEvent { key },
            wake_key: Some(key.wake_key),
            registration: Some(key.registration),
            source_token: SourceWakeToken::HostEvent(key),
            resume: ResumePolicy::PreserveFramePc,
            payload: RuntimePayload::None,
            gc_roots: GcRootEffect::AllRootsDirty,
        }
    }

    pub fn host_event_wake_with_data(key: HostWaitKey, data: Vec<u8>) -> Self {
        Self {
            source: key.source.wait_source(),
            target: RuntimeCommandTarget::HostEvent { key },
            wake_key: Some(key.wake_key),
            registration: Some(key.registration),
            source_token: SourceWakeToken::HostEvent(key),
            resume: ResumePolicy::PreserveFramePc,
            payload: RuntimePayload::HostEventData(data),
            gc_roots: GcRootEffect::AllRootsDirty,
        }
    }

    #[cfg(feature = "std")]
    pub fn io_ready(key: IoWaitKey) -> Self {
        Self {
            source: WaitSource::Io,
            target: RuntimeCommandTarget::Io { key },
            wake_key: Some(key.wake_key),
            registration: Some(key.registration),
            source_token: SourceWakeToken::Io(key),
            resume: ResumePolicy::PreserveFramePc,
            payload: RuntimePayload::None,
            gc_roots: GcRootEffect::AllRootsDirty,
        }
    }

    pub fn endpoint_response(
        endpoint_id: u64,
        from_island: u32,
        fiber_key: u64,
        wait_id: u64,
        kind: EndpointResponseKind,
    ) -> Self {
        Self {
            source: WaitSource::IslandEndpoint,
            target: RuntimeCommandTarget::EndpointResponse {
                endpoint_id,
                from_island,
                fiber_key,
                wait_id,
            },
            wake_key: Some(FiberWakeKey::new(
                fiber_key as u32,
                (fiber_key >> 32) as u32,
            )),
            registration: None,
            source_token: SourceWakeToken::EndpointResponse {
                endpoint_id,
                from_island,
                fiber_key,
                wait_id,
            },
            resume: ResumePolicy::PreserveFramePc,
            payload: RuntimePayload::EndpointResponse(EndpointRuntimeResponse::from(kind)),
            gc_roots: GcRootEffect::AllRootsDirty,
        }
    }

    pub fn endpoint_closed_response(endpoint_id: u64, from_island: u32) -> Self {
        Self {
            source: WaitSource::IslandEndpoint,
            target: RuntimeCommandTarget::EndpointClosed {
                endpoint_id,
                from_island,
            },
            wake_key: None,
            registration: None,
            source_token: SourceWakeToken::EndpointClosed {
                endpoint_id,
                from_island,
            },
            resume: ResumePolicy::PreserveFramePc,
            payload: RuntimePayload::EndpointResponse(EndpointRuntimeResponse::Closed),
            gc_roots: GcRootEffect::None,
        }
    }

    pub fn island_wake(waiter: QueueWaiter) -> Self {
        let fiber_key = waiter.fiber_key();
        let registration = queue_wake_registration(
            WaitSource::IslandWake,
            &QueueRuntimeWake::Waiter {
                waiter: waiter.clone(),
                select_result: None,
            },
        );
        Self {
            source: WaitSource::IslandWake,
            target: RuntimeCommandTarget::IslandWake {
                waiter: waiter.clone(),
            },
            wake_key: Some(FiberWakeKey::from_packed(fiber_key)),
            registration: Some(registration),
            source_token: SourceWakeToken::IslandWake(waiter),
            resume: ResumePolicy::PreserveFramePc,
            payload: RuntimePayload::None,
            gc_roots: GcRootEffect::AllRootsDirty,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeCommandTarget {
    HostEvent {
        key: HostWaitKey,
    },
    #[cfg(feature = "std")]
    Io {
        key: IoWaitKey,
    },
    EndpointResponse {
        endpoint_id: u64,
        from_island: u32,
        fiber_key: u64,
        wait_id: u64,
    },
    EndpointClosed {
        endpoint_id: u64,
        from_island: u32,
    },
    IslandWake {
        waiter: QueueWaiter,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceWakeToken {
    None,
    HostEvent(HostWaitKey),
    #[cfg(feature = "std")]
    Io(IoWaitKey),
    EndpointResponse {
        endpoint_id: u64,
        from_island: u32,
        fiber_key: u64,
        wait_id: u64,
    },
    EndpointClosed {
        endpoint_id: u64,
        from_island: u32,
    },
    IslandWake(QueueWaiter),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimePayload {
    None,
    HostEventData(Vec<u8>),
    EndpointResponse(EndpointRuntimeResponse),
    QueueWake(QueueRuntimeWake),
}

impl QueueRuntimeWake {
    fn waiter(&self) -> &QueueWaiter {
        match self {
            Self::Waiter { waiter, .. }
            | Self::ClosedReceiver { waiter, .. }
            | Self::ClosedSender { waiter, .. } => waiter,
        }
    }

    fn source(&self) -> WaitSource {
        if self.waiter().select.is_some() {
            WaitSource::Select
        } else {
            WaitSource::Queue
        }
    }

    fn select_activation_key(&self) -> Option<SelectActivationWakeKey> {
        let waiter = self.waiter();
        let select = waiter.select.as_ref()?;
        Some(SelectActivationWakeKey {
            island_id: waiter.island_id,
            fiber_key: waiter.fiber_key(),
            endpoint_wait_id: waiter.endpoint_wait_id(),
            select_id: select.select_id,
        })
    }
}

fn queue_wake_registration(source: WaitSource, wake: &QueueRuntimeWake) -> WaitRegistrationKey {
    fn mix(mut state: u64, value: u64) -> u64 {
        state ^= value.wrapping_add(0x9e37_79b9_7f4a_7c15);
        state = state.rotate_left(27).wrapping_mul(0x94d0_49bb_1331_11eb);
        state
    }

    let waiter = wake.waiter();
    let mut token = mix(0xcbf2_9ce4_8422_2325, source as u64);
    token = mix(token, waiter.island_id as u64);
    token = mix(token, waiter.fiber_key());
    token = mix(token, waiter.registration_id);
    token = mix(token, waiter.endpoint_wait_id());
    token = mix(token, waiter.queue_ref);
    token = mix(
        token,
        waiter
            .kind
            .map(|kind| kind.to_raw() as u64 + 1)
            .unwrap_or(0),
    );
    if let Some(select) = waiter.select.as_ref() {
        token = mix(token, select.case_index as u64);
        token = mix(token, select.select_id);
        token = mix(token, select.queue_ref);
        token = mix(token, select.kind.to_raw() as u64);
    }
    token = match wake {
        QueueRuntimeWake::Waiter { .. } => mix(token, 1),
        QueueRuntimeWake::ClosedReceiver { endpoint_id, .. } => {
            mix(mix(token, 2), endpoint_id.unwrap_or(0))
        }
        QueueRuntimeWake::ClosedSender { endpoint_id, .. } => {
            mix(mix(token, 3), endpoint_id.unwrap_or(0))
        }
    };
    if token == 0 {
        token = 1;
    }
    WaitRegistrationKey { token }
}

fn validate_queue_waiter_identity(waiter: &QueueWaiter) -> Result<(), String> {
    if waiter.endpoint_wait_id() != 0 {
        return Ok(());
    }
    if let Some(select) = waiter.select.as_ref() {
        if waiter.registration_id == 0 {
            return Err("queue wake missing wait registration identity".to_string());
        }
        if waiter.queue_ref != select.queue_ref || waiter.kind != Some(select.kind) {
            return Err("queue wake select identity mismatch".to_string());
        }
        return Ok(());
    }
    if waiter.kind.is_some() && waiter.registration_id != 0 {
        return Ok(());
    }
    Err("queue wake missing wait registration identity".to_string())
}

pub(crate) fn validate_canonical_fiber_key(key: u64, context: &str) -> Result<(), String> {
    if FiberWakeKey::from_packed(key).generation == 0 {
        return Err(format!("{context} used raw fiber slot identity"));
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QueueRuntimeWake {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EndpointRuntimeResponse {
    SendAck { closed: bool },
    RecvData { data: Vec<u8>, closed: bool },
    RecvError,
    Closed,
}

impl From<EndpointResponseKind> for EndpointRuntimeResponse {
    fn from(kind: EndpointResponseKind) -> Self {
        match kind {
            EndpointResponseKind::SendAck { closed } => Self::SendAck { closed },
            EndpointResponseKind::RecvData { data, closed } => Self::RecvData { data, closed },
            EndpointResponseKind::RecvError => Self::RecvError,
            EndpointResponseKind::Closed => Self::Closed,
        }
    }
}

impl EndpointRuntimeResponse {
    fn into_endpoint_response_kind(self) -> EndpointResponseKind {
        match self {
            Self::SendAck { closed } => EndpointResponseKind::SendAck { closed },
            Self::RecvData { data, closed } => EndpointResponseKind::RecvData { data, closed },
            Self::RecvError => EndpointResponseKind::RecvError,
            Self::Closed => EndpointResponseKind::Closed,
        }
    }
}

fn endpoint_response_kind_is_closed(kind: &EndpointResponseKind) -> bool {
    matches!(
        kind,
        EndpointResponseKind::Closed
            | EndpointResponseKind::SendAck { closed: true }
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
        self.state.pending_runtime_transitions.push(transition);
    }

    #[cfg(feature = "jit")]
    pub(crate) fn discard_pending_response_island_commands_from_pending_transitions(&mut self) {
        for pending in &mut self.state.pending_runtime_transitions {
            pending.discard_pending_response_island_commands();
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
        for pending in core::mem::take(&mut self.state.pending_runtime_transitions) {
            self.restore_pending_runtime_transition_rollback(pending);
        }
    }

    #[cfg(feature = "jit")]
    fn drain_pending_runtime_transitions_into(
        &mut self,
        transition: &mut RuntimeTransition,
        should_commit: impl Fn(PendingTransitionTerminalPolicy) -> bool,
    ) -> bool {
        let mut committed_any = false;
        for pending in core::mem::take(&mut self.state.pending_runtime_transitions) {
            if should_commit(pending.pending_terminal_policy) {
                transition.merge_side_effects_from(pending);
                committed_any = true;
            } else {
                self.restore_pending_runtime_transition_rollback(pending);
            }
        }
        committed_any
    }

    #[cfg(feature = "jit")]
    pub(crate) fn attach_pending_runtime_transitions(&mut self, result: ExecResult) -> ExecResult {
        if self.state.pending_runtime_transitions.is_empty() {
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

        if let Err(err) = self.preflight_runtime_transition(current_fiber, &transition) {
            return self.reject_runtime_transition_before_commit(
                current_fiber,
                &mut transition,
                err,
            );
        }

        let RuntimeTransition {
            boundary,
            resume,
            wakes,
            gc_roots,
            island_commands,
            endpoint_tombstones,
            spawns,
            rollback,
            #[cfg(feature = "jit")]
                pending_terminal_policy: _,
        } = transition;
        let mut rollback = rollback;

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

        self.apply_resume_policy(current_fiber, resume, "runtime transition")?;

        let gc_roots = if spawns.is_empty() && endpoint_tombstones.is_empty() {
            gc_roots
        } else {
            merge_gc_root_effects(gc_roots, GcRootEffect::AllRootsDirty)
        };

        for wake in wakes {
            self.apply_gc_root_effect(wake.gc_roots, current_fiber);
            self.apply_runtime_wake(wake)?;
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
            self.apply_block_boundary(reason.clone());
        }
        for command in island_commands {
            self.apply_island_command_effect(command)?;
        }
        self.apply_pending_spawns(spawns)?;
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

    fn apply_block_boundary(&mut self, reason: BlockReason) {
        match reason {
            BlockReason::Queue => self.scheduler.block_for_queue(),
            #[cfg(feature = "std")]
            BlockReason::Io(token) => self.scheduler.block_for_io(token),
            BlockReason::HostEvent { token, delay_ms } => {
                self.scheduler.block_for_host_event(token, delay_ms)
            }
            BlockReason::HostEventReplay { token, source } => {
                self.scheduler.block_for_host_event_replay(token, source)
            }
        }
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
            ExecResult::JitError(message) => RuntimeBoundary::FatalInfra(message.clone()),
            ExecResult::Done => RuntimeBoundary::Done,
            ExecResult::FrameChanged | ExecResult::CallClosure { .. } => RuntimeBoundary::Continue,
        }
    }

    pub(crate) fn apply_runtime_command(
        &mut self,
        command: RuntimeCommand,
    ) -> RuntimeCommandOutcome {
        let gc_roots = command.gc_roots;
        match command.target {
            RuntimeCommandTarget::HostEvent { key } => {
                if command.source != key.source.wait_source()
                    || command.wake_key != Some(key.wake_key)
                    || command.registration != Some(key.registration)
                    || command.source_token != SourceWakeToken::HostEvent(key)
                {
                    return RuntimeCommandOutcome {
                        applied: false,
                        payload_accepted: false,
                    };
                }
                match command.payload {
                    RuntimePayload::None => {
                        self.apply_gc_root_effect(gc_roots, None);
                        let applied = self.scheduler.wake_host_event(key);
                        RuntimeCommandOutcome {
                            applied,
                            payload_accepted: false,
                        }
                    }
                    RuntimePayload::HostEventData(data) => {
                        self.apply_gc_root_effect(gc_roots, None);
                        let payload_accepted = self.scheduler.wake_host_event_with_data(key, data);
                        RuntimeCommandOutcome {
                            applied: payload_accepted,
                            payload_accepted,
                        }
                    }
                    RuntimePayload::EndpointResponse(_) | RuntimePayload::QueueWake(_) => {
                        RuntimeCommandOutcome {
                            applied: false,
                            payload_accepted: false,
                        }
                    }
                }
            }
            #[cfg(feature = "std")]
            RuntimeCommandTarget::Io { key } => {
                if !matches!(command.source, WaitSource::Io)
                    || command.wake_key != Some(key.wake_key)
                    || command.registration != Some(key.registration)
                    || command.source_token != SourceWakeToken::Io(key)
                {
                    return RuntimeCommandOutcome {
                        applied: false,
                        payload_accepted: false,
                    };
                }
                self.apply_gc_root_effect(gc_roots, None);
                let applied = self.scheduler.wake_io(key);
                RuntimeCommandOutcome {
                    applied,
                    payload_accepted: applied,
                }
            }
            RuntimeCommandTarget::EndpointResponse {
                endpoint_id,
                from_island,
                fiber_key,
                wait_id,
            } => {
                if !matches!(command.source, WaitSource::IslandEndpoint)
                    || command.wake_key != Some(FiberWakeKey::from_packed(fiber_key))
                {
                    return RuntimeCommandOutcome {
                        applied: false,
                        payload_accepted: false,
                    };
                }
                if !matches!(
                    command.source_token,
                    SourceWakeToken::EndpointResponse {
                        endpoint_id: source_endpoint_id,
                        from_island: source_from_island,
                        fiber_key: source_fiber_key,
                        wait_id: source_wait_id,
                    } if source_endpoint_id == endpoint_id
                        && source_from_island == from_island
                        && source_fiber_key == fiber_key
                        && source_wait_id == wait_id
                ) {
                    return RuntimeCommandOutcome {
                        applied: false,
                        payload_accepted: false,
                    };
                }
                let RuntimePayload::EndpointResponse(response) = command.payload else {
                    return RuntimeCommandOutcome {
                        applied: false,
                        payload_accepted: false,
                    };
                };
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
                    let kind = response.into_endpoint_response_kind();
                    let replay_closed_send = match &kind {
                        EndpointResponseKind::SendAck { closed } => *closed,
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
                    if !fiber.apply_endpoint_response(endpoint_id, wait_id, &kind) {
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
                self.apply_gc_root_effect(gc_roots, None);
                self.state.pending_island_responses -= 1;
                let applied = self.scheduler.try_wake_fiber(fid);
                RuntimeCommandOutcome {
                    applied,
                    payload_accepted: applied,
                }
            }
            RuntimeCommandTarget::EndpointClosed {
                endpoint_id,
                from_island,
            } => {
                if command.source != WaitSource::IslandEndpoint
                    || command.source_token
                        != (SourceWakeToken::EndpointClosed {
                            endpoint_id,
                            from_island,
                        })
                    || command.wake_key.is_some()
                    || command.registration.is_some()
                    || command.payload
                        != RuntimePayload::EndpointResponse(EndpointRuntimeResponse::Closed)
                {
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
                self.apply_gc_root_effect(gc_roots, None);
                if let Some(ch) = self.state.endpoint_registry.get_live(endpoint_id) {
                    if queue::is_remote(ch) {
                        queue::mark_remote_closed(ch);
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
            RuntimeCommandTarget::IslandWake { waiter } => {
                let expected_registration = queue_wake_registration(
                    WaitSource::IslandWake,
                    &QueueRuntimeWake::Waiter {
                        waiter: waiter.clone(),
                        select_result: None,
                    },
                );
                if !matches!(command.source, WaitSource::IslandWake)
                    || command.wake_key != Some(FiberWakeKey::from_packed(waiter.fiber_key()))
                    || command.registration != Some(expected_registration)
                    || command.source_token != SourceWakeToken::IslandWake(waiter.clone())
                    || waiter.island_id != self.state.current_island_id
                    || waiter.select.is_some()
                    || validate_queue_waiter_identity(&waiter).is_err()
                {
                    return RuntimeCommandOutcome {
                        applied: false,
                        payload_accepted: false,
                    };
                }
                let wake_key = FiberWakeKey::from_packed(waiter.fiber_key());
                let target_fiber = self.scheduler.try_get_fiber(wake_key.fiber_id());
                if waiter.endpoint_wait_id() != 0
                    || target_fiber.is_none_or(|fiber| {
                        fiber.generation != wake_key.generation
                            || fiber.remote_endpoint_wait.is_some()
                    })
                {
                    return RuntimeCommandOutcome {
                        applied: false,
                        payload_accepted: false,
                    };
                }
                self.apply_gc_root_effect(gc_roots, None);
                let applied = self.scheduler.wake_queue_waiter(&waiter);
                if applied {
                    self.cancel_select_sibling_waiters_for_wake(&waiter);
                }
                RuntimeCommandOutcome {
                    applied,
                    payload_accepted: applied,
                }
            }
        }
    }

    fn preflight_runtime_transition(
        &self,
        current_fiber: Option<FiberId>,
        transition: &RuntimeTransition,
    ) -> Result<(), VmError> {
        self.validate_resume_policy(current_fiber, transition.resume, "runtime transition")?;
        self.preflight_unique_wake_activations(transition)?;
        self.preflight_unique_endpoint_response_activations(transition)?;
        self.preflight_endpoint_response_capacity(transition)?;
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

    fn preflight_unique_wake_activations(
        &self,
        transition: &RuntimeTransition,
    ) -> Result<(), VmError> {
        let mut seen = Vec::new();
        for wake in &transition.wakes {
            let Some(key) = wake.activation_key() else {
                continue;
            };
            if seen.contains(&key) {
                return Err(VmError::Jit(
                    "runtime transition contains duplicate wake activation".to_string(),
                ));
            }
            seen.push(key);
        }
        for effect in &transition.island_commands {
            let Some(key) = self.wake_activation_key_for_island_command(effect) else {
                continue;
            };
            if seen.contains(&key) {
                return Err(VmError::Jit(
                    "runtime transition contains duplicate wake activation".to_string(),
                ));
            }
            seen.push(key);
        }
        Ok(())
    }

    fn wake_activation_key_for_island_command(
        &self,
        effect: &IslandCommandEffect,
    ) -> Option<WakeActivationKey> {
        let IslandCommand::WakeFiber { waiter } = &effect.command else {
            return None;
        };
        WakeCommand::queue_waiter(waiter.clone()).activation_key()
    }

    fn preflight_unique_endpoint_response_activations(
        &self,
        transition: &RuntimeTransition,
    ) -> Result<(), VmError> {
        let mut seen = Vec::new();
        for wake in &transition.wakes {
            let Some(key) = self.endpoint_response_activation_key_for_wake(wake) else {
                continue;
            };
            if seen.contains(&key) {
                return Err(VmError::Jit(
                    "runtime transition contains duplicate endpoint response activation"
                        .to_string(),
                ));
            }
            seen.push(key);
        }
        for command in &transition.island_commands {
            let Some(key) = self.endpoint_response_activation_key_for_island_command(command)
            else {
                continue;
            };
            if seen.contains(&key) {
                return Err(VmError::Jit(
                    "runtime transition contains duplicate endpoint response activation"
                        .to_string(),
                ));
            }
            seen.push(key);
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
    ) -> Option<EndpointResponseActivationKey> {
        let RuntimePayload::QueueWake(
            QueueRuntimeWake::ClosedReceiver {
                waiter,
                endpoint_id,
            }
            | QueueRuntimeWake::ClosedSender {
                waiter,
                endpoint_id,
            },
        ) = &wake.payload
        else {
            return None;
        };
        if waiter.endpoint_wait_id() == 0 {
            return None;
        }
        Some(EndpointResponseActivationKey {
            endpoint_id: (*endpoint_id)?,
            fiber_key: waiter.fiber_key(),
            wait_id: waiter.endpoint_wait_id(),
        })
    }

    fn endpoint_response_activation_key_for_island_command(
        &self,
        effect: &IslandCommandEffect,
    ) -> Option<EndpointResponseActivationKey> {
        let IslandCommand::EndpointResponse {
            endpoint_id,
            kind,
            from_island,
            fiber_key,
            wait_id,
        } = &effect.command
        else {
            return None;
        };
        if matches!(kind, EndpointResponseKind::Closed)
            || *from_island != self.state.current_island_id
        {
            return None;
        }
        if effect.island_id != self.state.current_island_id
            && !crate::vm::endpoint_response_from_authorized_source(
                self,
                *endpoint_id,
                *from_island,
            )
        {
            return None;
        }
        Some(EndpointResponseActivationKey {
            endpoint_id: *endpoint_id,
            fiber_key: *fiber_key,
            wait_id: *wait_id,
        })
    }

    fn local_endpoint_response_consumption_key_for_wake(
        &self,
        wake: &WakeCommand,
    ) -> Option<EndpointResponseActivationKey> {
        let key = self.endpoint_response_activation_key_for_wake(wake)?;
        match &wake.payload {
            RuntimePayload::QueueWake(
                QueueRuntimeWake::ClosedReceiver { waiter, .. }
                | QueueRuntimeWake::ClosedSender { waiter, .. },
            ) if waiter.island_id == self.state.current_island_id => Some(key),
            _ => None,
        }
    }

    fn local_endpoint_response_consumption_key_for_island_command(
        &self,
        effect: &IslandCommandEffect,
    ) -> Option<EndpointResponseActivationKey> {
        let key = self.endpoint_response_activation_key_for_island_command(effect)?;
        let IslandCommand::EndpointResponse {
            endpoint_id,
            kind,
            from_island,
            ..
        } = &effect.command
        else {
            return None;
        };
        if effect.island_id != self.state.current_island_id
            || *from_island != self.state.current_island_id
            || matches!(kind, EndpointResponseKind::Closed)
            || !crate::vm::endpoint_response_from_authorized_source(
                self,
                *endpoint_id,
                *from_island,
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
        for wake in &transition.wakes {
            let Some(source) = self.endpoint_response_authorization_source_for_wake(wake) else {
                continue;
            };
            self.preflight_endpoint_response_authorization_stable(source, transition)?;
        }
        for effect in &transition.island_commands {
            let Some(source) =
                self.endpoint_response_authorization_source_for_island_command(effect)
            else {
                continue;
            };
            self.preflight_endpoint_response_authorization_stable(source, transition)?;
        }
        Ok(())
    }

    fn endpoint_response_authorization_source_for_wake(
        &self,
        wake: &WakeCommand,
    ) -> Option<EndpointResponseAuthorizationSource> {
        let key = self.endpoint_response_activation_key_for_wake(wake)?;
        match &wake.payload {
            RuntimePayload::QueueWake(
                QueueRuntimeWake::ClosedReceiver { waiter, .. }
                | QueueRuntimeWake::ClosedSender { waiter, .. },
            ) if waiter.island_id != self.state.current_island_id => {
                Some(EndpointResponseAuthorizationSource {
                    endpoint_id: key.endpoint_id,
                    from_island: self.state.current_island_id,
                    target_island: waiter.island_id,
                })
            }
            _ => None,
        }
    }

    fn endpoint_response_authorization_source_for_island_command(
        &self,
        effect: &IslandCommandEffect,
    ) -> Option<EndpointResponseAuthorizationSource> {
        let IslandCommand::EndpointResponse {
            endpoint_id,
            from_island,
            ..
        } = &effect.command
        else {
            return None;
        };
        Some(EndpointResponseAuthorizationSource {
            endpoint_id: *endpoint_id,
            from_island: *from_island,
            target_island: effect.island_id,
        })
    }

    fn preflight_endpoint_response_authorization_stable(
        &self,
        source: EndpointResponseAuthorizationSource,
        transition: &RuntimeTransition,
    ) -> Result<(), VmError> {
        let before = crate::vm::endpoint_response_from_authorized_source(
            self,
            source.endpoint_id,
            source.from_island,
        );
        let after = self.endpoint_response_authorized_after_pre_island_effects(source, transition);
        if before != after {
            return Err(VmError::Jit(
                "runtime transition endpoint response authorization drift".to_string(),
            ));
        }
        Ok(())
    }

    fn endpoint_response_authorized_after_pre_island_effects(
        &self,
        source: EndpointResponseAuthorizationSource,
        transition: &RuntimeTransition,
    ) -> bool {
        let mut tombstone_response_source = self
            .state
            .endpoint_registry
            .tombstone_response_source(source.endpoint_id);
        for wake in &transition.wakes {
            let RuntimePayload::QueueWake(
                QueueRuntimeWake::ClosedReceiver {
                    waiter,
                    endpoint_id: wake_endpoint_id,
                }
                | QueueRuntimeWake::ClosedSender {
                    waiter,
                    endpoint_id: wake_endpoint_id,
                },
            ) = &wake.payload
            else {
                continue;
            };
            let Some(wake_endpoint_id) = wake_endpoint_id else {
                continue;
            };
            if *wake_endpoint_id != source.endpoint_id
                || waiter.island_id != self.state.current_island_id
                || waiter.endpoint_wait_id() == 0
            {
                continue;
            }
            tombstone_response_source = Some(Some(self.state.current_island_id));
        }
        for tombstone in &transition.endpoint_tombstones {
            if tombstone.endpoint_id != source.endpoint_id {
                continue;
            }
            if self.endpoint_response_tombstone_is_close_handoff(tombstone, source, transition) {
                continue;
            }
            let preserved_source = tombstone_response_source.flatten();
            tombstone_response_source = Some(tombstone.response_source.or(preserved_source));
        }
        match tombstone_response_source {
            Some(Some(owner)) => owner == source.from_island,
            Some(None) => source.from_island == self.state.current_island_id,
            None => crate::vm::endpoint_response_from_authorized_source(
                self,
                source.endpoint_id,
                source.from_island,
            ),
        }
    }

    fn endpoint_response_tombstone_is_close_handoff(
        &self,
        tombstone: &EndpointTombstone,
        source: EndpointResponseAuthorizationSource,
        transition: &RuntimeTransition,
    ) -> bool {
        if source.from_island != self.state.current_island_id
            || tombstone.response_source != Some(source.target_island)
        {
            return false;
        }
        transition.island_commands.iter().any(|effect| {
            if effect.island_id != source.target_island {
                return false;
            }
            matches!(
                &effect.command,
                IslandCommand::EndpointRequest {
                    endpoint_id,
                    kind: EndpointRequestKind::Close,
                    from_island,
                    ..
                } if *endpoint_id == source.endpoint_id
                    && *from_island == self.state.current_island_id
            )
        })
    }

    fn preflight_unique_endpoint_request_activations(
        &self,
        transition: &RuntimeTransition,
    ) -> Result<(), VmError> {
        let mut seen = Vec::new();
        for command in &transition.island_commands {
            let Some(key) = self.endpoint_request_activation_key_for_island_command(command)?
            else {
                continue;
            };
            if seen.contains(&key) {
                return Err(VmError::Jit(
                    "runtime transition contains duplicate endpoint request activation".to_string(),
                ));
            }
            seen.push(key);
        }
        Ok(())
    }

    fn endpoint_request_activation_key_for_island_command(
        &self,
        effect: &IslandCommandEffect,
    ) -> Result<Option<EndpointRequestActivationKey>, VmError> {
        let IslandCommand::EndpointRequest {
            endpoint_id,
            kind,
            fiber_key,
            wait_id,
            ..
        } = &effect.command
        else {
            return Ok(None);
        };
        if !endpoint_request_expects_pending_response(kind) {
            return Ok(None);
        }
        if *fiber_key == 0 || *wait_id == 0 {
            return Err(VmError::Jit(
                "endpoint request activation missing response wait identity".to_string(),
            ));
        }
        validate_canonical_fiber_key(*fiber_key, "endpoint request activation")
            .map_err(VmError::Jit)?;
        Ok(Some(EndpointRequestActivationKey {
            endpoint_id: *endpoint_id,
            fiber_key: *fiber_key,
            wait_id: *wait_id,
        }))
    }

    fn preflight_runtime_wake(&self, wake: &WakeCommand) -> Result<(), VmError> {
        if let RuntimePayload::QueueWake(ref queue_wake) = wake.payload {
            wake.validate_queue_identity(queue_wake)
                .map_err(VmError::Jit)?;
            self.validate_queue_wake_payload(queue_wake)
                .map_err(VmError::Jit)?;
        }
        match &wake.payload {
            RuntimePayload::QueueWake(QueueRuntimeWake::Waiter {
                waiter,
                select_result,
            }) => self.preflight_queue_waiter_wake(waiter, select_result.as_ref()),
            RuntimePayload::QueueWake(QueueRuntimeWake::ClosedReceiver {
                waiter,
                endpoint_id,
            }) => self.preflight_closed_receiver_wake(waiter, *endpoint_id),
            RuntimePayload::QueueWake(QueueRuntimeWake::ClosedSender {
                waiter,
                endpoint_id,
            }) => self.preflight_closed_sender_wake(waiter, *endpoint_id),
            _ => Ok(()),
        }
    }

    fn preflight_queue_waiter_wake(
        &self,
        waiter: &QueueWaiter,
        select_result: Option<&SelectWokenResult>,
    ) -> Result<(), VmError> {
        if waiter.endpoint_wait_id() != 0 {
            return Err(VmError::Jit(
                "runtime queue waiter wake was rejected".to_string(),
            ));
        }
        if waiter.island_id == self.state.current_island_id {
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

    fn preflight_remote_wake_fiber_command(&self, waiter: &QueueWaiter) -> Result<(), VmError> {
        let _ = waiter;
        Err(VmError::Jit(
            "remote WakeFiber command must use an endpoint response".to_string(),
        ))
    }

    fn preflight_remote_select_wake_shape(&self, waiter: &QueueWaiter) -> Result<(), VmError> {
        if waiter.select.is_some() {
            return Err(VmError::Jit(
                "remote select wake cannot be represented without select payload".to_string(),
            ));
        }
        validate_canonical_fiber_key(waiter.fiber_key(), "remote queue waiter wake")
            .map_err(VmError::Jit)?;
        self.preflight_island_route(waiter.island_id)
    }

    fn preflight_local_closed_queue_waiter(
        &self,
        waiter: &QueueWaiter,
        context: &str,
    ) -> Result<(), VmError> {
        let ch =
            crate::exec::validate_queue_handle(&self.state.gc, waiter.queue_ref as GcRef, context)
                .map_err(VmError::Jit)?;
        if queue::is_closed(ch) {
            return Ok(());
        }
        Err(VmError::Jit(format!("{context} referenced open queue")))
    }

    fn preflight_closed_receiver_wake(
        &self,
        waiter: &QueueWaiter,
        endpoint_id: Option<u64>,
    ) -> Result<(), VmError> {
        if waiter.endpoint_wait_id() != 0 {
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
                },
            );
        }
        if waiter.island_id == self.state.current_island_id {
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
        if waiter.endpoint_wait_id() != 0 {
            let Some(endpoint_id) = endpoint_id else {
                return Err(VmError::Jit(
                    "closed endpoint sender wake missing endpoint id".to_string(),
                ));
            };
            return self.preflight_endpoint_response_for_waiter(
                waiter,
                endpoint_id,
                &EndpointResponseKind::SendAck { closed: true },
            );
        }
        if waiter.island_id == self.state.current_island_id {
            self.preflight_local_closed_queue_waiter(waiter, "closed sender wake")?;
            if self.scheduler.can_wake_queue_sender_closed(waiter) {
                if waiter.select.is_none() {
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
        if waiter.island_id != self.state.current_island_id {
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
            return self.preflight_island_route(waiter.island_id);
        }
        self.preflight_same_island_endpoint_wake_response_source(endpoint_id, kind)?;
        if self.can_accept_endpoint_response(
            waiter.fiber_key(),
            endpoint_id,
            waiter.endpoint_wait_id(),
            kind,
        ) {
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
                queue::is_remote(ch) && queue::remote_proxy(ch).home_island != from_island
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

    fn can_accept_endpoint_response(
        &self,
        fiber_key: u64,
        endpoint_id: u64,
        wait_id: u64,
        kind: &EndpointResponseKind,
    ) -> bool {
        if self.state.pending_island_responses == 0 {
            return false;
        }
        let Some(fiber) = self
            .scheduler
            .try_get_fiber_by_endpoint_response_key(fiber_key)
        else {
            return false;
        };
        if !matches!(
            fiber.state,
            crate::fiber::FiberState::Blocked(BlockReason::Queue)
        ) {
            return false;
        }
        if matches!(kind, EndpointResponseKind::SendAck { closed: true })
            && replay_current_instruction_policy(fiber, "endpoint closed send response").is_err()
        {
            return false;
        }
        fiber.can_apply_endpoint_response(endpoint_id, wait_id, kind)
    }

    fn preflight_same_island_endpoint_response_command(
        &self,
        endpoint_id: u64,
        kind: &EndpointResponseKind,
        from_island: u32,
        fiber_key: u64,
        wait_id: u64,
    ) -> Result<(), VmError> {
        validate_same_island_endpoint_response_source(self.state.current_island_id, from_island)?;
        if !crate::vm::endpoint_response_from_authorized_source(self, endpoint_id, from_island) {
            return Ok(());
        }
        if matches!(kind, EndpointResponseKind::Closed) {
            return Ok(());
        }
        if self.can_accept_endpoint_response(fiber_key, endpoint_id, wait_id, kind) {
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
                IslandCommand::EndpointRequest {
                    endpoint_id,
                    kind,
                    from_island,
                    fiber_key,
                    wait_id,
                } => {
                    validate_same_island_endpoint_request_source(
                        self.state.current_island_id,
                        *from_island,
                    )?;
                    validate_endpoint_request_pending_response(kind, effect.pending_response)?;
                    self.preflight_endpoint_request_command(
                        *endpoint_id,
                        kind,
                        *from_island,
                        *fiber_key,
                        *wait_id,
                    )?;
                }
                IslandCommand::EndpointResponse {
                    endpoint_id,
                    kind,
                    from_island,
                    fiber_key,
                    wait_id,
                } => {
                    validate_non_request_pending_response(
                        "EndpointResponse",
                        effect.pending_response,
                    )?;
                    self.preflight_same_island_endpoint_response_command(
                        *endpoint_id,
                        kind,
                        *from_island,
                        *fiber_key,
                        *wait_id,
                    )?;
                }
                IslandCommand::WakeFiber { waiter } => {
                    validate_non_request_pending_response("WakeFiber", effect.pending_response)?;
                    self.preflight_same_island_wake_fiber_command(waiter)?;
                }
                IslandCommand::Shutdown => {
                    validate_non_request_pending_response("Shutdown", effect.pending_response)?;
                }
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
            IslandCommand::SpawnFiber { .. } | IslandCommand::Shutdown => {
                if effect.pending_response {
                    return Err(VmError::Jit(
                        "remote island command pending-response contract was rejected".to_string(),
                    ));
                }
            }
            IslandCommand::WakeFiber { waiter } => {
                if waiter.island_id != effect.island_id || effect.pending_response {
                    return Err(VmError::Jit(
                        "remote WakeFiber command was rejected".to_string(),
                    ));
                }
                self.preflight_remote_wake_fiber_command(waiter)?;
            }
            IslandCommand::EndpointRequest {
                kind, from_island, ..
            } => {
                if *from_island != self.state.current_island_id {
                    return Err(VmError::Jit(
                        "remote EndpointRequest command was rejected".to_string(),
                    ));
                }
                validate_endpoint_request_pending_response(kind, effect.pending_response)?;
            }
            IslandCommand::EndpointResponse {
                endpoint_id,
                kind,
                from_island,
                fiber_key,
                wait_id,
            } => {
                if *from_island != self.state.current_island_id
                    || effect.pending_response
                    || (!matches!(kind, EndpointResponseKind::Closed)
                        && (*fiber_key == 0
                            || *wait_id == 0
                            || validate_canonical_fiber_key(
                                *fiber_key,
                                "remote EndpointResponse command",
                            )
                            .is_err()))
                    || !crate::vm::endpoint_response_from_authorized_source(
                        self,
                        *endpoint_id,
                        *from_island,
                    )
                {
                    return Err(VmError::Jit(
                        "remote EndpointResponse command was rejected".to_string(),
                    ));
                }
            }
        }
        Ok(())
    }

    fn preflight_same_island_wake_fiber_command(
        &self,
        waiter: &QueueWaiter,
    ) -> Result<(), VmError> {
        if waiter.island_id != self.state.current_island_id
            || waiter.endpoint_wait_id() != 0
            || waiter.select.is_some()
            || validate_queue_waiter_identity(waiter).is_err()
        {
            return Err(VmError::Jit(
                "same-island WakeFiber command was rejected".to_string(),
            ));
        }
        let wake_key = FiberWakeKey::from_packed(waiter.fiber_key());
        let Some(fiber) = self.scheduler.try_get_fiber_by_wake_key(wake_key) else {
            return Err(VmError::Jit(
                "same-island WakeFiber command was rejected".to_string(),
            ));
        };
        if fiber.remote_endpoint_wait.is_some() {
            return Err(VmError::Jit(
                "same-island WakeFiber command was rejected".to_string(),
            ));
        }
        self.preflight_queue_waiter_wake(waiter, None)
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
        let RuntimePayload::QueueWake(queue_wake) = &wake.payload else {
            return Ok(None);
        };
        let waiter = queue_wake.waiter();
        if waiter.island_id == self.state.current_island_id {
            return Ok(None);
        }

        let effect = match queue_wake {
            QueueRuntimeWake::Waiter { waiter, .. } => {
                self.preflight_remote_select_wake_shape(waiter)?;
                return Err(VmError::Jit(
                    "remote queue waiter wake must use an endpoint response".to_string(),
                ));
            }
            QueueRuntimeWake::ClosedReceiver {
                waiter,
                endpoint_id,
            } => {
                let Some(endpoint_id) = *endpoint_id else {
                    return Err(VmError::Jit(
                        "closed endpoint receiver wake missing endpoint id".to_string(),
                    ));
                };
                IslandCommandEffect::endpoint_response(
                    waiter.island_id,
                    self.state.current_island_id,
                    endpoint_id,
                    EndpointResponseKind::RecvData {
                        data: Vec::new(),
                        closed: true,
                    },
                    waiter.fiber_key(),
                    waiter.endpoint_wait_id(),
                )
            }
            QueueRuntimeWake::ClosedSender {
                waiter,
                endpoint_id,
            } => {
                let Some(endpoint_id) = *endpoint_id else {
                    return Err(VmError::Jit(
                        "closed endpoint sender wake missing endpoint id".to_string(),
                    ));
                };
                IslandCommandEffect::endpoint_response(
                    waiter.island_id,
                    self.state.current_island_id,
                    endpoint_id,
                    EndpointResponseKind::SendAck { closed: true },
                    waiter.fiber_key(),
                    waiter.endpoint_wait_id(),
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
            let RuntimePayload::QueueWake(queue_wake) = &wake.payload else {
                continue;
            };
            if let Some(rollback) =
                self.select_waiter_rollback_for_pending_wake(queue_wake.waiter())
            {
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
        let select = waiter.select.as_ref()?;
        if waiter.endpoint_wait_id() != 0 || waiter.island_id != self.state.current_island_id {
            return None;
        }
        let wake_key = FiberWakeKey::from_packed(waiter.fiber_key());
        let fiber = self.scheduler.try_get_fiber_by_wake_key(wake_key)?;
        let select_state = fiber.select_state.as_ref()?;
        if select_state.select_id != select.select_id {
            return None;
        }
        let queues = select_state
            .registered_queues
            .iter()
            .filter(|registered| !registered.queue.is_null())
            .map(|registered| {
                (
                    registered.queue,
                    queue::local_state(registered.queue).clone(),
                )
            })
            .collect();
        Some(RuntimeRollback::select_waiters(
            waiter.fiber_key(),
            fiber.select_state.clone(),
            queues,
        ))
    }

    #[cfg(feature = "jit")]
    fn cancel_select_sibling_waiters_for_transition(&mut self, transition: &RuntimeTransition) {
        for wake in &transition.wakes {
            let RuntimePayload::QueueWake(queue_wake) = &wake.payload else {
                continue;
            };
            self.cancel_select_sibling_waiters_for_pending_wake(queue_wake.waiter());
        }
    }

    #[cfg(feature = "jit")]
    fn cancel_select_sibling_waiters_for_pending_wake(&mut self, waiter: &QueueWaiter) {
        let Some(select) = waiter.select.as_ref() else {
            return;
        };
        if waiter.endpoint_wait_id() != 0 || waiter.island_id != self.state.current_island_id {
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
        let mut selected = Vec::new();
        for registered in core::mem::take(&mut select_state.registered_queues) {
            let is_selected = registered.case_index == select.case_index
                && registered.queue as u64 == select.queue_ref
                && registered.kind.wait_kind() == select.kind;
            if is_selected {
                selected.push(registered);
            } else if !registered.queue.is_null() {
                queue::cancel_select_waiters(
                    registered.queue,
                    waiter.fiber_key(),
                    select.select_id,
                );
            }
        }
        select_state.registered_queues = selected;
    }

    fn cancel_select_sibling_waiters_for_wake(&mut self, waiter: &QueueWaiter) {
        let Some(select) = waiter.select.as_ref() else {
            return;
        };
        if waiter.endpoint_wait_id() != 0 || waiter.island_id != self.state.current_island_id {
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
        if let RuntimePayload::QueueWake(ref queue_wake) = wake.payload {
            wake.validate_queue_identity(queue_wake)
                .map_err(VmError::Jit)?;
            self.validate_queue_wake_payload(queue_wake)
                .map_err(VmError::Jit)?;
        }
        match wake.payload {
            RuntimePayload::QueueWake(QueueRuntimeWake::Waiter {
                waiter,
                select_result,
            }) => {
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
            RuntimePayload::QueueWake(QueueRuntimeWake::ClosedReceiver {
                waiter,
                endpoint_id,
            }) => {
                if waiter.endpoint_wait_id() != 0 {
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
            RuntimePayload::QueueWake(QueueRuntimeWake::ClosedSender {
                waiter,
                endpoint_id,
            }) => {
                if waiter.endpoint_wait_id() != 0 {
                    let Some(endpoint_id) = endpoint_id else {
                        return Err(VmError::Jit(
                            "closed endpoint sender wake missing endpoint id".to_string(),
                        ));
                    };
                    self.apply_endpoint_response_for_waiter(
                        &waiter,
                        endpoint_id,
                        EndpointResponseKind::SendAck { closed: true },
                    )?;
                    return Ok(());
                }
                let local_simple_sender =
                    waiter.island_id == self.state.current_island_id && waiter.select.is_none();
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
            RuntimePayload::None
            | RuntimePayload::HostEventData(_)
            | RuntimePayload::EndpointResponse(_) => Ok(()),
        }
    }

    fn apply_endpoint_response_for_waiter(
        &mut self,
        waiter: &QueueWaiter,
        endpoint_id: u64,
        kind: EndpointResponseKind,
    ) -> Result<(), VmError> {
        if waiter.island_id == self.state.current_island_id {
            let from_island = self.state.current_island_id;
            let endpoint_registry_snapshot = if endpoint_response_kind_is_closed(&kind)
                && !crate::vm::endpoint_response_from_authorized_source(
                    self,
                    endpoint_id,
                    from_island,
                ) {
                if !self.can_accept_endpoint_response(
                    waiter.fiber_key(),
                    endpoint_id,
                    waiter.endpoint_wait_id(),
                    &kind,
                ) {
                    return Err(VmError::Jit(
                        "same-island endpoint wake response was rejected".to_string(),
                    ));
                }
                let foreign_live_remote = self
                    .state
                    .endpoint_registry
                    .get_live(endpoint_id)
                    .is_some_and(|ch| {
                        queue::is_remote(ch) && queue::remote_proxy(ch).home_island != from_island
                    });
                let foreign_tombstone = matches!(
                    self.state
                        .endpoint_registry
                        .tombstone_response_source(endpoint_id),
                    Some(Some(source)) if source != from_island
                );
                if !foreign_live_remote && !foreign_tombstone {
                    let snapshot = self.state.endpoint_registry.snapshot();
                    self.state
                        .endpoint_registry
                        .mark_tombstone_with_response_source(endpoint_id, Some(from_island));
                    Some(snapshot)
                } else {
                    None
                }
            } else {
                None
            };
            let outcome = self.apply_runtime_command(RuntimeCommand::endpoint_response(
                endpoint_id,
                from_island,
                waiter.fiber_key(),
                waiter.endpoint_wait_id(),
                kind,
            ));
            if !outcome.applied || !outcome.payload_accepted {
                if let Some(snapshot) = endpoint_registry_snapshot {
                    self.state.endpoint_registry.restore(snapshot);
                }
                return Err(VmError::Jit(
                    "same-island endpoint wake response was rejected".to_string(),
                ));
            }
            return Ok(());
        }
        self.apply_island_command_effect(IslandCommandEffect::endpoint_response(
            waiter.island_id,
            self.state.current_island_id,
            endpoint_id,
            kind,
            waiter.fiber_key(),
            waiter.endpoint_wait_id(),
        ))
    }

    fn validate_queue_wake_payload(&self, wake: &QueueRuntimeWake) -> Result<(), String> {
        let QueueRuntimeWake::Waiter {
            waiter,
            select_result,
        } = wake
        else {
            return Ok(());
        };
        match (waiter.select.as_ref(), select_result) {
            (None, None) => Ok(()),
            (None, Some(_)) => Err("select wake payload attached to non-select waiter".to_string()),
            (Some(select), None) if select.kind == SelectWaitKind::Recv => {
                Err("select recv wake missing payload".to_string())
            }
            (Some(select), None) if select.kind == SelectWaitKind::Send => {
                Err("select send wake missing payload".to_string())
            }
            (Some(select), Some(SelectWokenResult::SendAccepted))
                if select.kind == SelectWaitKind::Send
                    && waiter.kind == Some(SelectWaitKind::Send) =>
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
            ) if select.kind == SelectWaitKind::Recv
                && waiter.kind == Some(SelectWaitKind::Recv) =>
            {
                let ch = crate::exec::validate_queue_handle(
                    &self.state.gc,
                    select.queue_ref as vo_runtime::gc::GcRef,
                    "select wake recv payload",
                )?;
                let expected_slot_types =
                    crate::exec::queue::select_woken_recv_slot_types(ch, self.module.as_ref())?;
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
        if effect.island_id == self.state.current_island_id {
            if effect.pending_response {
                self.state.pending_island_responses += 1;
            }
            let result = self.dispatch_island_command(effect.command);
            if result.is_err() && effect.pending_response {
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
        if effect.pending_response {
            self.state.pending_island_responses += 1;
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
                pending_response: effect.pending_response,
                #[cfg(feature = "std")]
                reservation,
            });
        }
        Ok((local_commands, remote_commands))
    }

    fn commit_remote_island_commands(&mut self, remote_commands: Vec<RemoteIslandCommandCommit>) {
        for effect in remote_commands {
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
            if effect.pending_response {
                self.state.pending_island_responses += 1;
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

    fn apply_pending_spawns(&mut self, spawns: Vec<Fiber>) -> Result<(), VmError> {
        for fiber in spawns {
            self.scheduler.spawn(fiber);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests;
