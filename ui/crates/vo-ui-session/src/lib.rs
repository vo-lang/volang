#![no_std]

extern crate alloc;

mod component;

pub use component::{ComponentLifecycle, ComponentSession, ComponentSessionError, ComponentTurn};

use alloc::collections::BTreeMap;
use alloc::vec::Vec;
use core::fmt;
use vo_ui_core::{NodeId, TaskId};
use vo_ui_plan::{SlotId, SlotValue, ValidatedPlan};
use vo_ui_protocol::{MutationBatch, Renderer};
use vo_ui_reactive::ScopeId;
use vo_ui_runtime::{TemplateError, TemplateRuntime};
use vo_ui_scheduler::{
    QueueFull, SchedulerConfig, SchedulerError, ScopedMessage, TaskCompletion, UiScheduler,
};

/// Coalesced slot writes produced while handling one UI turn. The last write
/// to a slot wins, matching sequential Volang assignment semantics.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct SlotWrites {
    writes: BTreeMap<SlotId, SlotValue>,
}

impl SlotWrites {
    pub const fn new() -> Self {
        Self {
            writes: BTreeMap::new(),
        }
    }

    pub fn set(&mut self, slot: SlotId, value: SlotValue) -> Option<SlotValue> {
        self.writes.insert(slot, value)
    }

    pub fn get(&self, slot: SlotId) -> Option<&SlotValue> {
        self.writes.get(&slot)
    }

    pub fn len(&self) -> usize {
        self.writes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.writes.is_empty()
    }
}

impl IntoIterator for SlotWrites {
    type Item = (SlotId, SlotValue);
    type IntoIter = alloc::collections::btree_map::IntoIter<SlotId, SlotValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.writes.into_iter()
    }
}

/// Reduces a completed goroutine message into component slot writes. The
/// callback runs only on the owning UI Island.
pub trait MessageHandler<M, O = ScopeId> {
    fn handle(&mut self, message: ScopedMessage<M, O>, writes: &mut SlotWrites);
}

impl<M, O, F> MessageHandler<M, O> for F
where
    F: FnMut(ScopedMessage<M, O>, &mut SlotWrites),
{
    fn handle(&mut self, message: ScopedMessage<M, O>, writes: &mut SlotWrites) {
        self(message, writes);
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CommitReport {
    pub revision: u64,
    pub mutation_count: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SessionTurn {
    pub inspected_completions: usize,
    pub stale_completions: usize,
    pub delivered_messages: usize,
    pub coalesced_slot_writes: usize,
    pub has_more: bool,
    pub commit: Option<CommitReport>,
}

#[derive(Debug)]
pub enum SessionError<E> {
    Scheduler(SchedulerError),
    Template(TemplateError<E>),
}

impl<E: fmt::Display> fmt::Display for SessionError<E> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Scheduler(error) => write!(formatter, "UI scheduler failed: {error}"),
            Self::Template(error) => write!(formatter, "UI template runtime failed: {error}"),
        }
    }
}

impl<E> From<SchedulerError> for SessionError<E> {
    fn from(error: SchedulerError) -> Self {
        Self::Scheduler(error)
    }
}

impl<E> From<TemplateError<E>> for SessionError<E> {
    fn from(error: TemplateError<E>) -> Self {
        Self::Template(error)
    }
}

/// Owns one compiled component, its renderer, and its goroutine completion
/// mailbox. A turn has one mutable owner and produces at most one renderer
/// revision, which keeps Web and desktop backends deterministic.
pub struct UiSession<R: Renderer, M, H, O = ScopeId> {
    runtime: TemplateRuntime<R>,
    scheduler: UiScheduler<M, O>,
    handler: H,
}

impl<R, M, H, O> UiSession<R, M, H, O>
where
    R: Renderer,
    H: MessageHandler<M, O>,
    O: Copy + Ord,
{
    pub fn new(
        renderer: R,
        session_epoch: u64,
        root: NodeId,
        config: SchedulerConfig,
        handler: H,
    ) -> Result<Self, SessionError<R::Error>> {
        Ok(Self {
            runtime: TemplateRuntime::new(renderer, session_epoch, root),
            scheduler: UiScheduler::new(config)?,
            handler,
        })
    }

    pub fn mount(
        &mut self,
        plan: ValidatedPlan,
        slots: Vec<SlotValue>,
    ) -> Result<MutationBatch, SessionError<R::Error>> {
        self.runtime.mount(plan, slots).map_err(SessionError::from)
    }

    pub fn unmount(&mut self) -> Result<MutationBatch, SessionError<R::Error>> {
        self.runtime.unmount().map_err(SessionError::from)
    }

    pub fn replace(
        &mut self,
        plan: ValidatedPlan,
        slots: Vec<SlotValue>,
    ) -> Result<MutationBatch, SessionError<R::Error>> {
        self.runtime
            .replace(plan, slots)
            .map_err(SessionError::from)
    }

    pub fn spawn_task(&mut self, owner: O) -> Result<TaskId, SessionError<R::Error>> {
        self.scheduler.spawn_task(owner).map_err(SessionError::from)
    }

    pub fn enqueue_completion(
        &mut self,
        completion: TaskCompletion<M>,
    ) -> Result<(), QueueFull<M>> {
        self.scheduler.enqueue_completion(completion)
    }

    pub fn cancel_task(&mut self, task: TaskId) -> Option<TaskId> {
        self.scheduler.cancel_task(task)
    }

    pub fn cancel_scope(&mut self, owner: O) -> Vec<TaskId> {
        self.scheduler.cancel_scope(owner)
    }

    pub fn runtime(&self) -> &TemplateRuntime<R> {
        &self.runtime
    }

    pub fn runtime_mut(&mut self) -> &mut TemplateRuntime<R> {
        &mut self.runtime
    }

    pub fn scheduler(&self) -> &UiScheduler<M, O> {
        &self.scheduler
    }

    pub fn handler(&self) -> &H {
        &self.handler
    }

    pub fn handler_mut(&mut self) -> &mut H {
        &mut self.handler
    }

    /// Processes one bounded mailbox turn and applies all resulting slot
    /// changes as at most one atomic renderer batch.
    pub fn drain_turn(&mut self) -> Result<SessionTurn, SessionError<R::Error>> {
        let turn = self.scheduler.drain_turn();
        let delivered_messages = turn.messages.len();
        let mut writes = SlotWrites::new();
        for message in turn.messages {
            self.handler.handle(message, &mut writes);
        }
        let coalesced_slot_writes = writes.len();
        let commit = if writes.is_empty() {
            None
        } else {
            self.runtime
                .update_slots_in_place(writes)?
                .map(|batch| CommitReport {
                    revision: batch.revision,
                    mutation_count: batch.mutations.len(),
                })
        };
        Ok(SessionTurn {
            inspected_completions: turn.inspected_completions,
            stale_completions: turn.stale_completions,
            delivered_messages,
            coalesced_slot_writes,
            has_more: turn.has_more,
            commit,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::{String, ToString};
    use vo_ui_headless::HeadlessRenderer;
    use vo_ui_plan::{ComponentPlan, LocalNodeId, PlanLimits, SlotKind, TemplateNode, UpdateSite};
    use vo_ui_protocol::ProtocolLimits;
    use vo_ui_reactive::{Runtime as ReactiveRuntime, RuntimeConfig};

    fn text_plan() -> ValidatedPlan {
        let mut plan = ComponentPlan::new(LocalNodeId::new(0));
        plan.slots.push(SlotKind::Text);
        plan.nodes.push(TemplateNode::text(LocalNodeId::new(0), ""));
        plan.updates
            .push(UpdateSite::text(SlotId::new(0), LocalNodeId::new(0)));
        plan.validate(PlanLimits::default()).unwrap()
    }

    fn scope() -> ScopeId {
        let mut reactive = ReactiveRuntime::new(RuntimeConfig::default());
        reactive.create_scope(reactive.root_scope()).unwrap()
    }

    fn session(
        handler: impl MessageHandler<String>,
    ) -> UiSession<HeadlessRenderer, String, impl MessageHandler<String>> {
        let root = NodeId::new(0, 1);
        UiSession::new(
            HeadlessRenderer::new(9, root, ProtocolLimits::default()),
            9,
            root,
            SchedulerConfig::default(),
            handler,
        )
        .unwrap()
    }

    #[test]
    fn goroutine_results_coalesce_into_one_renderer_commit() {
        let mut session = session(|message: ScopedMessage<String>, writes: &mut SlotWrites| {
            writes.set(SlotId::new(0), SlotValue::Text(message.message));
        });
        session
            .mount(
                text_plan(),
                alloc::vec![SlotValue::Text("start".to_string())],
            )
            .unwrap();
        let owner = scope();
        let first = session.spawn_task(owner).unwrap();
        let second = session.spawn_task(owner).unwrap();
        session
            .enqueue_completion(TaskCompletion::new(first, "first".to_string()))
            .unwrap();
        session
            .enqueue_completion(TaskCompletion::new(second, "second".to_string()))
            .unwrap();

        let turn = session.drain_turn().unwrap();
        assert_eq!(turn.delivered_messages, 2);
        assert_eq!(turn.coalesced_slot_writes, 1);
        assert_eq!(
            turn.commit,
            Some(CommitReport {
                revision: 2,
                mutation_count: 1,
            })
        );
        let root = session.runtime().renderer().root();
        let text = session.runtime().renderer().node(root).unwrap().children[0];
        assert_eq!(
            session.runtime().renderer().node(text).unwrap().text,
            "second"
        );
    }

    #[test]
    fn late_completion_after_scope_cancel_has_no_render_effect() {
        let mut session = session(|message: ScopedMessage<String>, writes: &mut SlotWrites| {
            writes.set(SlotId::new(0), SlotValue::Text(message.message));
        });
        session
            .mount(
                text_plan(),
                alloc::vec![SlotValue::Text("stable".to_string())],
            )
            .unwrap();
        let owner = scope();
        let task = session.spawn_task(owner).unwrap();
        assert_eq!(session.cancel_scope(owner), alloc::vec![task]);
        session
            .enqueue_completion(TaskCompletion::new(task, "late".to_string()))
            .unwrap();

        let turn = session.drain_turn().unwrap();
        assert_eq!(turn.stale_completions, 1);
        assert_eq!(turn.delivered_messages, 0);
        assert_eq!(turn.commit, None);
        assert_eq!(session.runtime().revision(), 1);
    }
}
