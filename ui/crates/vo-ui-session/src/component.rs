use alloc::collections::{BTreeMap, BTreeSet};
use alloc::vec::Vec;
use core::fmt;
use vo_ui_artifact::{ComponentTypeId, EffectId, TaskSiteId};
use vo_ui_core::TaskId;
use vo_ui_runtime::{
    ComponentForest, ComponentForestCommit, ComponentForestError, ComponentForestLimits,
    ComponentInstanceId, ComponentSpec, ComponentStateCell, ComponentValue,
};
use vo_ui_scheduler::{
    QueueFull, SchedulerConfig, SchedulerError, ScopedMessage, TaskCompletion, UiScheduler,
};

/// Post-commit lifecycle sink. Hosts use this boundary to invoke compiled
/// lifecycle/effect functions. Calls are deterministic and cannot veto an
/// already accepted renderer/component revision.
pub trait ComponentLifecycle {
    fn mounted(&mut self, _instance: ComponentInstanceId) {}
    fn updated(&mut self, _instance: ComponentInstanceId) {}
    fn effect_started(&mut self, _instance: ComponentInstanceId, _effect: EffectId) {}
    fn task_cancelled(&mut self, _instance: ComponentInstanceId, _site: TaskSiteId, _task: TaskId) {
    }
    fn effect_cleaned(&mut self, _instance: ComponentInstanceId, _effect: EffectId) {}
    fn disposed(&mut self, _instance: ComponentInstanceId) {}
}

impl ComponentLifecycle for () {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ComponentSessionError {
    Forest(ComponentForestError),
    Scheduler(SchedulerError),
    StaleOwner(ComponentInstanceId),
    UndeclaredTaskSite {
        owner: ComponentInstanceId,
        site: TaskSiteId,
    },
}

impl fmt::Display for ComponentSessionError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "component session rejected operation: {self:?}")
    }
}

impl From<ComponentForestError> for ComponentSessionError {
    fn from(error: ComponentForestError) -> Self {
        Self::Forest(error)
    }
}

impl From<SchedulerError> for ComponentSessionError {
    fn from(error: SchedulerError) -> Self {
        Self::Scheduler(error)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ComponentTurn<M> {
    pub messages: Vec<ScopedMessage<M, ComponentInstanceId>>,
    pub inspected_completions: usize,
    pub stale_completions: usize,
    pub has_more: bool,
}

/// Joins the persistent component forest to the bounded goroutine mailbox.
/// Forest commits publish first; lifecycle/effects run post-commit; disposal
/// invalidates and cancels every task before effect/component cleanup.
pub struct ComponentSession<M, L = ()> {
    forest: ComponentForest,
    scheduler: UiScheduler<M, ComponentInstanceId>,
    lifecycle: L,
    task_sites: BTreeMap<TaskId, (ComponentInstanceId, TaskSiteId)>,
    effects: BTreeMap<ComponentInstanceId, Vec<EffectId>>,
}

impl<M, L: ComponentLifecycle> ComponentSession<M, L> {
    pub fn new(
        forest_limits: ComponentForestLimits,
        scheduler_config: SchedulerConfig,
        lifecycle: L,
    ) -> Result<Self, ComponentSessionError> {
        Ok(Self {
            forest: ComponentForest::new(forest_limits)?,
            scheduler: UiScheduler::new(scheduler_config)?,
            lifecycle,
            task_sites: BTreeMap::new(),
            effects: BTreeMap::new(),
        })
    }

    pub fn forest(&self) -> &ComponentForest {
        &self.forest
    }

    pub fn lifecycle(&self) -> &L {
        &self.lifecycle
    }

    pub fn lifecycle_mut(&mut self) -> &mut L {
        &mut self.lifecycle
    }

    pub fn scheduler(&self) -> &UiScheduler<M, ComponentInstanceId> {
        &self.scheduler
    }

    pub fn mount(
        &mut self,
        root_type: ComponentTypeId,
        children: Vec<ComponentSpec>,
    ) -> Result<ComponentForestCommit, ComponentSessionError> {
        self.mount_with_contract(
            root_type,
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            children,
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub fn mount_with_contract(
        &mut self,
        root_type: ComponentTypeId,
        root_props: Vec<ComponentValue>,
        root_state: Vec<ComponentStateCell>,
        root_handlers: Vec<vo_ui_artifact::HandlerSiteId>,
        root_effects: Vec<EffectId>,
        root_tasks: Vec<TaskSiteId>,
        root_slots: Vec<vo_ui_plan::SlotValue>,
        children: Vec<ComponentSpec>,
    ) -> Result<ComponentForestCommit, ComponentSessionError> {
        let commit = self.forest.mount_with_state(
            root_type,
            root_props,
            root_state,
            root_handlers,
            root_effects,
            root_tasks,
            root_slots,
            children,
        )?;
        self.publish_lifecycle(&commit);
        Ok(commit)
    }

    pub fn reconcile(
        &mut self,
        children: Vec<ComponentSpec>,
    ) -> Result<ComponentForestCommit, ComponentSessionError> {
        let commit = self.forest.reconcile(children)?;
        self.publish_lifecycle(&commit);
        Ok(commit)
    }

    pub fn unmount(&mut self) -> Result<ComponentForestCommit, ComponentSessionError> {
        let commit = self.forest.unmount()?;
        self.publish_lifecycle(&commit);
        Ok(commit)
    }

    pub fn spawn_task(
        &mut self,
        owner: ComponentInstanceId,
        site: TaskSiteId,
    ) -> Result<TaskId, ComponentSessionError> {
        let instance = self
            .forest
            .get(owner)
            .ok_or(ComponentSessionError::StaleOwner(owner))?;
        if instance.tasks.binary_search(&site).is_err() {
            return Err(ComponentSessionError::UndeclaredTaskSite { owner, site });
        }
        let task = self.scheduler.spawn_task(owner)?;
        self.task_sites.insert(task, (owner, site));
        Ok(task)
    }

    pub fn enqueue_completion(
        &mut self,
        completion: TaskCompletion<M>,
    ) -> Result<(), QueueFull<M>> {
        self.scheduler.enqueue_completion(completion)
    }

    pub fn cancel_task(&mut self, task: TaskId) -> Option<TaskId> {
        let cancelled = self.scheduler.cancel_task(task)?;
        self.task_sites.remove(&task);
        Some(cancelled)
    }

    pub fn drain_turn(&mut self) -> ComponentTurn<M> {
        let turn = self.scheduler.drain_turn();
        let mut messages = Vec::new();
        let mut stale = turn.stale_completions;
        for message in turn.messages {
            self.task_sites.remove(&message.task);
            if self.forest.contains(message.owner) {
                messages.push(message);
            } else {
                stale += 1;
            }
        }
        ComponentTurn {
            messages,
            inspected_completions: turn.inspected_completions,
            stale_completions: stale,
            has_more: turn.has_more,
        }
    }

    fn publish_lifecycle(&mut self, commit: &ComponentForestCommit) {
        for instance in &commit.created {
            let effects = self
                .forest
                .get(*instance)
                .expect("created component is live after accepted commit")
                .effects
                .clone();
            self.effects.insert(*instance, effects.clone());
            self.lifecycle.mounted(*instance);
            for effect in effects {
                self.lifecycle.effect_started(*instance, effect);
            }
        }

        let created = commit.created.iter().copied().collect::<BTreeSet<_>>();
        for instance in &commit.reused {
            if created.contains(instance) {
                continue;
            }
            self.lifecycle.updated(*instance);
            let next = self
                .forest
                .get(*instance)
                .expect("reused component is live after accepted commit")
                .effects
                .clone();
            let previous = self
                .effects
                .insert(*instance, next.clone())
                .unwrap_or_default();
            for effect in previous.iter().filter(|effect| !next.contains(effect)) {
                self.lifecycle.effect_cleaned(*instance, *effect);
            }
            for effect in next.iter().filter(|effect| !previous.contains(effect)) {
                self.lifecycle.effect_started(*instance, *effect);
            }
        }

        // Invalidate all asynchronous ownership before any cleanup callback.
        let mut cancelled = Vec::new();
        for instance in &commit.disposed {
            for task in self.scheduler.cancel_scope(*instance) {
                if let Some((owner, site)) = self.task_sites.remove(&task) {
                    cancelled.push((owner, site, task));
                }
            }
        }
        for (owner, site, task) in cancelled {
            self.lifecycle.task_cancelled(owner, site, task);
        }
        for instance in &commit.disposed {
            if let Some(effects) = self.effects.remove(instance) {
                for effect in effects {
                    self.lifecycle.effect_cleaned(*instance, effect);
                }
            }
            self.lifecycle.disposed(*instance);
        }
    }
}

impl<M> Default for ComponentSession<M, ()> {
    fn default() -> Self {
        Self::new(
            ComponentForestLimits::default(),
            SchedulerConfig::default(),
            (),
        )
        .expect("default component session limits are valid")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::format;
    use alloc::string::String;
    use vo_ui_artifact::ComponentCallSiteId;

    #[derive(Default)]
    struct Events(Vec<String>);

    impl ComponentLifecycle for Events {
        fn mounted(&mut self, instance: ComponentInstanceId) {
            self.0.push(format!("mount:{}", instance.index()));
        }

        fn effect_started(&mut self, instance: ComponentInstanceId, effect: EffectId) {
            self.0.push(format!(
                "effect-start:{}:{}",
                instance.index(),
                effect.value()
            ));
        }

        fn task_cancelled(
            &mut self,
            instance: ComponentInstanceId,
            site: TaskSiteId,
            _task: TaskId,
        ) {
            self.0
                .push(format!("task-cancel:{}:{}", instance.index(), site.value()));
        }

        fn effect_cleaned(&mut self, instance: ComponentInstanceId, effect: EffectId) {
            self.0.push(format!(
                "effect-clean:{}:{}",
                instance.index(),
                effect.value()
            ));
        }

        fn disposed(&mut self, instance: ComponentInstanceId) {
            self.0.push(format!("dispose:{}", instance.index()));
        }
    }

    fn typ(name: &str) -> ComponentTypeId {
        ComponentTypeId::new("github.com/acme/app", name)
    }

    fn worker(key: i64) -> ComponentSpec {
        ComponentSpec::new(ComponentCallSiteId::new(1), typ("Worker"))
            .keyed(key)
            .effects([EffectId::new(0)])
            .tasks([TaskSiteId::new(0)])
    }

    #[test]
    fn disposal_cancels_before_cleanup_and_rejects_late_completion() {
        let mut session = ComponentSession::<&'static str, _>::new(
            ComponentForestLimits::default(),
            SchedulerConfig::default(),
            Events::default(),
        )
        .unwrap();
        let mounted = session.mount(typ("App"), alloc::vec![worker(1)]).unwrap();
        let owner = mounted.created[1];
        let task = session.spawn_task(owner, TaskSiteId::new(0)).unwrap();
        session.reconcile(Vec::new()).unwrap();
        let events = &session.lifecycle().0;
        let cancel = events
            .iter()
            .position(|event| event.starts_with("task-cancel"));
        let effect = events
            .iter()
            .position(|event| event.starts_with("effect-clean"));
        let dispose = events.iter().position(|event| event.starts_with("dispose"));
        assert!(cancel < effect && effect < dispose);
        assert!(!session.forest().contains(owner));

        session
            .enqueue_completion(TaskCompletion::new(task, "late"))
            .unwrap();
        let turn = session.drain_turn();
        assert!(turn.messages.is_empty());
        assert_eq!(turn.stale_completions, 1);
    }

    #[test]
    fn rejected_candidate_starts_no_lifecycle_work_and_keeps_tasks_live() {
        let mut session = ComponentSession::<&'static str, _>::new(
            ComponentForestLimits::default(),
            SchedulerConfig::default(),
            Events::default(),
        )
        .unwrap();
        let mounted = session.mount(typ("App"), alloc::vec![worker(1)]).unwrap();
        let owner = mounted.created[1];
        let task = session.spawn_task(owner, TaskSiteId::new(0)).unwrap();
        let event_count = session.lifecycle().0.len();
        assert!(session
            .reconcile(alloc::vec![worker(2), worker(2)])
            .is_err());
        assert_eq!(session.lifecycle().0.len(), event_count);
        assert!(session.scheduler().is_task_live(task));
        assert!(session.forest().contains(owner));
    }
}
