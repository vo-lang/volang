#![no_std]

extern crate alloc;

use alloc::collections::{BTreeMap, BTreeSet, VecDeque};
use alloc::vec::Vec;
use core::fmt;
use vo_ui_core::TaskId;
use vo_ui_reactive::ScopeId;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SchedulerConfig {
    pub max_live_tasks: usize,
    pub max_pending_completions: usize,
    pub max_completions_per_turn: usize,
}

impl Default for SchedulerConfig {
    fn default() -> Self {
        Self {
            max_live_tasks: 65_536,
            max_pending_completions: 65_536,
            max_completions_per_turn: 1_024,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SchedulerError {
    InvalidConfiguration,
    TaskLimitExceeded,
    TaskIdentityExhausted,
}

impl fmt::Display for SchedulerError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::InvalidConfiguration => "UI scheduler limits must be greater than zero",
            Self::TaskLimitExceeded => "UI task limit exceeded",
            Self::TaskIdentityExhausted => "UI task identity space exhausted",
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TaskCompletion<M> {
    pub task: TaskId,
    pub message: M,
}

impl<M> TaskCompletion<M> {
    pub const fn new(task: TaskId, message: M) -> Self {
        Self { task, message }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ScopedMessage<M, O = ScopeId> {
    pub owner: O,
    pub task: TaskId,
    pub message: M,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueueFull<M> {
    pub completion: TaskCompletion<M>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UiTurn<M, O = ScopeId> {
    pub messages: Vec<ScopedMessage<M, O>>,
    pub inspected_completions: usize,
    pub stale_completions: usize,
    pub has_more: bool,
}

#[derive(Clone, Debug)]
struct TaskSlot<O> {
    generation: u32,
    owner: Option<O>,
}

#[derive(Clone, Debug)]
struct TaskRegistry<O> {
    slots: Vec<TaskSlot<O>>,
    free: Vec<u32>,
    by_owner: BTreeMap<O, BTreeSet<TaskId>>,
    live: usize,
    limit: usize,
}

impl<O: Copy + Ord> TaskRegistry<O> {
    fn new(limit: usize) -> Self {
        Self {
            slots: Vec::new(),
            free: Vec::new(),
            by_owner: BTreeMap::new(),
            live: 0,
            limit,
        }
    }

    fn spawn(&mut self, owner: O) -> Result<TaskId, SchedulerError> {
        if self.live >= self.limit {
            return Err(SchedulerError::TaskLimitExceeded);
        }
        let task = if let Some(index) = self.free.pop() {
            let slot = &mut self.slots[index as usize];
            slot.owner = Some(owner);
            TaskId::new(index, slot.generation)
        } else {
            let index = u32::try_from(self.slots.len())
                .map_err(|_| SchedulerError::TaskIdentityExhausted)?;
            self.slots.push(TaskSlot {
                generation: 1,
                owner: Some(owner),
            });
            TaskId::new(index, 1)
        };
        self.by_owner.entry(owner).or_default().insert(task);
        self.live += 1;
        Ok(task)
    }

    fn is_live(&self, task: TaskId) -> bool {
        self.slots
            .get(task.index() as usize)
            .is_some_and(|slot| slot.generation == task.generation() && slot.owner.is_some())
    }

    fn accept(&mut self, task: TaskId) -> Option<O> {
        if !self.is_live(task) {
            return None;
        }
        self.retire(task)
    }

    fn cancel(&mut self, task: TaskId) -> bool {
        if !self.is_live(task) {
            return false;
        }
        self.retire(task).is_some()
    }

    fn cancel_scope(&mut self, owner: O) -> Vec<TaskId> {
        let tasks = self
            .by_owner
            .get(&owner)
            .map(|tasks| tasks.iter().copied().collect::<Vec<_>>())
            .unwrap_or_default();
        for task in &tasks {
            self.retire(*task);
        }
        tasks
    }

    fn retire(&mut self, task: TaskId) -> Option<O> {
        let slot = self.slots.get_mut(task.index() as usize)?;
        if slot.generation != task.generation() {
            return None;
        }
        let owner = slot.owner.take()?;
        slot.generation = slot.generation.wrapping_add(1).max(1);
        self.free.push(task.index());
        self.live -= 1;
        if let Some(tasks) = self.by_owner.get_mut(&owner) {
            tasks.remove(&task);
            if tasks.is_empty() {
                self.by_owner.remove(&owner);
            }
        }
        Some(owner)
    }
}

/// UI-owned scheduler state. Goroutines send `TaskCompletion` values through a
/// host MPSC channel; the owning UI Island enqueues them here and drains one
/// bounded turn at a time.
pub struct UiScheduler<M, O = ScopeId> {
    config: SchedulerConfig,
    tasks: TaskRegistry<O>,
    pending: VecDeque<TaskCompletion<M>>,
}

impl<M, O: Copy + Ord> UiScheduler<M, O> {
    pub fn new(config: SchedulerConfig) -> Result<Self, SchedulerError> {
        if config.max_live_tasks == 0
            || config.max_pending_completions == 0
            || config.max_completions_per_turn == 0
        {
            return Err(SchedulerError::InvalidConfiguration);
        }
        Ok(Self {
            tasks: TaskRegistry::new(config.max_live_tasks),
            pending: VecDeque::new(),
            config,
        })
    }

    pub fn spawn_task(&mut self, owner: O) -> Result<TaskId, SchedulerError> {
        self.tasks.spawn(owner)
    }

    pub fn is_task_live(&self, task: TaskId) -> bool {
        self.tasks.is_live(task)
    }

    pub const fn live_task_count(&self) -> usize {
        self.tasks.live
    }

    pub fn pending_completion_count(&self) -> usize {
        self.pending.len()
    }

    pub fn enqueue_completion(
        &mut self,
        completion: TaskCompletion<M>,
    ) -> Result<(), QueueFull<M>> {
        if self.pending.len() >= self.config.max_pending_completions {
            return Err(QueueFull { completion });
        }
        self.pending.push_back(completion);
        Ok(())
    }

    /// Invalidates the task immediately. The returned identity lets the host
    /// signal cancellation to a worker goroutine when that facility exists.
    pub fn cancel_task(&mut self, task: TaskId) -> Option<TaskId> {
        self.tasks.cancel(task).then_some(task)
    }

    /// Invalidates every task owned by a component scope. Completions already
    /// in flight remain safe because their generations no longer match.
    pub fn cancel_scope(&mut self, owner: O) -> Vec<TaskId> {
        self.tasks.cancel_scope(owner)
    }

    /// Takes bounded work from the mailbox. The caller processes every message
    /// inside one reactive batch and performs at most one renderer commit.
    pub fn drain_turn(&mut self) -> UiTurn<M, O> {
        let mut messages = Vec::new();
        let mut inspected = 0;
        let mut stale = 0;
        while inspected < self.config.max_completions_per_turn {
            let Some(completion) = self.pending.pop_front() else {
                break;
            };
            inspected += 1;
            if let Some(owner) = self.tasks.accept(completion.task) {
                messages.push(ScopedMessage {
                    owner,
                    task: completion.task,
                    message: completion.message,
                });
            } else {
                stale += 1;
            }
        }
        UiTurn {
            messages,
            inspected_completions: inspected,
            stale_completions: stale,
            has_more: !self.pending.is_empty(),
        }
    }
}

impl<M, O: Copy + Ord> Default for UiScheduler<M, O> {
    fn default() -> Self {
        Self::new(SchedulerConfig::default()).expect("default UI scheduler limits are valid")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_ui_reactive::{Runtime, RuntimeConfig};

    fn scopes() -> (ScopeId, ScopeId) {
        let mut runtime = Runtime::new(RuntimeConfig::default());
        let first = runtime.create_scope(runtime.root_scope()).unwrap();
        let second = runtime.create_scope(runtime.root_scope()).unwrap();
        (first, second)
    }

    #[test]
    fn scope_cancellation_rejects_late_completion_and_reuses_generation() {
        let (scope, _) = scopes();
        let mut scheduler = UiScheduler::default();
        let stale = scheduler.spawn_task(scope).unwrap();
        assert_eq!(scheduler.cancel_scope(scope), alloc::vec![stale]);
        scheduler
            .enqueue_completion(TaskCompletion::new(stale, "late"))
            .unwrap();
        let current = scheduler.spawn_task(scope).unwrap();
        assert_eq!(stale.index(), current.index());
        assert_ne!(stale.generation(), current.generation());

        let turn = scheduler.drain_turn();
        assert!(turn.messages.is_empty());
        assert_eq!(turn.stale_completions, 1);
        assert!(scheduler.is_task_live(current));
    }

    #[test]
    fn a_turn_is_fifo_bounded_and_delivers_each_task_once() {
        let (first, second) = scopes();
        let config = SchedulerConfig {
            max_live_tasks: 8,
            max_pending_completions: 8,
            max_completions_per_turn: 2,
        };
        let mut scheduler = UiScheduler::new(config).unwrap();
        let a = scheduler.spawn_task(first).unwrap();
        let b = scheduler.spawn_task(second).unwrap();
        scheduler
            .enqueue_completion(TaskCompletion::new(a, 10))
            .unwrap();
        scheduler
            .enqueue_completion(TaskCompletion::new(a, 11))
            .unwrap();
        scheduler
            .enqueue_completion(TaskCompletion::new(b, 12))
            .unwrap();

        let first_turn = scheduler.drain_turn();
        assert_eq!(first_turn.inspected_completions, 2);
        assert_eq!(first_turn.messages.len(), 1);
        assert_eq!(first_turn.messages[0].message, 10);
        assert_eq!(first_turn.stale_completions, 1);
        assert!(first_turn.has_more);

        let second_turn = scheduler.drain_turn();
        assert_eq!(second_turn.messages[0].message, 12);
        assert!(!second_turn.has_more);
    }

    #[test]
    fn capacity_errors_return_ownership_of_completion() {
        let (scope, _) = scopes();
        let config = SchedulerConfig {
            max_live_tasks: 1,
            max_pending_completions: 1,
            max_completions_per_turn: 1,
        };
        let mut scheduler = UiScheduler::new(config).unwrap();
        let task = scheduler.spawn_task(scope).unwrap();
        assert_eq!(
            scheduler.spawn_task(scope),
            Err(SchedulerError::TaskLimitExceeded)
        );
        scheduler
            .enqueue_completion(TaskCompletion::new(task, 1))
            .unwrap();
        let error = scheduler
            .enqueue_completion(TaskCompletion::new(task, 2))
            .unwrap_err();
        assert_eq!(error.completion.message, 2);
    }
}
