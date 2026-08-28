#![no_std]

extern crate alloc;

use alloc::boxed::Box;
use alloc::collections::{BTreeSet, VecDeque};
use alloc::vec::Vec;
use core::any::Any;
use core::fmt;
use core::marker::PhantomData;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ScopeId {
    index: u32,
    generation: u32,
}

impl ScopeId {
    pub const fn index(self) -> u32 {
        self.index
    }

    pub const fn generation(self) -> u32 {
        self.generation
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct EffectId {
    index: u32,
    generation: u32,
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Signal<T> {
    index: u32,
    generation: u32,
    marker: PhantomData<fn() -> T>,
}

impl<T> Copy for Signal<T> {}

impl<T> Clone for Signal<T> {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Lane {
    Sync,
    Render,
    Idle,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RuntimeConfig {
    pub max_effect_runs_per_flush: usize,
}

impl Default for RuntimeConfig {
    fn default() -> Self {
        Self {
            max_effect_runs_per_flush: 10_000,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct FlushReport {
    pub sync_runs: usize,
    pub render_runs: usize,
    pub idle_runs: usize,
}

impl FlushReport {
    pub const fn total_runs(self) -> usize {
        self.sync_runs + self.render_runs + self.idle_runs
    }

    fn record(&mut self, lane: Lane) {
        match lane {
            Lane::Sync => self.sync_runs += 1,
            Lane::Render => self.render_runs += 1,
            Lane::Idle => self.idle_runs += 1,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReactiveError {
    InvalidScope,
    InvalidSignal,
    InvalidEffect,
    SignalTypeMismatch,
    FlushLimitExceeded,
}

impl fmt::Display for ReactiveError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::InvalidScope => "reactive scope is stale or missing",
            Self::InvalidSignal => "reactive signal is stale or missing",
            Self::InvalidEffect => "reactive effect is stale or missing",
            Self::SignalTypeMismatch => "reactive signal type does not match its handle",
            Self::FlushLimitExceeded => "reactive flush exceeded its bounded work limit",
        })
    }
}

struct SignalNode {
    owner: ScopeId,
    value: Box<dyn Any>,
    subscribers: BTreeSet<EffectId>,
}

struct SignalSlot {
    generation: u32,
    node: Option<SignalNode>,
}

type EffectCallback = Box<dyn FnMut(&mut Runtime)>;

struct EffectNode {
    owner: ScopeId,
    lane: Lane,
    callback: Option<EffectCallback>,
    dependencies: BTreeSet<(u32, u32)>,
    queued: bool,
    running: bool,
}

struct EffectSlot {
    generation: u32,
    node: Option<EffectNode>,
}

struct ScopeNode {
    parent: Option<ScopeId>,
    children: BTreeSet<ScopeId>,
    signals: BTreeSet<(u32, u32)>,
    effects: BTreeSet<EffectId>,
}

struct ScopeSlot {
    generation: u32,
    node: Option<ScopeNode>,
}

pub struct Runtime {
    config: RuntimeConfig,
    scopes: Vec<ScopeSlot>,
    free_scopes: Vec<u32>,
    signals: Vec<SignalSlot>,
    free_signals: Vec<u32>,
    effects: Vec<EffectSlot>,
    free_effects: Vec<u32>,
    sync_queue: VecDeque<EffectId>,
    render_queue: VecDeque<EffectId>,
    idle_queue: VecDeque<EffectId>,
    current_effect: Option<EffectId>,
    batch_depth: usize,
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new(RuntimeConfig::default())
    }
}

impl Runtime {
    pub fn new(config: RuntimeConfig) -> Self {
        Self {
            config,
            scopes: alloc::vec![ScopeSlot {
                generation: 1,
                node: Some(ScopeNode {
                    parent: None,
                    children: BTreeSet::new(),
                    signals: BTreeSet::new(),
                    effects: BTreeSet::new(),
                }),
            }],
            free_scopes: Vec::new(),
            signals: Vec::new(),
            free_signals: Vec::new(),
            effects: Vec::new(),
            free_effects: Vec::new(),
            sync_queue: VecDeque::new(),
            render_queue: VecDeque::new(),
            idle_queue: VecDeque::new(),
            current_effect: None,
            batch_depth: 0,
        }
    }

    pub const fn root_scope(&self) -> ScopeId {
        ScopeId {
            index: 0,
            generation: 1,
        }
    }

    pub fn create_scope(&mut self, parent: ScopeId) -> Result<ScopeId, ReactiveError> {
        self.scope(parent)?;
        let id = if let Some(index) = self.free_scopes.pop() {
            let slot = &mut self.scopes[index as usize];
            let id = ScopeId {
                index,
                generation: slot.generation,
            };
            slot.node = Some(ScopeNode {
                parent: Some(parent),
                children: BTreeSet::new(),
                signals: BTreeSet::new(),
                effects: BTreeSet::new(),
            });
            id
        } else {
            let id = ScopeId {
                index: self.scopes.len() as u32,
                generation: 1,
            };
            self.scopes.push(ScopeSlot {
                generation: 1,
                node: Some(ScopeNode {
                    parent: Some(parent),
                    children: BTreeSet::new(),
                    signals: BTreeSet::new(),
                    effects: BTreeSet::new(),
                }),
            });
            id
        };
        self.scope_mut(parent)?.children.insert(id);
        Ok(id)
    }

    pub fn create_signal<T: 'static>(
        &mut self,
        owner: ScopeId,
        value: T,
    ) -> Result<Signal<T>, ReactiveError> {
        self.scope(owner)?;
        let (index, generation) = if let Some(index) = self.free_signals.pop() {
            let slot = &mut self.signals[index as usize];
            slot.node = Some(SignalNode {
                owner,
                value: Box::new(value),
                subscribers: BTreeSet::new(),
            });
            (index, slot.generation)
        } else {
            let index = self.signals.len() as u32;
            self.signals.push(SignalSlot {
                generation: 1,
                node: Some(SignalNode {
                    owner,
                    value: Box::new(value),
                    subscribers: BTreeSet::new(),
                }),
            });
            (index, 1)
        };
        self.scope_mut(owner)?.signals.insert((index, generation));
        Ok(Signal {
            index,
            generation,
            marker: PhantomData,
        })
    }

    pub fn get<T: Clone + 'static>(&mut self, signal: Signal<T>) -> Result<T, ReactiveError> {
        let value = self
            .signal(signal.index, signal.generation)?
            .value
            .downcast_ref::<T>()
            .ok_or(ReactiveError::SignalTypeMismatch)?
            .clone();
        if let Some(effect) = self.current_effect {
            self.signal_mut(signal.index, signal.generation)?
                .subscribers
                .insert(effect);
            self.effect_mut(effect)?
                .dependencies
                .insert((signal.index, signal.generation));
        }
        Ok(value)
    }

    pub fn set<T: 'static>(&mut self, signal: Signal<T>, value: T) -> Result<(), ReactiveError> {
        let subscribers = {
            let node = self.signal_mut(signal.index, signal.generation)?;
            let target = node
                .value
                .downcast_mut::<T>()
                .ok_or(ReactiveError::SignalTypeMismatch)?;
            *target = value;
            node.subscribers.iter().copied().collect::<Vec<_>>()
        };
        for effect in subscribers {
            self.schedule(effect)?;
        }
        Ok(())
    }

    pub fn set_if_changed<T: PartialEq + 'static>(
        &mut self,
        signal: Signal<T>,
        value: T,
    ) -> Result<bool, ReactiveError> {
        let changed = {
            let node = self.signal_mut(signal.index, signal.generation)?;
            let target = node
                .value
                .downcast_mut::<T>()
                .ok_or(ReactiveError::SignalTypeMismatch)?;
            if *target == value {
                false
            } else {
                *target = value;
                true
            }
        };
        if changed {
            let subscribers = self
                .signal(signal.index, signal.generation)?
                .subscribers
                .iter()
                .copied()
                .collect::<Vec<_>>();
            for effect in subscribers {
                self.schedule(effect)?;
            }
        }
        Ok(changed)
    }

    pub fn update<T: 'static>(
        &mut self,
        signal: Signal<T>,
        update: impl FnOnce(&mut T),
    ) -> Result<(), ReactiveError> {
        let subscribers = {
            let node = self.signal_mut(signal.index, signal.generation)?;
            let target = node
                .value
                .downcast_mut::<T>()
                .ok_or(ReactiveError::SignalTypeMismatch)?;
            update(target);
            node.subscribers.iter().copied().collect::<Vec<_>>()
        };
        for effect in subscribers {
            self.schedule(effect)?;
        }
        Ok(())
    }

    pub fn create_effect(
        &mut self,
        owner: ScopeId,
        lane: Lane,
        callback: impl FnMut(&mut Runtime) + 'static,
    ) -> Result<EffectId, ReactiveError> {
        self.scope(owner)?;
        let id = if let Some(index) = self.free_effects.pop() {
            let slot = &mut self.effects[index as usize];
            let id = EffectId {
                index,
                generation: slot.generation,
            };
            slot.node = Some(EffectNode {
                owner,
                lane,
                callback: Some(Box::new(callback)),
                dependencies: BTreeSet::new(),
                queued: false,
                running: false,
            });
            id
        } else {
            let id = EffectId {
                index: self.effects.len() as u32,
                generation: 1,
            };
            self.effects.push(EffectSlot {
                generation: 1,
                node: Some(EffectNode {
                    owner,
                    lane,
                    callback: Some(Box::new(callback)),
                    dependencies: BTreeSet::new(),
                    queued: false,
                    running: false,
                }),
            });
            id
        };
        self.scope_mut(owner)?.effects.insert(id);
        self.schedule(id)?;
        Ok(id)
    }

    pub fn batch<R>(&mut self, update: impl FnOnce(&mut Self) -> R) -> R {
        self.batch_depth += 1;
        let result = update(self);
        self.batch_depth -= 1;
        result
    }

    pub const fn is_batching(&self) -> bool {
        self.batch_depth != 0
    }

    pub fn flush(&mut self) -> Result<FlushReport, ReactiveError> {
        if self.is_batching() {
            return Ok(FlushReport::default());
        }
        let mut report = FlushReport::default();
        loop {
            let Some((effect, lane)) = self.dequeue() else {
                break;
            };
            if report.total_runs() >= self.config.max_effect_runs_per_flush {
                return Err(ReactiveError::FlushLimitExceeded);
            }
            if self.effect(effect).is_err() {
                continue;
            }
            self.run_effect(effect)?;
            report.record(lane);
        }
        Ok(report)
    }

    pub fn dispose_scope(&mut self, scope: ScopeId) -> Result<(), ReactiveError> {
        if scope == self.root_scope() {
            return Err(ReactiveError::InvalidScope);
        }
        let (parent, children, effects, signals) = {
            let node = self.scope(scope)?;
            (
                node.parent,
                node.children.iter().copied().collect::<Vec<_>>(),
                node.effects.iter().copied().collect::<Vec<_>>(),
                node.signals.iter().copied().collect::<Vec<_>>(),
            )
        };
        for child in children {
            self.dispose_scope(child)?;
        }
        for effect in effects {
            self.dispose_effect(effect)?;
        }
        for (index, generation) in signals {
            self.dispose_signal(index, generation)?;
        }
        if let Some(parent) = parent {
            self.scope_mut(parent)?.children.remove(&scope);
        }
        let slot = self
            .scopes
            .get_mut(scope.index as usize)
            .ok_or(ReactiveError::InvalidScope)?;
        slot.node = None;
        slot.generation = next_generation(slot.generation);
        self.free_scopes.push(scope.index);
        Ok(())
    }

    fn run_effect(&mut self, effect: EffectId) -> Result<(), ReactiveError> {
        let dependencies = self
            .effect(effect)?
            .dependencies
            .iter()
            .copied()
            .collect::<Vec<_>>();
        for (index, generation) in dependencies {
            if let Ok(signal) = self.signal_mut(index, generation) {
                signal.subscribers.remove(&effect);
            }
        }
        let mut callback = {
            let node = self.effect_mut(effect)?;
            node.dependencies.clear();
            node.running = true;
            node.callback.take().ok_or(ReactiveError::InvalidEffect)?
        };
        let previous = self.current_effect.replace(effect);
        callback(self);
        self.current_effect = previous;
        if let Ok(node) = self.effect_mut(effect) {
            node.running = false;
            node.callback = Some(callback);
        }
        Ok(())
    }

    fn schedule(&mut self, effect: EffectId) -> Result<(), ReactiveError> {
        let lane = {
            let node = self.effect_mut(effect)?;
            if node.queued {
                return Ok(());
            }
            node.queued = true;
            node.lane
        };
        self.queue_mut(lane).push_back(effect);
        Ok(())
    }

    fn dequeue(&mut self) -> Option<(EffectId, Lane)> {
        for lane in [Lane::Sync, Lane::Render, Lane::Idle] {
            while let Some(effect) = self.queue_mut(lane).pop_front() {
                if let Ok(node) = self.effect_mut(effect) {
                    node.queued = false;
                    return Some((effect, lane));
                }
            }
        }
        None
    }

    fn dispose_effect(&mut self, effect: EffectId) -> Result<(), ReactiveError> {
        let (owner, dependencies) = {
            let node = self.effect(effect)?;
            (
                node.owner,
                node.dependencies.iter().copied().collect::<Vec<_>>(),
            )
        };
        for (index, generation) in dependencies {
            if let Ok(signal) = self.signal_mut(index, generation) {
                signal.subscribers.remove(&effect);
            }
        }
        if let Ok(scope) = self.scope_mut(owner) {
            scope.effects.remove(&effect);
        }
        let slot = self
            .effects
            .get_mut(effect.index as usize)
            .ok_or(ReactiveError::InvalidEffect)?;
        slot.node = None;
        slot.generation = next_generation(slot.generation);
        self.free_effects.push(effect.index);
        Ok(())
    }

    fn dispose_signal(&mut self, index: u32, generation: u32) -> Result<(), ReactiveError> {
        let (owner, subscribers) = {
            let node = self.signal(index, generation)?;
            (
                node.owner,
                node.subscribers.iter().copied().collect::<Vec<_>>(),
            )
        };
        for effect in subscribers {
            if let Ok(node) = self.effect_mut(effect) {
                node.dependencies.remove(&(index, generation));
            }
        }
        if let Ok(scope) = self.scope_mut(owner) {
            scope.signals.remove(&(index, generation));
        }
        let slot = self
            .signals
            .get_mut(index as usize)
            .ok_or(ReactiveError::InvalidSignal)?;
        slot.node = None;
        slot.generation = next_generation(slot.generation);
        self.free_signals.push(index);
        Ok(())
    }

    fn scope(&self, id: ScopeId) -> Result<&ScopeNode, ReactiveError> {
        let slot = self
            .scopes
            .get(id.index as usize)
            .ok_or(ReactiveError::InvalidScope)?;
        if slot.generation != id.generation {
            return Err(ReactiveError::InvalidScope);
        }
        slot.node.as_ref().ok_or(ReactiveError::InvalidScope)
    }

    fn scope_mut(&mut self, id: ScopeId) -> Result<&mut ScopeNode, ReactiveError> {
        let slot = self
            .scopes
            .get_mut(id.index as usize)
            .ok_or(ReactiveError::InvalidScope)?;
        if slot.generation != id.generation {
            return Err(ReactiveError::InvalidScope);
        }
        slot.node.as_mut().ok_or(ReactiveError::InvalidScope)
    }

    fn signal(&self, index: u32, generation: u32) -> Result<&SignalNode, ReactiveError> {
        let slot = self
            .signals
            .get(index as usize)
            .ok_or(ReactiveError::InvalidSignal)?;
        if slot.generation != generation {
            return Err(ReactiveError::InvalidSignal);
        }
        slot.node.as_ref().ok_or(ReactiveError::InvalidSignal)
    }

    fn signal_mut(
        &mut self,
        index: u32,
        generation: u32,
    ) -> Result<&mut SignalNode, ReactiveError> {
        let slot = self
            .signals
            .get_mut(index as usize)
            .ok_or(ReactiveError::InvalidSignal)?;
        if slot.generation != generation {
            return Err(ReactiveError::InvalidSignal);
        }
        slot.node.as_mut().ok_or(ReactiveError::InvalidSignal)
    }

    fn effect(&self, id: EffectId) -> Result<&EffectNode, ReactiveError> {
        let slot = self
            .effects
            .get(id.index as usize)
            .ok_or(ReactiveError::InvalidEffect)?;
        if slot.generation != id.generation {
            return Err(ReactiveError::InvalidEffect);
        }
        slot.node.as_ref().ok_or(ReactiveError::InvalidEffect)
    }

    fn effect_mut(&mut self, id: EffectId) -> Result<&mut EffectNode, ReactiveError> {
        let slot = self
            .effects
            .get_mut(id.index as usize)
            .ok_or(ReactiveError::InvalidEffect)?;
        if slot.generation != id.generation {
            return Err(ReactiveError::InvalidEffect);
        }
        slot.node.as_mut().ok_or(ReactiveError::InvalidEffect)
    }

    fn queue_mut(&mut self, lane: Lane) -> &mut VecDeque<EffectId> {
        match lane {
            Lane::Sync => &mut self.sync_queue,
            Lane::Render => &mut self.render_queue,
            Lane::Idle => &mut self.idle_queue,
        }
    }
}

fn next_generation(current: u32) -> u32 {
    current.wrapping_add(1).max(1)
}

#[cfg(test)]
mod tests {
    extern crate std;

    use super::*;
    use alloc::rc::Rc;
    use core::cell::RefCell;

    #[test]
    fn dynamic_dependencies_follow_the_latest_branch() {
        let mut runtime = Runtime::default();
        let root = runtime.root_scope();
        let enabled = runtime.create_signal(root, true).unwrap();
        let first = runtime.create_signal(root, 1_i64).unwrap();
        let second = runtime.create_signal(root, 10_i64).unwrap();
        let seen = Rc::new(RefCell::new(Vec::new()));
        let output = Rc::clone(&seen);
        runtime
            .create_effect(root, Lane::Render, move |runtime| {
                let enabled = runtime.get(enabled).unwrap();
                let value = if enabled {
                    runtime.get(first).unwrap()
                } else {
                    runtime.get(second).unwrap()
                };
                output.borrow_mut().push(value);
            })
            .unwrap();

        runtime.flush().unwrap();
        runtime.set(second, 11).unwrap();
        assert_eq!(runtime.flush().unwrap().total_runs(), 0);
        runtime.set(enabled, false).unwrap();
        runtime.flush().unwrap();
        runtime.set(first, 2).unwrap();
        assert_eq!(runtime.flush().unwrap().total_runs(), 0);
        runtime.set(second, 12).unwrap();
        runtime.flush().unwrap();

        assert_eq!(&*seen.borrow(), &[1, 11, 12]);
    }

    #[test]
    fn batching_deduplicates_effects_and_preserves_lane_order() {
        let mut runtime = Runtime::default();
        let root = runtime.root_scope();
        let signal = runtime.create_signal(root, 0_i64).unwrap();
        let order = Rc::new(RefCell::new(Vec::new()));
        for (lane, label) in [(Lane::Idle, 3), (Lane::Render, 2), (Lane::Sync, 1)] {
            let order = Rc::clone(&order);
            runtime
                .create_effect(root, lane, move |runtime| {
                    let _ = runtime.get(signal).unwrap();
                    order.borrow_mut().push(label);
                })
                .unwrap();
        }
        runtime.flush().unwrap();
        order.borrow_mut().clear();

        runtime.batch(|runtime| {
            runtime.set(signal, 1).unwrap();
            runtime.set(signal, 2).unwrap();
        });
        let report = runtime.flush().unwrap();

        assert_eq!(&*order.borrow(), &[1, 2, 3]);
        assert_eq!(report.total_runs(), 3);
    }

    #[test]
    fn disposing_scope_invalidates_owned_handles() {
        let mut runtime = Runtime::default();
        let scope = runtime.create_scope(runtime.root_scope()).unwrap();
        let signal = runtime.create_signal(scope, 7_i64).unwrap();
        runtime.dispose_scope(scope).unwrap();
        assert_eq!(runtime.get(signal), Err(ReactiveError::InvalidSignal));
    }
}
