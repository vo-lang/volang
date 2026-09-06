//! Reserve declared resources when choosing work, so a busy resource never
//! occupies a worker that could execute an unrelated case.
use std::collections::{BTreeSet, VecDeque};
use std::sync::{Condvar, Mutex};

struct State {
    pending: VecDeque<usize>,
    active: BTreeSet<String>,
}

pub(crate) struct Resources {
    names: Vec<Option<String>>,
    state: Mutex<State>,
    changed: Condvar,
}

pub(crate) struct Permit<'a> {
    owner: &'a Resources,
    pub(crate) index: usize,
}

impl Resources {
    pub(crate) fn new(names: Vec<Option<String>>) -> Self {
        let pending = (0..names.len()).collect();
        Self {
            names,
            state: Mutex::new(State {
                pending,
                active: BTreeSet::new(),
            }),
            changed: Condvar::new(),
        }
    }

    pub(crate) fn next(&self) -> Option<Permit<'_>> {
        let mut state = self.state.lock().unwrap_or_else(|error| error.into_inner());
        loop {
            if state.pending.is_empty() {
                return None;
            }
            let available = state.pending.iter().position(|index| {
                self.names[*index]
                    .as_ref()
                    .is_none_or(|name| !state.active.contains(name))
            });
            if let Some(position) = available {
                let index = state
                    .pending
                    .remove(position)
                    .expect("pending index exists");
                if let Some(name) = &self.names[index] {
                    state.active.insert(name.clone());
                }
                return Some(Permit { owner: self, index });
            }
            state = self
                .changed
                .wait(state)
                .unwrap_or_else(|error| error.into_inner());
        }
    }
}

impl Drop for Permit<'_> {
    fn drop(&mut self) {
        if let Some(name) = &self.owner.names[self.index] {
            self.owner
                .state
                .lock()
                .unwrap_or_else(|error| error.into_inner())
                .active
                .remove(name);
            self.owner.changed.notify_all();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn occupied_resource_does_not_block_independent_work_and_release_resumes_fifo() {
        let resources = Resources::new(vec![
            Some("socket".into()),
            Some("socket".into()),
            None,
            Some("font".into()),
            Some("socket".into()),
        ]);
        let first = resources.next().unwrap();
        assert_eq!(first.index, 0);
        let independent = resources.next().unwrap();
        assert_eq!(independent.index, 2);
        let other = resources.next().unwrap();
        assert_eq!(other.index, 3);
        drop(first);
        let second = resources.next().unwrap();
        assert_eq!(second.index, 1);
        drop(second);
        assert_eq!(resources.next().unwrap().index, 4);
        assert!(resources.next().is_none());
    }

    #[test]
    fn released_resource_wakes_a_waiting_worker() {
        let resources = Resources::new(vec![Some("shared".into()), Some("shared".into())]);
        let first = resources.next().unwrap();
        std::thread::scope(|scope| {
            let waiting = scope.spawn(|| resources.next().unwrap().index);
            drop(first);
            assert_eq!(waiting.join().unwrap(), 1);
        });
        assert!(resources.next().is_none());
    }
}
