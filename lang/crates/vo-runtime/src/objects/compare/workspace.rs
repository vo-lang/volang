//! Temporary type traversal state. Small graphs stay on the native stack;
//! larger graphs spill once to a hash set and retain linear traversal cost.

use hashbrown::HashSet;
use smallvec::SmallVec;

pub(super) enum TypeSet {
    Inline(SmallVec<[u32; 8]>),
    Hashed(HashSet<u32>),
}

impl TypeSet {
    pub(super) fn new() -> Self {
        Self::Inline(SmallVec::new())
    }

    pub(super) fn contains(&self, value: &u32) -> bool {
        match self {
            Self::Inline(values) => values.contains(value),
            Self::Hashed(values) => values.contains(value),
        }
    }

    pub(super) fn insert(&mut self, value: u32) -> bool {
        match self {
            Self::Inline(values) => {
                if values.contains(&value) {
                    return false;
                }
                if values.len() < values.inline_size() {
                    values.push(value);
                } else {
                    let mut hashed = HashSet::with_capacity(values.len() * 2);
                    hashed.extend(values.iter().copied());
                    hashed.insert(value);
                    *self = Self::Hashed(hashed);
                }
                true
            }
            Self::Hashed(values) => values.insert(value),
        }
    }

    pub(super) fn remove(&mut self, value: &u32) -> bool {
        match self {
            Self::Inline(values) => {
                if let Some(index) = values.iter().position(|candidate| candidate == value) {
                    values.swap_remove(index);
                    true
                } else {
                    false
                }
            }
            Self::Hashed(values) => values.remove(value),
        }
    }
}
