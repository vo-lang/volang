//! Arena-based storage with typed keys.
//!
//! Provides an append-only arena container that uses typed keys for indexing.
//! This is similar to slotmap but simpler - no deletion support.

use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

/// Trait for typed arena keys.
pub trait ArenaKey: Copy + Clone + Eq + PartialEq + std::hash::Hash + std::fmt::Debug {
    fn from_usize(idx: usize) -> Self;
    fn as_usize(&self) -> usize;
    fn null() -> Self;
    fn is_null(&self) -> bool {
        *self == Self::null()
    }
}

/// An append-only arena container with typed keys.
#[derive(Debug)]
pub struct Arena<K: ArenaKey, V> {
    vec: Vec<V>,
    _marker: PhantomData<K>,
}

impl<K: ArenaKey, V> Default for Arena<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: ArenaKey, V> Arena<K, V> {
    /// Creates a new empty arena.
    pub fn new() -> Self {
        Self {
            vec: Vec::new(),
            _marker: PhantomData,
        }
    }

    /// Creates an arena with pre-allocated capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            vec: Vec::with_capacity(capacity),
            _marker: PhantomData,
        }
    }

    /// Inserts a value and returns its key.
    #[inline]
    pub fn insert(&mut self, value: V) -> K {
        let idx = self.vec.len();
        self.vec.push(value);
        K::from_usize(idx)
    }

    /// Gets a reference to a value by key.
    #[inline]
    pub fn get(&self, key: K) -> Option<&V> {
        self.vec.get(key.as_usize())
    }

    /// Gets a mutable reference to a value by key.
    #[inline]
    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.vec.get_mut(key.as_usize())
    }

    /// Returns the number of elements in the arena.
    #[inline]
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    /// Returns true if the arena is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    /// Returns an iterator over all values.
    pub fn iter(&self) -> impl Iterator<Item = (K, &V)> {
        self.vec
            .iter()
            .enumerate()
            .map(|(i, v)| (K::from_usize(i), v))
    }

    /// Returns a mutable iterator over all values.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (K, &mut V)> {
        self.vec
            .iter_mut()
            .enumerate()
            .map(|(i, v)| (K::from_usize(i), v))
    }

    /// Returns the underlying vec.
    pub fn as_slice(&self) -> &[V] {
        &self.vec
    }
}

impl<K: ArenaKey, V> Index<K> for Arena<K, V> {
    type Output = V;

    #[inline]
    fn index(&self, key: K) -> &Self::Output {
        &self.vec[key.as_usize()]
    }
}

impl<K: ArenaKey, V> IndexMut<K> for Arena<K, V> {
    #[inline]
    fn index_mut(&mut self, key: K) -> &mut Self::Output {
        &mut self.vec[key.as_usize()]
    }
}

/// Macro to define a typed arena key.
#[macro_export]
macro_rules! define_key {
    ($($(#[$attr:meta])* $vis:vis struct $name:ident;)*) => {
        $(
            $(#[$attr])*
            #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Default)]
            #[repr(transparent)]
            $vis struct $name(u32);

            impl $name {
                /// Creates a new key from a raw u32 value.
                #[inline]
                pub fn new(idx: u32) -> Self {
                    Self(idx)
                }

                /// Returns the raw u32 value.
                #[inline]
                pub fn raw(&self) -> u32 {
                    self.0
                }
            }

            impl $crate::arena::ArenaKey for $name {
                #[inline]
                fn from_usize(idx: usize) -> Self {
                    Self(idx as u32)
                }

                #[inline]
                fn as_usize(&self) -> usize {
                    self.0 as usize
                }

                #[inline]
                fn null() -> Self {
                    Self(u32::MAX)
                }
            }
        )*
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    define_key! {
        struct TestKey;
    }

    #[test]
    fn test_arena_insert_and_get() {
        let mut arena: Arena<TestKey, String> = Arena::new();
        let k1 = arena.insert("hello".to_string());
        let k2 = arena.insert("world".to_string());

        assert_eq!(arena[k1], "hello");
        assert_eq!(arena[k2], "world");
        assert_eq!(arena.len(), 2);
    }

    #[test]
    fn test_arena_iter() {
        let mut arena: Arena<TestKey, i32> = Arena::new();
        arena.insert(10);
        arena.insert(20);
        arena.insert(30);

        let sum: i32 = arena.iter().map(|(_, v)| v).sum();
        assert_eq!(sum, 60);
    }

    #[test]
    fn test_null_key() {
        let null = TestKey::null();
        assert!(null.is_null());
        assert_eq!(null.raw(), u32::MAX);
    }
}
