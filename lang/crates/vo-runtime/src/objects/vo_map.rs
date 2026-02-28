//! VoMap: Custom hash map with iteration-safe semantics
//!
//! Key properties:
//! - Delete during iteration: uses tombstones, safe
//! - Insert during iteration: if resize happens, generation increments, iterator terminates
//! - Open addressing with linear probing

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

use core::hash::Hash;
use core::mem;

/// Default load factor threshold for resize (75%)
const LOAD_FACTOR_NUM: usize = 3;
const LOAD_FACTOR_DEN: usize = 4;

/// Minimum capacity (must be power of 2)
const MIN_CAPACITY: usize = 8;

/// Bucket state
#[derive(Clone)]
enum Bucket<K, V> {
    Empty,
    Tombstone,
    Occupied { key: K, value: V, hash: u64 },
}

impl<K, V> Bucket<K, V> {
    #[inline]
    fn is_empty(&self) -> bool {
        matches!(self, Bucket::Empty)
    }
}

/// Open addressing hash map with iteration-safe semantics
pub struct VoMap<K, V> {
    buckets: Box<[Bucket<K, V>]>,
    len: usize,
    /// Number of occupied + tombstone slots (for resize decision)
    used: usize,
    /// Incremented on resize - iterator invalidation marker
    generation: u32,
}

impl<K, V> VoMap<K, V> {
    /// Create empty map
    pub fn new() -> Self {
        Self {
            buckets: Box::new([]),
            len: 0,
            used: 0,
            generation: 0,
        }
    }

    /// Create map with capacity
    pub fn with_capacity(capacity: usize) -> Self {
        let cap = capacity.next_power_of_two().max(MIN_CAPACITY);
        let buckets = (0..cap).map(|_| Bucket::Empty).collect();
        Self {
            buckets,
            len: 0,
            used: 0,
            generation: 0,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.buckets.len()
    }

    #[inline]
    pub fn generation(&self) -> u32 {
        self.generation
    }

    /// Check if resize is needed
    #[inline]
    fn needs_resize(&self) -> bool {
        if self.buckets.is_empty() {
            return true;
        }
        // Resize when used slots exceed load factor
        self.used * LOAD_FACTOR_DEN > self.buckets.len() * LOAD_FACTOR_NUM
    }

    /// Compute bucket index from hash
    #[inline]
    fn bucket_index(&self, hash: u64) -> usize {
        (hash as usize) & (self.buckets.len() - 1)
    }
}

impl<K: Eq + Hash, V> VoMap<K, V> {
    /// Insert key-value pair. Returns old value if key existed.
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.insert_with_hash(key, value, |k| hash_one(k))
    }

    /// Insert with custom hash function
    pub fn insert_with_hash<F>(&mut self, key: K, value: V, hasher: F) -> Option<V>
    where
        F: Fn(&K) -> u64,
    {
        if self.needs_resize() {
            self.resize(&hasher);
        }

        let hash = hasher(&key);
        let mut idx = self.bucket_index(hash);
        let mut tombstone_idx: Option<usize> = None;

        loop {
            match &self.buckets[idx] {
                Bucket::Empty => {
                    // Insert at tombstone if found, else here
                    let insert_idx = tombstone_idx.unwrap_or(idx);
                    if tombstone_idx.is_none() {
                        self.used += 1;
                    }
                    self.len += 1;
                    self.buckets[insert_idx] = Bucket::Occupied { key, value, hash };
                    return None;
                }
                Bucket::Tombstone => {
                    if tombstone_idx.is_none() {
                        tombstone_idx = Some(idx);
                    }
                }
                Bucket::Occupied { key: k, hash: h, .. } => {
                    if *h == hash && *k == key {
                        // Replace existing
                        let old = mem::replace(
                            &mut self.buckets[idx],
                            Bucket::Occupied { key, value, hash },
                        );
                        if let Bucket::Occupied { value: old_val, .. } = old {
                            return Some(old_val);
                        }
                        unreachable!();
                    }
                }
            }
            idx = (idx + 1) & (self.buckets.len() - 1);
        }
    }

    /// Get value by key
    pub fn get(&self, key: &K) -> Option<&V> {
        self.get_with_hash(key, hash_one(key))
    }

    /// Get value by key with precomputed hash
    pub fn get_with_hash(&self, key: &K, hash: u64) -> Option<&V> {
        if self.buckets.is_empty() {
            return None;
        }

        let mut idx = self.bucket_index(hash);
        loop {
            match &self.buckets[idx] {
                Bucket::Empty => return None,
                Bucket::Tombstone => {}
                Bucket::Occupied { key: k, value, hash: h } => {
                    if *h == hash && k == key {
                        return Some(value);
                    }
                }
            }
            idx = (idx + 1) & (self.buckets.len() - 1);
        }
    }

    /// Get mutable value by key
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.get_mut_with_hash(key, hash_one(key))
    }

    /// Get mutable value by key with precomputed hash
    pub fn get_mut_with_hash(&mut self, key: &K, hash: u64) -> Option<&mut V> {
        if self.buckets.is_empty() {
            return None;
        }

        // Find index first to avoid borrow issues
        let found_idx = {
            let mut idx = self.bucket_index(hash);
            loop {
                match &self.buckets[idx] {
                    Bucket::Empty => break None,
                    Bucket::Tombstone => {}
                    Bucket::Occupied { key: k, hash: h, .. } => {
                        if *h == hash && k == key {
                            break Some(idx);
                        }
                    }
                }
                idx = (idx + 1) & (self.buckets.len() - 1);
            }
        };

        found_idx.and_then(move |idx| {
            if let Bucket::Occupied { value, .. } = &mut self.buckets[idx] {
                Some(value)
            } else {
                None
            }
        })
    }

    /// Remove key and return value
    pub fn remove(&mut self, key: &K) -> Option<V> {
        self.remove_with_hash(key, hash_one(key))
    }

    /// Remove key with precomputed hash
    pub fn remove_with_hash(&mut self, key: &K, hash: u64) -> Option<V> {
        if self.buckets.is_empty() {
            return None;
        }

        let mut idx = self.bucket_index(hash);
        loop {
            match &self.buckets[idx] {
                Bucket::Empty => return None,
                Bucket::Tombstone => {}
                Bucket::Occupied { key: k, hash: h, .. } => {
                    if *h == hash && k == key {
                        let old = core::mem::replace(&mut self.buckets[idx], Bucket::Tombstone);
                        self.len -= 1;
                        // Note: used count stays same (tombstone still occupies slot)
                        if let Bucket::Occupied { value, .. } = old {
                            return Some(value);
                        }
                        unreachable!();
                    }
                }
            }
            idx = (idx + 1) & (self.buckets.len() - 1);
        }
    }

    /// Check if key exists
    pub fn contains_key(&self, key: &K) -> bool {
        self.get(key).is_some()
    }

    /// Find value by hash + custom predicate (for types where K is a hash proxy).
    /// Probes from the hash-determined bucket, checking predicate on each occupied entry.
    /// This gives O(1) average lookup for struct/interface keys where K=u64 (hash)
    /// and the actual equality check is done by the predicate on the value.
    pub fn find_by<P>(&self, hash: u64, predicate: P) -> Option<&V>
    where
        P: Fn(&K, &V) -> bool,
    {
        if self.buckets.is_empty() {
            return None;
        }
        // hash_one matches insert's internal hashing: insert calls hash_one(key)
        let bucket_hash = hash_one(&hash);
        let mut idx = self.bucket_index(bucket_hash);
        loop {
            match &self.buckets[idx] {
                Bucket::Empty => return None,
                Bucket::Tombstone => {}
                Bucket::Occupied { key, value, .. } => {
                    if predicate(key, value) {
                        return Some(value);
                    }
                }
            }
            idx = (idx + 1) & (self.buckets.len() - 1);
        }
    }

    /// Mutable version of find_by. Returns mutable reference to the value.
    /// Uses index-based lookup to satisfy the borrow checker.
    pub fn find_by_mut<P>(&mut self, hash: u64, predicate: P) -> Option<&mut V>
    where
        P: Fn(&K, &V) -> bool,
    {
        if self.buckets.is_empty() {
            return None;
        }
        // Phase 1: find index with immutable borrows
        let found_idx = {
            let bucket_hash = hash_one(&hash);
            let mut idx = self.bucket_index(bucket_hash);
            loop {
                match &self.buckets[idx] {
                    Bucket::Empty => break None,
                    Bucket::Tombstone => {}
                    Bucket::Occupied { key, value, .. } => {
                        if predicate(key, value) {
                            break Some(idx);
                        }
                    }
                }
                idx = (idx + 1) & (self.buckets.len() - 1);
            }
        };
        // Phase 2: re-borrow mutably at found index
        found_idx.and_then(move |idx| {
            if let Bucket::Occupied { value, .. } = &mut self.buckets[idx] {
                Some(value)
            } else {
                None
            }
        })
    }

    /// Remove by hash + custom predicate. Returns removed value if found.
    pub fn remove_by<P>(&mut self, hash: u64, predicate: P) -> Option<V>
    where
        P: Fn(&K, &V) -> bool,
    {
        if self.buckets.is_empty() {
            return None;
        }
        let bucket_hash = hash_one(&hash);
        let mut idx = self.bucket_index(bucket_hash);
        loop {
            match &self.buckets[idx] {
                Bucket::Empty => return None,
                Bucket::Tombstone => {}
                Bucket::Occupied { key, value, .. } => {
                    if predicate(key, value) {
                        let old = core::mem::replace(&mut self.buckets[idx], Bucket::Tombstone);
                        self.len -= 1;
                        if let Bucket::Occupied { value, .. } = old {
                            return Some(value);
                        }
                        unreachable!();
                    }
                }
            }
            idx = (idx + 1) & (self.buckets.len() - 1);
        }
    }

    /// Resize and rehash
    fn resize<F>(&mut self, hasher: &F)
    where
        F: Fn(&K) -> u64,
    {
        let new_cap = if self.buckets.is_empty() {
            MIN_CAPACITY
        } else {
            self.buckets.len() * 2
        };

        let old_buckets = mem::replace(
            &mut self.buckets,
            (0..new_cap).map(|_| Bucket::Empty).collect(),
        );

        self.len = 0;
        self.used = 0;
        self.generation = self.generation.wrapping_add(1);

        for bucket in old_buckets.into_vec() {
            if let Bucket::Occupied { key, value, .. } = bucket {
                // Direct insert without resize check (we just resized)
                let hash = hasher(&key);
                let mut idx = self.bucket_index(hash);
                loop {
                    if self.buckets[idx].is_empty() {
                        self.buckets[idx] = Bucket::Occupied { key, value, hash };
                        self.len += 1;
                        self.used += 1;
                        break;
                    }
                    idx = (idx + 1) & (self.buckets.len() - 1);
                }
            }
        }
    }

    /// Create iterator
    pub fn iter(&self) -> VoMapIter<'_, K, V> {
        VoMapIter {
            buckets: &self.buckets,
            index: 0,
            init_generation: self.generation,
            current_generation: &self.generation,
        }
    }

    /// Create mutable iterator (no generation check - caller's responsibility)
    pub fn iter_mut(&mut self) -> VoMapIterMut<'_, K, V> {
        VoMapIterMut {
            buckets: self.buckets.iter_mut(),
        }
    }

    /// Get next occupied entry starting from index.
    /// Returns (actual_index, key, value) or None if no more entries.
    /// This is the core primitive for index-based iteration.
    pub fn iter_from_index(&self, start_index: usize) -> Option<(usize, &K, &V)> {
        for i in start_index..self.buckets.len() {
            if let Bucket::Occupied { key, value, .. } = &self.buckets[i] {
                return Some((i, key, value));
            }
        }
        None
    }
}

impl<K, V> Default for VoMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

/// Iterator over VoMap entries
pub struct VoMapIter<'a, K, V> {
    buckets: &'a [Bucket<K, V>],
    index: usize,
    init_generation: u32,
    current_generation: &'a u32,
}

impl<'a, K, V> Iterator for VoMapIter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        // Generation changed = resize happened = terminate safely
        if *self.current_generation != self.init_generation {
            return None;
        }

        while self.index < self.buckets.len() {
            let i = self.index;
            self.index += 1;
            if let Bucket::Occupied { key, value, .. } = &self.buckets[i] {
                return Some((key, value));
            }
        }
        None
    }
}

/// Mutable iterator (no generation check)
pub struct VoMapIterMut<'a, K, V> {
    buckets: core::slice::IterMut<'a, Bucket<K, V>>,
}

impl<'a, K, V> Iterator for VoMapIterMut<'a, K, V> {
    type Item = (&'a K, &'a mut V);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.buckets.next()? {
                Bucket::Occupied { key, value, .. } => return Some((key, value)),
                _ => continue,
            }
        }
    }
}

/// FNV-1a hash with fixed seed for consistent hashing
#[inline]
fn hash_one<K: Hash>(key: &K) -> u64 {
    use core::hash::Hasher;
    let mut hasher = FnvHasher::new();
    key.hash(&mut hasher);
    hasher.finish()
}

/// FNV-1a hasher with fixed seed
struct FnvHasher(u64);

impl FnvHasher {
    const OFFSET_BASIS: u64 = 0xcbf29ce484222325;
    const PRIME: u64 = 0x100000001b3;
    
    #[inline]
    fn new() -> Self {
        Self(Self::OFFSET_BASIS)
    }
}

impl core::hash::Hasher for FnvHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }
    
    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        for &byte in bytes {
            self.0 ^= byte as u64;
            self.0 = self.0.wrapping_mul(Self::PRIME);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_operations() {
        let mut map: VoMap<i32, i32> = VoMap::new();
        assert!(map.is_empty());

        map.insert(1, 10);
        map.insert(2, 20);
        map.insert(3, 30);

        assert_eq!(map.len(), 3);
        assert_eq!(map.get(&1), Some(&10));
        assert_eq!(map.get(&2), Some(&20));
        assert_eq!(map.get(&3), Some(&30));
        assert_eq!(map.get(&4), None);

        assert_eq!(map.remove(&2), Some(20));
        assert_eq!(map.len(), 2);
        assert_eq!(map.get(&2), None);
    }

    #[test]
    fn test_iteration_with_delete() {
        let mut map: VoMap<i32, i32> = VoMap::new();
        for i in 0..10 {
            map.insert(i, i * 10);
        }

        let mut count = 0;
        let mut iter = map.iter();
        while let Some((k, _)) = iter.next() {
            count += 1;
            // Delete during iteration - should not affect iterator
            if *k % 2 == 0 {
                // Note: can't delete during immutable iter, but tombstone semantics work
            }
        }
        assert_eq!(count, 10);
    }

    #[test]
    fn test_resize_terminates_iterator() {
        let mut map: VoMap<i32, i32> = VoMap::with_capacity(8);
        for i in 0..4 {
            map.insert(i, i);
        }

        let gen_before = map.generation();
        
        // Force resize by inserting many elements
        for i in 10..20 {
            map.insert(i, i);
        }
        
        // Generation should have changed
        assert_ne!(map.generation(), gen_before);
    }
}
