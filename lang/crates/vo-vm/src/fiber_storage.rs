//! Fallible, capacity-accounted storage for guest-driven Fiber metadata.

#[cfg(not(feature = "std"))]
use alloc::{sync::Arc, vec::Vec};
#[cfg(feature = "std")]
use std::sync::Arc;

use crate::fiber::{FiberCapacityError, FiberStorageBudget};

#[derive(Debug)]
pub struct AuxiliaryVec<T> {
    values: Vec<T>,
    budget: Arc<FiberStorageBudget>,
    charged: usize,
}

impl<T> AuxiliaryVec<T> {
    pub(crate) fn new(budget: Arc<FiberStorageBudget>) -> Self {
        Self {
            values: Vec::new(),
            budget,
            charged: 0,
        }
    }

    pub(crate) fn empty_like(&self) -> Self {
        Self::new(Arc::clone(&self.budget))
    }

    pub fn capacity(&self) -> usize {
        self.values.capacity()
    }

    pub fn try_reserve(&mut self, additional: usize) -> Result<(), FiberCapacityError> {
        let required = self
            .values
            .len()
            .checked_add(additional)
            .ok_or_else(|| self.limit_error(usize::MAX))?;
        if required <= self.capacity() {
            return Ok(());
        }
        let capacity = required.max(self.capacity().saturating_mul(2)).max(4);
        self.reserve_capacity(capacity)
    }

    pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), FiberCapacityError> {
        let capacity = self
            .values
            .len()
            .checked_add(additional)
            .ok_or_else(|| self.limit_error(usize::MAX))?;
        if capacity <= self.capacity() {
            return Ok(());
        }
        self.reserve_capacity(capacity)
    }

    fn limit_error(&self, requested: usize) -> FiberCapacityError {
        FiberCapacityError::HostStorage {
            resource: "fiber auxiliary storage",
            requested_bytes: requested,
            limit_bytes: self.budget.auxiliary_limit_bytes(),
        }
    }

    fn reserve_capacity(&mut self, capacity: usize) -> Result<(), FiberCapacityError> {
        let size = core::mem::size_of::<T>();
        let bytes = (capacity - self.capacity())
            .checked_mul(size)
            .ok_or_else(|| self.limit_error(usize::MAX))?;
        if !self.budget.try_charge_auxiliary(bytes) {
            return Err(self.limit_error(self.budget.auxiliary_used_bytes().saturating_add(bytes)));
        }
        if self
            .values
            .try_reserve_exact(capacity - self.values.len())
            .is_err()
        {
            self.budget.release_auxiliary(bytes);
            return Err(FiberCapacityError::HostAllocation {
                resource: "fiber auxiliary storage",
            });
        }
        self.charged += bytes;
        debug_assert_eq!(self.charged, self.capacity() * size);
        Ok(())
    }

    /// Commit after a successful reservation. Never grows the allocation.
    pub(crate) fn push_reserved(&mut self, value: T) {
        assert!(
            self.values.len() < self.capacity(),
            "auxiliary push requires admission"
        );
        self.values.push(value);
    }

    pub fn try_push(&mut self, value: T) -> Result<(), FiberCapacityError> {
        self.try_reserve(1)?;
        self.push_reserved(value);
        Ok(())
    }

    pub fn pop(&mut self) -> Option<T> {
        self.values.pop()
    }
    pub fn clear(&mut self) {
        self.values.clear();
    }
    pub fn truncate(&mut self, len: usize) {
        self.values.truncate(len);
    }
    pub fn retain(&mut self, predicate: impl FnMut(&T) -> bool) {
        self.values.retain(predicate);
    }
    pub(crate) fn dedup_by_key<K: PartialEq>(&mut self, key: impl FnMut(&mut T) -> K) {
        self.values.dedup_by_key(key);
    }

    pub fn drain(&mut self, range: impl core::ops::RangeBounds<usize>) -> alloc::vec::Drain<'_, T> {
        self.values.drain(range)
    }

    pub(crate) fn charge_payload(
        &self,
        bytes: usize,
    ) -> Result<AuxiliaryCharge, FiberCapacityError> {
        if !self.budget.try_charge_auxiliary(bytes) {
            return Err(self.limit_error(self.budget.auxiliary_used_bytes().saturating_add(bytes)));
        }
        Ok(AuxiliaryCharge {
            budget: Arc::clone(&self.budget),
            bytes,
        })
    }

    pub fn resize_reserved(&mut self, len: usize, value: T)
    where
        T: Clone,
    {
        assert!(
            len <= self.capacity(),
            "auxiliary resize requires admission"
        );
        self.values.resize(len, value);
    }

    pub fn try_extend_from_slice(&mut self, values: &[T]) -> Result<(), FiberCapacityError>
    where
        T: Clone,
    {
        self.try_reserve(values.len())?;
        self.values.extend_from_slice(values);
        Ok(())
    }

    #[cfg(test)]
    pub fn extend(&mut self, values: impl IntoIterator<Item = T>) {
        for value in values {
            self.push(value);
        }
    }

    #[cfg(test)]
    pub fn push(&mut self, value: T) {
        self.try_push(value).expect("test auxiliary allocation");
    }
}

impl<T> core::ops::Deref for AuxiliaryVec<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        &self.values
    }
}
impl<T> core::ops::DerefMut for AuxiliaryVec<T> {
    fn deref_mut(&mut self) -> &mut [T] {
        &mut self.values
    }
}
impl<T> Drop for AuxiliaryVec<T> {
    fn drop(&mut self) {
        self.budget.release_auxiliary(self.charged);
    }
}
impl<T> Default for AuxiliaryVec<T> {
    fn default() -> Self {
        Self::new(Arc::new(FiberStorageBudget::new(0)))
    }
}

extern crate alloc;

#[cfg(test)]
impl<T: Clone> Clone for AuxiliaryVec<T> {
    fn clone(&self) -> Self {
        let mut result = self.empty_like();
        result
            .try_extend_from_slice(self)
            .expect("test auxiliary clone");
        result
    }
}
#[cfg(test)]
impl<T> From<Vec<T>> for AuxiliaryVec<T> {
    fn from(values: Vec<T>) -> Self {
        let mut result = Self::default();
        result
            .try_reserve_exact(values.len())
            .expect("test auxiliary conversion");
        for value in values {
            result.push_reserved(value);
        }
        result
    }
}
impl<'a, T> IntoIterator for &'a AuxiliaryVec<T> {
    type Item = &'a T;
    type IntoIter = core::slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.values.iter()
    }
}

/// Owns the budget for nested payloads whose allocation is transferred between
/// runtime states. Dropping the last owning state releases its accounting.
#[derive(Debug)]
pub struct AuxiliaryCharge {
    budget: Arc<FiberStorageBudget>,
    bytes: usize,
}
impl Drop for AuxiliaryCharge {
    fn drop(&mut self) {
        self.budget.release_auxiliary(self.bytes);
    }
}
#[cfg(test)]
impl Clone for AuxiliaryCharge {
    fn clone(&self) -> Self {
        assert!(self.budget.try_charge_auxiliary(self.bytes));
        Self {
            budget: Arc::clone(&self.budget),
            bytes: self.bytes,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn auxiliary_capacity_is_shared_fallible_and_refunded_on_drop() {
        let budget = Arc::new(FiberStorageBudget::with_limits(1024, 64));
        let mut first = AuxiliaryVec::<u64>::new(Arc::clone(&budget));
        first.try_reserve_exact(5).unwrap();
        assert_eq!(budget.auxiliary_used_bytes(), 40);
        first.try_reserve_exact(5).unwrap();
        assert_eq!(budget.auxiliary_used_bytes(), 40);
        let mut second = first.empty_like();
        assert!(matches!(
            second.try_reserve_exact(4),
            Err(FiberCapacityError::HostStorage { .. })
        ));
        assert_eq!(second.capacity(), 0);
        first.clear();
        assert_eq!(
            budget.auxiliary_used_bytes(),
            40,
            "retained capacity remains charged"
        );
        drop(first);
        second.try_reserve_exact(8).unwrap();
        assert_eq!(budget.auxiliary_used_bytes(), 64);
        drop(second);
        assert_eq!(budget.auxiliary_used_bytes(), 0);
    }

    #[test]
    fn transferring_a_buffer_preserves_its_charge_without_allocating() {
        let budget = Arc::new(FiberStorageBudget::with_limits(0, 64));
        let mut buffer = AuxiliaryVec::<u64>::new(Arc::clone(&budget));
        buffer.try_push(42).unwrap();
        let pointer = buffer.as_ptr();
        let empty = buffer.empty_like();
        let moved = core::mem::replace(&mut buffer, empty);
        assert_eq!(moved.as_ptr(), pointer);
        assert_eq!(budget.auxiliary_used_bytes(), 32);
        drop(moved);
        assert_eq!(budget.auxiliary_used_bytes(), 0);
    }
}

/// Select descriptions and published waiter lists are immutable while a
/// rollback snapshot exists. Sharing preserves both ownership and accounting.
#[derive(Debug)]
pub struct SharedAuxiliaryVec<T>(Arc<AuxiliaryVec<T>>);
impl<T> Clone for SharedAuxiliaryVec<T> {
    fn clone(&self) -> Self {
        Self(Arc::clone(&self.0))
    }
}
impl<T> From<AuxiliaryVec<T>> for SharedAuxiliaryVec<T> {
    fn from(values: AuxiliaryVec<T>) -> Self {
        Self(Arc::new(values))
    }
}
impl<T> core::ops::Deref for SharedAuxiliaryVec<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        &self.0
    }
}
impl<T> SharedAuxiliaryVec<T> {
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }
    pub(crate) fn try_reserve(&mut self, additional: usize) -> Result<(), FiberCapacityError>
    where
        T: Clone,
    {
        if let Some(values) = Arc::get_mut(&mut self.0) {
            return values.try_reserve(additional);
        }
        let mut values = self.0.empty_like();
        values.try_reserve_exact(self.len().saturating_add(additional))?;
        values.try_extend_from_slice(&self.0)?;
        self.0 = Arc::new(values);
        Ok(())
    }
    pub(crate) fn push_reserved(&mut self, value: T) {
        Arc::get_mut(&mut self.0)
            .expect("select construction owns its buffer")
            .push_reserved(value);
    }
    pub(crate) fn charge_payload(
        &self,
        bytes: usize,
    ) -> Result<AuxiliaryCharge, FiberCapacityError> {
        self.0.charge_payload(bytes)
    }
    pub(crate) fn clear(&mut self) {
        if let Some(values) = Arc::get_mut(&mut self.0) {
            values.clear();
        } else {
            self.0 = Arc::new(self.0.empty_like());
        }
    }
    pub(crate) fn unique_mut(&mut self) -> Option<&mut AuxiliaryVec<T>> {
        Arc::get_mut(&mut self.0)
    }
    #[cfg(test)]
    pub fn push(&mut self, value: T)
    where
        T: Clone,
    {
        if Arc::get_mut(&mut self.0).is_none() {
            self.0 = Arc::new((*self.0).clone());
        }
        Arc::get_mut(&mut self.0).unwrap().push(value);
    }
}
impl<T> Default for SharedAuxiliaryVec<T> {
    fn default() -> Self {
        AuxiliaryVec::default().into()
    }
}
impl<'a, T> IntoIterator for &'a SharedAuxiliaryVec<T> {
    type Item = &'a T;
    type IntoIter = core::slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
#[cfg(test)]
impl<T> From<Vec<T>> for SharedAuxiliaryVec<T> {
    fn from(values: Vec<T>) -> Self {
        AuxiliaryVec::from(values).into()
    }
}
