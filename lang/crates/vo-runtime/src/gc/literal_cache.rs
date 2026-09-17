//! Bounded, non-rooting literal reuse owned by one collector and module scope.
//!
//! Entries may contain reclaimed addresses. The collector checks an idle,
//! unsaturated completed-cycle epoch before an entry can be returned. Nothing
//! traverses or dereferences cached objects during collection or invalidation.
use super::{Gc, GcRef, GcState, MemoryError};
#[cfg(not(feature = "std"))]
use alloc::{
    sync::{Arc, Weak},
    vec::Vec,
};
#[cfg(feature = "std")]
use std::{
    sync::{Arc, Weak},
    vec::Vec,
};
use vo_common_core::bytecode::LoadedModule;

const CAPACITY: usize = 256;

#[derive(Clone, Copy, PartialEq, Eq)]
struct Epoch {
    minor: u64,
    major: u64,
}

#[derive(Clone, Copy)]
struct Entry {
    epoch: Epoch,
    value: GcRef,
    constant: u32,
}
impl Entry {
    const EMPTY: Self = Self {
        epoch: Epoch { minor: 0, major: 0 },
        value: core::ptr::null_mut(),
        constant: 0,
    };
}

pub(super) struct LiteralCache {
    // A Weak keeps the Arc control block's address unavailable for reuse
    // without keeping the module payload or any managed object alive.
    scope: Weak<LoadedModule>,
    entries: Vec<Entry>,
    admission_attempted: bool,
}

impl LiteralCache {
    pub(super) fn new() -> Self {
        Self {
            scope: Weak::new(),
            entries: Vec::new(),
            admission_attempted: false,
        }
    }

    fn bind(&mut self, module: &Arc<LoadedModule>) {
        // Entry is Copy with no destructor: clearing invalidates in O(1).
        // Reset even for equal GC epochs or repeated constant IDs.
        self.entries.clear();
        self.scope = Arc::downgrade(module);
        self.admission_attempted = false;
    }

    #[inline]
    fn scope_matches(&self, module: &LoadedModule) -> bool {
        core::ptr::eq(self.scope.as_ptr(), module)
    }

    #[inline]
    fn get(&self, constant: u32, epoch: Epoch) -> Option<GcRef> {
        let entry = self.entries.get(constant as usize & (CAPACITY - 1))?;
        (entry.constant == constant && entry.epoch == epoch && !entry.value.is_null())
            .then_some(entry.value)
    }

    fn admit_with(
        &mut self,
        allow_growth: bool,
        reserve: impl FnOnce(&mut Vec<Entry>) -> bool,
    ) -> bool {
        if !self.entries.is_empty() {
            return true;
        }
        if self.entries.capacity() < CAPACITY {
            if !allow_growth || self.admission_attempted {
                return false;
            }
            self.admission_attempted = true;
            if !reserve(&mut self.entries) {
                return false;
            }
            debug_assert!(self.entries.capacity() >= CAPACITY);
        }
        self.entries.resize(CAPACITY, Entry::EMPTY);
        true
    }

    fn publish(&mut self, constant: u32, epoch: Epoch, value: GcRef, allow_growth: bool) {
        if value.is_null() {
            return;
        }
        // Optional host metadata never steps GC or changes guest OOM state.
        // Avoid new cache allocation after entering no-growth mode; already
        // admitted storage remains usable across a module scope replacement.
        if !self.admit_with(allow_growth, |entries| {
            entries.try_reserve_exact(CAPACITY).is_ok()
        }) {
            return;
        }
        self.entries[constant as usize & (CAPACITY - 1)] = Entry {
            epoch,
            value,
            constant,
        };
    }
}

impl Gc {
    /// Bind optional literal reuse to a successfully installed immutable image.
    /// Invalidates old entries without adding roots or GC-step work. Existing
    /// bounded cache storage may be reused; an extension facade cannot own it.
    pub fn bind_literal_module(&mut self, module: &Arc<LoadedModule>) {
        self.reject_owner_proxy_api("bind_literal_module");
        self.literal_cache.bind(module);
    }

    /// Host cache backing bytes, separate from managed-heap and external bytes.
    pub fn literal_cache_metadata_bytes(&self) -> usize {
        self.reject_owner_proxy_api("literal_cache_metadata_bytes");
        self.literal_cache.entries.capacity() * core::mem::size_of::<Entry>()
    }

    #[inline]
    fn literal_epoch(&self, module: &LoadedModule) -> Option<Epoch> {
        if self.owner_dispatch.is_some()
            || self.state != GcState::Pause
            || self.minor_cycles == u64::MAX
            || self.major_cycles == u64::MAX
            || !self.literal_cache.scope_matches(module)
        {
            return None;
        }
        Some(Epoch {
            minor: self.minor_cycles,
            major: self.major_cycles,
        })
    }

    #[inline]
    pub(crate) fn cached_literal(
        &self,
        module: &LoadedModule,
        constant: u32,
    ) -> Result<Option<GcRef>, MemoryError> {
        // Facades fall back to normal host-dispatched allocation. They must
        // neither inspect host-owned cache state nor retain module Weak refs.
        if self.owner_dispatch.is_some() {
            return Ok(None);
        }
        if let Some(error) = self.last_memory_error {
            return Err(error);
        }
        Ok(self
            .literal_epoch(module)
            .and_then(|epoch| self.literal_cache.get(constant, epoch)))
    }

    pub(crate) fn remember_literal(&mut self, module: &LoadedModule, constant: u32, value: GcRef) {
        if self.last_memory_error.is_some() {
            return;
        }
        let Some(epoch) = self.literal_epoch(module) else {
            return;
        };
        let allow_growth = self.heap.growth_allowed();
        self.literal_cache
            .publish(constant, epoch, value, allow_growth);
    }
}

#[cfg(test)]
mod tests;
