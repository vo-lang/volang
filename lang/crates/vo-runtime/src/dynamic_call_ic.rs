//! Interpreter/JIT-shared bounded polymorphic dynamic-call cache.

use alloc::vec::Vec;

/// One cache entry per verified dynamic callsite.
///
/// The interpreter owns cache population. Native code reads the same stable
/// C layout when JIT support is enabled.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct DynCallICEntry {
    /// Call-kind-specific dispatch identity. Interface calls use their packed
    /// receiver slot0; closure calls use their function id and capture shape.
    pub dispatch_key: u64,
    pub jit_func_ptr: u64,
    pub local_slots: u32,
    pub func_id: u32,
    /// Generation of the dispatch entry that supplied `jit_func_ptr`.
    pub dispatch_generation: u64,
    pub valid: u16,
    /// Whether the cached native target can reach a managed-heap safepoint.
    pub jit_may_gc: u16,
    /// Whether the cached native entry owns no observable VM frame transition.
    pub jit_frame_elided: u16,
}

impl Default for DynCallICEntry {
    fn default() -> Self {
        // The all-zero representation is the invalid cache state.
        unsafe { core::mem::zeroed() }
    }
}

impl DynCallICEntry {
    pub const SIZE: usize = core::mem::size_of::<Self>();
    pub const OFFSET_DISPATCH_KEY: i32 = core::mem::offset_of!(Self, dispatch_key) as i32;
    pub const OFFSET_JIT_FUNC_PTR: i32 = core::mem::offset_of!(Self, jit_func_ptr) as i32;
    pub const OFFSET_LOCAL_SLOTS: i32 = core::mem::offset_of!(Self, local_slots) as i32;
    pub const OFFSET_FUNC_ID: i32 = core::mem::offset_of!(Self, func_id) as i32;
    pub const OFFSET_DISPATCH_GENERATION: i32 =
        core::mem::offset_of!(Self, dispatch_generation) as i32;
    pub const OFFSET_VALID: i32 = core::mem::offset_of!(Self, valid) as i32;
    pub const OFFSET_JIT_MAY_GC: i32 = core::mem::offset_of!(Self, jit_may_gc) as i32;
    pub const OFFSET_JIT_FRAME_ELIDED: i32 = core::mem::offset_of!(Self, jit_frame_elided) as i32;

    /// Read an interpreter target whose first successful resolution proved the
    /// dynamic call contract for this exact key.
    #[inline]
    pub fn probe(&self, dispatch_key: u64) -> Option<DynamicCallTarget> {
        if self.valid == 0 || self.dispatch_key != dispatch_key {
            return None;
        }
        Some(DynamicCallTarget {
            func_id: self.func_id,
            local_slots: u16::try_from(self.local_slots).ok()?,
        })
    }

    /// Publish an interpreter-validated target while preserving native code
    /// attached to the same dispatch identity and function.
    #[inline]
    pub fn publish_interpreter_target(&mut self, dispatch_key: u64, target: DynamicCallTarget) {
        if self.dispatch_key != dispatch_key || self.func_id != target.func_id {
            self.jit_func_ptr = 0;
            self.dispatch_generation = 0;
            self.jit_may_gc = 0;
            self.jit_frame_elided = 0;
        }
        self.dispatch_key = dispatch_key;
        self.local_slots = u32::from(target.local_slots);
        self.func_id = target.func_id;
        self.valid = 1;
    }
}

/// Frame facts retained after a dynamic target passes full validation.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DynamicCallTarget {
    pub func_id: u32,
    pub local_slots: u16,
}

/// A small fixed cache preserves the common monomorphic first lane and
/// alternating receivers without unbounded growth. Once full, new identities
/// use ordinary resolution; existing lanes remain stable and versioned.
#[derive(Debug, Default)]
#[repr(C)]
pub struct DynCallIC {
    pub entries: [DynCallICEntry; Self::WAYS],
}

impl DynCallIC {
    pub const WAYS: usize = 4;
    pub const SIZE: usize = core::mem::size_of::<Self>();

    pub fn probe(&self, key: u64) -> Option<DynamicCallTarget> {
        self.entries.iter().find_map(|entry| entry.probe(key))
    }

    fn publication_entry(&mut self, key: u64) -> Option<&mut DynCallICEntry> {
        let index = self
            .entries
            .iter()
            .position(|entry| entry.valid != 0 && entry.dispatch_key == key)
            .or_else(|| self.entries.iter().position(|entry| entry.valid == 0))?;
        Some(&mut self.entries[index])
    }

    pub fn publish_interpreter_target(&mut self, key: u64, target: DynamicCallTarget) {
        if let Some(entry) = self.publication_entry(key) {
            entry.publish_interpreter_target(key, target);
        }
    }

    #[cfg(feature = "std")]
    pub fn publish_native_target(
        &mut self,
        key: u64,
        prepared: &crate::jit_api::PreparedCall,
    ) -> bool {
        if prepared.ic_jit_func_ptr.is_null() {
            return false;
        }
        let Some(entry) = self.publication_entry(key) else {
            return false;
        };
        *entry = DynCallICEntry {
            dispatch_key: key,
            jit_func_ptr: prepared.ic_jit_func_ptr as u64,
            local_slots: prepared.callee_local_slots,
            func_id: prepared.func_id,
            dispatch_generation: prepared.dispatch_generation,
            valid: 1,
            jit_may_gc: prepared.jit_may_gc,
            jit_frame_elided: prepared.jit_frame_elided,
        };
        true
    }
}

const _: () = assert!(DynCallICEntry::SIZE == 40);
const _: () = assert!(DynCallIC::SIZE == DynCallICEntry::SIZE * DynCallIC::WAYS);

pub fn alloc_ic_table(len: usize) -> Vec<DynCallIC> {
    let mut table = Vec::with_capacity(len);
    unsafe {
        core::ptr::write_bytes(table.as_mut_ptr(), 0, len);
        table.set_len(len);
    }
    table
}

#[cfg(test)]
mod tests {
    use super::{DynCallIC, DynCallICEntry, DynamicCallTarget};

    #[test]
    fn bounded_polymorphic_cache_preserves_hot_identities_when_full() {
        let mut cache = DynCallIC::default();
        for i in 0..8 {
            cache.publish_interpreter_target(
                i,
                DynamicCallTarget {
                    func_id: i as u32,
                    local_slots: 3,
                },
            );
        }
        for i in 0..4 {
            assert_eq!(cache.probe(i).unwrap().func_id, i as u32);
        }
        assert_eq!(cache.probe(4), None);
        assert_eq!(cache.entries.len(), 4);
    }

    #[test]
    fn interpreter_publication_preserves_native_target_only_for_same_dispatch() {
        let mut entry = DynCallICEntry {
            jit_func_ptr: 0x1234,
            dispatch_generation: 9,
            jit_frame_elided: 1,
            ..Default::default()
        };
        let first = DynamicCallTarget {
            func_id: 7,
            local_slots: 12,
        };

        entry.publish_interpreter_target(0xaaaa, first);
        assert_eq!(entry.jit_func_ptr, 0);
        assert_eq!(entry.dispatch_generation, 0);
        assert_eq!(entry.jit_frame_elided, 0);
        assert_eq!(entry.probe(0xaaaa), Some(first));

        entry.jit_func_ptr = 0x5678;
        entry.dispatch_generation = 11;
        entry.jit_frame_elided = 1;
        entry.valid = 0;
        entry.publish_interpreter_target(0xaaaa, first);
        assert_eq!(entry.jit_func_ptr, 0x5678);
        assert_eq!(entry.dispatch_generation, 11);
        assert_eq!(entry.jit_frame_elided, 1);

        let second = DynamicCallTarget {
            func_id: 8,
            local_slots: 9,
        };
        entry.publish_interpreter_target(0xbbbb, second);
        assert_eq!(entry.jit_func_ptr, 0);
        assert_eq!(entry.dispatch_generation, 0);
        assert_eq!(entry.jit_frame_elided, 0);
        assert_eq!(entry.probe(0xaaaa), None);
        assert_eq!(entry.probe(0xbbbb), Some(second));
    }
}
