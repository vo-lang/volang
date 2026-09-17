//! Read-only sources for copy and spread append. Strings and mutable slices
//! share sequence semantics while retaining independent descriptor layouts.

use crate::ffi::ExternCallContext;
use crate::gc::{Gc, GcRef};
use crate::objects::{slice, string};
use crate::{ValueKind, ValueMeta};
#[cfg(not(feature = "std"))]
use alloc::vec;

#[derive(Clone, Copy)]
pub(super) enum SequenceSource {
    Slice(GcRef),
    String(GcRef),
}

impl SequenceSource {
    /// The non-null source must remain a rooted, live slice or string for all
    /// uses of this accessor, including intervening descriptor allocations.
    pub(super) unsafe fn new(source: GcRef) -> Self {
        match unsafe { Gc::header(source) }.kind() {
            ValueKind::Slice => Self::Slice(source),
            ValueKind::String => Self::String(source),
            kind => panic!("verified sequence source has unexpected kind {kind:?}"),
        }
    }

    pub(super) unsafe fn len(self) -> usize {
        match self {
            Self::Slice(source) => unsafe { slice::len(source) },
            Self::String(source) => unsafe { string::len(source) },
        }
    }

    pub(super) unsafe fn element_layout(self) -> (ValueMeta, usize) {
        match self {
            Self::Slice(source) => unsafe { (slice::elem_meta(source), slice::elem_bytes(source)) },
            Self::String(_) => (ValueMeta::new(0, ValueKind::Uint8), 1),
        }
    }

    pub(super) unsafe fn barrier_to(
        self,
        call: &mut ExternCallContext,
        owner: GcRef,
        count: usize,
        meta: ValueMeta,
    ) {
        if let Self::Slice(source) = self {
            if meta.value_kind().may_contain_gc_refs() {
                let mut value = vec![0; unsafe { slice::logical_elem_slots(source) }];
                for index in 0..count {
                    unsafe { slice::read_logical_slots(source, index, &mut value) };
                    call.typed_write_barrier_by_meta(owner, &value, meta);
                }
            }
        }
    }

    pub(super) unsafe fn copy_to(self, dst: GcRef, dst_start: usize, count: usize) {
        match self {
            Self::Slice(source) => unsafe {
                slice::copy_logical_elements_at(dst, dst_start, source, 0, count)
            },
            Self::String(source) => unsafe {
                slice::write_bytes_at(dst, dst_start, &string::bytes_unchecked(source)[..count])
            },
        }
    }
}
