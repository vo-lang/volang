//! Heap object operations.

/// Error codes for allocation operations (shared by slice/chan).
pub mod alloc_error {
    pub const OK: i32 = 0;
    pub const NEGATIVE_LEN: i32 = 1;
    pub const NEGATIVE_CAP: i32 = 2;
    pub const LEN_GT_CAP: i32 = 3;
    pub const OVERFLOW: i32 = 4;
}

/// Macro to implement raw as_ref/as_mut access for GC object header types.
macro_rules! impl_gc_object {
    ($name:ident) => {
        impl $name {
            #[inline]
            /// # Safety
            /// `p` must point to a live object of this exact header type for the
            /// returned borrow. The object must not be finalized or mutably
            /// aliased while the borrow is used.
            pub unsafe fn as_ref<'a>(p: GcRef) -> &'a Self {
                unsafe { &*(p as *const Self) }
            }
            #[inline]
            /// # Safety
            /// `p` must be a valid unique reference to a live object header. If the
            /// mutation publishes GC-visible references, the caller must apply the
            /// required write barrier first or be initializing a freshly allocated
            /// object that will be marked for scanning before collection.
            pub unsafe fn as_mut<'a>(p: GcRef) -> &'a mut Self {
                unsafe { &mut *(p as *mut Self) }
            }
        }
    };
}

#[allow(unused_imports)]
pub(crate) use impl_gc_object;

pub mod array;
pub mod queue;
pub use self::queue as channel;
pub mod closure;
pub mod compare;
pub mod interface;
pub mod map;
pub mod queue_state;
pub mod slice;
pub mod string;
pub mod struct_ops;
