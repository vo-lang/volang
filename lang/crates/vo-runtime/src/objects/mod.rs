//! Heap object operations.

/// Error codes for allocation operations (shared by slice/chan/port).
pub mod alloc_error {
    pub const OK: i32 = 0;
    pub const NEGATIVE_LEN: i32 = 1;
    pub const NEGATIVE_CAP: i32 = 2;
    pub const LEN_GT_CAP: i32 = 3;
    pub const OVERFLOW: i32 = 4;
}


/// Macro to implement as_ref/as_mut for GC object header types.
macro_rules! impl_gc_object {
    ($name:ident) => {
        impl $name {
            #[inline]
            pub fn as_ref(p: GcRef) -> &'static Self {
                unsafe { &*(p as *const Self) }
            }
            #[inline]
            pub fn as_mut(p: GcRef) -> &'static mut Self {
                unsafe { &mut *(p as *mut Self) }
            }
        }
    };
}

#[allow(unused_imports)]
pub(crate) use impl_gc_object;

pub mod array;
pub mod channel;
pub mod queue_state;
pub mod closure;
pub mod compare;
pub mod interface;
pub mod map;
pub mod port;
pub mod slice;
pub mod string;
pub mod struct_ops;
pub mod vo_map;
