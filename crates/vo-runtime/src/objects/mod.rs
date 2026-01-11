//! Heap object operations.


/// Macro to implement as_ref/as_mut for GC object header types.
macro_rules! impl_gc_object {
    ($name:ident) => {
        impl $name {
            #[inline]
            fn as_ref(p: GcRef) -> &'static Self {
                unsafe { &*(p as *const Self) }
            }
            #[inline]
            fn as_mut(p: GcRef) -> &'static mut Self {
                unsafe { &mut *(p as *mut Self) }
            }
        }
    };
}

pub(crate) use impl_gc_object;

pub mod array;
pub mod channel;
pub mod closure;
pub mod compare;
pub mod interface;
pub mod map;
pub mod slice;
pub mod string;
pub mod struct_ops;
