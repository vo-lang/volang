//! Built-in functions (len, cap) implementation.

use crate::gc::GcRef;
use crate::objects::{string, array, slice, map};

/// Type tags for len/cap
pub const TYPE_STRING: u8 = 1;
pub const TYPE_ARRAY: u8 = 2;
pub const TYPE_SLICE: u8 = 3;
pub const TYPE_MAP: u8 = 4;

/// Implementation of len() built-in.
pub unsafe fn len_impl(type_tag: u8, ptr: GcRef) -> usize {
    match type_tag {
        TYPE_STRING => string::len(ptr),
        TYPE_ARRAY => array::len(ptr),
        TYPE_SLICE => slice::len(ptr),
        TYPE_MAP => map::len(ptr),
        _ => 0,
    }
}

/// Implementation of cap() built-in.
pub unsafe fn cap_impl(type_tag: u8, ptr: GcRef) -> usize {
    match type_tag {
        TYPE_SLICE => slice::cap(ptr),
        _ => len_impl(type_tag, ptr), // For non-slices, cap == len
    }
}
