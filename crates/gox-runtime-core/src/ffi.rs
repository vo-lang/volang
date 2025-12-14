//! FFI type definitions for cross-backend interoperability.

/// A GoX string in FFI context.
#[repr(C)]
pub struct GoxString {
    pub ptr: *const u8,
    pub len: usize,
}

/// A GoX slice in FFI context.
#[repr(C)]
pub struct GoxSlice {
    pub ptr: *mut u8,
    pub len: usize,
    pub cap: usize,
}

/// A GoX bytes slice in FFI context.
#[repr(C)]
pub struct GoxBytes {
    pub ptr: *mut u8,
    pub len: usize,
    pub cap: usize,
}
