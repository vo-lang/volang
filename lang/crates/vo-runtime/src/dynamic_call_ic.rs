//! Interpreter/JIT-shared monomorphic interface-call cache.

use alloc::vec::Vec;

/// One cache entry per verified dynamic callsite.
///
/// The interpreter owns cache population. Native code reads the same stable
/// C layout when JIT support is enabled.
#[derive(Debug)]
#[repr(C)]
pub struct DynCallIC {
    pub receiver_slot0: u64,
    pub jit_func_ptr: u64,
    pub local_slots: u32,
    pub func_id: u32,
    /// Generation of the dispatch entry that supplied `jit_func_ptr`.
    pub dispatch_generation: u64,
    pub gc_scan_slots: u16,
    pub valid: u16,
    /// Whether the cached native target can reach a managed-heap safepoint.
    pub jit_may_gc: u16,
    pub reserved: u16,
}

impl Default for DynCallIC {
    fn default() -> Self {
        // The all-zero representation is the invalid cache state.
        unsafe { core::mem::zeroed() }
    }
}

impl DynCallIC {
    pub const SIZE: usize = core::mem::size_of::<Self>();
    pub const OFFSET_RECEIVER_SLOT0: i32 = core::mem::offset_of!(Self, receiver_slot0) as i32;
    pub const OFFSET_JIT_FUNC_PTR: i32 = core::mem::offset_of!(Self, jit_func_ptr) as i32;
    pub const OFFSET_LOCAL_SLOTS: i32 = core::mem::offset_of!(Self, local_slots) as i32;
    pub const OFFSET_FUNC_ID: i32 = core::mem::offset_of!(Self, func_id) as i32;
    pub const OFFSET_DISPATCH_GENERATION: i32 =
        core::mem::offset_of!(Self, dispatch_generation) as i32;
    pub const OFFSET_GC_SCAN_SLOTS: i32 = core::mem::offset_of!(Self, gc_scan_slots) as i32;
    pub const OFFSET_VALID: i32 = core::mem::offset_of!(Self, valid) as i32;
    pub const OFFSET_JIT_MAY_GC: i32 = core::mem::offset_of!(Self, jit_may_gc) as i32;
}

const _: () = assert!(DynCallIC::SIZE == 40);

pub fn alloc_ic_table(len: usize) -> Vec<DynCallIC> {
    let mut table = Vec::with_capacity(len);
    unsafe {
        core::ptr::write_bytes(table.as_mut_ptr(), 0, len);
        table.set_len(len);
    }
    table
}
