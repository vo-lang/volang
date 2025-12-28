//! JIT runtime API.
//!
//! This module defines the C ABI interface between JIT-compiled code and the
//! Vo runtime. All functions here are `extern "C"` and can be called from
//! JIT-generated native code.
//!
//! # Architecture
//!
//! ```text
//! JIT Code                    Runtime
//! --------                    -------
//!    |                           |
//!    |-- vo_gc_alloc() --------->|  Allocate GC object
//!    |-- vo_gc_write_barrier() ->|  Write barrier for GC
//!    |-- vo_gc_safepoint() ----->|  GC safepoint check
//!    |-- vo_call_vm() ---------->|  Call VM-interpreted function
//!    |-- vo_panic() ------------>|  Trigger panic
//!    |                           |
//! ```

use std::ffi::c_void;

use crate::gc::Gc;

// =============================================================================
// JitContext
// =============================================================================

/// Runtime context passed to JIT functions.
///
/// This struct is passed as the first argument to all JIT functions.
/// It provides access to runtime resources needed during execution.
///
/// # Memory Layout
///
/// This struct uses `#[repr(C)]` to ensure predictable field layout for
/// access from JIT-generated code.
/// Function pointer type for VM call trampoline.
/// This allows vo_call_vm to call back into the VM without circular dependency.
pub type VmCallFn = extern "C" fn(
    vm: *mut c_void,
    fiber: *mut c_void,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult;

#[repr(C)]
pub struct JitContext {
    /// Pointer to the GC instance.
    pub gc: *mut Gc,
    
    /// Pointer to the global variables array.
    pub globals: *mut u64,
    
    /// Pointer to safepoint flag (read by JIT to check if GC wants to run).
    /// When this is true, JIT should call vo_gc_safepoint().
    pub safepoint_flag: *const bool,
    
    /// Pointer to panic flag (set by JIT when panic occurs).
    pub panic_flag: *mut bool,
    
    /// Opaque pointer to VM instance.
    /// Used by vo_call_vm to execute VM functions.
    /// Cast to `*mut Vm` in trampoline code.
    pub vm: *mut c_void,
    
    /// Opaque pointer to current Fiber.
    /// Used by vo_call_vm for stack management.
    /// Cast to `*mut Fiber` in trampoline code.
    pub fiber: *mut c_void,
    
    /// Callback to execute a function in VM.
    /// Set by VM when creating JitContext.
    pub call_vm_fn: Option<VmCallFn>,
    
    /// Pointer to itab array for interface method dispatch.
    /// Each Itab contains a Vec<u32> of method func_ids.
    /// Layout: *const Vec<Itab> (opaque, used by vo_call_iface)
    pub itabs: *const c_void,
    
    /// Function to lookup method from itab: (itabs, itab_id, method_idx) -> func_id
    pub itab_lookup_fn: Option<extern "C" fn(*const c_void, u32, u32) -> u32>,
    
    /// Pointer to ExternRegistry for calling extern functions.
    pub extern_registry: *const c_void,
    
    /// Callback to call extern function: (registry, gc, extern_id, args, arg_count, ret) -> JitResult
    pub call_extern_fn: Option<extern "C" fn(*const c_void, *mut Gc, u32, *const u64, u32, *mut u64) -> JitResult>,
    
    /// Pointer to ItabCache for interface assertions.
    pub itab_cache: *mut c_void,
    
    /// Pointer to Module for type information.
    pub module: *const c_void,
    
    /// Callback for interface assertion: (ctx, slot0, slot1, target_id, flags, dst) -> JitResult
    /// Returns matches in dst[0], and result values in subsequent slots
    pub iface_assert_fn: Option<extern "C" fn(*mut JitContext, u64, u64, u32, u16, *mut u64) -> u64>,
}

// =============================================================================
// JitResult
// =============================================================================

/// Result of JIT function execution.
///
/// JIT functions return this to indicate success or failure.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JitResult {
    /// Function completed successfully.
    Ok = 0,
    /// Function panicked.
    Panic = 1,
}

// =============================================================================
// Runtime Helper Functions
// =============================================================================

// NOTE: These functions are declared but not yet implemented.
// They will be implemented when we integrate JIT with the VM.
//
// The JIT compiler registers these symbols with Cranelift's JITBuilder,
// so JIT-generated code can call them directly.

/// Allocate a new GC object.
///
/// # Arguments
/// - `gc`: Pointer to GC instance
/// - `meta`: ValueMeta for the object (packed meta_id + value_kind)
/// - `slots`: Number of 64-bit slots to allocate
///
/// # Returns
/// GcRef (pointer to allocated object data)
///
/// # Safety
/// - `gc` must be a valid pointer to a Gc instance
#[no_mangle]
pub extern "C" fn vo_gc_alloc(gc: *mut Gc, meta: u32, slots: u32) -> u64 {
    use crate::ValueMeta;
    
    unsafe {
        let gc = &mut *gc;
        let value_meta = ValueMeta::from_raw(meta);
        gc.alloc(value_meta, slots as u16) as u64
    }
}

/// Write barrier for GC.
///
/// Called when storing a GcRef into a heap object. This is the slow path;
/// JIT generates inline fast path that checks `is_marking` first.
///
/// # Arguments
/// - `gc`: Pointer to GC instance
/// - `obj`: The object being written to (GcRef)
/// - `offset`: Slot offset within the object
/// - `val`: The value being stored (may be GcRef)
///
/// # Safety
/// - `gc` must be a valid pointer to a Gc instance
/// - `obj` must be a valid GcRef
#[no_mangle]
pub extern "C" fn vo_gc_write_barrier(_gc: *mut Gc, _obj: u64, _offset: u32, _val: u64) {
    // TODO: Implement
    // This is called when is_marking is true.
    // Need to mark the old value gray to preserve tri-color invariant.
}

/// GC safepoint.
///
/// Called at loop back-edges and before function calls when safepoint_flag
/// is set. May trigger garbage collection.
///
/// # Arguments
/// - `ctx`: JIT context
///
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
#[no_mangle]
pub extern "C" fn vo_gc_safepoint(_ctx: *mut JitContext) {
    // TODO: Implement
    // 1. Check if GC wants to run
    // 2. If so, scan JIT stack frames using stack maps
    // 3. Run GC
    // 4. Clear safepoint_flag
}

/// Call a VM-interpreted function from JIT code.
///
/// This is the trampoline from JIT to VM. All function calls from JIT
/// go through this (for simplicity - we don't inline JIT->JIT calls yet).
///
/// # Arguments
/// - `ctx`: JIT context
/// - `func_id`: Function ID to call
/// - `args`: Pointer to argument slots
/// - `arg_count`: Number of argument slots
/// - `ret`: Pointer to return value slots
/// - `ret_count`: Number of return value slots
///
/// # Returns
/// - `JitResult::Ok` if function completed normally
/// - `JitResult::Panic` if function panicked
///
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
/// - `args` must point to at least `arg_count` u64 values
/// - `ret` must point to space for at least `ret_count` u64 values
#[no_mangle]
pub extern "C" fn vo_call_vm(
    ctx: *mut JitContext,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    // Safety: ctx must be valid
    let ctx = unsafe { &*ctx };
    
    // Get the VM call callback
    let call_fn = match ctx.call_vm_fn {
        Some(f) => f,
        None => return JitResult::Panic, // No callback registered
    };
    
    // Call back into VM
    call_fn(ctx.vm, ctx.fiber, func_id, args, arg_count, ret, ret_count)
}

/// Trigger a panic.
///
/// # Arguments
/// - `ctx`: JIT context
/// - `msg`: Panic message (GcRef to string, or 0 for no message)
///
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
#[no_mangle]
pub extern "C" fn vo_panic(ctx: *mut JitContext, _msg: u64) {
    unsafe {
        let ctx = &mut *ctx;
        *ctx.panic_flag = true;
    }
}

/// Call an extern function from JIT code.
///
/// # Arguments
/// - `ctx`: JIT context
/// - `extern_id`: Extern function ID
/// - `args`: Pointer to argument slots
/// - `arg_count`: Number of argument slots
/// - `ret`: Pointer to return value slots
///
/// # Returns
/// - `JitResult::Ok` if function completed normally
/// - `JitResult::Panic` if function panicked
#[no_mangle]
pub extern "C" fn vo_call_extern(
    ctx: *mut JitContext,
    extern_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
) -> JitResult {
    let ctx_ref = unsafe { &*ctx };
    
    let call_fn = match ctx_ref.call_extern_fn {
        Some(f) => f,
        None => return JitResult::Panic,
    };
    
    call_fn(ctx_ref.extern_registry, ctx_ref.gc, extern_id, args, arg_count, ret)
}

/// Call a closure from JIT code.
///
/// # Arguments
/// - `ctx`: JIT context
/// - `closure_ref`: GcRef to the closure object
/// - `args`: Pointer to argument slots (closure_ref is NOT included)
/// - `arg_count`: Number of argument slots
/// - `ret`: Pointer to return value slots
/// - `ret_count`: Number of return value slots
///
/// # Returns
/// - `JitResult::Ok` if function completed normally
/// - `JitResult::Panic` if function panicked
#[no_mangle]
pub extern "C" fn vo_call_closure(
    ctx: *mut JitContext,
    closure_ref: u64,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    use crate::objects::closure;
    
    let ctx_ref = unsafe { &*ctx };
    let func_id = closure::func_id(closure_ref as crate::gc::GcRef);
    
    // Prepare args with closure as first slot
    let mut full_args = vec![closure_ref];
    for i in 0..arg_count {
        full_args.push(unsafe { *args.add(i as usize) });
    }
    
    let call_fn = match ctx_ref.call_vm_fn {
        Some(f) => f,
        None => return JitResult::Panic,
    };
    
    call_fn(
        ctx_ref.vm,
        ctx_ref.fiber,
        func_id,
        full_args.as_ptr(),
        (arg_count + 1),
        ret,
        ret_count,
    )
}

/// Call an interface method from JIT code.
///
/// # Arguments
/// - `ctx`: JIT context
/// - `iface_slot0`: Interface slot0 (itab_id << 32 | value_meta)
/// - `iface_slot1`: Interface slot1 (data: immediate or GcRef)
/// - `method_idx`: Method index in the interface
/// - `args`: Pointer to argument slots (receiver is NOT included)
/// - `arg_count`: Number of argument slots (excluding receiver)
/// - `ret`: Pointer to return value slots
/// - `ret_count`: Number of return value slots
///
/// # Returns
/// - `JitResult::Ok` if function completed normally
/// - `JitResult::Panic` if function panicked
#[no_mangle]
pub extern "C" fn vo_call_iface(
    ctx: *mut JitContext,
    iface_slot0: u64,
    iface_slot1: u64,
    method_idx: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
    _func_id_hint: u32, // Reserved for future optimization
) -> JitResult {
    let ctx_ref = unsafe { &*ctx };
    
    // Extract itab_id from slot0 (high 32 bits)
    let itab_id = (iface_slot0 >> 32) as u32;
    
    // Lookup func_id from itab
    let func_id = match ctx_ref.itab_lookup_fn {
        Some(lookup) => lookup(ctx_ref.itabs, itab_id, method_idx),
        None => return JitResult::Panic, // No lookup function registered
    };
    
    // Prepare args with receiver as first slot
    let mut full_args = vec![iface_slot1];
    for i in 0..arg_count {
        full_args.push(unsafe { *args.add(i as usize) });
    }
    
    let call_fn = match ctx_ref.call_vm_fn {
        Some(f) => f,
        None => return JitResult::Panic,
    };
    
    call_fn(
        ctx_ref.vm,
        ctx_ref.fiber,
        func_id,
        full_args.as_ptr(),
        arg_count + 1,
        ret,
        ret_count,
    )
}

// =============================================================================
// Map Helpers
// =============================================================================

/// Create a new map.
#[no_mangle]
pub extern "C" fn vo_map_new(gc: *mut Gc, key_meta: u32, val_meta: u32, key_slots: u32, val_slots: u32) -> u64 {
    use crate::objects::map;
    use crate::ValueMeta;
    unsafe {
        let gc = &mut *gc;
        map::create(gc, ValueMeta::from_raw(key_meta), ValueMeta::from_raw(val_meta), key_slots as u16, val_slots as u16) as u64
    }
}

/// Get map length.
#[no_mangle]
pub extern "C" fn vo_map_len(m: u64) -> u64 {
    use crate::objects::map;
    if m == 0 { return 0; }
    map::len(m as crate::gc::GcRef) as u64
}

/// Get value from map. Returns pointer to value slots, or null if not found.
/// key_ptr points to key_slots u64 values.
/// val_ptr is output buffer for val_slots u64 values.
/// Returns 1 if found, 0 if not found.
#[no_mangle]
pub extern "C" fn vo_map_get(m: u64, key_ptr: *const u64, key_slots: u32, val_ptr: *mut u64, val_slots: u32) -> u64 {
    use crate::objects::map;
    if m == 0 { return 0; }
    
    let key = unsafe { core::slice::from_raw_parts(key_ptr, key_slots as usize) };
    let (val_opt, ok) = map::get_with_ok(m as crate::gc::GcRef, key);
    
    if let Some(val) = val_opt {
        let copy_len = (val_slots as usize).min(val.len());
        unsafe {
            core::ptr::copy_nonoverlapping(val.as_ptr(), val_ptr, copy_len);
        }
    }
    ok as u64
}

/// Set value in map.
#[no_mangle]
pub extern "C" fn vo_map_set(m: u64, key_ptr: *const u64, key_slots: u32, val_ptr: *const u64, val_slots: u32) {
    use crate::objects::map;
    if m == 0 { return; }
    
    let key = unsafe { core::slice::from_raw_parts(key_ptr, key_slots as usize) };
    let val = unsafe { core::slice::from_raw_parts(val_ptr, val_slots as usize) };
    map::set(m as crate::gc::GcRef, key, val);
}

/// Delete key from map.
#[no_mangle]
pub extern "C" fn vo_map_delete(m: u64, key_ptr: *const u64, key_slots: u32) {
    use crate::objects::map;
    if m == 0 { return; }
    
    let key = unsafe { core::slice::from_raw_parts(key_ptr, key_slots as usize) };
    map::delete(m as crate::gc::GcRef, key);
}

/// Get key-value by index for iteration.
/// Returns 1 if valid entry exists, 0 if end of map.
#[no_mangle]
pub extern "C" fn vo_map_iter_get(m: u64, idx: u64, key_ptr: *mut u64, key_slots: u32, val_ptr: *mut u64, val_slots: u32) -> u64 {
    use crate::objects::map;
    if m == 0 { return 0; }
    
    if let Some((key, val)) = map::iter_at(m as crate::gc::GcRef, idx as usize) {
        let key_copy = (key_slots as usize).min(key.len());
        let val_copy = (val_slots as usize).min(val.len());
        unsafe {
            core::ptr::copy_nonoverlapping(key.as_ptr(), key_ptr, key_copy);
            core::ptr::copy_nonoverlapping(val.as_ptr(), val_ptr, val_copy);
        }
        1
    } else {
        0
    }
}

// =============================================================================
// Map/String Iteration Helpers
// =============================================================================

/// Get next key-value pair from map iteration.
///
/// # Arguments
/// - `map`: Map GcRef
/// - `cursor`: Pointer to cursor (updated by this function)
/// - `key`: Pointer to store key
/// - `val`: Pointer to store value
///
/// # Returns
/// - `true` if there was a next element
/// - `false` if iteration is complete
///
/// # Safety
/// - All pointers must be valid
#[no_mangle]
pub extern "C" fn vo_map_iter_next(
    _map: u64,
    _cursor: *mut u64,
    _key: *mut u64,
    _val: *mut u64,
) -> bool {
    // TODO: Implement
    todo!("vo_map_iter_next not yet implemented")
}

/// Decode a UTF-8 rune from a string.
///
/// # Arguments
/// - `s`: String GcRef
/// - `pos`: Byte position in string
///
/// # Returns
/// Packed value: `(rune << 32) | width`
#[no_mangle]
pub extern "C" fn vo_str_decode_rune(s: u64, pos: u64) -> u64 {
    use crate::objects::string;
    let (rune, width) = string::decode_rune_at(s as crate::gc::GcRef, pos as usize);
    ((rune as u64) << 32) | (width as u64)
}

/// Get string length.
#[no_mangle]
pub extern "C" fn vo_str_len(s: u64) -> u64 {
    use crate::objects::string;
    string::len(s as crate::gc::GcRef) as u64
}

/// Get byte at index.
#[no_mangle]
pub extern "C" fn vo_str_index(s: u64, idx: u64) -> u64 {
    use crate::objects::string;
    string::index(s as crate::gc::GcRef, idx as usize) as u64
}

/// Concatenate two strings.
#[no_mangle]
pub extern "C" fn vo_str_concat(gc: *mut Gc, a: u64, b: u64) -> u64 {
    use crate::objects::string;
    unsafe {
        let gc = &mut *gc;
        string::concat(gc, a as crate::gc::GcRef, b as crate::gc::GcRef) as u64
    }
}

/// Create string slice.
#[no_mangle]
pub extern "C" fn vo_str_slice(gc: *mut Gc, s: u64, lo: u64, hi: u64) -> u64 {
    use crate::objects::string;
    unsafe {
        let gc = &mut *gc;
        string::slice_of(gc, s as crate::gc::GcRef, lo as usize, hi as usize) as u64
    }
}

/// Compare strings for equality.
#[no_mangle]
pub extern "C" fn vo_str_eq(a: u64, b: u64) -> u64 {
    use crate::objects::string;
    string::eq(a as crate::gc::GcRef, b as crate::gc::GcRef) as u64
}

/// Compare strings.
#[no_mangle]
pub extern "C" fn vo_str_cmp(a: u64, b: u64) -> i32 {
    use crate::objects::string;
    string::cmp(a as crate::gc::GcRef, b as crate::gc::GcRef)
}

/// Create string from constant index.
#[no_mangle]
pub extern "C" fn vo_str_new(gc: *mut Gc, data: *const u8, len: u64) -> u64 {
    use crate::objects::string;
    unsafe {
        let gc = &mut *gc;
        let bytes = core::slice::from_raw_parts(data, len as usize);
        string::create(gc, bytes) as u64
    }
}

// =============================================================================
// Interface Helpers
// =============================================================================

/// Pack interface slot0: (itab_id << 32) | (rttid << 8) | value_kind
#[no_mangle]
pub extern "C" fn vo_iface_pack_slot0(itab_id: u32, rttid: u32, vk: u8) -> u64 {
    ((itab_id as u64) << 32) | ((rttid as u64) << 8) | (vk as u64)
}

/// Clone a GcRef (deep copy for value semantics).
#[no_mangle]
pub extern "C" fn vo_ptr_clone(gc: *mut Gc, ptr: u64) -> u64 {
    if ptr == 0 {
        return 0;
    }
    unsafe {
        let gc = &mut *gc;
        gc.ptr_clone(ptr as crate::gc::GcRef) as u64
    }
}

// =============================================================================
// Closure Helpers
// =============================================================================

/// Create a new closure.
#[no_mangle]
pub extern "C" fn vo_closure_new(gc: *mut Gc, func_id: u32, capture_count: u32) -> u64 {
    use crate::objects::closure;
    unsafe {
        let gc = &mut *gc;
        closure::create(gc, func_id, capture_count as usize) as u64
    }
}

// =============================================================================
// Channel Helpers
// =============================================================================

/// Create a new channel.
#[no_mangle]
pub extern "C" fn vo_chan_new(gc: *mut Gc, elem_meta: u32, elem_slots: u32, cap: u64) -> u64 {
    use crate::objects::channel;
    use crate::ValueMeta;
    unsafe {
        let gc = &mut *gc;
        channel::create(gc, ValueMeta::from_raw(elem_meta), elem_slots as u16, cap as usize) as u64
    }
}

// =============================================================================
// Array Helpers
// =============================================================================

/// Create a new array.
/// Returns GcRef to array data (after ArrayHeader).
#[no_mangle]
pub extern "C" fn vo_array_new(gc: *mut Gc, elem_meta: u32, elem_slots: u32, len: u64) -> u64 {
    use crate::objects::array;
    use crate::ValueMeta;
    unsafe {
        let gc = &mut *gc;
        array::create(gc, ValueMeta::from_raw(elem_meta), elem_slots as usize, len as usize) as u64
    }
}

/// Get element from array.
/// Returns the value at arr[slot_offset].
#[no_mangle]
pub extern "C" fn vo_array_get(arr: u64, slot_offset: u64) -> u64 {
    use crate::objects::array;
    array::get(arr as crate::gc::GcRef, slot_offset as usize)
}

/// Set element in array.
#[no_mangle]
pub extern "C" fn vo_array_set(arr: u64, slot_offset: u64, val: u64) {
    use crate::objects::array;
    array::set(arr as crate::gc::GcRef, slot_offset as usize, val);
}

/// Get array length.
#[no_mangle]
pub extern "C" fn vo_array_len(arr: u64) -> u64 {
    use crate::objects::array;
    if arr == 0 { return 0; }
    array::len(arr as crate::gc::GcRef) as u64
}

// =============================================================================
// Slice Helpers
// =============================================================================

/// Create a new slice.
#[no_mangle]
pub extern "C" fn vo_slice_new(gc: *mut Gc, elem_meta: u32, elem_slots: u32, len: u64, cap: u64) -> u64 {
    use crate::objects::slice;
    use crate::ValueMeta;
    unsafe {
        let gc = &mut *gc;
        slice::create(gc, ValueMeta::from_raw(elem_meta), elem_slots as usize, len as usize, cap as usize) as u64
    }
}

/// Get slice length.
#[no_mangle]
pub extern "C" fn vo_slice_len(s: u64) -> u64 {
    use crate::objects::slice;
    slice::len(s as crate::gc::GcRef) as u64
}

/// Get slice capacity.
#[no_mangle]
pub extern "C" fn vo_slice_cap(s: u64) -> u64 {
    use crate::objects::slice;
    slice::cap(s as crate::gc::GcRef) as u64
}

/// Get element from slice.
#[no_mangle]
pub extern "C" fn vo_slice_get(s: u64, slot_offset: u64) -> u64 {
    use crate::objects::slice;
    slice::get(s as crate::gc::GcRef, slot_offset as usize)
}

/// Set element in slice.
#[no_mangle]
pub extern "C" fn vo_slice_set(s: u64, slot_offset: u64, val: u64) {
    use crate::objects::slice;
    slice::set(s as crate::gc::GcRef, slot_offset as usize, val);
}

/// Create a sub-slice (two-index: s[lo:hi]).
#[no_mangle]
pub extern "C" fn vo_slice_slice(gc: *mut Gc, s: u64, lo: u64, hi: u64) -> u64 {
    use crate::objects::slice;
    unsafe {
        let gc = &mut *gc;
        slice::slice_of(gc, s as crate::gc::GcRef, lo as usize, hi as usize) as u64
    }
}

/// Create a sub-slice with cap (three-index: s[lo:hi:max]).
#[no_mangle]
pub extern "C" fn vo_slice_slice3(gc: *mut Gc, s: u64, lo: u64, hi: u64, max: u64) -> u64 {
    use crate::objects::slice;
    unsafe {
        let gc = &mut *gc;
        slice::slice_of_with_cap(gc, s as crate::gc::GcRef, lo as usize, hi as usize, max as usize) as u64
    }
}

/// Append single element to slice.
/// val_ptr points to elem_slots u64 values.
#[no_mangle]
pub extern "C" fn vo_slice_append(gc: *mut Gc, elem_meta: u32, elem_slots: u32, s: u64, val_ptr: *const u64) -> u64 {
    use crate::objects::slice;
    use crate::ValueMeta;
    unsafe {
        let gc = &mut *gc;
        let val = core::slice::from_raw_parts(val_ptr, elem_slots as usize);
        slice::append(gc, ValueMeta::from_raw(elem_meta), elem_slots as usize, s as crate::gc::GcRef, val) as u64
    }
}

/// Create slice from array range (arr[lo:hi]).
#[no_mangle]
pub extern "C" fn vo_slice_from_array(gc: *mut Gc, arr: u64, lo: u64, hi: u64) -> u64 {
    use crate::objects::slice;
    unsafe {
        let gc = &mut *gc;
        let len = (hi - lo) as usize;
        slice::from_array_range(gc, arr as crate::gc::GcRef, lo as usize, len, len) as u64
    }
}

/// Create slice from array range with cap (arr[lo:hi:max]).
#[no_mangle]
pub extern "C" fn vo_slice_from_array3(gc: *mut Gc, arr: u64, lo: u64, hi: u64, max: u64) -> u64 {
    use crate::objects::slice;
    unsafe {
        let gc = &mut *gc;
        let len = (hi - lo) as usize;
        let cap = (max - lo) as usize;
        slice::from_array_range(gc, arr as crate::gc::GcRef, lo as usize, len, cap) as u64
    }
}

// =============================================================================
// Interface Helpers
// =============================================================================

/// Interface assertion.
/// Returns: 1 if matches, 0 if not (when has_ok), or panics (when !has_ok && !matches)
/// dst layout: [result_slots...][ok_flag if has_ok]
#[no_mangle]
pub extern "C" fn vo_iface_assert(
    ctx: *mut JitContext,
    slot0: u64,
    slot1: u64,
    target_id: u32,
    flags: u16,
    dst: *mut u64,
) -> u64 {
    use crate::objects::interface;
    use crate::ValueKind;
    
    let assert_kind = flags & 0x3;
    let has_ok = ((flags >> 2) & 0x1) != 0;
    let target_slots = (flags >> 3) as usize;
    
    let src_rttid = interface::unpack_rttid(slot0);
    let src_vk = interface::unpack_value_kind(slot0);
    
    // nil interface always fails
    let matches = if src_vk == ValueKind::Void {
        false
    } else if assert_kind == 0 {
        // Type comparison: check rttid
        src_rttid == target_id
    } else {
        // Interface method check - call through callback
        unsafe {
            let ctx_ref = &*ctx;
            if let Some(iface_assert_fn) = ctx_ref.iface_assert_fn {
                iface_assert_fn(ctx, slot0, slot1, target_id, flags, dst) != 0
            } else {
                false
            }
        }
    };
    
    unsafe {
        if has_ok {
            // Write ok flag
            let ok_slot = if assert_kind == 1 { 2 } else if target_slots > 1 { target_slots } else { 1 };
            *dst.add(ok_slot) = matches as u64;
            
            if matches {
                write_iface_assert_success(ctx, slot0, slot1, assert_kind, target_slots, target_id, dst);
            } else {
                // Zero out on failure
                let dst_slots = if assert_kind == 1 { 2 } else { target_slots.max(1) };
                for i in 0..dst_slots {
                    *dst.add(i) = 0;
                }
            }
            1 // Continue
        } else if matches {
            write_iface_assert_success(ctx, slot0, slot1, assert_kind, target_slots, target_id, dst);
            1 // Continue
        } else {
            0 // Panic
        }
    }
}

unsafe fn write_iface_assert_success(
    ctx: *mut JitContext,
    slot0: u64,
    slot1: u64,
    assert_kind: u16,
    target_slots: usize,
    target_id: u32,
    dst: *mut u64,
) {
    use crate::objects::interface;
    use crate::ValueKind;
    use crate::gc::GcRef;
    
    let src_rttid = interface::unpack_rttid(slot0);
    let src_vk = interface::unpack_value_kind(slot0);
    
    if assert_kind == 1 {
        // Interface assertion: call through callback to get new itab
        let ctx_ref = &*ctx;
        if let Some(iface_assert_fn) = ctx_ref.iface_assert_fn {
            // Callback handles writing dst
            iface_assert_fn(ctx, slot0, slot1, target_id, 0x8000 | (assert_kind as u16), dst);
        } else {
            *dst = slot0;
            *dst.add(1) = slot1;
        }
    } else if src_vk == ValueKind::Struct || src_vk == ValueKind::Array {
        // Copy value from GcRef
        let gc_ref = slot1 as GcRef;
        let slots = target_slots.max(1);
        if slot1 != 0 {
            for i in 0..slots {
                *dst.add(i) = *gc_ref.add(i);
            }
        } else {
            for i in 0..slots {
                *dst.add(i) = 0;
            }
        }
    } else {
        // Other types: slot1 is the value
        *dst = slot1;
    }
}

// =============================================================================
// Symbol Registration
// =============================================================================

/// Get all runtime symbols for JIT registration.
///
/// Returns a slice of (name, function_pointer) pairs that should be
/// registered with Cranelift's JITBuilder.
pub fn get_runtime_symbols() -> &'static [(&'static str, *const u8)] {
    &[
        ("vo_gc_alloc", vo_gc_alloc as *const u8),
        ("vo_gc_write_barrier", vo_gc_write_barrier as *const u8),
        ("vo_gc_safepoint", vo_gc_safepoint as *const u8),
        ("vo_call_vm", vo_call_vm as *const u8),
        ("vo_call_closure", vo_call_closure as *const u8),
        ("vo_call_iface", vo_call_iface as *const u8),
        ("vo_panic", vo_panic as *const u8),
        ("vo_call_extern", vo_call_extern as *const u8),
        ("vo_map_iter_next", vo_map_iter_next as *const u8),
        ("vo_str_new", vo_str_new as *const u8),
        ("vo_str_len", vo_str_len as *const u8),
        ("vo_str_index", vo_str_index as *const u8),
        ("vo_str_concat", vo_str_concat as *const u8),
        ("vo_str_slice", vo_str_slice as *const u8),
        ("vo_str_eq", vo_str_eq as *const u8),
        ("vo_str_cmp", vo_str_cmp as *const u8),
        ("vo_str_decode_rune", vo_str_decode_rune as *const u8),
        ("vo_closure_new", vo_closure_new as *const u8),
        ("vo_chan_new", vo_chan_new as *const u8),
        ("vo_array_new", vo_array_new as *const u8),
        ("vo_array_get", vo_array_get as *const u8),
        ("vo_array_set", vo_array_set as *const u8),
        ("vo_array_len", vo_array_len as *const u8),
        ("vo_slice_new", vo_slice_new as *const u8),
        ("vo_slice_len", vo_slice_len as *const u8),
        ("vo_slice_cap", vo_slice_cap as *const u8),
        ("vo_slice_get", vo_slice_get as *const u8),
        ("vo_slice_set", vo_slice_set as *const u8),
        ("vo_slice_slice", vo_slice_slice as *const u8),
        ("vo_slice_slice3", vo_slice_slice3 as *const u8),
        ("vo_slice_append", vo_slice_append as *const u8),
        ("vo_slice_from_array", vo_slice_from_array as *const u8),
        ("vo_slice_from_array3", vo_slice_from_array3 as *const u8),
        ("vo_iface_pack_slot0", vo_iface_pack_slot0 as *const u8),
        ("vo_iface_assert", vo_iface_assert as *const u8),
        ("vo_ptr_clone", vo_ptr_clone as *const u8),
        ("vo_map_new", vo_map_new as *const u8),
        ("vo_map_len", vo_map_len as *const u8),
        ("vo_map_get", vo_map_get as *const u8),
        ("vo_map_set", vo_map_set as *const u8),
        ("vo_map_delete", vo_map_delete as *const u8),
        ("vo_map_iter_get", vo_map_iter_get as *const u8),
    ]
}
