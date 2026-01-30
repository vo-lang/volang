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

use crate::gc::{Gc, GcRef};
use crate::slot::slots_for_bytes;
use crate::itab::ItabCache;
use crate::objects::interface::InterfaceSlot;
use crate::ValueKind;
use vo_common_core::bytecode::Module;

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
    
    /// Panic message (interface as InterfaceSlot, set by vo_panic for user panics).
    pub panic_msg: *mut InterfaceSlot,
    
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
    
    /// Pointer to ItabCache for interface method dispatch and dynamic itab creation.
    pub itab_cache: *mut ItabCache,
    
    /// Pointer to ExternRegistry for calling extern functions.
    pub extern_registry: *const c_void,
    
    /// Callback to call extern function: (ctx, registry, gc, module, extern_id, args, arg_count, ret, ret_slots) -> JitResult
    pub call_extern_fn: Option<extern "C" fn(*mut JitContext, *const c_void, *mut Gc, *const c_void, u32, *const u64, u32, *mut u64, u32) -> JitResult>,
    
    /// Pointer to Module for type information.
    pub module: *const Module,
    
    /// JIT function pointer table: jit_func_table[func_id] = pointer to JIT function (or null if not compiled).
    /// Used for direct JIT-to-JIT calls without going through VM trampoline.
    pub jit_func_table: *const *const u8,
    
    /// Number of functions (length of jit_func_table).
    pub jit_func_count: u32,
    
    /// Pointer to program arguments.
    pub program_args: *const Vec<String>,
    
    /// Pointer to sentinel error cache.
    pub sentinel_errors: *mut crate::ffi::SentinelErrorCache,
    
    /// Pointer to shared IoRuntime for async I/O operations.
    /// JIT extern calls use this instead of creating per-call IoRuntime.
    #[cfg(feature = "std")]
    pub io: *mut crate::io::IoRuntime,
    
    /// NeedVm entry PC (set by JIT when returning NeedVm).
    /// This is the PC of the instruction that needs VM execution.
    pub need_vm_entry_pc: u32,
    
    /// NeedVm resume PC (set by JIT when returning NeedVm).
    /// This is the continuation PC where JIT should resume after VM completes.
    pub need_vm_resume_pc: u32,
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
    /// Function completed successfully (or returned normally).
    Ok = 0,
    /// Function panicked.
    Panic = 1,
    /// Hand off execution to VM interpreter.
    /// need_vm_entry_pc and need_vm_resume_pc in JitContext contain the PCs.
    NeedVm = 2,
}

// =============================================================================
// Runtime Helper Functions
// =============================================================================

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
/// - `obj` must be a valid GcRef (parent object being written to)
/// - `val` must be a valid GcRef or 0 (child value being written)
#[no_mangle]
pub extern "C" fn vo_gc_write_barrier(gc: *mut Gc, obj: u64, _offset: u32, val: u64) {
    if gc.is_null() || obj == 0 {
        return;
    }
    let gc = unsafe { &mut *gc };
    let parent = obj as GcRef;
    let child = val as GcRef;
    gc.write_barrier(parent, child);
}

/// GC safepoint.
///
/// Called at loop back-edges and before function calls when safepoint_flag
/// is set. Currently a no-op placeholder - full GC integration requires
/// stack map support which is not yet implemented.
///
/// The synchronous JIT design means GC can run when:
/// 1. JIT calls vo_call_vm (VM will handle GC)
/// 2. JIT calls vo_gc_alloc (checks debt)
///
/// # Arguments
/// - `ctx`: JIT context
///
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
#[no_mangle]
pub extern "C" fn vo_gc_safepoint(_ctx: *mut JitContext) {
    // Currently no-op. GC is triggered by:
    // - vo_gc_alloc when debt > 0
    // - vo_call_vm which enters VM context where GC can run
    //
    // Full safepoint support would require:
    // 1. Stack maps to identify GcRef locations in JIT frames
    // 2. Cooperation with VM to scan roots
    // For now, the synchronous JIT design ensures GC safety at call boundaries.
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

/// Set NeedVm state in JitContext.
/// Called by JIT when it needs to hand off to VM for a blocking operation.
/// 
/// # Arguments
/// - `ctx`: JIT context
/// - `entry_pc`: PC of the instruction that needs VM execution
/// - `resume_pc`: PC where JIT should resume after VM completes
/// 
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
#[no_mangle]
pub extern "C" fn vo_set_need_vm(ctx: *mut JitContext, entry_pc: u32, resume_pc: u32) {
    unsafe {
        (*ctx).need_vm_entry_pc = entry_pc;
        (*ctx).need_vm_resume_pc = resume_pc;
    }
}

/// Trigger a user panic from JIT code.
/// The panic message is stored in JitContext for the VM to read later.
/// 
/// # Arguments
/// - `ctx`: JIT context
/// - `msg_slot0`: Interface slot0 (packed metadata)
/// - `msg_slot1`: Interface slot1 (data: string GcRef or immediate)
/// 
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
#[no_mangle]
pub extern "C" fn vo_panic(ctx: *mut JitContext, msg_slot0: u64, msg_slot1: u64) {
    unsafe {
        let ctx = &mut *ctx;
        *ctx.panic_flag = true;
        // Store panic message in JitContext for VM to read
        (*ctx.panic_msg).slot0 = msg_slot0;
        (*ctx.panic_msg).slot1 = msg_slot1;
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
    ret_slots: u32,
) -> JitResult {
    let ctx_ref = unsafe { &*ctx };
    
    let call_fn = match ctx_ref.call_extern_fn {
        Some(f) => f,
        None => return JitResult::Panic,
    };
    
    call_fn(ctx, ctx_ref.extern_registry, ctx_ref.gc, ctx_ref.module as *const c_void, extern_id, args, arg_count, ret, ret_slots)
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
    let closure_gcref = closure_ref as crate::gc::GcRef;
    let func_id = closure::func_id(closure_gcref);
    
    // Get func_def to check recv_slots (for method closures)
    let module = unsafe { &*ctx_ref.module };
    let func_def = &module.functions[func_id as usize];
    let recv_slots = func_def.recv_slots as usize;
    
    // Use common closure call layout logic
    let layout = closure::call_layout(
        closure_ref,
        closure_gcref,
        recv_slots,
        func_def.is_closure,
    );
    
    let mut full_args = Vec::with_capacity(layout.slot0.is_some() as usize + arg_count as usize);
    full_args.extend(layout.slot0);
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
        full_args.len() as u32,
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
    
    // Lookup func_id from itab directly
    let func_id = unsafe {
        let itab_cache = &*ctx_ref.itab_cache;
        itab_cache.lookup_method(itab_id, method_idx as usize)
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
pub extern "C" fn vo_map_new(gc: *mut Gc, key_meta: u32, val_meta: u32, key_slots: u32, val_slots: u32, key_rttid: u32) -> u64 {
    use crate::objects::map;
    use crate::ValueMeta;
    unsafe {
        let gc = &mut *gc;
        map::create(gc, ValueMeta::from_raw(key_meta), ValueMeta::from_raw(val_meta), key_slots as u16, val_slots as u16, key_rttid) as u64
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
    let (val_opt, ok) = map::get_with_ok(m as crate::gc::GcRef, key, None);
    
    if let Some(val) = val_opt {
        let copy_len = (val_slots as usize).min(val.len());
        unsafe {
            core::ptr::copy_nonoverlapping(val.as_ptr(), val_ptr, copy_len);
        }
    }
    ok as u64
}

/// Set value in map.
/// Returns: 0 = success, 1 = panic (interface key with uncomparable type)
#[no_mangle]
pub extern "C" fn vo_map_set(m: u64, key_ptr: *const u64, key_slots: u32, val_ptr: *const u64, val_slots: u32) -> u64 {
    use crate::objects::{map, interface};
    use crate::ValueKind;
    if m == 0 { return 0; }
    
    let key = unsafe { core::slice::from_raw_parts(key_ptr, key_slots as usize) };
    let val = unsafe { core::slice::from_raw_parts(val_ptr, val_slots as usize) };
    
    // Check if key is interface (2 slots) with uncomparable underlying type
    if key_slots == 2 {
        let key_vk = map::key_kind(m as crate::gc::GcRef);
        if key_vk == ValueKind::Interface {
            let slot0 = key[0];
            let inner_vk = interface::unpack_value_kind(slot0);
            match inner_vk {
                ValueKind::Slice | ValueKind::Map | ValueKind::Closure => {
                    return 1; // Uncomparable type - should panic
                }
                _ => {}
            }
        }
    }
    
    map::set(m as crate::gc::GcRef, key, val, None);
    0
}

/// Delete key from map.
#[no_mangle]
pub extern "C" fn vo_map_delete(m: u64, key_ptr: *const u64, key_slots: u32) {
    use crate::objects::map;
    if m == 0 { return; }
    
    let key = unsafe { core::slice::from_raw_parts(key_ptr, key_slots as usize) };
    map::delete(m as crate::gc::GcRef, key, None);
}

/// Initialize a map iterator. Writes MAP_ITER_SLOTS * SLOT_BYTES bytes to iter_ptr.
#[no_mangle]
pub extern "C" fn vo_map_iter_init(m: u64, iter_ptr: *mut u64) {
    use crate::objects::map;
    const SLOTS: usize = map::MAP_ITER_SLOTS;
    let iter = map::iter_init(m as crate::gc::GcRef);
    unsafe {
        core::ptr::copy_nonoverlapping(
            &iter as *const map::MapIterator as *const u64,
            iter_ptr,
            SLOTS,
        );
    }
    core::mem::forget(iter);
}

/// Advance map iterator and get next key-value pair.
/// Returns 1 if valid entry exists, 0 if exhausted.
#[no_mangle]
pub extern "C" fn vo_map_iter_next(iter_ptr: *mut u64, key_ptr: *mut u64, key_slots: u32, val_ptr: *mut u64, val_slots: u32) -> u64 {
    use crate::objects::map;
    let iter = unsafe { &mut *(iter_ptr as *mut map::MapIterator) };
    
    match map::iter_next(iter) {
        Some((key, val)) => {
            let key_copy = (key_slots as usize).min(key.len());
            let val_copy = (val_slots as usize).min(val.len());
            unsafe {
                core::ptr::copy_nonoverlapping(key.as_ptr(), key_ptr, key_copy);
                core::ptr::copy_nonoverlapping(val.as_ptr(), val_ptr, val_copy);
            }
            1
        }
        None => 0,
    }
}

// =============================================================================
// String Helpers
// =============================================================================

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

/// Get channel length (number of elements in buffer).
#[no_mangle]
pub extern "C" fn vo_chan_len(ch: u64) -> u64 {
    use crate::objects::channel;
    use crate::gc::GcRef;
    let ch = ch as GcRef;
    if ch.is_null() { 0 } else { channel::len(ch) as u64 }
}

/// Get channel capacity.
#[no_mangle]
pub extern "C" fn vo_chan_cap(ch: u64) -> u64 {
    use crate::objects::queue_state;
    use crate::gc::GcRef;
    let ch = ch as GcRef;
    if ch.is_null() { 0 } else { queue_state::capacity(ch) as u64 }
}

// =============================================================================
// Array Helpers
// =============================================================================

/// Create a new array with packed element storage.
/// elem_bytes: actual byte size per element (1/2/4/8 for packed, slots*8 for slot-based)
#[no_mangle]
pub extern "C" fn vo_array_new(gc: *mut Gc, elem_meta: u32, elem_bytes: u32, len: u64) -> u64 {
    use crate::objects::array;
    use crate::ValueMeta;
    unsafe {
        let gc = &mut *gc;
        array::create(gc, ValueMeta::from_raw(elem_meta), elem_bytes as usize, len as usize) as u64
    }
}

/// Get element from array with automatic sign extension.
/// idx is element index, elem_bytes is byte size per element.
#[no_mangle]
pub extern "C" fn vo_array_get(arr: u64, idx: u64, elem_bytes: u64) -> u64 {
    use crate::objects::array;
    array::get_auto(arr as crate::gc::GcRef, idx as usize, elem_bytes as usize)
}

/// Set element in array with automatic type conversion.
/// idx is element index, elem_bytes is byte size per element.
#[no_mangle]
pub extern "C" fn vo_array_set(arr: u64, idx: u64, val: u64, elem_bytes: u64) {
    use crate::objects::array;
    array::set_auto(arr as crate::gc::GcRef, idx as usize, val, elem_bytes as usize);
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

/// Create a new slice with packed element storage.
/// elem_bytes: actual byte size per element (1/2/4/8 for packed, slots*8 for slot-based)
#[no_mangle]
pub extern "C" fn vo_slice_new(gc: *mut Gc, elem_meta: u32, elem_bytes: u32, len: u64, cap: u64) -> u64 {
    use crate::objects::slice;
    use crate::ValueMeta;
    unsafe {
        let gc = &mut *gc;
        slice::create(gc, ValueMeta::from_raw(elem_meta), elem_bytes as usize, len as usize, cap as usize) as u64
    }
}

/// Get slice length.
#[no_mangle]
pub extern "C" fn vo_slice_len(s: u64) -> u64 {
    if s == 0 { return 0; }
    use crate::objects::slice;
    slice::len(s as crate::gc::GcRef) as u64
}

/// Get slice capacity.
#[no_mangle]
pub extern "C" fn vo_slice_cap(s: u64) -> u64 {
    if s == 0 { return 0; }
    use crate::objects::slice;
    slice::cap(s as crate::gc::GcRef) as u64
}

/// Get element from slice with automatic sign extension.
/// idx is element index, elem_bytes is byte size per element.
#[no_mangle]
pub extern "C" fn vo_slice_get(s: u64, idx: u64, elem_bytes: u64) -> u64 {
    use crate::objects::slice;
    let s_ref = s as crate::gc::GcRef;
    let base_ptr = slice::data_ptr(s_ref);
    let elem_kind = slice::elem_kind(s_ref);
    slice::get_auto(base_ptr, idx as usize, elem_bytes as usize, elem_kind)
}

/// Set element in slice with automatic type conversion.
/// idx is element index, elem_bytes is byte size per element.
#[no_mangle]
pub extern "C" fn vo_slice_set(s: u64, idx: u64, val: u64, elem_bytes: u64) {
    use crate::objects::slice;
    let s_ref = s as crate::gc::GcRef;
    let base_ptr = slice::data_ptr(s_ref);
    let elem_kind = slice::elem_kind(s_ref);
    slice::set_auto(base_ptr, idx as usize, val, elem_bytes as usize, elem_kind);
}

/// Create a sub-slice (two-index: s[lo:hi]).
/// Returns u64::MAX on bounds error.
#[no_mangle]
pub extern "C" fn vo_slice_slice(gc: *mut Gc, s: u64, lo: u64, hi: u64) -> u64 {
    use crate::objects::slice;
    unsafe {
        let gc = &mut *gc;
        match slice::slice_of(gc, s as crate::gc::GcRef, lo as usize, hi as usize) {
            Some(r) => r as u64,
            None => u64::MAX,
        }
    }
}

/// Create a sub-slice with cap (three-index: s[lo:hi:max]).
/// Returns u64::MAX on bounds error.
#[no_mangle]
pub extern "C" fn vo_slice_slice3(gc: *mut Gc, s: u64, lo: u64, hi: u64, max: u64) -> u64 {
    use crate::objects::slice;
    unsafe {
        let gc = &mut *gc;
        match slice::slice_of_with_cap(gc, s as crate::gc::GcRef, lo as usize, hi as usize, max as usize) {
            Some(r) => r as u64,
            None => u64::MAX,
        }
    }
}

/// Append single element to slice.
/// elem_bytes: actual byte size per element
/// val_ptr points to ceil(elem_bytes / SLOT_BYTES) u64 values.
#[no_mangle]
pub extern "C" fn vo_slice_append(gc: *mut Gc, elem_meta: u32, elem_bytes: u32, s: u64, val_ptr: *const u64) -> u64 {
    use crate::objects::slice;
    use crate::ValueMeta;
    unsafe {
        let gc = &mut *gc;
        let val_slots = slots_for_bytes(elem_bytes as usize);
        let val = core::slice::from_raw_parts(val_ptr, val_slots);
        slice::append(gc, ValueMeta::from_raw(elem_meta), elem_bytes as usize, s as crate::gc::GcRef, val) as u64
    }
}

/// Create slice from array range (arr[lo:hi]).
/// Returns u64::MAX on bounds error.
#[no_mangle]
pub extern "C" fn vo_slice_from_array(gc: *mut Gc, arr: u64, lo: u64, hi: u64) -> u64 {
    use crate::objects::slice;
    unsafe {
        let gc = &mut *gc;
        match slice::array_slice(gc, arr as crate::gc::GcRef, lo as usize, hi as usize) {
            Some(r) => r as u64,
            None => u64::MAX,
        }
    }
}

/// Create slice from array range with cap (arr[lo:hi:max]).
/// Returns u64::MAX on bounds error.
#[no_mangle]
pub extern "C" fn vo_slice_from_array3(gc: *mut Gc, arr: u64, lo: u64, hi: u64, max: u64) -> u64 {
    use crate::objects::slice;
    unsafe {
        let gc = &mut *gc;
        match slice::array_slice_with_cap(gc, arr as crate::gc::GcRef, lo as usize, hi as usize, max as usize) {
            Some(r) => r as u64,
            None => u64::MAX,
        }
    }
}

// =============================================================================
// Interface Helpers
// =============================================================================

/// Extract named_type_id from RuntimeType (recursively unwraps Pointer).
fn extract_named_type_id(rt: &crate::RuntimeType, runtime_types: &[crate::RuntimeType]) -> Option<u32> {
    use crate::RuntimeType;
    match rt {
        RuntimeType::Named { id, .. } => Some(*id),
        RuntimeType::Pointer(elem_value_rttid) => {
            runtime_types.get(elem_value_rttid.rttid() as usize)
                .and_then(|inner| extract_named_type_id(inner, runtime_types))
        }
        _ => None,
    }
}


/// Interface to interface assignment with runtime itab lookup.
/// src_slot0/slot1: source interface (2 slots)
/// iface_meta_id: target interface meta id
/// Returns: new slot0 with updated itab_id
#[no_mangle]
pub extern "C" fn vo_iface_to_iface(
    ctx: *mut JitContext,
    src_slot0: u64,
    iface_meta_id: u32,
) -> u64 {
    use crate::objects::interface;
    
    let src_rttid = interface::unpack_rttid(src_slot0);
    let src_vk = interface::unpack_value_kind(src_slot0);
    
    let ctx_ref = unsafe { &*ctx };
    let module = unsafe { &*ctx_ref.module };
    let itab_cache = unsafe { &mut *ctx_ref.itab_cache };
    
    let named_type_id_opt = module.runtime_types.get(src_rttid as usize)
        .and_then(|rt| extract_named_type_id(rt, &module.runtime_types));
    
    let new_itab_id = if let Some(named_type_id) = named_type_id_opt {
        // Value types (non-pointer) cannot use pointer receiver methods
        let src_is_pointer = src_vk == ValueKind::Pointer;
        itab_cache.get_or_create(
            named_type_id,
            iface_meta_id,
            src_is_pointer,
            &module.named_type_metas,
            &module.interface_metas,
        )
    } else {
        0
    };
    
    interface::pack_slot0(new_itab_id, src_rttid, src_vk)
}

/// Interface equality comparison.
/// Returns: 0 = not equal, 1 = equal, 2 = panic (uncomparable type)
#[no_mangle]
pub extern "C" fn vo_iface_eq(
    ctx: *mut JitContext,
    b_slot0: u64,
    b_slot1: u64,
    c_slot0: u64,
    c_slot1: u64,
) -> u64 {
    use crate::objects::compare;
    
    let ctx_ref = unsafe { &*ctx };
    let module = unsafe { &*ctx_ref.module };
    
    compare::iface_eq(b_slot0, b_slot1, c_slot0, c_slot1, module)
}

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
    
    let ctx_ref = unsafe { &*ctx };
    let module = unsafe { &*ctx_ref.module };
    
    // nil interface always fails
    let matches = if src_vk == ValueKind::Void {
        false
    } else if assert_kind == 0 {
        // Type comparison: check rttid
        src_rttid == target_id
    } else {
        // Interface method check - use shared function from itab module
        crate::itab::check_interface_satisfaction(src_rttid, src_vk, target_id, module)
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
        // Interface assertion: create new itab and write result
        let ctx_ref = &*ctx;
        let module = &*ctx_ref.module;
        let itab_cache = &mut *ctx_ref.itab_cache;
        
        let named_type_id = module.runtime_types.get(src_rttid as usize)
            .and_then(|rt| extract_named_type_id(rt, &module.runtime_types))
            .unwrap_or(0);
        // Value types (non-pointer) cannot use pointer receiver methods
        let src_is_pointer = src_vk == ValueKind::Pointer;
        let new_itab_id = itab_cache.get_or_create(
            named_type_id,
            target_id,
            src_is_pointer,
            &module.named_type_metas,
            &module.interface_metas,
        );
        let new_slot0 = interface::pack_slot0(new_itab_id, src_rttid, src_vk);
        *dst = new_slot0;
        *dst.add(1) = slot1;
    } else if src_vk == ValueKind::Struct {
        // Copy value from GcRef (struct layout: [GcHeader][data...])
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
    } else if src_vk == ValueKind::Array {
        // Copy elements from array (layout: [GcHeader][ArrayHeader][elements...])
        use crate::objects::array;
        let gc_ref = slot1 as GcRef;
        let slots = target_slots.max(1);
        if slot1 != 0 {
            let data_ptr = array::data_ptr_bytes(gc_ref) as *const u64;
            for i in 0..slots {
                *dst.add(i) = *data_ptr.add(i);
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
// Slice Copy
// =============================================================================

/// Copy elements from src slice to dst slice (for JIT).
/// Returns the number of elements copied (min of dst len and src len).
#[no_mangle]
pub extern "C" fn vo_copy(dst: u64, src: u64) -> u64 {
    use crate::objects::{slice, array};
    use crate::gc::GcRef;
    
    if dst == 0 || src == 0 {
        return 0;
    }
    
    let dst_ref = dst as GcRef;
    let src_ref = src as GcRef;
    
    let dst_len = slice::len(dst_ref);
    let src_len = slice::len(src_ref);
    let copy_len = dst_len.min(src_len);
    
    if copy_len == 0 {
        return 0;
    }
    
    let dst_arr = slice::array_ref(dst_ref);
    let elem_bytes = array::elem_bytes(dst_arr);
    let dst_ptr = slice::data_ptr(dst_ref);
    let src_ptr = slice::data_ptr(src_ref);
    
    unsafe { core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, copy_len * elem_bytes) };
    
    copy_len as u64
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
        ("vo_iface_to_iface", vo_iface_to_iface as *const u8),
        ("vo_iface_eq", vo_iface_eq as *const u8),
        ("vo_iface_assert", vo_iface_assert as *const u8),
        ("vo_ptr_clone", vo_ptr_clone as *const u8),
        ("vo_map_new", vo_map_new as *const u8),
        ("vo_map_len", vo_map_len as *const u8),
        ("vo_map_get", vo_map_get as *const u8),
        ("vo_map_set", vo_map_set as *const u8),
        ("vo_map_delete", vo_map_delete as *const u8),
        ("vo_map_iter_init", vo_map_iter_init as *const u8),
        ("vo_map_iter_next", vo_map_iter_next as *const u8),
        ("vo_copy", vo_copy as *const u8),
    ]
}

