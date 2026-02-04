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

/// Function pointer type for pushing a JIT frame.
/// Also updates caller's frame.pc to caller_resume_pc for nested Call handling.
/// Returns: args_ptr for the new frame (fiber.stack_ptr + new_bp)
pub type JitPushFrameFn = extern "C" fn(
    ctx: *mut JitContext,
    func_id: u32,
    local_slots: u32,
    ret_reg: u32,
    ret_slots: u32,
    caller_resume_pc: u32,
) -> *mut u64;

/// Function pointer type for popping a JIT frame.
/// Takes caller_bp as parameter since we don't store it in resume_stack anymore.
pub type JitPopFrameFn = extern "C" fn(ctx: *mut JitContext, caller_bp: u32);

/// Function pointer type for pushing a resume point on side-exit (Call/WaitIo).
/// Called by JIT code when callee returns non-OK, before propagating the result.
/// This is the "lazy push" - only called on the slow path.
pub type JitPushResumePointFn = extern "C" fn(
    ctx: *mut JitContext,
    func_id: u32,
    resume_pc: u32,
    bp: u32,
    caller_bp: u32,
    ret_reg: u32,
    ret_slots: u32,
);

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
    
    /// Call request: callee function ID (for non-jittable calls)
    pub call_func_id: u32,
    
    /// Call request: arg_start offset in JIT's local variable area
    pub call_arg_start: u16,
    
    /// Call request: resume PC for VM to continue execution
    pub call_resume_pc: u32,
    
    /// Call request: number of return slots caller expects
    pub call_ret_slots: u16,
    
    /// Call request: call kind (0=regular, 1=closure, 2=iface, 253=yield, 254=block)
    pub call_kind: u8,
    
    /// Call request: actual number of arg slots passed by caller
    pub call_arg_slots: u16,
    
    /// Call request: for closure calls, the closure GcRef
    /// Used to compute call_layout for slot0 and arg_offset
    pub call_closure_ref: u64,
    
    /// Call request: for iface calls, the receiver value (slot1)
    pub call_iface_recv: u64,
    
    /// I/O wait token (for WaitIo result).
    /// Set by jit_call_extern when extern returns WaitIo.
    #[cfg(feature = "std")]
    pub wait_io_token: u64,
    
    /// Loop OSR exit PC. Set by loop function on normal exit.
    /// When loop returns JitResult::Ok, this contains the PC to resume at.
    pub loop_exit_pc: u32,
    
    // =========================================================================
    // Fiber stack access (for JIT-to-JIT calls with proper frame management)
    // =========================================================================
    
    /// Pointer to fiber.stack base (fiber.stack.as_mut_ptr()).
    /// Updated by VM before each JIT call.
    pub stack_ptr: *mut u64,
    
    /// Capacity of fiber.stack (fiber.stack.len()).
    /// Allows JIT to check if frame push needs reallocation.
    pub stack_cap: u32,
    
    /// Current JIT frame base pointer (index into fiber.stack).
    /// Updated by vo_jit_push_frame / vo_jit_pop_frame.
    pub jit_bp: u32,
    
    /// Current stack pointer (fiber.sp). JIT uses this for fast path frame management.
    /// Updated inline by JIT code on fast path, or by push_frame/pop_frame on slow path.
    pub fiber_sp: u32,
    
    /// Callback to push a JIT frame for nested JIT-to-JIT calls.
    /// Set by VM when creating JitContext.
    pub push_frame_fn: Option<JitPushFrameFn>,
    
    /// Callback to pop a JIT frame after callee returns.
    /// Set by VM when creating JitContext.
    pub pop_frame_fn: Option<JitPopFrameFn>,
    
    /// Callback to push a resume point on side-exit (Call/WaitIo).
    /// Only called on the slow path when callee returns non-OK.
    pub push_resume_point_fn: Option<JitPushResumePointFn>,
    
    // =========================================================================
    // VM Callbacks for advanced opcodes (Batch 1+)
    // =========================================================================
    
    /// Callback to create a new island.
    /// Returns the island handle as u64.
    pub create_island_fn: Option<extern "C" fn(*mut JitContext) -> u64>,
    
    /// Callback to close a channel.
    /// Returns JitResult (Ok or Panic).
    pub chan_close_fn: Option<extern "C" fn(*mut JitContext, chan: u64) -> JitResult>,
    
    /// Callback to close a port.
    /// Returns JitResult (Ok or Panic).
    pub port_close_fn: Option<extern "C" fn(*mut JitContext, port: u64) -> JitResult>,
    
    // Batch 2: Channel Send/Recv
    /// Callback to send on a channel.
    /// Returns JitResult (Ok, Panic, or WaitIo).
    pub chan_send_fn: Option<extern "C" fn(*mut JitContext, chan: u64, val_ptr: *const u64, val_slots: u32) -> JitResult>,
    
    /// Callback to receive from a channel.
    /// Returns JitResult (Ok, Panic, or WaitIo).
    pub chan_recv_fn: Option<extern "C" fn(*mut JitContext, chan: u64, dst_ptr: *mut u64, elem_slots: u32, has_ok: u32) -> JitResult>,
    
    // Batch 3: Port Send/Recv
    /// Callback to send on a port.
    /// Returns JitResult (Ok, Panic, or WaitIo).
    pub port_send_fn: Option<extern "C" fn(*mut JitContext, port: u64, val_ptr: *const u64, val_slots: u32) -> JitResult>,
    
    /// Callback to receive from a port.
    /// Returns JitResult (Ok, Panic, or WaitIo).
    pub port_recv_fn: Option<extern "C" fn(*mut JitContext, port: u64, dst_ptr: *mut u64, elem_slots: u32, has_ok: u32) -> JitResult>,
    
    // Batch 4: Goroutine Start
    /// Callback to spawn a new goroutine.
    /// func_id: function to run, is_closure: 1 if closure, closure_ref: closure GcRef (or 0), 
    /// args_ptr: pointer to arguments, arg_slots: number of argument slots
    pub go_start_fn: Option<extern "C" fn(*mut JitContext, func_id: u32, is_closure: u32, closure_ref: u64, args_ptr: *const u64, arg_slots: u32)>,
}

/// JitContext field offsets for JIT compiler.
/// These must match the actual struct layout.
impl JitContext {
    pub const OFFSET_JIT_FUNC_TABLE: i32 = std::mem::offset_of!(JitContext, jit_func_table) as i32;
    pub const OFFSET_JIT_FUNC_COUNT: i32 = std::mem::offset_of!(JitContext, jit_func_count) as i32;
    pub const OFFSET_CALL_FUNC_ID: i32 = std::mem::offset_of!(JitContext, call_func_id) as i32;
    pub const OFFSET_CALL_ARG_START: i32 = std::mem::offset_of!(JitContext, call_arg_start) as i32;
    pub const OFFSET_CALL_RESUME_PC: i32 = std::mem::offset_of!(JitContext, call_resume_pc) as i32;
    pub const OFFSET_CALL_RET_SLOTS: i32 = std::mem::offset_of!(JitContext, call_ret_slots) as i32;
    pub const OFFSET_CALL_KIND: i32 = std::mem::offset_of!(JitContext, call_kind) as i32;
    pub const OFFSET_CALL_CLOSURE_REF: i32 = std::mem::offset_of!(JitContext, call_closure_ref) as i32;
    pub const OFFSET_CALL_IFACE_RECV: i32 = std::mem::offset_of!(JitContext, call_iface_recv) as i32;
    #[cfg(feature = "std")]
    pub const OFFSET_WAIT_IO_TOKEN: i32 = std::mem::offset_of!(JitContext, wait_io_token) as i32;
    pub const OFFSET_LOOP_EXIT_PC: i32 = std::mem::offset_of!(JitContext, loop_exit_pc) as i32;
    
    // JitResult constants for Call infrastructure
    pub const JIT_RESULT_OK: u32 = 0;
    pub const JIT_RESULT_PANIC: u32 = 1;
    pub const JIT_RESULT_CALL: u32 = 2;
    pub const JIT_RESULT_WAIT_IO: u32 = 3;

    // call_kind constants
    pub const CALL_KIND_REGULAR: u8 = 0;
    pub const CALL_KIND_CLOSURE: u8 = 1;
    pub const CALL_KIND_IFACE: u8 = 2;
    pub const CALL_KIND_YIELD: u8 = 253;
    pub const CALL_KIND_BLOCK: u8 = 254;

    // Fiber stack access offsets
    pub const OFFSET_STACK_PTR: i32 = std::mem::offset_of!(JitContext, stack_ptr) as i32;
    pub const OFFSET_STACK_CAP: i32 = std::mem::offset_of!(JitContext, stack_cap) as i32;
    pub const OFFSET_JIT_BP: i32 = std::mem::offset_of!(JitContext, jit_bp) as i32;
    pub const OFFSET_FIBER_SP: i32 = std::mem::offset_of!(JitContext, fiber_sp) as i32;
    pub const OFFSET_PUSH_FRAME_FN: i32 = std::mem::offset_of!(JitContext, push_frame_fn) as i32;
    pub const OFFSET_POP_FRAME_FN: i32 = std::mem::offset_of!(JitContext, pop_frame_fn) as i32;
    pub const OFFSET_PUSH_RESUME_POINT_FN: i32 = std::mem::offset_of!(JitContext, push_resume_point_fn) as i32;
    
    // VM callback offsets (Batch 1+)
    pub const OFFSET_CREATE_ISLAND_FN: i32 = std::mem::offset_of!(JitContext, create_island_fn) as i32;
    pub const OFFSET_CHAN_CLOSE_FN: i32 = std::mem::offset_of!(JitContext, chan_close_fn) as i32;
    pub const OFFSET_PORT_CLOSE_FN: i32 = std::mem::offset_of!(JitContext, port_close_fn) as i32;
    // Batch 2
    pub const OFFSET_CHAN_SEND_FN: i32 = std::mem::offset_of!(JitContext, chan_send_fn) as i32;
    pub const OFFSET_CHAN_RECV_FN: i32 = std::mem::offset_of!(JitContext, chan_recv_fn) as i32;
    // Batch 3
    pub const OFFSET_PORT_SEND_FN: i32 = std::mem::offset_of!(JitContext, port_send_fn) as i32;
    pub const OFFSET_PORT_RECV_FN: i32 = std::mem::offset_of!(JitContext, port_recv_fn) as i32;
    // Batch 4
    pub const OFFSET_GO_START_FN: i32 = std::mem::offset_of!(JitContext, go_start_fn) as i32;
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
    /// JIT requests VM to execute a call (non-jittable callee).
    /// call_func_id, call_arg_start, call_resume_pc in JitContext contain the info.
    Call = 2,
    /// JIT requests VM to wait for I/O completion.
    /// The IoToken is stored in JitContext.wait_io_token.
    /// After I/O completes, VM resumes execution in interpreter at call_resume_pc.
    WaitIo = 3,
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

/// GC safepoint - intentionally a no-op.
///
/// The synchronous JIT design with non-moving GC means stack maps are NOT needed:
///
/// 1. **Non-moving GC**: GcRef pointers never change, so JIT's SSA variables
///    holding GcRefs remain valid even after GC runs.
///
/// 2. **Reachability**: All GcRefs in JIT code originate from `fiber.stack`
///    (already tracked by GC) or from helper return values (already rooted).
///
/// 3. **GC triggers only at call boundaries**:
///    - `vo_gc_alloc` - checks debt and may trigger collection
///    - `vo_call_vm` - enters VM context where GC can run
///    - Other extern calls
///
/// This design eliminates the need for expensive stack maps while maintaining
/// GC correctness. The trade-off is that long JIT loops without allocations
/// will delay GC, but this only affects latency, not correctness.
#[no_mangle]
pub extern "C" fn vo_gc_safepoint(_ctx: *mut JitContext) {
    // Intentionally no-op. See doc comment for design rationale.
}

/// Call a VM-interpreted function from JIT code.
///
/// This is the fallback path when callee is not JIT-compiled.
/// JIT-to-JIT calls go directly through function pointers when available.
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

/// Set Call request state in JitContext.
/// Called by JIT when it needs to hand off to VM for a non-jittable callee.
/// 
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
#[no_mangle]
pub extern "C" fn vo_set_call_request(ctx: *mut JitContext, func_id: u32, arg_start: u32, resume_pc: u32, ret_slots: u32) {
    unsafe {
        (*ctx).call_func_id = func_id;
        (*ctx).call_arg_start = arg_start as u16;
        (*ctx).call_resume_pc = resume_pc;
        (*ctx).call_ret_slots = ret_slots as u16;
        (*ctx).call_kind = JitContext::CALL_KIND_REGULAR;
    }
}

/// Called by JIT for closure call that needs VM fallback.
/// 
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
#[no_mangle]
pub extern "C" fn vo_set_closure_call_request(
    ctx: *mut JitContext,
    func_id: u32,
    arg_start: u32,
    resume_pc: u32,
    ret_slots: u32,
    arg_slots: u32,
    closure_ref: u64,
) {
    unsafe {
        (*ctx).call_func_id = func_id;
        (*ctx).call_arg_start = arg_start as u16;
        (*ctx).call_resume_pc = resume_pc;
        (*ctx).call_ret_slots = ret_slots as u16;
        (*ctx).call_kind = JitContext::CALL_KIND_CLOSURE;
        (*ctx).call_arg_slots = arg_slots as u16;
        (*ctx).call_closure_ref = closure_ref;
    }
}

/// Called by JIT for interface call that needs VM fallback.
/// 
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
#[no_mangle]
pub extern "C" fn vo_set_iface_call_request(
    ctx: *mut JitContext,
    func_id: u32,
    arg_start: u32,
    resume_pc: u32,
    ret_slots: u32,
    arg_slots: u32,
    iface_recv: u64,
) {
    unsafe {
        (*ctx).call_func_id = func_id;
        (*ctx).call_arg_start = arg_start as u16;
        (*ctx).call_resume_pc = resume_pc;
        (*ctx).call_ret_slots = ret_slots as u16;
        (*ctx).call_kind = JitContext::CALL_KIND_IFACE;
        (*ctx).call_arg_slots = arg_slots as u16;
        (*ctx).call_iface_recv = iface_recv;
    }
}

/// Push a new frame for JIT-to-JIT call.
/// 
/// This function:
/// 1. Ensures fiber.stack has capacity for local_slots
/// 2. Zeros the new frame region
/// 3. Updates fiber.sp
/// 4. Pushes CallFrame to fiber.frames
/// 5. Updates ctx.jit_bp and ctx.stack_ptr (in case of reallocation)
///
/// # Returns
/// args_ptr for the new frame (fiber.stack_ptr + new_bp)
///
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
/// - `ctx.push_frame_fn` must be set
#[no_mangle]
pub extern "C" fn vo_jit_push_frame(
    ctx: *mut JitContext,
    func_id: u32,
    local_slots: u32,
    ret_reg: u32,
    ret_slots: u32,
    caller_resume_pc: u32,
) -> *mut u64 {
    let ctx_ref = unsafe { &*ctx };
    match ctx_ref.push_frame_fn {
        Some(f) => f(ctx, func_id, local_slots, ret_reg, ret_slots, caller_resume_pc),
        None => core::ptr::null_mut(),
    }
}

/// Pop the current JIT frame after callee returns.
/// Restores ctx.jit_bp to caller's bp.
///
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
/// - `ctx.pop_frame_fn` must be set
#[no_mangle]
pub extern "C" fn vo_jit_pop_frame(ctx: *mut JitContext, caller_bp: u32) {
    let ctx_ref = unsafe { &*ctx };
    if let Some(f) = ctx_ref.pop_frame_fn {
        f(ctx, caller_bp);
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

/// Get func_id from closure. Returns 0 if closure_ref is nil.
#[no_mangle]
pub extern "C" fn vo_closure_get_func_id(closure_ref: u64) -> u32 {
    use crate::objects::closure;
    use crate::gc::GcRef;
    
    if closure_ref == 0 {
        return 0; // Will cause nil pointer panic in JIT
    }
    
    closure::func_id(closure_ref as GcRef)
}

/// Get func_id from interface method. Looks up via itab cache.
#[no_mangle]
pub extern "C" fn vo_iface_get_func_id(ctx: *mut JitContext, slot0: u64, method_idx: u32) -> u32 {
    use crate::objects::interface;
    
    let ctx_ref = unsafe { &*ctx };
    let itab_cache = unsafe { &*ctx_ref.itab_cache };
    
    let itab_id = interface::unpack_itab_id(slot0);
    itab_cache.lookup_method(itab_id, method_idx as usize)
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
pub extern "C" fn vo_map_get(ctx: *mut JitContext, m: u64, key_ptr: *const u64, key_slots: u32, val_ptr: *mut u64, val_slots: u32) -> u64 {
    use crate::objects::map;
    if m == 0 {
        // nil map read returns zero value (Go semantics)
        unsafe { core::ptr::write_bytes(val_ptr, 0, val_slots as usize); }
        return 0;
    }
    
    let module = unsafe { (*ctx).module.as_ref() };
    let key = unsafe { core::slice::from_raw_parts(key_ptr, key_slots as usize) };
    let (val_opt, ok) = map::get_with_ok(m as crate::gc::GcRef, key, module);
    
    if let Some(val) = val_opt {
        let copy_len = (val_slots as usize).min(val.len());
        unsafe {
            core::ptr::copy_nonoverlapping(val.as_ptr(), val_ptr, copy_len);
        }
    } else {
        // Zero out val_ptr buffer for non-existent keys
        unsafe {
            core::ptr::write_bytes(val_ptr, 0, val_slots as usize);
        }
    }
    ok as u64
}

/// Set value in map.
/// Returns: 0 = success, 1 = panic (interface key with uncomparable type)
#[no_mangle]
pub extern "C" fn vo_map_set(ctx: *mut JitContext, m: u64, key_ptr: *const u64, key_slots: u32, val_ptr: *const u64, val_slots: u32) -> u64 {
    use crate::objects::{map, interface};
    use crate::ValueKind;
    if m == 0 { return 0; }
    
    let module = unsafe { (*ctx).module.as_ref() };
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
    
    map::set(m as crate::gc::GcRef, key, val, module);
    0
}

/// Delete key from map.
#[no_mangle]
pub extern "C" fn vo_map_delete(ctx: *mut JitContext, m: u64, key_ptr: *const u64, key_slots: u32) {
    use crate::objects::map;
    if m == 0 { return; }
    
    let module = unsafe { (*ctx).module.as_ref() };
    let key = unsafe { core::slice::from_raw_parts(key_ptr, key_slots as usize) };
    map::delete(m as crate::gc::GcRef, key, module);
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
// Port Helpers
// =============================================================================

/// Create a new port.
#[no_mangle]
pub extern "C" fn vo_port_new(gc: *mut Gc, elem_meta: u32, elem_slots: u32, cap: u64) -> u64 {
    use crate::objects::port;
    use crate::ValueMeta;
    unsafe {
        let gc = &mut *gc;
        port::create(gc, ValueMeta::from_raw(elem_meta), elem_slots as u16, cap as usize) as u64
    }
}

/// Get port length (number of elements in buffer).
#[no_mangle]
pub extern "C" fn vo_port_len(p: u64) -> u64 {
    use crate::objects::port;
    use crate::gc::GcRef;
    let p = p as GcRef;
    if p.is_null() { 0 } else { port::len(p) as u64 }
}

/// Get port capacity.
#[no_mangle]
pub extern "C" fn vo_port_cap(p: u64) -> u64 {
    use crate::objects::queue_state;
    use crate::gc::GcRef;
    let p = p as GcRef;
    if p.is_null() { 0 } else { queue_state::capacity(p) as u64 }
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
        ("vo_closure_get_func_id", vo_closure_get_func_id as *const u8),
        ("vo_iface_get_func_id", vo_iface_get_func_id as *const u8),
        ("vo_set_closure_call_request", vo_set_closure_call_request as *const u8),
        ("vo_set_iface_call_request", vo_set_iface_call_request as *const u8),
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
        // Batch 1: Island/Channel/Port operations
        ("vo_island_new", vo_island_new as *const u8),
        ("vo_chan_close", vo_chan_close as *const u8),
        ("vo_port_close", vo_port_close as *const u8),
        // Batch 2: Channel Send/Recv
        ("vo_chan_send", vo_chan_send as *const u8),
        ("vo_chan_recv", vo_chan_recv as *const u8),
        // Batch 3: Port Send/Recv
        ("vo_port_send", vo_port_send as *const u8),
        ("vo_port_recv", vo_port_recv as *const u8),
        // Batch 4: Goroutine Start
        ("vo_go_start", vo_go_start as *const u8),
    ]
}

// =============================================================================
// Batch 1: Island/Channel/Port JIT Helpers
// =============================================================================

/// Create a new island.
/// Calls into VM via callback to properly register with scheduler.
/// Returns the island handle.
#[no_mangle]
pub extern "C" fn vo_island_new(ctx: *mut JitContext) -> u64 {
    let ctx = unsafe { &mut *ctx };
    let create_fn = ctx.create_island_fn.expect("create_island_fn not set");
    create_fn(ctx)
}

/// Close a channel.
/// Returns JitResult::Ok on success, JitResult::Panic on nil/closed channel.
#[no_mangle]
pub extern "C" fn vo_chan_close(ctx: *mut JitContext, chan: u64) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let close_fn = ctx.chan_close_fn.expect("chan_close_fn not set");
    close_fn(ctx, chan)
}

/// Close a port.
/// Returns JitResult::Ok on success, JitResult::Panic on nil/closed port.
#[no_mangle]
pub extern "C" fn vo_port_close(ctx: *mut JitContext, port: u64) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let close_fn = ctx.port_close_fn.expect("port_close_fn not set");
    close_fn(ctx, port)
}

// =============================================================================
// Batch 2: Channel Send/Recv
// =============================================================================

/// Send on a channel.
/// Returns JitResult::Ok on success, JitResult::Panic on nil/closed channel,
/// or JitResult::WaitIo if the send would block.
#[no_mangle]
pub extern "C" fn vo_chan_send(ctx: *mut JitContext, chan: u64, val_ptr: *const u64, val_slots: u32) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let send_fn = ctx.chan_send_fn.expect("chan_send_fn not set");
    send_fn(ctx, chan, val_ptr, val_slots)
}

/// Receive from a channel.
/// Returns JitResult::Ok on success (including closed channel),
/// JitResult::Panic on nil channel, or JitResult::WaitIo if would block.
#[no_mangle]
pub extern "C" fn vo_chan_recv(ctx: *mut JitContext, chan: u64, dst_ptr: *mut u64, elem_slots: u32, has_ok: u32) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let recv_fn = ctx.chan_recv_fn.expect("chan_recv_fn not set");
    recv_fn(ctx, chan, dst_ptr, elem_slots, has_ok)
}

// =============================================================================
// Batch 3: Port Send/Recv
// =============================================================================

/// Send on a port.
/// Returns JitResult::Ok on success, JitResult::Panic on closed port,
/// or JitResult::WaitIo if the send would block.
#[no_mangle]
pub extern "C" fn vo_port_send(ctx: *mut JitContext, port: u64, val_ptr: *const u64, val_slots: u32) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let send_fn = ctx.port_send_fn.expect("port_send_fn not set");
    send_fn(ctx, port, val_ptr, val_slots)
}

/// Receive from a port.
/// Returns JitResult::Ok on success (including closed port),
/// or JitResult::WaitIo if would block.
#[no_mangle]
pub extern "C" fn vo_port_recv(ctx: *mut JitContext, port: u64, dst_ptr: *mut u64, elem_slots: u32, has_ok: u32) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let recv_fn = ctx.port_recv_fn.expect("port_recv_fn not set");
    recv_fn(ctx, port, dst_ptr, elem_slots, has_ok)
}

// =============================================================================
// Batch 4: Goroutine Start
// =============================================================================

/// Spawn a new goroutine.
/// This is fire-and-forget - the new fiber runs concurrently.
#[no_mangle]
pub extern "C" fn vo_go_start(
    ctx: *mut JitContext,
    func_id: u32,
    is_closure: u32,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_slots: u32,
) {
    let ctx = unsafe { &mut *ctx };
    let go_fn = ctx.go_start_fn.expect("go_start_fn not set");
    go_fn(ctx, func_id, is_closure, closure_ref, args_ptr, arg_slots)
}
