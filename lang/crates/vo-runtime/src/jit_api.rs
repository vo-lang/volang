#![allow(clippy::not_unsafe_ptr_arg_deref)]
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
use crate::itab::ItabCache;
use crate::objects::interface::InterfaceSlot;
use crate::slot::slots_for_bytes;
use crate::ValueKind;
use vo_common_core::bytecode::Module;

// =============================================================================
// JitContext
// =============================================================================

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JitRuntimeTrapKind {
    None = 0,
    NilPointerDereference = 1,
    NilMapWrite = 2,
    UnhashableType = 3,
    UncomparableType = 4,
    NegativeShift = 5,
    NilFuncCall = 6,
    TypeAssertionFailed = 7,
    DivisionByZero = 8,
    IndexOutOfBounds = 9,
    SliceBoundsOutOfRange = 10,
    MakeSlice = 11,
    MakeChan = 12,
    MakePort = 13,
    SendOnClosedChannel = 14,
    SendOnNilChannel = 15,
    RecvOnNilChannel = 16,
    CloseNilChannel = 17,
    CloseClosedChannel = 18,
    StackOverflow = 19,
}

impl JitRuntimeTrapKind {
    pub fn from_u8(raw: u8) -> Option<Self> {
        match raw {
            0 => Some(Self::None),
            1 => Some(Self::NilPointerDereference),
            2 => Some(Self::NilMapWrite),
            3 => Some(Self::UnhashableType),
            4 => Some(Self::UncomparableType),
            5 => Some(Self::NegativeShift),
            6 => Some(Self::NilFuncCall),
            7 => Some(Self::TypeAssertionFailed),
            8 => Some(Self::DivisionByZero),
            9 => Some(Self::IndexOutOfBounds),
            10 => Some(Self::SliceBoundsOutOfRange),
            11 => Some(Self::MakeSlice),
            12 => Some(Self::MakeChan),
            13 => Some(Self::MakePort),
            14 => Some(Self::SendOnClosedChannel),
            15 => Some(Self::SendOnNilChannel),
            16 => Some(Self::RecvOnNilChannel),
            17 => Some(Self::CloseNilChannel),
            18 => Some(Self::CloseClosedChannel),
            19 => Some(Self::StackOverflow),
            _ => None,
        }
    }
}

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

/// Function pointer type for popping a prepared JIT frame on the OK path.
/// Takes caller_bp directly because no resume point is pushed unless a side-exit happens.
pub type JitPopFrameFn = extern "C" fn(ctx: *mut JitContext, caller_bp: u32);

/// Function pointer type for converting a JIT stack guard failure into a
/// recoverable runtime panic.
pub type JitStackOverflowFn = extern "C" fn(ctx: *mut JitContext) -> JitResult;

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
) -> JitResult;

/// Monomorphic inline cache entry for dynamic calls (closure/iface).
///
/// Each JIT callsite hashes to an IC entry. On hit, the JIT fast path
/// skips the prepare callback entirely and does a direct JIT-to-JIT call
/// with native stack args.
///
/// Layout: 40 bytes, 8-aligned.
#[derive(Debug)]
#[repr(C)]
pub struct DynCallIC {
    /// Tagged cache key: closure func_id or full iface (itab_id, method_idx).
    /// 0 = empty/invalid entry.
    pub key: u64,
    /// Cached JIT function pointer (0 = callee not compiled or not in direct_call_table).
    pub jit_func_ptr: u64,
    /// Callee's local_slots count.
    pub local_slots: u32,
    /// Offset where user args start in callee frame (0, 1, or recv_slots).
    pub arg_offset: u32,
    /// What to place in slot0:
    ///   0 = nothing (named function wrapper)
    ///   1 = closure_ref (anonymous closure or closure with captures)
    ///   2 = captures[0] (method closure - load from closure GcRef + HEADER_SLOTS*8)
    ///   3 = iface receiver (iface_slot1)
    pub slot0_kind: u32,
    /// Resolved func_id (same as key for closure; for iface, resolved via itab).
    pub func_id: u32,
    /// Whether callee is a leaf function (no Call/CallClosure/CallIface/CallExtern).
    /// Leaf callees never read ctx.jit_bp/fiber_sp, so IC hit can skip ctx stores.
    pub is_leaf: u32,
}

impl Default for DynCallIC {
    fn default() -> Self {
        // All zeros = empty/invalid entry (key=0)
        unsafe { core::mem::zeroed() }
    }
}

impl DynCallIC {
    pub const SIZE: usize = core::mem::size_of::<DynCallIC>();
    pub const OFFSET_KEY: i32 = core::mem::offset_of!(DynCallIC, key) as i32;
    pub const OFFSET_JIT_FUNC_PTR: i32 = core::mem::offset_of!(DynCallIC, jit_func_ptr) as i32;
    pub const OFFSET_LOCAL_SLOTS: i32 = core::mem::offset_of!(DynCallIC, local_slots) as i32;
    pub const OFFSET_ARG_OFFSET: i32 = core::mem::offset_of!(DynCallIC, arg_offset) as i32;
    pub const OFFSET_SLOT0_KIND: i32 = core::mem::offset_of!(DynCallIC, slot0_kind) as i32;
    pub const OFFSET_FUNC_ID: i32 = core::mem::offset_of!(DynCallIC, func_id) as i32;
    pub const OFFSET_IS_LEAF: i32 = core::mem::offset_of!(DynCallIC, is_leaf) as i32;

    pub const SLOT0_NONE: u32 = 0;
    pub const SLOT0_CLOSURE_REF: u32 = 1;
    pub const SLOT0_CAPTURE0: u32 = 2;
    pub const SLOT0_IFACE_RECEIVER: u32 = 3;

    pub const KEY_KIND_SHIFT: u64 = 56;
    pub const KEY_KIND_CLOSURE: u64 = 1;
    pub const KEY_KIND_IFACE: u64 = 2;

    /// IC table size (must be power of 2).
    pub const TABLE_SIZE: usize = 512;
    pub const TABLE_MASK: u32 = (Self::TABLE_SIZE - 1) as u32;

    #[inline]
    pub const fn closure_key(func_id: u32) -> u64 {
        (Self::KEY_KIND_CLOSURE << Self::KEY_KIND_SHIFT) | func_id as u64
    }

    #[inline]
    pub const fn iface_key(itab_id: u32, method_idx: u32) -> u64 {
        (Self::KEY_KIND_IFACE << Self::KEY_KIND_SHIFT)
            | ((method_idx as u64) << 32)
            | itab_id as u64
    }
}

const _: () = assert!(DynCallIC::SIZE == 40);
const _: () = assert!(DynCallIC::TABLE_SIZE.is_power_of_two());

/// Allocate a zeroed IC table with TABLE_SIZE entries.
pub fn alloc_ic_table() -> Vec<DynCallIC> {
    let mut table = Vec::with_capacity(DynCallIC::TABLE_SIZE);
    unsafe {
        core::ptr::write_bytes(table.as_mut_ptr(), 0, DynCallIC::TABLE_SIZE);
        table.set_len(DynCallIC::TABLE_SIZE);
    }
    table
}

/// Result of preparing a closure or interface call.
/// Note: SIZE must match core::mem::size_of::<PreparedCall>() (checked below).
/// Contains information needed for JIT-to-JIT direct call or VM call materialization.
#[repr(C)]
pub struct PreparedCall {
    /// JIT function pointer for direct call. Null if callee should use trampoline.
    pub jit_func_ptr: *const u8,
    /// Pointer to callee's args in fiber.stack (after push_frame).
    /// Only valid if jit_func_ptr is not null.
    pub callee_args_ptr: *mut u64,
    /// Pointer to ret slot (native stack, for JIT call to write returns).
    pub ret_ptr: *mut u64,
    /// Callee's local_slots (for caller_bp restoration).
    pub callee_local_slots: u32,
    /// Resolved func_id (for VM call materialization).
    pub func_id: u32,
    /// Offset where user args start in callee frame.
    pub arg_offset: u32,
    /// slot0_kind (see DynCallIC::SLOT0_*).
    pub slot0_kind: u32,
    /// Whether callee is a leaf function (no Call/CallClosure/CallIface/CallExtern).
    pub is_leaf: u32,
}

impl PreparedCall {
    pub const OFFSET_JIT_FUNC_PTR: i32 = core::mem::offset_of!(PreparedCall, jit_func_ptr) as i32;
    pub const OFFSET_CALLEE_ARGS_PTR: i32 =
        core::mem::offset_of!(PreparedCall, callee_args_ptr) as i32;
    pub const OFFSET_RET_PTR: i32 = core::mem::offset_of!(PreparedCall, ret_ptr) as i32;
    pub const OFFSET_CALLEE_LOCAL_SLOTS: i32 =
        core::mem::offset_of!(PreparedCall, callee_local_slots) as i32;
    pub const OFFSET_FUNC_ID: i32 = core::mem::offset_of!(PreparedCall, func_id) as i32;
    pub const OFFSET_ARG_OFFSET: i32 = core::mem::offset_of!(PreparedCall, arg_offset) as i32;
    pub const OFFSET_SLOT0_KIND: i32 = core::mem::offset_of!(PreparedCall, slot0_kind) as i32;
    pub const OFFSET_IS_LEAF: i32 = core::mem::offset_of!(PreparedCall, is_leaf) as i32;
    pub const SIZE: usize = core::mem::size_of::<PreparedCall>();

    /// Create a result whose callee must be materialized through the VM call trampoline.
    pub fn vm_materialization(func_id: u32, callee_local_slots: u32) -> Self {
        PreparedCall {
            jit_func_ptr: core::ptr::null(),
            callee_args_ptr: core::ptr::null_mut(),
            ret_ptr: core::ptr::null_mut(),
            callee_local_slots,
            func_id,
            arg_offset: 0,
            slot0_kind: DynCallIC::SLOT0_NONE,
            is_leaf: 0,
        }
    }
}

const _: () = assert!(PreparedCall::SIZE == core::mem::size_of::<PreparedCall>());
const _: () = assert!(PreparedCall::SIZE == 48);

/// Function pointer type for preparing a closure call.
/// Writes result to `out` pointer instead of returning struct (avoids ABI mismatch
/// on SystemV x86_64 where structs > 16 bytes use sret).
pub type PrepareClosureCallFn = extern "C" fn(
    ctx: *mut JitContext,
    closure_ref: u64,
    ret_reg: u32,
    ret_slots: u32,
    caller_resume_pc: u32,
    user_args: *const u64,
    user_arg_count: u32,
    ret_ptr: *mut u64,
    out: *mut PreparedCall,
) -> JitResult;

/// Function pointer type for preparing an interface method call.
/// Writes result to `out` pointer instead of returning struct.
pub type PrepareIfaceCallFn = extern "C" fn(
    ctx: *mut JitContext,
    iface_slot0: u64,
    iface_slot1: u64,
    method_idx: u32,
    ret_reg: u32,
    ret_slots: u32,
    caller_resume_pc: u32,
    user_args: *const u64,
    user_arg_count: u32,
    ret_ptr: *mut u64,
    out: *mut PreparedCall,
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

    /// Flag indicating this is a user panic (via panic() call), not a runtime error.
    /// When true, panic_msg contains the user-provided value (may be nil).
    /// When false, panic_msg should be ignored and a default runtime error message created.
    pub is_user_panic: *mut bool,

    /// Panic message (interface as InterfaceSlot, set by vo_panic for user panics).
    pub panic_msg: *mut InterfaceSlot,

    /// Bytecode pc for user panics raised by JIT code or extern callbacks.
    ///
    /// This is deliberately separate from `call_resume_pc`, which is owned by
    /// WaitIo/Replay/call materialization, and from `runtime_trap_pc`, which is
    /// owned by typed runtime traps.
    pub user_panic_pc: u32,

    /// Runtime trap kind for JIT-generated runtime panics.
    ///
    /// `JitRuntimeTrapKind::None` means no typed runtime trap was recorded. The
    /// arg fields carry trap-specific dynamic details, such as index and length
    /// for bounds panics. `runtime_trap_pc` is the bytecode pc that caused the
    /// trap, or `u32::MAX` when unknown.
    pub runtime_trap_kind: u8,
    pub runtime_trap_arg0: u64,
    pub runtime_trap_arg1: u64,
    pub runtime_trap_pc: u32,

    /// Opaque pointer to VM instance.
    /// Cast to `*mut Vm` in trampoline code.
    pub vm: *mut c_void,

    /// Opaque pointer to current Fiber.
    /// Cast to `*mut Fiber` in trampoline code.
    pub fiber: *mut c_void,

    /// Pointer to ItabCache for interface method dispatch and dynamic itab creation.
    pub itab_cache: *mut ItabCache,

    /// Pointer to ExternRegistry for calling extern functions.
    pub extern_registry: *const c_void,

    /// Callback to call extern function: (ctx, registry, gc, module, extern_id, args, arg_count, ret, ret_slots) -> JitResult
    pub call_extern_fn: Option<
        extern "C" fn(
            *mut JitContext,
            *const c_void,
            *mut Gc,
            *const c_void,
            u32,
            *const u64,
            u32,
            *mut u64,
            u32,
        ) -> JitResult,
    >,

    /// Pointer to Module for type information.
    pub module: *const Module,

    /// JIT function pointer table: jit_func_table[func_id] = pointer to JIT function (or null if not compiled).
    /// Used for direct JIT-to-JIT calls without going through VM trampoline.
    pub jit_func_table: *const *const u8,

    /// Number of functions (length of jit_func_table).
    pub jit_func_count: u32,

    /// Direct call table: only contains entries for functions that pass the
    /// JIT effect-contract frame-elision predicate. Other compiled functions
    /// may still run through the prepared VM-frame path.
    pub direct_call_table: *const *const u8,

    /// Number of entries in direct_call_table.
    pub direct_call_count: u32,

    /// Pointer to program arguments.
    pub program_args: *const Vec<String>,

    /// Pointer to sentinel error cache.
    pub sentinel_errors: *mut crate::ffi::SentinelErrorCache,

    /// Pointer to the VM's output sink (trait object data+vtable).
    /// Avoids dereferencing `vm` (which would alias `gc`) just to read output.
    pub output: *const dyn crate::output::OutputSink,

    /// Pointer to host output channel (FFI → Host byte output).
    pub host_output: *mut Option<Vec<u8>>,

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

    /// Call request: ret_reg offset in caller's frame where return values should go.
    /// For REGULAR calls, this may differ from call_arg_start (inst.a vs inst.c).
    /// For PREPARED calls, this equals call_arg_start.
    pub call_ret_reg: u16,

    /// Call request: call kind (0=regular, 253=yield, 254=block)
    pub call_kind: u8,

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

    /// Maximum stack slots allowed for direct JIT native-stack call chains.
    pub stack_limit: u32,

    /// Current direct JIT call depth.
    pub call_depth: u32,

    /// Maximum direct JIT call depth before reporting stack overflow.
    pub call_depth_limit: u32,

    /// Current JIT frame base pointer (index into fiber.stack).
    /// Updated by push_frame_fn / pop_frame_fn callbacks.
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

    /// Callback used by generated stack guards to report stack overflow.
    pub stack_overflow_fn: Option<JitStackOverflowFn>,

    /// Callback to push a resume point on side-exit (Call/WaitIo).
    /// Only called on the slow path when callee returns non-OK.
    pub push_resume_point_fn: Option<JitPushResumePointFn>,

    // =========================================================================
    // VM Callbacks for advanced opcodes
    // =========================================================================
    /// Callback to create a new island.
    /// Returns the island handle as u64.
    pub create_island_fn: Option<extern "C" fn(*mut JitContext) -> u64>,

    /// Callback to close a channel.
    /// Returns JitResult (Ok or Panic).
    pub queue_close_fn: Option<extern "C" fn(*mut JitContext, chan: u64) -> JitResult>,

    /// Callback to send on a channel.
    /// Returns JitResult (Ok, Panic, or WaitIo).
    pub queue_send_fn: Option<
        extern "C" fn(*mut JitContext, chan: u64, val_ptr: *const u64, val_slots: u32) -> JitResult,
    >,

    /// Callback to receive from a channel.
    /// Returns JitResult (Ok, Panic, or WaitIo).
    pub queue_recv_fn: Option<
        extern "C" fn(
            *mut JitContext,
            chan: u64,
            dst_ptr: *mut u64,
            elem_slots: u32,
            has_ok: u32,
        ) -> JitResult,
    >,

    /// Callback to spawn a new goroutine.
    /// func_id: function to run, is_closure: 1 if closure, closure_ref: closure GcRef (or 0),
    /// args_ptr: pointer to arguments, arg_slots: number of argument slots
    pub go_start_fn: Option<
        extern "C" fn(
            *mut JitContext,
            func_id: u32,
            is_closure: u32,
            closure_ref: u64,
            args_ptr: *const u64,
            arg_slots: u32,
        ) -> JitResult,
    >,

    /// Callback to spawn a goroutine on a specific island.
    /// island: island handle, closure_ref: closure GcRef, args_ptr: arguments, arg_slots: count
    pub go_island_fn: Option<
        extern "C" fn(
            *mut JitContext,
            island: u64,
            closure_ref: u64,
            args_ptr: *const u64,
            arg_slots: u32,
        ) -> JitResult,
    >,

    // =========================================================================
    // Defer/Recover Support
    // =========================================================================
    /// Callback to push a defer entry.
    /// func_id: function to call (0 if closure), is_closure: 1 if closure,
    /// closure_ref: closure GcRef (or 0), args_ptr: captured args, arg_count: number of args,
    /// is_errdefer: 1 for errdefer
    pub defer_push_fn: Option<
        extern "C" fn(
            ctx: *mut JitContext,
            func_id: u32,
            is_closure: u32,
            closure_ref: u64,
            arg_start: u32,
            args_ptr: *const u64,
            arg_count: u32,
            is_errdefer: u32,
        ) -> JitResult,
    >,

    /// Callback for recover() - writes result to output (2 slots).
    pub recover_fn: Option<extern "C" fn(ctx: *mut JitContext, result_ptr: *mut u64) -> JitResult>,

    // =========================================================================
    // Select Statement Support
    // =========================================================================
    /// Callback to initialize a select statement.
    pub select_begin_fn:
        Option<extern "C" fn(ctx: *mut JitContext, case_count: u32, has_default: u32) -> JitResult>,

    /// Callback to add a send case to select.
    pub select_send_fn: Option<
        extern "C" fn(
            ctx: *mut JitContext,
            queue_reg: u32,
            val_reg: u32,
            elem_slots: u32,
            case_idx: u32,
        ) -> JitResult,
    >,

    /// Callback to add a recv case to select.
    pub select_recv_fn: Option<
        extern "C" fn(
            ctx: *mut JitContext,
            dst_reg: u32,
            queue_reg: u32,
            elem_slots: u32,
            has_ok: u32,
            case_idx: u32,
        ) -> JitResult,
    >,

    /// Callback to execute select statement.
    /// Returns JitResult::Ok (with result in result_reg), WaitIo (blocked), or Panic.
    pub select_exec_fn: Option<extern "C" fn(ctx: *mut JitContext, result_reg: u32) -> JitResult>,

    /// Set by JIT Return to indicate explicit `fail` return (for errdefer).
    pub is_error_return: u8,

    /// For heap returns: starting register of GcRefs.
    /// Used when function has heap-escaped named returns and defers.
    pub ret_gcref_start: u16,

    /// True if function uses heap returns (named returns that escaped).
    /// VM reads GcRefs from fiber.stack instead of ret buffer when this is set.
    pub ret_is_heap: u8,

    /// Starting slot of return values (from Return instruction's inst.a).
    /// Used by VM to extract ret_slot_types from func.slot_types for GC scanning.
    pub ret_start: u16,

    // =========================================================================
    // JIT-to-JIT Direct Call for Closure/Iface (Phase 2)
    // =========================================================================
    /// Callback to prepare a closure call for JIT-to-JIT dispatch.
    /// Resolves func_id, checks jit_func_table, does push_frame + arg layout.
    /// Returns PreparedCall with jit_func_ptr set if direct call is possible.
    pub prepare_closure_call_fn: Option<PrepareClosureCallFn>,

    /// Callback to prepare an interface method call for JIT-to-JIT dispatch.
    pub prepare_iface_call_fn: Option<PrepareIfaceCallFn>,

    // =========================================================================
    // Monomorphic Inline Cache for dynamic calls
    // =========================================================================
    /// Pointer to DynCallIC table (DynCallIC::TABLE_SIZE entries).
    /// JIT callsites hash into this table for fast-path dispatch.
    pub ic_table: *mut DynCallIC,
}

/// JitContext field offsets for JIT compiler.
/// These must match the actual struct layout.
impl JitContext {
    pub fn validate_required_callbacks(&self) -> Result<(), &'static str> {
        for field in jit_callback_abi_fields() {
            if field.kind.is_missing(self) {
                return Err(field.name);
            }
        }
        Ok(())
    }

    pub const OFFSET_JIT_FUNC_TABLE: i32 = std::mem::offset_of!(JitContext, jit_func_table) as i32;
    pub const OFFSET_JIT_FUNC_COUNT: i32 = std::mem::offset_of!(JitContext, jit_func_count) as i32;
    pub const OFFSET_DIRECT_CALL_TABLE: i32 =
        std::mem::offset_of!(JitContext, direct_call_table) as i32;
    pub const OFFSET_DIRECT_CALL_COUNT: i32 =
        std::mem::offset_of!(JitContext, direct_call_count) as i32;
    pub const OFFSET_CALL_FUNC_ID: i32 = std::mem::offset_of!(JitContext, call_func_id) as i32;
    pub const OFFSET_CALL_ARG_START: i32 = std::mem::offset_of!(JitContext, call_arg_start) as i32;
    pub const OFFSET_CALL_RESUME_PC: i32 = std::mem::offset_of!(JitContext, call_resume_pc) as i32;
    pub const OFFSET_CALL_RET_SLOTS: i32 = std::mem::offset_of!(JitContext, call_ret_slots) as i32;
    pub const OFFSET_CALL_RET_REG: i32 = std::mem::offset_of!(JitContext, call_ret_reg) as i32;
    pub const OFFSET_CALL_KIND: i32 = std::mem::offset_of!(JitContext, call_kind) as i32;
    pub const OFFSET_RUNTIME_TRAP_KIND: i32 =
        std::mem::offset_of!(JitContext, runtime_trap_kind) as i32;
    pub const OFFSET_RUNTIME_TRAP_ARG0: i32 =
        std::mem::offset_of!(JitContext, runtime_trap_arg0) as i32;
    pub const OFFSET_RUNTIME_TRAP_ARG1: i32 =
        std::mem::offset_of!(JitContext, runtime_trap_arg1) as i32;
    pub const OFFSET_RUNTIME_TRAP_PC: i32 =
        std::mem::offset_of!(JitContext, runtime_trap_pc) as i32;
    pub const OFFSET_USER_PANIC_PC: i32 = std::mem::offset_of!(JitContext, user_panic_pc) as i32;
    #[cfg(feature = "std")]
    pub const OFFSET_WAIT_IO_TOKEN: i32 = std::mem::offset_of!(JitContext, wait_io_token) as i32;
    pub const OFFSET_LOOP_EXIT_PC: i32 = std::mem::offset_of!(JitContext, loop_exit_pc) as i32;

    // JitResult constants for Call infrastructure
    pub const JIT_RESULT_OK: u32 = 0;
    pub const JIT_RESULT_PANIC: u32 = 1;
    pub const JIT_RESULT_CALL: u32 = 2;
    pub const JIT_RESULT_WAIT_IO: u32 = 3;
    pub const JIT_RESULT_WAIT_QUEUE: u32 = 4;
    pub const JIT_RESULT_REPLAY: u32 = 5;
    pub const JIT_RESULT_JIT_ERROR: u32 = 6;

    // call_kind constants
    pub const CALL_KIND_REGULAR: u8 = 0;
    pub const CALL_KIND_PREPARED: u8 = 1; // prepare callback already did push_frame + arg layout
    pub const CALL_KIND_YIELD: u8 = 253;
    pub const CALL_KIND_BLOCK: u8 = 254;

    // Fiber stack access offsets
    pub const OFFSET_STACK_PTR: i32 = std::mem::offset_of!(JitContext, stack_ptr) as i32;
    pub const OFFSET_STACK_CAP: i32 = std::mem::offset_of!(JitContext, stack_cap) as i32;
    pub const OFFSET_STACK_LIMIT: i32 = std::mem::offset_of!(JitContext, stack_limit) as i32;
    pub const OFFSET_CALL_DEPTH: i32 = std::mem::offset_of!(JitContext, call_depth) as i32;
    pub const OFFSET_CALL_DEPTH_LIMIT: i32 =
        std::mem::offset_of!(JitContext, call_depth_limit) as i32;
    pub const OFFSET_JIT_BP: i32 = std::mem::offset_of!(JitContext, jit_bp) as i32;
    pub const OFFSET_FIBER_SP: i32 = std::mem::offset_of!(JitContext, fiber_sp) as i32;
    pub const OFFSET_PUSH_FRAME_FN: i32 = std::mem::offset_of!(JitContext, push_frame_fn) as i32;
    pub const OFFSET_POP_FRAME_FN: i32 = std::mem::offset_of!(JitContext, pop_frame_fn) as i32;
    pub const OFFSET_STACK_OVERFLOW_FN: i32 =
        std::mem::offset_of!(JitContext, stack_overflow_fn) as i32;
    pub const OFFSET_PUSH_RESUME_POINT_FN: i32 =
        std::mem::offset_of!(JitContext, push_resume_point_fn) as i32;

    // VM callback offsets
    pub const OFFSET_CREATE_ISLAND_FN: i32 =
        std::mem::offset_of!(JitContext, create_island_fn) as i32;
    pub const OFFSET_QUEUE_CLOSE_FN: i32 = std::mem::offset_of!(JitContext, queue_close_fn) as i32;
    pub const OFFSET_QUEUE_SEND_FN: i32 = std::mem::offset_of!(JitContext, queue_send_fn) as i32;
    pub const OFFSET_QUEUE_RECV_FN: i32 = std::mem::offset_of!(JitContext, queue_recv_fn) as i32;
    pub const OFFSET_GO_START_FN: i32 = std::mem::offset_of!(JitContext, go_start_fn) as i32;
    pub const OFFSET_GO_ISLAND_FN: i32 = std::mem::offset_of!(JitContext, go_island_fn) as i32;

    // Defer/Recover
    pub const OFFSET_DEFER_PUSH_FN: i32 = std::mem::offset_of!(JitContext, defer_push_fn) as i32;
    pub const OFFSET_RECOVER_FN: i32 = std::mem::offset_of!(JitContext, recover_fn) as i32;

    // Select
    pub const OFFSET_SELECT_BEGIN_FN: i32 =
        std::mem::offset_of!(JitContext, select_begin_fn) as i32;
    pub const OFFSET_SELECT_SEND_FN: i32 = std::mem::offset_of!(JitContext, select_send_fn) as i32;
    pub const OFFSET_SELECT_RECV_FN: i32 = std::mem::offset_of!(JitContext, select_recv_fn) as i32;
    pub const OFFSET_SELECT_EXEC_FN: i32 = std::mem::offset_of!(JitContext, select_exec_fn) as i32;

    pub const OFFSET_IS_ERROR_RETURN: i32 =
        std::mem::offset_of!(JitContext, is_error_return) as i32;
    pub const OFFSET_RET_GCREF_START: i32 =
        std::mem::offset_of!(JitContext, ret_gcref_start) as i32;
    pub const OFFSET_RET_IS_HEAP: i32 = std::mem::offset_of!(JitContext, ret_is_heap) as i32;
    pub const OFFSET_RET_START: i32 = std::mem::offset_of!(JitContext, ret_start) as i32;

    // JIT-to-JIT Direct Call for Closure/Iface
    pub const OFFSET_PREPARE_CLOSURE_CALL_FN: i32 =
        std::mem::offset_of!(JitContext, prepare_closure_call_fn) as i32;
    pub const OFFSET_PREPARE_IFACE_CALL_FN: i32 =
        std::mem::offset_of!(JitContext, prepare_iface_call_fn) as i32;

    // Inline Cache
    pub const OFFSET_IC_TABLE: i32 = std::mem::offset_of!(JitContext, ic_table) as i32;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JitContextAbiField {
    pub name: &'static str,
    pub offset: i32,
}

/// JitContext fields that generated code may address by raw offset.
///
/// Keep this table in sync with `JitContext` and the offset constants above; it
/// gives tests and downstream debug tooling a single machine-readable ABI view.
pub fn jit_context_abi_fields() -> &'static [JitContextAbiField] {
    &[
        JitContextAbiField {
            name: "jit_func_table",
            offset: JitContext::OFFSET_JIT_FUNC_TABLE,
        },
        JitContextAbiField {
            name: "jit_func_count",
            offset: JitContext::OFFSET_JIT_FUNC_COUNT,
        },
        JitContextAbiField {
            name: "direct_call_table",
            offset: JitContext::OFFSET_DIRECT_CALL_TABLE,
        },
        JitContextAbiField {
            name: "direct_call_count",
            offset: JitContext::OFFSET_DIRECT_CALL_COUNT,
        },
        JitContextAbiField {
            name: "call_func_id",
            offset: JitContext::OFFSET_CALL_FUNC_ID,
        },
        JitContextAbiField {
            name: "call_arg_start",
            offset: JitContext::OFFSET_CALL_ARG_START,
        },
        JitContextAbiField {
            name: "call_resume_pc",
            offset: JitContext::OFFSET_CALL_RESUME_PC,
        },
        JitContextAbiField {
            name: "call_ret_slots",
            offset: JitContext::OFFSET_CALL_RET_SLOTS,
        },
        JitContextAbiField {
            name: "call_ret_reg",
            offset: JitContext::OFFSET_CALL_RET_REG,
        },
        JitContextAbiField {
            name: "call_kind",
            offset: JitContext::OFFSET_CALL_KIND,
        },
        JitContextAbiField {
            name: "runtime_trap_kind",
            offset: JitContext::OFFSET_RUNTIME_TRAP_KIND,
        },
        JitContextAbiField {
            name: "runtime_trap_arg0",
            offset: JitContext::OFFSET_RUNTIME_TRAP_ARG0,
        },
        JitContextAbiField {
            name: "runtime_trap_arg1",
            offset: JitContext::OFFSET_RUNTIME_TRAP_ARG1,
        },
        JitContextAbiField {
            name: "runtime_trap_pc",
            offset: JitContext::OFFSET_RUNTIME_TRAP_PC,
        },
        JitContextAbiField {
            name: "user_panic_pc",
            offset: JitContext::OFFSET_USER_PANIC_PC,
        },
        #[cfg(feature = "std")]
        JitContextAbiField {
            name: "wait_io_token",
            offset: JitContext::OFFSET_WAIT_IO_TOKEN,
        },
        JitContextAbiField {
            name: "loop_exit_pc",
            offset: JitContext::OFFSET_LOOP_EXIT_PC,
        },
        JitContextAbiField {
            name: "stack_ptr",
            offset: JitContext::OFFSET_STACK_PTR,
        },
        JitContextAbiField {
            name: "stack_cap",
            offset: JitContext::OFFSET_STACK_CAP,
        },
        JitContextAbiField {
            name: "stack_limit",
            offset: JitContext::OFFSET_STACK_LIMIT,
        },
        JitContextAbiField {
            name: "call_depth",
            offset: JitContext::OFFSET_CALL_DEPTH,
        },
        JitContextAbiField {
            name: "call_depth_limit",
            offset: JitContext::OFFSET_CALL_DEPTH_LIMIT,
        },
        JitContextAbiField {
            name: "jit_bp",
            offset: JitContext::OFFSET_JIT_BP,
        },
        JitContextAbiField {
            name: "fiber_sp",
            offset: JitContext::OFFSET_FIBER_SP,
        },
        JitContextAbiField {
            name: "push_frame_fn",
            offset: JitContext::OFFSET_PUSH_FRAME_FN,
        },
        JitContextAbiField {
            name: "pop_frame_fn",
            offset: JitContext::OFFSET_POP_FRAME_FN,
        },
        JitContextAbiField {
            name: "stack_overflow_fn",
            offset: JitContext::OFFSET_STACK_OVERFLOW_FN,
        },
        JitContextAbiField {
            name: "push_resume_point_fn",
            offset: JitContext::OFFSET_PUSH_RESUME_POINT_FN,
        },
        JitContextAbiField {
            name: "create_island_fn",
            offset: JitContext::OFFSET_CREATE_ISLAND_FN,
        },
        JitContextAbiField {
            name: "queue_close_fn",
            offset: JitContext::OFFSET_QUEUE_CLOSE_FN,
        },
        JitContextAbiField {
            name: "queue_send_fn",
            offset: JitContext::OFFSET_QUEUE_SEND_FN,
        },
        JitContextAbiField {
            name: "queue_recv_fn",
            offset: JitContext::OFFSET_QUEUE_RECV_FN,
        },
        JitContextAbiField {
            name: "go_start_fn",
            offset: JitContext::OFFSET_GO_START_FN,
        },
        JitContextAbiField {
            name: "go_island_fn",
            offset: JitContext::OFFSET_GO_ISLAND_FN,
        },
        JitContextAbiField {
            name: "defer_push_fn",
            offset: JitContext::OFFSET_DEFER_PUSH_FN,
        },
        JitContextAbiField {
            name: "recover_fn",
            offset: JitContext::OFFSET_RECOVER_FN,
        },
        JitContextAbiField {
            name: "select_begin_fn",
            offset: JitContext::OFFSET_SELECT_BEGIN_FN,
        },
        JitContextAbiField {
            name: "select_send_fn",
            offset: JitContext::OFFSET_SELECT_SEND_FN,
        },
        JitContextAbiField {
            name: "select_recv_fn",
            offset: JitContext::OFFSET_SELECT_RECV_FN,
        },
        JitContextAbiField {
            name: "select_exec_fn",
            offset: JitContext::OFFSET_SELECT_EXEC_FN,
        },
        JitContextAbiField {
            name: "is_error_return",
            offset: JitContext::OFFSET_IS_ERROR_RETURN,
        },
        JitContextAbiField {
            name: "ret_gcref_start",
            offset: JitContext::OFFSET_RET_GCREF_START,
        },
        JitContextAbiField {
            name: "ret_is_heap",
            offset: JitContext::OFFSET_RET_IS_HEAP,
        },
        JitContextAbiField {
            name: "ret_start",
            offset: JitContext::OFFSET_RET_START,
        },
        JitContextAbiField {
            name: "prepare_closure_call_fn",
            offset: JitContext::OFFSET_PREPARE_CLOSURE_CALL_FN,
        },
        JitContextAbiField {
            name: "prepare_iface_call_fn",
            offset: JitContext::OFFSET_PREPARE_IFACE_CALL_FN,
        },
        JitContextAbiField {
            name: "ic_table",
            offset: JitContext::OFFSET_IC_TABLE,
        },
    ]
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
    /// JIT requests VM to block on queue operation (channel send/recv).
    /// Unlike WaitIo, no token is needed - fiber is woken via ChannelWaiter.
    /// After being woken, VM resumes execution in interpreter at call_resume_pc.
    WaitQueue = 4,
    /// JIT requests VM to replay the current CallExtern instruction in interpreter mode.
    /// Used when an extern returns CallClosure — JIT exits, frames are materialized,
    /// and VM re-executes the CallExtern which handles suspend/replay natively.
    Replay = 5,
    /// Fatal JIT infrastructure error. User code cannot recover this.
    JitError = 6,
}

pub const JIT_INFRA_ERROR_SENTINEL: u64 = u64::MAX;
pub const JIT_INFRA_ERROR_MISSING_CALLBACK: u64 = 1;
pub const JIT_INFRA_ERROR_INVALID_CALLBACK_STATE: u64 = 2;
pub const JIT_INFRA_ERROR_INVALID_METADATA: u64 = 3;
pub const JIT_HELPER_U64_ERROR: u64 = u64::MAX;
pub const JIT_HELPER_MAP_GET_LAYOUT: u64 = 101;
pub const JIT_HELPER_MAP_SET_LAYOUT: u64 = 102;
pub const JIT_HELPER_MAP_DELETE_LAYOUT: u64 = 103;
pub const JIT_HELPER_MAP_ITER_NEXT_LAYOUT: u64 = 104;
pub const JIT_HELPER_TYPED_WRITE_BARRIER_LAYOUT: u64 = 105;
pub const JIT_CALLBACK_DEFER_PUSH: u64 = 1;
pub const JIT_CALLBACK_RECOVER: u64 = 2;
pub const JIT_CALLBACK_GO_START: u64 = 3;
pub const JIT_CALLBACK_GO_ISLAND: u64 = 4;
pub const JIT_CALLBACK_QUEUE_CLOSE: u64 = 5;
pub const JIT_CALLBACK_QUEUE_SEND: u64 = 6;
pub const JIT_CALLBACK_QUEUE_RECV: u64 = 7;
pub const JIT_CALLBACK_SELECT_BEGIN: u64 = 8;
pub const JIT_CALLBACK_SELECT_SEND: u64 = 9;
pub const JIT_CALLBACK_SELECT_RECV: u64 = 10;
pub const JIT_CALLBACK_SELECT_EXEC: u64 = 11;
pub const JIT_CALLBACK_CREATE_ISLAND: u64 = 12;
pub const JIT_CALLBACK_IFACE_ASSERT: u64 = 13;
pub const JIT_CALLBACK_CALL_EXTERN: u64 = 14;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JitContextDependencyKind {
    CallExternFn,
    PushFrameFn,
    PopFrameFn,
    StackOverflowFn,
    PushResumePointFn,
    CreateIslandFn,
    QueueCloseFn,
    QueueSendFn,
    QueueRecvFn,
    GoStartFn,
    GoIslandFn,
    DeferPushFn,
    RecoverFn,
    SelectBeginFn,
    SelectSendFn,
    SelectRecvFn,
    SelectExecFn,
    PrepareClosureCallFn,
    PrepareIfaceCallFn,
    InlineCacheTable,
}

impl JitContextDependencyKind {
    fn is_missing(self, ctx: &JitContext) -> bool {
        match self {
            Self::CallExternFn => ctx.call_extern_fn.is_none(),
            Self::PushFrameFn => ctx.push_frame_fn.is_none(),
            Self::PopFrameFn => ctx.pop_frame_fn.is_none(),
            Self::StackOverflowFn => ctx.stack_overflow_fn.is_none(),
            Self::PushResumePointFn => ctx.push_resume_point_fn.is_none(),
            Self::CreateIslandFn => ctx.create_island_fn.is_none(),
            Self::QueueCloseFn => ctx.queue_close_fn.is_none(),
            Self::QueueSendFn => ctx.queue_send_fn.is_none(),
            Self::QueueRecvFn => ctx.queue_recv_fn.is_none(),
            Self::GoStartFn => ctx.go_start_fn.is_none(),
            Self::GoIslandFn => ctx.go_island_fn.is_none(),
            Self::DeferPushFn => ctx.defer_push_fn.is_none(),
            Self::RecoverFn => ctx.recover_fn.is_none(),
            Self::SelectBeginFn => ctx.select_begin_fn.is_none(),
            Self::SelectSendFn => ctx.select_send_fn.is_none(),
            Self::SelectRecvFn => ctx.select_recv_fn.is_none(),
            Self::SelectExecFn => ctx.select_exec_fn.is_none(),
            Self::PrepareClosureCallFn => ctx.prepare_closure_call_fn.is_none(),
            Self::PrepareIfaceCallFn => ctx.prepare_iface_call_fn.is_none(),
            Self::InlineCacheTable => ctx.ic_table.is_null(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JitCallbackReturnPolicy {
    RawPointer,
    RawVoid,
    RawHandle,
    JitResult,
    JitResultWithOutPointer,
    PreparedCallOutPointer,
    TablePointer,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum JitAbiType {
    Void,
    Ptr,
    U8,
    U16,
    U32,
    I32,
    U64,
    I64,
    JitResult,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JitRuntimeHelperReturnPolicy {
    Void,
    RawI32,
    RawU64,
    JitResult,
    I32StatusOutPointer,
    U64ErrorSentinel,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JitRuntimeHelperPanicPolicy {
    MustNotPanicAcrossAbi,
    ReturnsJitResult,
    ReturnsStatusOrSentinel,
    RecordsRuntimeTrap,
    RecordsUserPanic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JitRuntimeHelperAbi {
    pub name: &'static str,
    pub params: &'static [JitAbiType],
    pub ret: JitAbiType,
    pub return_policy: JitRuntimeHelperReturnPolicy,
    pub panic_policy: JitRuntimeHelperPanicPolicy,
    pub may_gc: bool,
    pub may_schedule: bool,
    pub observes_frame: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JitCallbackAbiField {
    pub kind: JitContextDependencyKind,
    pub name: &'static str,
    pub params: &'static [JitAbiType],
    pub ret: JitAbiType,
    pub infra_error_id: Option<u64>,
    pub return_policy: JitCallbackReturnPolicy,
    pub may_gc: bool,
    pub may_schedule: bool,
    pub observes_frame: bool,
}

pub fn jit_callback_abi_fields() -> &'static [JitCallbackAbiField] {
    use JitAbiType as T;
    use JitCallbackReturnPolicy as Ret;
    use JitContextDependencyKind as Kind;

    &[
        #[cfg(feature = "std")]
        JitCallbackAbiField {
            kind: Kind::CallExternFn,
            name: "call_extern_fn",
            params: &[
                T::Ptr,
                T::Ptr,
                T::Ptr,
                T::Ptr,
                T::U32,
                T::Ptr,
                T::U32,
                T::Ptr,
                T::U32,
            ],
            ret: T::JitResult,
            infra_error_id: Some(JIT_CALLBACK_CALL_EXTERN),
            return_policy: Ret::JitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::PushFrameFn,
            name: "push_frame_fn",
            params: &[T::Ptr, T::U32, T::U32, T::U32, T::U32, T::U32],
            ret: T::Ptr,
            infra_error_id: None,
            return_policy: Ret::RawPointer,
            may_gc: false,
            may_schedule: false,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::PopFrameFn,
            name: "pop_frame_fn",
            params: &[T::Ptr, T::U32],
            ret: T::Void,
            infra_error_id: None,
            return_policy: Ret::RawVoid,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitCallbackAbiField {
            kind: Kind::StackOverflowFn,
            name: "stack_overflow_fn",
            params: &[T::Ptr],
            ret: T::JitResult,
            infra_error_id: None,
            return_policy: Ret::JitResult,
            may_gc: true,
            may_schedule: false,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::PushResumePointFn,
            name: "push_resume_point_fn",
            params: &[T::Ptr, T::U32, T::U32, T::U32, T::U32, T::U32, T::U32],
            ret: T::JitResult,
            infra_error_id: None,
            return_policy: Ret::JitResult,
            may_gc: true,
            may_schedule: false,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::CreateIslandFn,
            name: "create_island_fn",
            params: &[T::Ptr],
            ret: T::U64,
            infra_error_id: Some(JIT_CALLBACK_CREATE_ISLAND),
            return_policy: Ret::RawHandle,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::QueueCloseFn,
            name: "queue_close_fn",
            params: &[T::Ptr, T::U64],
            ret: T::JitResult,
            infra_error_id: Some(JIT_CALLBACK_QUEUE_CLOSE),
            return_policy: Ret::JitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::QueueSendFn,
            name: "queue_send_fn",
            params: &[T::Ptr, T::U64, T::Ptr, T::U32],
            ret: T::JitResult,
            infra_error_id: Some(JIT_CALLBACK_QUEUE_SEND),
            return_policy: Ret::JitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::QueueRecvFn,
            name: "queue_recv_fn",
            params: &[T::Ptr, T::U64, T::Ptr, T::U32, T::U32],
            ret: T::JitResult,
            infra_error_id: Some(JIT_CALLBACK_QUEUE_RECV),
            return_policy: Ret::JitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::GoStartFn,
            name: "go_start_fn",
            params: &[T::Ptr, T::U32, T::U32, T::U64, T::Ptr, T::U32],
            ret: T::JitResult,
            infra_error_id: Some(JIT_CALLBACK_GO_START),
            return_policy: Ret::JitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::GoIslandFn,
            name: "go_island_fn",
            params: &[T::Ptr, T::U64, T::U64, T::Ptr, T::U32],
            ret: T::JitResult,
            infra_error_id: Some(JIT_CALLBACK_GO_ISLAND),
            return_policy: Ret::JitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::DeferPushFn,
            name: "defer_push_fn",
            params: &[
                T::Ptr,
                T::U32,
                T::U32,
                T::U64,
                T::U32,
                T::Ptr,
                T::U32,
                T::U32,
            ],
            ret: T::JitResult,
            infra_error_id: Some(JIT_CALLBACK_DEFER_PUSH),
            return_policy: Ret::JitResult,
            may_gc: true,
            may_schedule: false,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::RecoverFn,
            name: "recover_fn",
            params: &[T::Ptr, T::Ptr],
            ret: T::JitResult,
            infra_error_id: Some(JIT_CALLBACK_RECOVER),
            return_policy: Ret::JitResultWithOutPointer,
            may_gc: true,
            may_schedule: false,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::SelectBeginFn,
            name: "select_begin_fn",
            params: &[T::Ptr, T::U32, T::U32],
            ret: T::JitResult,
            infra_error_id: Some(JIT_CALLBACK_SELECT_BEGIN),
            return_policy: Ret::JitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::SelectSendFn,
            name: "select_send_fn",
            params: &[T::Ptr, T::U32, T::U32, T::U32, T::U32],
            ret: T::JitResult,
            infra_error_id: Some(JIT_CALLBACK_SELECT_SEND),
            return_policy: Ret::JitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::SelectRecvFn,
            name: "select_recv_fn",
            params: &[T::Ptr, T::U32, T::U32, T::U32, T::U32, T::U32],
            ret: T::JitResult,
            infra_error_id: Some(JIT_CALLBACK_SELECT_RECV),
            return_policy: Ret::JitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::SelectExecFn,
            name: "select_exec_fn",
            params: &[T::Ptr, T::U32],
            ret: T::JitResult,
            infra_error_id: Some(JIT_CALLBACK_SELECT_EXEC),
            return_policy: Ret::JitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::PrepareClosureCallFn,
            name: "prepare_closure_call_fn",
            params: &[
                T::Ptr,
                T::U64,
                T::U32,
                T::U32,
                T::U32,
                T::Ptr,
                T::U32,
                T::Ptr,
                T::Ptr,
            ],
            ret: T::JitResult,
            infra_error_id: None,
            return_policy: Ret::PreparedCallOutPointer,
            may_gc: true,
            may_schedule: false,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::PrepareIfaceCallFn,
            name: "prepare_iface_call_fn",
            params: &[
                T::Ptr,
                T::U64,
                T::U64,
                T::U32,
                T::U32,
                T::U32,
                T::U32,
                T::Ptr,
                T::U32,
                T::Ptr,
                T::Ptr,
            ],
            ret: T::JitResult,
            infra_error_id: None,
            return_policy: Ret::PreparedCallOutPointer,
            may_gc: true,
            may_schedule: false,
            observes_frame: true,
        },
        JitCallbackAbiField {
            kind: Kind::InlineCacheTable,
            name: "ic_table",
            params: &[],
            ret: T::Ptr,
            infra_error_id: None,
            return_policy: Ret::TablePointer,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
    ]
}

#[inline]
pub fn set_jit_infra_error(ctx: *mut JitContext, code: u64, detail: u64) -> JitResult {
    unsafe {
        let ctx = &mut *ctx;
        *ctx.panic_flag = false;
        *ctx.is_user_panic = false;
        ctx.runtime_trap_kind = JitRuntimeTrapKind::None as u8;
        ctx.runtime_trap_arg0 = JIT_INFRA_ERROR_SENTINEL;
        ctx.runtime_trap_arg1 = code;
        ctx.runtime_trap_pc = detail as u32;
    }
    JitResult::JitError
}

#[inline]
fn missing_callback(ctx: *mut JitContext, callback_id: u64) -> JitResult {
    set_jit_infra_error(ctx, JIT_INFRA_ERROR_MISSING_CALLBACK, callback_id)
}

#[inline]
fn set_invalid_metadata_u64(ctx: *mut JitContext, detail: u64) -> u64 {
    let _ = set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_METADATA, detail);
    JIT_HELPER_U64_ERROR
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
///
/// Raw write barrier for one known-reference value. Multi-slot typed writes
/// should use `vo_gc_typed_write_barrier_by_meta` so struct/interface layouts
/// are interpreted with the same metadata as VM helpers.
#[no_mangle]
pub extern "C" fn vo_gc_write_barrier(gc: *mut Gc, obj: u64, _offset: u32, val: u64) {
    if gc.is_null() || obj == 0 {
        return;
    }
    // Non-GcRef values (integers, floats, booleans) may be misaligned.
    // GcRefs are always at least 4-byte aligned since GcHeader contains i32 fields.
    if val == 0 || (val & 3) != 0 {
        return;
    }
    let gc = unsafe { &mut *gc };
    let parent = obj as GcRef;
    let child = val as GcRef;
    gc.write_barrier(parent, child);
}

/// Type-safe write barrier for JIT writes whose element metadata is known only
/// at runtime from an array/slice/map header.
#[no_mangle]
pub extern "C" fn vo_gc_typed_write_barrier_by_meta(
    ctx: *mut JitContext,
    parent: u64,
    vals: *const u64,
    val_slots: u32,
    elem_meta_raw: u32,
) -> JitResult {
    if ctx.is_null() {
        return JitResult::JitError;
    }
    if parent == 0 || val_slots == 0 {
        return JitResult::Ok;
    }
    if vals.is_null() {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_HELPER_TYPED_WRITE_BARRIER_LAYOUT,
        );
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.gc.is_null() {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_HELPER_TYPED_WRITE_BARRIER_LAYOUT,
        );
    }
    let gc = unsafe { &mut *ctx.gc };
    let vals = unsafe { std::slice::from_raw_parts(vals, val_slots as usize) };
    let module = unsafe { ctx.module.as_ref() };
    match crate::gc_types::try_typed_write_barrier_by_meta(
        gc,
        parent as GcRef,
        vals,
        crate::ValueMeta::from_raw(elem_meta_raw),
        module,
    ) {
        Ok(()) => JitResult::Ok,
        Err(_) => set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_METADATA,
            JIT_HELPER_TYPED_WRITE_BARRIER_LAYOUT,
        ),
    }
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

/// Set Call request state in JitContext.
/// Called by JIT when it needs to hand off to VM for a non-jittable callee.
///
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
#[no_mangle]
pub extern "C" fn vo_set_call_request(
    ctx: *mut JitContext,
    func_id: u32,
    arg_start: u32,
    resume_pc: u32,
    ret_slots: u32,
    ret_reg: u32,
    call_kind: u32,
) {
    unsafe {
        (*ctx).call_func_id = func_id;
        (*ctx).call_arg_start = arg_start as u16;
        (*ctx).call_resume_pc = resume_pc;
        (*ctx).call_ret_slots = ret_slots as u16;
        (*ctx).call_ret_reg = ret_reg as u16;
        (*ctx).call_kind = call_kind as u8;
    }
}

/// Push a defer entry from JIT code.
///
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
/// - `ctx.defer_push_fn` must be set
#[no_mangle]
pub extern "C" fn vo_defer_push(
    ctx: *mut JitContext,
    func_id: u32,
    is_closure: u32,
    closure_ref: u64,
    arg_start: u32,
    args_ptr: *const u64,
    arg_count: u32,
    is_errdefer: u32,
) -> JitResult {
    let ctx_ref = unsafe { &*ctx };
    let Some(f) = ctx_ref.defer_push_fn else {
        return missing_callback(ctx, JIT_CALLBACK_DEFER_PUSH);
    };
    f(
        ctx,
        func_id,
        is_closure,
        closure_ref,
        arg_start,
        args_ptr,
        arg_count,
        is_errdefer,
    )
}

/// Execute recover() from JIT code.
/// Result is written to result_ptr (2 slots for interface{}).
///
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
/// - `ctx.recover_fn` must be set
/// - `result_ptr` must point to at least 2 u64 slots
#[no_mangle]
pub extern "C" fn vo_recover(ctx: *mut JitContext, result_ptr: *mut u64) -> JitResult {
    let ctx_ref = unsafe { &*ctx };
    let Some(f) = ctx_ref.recover_fn else {
        return missing_callback(ctx, JIT_CALLBACK_RECOVER);
    };
    f(ctx, result_ptr)
}

/// Trigger a user panic from JIT code (explicit `panic()` call or `?` operator).
///
/// Sets `is_user_panic=true` to distinguish from runtime errors (nil deref, bounds check).
/// The panic message is stored in `ctx.panic_msg` and may be a nil interface.
///
/// # Arguments
/// - `ctx`: JIT context
/// - `msg_slot0`: Interface slot0 (packed metadata, 0 for nil interface)
/// - `msg_slot1`: Interface slot1 (data pointer or immediate value)
///
/// # Safety
/// - `ctx` must be a valid pointer to JitContext
#[no_mangle]
pub extern "C" fn vo_panic(ctx: *mut JitContext, msg_slot0: u64, msg_slot1: u64) {
    unsafe {
        let ctx = &mut *ctx;
        *ctx.panic_flag = true;
        *ctx.is_user_panic = true;
        ctx.runtime_trap_kind = JitRuntimeTrapKind::None as u8;
        ctx.runtime_trap_pc = u32::MAX;
        (*ctx.panic_msg).slot0 = msg_slot0;
        (*ctx.panic_msg).slot1 = msg_slot1;
    }
}

/// Trigger a typed runtime trap from JIT code.
///
/// The VM side converts the compact kind and arguments back into
/// `RuntimeTrapKind`, message text, and source location before unwinding.
#[no_mangle]
pub extern "C" fn vo_runtime_trap(
    ctx: *mut JitContext,
    kind: u32,
    arg0: u64,
    arg1: u64,
    pc: u32,
) -> JitResult {
    unsafe {
        let ctx = &mut *ctx;
        *ctx.panic_flag = true;
        *ctx.is_user_panic = false;
        ctx.runtime_trap_kind = kind as u8;
        ctx.runtime_trap_arg0 = arg0;
        ctx.runtime_trap_arg1 = arg1;
        ctx.runtime_trap_pc = pc;
    }
    JitResult::Panic
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
        None => return missing_callback(ctx, JIT_CALLBACK_CALL_EXTERN),
    };

    call_fn(
        ctx,
        ctx_ref.extern_registry,
        ctx_ref.gc,
        ctx_ref.module as *const c_void,
        extern_id,
        args,
        arg_count,
        ret,
        ret_slots,
    )
}

// =============================================================================
// Map Helpers
// =============================================================================

/// Create a new map.
#[no_mangle]
pub extern "C" fn vo_map_new(
    gc: *mut Gc,
    key_meta: u32,
    val_meta: u32,
    key_slots: u32,
    val_slots: u32,
    key_rttid: u32,
) -> u64 {
    use crate::objects::map;
    use crate::ValueMeta;
    unsafe {
        let gc = &mut *gc;
        map::create(
            gc,
            ValueMeta::from_raw(key_meta),
            ValueMeta::from_raw(val_meta),
            key_slots as u16,
            val_slots as u16,
            key_rttid,
        ) as u64
    }
}

/// Get map length.
#[no_mangle]
pub extern "C" fn vo_map_len(m: u64) -> u64 {
    use crate::objects::map;
    if m == 0 {
        return 0;
    }
    map::len(m as crate::gc::GcRef) as u64
}

/// Get value from map. Returns pointer to value slots, or null if not found.
/// key_ptr points to key_slots u64 values.
/// val_ptr is output buffer for val_slots u64 values.
/// Returns 1 if found, 0 if not found.
#[no_mangle]
pub extern "C" fn vo_map_get(
    ctx: *mut JitContext,
    m: u64,
    key_ptr: *const u64,
    key_slots: u32,
    val_ptr: *mut u64,
    val_slots: u32,
) -> u64 {
    use crate::objects::map;
    if m == 0 {
        // nil map read returns zero value (Go semantics)
        unsafe {
            core::ptr::write_bytes(val_ptr, 0, val_slots as usize);
        }
        return 0;
    }

    let m_ref = m as crate::gc::GcRef;
    if map::key_slots(m_ref) as u32 != key_slots || map::val_slots(m_ref) as u32 != val_slots {
        return set_invalid_metadata_u64(ctx, JIT_HELPER_MAP_GET_LAYOUT);
    }

    let module = unsafe { (*ctx).module.as_ref() };
    let key = unsafe { core::slice::from_raw_parts(key_ptr, key_slots as usize) };
    let (val_opt, ok) = map::get_with_ok(m_ref, key, module);

    if let Some(val) = val_opt {
        if val.len() != val_slots as usize {
            return set_invalid_metadata_u64(ctx, JIT_HELPER_MAP_GET_LAYOUT);
        }
        unsafe {
            core::ptr::copy_nonoverlapping(val.as_ptr(), val_ptr, val_slots as usize);
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
pub extern "C" fn vo_map_set(
    ctx: *mut JitContext,
    m: u64,
    key_ptr: *const u64,
    key_slots: u32,
    val_ptr: *const u64,
    val_slots: u32,
) -> u64 {
    use crate::objects::{interface, map};
    use crate::ValueKind;
    if m == 0 {
        return 0;
    }

    let m_ref = m as crate::gc::GcRef;
    if map::key_slots(m_ref) as u32 != key_slots || map::val_slots(m_ref) as u32 != val_slots {
        return set_invalid_metadata_u64(ctx, JIT_HELPER_MAP_SET_LAYOUT);
    }

    let module = unsafe { (*ctx).module.as_ref() };
    let key = unsafe { core::slice::from_raw_parts(key_ptr, key_slots as usize) };
    let val = unsafe { core::slice::from_raw_parts(val_ptr, val_slots as usize) };

    // Check if key is interface (2 slots) with uncomparable underlying type
    if key_slots == 2 {
        let key_vk = map::key_kind(m_ref);
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

    map::set(m_ref, key, val, module);

    // Write barrier: use map's stored metadata to barrier only actual GcRef slots.
    let gc = unsafe { &mut *(*ctx).gc };
    let km = map::key_meta(m_ref);
    let vm = map::val_meta(m_ref);
    if km.value_kind().may_contain_gc_refs()
        && crate::gc_types::try_typed_write_barrier_by_meta(gc, m_ref, key, km, module).is_err()
    {
        return set_invalid_metadata_u64(ctx, JIT_HELPER_MAP_SET_LAYOUT);
    }
    if vm.value_kind().may_contain_gc_refs()
        && crate::gc_types::try_typed_write_barrier_by_meta(gc, m_ref, val, vm, module).is_err()
    {
        return set_invalid_metadata_u64(ctx, JIT_HELPER_MAP_SET_LAYOUT);
    }
    0
}

/// Delete key from map.
#[no_mangle]
pub extern "C" fn vo_map_delete(
    ctx: *mut JitContext,
    m: u64,
    key_ptr: *const u64,
    key_slots: u32,
) -> u64 {
    use crate::objects::map;
    if m == 0 {
        return 0;
    }

    let m_ref = m as crate::gc::GcRef;
    if map::key_slots(m_ref) as u32 != key_slots {
        return set_invalid_metadata_u64(ctx, JIT_HELPER_MAP_DELETE_LAYOUT);
    }

    let module = unsafe { (*ctx).module.as_ref() };
    let key = unsafe { core::slice::from_raw_parts(key_ptr, key_slots as usize) };
    map::delete(m_ref, key, module);
    0
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
}

/// Advance map iterator and get next key-value pair.
/// Returns 1 if valid entry exists, 0 if exhausted.
#[no_mangle]
pub extern "C" fn vo_map_iter_next(
    ctx: *mut JitContext,
    iter_ptr: *mut u64,
    key_ptr: *mut u64,
    key_slots: u32,
    val_ptr: *mut u64,
    val_slots: u32,
) -> u64 {
    use crate::objects::map;
    let iter = unsafe { &mut *(iter_ptr as *mut map::MapIterator) };
    let key_slots = key_slots as usize;
    let val_slots = val_slots as usize;
    let map_ref = iter.map_ref as crate::gc::GcRef;

    if !map_ref.is_null()
        && (map::key_slots(map_ref) as usize != key_slots
            || map::val_slots(map_ref) as usize != val_slots)
    {
        return set_invalid_metadata_u64(ctx, JIT_HELPER_MAP_ITER_NEXT_LAYOUT);
    }

    unsafe {
        if key_slots > 0 {
            core::ptr::write_bytes(key_ptr, 0, key_slots);
        }
        if val_slots > 0 {
            core::ptr::write_bytes(val_ptr, 0, val_slots);
        }
    }

    match map::iter_next(iter) {
        Some((key, val)) => {
            if key.len() != key_slots || val.len() != val_slots {
                return set_invalid_metadata_u64(ctx, JIT_HELPER_MAP_ITER_NEXT_LAYOUT);
            }
            unsafe {
                core::ptr::copy_nonoverlapping(key.as_ptr(), key_ptr, key_slots);
                core::ptr::copy_nonoverlapping(val.as_ptr(), val_ptr, val_slots);
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
        match string::slice_of(gc, s as crate::gc::GcRef, lo as usize, hi as usize) {
            Some(result) => result as u64,
            None => JIT_HELPER_U64_ERROR,
        }
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

/// Create a new queue with validation (unified logic for VM and JIT).
#[no_mangle]
pub extern "C" fn vo_queue_new_checked(
    gc: *mut Gc,
    kind: u32,
    elem_type: u64,
    elem_slots: u32,
    cap: i64,
    out: *mut u64,
) -> i32 {
    unsafe {
        queue_new_checked(
            gc,
            crate::objects::queue_state::QueueKind::from_raw(kind as u16),
            elem_type,
            elem_slots,
            cap,
            out,
        )
    }
}

unsafe fn queue_new_checked(
    gc: *mut Gc,
    kind: crate::objects::queue_state::QueueKind,
    elem_type: u64,
    elem_slots: u32,
    cap: i64,
    out: *mut u64,
) -> i32 {
    use crate::objects::queue;
    use crate::{ValueMeta, ValueRttid};
    let elem_meta = ValueMeta::from_raw(elem_type as u32);
    let elem_rttid = ValueRttid::from_raw((elem_type >> 32) as u32);
    match queue::create_checked(
        &mut *gc,
        kind,
        elem_meta,
        elem_rttid,
        elem_slots as u16,
        cap,
    ) {
        Ok(result) => {
            *out = result as u64;
            0
        }
        Err(code) => code,
    }
}

/// Get channel length (number of elements in buffer).
#[no_mangle]
pub extern "C" fn vo_chan_len(ch: u64) -> u64 {
    use crate::gc::GcRef;
    use crate::objects::queue;
    let ch = ch as GcRef;
    if ch.is_null() {
        0
    } else {
        queue::len(ch) as u64
    }
}

/// Get channel capacity.
#[no_mangle]
pub extern "C" fn vo_chan_cap(ch: u64) -> u64 {
    use crate::gc::GcRef;
    use crate::objects::queue_state;
    let ch = ch as GcRef;
    if ch.is_null() {
        0
    } else {
        queue_state::capacity(ch) as u64
    }
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
        array::create(
            gc,
            ValueMeta::from_raw(elem_meta),
            elem_bytes as usize,
            len as usize,
        ) as u64
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
    array::set_auto(
        arr as crate::gc::GcRef,
        idx as usize,
        val,
        elem_bytes as usize,
    );
}

/// Get array length.
#[no_mangle]
pub extern "C" fn vo_array_len(arr: u64) -> u64 {
    use crate::objects::array;
    if arr == 0 {
        return 0;
    }
    array::len(arr as crate::gc::GcRef) as u64
}

// =============================================================================
// Slice Helpers
// =============================================================================

/// Create a new slice with validation (unified logic for VM and JIT).
/// Returns: error code (0 = success), writes result to *out on success.
#[no_mangle]
pub extern "C" fn vo_slice_new_checked(
    gc: *mut Gc,
    elem_meta: u32,
    elem_bytes: u32,
    len: i64,
    cap: i64,
    out: *mut u64,
) -> i32 {
    use crate::objects::slice;
    unsafe {
        match slice::create_checked(&mut *gc, elem_meta, elem_bytes as usize, len, cap) {
            Ok(result) => {
                *out = result as u64;
                0
            }
            Err(code) => code,
        }
    }
}

/// Get slice length.
#[no_mangle]
pub extern "C" fn vo_slice_len(s: u64) -> u64 {
    if s == 0 {
        return 0;
    }
    use crate::objects::slice;
    slice::len(s as crate::gc::GcRef) as u64
}

/// Get slice capacity.
#[no_mangle]
pub extern "C" fn vo_slice_cap(s: u64) -> u64 {
    if s == 0 {
        return 0;
    }
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
/// Go semantics: nil[0:0] == nil (null input returns null when lo==0 and hi==0).
#[no_mangle]
pub extern "C" fn vo_slice_slice(gc: *mut Gc, s: u64, lo: u64, hi: u64) -> u64 {
    use crate::objects::slice;
    if s == 0 {
        return if lo == 0 && hi == 0 { 0 } else { u64::MAX };
    }
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
/// Go semantics: nil[0:0:0] == nil (null input returns null when all indices are 0).
#[no_mangle]
pub extern "C" fn vo_slice_slice3(gc: *mut Gc, s: u64, lo: u64, hi: u64, max: u64) -> u64 {
    use crate::objects::slice;
    if s == 0 {
        return if lo == 0 && hi == 0 && max == 0 {
            0
        } else {
            u64::MAX
        };
    }
    unsafe {
        let gc = &mut *gc;
        match slice::slice_of_with_cap(
            gc,
            s as crate::gc::GcRef,
            lo as usize,
            hi as usize,
            max as usize,
        ) {
            Some(r) => r as u64,
            None => u64::MAX,
        }
    }
}

/// Append single element to slice.
/// elem_bytes: actual byte size per element
/// val_ptr points to ceil(elem_bytes / SLOT_BYTES) u64 values.
#[no_mangle]
pub extern "C" fn vo_slice_append(
    ctx: *mut JitContext,
    elem_meta: u32,
    elem_bytes: u32,
    s: u64,
    val_ptr: *const u64,
) -> u64 {
    use crate::objects::slice;
    use crate::ValueMeta;

    if ctx.is_null() || val_ptr.is_null() {
        return JIT_HELPER_U64_ERROR;
    }

    unsafe {
        let ctx_ref = &mut *ctx;
        if ctx_ref.gc.is_null() {
            return set_invalid_metadata_u64(ctx, JIT_HELPER_TYPED_WRITE_BARRIER_LAYOUT);
        }
        let gc = &mut *ctx_ref.gc;
        let module = ctx_ref.module.as_ref();
        let elem_meta = ValueMeta::from_raw(elem_meta);
        if elem_meta.value_kind() == ValueKind::Struct {
            let missing = module
                .and_then(|module| module.struct_metas.get(elem_meta.meta_id() as usize))
                .is_none();
            if missing {
                return set_invalid_metadata_u64(ctx, JIT_HELPER_TYPED_WRITE_BARRIER_LAYOUT);
            }
        }
        let s_ref = s as crate::gc::GcRef;
        if !s_ref.is_null() {
            let actual_meta = slice::elem_meta(s_ref);
            if actual_meta.value_kind() == ValueKind::Struct {
                let missing = module
                    .and_then(|module| module.struct_metas.get(actual_meta.meta_id() as usize))
                    .is_none();
                if missing {
                    return set_invalid_metadata_u64(ctx, JIT_HELPER_TYPED_WRITE_BARRIER_LAYOUT);
                }
            }
        }
        let val_slots = slots_for_bytes(elem_bytes as usize);
        let val = core::slice::from_raw_parts(val_ptr, val_slots);
        match slice::try_append(gc, elem_meta, elem_bytes as usize, s_ref, val, module) {
            Ok(result) => result as u64,
            Err(_) => set_invalid_metadata_u64(ctx, JIT_HELPER_TYPED_WRITE_BARRIER_LAYOUT),
        }
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
        match slice::array_slice_with_cap(
            gc,
            arr as crate::gc::GcRef,
            lo as usize,
            hi as usize,
            max as usize,
        ) {
            Some(r) => r as u64,
            None => u64::MAX,
        }
    }
}

// =============================================================================
// Interface Helpers
// =============================================================================

/// Extract named_type_id from RuntimeType (recursively unwraps Pointer).
fn extract_named_type_id(
    rt: &crate::RuntimeType,
    runtime_types: &[crate::RuntimeType],
) -> Option<u32> {
    use crate::RuntimeType;
    match rt {
        RuntimeType::Named { id, .. } => Some(*id),
        RuntimeType::Pointer(elem_value_rttid) => runtime_types
            .get(elem_value_rttid.rttid() as usize)
            .and_then(|inner| extract_named_type_id(inner, runtime_types)),
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

    if interface::is_nil(src_slot0) {
        return 0;
    }

    let src_rttid = interface::unpack_rttid(src_slot0);
    let src_vk = interface::unpack_value_kind(src_slot0);

    let ctx_ref = unsafe { &*ctx };
    let module = unsafe { &*ctx_ref.module };
    let itab_cache = unsafe { &mut *ctx_ref.itab_cache };

    let named_type_id_opt = module
        .runtime_types
        .get(src_rttid as usize)
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
/// Returns JitResult::Ok on success, JitResult::Panic for a language type assertion panic,
/// or JitResult::JitError for malformed runtime metadata.
/// dst layout: [result_slots...][ok_flag if has_ok]
#[no_mangle]
pub extern "C" fn vo_iface_assert(
    ctx: *mut JitContext,
    slot0: u64,
    slot1: u64,
    target_id: u32,
    flags: u16,
    dst: *mut u64,
) -> JitResult {
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
            let ok_slot = if assert_kind == 1 {
                2
            } else if target_slots > 1 {
                target_slots
            } else {
                1
            };
            *dst.add(ok_slot) = matches as u64;

            if matches {
                let result = write_iface_assert_success(
                    ctx,
                    slot0,
                    slot1,
                    assert_kind,
                    target_slots,
                    target_id,
                    dst,
                );
                if result != JitResult::Ok {
                    return result;
                }
            } else {
                // Zero out on failure
                let dst_slots = if assert_kind == 1 {
                    2
                } else {
                    target_slots.max(1)
                };
                for i in 0..dst_slots {
                    *dst.add(i) = 0;
                }
            }
            JitResult::Ok
        } else if matches {
            let result = write_iface_assert_success(
                ctx,
                slot0,
                slot1,
                assert_kind,
                target_slots,
                target_id,
                dst,
            );
            if result != JitResult::Ok {
                return result;
            }
            JitResult::Ok
        } else {
            let ctx_ref = &mut *ctx;
            *ctx_ref.panic_flag = false;
            *ctx_ref.is_user_panic = false;
            ctx_ref.runtime_trap_kind = JitRuntimeTrapKind::TypeAssertionFailed as u8;
            ctx_ref.runtime_trap_arg0 = 0;
            ctx_ref.runtime_trap_arg1 = 0;
            JitResult::Panic
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
) -> JitResult {
    use crate::gc::GcRef;
    use crate::objects::interface;
    use crate::ValueKind;

    let src_rttid = interface::unpack_rttid(slot0);
    let src_vk = interface::unpack_value_kind(slot0);

    if assert_kind == 1 {
        // Interface assertion: create new itab and write result
        let ctx_ref = &*ctx;
        let module = &*ctx_ref.module;
        let itab_cache = &mut *ctx_ref.itab_cache;

        let new_itab_id = if target_id == 0 {
            0
        } else {
            let Some(named_type_id) = module
                .runtime_types
                .get(src_rttid as usize)
                .and_then(|rt| extract_named_type_id(rt, &module.runtime_types))
            else {
                return set_jit_infra_error(
                    ctx,
                    JIT_INFRA_ERROR_INVALID_METADATA,
                    JIT_CALLBACK_IFACE_ASSERT,
                );
            };
            // Value types (non-pointer) cannot use pointer receiver methods
            let src_is_pointer = src_vk == ValueKind::Pointer;
            itab_cache.get_or_create(
                named_type_id,
                target_id,
                src_is_pointer,
                &module.named_type_metas,
                &module.interface_metas,
            )
        };
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
    JitResult::Ok
}

// =============================================================================
// Slice Copy
// =============================================================================

/// Copy elements from src slice to dst slice (for JIT).
/// Returns the number of elements copied (min of dst len and src len).
#[no_mangle]
pub extern "C" fn vo_copy(dst: u64, src: u64) -> u64 {
    use crate::gc::GcRef;
    use crate::objects::{array, slice};

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

    unsafe { core::ptr::copy(src_ptr, dst_ptr, copy_len * elem_bytes) };

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
        (
            "vo_gc_typed_write_barrier_by_meta",
            vo_gc_typed_write_barrier_by_meta as *const u8,
        ),
        ("vo_gc_safepoint", vo_gc_safepoint as *const u8),
        ("vo_panic", vo_panic as *const u8),
        ("vo_runtime_trap", vo_runtime_trap as *const u8),
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
        ("vo_queue_new_checked", vo_queue_new_checked as *const u8),
        ("vo_chan_len", vo_chan_len as *const u8),
        ("vo_chan_cap", vo_chan_cap as *const u8),
        ("vo_array_new", vo_array_new as *const u8),
        ("vo_array_get", vo_array_get as *const u8),
        ("vo_array_set", vo_array_set as *const u8),
        ("vo_array_len", vo_array_len as *const u8),
        ("vo_slice_new_checked", vo_slice_new_checked as *const u8),
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
        ("vo_set_call_request", vo_set_call_request as *const u8),
        ("vo_ptr_clone", vo_ptr_clone as *const u8),
        ("vo_map_new", vo_map_new as *const u8),
        ("vo_map_len", vo_map_len as *const u8),
        ("vo_map_get", vo_map_get as *const u8),
        ("vo_map_set", vo_map_set as *const u8),
        ("vo_map_delete", vo_map_delete as *const u8),
        ("vo_map_iter_init", vo_map_iter_init as *const u8),
        ("vo_map_iter_next", vo_map_iter_next as *const u8),
        ("vo_copy", vo_copy as *const u8),
        ("vo_island_new", vo_island_new as *const u8),
        ("vo_chan_close", vo_chan_close as *const u8),
        ("vo_chan_send", vo_chan_send as *const u8),
        ("vo_chan_recv", vo_chan_recv as *const u8),
        ("vo_go_start", vo_go_start as *const u8),
        ("vo_go_island", vo_go_island as *const u8),
        ("vo_defer_push", vo_defer_push as *const u8),
        ("vo_recover", vo_recover as *const u8),
        ("vo_select_begin", vo_select_begin as *const u8),
        ("vo_select_send", vo_select_send as *const u8),
        ("vo_select_recv", vo_select_recv as *const u8),
        ("vo_select_exec", vo_select_exec as *const u8),
    ]
}

pub fn runtime_symbol_names() -> &'static [&'static str] {
    &[
        "vo_gc_alloc",
        "vo_gc_write_barrier",
        "vo_gc_typed_write_barrier_by_meta",
        "vo_gc_safepoint",
        "vo_panic",
        "vo_runtime_trap",
        "vo_call_extern",
        "vo_str_new",
        "vo_str_len",
        "vo_str_index",
        "vo_str_concat",
        "vo_str_slice",
        "vo_str_eq",
        "vo_str_cmp",
        "vo_str_decode_rune",
        "vo_closure_new",
        "vo_queue_new_checked",
        "vo_chan_len",
        "vo_chan_cap",
        "vo_array_new",
        "vo_array_get",
        "vo_array_set",
        "vo_array_len",
        "vo_slice_new_checked",
        "vo_slice_len",
        "vo_slice_cap",
        "vo_slice_get",
        "vo_slice_set",
        "vo_slice_slice",
        "vo_slice_slice3",
        "vo_slice_append",
        "vo_slice_from_array",
        "vo_slice_from_array3",
        "vo_iface_pack_slot0",
        "vo_iface_to_iface",
        "vo_iface_eq",
        "vo_iface_assert",
        "vo_set_call_request",
        "vo_ptr_clone",
        "vo_map_new",
        "vo_map_len",
        "vo_map_get",
        "vo_map_set",
        "vo_map_delete",
        "vo_map_iter_init",
        "vo_map_iter_next",
        "vo_copy",
        "vo_island_new",
        "vo_chan_close",
        "vo_chan_send",
        "vo_chan_recv",
        "vo_go_start",
        "vo_go_island",
        "vo_defer_push",
        "vo_recover",
        "vo_select_begin",
        "vo_select_send",
        "vo_select_recv",
        "vo_select_exec",
    ]
}

pub fn runtime_helper_abi_fields() -> &'static [JitRuntimeHelperAbi] {
    use JitAbiType as T;
    use JitRuntimeHelperPanicPolicy as Panic;
    use JitRuntimeHelperReturnPolicy as Ret;

    &[
        JitRuntimeHelperAbi {
            name: "vo_gc_alloc",
            params: &[T::Ptr, T::U32, T::U32],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_gc_write_barrier",
            params: &[T::Ptr, T::U64, T::U32, T::U64],
            ret: T::Void,
            return_policy: Ret::Void,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_gc_typed_write_barrier_by_meta",
            params: &[T::Ptr, T::U64, T::Ptr, T::U32, T::U32],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_gc_safepoint",
            params: &[T::Ptr],
            ret: T::Void,
            return_policy: Ret::Void,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_panic",
            params: &[T::Ptr, T::U64, T::U64],
            ret: T::Void,
            return_policy: Ret::Void,
            panic_policy: Panic::RecordsUserPanic,
            may_gc: false,
            may_schedule: false,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_runtime_trap",
            params: &[T::Ptr, T::U32, T::U64, T::U64, T::U32],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::RecordsRuntimeTrap,
            may_gc: false,
            may_schedule: false,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_call_extern",
            params: &[T::Ptr, T::U32, T::Ptr, T::U32, T::Ptr, T::U32],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_str_new",
            params: &[T::Ptr, T::Ptr, T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_str_len",
            params: &[T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_str_index",
            params: &[T::U64, T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_str_concat",
            params: &[T::Ptr, T::U64, T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_str_slice",
            params: &[T::Ptr, T::U64, T::U64, T::U64],
            ret: T::U64,
            return_policy: Ret::U64ErrorSentinel,
            panic_policy: Panic::ReturnsStatusOrSentinel,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_str_eq",
            params: &[T::U64, T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_str_cmp",
            params: &[T::U64, T::U64],
            ret: T::I32,
            return_policy: Ret::RawI32,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_str_decode_rune",
            params: &[T::U64, T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_closure_new",
            params: &[T::Ptr, T::U32, T::U32],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_queue_new_checked",
            params: &[T::Ptr, T::U32, T::U64, T::U32, T::I64, T::Ptr],
            ret: T::I32,
            return_policy: Ret::I32StatusOutPointer,
            panic_policy: Panic::ReturnsStatusOrSentinel,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_chan_len",
            params: &[T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_chan_cap",
            params: &[T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_array_new",
            params: &[T::Ptr, T::U32, T::U32, T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_array_get",
            params: &[T::U64, T::U64, T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_array_set",
            params: &[T::U64, T::U64, T::U64, T::U64],
            ret: T::Void,
            return_policy: Ret::Void,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_array_len",
            params: &[T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_slice_new_checked",
            params: &[T::Ptr, T::U32, T::U32, T::I64, T::I64, T::Ptr],
            ret: T::I32,
            return_policy: Ret::I32StatusOutPointer,
            panic_policy: Panic::ReturnsStatusOrSentinel,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_slice_len",
            params: &[T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_slice_cap",
            params: &[T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_slice_get",
            params: &[T::U64, T::U64, T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_slice_set",
            params: &[T::U64, T::U64, T::U64, T::U64],
            ret: T::Void,
            return_policy: Ret::Void,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_slice_slice",
            params: &[T::Ptr, T::U64, T::U64, T::U64],
            ret: T::U64,
            return_policy: Ret::U64ErrorSentinel,
            panic_policy: Panic::ReturnsStatusOrSentinel,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_slice_slice3",
            params: &[T::Ptr, T::U64, T::U64, T::U64, T::U64],
            ret: T::U64,
            return_policy: Ret::U64ErrorSentinel,
            panic_policy: Panic::ReturnsStatusOrSentinel,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_slice_append",
            params: &[T::Ptr, T::U32, T::U32, T::U64, T::Ptr],
            ret: T::U64,
            return_policy: Ret::U64ErrorSentinel,
            panic_policy: Panic::ReturnsStatusOrSentinel,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_slice_from_array",
            params: &[T::Ptr, T::U64, T::U64, T::U64],
            ret: T::U64,
            return_policy: Ret::U64ErrorSentinel,
            panic_policy: Panic::ReturnsStatusOrSentinel,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_slice_from_array3",
            params: &[T::Ptr, T::U64, T::U64, T::U64, T::U64],
            ret: T::U64,
            return_policy: Ret::U64ErrorSentinel,
            panic_policy: Panic::ReturnsStatusOrSentinel,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_iface_pack_slot0",
            params: &[T::U32, T::U32, T::U8],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_iface_to_iface",
            params: &[T::Ptr, T::U64, T::U32],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_iface_eq",
            params: &[T::Ptr, T::U64, T::U64, T::U64, T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::RecordsRuntimeTrap,
            may_gc: false,
            may_schedule: false,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_iface_assert",
            params: &[T::Ptr, T::U64, T::U64, T::U32, T::U16, T::Ptr],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: false,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_set_call_request",
            params: &[T::Ptr, T::U32, T::U32, T::U32, T::U32, T::U32, T::U32],
            ret: T::Void,
            return_policy: Ret::Void,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_ptr_clone",
            params: &[T::Ptr, T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_map_new",
            params: &[T::Ptr, T::U32, T::U32, T::U32, T::U32, T::U32],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: true,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_map_len",
            params: &[T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_map_get",
            params: &[T::Ptr, T::U64, T::Ptr, T::U32, T::Ptr, T::U32],
            ret: T::U64,
            return_policy: Ret::U64ErrorSentinel,
            panic_policy: Panic::ReturnsStatusOrSentinel,
            may_gc: false,
            may_schedule: false,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_map_set",
            params: &[T::Ptr, T::U64, T::Ptr, T::U32, T::Ptr, T::U32],
            ret: T::U64,
            return_policy: Ret::U64ErrorSentinel,
            panic_policy: Panic::ReturnsStatusOrSentinel,
            may_gc: true,
            may_schedule: false,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_map_delete",
            params: &[T::Ptr, T::U64, T::Ptr, T::U32],
            ret: T::U64,
            return_policy: Ret::U64ErrorSentinel,
            panic_policy: Panic::ReturnsStatusOrSentinel,
            may_gc: false,
            may_schedule: false,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_map_iter_init",
            params: &[T::U64, T::Ptr],
            ret: T::Void,
            return_policy: Ret::Void,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_map_iter_next",
            params: &[T::Ptr, T::Ptr, T::Ptr, T::U32, T::Ptr, T::U32],
            ret: T::U64,
            return_policy: Ret::U64ErrorSentinel,
            panic_policy: Panic::ReturnsStatusOrSentinel,
            may_gc: false,
            may_schedule: false,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_copy",
            params: &[T::U64, T::U64],
            ret: T::U64,
            return_policy: Ret::RawU64,
            panic_policy: Panic::MustNotPanicAcrossAbi,
            may_gc: false,
            may_schedule: false,
            observes_frame: false,
        },
        JitRuntimeHelperAbi {
            name: "vo_island_new",
            params: &[T::Ptr, T::Ptr],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_chan_close",
            params: &[T::Ptr, T::U64],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_chan_send",
            params: &[T::Ptr, T::U64, T::Ptr, T::U32],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_chan_recv",
            params: &[T::Ptr, T::U64, T::Ptr, T::U32, T::U32],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_go_start",
            params: &[T::Ptr, T::U32, T::U32, T::U64, T::Ptr, T::U32],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_go_island",
            params: &[T::Ptr, T::U64, T::U64, T::Ptr, T::U32],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_defer_push",
            params: &[
                T::Ptr,
                T::U32,
                T::U32,
                T::U64,
                T::U32,
                T::Ptr,
                T::U32,
                T::U32,
            ],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: false,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_recover",
            params: &[T::Ptr, T::Ptr],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: false,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_select_begin",
            params: &[T::Ptr, T::U32, T::U32],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_select_send",
            params: &[T::Ptr, T::U32, T::U32, T::U32, T::U32],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_select_recv",
            params: &[T::Ptr, T::U32, T::U32, T::U32, T::U32, T::U32],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
        JitRuntimeHelperAbi {
            name: "vo_select_exec",
            params: &[T::Ptr, T::U32],
            ret: T::JitResult,
            return_policy: Ret::JitResult,
            panic_policy: Panic::ReturnsJitResult,
            may_gc: true,
            may_schedule: true,
            observes_frame: true,
        },
    ]
}

// =============================================================================
// Island/Channel JIT Helpers
// =============================================================================

/// Create a new island.
/// Calls into VM via callback to properly register with scheduler.
#[no_mangle]
pub extern "C" fn vo_island_new(ctx: *mut JitContext, out: *mut u64) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let Some(create_fn) = ctx.create_island_fn else {
        return missing_callback(ctx, JIT_CALLBACK_CREATE_ISLAND);
    };
    unsafe {
        let handle = create_fn(ctx);
        if ctx.runtime_trap_arg0 == JIT_INFRA_ERROR_SENTINEL {
            return JitResult::JitError;
        }
        *out = handle;
    }
    JitResult::Ok
}

/// Close a channel.
/// Returns JitResult::Ok on success, JitResult::Panic on nil/closed channel.
#[no_mangle]
pub extern "C" fn vo_chan_close(ctx: *mut JitContext, chan: u64) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let Some(close_fn) = ctx.queue_close_fn else {
        return missing_callback(ctx, JIT_CALLBACK_QUEUE_CLOSE);
    };
    close_fn(ctx, chan)
}

// =============================================================================
// Channel Send/Recv
// =============================================================================

/// Send on a channel.
/// Returns JitResult::Ok on success, JitResult::Panic on nil/closed channel,
/// or JitResult::WaitIo if the send would block.
#[no_mangle]
pub extern "C" fn vo_chan_send(
    ctx: *mut JitContext,
    chan: u64,
    val_ptr: *const u64,
    val_slots: u32,
) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let Some(send_fn) = ctx.queue_send_fn else {
        return missing_callback(ctx, JIT_CALLBACK_QUEUE_SEND);
    };
    send_fn(ctx, chan, val_ptr, val_slots)
}

/// Receive from a channel.
/// Returns JitResult::Ok on success (including closed channel),
/// JitResult::Panic on nil channel, or JitResult::WaitIo if would block.
#[no_mangle]
pub extern "C" fn vo_chan_recv(
    ctx: *mut JitContext,
    chan: u64,
    dst_ptr: *mut u64,
    elem_slots: u32,
    has_ok: u32,
) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let Some(recv_fn) = ctx.queue_recv_fn else {
        return missing_callback(ctx, JIT_CALLBACK_QUEUE_RECV);
    };
    recv_fn(ctx, chan, dst_ptr, elem_slots, has_ok)
}

// =============================================================================
// Goroutine Start
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
) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let Some(go_fn) = ctx.go_start_fn else {
        return missing_callback(ctx, JIT_CALLBACK_GO_START);
    };
    go_fn(ctx, func_id, is_closure, closure_ref, args_ptr, arg_slots)
}

/// Spawn a goroutine on a specific island.
/// If island_id == 0, spawns locally. Otherwise sends to remote island.
#[no_mangle]
pub extern "C" fn vo_go_island(
    ctx: *mut JitContext,
    island: u64,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_slots: u32,
) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let Some(go_fn) = ctx.go_island_fn else {
        return missing_callback(ctx, JIT_CALLBACK_GO_ISLAND);
    };
    go_fn(ctx, island, closure_ref, args_ptr, arg_slots)
}

// =============================================================================
// Select Statement
// =============================================================================

/// Initialize a select statement.
#[no_mangle]
pub extern "C" fn vo_select_begin(
    ctx: *mut JitContext,
    case_count: u32,
    has_default: u32,
) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let Some(begin_fn) = ctx.select_begin_fn else {
        return missing_callback(ctx, JIT_CALLBACK_SELECT_BEGIN);
    };
    begin_fn(ctx, case_count, has_default)
}

/// Add a send case to the current select.
#[no_mangle]
pub extern "C" fn vo_select_send(
    ctx: *mut JitContext,
    queue_reg: u32,
    val_reg: u32,
    elem_slots: u32,
    case_idx: u32,
) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let Some(send_fn) = ctx.select_send_fn else {
        return missing_callback(ctx, JIT_CALLBACK_SELECT_SEND);
    };
    send_fn(ctx, queue_reg, val_reg, elem_slots, case_idx)
}

/// Add a recv case to the current select.
#[no_mangle]
pub extern "C" fn vo_select_recv(
    ctx: *mut JitContext,
    dst_reg: u32,
    queue_reg: u32,
    elem_slots: u32,
    has_ok: u32,
    case_idx: u32,
) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let Some(recv_fn) = ctx.select_recv_fn else {
        return missing_callback(ctx, JIT_CALLBACK_SELECT_RECV);
    };
    recv_fn(ctx, dst_reg, queue_reg, elem_slots, has_ok, case_idx)
}

/// Execute the select statement.
/// Returns JitResult::Ok (result written to fiber stack), WaitIo (blocked), or Panic.
#[no_mangle]
pub extern "C" fn vo_select_exec(ctx: *mut JitContext, result_reg: u32) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let Some(exec_fn) = ctx.select_exec_fn else {
        return missing_callback(ctx, JIT_CALLBACK_SELECT_EXEC);
    };
    exec_fn(ctx, result_reg)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn runtime_symbols_include_jit_control_helpers() {
        let symbols = get_runtime_symbols();
        assert!(symbols
            .iter()
            .any(|(name, _)| *name == "vo_set_call_request"));
        assert!(symbols.iter().any(|(name, _)| *name == "vo_defer_push"));
        assert!(symbols.iter().any(|(name, _)| *name == "vo_recover"));
    }

    #[test]
    fn jit_abi_wrappers_do_not_unwrap_missing_callbacks() {
        let src = include_str!("jit_api.rs");
        let start = src
            .find("// Island/Channel JIT Helpers")
            .expect("JIT helper section");
        let end = src.find("#[cfg(test)]").expect("test section");
        let wrappers = &src[start..end];
        assert!(
            !wrappers.contains(".expect("),
            "JIT ABI wrappers must return JitResult::JitError instead of panicking"
        );
        assert!(
            !wrappers.contains("JIT ABI violation"),
            "JIT ABI wrappers must use the machine-readable infra-error path"
        );
        assert!(
            !wrappers.contains("*result_ptr = 0"),
            "JIT ABI wrappers must not write default zero values before surfacing missing callbacks"
        );
        let recover_start = src
            .find("pub extern \"C\" fn vo_recover")
            .expect("vo_recover wrapper");
        let recover_end = src[recover_start..]
            .find("/// Trigger a user panic")
            .map(|offset| recover_start + offset)
            .expect("vo_recover end marker");
        let recover_wrapper = &src[recover_start..recover_end];
        assert!(
            !recover_wrapper.contains("*result_ptr = 0"),
            "vo_recover must not write a default nil interface before surfacing a missing callback"
        );
    }

    #[test]
    fn jit_map_helpers_do_not_min_copy_layout_mismatches() {
        let src = include_str!("jit_api.rs");
        let start = src
            .find("pub extern \"C\" fn vo_map_get")
            .expect("vo_map_get helper");
        let end = src[start..]
            .find("// =============================================================================\n// String Helpers")
            .map(|offset| start + offset)
            .expect("string helper section");
        let map_helpers = &src[start..end];

        assert!(
            !map_helpers.contains(".min(") && !map_helpers.contains("copy_len"),
            "JIT map helpers must treat key/value slot counts as exact ABI metadata, not silently min-copy layout mismatches"
        );
    }

    #[test]
    fn jit_copy_helper_uses_overlap_safe_memmove_semantics() {
        let src = include_str!("jit_api.rs");
        let start = src
            .find("pub extern \"C\" fn vo_copy")
            .expect("vo_copy helper");
        let end = src[start..]
            .find("// =============================================================================\n// Symbol Registration")
            .map(|offset| start + offset)
            .expect("symbol registration section");
        let copy_helper = &src[start..end];

        assert!(
            copy_helper.contains("core::ptr::copy("),
            "JIT copy helper must use overlap-safe memmove semantics for slice copy"
        );
        assert!(
            !copy_helper.contains("copy_nonoverlapping"),
            "JIT copy helper must not use non-overlap copy; source and destination slices can alias"
        );
    }

    #[test]
    fn typed_write_barrier_helper_reports_invalid_struct_meta_as_jit_error() {
        let safepoint_flag = false;
        let mut panic_flag = false;
        let mut is_user_panic = false;
        let mut panic_msg = InterfaceSlot::nil();
        let output = crate::output::CaptureSink::new();
        let program_args = Vec::new();
        let mut sentinel_errors = crate::ffi::SentinelErrorCache::new();
        let mut host_output = None;
        let mut module = Module::new("test".to_string());
        module.struct_metas.clear();
        let mut gc = Gc::new();
        let parent = gc.alloc(crate::ValueMeta::new(0, ValueKind::Array), 1);
        let vals = [0_u64];
        let mut ctx = JitContext {
            gc: &mut gc,
            globals: core::ptr::null_mut(),
            safepoint_flag: &safepoint_flag,
            panic_flag: &mut panic_flag,
            is_user_panic: &mut is_user_panic,
            panic_msg: &mut panic_msg,
            user_panic_pc: u32::MAX,
            runtime_trap_kind: JitRuntimeTrapKind::None as u8,
            runtime_trap_arg0: 0,
            runtime_trap_arg1: 0,
            runtime_trap_pc: u32::MAX,
            vm: core::ptr::null_mut(),
            fiber: core::ptr::null_mut(),
            itab_cache: core::ptr::null_mut(),
            extern_registry: core::ptr::null(),
            call_extern_fn: None,
            module: &module,
            jit_func_table: core::ptr::null(),
            jit_func_count: 0,
            direct_call_table: core::ptr::null(),
            direct_call_count: 0,
            program_args: &program_args,
            sentinel_errors: &mut sentinel_errors,
            output: &*output as *const dyn crate::output::OutputSink,
            host_output: &mut host_output,
            #[cfg(feature = "std")]
            io: core::ptr::null_mut(),
            call_func_id: 0,
            call_arg_start: 0,
            call_resume_pc: 0,
            call_ret_slots: 0,
            call_ret_reg: 0,
            call_kind: 0,
            #[cfg(feature = "std")]
            wait_io_token: 0,
            loop_exit_pc: 0,
            stack_ptr: core::ptr::null_mut(),
            stack_cap: 0,
            stack_limit: 0,
            call_depth: 0,
            call_depth_limit: 0,
            jit_bp: 0,
            fiber_sp: 0,
            push_frame_fn: None,
            pop_frame_fn: None,
            stack_overflow_fn: None,
            push_resume_point_fn: None,
            create_island_fn: None,
            queue_close_fn: None,
            queue_send_fn: None,
            queue_recv_fn: None,
            go_start_fn: None,
            go_island_fn: None,
            defer_push_fn: None,
            recover_fn: None,
            select_begin_fn: None,
            select_send_fn: None,
            select_recv_fn: None,
            select_exec_fn: None,
            is_error_return: 0,
            ret_gcref_start: 0,
            ret_is_heap: 0,
            ret_start: 0,
            prepare_closure_call_fn: None,
            prepare_iface_call_fn: None,
            ic_table: core::ptr::null_mut(),
        };

        let result = vo_gc_typed_write_barrier_by_meta(
            &mut ctx,
            parent as u64,
            vals.as_ptr(),
            vals.len() as u32,
            crate::ValueMeta::new(123, ValueKind::Struct).to_raw(),
        );

        assert_eq!(result, JitResult::JitError);
        assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
        assert_eq!(
            ctx.runtime_trap_pc,
            JIT_HELPER_TYPED_WRITE_BARRIER_LAYOUT as u32
        );
    }

    #[test]
    fn slice_append_metadata_drift_returns_sentinel_instead_of_panicking() {
        let safepoint_flag = false;
        let mut panic_flag = false;
        let mut is_user_panic = false;
        let mut panic_msg = InterfaceSlot::nil();
        let output = crate::output::CaptureSink::new();
        let program_args = Vec::new();
        let mut sentinel_errors = crate::ffi::SentinelErrorCache::new();
        let mut host_output = None;
        let mut module = Module::new("test".to_string());
        module.struct_metas.clear();
        let mut gc = Gc::new();
        let elem_meta = crate::ValueMeta::new(123, ValueKind::Struct);
        let slice = crate::objects::slice::create(&mut gc, elem_meta, 8, 1, 2);
        let vals = [0_u64];
        let mut ctx = JitContext {
            gc: &mut gc,
            globals: core::ptr::null_mut(),
            safepoint_flag: &safepoint_flag,
            panic_flag: &mut panic_flag,
            is_user_panic: &mut is_user_panic,
            panic_msg: &mut panic_msg,
            user_panic_pc: u32::MAX,
            runtime_trap_kind: JitRuntimeTrapKind::None as u8,
            runtime_trap_arg0: 0,
            runtime_trap_arg1: 0,
            runtime_trap_pc: u32::MAX,
            vm: core::ptr::null_mut(),
            fiber: core::ptr::null_mut(),
            itab_cache: core::ptr::null_mut(),
            extern_registry: core::ptr::null(),
            call_extern_fn: None,
            module: &module,
            jit_func_table: core::ptr::null(),
            jit_func_count: 0,
            direct_call_table: core::ptr::null(),
            direct_call_count: 0,
            program_args: &program_args,
            sentinel_errors: &mut sentinel_errors,
            output: &*output as *const dyn crate::output::OutputSink,
            host_output: &mut host_output,
            #[cfg(feature = "std")]
            io: core::ptr::null_mut(),
            call_func_id: 0,
            call_arg_start: 0,
            call_resume_pc: 0,
            call_ret_slots: 0,
            call_ret_reg: 0,
            call_kind: 0,
            #[cfg(feature = "std")]
            wait_io_token: 0,
            loop_exit_pc: 0,
            stack_ptr: core::ptr::null_mut(),
            stack_cap: 0,
            stack_limit: 0,
            call_depth: 0,
            call_depth_limit: 0,
            jit_bp: 0,
            fiber_sp: 0,
            push_frame_fn: None,
            pop_frame_fn: None,
            stack_overflow_fn: None,
            push_resume_point_fn: None,
            create_island_fn: None,
            queue_close_fn: None,
            queue_send_fn: None,
            queue_recv_fn: None,
            go_start_fn: None,
            go_island_fn: None,
            defer_push_fn: None,
            recover_fn: None,
            select_begin_fn: None,
            select_send_fn: None,
            select_recv_fn: None,
            select_exec_fn: None,
            is_error_return: 0,
            ret_gcref_start: 0,
            ret_is_heap: 0,
            ret_start: 0,
            prepare_closure_call_fn: None,
            prepare_iface_call_fn: None,
            ic_table: core::ptr::null_mut(),
        };

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            vo_slice_append(&mut ctx, elem_meta.to_raw(), 8, slice as u64, vals.as_ptr())
        }));

        let result = result.expect("vo_slice_append must not panic across extern C ABI");
        assert_eq!(result, JIT_HELPER_U64_ERROR);
        assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
        assert_eq!(
            ctx.runtime_trap_pc,
            JIT_HELPER_TYPED_WRITE_BARRIER_LAYOUT as u32
        );
    }

    #[test]
    fn nil_slice_slice_helpers_only_accept_zero_bounds() {
        let mut gc = Gc::new();

        assert_eq!(vo_slice_slice(&mut gc, 0, 0, 0), 0);
        assert_eq!(vo_slice_slice(&mut gc, 0, 1, 1), JIT_HELPER_U64_ERROR);
        assert_eq!(vo_slice_slice(&mut gc, 0, 0, 1), JIT_HELPER_U64_ERROR);

        assert_eq!(vo_slice_slice3(&mut gc, 0, 0, 0, 0), 0);
        assert_eq!(vo_slice_slice3(&mut gc, 0, 0, 0, 1), JIT_HELPER_U64_ERROR);
        assert_eq!(vo_slice_slice3(&mut gc, 0, 1, 1, 1), JIT_HELPER_U64_ERROR);
    }

    #[test]
    fn str_slice_helper_reports_bounds_errors_with_sentinel() {
        let mut gc = Gc::new();
        let s = crate::objects::string::from_rust_str(&mut gc, "abc") as u64;

        assert_ne!(vo_str_slice(&mut gc, s, 1, 1), JIT_HELPER_U64_ERROR);
        assert_eq!(vo_str_slice(&mut gc, s, 2, 1), JIT_HELPER_U64_ERROR);
        assert_eq!(vo_str_slice(&mut gc, s, 0, 4), JIT_HELPER_U64_ERROR);
        assert_eq!(vo_str_slice(&mut gc, 0, 1, 1), JIT_HELPER_U64_ERROR);
    }

    #[test]
    fn runtime_symbol_name_manifest_matches_registered_symbols() {
        let symbols = get_runtime_symbols();
        let names = runtime_symbol_names();
        assert_eq!(symbols.len(), names.len());
        for ((registered, _), manifest) in symbols.iter().zip(names.iter()) {
            assert_eq!(registered, manifest);
        }
    }

    #[test]
    fn runtime_helper_abi_manifest_matches_registered_symbols() {
        let names = runtime_symbol_names();
        let abi = runtime_helper_abi_fields();
        assert_eq!(abi.len(), names.len());
        for (field, manifest) in abi.iter().zip(names.iter()) {
            assert_eq!(field.name, *manifest);
            assert!(
                !field.params.is_empty()
                    || matches!(field.return_policy, JitRuntimeHelperReturnPolicy::RawU64),
                "{} should declare its ABI parameters explicitly",
                field.name
            );
            if field.may_schedule {
                assert!(
                    field.observes_frame,
                    "{} may schedule and must observe/materialize the frame",
                    field.name
                );
            }
            if field.ret == JitAbiType::JitResult {
                assert_eq!(
                    field.return_policy,
                    JitRuntimeHelperReturnPolicy::JitResult,
                    "{} returns JitResult and must be checked by lowering",
                    field.name
                );
            }
            if matches!(
                field.return_policy,
                JitRuntimeHelperReturnPolicy::JitResult
                    | JitRuntimeHelperReturnPolicy::I32StatusOutPointer
                    | JitRuntimeHelperReturnPolicy::U64ErrorSentinel
            ) {
                assert_ne!(
                    field.panic_policy,
                    JitRuntimeHelperPanicPolicy::MustNotPanicAcrossAbi,
                    "{} has a control-flow-significant status but no failure policy",
                    field.name
                );
            }
        }
    }

    #[test]
    fn jit_callback_abi_manifest_is_sorted_unique_and_machine_readable() {
        let fields = jit_callback_abi_fields();
        assert!(!fields.is_empty());
        let mut names = std::collections::BTreeSet::new();
        let mut ids = std::collections::BTreeSet::new();
        for field in fields {
            assert!(
                names.insert(field.name),
                "duplicate callback {}",
                field.name
            );
            if let Some(id) = field.infra_error_id {
                assert!(ids.insert(id), "duplicate JIT callback infra-error id {id}");
                assert!(id > 0, "callback infra-error id must be non-zero");
            }
            if field.may_schedule {
                assert!(
                    field.observes_frame,
                    "{} may schedule and must observe/materialize the frame",
                    field.name
                );
            }
            if field.kind != JitContextDependencyKind::InlineCacheTable {
                assert_eq!(
                    field.params.first(),
                    Some(&JitAbiType::Ptr),
                    "{} callback must take JitContext as its first ABI parameter",
                    field.name
                );
            }
            match field.return_policy {
                JitCallbackReturnPolicy::RawVoid => assert_eq!(field.ret, JitAbiType::Void),
                JitCallbackReturnPolicy::RawPointer | JitCallbackReturnPolicy::TablePointer => {
                    assert_eq!(field.ret, JitAbiType::Ptr)
                }
                JitCallbackReturnPolicy::RawHandle => assert_eq!(field.ret, JitAbiType::U64),
                JitCallbackReturnPolicy::JitResult
                | JitCallbackReturnPolicy::JitResultWithOutPointer
                | JitCallbackReturnPolicy::PreparedCallOutPointer => {
                    assert_eq!(field.ret, JitAbiType::JitResult)
                }
            }
        }
    }

    #[test]
    fn dyn_call_iface_key_keeps_full_itab_id() {
        let method_idx = 7;
        let low = DynCallIC::iface_key(0x0000_0002, method_idx);
        let high = DynCallIC::iface_key(0x0001_0002, method_idx);
        let legacy_low = (0x0000_0002_u32 << 16) | method_idx;
        let legacy_high = (0x0001_0002_u32 << 16) | method_idx;

        assert_eq!(legacy_low, legacy_high, "legacy u32 key collided");
        assert_ne!(low, high, "tagged key must retain high itab_id bits");
        assert_ne!(low, DynCallIC::closure_key(0x0000_0002));
    }

    #[test]
    fn call_extern_missing_callback_uses_infra_error_path() {
        let safepoint_flag = false;
        let mut panic_flag = false;
        let mut is_user_panic = false;
        let mut panic_msg = InterfaceSlot::nil();
        let output = crate::output::CaptureSink::new();
        let program_args = Vec::new();
        let mut sentinel_errors = crate::ffi::SentinelErrorCache::new();
        let mut host_output = None;
        let module = Module::new("test".to_string());
        let mut ctx = JitContext {
            gc: core::ptr::null_mut(),
            globals: core::ptr::null_mut(),
            safepoint_flag: &safepoint_flag,
            panic_flag: &mut panic_flag,
            is_user_panic: &mut is_user_panic,
            panic_msg: &mut panic_msg,
            user_panic_pc: u32::MAX,
            runtime_trap_kind: JitRuntimeTrapKind::None as u8,
            runtime_trap_arg0: 0,
            runtime_trap_arg1: 0,
            runtime_trap_pc: u32::MAX,
            vm: core::ptr::null_mut(),
            fiber: core::ptr::null_mut(),
            itab_cache: core::ptr::null_mut(),
            extern_registry: core::ptr::null(),
            call_extern_fn: None,
            module: &module,
            jit_func_table: core::ptr::null(),
            jit_func_count: 0,
            direct_call_table: core::ptr::null(),
            direct_call_count: 0,
            program_args: &program_args,
            sentinel_errors: &mut sentinel_errors,
            output: &*output as *const dyn crate::output::OutputSink,
            host_output: &mut host_output,
            #[cfg(feature = "std")]
            io: core::ptr::null_mut(),
            call_func_id: 0,
            call_arg_start: 0,
            call_resume_pc: 0,
            call_ret_slots: 0,
            call_ret_reg: 0,
            call_kind: 0,
            #[cfg(feature = "std")]
            wait_io_token: 0,
            loop_exit_pc: 0,
            stack_ptr: core::ptr::null_mut(),
            stack_cap: 0,
            stack_limit: 0,
            call_depth: 0,
            call_depth_limit: 0,
            jit_bp: 0,
            fiber_sp: 0,
            push_frame_fn: None,
            pop_frame_fn: None,
            stack_overflow_fn: None,
            push_resume_point_fn: None,
            create_island_fn: None,
            queue_close_fn: None,
            queue_send_fn: None,
            queue_recv_fn: None,
            go_start_fn: None,
            go_island_fn: None,
            defer_push_fn: None,
            recover_fn: None,
            select_begin_fn: None,
            select_send_fn: None,
            select_recv_fn: None,
            select_exec_fn: None,
            is_error_return: 0,
            ret_gcref_start: 0,
            ret_is_heap: 0,
            ret_start: 0,
            prepare_closure_call_fn: None,
            prepare_iface_call_fn: None,
            ic_table: core::ptr::null_mut(),
        };

        let result = vo_call_extern(&mut ctx, 7, core::ptr::null(), 0, core::ptr::null_mut(), 0);

        assert_eq!(result, JitResult::JitError);
        assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(ctx.runtime_trap_arg1, JIT_INFRA_ERROR_MISSING_CALLBACK);
        assert_eq!(ctx.runtime_trap_pc, JIT_CALLBACK_CALL_EXTERN as u32);
    }

    #[test]
    fn jit_context_abi_field_manifest_is_sorted_and_unique() {
        let fields = jit_context_abi_fields();
        assert!(!fields.is_empty());
        for pair in fields.windows(2) {
            assert_ne!(pair[0].name, pair[1].name);
            assert!(
                pair[0].offset < pair[1].offset || pair[0].name < pair[1].name,
                "ABI fields should remain inspectable without duplicate adjacent entries"
            );
        }
    }
}
