//! Structured call types for the extern function interface.
//!
//! These types work with `ExternCallContext` to provide a compact calling convention
//! for extern function dispatch.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::Gc;
use crate::itab::ItabCache;
use vo_common_core::bytecode::Module;
#[cfg(feature = "std")]
use crate::io::{IoRuntime, IoToken};

use super::SentinelErrorCache;

// =============================================================================
// ExternInvoke: compact call descriptor
// =============================================================================

/// Compact call descriptor that describes how to interpret a `stack: &mut [u64]`
/// as an extern call frame.
///
/// **Key property**: args and returns may overlap (required by the JIT path).
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct ExternInvoke {
    /// Extern function ID in the registry.
    pub extern_id: u32,
    /// Base pointer (frame start in the stack slice).
    pub bp: u32,
    /// Argument start slot (relative to bp).
    pub arg_start: u16,
    /// Argument slot count (u64 slots, not parameter count).
    pub arg_slots: u16,
    /// Return value start slot (relative to bp).
    pub ret_start: u16,
    /// Return value slot count (u64 slots, not return count).
    pub ret_slots: u16,
}

// =============================================================================
// ExternWorld: borrowed runtime state (no fiber-owned fields)
// =============================================================================

/// Groups all borrowed "world state" needed by extern functions.
///
/// These are naturally owned by the VM runtime state. Fiber-owned inputs
/// (replay state, resume tokens) belong in `ExternFiberInputs`, not here.
pub struct ExternWorld<'env> {
    pub gc: &'env mut Gc,
    pub module: &'env Module,
    pub itab_cache: &'env mut ItabCache,

    /// Opaque handle to the VM instance. Extensions must not dereference this.
    pub vm_opaque: *mut core::ffi::c_void,

    pub program_args: &'env [String],
    pub sentinel_errors: &'env mut SentinelErrorCache,

    #[cfg(feature = "std")]
    pub io: &'env mut IoRuntime,
}

// =============================================================================
// ExternFiberInputs: one-shot fiber-derived inputs
// =============================================================================

/// One-shot inputs derived from the active fiber immediately before calling
/// an extern function. These are moved out of the fiber for this single
/// extern execution.
pub struct ExternFiberInputs {
    /// Opaque pointer to the current fiber.
    pub fiber_opaque: *mut core::ffi::c_void,

    /// I/O completion token that woke this fiber. Present only on the
    /// PC re-execution path (second execution of the same `CallExtern`
    /// after the runtime resumes the fiber).
    #[cfg(feature = "std")]
    pub resume_io_token: Option<IoToken>,

    /// Callback token that woke this fiber. Present only on the PC re-execution
    /// path (second execution of the same `CallExtern`) after `CallbackWaitAndResume`.
    pub resume_callback_token: Option<u64>,

    /// Cached closure results from previous `CallClosure` round-trips.
    /// Consumed in order via `ExternCallContext.replay_index`.
    pub replay_results: Vec<Vec<u64>>,

    /// Whether a closure-for-replay panicked.
    pub replay_panicked: bool,
}
