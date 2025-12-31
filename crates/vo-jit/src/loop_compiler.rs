//! Loop function compiler for OSR.
//!
//! Compiles a loop body into an independent JIT function that can be called
//! from VM during OSR. This avoids the SSA complexity of mid-function entry.
//!
//! ## Return Value Convention
//!
//! The loop function returns `u32` which is interpreted as:
//! - **Normal exit / break**: Returns `exit_pc` (the PC after the loop)
//! - **Return inside loop**: Returns the PC of the `Return` instruction
//! - **Panic**: Returns `LOOP_RESULT_PANIC` (u32::MAX)
//!
//! The VM resumes execution at the returned PC. For `Return` instructions,
//! the VM handles the full return sequence.

/// Special return value indicating panic occurred in loop.
pub const LOOP_RESULT_PANIC: u32 = u32::MAX;

/// Loop function signature:
/// ```ignore
/// extern "C" fn(ctx: *mut JitContext, locals: *mut u64) -> u32 (exit_pc or LOOP_RESULT_PANIC)
/// ```
pub type LoopFunc = extern "C" fn(*mut crate::JitContext, *mut u64) -> u32;
