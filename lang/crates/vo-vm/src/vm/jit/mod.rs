//! JIT dispatch: bridge between VM interpreter and JIT-compiled code.
//!
//! ## Overview
//!
//! When VM encounters a `Call` instruction for a JIT-compiled function:
//! 1. `dispatch_jit_call` allocates frame in fiber.stack and prepares JitContext
//! 2. JIT function executes natively, using fiber.stack directly
//! 3. Results (Ok/Panic/Call/WaitIo/WaitQueue) are translated back to VM state
//!
//! ## fiber.stack ABI
//!
//! Top-level JIT entries use `fiber.stack[jit_bp..]` as their ABI buffer. Nested
//! direct JIT calls may pass arguments through native stack slots on the OK path,
//! then spill/materialize into `fiber.stack` before any side-exit.
//!
//! ## Shadow Frame Design
//!
//! JIT-to-JIT calls keep `fiber.frames` shallow on the OK path. A side-exit lazily
//! records shadow frames in `resume_stack` just before returning to the VM:
//! - `jit_push_frame`: reserve a callee window in `fiber.stack` without a real frame
//! - `jit_pop_frame`: restore caller bp/sp after an OK prepared call
//! - `jit_push_resume_point`: record a side-exiting callee frame
//! - On Call/WaitIo/WaitQueue: `materialize_jit_frames` converts shadow frames to real CallFrames
//!
//! This avoids redundant frame management during pure JIT execution.

mod bridge_result;
pub mod callbacks;
mod context;
mod extern_call;
mod frame;
mod invoke;
mod materialize;
mod osr;
mod panic_setup;
mod side_exit;
mod transition;

#[cfg(test)]
pub(crate) use context::build_jit_context;
#[cfg(feature = "std")]
pub use extern_call::jit_call_extern;
pub use invoke::{dispatch_jit_call, dispatch_jit_frame};
pub(crate) use osr::{try_loop_osr, OsrResult};

#[cfg(all(test, feature = "jit"))]
mod test_support;
