//! JIT callback functions for runtime operations.
//!
//! These extern "C" functions are called by JIT-compiled code to perform
//! operations that require VM access (channel ops, goroutine spawning, etc).

pub mod helpers;
pub mod island;
pub mod queue;
pub mod goroutine;
pub mod defer;
pub mod select;
pub mod closure_call;

pub use island::jit_create_island;
pub use queue::{jit_queue_close, jit_queue_send, jit_queue_recv};
pub use goroutine::{jit_go_start, jit_go_island};
pub use defer::{jit_defer_push, jit_recover};
pub use select::{jit_select_begin, jit_select_send, jit_select_recv, jit_select_exec};
pub use closure_call::{jit_prepare_closure_call, jit_prepare_iface_call};
