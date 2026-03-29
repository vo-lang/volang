//! JIT callback functions for runtime operations.
//!
//! These extern "C" functions are called by JIT-compiled code to perform
//! operations that require VM access (channel ops, goroutine spawning, etc).

pub mod closure_call;
pub mod defer;
pub mod goroutine;
pub mod helpers;
pub mod island;
pub mod queue;
pub mod select;

pub use closure_call::{jit_prepare_closure_call, jit_prepare_iface_call};
pub use defer::{jit_defer_push, jit_recover};
pub use goroutine::{jit_go_island, jit_go_start};
pub use island::jit_create_island;
pub use queue::{jit_queue_close, jit_queue_recv, jit_queue_send};
pub use select::{jit_select_begin, jit_select_exec, jit_select_recv, jit_select_send};
