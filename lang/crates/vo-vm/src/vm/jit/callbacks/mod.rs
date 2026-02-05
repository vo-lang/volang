//! JIT callback functions for runtime operations.
//!
//! These extern "C" functions are called by JIT-compiled code to perform
//! operations that require VM access (channel/port ops, goroutine spawning, etc).

pub mod helpers;
pub mod island;
pub mod channel;
pub mod port;
pub mod goroutine;

pub use island::jit_create_island;
pub use channel::{jit_chan_close, jit_chan_send, jit_chan_recv};
pub use port::{jit_port_close, jit_port_send, jit_port_recv};
pub use goroutine::{jit_go_start, jit_go_island};
