//! Native runtime support for GoX AOT and JIT compilation.
//!
//! This crate provides the runtime symbol table and utilities needed
//! by both AOT and JIT backends to link with the GoX runtime.
//!
//! # Usage
//!
//! For JIT compilation, use `RuntimeSymbols` to register function pointers:
//! ```ignore
//! let symbols = RuntimeSymbols::new();
//! for (name, ptr) in symbols.iter() {
//!     jit_builder.symbol(name, ptr);
//! }
//! ```
//!
//! For AOT compilation, the symbols are resolved at link time using
//! the same names exported by `gox-runtime-core`.

mod symbols;
pub mod goroutine;
pub mod debug;
pub mod gc_global;

pub use symbols::{RuntimeSymbols, RuntimeSymbol};
pub use gc_global::{init_gc, init_globals, with_gc};

// Re-export core runtime types that native code might need
pub use gox_runtime_core::ffi::*;

// Re-export goroutine C ABI functions
pub use goroutine::{
    gox_go_spawn,
    gox_yield,
    gox_chan_send,
    gox_chan_recv,
    gox_chan_close,
    gox_defer_push,
    gox_defer_pop,
    gox_panic,
    gox_recover,
    gox_select_start,
    gox_select_add_send,
    gox_select_add_recv,
    gox_select_exec,
    gox_iter_begin,
    gox_iter_next,
    gox_iter_end,
};

// Re-export debug/assert C ABI functions
pub use debug::{
    gox_debug_print,
    gox_assert_begin,
    gox_assert_arg,
    gox_assert_end,
};
