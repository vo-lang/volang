//! Native runtime support for Vo JIT compilation.
//!
//! This crate provides the runtime symbol table and utilities needed
//! by JIT backend to link with the Vo runtime.
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
//! For AOT compilation (subset of JIT), the symbols are resolved at link time using
//! the same names exported by `vo-runtime-core`.

mod symbols;
pub mod goroutine;
pub mod debug;
pub mod gc_global;
pub mod stack_map;
pub mod extern_dispatch;
pub mod extern_fns;

pub use symbols::{RuntimeSymbols, RuntimeSymbol};
pub use gc_global::{init_gc, init_globals, init_func_table, set_func_ptr, with_gc};
// Note: Type table is now in vo_runtime_core::gc_types

// Re-export core runtime types that native code might need
pub use vo_runtime_core::ffi::*;

// Re-export goroutine C ABI functions
pub use goroutine::{
    vo_go_spawn,
    vo_yield,
    vo_chan_send,
    vo_chan_recv,
    vo_chan_close,
    vo_defer_push,
    vo_defer_pop,
    vo_panic,
    vo_recover,
    vo_select_start,
    vo_select_add_send,
    vo_select_add_recv,
    vo_select_exec,
    vo_iter_begin,
    vo_iter_next,
    vo_iter_end,
};

// Re-export debug/assert C ABI functions
pub use debug::{
    vo_debug_print,
    vo_assert_begin,
    vo_assert_arg,
    vo_assert_end,
};
