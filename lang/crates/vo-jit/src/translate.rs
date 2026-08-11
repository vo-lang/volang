#![allow(clippy::result_large_err)]
//! Shared instruction translation logic.

mod collections;
mod conversions;
mod dispatch;
mod forloop;
mod memory;
mod runtime_ops;
mod scalar;
mod traps;

pub use dispatch::translate_inst;
pub use forloop::emit_forloop_step;
pub(in crate::translate) use traps::{
    emit_jit_error_if_zero, emit_nil_ptr_check_for_slot, emit_runtime_trap_if, mark_runtime_trap_pc,
};
