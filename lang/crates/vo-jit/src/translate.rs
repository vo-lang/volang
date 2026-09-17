#![allow(clippy::result_large_err)]
//! Shared instruction translation logic.

mod collections;
mod conversions;
mod dispatch;
mod float32;
mod forloop;
mod memory;
mod runtime_ops;
mod scalar;
mod traps;

pub(crate) use collections::{emit_slice_bounds_check_at, emit_slice_storage_address};
pub use dispatch::translate_inst;
pub(crate) use float32::emit_float32_bits;
pub use forloop::emit_forloop_step;
pub(crate) use memory::{fresh_ptr_get, fresh_ptr_set};
pub(crate) use runtime_ops::materialize_scalar_replaced_ptr_new;
pub(in crate::translate) use traps::{
    emit_jit_error_if_zero, emit_nil_ptr_check_for_slot, emit_return_if_u64_jit_error,
    emit_runtime_trap_if, mark_runtime_trap_pc,
};
