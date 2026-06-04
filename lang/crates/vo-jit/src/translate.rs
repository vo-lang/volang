#![allow(clippy::result_large_err)]
//! Shared instruction translation logic.

use cranelift_codegen::ir::FuncRef;

use crate::JitError;

mod collections;
mod conversions;
mod dispatch;
mod forloop;
mod memory;
mod runtime_ops;
mod scalar;
mod traps;

#[cfg(test)]
pub(crate) use dispatch::translate_dispatch_lowering_owner;
pub use dispatch::translate_inst;
pub use forloop::emit_forloop_step;
pub(in crate::translate) use traps::{
    emit_nil_ptr_check_for_slot, emit_runtime_trap_if, mark_runtime_trap_pc,
};

pub(crate) fn require_helper(
    helper: Option<FuncRef>,
    name: &'static str,
) -> Result<FuncRef, JitError> {
    helper.ok_or_else(|| JitError::Internal(format!("JIT helper {name} is not registered")))
}
