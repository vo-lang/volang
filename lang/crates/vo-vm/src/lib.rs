//! Vo bytecode virtual machine.
//!
//! Disabling default features produces an alloc-only VM, including its
//! standard-library provider dependency. Native I/O, threads, dynamic loading,
//! and JIT support remain explicit features.

#![cfg_attr(not(feature = "std"), no_std)]
#![allow(
    clippy::large_enum_variant,
    clippy::result_large_err,
    clippy::too_many_arguments,
    clippy::type_complexity,
    clippy::vec_box
)]
#![cfg_attr(
    test,
    allow(
        clippy::drop_non_drop,
        clippy::items_after_test_module,
        clippy::manual_contains,
        clippy::manual_dangling_ptr,
        clippy::manual_pattern_char_comparison,
        clippy::needless_range_loop,
        clippy::unneeded_struct_pattern,
        clippy::useless_conversion,
        clippy::useless_vec
    )
)]

#[cfg(not(feature = "std"))]
extern crate alloc;

pub(crate) mod exec;
pub mod fiber;
mod frame_call;
mod gc_roots;
pub mod runtime_boundary;
mod runtime_externs;
pub mod scheduler;
#[cfg(test)]
pub(crate) mod test_support;
pub mod vm;

// Re-export runtime bytecode modules
pub use vo_runtime::bytecode;
pub use vo_runtime::gc::{
    GcCycleKind, GcMode, MemoryError, MemoryStats, OomPolicy, VmMemoryConfig,
};
pub use vo_runtime::instruction;
pub use vo_runtime::serialize;

// Re-export semantic JIT observation types for all builds.
pub use fiber::VmResourceLimits;
#[cfg(feature = "jit")]
pub use vm::JitConfig;
pub use vm::VmResourceError;
pub use vm::{JitExecutionStats, JitSideExitReason, JitSideExitReasonStats};
#[cfg(feature = "jit")]
pub use vo_jit::{JitCodeMemoryStats, JitFailureKind};
