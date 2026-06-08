#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

pub mod exec;
pub mod fiber;
mod gc_roots;
pub mod scheduler;
pub mod vm;

// Re-export runtime bytecode modules
pub use vo_runtime::bytecode;
pub use vo_runtime::instruction;
pub use vo_runtime::serialize;

// Re-export JitConfig for external use
#[cfg(feature = "jit")]
pub use vm::jit_mgr::JitConfig;
