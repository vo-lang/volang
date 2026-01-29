#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

pub mod fiber;
pub mod scheduler;
pub mod vm;
pub mod exec;
mod gc_roots;


// Re-export from vo-common-core for backward compatibility
pub use vo_runtime::bytecode;
pub use vo_runtime::instruction;
pub use vo_runtime::serialize;

// Re-export JitConfig for external use
#[cfg(feature = "jit")]
pub use vm::jit_mgr::JitConfig;
