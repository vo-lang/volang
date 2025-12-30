#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

pub mod fiber;
pub mod scheduler;
pub mod itab;
pub mod vm;
pub mod exec;
pub mod hot_counter;
mod gc_roots;

#[cfg(feature = "jit")]
pub mod jit_bridge;

// Re-export from vo-common-core for backward compatibility
pub use vo_runtime::bytecode;
pub use vo_runtime::instruction;
pub use vo_runtime::serialize;
