#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

pub mod instruction;
pub mod bytecode;
pub mod fiber;
pub mod scheduler;
pub mod itab;
pub mod vm;
pub mod exec;
pub mod serialize;
mod gc_roots;
