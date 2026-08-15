//! Instruction execution modules.
#![allow(dead_code)]

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(feature = "std")]
use std::string::String;
use vo_runtime::gc::MemoryError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstructionError {
    Malformed(String),
    Memory(MemoryError),
}

impl core::fmt::Display for InstructionError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Malformed(message) => f.write_str(message),
            Self::Memory(error) => error.fmt(f),
        }
    }
}

impl From<String> for InstructionError {
    fn from(message: String) -> Self {
        Self::Malformed(message)
    }
}

impl From<MemoryError> for InstructionError {
    fn from(error: MemoryError) -> Self {
        Self::Memory(error)
    }
}

mod array;
mod call;
mod closure;
mod copy;
mod defer;
mod global;
mod goroutine;
mod iface;
mod island;
mod load;
mod map;
mod ptr;
pub mod queue;
mod select;
mod slice;
mod string;
mod transport;
mod unwind;

pub use array::*;
pub use call::*;
pub use closure::*;
pub use copy::*;
pub use defer::*;
pub use global::*;
pub use goroutine::*;
pub use iface::*;
pub use island::*;
pub use load::*;
pub use map::*;
pub use ptr::*;
pub use queue::*;
pub use select::*;
pub use slice::*;
pub use string::*;
pub use transport::*;
pub use unwind::*;
