//! Custom virtual machine runtime for GoX.
//!
//! This crate provides a register-based bytecode VM with:
//! - Incremental-ready garbage collector (Phase 1: stop-the-world)
//! - Fiber-based concurrency (goroutines)
//! - Full Go-like type system support
//! - Zero-copy native function interface
//!
//! # Features
//! - `std` (default): Enable std features like bytecode loading from io::Read

#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

// Re-export alloc types for internal use

pub mod gc;
pub mod types;
pub mod instruction;
pub mod fiber;
pub mod objects;
pub mod bytecode;
pub mod vm;
pub mod extern_fn;
pub mod value;

#[cfg(feature = "multithread")]
pub mod scheduler;

pub use gc::{Gc, GcRef, GcHeader, GcColor, NULL_REF};
pub use types::{TypeId, TypeMeta, TypeTable, builtin};
pub use instruction::{Instruction, Opcode};
pub use fiber::{Fiber, FiberId, FiberStatus, CallFrame, Scheduler};
pub use bytecode::{Module, FunctionDef, Constant, BytecodeError};
pub use vm::{Vm, VmResult};

// Zero-copy extern API (extern = calls from GoX to outside)
pub use extern_fn::{ExternFn, ExternCtx, ExternResult, ExternRegistry, TypeTag};

// Typed value for extern functions
pub use value::GoxValue;

// Re-export ValueKind from common
pub use gox_common_core::ValueKind;
