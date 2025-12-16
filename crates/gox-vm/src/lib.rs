//! Custom virtual machine runtime for GoX.
//!
//! This crate provides a register-based bytecode VM with:
//! - Incremental-ready garbage collector (Phase 1: stop-the-world)
//! - Fiber-based concurrency (goroutines)
//! - Full Go-like type system support
//! - Native function interface

pub mod gc;
pub mod types;
pub mod instruction;
pub mod fiber;
pub mod objects;
pub mod bytecode;
pub mod vm;
pub mod ffi;

pub use gc::{Gc, GcRef, GcHeader, GcColor, NULL_REF};
pub use types::{TypeId, TypeMeta, TypeTable, builtin};
pub use instruction::{Instruction, Opcode};
pub use fiber::{Fiber, FiberId, FiberStatus, CallFrame, Scheduler};
pub use bytecode::{Module, FunctionDef, Constant, BytecodeError};
pub use vm::{Vm, VmResult, NativeFn, NativeCtx, NativeRegistry};
pub use ffi::{TypeTag, GoxValue, GoxArgs, ArgOffset};

// Re-export ValueKind from common
pub use gox_common::ValueKind;
