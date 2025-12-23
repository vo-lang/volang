//! Shared Cranelift code generation for Vo AOT and JIT compilation.
//!
//! This crate provides the core translation logic from Vo bytecode
//! to Cranelift IR, shared by both AOT and JIT backends.
//!
//! # Architecture
//!
//! ```text
//! vo-vm::BytecodeModule
//!          │
//!          ▼
//!    ┌───────────────────┐
//!    │ FunctionTranslator│  ← This crate
//!    │ CompileContext    │
//!    │ RuntimeFunc       │
//!    └───────────────────┘
//!          │
//!          ▼
//!    Cranelift IR (Function)
//!          │
//!     ┌────┴────┐
//!     ▼         ▼
//!  vo-aot   vo-jit
//! (Object)  (JITModule)
//! ```

pub mod context;
pub mod runtime;
pub mod translate;

pub use context::CompileContext;
pub use runtime::{RuntimeFunc, RuntimeFuncs};
pub use translate::FunctionTranslator;
