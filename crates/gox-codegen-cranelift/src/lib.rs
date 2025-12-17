//! Shared Cranelift code generation for GoX AOT and JIT compilation.
//!
//! This crate provides the core translation logic from GoX bytecode
//! to Cranelift IR, shared by both AOT and JIT backends.
//!
//! # Architecture
//!
//! ```text
//! gox-vm::BytecodeModule
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
//!  gox-aot   gox-jit
//! (Object)  (JITModule)
//! ```

pub mod context;
pub mod runtime;
pub mod translate;

pub use context::CompileContext;
pub use runtime::{RuntimeFunc, RuntimeFuncs};
pub use translate::FunctionTranslator;
