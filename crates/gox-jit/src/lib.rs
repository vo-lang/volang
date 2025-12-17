//! GoX JIT Compiler
//!
//! This crate provides JIT (Just-In-Time) compilation for GoX bytecode
//! using Cranelift as the code generation backend.
//!
//! ## Architecture
//!
//! ```text
//! gox-codegen-cranelift (shared)
//!        │
//!        ▼
//!    gox-jit ──► JITModule ──► Function Pointers
//! ```
//!
//! ## Usage
//!
//! ```ignore
//! let mut jit = JitCompiler::new()?;
//! jit.compile_module(&bytecode)?;
//! let add_fn: fn(i64, i64) -> i64 = jit.get_function("add")?;
//! let result = add_fn(3, 5);
//! ```

mod jit;

pub use jit::{JitCompiler, CompiledFunction};

// Re-export shared codegen types
pub use gox_codegen_cranelift::{
    CompileContext,
    FunctionTranslator,
    RuntimeFunc,
    RuntimeFuncs,
};
