//! Vo AOT Compiler
//!
//! This crate provides Vo AOT (Ahead-Of-Time) Compiler using Cranelift.
//!
//! This crate compiles Vo bytecode to native object files using Cranelift.
//!
//! # Example
//!
//! ```ignore
//! let mut compiler = AotCompiler::new()?;
//! compiler.compile_module(&bytecode)?;
//! let output = compiler.finish()?;
//! std::fs::write("output.o", &output.bytes)?;
//! ```
//!
//! ## Architecture
//!
//! ```text
//! vo-codegen-cranelift (shared)
//!        │
//!        ▼
//!    vo-aot ──► ObjectModule ──► .o file
//! ```

mod compiler;

// Re-export shared codegen types
pub use vo_codegen_cranelift::{
    CompileContext,
    FunctionTranslator,
    RuntimeFunc,
    RuntimeFuncs,
};

// Re-export shared modules for direct access
pub use vo_codegen_cranelift::context;
pub use vo_codegen_cranelift::runtime;
pub use vo_codegen_cranelift::translate;

pub use compiler::{AotCompiler, ObjectOutput};
