//! GoX AOT Compiler
//!
//! This crate provides GoX AOT (Ahead-Of-Time) Compiler using Cranelift.
//!
//! This crate compiles GoX bytecode to native object files using Cranelift.
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
//! gox-codegen-cranelift (shared)
//!        │
//!        ▼
//!    gox-aot ──► ObjectModule ──► .o file
//! ```

mod compiler;

// Re-export shared codegen types
pub use gox_codegen_cranelift::{
    CompileContext,
    FunctionTranslator,
    RuntimeFunc,
    RuntimeFuncs,
};

// Re-export shared modules for direct access
pub use gox_codegen_cranelift::context;
pub use gox_codegen_cranelift::runtime;
pub use gox_codegen_cranelift::translate;

pub use compiler::{AotCompiler, ObjectOutput};
