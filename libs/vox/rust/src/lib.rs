//! Vox - compile, run, and inspect Vo programs.
//!
//! This crate provides:
//! - Re-exports of vo-engine (compile, run, etc.)
//! - AST parsing and printing
//! - Bytecode formatting
//! - FFI bindings for the vox package in Vo

mod ffi;
mod printer;
mod format;
pub mod gui;

/// Force link this crate's FFI functions.
/// Call this from your main() to ensure linkme symbols are included.
pub fn ensure_linked() {
    // Reference vo_ext_get_entries to force linker to include FFI symbols
    extern "C" {
        fn vo_ext_get_entries() -> vo_ext::ExtensionTable;
    }
    // Use black_box to prevent optimization
    let _ = std::hint::black_box(unsafe { vo_ext_get_entries() });
}

// Re-export vo-engine
pub use vo_engine::{compile, compile_with_cache, compile_string, CompileError, CompileOutput};
pub use vo_engine::{run, RunMode, RunError, RuntimeError, RuntimeErrorKind};
pub use vo_engine::Module;

pub use printer::AstPrinter;
pub use format::{format_text, parse_text};
