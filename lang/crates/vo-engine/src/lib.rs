//! Vo Compilation and Execution Core
//!
//! This crate provides the core compile and run functionality for Vo programs.
//! It is used by both the Vo CLI launcher and the vox library.

mod compile;
mod format;
mod run;

pub use compile::{compile, compile_with_cache, compile_string, compile_source_at, compile_with_auto_install, compile_from_memory, CompileError, CompileOutput};
pub use format::{format_text, parse_text};
pub use run::{run, RunMode, RunError, RuntimeError, RuntimeErrorKind};

pub use vo_vm::bytecode::Module;
