//! Vo Compilation and Execution Core
//!
//! This crate provides the core compile and run functionality for Vo programs.
//! It is used by both the Vo CLI launcher and the vox library.

mod compile;
mod format;
mod run;
mod toolchain;

pub use compile::{compile, compile_prepared, compile_with_cache, compile_string, compile_source_at, compile_with_auto_install, compile_from_memory, prepare_with_auto_install, CompileError, CompileOutput};
pub use format::{format_text, parse_text};
pub use run::{run, run_with_output, run_with_output_interruptible, RunMode, RunError, RuntimeError, RuntimeErrorKind};
pub use toolchain::ensure_toolchain_host_installed;

pub use vo_vm::bytecode::Module;
