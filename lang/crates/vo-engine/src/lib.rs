//! Vo Compilation and Execution Core
//!
//! This crate provides the core compile and run functionality for Vo programs.
//! It is used by both the Vo CLI launcher and the vox library.

mod compile;
mod format;
mod run;
mod toolchain;

pub use compile::{compile, compile_with_auto_install, compile_with_cache, compile_from_memory, compile_source_at, compile_string, CompileError, CompileOutput, default_mod_cache_root, ensure_extension_manifests_built};
pub use format::{format_text, parse_text};
pub use run::{run, run_with_output, run_with_output_interruptible, build_gui_vm, RunMode, RunError, RuntimeError, RuntimeErrorKind};
pub use toolchain::{ensure_toolchain_host_installed, install_module};

pub use vo_vm::bytecode::Module;
pub use vo_runtime::output::CaptureSink;
