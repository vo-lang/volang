//! Vo Compilation and Execution Core
//!
//! This crate provides the core compile and run functionality for Vo programs.
//! It is used by both the Vo CLI launcher and the vox library.

mod compile;
mod format;
mod run;
mod toolchain;

pub use compile::{
    check, compile, compile_from_memory, compile_source_at, compile_string,
    compile_with_auto_install, compile_with_cache, default_mod_cache_root,
    ensure_extension_manifests_built, with_compile_log_sink, CompileError, CompileLogRecord,
    CompileOutput,
};
pub use format::{format_text, parse_text};
pub use run::{
    build_gui_vm, run, run_with_output, run_with_output_interruptible, RunError, RunMode,
    RuntimeError, RuntimeErrorKind,
};
pub use toolchain::{ensure_toolchain_host_installed, install_module};

/// Format Vo source code, returning the formatted string.
/// Returns an error if the source has parse errors.
pub fn format_source(source: &str) -> Result<String, String> {
    let (file, diags, interner) = vo_syntax::parser::parse(source, 0);
    if diags.has_errors() {
        return Err(diags
            .iter()
            .map(|diag| diag.message.as_str())
            .collect::<Vec<_>>()
            .join("; "));
    }
    Ok(vo_syntax::display::format_file(&file, &interner))
}

pub use vo_runtime::output::CaptureSink;
pub use vo_vm::bytecode::Module;
