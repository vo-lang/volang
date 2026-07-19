//! Vo Compilation and Execution Core
//!
//! This crate provides the core compile and run functionality for Vo programs.
//! It is used by both the Vo CLI launcher and the vox library.

mod compile;
mod format;
mod run;
mod scan;
mod toolchain;

pub use compile::{
    check, check_path, check_path_with_auto_install, check_path_with_auto_install_with_options,
    check_path_with_options, check_with_auto_install, check_with_auto_install_with_options,
    check_with_options, compile, compile_from_memory, compile_path, compile_path_with_auto_install,
    compile_source_at, compile_string, compile_with_auto_install,
    compile_with_auto_install_with_options, compile_with_cache, compile_with_cache_with_options,
    compile_with_options, default_mod_cache_root, prepare_native_extension_specs,
    with_compile_log_sink, CompileError, CompileLogRecord, CompileOutput, ModuleSystemError,
    ModuleSystemErrorKind, ModuleSystemStage, PreparedNativeExtension,
};
pub use format::format_text;
pub use run::{
    build_gui_vm, run, run_with_byte_args, run_with_output, run_with_output_interruptible,
    run_with_output_interruptible_observed, run_with_output_observed, RunError, RunMode,
    RunObservation, RuntimeError, RuntimeErrorKind,
};
pub use scan::scan_external_imports;
pub use toolchain::ensure_toolchain_host_installed;
pub use vo_runtime::output::CaptureSink;
pub use vo_vm::bytecode::Module;
