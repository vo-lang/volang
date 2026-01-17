//! `vo run` command - Run a Vo program.

use std::path::{Path, PathBuf};
use vo_common::diagnostics::render_error;
use vo_common::vfs::{FileSet, RealFs, ZipFs};
use vo_analysis::analyze_project;
use vo_module::{PackageResolverMixed, StdSource, LocalSource, ModSource};
use vo_codegen::compile_project;
use vo_vm::bytecode::Module;
use vo_vm::vm::{Vm, VmError};
use vo_syntax::parser;
use crate::printer::AstPrinter;
use crate::bytecode_text;
use crate::stdlib::{EmbeddedStdlib, create_resolver};
use crate::output::{TAG_OK, format_tag, ErrorKind, SourceLoc, report_analysis_error};

/// Execution mode for running programs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RunMode {
    #[default]
    Vm,
    Jit,
}

/// Standard library mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum StdMode {
    /// Core mode: no OS dependencies (for WASM, embedded)
    Core,
    /// Full mode: complete standard library
    #[default]
    Full,
}

impl std::fmt::Display for RunMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunMode::Vm => write!(f, "vm"),
            RunMode::Jit => write!(f, "jit"),
        }
    }
}

impl std::str::FromStr for RunMode {
    type Err = String;
    
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "vm" => Ok(RunMode::Vm),
            "jit" => Ok(RunMode::Jit),
            _ => Err(format!("unknown mode '{}', expected 'vm' or 'jit'", s)),
        }
    }
}

/// Run a Vo program.
///
/// Returns true if successful, false if failed (error already reported).
///
/// # Output Tags (for script parsing)
/// * `[VO:OK]` - Execution completed successfully
/// * `[VO:PARSE:...]` - Parse error
/// * `[VO:CHECK:...]` - Type check error
/// * `[VO:CODEGEN:...]` - Code generation error
/// * `[VO:PANIC:...]` - Runtime panic
/// * `[VO:IO:...]` - IO error
pub fn run(
    file: &str,
    mode: RunMode,
    std_mode: StdMode,
    print_ast: bool,
    print_codegen: bool,
) -> bool {
    // Initialize GC debug if enabled via environment variable
    #[cfg(feature = "gc-debug")]
    if std::env::var("VO_GC_DEBUG").is_ok() {
        vo_runtime::gc_debug::enable();
    }
    
    let path = Path::new(file);
    
    // Handle --ast flag: parse and print AST only
    if print_ast {
        return print_ast_only(path);
    }
    
    // Compile the program
    let (module, source_root) = match compile_source(file, std_mode) {
        Some(result) => result,
        None => return false, // Error already reported
    };
    
    // Handle --codegen flag: print bytecode and exit
    if print_codegen {
        let text = bytecode_text::format_text(&module);
        print!("{}", text);
        return true;
    }
    
    // Run the module
    match mode {
        RunMode::Vm => run_vm(module, &source_root),
        RunMode::Jit => run_jit(module, &source_root),
    }
}

/// Parse and print AST only.
fn print_ast_only(path: &Path) -> bool {
    let content = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            println!("{}", format_tag(ErrorKind::Io, None, &e.to_string()));
            return false;
        }
    };
    let (file, diag, interner) = parser::parse(&content, 0);
    
    if diag.has_errors() {
        for d in diag.iter() {
            eprintln!("{}", d.message);
        }
        println!("{}", format_tag(ErrorKind::Parse, None, "parse error"));
        return false;
    }
    
    let mut printer = AstPrinter::new(&interner);
    let ast = printer.print_file(&file);
    println!("{}", ast);
    true
}

/// Compile source file/directory to a Module.
/// Returns (Module, source_root) or None if error (already reported).
fn compile_source(file: &str, std_mode: StdMode) -> Option<(Module, PathBuf)> {
    let path = Path::new(file);
    
    if file.ends_with(".vo") {
        compile_source_file(path)
    } else if let Some((zip_path, internal_root)) = parse_zip_path(file) {
        compile_zip(Path::new(&zip_path), internal_root.as_deref())
    } else if file.ends_with(".voc") || file.ends_with(".vob") {
        let source_root = path.parent().unwrap_or(Path::new(".")).to_path_buf();
        match std::fs::read(file) {
            Ok(bytes) => match Module::deserialize(&bytes) {
                Ok(m) => Some((m, source_root)),
                Err(e) => {
                    println!("{}", format_tag(ErrorKind::Io, None, &format!("{:?}", e)));
                    None
                }
            },
            Err(e) => {
                println!("{}", format_tag(ErrorKind::Io, None, &e.to_string()));
                None
            }
        }
    } else if file.ends_with(".vot") {
        let source_root = path.parent().unwrap_or(Path::new(".")).to_path_buf();
        match std::fs::read_to_string(file) {
            Ok(content) => match bytecode_text::parse_text(&content) {
                Ok(m) => Some((m, source_root)),
                Err(e) => {
                    println!("{}", format_tag(ErrorKind::Io, None, &e.to_string()));
                    None
                }
            },
            Err(e) => {
                println!("{}", format_tag(ErrorKind::Io, None, &e.to_string()));
                None
            }
        }
    } else if path.is_dir() {
        // Directory - use build command (TODO: unify with build command)
        if crate::commands::build::run(file, std_mode) {
            None // Build succeeded, but no module to return
        } else {
            None // Build failed, error already reported
        }
    } else {
        compile_source_file(path)
    }
}

/// Parse zip path format: "file.zip" or "file.zip:internal/path"
/// Returns (zip_path, optional_internal_root)
fn parse_zip_path(file: &str) -> Option<(String, Option<String>)> {
    if file.ends_with(".zip") {
        Some((file.to_string(), None))
    } else if file.contains(".zip:") {
        let parts: Vec<&str> = file.splitn(2, ".zip:").collect();
        if parts.len() == 2 {
            Some((format!("{}.zip", parts[0]), Some(parts[1].to_string())))
        } else {
            None
        }
    } else {
        None
    }
}

/// Compile a single source file to a Module.
/// Returns (Module, source_root) or None if error (already reported).
fn compile_source_file(path: &Path) -> Option<(Module, PathBuf)> {
    // Determine root directory and file path
    let (abs_root, rel_path) = if path.is_file() {
        let abs_path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        let root = abs_path.parent().unwrap_or(Path::new(".")).to_path_buf();
        let file_name = abs_path.file_name().unwrap_or_default();
        (root, PathBuf::from(file_name))
    } else {
        let root = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        (root, PathBuf::from("."))
    };
    
    let fs = RealFs::new(&abs_root);
    
    // Use from_file for single file, collect for directory
    let file_set = match if path.is_file() {
        FileSet::from_file(&fs, &rel_path, abs_root.clone())
    } else {
        FileSet::collect(&fs, Path::new("."), abs_root.clone())
    } {
        Ok(fs) => fs,
        Err(e) => {
            println!("{}", format_tag(ErrorKind::Io, None, &e.to_string()));
            return None;
        }
    };
    
    if file_set.files.is_empty() {
        println!("{}", format_tag(ErrorKind::Io, None, "no .vo files found"));
        return None;
    }
    
    let source_root = file_set.root.clone();
    let resolver = create_resolver(&abs_root);
    
    let project = match analyze_project(file_set, &resolver) {
        Ok(p) => p,
        Err(e) => {
            report_analysis_error(&e);
            return None;
        }
    };
    
    match compile_project(&project) {
        Ok(m) => Some((m, source_root)),
        Err(e) => {
            println!("{}", format_tag(ErrorKind::Codegen, None, &format!("{:?}", e)));
            None
        }
    }
}


/// Compile a zip file to a Module.
/// 
/// Supports two formats:
/// - `project.zip` - project at zip root
/// - `project.zip:src/` - project in subdirectory
/// 
/// Returns (Module, zip path for source display) or None if error.
fn compile_zip(zip_path: &Path, internal_root: Option<&str>) -> Option<(Module, PathBuf)> {
    let zip_fs = match internal_root {
        Some(root) => ZipFs::from_path_with_root(zip_path, root),
        None => ZipFs::from_path(zip_path),
    };
    let zip_fs = match zip_fs {
        Ok(fs) => fs,
        Err(e) => {
            println!("{}", format_tag(ErrorKind::Io, None, &e.to_string()));
            return None;
        }
    };
    
    // Use the zip file path as the "root" for error display
    let abs_root = zip_path.canonicalize().unwrap_or_else(|_| zip_path.to_path_buf());
    
    let file_set = match FileSet::collect(&zip_fs, Path::new("."), abs_root.clone()) {
        Ok(fs) => fs,
        Err(e) => {
            println!("{}", format_tag(ErrorKind::Io, None, &e.to_string()));
            return None;
        }
    };
    
    if file_set.files.is_empty() {
        println!("{}", format_tag(ErrorKind::Io, None, "no .vo files found in zip"));
        return None;
    }
    
    // Create resolver with embedded stdlib and zip filesystem for local
    let mod_root = dirs::home_dir()
        .map(|h| h.join(".vo/mod"))
        .unwrap_or_else(|| abs_root.join(".vo/mod"));
    
    let resolver = PackageResolverMixed {
        std: StdSource::with_fs(EmbeddedStdlib::new()),
        local: LocalSource::with_fs(zip_fs),
        r#mod: ModSource::with_fs(RealFs::new(mod_root)),
    };
    
    let project = match analyze_project(file_set, &resolver) {
        Ok(p) => p,
        Err(e) => {
            report_analysis_error(&e);
            return None;
        }
    };
    
    match compile_project(&project) {
        Ok(m) => Some((m, abs_root)),
        Err(e) => {
            println!("{}", format_tag(ErrorKind::Codegen, None, &format!("{:?}", e)));
            None
        }
    }
}

/// Convert VM error location to SourceLoc using debug info.
fn vm_error_loc(loc: &Option<vo_vm::vm::ErrorLocation>, module: &Module) -> Option<SourceLoc> {
    loc.as_ref().and_then(|l| {
        module.debug_info.lookup(l.func_id, l.pc)
    })
}

/// Extract (message, location) from VmError.
fn vm_error_info(e: &VmError, module: &Module) -> (&'static str, Option<SourceLoc>) {
    match e {
        VmError::PanicUnwound { msg: _, loc } => ("", vm_error_loc(loc, module)),
        VmError::IndexOutOfBounds(loc) => ("index out of bounds", vm_error_loc(loc, module)),
        VmError::NilPointerDereference(loc) => ("nil pointer dereference", vm_error_loc(loc, module)),
        VmError::TypeAssertionFailed(loc) => ("type assertion failed", vm_error_loc(loc, module)),
        VmError::DivisionByZero(loc) => ("division by zero", vm_error_loc(loc, module)),
        VmError::SendOnClosedChannel(loc) => ("send on closed channel", vm_error_loc(loc, module)),
        _ => ("", None),
    }
}

/// Get the panic message from VmError (for PanicUnwound only).
fn vm_panic_msg(e: &VmError) -> &str {
    match e {
        VmError::PanicUnwound { msg, .. } => msg.as_deref().unwrap_or("panic"),
        _ => "",
    }
}

/// Report VM error and return false.
fn report_vm_error(vm: &Vm, e: &VmError, source_root: &Path) -> bool {
    let (base_msg, loc) = vm.module()
        .map(|m| vm_error_info(e, m))
        .unwrap_or(("", None));
    let msg = if base_msg.is_empty() { vm_panic_msg(e) } else { base_msg };
    let fs = RealFs::new(source_root);
    render_error(loc.as_ref(), "panic", msg, &fs);
    println!("{}", format_tag(ErrorKind::Panic, loc.as_ref(), msg));
    false
}

/// Run a module using the VM interpreter.
fn run_vm(module: Module, source_root: &Path) -> bool {
    // Discover and load native extensions from source root
    let ext_loader = load_extensions(source_root);
    
    let mut vm = Vm::new();
    vm.load_with_extensions(module, ext_loader.as_ref());
    
    match vm.run() {
        Ok(()) => {
            println!("{}", TAG_OK);
            true
        }
        Err(e) => report_vm_error(&vm, &e, source_root),
    }
}

/// Discover and load native extensions from imported library directories.
/// 
/// For each library with vo.ext.toml, loads the native library specified.
/// Native library path is either:
/// 1. Specified in vo.ext.toml `path` field (relative to library root)
/// 2. Auto-detected in library root (lib*.so / lib*.dylib / *.dll)
fn load_extensions(source_root: &Path) -> Option<vo_runtime::ext_loader::ExtensionLoader> {
    use vo_runtime::ext_loader::{ExtensionLoader, discover_extensions};
    
    let mut loader = ExtensionLoader::new();
    let mut found_any = false;
    
    // Find libs/ directory relative to source_root
    let libs_path = source_root.join("../libs");
    let libs_dir = libs_path.canonicalize().ok()?;
    if !libs_dir.is_dir() {
        return None;
    }
    
    // Load extensions from each library in libs/
    if let Ok(entries) = std::fs::read_dir(&libs_dir) {
        for entry in entries.flatten() {
            let lib_path = entry.path();
            if lib_path.is_dir() {
                if let Ok(manifests) = discover_extensions(&lib_path) {
                    for manifest in manifests {
                        if let Err(e) = loader.load(&manifest.native_path, &manifest.name) {
                            eprintln!("Warning: failed to load extension '{}': {}", manifest.name, e);
                        } else {
                            found_any = true;
                        }
                    }
                }
            }
        }
    }
    
    if found_any { Some(loader) } else { None }
}

/// Run a module using the JIT compiler.
///
/// Environment variables:
/// - `VO_JIT_CALL_THRESHOLD`: Call count threshold (default: 100)
/// - `VO_JIT_LOOP_THRESHOLD`: Loop back-edge threshold (default: 1000)
/// - `VO_JIT_DEBUG`: Print Cranelift IR for compiled functions
fn run_jit(module: Module, source_root: &Path) -> bool {
    use vo_vm::JitConfig;
    
    // Discover and load native extensions from source root
    let ext_loader = load_extensions(source_root);
    
    let call_threshold = std::env::var("VO_JIT_CALL_THRESHOLD")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(100);
    let loop_threshold = std::env::var("VO_JIT_LOOP_THRESHOLD")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(1000);
    let debug_ir = std::env::var("VO_JIT_DEBUG").is_ok();
    
    let config = JitConfig {
        call_threshold,
        loop_threshold,
        debug_ir,
    };
    let mut vm = Vm::with_jit_config(config);
    vm.init_jit();
    vm.load_with_extensions(module, ext_loader.as_ref());
    
    match vm.run() {
        Ok(()) => {
            println!("{}", TAG_OK);
            true
        }
        Err(e) => report_vm_error(&vm, &e, source_root),
    }
}
