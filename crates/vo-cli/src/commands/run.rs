//! `vo run` command - Run a Vo program.

use std::path::Path;
use vo_common::diagnostics::DiagnosticEmitter;
use vo_common::vfs::{FileSet, RealFs};
use vo_analysis::{analyze_project, AnalysisError};
use vo_module::VfsConfig;
use vo_codegen::compile_project;
use vo_vm::bytecode::Module;
use vo_vm::vm::Vm;
use vo_syntax::parser;
use crate::printer::AstPrinter;
use crate::bytecode_text;

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
/// # Arguments
/// * `file` - Path to .vo source, .vot bytecode, or project directory
/// * `mode` - Execution mode (vm or jit)
/// * `std_mode` - Stdlib mode (core or full)
/// * `print_ast` - If true, print AST and exit
/// * `print_codegen` - If true, print bytecode and exit
///
/// # Output Tags (for script parsing)
/// * `[VO:OK]` - Execution completed successfully
/// * `[VO:PANIC:message]` - Program panicked
/// * `[VO:ERROR:message]` - Compilation/analysis error
///
/// # Examples
/// ```text
/// vo run hello.vo
/// vo run hello.vo --mode=jit
/// vo run hello.vo --ast
/// vo run hello.vo --codegen
/// vo run myproject/
/// ```
pub fn run(
    file: &str,
    mode: RunMode,
    std_mode: StdMode,
    print_ast: bool,
    print_codegen: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let path = Path::new(file);
    
    // Handle --ast flag: parse and print AST only
    if print_ast {
        return print_ast_only(path);
    }
    
    // Compile the program
    let module = match compile_source(file, std_mode)? {
        Some(m) => m,
        None => return Ok(()), // Already handled (e.g., directory -> build)
    };
    
    // Handle --codegen flag: print bytecode and exit
    if print_codegen {
        let text = bytecode_text::format_text(&module);
        print!("{}", text);
        return Ok(());
    }
    
    // Run the module
    match mode {
        RunMode::Vm => run_vm(module, std_mode),
        RunMode::Jit => run_jit(module, std_mode),
    }
}

/// Parse and print AST only.
fn print_ast_only(path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let content = std::fs::read_to_string(path)?;
    let (file, diag, interner) = parser::parse(&content, 0);
    
    if diag.has_errors() {
        for d in diag.iter() {
            eprintln!("{}", d.message);
        }
        println!("[VO:ERROR:parse error]");
        return Err("parse error".into());
    }
    
    let mut printer = AstPrinter::new(&interner);
    let ast = printer.print_file(&file);
    println!("{}", ast);
    Ok(())
}

/// Compile source file/directory to a Module.
fn compile_source(file: &str, std_mode: StdMode) -> Result<Option<Module>, Box<dyn std::error::Error>> {
    let path = Path::new(file);
    
    if file.ends_with(".vo") {
        // Source file - compile and return
        Ok(Some(compile_source_file(path)?))
    } else if file.ends_with(".voc") || file.ends_with(".vob") {
        // Binary format
        let bytes = std::fs::read(file)?;
        Ok(Some(Module::deserialize(&bytes).map_err(|e| format!("{:?}", e))?))
    } else if file.ends_with(".vot") {
        // Text format (.vot)
        let content = std::fs::read_to_string(file)?;
        Ok(Some(bytecode_text::parse_text(&content)?))
    } else if path.is_dir() {
        // Directory - use build command
        crate::commands::build::run(file, std_mode)?;
        Ok(None)
    } else {
        // Assume source file
        Ok(Some(compile_source_file(path)?))
    }
}

/// Compile a single source file to a Module.
fn compile_source_file(path: &Path) -> Result<Module, Box<dyn std::error::Error>> {
    let fs = RealFs;
    
    // Use from_file for single file, collect for directory
    let file_set = if path.is_file() {
        FileSet::from_file(&fs, path)?
    } else {
        FileSet::collect(&fs, path)?
    };
    
    if file_set.files.is_empty() {
        println!("[VO:ERROR:no .vo files found]");
        return Err("no .vo files found".into());
    }
    
    let vfs_config = VfsConfig::from_env(file_set.root.clone());
    let vfs = vfs_config.to_vfs();
    
    let project = analyze_project(file_set, &vfs).map_err(|e| {
        // Use DiagnosticEmitter for rich error output with source code
        if let AnalysisError::Check(ref diags, ref source_map) = e {
            let emitter = DiagnosticEmitter::new(source_map);
            eprintln!(); // blank line before errors
            emitter.emit_all(diags);
            println!("[VO:ERROR:type check failed: {} error(s)]", diags.error_count());
        } else {
            println!("[VO:ERROR:{}]", e);
        }
        format!("analysis error: {}", e)
    })?;
    
    let module = compile_project(&project).map_err(|e| {
        println!("[VO:ERROR:{:?}]", e);
        format!("codegen error: {:?}", e)
    })?;
    
    Ok(module)
}

/// Run a module using the VM interpreter.
fn run_vm(module: Module, _std_mode: StdMode) -> Result<(), Box<dyn std::error::Error>> {
    let mut vm = Vm::new();
    vm.load(module);
    
    match vm.run() {
        Ok(()) => {
            println!("[VO:OK]");
            Ok(())
        }
        Err(e) => {
            println!("[VO:PANIC:{:?}]", e);
            Err(format!("panic: {:?}", e).into())
        }
    }
}

/// Run a module using the JIT compiler.
///
/// Uses the same VM as interpreter mode, but with JIT enabled.
/// Hot functions are automatically compiled to native code.
///
/// Environment variables:
/// - `VO_JIT_CALL_THRESHOLD`: Call count threshold (default: 100)
/// - `VO_JIT_LOOP_THRESHOLD`: Loop back-edge threshold (default: 1000)
fn run_jit(module: Module, _std_mode: StdMode) -> Result<(), Box<dyn std::error::Error>> {
    let call_threshold = std::env::var("VO_JIT_CALL_THRESHOLD")
        .ok()
        .and_then(|s| s.parse().ok());
    let loop_threshold = std::env::var("VO_JIT_LOOP_THRESHOLD")
        .ok()
        .and_then(|s| s.parse().ok());
    
    let mut vm = match (call_threshold, loop_threshold) {
        (Some(call), Some(loop_)) => Vm::with_jit_thresholds(call, loop_),
        (Some(call), None) => Vm::with_jit_thresholds(call, 1000),
        (None, Some(loop_)) => Vm::with_jit_thresholds(100, loop_),
        (None, None) => Vm::new(),
    };
    vm.init_jit();
    vm.load(module);
    
    match vm.run() {
        Ok(()) => {
            println!("[VO:OK]");
            Ok(())
        }
        Err(e) => {
            println!("[VO:PANIC:{:?}]", e);
            Err(format!("panic: {:?}", e).into())
        }
    }
}
