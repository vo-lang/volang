//! `gox run` command - Run a GoX program.

use std::path::Path;
use gox_common::diagnostics::DiagnosticEmitter;
use gox_common::vfs::{FileSet, RealFs};
use gox_analysis::{analyze_project, AnalysisError};
use gox_module::VfsConfig;
use gox_codegen_vm::compile_project;
use gox_runtime_vm::extern_fn::StdMode;
use gox_vm::{Module, VmResult};
use gox_syntax::parser;
use crate::printer::AstPrinter;
use crate::bytecode_text;

/// Execution mode for running programs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RunMode {
    #[default]
    Vm,
    Jit,
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

/// Run a GoX program.
///
/// # Arguments
/// * `file` - Path to .gox source, .goxt bytecode, or project directory
/// * `mode` - Execution mode (vm or jit)
/// * `std_mode` - Stdlib mode (core or full)
/// * `print_ast` - If true, print AST and exit
/// * `print_codegen` - If true, print bytecode and exit
///
/// # Output Tags (for script parsing)
/// * `[GOX:OK]` - Execution completed successfully
/// * `[GOX:PANIC:message]` - Program panicked
/// * `[GOX:ERROR:message]` - Compilation/analysis error
///
/// # Examples
/// ```text
/// gox run hello.gox
/// gox run hello.gox --mode=jit
/// gox run hello.gox --ast
/// gox run hello.gox --codegen
/// gox run myproject/
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
        RunMode::Jit => run_jit(module),
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
        println!("[GOX:ERROR:parse error]");
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
    
    if file.ends_with(".gox") {
        // Source file - compile and return
        Ok(Some(compile_source_file(path)?))
    } else if file.ends_with(".goxc") || file.ends_with(".goxb") {
        // Binary format
        let bytes = std::fs::read(file)?;
        Ok(Some(Module::from_bytes(&bytes)?))
    } else if file.ends_with(".goxt") {
        // Text format (.goxt)
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
        println!("[GOX:ERROR:no .gox files found]");
        return Err("no .gox files found".into());
    }
    
    let vfs_config = VfsConfig::from_env(file_set.root.clone());
    let vfs = vfs_config.to_vfs();
    
    let project = analyze_project(file_set, &vfs).map_err(|e| {
        // Use DiagnosticEmitter for rich error output with source code
        if let AnalysisError::Check(ref diags, ref source_map) = e {
            let emitter = DiagnosticEmitter::new(source_map);
            eprintln!(); // blank line before errors
            emitter.emit_all(diags);
            println!("[GOX:ERROR:type check failed: {} error(s)]", diags.error_count());
        } else {
            println!("[GOX:ERROR:{}]", e);
        }
        format!("analysis error: {}", e)
    })?;
    
    let module = compile_project(&project).map_err(|e| {
        println!("[GOX:ERROR:{:?}]", e);
        format!("codegen error: {:?}", e)
    })?;
    
    Ok(module)
}

/// Run a module using the VM interpreter.
fn run_vm(module: Module, std_mode: StdMode) -> Result<(), Box<dyn std::error::Error>> {
    let mut vm = gox_runtime_vm::create_vm_with_mode(std_mode);
    vm.load_module(module);
    
    match vm.run() {
        VmResult::Done | VmResult::Ok => {
            println!("[GOX:OK]");
            Ok(())
        }
        VmResult::Panic(msg) => {
            println!("[GOX:PANIC:{}]", msg);
            Err(format!("panic: {}", msg).into())
        }
        VmResult::Yield => {
            println!("[GOX:ERROR:unexpected yield]");
            Err("unexpected yield".into())
        }
    }
}

/// Run a module using the JIT compiler.
fn run_jit(module: Module) -> Result<(), Box<dyn std::error::Error>> {
    let mut jit = gox_jit::JitCompiler::new()
        .map_err(|e| {
            println!("[GOX:ERROR:JIT init: {}]", e);
            format!("JIT init error: {}", e)
        })?;
    
    jit.compile_module(&module)
        .map_err(|e| {
            println!("[GOX:ERROR:JIT compile: {}]", e);
            format!("JIT compile error: {}", e)
        })?;
    
    // Get the entry function and call it
    let entry_fn: fn() = unsafe {
        jit.get_function("$entry")
            .ok_or_else(|| {
                println!("[GOX:ERROR:entry function not found]");
                "entry function not found"
            })?
    };
    
    // Initialize scheduler for channel/goroutine support
    let scheduler = gox_runtime_native::goroutine::Scheduler::new();
    gox_runtime_native::goroutine::set_current_scheduler(&scheduler);
    
    entry_fn();
    println!("[GOX:OK]");
    Ok(())
}
