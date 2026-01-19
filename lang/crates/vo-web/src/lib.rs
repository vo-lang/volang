//! WebAssembly bindings for Vo Playground.
//!
//! Provides compile and run APIs for executing Vo code in browsers.
//!
//! # Features
//! - `compiler` (default): Full compiler chain, can compile Vo source code
//! - No features: Bytecode execution only

use wasm_bindgen::prelude::*;

#[cfg(feature = "compiler")]
use std::path::{Path, PathBuf};

#[cfg(feature = "compiler")]
use vo_common::vfs::{FileSet, MemoryFs};

/// Initialize panic hook for better error messages in console.
#[cfg(feature = "compiler")]
#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

/// Get version information.
#[wasm_bindgen]
pub fn version() -> String {
    concat!("Vo Web ", env!("CARGO_PKG_VERSION")).into()
}

/// Compilation result returned to JavaScript.
#[wasm_bindgen]
pub struct CompileResult {
    success: bool,
    bytecode: Option<Vec<u8>>,
    error_message: Option<String>,
    error_line: Option<u32>,
    error_column: Option<u32>,
}

#[wasm_bindgen]
impl CompileResult {
    #[wasm_bindgen(getter)]
    pub fn success(&self) -> bool {
        self.success
    }

    #[wasm_bindgen(getter)]
    pub fn bytecode(&self) -> Option<Vec<u8>> {
        self.bytecode.clone()
    }

    #[wasm_bindgen(getter, js_name = "errorMessage")]
    pub fn error_message(&self) -> Option<String> {
        self.error_message.clone()
    }

    #[wasm_bindgen(getter, js_name = "errorLine")]
    pub fn error_line(&self) -> Option<u32> {
        self.error_line
    }

    #[wasm_bindgen(getter, js_name = "errorColumn")]
    pub fn error_column(&self) -> Option<u32> {
        self.error_column
    }
}

/// Run result returned to JavaScript.
#[wasm_bindgen]
pub struct RunResult {
    status: String,
    stdout: String,
    stderr: String,
}

#[wasm_bindgen]
impl RunResult {
    #[wasm_bindgen(getter)]
    pub fn status(&self) -> String {
        self.status.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn stdout(&self) -> String {
        self.stdout.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn stderr(&self) -> String {
        self.stderr.clone()
    }
}

/// Compile Vo source code to bytecode.
///
/// # Arguments
/// * `source` - Vo source code
/// * `filename` - Optional filename for error messages (defaults to "main.vo")
///
/// # Returns
/// CompileResult with bytecode on success, or error details on failure.
#[cfg(feature = "compiler")]
#[wasm_bindgen]
pub fn compile(source: &str, filename: Option<String>) -> CompileResult {
    let filename = filename.unwrap_or_else(|| "main.vo".to_string());
    
    match compile_source(source, &filename) {
        Ok(bytecode) => CompileResult {
            success: true,
            bytecode: Some(bytecode),
            error_message: None,
            error_line: None,
            error_column: None,
        },
        Err(err) => CompileResult {
            success: false,
            bytecode: None,
            error_message: Some(err.message),
            error_line: err.line,
            error_column: err.column,
        },
    }
}

#[cfg(feature = "compiler")]
struct CompileError {
    message: String,
    line: Option<u32>,
    column: Option<u32>,
}

#[cfg(feature = "compiler")]
use include_dir::{include_dir, Dir};

#[cfg(feature = "compiler")]
static STDLIB_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../../stdlib");

#[cfg(feature = "compiler")]
fn build_stdlib_fs() -> MemoryFs {
    let mut fs = MemoryFs::new();
    add_dir_to_fs(&STDLIB_DIR, &mut fs, "");
    fs
}

#[cfg(feature = "compiler")]
fn add_dir_to_fs(dir: &Dir<'_>, fs: &mut MemoryFs, _prefix: &str) {
    for file in dir.files() {
        if let Some(name) = file.path().to_str() {
            if name.ends_with(".vo") {
                if let Some(content) = file.contents_utf8() {
                    fs.add_file(PathBuf::from(name), content.to_string());
                }
            }
        }
    }
    for subdir in dir.dirs() {
        if let Some(name) = subdir.path().to_str() {
            add_dir_to_fs(subdir, fs, name);
        }
    }
}

#[cfg(feature = "compiler")]
fn compile_source(source: &str, filename: &str) -> Result<Vec<u8>, CompileError> {
    use vo_analysis::analyze_project;
    use vo_codegen::compile_project;
    use vo_module::vfs::{PackageResolver, StdSource, LocalSource, ModSource};
    
    // Create virtual file system with the source
    let mut fs = MemoryFs::new();
    fs.add_file(PathBuf::from(filename), source.to_string());
    
    // Create stdlib FS with embedded stdlib
    let std_fs = build_stdlib_fs();
    
    // Create FileSet
    let file_set = FileSet::from_file(&fs, Path::new(filename), PathBuf::from("."))
        .map_err(|e| CompileError {
            message: format!("Failed to read file: {}", e),
            line: None,
            column: None,
        })?;
    
    // Create package resolver with full stdlib
    let empty_fs = MemoryFs::new();
    let resolver = PackageResolver {
        std: StdSource::with_fs(std_fs),
        local: LocalSource::with_fs(fs.clone()),
        r#mod: ModSource::with_fs(empty_fs),
    };
    
    // Analyze project
    let project = analyze_project(file_set, &resolver)
        .map_err(|e| {
            CompileError {
                message: format!("{}", e),
                line: None,
                column: None,
            }
        })?;
    
    // Compile to bytecode
    let module = compile_project(&project)
        .map_err(|e| CompileError {
            message: format!("{:?}", e),
            line: None,
            column: None,
        })?;
    
    // Serialize to bytes
    Ok(module.serialize())
}

/// Run bytecode.
///
/// # Arguments
/// * `bytecode` - Compiled bytecode from compile()
///
/// # Returns
/// RunResult with status. Note: stdout goes to console.log in WASM.
#[wasm_bindgen]
pub fn run(bytecode: &[u8]) -> RunResult {
    match run_bytecode(bytecode) {
        Ok(()) => RunResult {
            status: "ok".to_string(),
            stdout: String::new(),
            stderr: String::new(),
        },
        Err(msg) => RunResult {
            status: "error".to_string(),
            stdout: String::new(),
            stderr: msg,
        },
    }
}

fn run_bytecode(bytecode: &[u8]) -> Result<(), String> {
    use vo_vm::vm::Vm;
    use vo_vm::bytecode::Module;
    
    // Deserialize module
    let module = Module::deserialize(bytecode)
        .map_err(|e| format!("Failed to load bytecode: {:?}", e))?;
    
    // Create and run VM
    let mut vm = Vm::new();
    vm.load(module);
    
    vm.run().map_err(|e| format!("{:?}", e))
}

/// Compile and run in one step.
///
/// Convenience function that combines compile() and run().
#[cfg(feature = "compiler")]
#[wasm_bindgen(js_name = "compileAndRun")]
pub fn compile_and_run(source: &str, filename: Option<String>) -> RunResult {
    let result = compile(source, filename);
    if !result.success {
        return RunResult {
            status: "compile_error".to_string(),
            stdout: String::new(),
            stderr: result.error_message.unwrap_or_default(),
        };
    }
    
    run(&result.bytecode.unwrap())
}
