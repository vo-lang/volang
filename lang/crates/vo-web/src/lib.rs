//! Vo Web Runtime - WASM bindings and generic VM management.
//!
//! # Layers
//! 1. **WASM API** (`compile`, `run`, `compileAndRun`) - for JS interop
//! 2. **Generic VM API** (`create_vm`, `call_closure`) - for event-driven apps
//!
//! # Features
//! - `compiler` (default): Full compiler chain
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
#[cfg(feature = "compiler")]
#[wasm_bindgen]
pub fn compile(source: &str, filename: Option<String>) -> CompileResult {
    let filename = filename.unwrap_or_else(|| "main.vo".to_string());
    
    match compile_source_with_std_fs(source, &filename, build_stdlib_fs()) {
        Ok(bytecode) => CompileResult {
            success: true,
            bytecode: Some(bytecode),
            error_message: None,
            error_line: None,
            error_column: None,
        },
        Err(msg) => CompileResult {
            success: false,
            bytecode: None,
            error_message: Some(msg),
            error_line: None,
            error_column: None,
        },
    }
}

// Re-export stdlib filesystem
pub use vo_stdlib::EmbeddedStdlib;

/// Build the standard library filesystem. Exported for libraries to extend.
#[cfg(feature = "compiler")]
pub fn build_stdlib_fs() -> MemoryFs {
    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let mut fs = MemoryFs::new();
    
    fn add_dir_recursive(stdlib: &vo_stdlib::EmbeddedStdlib, fs: &mut MemoryFs, path: &Path) {
        use vo_common::vfs::FileSystem;
        if let Ok(entries) = stdlib.read_dir(path) {
            for entry in entries {
                if stdlib.is_dir(&entry) {
                    add_dir_recursive(stdlib, fs, &entry);
                } else if entry.to_str().map(|s| s.ends_with(".vo")).unwrap_or(false) {
                    if let Ok(content) = stdlib.read_file(&entry) {
                        fs.add_file(entry, content);
                    }
                }
            }
        }
    }
    
    add_dir_recursive(&stdlib, &mut fs, Path::new("."));
    fs
}

/// Compile source with a custom stdlib filesystem.
/// Exported for libraries (like vogui) that need to add extra packages.
#[cfg(feature = "compiler")]
pub fn compile_source_with_std_fs(source: &str, filename: &str, std_fs: MemoryFs) -> Result<Vec<u8>, String> {
    use vo_analysis::analyze_project;
    use vo_codegen::compile_project;
    use vo_module::vfs::{PackageResolver, StdSource, LocalSource, ModSource};
    
    // Create virtual file system with the source
    let mut fs = MemoryFs::new();
    fs.add_file(PathBuf::from(filename), source.to_string());
    
    // Create FileSet
    let file_set = FileSet::from_file(&fs, Path::new(filename), PathBuf::from("."))
        .map_err(|e| format!("Failed to read file: {}", e))?;
    
    // Create package resolver with provided stdlib
    let empty_fs = MemoryFs::new();
    let resolver = PackageResolver {
        std: StdSource::with_fs(std_fs),
        local: LocalSource::with_fs(fs.clone()),
        r#mod: ModSource::with_fs(empty_fs),
    };
    
    // Analyze project
    let project = analyze_project(file_set, &resolver)
        .map_err(|e| format!("{}", e))?;
    
    // Compile to bytecode
    let module = compile_project(&project)
        .map_err(|e| format!("{:?}", e))?;
    
    // Serialize to bytes
    Ok(module.serialize())
}

/// Run bytecode.
#[wasm_bindgen]
pub fn run(bytecode: &[u8]) -> RunResult {
    match create_vm(bytecode, |_, _| {}) {
        Ok(_) => RunResult {
            status: "ok".to_string(),
            stdout: vo_runtime::output::take_output(),
            stderr: String::new(),
        },
        Err(msg) => RunResult {
            status: "error".to_string(),
            stdout: vo_runtime::output::take_output(),
            stderr: msg,
        },
    }
}

/// Compile and run in one step.
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

// =============================================================================
// Generic VM Management API (for event-driven apps)
// =============================================================================

pub use vo_vm::vm::Vm;
pub use vo_vm::bytecode::{Module, ExternDef};
pub use vo_runtime::ffi::ExternRegistry;
pub use vo_runtime::gc::GcRef;

/// Type alias for extern registration function.
pub type ExternRegistrar = fn(&mut ExternRegistry, &[ExternDef]);

/// Create a VM from bytecode, register externs, and run initialization.
pub fn create_vm(bytecode: &[u8], register_externs: ExternRegistrar) -> Result<Vm, String> {
    let module = Module::deserialize(bytecode)
        .map_err(|e| format!("Failed to load bytecode: {:?}", e))?;
    
    create_vm_from_module(module, register_externs)
}

/// Create a VM from a pre-deserialized module.
pub fn create_vm_from_module(module: Module, register_externs: ExternRegistrar) -> Result<Vm, String> {
    vo_runtime::output::clear_output();
    
    let mut vm = Vm::new();
    let reg = &mut vm.state.extern_registry;
    let exts = &module.externs;
    
    // stdlib (cross-platform)
    vo_stdlib::register_externs(reg, exts);
    
    // wasm platform
    vo_web_runtime_wasm::os::register_externs(reg, exts);
    vo_web_runtime_wasm::time::register_externs(reg, exts);
    vo_web_runtime_wasm::regexp::register_externs(reg, exts);
    vo_web_runtime_wasm::filepath::register_externs(reg, exts);
    
    // caller
    register_externs(reg, exts);
    
    vm.load(module);
    vm.run().map_err(|e| format!("{:?}", e))?;
    Ok(vm)
}

/// Call a closure in the VM (for handling external events).
pub fn call_closure(vm: &mut Vm, closure: GcRef, args: &[u64]) -> Result<(), String> {
    vo_runtime::output::clear_output();
    
    use vo_runtime::objects::closure;
    let func_id = closure::func_id(closure);
    let module = vm.module().expect("module not set");
    let func_def = &module.functions[func_id as usize];
    
    let full_args = vo_vm::vm::helpers::build_closure_args(
        closure as u64,
        closure,
        func_def,
        args.as_ptr(),
        args.len() as u32,
    );
    
    vm.spawn_call(func_id, &full_args);
    vm.run_scheduled().map_err(|e| format!("{:?}", e))?;
    
    Ok(())
}

/// Allocate a string in the VM's GC heap.
pub fn alloc_string(vm: &mut Vm, s: &str) -> GcRef {
    vo_runtime::objects::string::from_rust_str(&mut vm.state.gc, s)
}

/// Take captured output since last clear.
pub fn take_output() -> String {
    vo_runtime::output::take_output()
}
