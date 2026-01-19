//! Execution functions for Vo modules.

use std::path::Path;
use vo_common_core::debug_info::SourceLoc;
use vo_vm::bytecode::Module;
use vo_vm::vm::{Vm, VmError};
use vo_runtime::ext_loader::{ExtensionLoader, ExtensionManifest};

use crate::compile::{compile_source, CompileOutput, CompileError};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RunMode {
    #[default]
    Vm,
    Jit,
}

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub location: Option<SourceLoc>,
    pub kind: RuntimeErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeErrorKind {
    Panic,
    IndexOutOfBounds,
    NilPointerDereference,
    TypeAssertionFailed,
    DivisionByZero,
    SendOnClosedChannel,
    Other,
}

impl RuntimeError {
    fn from_vm_error(e: &VmError, module: &Module) -> Self {
        let (message, location, kind) = match e {
            VmError::PanicUnwound { msg, loc } => {
                let msg = msg.as_deref().unwrap_or("panic").to_string();
                let loc = loc.as_ref().and_then(|l| module.debug_info.lookup(l.func_id, l.pc));
                (msg, loc, RuntimeErrorKind::Panic)
            }
            VmError::IndexOutOfBounds(loc) => {
                let loc = loc.as_ref().and_then(|l| module.debug_info.lookup(l.func_id, l.pc));
                ("index out of bounds".to_string(), loc, RuntimeErrorKind::IndexOutOfBounds)
            }
            VmError::NilPointerDereference(loc) => {
                let loc = loc.as_ref().and_then(|l| module.debug_info.lookup(l.func_id, l.pc));
                ("nil pointer dereference".to_string(), loc, RuntimeErrorKind::NilPointerDereference)
            }
            VmError::TypeAssertionFailed(loc) => {
                let loc = loc.as_ref().and_then(|l| module.debug_info.lookup(l.func_id, l.pc));
                ("type assertion failed".to_string(), loc, RuntimeErrorKind::TypeAssertionFailed)
            }
            VmError::DivisionByZero(loc) => {
                let loc = loc.as_ref().and_then(|l| module.debug_info.lookup(l.func_id, l.pc));
                ("division by zero".to_string(), loc, RuntimeErrorKind::DivisionByZero)
            }
            VmError::SendOnClosedChannel(loc) => {
                let loc = loc.as_ref().and_then(|l| module.debug_info.lookup(l.func_id, l.pc));
                ("send on closed channel".to_string(), loc, RuntimeErrorKind::SendOnClosedChannel)
            }
            _ => (format!("{:?}", e), None, RuntimeErrorKind::Other),
        };
        RuntimeError { message, location, kind }
    }
}

#[derive(Debug)]
pub enum RunError {
    Compile(CompileError),
    Runtime(RuntimeError),
}

impl std::fmt::Display for RunError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunError::Compile(e) => write!(f, "{}", e),
            RunError::Runtime(e) => write!(f, "{}", e.message),
        }
    }
}

impl std::error::Error for RunError {}

impl From<CompileError> for RunError {
    fn from(e: CompileError) -> Self {
        RunError::Compile(e)
    }
}

pub fn run_module(module: Module, mode: RunMode) -> Result<(), RunError> {
    let source_root = std::env::current_dir().unwrap_or_default();
    run_module_impl(module, &source_root, mode, &[], std::env::args().collect())
}

pub fn run_file(path: &str) -> Result<(), RunError> {
    run_file_with_mode(path, RunMode::Vm)
}

pub fn run_file_with_mode(path: &str, mode: RunMode) -> Result<(), RunError> {
    let output = compile_source(path)?;
    let args = vec![path.to_string()];
    run_module_with_extensions(output, mode, args)
}

pub fn run_module_with_extensions(output: CompileOutput, mode: RunMode, args: Vec<String>) -> Result<(), RunError> {
    run_module_impl(output.module, &output.source_root, mode, &output.extensions, args)
}

fn run_module_impl(
    module: Module,
    _source_root: &Path,
    mode: RunMode,
    extensions: &[ExtensionManifest],
    args: Vec<String>,
) -> Result<(), RunError> {
    let ext_loader = load_extensions(extensions)?;
    
    #[cfg(feature = "jit")]
    let mut vm = match mode {
        RunMode::Vm => Vm::new(),
        RunMode::Jit => {
            use vo_vm::JitConfig;
            
            let call_threshold = std::env::var("VO_JIT_CALL_THRESHOLD")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(100);
            let loop_threshold = std::env::var("VO_JIT_LOOP_THRESHOLD")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(1000);
            let debug_ir = std::env::var("VO_JIT_DEBUG").is_ok();
            
            let config = JitConfig { call_threshold, loop_threshold, debug_ir };
            let mut vm = Vm::with_jit_config(config);
            vm.init_jit();
            vm
        }
    };
    
    #[cfg(not(feature = "jit"))]
    let mut vm = {
        if mode == RunMode::Jit {
            eprintln!("Warning: JIT mode requested but not available, falling back to VM");
        }
        Vm::new()
    };
    
    vm.set_program_args(args);
    vm.load_with_extensions(module, ext_loader.as_ref());
    
    match vm.run() {
        Ok(()) => Ok(()),
        Err(e) => {
            let runtime_err = vm.module()
                .map(|m| RuntimeError::from_vm_error(&e, m))
                .unwrap_or_else(|| RuntimeError {
                    message: format!("{:?}", e),
                    location: None,
                    kind: RuntimeErrorKind::Other,
                });
            Err(RunError::Runtime(runtime_err))
        }
    }
}

fn load_extensions(manifests: &[ExtensionManifest]) -> Result<Option<ExtensionLoader>, RunError> {
    if manifests.is_empty() {
        return Ok(None);
    }
    eprintln!("[DEBUG] Loading {} extensions", manifests.len());
    for m in manifests {
        eprintln!("[DEBUG]   - {} @ {:?}", m.name, m.native_path);
    }

    let mut loader = ExtensionLoader::new();
    for manifest in manifests {
        match loader.load(&manifest.native_path, &manifest.name) {
            Ok(()) => eprintln!("[DEBUG]   Loaded: {}", manifest.name),
            Err(e) => {
                let message = format!("failed to load extension '{}': {}", manifest.name, e);
                return Err(RunError::Runtime(RuntimeError {
                    message,
                    location: None,
                    kind: RuntimeErrorKind::Other,
                }));
            }
        }
    }
    Ok(Some(loader))
}
