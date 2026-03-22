//! Execution functions for Vo modules.

use std::fmt;
use std::sync::{Arc, atomic::AtomicBool};
use vo_common_core::debug_info::SourceLoc;
use vo_vm::bytecode::Module;
use vo_vm::vm::{RuntimeTrapKind, SchedulingOutcome, Vm, VmError};
use vo_runtime::ext_loader::{ExtensionLoader, ExtensionManifest};
use vo_runtime::output::{OutputSink, StdoutSink};

use crate::compile::{CompileOutput, CompileError};
use crate::toolchain::ensure_toolchain_host_installed;

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
    Interrupted,
    IndexOutOfBounds,
    NilPointerDereference,
    TypeAssertionFailed,
    DivisionByZero,
    SendOnClosedChannel,
    Deadlock,
    Other,
}

impl RuntimeError {
    fn from_vm_error(e: &VmError, module: &Module) -> Self {
        let lookup = |loc: &Option<vo_vm::vm::ErrorLocation>| {
            loc.as_ref().and_then(|l| module.debug_info.lookup(l.func_id, l.pc))
        };
        
        let (message, location, kind) = match e {
            VmError::Interrupted => {
                ("interrupted by host".to_string(), None, RuntimeErrorKind::Interrupted)
            }
            VmError::RuntimeTrap { kind, msg, loc } => {
                let k = match kind {
                    RuntimeTrapKind::IndexOutOfBounds => RuntimeErrorKind::IndexOutOfBounds,
                    RuntimeTrapKind::NilPointerDereference => RuntimeErrorKind::NilPointerDereference,
                    RuntimeTrapKind::TypeAssertionFailed => RuntimeErrorKind::TypeAssertionFailed,
                    RuntimeTrapKind::DivisionByZero => RuntimeErrorKind::DivisionByZero,
                    RuntimeTrapKind::SendOnClosedChannel => RuntimeErrorKind::SendOnClosedChannel,
                    _ => RuntimeErrorKind::Other,
                };
                (msg.clone(), lookup(loc), k)
            }
            VmError::PanicUnwound { msg, loc } => {
                (msg.as_deref().unwrap_or("panic").to_string(), lookup(loc), RuntimeErrorKind::Panic)
            }
            VmError::Deadlock(msg) => {
                (msg.clone(), None, RuntimeErrorKind::Deadlock)
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

impl fmt::Display for RunError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RunError::Compile(e) => write!(f, "{}", e),
            RunError::Runtime(e) => {
                if let Some(loc) = &e.location {
                    write!(f, "{}:{}: {}", loc.file, loc.line, e.message)
                } else {
                    write!(f, "{}", e.message)
                }
            }
        }
    }
}

impl std::error::Error for RunError {}

impl From<CompileError> for RunError {
    fn from(e: CompileError) -> Self {
        RunError::Compile(e)
    }
}

/// Run a compiled module with output to stdout.
pub fn run(compiled: CompileOutput, mode: RunMode, args: Vec<String>) -> Result<(), RunError> {
    run_with_output(compiled, mode, args, Arc::new(StdoutSink))
}

/// Run a compiled module with a custom output sink.
///
/// The sink receives all output from `fmt.Print`, `println`, etc.
/// Use `CaptureSink` to collect output, or `StdoutSink` for normal behavior.
pub fn run_with_output(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<String>,
    sink: Arc<dyn OutputSink>,
) -> Result<(), RunError> {
    run_with_output_interruptible(compiled, mode, args, sink, None)
}

pub fn run_with_output_interruptible(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<String>,
    sink: Arc<dyn OutputSink>,
    interrupt_flag: Option<Arc<AtomicBool>>,
) -> Result<(), RunError> {
    ensure_toolchain_host_installed();
    let CompileOutput {
        module,
        source_root: _,
        extensions,
        locked_modules,
    } = compiled;
    let ext_loader = load_extensions(&extensions, &locked_modules)?;
    
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
                .unwrap_or(50);
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
    
    vm.state.output = sink;
    vm.set_program_args(args);
    if let Some(interrupt_flag) = interrupt_flag {
        vm.set_interrupt_flag(interrupt_flag);
    }
    vm.load_with_extensions(module, ext_loader);

    let outcome = vm.run().map_err(|e| vm_err_to_run_err(&vm, &e))?;
    if outcome == SchedulingOutcome::Blocked {
        let e = vm.deadlock_err();
        return Err(vm_err_to_run_err(&vm, &e));
    }
    Ok(())
}

fn vm_err_to_run_err(vm: &Vm, e: &VmError) -> RunError {
    let runtime_err = vm.module()
        .map(|m| RuntimeError::from_vm_error(e, m))
        .unwrap_or_else(|| RuntimeError {
            message: format!("{:?}", e),
            location: None,
            kind: RuntimeErrorKind::Other,
        });
    RunError::Runtime(runtime_err)
}

/// Build a GUI VM from compiled output, ready for use with `vo-app-runtime`.
///
/// This handles the standard sequence: ensure toolchain installed, build
/// native extensions, create a VM with external island transport enabled,
/// and load the module with extensions.
pub fn build_gui_vm(compiled: CompileOutput) -> Result<Vm, String> {
    ensure_toolchain_host_installed();
    let ext_loader = load_extensions(&compiled.extensions, &compiled.locked_modules)
        .map_err(|e| e.to_string())?;
    let mut vm = Vm::new();
    vm.enable_external_island_transport();
    vm.load_with_extensions(compiled.module, ext_loader);
    Ok(vm)
}

fn load_extensions(
    manifests: &[ExtensionManifest],
    locked_modules: &[vo_module::schema::lockfile::LockedModule],
) -> Result<Option<ExtensionLoader>, RunError> {
    if manifests.is_empty() {
        return Ok(None);
    }

    crate::compile::ensure_extension_manifests_built(manifests, locked_modules)
        .map_err(|e| RunError::Runtime(RuntimeError {
            message: format!("failed to prepare native extensions: {}", e),
            location: None,
            kind: RuntimeErrorKind::Other,
        }))?;

    let loader = ExtensionLoader::from_manifests(manifests)
        .map_err(|e| RunError::Runtime(RuntimeError {
            message: format!("failed to load extensions: {}", e),
            location: None,
            kind: RuntimeErrorKind::Other,
        }))?;
    Ok(Some(loader))
}
