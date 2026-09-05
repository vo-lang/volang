//! Execution functions for Vo modules.

use std::fmt;
use std::sync::{atomic::AtomicBool, Arc};

use vo_common_core::debug_info::SourceLoc;
use vo_runtime::ext_loader::{ExtensionLoader, NativeExtensionSpec};
use vo_runtime::output::{OutputSink, StdoutSink};
use vo_vm::bytecode::Module;
use vo_vm::vm::{RuntimeTrapKind, SchedulingOutcome, Vm, VmError};

use crate::compile::{CompileError, CompileOutput};
use crate::toolchain::ensure_toolchain_host_installed;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RunMode {
    #[default]
    Vm,
    Jit,
}

#[cfg(feature = "jit")]
fn jit_config_error(message: String) -> RunError {
    RunError::Runtime(RuntimeError {
        message,
        location: None,
        kind: RuntimeErrorKind::Other,
    })
}

#[cfg(feature = "jit")]
fn jit_env_u32(name: &str, default: u32) -> Result<u32, RunError> {
    match std::env::var(name) {
        Ok(value) => value.parse::<u32>().map_err(|_| {
            jit_config_error(format!(
                "invalid {name} value {value:?}: expected an unsigned 32-bit integer"
            ))
        }),
        Err(std::env::VarError::NotPresent) => Ok(default),
        Err(std::env::VarError::NotUnicode(_)) => Err(jit_config_error(format!(
            "invalid {name}: value is not valid Unicode"
        ))),
    }
}

#[cfg(feature = "jit")]
fn jit_env_u64(name: &str, default: u64) -> Result<u64, RunError> {
    match std::env::var(name) {
        Ok(value) => value.parse::<u64>().map_err(|_| {
            jit_config_error(format!(
                "invalid {name} value {value:?}: expected an unsigned 64-bit integer"
            ))
        }),
        Err(std::env::VarError::NotPresent) => Ok(default),
        Err(std::env::VarError::NotUnicode(_)) => Err(jit_config_error(format!(
            "invalid {name}: value is not valid Unicode"
        ))),
    }
}

#[cfg(feature = "jit")]
fn jit_env_bool(name: &str, default: bool) -> Result<bool, RunError> {
    match std::env::var(name) {
        Ok(value) => {
            let normalized = value.to_ascii_lowercase();
            match normalized.as_str() {
                "1" | "true" | "yes" => Ok(true),
                "0" | "false" | "no" => Ok(false),
                _ => Err(jit_config_error(format!(
                    "invalid {name} value {value:?}: expected true/false, yes/no, or 1/0"
                ))),
            }
        }
        Err(std::env::VarError::NotPresent) => Ok(default),
        Err(std::env::VarError::NotUnicode(_)) => Err(jit_config_error(format!(
            "invalid {name}: value is not valid Unicode"
        ))),
    }
}

pub type RunObservation = vo_vm::JitExecutionStats;

/// Executes the initial UI turn in the portable VM and renders its committed
/// protocol tree into useful semantic HTML plus a selective activation map.
/// Server and SSG tools use this path, keeping SSR behavior byte-identical to
/// the first browser/native render.
pub fn render_initial_ui_document(
    output: CompileOutput,
    mode: RunMode,
    metadata: &vo_ui_web::DocumentMetadata,
    limits: vo_ui_web::SsrLimits,
) -> Result<vo_ui_web::RenderedDocument, String> {
    render_initial_ui_document_at(output, mode, "/", metadata, limits)
}

/// Renders one declared route for SSR/SSG after installing the platform
/// location and before the application's initial mount executes.
pub fn render_initial_ui_document_at(
    output: CompileOutput,
    mode: RunMode,
    location: &str,
    metadata: &vo_ui_web::DocumentMetadata,
    limits: vo_ui_web::SsrLimits,
) -> Result<vo_ui_web::RenderedDocument, String> {
    let mut vm = build_native_gui_vm_for_mode(output, mode).map_err(|error| error.to_string())?;
    vo_ui_vm::set_platform_location(location).map_err(str::to_string)?;
    let outcome = vm
        .run()
        .map_err(|error| format!("SSR UI execution failed: {error:?}"))?;
    if outcome != SchedulingOutcome::SuspendedForHostEvents {
        return Err(format!(
            "SSR UI expected a mounted event wait; received {outcome:?}"
        ));
    }
    let frame = vm
        .take_host_output()
        .ok_or_else(|| "SSR UI did not publish an initial mutation batch".to_string())?;
    let protocol_limits = vo_ui_protocol::ProtocolLimits::default();
    let batch = vo_ui_protocol::decode_batch(&frame, protocol_limits)
        .map_err(|error| format!("SSR UI mutation frame is invalid: {error:?}"))?;
    let root = vo_ui_core::NodeId::new(0, 1);
    let mut tree = vo_ui_protocol::TreeMirror::new(batch.session_epoch, root, protocol_limits);
    tree.apply(&batch)
        .map_err(|error| format!("SSR UI tree rejected its initial batch: {error:?}"))?;
    vo_ui_web::render_document(&tree, metadata, limits)
        .map_err(|error| format!("SSR UI document rendering failed: {error:?}"))
}

pub fn render_run_observation_json(
    observation: RunObservation,
) -> Result<Vec<u8>, serde_json::Error> {
    let side_exits = vo_vm::JitSideExitReason::ALL
        .into_iter()
        .map(|reason| {
            (
                reason.as_str().to_string(),
                serde_json::Value::from(observation.side_exit_count(reason)),
            )
        })
        .collect::<serde_json::Map<_, _>>();
    serde_json::to_vec_pretty(&serde_json::json!({
        "schema": "volang.jit.execution-stats.v1",
        "scope": "root_vm",
        "function_entries": observation.function_entries,
        "loop_entries": observation.loop_entries,
        "side_exits": side_exits,
        "low_progress_function_disables": observation.low_progress_function_disables,
        "low_progress_loop_disables": observation.low_progress_loop_disables,
        "function_compilations": observation.function_compilations,
        "loop_compilations": observation.loop_compilations,
        "compilation_cache_hits": observation.compilation_cache_hits,
        "compilation_time_ns": observation.compilation_time_ns,
        "compiled_code_bytes": observation.compiled_code_bytes,
        "closure_prepare_callbacks": observation.closure_prepare_callbacks,
        "iface_prepare_callbacks": observation.iface_prepare_callbacks,
        "prepared_frame_reservations": observation.prepared_frame_reservations,
        "prepared_frame_slots_reserved": observation.prepared_frame_slots_reserved,
        "prepared_jit_dispatches": observation.prepared_jit_dispatches,
        "prepared_vm_dispatches": observation.prepared_vm_dispatches,
        "dynamic_ic_publications": observation.dynamic_ic_publications,
        "gc_safepoint_callbacks": observation.gc_safepoint_callbacks,
        "native_root_frames_scanned": observation.native_root_frames_scanned,
        "native_roots_scanned": observation.native_roots_scanned,
        "native_root_conditional_frames": observation.native_root_conditional_frames,
        "native_root_scan_budget_exhaustions": observation.native_root_scan_budget_exhaustions,
    }))
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
            loc.as_ref()
                .and_then(|l| module.debug_info.lookup(l.func_id, l.pc))
        };

        let (message, location, kind) = match e {
            VmError::Interrupted => (
                "interrupted by host".to_string(),
                None,
                RuntimeErrorKind::Interrupted,
            ),
            VmError::RuntimeTrap { kind, msg, loc } => {
                let k = match kind {
                    RuntimeTrapKind::IndexOutOfBounds => RuntimeErrorKind::IndexOutOfBounds,
                    RuntimeTrapKind::NilPointerDereference => {
                        RuntimeErrorKind::NilPointerDereference
                    }
                    RuntimeTrapKind::TypeAssertionFailed => RuntimeErrorKind::TypeAssertionFailed,
                    RuntimeTrapKind::DivisionByZero => RuntimeErrorKind::DivisionByZero,
                    RuntimeTrapKind::SendOnClosedChannel => RuntimeErrorKind::SendOnClosedChannel,
                    _ => RuntimeErrorKind::Other,
                };
                (msg.clone(), lookup(loc), k)
            }
            VmError::PanicUnwound { msg, loc } => (
                msg.as_deref().unwrap_or("panic").to_string(),
                lookup(loc),
                RuntimeErrorKind::Panic,
            ),
            VmError::IslandMemory(error) => (
                format!("Island managed-memory failure: {error}"),
                None,
                RuntimeErrorKind::Other,
            ),
            VmError::Deadlock(msg) => (msg.clone(), None, RuntimeErrorKind::Deadlock),
            VmError::Jit(msg) => (msg.clone(), None, RuntimeErrorKind::Other),
            _ => (format!("{:?}", e), None, RuntimeErrorKind::Other),
        };
        RuntimeError {
            message,
            location,
            kind,
        }
    }
}

#[derive(Debug)]
pub enum RunError {
    Compile(CompileError),
    Runtime(RuntimeError),
    /// The guest called `os.Exit` with a non-zero status.
    Exited(i32),
}

impl fmt::Display for RunError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RunError::Compile(e) => write!(f, "{}", e),
            RunError::Exited(code) => write!(f, "program exited with status {code}"),
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

/// Run with arbitrary-byte program arguments.
pub fn run_with_byte_args(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<Vec<u8>>,
) -> Result<(), RunError> {
    run_with_output_interruptible_observed_bytes(
        compiled,
        mode,
        args,
        Arc::new(StdoutSink),
        None,
        vo_vm::VmMemoryConfig::default(),
    )
    .map(|_| ())
}

pub fn run_with_byte_args_and_memory(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<Vec<u8>>,
    memory_config: vo_vm::VmMemoryConfig,
) -> Result<(), RunError> {
    run_with_byte_args_and_memory_observed(compiled, mode, args, memory_config).map(|_| ())
}

pub fn run_with_byte_args_and_memory_observed(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<Vec<u8>>,
    memory_config: vo_vm::VmMemoryConfig,
) -> Result<RunObservation, RunError> {
    run_with_output_interruptible_observed_bytes(
        compiled,
        mode,
        args,
        Arc::new(StdoutSink),
        None,
        memory_config,
    )
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

pub fn run_with_output_observed(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<String>,
    sink: Arc<dyn OutputSink>,
) -> Result<RunObservation, RunError> {
    run_with_output_interruptible_observed(compiled, mode, args, sink, None)
}

pub fn run_with_output_interruptible(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<String>,
    sink: Arc<dyn OutputSink>,
    interrupt_flag: Option<Arc<AtomicBool>>,
) -> Result<(), RunError> {
    run_with_output_interruptible_observed(compiled, mode, args, sink, interrupt_flag).map(|_| ())
}

pub fn run_with_output_interruptible_observed(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<String>,
    sink: Arc<dyn OutputSink>,
    interrupt_flag: Option<Arc<AtomicBool>>,
) -> Result<RunObservation, RunError> {
    run_with_output_interruptible_observed_bytes(
        compiled,
        mode,
        args.into_iter().map(String::into_bytes).collect(),
        sink,
        interrupt_flag,
        vo_vm::VmMemoryConfig::default(),
    )
}

fn run_with_output_interruptible_observed_bytes(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<Vec<u8>>,
    sink: Arc<dyn OutputSink>,
    interrupt_flag: Option<Arc<AtomicBool>>,
    memory_config: vo_vm::VmMemoryConfig,
) -> Result<RunObservation, RunError> {
    ensure_toolchain_host_installed();
    let CompileOutput {
        module,
        source_root: _,
        extensions,
        locked_modules: _,
    } = compiled;
    let ext_loader = load_extensions(&extensions)?;

    #[cfg(feature = "jit")]
    let mut vm = match mode {
        RunMode::Vm => Vm::try_with_memory_config(memory_config).map_err(|err| {
            RunError::Runtime(RuntimeError {
                message: format!("VM initialization failed: {err}"),
                location: None,
                kind: RuntimeErrorKind::Other,
            })
        })?,
        RunMode::Jit => {
            use vo_vm::JitConfig;

            let call_threshold = jit_env_u32("VO_JIT_CALL_THRESHOLD", 100)?;
            let loop_threshold = jit_env_u32("VO_JIT_LOOP_THRESHOLD", 50)?;
            let optimizing_threshold = jit_env_u64("VO_JIT_OPTIMIZING_THRESHOLD", 10_000)?;
            let debug_ir = jit_env_bool("VO_JIT_DEBUG", false)?;

            let config = JitConfig {
                call_threshold,
                loop_threshold,
                optimizing_threshold,
                debug_ir,
                ..JitConfig::default()
            };
            Vm::try_with_jit_and_memory_config(config, memory_config).map_err(|err| {
                RunError::Runtime(RuntimeError {
                    message: format!("JIT initialization failed: {err}"),
                    location: None,
                    kind: RuntimeErrorKind::Other,
                })
            })?
        }
    };

    #[cfg(not(feature = "jit"))]
    let mut vm = {
        if mode == RunMode::Jit {
            return Err(RunError::Runtime(RuntimeError {
                message: "JIT mode requested but vo-engine was built without the jit feature"
                    .to_string(),
                location: None,
                kind: RuntimeErrorKind::Other,
            }));
        }
        Vm::try_with_memory_config(memory_config).map_err(|err| {
            RunError::Runtime(RuntimeError {
                message: format!("VM initialization failed: {err}"),
                location: None,
                kind: RuntimeErrorKind::Other,
            })
        })?
    };

    vm.set_output_sink(sink);
    vm.set_program_args_bytes(args);
    if let Some(interrupt_flag) = interrupt_flag {
        vm.set_interrupt_flag(interrupt_flag);
    }
    register_ui_externs(&mut vm, &module).map_err(|message| {
        RunError::Runtime(RuntimeError {
            message,
            location: None,
            kind: RuntimeErrorKind::Other,
        })
    })?;
    vm.load_verified_with_extensions(module, ext_loader)
        .map_err(|e| vm_err_to_run_err(&vm, &e))?;

    let outcome = vm.run().map_err(|e| vm_err_to_run_err(&vm, &e))?;
    require_terminal_outcome(&vm, outcome)?;
    Ok(run_observation(&vm))
}

fn require_terminal_outcome(vm: &Vm, outcome: SchedulingOutcome) -> Result<(), RunError> {
    match outcome {
        SchedulingOutcome::Completed => Ok(()),
        SchedulingOutcome::Exited(0) => Ok(()),
        SchedulingOutcome::Exited(code) => Err(RunError::Exited(code)),
        SchedulingOutcome::Blocked => Err(vm_err_to_run_err(vm, &vm.deadlock_err())),
        SchedulingOutcome::Suspended => Err(RunError::Runtime(RuntimeError {
            message:
                "execution suspended with pending island work; continue it through a VM session"
                    .to_string(),
            location: None,
            kind: RuntimeErrorKind::Other,
        })),
        SchedulingOutcome::SuspendedForHostEvents => Err(RunError::Runtime(RuntimeError {
            message: "execution suspended for host events; continue it through an async VM session"
                .to_string(),
            location: None,
            kind: RuntimeErrorKind::Other,
        })),
        SchedulingOutcome::Panicked => Err(RunError::Runtime(RuntimeError {
            message: "VM reported a panic outcome without a structured runtime error".to_string(),
            location: None,
            kind: RuntimeErrorKind::Other,
        })),
    }
}

fn run_observation(vm: &Vm) -> RunObservation {
    vm.jit_execution_stats()
}

fn vm_err_to_run_err(vm: &Vm, e: &VmError) -> RunError {
    let runtime_err = vm
        .module()
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
    build_gui_vm_with_memory(compiled, vo_vm::VmMemoryConfig::default())
}

/// Build a GUI VM with an explicit per-Island managed-memory admission policy.
pub fn build_gui_vm_with_memory(
    compiled: CompileOutput,
    memory_config: vo_vm::VmMemoryConfig,
) -> Result<Vm, String> {
    build_gui_vm_with_island_transport(compiled, true, memory_config, RunMode::Vm)
}

/// Build a native GUI VM whose child islands execute inside the current
/// process. Native framework hosts use this when they provide the rendering
/// surface and extension host APIs directly instead of forwarding island
/// frames to a browser or another process.
pub fn build_native_gui_vm(compiled: CompileOutput) -> Result<Vm, String> {
    build_native_gui_vm_with_memory(compiled, vo_vm::VmMemoryConfig::default())
}

/// Build an in-process GUI VM with an explicit per-Island memory policy.
pub fn build_native_gui_vm_with_memory(
    compiled: CompileOutput,
    memory_config: vo_vm::VmMemoryConfig,
) -> Result<Vm, String> {
    build_gui_vm_with_island_transport(compiled, false, memory_config, RunMode::Vm)
}

/// Build an in-process UI VM for deterministic development tests in either
/// interpreter or JIT mode.
pub fn build_native_gui_vm_for_mode(compiled: CompileOutput, mode: RunMode) -> Result<Vm, String> {
    build_gui_vm_with_island_transport(compiled, false, vo_vm::VmMemoryConfig::default(), mode)
}

/// A fully verified replacement VM whose UI provider table was registered
/// without mutating the currently mounted component arena.
pub struct PreparedNativeUiReload {
    pub(crate) vm: Vm,
    pub(crate) component: Option<vo_ui_vm::ComponentArtifact>,
    pub(crate) component_bundle: Option<vo_ui_vm::ComponentBundle>,
}

pub fn prepare_native_gui_reload_for_mode(
    compiled: CompileOutput,
    mode: RunMode,
) -> Result<PreparedNativeUiReload, String> {
    ensure_toolchain_host_installed();
    let ext_loader = load_extensions(&compiled.extensions).map_err(|error| error.to_string())?;
    let mut vm = new_gui_vm_for_mode(vo_vm::VmMemoryConfig::default(), mode)?;
    let artifacts = register_ui_reload_externs(&mut vm, &compiled.module)?;
    vm.load_verified_with_extensions(compiled.module, ext_loader)
        .map_err(|error| format!("{error:?}"))?;
    Ok(PreparedNativeUiReload {
        vm,
        component: artifacts.component,
        component_bundle: artifacts.component_bundle,
    })
}

fn build_gui_vm_with_island_transport(
    compiled: CompileOutput,
    external_island_transport: bool,
    memory_config: vo_vm::VmMemoryConfig,
    mode: RunMode,
) -> Result<Vm, String> {
    ensure_toolchain_host_installed();
    let ext_loader = load_extensions(&compiled.extensions).map_err(|e| e.to_string())?;
    let mut vm = new_gui_vm_for_mode(memory_config, mode)?;
    if external_island_transport {
        vm.enable_external_island_transport();
    }
    register_ui_externs(&mut vm, &compiled.module)?;
    vm.load_verified_with_extensions(compiled.module, ext_loader)
        .map_err(|e| format!("{:?}", e))?;
    Ok(vm)
}

fn new_gui_vm_for_mode(memory_config: vo_vm::VmMemoryConfig, mode: RunMode) -> Result<Vm, String> {
    #[cfg(feature = "jit")]
    let vm = match mode {
        RunMode::Vm => Vm::try_with_memory_config(memory_config)
            .map_err(|error| format!("failed to initialize VM: {error}"))?,
        RunMode::Jit => {
            let config = vo_vm::JitConfig {
                call_threshold: jit_env_u32("VO_JIT_CALL_THRESHOLD", 100)
                    .map_err(|error| error.to_string())?,
                loop_threshold: jit_env_u32("VO_JIT_LOOP_THRESHOLD", 50)
                    .map_err(|error| error.to_string())?,
                optimizing_threshold: jit_env_u64("VO_JIT_OPTIMIZING_THRESHOLD", 10_000)
                    .map_err(|error| error.to_string())?,
                debug_ir: jit_env_bool("VO_JIT_DEBUG", false).map_err(|error| error.to_string())?,
                ..vo_vm::JitConfig::default()
            };
            Vm::try_with_jit_and_memory_config(config, memory_config)
                .map_err(|error| format!("failed to initialize JIT VM: {error}"))?
        }
    };
    #[cfg(not(feature = "jit"))]
    let vm = match mode {
        RunMode::Vm => Vm::try_with_memory_config(memory_config)
            .map_err(|error| format!("failed to initialize VM: {error}"))?,
        RunMode::Jit => return Err("JIT mode is unavailable in this vo build".to_string()),
    };
    Ok(vm)
}

fn register_ui_externs(
    vm: &mut Vm,
    module: &vo_runtime::bytecode::LoadedModule,
) -> Result<(), String> {
    let registry = vm
        .extern_registry_mut()
        .map_err(|error| format!("failed to configure UI extern providers: {error:?}"))?;
    vo_ui_vm::register_module(registry, module.module())
        .map_err(|error| format!("failed to register UI extern providers: {error}"))
}

fn register_ui_reload_externs(
    vm: &mut Vm,
    module: &vo_runtime::bytecode::LoadedModule,
) -> Result<vo_ui_vm::PreparedReloadModule, String> {
    let registry = vm
        .extern_registry_mut()
        .map_err(|error| format!("failed to configure UI reload extern providers: {error:?}"))?;
    vo_ui_vm::prepare_reload_module(registry, module.module())
        .map_err(|error| format!("failed to register UI reload extern providers: {error}"))
}

fn load_extensions(specs: &[NativeExtensionSpec]) -> Result<Option<ExtensionLoader>, RunError> {
    if specs.is_empty() {
        return Ok(None);
    }

    let loader = ExtensionLoader::from_specs(specs).map_err(|e| {
        RunError::Runtime(RuntimeError {
            message: format!("failed to load extensions: {}", e),
            location: None,
            kind: RuntimeErrorKind::Other,
        })
    })?;
    Ok(Some(loader))
}

#[cfg(test)]
mod terminal_outcome_tests {
    use super::*;

    #[test]
    fn run_observation_json_uses_the_canonical_jit_stats_schema() {
        let observation = RunObservation {
            function_entries: 3,
            loop_entries: 5,
            low_progress_function_disables: 1,
            low_progress_loop_disables: 2,
            ..RunObservation::default()
        };
        let bytes = render_run_observation_json(observation).expect("render observation");
        let value: serde_json::Value =
            serde_json::from_slice(&bytes).expect("parse rendered observation");

        assert_eq!(value["schema"], "volang.jit.execution-stats.v1");
        assert_eq!(value["scope"], "root_vm");
        assert_eq!(value["function_entries"], 3);
        assert_eq!(value["loop_entries"], 5);
        assert_eq!(value["low_progress_function_disables"], 1);
        assert_eq!(value["low_progress_loop_disables"], 2);
        assert_eq!(value["function_compilations"], 0);
        assert_eq!(value["loop_compilations"], 0);
        assert_eq!(value["compilation_cache_hits"], 0);
        assert_eq!(value["compilation_time_ns"], 0);
        assert_eq!(value["compiled_code_bytes"], 0);
        assert_eq!(value["closure_prepare_callbacks"], 0);
        assert_eq!(value["iface_prepare_callbacks"], 0);
        assert_eq!(value["prepared_frame_reservations"], 0);
        assert_eq!(value["prepared_frame_slots_reserved"], 0);
        assert_eq!(value["prepared_jit_dispatches"], 0);
        assert_eq!(value["prepared_vm_dispatches"], 0);
        assert_eq!(value["dynamic_ic_publications"], 0);
        assert_eq!(value["gc_safepoint_callbacks"], 0);
        assert_eq!(value["native_root_frames_scanned"], 0);
        assert_eq!(value["native_roots_scanned"], 0);
        assert_eq!(value["native_root_conditional_frames"], 0);
        assert_eq!(value["native_root_scan_budget_exhaustions"], 0);
        for reason in vo_vm::JitSideExitReason::ALL {
            assert_eq!(value["side_exits"][reason.as_str()], 0);
        }
    }

    #[test]
    fn interpreter_vm_exposes_a_zero_jit_observation() {
        assert_eq!(Vm::new().jit_execution_stats(), RunObservation::default());
    }

    #[test]
    fn suspended_outcomes_are_explicit_engine_errors() {
        let vm = Vm::new();
        for (outcome, expected) in [
            (SchedulingOutcome::Suspended, "pending island work"),
            (
                SchedulingOutcome::SuspendedForHostEvents,
                "suspended for host events",
            ),
            (
                SchedulingOutcome::Panicked,
                "without a structured runtime error",
            ),
        ] {
            let error = require_terminal_outcome(&vm, outcome)
                .expect_err("non-terminal engine outcome must be surfaced");
            assert!(error.to_string().contains(expected), "{error}");
        }
    }

    #[test]
    fn completed_and_zero_exit_outcomes_are_direct_successes() {
        let vm = Vm::new();
        require_terminal_outcome(&vm, SchedulingOutcome::Completed)
            .expect("completed execution should succeed");
        require_terminal_outcome(&vm, SchedulingOutcome::Exited(0))
            .expect("an explicit zero status should succeed");
        let error = require_terminal_outcome(&vm, SchedulingOutcome::Exited(7))
            .expect_err("a non-zero explicit status must remain observable");
        assert!(matches!(error, RunError::Exited(7)));
        assert!(error.to_string().contains("status 7"));
    }

    #[test]
    fn interpreter_surfaces_managed_allocation_failure_as_island_memory_error() {
        let compiled = crate::compile_string(
            r#"
package main

func main() {
	value := "managed allocation"
	println(len(value))
}
"#,
        )
        .expect("memory failure fixture should compile");
        assert!(
            compiled
                .module
                .functions
                .iter()
                .flat_map(|function| function.code.iter())
                .any(|instruction| {
                    instruction.opcode() == vo_runtime::instruction::Opcode::StrNew
                }),
            "fixture must execute a managed allocation"
        );

        let error = run_with_byte_args_and_memory(
            compiled,
            RunMode::Vm,
            Vec::new(),
            vo_vm::VmMemoryConfig {
                allocation_allowed: false,
                oom_policy: vo_vm::OomPolicy::TerminateIsland,
                ..vo_vm::VmMemoryConfig::default()
            },
        )
        .expect_err("disabled allocation must terminate the current Island");

        let RunError::Runtime(runtime) = error else {
            panic!("expected structured runtime memory error, got {error:?}");
        };
        assert!(
            runtime.message.contains("Island managed-memory failure"),
            "{}",
            runtime.message
        );
        assert!(
            runtime.message.contains("managed allocation is disabled"),
            "{}",
            runtime.message
        );
    }
}

#[cfg(all(test, feature = "jit"))]
mod tests {
    use super::*;

    use std::sync::atomic::{AtomicU64, Ordering};
    use std::sync::{Mutex, OnceLock};
    use vo_common_core::instruction::HINT_LOOP;
    use vo_runtime::bytecode::{
        ExternDef, ExternEffects, InstructionMetadata, ParamShape, ReturnShape,
    };
    use vo_runtime::instruction::Opcode;
    use vo_runtime::output::CaptureSink;
    use vo_runtime::SlotType;

    static PROCESS_ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    static GUEST_EXIT_SUBPROCESS_LOCK: Mutex<()> = Mutex::new(());
    const GUEST_EXIT_SUBPROCESS_ENV: &str = "VOLANG_GUEST_EXIT_SUBPROCESS";
    const GUEST_EXIT_SUBPROCESS_MARKER: &str = "volang-guest-exit-subprocess-started";
    static UI_VM_TEST_COUNTER: AtomicU64 = AtomicU64::new(1);
    // The first JIT execution compiles guest and island code inside `Vm::run`.
    // Leave ample headroom for debug builds while retaining a real hard bound:
    // the parent can terminate a child even if it is blocked in `JoinHandle::join`.
    const GUEST_EXIT_SUBPROCESS_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(30);

    struct UiTestWorkspace(std::path::PathBuf);

    impl UiTestWorkspace {
        fn create() -> Self {
            Self::create_with_main(
                r#"
package main
import "github.com/vo-lang/ui"
var label = "before"
func App() ui.View {
	return ui.Padding(ui.Column(
		ui.Text(label),
		ui.TextInput(label, "Type", func(event ui.Event) { label = event.Text }),
	), 12)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
            )
        }

        fn create_with_main(source: &str) -> Self {
            let sequence = UI_VM_TEST_COUNTER.fetch_add(1, Ordering::Relaxed);
            let root = std::env::temp_dir().join(format!(
                "volang-ui-vm-test-{}-{sequence}",
                std::process::id()
            ));
            let app = root.join("app");
            let ui = root.join("ui");
            std::fs::create_dir_all(&app).unwrap();
            std::fs::create_dir_all(ui.join("animation")).unwrap();
            std::fs::create_dir_all(ui.join("assets")).unwrap();
            std::fs::create_dir_all(ui.join("chart")).unwrap();
            std::fs::create_dir_all(ui.join("commands")).unwrap();
            std::fs::create_dir_all(ui.join("desktop")).unwrap();
            std::fs::create_dir_all(ui.join("document")).unwrap();
            std::fs::create_dir_all(ui.join("editor")).unwrap();
            std::fs::create_dir_all(ui.join("kit")).unwrap();
            std::fs::create_dir_all(ui.join("kit/components")).unwrap();
            std::fs::create_dir_all(ui.join("kit/data")).unwrap();
            std::fs::create_dir_all(ui.join("kit/headless")).unwrap();
            std::fs::create_dir_all(ui.join("kit/icons")).unwrap();
            std::fs::create_dir_all(ui.join("kit/tokens")).unwrap();
            std::fs::create_dir_all(ui.join("forms")).unwrap();
            std::fs::create_dir_all(ui.join("gesture")).unwrap();
            std::fs::create_dir_all(ui.join("graphics")).unwrap();
            std::fs::create_dir_all(ui.join("i18n/core")).unwrap();
            std::fs::create_dir_all(ui.join("language")).unwrap();
            std::fs::create_dir_all(ui.join("media")).unwrap();
            std::fs::create_dir_all(ui.join("motion")).unwrap();
            std::fs::create_dir_all(ui.join("navigation")).unwrap();
            std::fs::create_dir_all(ui.join("observability")).unwrap();
            std::fs::create_dir_all(ui.join("persistence")).unwrap();
            std::fs::create_dir_all(ui.join("platform")).unwrap();
            std::fs::create_dir_all(ui.join("resource")).unwrap();
            std::fs::create_dir_all(ui.join("system")).unwrap();
            std::fs::create_dir_all(ui.join("task")).unwrap();
            std::fs::create_dir_all(ui.join("testing")).unwrap();
            std::fs::create_dir_all(ui.join("web/server")).unwrap();
            std::fs::create_dir_all(ui.join("workspace")).unwrap();
            std::fs::write(
                root.join("vo.work"),
                "format = 1\nmembers = [\"app\", \"ui\"]\n",
            )
            .unwrap();
            let app_mod = concat!(
                "format = 1\n",
                "module = \"github.com/acme/ui-test\"\n",
                "version = \"0.1.4\"\n",
                "vo = \"0.1.4\"\n",
                "[dependencies]\n",
                "\"github.com/vo-lang/ui\" = \"^0.1.4\"\n",
            );
            let ui_mod = include_str!("../../../../ui/vo.mod");
            std::fs::write(app.join("vo.mod"), app_mod).unwrap();
            std::fs::write(ui.join("vo.mod"), ui_mod).unwrap();
            std::fs::write(ui.join("ui.vo"), include_str!("../../../../ui/ui.vo")).unwrap();
            std::fs::write(
                ui.join("animation/animation.vo"),
                include_str!("../../../../ui/animation/animation.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("assets/assets.vo"),
                include_str!("../../../../ui/assets/assets.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("chart/chart.vo"),
                include_str!("../../../../ui/chart/chart.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("commands/commands.vo"),
                include_str!("../../../../ui/commands/commands.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("desktop/desktop.vo"),
                include_str!("../../../../ui/desktop/desktop.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("document/document.vo"),
                include_str!("../../../../ui/document/document.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("editor/editor.vo"),
                include_str!("../../../../ui/editor/editor.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("kit/kit.vo"),
                include_str!("../../../../ui/kit/kit.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("kit/components/components.vo"),
                include_str!("../../../../ui/kit/components/components.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("kit/data/data.vo"),
                include_str!("../../../../ui/kit/data/data.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("kit/headless/headless.vo"),
                include_str!("../../../../ui/kit/headless/headless.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("kit/icons/icons.vo"),
                include_str!("../../../../ui/kit/icons/icons.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("kit/tokens/tokens.vo"),
                include_str!("../../../../ui/kit/tokens/tokens.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("forms/forms.vo"),
                include_str!("../../../../ui/forms/forms.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("gesture/gesture.vo"),
                include_str!("../../../../ui/gesture/gesture.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("graphics/graphics.vo"),
                include_str!("../../../../ui/graphics/graphics.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("i18n/core/core.vo"),
                include_str!("../../../../ui/i18n/core/core.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("i18n/i18n.vo"),
                include_str!("../../../../ui/i18n/i18n.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("language/language.vo"),
                include_str!("../../../../ui/language/language.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("media/media.vo"),
                include_str!("../../../../ui/media/media.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("motion/motion.vo"),
                include_str!("../../../../ui/motion/motion.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("navigation/navigation.vo"),
                include_str!("../../../../ui/navigation/navigation.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("observability/observability.vo"),
                include_str!("../../../../ui/observability/observability.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("persistence/persistence.vo"),
                include_str!("../../../../ui/persistence/persistence.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("platform/platform.vo"),
                include_str!("../../../../ui/platform/platform.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("resource/resource.vo"),
                include_str!("../../../../ui/resource/resource.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("system/system.vo"),
                include_str!("../../../../ui/system/system.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("task/task.vo"),
                include_str!("../../../../ui/task/task.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("testing/testing.vo"),
                include_str!("../../../../ui/testing/testing.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("web/web.vo"),
                include_str!("../../../../ui/web/web.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("web/server/server.vo"),
                include_str!("../../../../ui/web/server/server.vo"),
            )
            .unwrap();
            std::fs::write(
                ui.join("workspace/workspace.vo"),
                include_str!("../../../../ui/workspace/workspace.vo"),
            )
            .unwrap();
            let root_manifest = vo_module::schema::modfile::ModFile::parse(app_mod).unwrap();
            let ui_manifest = vo_module::schema::modfile::ModFile::parse(ui_mod).unwrap();
            let lock = vo_module::schema::lockfile::LockFile {
                format: vo_module::schema::lockfile::LOCK_FILE_VERSION,
                root: vo_module::lock::module_intent_digest(&root_manifest).unwrap(),
                modules: vec![vo_module::schema::lockfile::LockedModule {
                    path: vo_module::identity::ModulePath::parse("github.com/vo-lang/ui").unwrap(),
                    version: vo_module::version::ExactVersion::parse("0.1.4").unwrap(),
                    origin: vo_module::schema::lockfile::LockOrigin::Workspace,
                    release: None,
                    intent: Some(vo_module::lock::module_intent_digest(&ui_manifest).unwrap()),
                    selection: None,
                }],
            };
            std::fs::write(app.join("vo.lock"), lock.render().unwrap()).unwrap();
            std::fs::write(app.join("main.vo"), source).unwrap();
            Self(root)
        }

        fn create_with_imported_component(app_source: &str, library_source: &str) -> Self {
            let workspace = Self::create_with_main(app_source);
            let root = &workspace.0;
            let app = root.join("app");
            let widgets = root.join("widgets");
            std::fs::create_dir_all(&widgets).unwrap();
            std::fs::write(
                root.join("vo.work"),
                "format = 1\nmembers = [\"app\", \"ui\", \"widgets\"]\n",
            )
            .unwrap();
            let app_mod = concat!(
                "format = 1\n",
                "module = \"github.com/acme/ui-test\"\n",
                "version = \"0.1.4\"\n",
                "vo = \"0.1.4\"\n",
                "[dependencies]\n",
                "\"github.com/acme/widgets\" = \"^0.1.4\"\n",
                "\"github.com/vo-lang/ui\" = \"^0.1.4\"\n",
            );
            let widgets_mod = concat!(
                "format = 1\n",
                "module = \"github.com/acme/widgets\"\n",
                "version = \"0.1.4\"\n",
                "vo = \"0.1.4\"\n",
                "[dependencies]\n",
                "\"github.com/vo-lang/ui\" = \"^0.1.4\"\n",
            );
            let ui_mod = include_str!("../../../../ui/vo.mod");
            std::fs::write(app.join("vo.mod"), app_mod).unwrap();
            std::fs::write(widgets.join("vo.mod"), widgets_mod).unwrap();
            std::fs::write(widgets.join("widgets.vo"), library_source).unwrap();
            let root_manifest = vo_module::schema::modfile::ModFile::parse(app_mod).unwrap();
            let widgets_manifest = vo_module::schema::modfile::ModFile::parse(widgets_mod).unwrap();
            let ui_manifest = vo_module::schema::modfile::ModFile::parse(ui_mod).unwrap();
            let lock = vo_module::schema::lockfile::LockFile {
                format: vo_module::schema::lockfile::LOCK_FILE_VERSION,
                root: vo_module::lock::module_intent_digest(&root_manifest).unwrap(),
                modules: vec![
                    vo_module::schema::lockfile::LockedModule {
                        path: vo_module::identity::ModulePath::parse("github.com/acme/widgets")
                            .unwrap(),
                        version: vo_module::version::ExactVersion::parse("0.1.4").unwrap(),
                        origin: vo_module::schema::lockfile::LockOrigin::Workspace,
                        release: None,
                        intent: Some(
                            vo_module::lock::module_intent_digest(&widgets_manifest).unwrap(),
                        ),
                        selection: None,
                    },
                    vo_module::schema::lockfile::LockedModule {
                        path: vo_module::identity::ModulePath::parse("github.com/vo-lang/ui")
                            .unwrap(),
                        version: vo_module::version::ExactVersion::parse("0.1.4").unwrap(),
                        origin: vo_module::schema::lockfile::LockOrigin::Workspace,
                        release: None,
                        intent: Some(vo_module::lock::module_intent_digest(&ui_manifest).unwrap()),
                        selection: None,
                    },
                ],
            };
            std::fs::write(app.join("vo.lock"), lock.render().unwrap()).unwrap();
            workspace
        }

        fn app(&self) -> std::path::PathBuf {
            self.0.join("app")
        }

        fn compile(&self) -> crate::CompileOutput {
            let workfile = self.0.join("vo.work");
            #[cfg(not(windows))]
            let workfile = workfile.canonicalize().unwrap();
            let options = vo_module::project::ProjectContextOptions::new(
                vo_module::workspace::WorkspaceDiscovery::Explicit(workfile),
            );
            crate::compile_with_options(self.app().to_string_lossy().as_ref(), &options).unwrap()
        }
    }

    impl Drop for UiTestWorkspace {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    fn ui_batches_for(
        module: Arc<vo_runtime::bytecode::LoadedModule>,
        mode: RunMode,
    ) -> (vo_ui_protocol::MutationBatch, vo_ui_protocol::MutationBatch) {
        let (initial, mut updates) = ui_input_batches_for(module, mode, &["after"]);
        (initial, updates.remove(0))
    }

    fn ui_initial_batch_for(
        module: Arc<vo_runtime::bytecode::LoadedModule>,
        mode: RunMode,
    ) -> vo_ui_protocol::MutationBatch {
        let mut vm = match mode {
            RunMode::Vm => Vm::new(),
            RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1,
                debug_ir: false,
                ..vo_vm::JitConfig::default()
            })
            .expect("JIT should initialize"),
        };
        register_ui_externs(&mut vm, &module).unwrap();
        vm.load_verified(module).unwrap();
        assert_eq!(vm.run().unwrap(), SchedulingOutcome::SuspendedForHostEvents);
        vo_ui_protocol::decode_batch(
            &vm.take_host_output()
                .expect("UI Mount should publish its initial mutation batch"),
            vo_ui_protocol::ProtocolLimits::default(),
        )
        .unwrap()
    }

    fn ui_input_batches_for(
        module: Arc<vo_runtime::bytecode::LoadedModule>,
        mode: RunMode,
        values: &[&str],
    ) -> (
        vo_ui_protocol::MutationBatch,
        Vec<vo_ui_protocol::MutationBatch>,
    ) {
        let mut vm = match mode {
            RunMode::Vm => Vm::new(),
            RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1,
                debug_ir: false,
                ..vo_vm::JitConfig::default()
            })
            .expect("JIT should initialize"),
        };
        register_ui_externs(&mut vm, &module).unwrap();
        vm.load_verified(module).unwrap();
        assert_eq!(vm.run().unwrap(), SchedulingOutcome::SuspendedForHostEvents);
        let limits = vo_ui_protocol::ProtocolLimits::default();
        let initial = vo_ui_protocol::decode_batch(
            &vm.take_host_output()
                .expect("UI Mount should publish its initial mutation batch"),
            limits,
        )
        .unwrap();
        let (target, handler) = initial
            .mutations
            .iter()
            .find_map(|mutation| match mutation {
                vo_ui_protocol::Mutation::Listen { id, listener }
                    if listener.event == vo_ui_core::EventType::INPUT =>
                {
                    Some((*id, listener.handler))
                }
                _ => None,
            })
            .expect("fixture should publish a live input listener");
        let mut updates = Vec::with_capacity(values.len());
        for (index, value) in values.iter().enumerate() {
            let event = vo_ui_protocol::EventEnvelope::new(
                initial.session_epoch,
                vo_ui_core::UiEvent {
                    handler,
                    event: vo_ui_core::EventType::INPUT,
                    target,
                    sequence: index as u64 + 1,
                    payload: vo_ui_core::EventPayload::Text((*value).to_string()),
                },
            );
            let event_bytes = vo_ui_protocol::encode_event(&event, limits).unwrap();
            let pending = vm.take_pending_host_events();
            assert_eq!(pending.len(), 1);
            assert!(pending[0].source.is_gui_event_replay());
            assert!(vm.wake_host_event_with_data(pending[0].key, event_bytes));
            assert_eq!(
                vm.run_scheduled().unwrap(),
                SchedulingOutcome::SuspendedForHostEvents
            );
            updates.push(
                vo_ui_protocol::decode_batch(
                    &vm.take_host_output()
                        .expect("UI handler should publish one update mutation batch"),
                    limits,
                )
                .unwrap(),
            );
        }
        (initial, updates)
    }

    fn ui_single_event_batches_for(
        module: Arc<vo_runtime::bytecode::LoadedModule>,
        mode: RunMode,
        event_type: vo_ui_core::EventType,
        payload: vo_ui_core::EventPayload,
    ) -> (vo_ui_protocol::MutationBatch, vo_ui_protocol::MutationBatch) {
        ui_single_event_batches_for_named(module, mode, event_type, payload, None)
    }

    fn ui_single_event_batches_for_named(
        module: Arc<vo_runtime::bytecode::LoadedModule>,
        mode: RunMode,
        event_type: vo_ui_core::EventType,
        payload: vo_ui_core::EventPayload,
        accessible_name: Option<&str>,
    ) -> (vo_ui_protocol::MutationBatch, vo_ui_protocol::MutationBatch) {
        let mut vm = match mode {
            RunMode::Vm => Vm::new(),
            RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1,
                debug_ir: false,
                ..vo_vm::JitConfig::default()
            })
            .expect("JIT should initialize"),
        };
        register_ui_externs(&mut vm, &module).unwrap();
        vm.load_verified(module).unwrap();
        assert_eq!(vm.run().unwrap(), SchedulingOutcome::SuspendedForHostEvents);
        let limits = vo_ui_protocol::ProtocolLimits::default();
        let initial = vo_ui_protocol::decode_batch(
            &vm.take_host_output()
                .expect("UI Mount should publish its initial mutation batch"),
            limits,
        )
        .unwrap();
        vm.gc_collect()
            .expect("mounted UI handler leases should survive a complete GC cycle");
        let named_target = accessible_name.and_then(|expected| {
            initial
                .mutations
                .iter()
                .find_map(|mutation| match mutation {
                    vo_ui_protocol::Mutation::SetProperty { id, property }
                        if property.id == vo_ui_core::PropertyId::ACCESSIBLE_NAME
                            && property.value == vo_ui_core::Value::Text(expected.to_string()) =>
                    {
                        Some(*id)
                    }
                    _ => None,
                })
        });
        let (target, handler) = initial
            .mutations
            .iter()
            .find_map(|mutation| match mutation {
                vo_ui_protocol::Mutation::Listen { id, listener }
                    if listener.event == event_type
                        && named_target.is_none_or(|target| target == *id) =>
                {
                    Some((*id, listener.handler))
                }
                _ => None,
            })
            .expect("fixture should publish the requested listener");
        let event = vo_ui_protocol::EventEnvelope::new(
            initial.session_epoch,
            vo_ui_core::UiEvent {
                handler,
                event: event_type,
                target,
                sequence: 1,
                payload,
            },
        );
        let pending = vm.take_pending_host_events();
        assert_eq!(pending.len(), 1);
        assert!(pending[0].source.is_gui_event_replay());
        assert!(vm.wake_host_event_with_data(
            pending[0].key,
            vo_ui_protocol::encode_event(&event, limits).unwrap()
        ));
        assert_eq!(
            vm.run_scheduled().unwrap(),
            SchedulingOutcome::SuspendedForHostEvents
        );
        let update = vo_ui_protocol::decode_batch(
            &vm.take_host_output()
                .expect("UI handler should publish one update mutation batch"),
            limits,
        )
        .unwrap();
        (initial, update)
    }

    fn click_first_native_button(
        session: &mut crate::NativeUiVmSession,
        window: vo_app_protocol::WindowHandle,
        view: vo_app_protocol::ViewHandle,
        now: std::time::Instant,
    ) -> crate::NativeUiSessionReport {
        let mut measurer = vo_ui_layout::ApproximateTextMeasurer;
        let layout = session
            .renderer_mut()
            .host_mut()
            .compute_and_set_layout(
                vo_ui_layout::Size::new(640.0, 480.0),
                vo_ui_layout::LayoutLimits::default(),
                &mut measurer,
            )
            .unwrap();
        let button = layout
            .iter()
            .find(|layout| {
                session
                    .renderer()
                    .host()
                    .tree()
                    .node(layout.node)
                    .is_some_and(|node| node.listeners.contains_key(&vo_ui_core::EventType::CLICK))
            })
            .unwrap();
        let x_milli = ((button.rect.x + button.rect.width / 2.0) * 1_000.0) as i32;
        let y_milli = ((button.rect.y + button.rect.height / 2.0) * 1_000.0) as i32;
        let input = |sequence, pressed| vo_app_host_native::NativeInputEvent {
            sequence,
            timestamp_micros: sequence,
            window,
            view,
            kind: vo_app_host_native::NativeInputKind::PointerButton {
                device: 1,
                button: vo_app_host_native::NativePointerButton::Primary,
                pressed,
                click_count: 1,
                x_milli,
                y_milli,
            },
        };
        assert!(!session.route_input(&input(1, true)).unwrap());
        assert!(session.route_input(&input(2, false)).unwrap());
        session.pump(now).unwrap()
    }

    fn click_named_native_button(
        session: &mut crate::NativeUiVmSession,
        window: vo_app_protocol::WindowHandle,
        view: vo_app_protocol::ViewHandle,
        name: &str,
        interaction: u64,
        now: std::time::Instant,
    ) -> crate::NativeUiSessionReport {
        let mut measurer = vo_ui_layout::ApproximateTextMeasurer;
        let layout = session
            .renderer_mut()
            .host_mut()
            .compute_and_set_layout(
                vo_ui_layout::Size::new(640.0, 480.0),
                vo_ui_layout::LayoutLimits::default(),
                &mut measurer,
            )
            .unwrap();
        let button = layout
            .iter()
            .find(|layout| {
                session
                    .renderer()
                    .host()
                    .tree()
                    .node(layout.node)
                    .is_some_and(|node| {
                        node.listeners.contains_key(&vo_ui_core::EventType::CLICK)
                            && node
                                .properties
                                .get(&vo_ui_core::PropertyId::ACCESSIBLE_NAME)
                                == Some(&vo_ui_core::Value::Text(name.to_string()))
                    })
            })
            .unwrap_or_else(|| panic!("button {name:?} is missing"));
        let x_milli = ((button.rect.x + button.rect.width / 2.0) * 1_000.0) as i32;
        let y_milli = ((button.rect.y + button.rect.height / 2.0) * 1_000.0) as i32;
        let first_sequence = interaction.checked_mul(2).unwrap().saturating_sub(1);
        let input = |sequence, pressed| vo_app_host_native::NativeInputEvent {
            sequence,
            timestamp_micros: sequence,
            window,
            view,
            kind: vo_app_host_native::NativeInputKind::PointerButton {
                device: 1,
                button: vo_app_host_native::NativePointerButton::Primary,
                pressed,
                click_count: 1,
                x_milli,
                y_milli,
            },
        };
        assert!(!session.route_input(&input(first_sequence, true)).unwrap());
        assert!(session
            .route_input(&input(first_sequence + 1, false))
            .unwrap());
        session.pump(now).unwrap()
    }

    #[test]
    fn component_bundle_mounts_nested_instances_in_vm_and_jit() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui"
func Label(label string) ui.View { return ui.Text(label) }
func App() ui.View {
	label := "linked"
	return ui.Column(Label(label), ui.Key(Label("fixed"), "fixed-key"))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        assert!(compiled
            .module
            .module()
            .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
            .is_none());
        assert!(compiled
            .module
            .module()
            .artifact(vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME)
            .is_some());
        for mode in [RunMode::Vm, RunMode::Jit] {
            let batch = ui_initial_batch_for(compiled.module.clone(), mode);
            for expected in ["linked", "fixed"] {
                assert!(batch.mutations.iter().any(|mutation| {
                    matches!(
                        mutation,
                        vo_ui_protocol::Mutation::SetText { text, .. } if text == expected
                    )
                }));
            }
        }
    }

    #[test]
    fn component_bundle_keeps_nested_state_and_handlers_instance_local() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func Counter(label string) ui.View {
	count := int64(0)
	return ui.Button(label+" "+strconv.FormatInt(count, 10), func(event ui.Event) {
		count++
	})
}
func App() ui.View {
	return ui.Column(Counter("A"), Counter("B"))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        assert!(compiled
            .module
            .module()
            .artifact(vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME)
            .is_some());
        for mode in [RunMode::Vm, RunMode::Jit] {
            let (initial, update) = ui_single_event_batches_for_named(
                compiled.module.clone(),
                mode,
                vo_ui_core::EventType::CLICK,
                vo_ui_core::EventPayload::None,
                Some("A 0"),
            );
            for expected in ["A 0", "B 0"] {
                assert!(initial.mutations.iter().any(|mutation| matches!(
                    mutation,
                    vo_ui_protocol::Mutation::SetText { text, .. } if text == expected
                )));
            }
            assert_eq!(update.revision, 2);
            assert!(
                update.mutations.iter().any(|mutation| matches!(
                    mutation,
                    vo_ui_protocol::Mutation::SetText { text, .. } if text == "A 1"
                )),
                "unexpected component update: {:?}",
                update.mutations
            );
            assert!(!update.mutations.iter().any(|mutation| matches!(
                mutation,
                vo_ui_protocol::Mutation::SetText { text, .. } if text == "B 1"
            )));
            assert!(update.mutations.iter().all(|mutation| matches!(
                mutation,
                vo_ui_protocol::Mutation::SetText { .. }
                    | vo_ui_protocol::Mutation::SetProperty { .. }
            )));
        }
    }

    #[test]
    fn imported_component_bundle_executes_state_and_handlers_in_vm_and_jit() {
        let workspace = UiTestWorkspace::create_with_imported_component(
            r#"
package main
import (
	"github.com/acme/widgets"
	"github.com/vo-lang/ui"
)
func App() ui.View {
	return ui.Column(widgets.Counter("Imported"), widgets.Counter("Second"))
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
            r#"
package widgets
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func Counter(label string) ui.View {
	count := int64(0)
	return ui.Button(label+" "+strconv.FormatInt(count, 10), func(event ui.Event) {
		count++
	})
}
"#,
        );
        let compiled = workspace.compile();
        let artifact = compiled
            .module
            .module()
            .artifact(vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME)
            .expect("imported component bundle");
        let bundle = vo_ui_artifact::decode_component_bundle(
            &artifact.payload,
            vo_ui_artifact::BundleLimits::default(),
            vo_ui_plan::PlanLimits::default(),
        )
        .unwrap();
        assert!(bundle.definitions.iter().any(|definition| {
            definition.type_id.module() == "github.com/acme/widgets"
                && definition.type_id.object() == "Counter"
        }));
        for mode in [RunMode::Vm, RunMode::Jit] {
            let (_, update) = ui_single_event_batches_for_named(
                compiled.module.clone(),
                mode,
                vo_ui_core::EventType::CLICK,
                vo_ui_core::EventPayload::None,
                Some("Imported 0"),
            );
            assert!(update.mutations.iter().any(|mutation| matches!(
                mutation,
                vo_ui_protocol::Mutation::SetText { text, .. } if text == "Imported 1"
            )));
            assert!(!update.mutations.iter().any(|mutation| matches!(
                mutation,
                vo_ui_protocol::Mutation::SetText { text, .. } if text == "Second 1"
            )));
        }
    }

    #[test]
    fn component_scopes_preserve_dynamic_keyed_state_in_vm_and_jit() {
        let workspace = UiTestWorkspace::create_with_imported_component(
            r#"
package main
import (
	"github.com/acme/widgets"
	"github.com/vo-lang/ui"
)
func App() ui.View {
	phase := int64(0)
	advance := func(event ui.Event) { phase++ }
	if phase == 0 { return ui.Column(ui.Button("Reorder", advance), ui.Key(widgets.Counter("Alpha"), "alpha"), ui.Key(widgets.Counter("Beta"), "beta")) }
	if phase == 1 { return ui.Column(ui.Button("Remove Beta", advance), ui.Key(widgets.Counter("Beta"), "beta"), ui.Key(widgets.Counter("Alpha"), "alpha")) }
	if phase == 2 { return ui.Column(ui.Button("Insert Beta", advance), ui.Key(widgets.Counter("Alpha"), "alpha")) }
	if phase == 3 { return ui.Column(ui.Button("Replace Alpha", advance), ui.Key(widgets.Counter("Alpha"), "alpha"), ui.Key(widgets.Counter("Beta"), "beta")) }
	return ui.Column(ui.Button("Complete", func(event ui.Event) {}), ui.Key(widgets.Counter("Alpha"), "alpha-v2"), ui.Key(widgets.Counter("Beta"), "beta"))
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
            r#"
package widgets
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func Counter(label string) ui.View {
	count := int64(0)
	return ui.Button(label+" "+strconv.FormatInt(count, 10), func(event ui.Event) { count++ })
}
"#,
        );
        let compiled = workspace.compile();
        assert!(compiled
            .module
            .module()
            .artifact(vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME)
            .is_none());
        for mode in [RunMode::Vm, RunMode::Jit] {
            let vm = build_native_gui_vm_for_mode(compiled.clone(), mode).unwrap();
            let window = vo_app_protocol::GenerationalHandle {
                index: 1,
                generation: 1,
            };
            let view = vo_app_protocol::GenerationalHandle {
                index: 2,
                generation: 1,
            };
            let now = std::time::Instant::now();
            let (mut session, _) = crate::NativeUiVmSession::start(
                vm,
                window,
                view,
                crate::NativeUiSessionConfig::default(),
                now,
            )
            .unwrap();
            let has_text = |session: &crate::NativeUiVmSession, expected: &str| {
                session
                    .renderer()
                    .host()
                    .tree()
                    .nodes()
                    .any(|node| node.text == expected)
            };
            assert!(has_text(&session, "Alpha 0") && has_text(&session, "Beta 0"));

            click_named_native_button(&mut session, window, view, "Alpha 0", 1, now);
            assert!(has_text(&session, "Alpha 1") && has_text(&session, "Beta 0"));

            click_named_native_button(&mut session, window, view, "Reorder", 2, now);
            assert!(has_text(&session, "Alpha 1") && has_text(&session, "Beta 0"));

            click_named_native_button(&mut session, window, view, "Remove Beta", 3, now);
            assert!(has_text(&session, "Alpha 1") && !has_text(&session, "Beta 0"));

            click_named_native_button(&mut session, window, view, "Insert Beta", 4, now);
            assert!(has_text(&session, "Alpha 1") && has_text(&session, "Beta 0"));

            click_named_native_button(&mut session, window, view, "Replace Alpha", 5, now);
            assert!(has_text(&session, "Alpha 0") && has_text(&session, "Beta 0"));
            assert!(!has_text(&session, "Alpha 1"));
        }
    }

    #[test]
    fn official_ui_mount_is_vm_jit_protocol_equivalent() {
        let workspace = UiTestWorkspace::create();
        let compiled = workspace.compile();
        let (vm_initial, vm_update) = ui_batches_for(compiled.module.clone(), RunMode::Vm);
        let (jit_initial, jit_update) = ui_batches_for(compiled.module, RunMode::Jit);

        assert_eq!(vm_initial.revision, 1);
        assert_eq!(vm_initial.mutations, jit_initial.mutations);
        assert!(vm_initial.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { text, .. } if text == "before"
        )));
        assert_eq!(vm_update.revision, 2);
        assert_eq!(vm_update.mutations, jit_update.mutations);
        assert_eq!(vm_update.mutations.len(), 2);
        assert!(vm_update.mutations.iter().all(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { .. } | vo_ui_protocol::Mutation::SetProperty { .. }
        )));
        assert!(vm_update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { text, .. } if text == "after"
        )));
    }

    #[test]
    fn official_ui_native_vm_jit_session_routes_clicks_through_desktop_host() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func App() ui.View {
	count := int64(0)
	return ui.Button("Count " + strconv.FormatInt(count, 10), func(event ui.Event) {
		count++
	})
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        for mode in [RunMode::Vm, RunMode::Jit] {
            let compiled = workspace.compile();
            let vm = build_native_gui_vm_for_mode(compiled, mode).unwrap();
            let window = vo_app_protocol::GenerationalHandle {
                index: 1,
                generation: 1,
            };
            let view = vo_app_protocol::GenerationalHandle {
                index: 2,
                generation: 1,
            };
            let now = std::time::Instant::now();
            let (mut session, started) = crate::NativeUiVmSession::start(
                vm,
                window,
                view,
                crate::NativeUiSessionConfig::default(),
                now,
            )
            .unwrap();
            assert_eq!(started.revision, 1);
            let update = click_first_native_button(&mut session, window, view, now);
            assert_eq!(update.revision, 2);
            assert_eq!(update.delivered_events, 1);
            assert_eq!(update.applied_frames, 1);
            assert!(session
                .renderer()
                .host()
                .tree()
                .nodes()
                .any(|node| node.text == "Count 1"));
        }
    }

    #[test]
    fn official_uikit_accessibility_and_paint_goldens_match_vm_and_jit() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import (
	"github.com/vo-lang/ui"
	"github.com/vo-lang/ui/kit"
	"github.com/vo-lang/ui/motion"
)
func App() ui.View {
	theme := kit.LightTheme()
	progress := motion.UseValue(42)
	return kit.Screen(theme, kit.Card(theme, ui.Gap(ui.Column(
		kit.Heading(theme, "Account"),
		kit.ValidatedTextField(theme, "Display name", "Ada", "Your name", true, false, "Name is required", func(event ui.Event) {}),
		kit.ToggleField(theme, "Email updates", true, false, func(event ui.Event) {}),
		kit.MotionProgress(theme, progress, 100),
		kit.FormActions(theme, kit.PrimaryButton(theme, "Save", false, func(event ui.Event) {})),
	), theme.Space * 2)))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        let mut snapshots = Vec::new();
        for mode in [RunMode::Vm, RunMode::Jit] {
            let vm = build_native_gui_vm_for_mode(compiled.clone(), mode).unwrap();
            let window = vo_app_protocol::GenerationalHandle {
                index: 1,
                generation: 1,
            };
            let view = vo_app_protocol::GenerationalHandle {
                index: 2,
                generation: 1,
            };
            let (session, _) = crate::NativeUiVmSession::start(
                vm,
                window,
                view,
                crate::NativeUiSessionConfig::default(),
                std::time::Instant::now(),
            )
            .unwrap();
            let host = session.renderer().host();
            let mut measurer = vo_ui_layout::ApproximateTextMeasurer;
            let layout = vo_ui_layout::compute_layout(
                host.tree(),
                vo_ui_layout::Size::new(760.0, 520.0),
                vo_ui_layout::LayoutLimits::default(),
                &mut measurer,
            )
            .unwrap();
            let accessibility = host
                .build_accessibility_tree(
                    &layout,
                    vo_ui_accessibility::AccessibilityLimits::default(),
                )
                .unwrap();
            let paint = host
                .build_paint_scene(&layout, vo_ui_paint::PaintLimits::default())
                .unwrap();
            snapshots.push((
                vo_ui_golden::accessibility_snapshot(&accessibility),
                vo_ui_golden::paint_snapshot(&paint),
            ));
        }
        assert_eq!(snapshots[0], snapshots[1]);
        assert_eq!(
            snapshots[0].0,
            include_str!("../../../../ui/testdata/goldens/uikit.accessibility.txt")
        );
        assert_eq!(
            snapshots[0].1,
            include_str!("../../../../ui/testdata/goldens/uikit.paint.txt")
        );
    }

    #[test]
    fn official_uikit_portable_menu_is_vm_jit_protocol_equivalent() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import (
	"github.com/vo-lang/ui"
	"github.com/vo-lang/ui/kit"
)
func App() ui.View {
	theme := kit.LightTheme()
	status := "Ready"
	return ui.Column(
		kit.MenuBar(theme, "Application menu",
			kit.MenuAction(theme, "New", false, func(event ui.Event) { status = "Created" }),
			kit.MenuToggleAction(theme, "Auto save", true, false, func(event ui.Event) {}),
		),
		kit.Body(theme, status),
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        let (vm_initial, vm_update) = ui_single_event_batches_for(
            compiled.module.clone(),
            RunMode::Vm,
            vo_ui_core::EventType::CLICK,
            vo_ui_core::EventPayload::None,
        );
        let (jit_initial, jit_update) = ui_single_event_batches_for(
            compiled.module,
            RunMode::Jit,
            vo_ui_core::EventType::CLICK,
            vo_ui_core::EventPayload::None,
        );
        assert_eq!(vm_initial.mutations, jit_initial.mutations);
        assert_eq!(vm_update.mutations, jit_update.mutations);
        for expected in ["menubar", "menuitem", "menuitemcheckbox"] {
            assert!(vm_initial.mutations.iter().any(|mutation| matches!(
                mutation,
                vo_ui_protocol::Mutation::SetProperty { property, .. }
                    if property.id == vo_ui_core::PropertyId::ROLE
                        && property.value == vo_ui_core::Value::Text(expected.to_string())
            )));
        }
        assert!(vm_update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { text, .. } if text == "Created"
        )));
    }

    #[test]
    fn official_ui_system_request_suspends_one_goroutine_and_commits_response() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import (
	"github.com/vo-lang/ui"
	uisystem "github.com/vo-lang/ui/system"
)
func App() ui.View {
	status := ui.UseStringState("waiting")
	started := ui.UseBoolState(false)
	if !ui.BoolStateValue(started) {
		ui.SetBoolState(started, true)
		go func() {
			value, present, err := uisystem.ReadClipboardText()
			if err != nil { ui.SetStringState(status, err.Error())
			} else if present { ui.SetStringState(status, value)
			} else { ui.SetStringState(status, "empty") }
			ui.Invalidate()
		}()
	}
	return ui.Text(ui.StringStateValue(status))
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
        );
        for mode in [RunMode::Vm, RunMode::Jit] {
            let compiled = workspace.compile();
            let vm = build_native_gui_vm_for_mode(compiled, mode).unwrap();
            let window = vo_app_protocol::GenerationalHandle {
                index: 1,
                generation: 1,
            };
            let view = vo_app_protocol::GenerationalHandle {
                index: 2,
                generation: 1,
            };
            let now = std::time::Instant::now();
            let (mut session, started) = crate::NativeUiVmSession::start(
                vm,
                window,
                view,
                crate::NativeUiSessionConfig::default(),
                now,
            )
            .unwrap();
            assert_eq!(started.revision, 1);
            assert!(session
                .renderer()
                .host()
                .tree()
                .nodes()
                .any(|node| node.text == "waiting"));

            let requests = session.take_system_requests().unwrap();
            assert_eq!(requests.len(), 1);
            let decoded = vo_ui_system::decode_system_request(
                &requests[0].frame,
                vo_ui_system::SystemLimits::default(),
            )
            .unwrap();
            assert!(matches!(
                decoded.request,
                vo_ui_system::SystemRequest::ReadClipboard(vo_ui_system::ClipboardFormat::Text)
            ));
            let response = vo_ui_system::encode_system_response(
                &vo_ui_system::SystemResponseEnvelope {
                    request_id: requests[0].request_id,
                    response: vo_ui_system::SystemResponse::Clipboard(Some(
                        vo_ui_system::ClipboardContent::Text("copied".to_string()),
                    )),
                },
                vo_ui_system::SystemLimits::default(),
            )
            .unwrap();
            let report = session
                .complete_system_request(&requests[0], response, now)
                .unwrap();
            assert_eq!(report.completed_system_requests, 1);
            assert_eq!(report.revision, 2);
            assert!(session
                .renderer()
                .host()
                .tree()
                .nodes()
                .any(|node| node.text == "copied"));
        }
    }

    #[test]
    fn official_ui_file_drag_suspends_one_goroutine_across_vm_and_jit() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import (
	"github.com/vo-lang/ui"
	uisystem "github.com/vo-lang/ui/system"
)
func App() ui.View {
	status := ui.UseStringState("waiting")
	started := ui.UseBoolState(false)
	if !ui.BoolStateValue(started) {
		ui.SetBoolState(started, true)
		go func() {
			err := uisystem.BeginFileDrag(
				[]string{"/tmp/alpha.vo", "/tmp/beta.vo"},
				uisystem.FileDragOptions{Mode: uisystem.FileDragMove, Preview: "/tmp/preview.png"},
			)
			if err != nil { ui.SetStringState(status, err.Error())
			} else { ui.SetStringState(status, "dragging") }
			ui.Invalidate()
		}()
	}
	return ui.Text(ui.StringStateValue(status))
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
        );
        for mode in [RunMode::Vm, RunMode::Jit] {
            let compiled = workspace.compile();
            let vm = build_native_gui_vm_for_mode(compiled, mode).unwrap();
            let window = vo_app_protocol::GenerationalHandle {
                index: 1,
                generation: 1,
            };
            let view = vo_app_protocol::GenerationalHandle {
                index: 2,
                generation: 1,
            };
            let now = std::time::Instant::now();
            let (mut session, started) = crate::NativeUiVmSession::start(
                vm,
                window,
                view,
                crate::NativeUiSessionConfig::default(),
                now,
            )
            .unwrap();
            assert_eq!(started.revision, 1);
            assert!(session
                .renderer()
                .host()
                .tree()
                .nodes()
                .any(|node| node.text == "waiting"));

            let requests = session.take_system_requests().unwrap();
            assert_eq!(requests.len(), 1);
            let decoded = vo_ui_system::decode_system_request(
                &requests[0].frame,
                vo_ui_system::SystemLimits::default(),
            )
            .unwrap();
            let vo_ui_system::SystemRequest::BeginFileDrag(request) = decoded.request else {
                panic!("expected native file drag request");
            };
            assert_eq!(request.paths, ["/tmp/alpha.vo", "/tmp/beta.vo"]);
            assert_eq!(request.preview.as_deref(), Some("/tmp/preview.png"));
            assert_eq!(request.mode, vo_ui_system::FileDragMode::Move);

            let response = vo_ui_system::encode_system_response(
                &vo_ui_system::SystemResponseEnvelope {
                    request_id: requests[0].request_id,
                    response: vo_ui_system::SystemResponse::Complete,
                },
                vo_ui_system::SystemLimits::default(),
            )
            .unwrap();
            let report = session
                .complete_system_request(&requests[0], response, now)
                .unwrap();
            assert_eq!(report.completed_system_requests, 1);
            assert_eq!(report.revision, 2);
            assert!(session
                .renderer()
                .host()
                .tree()
                .nodes()
                .any(|node| node.text == "dragging"));
        }
    }

    #[test]
    fn official_ui_native_vm_jit_reload_is_stateful_and_transactional() {
        const INITIAL: &str = r#"
package main
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func Counter(label string) ui.View {
	count := int64(0)
	return ui.Button(label+" "+strconv.FormatInt(count, 10), func(event ui.Event) { count++ })
}
func App() ui.View { return ui.Column(ui.Key(Counter("Count"), "counter")) }
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#;
        const UPDATED: &str = r#"
package main
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func Counter(label string) ui.View {
	count := int64(0)
	return ui.Button(label+" "+strconv.FormatInt(count, 10), func(event ui.Event) { count++ })
}
func updatedLabel() string { return "Value" }
func App() ui.View { return ui.Column(ui.Key(Counter(updatedLabel()), "counter")) }
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#;
        const PANICKING: &str = r#"
package main
import "github.com/vo-lang/ui"
func App() ui.View {
	panic("candidate failed")
	return ui.Text("unreachable")
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#;
        let workspace = UiTestWorkspace::create_with_main(INITIAL);
        for mode in [RunMode::Vm, RunMode::Jit] {
            std::fs::write(workspace.app().join("main.vo"), INITIAL).unwrap();
            let compiled = workspace.compile();
            let vm = build_native_gui_vm_for_mode(compiled, mode).unwrap();
            let window = vo_app_protocol::GenerationalHandle {
                index: 1,
                generation: 1,
            };
            let view = vo_app_protocol::GenerationalHandle {
                index: 2,
                generation: 1,
            };
            let now = std::time::Instant::now();
            let (mut session, _) = crate::NativeUiVmSession::start(
                vm,
                window,
                view,
                crate::NativeUiSessionConfig::default(),
                now,
            )
            .unwrap();
            assert_eq!(
                click_first_native_button(&mut session, window, view, now).revision,
                2
            );

            std::fs::write(workspace.app().join("main.vo"), UPDATED).unwrap();
            let updated = workspace.compile();
            let prepared = prepare_native_gui_reload_for_mode(updated, mode).unwrap();
            let previous_epoch = session.renderer().host().session_epoch();
            let reloaded = session.reload(prepared, now).unwrap();
            assert_eq!(reloaded.revision, 1);
            assert!(session.renderer().host().session_epoch() > previous_epoch);
            assert!(session
                .renderer()
                .host()
                .tree()
                .nodes()
                .any(|node| node.text == "Value 1"));

            std::fs::write(workspace.app().join("main.vo"), PANICKING).unwrap();
            let panicking = workspace.compile();
            let prepared = prepare_native_gui_reload_for_mode(panicking, mode).unwrap();
            let failed = session.reload(prepared, now);
            assert!(
                matches!(
                    failed,
                    Err(crate::NativeUiSessionError::Vm(_))
                        | Err(crate::NativeUiSessionError::Terminal(
                            SchedulingOutcome::Panicked
                        ))
                ),
                "unexpected reload result: {failed:?}"
            );
            assert!(session
                .renderer()
                .host()
                .tree()
                .nodes()
                .any(|node| node.text == "Value 1"));
            assert_eq!(
                click_first_native_button(&mut session, window, view, now).revision,
                2
            );
            assert!(session
                .renderer()
                .host()
                .tree()
                .nodes()
                .any(|node| node.text == "Value 2"));
        }
    }

    #[test]
    fn official_ui_native_vm_jit_session_commits_goroutine_invalidation() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func App() ui.View {
	count := ui.UseIntState(0)
	started := ui.UseBoolState(false)
	if !ui.BoolStateValue(started) {
		ui.SetBoolState(started, true)
		go func() {
			ui.SetIntState(count, ui.IntStateValue(count)+1)
			ui.Invalidate()
		}()
	}
	return ui.Text("Count " + strconv.FormatInt(ui.IntStateValue(count), 10))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        for mode in [RunMode::Vm, RunMode::Jit] {
            let compiled = workspace.compile();
            let vm = build_native_gui_vm_for_mode(compiled, mode).unwrap();
            let window = vo_app_protocol::GenerationalHandle {
                index: 1,
                generation: 1,
            };
            let view = vo_app_protocol::GenerationalHandle {
                index: 2,
                generation: 1,
            };
            let now = std::time::Instant::now();
            let (session, started) = crate::NativeUiVmSession::start(
                vm,
                window,
                view,
                crate::NativeUiSessionConfig::default(),
                now,
            )
            .unwrap();
            assert_eq!(started.pending_timers, 0);
            assert_eq!(started.woken_timers, 0);
            assert_eq!(started.delivered_events, 1);
            assert_eq!(started.applied_frames, 2);
            assert_eq!(started.revision, 2);
            assert!(session
                .renderer()
                .host()
                .tree()
                .nodes()
                .any(|node| node.text == "Count 1"));
        }
    }

    #[test]
    fn official_motion_tween_advances_on_host_timers_in_vm_and_jit() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import (
	"github.com/vo-lang/ui"
	"github.com/vo-lang/ui/motion"
	"time"
)
func App() ui.View {
	value := motion.UseValue(0)
	status := "idle"
	if value.IsRunning() { status = "running" }
	return ui.Column(ui.Text(status), ui.Padding(ui.Button("Animate", func(event ui.Event) {
		if value.IsRunning() { value.Stop() } else {
			value.AnimateTo(80, motion.Tween{Duration: 100 * time.Millisecond, Curve: motion.Linear})
		}
	}), value.Current()))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        let mut final_values = Vec::new();
        for mode in [RunMode::Vm, RunMode::Jit] {
            let mut vm = build_native_gui_vm_for_mode(compiled.clone(), mode).unwrap();
            let clock = vo_runtime::io::ManualClock::new(1_700_000_000_000_000_000);
            vm.set_manual_clock(clock.clone()).unwrap();
            let window = vo_app_protocol::GenerationalHandle {
                index: 1,
                generation: 1,
            };
            let view = vo_app_protocol::GenerationalHandle {
                index: 2,
                generation: 1,
            };
            let started = std::time::Instant::now();
            let (mut session, _) = crate::NativeUiVmSession::start(
                vm,
                window,
                view,
                crate::NativeUiSessionConfig::default(),
                started,
            )
            .unwrap();
            let clicked = click_first_native_button(&mut session, window, view, started);
            assert_eq!(clicked.revision, 2);
            let padding = |session: &crate::NativeUiVmSession| {
                session
                    .renderer()
                    .host()
                    .tree()
                    .nodes()
                    .find_map(|node| {
                        node.properties
                            .get(&vo_ui_core::PropertyId::PADDING)
                            .cloned()
                    })
                    .expect("animated padding")
            };
            clock.advance(std::time::Duration::from_millis(20)).unwrap();
            session
                .pump(started + std::time::Duration::from_millis(20))
                .unwrap();
            let intermediate = padding(&session);
            assert!(
                matches!(intermediate, vo_ui_core::Value::Length(vo_ui_core::Length::Px(value)) if value > 0.0 && value < 80.0)
            );
            click_first_native_button(
                &mut session,
                window,
                view,
                started + std::time::Duration::from_millis(20),
            );
            clock
                .advance(std::time::Duration::from_millis(100))
                .unwrap();
            session
                .pump(started + std::time::Duration::from_millis(120))
                .unwrap();
            assert_eq!(
                padding(&session),
                intermediate,
                "cancelled animation must retain its last value"
            );
            click_first_native_button(
                &mut session,
                window,
                view,
                started + std::time::Duration::from_millis(120),
            );
            let mut animation_frames = 0;
            for tick in 1..=12 {
                clock.advance(std::time::Duration::from_millis(20)).unwrap();
                let report = session
                    .pump(started + std::time::Duration::from_millis(120 + tick * 20))
                    .unwrap();
                animation_frames += report.applied_frames;
            }
            assert!(
                animation_frames >= 2,
                "motion worker should publish multiple coalesced frames; got {animation_frames}"
            );
            let value = session
                .renderer()
                .host()
                .tree()
                .nodes()
                .find_map(|node| {
                    node.properties
                        .get(&vo_ui_core::PropertyId::PADDING)
                        .cloned()
                })
                .expect("animated padding property");
            assert_eq!(
                value,
                vo_ui_core::Value::Length(vo_ui_core::Length::Px(80.0))
            );
            if mode == RunMode::Jit {
                assert!(
                    session.vm().jit_execution_stats().function_entries > 0,
                    "JIT must enter compiled code"
                );
            }
            final_values.push(value);
        }
        assert_eq!(final_values[0], final_values[1]);
    }

    #[test]
    fn official_ui_state_cells_drive_only_dependent_direct_slots() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui"
func App() ui.View {
	name := ui.UseStringState("before")
	locked := ui.UseBoolState(false)
	return ui.Column(
		ui.TextInput(ui.StringStateValue(name), "Type", func(event ui.Event) {
			ui.SetStringState(name, event.Text)
		}),
		ui.Disabled(ui.Button("Locked", func(event ui.Event) {}), ui.BoolStateValue(locked)),
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        let artifact = compiled
            .module
            .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
            .expect("static UI component artifact");
        let component = vo_ui_artifact::decode_component_artifact(
            &artifact.payload,
            vo_ui_artifact::ArtifactLimits::default(),
            vo_ui_plan::PlanLimits::default(),
        )
        .unwrap();
        assert_eq!(component.mode, vo_ui_artifact::ExecutionMode::Direct);
        assert_eq!(component.states.len(), 2);
        assert!(component
            .states
            .iter()
            .all(|state| state.initializer_func.is_some()));
        assert!(component
            .slots
            .iter()
            .all(|binding| binding.evaluator_func.is_some()));

        let (vm_initial, vm_updates) =
            ui_input_batches_for(compiled.module.clone(), RunMode::Vm, &["after", "again"]);
        let vm_profile = vo_ui_vm::reactive_profile();
        let (jit_initial, jit_updates) =
            ui_input_batches_for(compiled.module, RunMode::Jit, &["after", "again"]);
        let jit_profile = vo_ui_vm::reactive_profile();
        assert_eq!(vm_initial.mutations, jit_initial.mutations);
        assert_eq!(vm_updates.len(), jit_updates.len());
        assert_eq!(vm_profile, jit_profile);
        assert_eq!(vm_profile.changed_state_writes, 2);
        assert_eq!(vm_profile.root_evaluations, 1);
        assert_eq!(vm_profile.direct_update_turns, 2);
        assert_eq!(vm_profile.scheduled_bindings, 2);
        assert_eq!(vm_profile.evaluator_calls, 2);
        assert_eq!(vm_profile.submitted_slots, 2);
        assert_eq!(vm_profile.emitted_revisions, 3);
        assert_eq!(vm_profile.no_op_updates, 0);
        assert_eq!(
            vm_profile.emitted_mutations,
            (vm_initial.mutations.len()
                + vm_updates
                    .iter()
                    .map(|batch| batch.mutations.len())
                    .sum::<usize>()) as u64
        );
        for (vm, jit) in vm_updates.iter().zip(&jit_updates) {
            assert_eq!(vm.revision, jit.revision);
            assert_eq!(vm.mutations, jit.mutations);
        }
        assert_eq!(vm_updates[0].mutations.len(), 1);
        assert!(matches!(
            &vm_updates[0].mutations[0],
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::VALUE
                    && property.value == vo_ui_core::Value::Text("after".to_string())
        ));
        assert_eq!(vm_updates[1].revision, 3);
        assert!(matches!(
            &vm_updates[1].mutations[0],
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::VALUE
                    && property.value == vo_ui_core::Value::Text("again".to_string())
        ));
    }

    #[test]
    fn official_ui_ordinary_local_state_is_automatically_persistent() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui"
import "strconv"
func App() ui.View {
	count := int64(0)
	return ui.Button("Count "+strconv.FormatInt(count, 10), func(event ui.Event) {
		count++
	})
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        let artifact = compiled
            .module
            .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
            .expect("automatic local state should retain a direct component artifact");
        let component = vo_ui_artifact::decode_component_artifact(
            &artifact.payload,
            vo_ui_artifact::ArtifactLimits::default(),
            vo_ui_plan::PlanLimits::default(),
        )
        .unwrap();
        assert_eq!(component.mode, vo_ui_artifact::ExecutionMode::Direct);
        assert_eq!(component.states.len(), 1);

        let (vm_initial, vm_update) = ui_single_event_batches_for(
            compiled.module.clone(),
            RunMode::Vm,
            vo_ui_core::EventType::CLICK,
            vo_ui_core::EventPayload::None,
        );
        let (jit_initial, jit_update) = ui_single_event_batches_for(
            compiled.module,
            RunMode::Jit,
            vo_ui_core::EventType::CLICK,
            vo_ui_core::EventPayload::None,
        );
        assert_eq!(vm_initial.mutations, jit_initial.mutations);
        assert_eq!(vm_update.mutations, jit_update.mutations);
        assert!(vm_initial.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { text, .. } if text == "Count 0"
        )));
        assert!(vm_update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { text, .. } if text == "Count 1"
        )));
        assert!(vm_update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::ACCESSIBLE_NAME
                    && property.value == vo_ui_core::Value::Text("Count 1".to_string())
        )));
    }

    #[test]
    fn official_ui_automatic_cells_cover_all_scalar_state_kinds() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui"
import "strconv"
func App() ui.View {
	name := "before"
	locked := false
	count := int64(0)
	width := 1.5
	return ui.Column(
		ui.TextInput(name, "Name", func(event ui.Event) {}),
		ui.Disabled(ui.Button(strconv.FormatInt(count, 10), func(event ui.Event) {
			name = "after"
			locked = true
			count += 2
			width *= 2
		}), locked),
		ui.Width(ui.Box(), width),
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        let artifact = compiled
            .module
            .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
            .unwrap();
        let component = vo_ui_artifact::decode_component_artifact(
            &artifact.payload,
            vo_ui_artifact::ArtifactLimits::default(),
            vo_ui_plan::PlanLimits::default(),
        )
        .unwrap();
        assert_eq!(component.mode, vo_ui_artifact::ExecutionMode::Direct);
        assert_eq!(component.states.len(), 4);

        let (vm_initial, vm_update) = ui_single_event_batches_for(
            compiled.module.clone(),
            RunMode::Vm,
            vo_ui_core::EventType::CLICK,
            vo_ui_core::EventPayload::None,
        );
        let (jit_initial, jit_update) = ui_single_event_batches_for(
            compiled.module,
            RunMode::Jit,
            vo_ui_core::EventType::CLICK,
            vo_ui_core::EventPayload::None,
        );
        assert_eq!(vm_initial.mutations, jit_initial.mutations);
        assert_eq!(vm_update.mutations, jit_update.mutations);
        for (property_id, expected) in [
            (
                vo_ui_core::PropertyId::VALUE,
                vo_ui_core::Value::Text("after".to_string()),
            ),
            (
                vo_ui_core::PropertyId::DISABLED,
                vo_ui_core::Value::Bool(true),
            ),
            (
                vo_ui_core::PropertyId::WIDTH,
                vo_ui_core::Value::Length(vo_ui_core::Length::Px(3.0)),
            ),
        ] {
            assert!(vm_update.mutations.iter().any(|mutation| matches!(
                mutation,
                vo_ui_protocol::Mutation::SetProperty { property, .. }
                    if property.id == property_id && property.value == expected
            )));
        }
        assert!(vm_update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { text, .. } if text == "2"
        )));
    }

    #[test]
    fn official_ui_automatic_local_state_survives_library_fallback() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui"
import "strconv"
func Card(child ui.View) ui.View {
	return ui.Padding(ui.Background(child, 0xffffffff), 12)
}
func App() ui.View {
	count := int64(0)
	return Card(ui.Button("Count "+strconv.FormatInt(count, 10), func(event ui.Event) {
		count++
	}))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        assert!(compiled
            .module
            .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
            .is_none());
        let (vm_initial, vm_update) = ui_single_event_batches_for(
            compiled.module.clone(),
            RunMode::Vm,
            vo_ui_core::EventType::CLICK,
            vo_ui_core::EventPayload::None,
        );
        let (jit_initial, jit_update) = ui_single_event_batches_for(
            compiled.module,
            RunMode::Jit,
            vo_ui_core::EventType::CLICK,
            vo_ui_core::EventPayload::None,
        );
        assert_eq!(vm_initial.mutations, jit_initial.mutations);
        assert_eq!(vm_update.mutations, jit_update.mutations);
        assert!(vm_update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { text, .. } if text == "Count 1"
        )));
    }

    #[test]
    fn official_ui_and_system_externs_lower_to_native_and_core_wasm_aot() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import (
	"github.com/vo-lang/ui"
	uisystem "github.com/vo-lang/ui/system"
)
func App() ui.View {
	label := "before"
	go func() {
		value, present, err := uisystem.ReadClipboardText()
		if err == nil && present && value != "" { label = value }
		_ = uisystem.BeginFileDrag([]string{"/tmp/demo.vo"}, uisystem.DefaultFileDragOptions())
	}()
	return ui.TextInput(label, "Name", func(event ui.Event) { label = event.Text })
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        let target = vo_target::TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let image = crate::compile_wasm_aot_image(&compiled, &target).unwrap();
        assert!(image.bytes.starts_with(b"\0asm"));
        assert_eq!(
            image.manifest.target_triple,
            vo_target::WASM32_UNKNOWN_UNKNOWN
        );
        let native_target = vo_target::TargetSpec::parse(env!("VO_TARGET_TRIPLE")).unwrap();
        let object = crate::compile_native_aot_object(&compiled, &native_target, false).unwrap();
        assert!(!object.bytes.is_empty());
        assert_eq!(object.target_triple, env!("VO_TARGET_TRIPLE"));
    }

    #[test]
    fn component_bundle_evaluators_lower_to_native_and_core_wasm_aot() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func Counter(label string) ui.View {
	count := int64(0)
	return ui.Button(label+" "+strconv.FormatInt(count, 10), func(event ui.Event) { count++ })
}
func App() ui.View { return ui.Column(Counter("A"), ui.Key(Counter("B"), "b")) }
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
        );
        let compiled = workspace.compile();
        assert!(compiled
            .module
            .module()
            .artifact(vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME)
            .is_some());

        let wasm_target = vo_target::TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let image = crate::compile_wasm_aot_image(&compiled, &wasm_target).unwrap();
        let artifacts = vo_wasm_aot::decode_wasm_aot_artifacts(&image.bytes).unwrap();
        assert!(artifacts.iter().any(|artifact| {
            artifact.name == vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME
                && artifact.version == vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_VERSION
        }));

        let native_target = vo_target::TargetSpec::parse(env!("VO_TARGET_TRIPLE")).unwrap();
        let object = crate::compile_native_aot_object(&compiled, &native_target, false).unwrap();
        assert!(!object.bytes.is_empty());
        assert!(object.functions.len() >= compiled.module.module().functions.len());
    }

    #[test]
    fn component_dynamic_scopes_lower_to_native_and_core_wasm_aot() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui"
func Child(label string) ui.View { count := int64(0); return ui.Button(label, func(event ui.Event) { count++ }) }
func App() ui.View {
	reversed := false
	if reversed { return ui.Column(ui.Key(Child("B"), "b"), ui.Key(Child("A"), "a")) }
	return ui.Column(ui.Key(Child("A"), "a"), ui.Key(Child("B"), "b"))
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
        );
        let compiled = workspace.compile();
        assert!(compiled
            .module
            .module()
            .artifact(vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME)
            .is_none());
        for expected in ["runtimeEnterComponent", "runtimeExitComponent"] {
            assert!(compiled.module.module().externs.iter().any(|external| {
                vo_common_core::extern_key::decode_extern_name(&external.name).is_ok_and(|key| {
                    key.package() == "github.com/vo-lang/ui" && key.function() == expected
                })
            }));
        }

        let wasm_target = vo_target::TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let image = crate::compile_wasm_aot_image(&compiled, &wasm_target).unwrap();
        assert!(image.bytes.starts_with(b"\0asm"));

        let native_target = vo_target::TargetSpec::parse(env!("VO_TARGET_TRIPLE")).unwrap();
        let object = crate::compile_native_aot_object(&compiled, &native_target, false).unwrap();
        assert!(!object.bytes.is_empty());
        assert!(object.functions.len() >= compiled.module.module().functions.len());
    }

    #[test]
    fn official_motion_and_gesture_packages_lower_to_native_and_core_wasm_aot() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import (
	"github.com/vo-lang/ui"
	"github.com/vo-lang/ui/gesture"
	"github.com/vo-lang/ui/motion"
)
func App() ui.View {
	value := motion.UseValue(0)
	drag := gesture.UseDrag()
	return gesture.BindDrag(ui.Padding(ui.Box(), value.Current()), drag, gesture.DefaultDragOptions(), func(snapshot gesture.DragSnapshot) {
		if snapshot.Phase == gesture.Ended {
			value.SpringTo(0, motion.DefaultSpring())
		} else {
			value.AnimateTo(snapshot.DeltaX, motion.DefaultTween())
		}
	})
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        let wasm_target = vo_target::TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let image = crate::compile_wasm_aot_image(&compiled, &wasm_target).unwrap();
        assert!(image.bytes.starts_with(b"\0asm"));
        let native_target = vo_target::TargetSpec::parse(env!("VO_TARGET_TRIPLE")).unwrap();
        let object = crate::compile_native_aot_object(&compiled, &native_target, false).unwrap();
        assert!(!object.bytes.is_empty());
    }

    fn ui_key_update_for(
        module: Arc<vo_runtime::bytecode::LoadedModule>,
        mode: RunMode,
        key: &str,
        code: &str,
        modifiers: vo_ui_core::EventModifiers,
        repeat: bool,
        composing: bool,
    ) -> vo_ui_protocol::MutationBatch {
        let mut vm = match mode {
            RunMode::Vm => Vm::new(),
            RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1,
                debug_ir: false,
                ..vo_vm::JitConfig::default()
            })
            .unwrap(),
        };
        register_ui_externs(&mut vm, &module).unwrap();
        vm.load_verified(module).unwrap();
        assert_eq!(vm.run().unwrap(), SchedulingOutcome::SuspendedForHostEvents);
        let limits = vo_ui_protocol::ProtocolLimits::default();
        let initial =
            vo_ui_protocol::decode_batch(&vm.take_host_output().unwrap(), limits).unwrap();
        let (target, handler) = initial
            .mutations
            .iter()
            .find_map(|mutation| match mutation {
                vo_ui_protocol::Mutation::Listen { id, listener }
                    if listener.event == vo_ui_core::EventType::KEY_DOWN =>
                {
                    Some((*id, listener.handler))
                }
                _ => None,
            })
            .unwrap();
        let event = vo_ui_protocol::EventEnvelope::new(
            initial.session_epoch,
            vo_ui_core::UiEvent {
                handler,
                event: vo_ui_core::EventType::KEY_DOWN,
                target,
                sequence: 1,
                payload: vo_ui_core::EventPayload::Key(vo_ui_core::KeyEventData {
                    key: key.to_string(),
                    code: code.to_string(),
                    modifiers,
                    repeat,
                    composing,
                }),
            },
        );
        let pending = vm.take_pending_host_events();
        assert!(vm.wake_host_event_with_data(
            pending[0].key,
            vo_ui_protocol::encode_event(&event, limits).unwrap()
        ));
        assert_eq!(
            vm.run_scheduled().unwrap(),
            SchedulingOutcome::SuspendedForHostEvents
        );
        vo_ui_protocol::decode_batch(&vm.take_host_output().unwrap(), limits).unwrap()
    }

    #[test]
    fn official_ui_key_event_fields_are_vm_jit_equivalent() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui"
var observed = "waiting"
func App() ui.View {
	return ui.OnKeyDown(
		ui.TextInput(observed, "Key", func(event ui.Event) {}),
		func(event ui.Event) {
			observed = event.Key + ":" + event.Code
			if event.Repeat { observed += ":repeat" }
			if event.Composing { observed += ":ime" }
		},
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        let key_modifiers = vo_ui_core::EventModifiers {
            shift: true,
            ..vo_ui_core::EventModifiers::default()
        };
        let vm = ui_key_update_for(
            compiled.module.clone(),
            RunMode::Vm,
            "Enter",
            "NumpadEnter",
            key_modifiers,
            true,
            true,
        );
        let jit = ui_key_update_for(
            compiled.module,
            RunMode::Jit,
            "Enter",
            "NumpadEnter",
            key_modifiers,
            true,
            true,
        );
        assert_eq!(vm.mutations, jit.mutations);
        assert!(vm.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::VALUE
                    && property.value
                        == vo_ui_core::Value::Text("Enter:NumpadEnter:repeat:ime".to_string())
        )));
    }

    #[test]
    fn official_ui_command_shortcuts_match_shifted_letter_case_in_vm_and_jit() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import (
	"github.com/vo-lang/ui"
	"github.com/vo-lang/ui/commands"
)
var observed = "waiting"
func App() ui.View {
	input := ui.TextInput(observed, "Command", func(event ui.Event) {})
	return commands.Bind(input, commands.New("palette", "Palette", commands.Key("p", ui.ModifierMeta|ui.ModifierShift), func() {
		observed = "matched"
	}))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        let modifiers = vo_ui_core::EventModifiers {
            shift: true,
            meta: true,
            ..vo_ui_core::EventModifiers::default()
        };
        for mode in [RunMode::Vm, RunMode::Jit] {
            let batch = ui_key_update_for(
                compiled.module.clone(),
                mode,
                "P",
                "KeyP",
                modifiers,
                false,
                false,
            );
            assert!(batch.mutations.iter().any(|mutation| matches!(
                mutation,
                vo_ui_protocol::Mutation::SetProperty { property, .. }
                    if property.id == vo_ui_core::PropertyId::VALUE
                        && property.value
                            == vo_ui_core::Value::Text("matched".to_string())
            )));
        }
    }

    fn ui_composition_update_for(
        module: Arc<vo_runtime::bytecode::LoadedModule>,
        mode: RunMode,
    ) -> vo_ui_protocol::MutationBatch {
        let mut vm = match mode {
            RunMode::Vm => Vm::new(),
            RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1,
                debug_ir: false,
                ..vo_vm::JitConfig::default()
            })
            .unwrap(),
        };
        register_ui_externs(&mut vm, &module).unwrap();
        vm.load_verified(module).unwrap();
        assert_eq!(vm.run().unwrap(), SchedulingOutcome::SuspendedForHostEvents);
        let limits = vo_ui_protocol::ProtocolLimits::default();
        let initial =
            vo_ui_protocol::decode_batch(&vm.take_host_output().unwrap(), limits).unwrap();
        let (target, handler) = initial
            .mutations
            .iter()
            .find_map(|mutation| match mutation {
                vo_ui_protocol::Mutation::Listen { id, listener }
                    if listener.event == vo_ui_core::EventType::COMPOSITION_UPDATE =>
                {
                    Some((*id, listener.handler))
                }
                _ => None,
            })
            .unwrap();
        let event = vo_ui_protocol::EventEnvelope::new(
            initial.session_epoch,
            vo_ui_core::UiEvent {
                handler,
                event: vo_ui_core::EventType::COMPOSITION_UPDATE,
                target,
                sequence: 1,
                payload: vo_ui_core::EventPayload::Composition(vo_ui_core::CompositionEventData {
                    text: "拼音".to_string(),
                    selection_start_utf16: 1,
                    selection_length_utf16: 2,
                }),
            },
        );
        let pending = vm.take_pending_host_events();
        assert!(vm.wake_host_event_with_data(
            pending[0].key,
            vo_ui_protocol::encode_event(&event, limits).unwrap()
        ));
        assert_eq!(
            vm.run_scheduled().unwrap(),
            SchedulingOutcome::SuspendedForHostEvents
        );
        vo_ui_protocol::decode_batch(&vm.take_host_output().unwrap(), limits).unwrap()
    }

    #[test]
    fn official_ui_composition_selection_is_vm_jit_equivalent() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui"
import "strconv"
var observed = "waiting"
func App() ui.View {
	return ui.OnCompositionUpdate(
		ui.TextInput(observed, "IME", func(event ui.Event) {}),
		func(event ui.Event) {
			observed = event.Text + ":" +
				strconv.FormatInt(event.SelectionStartUTF16, 10) + ":" +
				strconv.FormatInt(event.SelectionLengthUTF16, 10)
		},
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        let vm = ui_composition_update_for(compiled.module.clone(), RunMode::Vm);
        let jit = ui_composition_update_for(compiled.module, RunMode::Jit);
        assert_eq!(vm.mutations, jit.mutations);
        assert!(vm.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::VALUE
                    && property.value
                        == vo_ui_core::Value::Text("拼音:1:2".to_string())
        )));
    }

    #[test]
    fn official_kit_is_vm_jit_equivalent_through_generic_reconciliation() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui"
import "github.com/vo-lang/ui/kit"
var name = "before"
func App() ui.View {
	theme := kit.LightTheme()
	return kit.Screen(theme, kit.Card(theme,
		ui.SelectionLengthUTF16(ui.SelectionStartUTF16(kit.ValidatedTextField(theme, "Name", name, "Type", true, false, "Name is required", func(event ui.Event) {
			name = event.Text
		}), 1), 2),
	))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        assert!(compiled
            .module
            .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
            .is_none());
        let (vm_initial, vm_update) = ui_batches_for(compiled.module.clone(), RunMode::Vm);
        let (jit_initial, jit_update) = ui_batches_for(compiled.module, RunMode::Jit);
        assert_eq!(vm_initial.mutations, jit_initial.mutations);
        assert_eq!(vm_update.mutations, jit_update.mutations);
        assert!(vm_initial.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::RADIUS
                    && property.value == vo_ui_core::Value::Length(vo_ui_core::Length::Px(10.0))
        )));
        for (id, value) in [
            (
                vo_ui_core::PropertyId::REQUIRED,
                vo_ui_core::Value::Bool(true),
            ),
            (
                vo_ui_core::PropertyId::INVALID,
                vo_ui_core::Value::Bool(true),
            ),
            (
                vo_ui_core::PropertyId::ACCESSIBLE_DESCRIPTION,
                vo_ui_core::Value::Text("Name is required".to_string()),
            ),
            (
                vo_ui_core::PropertyId::SELECTION_START_UTF16,
                vo_ui_core::Value::I64(1),
            ),
            (
                vo_ui_core::PropertyId::SELECTION_LENGTH_UTF16,
                vo_ui_core::Value::I64(2),
            ),
        ] {
            assert!(vm_initial.mutations.iter().any(|mutation| matches!(
                mutation,
                vo_ui_protocol::Mutation::SetProperty { property, .. }
                    if property.id == id && property.value == value
            )));
        }
        assert!(vm_initial.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::ROLE
                    && property.value == vo_ui_core::Value::Text("alert".to_string())
        )));
        assert!(vm_update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::VALUE
                    && property.value == vo_ui_core::Value::Text("after".to_string())
        )));
    }

    #[test]
    fn official_ui_keys_preserve_vm_jit_nodes_across_reordering() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui"
var flipped = false
func App() ui.View {
	a := ui.Key(ui.Button("A", func(event ui.Event) { flipped = !flipped }), "a")
	b := ui.Key(ui.Button("B", func(event ui.Event) { flipped = !flipped }), "b")
	if flipped { return ui.Column(b, a) }
	return ui.Column(a, b)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        assert!(compiled
            .module
            .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
            .is_none());
        let (vm_initial, vm_update) = ui_single_event_batches_for(
            compiled.module.clone(),
            RunMode::Vm,
            vo_ui_core::EventType::CLICK,
            vo_ui_core::EventPayload::None,
        );
        let (jit_initial, jit_update) = ui_single_event_batches_for(
            compiled.module,
            RunMode::Jit,
            vo_ui_core::EventType::CLICK,
            vo_ui_core::EventPayload::None,
        );
        assert_eq!(vm_initial.mutations, jit_initial.mutations);
        assert_eq!(vm_update.mutations, jit_update.mutations);

        let buttons = vm_initial
            .mutations
            .iter()
            .filter_map(|mutation| match mutation {
                vo_ui_protocol::Mutation::Create {
                    id,
                    kind: vo_ui_protocol::NodeKind::Element(vo_ui_core::Primitive::Button),
                } => Some(*id),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(buttons.len(), 2);
        assert!(vm_update.mutations.iter().all(|mutation| !matches!(
            mutation,
            vo_ui_protocol::Mutation::Create { .. } | vo_ui_protocol::Mutation::Delete { .. }
        )));
        assert!(vm_update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::InsertBefore {
                child,
                before: Some(before),
                ..
            } if *child == buttons[1] && *before == buttons[0]
        )));
    }

    #[test]
    fn official_kit_virtual_list_bounds_vm_jit_materialization() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui"
import "github.com/vo-lang/ui/kit"
import "strconv"
var offset = 0.0
func App() ui.View {
	window := kit.VisibleRange(100, 20, 200, 100, 2)
	if window.Start != 8 || window.End != 18 { panic("invalid virtual range") }
	return kit.VirtualList(100, 20, offset, 100, 2, func(index int64) ui.View {
		return ui.Text("Row "+strconv.FormatInt(index, 10))
	}, func(event ui.Event) { offset = event.Y })
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        assert!(compiled
            .module
            .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
            .is_none());
        let payload = vo_ui_core::EventPayload::Scroll(vo_ui_core::ScrollEventData {
            x: 0.0,
            y: 200.0,
            delta_x: 0.0,
            delta_y: 200.0,
            unit: vo_ui_core::ScrollUnit::Pixel,
            modifiers: vo_ui_core::EventModifiers::default(),
        });
        let (vm_initial, vm_update) = ui_single_event_batches_for(
            compiled.module.clone(),
            RunMode::Vm,
            vo_ui_core::EventType::SCROLL,
            payload.clone(),
        );
        let (jit_initial, jit_update) = ui_single_event_batches_for(
            compiled.module,
            RunMode::Jit,
            vo_ui_core::EventType::SCROLL,
            payload,
        );
        assert_eq!(vm_initial.mutations, jit_initial.mutations);
        assert_eq!(vm_update.mutations, jit_update.mutations);
        assert_eq!(
            vm_initial
                .mutations
                .iter()
                .filter(|mutation| matches!(mutation, vo_ui_protocol::Mutation::SetText { .. }))
                .count(),
            8
        );
        assert_eq!(
            vm_update
                .mutations
                .iter()
                .filter(|mutation| matches!(mutation, vo_ui_protocol::Mutation::SetText { .. }))
                .count(),
            10
        );
        assert!(vm_update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::SCROLL_Y
                    && property.value == vo_ui_core::Value::F64(200.0)
        )));
    }

    #[test]
    fn official_data_application_compiles_and_bounds_vm_jit_materialization() {
        let workspace = UiTestWorkspace::create_with_main(include_str!(
            "../../../../ui/showcases/data-application/main.vo"
        ));
        let compiled = workspace.compile();
        let vm = ui_initial_batch_for(compiled.module.clone(), RunMode::Vm);
        let jit = ui_initial_batch_for(compiled.module.clone(), RunMode::Jit);
        assert_eq!(vm.mutations, jit.mutations);
        let text_count = vm
            .mutations
            .iter()
            .filter(|mutation| matches!(mutation, vo_ui_protocol::Mutation::SetText { .. }))
            .count();
        assert!(
            text_count < 200,
            "virtual data app mounted {text_count} text nodes"
        );
        assert!(vm.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { text, .. } if text == "Member 0"
        )));
        assert!(vm.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::ROLE
                    && property.value == vo_ui_core::Value::Text("grid".to_string())
        )));
        let wasm_target = vo_target::TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let wasm = crate::compile_wasm_aot_image(&compiled, &wasm_target).unwrap();
        assert!(wasm.bytes.starts_with(b"\0asm"));
        let native_target = vo_target::TargetSpec::parse(env!("VO_TARGET_TRIPLE")).unwrap();
        let native = crate::compile_native_aot_object(&compiled, &native_target, false).unwrap();
        assert!(!native.bytes.is_empty());
    }

    #[test]
    fn official_e4_application_models_are_vm_jit_equivalent() {
        let workspace = UiTestWorkspace::create_with_main(include_str!(
            "../../../../ui/tests/application-platform/main.vo"
        ));
        let compiled = workspace.compile();
        for mode in [RunMode::Vm, RunMode::Jit] {
            let mut vm = match mode {
                RunMode::Vm => Vm::new(),
                RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                    call_threshold: 1,
                    loop_threshold: 1,
                    debug_ir: false,
                    ..vo_vm::JitConfig::default()
                })
                .unwrap(),
            };
            register_ui_externs(&mut vm, &compiled.module).unwrap();
            vm.load_verified(compiled.module.clone()).unwrap();
            assert_eq!(
                vm.run().unwrap(),
                SchedulingOutcome::Completed,
                "mode {mode:?}"
            );
        }
    }

    #[test]
    fn official_e5_web_and_desktop_models_are_vm_jit_equivalent() {
        let workspace = UiTestWorkspace::create_with_main(include_str!(
            "../../../../ui/tests/web-desktop-product/main.vo"
        ));
        let compiled = workspace.compile();
        for mode in [RunMode::Vm, RunMode::Jit] {
            let mut vm = match mode {
                RunMode::Vm => Vm::new(),
                RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                    call_threshold: 1,
                    loop_threshold: 1,
                    debug_ir: false,
                    ..vo_vm::JitConfig::default()
                })
                .unwrap(),
            };
            register_ui_externs(&mut vm, &compiled.module).unwrap();
            vm.load_verified(compiled.module.clone()).unwrap();
            assert_eq!(
                vm.run().unwrap(),
                SchedulingOutcome::Completed,
                "mode {mode:?}"
            );
        }
    }

    #[test]
    fn browser_aot_rejects_server_authority() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui/web/server"
func main() { _ = server.NewAuthority(nil, nil, nil) }
"#,
        );
        let compiled = workspace.compile();
        let target = vo_target::TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let error = crate::compile_wasm_aot_image(&compiled, &target).unwrap_err();
        assert!(
            error.to_string().contains("web/server authority"),
            "{error}"
        );
        let native_target = vo_target::TargetSpec::parse(env!("VO_TARGET_TRIPLE")).unwrap();
        let native = crate::compile_native_aot_object(&compiled, &native_target, false).unwrap();
        assert!(!native.bytes.is_empty());
    }

    #[test]
    fn official_content_site_renders_distinct_useful_routes_and_web_aot() {
        let workspace = UiTestWorkspace::create_with_main(include_str!(
            "../../../../ui/showcases/content-site/main.vo"
        ));
        let compiled = workspace.compile();
        let metadata = vo_ui_web::DocumentMetadata {
            language: "en".to_string(),
            direction: "ltr".to_string(),
            title: "Volang Field Notes".to_string(),
            description: "Useful HTML".to_string(),
            ..vo_ui_web::DocumentMetadata::default()
        };
        let home = render_initial_ui_document_at(
            compiled.clone(),
            RunMode::Vm,
            "/",
            &metadata,
            vo_ui_web::SsrLimits::default(),
        )
        .unwrap();
        let article = render_initial_ui_document_at(
            compiled.clone(),
            RunMode::Jit,
            "/articles/wasm-aot",
            &metadata,
            vo_ui_web::SsrLimits::default(),
        )
        .unwrap();
        assert!(home.html.contains("zero JavaScript application code"));
        assert!(article
            .html
            .contains("Wasm AOT without a JavaScript framework"));
        assert!(!article.html.contains("zero JavaScript application code"));
        assert!(!article.activation.is_empty());
        let chunks = vo_ui_web::stream_document(&article, 1024).unwrap();
        assert!(chunks.len() > 1);
        assert_eq!(chunks.concat(), article.html);
        let target = vo_target::TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let image = crate::compile_wasm_aot_image(&compiled, &target).unwrap();
        assert!(image.bytes.starts_with(b"\0asm"));
    }

    #[test]
    fn official_advanced_packs_are_vm_jit_and_aot_equivalent() {
        let workspace = UiTestWorkspace::create_with_main(include_str!(
            "../../../../ui/tests/advanced-packs/main.vo"
        ));
        let compiled = workspace.compile();
        let vm_initial = ui_initial_batch_for(compiled.module.clone(), RunMode::Vm);
        let jit_initial = ui_initial_batch_for(compiled.module.clone(), RunMode::Jit);
        assert_eq!(vm_initial.mutations, jit_initial.mutations);
        assert!(vm_initial.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::Create {
                kind: vo_ui_protocol::NodeKind::Element(vo_ui_core::Primitive::Canvas),
                ..
            }
        )));
        assert!(vm_initial.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::GRAPHICS_PROGRAM
        )));
        let web_target = vo_target::TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        assert!(!crate::compile_wasm_aot_image(&compiled, &web_target)
            .unwrap()
            .bytes
            .is_empty());
        let native_target = vo_target::TargetSpec::parse(env!("VO_TARGET_TRIPLE")).unwrap();
        assert!(
            !crate::compile_native_aot_object(&compiled, &native_target, false)
                .unwrap()
                .bytes
                .is_empty()
        );
    }

    #[test]
    fn official_media_and_studio_showcases_use_public_advanced_packs() {
        for (name, source) in [
            (
                "media",
                include_str!("../../../../ui/showcases/media-application/main.vo"),
            ),
            (
                "studio",
                include_str!("../../../../ui/showcases/studio-workbench/main.vo"),
            ),
        ] {
            let workspace = UiTestWorkspace::create_with_main(source);
            let compiled = workspace.compile();
            let vm = ui_initial_batch_for(compiled.module.clone(), RunMode::Vm);
            let jit = ui_initial_batch_for(compiled.module.clone(), RunMode::Jit);
            assert_eq!(vm.mutations, jit.mutations, "{name} VM/JIT tree");
            assert!(vm.mutations.len() > 20, "{name} should render useful UI");
            let web_target =
                vo_target::TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
            assert!(!crate::compile_wasm_aot_image(&compiled, &web_target)
                .unwrap()
                .bytes
                .is_empty());
        }
    }

    #[test]
    fn official_multi_window_editor_preserves_shared_document_and_native_aot() {
        let workspace = UiTestWorkspace::create_with_main(include_str!(
            "../../../../ui/tests/multi-window-editor/main.vo"
        ));
        let compiled = workspace.compile();
        let vm = ui_initial_batch_for(compiled.module.clone(), RunMode::Vm);
        let jit = ui_initial_batch_for(compiled.module.clone(), RunMode::Jit);
        assert_eq!(vm.mutations, jit.mutations);
        let native_target = vo_target::TargetSpec::parse(env!("VO_TARGET_TRIPLE")).unwrap();
        assert!(
            !crate::compile_native_aot_object(&compiled, &native_target, false)
                .unwrap()
                .bytes
                .is_empty()
        );
    }

    #[test]
    fn official_ui_testing_and_observability_are_vm_jit_and_aot_equivalent() {
        let workspace = UiTestWorkspace::create_with_main(include_str!(
            "../../../../ui/tests/tooling-resilience/main.vo"
        ));
        let compiled = workspace.compile();
        let vm = ui_initial_batch_for(compiled.module.clone(), RunMode::Vm);
        let jit = ui_initial_batch_for(compiled.module.clone(), RunMode::Jit);
        assert_eq!(vm.mutations, jit.mutations);
        assert!(vm.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { text, .. }
                if text == "tooling-resilience: ok"
        )));
        let web_target = vo_target::TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        assert!(!crate::compile_wasm_aot_image(&compiled, &web_target)
            .unwrap()
            .bytes
            .is_empty());
        let native_target = vo_target::TargetSpec::parse(env!("VO_TARGET_TRIPLE")).unwrap();
        assert!(
            !crate::compile_native_aot_object(&compiled, &native_target, false)
                .unwrap()
                .bytes
                .is_empty()
        );
    }

    #[test]
    fn official_typed_state_is_persistent_and_vm_jit_equivalent() {
        let workspace = UiTestWorkspace::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui"
func App() ui.View {
	name := ui.UseStringState("before")
	return ui.TextInput(ui.StringStateValue(name), "Name", func(event ui.Event) {
		ui.SetStringState(name, event.Text)
	})
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let compiled = workspace.compile();
        assert!(compiled
            .module
            .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
            .is_some());
        let (vm_initial, vm_update) = ui_batches_for(compiled.module.clone(), RunMode::Vm);
        let (jit_initial, jit_update) = ui_batches_for(compiled.module, RunMode::Jit);
        assert_eq!(vm_initial.mutations, jit_initial.mutations);
        assert_eq!(vm_update.mutations, jit_update.mutations);
        assert!(vm_update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::VALUE
                    && property.value == vo_ui_core::Value::Text("after".to_string())
        )));
    }

    #[test]
    fn jit_stops_before_consuming_a_null_managed_allocation() {
        let compiled = crate::compile_string(
            r#"
package main

func allocate() string {
	return "managed allocation"
}

func main() {
	_ = allocate()
}
"#,
        )
        .expect("JIT memory failure fixture should compile");
        let mut vm = Vm::try_with_jit_and_memory_config(
            vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1,
                debug_ir: false,
                ..vo_vm::JitConfig::default()
            },
            vo_vm::VmMemoryConfig {
                allocation_allowed: false,
                oom_policy: vo_vm::OomPolicy::TerminateIsland,
                ..vo_vm::VmMemoryConfig::default()
            },
        )
        .expect("JIT VM should initialize");
        vm.load_verified(compiled.module)
            .expect("JIT memory failure fixture should load");

        let error = vm
            .run()
            .expect_err("JIT allocation failure must terminate the current Island");
        assert!(matches!(
            error,
            VmError::IslandMemory(vo_runtime::gc::MemoryError::AllocationForbidden)
        ));
        assert!(
            vm.jit_execution_stats().executed_jit_code(),
            "fixture must execute generated code before the memory failure"
        );
    }

    fn run_guest_exit_subprocess(test_name: &str, scenario: impl FnOnce()) {
        if std::env::var_os(GUEST_EXIT_SUBPROCESS_ENV).is_some() {
            eprintln!("{GUEST_EXIT_SUBPROCESS_MARKER}:{test_name}");
            scenario();
            return;
        }

        let _serial = GUEST_EXIT_SUBPROCESS_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let mut child = std::process::Command::new(
            std::env::current_exe().expect("guest-exit test executable should be available"),
        )
        .arg(test_name)
        .arg("--exact")
        .arg("--nocapture")
        .env(GUEST_EXIT_SUBPROCESS_ENV, "1")
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("guest-exit subprocess should start");
        let deadline = std::time::Instant::now() + GUEST_EXIT_SUBPROCESS_TIMEOUT;

        loop {
            match child
                .try_wait()
                .expect("guest-exit subprocess should remain waitable")
            {
                Some(status) => {
                    let output = read_guest_exit_subprocess_output(&mut child);
                    assert!(
                        output.contains(&format!("{GUEST_EXIT_SUBPROCESS_MARKER}:{test_name}")),
                        "guest-exit subprocess filter did not execute {test_name}; output:\n{output}"
                    );
                    assert!(
                        status.success(),
                        "guest-exit subprocess {test_name} failed with {status}; output:\n{output}"
                    );
                    return;
                }
                None if std::time::Instant::now() < deadline => {
                    std::thread::sleep(std::time::Duration::from_millis(10));
                }
                None => {
                    let kill_error = child.kill().err();
                    let wait_result = child.wait();
                    let output = read_guest_exit_subprocess_output(&mut child);
                    panic!(
                        "guest-exit subprocess {test_name} exceeded {GUEST_EXIT_SUBPROCESS_TIMEOUT:?}; kill_error={kill_error:?}; wait_result={wait_result:?}; output:\n{output}"
                    );
                }
            }
        }
    }

    fn read_guest_exit_subprocess_output(child: &mut std::process::Child) -> String {
        use std::io::Read;

        let mut output = String::new();
        if let Some(mut stdout) = child.stdout.take() {
            stdout
                .read_to_string(&mut output)
                .expect("guest-exit subprocess stdout should be readable");
        }
        if let Some(mut stderr) = child.stderr.take() {
            if !output.is_empty() && !output.ends_with('\n') {
                output.push('\n');
            }
            stderr
                .read_to_string(&mut output)
                .expect("guest-exit subprocess stderr should be readable");
        }
        output
    }

    struct ScopedEnvVar {
        key: &'static str,
        old: Option<String>,
    }

    impl ScopedEnvVar {
        fn set(key: &'static str, value: &str) -> Self {
            let old = std::env::var(key).ok();
            std::env::set_var(key, value);
            Self { key, old }
        }
    }

    impl Drop for ScopedEnvVar {
        fn drop(&mut self) {
            if let Some(value) = &self.old {
                std::env::set_var(self.key, value);
            } else {
                std::env::remove_var(self.key);
            }
        }
    }

    fn vm_error_for(source: &str, mode: RunMode) -> VmError {
        let compiled = crate::compile_string(source).expect("source should compile");
        let mut vm = match mode {
            RunMode::Vm => Vm::new(),
            RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1_000_000,
                debug_ir: false,
                ..vo_vm::JitConfig::default()
            })
            .expect("JIT should initialize"),
        };
        vm.set_output_sink(CaptureSink::new());
        if let Err(err) = vm.load_verified(compiled.module) {
            return err;
        }
        match vm.run() {
            Err(err) => err,
            Ok(outcome) => panic!("expected runtime error, VM returned {outcome:?}"),
        }
    }

    fn vm_error_for_module(module: Module, config: vo_vm::JitConfig) -> VmError {
        let mut vm = Vm::try_with_jit_config(config).expect("JIT should initialize");
        vm.set_output_sink(CaptureSink::new());
        if let Err(err) = vm.load(module) {
            return err;
        }
        match vm.run() {
            Err(err) => err,
            Ok(outcome) => panic!("expected runtime error, VM returned {outcome:?}"),
        }
    }

    fn output_for(
        source: &str,
        mode: RunMode,
        config: vo_vm::JitConfig,
    ) -> (String, RunObservation) {
        let compiled = crate::compile_string(source).expect("source should compile");
        let mut vm = match mode {
            RunMode::Vm => Vm::new(),
            RunMode::Jit => Vm::try_with_jit_config(config).expect("JIT should initialize"),
        };
        let sink = CaptureSink::new();
        vm.set_output_sink(sink.clone());
        vm.load_verified(compiled.module).unwrap();
        let outcome = vm.run().expect("program should run");
        assert_ne!(
            outcome,
            SchedulingOutcome::Blocked,
            "program should not block"
        );
        (sink.take(), run_observation(&vm))
    }

    #[test]
    fn os_exit_terminates_only_the_guest_vm_in_vm_and_jit_modes() {
        let source = r#"
package main

import (
	"fmt"
	"os"
)

func main() {
	fmt.Print("before")
	os.Exit(37)
	fmt.Print("after")
}
"#;

        for mode in [RunMode::Vm, RunMode::Jit] {
            let compiled = crate::compile_string(source).expect("os.Exit source should compile");
            let mut vm = match mode {
                RunMode::Vm => Vm::new(),
                RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                    call_threshold: 1,
                    loop_threshold: 1,
                    debug_ir: false,
                    ..vo_vm::JitConfig::default()
                })
                .expect("JIT should initialize"),
            };
            let sink = CaptureSink::new();
            vm.set_output_sink(sink.clone());
            vm.load_verified(compiled.module)
                .expect("module should load");

            assert_eq!(
                vm.run().expect("os.Exit should be a normal VM outcome"),
                SchedulingOutcome::Exited(37),
                "mode {mode:?}",
            );
            assert_eq!(vm.exit_code(), Some(37), "mode {mode:?}");
            assert_eq!(sink.take(), "before", "mode {mode:?}");
        }
    }

    #[test]
    fn child_island_exit_terminates_the_guest_with_its_exact_code() {
        run_guest_exit_subprocess(
            "run::tests::child_island_exit_terminates_the_guest_with_its_exact_code",
            || {
                let source = r#"
package main

import "os"

func main() {
	worker := make(island)
	wait := make(port int, 1)
	go @(worker) func(wait port<- int) {
		os.Exit(41)
		wait <- 1
	}(wait)
	<-wait
	panic("continued after child island exit")
}
"#;

                for mode in [RunMode::Vm, RunMode::Jit] {
                    let compiled =
                        crate::compile_string(source).expect("child island exit should compile");
                    let mut vm = match mode {
                        RunMode::Vm => Vm::new(),
                        RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                            call_threshold: 1,
                            loop_threshold: 1,
                            debug_ir: false,
                            ..vo_vm::JitConfig::default()
                        })
                        .expect("JIT should initialize"),
                    };
                    vm.load_verified(compiled.module)
                        .expect("module should load");

                    let outcome = vm.run().unwrap_or_else(|error| {
                        panic!("child os.Exit should be a normal {mode:?} outcome: {error:?}")
                    });
                    assert_eq!(outcome, SchedulingOutcome::Exited(41), "mode {mode:?}",);
                    assert_eq!(vm.exit_code(), Some(41), "mode {mode:?}");
                }
            },
        );
    }

    #[test]
    fn main_exit_joins_a_live_child_island_before_returning() {
        run_guest_exit_subprocess(
            "run::tests::main_exit_joins_a_live_child_island_before_returning",
            || {
                let source = r#"
package main

import "os"

func main() {
	worker := make(island)
	ready := make(port int, 1)
	go @(worker) func(ready port<- int) {
		ready <- 1
	}(ready)
	<-ready
	os.Exit(45)
}
"#;

                for mode in [RunMode::Vm, RunMode::Jit] {
                    let compiled = crate::compile_string(source)
                        .expect("main island exit fixture should compile");
                    let mut vm = match mode {
                        RunMode::Vm => Vm::new(),
                        RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                            call_threshold: 1,
                            loop_threshold: 1,
                            debug_ir: false,
                            ..vo_vm::JitConfig::default()
                        })
                        .expect("JIT should initialize"),
                    };
                    vm.load_verified(compiled.module)
                        .expect("module should load");

                    let outcome = vm.run().unwrap_or_else(|error| {
                        panic!("main os.Exit should be a normal {mode:?} outcome: {error:?}")
                    });
                    assert_eq!(outcome, SchedulingOutcome::Exited(45), "mode {mode:?}",);
                    assert_eq!(vm.exit_code(), Some(45), "mode {mode:?}");
                }
            },
        );
    }

    #[test]
    fn child_island_init_exit_survives_vm_and_jit_startup_boundaries() {
        run_guest_exit_subprocess(
            "run::tests::child_island_init_exit_survives_vm_and_jit_startup_boundaries",
            || {
                let source = r#"
package main

import "os"

func init() {
	if len(os.Args) == 0 {
		os.Exit(43)
	}
}

func main() {
	worker := make(island)
	wait := make(port int, 1)
	go @(worker) func(wait port<- int) {
		wait <- 1
	}(wait)
	<-wait
	panic("continued after child island init exit")
}
"#;

                for mode in [RunMode::Vm, RunMode::Jit] {
                    let compiled = crate::compile_string(source)
                        .expect("child island init exit should compile");
                    let mut vm = match mode {
                        RunMode::Vm => Vm::new(),
                        RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                            call_threshold: 1,
                            loop_threshold: 1,
                            debug_ir: false,
                            ..vo_vm::JitConfig::default()
                        })
                        .expect("JIT should initialize"),
                    };
                    vm.set_program_args(vec!["main".to_string()]);
                    vm.load_verified(compiled.module)
                        .expect("module should load");

                    assert_eq!(
                        vm.run()
                            .expect("child init os.Exit should be a normal VM outcome"),
                        SchedulingOutcome::Exited(43),
                        "mode {mode:?}",
                    );
                    assert_eq!(vm.exit_code(), Some(43), "mode {mode:?}");
                }
            },
        );
    }

    #[test]
    fn vm_and_jit_output_preserve_arbitrary_string_bytes() {
        let source = r#"
package main

import (
	"fmt"
	"os"
)

func main() {
	fmt.Print(os.Args[0])
	fmt.Print(os.Args[1])
	raw := string([]byte{'a', 0xff, 'z'})
	fmt.Print(raw)
	print(raw)
	println(raw)
	fmt.Printf("%s|%q|%x", raw, raw, raw)
}
"#;
        let expected = b"p\xfea\xffza\xffza\xffz\na\xffz|\"a\\xffz\"|61ff7a";

        for mode in [RunMode::Vm, RunMode::Jit] {
            let compiled = crate::compile_string(source).expect("source should compile");
            let mut vm = match mode {
                RunMode::Vm => Vm::new(),
                RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                    call_threshold: 1,
                    loop_threshold: 1,
                    debug_ir: false,
                    ..vo_vm::JitConfig::default()
                })
                .expect("JIT should initialize"),
            };
            let sink = CaptureSink::new();
            vm.set_output_sink(sink.clone());
            vm.set_program_args_bytes(vec![b"p\xfe".to_vec(), Vec::new()]);
            vm.load_verified(compiled.module)
                .expect("module should load");
            let outcome = vm.run().expect("program should run");
            assert_eq!(outcome, SchedulingOutcome::Completed);
            assert_eq!(sink.take_bytes(), expected, "mode {mode:?}");
            if mode == RunMode::Jit {
                assert!(run_observation(&vm).function_entries > 0);
            }
        }
    }

    #[test]
    fn os_dirfs_implements_fs_fallbacks_path_errors_and_directory_entries() {
        struct TempTree(std::path::PathBuf);

        impl Drop for TempTree {
            fn drop(&mut self) {
                let _ = std::fs::remove_dir_all(&self.0);
            }
        }

        let base = std::env::temp_dir();
        let root = (0..1000)
            .find_map(|attempt| {
                let path = base.join(format!("volang-dirfs-{}-{attempt}", std::process::id()));
                match std::fs::create_dir(&path) {
                    Ok(()) => Some(path),
                    Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => None,
                    Err(error) => panic!("create DirFS test root: {error}"),
                }
            })
            .expect("allocate unique DirFS test root");
        let root = TempTree(root);
        std::fs::write(root.0.join("a.txt"), b"alpha").expect("write a.txt");
        std::fs::create_dir(root.0.join("b")).expect("create b directory");
        std::fs::write(root.0.join("b/c.txt"), b"charlie").expect("write b/c.txt");
        std::fs::write(root.0.join("z.txt"), b"zulu").expect("write z.txt");

        let source = r#"
package main

import (
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"strings"
)

func must(ok bool, message string) {
	if !ok {
		panic(message)
	}
}

func main() {
	fsys := os.DirFS(os.Args[0])
	_, err := fsys.Open("../escape")
	pathErr, ok := err.(*fs.PathError)
	must(ok, "invalid path did not return PathError")
	fmt.Printf("invalid=%s:%s:%t\n", pathErr.Op, pathErr.Path, errors.Is(err, fs.ErrInvalid))

	_, err = fsys.Open("missing")
	pathErr, ok = err.(*fs.PathError)
	must(ok, "missing path did not return PathError")
	fmt.Printf("missing=%s:%s:%t\n", pathErr.Op, pathErr.Path, errors.Is(err, fs.ErrNotExist))

	entries, err := fs.ReadDir(fsys, ".")
	must(err == nil, "ReadDir failed")
	for _, entry := range entries {
		info, infoErr := entry.Info()
		must(infoErr == nil, "DirEntry.Info failed")
		fmt.Printf("entry=%s:%t:%s\n", entry.Name(), entry.IsDir(), info.Name())
	}

	data, err := fs.ReadFile(fsys, "a.txt")
	must(err == nil, "ReadFile failed")
	fmt.Printf("read=%s\n", string(data))
	info, err := fs.Stat(fsys, "a.txt")
	must(err == nil, "Stat failed")
	fmt.Printf("stat=%s:%d\n", info.Name(), info.Size())

	file, err := fsys.Open(".")
	must(err == nil, "Open root failed")
	dir, ok := file.(fs.ReadDirFile)
	must(ok, "directory file does not implement ReadDirFile")
	first, firstErr := dir.ReadDir(1)
	rest, restErr := dir.ReadDir(10)
	end, endErr := dir.ReadDir(1)
	must(firstErr == nil && restErr == nil, "paginated ReadDir failed")
	fmt.Printf("paged=%s:%d:%d:%t\n", first[0].Name(), len(rest), len(end), endErr == io.EOF)
	must(file.Close() == nil, "Close failed")

	paths := make([]string, 0)
	err = fs.WalkDir(fsys, ".", func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		paths = append(paths, path)
		return nil
	})
	must(err == nil, "WalkDir failed")
	fmt.Printf("walk=%s\n", strings.Join(paths, ","))
}
"#;
        let expected = concat!(
            "invalid=open:../escape:true\n",
            "missing=open:missing:true\n",
            "entry=a.txt:false:a.txt\n",
            "entry=b:true:b\n",
            "entry=z.txt:false:z.txt\n",
            "read=alpha\n",
            "stat=a.txt:5\n",
            "paged=a.txt:2:0:true\n",
            "walk=.,a.txt,b,b/c.txt,z.txt\n",
        );

        for mode in [RunMode::Vm, RunMode::Jit] {
            let compiled = crate::compile_string(source).expect("DirFS source should compile");
            let mut vm = match mode {
                RunMode::Vm => Vm::new(),
                RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                    call_threshold: 1,
                    loop_threshold: 1,
                    debug_ir: false,
                    ..vo_vm::JitConfig::default()
                })
                .expect("JIT should initialize"),
            };
            let sink = CaptureSink::new();
            vm.set_output_sink(sink.clone());
            vm.set_program_args_bytes(vec![root.0.as_os_str().as_encoded_bytes().to_vec()]);
            vm.load_verified(compiled.module)
                .expect("DirFS module should load");
            assert_eq!(
                vm.run().expect("DirFS program should run"),
                SchedulingOutcome::Completed,
                "mode {mode:?}",
            );
            assert_eq!(sink.take(), expected, "mode {mode:?}");
        }
    }

    #[test]
    fn concurrent_vms_keep_async_http_completion_tokens_isolated() {
        use std::io::{Read, Write};
        use std::net::{TcpListener, TcpStream};

        fn read_request_path(stream: &mut TcpStream) -> String {
            stream
                .set_read_timeout(Some(std::time::Duration::from_secs(5)))
                .expect("set request timeout");
            let mut request = Vec::new();
            let mut chunk = [0_u8; 256];
            while !request.windows(4).any(|window| window == b"\r\n\r\n") {
                let count = stream.read(&mut chunk).expect("read HTTP request");
                assert!(count > 0, "HTTP request ended before its headers");
                request.extend_from_slice(&chunk[..count]);
                assert!(request.len() <= 16 * 1024, "HTTP test request is too large");
            }
            let line_end = request
                .windows(2)
                .position(|window| window == b"\r\n")
                .expect("HTTP request line");
            let line = std::str::from_utf8(&request[..line_end]).expect("ASCII request line");
            line.split_ascii_whitespace()
                .nth(1)
                .expect("HTTP request path")
                .to_string()
        }

        fn source(url: &str) -> String {
            format!(
                r#"
package main

import (
	"fmt"
	"io"
	"net/http"
)

func main() {{
	response, err := http.Get("{url}")
	if err != nil {{
		panic(err.Error())
	}}
	body, err := io.ReadAll(response.Body)
	if err != nil {{
		panic(err.Error())
	}}
	fmt.Print(string(body))
}}
"#
            )
        }

        fn run_captured(compiled: CompileOutput) -> Vec<u8> {
            let sink = CaptureSink::new();
            run_with_output(compiled, RunMode::Vm, Vec::new(), sink.clone())
                .expect("HTTP client VM");
            sink.take_bytes()
        }

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind local HTTP server");
        let address = listener.local_addr().expect("local HTTP address");
        let server = std::thread::spawn(move || {
            let mut requests = Vec::new();
            for _ in 0..2 {
                let (mut stream, _) = listener.accept().expect("accept HTTP client");
                let path = read_request_path(&mut stream);
                requests.push((stream, path));
            }
            for (mut stream, path) in requests {
                let body = match path.as_str() {
                    "/alpha" => b"alpha".as_slice(),
                    "/beta" => b"beta".as_slice(),
                    other => panic!("unexpected HTTP path {other}"),
                };
                write!(
                    stream,
                    "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                    body.len()
                )
                .expect("write HTTP response headers");
                stream.write_all(body).expect("write HTTP response body");
            }
        });

        let alpha = crate::compile_string(&source(&format!("http://{address}/alpha")))
            .expect("compile alpha HTTP client");
        let beta = crate::compile_string(&source(&format!("http://{address}/beta")))
            .expect("compile beta HTTP client");
        let alpha = std::thread::spawn(move || run_captured(alpha));
        let beta = std::thread::spawn(move || run_captured(beta));

        assert_eq!(alpha.join().expect("alpha VM"), b"alpha");
        assert_eq!(beta.join().expect("beta VM"), b"beta");
        server.join().expect("local HTTP server");
    }

    #[test]
    fn interrupted_vm_and_jit_release_late_http_worker_state() {
        use std::io::{Read, Write};
        use std::net::{Shutdown, TcpListener};
        use std::sync::atomic::{AtomicBool, Ordering};
        use std::sync::{mpsc, Arc};

        fn source(url: &str) -> String {
            format!(
                r#"
package main

import "net/http"

func main() {{
	response, err := http.Get("{url}")
	if err != nil {{
		panic(err.Error())
	}}
	response.Body.Close()
}}
"#
            )
        }

        for mode in [RunMode::Vm, RunMode::Jit] {
            let listener = TcpListener::bind("127.0.0.1:0").expect("bind late HTTP server");
            let address = listener.local_addr().expect("late HTTP server address");
            let interrupt = Arc::new(AtomicBool::new(false));
            let server_interrupt = interrupt.clone();
            let (request_tx, request_rx) = mpsc::channel();
            let (release_tx, release_rx) = mpsc::channel();
            let server = std::thread::spawn(move || {
                let (mut stream, _) = listener.accept().expect("accept late HTTP request");
                stream
                    .set_read_timeout(Some(std::time::Duration::from_secs(5)))
                    .expect("set late request timeout");
                let mut request = Vec::new();
                let mut chunk = [0u8; 256];
                while !request.windows(4).any(|window| window == b"\r\n\r\n") {
                    let count = stream.read(&mut chunk).expect("read late HTTP request");
                    assert!(count > 0, "late HTTP request ended before headers");
                    request.extend_from_slice(&chunk[..count]);
                }

                server_interrupt.store(true, Ordering::SeqCst);
                request_tx.send(()).expect("report late HTTP request");
                release_rx.recv().expect("release late HTTP response");
                stream
                    .write_all(
                        b"HTTP/1.1 200 OK\r\nContent-Length: 4\r\nConnection: close\r\n\r\nlate",
                    )
                    .expect("write late HTTP response");
                stream
                    .shutdown(Shutdown::Write)
                    .expect("finish late HTTP response");
                loop {
                    match stream.read(&mut chunk) {
                        Ok(0) => break,
                        Ok(_) => {}
                        Err(error) => panic!("late HTTP client did not close: {error}"),
                    }
                }
            });

            let compiled = crate::compile_string(&source(&format!("http://{address}/late")))
                .expect("compile late HTTP client");
            let mut vm = match mode {
                RunMode::Vm => Vm::new(),
                RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                    call_threshold: 1,
                    loop_threshold: 1,
                    debug_ir: false,
                    ..vo_vm::JitConfig::default()
                })
                .expect("JIT should initialize"),
            };
            vm.set_interrupt_flag(interrupt);
            vm.load_verified(compiled.module)
                .expect("late HTTP module should load");
            assert!(
                matches!(vm.run(), Err(VmError::Interrupted)),
                "mode {mode:?} should stop with its HTTP request pending"
            );
            request_rx
                .recv_timeout(std::time::Duration::from_secs(5))
                .expect("late HTTP request should reach server");
            drop(vm);

            release_tx.send(()).expect("release late HTTP worker");
            server.join().expect("late HTTP server");
        }
    }

    #[cfg(unix)]
    #[test]
    fn process_wait_yields_to_other_goroutines_in_vm_and_jit() {
        let source = r#"
package main

import (
	"fmt"
	"os/exec"
	"time"
)

func main() {
	// Warm the Cmd.Wait call path before starting the timed child. In JIT mode,
	// first-call compilation can otherwise outlive a short-lived child and make
	// Wait return immediately without exercising its I/O suspension path.
	warm := exec.Command("/usr/bin/true")
	if err := warm.Run(); err != nil {
		panic(err.Error())
	}
	cmd := exec.Command("/bin/sleep", "0.1")
	if err := cmd.Start(); err != nil {
		panic(err.Error())
	}
	tick := make(chan time.Time, 1)
	go func() {
		time.Sleep(5 * time.Millisecond)
		tick <- time.Now()
	}()
	if err := cmd.Wait(); err != nil {
		panic(err.Error())
	}
	waitDone := time.Now()
	if !(<-tick).Before(waitDone) {
		panic("process Wait blocked the VM scheduler")
	}
	fmt.Print("ok")
}
"#;

        for mode in [RunMode::Vm, RunMode::Jit] {
            let compiled = crate::compile_string(source).expect("compile async process wait");
            let mut vm = match mode {
                RunMode::Vm => Vm::new(),
                RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                    call_threshold: 1,
                    loop_threshold: 1,
                    debug_ir: false,
                    ..vo_vm::JitConfig::default()
                })
                .expect("JIT should initialize"),
            };
            let sink = CaptureSink::new();
            vm.set_output_sink(sink.clone());
            vm.load_verified(compiled.module)
                .expect("async process wait module should load");
            assert_eq!(
                vm.run().expect("async process wait should run"),
                SchedulingOutcome::Completed,
                "mode {mode:?}"
            );
            if mode == RunMode::Jit {
                let wait_io_side_exits = vm
                    .jit_execution_stats()
                    .side_exit_count(vo_vm::JitSideExitReason::WaitIo);
                assert!(
                    wait_io_side_exits >= 2,
                    "timed process wait should exercise JIT WaitIo; observed {wait_io_side_exits} side exits"
                );
            }
            assert_eq!(sink.take_bytes(), b"ok", "mode {mode:?}");
        }
    }

    fn assert_jit_runtime_trap_matches_vm(
        source: &str,
        expected_message: &str,
        expected_kind: RuntimeTrapKind,
    ) {
        let vm = vm_error_for(source, RunMode::Vm);
        let jit = vm_error_for(source, RunMode::Jit);

        let VmError::RuntimeTrap {
            kind: vm_kind,
            msg: vm_msg,
            loc: vm_loc,
        } = vm
        else {
            panic!("expected VM runtime trap, got {vm:?}");
        };
        let VmError::RuntimeTrap {
            kind: jit_kind,
            msg: jit_msg,
            loc: jit_loc,
        } = jit
        else {
            panic!("expected JIT runtime trap, got {jit:?}");
        };

        assert_eq!(vm_msg, expected_message);
        assert_eq!(jit_msg, vm_msg);
        assert_eq!(vm_kind, expected_kind);
        assert_eq!(jit_kind, expected_kind);
        assert_eq!(
            jit_loc.map(|loc| (loc.func_id, loc.pc)),
            vm_loc.map(|loc| (loc.func_id, loc.pc))
        );
        assert!(
            jit_loc.is_some(),
            "JIT trap should preserve VM error location"
        );
    }

    fn assert_jit_user_panic_matches_vm(source: &str, expected_message: &str) {
        let vm = vm_error_for(source, RunMode::Vm);
        let jit = vm_error_for(source, RunMode::Jit);

        let VmError::PanicUnwound {
            msg: vm_msg,
            loc: vm_loc,
        } = vm
        else {
            panic!("expected VM user panic, got {vm:?}");
        };
        let VmError::PanicUnwound {
            msg: jit_msg,
            loc: jit_loc,
        } = jit
        else {
            panic!("expected JIT user panic, got {jit:?}");
        };

        assert_eq!(vm_msg.as_deref(), Some(expected_message));
        assert_eq!(jit_msg, vm_msg);
        assert_eq!(
            jit_loc.map(|loc| (loc.func_id, loc.pc)),
            vm_loc.map(|loc| (loc.func_id, loc.pc))
        );
        assert!(
            jit_loc.is_some(),
            "JIT user panic should preserve VM error location"
        );
    }

    #[test]
    fn jit_extern_assert_panic_preserves_message_and_location() {
        assert_jit_user_panic_matches_vm(
            r#"
package main

func failIfZero(n int) {
	assert(n != 0, "boom")
}

func main() {
	failIfZero(0)
}
"#,
            "assertion failed: boom",
        );
    }

    #[test]
    fn jit_explicit_panic_preserves_message_and_location() {
        assert_jit_user_panic_matches_vm(
            r#"
package main

func explode() {
	panic("boom")
}

func main() {
	explode()
}
"#,
            "boom",
        );
    }

    #[test]
    fn vm_and_jit_panic_diagnostics_escape_arbitrary_string_bytes() {
        assert_jit_user_panic_matches_vm(
            r#"
package main

func main() {
	panic(string([]byte{'a', 0xff, 'z'}))
}
"#,
            "a\\xffz",
        );
    }

    #[test]
    fn jit_division_by_zero_preserves_runtime_trap_kind_message_and_location() {
        assert_jit_runtime_trap_matches_vm(
            r#"
package main

func div(x int) int {
	return 10 / x
}

func main() {
	_ = div(0)
}
"#,
            "runtime error: integer divide by zero",
            RuntimeTrapKind::DivisionByZero,
        );
    }

    #[test]
    fn jit_negative_shift_preserves_runtime_trap_kind_message_and_location() {
        assert_jit_runtime_trap_matches_vm(
            r#"
package main

func shift(x int) int {
	return 1 << x
}

func main() {
	_ = shift(-1)
}
"#,
            "runtime error: negative shift amount",
            RuntimeTrapKind::NegativeShift,
        );
    }

    #[test]
    fn jit_bounds_check_preserves_runtime_trap_kind_message_and_location() {
        assert_jit_runtime_trap_matches_vm(
            r#"
package main

func get(s []int) int {
	return s[3]
}

func main() {
	s := []int{1, 2}
	_ = get(s)
}
"#,
            "runtime error: index out of range [3] with length 2",
            RuntimeTrapKind::IndexOutOfBounds,
        );
    }

    #[test]
    fn jit_nil_map_write_preserves_runtime_trap_kind_message_and_location() {
        assert_jit_runtime_trap_matches_vm(
            r#"
package main

func put(m map[string]int) {
	m["x"] = 1
}

func main() {
	var m map[string]int
	put(m)
}
"#,
            "runtime error: assignment to entry in nil map",
            RuntimeTrapKind::NilMapWrite,
        );
    }

    #[test]
    fn jit_type_assertion_preserves_runtime_trap_kind_message_and_location() {
        assert_jit_runtime_trap_matches_vm(
            r#"
package main

func asInt(v any) int {
	return v.(int)
}

func main() {
	_ = asInt("not an int")
}
"#,
            "runtime error: interface conversion: interface is nil, not",
            RuntimeTrapKind::TypeAssertionFailed,
        );
    }

    #[test]
    fn jit_interface_eq_preserves_runtime_trap_kind_message_and_location() {
        assert_jit_runtime_trap_matches_vm(
            r#"
package main

func eq(a any, b any) bool {
	return a == b
}

func main() {
	s := []int{1}
	_ = eq(s, s)
}
"#,
            "runtime error: comparing uncomparable type in interface value",
            RuntimeTrapKind::UncomparableType,
        );
    }

    #[test]
    fn jit_map_hash_preserves_runtime_trap_kind_message_and_location() {
        assert_jit_runtime_trap_matches_vm(
            r#"
package main

func put(m map[any]int, k any) {
	m[k] = 1
}

func main() {
	m := make(map[any]int)
	k := []int{1}
	put(m, k)
}
"#,
            "runtime error: hash of unhashable type",
            RuntimeTrapKind::UnhashableType,
        );
    }

    #[test]
    fn jit_queue_callback_preserves_runtime_trap_kind_message_and_location() {
        assert_jit_runtime_trap_matches_vm(
            r#"
package main

func sendClosed(ch chan int) {
	close(ch)
	ch <- 1
}

func main() {
	ch := make(chan int, 1)
	sendClosed(ch)
}
"#,
            "runtime error: send on closed channel",
            RuntimeTrapKind::SendOnClosedChannel,
        );
    }

    #[test]
    fn jit_make_slice_negative_len_preserves_runtime_trap_kind_message_and_location() {
        assert_jit_runtime_trap_matches_vm(
            r#"
package main

func mk(n int) []int {
	return make([]int, n)
}

func main() {
	_ = mk(-1)
}
"#,
            "runtime error: makeslice: len out of range",
            RuntimeTrapKind::MakeSlice,
        );
    }

    #[test]
    fn jit_make_slice_len_larger_than_cap_preserves_runtime_trap_kind_message_and_location() {
        assert_jit_runtime_trap_matches_vm(
            r#"
package main

func mk(n int, c int) []int {
	return make([]int, n, c)
}

func main() {
	_ = mk(2, 1)
}
"#,
            "runtime error: makeslice: len larger than cap",
            RuntimeTrapKind::MakeSlice,
        );
    }

    #[test]
    fn jit_make_chan_negative_size_preserves_runtime_trap_kind_message_and_location() {
        assert_jit_runtime_trap_matches_vm(
            r#"
package main

func mk(n int) chan int {
	return make(chan int, n)
}

func main() {
	_ = mk(-1)
}
"#,
            "runtime error: makechan: size out of range",
            RuntimeTrapKind::MakeChan,
        );
    }

    #[test]
    fn jit_make_port_negative_size_preserves_runtime_trap_kind_message_and_location() {
        assert_jit_runtime_trap_matches_vm(
            r#"
package main

func mk(n int) port int {
	return make(port int, n)
}

func main() {
	_ = mk(-1)
}
"#,
            "runtime error: makeport: size out of range",
            RuntimeTrapKind::MakePort,
        );
    }

    #[test]
    fn jit_float_to_int_edges_match_vm_output() {
        let source = r#"
package main

import "math"

func conv(x float64) int {
	return int(x)
}

func main() {
	println(conv(math.NaN()))
	println(conv(math.Inf(1)))
	println(conv(math.Inf(-1)))
	println(conv(1e300))
	println(conv(-1e300))
	println(conv(3.9))
	println(conv(-3.9))
}
"#;
        let config = vo_vm::JitConfig {
            call_threshold: 1,
            loop_threshold: 1_000_000,
            debug_ir: false,
            ..vo_vm::JitConfig::default()
        };
        let (vm_out, _) = output_for(source, RunMode::Vm, config.clone());
        let (jit_out, observation) = output_for(source, RunMode::Jit, config);

        assert_eq!(jit_out, vm_out);
        assert!(
            observation.function_entries > 0,
            "test must execute full-function JIT code"
        );
    }

    #[test]
    fn prepared_shadow_closure_preserves_ptr_trap_and_recover() {
        let source = r#"
package main

type Box struct {
	value int
}

func invoke(f func() int) int {
	return f()
}

func main() {
	var cell *Box = &Box{value: 7}
	f := func() int {
		return cell.value
	}
	println(invoke(f))

	cell = nil
	recovered := false
	func() {
		defer func() {
			if recover() != nil {
				recovered = true
			}
		}()
		_ = invoke(f)
	}()
	println(recovered)
}
"#;
        let config = vo_vm::JitConfig {
            call_threshold: 1,
            loop_threshold: 1_000_000,
            debug_ir: false,
            ..vo_vm::JitConfig::default()
        };
        let (vm_out, _) = output_for(source, RunMode::Vm, config.clone());
        let (jit_out, observation) = output_for(source, RunMode::Jit, config);

        assert_eq!(vm_out, "7\ntrue\n");
        assert_eq!(jit_out, vm_out);
        assert!(
            observation.function_entries > 0,
            "test must execute full-function JIT code"
        );
    }

    #[test]
    fn vm_jit_trampoline_select_017_compile_path_runs_pending_spawn_select_wake() {
        let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../..");
        let case_path = repo_root.join("tests/lang/cases/jit/2026_01_29_jit_trampoline_select.vo");
        let compiled = crate::compile(
            case_path
                .to_str()
                .expect("trampoline select case path should be valid utf-8"),
        )
        .expect("trampoline select case should compile");
        let mut vm = Vm::try_with_jit_config(vo_vm::JitConfig {
            call_threshold: 1,
            loop_threshold: 50,
            debug_ir: false,
            ..vo_vm::JitConfig::default()
        })
        .expect("JIT should initialize");
        let sink = CaptureSink::new();
        vm.set_output_sink(sink.clone());
        vm.load_verified(compiled.module)
            .expect("module should load");

        let outcome = vm.run().expect("program should run");

        assert_ne!(
            outcome,
            SchedulingOutcome::Blocked,
            "JIT full-function select side exit must preserve pending goroutine spawns and queue wakes"
        );
        assert_eq!(
            sink.take(),
            "Test 6: PASSED - select from goroutines\ndone\n"
        );
        let observation = run_observation(&vm);
        assert!(
            observation.function_entries > 0,
            "proof must execute full-function JIT code"
        );
    }

    #[test]
    fn vm_jit_select_source_index_017_default_middle_recv_reloads_selected_value() {
        let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../..");
        let case_path = repo_root
            .join("tests/lang/cases/jit/2026_02_18_jit_select_default_middle_recv_value.vo");
        let compiled = crate::compile_with_auto_install(
            case_path
                .to_str()
                .expect("select source-index case path should be valid utf-8"),
        )
        .expect("select source-index case should compile");
        let sink = CaptureSink::new();
        let _env_guard = PROCESS_ENV_LOCK
            .get_or_init(|| Mutex::new(()))
            .lock()
            .expect("process env lock poisoned");
        let _call_threshold = ScopedEnvVar::set("VO_JIT_CALL_THRESHOLD", "1");
        let _loop_threshold = ScopedEnvVar::set("VO_JIT_LOOP_THRESHOLD", "50");
        let result = run_with_output_observed(compiled, RunMode::Jit, Vec::new(), sink.clone());

        let observation = result.expect("program should run");
        assert_eq!(sink.take(), "jit select default middle recv value ok\n");
        assert!(
            observation.function_entries > 0,
            "proof must execute full-function JIT code"
        );
    }

    #[test]
    fn strict_jit_extern_not_registered_fails_fast() {
        let compiled = crate::compile_string(
            r#"
package main

import "fmt"

func callPrint() {
	fmt.Println("hello")
}

func main() {
	callPrint()
}
"#,
        )
        .expect("source should compile");
        let unregistered =
            vo_common_core::ExternKeyRef::new("github.com/acme/unregistered", "Missing")
                .encode()
                .expect("unregistered extern fixture must use the canonical codec");
        let mut module = compiled.module.module().clone();
        module.externs.push(ExternDef::new(
            unregistered,
            ParamShape::Exact { slots: 0 },
            ReturnShape::slots(0),
            ExternEffects::NONE,
            Vec::new(),
        ));
        let mut vm = Vm::try_with_jit_config(vo_vm::JitConfig {
            call_threshold: 1,
            loop_threshold: 1_000_000,
            debug_ir: false,
            ..vo_vm::JitConfig::default()
        })
        .expect("JIT should initialize");
        vm.set_output_sink(CaptureSink::new());

        let err = match vm.load(module) {
            Err(err) => err,
            Ok(()) => panic!("expected JIT extern registration error during load"),
        };

        let VmError::Jit(msg) = err else {
            panic!("expected strict JIT extern registration error, got {err:?}");
        };
        assert!(msg.contains("extern function"), "{msg}");
        assert!(msg.contains("no provider registered"), "{msg}");
        assert!(!msg.contains("JIT panic"), "{msg}");
    }

    #[test]
    fn common_verifier_rejects_full_jit_metadata_drift_before_execution() {
        let compiled = crate::compile_string(
            r#"
package main

func hot(x int) int {
	return x + 1
}

func main() {
	_ = hot(41)
}
"#,
        )
        .expect("source should compile");
        let mut module = compiled.module.module().clone();
        let func = module
            .functions
            .iter_mut()
            .find(|func| func.name.ends_with("hot"))
            .expect("hot function");
        let return_pc = func
            .code
            .iter()
            .position(|inst| inst.opcode() == Opcode::Return)
            .expect("return pc");
        func.instruction_metadata[return_pc] = InstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Value],
        };

        let err = vm_error_for_module(
            module,
            vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1_000_000,
                debug_ir: false,
                ..vo_vm::JitConfig::default()
            },
        );

        let VmError::Jit(msg) = err else {
            panic!("expected common verifier error, got {err:?}");
        };
        assert!(msg.contains("invalid module metadata"), "{msg}");
        assert!(
            msg.contains("wrong instruction metadata kind MapDelete for Return"),
            "{msg}"
        );
        assert!(msg.contains("hot"), "{msg}");
    }

    #[test]
    fn common_verifier_rejects_invalid_loop_metadata_before_osr() {
        let compiled = crate::compile_string(
            r#"
package main

func loopHot(n int) int {
	sum := 0
	for i := 0; i < n; i++ {
		sum += i
	}
	return sum
}

func main() {
	_ = loopHot(20)
}
"#,
        )
        .expect("source should compile");
        let mut module = compiled.module.module().clone();
        let func = module
            .functions
            .iter_mut()
            .find(|func| func.name.ends_with("loopHot"))
            .expect("loopHot function");
        let hint_pc = func
            .code
            .iter()
            .position(|inst| inst.opcode() == Opcode::Hint && inst.flags == HINT_LOOP)
            .expect("loop hint pc");
        func.instruction_metadata[hint_pc] = InstructionMetadata::LoopEnd {
            end_pc: hint_pc as u32,
        };

        let err = vm_error_for_module(
            module,
            vo_vm::JitConfig {
                call_threshold: 1_000_000,
                loop_threshold: 1,
                debug_ir: false,
                ..vo_vm::JitConfig::default()
            },
        );

        let VmError::Jit(msg) = err else {
            panic!("expected common verifier error, got {err:?}");
        };
        assert!(msg.contains("invalid module metadata"), "{msg}");
        assert!(msg.contains("LoopEnd"), "{msg}");
        assert!(msg.contains("loopHot"), "{msg}");
    }

    #[test]
    fn common_verifier_rejects_dynamic_callee_metadata_drift_before_precompile() {
        let compiled = crate::compile_string(
            r#"
package main

func target(x int) int {
	return x + 1
}

func call(fn func(int) int) int {
	return fn(41)
}

func main() {
	_ = call(target)
}
"#,
        )
        .expect("source should compile");
        let mut module = compiled.module.module().clone();
        let func = module
            .functions
            .iter_mut()
            .find(|func| func.name.ends_with("target"))
            .expect("target function");
        let return_pc = func
            .code
            .iter()
            .position(|inst| inst.opcode() == Opcode::Return)
            .expect("return pc");
        func.instruction_metadata[return_pc] = InstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Value],
        };

        let err = vm_error_for_module(
            module,
            vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1_000_000,
                debug_ir: false,
                ..vo_vm::JitConfig::default()
            },
        );

        let VmError::Jit(msg) = err else {
            panic!("expected common verifier error, got {err:?}");
        };
        assert!(msg.contains("invalid module metadata"), "{msg}");
        assert!(
            msg.contains("wrong instruction metadata kind MapDelete for Return"),
            "{msg}"
        );
        assert!(msg.contains("target"), "{msg}");
    }
}

#[cfg(all(test, not(feature = "jit")))]
mod no_jit_tests {
    use super::*;

    use vo_runtime::output::CaptureSink;

    #[test]
    fn jit_mode_without_jit_feature_fails_fast() {
        let compiled = crate::compile_string(
            r#"
package main

func main() {
	println("should not run")
}
"#,
        )
        .expect("source should compile");

        let err = run_with_output_observed(compiled, RunMode::Jit, Vec::new(), CaptureSink::new())
            .expect_err("RunMode::Jit must fail when jit feature is disabled");
        let RunError::Runtime(runtime) = err else {
            panic!("expected runtime error, got {err:?}");
        };
        assert_eq!(runtime.kind, RuntimeErrorKind::Other);
        assert!(
            runtime
                .message
                .contains("JIT mode requested but vo-engine was built without the jit feature"),
            "{}",
            runtime.message
        );
    }
}
