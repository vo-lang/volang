//! Execution functions for Vo modules.

use std::fmt;
use std::sync::{atomic::AtomicBool, Arc};

use vo_common_core::debug_info::SourceLoc;
use vo_runtime::ext_loader::{ExtensionLoader, NativeExtensionSpec};
use vo_runtime::output::{OutputSink, StdoutSink};
use vo_vm::bytecode::Module;
use vo_vm::vm::{RuntimeTrapKind, SchedulingOutcome, Vm, VmError};

use crate::compile::{CompileError, CompileOutput};

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
        inline_frames: Vec::new(),
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
        "optimizing_compilations": observation.optimizing_compilations,
        "optimizing_functions_executed": observation.optimizing_functions_executed,
        "optimizing_failures": observation.optimizing_failures,
        "deopts": observation.deopts,
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
    /// Leaf-to-caller source frames when optimization removed physical frames.
    /// Owned independently of the module; empty for an ordinary physical frame.
    pub inline_frames: Vec<vo_common_core::debug_info::ResolvedSourceFrame>,
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
                .and_then(|l| module.debug_info.lookup(l.func_id(), l.pc()))
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
            inline_frames: e
                .source_location()
                .map(|location| location.resolve_inline_frames(module))
                .unwrap_or_default(),
            kind,
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(loc) = &self.location {
            write!(f, "{}:{}: {}", loc.file, loc.line, self.message)?;
        } else {
            f.write_str(&self.message)?;
        }
        for (index, frame) in self.inline_frames.iter().enumerate() {
            let relation = if index == 0 { "at" } else { "inlined in" };
            write!(f, "\n  {relation} {frame}")?;
        }
        Ok(())
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
            RunError::Runtime(e) => write!(f, "{e}"),
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
    crate::Engine::default().run(compiled, mode, args)
}

/// Run with arbitrary-byte program arguments.
pub fn run_with_byte_args(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<Vec<u8>>,
) -> Result<(), RunError> {
    crate::Engine::default().run_with_byte_args(compiled, mode, args)
}

pub fn run_with_byte_args_and_memory(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<Vec<u8>>,
    memory_config: vo_vm::VmMemoryConfig,
) -> Result<(), RunError> {
    crate::Engine::default().run_with_byte_args_and_memory(compiled, mode, args, memory_config)
}

pub fn run_with_byte_args_and_memory_observed(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<Vec<u8>>,
    memory_config: vo_vm::VmMemoryConfig,
) -> Result<RunObservation, RunError> {
    crate::Engine::default().run_with_byte_args_and_memory_observed(
        compiled,
        mode,
        args,
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
    crate::Engine::default().run_with_output(compiled, mode, args, sink)
}

pub fn run_with_output_observed(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<String>,
    sink: Arc<dyn OutputSink>,
) -> Result<RunObservation, RunError> {
    crate::Engine::default().run_with_output_observed(compiled, mode, args, sink)
}

pub fn run_with_output_interruptible(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<String>,
    sink: Arc<dyn OutputSink>,
    interrupt_flag: Option<Arc<AtomicBool>>,
) -> Result<(), RunError> {
    crate::Engine::default().run_with_output_interruptible(
        compiled,
        mode,
        args,
        sink,
        interrupt_flag,
    )
}

pub fn run_with_output_interruptible_observed(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<String>,
    sink: Arc<dyn OutputSink>,
    interrupt_flag: Option<Arc<AtomicBool>>,
) -> Result<RunObservation, RunError> {
    crate::Engine::default().run_with_output_interruptible_observed(
        compiled,
        mode,
        args,
        sink,
        interrupt_flag,
    )
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
            inline_frames: Vec::new(),
            kind: RuntimeErrorKind::Other,
        })),
        SchedulingOutcome::SuspendedForHostEvents => Err(RunError::Runtime(RuntimeError {
            message: "execution suspended for host events; continue it through an async VM session"
                .to_string(),
            location: None,
            inline_frames: Vec::new(),
            kind: RuntimeErrorKind::Other,
        })),
        SchedulingOutcome::Panicked => Err(RunError::Runtime(RuntimeError {
            message: "VM reported a panic outcome without a structured runtime error".to_string(),
            location: None,
            inline_frames: Vec::new(),
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
            inline_frames: Vec::new(),
            kind: RuntimeErrorKind::Other,
        });
    RunError::Runtime(runtime_err)
}

/// Create an unloaded VM with the same memory and JIT policy used by execution.
/// Hosts register providers before loading a verified module into this VM.
pub fn new_vm_for_mode(
    memory_config: vo_vm::VmMemoryConfig,
    mode: RunMode,
) -> Result<Vm, RunError> {
    #[cfg(feature = "jit")]
    let vm = match mode {
        RunMode::Vm => Vm::try_with_memory_config(memory_config).map_err(|err| {
            RunError::Runtime(RuntimeError {
                message: format!("VM initialization failed: {err}"),
                location: None,
                inline_frames: Vec::new(),
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
                    inline_frames: Vec::new(),
                    kind: RuntimeErrorKind::Other,
                })
            })?
        }
    };

    #[cfg(not(feature = "jit"))]
    let vm = {
        if mode == RunMode::Jit {
            return Err(RunError::Runtime(RuntimeError {
                message: "JIT mode requested but vo-engine was built without the jit feature"
                    .to_string(),
                location: None,
                inline_frames: Vec::new(),
                kind: RuntimeErrorKind::Other,
            }));
        }
        Vm::try_with_memory_config(memory_config).map_err(|err| {
            RunError::Runtime(RuntimeError {
                message: format!("VM initialization failed: {err}"),
                location: None,
                inline_frames: Vec::new(),
                kind: RuntimeErrorKind::Other,
            })
        })?
    };

    Ok(vm)
}

pub fn load_extensions(specs: &[NativeExtensionSpec]) -> Result<Option<ExtensionLoader>, RunError> {
    if specs.is_empty() {
        return Ok(None);
    }

    let loader = ExtensionLoader::from_specs(specs).map_err(|e| {
        RunError::Runtime(RuntimeError {
            message: format!("failed to load extensions: {}", e),
            location: None,
            inline_frames: Vec::new(),
            kind: RuntimeErrorKind::Other,
        })
    })?;
    Ok(Some(loader))
}

impl crate::Engine {
    /// Run a compiled module with output to stdout.
    pub fn run(
        &self,
        compiled: CompileOutput,
        mode: RunMode,
        args: Vec<String>,
    ) -> Result<(), RunError> {
        self.run_with_output(compiled, mode, args, Arc::new(StdoutSink))
    }

    /// Run with arbitrary-byte program arguments.
    pub fn run_with_byte_args(
        &self,
        compiled: CompileOutput,
        mode: RunMode,
        args: Vec<Vec<u8>>,
    ) -> Result<(), RunError> {
        self.run_with_output_interruptible_observed_bytes(
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
        &self,
        compiled: CompileOutput,
        mode: RunMode,
        args: Vec<Vec<u8>>,
        memory_config: vo_vm::VmMemoryConfig,
    ) -> Result<(), RunError> {
        self.run_with_byte_args_and_memory_observed(compiled, mode, args, memory_config)
            .map(|_| ())
    }

    pub fn run_with_byte_args_and_memory_observed(
        &self,
        compiled: CompileOutput,
        mode: RunMode,
        args: Vec<Vec<u8>>,
        memory_config: vo_vm::VmMemoryConfig,
    ) -> Result<RunObservation, RunError> {
        self.run_with_output_interruptible_observed_bytes(
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
        &self,
        compiled: CompileOutput,
        mode: RunMode,
        args: Vec<String>,
        sink: Arc<dyn OutputSink>,
    ) -> Result<(), RunError> {
        self.run_with_output_interruptible(compiled, mode, args, sink, None)
    }

    pub fn run_with_output_observed(
        &self,
        compiled: CompileOutput,
        mode: RunMode,
        args: Vec<String>,
        sink: Arc<dyn OutputSink>,
    ) -> Result<RunObservation, RunError> {
        self.run_with_output_interruptible_observed(compiled, mode, args, sink, None)
    }

    pub fn run_with_output_interruptible(
        &self,
        compiled: CompileOutput,
        mode: RunMode,
        args: Vec<String>,
        sink: Arc<dyn OutputSink>,
        interrupt_flag: Option<Arc<AtomicBool>>,
    ) -> Result<(), RunError> {
        self.run_with_output_interruptible_observed(compiled, mode, args, sink, interrupt_flag)
            .map(|_| ())
    }

    pub fn run_with_output_interruptible_observed(
        &self,
        compiled: CompileOutput,
        mode: RunMode,
        args: Vec<String>,
        sink: Arc<dyn OutputSink>,
        interrupt_flag: Option<Arc<AtomicBool>>,
    ) -> Result<RunObservation, RunError> {
        self.run_with_output_interruptible_observed_bytes(
            compiled,
            mode,
            args.into_iter().map(String::into_bytes).collect(),
            sink,
            interrupt_flag,
            vo_vm::VmMemoryConfig::default(),
        )
    }

    fn run_with_output_interruptible_observed_bytes(
        &self,
        compiled: CompileOutput,
        mode: RunMode,
        args: Vec<Vec<u8>>,
        sink: Arc<dyn OutputSink>,
        interrupt_flag: Option<Arc<AtomicBool>>,
        memory_config: vo_vm::VmMemoryConfig,
    ) -> Result<RunObservation, RunError> {
        self.ensure_toolchain_host_installed();
        let CompileOutput {
            module,
            source_root: _,
            extensions,
            locked_modules: _,
        } = compiled;
        let ext_loader = load_extensions(&extensions)?;

        let mut vm = new_vm_for_mode(memory_config, mode)?;

        vm.set_output_sink(sink);
        vm.set_program_args_bytes(args);
        if let Some(interrupt_flag) = interrupt_flag {
            vm.set_interrupt_flag(interrupt_flag);
        }
        self.register_externs(&mut vm, &module).map_err(|message| {
            RunError::Runtime(RuntimeError {
                message,
                location: None,
                inline_frames: Vec::new(),
                kind: RuntimeErrorKind::Other,
            })
        })?;
        vm.load_verified_with_extensions(module, ext_loader)
            .map_err(|e| vm_err_to_run_err(&vm, &e))?;

        let outcome = vm.run().map_err(|e| vm_err_to_run_err(&vm, &e))?;
        require_terminal_outcome(&vm, outcome)?;
        Ok(run_observation(&vm))
    }
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
#[path = "run_tests.rs"]
mod tests;

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
