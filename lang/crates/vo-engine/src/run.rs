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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct RunObservation {
    pub jit_function_entries: u64,
    pub jit_loop_entries: u64,
    pub jit_regular_call_side_exits: u64,
    pub jit_function_code_bytes: usize,
    pub jit_loop_code_bytes: usize,
    pub jit_unsupported_functions: usize,
}

impl RunObservation {
    pub fn executed_jit_code(self) -> bool {
        self.jit_function_entries > 0 || self.jit_loop_entries > 0
    }
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
    run_with_output_interruptible_observed_bytes(compiled, mode, args, Arc::new(StdoutSink), None)
        .map(|_| ())
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
    )
}

fn run_with_output_interruptible_observed_bytes(
    compiled: CompileOutput,
    mode: RunMode,
    args: Vec<Vec<u8>>,
    sink: Arc<dyn OutputSink>,
    interrupt_flag: Option<Arc<AtomicBool>>,
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
        RunMode::Vm => Vm::try_new().map_err(|err| {
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
            let debug_ir = jit_env_bool("VO_JIT_DEBUG", false)?;

            let config = JitConfig {
                call_threshold,
                loop_threshold,
                debug_ir,
            };
            Vm::try_with_jit_config(config).map_err(|err| {
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
        Vm::try_new().map_err(|err| {
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
    vm.load_with_extensions(module, ext_loader)
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

#[cfg(feature = "jit")]
fn run_observation(vm: &Vm) -> RunObservation {
    let stats = vm.jit_execution_stats();
    let code = vm.jit_code_memory_stats();
    RunObservation {
        jit_function_entries: stats.function_entries,
        jit_loop_entries: stats.loop_entries,
        jit_regular_call_side_exits: stats.side_exit_count(vo_vm::JitSideExitReason::RegularCall),
        jit_function_code_bytes: code.function_bytes,
        jit_loop_code_bytes: code.loop_bytes,
        jit_unsupported_functions: vm.jit_unsupported_function_count(),
    }
}

#[cfg(not(feature = "jit"))]
fn run_observation(_vm: &Vm) -> RunObservation {
    RunObservation::default()
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
    ensure_toolchain_host_installed();
    let ext_loader = load_extensions(&compiled.extensions).map_err(|e| e.to_string())?;
    let mut vm = Vm::try_new().map_err(|error| format!("failed to initialize VM: {error}"))?;
    vm.enable_external_island_transport();
    vm.load_with_extensions(compiled.module, ext_loader)
        .map_err(|e| format!("{:?}", e))?;
    Ok(vm)
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
}

#[cfg(all(test, feature = "jit"))]
mod tests {
    use super::*;

    use std::sync::{Mutex, OnceLock};
    use vo_common_core::instruction::HINT_LOOP;
    use vo_runtime::bytecode::{
        ExternDef, ExternEffects, JitInstructionMetadata, ParamShape, ReturnShape,
    };
    use vo_runtime::instruction::Opcode;
    use vo_runtime::output::CaptureSink;
    use vo_runtime::SlotType;

    static PROCESS_ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    static GUEST_EXIT_SUBPROCESS_LOCK: Mutex<()> = Mutex::new(());
    const GUEST_EXIT_SUBPROCESS_ENV: &str = "VOLANG_GUEST_EXIT_SUBPROCESS";
    const GUEST_EXIT_SUBPROCESS_MARKER: &str = "volang-guest-exit-subprocess-started";
    // The first JIT execution compiles guest and island code inside `Vm::run`.
    // Leave ample headroom for debug CI while retaining a real hard bound:
    // the parent can terminate a child even if it is blocked in `JoinHandle::join`.
    const GUEST_EXIT_SUBPROCESS_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(30);

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
            })
            .expect("JIT should initialize"),
        };
        vm.set_output_sink(CaptureSink::new());
        if let Err(err) = vm.load(compiled.module) {
            return err;
        }
        match vm.run() {
            Err(err) => err,
            Ok(outcome) => panic!("expected runtime error, VM returned {outcome:?}"),
        }
    }

    fn vm_error_for_compiled(compiled: CompileOutput, config: vo_vm::JitConfig) -> VmError {
        let mut vm = Vm::try_with_jit_config(config).expect("JIT should initialize");
        vm.set_output_sink(CaptureSink::new());
        if let Err(err) = vm.load(compiled.module) {
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
        vm.load(compiled.module).unwrap();
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
                })
                .expect("JIT should initialize"),
            };
            let sink = CaptureSink::new();
            vm.set_output_sink(sink.clone());
            vm.load(compiled.module).expect("module should load");

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
                        })
                        .expect("JIT should initialize"),
                    };
                    vm.load(compiled.module).expect("module should load");

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
                        })
                        .expect("JIT should initialize"),
                    };
                    vm.load(compiled.module).expect("module should load");

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
                        })
                        .expect("JIT should initialize"),
                    };
                    vm.set_program_args(vec!["main".to_string()]);
                    vm.load(compiled.module).expect("module should load");

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
                })
                .expect("JIT should initialize"),
            };
            let sink = CaptureSink::new();
            vm.set_output_sink(sink.clone());
            vm.set_program_args_bytes(vec![b"p\xfe".to_vec(), Vec::new()]);
            vm.load(compiled.module).expect("module should load");
            let outcome = vm.run().expect("program should run");
            assert_eq!(outcome, SchedulingOutcome::Completed);
            assert_eq!(sink.take_bytes(), expected, "mode {mode:?}");
            if mode == RunMode::Jit {
                assert!(run_observation(&vm).jit_function_entries > 0);
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
                })
                .expect("JIT should initialize"),
            };
            let sink = CaptureSink::new();
            vm.set_output_sink(sink.clone());
            vm.set_program_args_bytes(vec![root.0.as_os_str().as_encoded_bytes().to_vec()]);
            vm.load(compiled.module).expect("DirFS module should load");
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
                })
                .expect("JIT should initialize"),
            };
            vm.set_interrupt_flag(interrupt);
            vm.load(compiled.module)
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
                })
                .expect("JIT should initialize"),
            };
            let sink = CaptureSink::new();
            vm.set_output_sink(sink.clone());
            vm.load(compiled.module)
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
        };
        let (vm_out, _) = output_for(source, RunMode::Vm, config.clone());
        let (jit_out, observation) = output_for(source, RunMode::Jit, config);

        assert_eq!(jit_out, vm_out);
        assert!(
            observation.jit_function_entries > 0,
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
        })
        .expect("JIT should initialize");
        let sink = CaptureSink::new();
        vm.set_output_sink(sink.clone());
        vm.load(compiled.module).expect("module should load");

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
            observation.jit_function_entries > 0,
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
            observation.jit_function_entries > 0,
            "proof must execute full-function JIT code"
        );
    }

    #[test]
    fn vm_jit_threshold_env_tests_use_process_env_lock_048() {
        let source = include_str!("run.rs");
        assert!(
            source.contains("static PROCESS_ENV_LOCK"),
            "tests that mutate process env must share a lock"
        );
        assert!(
            source.contains("let _env_guard = PROCESS_ENV_LOCK"),
            "JIT threshold tests must hold the process env lock while VO_JIT_* vars are overridden"
        );
    }

    #[test]
    fn strict_jit_extern_not_registered_fails_fast() {
        let mut compiled = crate::compile_string(
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
        compiled.module.externs.push(ExternDef::new(
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
        })
        .expect("JIT should initialize");
        vm.set_output_sink(CaptureSink::new());

        let err = match vm.load(compiled.module) {
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
    fn strict_jit_full_compile_invalid_metadata_fails_fast() {
        let mut compiled = crate::compile_string(
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
        let func = compiled
            .module
            .functions
            .iter_mut()
            .find(|func| func.name.ends_with("hot"))
            .expect("hot function");
        let return_pc = func
            .code
            .iter()
            .position(|inst| inst.opcode() == Opcode::Return)
            .expect("return pc");
        func.jit_metadata[return_pc] = JitInstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Value],
        };

        let err = vm_error_for_compiled(
            compiled,
            vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1_000_000,
                debug_ir: false,
            },
        );

        let VmError::Jit(msg) = err else {
            panic!("expected strict JIT error, got {err:?}");
        };
        assert!(msg.contains("invalid JIT metadata"), "{msg}");
        assert!(msg.contains("hot"), "{msg}");
    }

    #[test]
    fn strict_jit_osr_loop_analysis_error_fails_fast() {
        let mut compiled = crate::compile_string(
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
        let func = compiled
            .module
            .functions
            .iter_mut()
            .find(|func| func.name.ends_with("loopHot"))
            .expect("loopHot function");
        let hint_pc = func
            .code
            .iter()
            .position(|inst| inst.opcode() == Opcode::Hint && inst.flags == HINT_LOOP)
            .expect("loop hint pc");
        func.jit_metadata[hint_pc] = JitInstructionMetadata::LoopEnd {
            end_pc: hint_pc as u32,
        };

        let err = vm_error_for_compiled(
            compiled,
            vo_vm::JitConfig {
                call_threshold: 1_000_000,
                loop_threshold: 1,
                debug_ir: false,
            },
        );

        let VmError::Jit(msg) = err else {
            panic!("expected strict OSR JIT error, got {err:?}");
        };
        assert!(msg.contains("invalid JIT metadata"), "{msg}");
        assert!(msg.contains("LoopEnd"), "{msg}");
        assert!(msg.contains("loopHot"), "{msg}");
    }

    #[test]
    fn strict_jit_dynamic_callee_precompile_error_fails_fast() {
        let mut compiled = crate::compile_string(
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
        let func = compiled
            .module
            .functions
            .iter_mut()
            .find(|func| func.name.ends_with("target"))
            .expect("target function");
        let return_pc = func
            .code
            .iter()
            .position(|inst| inst.opcode() == Opcode::Return)
            .expect("return pc");
        func.jit_metadata[return_pc] = JitInstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Value],
        };

        let err = vm_error_for_compiled(
            compiled,
            vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1_000_000,
                debug_ir: false,
            },
        );

        let VmError::Jit(msg) = err else {
            panic!("expected strict dynamic callee JIT error, got {err:?}");
        };
        assert!(msg.contains("invalid JIT metadata"), "{msg}");
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
