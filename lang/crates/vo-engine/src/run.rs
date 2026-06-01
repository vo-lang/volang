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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct RunObservation {
    pub jit_function_entries: u64,
    pub jit_loop_entries: u64,
    pub jit_regular_call_fallbacks: u64,
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
        Vm::new()
    };

    vm.state.output = sink;
    vm.set_program_args(args);
    if let Some(interrupt_flag) = interrupt_flag {
        vm.set_interrupt_flag(interrupt_flag);
    }
    vm.load_with_extensions(module, ext_loader)
        .map_err(|e| vm_err_to_run_err(&vm, &e))?;

    let outcome = vm.run().map_err(|e| vm_err_to_run_err(&vm, &e))?;
    if outcome == SchedulingOutcome::Blocked {
        let e = vm.deadlock_err();
        return Err(vm_err_to_run_err(&vm, &e));
    }
    Ok(run_observation(&vm))
}

#[cfg(feature = "jit")]
fn run_observation(vm: &Vm) -> RunObservation {
    let stats = vm
        .jit_mgr
        .as_ref()
        .map(|mgr| mgr.execution_stats())
        .unwrap_or_default();
    RunObservation {
        jit_function_entries: stats.function_entries,
        jit_loop_entries: stats.loop_entries,
        jit_regular_call_fallbacks: stats
            .fallback_count(vo_vm::vm::jit_mgr::JitFallbackReason::RegularCall),
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
    let mut vm = Vm::new();
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

#[cfg(all(test, feature = "jit"))]
mod tests {
    use super::*;

    use vo_common_core::instruction::HINT_LOOP;
    use vo_runtime::bytecode::JitInstructionMetadata;
    use vo_runtime::instruction::Opcode;
    use vo_runtime::output::CaptureSink;
    use vo_runtime::SlotType;

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
        vm.state.output = CaptureSink::new();
        vm.load(compiled.module).unwrap();
        match vm.run() {
            Err(err) => err,
            Ok(outcome) => panic!("expected runtime error, VM returned {outcome:?}"),
        }
    }

    fn vm_error_for_compiled(compiled: CompileOutput, config: vo_vm::JitConfig) -> VmError {
        let mut vm = Vm::try_with_jit_config(config).expect("JIT should initialize");
        vm.state.output = CaptureSink::new();
        vm.load(compiled.module).unwrap();
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
        vm.state.output = sink.clone();
        vm.load(compiled.module).unwrap();
        let outcome = vm.run().expect("program should run");
        assert_ne!(
            outcome,
            SchedulingOutcome::Blocked,
            "program should not block"
        );
        (sink.take(), run_observation(&vm))
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
        let mut vm = Vm::try_with_jit_config(vo_vm::JitConfig {
            call_threshold: 1,
            loop_threshold: 1_000_000,
            debug_ir: false,
        })
        .expect("JIT should initialize");
        vm.state.output = CaptureSink::new();
        vm.load(compiled.module).unwrap();
        vm.state.extern_registry = vo_runtime::ExternRegistry::new();

        let err = match vm.run() {
            Err(err) => err,
            Ok(outcome) => panic!("expected JIT extern error, VM returned {outcome:?}"),
        };

        let VmError::Jit(msg) = err else {
            panic!("expected strict JIT extern registration error, got {err:?}");
        };
        assert!(msg.contains("extern function"), "{msg}");
        assert!(msg.contains("not registered"), "{msg}");
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
        assert!(msg.contains("JIT OSR compilation failed"), "{msg}");
        assert!(msg.contains("loop analysis failed"), "{msg}");
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
        assert!(
            msg.contains("prepared dynamic callee compilation")
                || msg.contains("callee compilation"),
            "{msg}"
        );
        assert!(msg.contains("invalid JIT metadata"), "{msg}");
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
