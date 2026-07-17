//! VM creation, extern registration, and synchronous execution helpers.

use vo_vm::vm::SchedulingOutcome;

use crate::js_types::RunResult;

#[cfg(not(target_arch = "wasm32"))]
use std::sync::{Arc, Mutex, MutexGuard};

#[cfg(any(all(target_arch = "wasm32", feature = "compiler"), test))]
const VOPLAY_PERF_REPORT_MARKER: &str = "__VOPLAY_PERF_REPORT__";
#[cfg(all(target_arch = "wasm32", feature = "compiler"))]
const VOPLAY_PERF_REPORT_CODE: &str = "voplay_perf_report";

// ── Re-exports for external consumers ────────────────────────────────────────

pub use vo_runtime::ffi::{ExternCallContext, ExternContractError, ExternRegistry, ExternResult};
pub use vo_runtime::gc::GcRef;
pub use vo_vm::bytecode::{ExternDef, Module};
pub use vo_vm::vm::Vm;

/// Generic WASM extension bridge. Use this to load ext modules and auto-register
/// their externs without any per-module hardcoding.
pub use vo_web_runtime_wasm::ext_bridge;

/// Type alias for extern registration function.
pub type ExternRegistrar = fn(&mut ExternRegistry, &[ExternDef]) -> Result<(), ExternContractError>;

// ── Extern registration ─────────────────────────────────────────────────────

pub(crate) fn register_wasm_runtime_externs(
    reg: &mut ExternRegistry,
    exts: &[ExternDef],
) -> Result<(), ExternContractError> {
    vo_stdlib::register_portable_externs(reg, exts)?;
    vo_web_runtime_wasm::os::register_externs(reg, exts)?;
    vo_web_runtime_wasm::exec::register_externs(reg, exts)?;
    vo_web_runtime_wasm::time::register_externs(reg, exts)?;
    vo_web_runtime_wasm::filepath::register_externs(reg, exts)?;
    vo_web_runtime_wasm::net_http::register_externs(reg, exts)?;
    Ok(())
}

// ── VM outcome helpers ──────────────────────────────────────────────────────

pub(crate) fn validate_sync_outcome(
    vm: &vo_vm::vm::Vm,
    outcome: SchedulingOutcome,
) -> Result<(), String> {
    match outcome {
        SchedulingOutcome::Completed
        | SchedulingOutcome::Exited(_)
        | SchedulingOutcome::Suspended
        | SchedulingOutcome::SuspendedForHostEvents => Ok(()),
        SchedulingOutcome::Blocked => Err(format!("{:?}", vm.deadlock_err())),
        SchedulingOutcome::Panicked => Err(String::from("unexpected bounded panic outcome")),
    }
}

// ── VM creation ─────────────────────────────────────────────────────────────

/// Write hook: flush each Vo println line to browser console immediately.
/// This ensures diagnostic output is visible even if a WASM trap occurs.
#[cfg(all(target_arch = "wasm32", feature = "compiler"))]
fn wasm_write_hook(s: &str) {
    if let Some(payload) = voplay_perf_report_payload(s) {
        crate::host_log::emit_host_log(
            crate::host_log::HostLogRecord::new("vo-web", VOPLAY_PERF_REPORT_CODE, "info")
                .text(payload),
        );
        return;
    }
    web_sys::console::log_1(&format!("[Vo] {}", s).into());
}

// The bytecode-only build intentionally has no `web-sys` or Studio host-log
// dependency. Output remains available through `take_output`; installing a
// no-op mirror hook keeps the VM initialization path feature-independent.
#[cfg(all(target_arch = "wasm32", not(feature = "compiler")))]
fn wasm_write_hook(_s: &str) {}

#[cfg(any(all(target_arch = "wasm32", feature = "compiler"), test))]
fn voplay_perf_report_payload(s: &str) -> Option<&str> {
    s.trim_start()
        .strip_prefix(VOPLAY_PERF_REPORT_MARKER)
        .map(str::trim)
}

fn init_output() {
    #[cfg(target_arch = "wasm32")]
    vo_runtime::output::set_write_hook(wasm_write_hook);
    vo_runtime::output::clear_output();
}

fn validate_external_bytecode_size(len: usize) -> Result<(), String> {
    vo_common_core::serialize::validate_vob_input_size(len)
        .map_err(|error| format!("Failed to load bytecode: {error}"))
}

pub(crate) fn decode_bytecode_module(bytecode: &[u8]) -> Result<Module, String> {
    validate_external_bytecode_size(bytecode.len())?;
    Module::deserialize(bytecode).map_err(|error| format!("Failed to load bytecode: {error}"))
}

/// Create a VM from bytecode, register externs, and run initialization.
pub fn create_vm(bytecode: &[u8], register_externs: ExternRegistrar) -> Result<Vm, String> {
    let module = decode_bytecode_module(bytecode)?;
    create_vm_from_module(module, register_externs)
}

/// Create a VM from a pre-deserialized module.
pub fn create_vm_from_module(
    module: Module,
    register_externs: ExternRegistrar,
) -> Result<Vm, String> {
    let vm = create_loaded_vm_from_module(module, register_externs)?;
    run_loaded_vm(vm)
}

fn run_loaded_vm(mut vm: Vm) -> Result<Vm, String> {
    let outcome = vm.run().map_err(|e| format!("{:?}", e))?;
    validate_sync_outcome(&vm, outcome)?;
    Ok(vm)
}

pub fn create_loaded_vm(bytecode: &[u8], register_externs: ExternRegistrar) -> Result<Vm, String> {
    let module = decode_bytecode_module(bytecode)?;
    create_loaded_vm_from_module(module, register_externs)
}

pub fn create_loaded_vm_from_module(
    module: Module,
    register_externs: ExternRegistrar,
) -> Result<Vm, String> {
    init_output();

    let mut vm = Vm::try_new().map_err(|error| format!("Failed to initialize VM: {error}"))?;
    let exts = &module.externs;
    let reg = vm
        .extern_registry_mut()
        .map_err(|error| format!("Failed to configure VM externs: {error:?}"))?;
    register_wasm_runtime_externs(reg, exts)
        .map_err(|error| format!("Failed to register WASM runtime externs: {error}"))?;

    // caller
    register_externs(reg, exts)
        .map_err(|error| format!("Failed to register caller externs: {error}"))?;

    vm.load_with_embedder_externs(module)
        .map_err(|e| format!("{:?}", e))?;
    Ok(vm)
}

// ── VM interaction ──────────────────────────────────────────────────────────

/// Call a closure in the VM (for handling external events).
pub fn call_closure(vm: &mut Vm, closure: GcRef, args: &[u64]) -> Result<(), String> {
    vo_runtime::output::clear_output();

    vm.spawn_closure_call(closure, args)
        .map_err(|e| format!("{:?}", e))?;
    let outcome = vm.run_scheduled().map_err(|e| format!("{:?}", e))?;
    validate_sync_outcome(vm, outcome)?;

    Ok(())
}

/// Take captured output since last clear.
pub fn take_output() -> String {
    vo_runtime::output::take_output()
}

/// Native builds can unify `vo-runtime/std` through another workspace crate.
/// Keep the exported host runner independent of that feature choice by owning
/// its capture sink per invocation. Browser builds retain the WASM global sink,
/// which also drives the immediate console hook.
#[cfg(not(target_arch = "wasm32"))]
struct NativeRunOutput(Mutex<Vec<u8>>);

#[cfg(not(target_arch = "wasm32"))]
impl NativeRunOutput {
    fn new() -> Arc<Self> {
        Arc::new(Self(Mutex::new(Vec::new())))
    }

    fn buffer(&self) -> MutexGuard<'_, Vec<u8>> {
        self.0
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }

    fn take(&self) -> String {
        let bytes = std::mem::take(&mut *self.buffer());
        render_native_output_text(&bytes)
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn render_native_output_text(bytes: &[u8]) -> String {
    use std::fmt::Write as _;

    let mut rendered = String::new();
    let mut remaining = bytes;
    while !remaining.is_empty() {
        match std::str::from_utf8(remaining) {
            Ok(text) => {
                rendered.push_str(text);
                break;
            }
            Err(error) => {
                let valid = error.valid_up_to();
                if valid > 0 {
                    rendered.push_str(
                        std::str::from_utf8(&remaining[..valid])
                            .expect("valid_up_to ends on a UTF-8 boundary"),
                    );
                }
                let invalid = remaining[valid];
                let _ = write!(rendered, "\\x{invalid:02x}");
                remaining = &remaining[valid + 1..];
            }
        }
    }
    rendered
}

#[cfg(not(target_arch = "wasm32"))]
impl vo_runtime::output::OutputSink for NativeRunOutput {
    fn write_bytes(&self, bytes: &[u8]) {
        self.buffer().extend_from_slice(bytes);
    }

    fn writeln_bytes(&self, bytes: &[u8]) {
        let mut output = self.buffer();
        output.extend_from_slice(bytes);
        output.push(b'\n');
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn run_with_host_output(bytecode: &[u8]) -> (Result<Vm, String>, String) {
    let output = NativeRunOutput::new();
    let result = create_loaded_vm(bytecode, |_, _| Ok(())).and_then(|mut vm| {
        vm.set_output_sink(output.clone());
        run_loaded_vm(vm)
    });
    let stdout = output.take();
    (result, stdout)
}

#[cfg(target_arch = "wasm32")]
fn run_with_host_output(bytecode: &[u8]) -> (Result<Vm, String>, String) {
    let result = create_vm(bytecode, |_, _| Ok(()));
    let stdout = vo_runtime::output::take_output();
    (result, stdout)
}

fn host_run_result(result: Result<Vm, String>, stdout: String) -> RunResult {
    match result {
        Ok(vm) => RunResult {
            status: if vm.exit_code().is_some() {
                "exited"
            } else {
                "ok"
            }
            .to_string(),
            stdout,
            stderr: String::new(),
            exit_code: vm.exit_code(),
        },
        Err(msg) => RunResult {
            status: "error".to_string(),
            stdout,
            stderr: msg,
            exit_code: None,
        },
    }
}

// ── WASM exports: run ────────────────────────────────────────────────────────

/// Run bytecode.
#[wasm_bindgen::prelude::wasm_bindgen]
pub fn run(bytecode: &[u8]) -> RunResult {
    let (result, stdout) = run_with_host_output(bytecode);
    host_run_result(result, stdout)
}

/// Run bytecode with explicit os.Args injected as a JS string array.
/// `args` must be a JS `Array<string>`. The args are visible to the program as `os.Args`.
#[wasm_bindgen::prelude::wasm_bindgen(js_name = "runWithArgs")]
pub fn run_with_args(bytecode: &[u8], args: js_sys::Array) -> RunResult {
    let args_vec: Vec<String> = args.iter().filter_map(|v| v.as_string()).collect();

    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = Some(args_vec);
    });

    let (run_result, stdout) = run_with_host_output(bytecode);
    let result = host_run_result(run_result, stdout);

    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = None;
    });

    result
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "compiler")]
    use super::run;
    use super::{
        decode_bytecode_module, register_wasm_runtime_externs, validate_external_bytecode_size,
        voplay_perf_report_payload,
    };

    #[test]
    fn combined_wasm_registration_has_one_provider_per_extern() {
        let externs = vo_stdlib::extern_manifest::EFFECT_MANIFEST
            .iter()
            .map(|entry| super::ExternDef {
                name: entry.name.to_string(),
                params: vo_runtime::bytecode::ParamShape::CallSiteVariadic,
                returns: vo_runtime::bytecode::ReturnShape::slots(0),
                allowed_effects: entry.effects,
                param_kinds: Vec::new(),
            })
            .collect::<Vec<_>>();
        let mut registry = super::ExternRegistry::new();

        register_wasm_runtime_externs(&mut registry, &externs)
            .expect("portable stdlib and WASM host providers must compose");

        for name in [
            vo_runtime::vo_extern_name!("math", "Sqrt"),
            vo_runtime::vo_extern_name!("time", "localOffsetAt"),
            vo_runtime::vo_extern_name!("os", "nativeExit"),
        ] {
            assert!(
                registry.registered_by_name(name).is_some(),
                "missing combined WASM provider for {name}"
            );
        }
    }

    #[test]
    fn web_bytecode_gate_uses_canonical_size_boundary() {
        let max = vo_common_core::serialize::MAX_VOB_BYTES;
        assert!(validate_external_bytecode_size(max).is_ok());
        assert!(validate_external_bytecode_size(max + 1).is_err());
    }

    #[test]
    fn web_bytecode_gate_rejects_huge_length_field_without_allocation() {
        let mut bytes = super::Module::new("fixture".to_string())
            .serialize()
            .expect("serialize gate fixture");
        bytes.truncate(11);
        bytes[7..11].copy_from_slice(&u32::MAX.to_le_bytes());
        assert!(decode_bytecode_module(&bytes).is_err());
    }

    #[test]
    fn vm_web_closure_calls_use_vm_owned_frame_entry_047() {
        let src = include_str!("vm.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("production section");

        assert!(
            src.contains("vm.spawn_closure_call(closure, args)"),
            "vo-web event closures must enter through the VM-owned closure-call API"
        );
        assert!(
            !src.contains("vo_vm::vm::helpers"),
            "vo-web must not depend on VM helper internals for closure argument shaping"
        );
        assert!(
            !src.contains("build_closure_args"),
            "closure argument layout must be owned by vo-vm"
        );
    }

    #[test]
    fn extracts_voplay_perf_report_payload() {
        assert_eq!(
            voplay_perf_report_payload("  __VOPLAY_PERF_REPORT__{\"kind\":\"perf-summary\"}\n"),
            Some("{\"kind\":\"perf-summary\"}")
        );
        assert_eq!(voplay_perf_report_payload("[Vo] normal"), None);
    }

    #[cfg(feature = "compiler")]
    #[test]
    fn wasm_os_exit_is_immediate_and_observable_by_the_host() {
        let source = r#"
            package main

            import (
                "fmt"
                "os"
            )

            func main() {
                defer fmt.Println("deferred")
                fmt.Println("before")
                os.Exit(37)
                fmt.Println("after")
            }
        "#;
        let bytecode = crate::compile::compile_source_with_std_fs(
            source,
            "main.vo",
            crate::compile::build_stdlib_fs(),
        )
        .expect("os.Exit fixture should compile");

        let result = run(&bytecode);

        assert_eq!(result.status, "exited", "stderr: {}", result.stderr);
        assert_eq!(result.exit_code, Some(37));
        assert_eq!(result.stdout, "before\n");
        assert!(result.stderr.is_empty());
    }
}
