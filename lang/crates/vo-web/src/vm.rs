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
pub use vo_vm::{GcMode, OomPolicy, VmMemoryConfig};

/// Generic WASM extension bridge. Use this to load ext modules and auto-register
/// their externs without any per-module hardcoding.
pub use vo_web_runtime_wasm::ext_bridge;

/// Type alias for extern registration function.
pub type ExternRegistrar = fn(&mut ExternRegistry, &[ExternDef]) -> Result<(), ExternContractError>;

pub const WASM_PAGE_BYTES: u64 = 64 * 1024;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WasmMemoryAdmission {
    pub reserve_bytes: u64,
    pub hard_limit_bytes: Option<u64>,
    pub maximum_pages: Option<u64>,
    pub growth_allowed: bool,
    pub allocation_allowed: bool,
    pub gc_mode: GcMode,
    pub automatic_gc: bool,
    pub oom_policy: OomPolicy,
}

impl Default for WasmMemoryAdmission {
    fn default() -> Self {
        Self {
            reserve_bytes: 0,
            hard_limit_bytes: None,
            maximum_pages: None,
            growth_allowed: true,
            allocation_allowed: true,
            gc_mode: GcMode::Generational,
            automatic_gc: true,
            oom_policy: OomPolicy::CollectThenTerminateIsland,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WasmMemoryAdmissionReport {
    pub initial_pages: u64,
    pub admitted_pages: u64,
    pub maximum_pages: Option<u64>,
}

fn bytes_to_wasm_pages(bytes: u64) -> Result<u64, String> {
    bytes
        .checked_add(WASM_PAGE_BYTES - 1)
        .map(|value| value / WASM_PAGE_BYTES)
        .ok_or_else(|| "WASM memory admission byte count overflow".to_string())
}

pub fn admit_wasm_memory(
    admission: WasmMemoryAdmission,
    current_pages: u64,
    mut grow: impl FnMut(u64) -> bool,
) -> Result<WasmMemoryAdmissionReport, String> {
    let reserve_pages = bytes_to_wasm_pages(admission.reserve_bytes)?;
    let hard_limit_pages = admission
        .hard_limit_bytes
        .map(bytes_to_wasm_pages)
        .transpose()?;
    if admission
        .hard_limit_bytes
        .is_some_and(|limit| admission.reserve_bytes > limit)
    {
        return Err("WASM reserve exceeds the Island hard limit".to_string());
    }
    if admission
        .maximum_pages
        .is_some_and(|maximum| current_pages > maximum)
    {
        return Err("current WASM memory exceeds module maximum pages".to_string());
    }
    if let (Some(limit), Some(maximum)) = (hard_limit_pages, admission.maximum_pages) {
        let required_pages = current_pages
            .checked_add(limit)
            .ok_or_else(|| "WASM hard-limit admission page count overflow".to_string())?;
        if required_pages > maximum {
            return Err(format!(
                "WASM Island hard limit requires {required_pages} total pages, above module maximum {maximum}"
            ));
        }
    }
    let admitted_pages = current_pages
        .checked_add(reserve_pages)
        .ok_or_else(|| "WASM reserve admission page count overflow".to_string())?;
    if admission
        .maximum_pages
        .is_some_and(|maximum| admitted_pages > maximum)
    {
        return Err("WASM reserve admission exceeds module maximum pages".to_string());
    }
    if reserve_pages > 0 && !grow(reserve_pages) {
        return Err(format!(
            "WASM memory pre-growth to {admitted_pages} pages failed"
        ));
    }
    Ok(WasmMemoryAdmissionReport {
        initial_pages: current_pages,
        admitted_pages,
        maximum_pages: admission.maximum_pages,
    })
}

#[cfg(target_arch = "wasm32")]
fn admit_current_wasm_memory(
    admission: WasmMemoryAdmission,
) -> Result<WasmMemoryAdmissionReport, String> {
    let current = core::arch::wasm32::memory_size(0) as u64;
    admit_wasm_memory(admission, current, |delta| {
        let Ok(delta) = usize::try_from(delta) else {
            return false;
        };
        core::arch::wasm32::memory_grow(0, delta) != usize::MAX
    })
}

#[cfg(not(target_arch = "wasm32"))]
fn admit_current_wasm_memory(
    admission: WasmMemoryAdmission,
) -> Result<WasmMemoryAdmissionReport, String> {
    admit_wasm_memory(admission, 0, |_| true)
}

// ── Extern registration ─────────────────────────────────────────────────────

pub(crate) fn register_wasm_runtime_module_externs(
    reg: &mut ExternRegistry,
    module: &vo_runtime::bytecode::Module,
) -> Result<(), ExternContractError> {
    register_wasm_platform_externs(reg, &module.externs)?;
    vo_ui_vm::register_module(reg, module)?;
    Ok(())
}

fn register_wasm_platform_externs(
    reg: &mut ExternRegistry,
    exts: &[ExternDef],
) -> Result<(), ExternContractError> {
    vo_stdlib::register_portable_externs(reg, exts)?;
    vo_web_runtime_wasm::os::register_externs(reg, exts)?;
    vo_web_runtime_wasm::exec::register_externs(reg, exts)?;
    vo_web_runtime_wasm::time::register_externs(reg, exts)?;
    vo_web_runtime_wasm::filepath::register_externs(reg, exts)?;
    vo_web_runtime_wasm::fmt::register_externs(reg, exts)?;
    vo_web_runtime_wasm::io::register_externs(reg, exts)?;
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
    create_loaded_vm_with_memory(bytecode, register_externs, WasmMemoryAdmission::default())
}

pub fn create_loaded_vm_with_memory(
    bytecode: &[u8],
    register_externs: ExternRegistrar,
    admission: WasmMemoryAdmission,
) -> Result<Vm, String> {
    let module = decode_bytecode_module(bytecode)?;
    create_loaded_vm_from_module_with_memory(module, register_externs, admission)
}

pub fn create_loaded_vm_from_module(
    module: Module,
    register_externs: ExternRegistrar,
) -> Result<Vm, String> {
    create_loaded_vm_from_module_with_memory(
        module,
        register_externs,
        WasmMemoryAdmission::default(),
    )
}

pub fn create_loaded_vm_from_module_with_memory(
    module: Module,
    register_externs: ExternRegistrar,
    admission: WasmMemoryAdmission,
) -> Result<Vm, String> {
    create_loaded_vm_from_module_with_ui_mode(module, register_externs, admission, false)
        .map(|(vm, _)| vm)
}

/// Builds and verifies a replacement UI VM without mutating the currently
/// mounted UI arena. The caller starts a transactional arena checkpoint only
/// after this function succeeds.
pub(crate) fn create_loaded_ui_reload_vm(
    bytecode: &[u8],
) -> Result<(Vm, vo_ui_vm::PreparedReloadModule), String> {
    fn no_extra_externs(
        _registry: &mut ExternRegistry,
        _externs: &[ExternDef],
    ) -> Result<(), ExternContractError> {
        Ok(())
    }

    let module = decode_bytecode_module(bytecode)?;
    let (vm, prepared) = create_loaded_vm_from_module_with_ui_mode(
        module,
        no_extra_externs,
        WasmMemoryAdmission::default(),
        true,
    )?;
    let prepared = prepared
        .ok_or_else(|| "UI reload preparation completed without a prepared module".to_string())?;
    Ok((vm, prepared))
}

fn create_loaded_vm_from_module_with_ui_mode(
    module: Module,
    register_externs: ExternRegistrar,
    admission: WasmMemoryAdmission,
    prepare_ui_reload: bool,
) -> Result<(Vm, Option<vo_ui_vm::PreparedReloadModule>), String> {
    init_output();

    let report = admit_current_wasm_memory(admission)?;
    let memory = VmMemoryConfig {
        initial_reserve_bytes: usize::try_from(admission.reserve_bytes)
            .map_err(|_| "WASM reserve does not fit target usize".to_string())?,
        hard_limit_bytes: admission
            .hard_limit_bytes
            .map(usize::try_from)
            .transpose()
            .map_err(|_| "WASM hard limit does not fit target usize".to_string())?,
        gc_mode: admission.gc_mode,
        automatic_gc: admission.automatic_gc,
        oom_policy: admission.oom_policy,
        growth_allowed: admission.growth_allowed,
        allocation_allowed: admission.allocation_allowed,
        ..VmMemoryConfig::default()
    };
    let mut vm = Vm::try_with_memory_config(memory)
        .map_err(|error| format!("Failed to initialize VM: {error}"))?;
    #[cfg(target_arch = "wasm32")]
    let current_pages = core::arch::wasm32::memory_size(0) as u64;
    #[cfg(not(target_arch = "wasm32"))]
    let current_pages = report.admitted_pages;
    vm.memory_set_wasm_pages(current_pages, report.maximum_pages);
    let exts = &module.externs;
    let reg = vm
        .extern_registry_mut()
        .map_err(|error| format!("Failed to configure VM externs: {error:?}"))?;
    register_wasm_platform_externs(reg, &module.externs)
        .map_err(|error| format!("Failed to register WASM platform externs: {error}"))?;
    let reload_component = if prepare_ui_reload {
        Some(
            vo_ui_vm::prepare_reload_module(reg, &module)
                .map_err(|error| format!("Failed to prepare UI reload externs: {error}"))?,
        )
    } else {
        vo_ui_vm::register_module(reg, &module)
            .map_err(|error| format!("Failed to register UI externs: {error}"))?;
        None
    };

    // caller
    register_externs(reg, exts)
        .map_err(|error| format!("Failed to register caller externs: {error}"))?;

    vm.load_with_embedder_externs(module)
        .map_err(|e| format!("{:?}", e))?;
    Ok((vm, reload_component))
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
        admit_wasm_memory, decode_bytecode_module, register_wasm_runtime_module_externs,
        validate_external_bytecode_size, voplay_perf_report_payload, WasmMemoryAdmission,
        WASM_PAGE_BYTES,
    };

    #[test]
    fn wasm_memory_admission_rejects_inconsistent_limits() {
        let reserve_above_limit = WasmMemoryAdmission {
            reserve_bytes: 2 * WASM_PAGE_BYTES,
            hard_limit_bytes: Some(WASM_PAGE_BYTES),
            ..WasmMemoryAdmission::default()
        };
        assert!(admit_wasm_memory(reserve_above_limit, 0, |_| true).is_err());

        let limit_above_module_maximum = WasmMemoryAdmission {
            hard_limit_bytes: Some(3 * WASM_PAGE_BYTES),
            maximum_pages: Some(2),
            ..WasmMemoryAdmission::default()
        };
        assert!(admit_wasm_memory(limit_above_module_maximum, 0, |_| true).is_err());

        let current_plus_limit_above_maximum = WasmMemoryAdmission {
            hard_limit_bytes: Some(8 * WASM_PAGE_BYTES),
            maximum_pages: Some(8),
            ..WasmMemoryAdmission::default()
        };
        assert!(admit_wasm_memory(current_plus_limit_above_maximum, 2, |_| true).is_err());
    }

    #[test]
    fn wasm_memory_admission_pre_grows_exact_page_delta() {
        let admission = WasmMemoryAdmission {
            reserve_bytes: 5 * WASM_PAGE_BYTES - 1,
            hard_limit_bytes: Some(8 * WASM_PAGE_BYTES),
            maximum_pages: Some(10),
            ..WasmMemoryAdmission::default()
        };
        let mut requested_delta = None;
        let report = admit_wasm_memory(admission, 2, |delta| {
            requested_delta = Some(delta);
            true
        })
        .expect("valid reserve should be admitted");

        assert_eq!(requested_delta, Some(5));
        assert_eq!(report.initial_pages, 2);
        assert_eq!(report.admitted_pages, 7);
        assert_eq!(report.maximum_pages, Some(10));
    }

    #[test]
    fn wasm_memory_admission_fails_when_pre_growth_fails() {
        let admission = WasmMemoryAdmission {
            reserve_bytes: 2 * WASM_PAGE_BYTES,
            ..WasmMemoryAdmission::default()
        };
        assert!(admit_wasm_memory(admission, 1, |_| false).is_err());
    }

    #[test]
    fn combined_wasm_registration_has_one_provider_per_extern() {
        let mut externs = vo_stdlib::extern_manifest::EFFECT_MANIFEST
            .iter()
            .map(|entry| super::ExternDef {
                name: entry.name.to_string(),
                params: vo_runtime::bytecode::ParamShape::CallSiteVariadic,
                returns: vo_runtime::bytecode::ReturnShape::slots(0),
                allowed_effects: entry.effects,
                param_kinds: Vec::new(),
            })
            .collect::<Vec<_>>();
        let ui_mount =
            vo_common_core::extern_key::ExternKeyRef::new(vo_ui_vm::UI_MODULE_PATH, "Mount")
                .encode()
                .unwrap();
        externs.push(super::ExternDef::new(
            ui_mount.clone(),
            vo_runtime::bytecode::ParamShape::CallSiteVariadic,
            vo_runtime::bytecode::ReturnShape::slots(0),
            vo_runtime::bytecode::ExternEffects::UNKNOWN_CONTROL,
            Vec::new(),
        ));
        let mut registry = super::ExternRegistry::new();
        let mut module = vo_runtime::bytecode::Module::new("web-provider-test".to_string());
        module.externs = externs;

        register_wasm_runtime_module_externs(&mut registry, &module)
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
        assert!(
            registry.registered_by_name(&ui_mount).is_some(),
            "missing official UI provider in the browser VM"
        );
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

    #[cfg(feature = "compiler")]
    #[test]
    fn runtime_mem_package_compiles_and_services_safe_boundary_requests() {
        let source = r#"
            package main

            import (
                "fmt"
                "runtime/mem"
            )

            func main() {
                stats := mem.ReadStats()
                fmt.Println(stats.ManagedCommittedBytes >= stats.ManagedLiveBytes)
                fmt.Println(mem.GCStep(1))
                fmt.Println(mem.GCCollect())
            }
        "#;
        let bytecode = crate::compile::compile_source_with_std_fs(
            source,
            "main.vo",
            crate::compile::build_stdlib_fs(),
        )
        .expect("runtime/mem fixture should compile");

        let result = run(&bytecode);

        assert_eq!(result.status, "ok", "stderr: {}", result.stderr);
        assert_eq!(result.stdout, "true\ntrue\ntrue\n");
        assert!(result.stderr.is_empty());
    }
}
