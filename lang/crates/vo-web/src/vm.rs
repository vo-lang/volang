//! VM creation, extern registration, and synchronous execution helpers.

use vo_vm::vm::SchedulingOutcome;

use crate::js_types::RunResult;

#[cfg(any(target_arch = "wasm32", test))]
const VOPLAY_PERF_REPORT_MARKER: &str = "__VOPLAY_PERF_REPORT__";
#[cfg(target_arch = "wasm32")]
const VOPLAY_PERF_REPORT_CODE: &str = "voplay_perf_report";

// ── Re-exports for external consumers ────────────────────────────────────────

pub use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult};
pub use vo_runtime::gc::GcRef;
pub use vo_vm::bytecode::{ExternDef, Module};
pub use vo_vm::vm::Vm;

/// Generic WASM extension bridge. Use this to load ext modules and auto-register
/// their externs without any per-module hardcoding.
pub use vo_web_runtime_wasm::ext_bridge;

/// Type alias for extern registration function.
pub type ExternRegistrar = fn(&mut ExternRegistry, &[ExternDef]);

// ── Extern registration ─────────────────────────────────────────────────────

pub(crate) fn register_wasm_runtime_externs(reg: &mut ExternRegistry, exts: &[ExternDef]) {
    vo_stdlib::register_externs(reg, exts);
    vo_web_runtime_wasm::os::register_externs(reg, exts);
    vo_web_runtime_wasm::exec::register_externs(reg, exts);
    vo_web_runtime_wasm::time::register_externs(reg, exts);
    vo_web_runtime_wasm::filepath::register_externs(reg, exts);
    vo_web_runtime_wasm::net_http::register_externs(reg, exts);
}

// ── VM outcome helpers ──────────────────────────────────────────────────────

pub(crate) fn validate_sync_outcome(
    vm: &vo_vm::vm::Vm,
    outcome: SchedulingOutcome,
) -> Result<(), String> {
    match outcome {
        SchedulingOutcome::Completed
        | SchedulingOutcome::Suspended
        | SchedulingOutcome::SuspendedForHostEvents => Ok(()),
        SchedulingOutcome::Blocked => Err(format!("{:?}", vm.deadlock_err())),
        SchedulingOutcome::Panicked => Err(String::from("unexpected bounded panic outcome")),
    }
}

// ── VM creation ─────────────────────────────────────────────────────────────

/// Write hook: flush each Vo println line to browser console immediately.
/// This ensures diagnostic output is visible even if a WASM trap occurs.
#[cfg(target_arch = "wasm32")]
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

#[cfg(any(target_arch = "wasm32", test))]
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

/// Create a VM from bytecode, register externs, and run initialization.
pub fn create_vm(bytecode: &[u8], register_externs: ExternRegistrar) -> Result<Vm, String> {
    let module =
        Module::deserialize(bytecode).map_err(|e| format!("Failed to load bytecode: {:?}", e))?;
    create_vm_from_module(module, register_externs)
}

/// Create a VM from a pre-deserialized module.
pub fn create_vm_from_module(
    module: Module,
    register_externs: ExternRegistrar,
) -> Result<Vm, String> {
    let mut vm = create_loaded_vm_from_module(module, register_externs)?;
    let outcome = vm.run().map_err(|e| format!("{:?}", e))?;
    validate_sync_outcome(&vm, outcome)?;
    Ok(vm)
}

pub fn create_loaded_vm(bytecode: &[u8], register_externs: ExternRegistrar) -> Result<Vm, String> {
    let module =
        Module::deserialize(bytecode).map_err(|e| format!("Failed to load bytecode: {:?}", e))?;
    create_loaded_vm_from_module(module, register_externs)
}

pub fn create_loaded_vm_from_module(
    module: Module,
    register_externs: ExternRegistrar,
) -> Result<Vm, String> {
    init_output();

    let mut vm = Vm::new();
    let exts = &module.externs;
    let reg = vm.extern_registry_mut();
    register_wasm_runtime_externs(reg, exts);

    // caller
    register_externs(reg, exts);

    vm.load(module).map_err(|e| format!("{:?}", e))?;
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

// ── WASM exports: run ────────────────────────────────────────────────────────

/// Run bytecode.
#[wasm_bindgen::prelude::wasm_bindgen]
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

/// Run bytecode with explicit os.Args injected as a JS string array.
/// `args` must be a JS `Array<string>`. The args are visible to the program as `os.Args`.
#[wasm_bindgen::prelude::wasm_bindgen(js_name = "runWithArgs")]
pub fn run_with_args(bytecode: &[u8], args: js_sys::Array) -> RunResult {
    let args_vec: Vec<String> = args.iter().filter_map(|v| v.as_string()).collect();

    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = Some(args_vec);
    });

    let result = match create_vm(bytecode, |_, _| {}) {
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
    };

    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = None;
    });

    result
}

#[cfg(test)]
mod tests {
    use super::voplay_perf_report_payload;

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
}
