//! Async VM execution loop for WASM targets.
//!
//! Handles the event-driven execution cycle including:
//! - HTTP fetch via browser Fetch API
//! - Timer-based sleep via setTimeout
//! - Host event processing (timers, async callbacks)

use core::cell::Cell;

use vo_vm::vm::SchedulingOutcome;
use wasm_bindgen::prelude::*;

use crate::js_types::make_run_result_obj;
use crate::vm::register_wasm_runtime_externs;
#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
use crate::vm::{ExternRegistrar, Module};

// ── Outcome helpers ─────────────────────────────────────────────────────────

fn finish_async_outcome(
    vm: &vo_vm::vm::Vm,
    outcome: SchedulingOutcome,
) -> (String, String, String) {
    let stdout = vo_runtime::output::take_output();
    match outcome {
        SchedulingOutcome::Completed => ("ok".into(), stdout, String::new()),
        SchedulingOutcome::Suspended => (
            "suspended".into(),
            stdout,
            String::from("vm suspended waiting for host-routed island commands/responses"),
        ),
        SchedulingOutcome::SuspendedForHostEvents => (
            "suspended".into(),
            stdout,
            String::from(
                "vm suspended waiting for host events, but the async runner had no event to drive",
            ),
        ),
        SchedulingOutcome::Blocked => ("error".into(), stdout, format!("{:?}", vm.deadlock_err())),
        SchedulingOutcome::Panicked => (
            "error".into(),
            stdout,
            String::from("unexpected bounded panic outcome"),
        ),
    }
}

// ── Fetch handling ──────────────────────────────────────────────────────────

/// Await a single fetch Promise: resolve it, store the result, wake the fiber, and resume VM.
async fn await_fetch(
    vm: &mut vo_vm::vm::Vm,
    key: vo_vm::scheduler::HostWaitKey,
    promise: js_sys::Promise,
) -> Result<SchedulingOutcome, vo_vm::vm::VmError> {
    let token = key.token;
    let result = wasm_bindgen_futures::JsFuture::from(promise).await;
    let fetch_result = match result {
        Ok(val) => vo_web_runtime_wasm::net_http::parse_fetch_js_value(token, &val),
        Err(e) => vo_web_runtime_wasm::net_http::FetchResult {
            status_code: 0,
            status: String::new(),
            proto: String::new(),
            headers: Vec::new(),
            body: Vec::new(),
            error: Some(format!("fetch error: {:?}", e)),
        },
    };
    vo_web_runtime_wasm::net_http::store_fetch_result(token, fetch_result);
    if !vm.wake_host_event(key) {
        return Err(vo_vm::vm::VmError::Jit(format!(
            "host replay wake rejected for fetch token {token}"
        )));
    }
    vm.run_scheduled()
}

fn fetch_host_wait_key(
    vm: &vo_vm::vm::Vm,
    token: u64,
) -> Result<vo_vm::scheduler::HostWaitKey, vo_vm::vm::VmError> {
    vm.host_event_key(
        vo_vm::scheduler::HostWaitSource::replay(vo_runtime::ffi::HostEventReplaySource::Fetch),
        token,
    )
    .ok_or_else(|| {
        vo_vm::vm::VmError::Jit(format!(
            "missing host replay wait key for fetch token {token}"
        ))
    })
}

// ── Sleep helpers ───────────────────────────────────────────────────────────

/// Await one JS setTimeout via a Promise. Returns false if setTimeout is unavailable.
async fn wasm_sleep_once_ms(ms: u32) -> bool {
    let used_set_timeout = Cell::new(false);
    let promise = js_sys::Promise::new(&mut |resolve, _reject| {
        let global = js_sys::global();
        let set_timeout = js_sys::Reflect::get(&global, &JsValue::from_str("setTimeout"))
            .unwrap_or(JsValue::UNDEFINED);
        if set_timeout.is_function() {
            used_set_timeout.set(true);
            let func = js_sys::Function::from(set_timeout);
            let _ = func.call2(&JsValue::NULL, &resolve, &JsValue::from(ms));
        } else {
            // Fallback: resolve immediately (no setTimeout available)
            let _ = resolve.call0(&JsValue::NULL);
        }
    });
    let _ = wasm_bindgen_futures::JsFuture::from(promise).await;
    used_set_timeout.get()
}

/// Await at least `ms` milliseconds using JS timers.
///
/// A single setTimeout can fire slightly early in some hosts; we therefore
/// loop until a deadline is reached to preserve Sleep lower-bound semantics.
async fn wasm_callback_sleep_ms(ms: u32) {
    if ms == 0 {
        return;
    }
    let deadline_ms = crate::now_ms() + ms as f64;
    loop {
        let remaining_ms = deadline_ms - crate::now_ms();
        if remaining_ms <= 0.0 {
            break;
        }
        let remaining = remaining_ms.ceil().max(1.0) as u32;
        if !wasm_sleep_once_ms(remaining).await {
            break;
        }
    }
}

// ── Async VM execution ─────────────────────────────────────────────────────

/// Async VM execution loop: runs until complete, awaiting JS callbacks for Sleep/fetch.
/// Returns (status, stdout, stderr).
async fn run_vm_async(bytecode: &[u8]) -> (String, String, String) {
    vo_runtime::output::clear_output();
    let module = match vo_vm::bytecode::Module::deserialize(bytecode) {
        Ok(m) => m,
        Err(e) => {
            return (
                "error".into(),
                String::new(),
                format!("Failed to load bytecode: {:?}", e),
            )
        }
    };

    let mut vm = vo_vm::vm::Vm::new();
    let exts = &module.externs;
    let reg = vm.extern_registry_mut();
    register_wasm_runtime_externs(reg, exts);
    if let Err(e) = vm.load(module) {
        return ("error".into(), String::new(), format!("{:?}", e));
    }

    run_vm_async_inner(&mut vm).await
}

/// Async VM execution with an additional extern registrar on top of stdlib+web.
///
/// This is the WASM equivalent of `create_vm_from_module` but uses the full
/// async event loop so WaitIo/Sleep/HTTP work correctly.
/// Returns `(status, stdout, stderr)`.
#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
pub async fn run_bytecode_async_with_externs(
    bytecode: &[u8],
    extra_reg: ExternRegistrar,
) -> (String, String, String) {
    vo_runtime::output::clear_output();
    let module = match Module::deserialize(bytecode) {
        Ok(m) => m,
        Err(e) => {
            return (
                "error".into(),
                String::new(),
                format!("Failed to load bytecode: {:?}", e),
            )
        }
    };

    let mut vm = vo_vm::vm::Vm::new();
    let exts = &module.externs;
    let reg = vm.extern_registry_mut();
    register_wasm_runtime_externs(reg, exts);
    extra_reg(reg, exts);
    if let Err(e) = vm.load(module) {
        return ("error".into(), String::new(), format!("{:?}", e));
    }

    run_vm_async_inner(&mut vm).await
}

/// Shared inner async run loop (extracted so both run_vm_async variants can use it).
pub(crate) async fn run_vm_async_inner(vm: &mut vo_vm::vm::Vm) -> (String, String, String) {
    let mut outcome = match vm.run() {
        Ok(o) => o,
        Err(e) => {
            return (
                "error".into(),
                vo_runtime::output::take_output(),
                format!("{:?}", e),
            )
        }
    };

    'host_event_loop: while outcome == SchedulingOutcome::SuspendedForHostEvents {
        loop {
            let fetches = vo_web_runtime_wasm::net_http::take_pending_fetch_promises();
            if fetches.is_empty() {
                break;
            }
            for (token, promise) in fetches {
                let key = match fetch_host_wait_key(vm, token) {
                    Ok(key) => key,
                    Err(e) => {
                        return (
                            "error".into(),
                            vo_runtime::output::take_output(),
                            format!("{:?}", e),
                        )
                    }
                };
                outcome = match await_fetch(vm, key, promise).await {
                    Ok(o) => o,
                    Err(e) => {
                        return (
                            "error".into(),
                            vo_runtime::output::take_output(),
                            format!("{:?}", e),
                        )
                    }
                };
                if outcome != SchedulingOutcome::SuspendedForHostEvents {
                    break 'host_event_loop;
                }
            }
        }

        let mut timer_events: Vec<_> = vm
            .take_pending_host_events()
            .into_iter()
            .filter(|e| !e.replay)
            .collect();
        if timer_events.is_empty() {
            break;
        }
        timer_events.sort_unstable_by_key(|e| e.delay_ms);
        let batch_start_ms = crate::now_ms();
        for ev in timer_events {
            let elapsed = crate::now_ms() - batch_start_ms;
            let remaining = ((ev.delay_ms as f64) - elapsed).ceil().max(0.0) as u32;
            if remaining > 0 {
                wasm_callback_sleep_ms(remaining).await;
            }
            if !vm.wake_host_event(ev.key) {
                return (
                    "error".into(),
                    vo_runtime::output::take_output(),
                    format!("host timer wake rejected for token {}", ev.token),
                );
            }
            outcome = match vm.run_scheduled() {
                Ok(o) => o,
                Err(e) => {
                    return (
                        "error".into(),
                        vo_runtime::output::take_output(),
                        format!("{:?}", e),
                    )
                }
            };
            if outcome != SchedulingOutcome::SuspendedForHostEvents {
                break 'host_event_loop;
            }

            for (token, promise) in vo_web_runtime_wasm::net_http::take_pending_fetch_promises() {
                let key = match fetch_host_wait_key(vm, token) {
                    Ok(key) => key,
                    Err(e) => {
                        return (
                            "error".into(),
                            vo_runtime::output::take_output(),
                            format!("{:?}", e),
                        )
                    }
                };
                outcome = match await_fetch(vm, key, promise).await {
                    Ok(o) => o,
                    Err(e) => {
                        return (
                            "error".into(),
                            vo_runtime::output::take_output(),
                            format!("{:?}", e),
                        )
                    }
                };
                if outcome != SchedulingOutcome::SuspendedForHostEvents {
                    break 'host_event_loop;
                }
            }
        }
    }

    finish_async_outcome(vm, outcome)
}

// ── WASM exports: compile-and-run ───────────────────────────────────────────

/// Compile and run in one step. Returns a Promise<{status,stdout,stderr}> to support async ops.
#[cfg(feature = "compiler")]
#[wasm_bindgen(js_name = "compileAndRun")]
pub fn compile_and_run(source: &str, filename: Option<String>) -> js_sys::Promise {
    let source = source.to_string();
    let result = crate::compile::compile(&source, filename);
    if !result.success {
        let obj = make_run_result_obj(
            "compile_error",
            "",
            &result.error_message.unwrap_or_default(),
        );
        return js_sys::Promise::resolve(&obj);
    }
    let bytecode = result.bytecode.unwrap();
    wasm_bindgen_futures::future_to_promise(async move {
        let (status, stdout, stderr) = run_vm_async(&bytecode).await;
        Ok(make_run_result_obj(&status, &stdout, &stderr))
    })
}

/// Compile and run Vo source that imports third-party GitHub modules.
///
/// Detects `import "github.com/..."` patterns, fetches Vo source files and
/// pre-compiled WASM binaries from GitHub, then compiles and runs with ext-bridge.
#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
#[wasm_bindgen(js_name = "compileAndRunWithModules")]
pub fn compile_and_run_with_modules(source: &str) -> js_sys::Promise {
    let source = source.to_string();
    wasm_bindgen_futures::future_to_promise(async move {
        let (status, stdout, stderr) = run_with_modules_inner(&source).await;
        Ok(make_run_result_obj(&status, &stdout, &stderr))
    })
}

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
async fn run_with_modules_inner(source: &str) -> (String, String, String) {
    let std_fs = crate::compile::build_stdlib_fs();
    let bytecode = match crate::compile::compile_source_with_std_fs(source, "main.vo", std_fs) {
        Ok(b) => b,
        Err(e) => return ("compile_error".into(), String::new(), e),
    };
    run_bytecode_async_with_externs(&bytecode, crate::vm::ext_bridge::register_wasm_ext_bridges)
        .await
}

/// Pre-load a WASM extension module before running Vo code.
///
/// Use this to register locally-built or pre-bundled WASM modules.
/// After pre-loading, `compileAndRunWithModules` will find the module
/// already registered and skip the GitHub fetch for it.
///
/// `module_path` is the Go-style module path, e.g. `"github.com/vo-lang/zip"`.
#[wasm_bindgen(js_name = "preloadExtModule")]
pub fn preload_ext_module(
    module_path: &str,
    bytes: &[u8],
    js_glue_url: Option<String>,
) -> js_sys::Promise {
    let module_path = module_path.to_string();
    let bytes = bytes.to_vec();
    let js_glue_url = js_glue_url.unwrap_or_default();
    wasm_bindgen_futures::future_to_promise(async move {
        crate::vm::ext_bridge::load_wasm_ext_module(&module_path, &bytes, &js_glue_url)
            .await
            .map_err(|e| JsValue::from_str(&e))?;
        Ok(JsValue::UNDEFINED)
    })
}

#[cfg(test)]
mod tests {
    #[test]
    fn vm_wasm_fetch_wake_key_002_async_fetch_wakes_with_host_wait_key() {
        let src = include_str!("async_runner.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("async runner source should contain tests section");
        assert!(
            src.contains("fn fetch_host_wait_key"),
            "fetch runner must resolve a complete HostWaitKey before await/wake"
        );
        assert!(
            src.contains("vm.wake_host_event(key)"),
            "fetch runner must wake through the HostWaitKey API"
        );
        assert!(
            src.contains("HostWaitSource::replay(vo_runtime::ffi::HostEventReplaySource::Fetch)"),
            "fetch runner must resolve the replay key with the Fetch host replay source"
        );
        assert!(
            !src.contains("wake_host_event_legacy_replay_token"),
            "fetch runner must not wake replay waiters through a legacy token"
        );
    }

    #[test]
    fn vm_wasm_fetch_replay_source_045_uses_fetch_source_identity() {
        let src = include_str!("async_runner.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("async runner source should contain tests section");
        assert!(
            src.contains("HostWaitSource::replay(vo_runtime::ffi::HostEventReplaySource::Fetch)"),
            "fetch wait-key lookup must not collapse Fetch replay into GUI or extension replay"
        );
    }
}
