//! Async VM execution loop for WASM targets.
//!
//! Handles the event-driven execution cycle including:
//! - HTTP fetch via browser Fetch API
//! - Timer-based sleep via setTimeout
//! - Host event processing (timers, async callbacks)

#[cfg(feature = "compiler")]
use core::cell::Cell;
#[cfg(feature = "compiler")]
use std::collections::{HashMap, HashSet};

#[cfg(feature = "compiler")]
use futures_util::future::{select, AbortHandle, Abortable, Either, LocalBoxFuture};
#[cfg(feature = "compiler")]
use futures_util::stream::{FuturesUnordered, StreamExt};
#[cfg(feature = "compiler")]
use futures_util::FutureExt;
#[cfg(feature = "compiler")]
use vo_vm::vm::SchedulingOutcome;
use wasm_bindgen::prelude::*;

#[cfg(feature = "compiler")]
use crate::js_types::make_run_result_obj;
#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
use crate::vm::ExternRegistrar;
#[cfg(feature = "compiler")]
use crate::vm::{decode_bytecode_module, register_wasm_runtime_externs};

// ── Outcome helpers ─────────────────────────────────────────────────────────

#[cfg(feature = "compiler")]
fn finish_async_outcome(
    vm: &vo_vm::vm::Vm,
    outcome: SchedulingOutcome,
) -> (String, String, String, Option<i32>) {
    let stdout = vo_runtime::output::take_output();
    match outcome {
        SchedulingOutcome::Completed => ("ok".into(), stdout, String::new(), None),
        SchedulingOutcome::Exited(code) => ("exited".into(), stdout, String::new(), Some(code)),
        SchedulingOutcome::Suspended => (
            "suspended".into(),
            stdout,
            String::from("vm suspended waiting for host-routed island commands/responses"),
            None,
        ),
        SchedulingOutcome::SuspendedForHostEvents => (
            "suspended".into(),
            stdout,
            String::from(
                "vm suspended waiting for host events, but the async runner had no event to drive",
            ),
            None,
        ),
        SchedulingOutcome::Blocked => (
            "error".into(),
            stdout,
            format!("{:?}", vm.deadlock_err()),
            None,
        ),
        SchedulingOutcome::Panicked => (
            "error".into(),
            stdout,
            String::from("unexpected bounded panic outcome"),
            None,
        ),
    }
}

// ── Fetch handling ──────────────────────────────────────────────────────────

#[cfg(feature = "compiler")]
struct FetchCompletion {
    request_id: i64,
    token: u64,
    result: vo_web_runtime_wasm::net_http::FetchResult,
}

/// Resolve one browser fetch without holding a VM borrow. The event loop can
/// race this future with Vo timers, allowing a timer-driven context cancel to
/// run and abort the same fetch.
#[cfg(feature = "compiler")]
async fn await_fetch(pending: vo_web_runtime_wasm::net_http::PendingFetch) -> FetchCompletion {
    let result = wasm_bindgen_futures::JsFuture::from(pending.promise).await;
    let fetch_result = match result {
        Ok(val) => vo_web_runtime_wasm::net_http::parse_fetch_js_value(&val),
        Err(e) => vo_web_runtime_wasm::net_http::FetchResult {
            status_code: 0,
            status: String::new(),
            proto: String::new(),
            headers: Vec::new(),
            body: Vec::new(),
            error: Some(format!("fetch error: {:?}", e)),
        },
    };
    FetchCompletion {
        request_id: pending.request_id,
        token: pending.token,
        result: fetch_result,
    }
}

#[cfg(feature = "compiler")]
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
#[cfg(feature = "compiler")]
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
#[cfg(feature = "compiler")]
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
/// Returns (status, stdout, stderr, exit_code).
#[cfg(feature = "compiler")]
async fn run_vm_async(bytecode: &[u8]) -> (String, String, String, Option<i32>) {
    vo_runtime::output::clear_output();
    let module = match decode_bytecode_module(bytecode) {
        Ok(m) => m,
        Err(e) => return ("error".into(), String::new(), e, None),
    };

    let mut vm = match vo_vm::vm::Vm::try_new() {
        Ok(vm) => vm,
        Err(error) => {
            return (
                "error".into(),
                String::new(),
                format!("Failed to initialize VM: {error}"),
                None,
            )
        }
    };
    let exts = &module.externs;
    let reg = match vm.extern_registry_mut() {
        Ok(registry) => registry,
        Err(error) => {
            return (
                "error".into(),
                String::new(),
                format!("Failed to configure VM externs: {error:?}"),
                None,
            )
        }
    };
    if let Err(error) = register_wasm_runtime_externs(reg, exts) {
        return (
            "error".into(),
            String::new(),
            format!("Failed to register WASM runtime externs: {error}"),
            None,
        );
    }
    if let Err(e) = vm.load_with_embedder_externs(module) {
        return ("error".into(), String::new(), format!("{:?}", e), None);
    }

    run_vm_async_inner(&mut vm).await
}

/// Async VM execution with an additional extern registrar on top of stdlib+web.
///
/// This is the WASM equivalent of `create_vm_from_module` but uses the full
/// async event loop so WaitIo/Sleep/HTTP work correctly.
/// Returns `(status, stdout, stderr, exit_code)`.
#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
pub async fn run_bytecode_async_with_externs(
    bytecode: &[u8],
    extra_reg: ExternRegistrar,
) -> (String, String, String, Option<i32>) {
    vo_runtime::output::clear_output();
    let module = match decode_bytecode_module(bytecode) {
        Ok(m) => m,
        Err(e) => return ("error".into(), String::new(), e, None),
    };

    let mut vm = match vo_vm::vm::Vm::try_new() {
        Ok(vm) => vm,
        Err(error) => {
            return (
                "error".into(),
                String::new(),
                format!("Failed to initialize VM: {error}"),
                None,
            )
        }
    };
    let exts = &module.externs;
    let reg = match vm.extern_registry_mut() {
        Ok(registry) => registry,
        Err(error) => {
            return (
                "error".into(),
                String::new(),
                format!("Failed to configure VM externs: {error:?}"),
                None,
            )
        }
    };
    if let Err(error) = register_wasm_runtime_externs(reg, exts) {
        return (
            "error".into(),
            String::new(),
            format!("Failed to register WASM runtime externs: {error}"),
            None,
        );
    }
    if let Err(error) = extra_reg(reg, exts) {
        return (
            "error".into(),
            String::new(),
            format!("Failed to register caller externs: {error}"),
            None,
        );
    }
    if let Err(e) = vm.load_with_embedder_externs(module) {
        return ("error".into(), String::new(), format!("{:?}", e), None);
    }

    run_vm_async_inner(&mut vm).await
}

/// Shared inner async run loop (extracted so both run_vm_async variants can use it).
#[cfg(feature = "compiler")]
pub(crate) async fn run_vm_async_inner(
    vm: &mut vo_vm::vm::Vm,
) -> (String, String, String, Option<i32>) {
    let Some(owner) = vo_web_runtime_wasm::net_http::allocate_http_owner() else {
        return (
            "error".into(),
            vo_runtime::output::take_output(),
            "process-wide HTTP VM owner identity space exhausted".into(),
            None,
        );
    };
    let cleanup = HttpRunCleanup { owner };
    let result = run_vm_async_owned(vm, owner).await;
    drop(cleanup);
    result
}

#[cfg(feature = "compiler")]
struct HttpRunCleanup {
    owner: u64,
}

#[cfg(feature = "compiler")]
impl Drop for HttpRunCleanup {
    fn drop(&mut self) {
        vo_web_runtime_wasm::net_http::cancel_http_requests_for_owner(self.owner);
    }
}

#[cfg(feature = "compiler")]
enum DrivenHostEvent {
    Fetch(FetchCompletion),
    Timer(Option<vo_vm::scheduler::HostWaitKey>),
    Idle,
}

#[cfg(feature = "compiler")]
async fn run_vm_async_owned(
    vm: &mut vo_vm::vm::Vm,
    owner: u64,
) -> (String, String, String, Option<i32>) {
    type FetchFuture = LocalBoxFuture<'static, FetchCompletion>;
    type TimerFuture = LocalBoxFuture<'static, Option<vo_vm::scheduler::HostWaitKey>>;

    let mut fetches = FuturesUnordered::<FetchFuture>::new();
    let mut fetch_tokens = HashSet::<u64>::new();
    let mut timers = FuturesUnordered::<TimerFuture>::new();
    let mut timer_aborts = HashMap::<vo_vm::scheduler::HostWaitKey, AbortHandle>::new();

    let mut outcome = match vo_web_runtime_wasm::net_http::with_http_owner(owner, || vm.run()) {
        Ok(o) => o,
        Err(e) => {
            return (
                "error".into(),
                vo_runtime::output::take_output(),
                format!("{:?}", e),
                None,
            )
        }
    };

    while outcome == SchedulingOutcome::SuspendedForHostEvents {
        for pending in vo_web_runtime_wasm::net_http::take_pending_fetch_promises() {
            if fetch_tokens.insert(pending.token) {
                fetches.push(await_fetch(pending).boxed_local());
            } else {
                vo_web_runtime_wasm::net_http::discard_fetch_result(
                    pending.request_id,
                    pending.token,
                );
            }
        }

        let timer_events = vm
            .take_pending_host_events()
            .into_iter()
            .filter(|e| !e.replay)
            .collect::<Vec<_>>();
        let live_timer_keys = timer_events
            .iter()
            .map(|event| event.key)
            .collect::<HashSet<_>>();
        let stale_timer_keys = timer_aborts
            .keys()
            .filter(|key| !live_timer_keys.contains(key))
            .copied()
            .collect::<Vec<_>>();
        for key in stale_timer_keys {
            if let Some(abort) = timer_aborts.remove(&key) {
                abort.abort();
            }
        }
        for event in timer_events {
            if timer_aborts.contains_key(&event.key) {
                continue;
            }
            let (abort, registration) = AbortHandle::new_pair();
            timer_aborts.insert(event.key, abort);
            timers.push(
                Abortable::new(
                    async move {
                        wasm_callback_sleep_ms(event.delay_ms).await;
                        event.key
                    },
                    registration,
                )
                .map(Result::ok)
                .boxed_local(),
            );
        }

        if fetches.is_empty() && timers.is_empty() {
            break;
        }

        let event = match (fetches.is_empty(), timers.is_empty()) {
            (false, false) => {
                let next_fetch = fetches.next();
                let next_timer = timers.next();
                futures_util::pin_mut!(next_fetch, next_timer);
                match select(next_fetch, next_timer).await {
                    Either::Left((Some(fetch), _)) => DrivenHostEvent::Fetch(fetch),
                    Either::Right((Some(timer), _)) => DrivenHostEvent::Timer(timer),
                    _ => DrivenHostEvent::Idle,
                }
            }
            (false, true) => fetches
                .next()
                .await
                .map(DrivenHostEvent::Fetch)
                .unwrap_or(DrivenHostEvent::Idle),
            (true, false) => timers
                .next()
                .await
                .map(DrivenHostEvent::Timer)
                .unwrap_or(DrivenHostEvent::Idle),
            (true, true) => DrivenHostEvent::Idle,
        };

        let should_run = match event {
            DrivenHostEvent::Fetch(fetch) => {
                fetch_tokens.remove(&fetch.token);
                let Ok(key) = fetch_host_wait_key(vm, fetch.token) else {
                    vo_web_runtime_wasm::net_http::discard_fetch_result(
                        fetch.request_id,
                        fetch.token,
                    );
                    continue;
                };
                if !vo_web_runtime_wasm::net_http::store_fetch_result(
                    fetch.request_id,
                    fetch.token,
                    fetch.result,
                ) {
                    continue;
                }
                if !vm.wake_host_event(key) {
                    vo_web_runtime_wasm::net_http::discard_fetch_result(
                        fetch.request_id,
                        fetch.token,
                    );
                    continue;
                }
                true
            }
            DrivenHostEvent::Timer(Some(key)) => {
                timer_aborts.remove(&key);
                vm.wake_host_event(key)
            }
            DrivenHostEvent::Timer(None) | DrivenHostEvent::Idle => false,
        };

        if should_run {
            outcome = match vo_web_runtime_wasm::net_http::with_http_owner(owner, || {
                vm.run_scheduled()
            }) {
                Ok(o) => o,
                Err(e) => {
                    return (
                        "error".into(),
                        vo_runtime::output::take_output(),
                        format!("{:?}", e),
                        None,
                    )
                }
            };
        }
    }

    finish_async_outcome(vm, outcome)
}

// ── WASM exports: compile-and-run ───────────────────────────────────────────

/// Compile and run in one step. Returns a
/// Promise<{status,stdout,stderr,exitCode}> to support async operations.
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
            None,
        );
        return js_sys::Promise::resolve(&obj);
    }
    let Some(bytecode) = result.bytecode else {
        let obj = make_run_result_obj(
            "compile_error",
            "",
            "compiler reported success without producing bytecode",
            None,
        );
        return js_sys::Promise::resolve(&obj);
    };
    wasm_bindgen_futures::future_to_promise(async move {
        let (status, stdout, stderr, exit_code) = run_vm_async(&bytecode).await;
        Ok(make_run_result_obj(&status, &stdout, &stderr, exit_code))
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
        let (status, stdout, stderr, exit_code) = run_with_modules_inner(&source).await;
        Ok(make_run_result_obj(&status, &stdout, &stderr, exit_code))
    })
}

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
async fn run_with_modules_inner(source: &str) -> (String, String, String, Option<i32>) {
    let std_fs = crate::compile::build_stdlib_fs();
    let bytecode = match crate::compile::compile_source_with_std_fs(source, "main.vo", std_fs) {
        Ok(b) => b,
        Err(e) => return ("compile_error".into(), String::new(), e, None),
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

    #[test]
    fn fetch_and_timer_waits_are_driven_concurrently_and_owner_scoped() {
        let src = include_str!("async_runner.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("async runner source should contain tests section");
        assert!(src.contains("select(next_fetch, next_timer).await"));
        assert!(src.contains("with_http_owner(owner"));
        assert!(src.contains("cancel_http_requests_for_owner(self.owner)"));
        assert!(src.contains("discard_fetch_result("));
    }
}
