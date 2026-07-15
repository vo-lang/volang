//! WASM net/http implementation via browser fetch API.
//!
//! Uses the HostEventWaitAndReplay mechanism:
//! - First extern call: starts fetch, registers Promise, returns HostEventWaitAndReplay
//! - After Promise resolves, result stored in FETCH_RESULTS
//! - Re-invoked extern: reads result, writes return slots, returns Ok

use core::cell::{Cell, RefCell};
use std::collections::BTreeSet;

use vo_runtime::builtins::error_helper::{create_error, write_error_to, write_nil_error};
use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{
    ExternCallContext, ExternContractError, ExternRegistry, ExternResult, HostEventReplaySource,
};
use vo_runtime::gc::GcRef;
use vo_runtime::objects::{slice, string};
use vo_runtime::slot::slot_to_ptr;
use wasm_bindgen::prelude::*;

use crate::text::{utf8_arg, utf8_bytes};

const NET_UNSUPPORTED: &str = "operation not supported on wasm";
const NET_ERROR_MESSAGES: [&str; 6] = [
    "use of closed network connection",
    "i/o timeout",
    "connection refused",
    "connection reset by peer",
    "address already in use",
    "address not available",
];
const HTTP_TIMEOUT_MESSAGE: &str = "request timeout";

#[wasm_bindgen(inline_js = r#"
const voHttpControllers = new Map();

export function voHttpBuildFetchPromise(requestId, method, url, headers, body, timeoutMs) {
  if (voHttpControllers.has(requestId)) {
    throw new Error(`duplicate HTTP request id ${requestId}`);
  }
  const controller = new AbortController();
  voHttpControllers.set(requestId, controller);
  const opts = { method, headers: {}, signal: controller.signal };
  for (const line of headers) {
    const index = line.indexOf(':');
    if (index < 0) {
      continue;
    }
    const key = line.slice(0, index).trim();
    const value = line.slice(index + 1).trimStart();
    if (key.length > 0) {
      opts.headers[key] = value;
    }
  }
  if (body && body.length > 0) {
    opts.body = body;
  }
  let timeoutId;
  let timedOut = false;
  if (Number.isFinite(timeoutMs) && timeoutMs > 0) {
    timeoutId = setTimeout(() => {
      timedOut = true;
      controller.abort();
    }, timeoutMs);
  }
  return fetch(url, opts)
    .then(async (response) => {
      const resolvedHeaders = [];
      response.headers.forEach((value, key) => {
        resolvedHeaders.push(`${key}: ${value}`);
      });
      return {
        statusCode: response.status,
        status: `HTTP/1.1 ${response.status} ${response.statusText}`,
        proto: 'HTTP/1.1',
        headers: resolvedHeaders,
        body: new Uint8Array(await response.arrayBuffer()),
      };
    })
    .catch((error) => ({ error: timedOut ? 'request timeout' : String(error) }))
    .finally(() => {
      if (timeoutId !== undefined) {
        clearTimeout(timeoutId);
      }
      if (voHttpControllers.get(requestId) === controller) {
        voHttpControllers.delete(requestId);
      }
    });
}

export function voHttpCancelFetch(requestId) {
  const controller = voHttpControllers.get(requestId);
  if (controller === undefined) {
    return false;
  }
  voHttpControllers.delete(requestId);
  controller.abort();
  return true;
}
"#)]
extern "C" {
    #[wasm_bindgen(catch, js_name = voHttpBuildFetchPromise)]
    fn js_build_fetch_promise(
        request_id: &str,
        method: &str,
        url: &str,
        headers: &js_sys::Array,
        body: &[u8],
        timeout_ms: u32,
    ) -> Result<js_sys::Promise, JsValue>;

    #[wasm_bindgen(js_name = voHttpCancelFetch)]
    fn js_cancel_fetch(request_id: &str) -> bool;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum WasmRequestPhase {
    Allocated,
    Started { token: u64 },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum WasmCancelAction {
    Missing,
    RemovedBeforeStart,
    AbortStarted,
}

#[derive(Clone, Copy, Debug)]
struct WasmRequest {
    id: i64,
    owner: u64,
    canceled: bool,
    phase: WasmRequestPhase,
}

pub struct PendingFetch {
    pub request_id: i64,
    pub token: u64,
    pub promise: js_sys::Promise,
}

struct StoredFetchResult {
    owner: u64,
    request_id: i64,
    token: u64,
    result: FetchResult,
}

thread_local! {
    static NEXT_HTTP_REQUEST_ID: Cell<u64> = const { Cell::new(1) };
    static NEXT_HTTP_OWNER_ID: Cell<u64> = const { Cell::new(1) };
    static CURRENT_HTTP_OWNER_ID: Cell<u64> = const { Cell::new(0) };
    static HTTP_REQUESTS: RefCell<Vec<WasmRequest>> = const { RefCell::new(Vec::new()) };
    static PENDING_FETCH_PROMISES: RefCell<Vec<PendingFetch>> = const { RefCell::new(Vec::new()) };
}

// Fetch results stored after Promise resolves. Consumed by extern re-invocation.
thread_local! {
    static FETCH_RESULTS: RefCell<Vec<StoredFetchResult>> = const { RefCell::new(Vec::new()) };
}

#[derive(Debug)]
pub struct FetchResult {
    pub status_code: i64,
    pub status: String,
    pub proto: String,
    pub headers: Vec<String>,
    pub body: Vec<u8>,
    pub error: Option<String>,
}

fn allocate_positive_i64(cell: &Cell<u64>) -> Option<i64> {
    let current = cell.get();
    if current == 0 || current > i64::MAX as u64 {
        return None;
    }
    cell.set(if current == i64::MAX as u64 {
        0
    } else {
        current + 1
    });
    i64::try_from(current).ok()
}

fn allocate_positive_u64(cell: &Cell<u64>) -> Option<u64> {
    let current = cell.get();
    if current == 0 {
        return None;
    }
    cell.set(current.checked_add(1).unwrap_or(0));
    Some(current)
}

pub fn allocate_http_owner() -> Option<u64> {
    NEXT_HTTP_OWNER_ID.with(allocate_positive_u64)
}

struct HttpOwnerGuard {
    previous: u64,
}

impl Drop for HttpOwnerGuard {
    fn drop(&mut self) {
        CURRENT_HTTP_OWNER_ID.with(|owner| owner.set(self.previous));
    }
}

pub fn with_http_owner<T>(owner: u64, f: impl FnOnce() -> T) -> T {
    let previous = CURRENT_HTTP_OWNER_ID.with(|current| current.replace(owner));
    let _guard = HttpOwnerGuard { previous };
    f()
}

fn allocate_request() -> Result<i64, String> {
    let owner = CURRENT_HTTP_OWNER_ID.with(Cell::get);
    if owner == 0 {
        return Err("http: client request is outside an owned async VM run".to_string());
    }
    let id = NEXT_HTTP_REQUEST_ID
        .with(allocate_positive_i64)
        .ok_or_else(|| "http: request handle space exhausted".to_string())?;
    HTTP_REQUESTS.with(|requests| {
        let mut requests = requests.borrow_mut();
        if requests.iter().any(|request| request.id == id) {
            return Err(format!("http: duplicate request handle {id}"));
        }
        requests.push(WasmRequest {
            id,
            owner,
            canceled: false,
            phase: WasmRequestPhase::Allocated,
        });
        Ok(id)
    })
}

fn request_is_canceled(id: i64) -> Result<bool, String> {
    HTTP_REQUESTS.with(|requests| {
        requests
            .borrow()
            .iter()
            .find(|request| request.id == id)
            .map(|request| request.canceled)
            .ok_or_else(|| "http: invalid or released request handle".to_string())
    })
}

fn mark_request_started(id: i64, token: u64) -> Result<(), String> {
    HTTP_REQUESTS.with(|requests| {
        let mut requests = requests.borrow_mut();
        let request = requests
            .iter_mut()
            .find(|request| request.id == id)
            .ok_or_else(|| "http: invalid or released request handle".to_string())?;
        if request.canceled {
            return Err("request canceled".to_string());
        }
        if request.phase != WasmRequestPhase::Allocated {
            return Err("http: request handle was already started".to_string());
        }
        request.phase = WasmRequestPhase::Started { token };
        Ok(())
    })
}

fn cancel_request_state(id: i64) -> WasmCancelAction {
    HTTP_REQUESTS.with(|requests| {
        let mut requests = requests.borrow_mut();
        let Some(index) = requests.iter().position(|request| request.id == id) else {
            return WasmCancelAction::Missing;
        };
        if requests[index].phase == WasmRequestPhase::Allocated {
            requests.remove(index);
            return WasmCancelAction::RemovedBeforeStart;
        }
        let request = &mut requests[index];
        request.canceled = true;
        WasmCancelAction::AbortStarted
    })
}

fn cancel_request(id: i64) {
    if cancel_request_state(id) == WasmCancelAction::AbortStarted {
        js_cancel_fetch(&id.to_string());
    }
}

fn release_request(id: i64, abort: bool) -> bool {
    let removed = HTTP_REQUESTS.with(|requests| {
        let mut requests = requests.borrow_mut();
        requests
            .iter()
            .position(|request| request.id == id)
            .map(|index| requests.remove(index))
    });
    if let Some(request) = removed {
        if abort && matches!(request.phase, WasmRequestPhase::Started { .. }) {
            js_cancel_fetch(&id.to_string());
        }
        true
    } else {
        false
    }
}

/// Register a fetch Promise (called by the extern). run_vm_async picks it up.
fn register_fetch_promise(request_id: i64, token: u64, promise: js_sys::Promise) {
    PENDING_FETCH_PROMISES.with(|cell| {
        cell.borrow_mut().push(PendingFetch {
            request_id,
            token,
            promise,
        });
    });
}

/// Take all pending fetch Promises. Called by run_vm_async.
pub fn take_pending_fetch_promises() -> Vec<PendingFetch> {
    PENDING_FETCH_PROMISES.with(|cell| core::mem::take(&mut *cell.borrow_mut()))
}

/// Store a fetch result when its request and token still match a live VM wait.
pub fn store_fetch_result(request_id: i64, token: u64, result: FetchResult) -> bool {
    let request = HTTP_REQUESTS.with(|requests| {
        let mut requests = requests.borrow_mut();
        let index = requests.iter().position(|request| {
            request.id == request_id && request.phase == (WasmRequestPhase::Started { token })
        })?;
        Some(requests.remove(index))
    });
    let Some(request) = request else {
        return false;
    };
    FETCH_RESULTS.with(|cell| {
        cell.borrow_mut().push(StoredFetchResult {
            owner: request.owner,
            request_id,
            token,
            result,
        });
    });
    true
}

/// Take the fetch result for an exact request/token pair on extern replay.
fn take_fetch_result(request_id: i64, token: u64) -> Option<FetchResult> {
    FETCH_RESULTS.with(|cell| {
        let mut v = cell.borrow_mut();
        v.iter()
            .position(|result| result.request_id == request_id && result.token == token)
            .map(|pos| v.remove(pos).result)
    })
}

pub fn discard_fetch_result(request_id: i64, token: u64) {
    release_request(request_id, true);
    FETCH_RESULTS.with(|results| {
        results
            .borrow_mut()
            .retain(|result| result.request_id != request_id || result.token != token);
    });
}

pub fn cancel_http_requests_for_owner(owner: u64) {
    let removed = HTTP_REQUESTS.with(|requests| {
        let mut requests = requests.borrow_mut();
        let mut removed = Vec::new();
        requests.retain(|request| {
            if request.owner == owner {
                removed.push(*request);
                false
            } else {
                true
            }
        });
        removed
    });
    let ids = removed
        .iter()
        .map(|request| request.id)
        .collect::<BTreeSet<_>>();
    for request in removed {
        if matches!(request.phase, WasmRequestPhase::Started { .. }) {
            js_cancel_fetch(&request.id.to_string());
        }
    }
    PENDING_FETCH_PROMISES.with(|pending| {
        pending
            .borrow_mut()
            .retain(|fetch| !ids.contains(&fetch.request_id));
    });
    FETCH_RESULTS.with(|results| {
        results.borrow_mut().retain(|result| result.owner != owner);
    });
}

fn build_fetch_promise(
    request_id: i64,
    method: &str,
    url: &str,
    headers: &[String],
    body: &[u8],
    timeout_ns: i64,
) -> Result<js_sys::Promise, String> {
    let header_values = js_sys::Array::new();
    for header in headers {
        header_values.push(&JsValue::from_str(header));
    }
    let timeout_ms = if timeout_ns > 0 {
        let nanoseconds = timeout_ns as u64;
        nanoseconds
            .saturating_add(999_999)
            .checked_div(1_000_000)
            .unwrap_or(0)
            .min(u64::from(u32::MAX)) as u32
    } else {
        0
    };
    js_build_fetch_promise(
        &request_id.to_string(),
        method,
        url,
        &header_values,
        body,
        timeout_ms,
    )
    .map_err(|error| {
        error
            .as_string()
            .unwrap_or_else(|| "failed to construct browser fetch request".to_string())
    })
}

/// Parse a resolved fetch Promise result JsValue into FetchResult.
pub fn parse_fetch_js_value(val: &JsValue) -> FetchResult {
    if let Some(err) = js_sys::Reflect::get(val, &"error".into())
        .ok()
        .and_then(|v| v.as_string())
    {
        return FetchResult {
            status_code: 0,
            status: String::new(),
            proto: String::new(),
            headers: Vec::new(),
            body: Vec::new(),
            error: Some(err),
        };
    }

    let status_code = js_sys::Reflect::get(val, &"statusCode".into())
        .ok()
        .and_then(|v| v.as_f64())
        .unwrap_or(0.0) as i64;

    let status = js_sys::Reflect::get(val, &"status".into())
        .ok()
        .and_then(|v| v.as_string())
        .unwrap_or_default();

    let proto = js_sys::Reflect::get(val, &"proto".into())
        .ok()
        .and_then(|v| v.as_string())
        .unwrap_or_else(|| "HTTP/1.1".to_string());

    let headers_arr = js_sys::Reflect::get(val, &"headers".into()).unwrap_or(JsValue::UNDEFINED);
    let mut headers = Vec::new();
    if let Ok(len) = js_sys::Reflect::get(&headers_arr, &"length".into())
        .map(|v| v.as_f64().unwrap_or(0.0) as u32)
    {
        for i in 0..len {
            if let Ok(item) = js_sys::Reflect::get_u32(&headers_arr, i) {
                if let Some(s) = item.as_string() {
                    headers.push(s);
                }
            }
        }
    }

    let body_arr = js_sys::Reflect::get(val, &"body".into()).unwrap_or(JsValue::UNDEFINED);
    let body = if body_arr.is_undefined() || body_arr.is_null() {
        Vec::new()
    } else {
        js_sys::Uint8Array::from(body_arr).to_vec()
    };

    FetchResult {
        status_code,
        status,
        proto,
        headers,
        body,
        error: None,
    }
}

fn read_string_slice(slice_ref: GcRef) -> Result<Vec<String>, String> {
    if slice_ref.is_null() {
        return Ok(Vec::new());
    }
    // Safety: the resolved extern ABI verifies this argument as a rooted
    // []string value before the host provider is invoked.
    let len = unsafe { slice::len(slice_ref) };
    let elem_bytes = core::mem::size_of::<GcRef>();
    let mut result = Vec::with_capacity(len);
    for i in 0..len {
        let raw = unsafe { slice::get(slice_ref, i, elem_bytes) };
        let str_ref: GcRef = slot_to_ptr(raw);
        if !str_ref.is_null() {
            // Safety: the typed slice supplies live string elements.
            let bytes = unsafe { vo_runtime::objects::string::to_bytes(str_ref) };
            let value = utf8_bytes(&bytes)
                .map_err(|_| format!("http: header line {i} contains invalid UTF-8"))?;
            result.push(value.to_string());
        } else {
            result.push(String::new());
        }
    }
    Ok(result)
}

fn write_request_error(call: &mut ExternCallContext, message: &str) -> ExternResult {
    call.ret_i64(0, 0);
    call.ret_str(1, "");
    call.ret_str(2, "");
    call.ret_nil(3);
    call.ret_nil(4);
    if message == HTTP_TIMEOUT_MESSAGE {
        init_http_errors(call);
        let Some(pair) = call.sentinel_errors().get_one("http", 0) else {
            return ExternResult::Panic("http sentinel error initialization failed".to_string());
        };
        call.ret_interface_pair(5, pair);
    } else {
        write_error_to(call, 5, message);
    }
    ExternResult::Ok
}

fn init_http_errors(call: &mut ExternCallContext) {
    if call.sentinel_errors().contains("http") {
        return;
    }
    let errors = vec![create_error(call, HTTP_TIMEOUT_MESSAGE)];
    call.sentinel_errors_mut().insert("http", errors);
}

fn http_get_errors(call: &mut ExternCallContext) -> ExternResult {
    init_http_errors(call);
    let Some(pair) = call.sentinel_errors().get_one("http", 0) else {
        return ExternResult::Panic("http sentinel error initialization failed".to_string());
    };
    call.ret_interface_pair(0, pair);
    ExternResult::Ok
}

fn wasm_http_native_new_request(call: &mut ExternCallContext) -> ExternResult {
    match allocate_request() {
        Ok(id) => {
            call.ret_i64(0, id);
            write_nil_error(call, 1);
        }
        Err(error) => {
            call.ret_i64(0, 0);
            write_error_to(call, 1, &error);
        }
    }
    ExternResult::Ok
}

fn wasm_http_native_cancel_request(call: &mut ExternCallContext) -> ExternResult {
    let id = call.arg_i64(0);
    if id > 0 {
        cancel_request(id);
    }
    ExternResult::Ok
}

fn wasm_http_native_release_request(call: &mut ExternCallContext) -> ExternResult {
    let id = call.arg_i64(0);
    if id > 0 {
        release_request(id, true);
    }
    ExternResult::Ok
}

fn wasm_http_native_request(call: &mut ExternCallContext) -> ExternResult {
    let request_id = call.arg_i64(0);
    if let Some(token) = call.take_resume_host_event_token() {
        let result = match take_fetch_result(request_id, token) {
            Some(r) => r,
            None => {
                return write_request_error(
                    call,
                    &format!("http: fetch result missing for request {request_id}, token {token}"),
                );
            }
        };

        if let Some(err_msg) = result.error {
            return write_request_error(call, &err_msg);
        }

        call.ret_i64(0, result.status_code);
        call.ret_str(1, &result.status);
        call.ret_str(2, &result.proto);
        call.ret_string_slice(3, &result.headers);
        call.ret_bytes(4, &result.body);
        write_nil_error(call, 5);
        ExternResult::Ok
    } else {
        match request_is_canceled(request_id) {
            Ok(false) => {}
            Ok(true) => {
                release_request(request_id, false);
                return write_request_error(call, "request canceled");
            }
            Err(error) => return write_request_error(call, &error),
        }
        let method = match utf8_arg(call, 1) {
            Ok(method) => method,
            Err(_) => return write_request_error(call, "http: method contains invalid UTF-8"),
        };
        let url = match utf8_arg(call, 2) {
            Ok(url) => url,
            Err(_) => return write_request_error(call, "http: URL contains invalid UTF-8"),
        };
        let headers_ref = call.arg_ref(3);
        let headers = match read_string_slice(headers_ref) {
            Ok(headers) => headers,
            Err(error) => return write_request_error(call, &error),
        };
        let body = call.arg_bytes(4);
        let timeout_ns = call.arg_i64(5);

        let Some(token) = call.try_next_host_event_token() else {
            release_request(request_id, false);
            return write_request_error(call, "http: host event token space exhausted");
        };
        match build_fetch_promise(request_id, &method, &url, &headers, body, timeout_ns) {
            Ok(promise) => {
                if let Err(error) = mark_request_started(request_id, token) {
                    js_cancel_fetch(&request_id.to_string());
                    release_request(request_id, false);
                    return write_request_error(call, &error);
                }
                register_fetch_promise(request_id, token, promise);
                ExternResult::HostEventWaitAndReplay {
                    token,
                    source: HostEventReplaySource::Fetch,
                }
            }
            Err(error) => {
                release_request(request_id, false);
                write_request_error(call, &error)
            }
        }
    }
}

fn net_int_error(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, -1);
    write_error_to(call, 1, NET_UNSUPPORTED);
    ExternResult::Ok
}

fn net_get_errors(call: &mut ExternCallContext) -> ExternResult {
    if !call.sentinel_errors().contains("net") {
        let errors = NET_ERROR_MESSAGES
            .iter()
            .map(|message| create_error(call, message))
            .collect();
        call.sentinel_errors_mut().insert("net", errors);
    }

    let Some(errors) = call
        .sentinel_errors()
        .get("net")
        .map(|errors| errors.to_vec())
    else {
        return ExternResult::Panic("net sentinel error initialization failed".to_string());
    };
    for (index, pair) in errors.into_iter().enumerate() {
        let slot = u16::try_from(index * 2).expect("the fixed net error table fits in u16 slots");
        call.ret_interface_pair(slot, pair);
    }
    ExternResult::Ok
}

fn net_count_error(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, 0);
    write_error_to(call, 1, NET_UNSUPPORTED);
    ExternResult::Ok
}

fn net_error_only(call: &mut ExternCallContext) -> ExternResult {
    write_error_to(call, 0, NET_UNSUPPORTED);
    ExternResult::Ok
}

fn net_empty_string(call: &mut ExternCallContext) -> ExternResult {
    let s = string::from_rust_str(call.gc(), "");
    call.ret_ref(0, s);
    ExternResult::Ok
}

fn net_count_addr_error(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, 0);
    let s = string::from_rust_str(call.gc(), "");
    call.ret_ref(1, s);
    write_error_to(call, 2, NET_UNSUPPORTED);
    ExternResult::Ok
}

fn net_ref_error(call: &mut ExternCallContext) -> ExternResult {
    call.ret_nil(0);
    write_error_to(call, 1, NET_UNSUPPORTED);
    ExternResult::Ok
}

pub fn register_externs(
    reg: &mut ExternRegistry,
    defs: &[ExternDef],
) -> Result<(), ExternContractError> {
    use vo_runtime::bytecode::ExternEffects;

    let mut seen_names = BTreeSet::new();
    for (id, def) in defs.iter().enumerate() {
        if !seen_names.insert(def.name.as_str()) {
            continue;
        }
        match def.name.as_str() {
            vo_runtime::vo_extern_name!("net", "getNetErrors") => {
                crate::register_wasm_host(reg, id as u32, &def.name, net_get_errors)?
            }
            vo_runtime::vo_extern_name!("net/http", "getHttpErrors") => {
                crate::register_wasm_host(reg, id as u32, &def.name, http_get_errors)?
            }
            vo_runtime::vo_extern_name!("net/http", "nativeNewClientRequest") => {
                crate::register_wasm_host(reg, id as u32, &def.name, wasm_http_native_new_request)?
            }
            vo_runtime::vo_extern_name!("net/http", "nativeCancelClientRequest") => {
                crate::register_wasm_host(
                    reg,
                    id as u32,
                    &def.name,
                    wasm_http_native_cancel_request,
                )?
            }
            vo_runtime::vo_extern_name!("net/http", "nativeReleaseClientRequest") => {
                crate::register_wasm_host(
                    reg,
                    id as u32,
                    &def.name,
                    wasm_http_native_release_request,
                )?
            }
            vo_runtime::vo_extern_name!("net/http", "nativeHttpsRequest") => reg
                .try_register_wasm_host_with_effects(
                    id as u32,
                    &def.name,
                    wasm_http_native_request,
                    ExternEffects::MAY_HOST_REPLAY,
                )?,
            vo_runtime::vo_extern_name!("net", "dial")
            | vo_runtime::vo_extern_name!("net", "listen")
            | vo_runtime::vo_extern_name!("net", "listenPacket")
            | vo_runtime::vo_extern_name!("net", "blocking_tcpListenerAccept")
            | vo_runtime::vo_extern_name!("net", "unixDial")
            | vo_runtime::vo_extern_name!("net", "unixListen")
            | vo_runtime::vo_extern_name!("net", "blocking_unixListenerAccept") => {
                crate::register_wasm_host(reg, id as u32, &def.name, net_int_error)?
            }
            vo_runtime::vo_extern_name!("net", "blocking_tcpConnRead")
            | vo_runtime::vo_extern_name!("net", "blocking_tcpConnWrite")
            | vo_runtime::vo_extern_name!("net", "blocking_udpConnWriteTo")
            | vo_runtime::vo_extern_name!("net", "blocking_unixConnRead")
            | vo_runtime::vo_extern_name!("net", "blocking_unixConnWrite") => {
                crate::register_wasm_host(reg, id as u32, &def.name, net_count_error)?
            }
            vo_runtime::vo_extern_name!("net", "tcpConnClose")
            | vo_runtime::vo_extern_name!("net", "tcpListenerClose")
            | vo_runtime::vo_extern_name!("net", "tcpConnSetDeadline")
            | vo_runtime::vo_extern_name!("net", "tcpConnSetReadDeadline")
            | vo_runtime::vo_extern_name!("net", "tcpConnSetWriteDeadline")
            | vo_runtime::vo_extern_name!("net", "udpConnClose")
            | vo_runtime::vo_extern_name!("net", "udpConnSetDeadline")
            | vo_runtime::vo_extern_name!("net", "udpConnSetReadDeadline")
            | vo_runtime::vo_extern_name!("net", "udpConnSetWriteDeadline")
            | vo_runtime::vo_extern_name!("net", "unixConnSetDeadline")
            | vo_runtime::vo_extern_name!("net", "unixConnSetReadDeadline")
            | vo_runtime::vo_extern_name!("net", "unixConnSetWriteDeadline")
            | vo_runtime::vo_extern_name!("net", "unixConnClose")
            | vo_runtime::vo_extern_name!("net", "unixListenerClose") => {
                crate::register_wasm_host(reg, id as u32, &def.name, net_error_only)?
            }
            vo_runtime::vo_extern_name!("net", "tcpConnLocalAddr")
            | vo_runtime::vo_extern_name!("net", "tcpConnRemoteAddr")
            | vo_runtime::vo_extern_name!("net", "tcpListenerAddr")
            | vo_runtime::vo_extern_name!("net", "udpConnLocalAddr") => {
                crate::register_wasm_host(reg, id as u32, &def.name, net_empty_string)?
            }
            vo_runtime::vo_extern_name!("net", "blocking_udpConnReadFrom") => {
                crate::register_wasm_host(reg, id as u32, &def.name, net_count_addr_error)?
            }
            vo_runtime::vo_extern_name!("net", "resolveTCPAddr")
            | vo_runtime::vo_extern_name!("net", "resolveUDPAddr")
            | vo_runtime::vo_extern_name!("net", "lookupHost")
            | vo_runtime::vo_extern_name!("net", "lookupIP")
            | vo_runtime::vo_extern_name!("net", "lookupAddr") => {
                crate::register_wasm_host(reg, id as u32, &def.name, net_ref_error)?
            }
            _ => {}
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn request_and_owner_ids_exhaust_without_wrapping() {
        let request_ids = Cell::new(i64::MAX as u64);
        assert_eq!(allocate_positive_i64(&request_ids), Some(i64::MAX));
        assert_eq!(allocate_positive_i64(&request_ids), None);
        assert_eq!(request_ids.get(), 0);

        let owner_ids = Cell::new(u64::MAX);
        assert_eq!(allocate_positive_u64(&owner_ids), Some(u64::MAX));
        assert_eq!(allocate_positive_u64(&owner_ids), None);
        assert_eq!(owner_ids.get(), 0);
    }

    #[test]
    fn owner_cleanup_removes_requests_that_never_reached_fetch() {
        let owner = allocate_http_owner().expect("HTTP owner id");
        let id = with_http_owner(owner, allocate_request).expect("HTTP request id");
        assert_eq!(request_is_canceled(id), Ok(false));
        assert!(
            allocate_request().is_err(),
            "owner guard must restore its caller"
        );

        cancel_http_requests_for_owner(owner);

        assert!(request_is_canceled(id).is_err());
    }

    #[test]
    fn pre_start_cancel_is_observed_before_fetch_registration() {
        let owner = allocate_http_owner().expect("HTTP owner id");
        let id = with_http_owner(owner, allocate_request).expect("HTTP request id");
        assert_eq!(
            cancel_request_state(id),
            WasmCancelAction::RemovedBeforeStart
        );
        assert!(request_is_canceled(id).is_err());
        assert!(mark_request_started(id, 7).is_err());
        assert!(!release_request(id, false));
    }

    #[test]
    fn started_cancel_preserves_the_exact_replay_identity_until_completion() {
        let owner = allocate_http_owner().expect("HTTP owner id");
        let id = with_http_owner(owner, allocate_request).expect("HTTP request id");
        mark_request_started(id, 41).expect("start request");

        assert_eq!(cancel_request_state(id), WasmCancelAction::AbortStarted);
        assert_eq!(request_is_canceled(id), Ok(true));
        assert!(store_fetch_result(
            id,
            41,
            FetchResult {
                status_code: 0,
                status: String::new(),
                proto: String::new(),
                headers: Vec::new(),
                body: Vec::new(),
                error: Some(String::from("AbortError")),
            },
        ));
        assert_eq!(
            take_fetch_result(id, 41).and_then(|result| result.error),
            Some(String::from("AbortError"))
        );
        assert!(request_is_canceled(id).is_err());
    }

    #[test]
    fn javascript_bridge_has_abort_and_exact_controller_cleanup_contracts() {
        let source = include_str!("net_http.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("net/http source should contain tests section");
        assert!(source.contains("const controller = new AbortController()"));
        assert!(source.contains("voHttpControllers.get(requestId) === controller"));
        assert!(source.contains("voHttpControllers.delete(requestId)"));
        assert!(source.contains("controller.abort()"));
    }
}
