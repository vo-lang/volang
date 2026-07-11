//! WASM net/http implementation via browser fetch API.
//!
//! Uses the HostEventWaitAndReplay mechanism:
//! - First extern call: starts fetch, registers Promise, returns HostEventWaitAndReplay
//! - After Promise resolves, result stored in FETCH_RESULTS
//! - Re-invoked extern: reads result, writes return slots, returns Ok

use core::cell::RefCell;

use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult, HostEventReplaySource};
use vo_runtime::gc::GcRef;
use vo_runtime::objects::{slice, string};
use vo_runtime::slot::slot_to_ptr;
use wasm_bindgen::prelude::*;

const NET_UNSUPPORTED: &str = "operation not supported on wasm";

#[wasm_bindgen(inline_js = r#"
export function voHttpBuildFetchPromise(method, url, headers, body) {
  const opts = { method, headers: {} };
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
    .catch((error) => ({ error: String(error) }));
}
"#)]
extern "C" {
    #[wasm_bindgen(js_name = voHttpBuildFetchPromise)]
    fn js_build_fetch_promise(
        method: &str,
        url: &str,
        headers: &js_sys::Array,
        body: &[u8],
    ) -> js_sys::Promise;
}

// Pending fetch Promises: (token, Promise). Consumed by run_vm_async.
thread_local! {
    static PENDING_FETCH_PROMISES: RefCell<Vec<(u64, js_sys::Promise)>> = const { RefCell::new(Vec::new()) };
}

// Fetch results stored after Promise resolves. Consumed by extern re-invocation.
thread_local! {
    static FETCH_RESULTS: RefCell<Vec<(u64, FetchResult)>> = const { RefCell::new(Vec::new()) };
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

/// Register a fetch Promise (called by the extern). run_vm_async picks it up.
pub fn register_fetch_promise(token: u64, promise: js_sys::Promise) {
    PENDING_FETCH_PROMISES.with(|cell| {
        cell.borrow_mut().push((token, promise));
    });
}

/// Take all pending fetch Promises (token, Promise). Called by run_vm_async.
pub fn take_pending_fetch_promises() -> Vec<(u64, js_sys::Promise)> {
    PENDING_FETCH_PROMISES.with(|cell| core::mem::take(&mut *cell.borrow_mut()))
}

/// Store a fetch result (called by run_vm_async after Promise resolves).
pub fn store_fetch_result(token: u64, result: FetchResult) {
    FETCH_RESULTS.with(|cell| {
        cell.borrow_mut().push((token, result));
    });
}

/// Take the fetch result for a token (called by the re-invoked extern).
pub fn take_fetch_result(token: u64) -> Option<FetchResult> {
    FETCH_RESULTS.with(|cell| {
        let mut v = cell.borrow_mut();
        v.iter()
            .position(|(t, _)| *t == token)
            .map(|pos| v.remove(pos).1)
    })
}

fn build_fetch_promise(
    method: &str,
    url: &str,
    headers: &[String],
    body: &[u8],
) -> Result<js_sys::Promise, String> {
    let header_values = js_sys::Array::new();
    for header in headers {
        header_values.push(&JsValue::from_str(header));
    }
    Ok(js_build_fetch_promise(method, url, &header_values, body))
}

/// Parse a resolved fetch Promise result JsValue into FetchResult.
pub fn parse_fetch_js_value(token: u64, val: &JsValue) -> FetchResult {
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

    let _ = token;
    FetchResult {
        status_code,
        status,
        proto,
        headers,
        body,
        error: None,
    }
}

fn read_string_slice(slice_ref: GcRef) -> Vec<String> {
    if slice_ref.is_null() {
        return Vec::new();
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
            result.push(unsafe { vo_runtime::objects::string::to_rust_string(str_ref) });
        } else {
            result.push(String::new());
        }
    }
    result
}

fn wasm_http_native_request(call: &mut ExternCallContext) -> ExternResult {
    if let Some(token) = call.take_resume_host_event_token() {
        let result = match take_fetch_result(token) {
            Some(r) => r,
            None => {
                return ExternResult::Panic(format!("fetch result missing for token {}", token));
            }
        };

        if let Some(err_msg) = result.error {
            call.ret_i64(0, 0);
            call.ret_str(1, "");
            call.ret_str(2, "");
            call.ret_nil(3);
            call.ret_nil(4);
            write_error_to(call, 5, &err_msg);
            return ExternResult::Ok;
        }

        call.ret_i64(0, result.status_code);
        call.ret_str(1, &result.status);
        call.ret_str(2, &result.proto);
        call.ret_string_slice(3, &result.headers);
        call.ret_bytes(4, &result.body);
        write_nil_error(call, 5);
        ExternResult::Ok
    } else {
        let method = call.arg_str(0).to_string();
        let url = call.arg_str(1).to_string();
        let headers_ref = call.arg_ref(2);
        let headers = read_string_slice(headers_ref);
        let body = call.arg_bytes(3).to_vec();
        // arg 4 (timeoutNs) ignored on WASM

        let token = call.next_host_event_token();
        match build_fetch_promise(&method, &url, &headers, &body) {
            Ok(promise) => {
                register_fetch_promise(token, promise);
                ExternResult::HostEventWaitAndReplay {
                    token,
                    source: HostEventReplaySource::Fetch,
                }
            }
            Err(e) => ExternResult::Panic(e),
        }
    }
}

fn net_int_error(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, -1);
    write_error_to(call, 1, NET_UNSUPPORTED);
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

pub fn register_externs(reg: &mut ExternRegistry, defs: &[ExternDef]) {
    use vo_runtime::bytecode::ExternEffects;

    for (id, def) in defs.iter().enumerate() {
        match def.name.as_str() {
            "net_http_nativeHttpsRequest" => reg.register_wasm_host_with_effects(
                id as u32,
                &def.name,
                wasm_http_native_request,
                ExternEffects::MAY_HOST_REPLAY,
            ),
            "net_dial"
            | "net_listen"
            | "net_listenPacket"
            | "net_blocking_tcpListenerAccept"
            | "net_unixDial"
            | "net_unixListen"
            | "net_blocking_unixListenerAccept" => {
                crate::register_wasm_host(reg, id as u32, &def.name, net_int_error)
            }
            "net_blocking_tcpConnRead"
            | "net_blocking_tcpConnWrite"
            | "net_blocking_udpConnWriteTo"
            | "net_blocking_unixConnRead"
            | "net_blocking_unixConnWrite" => {
                crate::register_wasm_host(reg, id as u32, &def.name, net_count_error)
            }
            "net_tcpConnClose"
            | "net_tcpListenerClose"
            | "net_tcpConnSetDeadline"
            | "net_tcpConnSetReadDeadline"
            | "net_tcpConnSetWriteDeadline"
            | "net_udpConnClose"
            | "net_udpConnSetDeadline"
            | "net_udpConnSetReadDeadline"
            | "net_udpConnSetWriteDeadline"
            | "net_unixConnSetDeadline"
            | "net_unixConnSetReadDeadline"
            | "net_unixConnSetWriteDeadline"
            | "net_unixConnClose"
            | "net_unixListenerClose" => {
                crate::register_wasm_host(reg, id as u32, &def.name, net_error_only)
            }
            "net_tcpConnLocalAddr"
            | "net_tcpConnRemoteAddr"
            | "net_tcpListenerAddr"
            | "net_udpConnLocalAddr" => {
                crate::register_wasm_host(reg, id as u32, &def.name, net_empty_string)
            }
            "net_blocking_udpConnReadFrom" => {
                crate::register_wasm_host(reg, id as u32, &def.name, net_count_addr_error)
            }
            "net_resolveTCPAddr" | "net_resolveUDPAddr" | "net_lookupHost" | "net_lookupIP"
            | "net_lookupAddr" => {
                crate::register_wasm_host(reg, id as u32, &def.name, net_ref_error)
            }
            _ => {}
        }
    }
}
