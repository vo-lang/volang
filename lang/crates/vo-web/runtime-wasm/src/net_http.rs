//! WASM net/http implementation via browser fetch API.
//!
//! Uses the HostEventWaitAndReplay mechanism:
//! - First extern call: starts fetch, registers Promise, returns HostEventWaitAndReplay
//! - After Promise resolves, result stored in FETCH_RESULTS
//! - Re-invoked extern: reads result, writes return slots, returns Ok

use core::cell::RefCell;
use core::sync::atomic::{AtomicU64, Ordering};

use vo_runtime::bytecode::ExternDef;
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult};
use vo_runtime::gc::GcRef;
use vo_runtime::objects::slice;
use vo_runtime::slot::slot_to_ptr;
use wasm_bindgen::prelude::*;

static FETCH_TOKEN: AtomicU64 = AtomicU64::new(1);

fn next_fetch_token() -> u64 {
    FETCH_TOKEN.fetch_add(1, Ordering::Relaxed)
}

/// Pending fetch Promises: (token, Promise). Consumed by run_vm_async.
thread_local! {
    static PENDING_FETCH_PROMISES: RefCell<Vec<(u64, js_sys::Promise)>> = RefCell::new(Vec::new());
}

/// Fetch results stored after Promise resolves. Consumed by extern re-invocation.
thread_local! {
    static FETCH_RESULTS: RefCell<Vec<(u64, FetchResult)>> = RefCell::new(Vec::new());
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
        if let Some(pos) = v.iter().position(|(t, _)| *t == token) {
            Some(v.remove(pos).1)
        } else {
            None
        }
    })
}

fn js_str_escape(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('\'', "\\'")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
}

fn build_js_headers_obj(headers: &[String]) -> String {
    let mut obj = "{".to_string();
    for line in headers {
        if let Some(pos) = line.find(": ") {
            let k = js_str_escape(&line[..pos]);
            let v = js_str_escape(&line[pos + 2..]);
            obj.push_str(&format!("'{}':'{}',", k, v));
        }
    }
    obj.push('}');
    obj
}

fn build_fetch_promise(method: &str, url: &str, headers: &[String], body: &[u8]) -> Result<js_sys::Promise, String> {
    let headers_obj = build_js_headers_obj(headers);
    let body_init = if body.is_empty() {
        String::new()
    } else {
        let bytes = body.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(",");
        format!("opts.body=new Uint8Array([{}]);", bytes)
    };

    let script = format!(
        concat!(
            "(function(){{",
            "var opts={{method:'{method}',headers:{headers}}};",
            "{body_init}",
            "return fetch('{url}',opts).then(function(r){{",
            "var sc=r.status;var st=r.statusText;",
            "var hds=[];r.headers.forEach(function(v,k){{hds.push(k+': '+v);}});",
            "return r.arrayBuffer().then(function(buf){{",
            "return{{statusCode:sc,status:'HTTP/1.1 '+sc+' '+st,proto:'HTTP/1.1',headers:hds,body:new Uint8Array(buf)}};",
            "}});",
            "}}).catch(function(e){{return{{error:String(e)}};}})",
            "}})()"
        ),
        method = js_str_escape(method),
        url = js_str_escape(url),
        headers = headers_obj,
        body_init = body_init,
    );

    js_sys::eval(&script)
        .map(|v| js_sys::Promise::from(v))
        .map_err(|e| format!("fetch init failed: {:?}", e))
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
    FetchResult { status_code, status, proto, headers, body, error: None }
}

fn read_string_slice(slice_ref: GcRef) -> Vec<String> {
    if slice_ref.is_null() {
        return Vec::new();
    }
    let len = slice::len(slice_ref);
    let elem_bytes = core::mem::size_of::<GcRef>();
    let mut result = Vec::with_capacity(len);
    for i in 0..len {
        let raw = slice::get(slice_ref, i, elem_bytes);
        let str_ref: GcRef = slot_to_ptr(raw);
        if !str_ref.is_null() {
            result.push(vo_runtime::objects::string::as_str(str_ref).to_string());
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
            None => return ExternResult::Panic(format!("fetch result missing for token {}", token)),
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

        let token = next_fetch_token();
        match build_fetch_promise(&method, &url, &headers, &body) {
            Ok(promise) => {
                register_fetch_promise(token, promise);
                ExternResult::HostEventWaitAndReplay { token }
            }
            Err(e) => ExternResult::Panic(e),
        }
    }
}

pub fn register_externs(reg: &mut ExternRegistry, defs: &[ExternDef]) {
    for (id, def) in defs.iter().enumerate() {
        if def.name.as_str() == "net_http_nativeHttpsRequest" {
            reg.register(id as u32, wasm_http_native_request);
        }
    }
}
