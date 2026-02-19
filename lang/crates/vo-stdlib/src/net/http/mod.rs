//! HTTP package native function implementations.
//!
//! Provides the `nativeHttpsRequest` extern backed by ureq running on a
//! background OS thread.  A pipe signals completion to the I/O reactor so
//! the VM scheduler is never blocked — other goroutines (including
//! in-process HTTP servers) keep running.

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(feature = "std")]
use std::io::Read;
#[cfg(feature = "std")]
use std::sync::Mutex;

#[cfg(feature = "std")]
use vo_ffi_macro::{vostd_errors, vostd_fn};
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
#[cfg(feature = "std")]
use vo_runtime::ffi::{ExternCallContext, ExternResult};
#[cfg(feature = "std")]
use vo_runtime::gc::GcRef;
#[cfg(feature = "std")]
use vo_runtime::io::IoToken;
#[cfg(feature = "std")]
use vo_runtime::objects::slice;
#[cfg(feature = "std")]
use vo_runtime::slot::slot_to_ptr;
use vo_runtime::ffi::ExternRegistry;

// ── Sentinel errors ──────────────────────────────────────────────────

#[cfg(feature = "std")]
vostd_errors! {
    "http" => {
        Timeout => "request timeout",
        BadRequest => "bad request",
        Unauthorized => "unauthorized",
        Forbidden => "forbidden",
        NotFound => "not found",
        ServerError => "internal server error",
        BadGateway => "bad gateway",
        ServiceUnavailable => "service unavailable",
        Unknown => "unknown http error",
    }
}

// ── Background HTTP result storage ───────────────────────────────────

#[cfg(feature = "std")]
struct HttpResponse {
    status_code: i64,
    status: String,
    proto: String,
    headers: Vec<String>,
    body: Vec<u8>,
    error: Option<String>,
    is_timeout: bool,
}

#[cfg(feature = "std")]
struct PipeCleanup {
    read_fd: i32,
    buf_ptr: usize, // leaked Box<u8> stored as usize (Send-safe)
}

#[cfg(feature = "std")]
lazy_static::lazy_static! {
    static ref PENDING_HTTP: Mutex<HashMap<IoToken, HttpResponse>> = Mutex::new(HashMap::new());
    static ref PIPE_CLEANUP: Mutex<HashMap<IoToken, PipeCleanup>> = Mutex::new(HashMap::new());
}

// ── ureq execution (runs on background thread) ──────────────────────

#[cfg(feature = "std")]
fn run_ureq(
    method: String,
    url: String,
    headers: Vec<String>,
    body: Vec<u8>,
    timeout_ns: i64,
) -> HttpResponse {
    let mut builder = ureq::AgentBuilder::new();
    if timeout_ns > 0 {
        builder = builder.timeout(std::time::Duration::from_nanos(timeout_ns as u64));
    }
    let agent = builder.build();

    let mut request = agent.request(&method, &url);
    for header in &headers {
        if let Some((key, value)) = split_header_line(header) {
            request = request.set(key, value);
        }
    }

    let response_result = if body.is_empty() {
        request.call()
    } else {
        request.send_bytes(&body)
    };

    let response = match response_result {
        Ok(resp) => resp,
        Err(ureq::Error::Status(_, resp)) => resp,
        Err(ureq::Error::Transport(err)) => {
            let msg = err.to_string();
            let lower = msg.to_ascii_lowercase();
            return HttpResponse {
                status_code: 0,
                status: String::new(),
                proto: String::new(),
                headers: Vec::new(),
                body: Vec::new(),
                is_timeout: lower.contains("timeout") || lower.contains("timed out"),
                error: Some(msg),
            };
        }
    };

    let status_code = response.status() as i64;
    let proto = "HTTP/1.1".to_string();
    let status = format!("{} {} {}", proto, status_code, response.status_text());

    let mut resp_headers = Vec::new();
    for name in response.headers_names() {
        for value in response.all(&name) {
            resp_headers.push(format!("{}: {}", name, value));
        }
    }

    let mut resp_body = Vec::new();
    if let Err(e) = response.into_reader().read_to_end(&mut resp_body) {
        return HttpResponse {
            status_code: 0,
            status: String::new(),
            proto: String::new(),
            headers: Vec::new(),
            body: Vec::new(),
            error: Some(e.to_string()),
            is_timeout: false,
        };
    }

    HttpResponse {
        status_code,
        status,
        proto,
        headers: resp_headers,
        body: resp_body,
        error: None,
        is_timeout: false,
    }
}

// ── Extern implementation ────────────────────────────────────────────

#[cfg(feature = "std")]
#[vostd_fn("http", "nativeHttpsRequest", std)]
fn http_native_https_request(call: &mut ExternCallContext) -> ExternResult {
    // ── Resume path: background thread completed ──
    if let Some(token) = call.take_resume_io_token() {
        let _completion = call.io_mut().take_completion(token);

        // Clean up pipe fd and leaked buffer
        if let Some(state) = PIPE_CLEANUP.lock().unwrap().remove(&token) {
            unsafe {
                libc::close(state.read_fd);
                drop(Box::from_raw(state.buf_ptr as *mut u8));
            }
        }

        let resp = PENDING_HTTP.lock().unwrap().remove(&token)
            .expect("HTTP response missing after WaitIo resume");

        return write_http_response(call, resp);
    }

    // ── First call: read args, create pipe, spawn thread ──
    let method = call.arg_str(0).to_string();
    let url = call.arg_str(1).to_string();
    let headers = read_string_slice(call.arg_ref(2));
    let body = call.arg_bytes(3).to_vec();
    let timeout_ns = call.arg_i64(4);

    // Create pipe for signaling completion to the I/O reactor
    let mut fds = [0i32; 2];
    if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
        return write_transport_error(call, "failed to create pipe for async HTTP");
    }
    let (read_fd, write_fd) = (fds[0], fds[1]);
    unsafe { libc::fcntl(read_fd, libc::F_SETFL, libc::O_NONBLOCK) };

    // Heap-allocate a 1-byte buffer (must outlive WaitIo suspension)
    let buf = Box::into_raw(Box::new(0u8));

    // Register pipe read-end with I/O reactor
    let token = call.io_mut().submit_read(read_fd as u64, buf, 1);

    // Fast path: already readable (shouldn't happen, but follow the pattern)
    if let Some(_c) = call.io_mut().try_take_completion(token) {
        unsafe {
            libc::close(read_fd);
            libc::close(write_fd);
            drop(Box::from_raw(buf));
        }
        let resp = run_ureq(method, url, headers, body, timeout_ns);
        return write_http_response(call, resp);
    }

    // Store cleanup state for resume
    PIPE_CLEANUP.lock().unwrap().insert(token, PipeCleanup {
        read_fd,
        buf_ptr: buf as usize,
    });

    // Spawn background thread — scheduler stays free for other fibers
    std::thread::spawn(move || {
        let resp = run_ureq(method, url, headers, body, timeout_ns);
        PENDING_HTTP.lock().unwrap().insert(token, resp);
        // Signal the pipe to wake the I/O reactor
        unsafe {
            libc::write(write_fd, [1u8].as_ptr() as *const libc::c_void, 1);
            libc::close(write_fd);
        }
    });

    ExternResult::WaitIo { token }
}

// ── Response writing helpers ─────────────────────────────────────────

#[cfg(feature = "std")]
fn write_http_response(call: &mut ExternCallContext, resp: HttpResponse) -> ExternResult {
    if let Some(err_msg) = resp.error {
        return if resp.is_timeout {
            write_timeout_error(call)
        } else {
            write_transport_error(call, &err_msg)
        };
    }
    call.ret_i64(0, resp.status_code);
    call.ret_str(1, &resp.status);
    call.ret_str(2, &resp.proto);
    call.ret_string_slice(3, &resp.headers);
    call.ret_bytes(4, &resp.body);
    write_nil_error(call, 5);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn write_transport_error(call: &mut ExternCallContext, msg: &str) -> ExternResult {
    call.ret_i64(0, 0);
    call.ret_str(1, "");
    call.ret_str(2, "");
    call.ret_nil(3);
    call.ret_nil(4);
    write_error_to(call, 5, msg);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn write_timeout_error(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, 0);
    call.ret_str(1, "");
    call.ret_str(2, "");
    call.ret_nil(3);
    call.ret_nil(4);
    let (slot0, slot1) = http_sentinel_error(call, HttpErrorKind::Timeout);
    call.ret_u64(5, slot0);
    call.ret_u64(6, slot1);
    ExternResult::Ok
}

// ── Helpers ──────────────────────────────────────────────────────────

#[cfg(feature = "std")]
fn read_string_slice(slice_ref: GcRef) -> Vec<String> {
    if slice_ref.is_null() {
        return Vec::new();
    }
    let len = slice::len(slice_ref);
    let mut result = Vec::with_capacity(len);
    for i in 0..len {
        let str_ref_raw = slice::get(slice_ref, i, std::mem::size_of::<GcRef>());
        let str_ref: GcRef = slot_to_ptr(str_ref_raw);
        if !str_ref.is_null() {
            result.push(vo_runtime::objects::string::as_str(str_ref).to_string());
        } else {
            result.push(String::new());
        }
    }
    result
}

#[cfg(feature = "std")]
fn split_header_line(line: &str) -> Option<(&str, &str)> {
    let idx = line.find(':')?;
    if idx == 0 {
        return None;
    }
    let key = line[..idx].trim();
    let value = line[idx + 1..].trim();
    if key.is_empty() {
        return None;
    }
    Some((key, value))
}

// ── Registration ─────────────────────────────────────────────────────

#[cfg(feature = "std")]
pub fn register_externs(registry: &mut ExternRegistry, externs: &[vo_runtime::bytecode::ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        if def.name.as_str() == "net_http_nativeHttpsRequest" {
            registry.register(id as u32, http_native_https_request);
        }
    }
}

#[cfg(not(feature = "std"))]
pub fn register_externs(
    _registry: &mut ExternRegistry,
    _externs: &[vo_runtime::bytecode::ExternDef],
) {
    // No-op
}
