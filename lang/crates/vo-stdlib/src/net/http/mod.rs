//! HTTP package native function implementations.
//!
//! HTTP client is primarily implemented in Vo.
//! This module provides:
//! - HTTP sentinel errors
//! - Native HTTPS request bridge (`nativeHttpsRequest`)

#[cfg(feature = "std")]
use std::io::Read;

#[cfg(feature = "std")]
use vo_ffi_macro::{vostd_errors, vostd_fn};
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
#[cfg(feature = "std")]
use vo_runtime::ffi::{ExternCallContext, ExternResult};
#[cfg(feature = "std")]
use vo_runtime::gc::GcRef;
#[cfg(feature = "std")]
use vo_runtime::objects::slice;
#[cfg(feature = "std")]
use vo_runtime::slot::slot_to_ptr;
use vo_runtime::ffi::ExternRegistry;

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

#[cfg(feature = "std")]
#[vostd_fn("http", "nativeHttpsRequest", std)]
fn http_native_https_request(call: &mut ExternCallContext) -> ExternResult {
    let method = call.arg_str(0);
    let url = call.arg_str(1);
    let headers = read_string_slice(call.arg_ref(2));
    let body = call.arg_bytes(3);
    let timeout_ns = call.arg_i64(4);

    let mut builder = ureq::AgentBuilder::new();
    if timeout_ns > 0 {
        builder = builder.timeout(std::time::Duration::from_nanos(timeout_ns as u64));
    }
    let agent = builder.build();

    let mut request = agent.request(method, url);
    for header in &headers {
        if let Some((key, value)) = split_header_line(header) {
            request = request.set(key, value);
        }
    }

    let response_result = if body.is_empty() {
        request.call()
    } else {
        request.send_bytes(body)
    };

    let response = match response_result {
        Ok(resp) => resp,
        Err(ureq::Error::Status(_, resp)) => resp,
        Err(ureq::Error::Transport(err)) => {
            return write_https_transport_error(call, &err.to_string());
        }
    };

    let status_code = response.status() as i64;
    let proto = "HTTP/1.1".to_string();
    let status = format!("{} {} {}", proto, status_code, response.status_text());

    let mut response_headers = Vec::new();
    for name in response.headers_names() {
        for value in response.all(&name) {
            response_headers.push(format!("{}: {}", name, value));
        }
    }

    let mut response_body = Vec::new();
    if let Err(e) = response.into_reader().read_to_end(&mut response_body) {
        return write_https_transport_error(call, &e.to_string());
    }

    call.ret_i64(0, status_code);
    call.ret_str(1, &status);
    call.ret_str(2, &proto);
    call.ret_string_slice(3, &response_headers);
    call.ret_bytes(4, &response_body);
    write_nil_error(call, 5);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn write_https_transport_error(call: &mut ExternCallContext, msg: &str) -> ExternResult {
    call.ret_i64(0, 0);
    call.ret_str(1, "");
    call.ret_str(2, "");
    call.ret_nil(3);
    call.ret_nil(4);

    let lower = msg.to_ascii_lowercase();
    if lower.contains("timeout") || lower.contains("timed out") {
        let (slot0, slot1) = http_sentinel_error(call, HttpErrorKind::Timeout);
        call.ret_u64(5, slot0);
        call.ret_u64(6, slot1);
    } else {
        write_error_to(call, 5, msg);
    }
    ExternResult::Ok
}

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
