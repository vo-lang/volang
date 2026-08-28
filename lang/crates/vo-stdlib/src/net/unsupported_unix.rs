//! Portable failure providers for Unix-domain socket APIs on targets that do
//! not expose the native Unix socket implementation.

use vo_ffi_macro::vostd_fn;
use vo_runtime::builtins::error_helper::write_error_to;
use vo_runtime::ffi::{ExternCallContext, ExternResult};

const UNSUPPORTED: &str = "Unix domain sockets are unavailable on this platform";

fn return_handle_error(call: &mut ExternCallContext, handle_slot: u16, error_slot: u16) {
    call.ret_i64(handle_slot, 0);
    write_error_to(call, error_slot, UNSUPPORTED);
}

fn return_error(call: &mut ExternCallContext, error_slot: u16) {
    write_error_to(call, error_slot, UNSUPPORTED);
}

#[vostd_fn("net", "unixDial", std)]
pub fn net_unix_dial(call: &mut ExternCallContext) -> ExternResult {
    return_handle_error(call, slots::RET_0, slots::RET_1);
    ExternResult::Ok
}

#[vostd_fn("net", "unixListen", std)]
pub fn net_unix_listen(call: &mut ExternCallContext) -> ExternResult {
    return_handle_error(call, slots::RET_0, slots::RET_1);
    ExternResult::Ok
}

#[vostd_fn("net", "blocking_unixConnRead", std)]
pub fn net_unix_conn_read(call: &mut ExternCallContext) -> ExternResult {
    return_handle_error(call, slots::RET_0, slots::RET_1);
    ExternResult::Ok
}

#[vostd_fn("net", "blocking_unixConnWrite", std)]
pub fn net_unix_conn_write(call: &mut ExternCallContext) -> ExternResult {
    return_handle_error(call, slots::RET_0, slots::RET_1);
    ExternResult::Ok
}

#[vostd_fn("net", "unixConnSetDeadline", std)]
pub fn net_unix_conn_set_deadline(call: &mut ExternCallContext) -> ExternResult {
    return_error(call, slots::RET_0);
    ExternResult::Ok
}

#[vostd_fn("net", "unixConnSetReadDeadline", std)]
pub fn net_unix_conn_set_read_deadline(call: &mut ExternCallContext) -> ExternResult {
    return_error(call, slots::RET_0);
    ExternResult::Ok
}

#[vostd_fn("net", "unixConnSetWriteDeadline", std)]
pub fn net_unix_conn_set_write_deadline(call: &mut ExternCallContext) -> ExternResult {
    return_error(call, slots::RET_0);
    ExternResult::Ok
}

#[vostd_fn("net", "unixConnClose", std)]
pub fn net_unix_conn_close(call: &mut ExternCallContext) -> ExternResult {
    return_error(call, slots::RET_0);
    ExternResult::Ok
}

#[vostd_fn("net", "blocking_unixListenerAccept", std)]
pub fn net_unix_listener_accept(call: &mut ExternCallContext) -> ExternResult {
    return_handle_error(call, slots::RET_0, slots::RET_1);
    ExternResult::Ok
}

#[vostd_fn("net", "unixListenerClose", std)]
pub fn net_unix_listener_close(call: &mut ExternCallContext) -> ExternResult {
    return_error(call, slots::RET_0);
    ExternResult::Ok
}
