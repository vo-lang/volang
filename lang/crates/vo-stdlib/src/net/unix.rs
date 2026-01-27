//! Unix domain socket implementations.

use std::io::{Read, Write};
use std::os::unix::net::{UnixStream, UnixListener};

use vo_ffi_macro::vostd_extern_ctx;
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::objects::slice;
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};

use super::{UNIX_CONN_HANDLES, UNIX_LISTENER_HANDLES, next_handle, write_io_error};

fn register_unix_conn(conn: UnixStream) -> i32 {
    let h = next_handle();
    UNIX_CONN_HANDLES.lock().unwrap().insert(h, conn);
    h
}

fn register_unix_listener(listener: UnixListener) -> i32 {
    let h = next_handle();
    UNIX_LISTENER_HANDLES.lock().unwrap().insert(h, listener);
    h
}

#[vostd_extern_ctx("net", "unixDial")]
pub fn net_unix_dial(call: &mut ExternCallContext) -> ExternResult {
    let address = call.arg_str(slots::ARG_ADDRESS).to_string();
    
    match UnixStream::connect(&address) {
        Ok(stream) => {
            let h = register_unix_conn(stream);
            call.ret_i64(slots::RET_0, h as i64);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "unixListen")]
pub fn net_unix_listen(call: &mut ExternCallContext) -> ExternResult {
    let address = call.arg_str(slots::ARG_ADDRESS).to_string();
    
    match UnixListener::bind(&address) {
        Ok(listener) => {
            let h = register_unix_listener(listener);
            call.ret_i64(slots::RET_0, h as i64);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "unixConnRead")]
pub fn net_unix_conn_read(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let buf_ref = call.arg_ref(slots::ARG_B);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);
    let buf = unsafe { std::slice::from_raw_parts_mut(buf_ptr, buf_len) };
    
    let mut handles = UNIX_CONN_HANDLES.lock().unwrap();
    if let Some(conn) = handles.get_mut(&handle) {
        match conn.read(buf) {
            Ok(0) => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "EOF");
            }
            Ok(n) => {
                call.ret_i64(slots::RET_0, n as i64);
                write_nil_error(call, slots::RET_1);
            }
            Err(e) => {
                call.ret_i64(slots::RET_0, 0);
                write_io_error(call, slots::RET_1, e);
            }
        }
    } else {
        call.ret_i64(slots::RET_0, 0);
        write_error_to(call, slots::RET_1, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "unixConnWrite")]
pub fn net_unix_conn_write(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let buf_ref = call.arg_ref(slots::ARG_B);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);
    let buf = unsafe { std::slice::from_raw_parts(buf_ptr, buf_len) };
    
    let mut handles = UNIX_CONN_HANDLES.lock().unwrap();
    if let Some(conn) = handles.get_mut(&handle) {
        match conn.write(buf) {
            Ok(n) => {
                call.ret_i64(slots::RET_0, n as i64);
                write_nil_error(call, slots::RET_1);
            }
            Err(e) => {
                call.ret_i64(slots::RET_0, 0);
                write_io_error(call, slots::RET_1, e);
            }
        }
    } else {
        call.ret_i64(slots::RET_0, 0);
        write_error_to(call, slots::RET_1, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "unixConnClose")]
pub fn net_unix_conn_close(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    
    if UNIX_CONN_HANDLES.lock().unwrap().remove(&handle).is_some() {
        write_nil_error(call, slots::RET_0);
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "unixListenerAccept")]
pub fn net_unix_listener_accept(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    
    let handles = UNIX_LISTENER_HANDLES.lock().unwrap();
    if let Some(listener) = handles.get(&handle) {
        match listener.accept() {
            Ok((stream, _addr)) => {
                drop(handles);
                let h = register_unix_conn(stream);
                call.ret_i64(slots::RET_0, h as i64);
                write_nil_error(call, slots::RET_1);
            }
            Err(e) => {
                call.ret_i64(slots::RET_0, 0);
                write_io_error(call, slots::RET_1, e);
            }
        }
    } else {
        call.ret_i64(slots::RET_0, 0);
        write_error_to(call, slots::RET_1, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "unixListenerClose")]
pub fn net_unix_listener_close(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    
    if UNIX_LISTENER_HANDLES.lock().unwrap().remove(&handle).is_some() {
        write_nil_error(call, slots::RET_0);
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}
