//! Unix domain socket implementations.

use std::os::unix::net::{UnixStream, UnixListener};
use std::os::fd::{AsRawFd, FromRawFd};

use vo_ffi_macro::vostd_fn;
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::io::{IoHandle, CompletionData, Completion};
use vo_runtime::objects::slice;
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};

use super::{UNIX_CONN_HANDLES, UNIX_LISTENER_HANDLES, next_handle, write_io_error};

fn register_unix_conn(conn: UnixStream) -> i32 {
    // Set non-blocking for async I/O
    conn.set_nonblocking(true).ok();
    let h = next_handle();
    UNIX_CONN_HANDLES.lock().unwrap().insert(h, conn);
    h
}

fn register_unix_listener(listener: UnixListener) -> i32 {
    // Set non-blocking for async accept
    listener.set_nonblocking(true).ok();
    let h = next_handle();
    UNIX_LISTENER_HANDLES.lock().unwrap().insert(h, listener);
    h
}

fn handle_rw_completion(
    call: &mut ExternCallContext,
    c: Completion,
    ret_size: u16,
    ret_err: u16,
    check_eof: bool,
) -> ExternResult {
    match c.result {
        Ok(CompletionData::Size(0)) if check_eof => {
            call.ret_i64(ret_size, 0);
            write_error_to(call, ret_err, "EOF");
        }
        Ok(CompletionData::Size(n)) => {
            call.ret_i64(ret_size, n as i64);
            write_nil_error(call, ret_err);
        }
        Ok(_) => panic!("unexpected completion data"),
        Err(e) => {
            call.ret_i64(ret_size, 0);
            write_io_error(call, ret_err, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("net", "unixDial", std)]
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

#[vostd_fn("net", "unixListen", std)]
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

#[vostd_fn("net", "blocking_unixConnRead", std)]
pub fn net_unix_conn_read(call: &mut ExternCallContext) -> ExternResult {
    let resume_token = call.take_resume_io_token();
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let buf_ref = call.arg_ref(slots::ARG_B);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);

    let fd = {
        let handles = UNIX_CONN_HANDLES.lock().unwrap();
        match handles.get(&handle) {
            Some(c) => c.as_raw_fd(),
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "use of closed network connection");
                return ExternResult::Ok;
            }
        }
    };

    let token = match resume_token {
        Some(token) => token,
        None => {
            let token = call.io_mut().submit_read(fd as IoHandle, buf_ptr, buf_len);
            match call.io_mut().try_take_completion(token) {
                Some(c) => return handle_rw_completion(call, c, slots::RET_0, slots::RET_1, true),
                None => return ExternResult::WaitIo { token },
            }
        }
    };

    let c = call.io_mut().take_completion(token);
    handle_rw_completion(call, c, slots::RET_0, slots::RET_1, true)
}

#[vostd_fn("net", "blocking_unixConnWrite", std)]
pub fn net_unix_conn_write(call: &mut ExternCallContext) -> ExternResult {
    let resume_token = call.take_resume_io_token();
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let buf_ref = call.arg_ref(slots::ARG_B);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);

    let fd = {
        let handles = UNIX_CONN_HANDLES.lock().unwrap();
        match handles.get(&handle) {
            Some(c) => c.as_raw_fd(),
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "use of closed network connection");
                return ExternResult::Ok;
            }
        }
    };

    let token = match resume_token {
        Some(token) => token,
        None => {
            let token = call.io_mut().submit_write(fd as IoHandle, buf_ptr, buf_len);
            match call.io_mut().try_take_completion(token) {
                Some(c) => return handle_rw_completion(call, c, slots::RET_0, slots::RET_1, false),
                None => return ExternResult::WaitIo { token },
            }
        }
    };

    let c = call.io_mut().take_completion(token);
    handle_rw_completion(call, c, slots::RET_0, slots::RET_1, false)
}

#[vostd_fn("net", "unixConnClose", std)]
pub fn net_unix_conn_close(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    
    if UNIX_CONN_HANDLES.lock().unwrap().remove(&handle).is_some() {
        write_nil_error(call, slots::RET_0);
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_fn("net", "blocking_unixListenerAccept", std)]
pub fn net_unix_listener_accept(call: &mut ExternCallContext) -> ExternResult {
    let resume_token = call.take_resume_io_token();
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    
    let fd = {
        let handles = UNIX_LISTENER_HANDLES.lock().unwrap();
        match handles.get(&handle) {
            Some(l) => l.as_raw_fd(),
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "use of closed network connection");
                return ExternResult::Ok;
            }
        }
    };

    let token = match resume_token {
        Some(token) => token,
        None => {
            let token = call.io_mut().submit_accept(fd as IoHandle);
            match call.io_mut().try_take_completion(token) {
                Some(c) => return handle_accept_completion(call, c, slots::RET_0, slots::RET_1),
                None => return ExternResult::WaitIo { token },
            }
        }
    };

    let c = call.io_mut().take_completion(token);
    handle_accept_completion(call, c, slots::RET_0, slots::RET_1)
}

fn handle_accept_completion(
    call: &mut ExternCallContext,
    c: Completion,
    ret_handle: u16,
    ret_err: u16,
) -> ExternResult {
    match c.result {
        Ok(CompletionData::Accept(new_fd)) => {
            let stream = unsafe { UnixStream::from_raw_fd(new_fd as i32) };
            let h = register_unix_conn(stream);
            call.ret_i64(ret_handle, h as i64);
            write_nil_error(call, ret_err);
        }
        Ok(_) => panic!("unexpected completion data for Accept"),
        Err(e) => {
            call.ret_i64(ret_handle, 0);
            write_io_error(call, ret_err, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("net", "unixListenerClose", std)]
pub fn net_unix_listener_close(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    
    if UNIX_LISTENER_HANDLES.lock().unwrap().remove(&handle).is_some() {
        write_nil_error(call, slots::RET_0);
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}
