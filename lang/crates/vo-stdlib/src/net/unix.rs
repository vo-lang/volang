//! Unix domain socket implementations.

use std::os::unix::net::{UnixListener, UnixStream};

use super::{
    checked_handle_arg, register_handle, write_io_error, UNIX_CONN_HANDLES, UNIX_LISTENER_HANDLES,
};
use vo_ffi_macro::vostd_fn;
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::io::{Completion, CompletionData, IoRuntime};

fn register_unix_conn(io: &mut IoRuntime, conn: UnixStream) -> std::io::Result<i32> {
    conn.set_nonblocking(true)?;
    register_handle(io, &UNIX_CONN_HANDLES, conn)
}

fn set_deadline(
    conn: &UnixStream,
    deadline_ns: i64,
    read: bool,
    write: bool,
) -> std::io::Result<()> {
    let timeout = super::deadline_to_timeout(deadline_ns);
    if read {
        conn.set_read_timeout(timeout)?;
    }
    if write {
        conn.set_write_timeout(timeout)?;
    }
    Ok(())
}

fn register_unix_listener(io: &mut IoRuntime, listener: UnixListener) -> std::io::Result<i32> {
    listener.set_nonblocking(true)?;
    register_handle(io, &UNIX_LISTENER_HANDLES, listener)
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
    let address = match crate::host_bytes::path_buf_from_bytes(
        call.arg_string_bytes(slots::ARG_ADDRESS),
        "Unix socket address",
    ) {
        Ok(address) => address,
        Err(error) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, error);
            return ExternResult::Ok;
        }
    };

    match UnixStream::connect(address) {
        Ok(stream) => match register_unix_conn(call.io_mut(), stream) {
            Ok(handle) => {
                call.ret_i64(slots::RET_0, i64::from(handle));
                write_nil_error(call, slots::RET_1);
            }
            Err(error) => {
                call.ret_i64(slots::RET_0, 0);
                write_io_error(call, slots::RET_1, error);
            }
        },
        Err(e) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("net", "unixListen", std)]
pub fn net_unix_listen(call: &mut ExternCallContext) -> ExternResult {
    let address = match crate::host_bytes::path_buf_from_bytes(
        call.arg_string_bytes(slots::ARG_ADDRESS),
        "Unix socket address",
    ) {
        Ok(address) => address,
        Err(error) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, error);
            return ExternResult::Ok;
        }
    };

    match UnixListener::bind(address) {
        Ok(listener) => match register_unix_listener(call.io_mut(), listener) {
            Ok(handle) => {
                call.ret_i64(slots::RET_0, i64::from(handle));
                write_nil_error(call, slots::RET_1);
            }
            Err(error) => {
                call.ret_i64(slots::RET_0, 0);
                write_io_error(call, slots::RET_1, error);
            }
        },
        Err(e) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("net", "blocking_unixConnRead", std)]
pub fn net_unix_conn_read(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_1) else {
        call.ret_i64(slots::RET_0, 0);
        return ExternResult::Ok;
    };
    let resume_token = call.take_resume_io_token();
    let buf_ref = call.arg_ref(slots::ARG_B);
    // Safety: `buf_ref` is a rooted []byte extern argument.

    let token = match resume_token {
        Some(token) => token,
        None => {
            let lease = {
                let handles = super::lock_recover(&UNIX_CONN_HANDLES);
                match handles.get(&handle) {
                    Some(conn) => match conn.lease(handle) {
                        Ok(lease) => lease,
                        Err(error) => {
                            call.ret_i64(slots::RET_0, 0);
                            write_io_error(call, slots::RET_1, error);
                            return ExternResult::Ok;
                        }
                    },
                    None => {
                        call.ret_i64(slots::RET_0, 0);
                        write_error_to(call, slots::RET_1, "use of closed network connection");
                        return ExternResult::Ok;
                    }
                }
            };
            let token = call.io_mut().submit_lease_slice_read(lease, buf_ref);
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
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_1) else {
        call.ret_i64(slots::RET_0, 0);
        return ExternResult::Ok;
    };
    let resume_token = call.take_resume_io_token();
    let buf_ref = call.arg_ref(slots::ARG_B);
    // Safety: `buf_ref` is a rooted []byte extern argument.

    let token = match resume_token {
        Some(token) => token,
        None => {
            let lease = {
                let handles = super::lock_recover(&UNIX_CONN_HANDLES);
                match handles.get(&handle) {
                    Some(conn) => match conn.lease(handle) {
                        Ok(lease) => lease,
                        Err(error) => {
                            call.ret_i64(slots::RET_0, 0);
                            write_io_error(call, slots::RET_1, error);
                            return ExternResult::Ok;
                        }
                    },
                    None => {
                        call.ret_i64(slots::RET_0, 0);
                        write_error_to(call, slots::RET_1, "use of closed network connection");
                        return ExternResult::Ok;
                    }
                }
            };
            let token = call.io_mut().submit_lease_slice_write(lease, buf_ref);
            match call.io_mut().try_take_completion(token) {
                Some(c) => return handle_rw_completion(call, c, slots::RET_0, slots::RET_1, false),
                None => return ExternResult::WaitIo { token },
            }
        }
    };

    let c = call.io_mut().take_completion(token);
    handle_rw_completion(call, c, slots::RET_0, slots::RET_1, false)
}

fn do_set_deadline(
    call: &mut ExternCallContext,
    handle: i32,
    deadline_ns: i64,
    ret_slot: u16,
    read: bool,
    write: bool,
) -> ExternResult {
    let handles = super::lock_recover(&UNIX_CONN_HANDLES);
    if let Some(conn) = handles.get(&handle) {
        match set_deadline(conn, deadline_ns, read, write) {
            Ok(()) => write_nil_error(call, ret_slot),
            Err(e) => write_io_error(call, ret_slot, e),
        }
    } else {
        write_error_to(call, ret_slot, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_fn("net", "unixConnSetDeadline", std)]
pub fn net_unix_conn_set_deadline(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);
    do_set_deadline(call, handle, deadline_ns, slots::RET_0, true, true)
}

#[vostd_fn("net", "unixConnSetReadDeadline", std)]
pub fn net_unix_conn_set_read_deadline(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);
    do_set_deadline(call, handle, deadline_ns, slots::RET_0, true, false)
}

#[vostd_fn("net", "unixConnSetWriteDeadline", std)]
pub fn net_unix_conn_set_write_deadline(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);
    do_set_deadline(call, handle, deadline_ns, slots::RET_0, false, true)
}

#[vostd_fn("net", "unixConnClose", std)]
pub fn net_unix_conn_close(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };

    if let Some(conn) = super::lock_recover(&UNIX_CONN_HANDLES).remove(&handle) {
        call.io_mut().disarm_resource_cleanup(conn.cleanup_token);
        let cancel_key = conn.cancel(handle);
        call.io_mut().cancel(cancel_key);
        write_nil_error(call, slots::RET_0);
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_fn("net", "blocking_unixListenerAccept", std)]
pub fn net_unix_listener_accept(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_1) else {
        call.ret_i64(slots::RET_0, 0);
        return ExternResult::Ok;
    };
    let resume_token = call.take_resume_io_token();

    let token = match resume_token {
        Some(token) => token,
        None => {
            let lease = {
                let handles = super::lock_recover(&UNIX_LISTENER_HANDLES);
                match handles.get(&handle) {
                    Some(listener) => match listener.lease(handle) {
                        Ok(lease) => lease,
                        Err(error) => {
                            call.ret_i64(slots::RET_0, 0);
                            write_io_error(call, slots::RET_1, error);
                            return ExternResult::Ok;
                        }
                    },
                    None => {
                        call.ret_i64(slots::RET_0, 0);
                        write_error_to(call, slots::RET_1, "use of closed network connection");
                        return ExternResult::Ok;
                    }
                }
            };
            let token = call.io_mut().submit_lease_accept(lease);
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
        Ok(CompletionData::Accept(accepted)) => {
            let stream = UnixStream::from(accepted);
            match register_unix_conn(call.io_mut(), stream) {
                Ok(handle) => {
                    call.ret_i64(ret_handle, i64::from(handle));
                    write_nil_error(call, ret_err);
                }
                Err(error) => {
                    call.ret_i64(ret_handle, 0);
                    write_io_error(call, ret_err, error);
                }
            }
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
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };

    if let Some(listener) = super::lock_recover(&UNIX_LISTENER_HANDLES).remove(&handle) {
        call.io_mut()
            .disarm_resource_cleanup(listener.cleanup_token);
        let cancel_key = listener.cancel(handle);
        call.io_mut().cancel(cancel_key);
        write_nil_error(call, slots::RET_0);
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}
