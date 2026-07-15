//! TCP connection and listener implementations.

#[cfg(not(unix))]
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream, ToSocketAddrs};
use std::time::Duration;

use vo_ffi_macro::vostd_fn;
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::io::IoRuntime;
#[cfg(unix)]
use vo_runtime::io::{Completion, CompletionData};
#[cfg(not(unix))]
use vo_runtime::objects::slice;

#[cfg(unix)]
use super::{
    checked_handle, checked_handle_arg, register_handle, write_io_error, TCP_CONN_HANDLES,
    TCP_LISTENER_HANDLES,
};

/// Handle read/write completion result.
#[cfg(unix)]
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

fn register_tcp_conn(io: &mut IoRuntime, conn: TcpStream) -> std::io::Result<i32> {
    #[cfg(unix)]
    conn.set_nonblocking(true)?;
    register_handle(io, &TCP_CONN_HANDLES, conn)
}

fn register_tcp_listener(io: &mut IoRuntime, listener: TcpListener) -> std::io::Result<i32> {
    #[cfg(unix)]
    listener.set_nonblocking(true)?;
    register_handle(io, &TCP_LISTENER_HANDLES, listener)
}

#[vostd_fn("net", "dial", std)]
pub fn net_dial(call: &mut ExternCallContext) -> ExternResult {
    let network = match crate::host_bytes::utf8_arg(call, slots::ARG_NETWORK, "network") {
        Ok(network) => network,
        Err(error) => {
            call.ret_i64(slots::RET_0, 0);
            write_error_to(call, slots::RET_1, &error);
            return ExternResult::Ok;
        }
    };
    let address = match crate::host_bytes::utf8_arg(call, slots::ARG_ADDRESS, "network address") {
        Ok(address) => address,
        Err(error) => {
            call.ret_i64(slots::RET_0, 0);
            write_error_to(call, slots::RET_1, &error);
            return ExternResult::Ok;
        }
    };
    let timeout_ns = call.arg_i64(slots::ARG_TIMEOUT);

    let result = if timeout_ns > 0 {
        let timeout = Duration::from_nanos(timeout_ns as u64);
        match address.to_socket_addrs() {
            Ok(mut addrs) => {
                if let Some(addr) = addrs.next() {
                    TcpStream::connect_timeout(&addr, timeout)
                } else {
                    Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        "no addresses",
                    ))
                }
            }
            Err(e) => Err(e),
        }
    } else {
        TcpStream::connect(&address)
    };

    match result {
        Ok(stream) => match register_tcp_conn(call.io_mut(), stream) {
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

    // Note: network type validation is done in Vo layer (Dial/DialTimeout functions)
    let _ = network; // silence unused warning
    ExternResult::Ok
}

#[vostd_fn("net", "listen", std)]
pub fn net_listen(call: &mut ExternCallContext) -> ExternResult {
    let network = match crate::host_bytes::utf8_arg(call, slots::ARG_NETWORK, "network") {
        Ok(network) => network,
        Err(error) => {
            call.ret_i64(slots::RET_0, 0);
            write_error_to(call, slots::RET_1, &error);
            return ExternResult::Ok;
        }
    };
    let address = match crate::host_bytes::utf8_arg(call, slots::ARG_ADDRESS, "network address") {
        Ok(address) => address,
        Err(error) => {
            call.ret_i64(slots::RET_0, 0);
            write_error_to(call, slots::RET_1, &error);
            return ExternResult::Ok;
        }
    };

    match TcpListener::bind(&address) {
        Ok(listener) => match register_tcp_listener(call.io_mut(), listener) {
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

    // Note: network type validation is done in Vo layer (Listen function)
    let _ = network; // silence unused warning
    ExternResult::Ok
}

#[vostd_fn("net", "blocking_tcpConnRead", std)]
pub fn net_tcp_conn_read(call: &mut ExternCallContext) -> ExternResult {
    #[cfg(unix)]
    {
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
                    let handles = super::lock_recover(&TCP_CONN_HANDLES);
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
                    Some(c) => {
                        return handle_rw_completion(call, c, slots::RET_0, slots::RET_1, true)
                    }
                    None => return ExternResult::WaitIo { token },
                }
            }
        };

        let c = call.io_mut().take_completion(token);
        handle_rw_completion(call, c, slots::RET_0, slots::RET_1, true)
    }

    #[cfg(not(unix))]
    {
        let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_1) else {
            call.ret_i64(slots::RET_0, 0);
            return ExternResult::Ok;
        };
        let buf_ref = call.arg_ref(slots::ARG_B);
        // Safety: `buf_ref` is a rooted []byte extern argument.
        let mut buf = vec![0u8; unsafe { slice::len(buf_ref) }];

        let mut handles = super::lock_recover(&TCP_CONN_HANDLES);
        match handles.get_mut(&handle) {
            Some(conn) => match conn.read(&mut buf) {
                Ok(0) => {
                    call.ret_i64(slots::RET_0, 0);
                    write_error_to(call, slots::RET_1, "EOF");
                }
                Ok(n) => {
                    unsafe { slice::write_bytes(buf_ref, &buf[..n]) };
                    call.ret_i64(slots::RET_0, n as i64);
                    write_nil_error(call, slots::RET_1);
                }
                Err(e) => {
                    call.ret_i64(slots::RET_0, 0);
                    write_io_error(call, slots::RET_1, e);
                }
            },
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "use of closed network connection");
            }
        }
        ExternResult::Ok
    }
}

#[vostd_fn("net", "blocking_tcpConnWrite", std)]
pub fn net_tcp_conn_write(call: &mut ExternCallContext) -> ExternResult {
    #[cfg(unix)]
    {
        let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_1) else {
            call.ret_i64(slots::RET_0, 0);
            return ExternResult::Ok;
        };
        let resume_token = call.take_resume_io_token();
        let buf_ref = call.arg_ref(slots::ARG_B);

        let token = match resume_token {
            Some(token) => token,
            None => {
                let lease = {
                    let handles = super::lock_recover(&TCP_CONN_HANDLES);
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
                    Some(c) => {
                        return handle_rw_completion(call, c, slots::RET_0, slots::RET_1, false)
                    }
                    None => return ExternResult::WaitIo { token },
                }
            }
        };

        let c = call.io_mut().take_completion(token);
        handle_rw_completion(call, c, slots::RET_0, slots::RET_1, false)
    }

    #[cfg(not(unix))]
    {
        let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_1) else {
            call.ret_i64(slots::RET_0, 0);
            return ExternResult::Ok;
        };
        let buf_ref = call.arg_ref(slots::ARG_B);
        let buf = unsafe { slice::byte_vec(buf_ref) };

        let mut handles = super::lock_recover(&TCP_CONN_HANDLES);
        match handles.get_mut(&handle) {
            Some(conn) => match conn.write(&buf) {
                Ok(n) => {
                    call.ret_i64(slots::RET_0, n as i64);
                    write_nil_error(call, slots::RET_1);
                }
                Err(e) => {
                    call.ret_i64(slots::RET_0, 0);
                    write_io_error(call, slots::RET_1, e);
                }
            },
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "use of closed network connection");
            }
        }
        ExternResult::Ok
    }
}

#[vostd_fn("net", "tcpConnClose", std)]
pub fn net_tcp_conn_close(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };

    let removed = super::lock_recover(&TCP_CONN_HANDLES).remove(&handle);
    if let Some(conn) = removed {
        call.io_mut().disarm_resource_cleanup(conn.cleanup_token);
        #[cfg(unix)]
        {
            let cancel_key = conn.cancel(handle);
            call.io_mut().cancel(cancel_key);
        }
        #[cfg(not(unix))]
        {
            let _ = conn;
        }
        write_nil_error(call, slots::RET_0);
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_fn("net", "tcpConnLocalAddr", std)]
pub fn net_tcp_conn_local_addr(call: &mut ExternCallContext) -> ExternResult {
    let handle = checked_handle(call.arg_i64(slots::ARG_HANDLE)).ok();

    let handles = super::lock_recover(&TCP_CONN_HANDLES);
    let addr_str = if let Some(conn) = handle.and_then(|handle| handles.get(&handle)) {
        conn.local_addr().map(|a| a.to_string()).unwrap_or_default()
    } else {
        String::new()
    };
    let s = call.alloc_str(&addr_str);
    call.ret_ref(slots::RET_0, s);
    ExternResult::Ok
}

#[vostd_fn("net", "tcpConnRemoteAddr", std)]
pub fn net_tcp_conn_remote_addr(call: &mut ExternCallContext) -> ExternResult {
    let handle = checked_handle(call.arg_i64(slots::ARG_HANDLE)).ok();

    let handles = super::lock_recover(&TCP_CONN_HANDLES);
    let addr_str = if let Some(conn) = handle.and_then(|handle| handles.get(&handle)) {
        conn.peer_addr().map(|a| a.to_string()).unwrap_or_default()
    } else {
        String::new()
    };
    let s = call.alloc_str(&addr_str);
    call.ret_ref(slots::RET_0, s);
    ExternResult::Ok
}

fn set_deadline(
    conn: &TcpStream,
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

#[cfg(unix)]
fn handle_accept_completion(
    call: &mut ExternCallContext,
    completion: Completion,
    ret_handle: u16,
    ret_error: u16,
) -> ExternResult {
    match completion.result {
        Ok(CompletionData::Accept(accepted)) => {
            let stream = TcpStream::from(accepted);
            match register_tcp_conn(call.io_mut(), stream) {
                Ok(handle) => {
                    call.ret_i64(ret_handle, i64::from(handle));
                    write_nil_error(call, ret_error);
                }
                Err(error) => {
                    call.ret_i64(ret_handle, 0);
                    write_io_error(call, ret_error, error);
                }
            }
        }
        Ok(_) => panic!("unexpected completion data for accept"),
        Err(error) => {
            call.ret_i64(ret_handle, 0);
            write_io_error(call, ret_error, error);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("net", "tcpConnSetDeadline", std)]
pub fn net_tcp_conn_set_deadline(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);

    let handles = super::lock_recover(&TCP_CONN_HANDLES);
    if let Some(conn) = handles.get(&handle) {
        match set_deadline(conn, deadline_ns, true, true) {
            Ok(()) => write_nil_error(call, slots::RET_0),
            Err(e) => write_io_error(call, slots::RET_0, e),
        }
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_fn("net", "tcpConnSetReadDeadline", std)]
pub fn net_tcp_conn_set_read_deadline(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);

    let handles = super::lock_recover(&TCP_CONN_HANDLES);
    if let Some(conn) = handles.get(&handle) {
        match set_deadline(conn, deadline_ns, true, false) {
            Ok(()) => write_nil_error(call, slots::RET_0),
            Err(e) => write_io_error(call, slots::RET_0, e),
        }
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_fn("net", "tcpConnSetWriteDeadline", std)]
pub fn net_tcp_conn_set_write_deadline(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);

    let handles = super::lock_recover(&TCP_CONN_HANDLES);
    if let Some(conn) = handles.get(&handle) {
        match set_deadline(conn, deadline_ns, false, true) {
            Ok(()) => write_nil_error(call, slots::RET_0),
            Err(e) => write_io_error(call, slots::RET_0, e),
        }
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_fn("net", "blocking_tcpListenerAccept", std)]
pub fn net_tcp_listener_accept(call: &mut ExternCallContext) -> ExternResult {
    #[cfg(unix)]
    {
        let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_1) else {
            call.ret_i64(slots::RET_0, 0);
            return ExternResult::Ok;
        };
        let resume_token = call.take_resume_io_token();

        let token = match resume_token {
            Some(token) => token,
            None => {
                let lease = {
                    let handles = super::lock_recover(&TCP_LISTENER_HANDLES);
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
                    Some(completion) => {
                        return handle_accept_completion(
                            call,
                            completion,
                            slots::RET_0,
                            slots::RET_1,
                        )
                    }
                    None => return ExternResult::WaitIo { token },
                }
            }
        };

        let completion = call.io_mut().take_completion(token);
        handle_accept_completion(call, completion, slots::RET_0, slots::RET_1)
    }

    #[cfg(not(unix))]
    {
        let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_1) else {
            call.ret_i64(slots::RET_0, 0);
            return ExternResult::Ok;
        };

        let mut handles = super::lock_recover(&TCP_LISTENER_HANDLES);
        match handles.get_mut(&handle) {
            Some(listener) => match listener.accept() {
                Ok((stream, _addr)) => match register_tcp_conn(call.io_mut(), stream) {
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
            },
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "use of closed network connection");
            }
        }
        ExternResult::Ok
    }
}

#[vostd_fn("net", "tcpListenerClose", std)]
pub fn net_tcp_listener_close(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };

    if let Some(listener) = super::lock_recover(&TCP_LISTENER_HANDLES).remove(&handle) {
        call.io_mut()
            .disarm_resource_cleanup(listener.cleanup_token);
        #[cfg(unix)]
        {
            let cancel_key = listener.cancel(handle);
            call.io_mut().cancel(cancel_key);
        }
        write_nil_error(call, slots::RET_0);
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_fn("net", "tcpListenerAddr", std)]
pub fn net_tcp_listener_addr(call: &mut ExternCallContext) -> ExternResult {
    let handle = checked_handle(call.arg_i64(slots::ARG_HANDLE)).ok();

    let handles = super::lock_recover(&TCP_LISTENER_HANDLES);
    let addr_str = if let Some(listener) = handle.and_then(|handle| handles.get(&handle)) {
        listener
            .local_addr()
            .map(|a| a.to_string())
            .unwrap_or_default()
    } else {
        String::new()
    };
    let s = call.alloc_str(&addr_str);
    call.ret_ref(slots::RET_0, s);
    ExternResult::Ok
}
