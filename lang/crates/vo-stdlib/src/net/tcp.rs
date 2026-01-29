//! TCP connection and listener implementations.

use std::io::{Read, Write, ErrorKind};
use std::net::{TcpStream, TcpListener, ToSocketAddrs};
use std::time::Duration;
use std::os::fd::AsRawFd;
use std::os::fd::FromRawFd;

use vo_ffi_macro::vostd_extern_ctx;
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::io::IoHandle;
use vo_runtime::objects::slice;
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};

use super::{TCP_CONN_HANDLES, TCP_LISTENER_HANDLES, next_handle, write_io_error};

fn register_tcp_conn(conn: TcpStream) -> i32 {
    // Set non-blocking mode for async I/O
    conn.set_nonblocking(true).ok();
    let h = next_handle();
    TCP_CONN_HANDLES.lock().unwrap().insert(h, conn);
    h
}

fn register_tcp_listener(listener: TcpListener) -> i32 {
    // Set non-blocking mode for async accept
    listener.set_nonblocking(true).ok();
    let h = next_handle();
    TCP_LISTENER_HANDLES.lock().unwrap().insert(h, listener);
    h
}

#[vostd_extern_ctx("net", "dial")]
pub fn net_dial(call: &mut ExternCallContext) -> ExternResult {
    let network = call.arg_str(slots::ARG_NETWORK).to_string();
    let address = call.arg_str(slots::ARG_ADDRESS).to_string();
    let timeout_ns = call.arg_i64(slots::ARG_TIMEOUT);
    
    let result = if timeout_ns > 0 {
        let timeout = Duration::from_nanos(timeout_ns as u64);
        match address.to_socket_addrs() {
            Ok(mut addrs) => {
                if let Some(addr) = addrs.next() {
                    TcpStream::connect_timeout(&addr, timeout)
                } else {
                    Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "no addresses"))
                }
            }
            Err(e) => Err(e),
        }
    } else {
        TcpStream::connect(&address)
    };
    
    match result {
        Ok(stream) => {
            let h = register_tcp_conn(stream);
            call.ret_i64(slots::RET_0, h as i64);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, e);
        }
    }
    
    // Note: network type validation is done in Vo layer (Dial/DialTimeout functions)
    let _ = network; // silence unused warning
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "listen")]
pub fn net_listen(call: &mut ExternCallContext) -> ExternResult {
    let network = call.arg_str(slots::ARG_NETWORK).to_string();
    let address = call.arg_str(slots::ARG_ADDRESS).to_string();
    
    match TcpListener::bind(&address) {
        Ok(listener) => {
            let h = register_tcp_listener(listener);
            call.ret_i64(slots::RET_0, h as i64);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, e);
        }
    }
    
    // Note: network type validation is done in Vo layer (Listen function)
    let _ = network; // silence unused warning
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "tcpConnRead")]
pub fn net_tcp_conn_read(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let buf_ref = call.arg_ref(slots::ARG_B);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);

    let fd = {
        let mut handles = TCP_CONN_HANDLES.lock().unwrap();
        let conn = match handles.get_mut(&handle) {
            Some(c) => c,
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "use of closed network connection");
                return ExternResult::Ok;
            }
        };
        conn.as_raw_fd()
    };

    let token = match call.resume_io_token() {
        Some(token) => token,
        None => {
            let token = call
                .io_mut()
                .submit_read(fd as IoHandle, buf_ptr, buf_len);
            match call.io_mut().try_take_completion(token) {
                Some(c) => {
                    match c.result {
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
                    return ExternResult::Ok;
                }
                None => return ExternResult::WaitIo { token },
            }
        }
    };

    let c = call.io_mut().take_completion(token);
    match c.result {
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
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "tcpConnWrite")]
pub fn net_tcp_conn_write(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let buf_ref = call.arg_ref(slots::ARG_B);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);

    let fd = {
        let mut handles = TCP_CONN_HANDLES.lock().unwrap();
        let conn = match handles.get_mut(&handle) {
            Some(c) => c,
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "use of closed network connection");
                return ExternResult::Ok;
            }
        };
        conn.as_raw_fd()
    };

    let token = match call.resume_io_token() {
        Some(token) => token,
        None => {
            let token = call
                .io_mut()
                .submit_write(fd as IoHandle, buf_ptr, buf_len);
            match call.io_mut().try_take_completion(token) {
                Some(c) => {
                    match c.result {
                        Ok(n) => {
                            call.ret_i64(slots::RET_0, n as i64);
                            write_nil_error(call, slots::RET_1);
                        }
                        Err(e) => {
                            call.ret_i64(slots::RET_0, 0);
                            write_io_error(call, slots::RET_1, e);
                        }
                    }
                    return ExternResult::Ok;
                }
                None => return ExternResult::WaitIo { token },
            }
        }
    };

    let c = call.io_mut().take_completion(token);
    match c.result {
        Ok(n) => {
            call.ret_i64(slots::RET_0, n as i64);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "tcpConnClose")]
pub fn net_tcp_conn_close(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    
    if TCP_CONN_HANDLES.lock().unwrap().remove(&handle).is_some() {
        write_nil_error(call, slots::RET_0);
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "tcpConnLocalAddr")]
pub fn net_tcp_conn_local_addr(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    
    let handles = TCP_CONN_HANDLES.lock().unwrap();
    let addr_str = if let Some(conn) = handles.get(&handle) {
        conn.local_addr().map(|a| a.to_string()).unwrap_or_default()
    } else {
        String::new()
    };
    let s = call.alloc_str(&addr_str);
    call.ret_ref(slots::RET_0, s);
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "tcpConnRemoteAddr")]
pub fn net_tcp_conn_remote_addr(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    
    let handles = TCP_CONN_HANDLES.lock().unwrap();
    let addr_str = if let Some(conn) = handles.get(&handle) {
        conn.peer_addr().map(|a| a.to_string()).unwrap_or_default()
    } else {
        String::new()
    };
    let s = call.alloc_str(&addr_str);
    call.ret_ref(slots::RET_0, s);
    ExternResult::Ok
}

fn set_deadline(conn: &TcpStream, deadline_ns: i64, read: bool, write: bool) -> std::io::Result<()> {
    let timeout = super::deadline_to_timeout(deadline_ns);
    if read {
        conn.set_read_timeout(timeout)?;
    }
    if write {
        conn.set_write_timeout(timeout)?;
    }
    Ok(())
}

#[vostd_extern_ctx("net", "tcpConnSetDeadline")]
pub fn net_tcp_conn_set_deadline(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);
    
    let handles = TCP_CONN_HANDLES.lock().unwrap();
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

#[vostd_extern_ctx("net", "tcpConnSetReadDeadline")]
pub fn net_tcp_conn_set_read_deadline(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);
    
    let handles = TCP_CONN_HANDLES.lock().unwrap();
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

#[vostd_extern_ctx("net", "tcpConnSetWriteDeadline")]
pub fn net_tcp_conn_set_write_deadline(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);
    
    let handles = TCP_CONN_HANDLES.lock().unwrap();
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

#[vostd_extern_ctx("net", "tcpListenerAccept")]
pub fn net_tcp_listener_accept(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;

    let fd = {
        let handles = TCP_LISTENER_HANDLES.lock().unwrap();
        let listener = match handles.get(&handle) {
            Some(l) => l,
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "use of closed network connection");
                return ExternResult::Ok;
            }
        };
        listener.as_raw_fd()
    };

    let token = match call.resume_io_token() {
        Some(token) => token,
        None => {
            let token = call.io_mut().submit_accept(fd as IoHandle);
            match call.io_mut().try_take_completion(token) {
                Some(c) => {
                    match c.result {
                        Ok(_) => {
                            let new_fd = i32::try_from(c.extra)
                                .unwrap_or_else(|_| panic!("invalid accepted fd: {}", c.extra));
                            let stream = unsafe { TcpStream::from_raw_fd(new_fd) };
                            let h = register_tcp_conn(stream);
                            call.ret_i64(slots::RET_0, h as i64);
                            write_nil_error(call, slots::RET_1);
                        }
                        Err(e) => {
                            call.ret_i64(slots::RET_0, 0);
                            write_io_error(call, slots::RET_1, e);
                        }
                    }
                    return ExternResult::Ok;
                }
                None => return ExternResult::WaitIo { token },
            }
        }
    };

    let c = call.io_mut().take_completion(token);
    match c.result {
        Ok(_) => {
            let new_fd = i32::try_from(c.extra)
                .unwrap_or_else(|_| panic!("invalid accepted fd: {}", c.extra));
            let stream = unsafe { TcpStream::from_raw_fd(new_fd) };
            let h = register_tcp_conn(stream);
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

#[vostd_extern_ctx("net", "tcpListenerClose")]
pub fn net_tcp_listener_close(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    
    if TCP_LISTENER_HANDLES.lock().unwrap().remove(&handle).is_some() {
        write_nil_error(call, slots::RET_0);
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "tcpListenerAddr")]
pub fn net_tcp_listener_addr(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    
    let handles = TCP_LISTENER_HANDLES.lock().unwrap();
    let addr_str = if let Some(listener) = handles.get(&handle) {
        listener.local_addr().map(|a| a.to_string()).unwrap_or_default()
    } else {
        String::new()
    };
    let s = call.alloc_str(&addr_str);
    call.ret_ref(slots::RET_0, s);
    ExternResult::Ok
}
