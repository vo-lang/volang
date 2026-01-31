//! UDP connection implementations.

use std::net::{UdpSocket, SocketAddr, ToSocketAddrs};
use std::os::fd::AsRawFd;

use vo_ffi_macro::vostd_extern_ctx;
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::io::{IoHandle, CompletionData};
use vo_runtime::objects::slice;
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};

use super::{UDP_CONN_HANDLES, next_handle, write_io_error};

fn register_udp_conn(conn: UdpSocket) -> i32 {
    // Set non-blocking for async I/O
    conn.set_nonblocking(true).ok();
    let h = next_handle();
    UDP_CONN_HANDLES.lock().unwrap().insert(h, conn);
    h
}

#[vostd_extern_ctx("net", "listenPacket")]
pub fn net_listen_packet(call: &mut ExternCallContext) -> ExternResult {
    let network = call.arg_str(slots::ARG_NETWORK).to_string();
    let address = call.arg_str(slots::ARG_ADDRESS).to_string();
    
    match network.as_str() {
        "udp" | "udp4" | "udp6" => {
            match UdpSocket::bind(address) {
                Ok(socket) => {
                    let h = register_udp_conn(socket);
                    call.ret_i64(slots::RET_0, h as i64);
                    write_nil_error(call, slots::RET_1);
                }
                Err(e) => {
                    call.ret_i64(slots::RET_0, 0);
                    write_io_error(call, slots::RET_1, e);
                }
            }
        }
        _ => {
            call.ret_i64(slots::RET_0, 0);
            write_error_to(call, slots::RET_1, &format!("unknown network: {}", network));
        }
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "waitio_udpConnReadFrom")]
pub fn net_udp_conn_read_from(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let buf_ref = call.arg_ref(slots::ARG_P);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);

    let fd = {
        let handles = UDP_CONN_HANDLES.lock().unwrap();
        match handles.get(&handle) {
            Some(c) => c.as_raw_fd(),
            None => {
                call.ret_i64(slots::RET_0, 0);
                call.ret_nil(slots::RET_1);
                write_error_to(call, slots::RET_2, "use of closed network connection");
                return ExternResult::Ok;
            }
        }
    };

    let token = match call.resume_io_token() {
        Some(token) => token,
        None => {
            let token = call.io_mut().submit_recv_from(fd as IoHandle, buf_ptr, buf_len);
            match call.io_mut().try_take_completion(token) {
                Some(c) => return handle_recv_from_completion(call, c, slots::RET_0, slots::RET_1, slots::RET_2),
                None => return ExternResult::WaitIo { token },
            }
        }
    };

    let c = call.io_mut().take_completion(token);
    handle_recv_from_completion(call, c, slots::RET_0, slots::RET_1, slots::RET_2)
}

fn handle_recv_from_completion(
    call: &mut ExternCallContext,
    c: vo_runtime::io::Completion,
    ret_n: u16,
    ret_addr: u16,
    ret_err: u16,
) -> ExternResult {
    match c.result {
        Ok(CompletionData::RecvFrom(n, addr)) => {
            call.ret_i64(ret_n, n as i64);
            let addr_str = call.alloc_str(&addr.to_string());
            call.ret_ref(ret_addr, addr_str);
            write_nil_error(call, ret_err);
        }
        Ok(_) => panic!("unexpected completion data for RecvFrom"),
        Err(e) => {
            call.ret_i64(ret_n, 0);
            call.ret_nil(ret_addr);
            write_io_error(call, ret_err, e);
        }
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "waitio_udpConnWriteTo")]
pub fn net_udp_conn_write_to(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let buf_ref = call.arg_ref(slots::ARG_P);
    let addr_str = call.arg_str(slots::ARG_ADDR);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);

    // Parse address first (blocking, but fast)
    let dest_addr: SocketAddr = match addr_str.to_socket_addrs() {
        Ok(mut addrs) => match addrs.next() {
            Some(addr) => addr,
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "invalid address");
                return ExternResult::Ok;
            }
        },
        Err(e) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, e);
            return ExternResult::Ok;
        }
    };

    let fd = {
        let handles = UDP_CONN_HANDLES.lock().unwrap();
        match handles.get(&handle) {
            Some(c) => c.as_raw_fd(),
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "use of closed network connection");
                return ExternResult::Ok;
            }
        }
    };

    let token = match call.resume_io_token() {
        Some(token) => token,
        None => {
            let token = call.io_mut().submit_send_to(fd as IoHandle, buf_ptr, buf_len, dest_addr);
            match call.io_mut().try_take_completion(token) {
                Some(c) => return handle_send_to_completion(call, c, slots::RET_0, slots::RET_1),
                None => return ExternResult::WaitIo { token },
            }
        }
    };

    let c = call.io_mut().take_completion(token);
    handle_send_to_completion(call, c, slots::RET_0, slots::RET_1)
}

fn handle_send_to_completion(
    call: &mut ExternCallContext,
    c: vo_runtime::io::Completion,
    ret_n: u16,
    ret_err: u16,
) -> ExternResult {
    match c.result {
        Ok(CompletionData::Size(n)) => {
            call.ret_i64(ret_n, n as i64);
            write_nil_error(call, ret_err);
        }
        Ok(_) => panic!("unexpected completion data for SendTo"),
        Err(e) => {
            call.ret_i64(ret_n, 0);
            write_io_error(call, ret_err, e);
        }
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "udpConnClose")]
pub fn net_udp_conn_close(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    
    if UDP_CONN_HANDLES.lock().unwrap().remove(&handle).is_some() {
        write_nil_error(call, slots::RET_0);
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "udpConnLocalAddr")]
pub fn net_udp_conn_local_addr(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    
    let handles = UDP_CONN_HANDLES.lock().unwrap();
    let addr_str = if let Some(conn) = handles.get(&handle) {
        conn.local_addr().map(|a| a.to_string()).unwrap_or_default()
    } else {
        String::new()
    };
    let s = call.alloc_str(&addr_str);
    call.ret_ref(slots::RET_0, s);
    ExternResult::Ok
}

fn set_udp_deadline(conn: &UdpSocket, deadline_ns: i64, read: bool, write: bool) -> std::io::Result<()> {
    let timeout = super::deadline_to_timeout(deadline_ns);
    if read {
        conn.set_read_timeout(timeout)?;
    }
    if write {
        conn.set_write_timeout(timeout)?;
    }
    Ok(())
}

#[vostd_extern_ctx("net", "udpConnSetDeadline")]
pub fn net_udp_conn_set_deadline(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);
    
    let handles = UDP_CONN_HANDLES.lock().unwrap();
    if let Some(conn) = handles.get(&handle) {
        match set_udp_deadline(conn, deadline_ns, true, true) {
            Ok(()) => write_nil_error(call, slots::RET_0),
            Err(e) => write_io_error(call, slots::RET_0, e),
        }
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "udpConnSetReadDeadline")]
pub fn net_udp_conn_set_read_deadline(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);
    
    let handles = UDP_CONN_HANDLES.lock().unwrap();
    if let Some(conn) = handles.get(&handle) {
        match set_udp_deadline(conn, deadline_ns, true, false) {
            Ok(()) => write_nil_error(call, slots::RET_0),
            Err(e) => write_io_error(call, slots::RET_0, e),
        }
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "udpConnSetWriteDeadline")]
pub fn net_udp_conn_set_write_deadline(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);
    
    let handles = UDP_CONN_HANDLES.lock().unwrap();
    if let Some(conn) = handles.get(&handle) {
        match set_udp_deadline(conn, deadline_ns, false, true) {
            Ok(()) => write_nil_error(call, slots::RET_0),
            Err(e) => write_io_error(call, slots::RET_0, e),
        }
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}
