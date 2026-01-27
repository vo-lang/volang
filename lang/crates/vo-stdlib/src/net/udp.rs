//! UDP connection implementations.

use std::net::{UdpSocket, ToSocketAddrs};

use vo_ffi_macro::vostd_extern_ctx;
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::objects::slice;
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};

use super::{UDP_CONN_HANDLES, next_handle, write_io_error};

fn register_udp_conn(conn: UdpSocket) -> i32 {
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

#[vostd_extern_ctx("net", "udpConnReadFrom")]
pub fn net_udp_conn_read_from(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let buf_ref = call.arg_ref(slots::ARG_P);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);
    let buf = unsafe { std::slice::from_raw_parts_mut(buf_ptr, buf_len) };
    
    let handles = UDP_CONN_HANDLES.lock().unwrap();
    if let Some(conn) = handles.get(&handle) {
        match conn.recv_from(buf) {
            Ok((n, addr)) => {
                call.ret_i64(slots::RET_0, n as i64);
                let addr_str = call.alloc_str(&addr.to_string());
                call.ret_ref(slots::RET_1, addr_str);
                write_nil_error(call, slots::RET_2);
            }
            Err(e) => {
                call.ret_i64(slots::RET_0, 0);
                call.ret_nil(slots::RET_1);
                write_io_error(call, slots::RET_2, e);
            }
        }
    } else {
        call.ret_i64(slots::RET_0, 0);
        call.ret_nil(slots::RET_1);
        write_error_to(call, slots::RET_2, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_extern_ctx("net", "udpConnWriteTo")]
pub fn net_udp_conn_write_to(call: &mut ExternCallContext) -> ExternResult {
    let handle = call.arg_i64(slots::ARG_HANDLE) as i32;
    let buf_ref = call.arg_ref(slots::ARG_P);
    let addr_str = call.arg_str(slots::ARG_ADDR);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);
    let buf = unsafe { std::slice::from_raw_parts(buf_ptr, buf_len) };
    
    let handles = UDP_CONN_HANDLES.lock().unwrap();
    if let Some(conn) = handles.get(&handle) {
        match addr_str.to_socket_addrs() {
            Ok(mut addrs) => {
                if let Some(addr) = addrs.next() {
                    match conn.send_to(buf, addr) {
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
                    write_error_to(call, slots::RET_1, "invalid address");
                }
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
