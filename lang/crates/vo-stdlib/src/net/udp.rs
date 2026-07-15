//! UDP connection implementations.

use std::net::{SocketAddr, ToSocketAddrs, UdpSocket};

use vo_ffi_macro::vostd_fn;
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::io::IoRuntime;
#[cfg(unix)]
use vo_runtime::io::{Completion, CompletionData};
#[cfg(not(unix))]
use vo_runtime::objects::slice;

use super::{
    checked_handle, checked_handle_arg, register_handle, write_io_error, UDP_CONN_HANDLES,
};

fn register_udp_conn(io: &mut IoRuntime, conn: UdpSocket) -> std::io::Result<i32> {
    #[cfg(unix)]
    conn.set_nonblocking(true)?;
    register_handle(io, &UDP_CONN_HANDLES, conn)
}

#[vostd_fn("net", "listenPacket", std)]
pub fn net_listen_packet(call: &mut ExternCallContext) -> ExternResult {
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

    match network.as_str() {
        "udp" | "udp4" | "udp6" => match UdpSocket::bind(address) {
            Ok(socket) => match register_udp_conn(call.io_mut(), socket) {
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
        _ => {
            call.ret_i64(slots::RET_0, 0);
            write_error_to(call, slots::RET_1, &format!("unknown network: {}", network));
        }
    }
    ExternResult::Ok
}

#[vostd_fn("net", "blocking_udpConnReadFrom", std)]
pub fn net_udp_conn_read_from(call: &mut ExternCallContext) -> ExternResult {
    #[cfg(unix)]
    {
        let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_2) else {
            call.ret_i64(slots::RET_0, 0);
            call.ret_nil(slots::RET_1);
            return ExternResult::Ok;
        };
        let resume_token = call.take_resume_io_token();
        let buf_ref = call.arg_ref(slots::ARG_P);
        // Safety: `buf_ref` is a rooted []byte extern argument.

        let token = match resume_token {
            Some(token) => token,
            None => {
                let lease = {
                    let handles = super::lock_recover(&UDP_CONN_HANDLES);
                    match handles.get(&handle) {
                        Some(conn) => match conn.lease(handle) {
                            Ok(lease) => lease,
                            Err(error) => {
                                call.ret_i64(slots::RET_0, 0);
                                call.ret_nil(slots::RET_1);
                                write_io_error(call, slots::RET_2, error);
                                return ExternResult::Ok;
                            }
                        },
                        None => {
                            call.ret_i64(slots::RET_0, 0);
                            call.ret_nil(slots::RET_1);
                            write_error_to(call, slots::RET_2, "use of closed network connection");
                            return ExternResult::Ok;
                        }
                    }
                };
                let token = call.io_mut().submit_lease_slice_recv_from(lease, buf_ref);
                match call.io_mut().try_take_completion(token) {
                    Some(c) => {
                        return handle_recv_from_completion(
                            call,
                            c,
                            slots::RET_0,
                            slots::RET_1,
                            slots::RET_2,
                        )
                    }
                    None => return ExternResult::WaitIo { token },
                }
            }
        };

        let c = call.io_mut().take_completion(token);
        handle_recv_from_completion(call, c, slots::RET_0, slots::RET_1, slots::RET_2)
    }

    #[cfg(not(unix))]
    {
        let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_2) else {
            call.ret_i64(slots::RET_0, 0);
            call.ret_nil(slots::RET_1);
            return ExternResult::Ok;
        };
        let buf_ref = call.arg_ref(slots::ARG_P);
        // Safety: `buf_ref` is a rooted []byte extern argument.
        let mut buf = vec![0u8; unsafe { slice::len(buf_ref) }];

        let mut handles = super::lock_recover(&UDP_CONN_HANDLES);
        match handles.get_mut(&handle) {
            Some(conn) => match conn.recv_from(&mut buf) {
                Ok((n, addr)) => {
                    unsafe { slice::write_bytes(buf_ref, &buf[..n]) };
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
            },
            None => {
                call.ret_i64(slots::RET_0, 0);
                call.ret_nil(slots::RET_1);
                write_error_to(call, slots::RET_2, "use of closed network connection");
            }
        }
        ExternResult::Ok
    }
}

#[cfg(unix)]
fn handle_recv_from_completion(
    call: &mut ExternCallContext,
    c: Completion,
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

#[vostd_fn("net", "blocking_udpConnWriteTo", std)]
pub fn net_udp_conn_write_to(call: &mut ExternCallContext) -> ExternResult {
    #[cfg(unix)]
    {
        let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_1) else {
            call.ret_i64(slots::RET_0, 0);
            return ExternResult::Ok;
        };
        let resume_token = call.take_resume_io_token();
        let buf_ref = call.arg_ref(slots::ARG_P);
        let addr_str =
            match crate::host_bytes::utf8_arg(call, slots::ARG_ADDR, "destination address") {
                Ok(address) => address,
                Err(error) => {
                    call.ret_i64(slots::RET_0, 0);
                    write_error_to(call, slots::RET_1, &error);
                    return ExternResult::Ok;
                }
            };

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

        let token = match resume_token {
            Some(token) => token,
            None => {
                let lease = {
                    let handles = super::lock_recover(&UDP_CONN_HANDLES);
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
                let token = call
                    .io_mut()
                    .submit_lease_slice_send_to(lease, buf_ref, dest_addr);
                match call.io_mut().try_take_completion(token) {
                    Some(c) => {
                        return handle_send_to_completion(call, c, slots::RET_0, slots::RET_1)
                    }
                    None => return ExternResult::WaitIo { token },
                }
            }
        };

        let c = call.io_mut().take_completion(token);
        handle_send_to_completion(call, c, slots::RET_0, slots::RET_1)
    }

    #[cfg(not(unix))]
    {
        let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_1) else {
            call.ret_i64(slots::RET_0, 0);
            return ExternResult::Ok;
        };
        let buf_ref = call.arg_ref(slots::ARG_P);
        let addr_str =
            match crate::host_bytes::utf8_arg(call, slots::ARG_ADDR, "destination address") {
                Ok(address) => address,
                Err(error) => {
                    call.ret_i64(slots::RET_0, 0);
                    write_error_to(call, slots::RET_1, &error);
                    return ExternResult::Ok;
                }
            };
        let buf = unsafe { slice::byte_vec(buf_ref) };

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

        let mut handles = super::lock_recover(&UDP_CONN_HANDLES);
        match handles.get_mut(&handle) {
            Some(conn) => match conn.send_to(&buf, dest_addr) {
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

#[cfg(unix)]
fn handle_send_to_completion(
    call: &mut ExternCallContext,
    c: Completion,
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

#[vostd_fn("net", "udpConnClose", std)]
pub fn net_udp_conn_close(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };

    if let Some(conn) = super::lock_recover(&UDP_CONN_HANDLES).remove(&handle) {
        call.io_mut().disarm_resource_cleanup(conn.cleanup_token);
        #[cfg(unix)]
        {
            let cancel_key = conn.cancel(handle);
            call.io_mut().cancel(cancel_key);
        }
        write_nil_error(call, slots::RET_0);
    } else {
        write_error_to(call, slots::RET_0, "use of closed network connection");
    }
    ExternResult::Ok
}

#[vostd_fn("net", "udpConnLocalAddr", std)]
pub fn net_udp_conn_local_addr(call: &mut ExternCallContext) -> ExternResult {
    let handle = checked_handle(call.arg_i64(slots::ARG_HANDLE)).ok();

    let handles = super::lock_recover(&UDP_CONN_HANDLES);
    let addr_str = if let Some(conn) = handle.and_then(|handle| handles.get(&handle)) {
        conn.local_addr().map(|a| a.to_string()).unwrap_or_default()
    } else {
        String::new()
    };
    let s = call.alloc_str(&addr_str);
    call.ret_ref(slots::RET_0, s);
    ExternResult::Ok
}

fn set_udp_deadline(
    conn: &UdpSocket,
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

#[vostd_fn("net", "udpConnSetDeadline", std)]
pub fn net_udp_conn_set_deadline(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);

    let handles = super::lock_recover(&UDP_CONN_HANDLES);
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

#[vostd_fn("net", "udpConnSetReadDeadline", std)]
pub fn net_udp_conn_set_read_deadline(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);

    let handles = super::lock_recover(&UDP_CONN_HANDLES);
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

#[vostd_fn("net", "udpConnSetWriteDeadline", std)]
pub fn net_udp_conn_set_write_deadline(call: &mut ExternCallContext) -> ExternResult {
    let Some(handle) = checked_handle_arg(call, slots::ARG_HANDLE, slots::RET_0) else {
        return ExternResult::Ok;
    };
    let deadline_ns = call.arg_i64(slots::ARG_DEADLINE_NS);

    let handles = super::lock_recover(&UDP_CONN_HANDLES);
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
