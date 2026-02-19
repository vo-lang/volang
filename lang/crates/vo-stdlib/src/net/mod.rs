//! net package native function implementations.
//!
//! This module requires std for network operations.

#[cfg(feature = "std")]
mod tcp;
#[cfg(feature = "std")]
mod udp;
#[cfg(feature = "std")]
mod unix;
#[cfg(feature = "std")]
mod dns;
pub mod http;

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(feature = "std")]
use std::sync::Mutex;
#[cfg(feature = "std")]
use std::net::{TcpStream, TcpListener, UdpSocket};
#[cfg(all(feature = "std", unix))]
use std::os::unix::net::{UnixStream, UnixListener};

#[cfg(feature = "std")]
use vo_ffi_macro::vostd_errors;
use vo_runtime::ffi::ExternRegistry;
#[cfg(feature = "std")]
use vo_runtime::ffi::ExternCallContext;
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::write_error_to;

#[cfg(feature = "std")]
lazy_static::lazy_static! {
    static ref TCP_CONN_HANDLES: Mutex<HashMap<i32, TcpStream>> = Mutex::new(HashMap::new());
    static ref TCP_LISTENER_HANDLES: Mutex<HashMap<i32, TcpListener>> = Mutex::new(HashMap::new());
    static ref UDP_CONN_HANDLES: Mutex<HashMap<i32, UdpSocket>> = Mutex::new(HashMap::new());
    static ref NEXT_HANDLE: Mutex<i32> = Mutex::new(1000);
}

#[cfg(all(feature = "std", unix))]
lazy_static::lazy_static! {
    static ref UNIX_CONN_HANDLES: Mutex<HashMap<i32, UnixStream>> = Mutex::new(HashMap::new());
    static ref UNIX_LISTENER_HANDLES: Mutex<HashMap<i32, UnixListener>> = Mutex::new(HashMap::new());
}

#[cfg(feature = "std")]
fn next_handle() -> i32 {
    let mut next = NEXT_HANDLE.lock().unwrap();
    let h = *next;
    *next += 1;
    h
}

#[cfg(feature = "std")]
vostd_errors! {
    "net" => {
        Closed => "use of closed network connection",
        Timeout => "i/o timeout",
        Refused => "connection refused",
        Reset => "connection reset by peer",
        AddrInUse => "address already in use",
        AddrNotAvail => "address not available",
        HostUnreach => "no route to host",
        NetUnreach => "network is unreachable",
        InvalidAddr => "invalid address",
        Unknown => "unknown network error",
    }
}

#[cfg(feature = "std")]
fn deadline_to_timeout(deadline_ns: i64) -> Option<std::time::Duration> {
    if deadline_ns <= 0 {
        None
    } else {
        let now_ns = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos() as i64;
        let diff = deadline_ns - now_ns;
        if diff <= 0 {
            Some(std::time::Duration::from_nanos(1))
        } else {
            Some(std::time::Duration::from_nanos(diff as u64))
        }
    }
}

#[cfg(feature = "std")]
fn write_io_error(call: &mut ExternCallContext, ret_slot: u16, err: std::io::Error) {
    use std::io::ErrorKind;
    let kind = match err.kind() {
        ErrorKind::ConnectionRefused => Some(NetErrorKind::Refused),
        ErrorKind::ConnectionReset => Some(NetErrorKind::Reset),
        ErrorKind::AddrInUse => Some(NetErrorKind::AddrInUse),
        ErrorKind::AddrNotAvailable => Some(NetErrorKind::AddrNotAvail),
        ErrorKind::TimedOut => Some(NetErrorKind::Timeout),
        ErrorKind::NotConnected => Some(NetErrorKind::Closed),
        _ => None,
    };
    
    if let Some(k) = kind {
        let pair = net_sentinel_error(call, k);
        call.ret_interface_pair(ret_slot, pair);
    } else {
        write_error_to(call, ret_slot, &err.to_string());
    }
}

#[cfg(feature = "std")]
pub fn register_externs(registry: &mut ExternRegistry, externs: &[vo_runtime::bytecode::ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        let id = id as u32;
        match def.name.as_str() {
            // TCP
            "net_dial" => registry.register(id, tcp::net_dial),
            "net_listen" => registry.register(id, tcp::net_listen),
            "net_blocking_tcpConnRead" => registry.register(id, tcp::net_tcp_conn_read),
            "net_blocking_tcpConnWrite" => registry.register(id, tcp::net_tcp_conn_write),
            "net_tcpConnClose" => registry.register(id, tcp::net_tcp_conn_close),
            "net_tcpConnLocalAddr" => registry.register(id, tcp::net_tcp_conn_local_addr),
            "net_tcpConnRemoteAddr" => registry.register(id, tcp::net_tcp_conn_remote_addr),
            "net_tcpConnSetDeadline" => registry.register(id, tcp::net_tcp_conn_set_deadline),
            "net_tcpConnSetReadDeadline" => registry.register(id, tcp::net_tcp_conn_set_read_deadline),
            "net_tcpConnSetWriteDeadline" => registry.register(id, tcp::net_tcp_conn_set_write_deadline),
            "net_blocking_tcpListenerAccept" => registry.register(id, tcp::net_tcp_listener_accept),
            "net_tcpListenerClose" => registry.register(id, tcp::net_tcp_listener_close),
            "net_tcpListenerAddr" => registry.register(id, tcp::net_tcp_listener_addr),
            // UDP (async)
            "net_listenPacket" => registry.register(id, udp::net_listen_packet),
            "net_blocking_udpConnReadFrom" => registry.register(id, udp::net_udp_conn_read_from),
            "net_blocking_udpConnWriteTo" => registry.register(id, udp::net_udp_conn_write_to),
            "net_udpConnClose" => registry.register(id, udp::net_udp_conn_close),
            "net_udpConnLocalAddr" => registry.register(id, udp::net_udp_conn_local_addr),
            "net_udpConnSetDeadline" => registry.register(id, udp::net_udp_conn_set_deadline),
            "net_udpConnSetReadDeadline" => registry.register(id, udp::net_udp_conn_set_read_deadline),
            "net_udpConnSetWriteDeadline" => registry.register(id, udp::net_udp_conn_set_write_deadline),
            // Unix (cfg(unix) only, async)
            #[cfg(unix)]
            "net_unixDial" => registry.register(id, unix::net_unix_dial),
            #[cfg(unix)]
            "net_unixListen" => registry.register(id, unix::net_unix_listen),
            #[cfg(unix)]
            "net_blocking_unixConnRead" => registry.register(id, unix::net_unix_conn_read),
            #[cfg(unix)]
            "net_blocking_unixConnWrite" => registry.register(id, unix::net_unix_conn_write),
            #[cfg(unix)]
            "net_unixConnSetDeadline" => registry.register(id, unix::net_unix_conn_set_deadline),
            #[cfg(unix)]
            "net_unixConnSetReadDeadline" => registry.register(id, unix::net_unix_conn_set_read_deadline),
            #[cfg(unix)]
            "net_unixConnSetWriteDeadline" => registry.register(id, unix::net_unix_conn_set_write_deadline),
            #[cfg(unix)]
            "net_unixConnClose" => registry.register(id, unix::net_unix_conn_close),
            #[cfg(unix)]
            "net_blocking_unixListenerAccept" => registry.register(id, unix::net_unix_listener_accept),
            #[cfg(unix)]
            "net_unixListenerClose" => registry.register(id, unix::net_unix_listener_close),
            // DNS
            "net_lookupHost" => registry.register(id, dns::net_lookup_host),
            "net_lookupIP" => registry.register(id, dns::net_lookup_ip),
            "net_lookupAddr" => registry.register(id, dns::net_lookup_addr),
            // Resolve
            "net_resolveTCPAddr" => registry.register(id, dns::net_resolve_tcp_addr),
            "net_resolveUDPAddr" => registry.register(id, dns::net_resolve_udp_addr),
            _ => {}
        }
    }
}

#[cfg(not(feature = "std"))]
pub fn register_externs(
    _registry: &mut ExternRegistry,
    _externs: &[vo_runtime::bytecode::ExternDef],
) {
    // No-op: WASM platform externs are registered by vo-web-runtime-wasm
}
