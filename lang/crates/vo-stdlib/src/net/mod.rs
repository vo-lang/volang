//! net package native function implementations.
//!
//! This module requires std for network operations.

#[cfg(feature = "std")]
mod dns;
pub mod http;
#[cfg(feature = "std")]
mod tcp;
#[cfg(feature = "std")]
mod udp;
#[cfg(all(feature = "std", unix))]
mod unix;

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(feature = "std")]
use std::net::{TcpListener, TcpStream, UdpSocket};
#[cfg(all(feature = "std", unix))]
use std::os::unix::net::{UnixListener, UnixStream};
#[cfg(feature = "std")]
use std::sync::Mutex;

#[cfg(feature = "std")]
use vo_ffi_macro::vostd_errors;
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::write_error_to;
#[cfg(feature = "std")]
use vo_runtime::ffi::ExternCallContext;
use vo_runtime::ffi::ExternRegistry;
#[cfg(feature = "std")]
use vo_runtime::ffi::StdlibEntry;

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
#[doc(hidden)]
pub const REGISTERED_EXTERNS: &[StdlibEntry] = &[
    StdlibEntry {
        name: "net_dial",
        func: tcp::net_dial,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_listen",
        func: tcp::net_listen,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_blocking_tcpConnRead",
        func: tcp::net_tcp_conn_read,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    StdlibEntry {
        name: "net_blocking_tcpConnWrite",
        func: tcp::net_tcp_conn_write,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    StdlibEntry {
        name: "net_tcpConnClose",
        func: tcp::net_tcp_conn_close,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_tcpConnLocalAddr",
        func: tcp::net_tcp_conn_local_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_tcpConnRemoteAddr",
        func: tcp::net_tcp_conn_remote_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_tcpConnSetDeadline",
        func: tcp::net_tcp_conn_set_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_tcpConnSetReadDeadline",
        func: tcp::net_tcp_conn_set_read_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_tcpConnSetWriteDeadline",
        func: tcp::net_tcp_conn_set_write_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_blocking_tcpListenerAccept",
        func: tcp::net_tcp_listener_accept,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    StdlibEntry {
        name: "net_tcpListenerClose",
        func: tcp::net_tcp_listener_close,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_tcpListenerAddr",
        func: tcp::net_tcp_listener_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_listenPacket",
        func: udp::net_listen_packet,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_blocking_udpConnReadFrom",
        func: udp::net_udp_conn_read_from,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    StdlibEntry {
        name: "net_blocking_udpConnWriteTo",
        func: udp::net_udp_conn_write_to,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    StdlibEntry {
        name: "net_udpConnClose",
        func: udp::net_udp_conn_close,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_udpConnLocalAddr",
        func: udp::net_udp_conn_local_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_udpConnSetDeadline",
        func: udp::net_udp_conn_set_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_udpConnSetReadDeadline",
        func: udp::net_udp_conn_set_read_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_udpConnSetWriteDeadline",
        func: udp::net_udp_conn_set_write_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: "net_unixDial",
        func: unix::net_unix_dial,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: "net_unixListen",
        func: unix::net_unix_listen,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: "net_blocking_unixConnRead",
        func: unix::net_unix_conn_read,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: "net_blocking_unixConnWrite",
        func: unix::net_unix_conn_write,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: "net_unixConnSetDeadline",
        func: unix::net_unix_conn_set_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: "net_unixConnSetReadDeadline",
        func: unix::net_unix_conn_set_read_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: "net_unixConnSetWriteDeadline",
        func: unix::net_unix_conn_set_write_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: "net_unixConnClose",
        func: unix::net_unix_conn_close,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: "net_blocking_unixListenerAccept",
        func: unix::net_unix_listener_accept,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: "net_unixListenerClose",
        func: unix::net_unix_listener_close,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_lookupHost",
        func: dns::net_lookup_host,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_lookupIP",
        func: dns::net_lookup_ip,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_lookupAddr",
        func: dns::net_lookup_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_resolveTCPAddr",
        func: dns::net_resolve_tcp_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: "net_resolveUDPAddr",
        func: dns::net_resolve_udp_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
];

#[cfg(feature = "std")]
pub fn register_externs(
    registry: &mut ExternRegistry,
    externs: &[vo_runtime::bytecode::ExternDef],
) {
    for (id, def) in externs.iter().enumerate() {
        let id = id as u32;
        for entry in REGISTERED_EXTERNS {
            if def.name == entry.name() {
                entry.register(registry, id);
                break;
            }
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
