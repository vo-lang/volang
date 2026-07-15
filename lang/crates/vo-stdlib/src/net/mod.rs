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
#[cfg(feature = "std")]
use std::ops::{Deref, DerefMut};
#[cfg(all(feature = "std", unix))]
use std::os::fd::AsFd;
#[cfg(all(feature = "std", unix))]
use std::os::unix::net::{UnixListener, UnixStream};
#[cfg(feature = "std")]
use std::sync::{Mutex, MutexGuard};

#[cfg(feature = "std")]
use vo_ffi_macro::vostd_errors;
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::write_error_to;
#[cfg(feature = "std")]
use vo_runtime::ffi::ExternCallContext;
use vo_runtime::ffi::ExternRegistry;
#[cfg(feature = "std")]
use vo_runtime::ffi::StdlibEntry;
#[cfg(all(feature = "std", unix))]
use vo_runtime::io::{IoCancelKey, IoCancellation, IoLease};
#[cfg(feature = "std")]
use vo_runtime::io::{IoResourceToken, IoRuntime};

#[cfg(feature = "std")]
struct NativeHandle<T> {
    inner: T,
    cleanup_token: IoResourceToken,
    #[cfg(unix)]
    cancellation: IoCancellation,
}

#[cfg(feature = "std")]
impl<T> NativeHandle<T> {
    fn new(inner: T, cleanup_token: IoResourceToken) -> std::io::Result<Self> {
        Ok(Self {
            inner,
            cleanup_token,
            #[cfg(unix)]
            cancellation: IoCancellation::new()?,
        })
    }
}

#[cfg(all(feature = "std", unix))]
impl<T: AsFd> NativeHandle<T> {
    /// Must be called while the registry lock is held.
    fn lease(&self, _handle: i32) -> std::io::Result<IoLease> {
        IoLease::try_clone(&self.inner, self.cancellation.clone())
    }

    /// Linearizes Close after registry removal, marks leases in every VM, and
    /// returns the source fd for fast cancellation in the current VM.
    fn cancel(&self, _handle: i32) -> IoCancelKey {
        self.cancellation.cancel();
        self.cancellation.cancel_key()
    }
}

#[cfg(feature = "std")]
impl<T> Deref for NativeHandle<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[cfg(feature = "std")]
impl<T> DerefMut for NativeHandle<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[cfg(feature = "std")]
lazy_static::lazy_static! {
    static ref TCP_CONN_HANDLES: Mutex<HashMap<i32, NativeHandle<TcpStream>>> = Mutex::new(HashMap::new());
    static ref TCP_LISTENER_HANDLES: Mutex<HashMap<i32, NativeHandle<TcpListener>>> = Mutex::new(HashMap::new());
    static ref UDP_CONN_HANDLES: Mutex<HashMap<i32, NativeHandle<UdpSocket>>> = Mutex::new(HashMap::new());
    static ref NEXT_HANDLE: Mutex<Option<i32>> = Mutex::new(Some(1000));
}

#[cfg(feature = "std")]
fn lock_recover<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}

#[cfg(all(feature = "std", unix))]
lazy_static::lazy_static! {
    static ref UNIX_CONN_HANDLES: Mutex<HashMap<i32, NativeHandle<UnixStream>>> = Mutex::new(HashMap::new());
    static ref UNIX_LISTENER_HANDLES: Mutex<HashMap<i32, NativeHandle<UnixListener>>> = Mutex::new(HashMap::new());
}

#[cfg(feature = "std")]
fn take_next_handle(next: &mut Option<i32>) -> std::io::Result<i32> {
    let handle = next
        .take()
        .ok_or_else(|| std::io::Error::other("network handle space exhausted"))?;
    *next = handle.checked_add(1);
    Ok(handle)
}

#[cfg(feature = "std")]
fn register_handle<T: Send + 'static>(
    io: &mut IoRuntime,
    table: &'static Mutex<HashMap<i32, NativeHandle<T>>>,
    value: T,
) -> std::io::Result<i32> {
    let handle = take_next_handle(&mut lock_recover(&NEXT_HANDLE))?;
    let mut handles = lock_recover(table);
    if handles.contains_key(&handle) {
        return Err(std::io::Error::other(format!(
            "network handle allocation would overwrite live handle {handle}"
        )));
    }
    let cleanup_token = io.register_resource_cleanup(move |token| {
        move || cleanup_native_handle(table, handle, token)
    })?;
    let resource = match NativeHandle::new(value, cleanup_token) {
        Ok(resource) => resource,
        Err(error) => {
            io.disarm_resource_cleanup(cleanup_token);
            return Err(error);
        }
    };
    handles.insert(handle, resource);
    Ok(handle)
}

#[cfg(feature = "std")]
fn cleanup_native_handle<T: Send + 'static>(
    table: &'static Mutex<HashMap<i32, NativeHandle<T>>>,
    handle: i32,
    cleanup_token: IoResourceToken,
) {
    let removed = {
        let mut handles = table
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if handles
            .get(&handle)
            .is_some_and(|resource| resource.cleanup_token == cleanup_token)
        {
            handles.remove(&handle)
        } else {
            None
        }
    };
    if let Some(resource) = removed {
        #[cfg(unix)]
        resource.cancellation.cancel();
        drop(resource);
    }
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
    }
}

#[cfg(feature = "std")]
fn deadline_to_timeout(deadline_ns: i64) -> Option<std::time::Duration> {
    if deadline_ns <= 0 {
        None
    } else {
        let now_ns = match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
            Ok(duration) => i128::try_from(duration.as_nanos()).unwrap_or(i128::MAX),
            Err(error) => -i128::try_from(error.duration().as_nanos()).unwrap_or(i128::MAX),
        };
        let diff = i128::from(deadline_ns) - now_ns;
        if diff <= 0 {
            Some(std::time::Duration::from_nanos(1))
        } else {
            Some(std::time::Duration::from_nanos(
                u64::try_from(diff).unwrap_or(u64::MAX),
            ))
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
fn checked_handle(raw: i64) -> std::io::Result<i32> {
    i32::try_from(raw).map_err(|_| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("network handle {raw} is outside the signed 32-bit range"),
        )
    })
}

#[cfg(feature = "std")]
fn checked_handle_arg(
    call: &mut ExternCallContext,
    arg_slot: u16,
    error_ret_slot: u16,
) -> Option<i32> {
    match checked_handle(call.arg_i64(arg_slot)) {
        Ok(handle) => Some(handle),
        Err(error) => {
            write_io_error(call, error_ret_slot, error);
            None
        }
    }
}

#[cfg(feature = "std")]
#[doc(hidden)]
pub const REGISTERED_EXTERNS: &[StdlibEntry] = &[
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "getNetErrors"),
        func: get_net_errors,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "dial"),
        func: tcp::net_dial,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "listen"),
        func: tcp::net_listen,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "blocking_tcpConnRead"),
        func: tcp::net_tcp_conn_read,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "blocking_tcpConnWrite"),
        func: tcp::net_tcp_conn_write,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "tcpConnClose"),
        func: tcp::net_tcp_conn_close,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "tcpConnLocalAddr"),
        func: tcp::net_tcp_conn_local_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "tcpConnRemoteAddr"),
        func: tcp::net_tcp_conn_remote_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "tcpConnSetDeadline"),
        func: tcp::net_tcp_conn_set_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "tcpConnSetReadDeadline"),
        func: tcp::net_tcp_conn_set_read_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "tcpConnSetWriteDeadline"),
        func: tcp::net_tcp_conn_set_write_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "blocking_tcpListenerAccept"),
        func: tcp::net_tcp_listener_accept,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "tcpListenerClose"),
        func: tcp::net_tcp_listener_close,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "tcpListenerAddr"),
        func: tcp::net_tcp_listener_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "listenPacket"),
        func: udp::net_listen_packet,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "blocking_udpConnReadFrom"),
        func: udp::net_udp_conn_read_from,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "blocking_udpConnWriteTo"),
        func: udp::net_udp_conn_write_to,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "udpConnClose"),
        func: udp::net_udp_conn_close,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "udpConnLocalAddr"),
        func: udp::net_udp_conn_local_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "udpConnSetDeadline"),
        func: udp::net_udp_conn_set_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "udpConnSetReadDeadline"),
        func: udp::net_udp_conn_set_read_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "udpConnSetWriteDeadline"),
        func: udp::net_udp_conn_set_write_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "unixDial"),
        func: unix::net_unix_dial,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "unixListen"),
        func: unix::net_unix_listen,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "blocking_unixConnRead"),
        func: unix::net_unix_conn_read,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "blocking_unixConnWrite"),
        func: unix::net_unix_conn_write,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "unixConnSetDeadline"),
        func: unix::net_unix_conn_set_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "unixConnSetReadDeadline"),
        func: unix::net_unix_conn_set_read_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "unixConnSetWriteDeadline"),
        func: unix::net_unix_conn_set_write_deadline,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "unixConnClose"),
        func: unix::net_unix_conn_close,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "blocking_unixListenerAccept"),
        func: unix::net_unix_listener_accept,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    #[cfg(unix)]
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "unixListenerClose"),
        func: unix::net_unix_listener_close,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "lookupHost"),
        func: dns::net_lookup_host,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "lookupIP"),
        func: dns::net_lookup_ip,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "lookupAddr"),
        func: dns::net_lookup_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "resolveTCPAddr"),
        func: dns::net_resolve_tcp_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net", "resolveUDPAddr"),
        func: dns::net_resolve_udp_addr,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
];

#[cfg(feature = "std")]
pub fn register_externs(
    registry: &mut ExternRegistry,
    externs: &[vo_runtime::bytecode::ExternDef],
) -> Result<(), vo_runtime::ffi::ExternContractError> {
    for (id, def) in externs.iter().enumerate() {
        let id = id as u32;
        for entry in REGISTERED_EXTERNS {
            if def.name == entry.name() {
                entry.try_register(registry, id)?;
                break;
            }
        }
    }
    Ok(())
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::{checked_handle, lock_recover, take_next_handle};

    #[cfg(unix)]
    use super::{cleanup_native_handle, register_handle, NativeHandle, UNIX_CONN_HANDLES};
    #[cfg(unix)]
    use std::os::unix::net::UnixStream;
    #[cfg(unix)]
    use vo_runtime::io::IoRuntime;

    #[test]
    fn network_handles_reject_values_outside_the_native_table_domain() {
        assert_eq!(checked_handle(i32::MIN as i64).unwrap(), i32::MIN);
        assert_eq!(checked_handle(i32::MAX as i64).unwrap(), i32::MAX);
        for raw in [i32::MIN as i64 - 1, i32::MAX as i64 + 1, i64::MIN, i64::MAX] {
            let error = checked_handle(raw).unwrap_err();
            assert_eq!(error.kind(), std::io::ErrorKind::InvalidInput);
            assert!(error.to_string().contains(&raw.to_string()));
        }
    }

    #[test]
    fn network_handle_allocator_exhausts_without_wrapping_or_aliasing() {
        let mut next = Some(i32::MAX - 1);
        assert_eq!(take_next_handle(&mut next).unwrap(), i32::MAX - 1);
        assert_eq!(take_next_handle(&mut next).unwrap(), i32::MAX);
        assert_eq!(next, None);

        let error = take_next_handle(&mut next).unwrap_err();
        assert_eq!(error.kind(), std::io::ErrorKind::Other);
        assert!(error.to_string().contains("handle space exhausted"));
    }

    #[test]
    fn network_registry_lock_recovers_after_poisoning() {
        let table = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let poisoner = std::sync::Arc::clone(&table);
        let result = std::thread::spawn(move || {
            let _guard = poisoner.lock().expect("fresh network registry lock");
            panic!("poison network registry lock");
        })
        .join();
        assert!(result.is_err());

        lock_recover(&table).push(7);
        assert_eq!(lock_recover(&table).as_slice(), [7]);
    }

    #[test]
    fn network_extern_inputs_never_narrow_i64_with_wrapping_casts() {
        for (name, source) in [
            ("tcp", include_str!("tcp.rs")),
            ("udp", include_str!("udp.rs")),
            ("unix", include_str!("unix.rs")),
        ] {
            assert!(
                !source.contains("arg_i64(slots::ARG_HANDLE) as i32"),
                "{name} extern handle must use checked conversion"
            );
        }
    }

    #[cfg(unix)]
    #[test]
    fn owner_vm_drop_removes_network_handle_and_cancels_cross_vm_io() {
        let (stream, _peer) = UnixStream::pair().expect("Unix stream pair");
        stream.set_nonblocking(true).expect("nonblocking stream");
        let mut owner = IoRuntime::new().expect("owner I/O runtime");
        let handle = register_handle(&mut owner, &UNIX_CONN_HANDLES, stream)
            .expect("register Unix connection");
        let lease = lock_recover(&UNIX_CONN_HANDLES)
            .get(&handle)
            .expect("registered Unix connection")
            .lease(handle)
            .expect("cross-VM I/O lease");
        let mut waiter = IoRuntime::new().expect("waiter I/O runtime");
        let token = waiter.submit_lease_owned_read(lease, 1);
        assert!(!waiter.has_completion(token));

        drop(owner);
        assert!(!lock_recover(&UNIX_CONN_HANDLES).contains_key(&handle));
        assert_eq!(waiter.poll(), vec![token]);
        let error = waiter.take_completion(token).result.unwrap_err();
        assert_eq!(error.kind(), std::io::ErrorKind::Interrupted);
        assert!(!waiter.has_pending());
    }

    #[cfg(unix)]
    #[test]
    fn stale_network_guard_cannot_remove_reused_handle() {
        let (old_stream, _old_peer) = UnixStream::pair().expect("old Unix stream pair");
        let mut old_owner = IoRuntime::new().expect("old owner I/O runtime");
        let handle = register_handle(&mut old_owner, &UNIX_CONN_HANDLES, old_stream)
            .expect("register old Unix connection");
        let old_resource = lock_recover(&UNIX_CONN_HANDLES)
            .remove(&handle)
            .expect("remove old resource without disarming its owner guard");
        let old_cancel_key = old_resource.cancellation.cancel_key();
        old_resource.cancellation.cancel();
        drop(old_resource);

        let (new_stream, _new_peer) = UnixStream::pair().expect("new Unix stream pair");
        let mut new_owner = IoRuntime::new().expect("new owner I/O runtime");
        let cleanup_token = new_owner
            .register_resource_cleanup(move |token| {
                move || cleanup_native_handle(&UNIX_CONN_HANDLES, handle, token)
            })
            .expect("new resource cleanup");
        lock_recover(&UNIX_CONN_HANDLES).insert(
            handle,
            NativeHandle::new(new_stream, cleanup_token).expect("new resource cancellation key"),
        );
        let new_cancel_key = lock_recover(&UNIX_CONN_HANDLES)
            .get(&handle)
            .expect("new resource")
            .cancellation
            .cancel_key();
        assert_ne!(old_cancel_key, new_cancel_key);

        drop(old_owner);
        {
            let handles = lock_recover(&UNIX_CONN_HANDLES);
            let resource = handles
                .get(&handle)
                .expect("stale owner cleanup removed a reused handle generation");
            assert_eq!(resource.cleanup_token, cleanup_token);
            assert!(!resource.cancellation.is_cancelled());
        }
        drop(new_owner);
        assert!(!lock_recover(&UNIX_CONN_HANDLES).contains_key(&handle));
    }
}

#[cfg(not(feature = "std"))]
pub fn register_externs(
    _registry: &mut ExternRegistry,
    _externs: &[vo_runtime::bytecode::ExternDef],
) -> Result<(), vo_runtime::ffi::ExternContractError> {
    // No-op: WASM platform externs are registered by vo-web-runtime-wasm
    Ok(())
}
