//! DNS lookup implementations using system resolver.

use std::net::ToSocketAddrs;

use vo_common_core::types::{ValueKind, ValueMeta};
use vo_ffi_macro::vostd_fn;
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::slice;

use super::write_io_error;

#[vostd_fn("net", "lookupHost", std)]
pub fn net_lookup_host(call: &mut ExternCallContext) -> ExternResult {
    let host = match crate::host_bytes::utf8_arg(call, slots::ARG_HOST, "host name") {
        Ok(host) => host,
        Err(error) => {
            call.ret_nil(slots::RET_0);
            write_error_to(call, slots::RET_1, &error);
            return ExternResult::Ok;
        }
    };

    // Use ToSocketAddrs with port 0 to resolve hostnames
    let lookup_addr = format!("{}:0", host);
    match lookup_addr.to_socket_addrs() {
        Ok(addrs) => {
            let addresses: Vec<String> = addrs.map(|a| a.ip().to_string()).collect();

            let arr = alloc_string_slice(call, &addresses);
            call.ret_ref(slots::RET_0, arr);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_nil(slots::RET_0);
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("net", "lookupIP", std)]
pub fn net_lookup_ip(call: &mut ExternCallContext) -> ExternResult {
    let host = match crate::host_bytes::utf8_arg(call, slots::ARG_HOST, "host name") {
        Ok(host) => host,
        Err(error) => {
            call.ret_nil(slots::RET_0);
            write_error_to(call, slots::RET_1, &error);
            return ExternResult::Ok;
        }
    };

    // Use ToSocketAddrs with port 0 to resolve hostnames
    let lookup_addr = format!("{}:0", host);
    match lookup_addr.to_socket_addrs() {
        Ok(addrs) => {
            let ips: Vec<Vec<u8>> = addrs.map(|a| ip_bytes(a.ip())).collect();

            let arr = alloc_ip_slice(call, &ips);
            call.ret_ref(slots::RET_0, arr);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_nil(slots::RET_0);
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("net", "lookupAddr", std)]
pub fn net_lookup_addr(call: &mut ExternCallContext) -> ExternResult {
    let addr = match crate::host_bytes::utf8_arg(call, slots::ARG_ADDR, "IP address") {
        Ok(addr) => addr,
        Err(error) => {
            call.ret_nil(slots::RET_0);
            write_error_to(call, slots::RET_1, &error);
            return ExternResult::Ok;
        }
    };

    // Reverse DNS lookup via libc
    match reverse_lookup(&addr) {
        Ok(names) => {
            let arr = alloc_string_slice(call, &names);
            call.ret_ref(slots::RET_0, arr);
            write_nil_error(call, slots::RET_1);
        }
        Err(msg) => {
            call.ret_nil(slots::RET_0);
            write_error_to(call, slots::RET_1, &msg);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("net", "resolveTCPAddr", std)]
pub fn net_resolve_tcp_addr(call: &mut ExternCallContext) -> ExternResult {
    let network = match crate::host_bytes::utf8_arg(call, slots::ARG_NETWORK, "network") {
        Ok(network) => network,
        Err(error) => {
            call.ret_nil(slots::RET_0);
            write_error_to(call, slots::RET_1, &error);
            return ExternResult::Ok;
        }
    };
    let address = match crate::host_bytes::utf8_arg(call, slots::ARG_ADDRESS, "network address") {
        Ok(address) => address,
        Err(error) => {
            call.ret_nil(slots::RET_0);
            write_error_to(call, slots::RET_1, &error);
            return ExternResult::Ok;
        }
    };

    if !matches!(network.as_str(), "tcp" | "tcp4" | "tcp6") {
        call.ret_nil(slots::RET_0);
        write_error_to(call, slots::RET_1, &format!("unknown network: {}", network));
        return ExternResult::Ok;
    }

    match address.to_socket_addrs() {
        Ok(mut addrs) => {
            if let Some(addr) = addrs.next() {
                let tcp_addr = alloc_tcp_addr(call, &addr);
                call.ret_ref(slots::RET_0, tcp_addr);
                write_nil_error(call, slots::RET_1);
            } else {
                call.ret_nil(slots::RET_0);
                write_error_to(call, slots::RET_1, "no addresses");
            }
        }
        Err(e) => {
            call.ret_nil(slots::RET_0);
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("net", "resolveUDPAddr", std)]
pub fn net_resolve_udp_addr(call: &mut ExternCallContext) -> ExternResult {
    let network = match crate::host_bytes::utf8_arg(call, slots::ARG_NETWORK, "network") {
        Ok(network) => network,
        Err(error) => {
            call.ret_nil(slots::RET_0);
            write_error_to(call, slots::RET_1, &error);
            return ExternResult::Ok;
        }
    };
    let address = match crate::host_bytes::utf8_arg(call, slots::ARG_ADDRESS, "network address") {
        Ok(address) => address,
        Err(error) => {
            call.ret_nil(slots::RET_0);
            write_error_to(call, slots::RET_1, &error);
            return ExternResult::Ok;
        }
    };

    if !matches!(network.as_str(), "udp" | "udp4" | "udp6") {
        call.ret_nil(slots::RET_0);
        write_error_to(call, slots::RET_1, &format!("unknown network: {}", network));
        return ExternResult::Ok;
    }

    match address.to_socket_addrs() {
        Ok(mut addrs) => {
            if let Some(addr) = addrs.next() {
                let udp_addr = alloc_udp_addr(call, &addr);
                call.ret_ref(slots::RET_0, udp_addr);
                write_nil_error(call, slots::RET_1);
            } else {
                call.ret_nil(slots::RET_0);
                write_error_to(call, slots::RET_1, "no addresses");
            }
        }
        Err(e) => {
            call.ret_nil(slots::RET_0);
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[cfg(unix)]
fn reverse_lookup(addr: &str) -> Result<Vec<String>, String> {
    use std::net::IpAddr;

    let ip: IpAddr = addr
        .parse()
        .map_err(|_| format!("invalid IP address: {}", addr))?;

    // Use libc getnameinfo for reverse DNS
    use libc::{getnameinfo, sockaddr_in, sockaddr_in6, AF_INET, AF_INET6, NI_NAMEREQD};
    use std::ffi::CStr;
    use std::mem::zeroed;

    let mut host = [0u8; 256];

    let result = unsafe {
        match ip {
            IpAddr::V4(v4) => {
                let mut sa: sockaddr_in = zeroed();
                sa.sin_family = AF_INET as libc::sa_family_t;
                sa.sin_addr.s_addr = u32::from_ne_bytes(v4.octets());
                getnameinfo(
                    &sa as *const sockaddr_in as *const _,
                    std::mem::size_of::<sockaddr_in>() as u32,
                    host.as_mut_ptr() as *mut _,
                    host.len() as u32,
                    std::ptr::null_mut(),
                    0,
                    NI_NAMEREQD,
                )
            }
            IpAddr::V6(v6) => {
                let mut sa: sockaddr_in6 = zeroed();
                sa.sin6_family = AF_INET6 as libc::sa_family_t;
                sa.sin6_addr.s6_addr = v6.octets();
                getnameinfo(
                    &sa as *const sockaddr_in6 as *const _,
                    std::mem::size_of::<sockaddr_in6>() as u32,
                    host.as_mut_ptr() as *mut _,
                    host.len() as u32,
                    std::ptr::null_mut(),
                    0,
                    NI_NAMEREQD,
                )
            }
        }
    };

    if result == 0 {
        let name = unsafe { CStr::from_ptr(host.as_ptr() as *const _) }
            .to_str()
            .map_err(|error| format!("reverse DNS name is not valid UTF-8: {error}"))?
            .to_owned();
        Ok(vec![name])
    } else {
        Err(format!("lookup {} failed", addr))
    }
}

#[cfg(not(unix))]
fn reverse_lookup(addr: &str) -> Result<Vec<String>, String> {
    addr.parse::<std::net::IpAddr>()
        .map_err(|_| format!("invalid IP address: {addr}"))?;
    Err("reverse lookup not supported on this platform".to_string())
}

fn alloc_string_slice(call: &mut ExternCallContext, strings: &[String]) -> GcRef {
    let len = strings.len();
    let elem_meta = ValueMeta::new(0, ValueKind::String);
    let elem_bytes = 8; // GcRef is 8 bytes
    let slice_ref = slice::create(call.gc(), elem_meta, elem_bytes, len, len);

    for (i, s) in strings.iter().enumerate() {
        let str_ref = call.alloc_str(s);
        unsafe { slice::set(slice_ref, i, str_ref as u64, elem_bytes) };
    }

    call.gc()
        .mark_allocated_for_scan(unsafe { vo_runtime::objects::slice::owner_ref(slice_ref) });
    slice_ref
}

fn alloc_ip_slice(call: &mut ExternCallContext, ips: &[Vec<u8>]) -> GcRef {
    let len = ips.len();
    // IP is []byte, which is a slice
    let elem_meta = ValueMeta::new(0, ValueKind::Slice);
    let elem_bytes = 8; // GcRef is 8 bytes
    let slice_ref = slice::create(call.gc(), elem_meta, elem_bytes, len, len);

    for (i, ip_bytes) in ips.iter().enumerate() {
        let ip_slice = alloc_bytes(call.gc(), ip_bytes);
        unsafe { slice::set(slice_ref, i, ip_slice as u64, elem_bytes) };
    }

    call.gc()
        .mark_allocated_for_scan(unsafe { vo_runtime::objects::slice::owner_ref(slice_ref) });
    slice_ref
}

fn alloc_bytes(gc: &mut Gc, bytes: &[u8]) -> GcRef {
    let len = bytes.len();
    let elem_meta = ValueMeta::new(0, ValueKind::Uint8);
    let elem_bytes = 1;
    let slice_ref = slice::create(gc, elem_meta, elem_bytes, len, len);
    // Safety: `slice_ref` is freshly allocated above.
    let data_ptr = unsafe { slice::data_ptr(slice_ref) };
    unsafe {
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), data_ptr, len);
    }
    slice_ref
}

fn alloc_socket_addr(call: &mut ExternCallContext, addr: &std::net::SocketAddr) -> GcRef {
    // TCPAddr/UDPAddr share layout: { IP []byte, Port int, Zone string }
    let struct_ref = call.gc_alloc_struct("net.TCPAddr");

    let ip_bytes = ip_bytes(addr.ip());

    let ip_slice = alloc_bytes(call.gc(), &ip_bytes);
    let zone = call.alloc_str("");

    unsafe {
        Gc::write_slot(struct_ref, 0, ip_slice as u64);
        Gc::write_slot(struct_ref, 1, addr.port() as u64);
        Gc::write_slot(struct_ref, 2, zone as u64);
    }
    call.gc().mark_allocated_for_scan(struct_ref);

    struct_ref
}

/// `net.IP` represents IPv4 results in the same 16-byte IPv4-mapped form as
/// `net.ParseIP` and Go's resolver APIs. Keeping every producer canonical
/// prevents address classification from depending on how an IP was obtained.
fn ip_bytes(ip: std::net::IpAddr) -> Vec<u8> {
    match ip {
        std::net::IpAddr::V4(v4) => {
            let mut bytes = vec![0; 16];
            bytes[10] = 0xff;
            bytes[11] = 0xff;
            bytes[12..].copy_from_slice(&v4.octets());
            bytes
        }
        std::net::IpAddr::V6(v6) => v6.octets().to_vec(),
    }
}

fn alloc_tcp_addr(call: &mut ExternCallContext, addr: &std::net::SocketAddr) -> GcRef {
    alloc_socket_addr(call, addr)
}

fn alloc_udp_addr(call: &mut ExternCallContext, addr: &std::net::SocketAddr) -> GcRef {
    alloc_socket_addr(call, addr)
}
