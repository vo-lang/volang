//! DNS lookup implementations using system resolver.

use std::net::ToSocketAddrs;

use vo_ffi_macro::vostd_extern_ctx;
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::slice;
use vo_common_core::types::{ValueKind, ValueMeta};
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};

use super::write_io_error;

#[vostd_extern_ctx("net", "lookupHost")]
pub fn net_lookup_host(call: &mut ExternCallContext) -> ExternResult {
    let host = call.arg_str(slots::ARG_HOST);
    
    // Use ToSocketAddrs with port 0 to resolve hostnames
    let lookup_addr = format!("{}:0", host);
    match lookup_addr.to_socket_addrs() {
        Ok(addrs) => {
            let addresses: Vec<String> = addrs
                .map(|a| a.ip().to_string())
                .collect();
            
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

#[vostd_extern_ctx("net", "lookupIP")]
pub fn net_lookup_ip(call: &mut ExternCallContext) -> ExternResult {
    let host = call.arg_str(slots::ARG_HOST);
    
    // Use ToSocketAddrs with port 0 to resolve hostnames
    let lookup_addr = format!("{}:0", host);
    match lookup_addr.to_socket_addrs() {
        Ok(addrs) => {
            let ips: Vec<Vec<u8>> = addrs
                .map(|a| match a.ip() {
                    std::net::IpAddr::V4(v4) => v4.octets().to_vec(),
                    std::net::IpAddr::V6(v6) => v6.octets().to_vec(),
                })
                .collect();
            
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

#[vostd_extern_ctx("net", "lookupAddr")]
pub fn net_lookup_addr(call: &mut ExternCallContext) -> ExternResult {
    let addr = call.arg_str(slots::ARG_ADDR);
    
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

#[vostd_extern_ctx("net", "resolveTCPAddr")]
pub fn net_resolve_tcp_addr(call: &mut ExternCallContext) -> ExternResult {
    let network = call.arg_str(slots::ARG_NETWORK).to_string();
    let address = call.arg_str(slots::ARG_ADDRESS).to_string();
    
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

#[vostd_extern_ctx("net", "resolveUDPAddr")]
pub fn net_resolve_udp_addr(call: &mut ExternCallContext) -> ExternResult {
    let network = call.arg_str(slots::ARG_NETWORK).to_string();
    let address = call.arg_str(slots::ARG_ADDRESS).to_string();
    
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

fn reverse_lookup(addr: &str) -> Result<Vec<String>, String> {
    use std::net::IpAddr;
    
    let ip: IpAddr = addr.parse().map_err(|_| format!("invalid IP address: {}", addr))?;
    
    // Use libc getnameinfo for reverse DNS
    #[cfg(unix)]
    {
        use libc::{sockaddr_in, sockaddr_in6, getnameinfo, NI_NAMEREQD, AF_INET, AF_INET6};
        use std::mem::zeroed;
        use std::ffi::CStr;
        
        let mut host = [0u8; 256];
        
        let result = unsafe {
            match ip {
                IpAddr::V4(v4) => {
                    let mut sa: sockaddr_in = zeroed();
                    sa.sin_family = AF_INET as u16;
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
                    sa.sin6_family = AF_INET6 as u16;
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
                .to_string_lossy()
                .into_owned();
            Ok(vec![name])
        } else {
            Err(format!("lookup {} failed", addr))
        }
    }
    
    #[cfg(not(unix))]
    {
        Err("reverse lookup not supported on this platform".to_string())
    }
}

fn alloc_string_slice(call: &mut ExternCallContext, strings: &[String]) -> GcRef {
    let len = strings.len();
    let elem_meta = ValueMeta::new(0, ValueKind::String);
    let elem_bytes = 8; // GcRef is 8 bytes
    let slice_ref = slice::create(call.gc(), elem_meta, elem_bytes, len, len);
    
    for (i, s) in strings.iter().enumerate() {
        let str_ref = call.alloc_str(s);
        slice::set(slice_ref, i, str_ref as u64, elem_bytes);
    }
    
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
        slice::set(slice_ref, i, ip_slice as u64, elem_bytes);
    }
    
    slice_ref
}

fn alloc_bytes(gc: &mut Gc, bytes: &[u8]) -> GcRef {
    let len = bytes.len();
    let elem_meta = ValueMeta::new(0, ValueKind::Uint8);
    let elem_bytes = 1;
    let slice_ref = slice::create(gc, elem_meta, elem_bytes, len, len);
    let data_ptr = slice::data_ptr(slice_ref);
    unsafe {
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), data_ptr, len);
    }
    slice_ref
}

fn alloc_socket_addr(call: &mut ExternCallContext, addr: &std::net::SocketAddr) -> GcRef {
    // TCPAddr/UDPAddr struct: { IP []byte, Port int, Zone string }
    // 3 slots total (IP is slice = 1 slot ref, Port = 1 slot, Zone = 1 slot ref)
    let struct_ref = call.gc_alloc(3, &[]);
    
    let ip_bytes = match addr.ip() {
        std::net::IpAddr::V4(v4) => v4.octets().to_vec(),
        std::net::IpAddr::V6(v6) => v6.octets().to_vec(),
    };
    
    let ip_slice = alloc_bytes(call.gc(), &ip_bytes);
    let zone = call.alloc_str("");
    
    unsafe {
        Gc::write_slot(struct_ref, 0, ip_slice as u64);
        Gc::write_slot(struct_ref, 1, addr.port() as u64);
        Gc::write_slot(struct_ref, 2, zone as u64);
    }
    
    struct_ref
}

fn alloc_tcp_addr(call: &mut ExternCallContext, addr: &std::net::SocketAddr) -> GcRef {
    alloc_socket_addr(call, addr)
}

fn alloc_udp_addr(call: &mut ExternCallContext, addr: &std::net::SocketAddr) -> GcRef {
    alloc_socket_addr(call, addr)
}
