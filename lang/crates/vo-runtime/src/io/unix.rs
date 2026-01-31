//! Unix I/O driver using epoll (Linux) or kqueue (BSD/macOS).
//!
//! Key design:
//! - Each fd can have one pending read AND one pending write simultaneously
//! - Driver handles the readiness -> completion translation internally
//! - poll() returns completed tokens with actual results

use std::collections::HashMap;
use std::io;

use super::{Completion, CompletionData, IoHandle, OpKind, PendingOp, SubmitResult};

/// Tracks pending operations for a single fd.
#[derive(Debug, Default)]
struct FdState {
    read: Option<PendingOp>,
    write: Option<PendingOp>,
}

/// Unix I/O driver.
#[derive(Debug)]
pub struct UnixDriver {
    fd_states: HashMap<i32, FdState>,

    #[cfg(target_os = "linux")]
    epoll_fd: i32,

    #[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd", target_os = "netbsd"))]
    kqueue_fd: i32,
}

impl UnixDriver {
    pub fn new() -> io::Result<Self> {
        #[cfg(target_os = "linux")]
        {
            let epoll_fd = unsafe { libc::epoll_create1(libc::EPOLL_CLOEXEC) };
            if epoll_fd < 0 {
                return Err(io::Error::last_os_error());
            }
            return Ok(Self {
                fd_states: HashMap::new(),
                epoll_fd,
            });
        }

        #[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd", target_os = "netbsd"))]
        {
            let kqueue_fd = unsafe { libc::kqueue() };
            if kqueue_fd < 0 {
                return Err(io::Error::last_os_error());
            }
            return Ok(Self {
                fd_states: HashMap::new(),
                kqueue_fd,
            });
        }

        #[cfg(not(any(
            target_os = "linux",
            target_os = "macos",
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "netbsd"
        )))]
        {
            Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "I/O driver not supported on this platform",
            ))
        }
    }

    /// Submit an operation. Returns Completed if it finished immediately, Pending otherwise.
    pub fn submit(&mut self, op: PendingOp) -> SubmitResult {
        let fd = op.handle as i32;
        let is_read = matches!(op.kind, OpKind::Read | OpKind::Accept | OpKind::Connect | OpKind::RecvFrom);

        // Check for concurrent operation on same direction
        if let Some(state) = self.fd_states.get(&fd) {
            if is_read && state.read.is_some() {
                panic!("concurrent read operation on fd {}", fd);
            }
            if !is_read && state.write.is_some() {
                panic!("concurrent write operation on fd {}", fd);
            }
        }

        // Try to complete immediately
        match try_complete(&op) {
            TryResult::Done(data) => {
                return SubmitResult::Completed(Completion {
                    token: op.token,
                    result: Ok(data),
                });
            }
            TryResult::Error(e) => {
                return SubmitResult::Completed(Completion {
                    token: op.token,
                    result: Err(e),
                });
            }
            TryResult::WouldBlock => {}
        }

        // Store the operation and register with poller
        let state = self.fd_states.entry(fd).or_default();
        if is_read {
            state.read = Some(op);
        } else {
            state.write = Some(op);
        }
        self.update_registration(fd);

        SubmitResult::Pending
    }

    /// Poll for completed operations. Returns completions.
    pub fn poll(&mut self) -> Vec<Completion> {
        let ready_fds = self.poll_ready();
        let mut completed = Vec::new();
        let mut fds_to_update = Vec::new();
        let mut fds_to_remove = Vec::new();

        for (fd, readable, writable) in ready_fds {
            let state = match self.fd_states.get_mut(&fd) {
                Some(s) => s,
                None => continue,
            };

            // Handle read completion
            if readable {
                if let Some(op) = state.read.take() {
                    match try_complete(&op) {
                        TryResult::Done(data) => {
                            completed.push(Completion { token: op.token, result: Ok(data) });
                        }
                        TryResult::Error(e) => {
                            completed.push(Completion { token: op.token, result: Err(e) });
                        }
                        TryResult::WouldBlock => {
                            state.read = Some(op);
                        }
                    }
                }
            }

            // Handle write completion
            if writable {
                if let Some(op) = state.write.take() {
                    match try_complete(&op) {
                        TryResult::Done(data) => {
                            completed.push(Completion { token: op.token, result: Ok(data) });
                        }
                        TryResult::Error(e) => {
                            completed.push(Completion { token: op.token, result: Err(e) });
                        }
                        TryResult::WouldBlock => {
                            state.write = Some(op);
                        }
                    }
                }
            }

            // Track fds that need registration update or removal
            if state.read.is_none() && state.write.is_none() {
                fds_to_remove.push(fd);
            } else {
                fds_to_update.push(fd);
            }
        }

        // Update registrations after releasing fd_states borrows
        for fd in fds_to_update {
            self.update_registration(fd);
        }
        for fd in fds_to_remove {
            self.fd_states.remove(&fd);
            self.unregister_fd(fd);
        }

        // Check for closed fds (important for handling Close() on listeners)
        let fds: Vec<i32> = self.fd_states.keys().copied().collect();
        for fd in fds {
            let ret = unsafe { libc::fcntl(fd, libc::F_GETFD) };
            if ret == -1 && io::Error::last_os_error().raw_os_error() == Some(libc::EBADF) {
                if let Some(state) = self.fd_states.remove(&fd) {
                    if let Some(op) = state.read {
                        completed.push(Completion {
                            token: op.token,
                            result: Err(io::Error::from_raw_os_error(libc::EBADF)),
                        });
                    }
                    if let Some(op) = state.write {
                        completed.push(Completion {
                            token: op.token,
                            result: Err(io::Error::from_raw_os_error(libc::EBADF)),
                        });
                    }
                }
            }
        }

        completed
    }

    /// Cancel all operations on a handle.
    pub fn cancel(&mut self, handle: IoHandle) {
        let fd = handle as i32;
        self.fd_states.remove(&fd);
        self.unregister_fd(fd);
    }

    pub fn has_pending(&self) -> bool {
        !self.fd_states.is_empty()
    }

    fn poll_ready(&mut self) -> Vec<(i32, bool, bool)> {
        let mut ready = Vec::new();

        #[cfg(target_os = "linux")]
        {
            let mut events: [libc::epoll_event; 64] = unsafe { std::mem::zeroed() };
            let n = unsafe { libc::epoll_wait(self.epoll_fd, events.as_mut_ptr(), 64, 0) };
            if n > 0 {
                for i in 0..n as usize {
                    let fd = events[i].u64 as i32;
                    let ev = events[i].events;
                    let readable = (ev & libc::EPOLLIN as u32) != 0 || (ev & libc::EPOLLERR as u32) != 0;
                    let writable = (ev & libc::EPOLLOUT as u32) != 0 || (ev & libc::EPOLLERR as u32) != 0;
                    ready.push((fd, readable, writable));
                }
            }
        }

        #[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd", target_os = "netbsd"))]
        {
            let mut events: [libc::kevent; 64] = unsafe { std::mem::zeroed() };
            let timeout = libc::timespec { tv_sec: 0, tv_nsec: 0 };
            let n = unsafe {
                libc::kevent(
                    self.kqueue_fd,
                    std::ptr::null(),
                    0,
                    events.as_mut_ptr(),
                    64,
                    &timeout,
                )
            };
            if n > 0 {
                // kqueue returns separate events for read/write, need to merge
                let mut fd_events: HashMap<i32, (bool, bool)> = HashMap::new();
                for i in 0..n as usize {
                    let fd = events[i].ident as i32;
                    let entry = fd_events.entry(fd).or_insert((false, false));
                    if events[i].filter == libc::EVFILT_READ {
                        entry.0 = true;
                    }
                    if events[i].filter == libc::EVFILT_WRITE {
                        entry.1 = true;
                    }
                }
                for (fd, (r, w)) in fd_events {
                    ready.push((fd, r, w));
                }
            }
        }

        ready
    }

    fn update_registration(&mut self, fd: i32) {
        let state = match self.fd_states.get(&fd) {
            Some(s) => s,
            None => {
                self.unregister_fd(fd);
                return;
            }
        };

        let want_read = state.read.is_some();
        let want_write = state.write.is_some();

        if !want_read && !want_write {
            self.unregister_fd(fd);
            return;
        }

        #[cfg(target_os = "linux")]
        {
            let mut events = libc::EPOLLONESHOT as u32;
            if want_read {
                events |= libc::EPOLLIN as u32;
            }
            if want_write {
                events |= libc::EPOLLOUT as u32;
            }
            
            let mut event = libc::epoll_event {
                events,
                u64: fd as u64,
            };

            // Try ADD first (most common case for new fd)
            let ret = unsafe { libc::epoll_ctl(self.epoll_fd, libc::EPOLL_CTL_ADD, fd, &mut event) };
            if ret < 0 {
                let err = io::Error::last_os_error();
                if err.raw_os_error() == Some(libc::EEXIST) {
                    // Already registered, modify it
                    unsafe { libc::epoll_ctl(self.epoll_fd, libc::EPOLL_CTL_MOD, fd, &mut event) };
                }
            }
        }

        #[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd", target_os = "netbsd"))]
        {
            self.kqueue_update_filter(fd, libc::EVFILT_READ, want_read);
            self.kqueue_update_filter(fd, libc::EVFILT_WRITE, want_write);
        }
    }

    #[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd", target_os = "netbsd"))]
    fn kqueue_update_filter(&self, fd: i32, filter: i16, enable: bool) {
        let flags = if enable { libc::EV_ADD | libc::EV_ONESHOT } else { libc::EV_DELETE };
        let event = libc::kevent {
            ident: fd as usize,
            filter,
            flags,
            fflags: 0,
            data: 0,
            udata: std::ptr::null_mut(),
        };
        unsafe {
            libc::kevent(self.kqueue_fd, &event, 1, std::ptr::null_mut(), 0, std::ptr::null());
        }
    }

    fn unregister_fd(&mut self, fd: i32) {
        #[cfg(target_os = "linux")]
        unsafe {
            libc::epoll_ctl(self.epoll_fd, libc::EPOLL_CTL_DEL, fd, std::ptr::null_mut());
        }

        #[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd", target_os = "netbsd"))]
        {
            self.kqueue_update_filter(fd, libc::EVFILT_READ, false);
            self.kqueue_update_filter(fd, libc::EVFILT_WRITE, false);
        }
    }
}

impl Drop for UnixDriver {
    fn drop(&mut self) {
        #[cfg(target_os = "linux")]
        unsafe {
            libc::close(self.epoll_fd);
        }

        #[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd", target_os = "netbsd"))]
        unsafe {
            libc::close(self.kqueue_fd);
        }
    }
}

enum TryResult {
    Done(CompletionData),
    Error(io::Error),
    WouldBlock,
}

fn try_complete(op: &PendingOp) -> TryResult {
    let fd = op.handle as i32;

    match op.kind {
        OpKind::Read => {
            let n = if op.offset >= 0 {
                unsafe {
                    libc::pread(
                        fd,
                        op.buf_ptr as *mut core::ffi::c_void,
                        op.buf_len,
                        op.offset,
                    )
                }
            } else {
                unsafe { libc::read(fd, op.buf_ptr as *mut core::ffi::c_void, op.buf_len) }
            };

            if n >= 0 {
                TryResult::Done(CompletionData::Size(n as usize))
            } else {
                let e = io::Error::last_os_error();
                if e.kind() == io::ErrorKind::WouldBlock {
                    TryResult::WouldBlock
                } else {
                    TryResult::Error(e)
                }
            }
        }

        OpKind::Write => {
            let n = if op.offset >= 0 {
                unsafe {
                    libc::pwrite(
                        fd,
                        op.buf_ptr as *const core::ffi::c_void,
                        op.buf_len,
                        op.offset,
                    )
                }
            } else {
                unsafe { libc::write(fd, op.buf_ptr as *const core::ffi::c_void, op.buf_len) }
            };

            if n >= 0 {
                TryResult::Done(CompletionData::Size(n as usize))
            } else {
                let e = io::Error::last_os_error();
                if e.kind() == io::ErrorKind::WouldBlock {
                    TryResult::WouldBlock
                } else {
                    TryResult::Error(e)
                }
            }
        }

        OpKind::Accept => {
            let nfd = unsafe { libc::accept(fd, core::ptr::null_mut(), core::ptr::null_mut()) };
            if nfd >= 0 {
                // Set non-blocking
                let flags = unsafe { libc::fcntl(nfd, libc::F_GETFL) };
                if flags != -1 {
                    let _ = unsafe { libc::fcntl(nfd, libc::F_SETFL, flags | libc::O_NONBLOCK) };
                }
                TryResult::Done(CompletionData::Accept(nfd as IoHandle))
            } else {
                let e = io::Error::last_os_error();
                if e.kind() == io::ErrorKind::WouldBlock {
                    TryResult::WouldBlock
                } else {
                    TryResult::Error(e)
                }
            }
        }

        OpKind::Connect => {
            // Connect is special: we check socket error via getsockopt
            let mut err: libc::c_int = 0;
            let mut len: libc::socklen_t = std::mem::size_of::<libc::c_int>() as libc::socklen_t;
            let ret = unsafe {
                libc::getsockopt(
                    fd,
                    libc::SOL_SOCKET,
                    libc::SO_ERROR,
                    &mut err as *mut _ as *mut core::ffi::c_void,
                    &mut len,
                )
            };

            if ret < 0 {
                TryResult::Error(io::Error::last_os_error())
            } else if err != 0 {
                TryResult::Error(io::Error::from_raw_os_error(err))
            } else {
                TryResult::Done(CompletionData::Connect)
            }
        }

        OpKind::RecvFrom => {
            let mut addr: libc::sockaddr_storage = unsafe { std::mem::zeroed() };
            let mut addr_len: libc::socklen_t = std::mem::size_of::<libc::sockaddr_storage>() as libc::socklen_t;
            let n = unsafe {
                libc::recvfrom(
                    fd,
                    op.buf_ptr as *mut core::ffi::c_void,
                    op.buf_len,
                    0,
                    &mut addr as *mut _ as *mut libc::sockaddr,
                    &mut addr_len,
                )
            };

            if n >= 0 {
                let socket_addr = sockaddr_to_std(&addr, addr_len);
                TryResult::Done(CompletionData::RecvFrom(n as usize, socket_addr))
            } else {
                let e = io::Error::last_os_error();
                if e.kind() == io::ErrorKind::WouldBlock {
                    TryResult::WouldBlock
                } else {
                    TryResult::Error(e)
                }
            }
        }

        OpKind::SendTo => {
            let addr = op.dest_addr.as_ref().expect("SendTo requires dest_addr");
            let (sockaddr, socklen) = std_to_sockaddr(addr);
            let n = unsafe {
                libc::sendto(
                    fd,
                    op.buf_ptr as *const core::ffi::c_void,
                    op.buf_len,
                    0,
                    &sockaddr as *const _ as *const libc::sockaddr,
                    socklen,
                )
            };

            if n >= 0 {
                TryResult::Done(CompletionData::Size(n as usize))
            } else {
                let e = io::Error::last_os_error();
                if e.kind() == io::ErrorKind::WouldBlock {
                    TryResult::WouldBlock
                } else {
                    TryResult::Error(e)
                }
            }
        }
    }
}

fn sockaddr_to_std(addr: &libc::sockaddr_storage, len: libc::socklen_t) -> std::net::SocketAddr {
    use std::net::{Ipv4Addr, Ipv6Addr, SocketAddr, SocketAddrV4, SocketAddrV6};
    
    unsafe {
        if addr.ss_family as i32 == libc::AF_INET && len as usize >= std::mem::size_of::<libc::sockaddr_in>() {
            let addr_in = &*(addr as *const _ as *const libc::sockaddr_in);
            let ip = Ipv4Addr::from(u32::from_be(addr_in.sin_addr.s_addr));
            let port = u16::from_be(addr_in.sin_port);
            SocketAddr::V4(SocketAddrV4::new(ip, port))
        } else if addr.ss_family as i32 == libc::AF_INET6 && len as usize >= std::mem::size_of::<libc::sockaddr_in6>() {
            let addr_in6 = &*(addr as *const _ as *const libc::sockaddr_in6);
            let ip = Ipv6Addr::from(addr_in6.sin6_addr.s6_addr);
            let port = u16::from_be(addr_in6.sin6_port);
            SocketAddr::V6(SocketAddrV6::new(ip, port, 0, 0))
        } else {
            // Fallback to 0.0.0.0:0
            SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::UNSPECIFIED, 0))
        }
    }
}

fn std_to_sockaddr(addr: &std::net::SocketAddr) -> (libc::sockaddr_storage, libc::socklen_t) {
    use std::net::SocketAddr;
    
    let mut storage: libc::sockaddr_storage = unsafe { std::mem::zeroed() };
    
    match addr {
        SocketAddr::V4(v4) => {
            let addr_in = unsafe { &mut *(&mut storage as *mut _ as *mut libc::sockaddr_in) };
            addr_in.sin_family = libc::AF_INET as libc::sa_family_t;
            addr_in.sin_port = v4.port().to_be();
            addr_in.sin_addr.s_addr = u32::from(*v4.ip()).to_be();
            (storage, std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t)
        }
        SocketAddr::V6(v6) => {
            let addr_in6 = unsafe { &mut *(&mut storage as *mut _ as *mut libc::sockaddr_in6) };
            addr_in6.sin6_family = libc::AF_INET6 as libc::sa_family_t;
            addr_in6.sin6_port = v6.port().to_be();
            addr_in6.sin6_addr.s6_addr = v6.ip().octets();
            (storage, std::mem::size_of::<libc::sockaddr_in6>() as libc::socklen_t)
        }
    }
}
