#![cfg(feature = "std")]

use std::collections::HashMap;
use std::io;

pub type IoHandle = u64;
pub type IoToken = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IoKind {
    Read,
    Write,
    Accept,
    Connect,
}

#[derive(Debug)]
pub struct Completion {
    pub token: IoToken,
    pub kind: IoKind,
    pub result: io::Result<usize>,
    pub extra: u64,
}

#[derive(Debug)]
pub struct IoRuntime {
    poller: IoPoller,
    next_token: IoToken,
    pending_ops: HashMap<IoToken, PendingOp>,
    completion_cache: HashMap<IoToken, Completion>,
    handle_to_token: HashMap<IoHandle, IoToken>,
}

#[derive(Debug, Clone, Copy)]
struct PendingOp {
    token: IoToken,
    handle: IoHandle,
    kind: IoKind,
    buf_ptr: usize,
    buf_len: usize,
    offset: i64,
}

impl IoRuntime {
    pub fn new() -> io::Result<Self> {
        Ok(Self {
            poller: IoPoller::new()?,
            next_token: 1,
            pending_ops: HashMap::new(),
            completion_cache: HashMap::new(),
            handle_to_token: HashMap::new(),
        })
    }

    #[inline]
    fn alloc_token(&mut self) -> IoToken {
        let token = self.next_token;
        self.next_token = self.next_token.wrapping_add(1);
        token
    }

    #[inline]
    pub fn try_take_completion(&mut self, token: IoToken) -> Option<Completion> {
        self.completion_cache.remove(&token)
    }

    #[inline]
    pub fn take_completion(&mut self, token: IoToken) -> Completion {
        self.try_take_completion(token)
            .unwrap_or_else(|| panic!("completion not found: token={}", token))
    }

    pub fn submit_read(&mut self, handle: IoHandle, dst: *mut u8, len: usize) -> IoToken {
        self.submit_op(handle, IoKind::Read, dst as usize, len, -1)
    }

    pub fn submit_write(&mut self, handle: IoHandle, src: *const u8, len: usize) -> IoToken {
        self.submit_op(handle, IoKind::Write, src as usize, len, -1)
    }

    pub fn submit_accept(&mut self, handle: IoHandle) -> IoToken {
        self.submit_op(handle, IoKind::Accept, 0, 0, -1)
    }

    pub fn submit_read_at(
        &mut self,
        handle: IoHandle,
        dst: *mut u8,
        len: usize,
        offset: i64,
    ) -> IoToken {
        self.submit_op(handle, IoKind::Read, dst as usize, len, offset)
    }

    pub fn submit_write_at(
        &mut self,
        handle: IoHandle,
        src: *const u8,
        len: usize,
        offset: i64,
    ) -> IoToken {
        self.submit_op(handle, IoKind::Write, src as usize, len, offset)
    }

    fn submit_op(
        &mut self,
        handle: IoHandle,
        kind: IoKind,
        buf_ptr: usize,
        buf_len: usize,
        offset: i64,
    ) -> IoToken {
        if self.handle_to_token.contains_key(&handle) {
            panic!("concurrent io op on same handle is not supported: handle={}", handle);
        }

        let token = self.alloc_token();
        let op = PendingOp {
            token,
            handle,
            kind,
            buf_ptr,
            buf_len,
            offset,
        };

        match self.try_complete_unix(op) {
            TryCompleteResult::Completed(c) => {
                self.completion_cache.insert(token, c);
            }
            TryCompleteResult::WouldBlock(op) => {
                self.pending_ops.insert(token, op);
                self.handle_to_token.insert(handle, token);
                self.poller
                    .register(handle, token, kind)
                    .unwrap_or_else(|e| panic!("IoPoller::register failed: {}", e));
            }
        }

        token
    }

    pub fn cancel_handle(&mut self, handle: IoHandle) {
        if let Some(token) = self.handle_to_token.remove(&handle) {
            self.pending_ops.remove(&token);
            self.completion_cache.remove(&token);
        }
        self.poller.unregister(handle)
    }

    pub fn poll(&mut self) -> Vec<Completion> {
        let events = self.poller.poll();
        let mut completed = Vec::new();

        for event in events {
            let token = event.token;
            let op = self.pending_ops.remove(&token).unwrap_or_else(|| {
                panic!("io poll got token with no pending op: token={}", token)
            });

            let handle = op.handle;
            let kind = op.kind;

            match self.try_complete_unix(op) {
                TryCompleteResult::Completed(c) => {
                    self.handle_to_token.remove(&handle);
                    self.completion_cache.insert(token, c);

                    // Return a lightweight completion to drive wakes. Real completion is cached
                    // and must be consumed by take_completion(token).
                    completed.push(Completion {
                        token,
                        kind,
                        result: Ok(0),
                        extra: handle,
                    });
                }
                TryCompleteResult::WouldBlock(op) => {
                    // Re-register and keep waiting.
                    self.pending_ops.insert(token, op);
                    self.poller
                        .register(op.handle, token, op.kind)
                        .unwrap_or_else(|e| panic!("IoPoller::register failed: {}", e));
                }
            }
        }

        completed
    }

    pub fn has_waiters(&self) -> bool {
        self.poller.has_waiters() || !self.pending_ops.is_empty()
    }

    /// Check if a completion exists for the given token without consuming it.
    #[inline]
    pub fn has_completion(&self, token: IoToken) -> bool {
        self.completion_cache.contains_key(&token)
    }
}

#[derive(Debug)]
enum TryCompleteResult {
    Completed(Completion),
    WouldBlock(PendingOp),
}

impl IoRuntime {
    fn try_complete_unix(&self, op: PendingOp) -> TryCompleteResult {
        #[cfg(unix)]
        {
            return unix_try_complete(op);
        }

        #[cfg(windows)]
        {
            let _ = op;
            unreachable!("submit op completion is windows-only via IOCP backend")
        }

        #[cfg(not(any(unix, windows)))]
        {
            let _ = op;
            unreachable!("unsupported platform")
        }
    }
}

#[cfg(unix)]
fn unix_try_complete(op: PendingOp) -> TryCompleteResult {
    let fd = i32::try_from(op.handle).unwrap_or_else(|_| panic!("invalid unix fd handle"));
    let (res, extra) = match op.kind {
        IoKind::Read => {
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
                (Ok(n as usize), op.handle)
            } else {
                let e = io::Error::last_os_error();
                if e.kind() == io::ErrorKind::WouldBlock {
                    return TryCompleteResult::WouldBlock(op);
                }
                (Err(e), op.handle)
            }
        }
        IoKind::Write => {
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
                (Ok(n as usize), op.handle)
            } else {
                let e = io::Error::last_os_error();
                if e.kind() == io::ErrorKind::WouldBlock {
                    return TryCompleteResult::WouldBlock(op);
                }
                (Err(e), op.handle)
            }
        }
        IoKind::Accept => {
            let nfd = unsafe { libc::accept(fd, core::ptr::null_mut(), core::ptr::null_mut()) };
            if nfd >= 0 {
                // Best-effort set non-blocking.
                let flags = unsafe { libc::fcntl(nfd, libc::F_GETFL) };
                if flags != -1 {
                    let _ = unsafe { libc::fcntl(nfd, libc::F_SETFL, flags | libc::O_NONBLOCK) };
                }
                (Ok(0), nfd as IoHandle)
            } else {
                let e = io::Error::last_os_error();
                if e.kind() == io::ErrorKind::WouldBlock {
                    return TryCompleteResult::WouldBlock(op);
                }
                (Err(e), op.handle)
            }
        }
        IoKind::Connect => {
            panic!("submit_connect not implemented yet")
        }
    };

    TryCompleteResult::Completed(Completion {
        token: op.token,
        kind: op.kind,
        result: res,
        extra,
    })
}

#[derive(Debug)]
pub enum IoPoller {
    Unix(unix::UnixPoller),

    #[cfg(windows)]
    Windows(windows::WindowsPoller),
}

impl IoPoller {
    pub fn new() -> io::Result<Self> {
        #[cfg(unix)]
        {
            return Ok(Self::Unix(unix::UnixPoller::new()?));
        }

        #[cfg(windows)]
        {
            return Ok(Self::Windows(windows::WindowsPoller::new()?));
        }

        #[cfg(not(any(unix, windows)))]
        {
            Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "I/O polling not supported on this platform",
            ))
        }
    }

    pub fn register(&mut self, handle: IoHandle, token: IoToken, kind: IoKind) -> io::Result<()> {
        match self {
            IoPoller::Unix(p) => p.register(handle, token, kind),
            #[cfg(windows)]
            IoPoller::Windows(p) => p.register(handle, token, kind),
        }
    }

    pub fn unregister(&mut self, handle: IoHandle) {
        match self {
            IoPoller::Unix(p) => p.unregister(handle),
            #[cfg(windows)]
            IoPoller::Windows(p) => p.unregister(handle),
        }
    }

    pub fn poll(&mut self) -> Vec<Completion> {
        match self {
            IoPoller::Unix(p) => p.poll(),
            #[cfg(windows)]
            IoPoller::Windows(p) => p.poll(),
        }
    }

    pub fn has_waiters(&self) -> bool {
        match self {
            IoPoller::Unix(p) => p.has_waiters(),
            #[cfg(windows)]
            IoPoller::Windows(p) => p.has_waiters(),
        }
    }
}

#[cfg(unix)]
mod unix;

#[cfg(windows)]
mod windows {
    use super::*;

    #[derive(Debug)]
    pub struct WindowsPoller;

    impl WindowsPoller {
        pub fn new() -> io::Result<Self> {
            Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "windows iocp backend not implemented",
            ))
        }

        pub fn register(&mut self, _handle: IoHandle, _token: IoToken, _kind: IoKind) -> io::Result<()> {
            unreachable!("windows iocp backend not implemented")
        }

        pub fn unregister(&mut self, _handle: IoHandle) {
            unreachable!("windows iocp backend not implemented")
        }

        pub fn poll(&mut self) -> Vec<Completion> {
            unreachable!("windows iocp backend not implemented")
        }

        pub fn has_waiters(&self) -> bool {
            unreachable!("windows iocp backend not implemented")
        }
    }
}
