//! Async I/O runtime with completion-based API.
//!
//! Design principles:
//! - Unified completion-based API hiding readiness vs completion differences
//! - Each fd can have at most one pending read and one pending write simultaneously
//! - Platform-specific drivers (epoll, kqueue, IOCP) implement the same interface

#![cfg(feature = "std")]

use std::collections::HashMap;
use std::io;

pub type IoHandle = u64;
pub type IoToken = u64;

/// Kind of I/O operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpKind {
    Read,
    Write,
    Accept,
    Connect,
    RecvFrom,
    SendTo,
}

/// Result of a completed I/O operation.
#[derive(Debug)]
pub struct Completion {
    pub token: IoToken,
    pub result: io::Result<CompletionData>,
}

/// Data returned by a completed operation.
#[derive(Debug)]
pub enum CompletionData {
    /// Read/Write: bytes transferred
    Size(usize),
    /// Accept: new fd
    Accept(IoHandle),
    /// Connect: success (no additional data)
    Connect,
    /// RecvFrom: bytes received + source address
    RecvFrom(usize, std::net::SocketAddr),
}

/// Pending operation descriptor.
#[derive(Debug, Clone)]
pub(crate) struct PendingOp {
    pub token: IoToken,
    pub handle: IoHandle,
    pub kind: OpKind,
    pub buf_ptr: usize,
    pub buf_len: usize,
    pub offset: i64,
    /// For SendTo: destination address
    pub dest_addr: Option<std::net::SocketAddr>,
}

/// Async I/O runtime.
#[derive(Debug)]
pub struct IoRuntime {
    driver: IoDriver,
    next_token: IoToken,
    completions: HashMap<IoToken, Completion>,
}

impl IoRuntime {
    pub fn new() -> io::Result<Self> {
        Ok(Self {
            driver: IoDriver::new()?,
            next_token: 1,
            completions: HashMap::new(),
        })
    }

    fn alloc_token(&mut self) -> IoToken {
        let token = self.next_token;
        self.next_token = self.next_token.wrapping_add(1);
        token
    }

    /// Submit a read operation.
    pub fn submit_read(&mut self, handle: IoHandle, buf: *mut u8, len: usize) -> IoToken {
        self.submit(handle, OpKind::Read, buf as usize, len, -1, None)
    }

    /// Submit a write operation.
    pub fn submit_write(&mut self, handle: IoHandle, buf: *const u8, len: usize) -> IoToken {
        self.submit(handle, OpKind::Write, buf as usize, len, -1, None)
    }

    /// Submit an accept operation.
    pub fn submit_accept(&mut self, handle: IoHandle) -> IoToken {
        self.submit(handle, OpKind::Accept, 0, 0, -1, None)
    }

    /// Submit a read at offset operation (for files).
    pub fn submit_read_at(&mut self, handle: IoHandle, buf: *mut u8, len: usize, offset: i64) -> IoToken {
        self.submit(handle, OpKind::Read, buf as usize, len, offset, None)
    }

    /// Submit a write at offset operation (for files).
    pub fn submit_write_at(&mut self, handle: IoHandle, buf: *const u8, len: usize, offset: i64) -> IoToken {
        self.submit(handle, OpKind::Write, buf as usize, len, offset, None)
    }

    /// Submit a recvfrom operation (UDP).
    pub fn submit_recv_from(&mut self, handle: IoHandle, buf: *mut u8, len: usize) -> IoToken {
        self.submit(handle, OpKind::RecvFrom, buf as usize, len, -1, None)
    }

    /// Submit a sendto operation (UDP).
    pub fn submit_send_to(&mut self, handle: IoHandle, buf: *const u8, len: usize, addr: std::net::SocketAddr) -> IoToken {
        self.submit(handle, OpKind::SendTo, buf as usize, len, -1, Some(addr))
    }

    fn submit(&mut self, handle: IoHandle, kind: OpKind, buf_ptr: usize, buf_len: usize, offset: i64, dest_addr: Option<std::net::SocketAddr>) -> IoToken {
        let token = self.alloc_token();
        let op = PendingOp { token, handle, kind, buf_ptr, buf_len, offset, dest_addr };

        // Driver handles concurrent op check and registration
        match self.driver.submit(op) {
            SubmitResult::Completed(c) => {
                self.completions.insert(token, c);
            }
            SubmitResult::Pending => {
                // Driver is now tracking the pending op
            }
        }

        token
    }

    /// Try to take a completion without blocking.
    #[inline]
    pub fn try_take_completion(&mut self, token: IoToken) -> Option<Completion> {
        self.completions.remove(&token)
    }

    /// Take a completion, panics if not found.
    #[inline]
    pub fn take_completion(&mut self, token: IoToken) -> Completion {
        self.try_take_completion(token)
            .unwrap_or_else(|| panic!("completion not found: token={}", token))
    }

    /// Check if a completion is ready.
    #[inline]
    pub fn has_completion(&self, token: IoToken) -> bool {
        self.completions.contains_key(&token)
    }

    /// Poll for completed operations (non-blocking).
    /// Returns tokens of newly completed operations.
    pub fn poll(&mut self) -> Vec<IoToken> {
        let driver_completions = self.driver.poll();
        let mut completed_tokens = Vec::with_capacity(driver_completions.len());

        for completion in driver_completions {
            let token = completion.token;
            self.completions.insert(token, completion);
            completed_tokens.push(token);
        }

        completed_tokens
    }

    /// Cancel all pending operations on a handle.
    pub fn cancel(&mut self, handle: IoHandle) {
        self.driver.cancel(handle);
    }

    /// Check if there are any pending operations.
    pub fn has_pending(&self) -> bool {
        self.driver.has_pending()
    }

    // Legacy API compatibility
    pub fn cancel_handle(&mut self, handle: IoHandle) {
        self.cancel(handle);
    }

    pub fn has_waiters(&self) -> bool {
        self.has_pending()
    }
}

/// Result of submitting an operation.
pub(crate) enum SubmitResult {
    /// Operation completed immediately.
    Completed(Completion),
    /// Operation is pending, will complete later via poll().
    Pending,
}

// Platform-specific driver
#[cfg(unix)]
mod unix;

#[cfg(unix)]
use unix::UnixDriver as IoDriver;

#[cfg(windows)]
mod windows;

#[cfg(windows)]
use windows::WindowsDriver as IoDriver;
