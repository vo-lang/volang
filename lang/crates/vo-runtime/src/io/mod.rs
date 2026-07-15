//! Async I/O runtime with completion-based API.
//!
//! Design principles:
//! - Unified completion-based API hiding readiness vs completion differences
//! - Each fd can have at most one pending read and one pending write simultaneously
//! - Platform-specific drivers (epoll, kqueue, IOCP) implement the same interface

#![cfg(feature = "std")]

use std::any::Any;
use std::collections::{hash_map::Entry, HashMap};
use std::fmt;
use std::io;
#[cfg(unix)]
use std::os::fd::{AsFd, AsRawFd, OwnedFd};
#[cfg(unix)]
use std::sync::atomic::AtomicBool;
use std::sync::atomic::{AtomicU64, Ordering};
#[cfg(unix)]
use std::sync::Arc;

use crate::gc::GcRef;
use crate::objects::slice;

pub type IoHandle = u64;
pub type IoToken = u64;

/// Internal cancellation identity. Raw operating-system handles and
/// VM-owned provider resources occupy distinct domains, so equal numeric
/// values can never interfere.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IoCancelKey {
    domain: IoCancelDomain,
    value: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum IoCancelDomain {
    RawHandle,
    VmResource,
}

impl IoCancelKey {
    #[inline]
    const fn raw_handle(handle: IoHandle) -> Self {
        Self {
            domain: IoCancelDomain::RawHandle,
            value: handle,
        }
    }

    #[inline]
    const fn vm_resource(value: u64) -> Self {
        Self {
            domain: IoCancelDomain::VmResource,
            value,
        }
    }
}

// Native providers may keep completion state in process-global registries.
// Tokens therefore have to be unique across every IoRuntime in the process,
// not merely within one VM.
static NEXT_IO_TOKEN: AtomicU64 = AtomicU64::new(1);
static NEXT_IO_RESOURCE_TOKEN: AtomicU64 = AtomicU64::new(1);
static NEXT_IO_CANCEL_HANDLE: AtomicU64 = AtomicU64::new(1);

fn allocate_io_token(counter: &AtomicU64) -> Option<IoToken> {
    counter
        .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |current| {
            if current == 0 {
                None
            } else {
                Some(current.checked_add(1).unwrap_or(0))
            }
        })
        .ok()
}

#[cfg(unix)]
fn allocate_io_cancel_handle(counter: &AtomicU64) -> io::Result<IoHandle> {
    allocate_io_token(counter)
        .ok_or_else(|| io::Error::other("process-wide I/O cancellation key space exhausted"))
}

/// Kind of I/O operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpKind {
    Read,
    Write,
    Accept,
    Connect,
    RecvFrom,
    SendTo,
    Timer,
}

/// Process-unique generation for a native resource owned by one VM. Providers
/// store this alongside their public handle so a stale VM cleanup guard cannot
/// remove a newer resource if a handle ID is ever reused.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IoResourceToken(u64);

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
    /// Accept: uniquely owned accepted fd. Dropping an unconsumed completion
    /// closes it, so VM teardown cannot leak accepted connections.
    #[cfg(unix)]
    Accept(OwnedFd),
    /// Accept: native handle on platforms whose driver does not expose an
    /// owned-fd abstraction yet.
    #[cfg(not(unix))]
    Accept(IoHandle),
    /// Connect: success (no additional data)
    Connect,
    /// RecvFrom: bytes received + source address
    RecvFrom(usize, std::net::SocketAddr),
    /// Timer: timer expired
    Timer,
}

/// Tokenless operation request assembled before process-wide token allocation.
#[derive(Debug)]
struct IoSubmission {
    handle: IoHandle,
    cancel_key: IoCancelKey,
    kind: OpKind,
    buf_ptr: usize,
    buf_len: usize,
    offset: i64,
    dest_addr: Option<std::net::SocketAddr>,
    lease: Option<IoLease>,
}

impl IoSubmission {
    fn into_pending(self, token: IoToken) -> PendingOp {
        PendingOp {
            token,
            handle: self.handle,
            cancel_key: self.cancel_key,
            kind: self.kind,
            buf_ptr: self.buf_ptr,
            buf_len: self.buf_len,
            offset: self.offset,
            dest_addr: self.dest_addr,
            lease: self.lease,
        }
    }
}

/// Pending operation descriptor with its process-unique completion token.
#[derive(Debug)]
pub(crate) struct PendingOp {
    pub token: IoToken,
    /// Actual OS handle used by the driver. For leased Unix I/O this is a dup
    /// owned exclusively by this operation.
    pub handle: IoHandle,
    /// Process-unique source resource key used for concurrency checks and
    /// explicit cancellation. It is deliberately independent of public handle
    /// values and operating-system descriptor numbers.
    pub cancel_key: IoCancelKey,
    pub kind: OpKind,
    pub buf_ptr: usize,
    pub buf_len: usize,
    pub offset: i64,
    /// For SendTo: destination address
    pub dest_addr: Option<std::net::SocketAddr>,
    /// Keeps the actual Unix fd alive until immediate completion, cancellation,
    /// driver completion, or driver drop.
    pub lease: Option<IoLease>,
}

impl PendingOp {
    #[inline]
    pub(crate) fn is_cancelled(&self) -> bool {
        self.lease.as_ref().is_some_and(IoLease::is_cancelled)
    }
}

/// Shared cancellation state for a native Unix resource. Closing the logical
/// resource marks every outstanding lease, including leases owned by another
/// VM's I/O runtime.
#[cfg(unix)]
#[derive(Clone, Debug)]
pub struct IoCancellation {
    cancelled: Arc<AtomicBool>,
    cancel_key: IoCancelKey,
}

#[cfg(unix)]
impl IoCancellation {
    pub fn new() -> io::Result<Self> {
        let cancel_handle = allocate_io_cancel_handle(&NEXT_IO_CANCEL_HANDLE)?;
        Ok(Self {
            cancelled: Arc::new(AtomicBool::new(false)),
            cancel_key: IoCancelKey::vm_resource(cancel_handle),
        })
    }

    #[inline]
    pub fn cancel(&self) {
        self.cancelled.store(true, Ordering::Release);
    }

    #[inline]
    pub fn is_cancelled(&self) -> bool {
        self.cancelled.load(Ordering::Acquire)
    }

    /// Process-unique identity used only inside I/O runtimes. Public file and
    /// network handle spaces may overlap, so they must never be cancellation
    /// identities.
    #[inline]
    pub fn cancel_key(&self) -> IoCancelKey {
        self.cancel_key
    }
}

/// Per-operation owned Unix fd. `try_clone` must run while the source resource
/// is protected by its registry lock, which linearizes lease creation against
/// Close and prevents any submission from observing a reused fd number.
#[derive(Debug)]
pub struct IoLease {
    #[cfg(unix)]
    fd: OwnedFd,
    #[cfg(unix)]
    cancel_key: IoCancelKey,
    #[cfg(unix)]
    cancellation: IoCancellation,
}

#[cfg(unix)]
impl IoLease {
    pub fn try_clone<T: AsFd>(source: &T, cancellation: IoCancellation) -> io::Result<Self> {
        if cancellation.is_cancelled() {
            return Err(closed_resource_error());
        }
        let fd = source.as_fd().try_clone_to_owned()?;
        if cancellation.is_cancelled() {
            return Err(closed_resource_error());
        }
        Ok(Self {
            fd,
            cancel_key: cancellation.cancel_key(),
            cancellation,
        })
    }

    pub fn from_owned(fd: OwnedFd, cancellation: IoCancellation) -> io::Result<Self> {
        if cancellation.is_cancelled() {
            return Err(closed_resource_error());
        }
        Ok(Self {
            fd,
            cancel_key: cancellation.cancel_key(),
            cancellation,
        })
    }

    #[inline]
    pub fn handle(&self) -> IoHandle {
        self.fd.as_raw_fd() as IoHandle
    }

    #[inline]
    pub fn cancel_key(&self) -> IoCancelKey {
        self.cancel_key
    }

    #[inline]
    fn is_cancelled(&self) -> bool {
        self.cancellation.is_cancelled()
    }
}

#[cfg(unix)]
fn closed_resource_error() -> io::Error {
    io::Error::new(
        io::ErrorKind::BrokenPipe,
        "cannot submit I/O after the resource was closed",
    )
}

#[cfg(not(unix))]
impl IoLease {
    #[inline]
    fn is_cancelled(&self) -> bool {
        false
    }
}

/// Async I/O runtime.
#[derive(Debug)]
pub struct IoRuntime {
    driver: Option<IoDriver>,
    completions: HashMap<IoToken, Completion>,
    /// Completions returned synchronously by submission and cancellation paths
    /// outside `driver.poll()` still need one scheduler notification.
    pending_notifications: Vec<IoToken>,
    slice_staging: HashMap<IoToken, StagedSliceIo>,
    /// Stable-index root slots used by the VM's bounded root scanner. Holes
    /// are reused, so storage is bounded by peak concurrent write-back reads.
    staged_gc_roots: Vec<Option<GcRef>>,
    free_staged_gc_root_slots: Vec<usize>,
    token_attachments: HashMap<IoToken, TokenAttachment>,
    resource_cleanups: HashMap<IoResourceToken, ResourceCleanup>,
}

#[derive(Debug)]
struct StagedSliceIo {
    bytes: Box<[u8]>,
    write_back: Option<GcRef>,
    gc_root_slot: Option<usize>,
}

struct TokenAttachment(Box<dyn Any + Send>);

impl fmt::Debug for TokenAttachment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("TokenAttachment(..)")
    }
}

struct ResourceCleanup(Option<Box<dyn FnOnce() + Send>>);

impl fmt::Debug for ResourceCleanup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("ResourceCleanup(..)")
    }
}

impl Drop for ResourceCleanup {
    fn drop(&mut self) {
        if let Some(cleanup) = self.0.take() {
            cleanup();
        }
    }
}

impl IoRuntime {
    pub fn new() -> io::Result<Self> {
        Ok(Self {
            driver: Some(IoDriver::new()?),
            completions: HashMap::new(),
            pending_notifications: Vec::new(),
            slice_staging: HashMap::new(),
            staged_gc_roots: Vec::new(),
            free_staged_gc_root_slots: Vec::new(),
            token_attachments: HashMap::new(),
            resource_cleanups: HashMap::new(),
        })
    }

    /// Permanently stop this runtime and release every VM-owned I/O resource.
    ///
    /// Guest termination is terminal for a VM, so no replacement driver is
    /// allocated here. Dropping the driver first retires pending operations
    /// before their staging buffers and provider attachments are released.
    /// Resource cleanups then cancel process, file, network, and request work
    /// while provider attachments are still available.
    pub fn shutdown(&mut self) {
        drop(self.driver.take());
        self.resource_cleanups.clear();
        self.completions.clear();
        self.pending_notifications.clear();
        self.slice_staging.clear();
        self.staged_gc_roots.clear();
        self.free_staged_gc_root_slots.clear();
        self.token_attachments.clear();
    }

    #[inline]
    fn driver_mut(&mut self) -> io::Result<&mut IoDriver> {
        self.driver
            .as_mut()
            .ok_or_else(|| io::Error::new(io::ErrorKind::BrokenPipe, "I/O runtime is shut down"))
    }

    fn try_alloc_token(&mut self) -> io::Result<IoToken> {
        allocate_io_token(&NEXT_IO_TOKEN)
            .ok_or_else(|| io::Error::other("process-wide I/O token space exhausted"))
    }

    #[cfg(test)]
    fn alloc_token(&mut self) -> IoToken {
        self.try_alloc_token()
            .expect("process-wide I/O token space exhausted")
    }

    /// Submit a read operation.
    pub fn submit_read(&mut self, handle: IoHandle, buf: *mut u8, len: usize) -> IoToken {
        self.submit(handle, OpKind::Read, buf as usize, len, -1, None)
    }

    /// Submit a write operation.
    pub fn submit_write(&mut self, handle: IoHandle, buf: *const u8, len: usize) -> IoToken {
        self.submit(handle, OpKind::Write, buf as usize, len, -1, None)
    }

    /// Submit a read into a logical Vo byte slice. The staging allocation stays
    /// alive across suspension and is copied back when completion is consumed.
    pub fn submit_slice_read(&mut self, handle: IoHandle, target: GcRef) -> IoToken {
        self.submit_slice_io(handle, OpKind::Read, target, -1, None, true)
    }

    #[cfg(unix)]
    pub fn submit_lease_slice_read(&mut self, lease: IoLease, target: GcRef) -> IoToken {
        self.submit_lease_slice_io(lease, OpKind::Read, target, -1, None, true)
    }

    /// Submit a read whose fd and stable buffer are both owned by this runtime.
    /// The buffer remains alive until the completion is consumed or the runtime
    /// is dropped. This is useful for one-byte wake channels whose caller does
    /// not need access to the received byte.
    #[cfg(unix)]
    pub fn submit_lease_owned_read(&mut self, lease: IoLease, len: usize) -> IoToken {
        self.try_submit_lease_owned_read(lease, len)
            .expect("owned leased read submission failed")
    }

    /// Fallible variant of [`IoRuntime::submit_lease_owned_read`] used by
    /// provider wake channels and other production suspension paths.
    #[cfg(unix)]
    pub fn try_submit_lease_owned_read(
        &mut self,
        lease: IoLease,
        len: usize,
    ) -> io::Result<IoToken> {
        let mut bytes = Vec::new();
        bytes.try_reserve_exact(len).map_err(|_| {
            io::Error::other(format!(
                "failed to reserve {len} bytes for owned leased read"
            ))
        })?;
        bytes.resize(len, 0);
        let bytes = bytes.into_boxed_slice();
        let ptr = bytes.as_ptr() as usize;
        self.slice_staging
            .try_reserve(1)
            .map_err(|_| io::Error::other("failed to reserve leased read staging state"))?;
        let token = self.try_submit_leased(lease, OpKind::Read, ptr, len, -1, None)?;
        self.slice_staging.insert(
            token,
            StagedSliceIo {
                bytes,
                write_back: None,
                gc_root_slot: None,
            },
        );
        Ok(token)
    }

    /// Submit a write from a logical Vo byte slice using stable contiguous
    /// staging storage across suspension.
    pub fn submit_slice_write(&mut self, handle: IoHandle, source: GcRef) -> IoToken {
        self.submit_slice_io(handle, OpKind::Write, source, -1, None, false)
    }

    #[cfg(unix)]
    pub fn submit_lease_slice_write(&mut self, lease: IoLease, source: GcRef) -> IoToken {
        self.submit_lease_slice_io(lease, OpKind::Write, source, -1, None, false)
    }

    /// Submit an accept operation.
    pub fn submit_accept(&mut self, handle: IoHandle) -> IoToken {
        self.submit(handle, OpKind::Accept, 0, 0, -1, None)
    }

    #[cfg(unix)]
    pub fn submit_lease_accept(&mut self, lease: IoLease) -> IoToken {
        self.submit_leased(lease, OpKind::Accept, 0, 0, -1, None)
    }

    /// Submit a read at offset operation (for files).
    pub fn submit_read_at(
        &mut self,
        handle: IoHandle,
        buf: *mut u8,
        len: usize,
        offset: i64,
    ) -> IoToken {
        self.submit(handle, OpKind::Read, buf as usize, len, offset, None)
    }

    /// Submit a write at offset operation (for files).
    pub fn submit_write_at(
        &mut self,
        handle: IoHandle,
        buf: *const u8,
        len: usize,
        offset: i64,
    ) -> IoToken {
        self.submit(handle, OpKind::Write, buf as usize, len, offset, None)
    }

    pub fn submit_slice_read_at(
        &mut self,
        handle: IoHandle,
        target: GcRef,
        offset: i64,
    ) -> IoToken {
        self.submit_slice_io(handle, OpKind::Read, target, offset, None, true)
    }

    #[cfg(unix)]
    pub fn submit_lease_slice_read_at(
        &mut self,
        lease: IoLease,
        target: GcRef,
        offset: i64,
    ) -> IoToken {
        self.submit_lease_slice_io(lease, OpKind::Read, target, offset, None, true)
    }

    pub fn submit_slice_write_at(
        &mut self,
        handle: IoHandle,
        source: GcRef,
        offset: i64,
    ) -> IoToken {
        self.submit_slice_io(handle, OpKind::Write, source, offset, None, false)
    }

    #[cfg(unix)]
    pub fn submit_lease_slice_write_at(
        &mut self,
        lease: IoLease,
        source: GcRef,
        offset: i64,
    ) -> IoToken {
        self.submit_lease_slice_io(lease, OpKind::Write, source, offset, None, false)
    }

    /// Submit a recvfrom operation (UDP).
    pub fn submit_recv_from(&mut self, handle: IoHandle, buf: *mut u8, len: usize) -> IoToken {
        self.submit(handle, OpKind::RecvFrom, buf as usize, len, -1, None)
    }

    pub fn submit_slice_recv_from(&mut self, handle: IoHandle, target: GcRef) -> IoToken {
        self.submit_slice_io(handle, OpKind::RecvFrom, target, -1, None, true)
    }

    #[cfg(unix)]
    pub fn submit_lease_slice_recv_from(&mut self, lease: IoLease, target: GcRef) -> IoToken {
        self.submit_lease_slice_io(lease, OpKind::RecvFrom, target, -1, None, true)
    }

    /// Submit a sendto operation (UDP).
    pub fn submit_send_to(
        &mut self,
        handle: IoHandle,
        buf: *const u8,
        len: usize,
        addr: std::net::SocketAddr,
    ) -> IoToken {
        self.submit(handle, OpKind::SendTo, buf as usize, len, -1, Some(addr))
    }

    pub fn submit_slice_send_to(
        &mut self,
        handle: IoHandle,
        source: GcRef,
        addr: std::net::SocketAddr,
    ) -> IoToken {
        self.submit_slice_io(handle, OpKind::SendTo, source, -1, Some(addr), false)
    }

    #[cfg(unix)]
    pub fn submit_lease_slice_send_to(
        &mut self,
        lease: IoLease,
        source: GcRef,
        addr: std::net::SocketAddr,
    ) -> IoToken {
        self.submit_lease_slice_io(lease, OpKind::SendTo, source, -1, Some(addr), false)
    }

    fn submit_slice_io(
        &mut self,
        handle: IoHandle,
        kind: OpKind,
        slice_ref: GcRef,
        offset: i64,
        dest_addr: Option<std::net::SocketAddr>,
        write_back: bool,
    ) -> IoToken {
        let bytes = if write_back {
            vec![0u8; unsafe { slice::len(slice_ref) }]
        } else {
            unsafe { slice::byte_vec(slice_ref) }
        }
        .into_boxed_slice();
        let ptr = bytes.as_ptr() as usize;
        let len = bytes.len();
        let token = self.submit(handle, kind, ptr, len, offset, dest_addr);
        let write_back = write_back.then_some(slice_ref);
        let gc_root_slot = write_back.map(|root| self.retain_staged_gc_root(root));
        self.slice_staging.insert(
            token,
            StagedSliceIo {
                bytes,
                write_back,
                gc_root_slot,
            },
        );
        token
    }

    #[cfg(unix)]
    fn submit_lease_slice_io(
        &mut self,
        lease: IoLease,
        kind: OpKind,
        slice_ref: GcRef,
        offset: i64,
        dest_addr: Option<std::net::SocketAddr>,
        write_back: bool,
    ) -> IoToken {
        let bytes = if write_back {
            vec![0u8; unsafe { slice::len(slice_ref) }]
        } else {
            unsafe { slice::byte_vec(slice_ref) }
        }
        .into_boxed_slice();
        let ptr = bytes.as_ptr() as usize;
        let len = bytes.len();
        let token = self.submit_leased(lease, kind, ptr, len, offset, dest_addr);
        let write_back = write_back.then_some(slice_ref);
        let gc_root_slot = write_back.map(|root| self.retain_staged_gc_root(root));
        self.slice_staging.insert(
            token,
            StagedSliceIo {
                bytes,
                write_back,
                gc_root_slot,
            },
        );
        token
    }

    /// Submit a timer operation (sleep).
    /// duration_ns: sleep duration in nanoseconds
    pub fn submit_timer(&mut self, duration_ns: i64) -> IoToken {
        self.try_submit_timer(duration_ns)
            .expect("process-wide I/O token space exhausted")
    }

    /// Fallible timer submission for production paths that must surface
    /// process-wide identity exhaustion as a language/runtime error.
    pub fn try_submit_timer(&mut self, duration_ns: i64) -> io::Result<IoToken> {
        let token = self.try_alloc_token()?;
        match self.driver_mut()?.submit_timer(token, duration_ns) {
            SubmitResult::Completed(completion) => {
                self.store_completion_for_poll(completion);
            }
            SubmitResult::Pending => {}
        }
        Ok(token)
    }

    fn submit(
        &mut self,
        handle: IoHandle,
        kind: OpKind,
        buf_ptr: usize,
        buf_len: usize,
        offset: i64,
        dest_addr: Option<std::net::SocketAddr>,
    ) -> IoToken {
        self.submit_inner(IoSubmission {
            handle,
            cancel_key: IoCancelKey::raw_handle(handle),
            kind,
            buf_ptr,
            buf_len,
            offset,
            dest_addr,
            lease: None,
        })
    }

    #[cfg(unix)]
    fn submit_leased(
        &mut self,
        lease: IoLease,
        kind: OpKind,
        buf_ptr: usize,
        buf_len: usize,
        offset: i64,
        dest_addr: Option<std::net::SocketAddr>,
    ) -> IoToken {
        self.try_submit_leased(lease, kind, buf_ptr, buf_len, offset, dest_addr)
            .expect("leased I/O submission failed")
    }

    #[cfg(unix)]
    fn try_submit_leased(
        &mut self,
        lease: IoLease,
        kind: OpKind,
        buf_ptr: usize,
        buf_len: usize,
        offset: i64,
        dest_addr: Option<std::net::SocketAddr>,
    ) -> io::Result<IoToken> {
        let handle = lease.handle();
        let cancel_key = lease.cancel_key();
        self.try_submit_inner(IoSubmission {
            handle,
            cancel_key,
            kind,
            buf_ptr,
            buf_len,
            offset,
            dest_addr,
            lease: Some(lease),
        })
    }

    fn submit_inner(&mut self, submission: IoSubmission) -> IoToken {
        self.try_submit_inner(submission)
            .expect("I/O submission failed")
    }

    fn try_submit_inner(&mut self, submission: IoSubmission) -> io::Result<IoToken> {
        let token = self.try_alloc_token()?;
        let op = submission.into_pending(token);

        // Driver handles concurrent op check and registration
        match self.driver_mut()?.submit(op) {
            SubmitResult::Completed(completion) => {
                self.store_completion_for_poll(completion);
            }
            SubmitResult::Pending => {
                // Driver is now tracking the pending op
            }
        }

        Ok(token)
    }

    /// Try to take a completion without blocking.
    #[inline]
    pub fn try_take_completion(&mut self, token: IoToken) -> Option<Completion> {
        let completion = self.completions.remove(&token)?;
        if let Some(index) = self
            .pending_notifications
            .iter()
            .position(|pending| *pending == token)
        {
            self.pending_notifications.remove(index);
        }
        if let Some(staging) = self.slice_staging.remove(&token) {
            if let Some(target) = staging.write_back {
                let transferred = match &completion.result {
                    Ok(CompletionData::Size(size)) | Ok(CompletionData::RecvFrom(size, _)) => *size,
                    _ => 0,
                }
                .min(staging.bytes.len());
                if transferred != 0 {
                    unsafe { slice::write_bytes(target, &staging.bytes[..transferred]) };
                }
            }
            if let Some(slot) = staging.gc_root_slot {
                self.release_staged_gc_root(slot);
            }
        }
        Some(completion)
    }

    /// Tie arbitrary provider state to an I/O token. Attachments are dropped
    /// on shutdown or with the runtime, so provider workers cannot leave
    /// process-global state behind when a VM exits before their result arrives.
    pub fn attach_to_token<T: Any + Send>(
        &mut self,
        token: IoToken,
        attachment: T,
    ) -> Result<(), T> {
        match self.token_attachments.entry(token) {
            Entry::Vacant(entry) => {
                entry.insert(TokenAttachment(Box::new(attachment)));
                Ok(())
            }
            Entry::Occupied(_) => Err(attachment),
        }
    }

    /// Remove a provider attachment with its concrete type. A type mismatch
    /// leaves the attachment in place so ownership is never lost silently.
    pub fn take_token_attachment<T: Any + Send>(&mut self, token: IoToken) -> Option<T> {
        let attachment = self.token_attachments.remove(&token)?;
        match attachment.0.downcast::<T>() {
            Ok(value) => Some(*value),
            Err(value) => {
                self.token_attachments.insert(token, TokenAttachment(value));
                None
            }
        }
    }

    /// Register cleanup for a native resource owned by this VM. The factory
    /// receives a process-unique generation that providers should store beside
    /// the public handle and compare before removing from a shared table.
    pub fn register_resource_cleanup<M, F>(
        &mut self,
        make_cleanup: M,
    ) -> io::Result<IoResourceToken>
    where
        M: FnOnce(IoResourceToken) -> F,
        F: FnOnce() + Send + 'static,
    {
        let raw = allocate_io_token(&NEXT_IO_RESOURCE_TOKEN)
            .ok_or_else(|| io::Error::other("process-wide I/O resource token space exhausted"))?;
        let token = IoResourceToken(raw);
        let cleanup = ResourceCleanup(Some(Box::new(make_cleanup(token))));
        match self.resource_cleanups.entry(token) {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(cleanup);
            }
            std::collections::hash_map::Entry::Occupied(_) => {
                drop(cleanup);
                return Err(io::Error::other(
                    "process-wide I/O resource token allocator produced a duplicate",
                ));
            }
        }
        Ok(token)
    }

    /// Disarm a resource cleanup after explicit Close. Repeated calls are safe,
    /// and a Close performed by a different VM simply finds no local guard.
    pub fn disarm_resource_cleanup(&mut self, token: IoResourceToken) -> bool {
        let Some(mut cleanup) = self.resource_cleanups.remove(&token) else {
            return false;
        };
        cleanup.0.take();
        true
    }

    /// Report whether this runtime owns the cleanup guard for a native resource.
    ///
    /// Providers with process-global handle tables must check this before
    /// observing or removing an entry. The resource token is process-unique,
    /// while the guard itself remains local to the VM that registered it.
    #[inline]
    pub fn owns_resource_cleanup(&self, token: IoResourceToken) -> bool {
        self.resource_cleanups.contains_key(&token)
    }

    /// Take a completion, returning a normal I/O error for a stale or unknown token.
    #[inline]
    pub fn take_completion(&mut self, token: IoToken) -> Completion {
        self.try_take_completion(token)
            .unwrap_or_else(|| Completion {
                token,
                result: Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("completion not found: token={token}"),
                )),
            })
    }

    /// Check if a completion is ready.
    #[inline]
    pub fn has_completion(&self, token: IoToken) -> bool {
        self.completions.contains_key(&token)
    }

    /// Poll for completed operations (non-blocking).
    /// Returns tokens of newly completed operations.
    pub fn poll(&mut self) -> Vec<IoToken> {
        let mut completed_tokens = std::mem::take(&mut self.pending_notifications);
        let Some(driver) = self.driver.as_mut() else {
            return completed_tokens;
        };
        let driver_completions = driver.poll();
        completed_tokens.reserve(driver_completions.len());

        for completion in driver_completions {
            let token = completion.token;
            if let Entry::Vacant(entry) = self.completions.entry(token) {
                entry.insert(completion);
                completed_tokens.push(token);
            }
        }

        completed_tokens
    }

    fn store_completion_for_poll(&mut self, completion: Completion) {
        let token = completion.token;
        if let Entry::Vacant(entry) = self.completions.entry(token) {
            entry.insert(completion);
            self.pending_notifications.push(token);
        }
    }

    /// Cancel all pending operations on a handle.
    pub fn cancel(&mut self, key: IoCancelKey) {
        let Some(driver) = self.driver.as_mut() else {
            return;
        };
        let mut cancelled = driver.cancel(key);
        cancelled.sort_unstable_by_key(|completion| completion.token);
        for completion in cancelled {
            self.store_completion_for_poll(completion);
        }
    }

    /// Check if there are any pending operations.
    pub fn has_pending(&self) -> bool {
        !self.pending_notifications.is_empty()
            || self.driver.as_ref().is_some_and(IoDriver::has_pending)
    }

    fn retain_staged_gc_root(&mut self, root: GcRef) -> usize {
        if let Some(slot) = self.free_staged_gc_root_slots.pop() {
            debug_assert!(self.staged_gc_roots[slot].is_none());
            self.staged_gc_roots[slot] = Some(root);
            slot
        } else {
            let slot = self.staged_gc_roots.len();
            self.staged_gc_roots.push(Some(root));
            slot
        }
    }

    fn release_staged_gc_root(&mut self, slot: usize) {
        let Some(root) = self.staged_gc_roots.get_mut(slot) else {
            debug_assert!(false, "missing staged GC root slot {slot}");
            return;
        };
        if root.take().is_none() {
            debug_assert!(false, "staged GC root slot {slot} was already free");
            return;
        }
        self.free_staged_gc_root_slots.push(slot);

        while self.staged_gc_roots.last().is_some_and(Option::is_none) {
            self.staged_gc_roots.pop();
        }
        let retained_len = self.staged_gc_roots.len();
        self.free_staged_gc_root_slots
            .retain(|free| *free < retained_len);
    }

    /// Stable root slots retained by asynchronous read write-back targets.
    /// The VM scans every slot, including holes, within its normal byte budget.
    pub fn staged_gc_root_slots(&self) -> &[Option<GcRef>] {
        &self.staged_gc_roots
    }
}

impl Drop for IoRuntime {
    fn drop(&mut self) {
        self.shutdown();
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;
    #[cfg(unix)]
    use std::fs::File;
    #[cfg(unix)]
    use std::io::Write;
    #[cfg(unix)]
    use std::os::fd::{AsRawFd, FromRawFd, IntoRawFd, OwnedFd};
    #[cfg(unix)]
    use std::os::unix::net::UnixStream;

    static IO_FD_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    #[test]
    fn io_tokens_are_unique_across_concurrent_runtimes() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let workers = (0..8)
            .map(|_| {
                std::thread::spawn(|| {
                    let mut runtime = IoRuntime::new().expect("test I/O runtime");
                    (0..128).map(|_| runtime.alloc_token()).collect::<Vec<_>>()
                })
            })
            .collect::<Vec<_>>();
        let tokens = workers
            .into_iter()
            .flat_map(|worker| worker.join().expect("token worker"))
            .collect::<Vec<_>>();
        let unique = tokens.iter().copied().collect::<HashSet<_>>();

        assert_eq!(unique.len(), tokens.len());
        assert!(!unique.contains(&0));
    }

    #[test]
    fn io_token_allocator_exhausts_without_wrapping_or_aliasing() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let counter = AtomicU64::new(u64::MAX - 1);

        assert_eq!(allocate_io_token(&counter), Some(u64::MAX - 1));
        assert_eq!(allocate_io_token(&counter), Some(u64::MAX));
        assert_eq!(allocate_io_token(&counter), None);
        assert_eq!(counter.load(Ordering::Relaxed), 0);
    }

    #[test]
    fn synchronous_timer_completion_is_published_once() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let mut runtime = IoRuntime::new().expect("test I/O runtime");

        let token = runtime.try_submit_timer(0).expect("submit immediate timer");
        assert!(runtime.has_completion(token));
        assert!(runtime.has_pending());
        assert_eq!(runtime.poll(), vec![token]);
        assert!(runtime.poll().is_empty());
        assert!(matches!(
            runtime.take_completion(token).result,
            Ok(CompletionData::Timer)
        ));
        assert!(!runtime.has_pending());
    }

    #[cfg(unix)]
    #[test]
    fn synchronous_timer_failure_is_published_once() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        runtime
            .driver
            .as_mut()
            .expect("active I/O driver")
            .fail_next_timer_for_test(io::Error::other("injected timer failure"));

        let token = runtime.try_submit_timer(1).expect("submit failed timer");
        assert!(runtime.has_completion(token));
        assert!(runtime.has_pending());
        assert_eq!(runtime.poll(), vec![token]);
        assert!(runtime.poll().is_empty());
        let error = runtime.take_completion(token).result.unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::Other);
        assert!(error.to_string().contains("injected timer failure"));
        assert!(!runtime.has_pending());
    }

    #[cfg(unix)]
    #[test]
    fn cancellation_key_allocator_exhausts_with_an_explicit_error() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let counter = AtomicU64::new(u64::MAX);

        assert_eq!(allocate_io_cancel_handle(&counter).unwrap(), u64::MAX);
        let error = allocate_io_cancel_handle(&counter).unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::Other);
        assert!(error
            .to_string()
            .contains("cancellation key space exhausted"));
        assert_eq!(counter.load(Ordering::Relaxed), 0);
    }

    #[cfg(unix)]
    #[test]
    fn cancellation_keys_are_process_unique_and_clones_preserve_identity() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let first = IoCancellation::new().expect("first cancellation key");
        let first_clone = first.clone();
        let second = IoCancellation::new().expect("second cancellation key");

        assert_eq!(first.cancel_key(), first_clone.cancel_key());
        assert_ne!(first.cancel_key(), second.cancel_key());
        assert_ne!(
            first.cancel_key(),
            IoCancelKey::raw_handle(first.cancel_key().value),
            "raw handles and VM resources must occupy separate cancellation domains"
        );
        first.cancel();
        assert!(first_clone.is_cancelled());
        assert!(!second.is_cancelled());
    }

    #[cfg(unix)]
    fn fd_is_open(fd: i32) -> bool {
        (unsafe { libc::fcntl(fd, libc::F_GETFD) }) != -1
    }

    #[cfg(unix)]
    fn high_numbered_stream(stream: UnixStream) -> UnixStream {
        let fd = unsafe { libc::fcntl(stream.as_raw_fd(), libc::F_DUPFD_CLOEXEC, 10_000) };
        assert!(
            fd >= 10_000,
            "failed to allocate high test fd: {}",
            io::Error::last_os_error()
        );
        drop(stream);
        unsafe { UnixStream::from_raw_fd(fd) }
    }

    #[cfg(unix)]
    fn pending_read(
        runtime: &mut IoRuntime,
        source: &UnixStream,
        cancellation: IoCancellation,
        buffer: &mut [u8],
    ) -> (IoToken, i32, IoCancelKey) {
        let lease = IoLease::try_clone(source, cancellation).expect("clone I/O lease");
        let leased_fd = lease.handle() as i32;
        let cancel_key = lease.cancel_key();
        let token = runtime.submit_leased(
            lease,
            OpKind::Read,
            buffer.as_mut_ptr() as usize,
            buffer.len(),
            -1,
            None,
        );
        assert!(!runtime.has_completion(token));
        (token, leased_fd, cancel_key)
    }

    #[cfg(unix)]
    #[test]
    fn close_cancels_owned_read_after_forced_source_fd_reuse() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let (source, _peer) = UnixStream::pair().expect("socket pair");
        let source = high_numbered_stream(source);
        source.set_nonblocking(true).expect("nonblocking source");
        let cancellation = IoCancellation::new().expect("I/O cancellation key");
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        let mut buffer = [0xa5];
        let source_fd = source.as_raw_fd();
        let (token, leased_fd, _cancel_key) =
            pending_read(&mut runtime, &source, cancellation.clone(), &mut buffer);

        cancellation.cancel();
        drop(source);
        let zero = File::open("/dev/zero").expect("open /dev/zero");
        let reused = unsafe { libc::dup2(zero.as_raw_fd(), source_fd) };
        assert_eq!(reused, source_fd, "force exact fd reuse");
        let reused = unsafe { OwnedFd::from_raw_fd(reused) };

        assert_eq!(runtime.poll(), vec![token]);
        let completion = runtime.take_completion(token);
        assert_eq!(
            completion.result.unwrap_err().kind(),
            io::ErrorKind::Interrupted
        );
        assert_eq!(buffer, [0xa5], "canceled read touched reused /dev/zero fd");
        assert!(!fd_is_open(leased_fd), "completed lease fd remained open");
        drop(reused);
    }

    #[cfg(unix)]
    #[test]
    fn explicit_cancel_returns_completion_and_releases_lease() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let (source, _peer) = UnixStream::pair().expect("socket pair");
        source.set_nonblocking(true).expect("nonblocking source");
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        let mut buffer = [0];
        let (token, leased_fd, cancel_key) = pending_read(
            &mut runtime,
            &source,
            IoCancellation::new().expect("I/O cancellation key"),
            &mut buffer,
        );

        runtime.cancel(cancel_key);
        runtime.cancel(cancel_key);
        assert!(runtime.has_completion(token));
        assert!(
            runtime.has_pending(),
            "the scheduler notification remains pending until poll"
        );
        assert!(!fd_is_open(leased_fd));
        assert_eq!(runtime.poll(), vec![token]);
        assert!(
            runtime.poll().is_empty(),
            "cancellation was published twice"
        );
        assert!(!runtime.has_pending());
        assert_eq!(
            runtime.take_completion(token).result.unwrap_err().kind(),
            io::ErrorKind::Interrupted
        );
    }

    #[cfg(unix)]
    #[test]
    fn taking_cancelled_completion_before_poll_suppresses_stale_notification() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let (source, _peer) = UnixStream::pair().expect("socket pair");
        source.set_nonblocking(true).expect("nonblocking source");
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        let mut buffer = [0];
        let (token, leased_fd, cancel_key) = pending_read(
            &mut runtime,
            &source,
            IoCancellation::new().expect("I/O cancellation key"),
            &mut buffer,
        );

        runtime.cancel(cancel_key);
        assert_eq!(
            runtime.take_completion(token).result.unwrap_err().kind(),
            io::ErrorKind::Interrupted
        );
        assert!(!fd_is_open(leased_fd));
        assert!(!runtime.has_pending());
        assert!(runtime.poll().is_empty());
    }

    #[cfg(unix)]
    #[test]
    fn completed_operation_is_not_republished_by_late_cancel() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let (source, mut peer) = UnixStream::pair().expect("socket pair");
        source.set_nonblocking(true).expect("nonblocking source");
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        let mut buffer = [0];
        let (token, leased_fd, cancel_key) = pending_read(
            &mut runtime,
            &source,
            IoCancellation::new().expect("I/O cancellation key"),
            &mut buffer,
        );

        peer.write_all(b"x").expect("make read ready");
        assert_eq!(runtime.poll(), vec![token]);
        runtime.cancel(cancel_key);
        assert!(runtime.poll().is_empty(), "completed token was republished");
        assert!(matches!(
            runtime.take_completion(token).result,
            Ok(CompletionData::Size(1))
        ));
        assert_eq!(buffer, *b"x");
        assert!(!fd_is_open(leased_fd));
        assert!(!runtime.has_pending());
    }

    #[cfg(unix)]
    #[test]
    fn staged_read_roots_cover_pending_ready_and_cancelled_completions() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let mut gc = crate::gc::Gc::new();
        let mut runtime = IoRuntime::new().expect("test I/O runtime");

        let (source, mut peer) = UnixStream::pair().expect("ready socket pair");
        source.set_nonblocking(true).expect("nonblocking source");
        let cancellation = IoCancellation::new().expect("ready cancellation key");
        let lease = IoLease::try_clone(&source, cancellation).expect("ready lease");
        let target = slice::create(
            &mut gc,
            crate::ValueMeta::new(0, crate::ValueKind::Uint8),
            1,
            1,
            1,
        );
        let token = runtime.submit_lease_slice_read(lease, target);
        assert_eq!(runtime.staged_gc_root_slots(), &[Some(target)]);
        peer.write_all(b"r").expect("wake ready read");
        assert_eq!(runtime.poll(), vec![token]);
        assert_eq!(
            runtime.staged_gc_root_slots(),
            &[Some(target)],
            "a ready but unconsumed completion must retain its write-back target"
        );
        assert!(matches!(
            runtime.take_completion(token).result,
            Ok(CompletionData::Size(1))
        ));
        assert!(runtime.staged_gc_root_slots().is_empty());
        assert_eq!(unsafe { slice::byte_vec(target) }, b"r");

        let (source, _peer) = UnixStream::pair().expect("cancel socket pair");
        source.set_nonblocking(true).expect("nonblocking source");
        let cancellation = IoCancellation::new().expect("cancel cancellation key");
        let cancel_key = cancellation.cancel_key();
        let lease = IoLease::try_clone(&source, cancellation.clone()).expect("cancel lease");
        let target = slice::create(
            &mut gc,
            crate::ValueMeta::new(0, crate::ValueKind::Uint8),
            1,
            1,
            1,
        );
        let token = runtime.submit_lease_slice_read(lease, target);
        assert_eq!(runtime.staged_gc_root_slots(), &[Some(target)]);
        cancellation.cancel();
        runtime.cancel(cancel_key);
        assert!(runtime.has_completion(token));
        assert_eq!(
            runtime.staged_gc_root_slots(),
            &[Some(target)],
            "a cancelled completion must retain its target until consumption"
        );
        assert_eq!(
            runtime.take_completion(token).result.unwrap_err().kind(),
            io::ErrorKind::Interrupted
        );
        assert!(runtime.staged_gc_root_slots().is_empty());
    }

    #[cfg(unix)]
    #[test]
    fn cancelling_one_resource_does_not_cancel_an_independent_resource() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let (first_source, _first_peer) = UnixStream::pair().expect("first socket pair");
        let (second_source, mut second_peer) = UnixStream::pair().expect("second socket pair");
        first_source
            .set_nonblocking(true)
            .expect("nonblocking first source");
        second_source
            .set_nonblocking(true)
            .expect("nonblocking second source");
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        let mut first_buffer = [0u8];
        let mut second_buffer = [0u8];
        let (first_token, _, first_key) = pending_read(
            &mut runtime,
            &first_source,
            IoCancellation::new().expect("first cancellation key"),
            &mut first_buffer,
        );
        let (second_token, _, second_key) = pending_read(
            &mut runtime,
            &second_source,
            IoCancellation::new().expect("second cancellation key"),
            &mut second_buffer,
        );
        assert_ne!(first_key, second_key);

        runtime.cancel(first_key);
        assert!(runtime.has_completion(first_token));
        assert!(!runtime.has_completion(second_token));
        assert!(runtime.has_pending());

        second_peer.write_all(b"z").expect("wake second resource");
        assert_eq!(runtime.poll(), vec![first_token, second_token]);
        assert!(matches!(
            runtime.take_completion(second_token).result,
            Ok(CompletionData::Size(1))
        ));
        assert_eq!(second_buffer, *b"z");
        assert_eq!(
            runtime
                .take_completion(first_token)
                .result
                .unwrap_err()
                .kind(),
            io::ErrorKind::Interrupted
        );
    }

    #[cfg(unix)]
    #[test]
    fn immediate_completion_and_runtime_drop_release_leases() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let (source, mut peer) = UnixStream::pair().expect("socket pair");
        source.set_nonblocking(true).expect("nonblocking source");
        peer.write_all(b"x").expect("prime socket");
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        let mut completed_buffer = [0];
        let lease = IoLease::try_clone(
            &source,
            IoCancellation::new().expect("I/O cancellation key"),
        )
        .expect("clone lease");
        let completed_fd = lease.handle() as i32;
        let token = runtime.submit_leased(
            lease,
            OpKind::Read,
            completed_buffer.as_mut_ptr() as usize,
            1,
            -1,
            None,
        );
        assert!(runtime.has_completion(token));
        assert!(runtime.has_pending());
        assert!(!fd_is_open(completed_fd));
        assert!(matches!(
            runtime.take_completion(token).result,
            Ok(CompletionData::Size(1))
        ));
        assert_eq!(completed_buffer, *b"x");
        assert!(!runtime.has_pending());
        assert!(runtime.poll().is_empty());

        let mut pending_buffer = [0];
        let (_token, pending_fd, _) = pending_read(
            &mut runtime,
            &source,
            IoCancellation::new().expect("I/O cancellation key"),
            &mut pending_buffer,
        );
        drop(runtime);
        assert!(!fd_is_open(pending_fd), "VM/runtime drop leaked a lease fd");
    }

    #[cfg(unix)]
    #[test]
    fn unconsumed_accept_completion_owns_and_closes_accepted_fd() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let (accepted, _peer) = UnixStream::pair().expect("accepted socket pair");
        let accepted = unsafe { OwnedFd::from_raw_fd(accepted.into_raw_fd()) };
        let accepted_fd = accepted.as_raw_fd();
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        let token = runtime.alloc_token();
        runtime.completions.insert(
            token,
            Completion {
                token,
                result: Ok(CompletionData::Accept(accepted)),
            },
        );
        assert!(fd_is_open(accepted_fd));
        drop(runtime);
        assert!(
            !fd_is_open(accepted_fd),
            "unconsumed accepted fd leaked on VM drop"
        );
    }

    #[cfg(unix)]
    #[test]
    fn registration_failure_is_an_immediate_completion_and_releases_lease() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let (source, _peer) = UnixStream::pair().expect("socket pair");
        source.set_nonblocking(true).expect("nonblocking source");
        let cancellation = IoCancellation::new().expect("I/O cancellation key");
        let lease = IoLease::try_clone(&source, cancellation).expect("clone I/O lease");
        let leased_fd = lease.handle() as i32;
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        runtime
            .driver
            .as_mut()
            .expect("active I/O driver")
            .fail_next_registration_for_test(io::Error::other("injected registration failure"));

        let token = runtime.submit_lease_owned_read(lease, 1);
        let error = runtime.take_completion(token).result.unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::Other);
        assert!(error.to_string().contains("injected registration failure"));
        assert!(!runtime.has_pending());
        assert!(!fd_is_open(leased_fd));
    }

    #[cfg(unix)]
    #[test]
    fn peer_hangup_wakes_a_pending_read_as_eof() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let (source, peer) = UnixStream::pair().expect("socket pair");
        source.set_nonblocking(true).expect("nonblocking source");
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        let mut byte = 0u8;
        let (token, _, _) = pending_read(
            &mut runtime,
            &source,
            IoCancellation::new().expect("I/O cancellation key"),
            std::slice::from_mut(&mut byte),
        );

        drop(peer);
        assert_eq!(runtime.poll(), vec![token]);
        assert!(matches!(
            runtime.take_completion(token).result,
            Ok(CompletionData::Size(0))
        ));
    }

    #[cfg(unix)]
    #[test]
    fn failed_reregistration_completes_every_operation_on_the_fd() {
        let _guard = IO_FD_TEST_LOCK.lock().unwrap();
        let (source, _peer) = UnixStream::pair().expect("socket pair");
        source.set_nonblocking(true).expect("nonblocking source");
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        let handle = source.as_raw_fd() as IoHandle;
        let mut read_byte = 0u8;
        let read_token = runtime.submit_read(handle, &mut read_byte, 1);
        assert!(!runtime.has_completion(read_token));

        let chunk = [0u8; 8192];
        let mut source_ref = &source;
        loop {
            match source_ref.write(&chunk) {
                Ok(0) => panic!("socket write made no progress before WouldBlock"),
                Ok(_) => {}
                Err(error) if error.kind() == io::ErrorKind::WouldBlock => break,
                Err(error) => panic!("failed to fill socket send buffer: {error}"),
            }
        }

        runtime
            .driver
            .as_mut()
            .expect("active I/O driver")
            .fail_next_registration_for_test(io::Error::other("injected rearm failure"));
        let write_byte = 1u8;
        let write_token = runtime.submit_write(handle, &write_byte, 1);
        assert!(runtime.has_completion(write_token));
        assert!(
            runtime.has_pending(),
            "the queued read failure must stay pollable"
        );
        assert_eq!(runtime.poll(), vec![write_token, read_token]);
        assert!(!runtime.has_pending());

        for token in [read_token, write_token] {
            let error = runtime.take_completion(token).result.unwrap_err();
            assert_eq!(error.kind(), io::ErrorKind::Other);
            assert!(error.to_string().contains("injected rearm failure"));
        }
    }

    #[test]
    fn token_attachments_preserve_type_and_follow_runtime_lifetime() {
        struct DropProbe(std::sync::Arc<std::sync::atomic::AtomicUsize>);

        impl Drop for DropProbe {
            fn drop(&mut self) {
                self.0.fetch_add(1, Ordering::SeqCst);
            }
        }

        let drops = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        let typed_token = runtime.alloc_token();
        runtime
            .attach_to_token(typed_token, String::from("response"))
            .expect("attach provider state");
        assert_eq!(runtime.take_token_attachment::<u64>(typed_token), None);
        assert_eq!(
            runtime.take_token_attachment::<String>(typed_token),
            Some(String::from("response"))
        );

        let drop_token = runtime.alloc_token();
        runtime
            .attach_to_token(drop_token, DropProbe(drops.clone()))
            .map_err(drop)
            .expect("attach drop probe");
        drop(runtime);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn shutdown_releases_attachments_and_resources_without_dropping_runtime() {
        struct DropProbe(std::sync::Arc<std::sync::atomic::AtomicUsize>);

        impl Drop for DropProbe {
            fn drop(&mut self) {
                self.0.fetch_add(1, Ordering::SeqCst);
            }
        }

        let attachment_drops = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let resource_cleanups = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        let token = runtime.alloc_token();
        runtime
            .attach_to_token(token, DropProbe(attachment_drops.clone()))
            .map_err(drop)
            .expect("attach drop probe");
        let cleanups = resource_cleanups.clone();
        runtime
            .register_resource_cleanup(move |_| {
                move || {
                    cleanups.fetch_add(1, Ordering::SeqCst);
                }
            })
            .expect("register cleanup");
        let synchronous_token = runtime
            .try_submit_timer(0)
            .expect("submit immediate timer before shutdown");
        assert!(runtime.has_completion(synchronous_token));
        assert!(runtime.has_pending());

        runtime.shutdown();

        assert_eq!(attachment_drops.load(Ordering::SeqCst), 1);
        assert_eq!(resource_cleanups.load(Ordering::SeqCst), 1);
        assert!(!runtime.has_completion(synchronous_token));
        assert!(!runtime.has_pending());
        assert!(runtime.poll().is_empty());
        let error = runtime.try_submit_timer(1).unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::BrokenPipe);

        runtime.shutdown();
        assert_eq!(attachment_drops.load(Ordering::SeqCst), 1);
        assert_eq!(resource_cleanups.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn resource_cleanup_is_disarmable_and_isolated_between_vms() {
        let cleaned = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let mut first = IoRuntime::new().expect("first I/O runtime");
        let mut second = IoRuntime::new().expect("second I/O runtime");
        let first_cleaned = cleaned.clone();
        let first_token = first
            .register_resource_cleanup(move |_| move || first_cleaned.lock().unwrap().push("first"))
            .expect("first resource cleanup");
        let second_cleaned = cleaned.clone();
        let second_token = second
            .register_resource_cleanup(move |_| {
                move || second_cleaned.lock().unwrap().push("second")
            })
            .expect("second resource cleanup");
        assert_ne!(first_token, second_token);
        assert!(first.owns_resource_cleanup(first_token));
        assert!(!first.owns_resource_cleanup(second_token));
        assert!(second.owns_resource_cleanup(second_token));
        assert!(!second.owns_resource_cleanup(first_token));

        assert!(first.disarm_resource_cleanup(first_token));
        assert!(!first.owns_resource_cleanup(first_token));
        assert!(!first.disarm_resource_cleanup(first_token));
        assert!(!first.disarm_resource_cleanup(second_token));
        drop(first);
        assert!(cleaned.lock().unwrap().is_empty());
        drop(second);
        assert_eq!(*cleaned.lock().unwrap(), ["second"]);
    }

    #[test]
    fn stale_vm_cleanup_cannot_remove_a_reused_handle_generation() {
        let table = std::sync::Arc::new(std::sync::Mutex::new(HashMap::new()));
        let handle = 17_i32;
        let mut old_vm = IoRuntime::new().expect("old I/O runtime");
        let old_table = table.clone();
        let old_token = old_vm
            .register_resource_cleanup(move |token| {
                move || {
                    let mut table = old_table.lock().unwrap();
                    if table.get(&handle) == Some(&token) {
                        table.remove(&handle);
                    }
                }
            })
            .expect("old resource cleanup");
        table.lock().unwrap().insert(handle, old_token);

        let mut new_vm = IoRuntime::new().expect("new I/O runtime");
        let new_table = table.clone();
        let new_token = new_vm
            .register_resource_cleanup(move |token| {
                move || {
                    let mut table = new_table.lock().unwrap();
                    if table.get(&handle) == Some(&token) {
                        table.remove(&handle);
                    }
                }
            })
            .expect("new resource cleanup");
        table.lock().unwrap().insert(handle, new_token);

        drop(old_vm);
        assert_eq!(table.lock().unwrap().get(&handle), Some(&new_token));
        drop(new_vm);
        assert!(!table.lock().unwrap().contains_key(&handle));
    }
}
