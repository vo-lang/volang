//! Unix I/O driver using epoll (Linux) or kqueue (BSD/macOS).
//!
//! Key design:
//! - Each fd can have one pending read AND one pending write simultaneously
//! - Driver handles the readiness -> completion translation internally
//! - poll() returns completed tokens with actual results

use std::collections::HashMap;
use std::io;
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};

use super::{Completion, CompletionData, IoCancelKey, IoToken, OpKind, PendingOp, SubmitResult};

type ReadyFd = (i32, bool, bool);
type ReadyBatch = (Vec<ReadyFd>, Vec<IoToken>);

/// Tracks pending operations for a single fd.
#[derive(Debug, Default)]
struct FdState {
    read: Option<PendingOp>,
    write: Option<PendingOp>,
}

/// Pending timer operation.
#[derive(Debug)]
struct TimerState {
    token: IoToken,
    #[cfg(target_os = "linux")]
    timerfd: OwnedFd,
}

/// Unix I/O driver.
#[derive(Debug)]
pub struct UnixDriver {
    fd_states: HashMap<i32, FdState>,
    /// Pending timers (token -> TimerState)
    timers: HashMap<IoToken, TimerState>,
    /// Registration failures discovered while rearming an existing fd are
    /// delivered on the next poll instead of leaving their fibers suspended.
    queued_completions: Vec<Completion>,

    #[cfg(test)]
    fail_next_registration: Option<io::Error>,
    #[cfg(test)]
    fail_next_timer: Option<io::Error>,

    #[cfg(target_os = "linux")]
    epoll_fd: OwnedFd,

    #[cfg(any(
        target_os = "macos",
        target_os = "freebsd",
        target_os = "openbsd",
        target_os = "netbsd"
    ))]
    kqueue_fd: OwnedFd,
}

impl UnixDriver {
    pub fn new() -> io::Result<Self> {
        #[cfg(target_os = "linux")]
        {
            let epoll_fd = unsafe { libc::epoll_create1(libc::EPOLL_CLOEXEC) };
            if epoll_fd < 0 {
                return Err(io::Error::last_os_error());
            }
            Ok(Self {
                fd_states: HashMap::new(),
                timers: HashMap::new(),
                queued_completions: Vec::new(),
                #[cfg(test)]
                fail_next_registration: None,
                #[cfg(test)]
                fail_next_timer: None,
                epoll_fd: unsafe { OwnedFd::from_raw_fd(epoll_fd) },
            })
        }

        #[cfg(any(
            target_os = "macos",
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "netbsd"
        ))]
        {
            let kqueue_fd = unsafe { libc::kqueue() };
            if kqueue_fd < 0 {
                return Err(io::Error::last_os_error());
            }
            Ok(Self {
                fd_states: HashMap::new(),
                timers: HashMap::new(),
                queued_completions: Vec::new(),
                #[cfg(test)]
                fail_next_registration: None,
                #[cfg(test)]
                fail_next_timer: None,
                kqueue_fd: unsafe { OwnedFd::from_raw_fd(kqueue_fd) },
            })
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
        if op.is_cancelled() {
            return SubmitResult::Completed(cancelled_completion(op.token));
        }
        let fd = op.handle as i32;
        let is_read = matches!(
            op.kind,
            OpKind::Read | OpKind::Accept | OpKind::Connect | OpKind::RecvFrom
        );

        // Check for concurrent operation on same direction
        for state in self.fd_states.values() {
            if is_read {
                if let Some(existing) = state
                    .read
                    .as_ref()
                    .filter(|existing| existing.cancel_key == op.cancel_key)
                {
                    return SubmitResult::Completed(Completion {
                        token: op.token,
                        result: Err(io::Error::new(
                            io::ErrorKind::WouldBlock,
                            format!(
                                "concurrent read on fd {fd}: existing token={}, new token={}",
                                existing.token, op.token
                            ),
                        )),
                    });
                }
            }
            if !is_read
                && state
                    .write
                    .as_ref()
                    .is_some_and(|existing| existing.cancel_key == op.cancel_key)
            {
                return SubmitResult::Completed(Completion {
                    token: op.token,
                    result: Err(io::Error::new(
                        io::ErrorKind::WouldBlock,
                        format!("concurrent write operation on fd {fd}"),
                    )),
                });
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
        let op_token = op.token;
        let state = self.fd_states.entry(fd).or_default();
        if is_read {
            state.read = Some(op);
        } else {
            state.write = Some(op);
        }
        if let Err(error) = self.update_registration(fd) {
            let mut failures = self.fail_fd(fd, &error);
            let index = failures
                .iter()
                .position(|completion| completion.token == op_token)
                .expect("newly submitted operation must remain in its fd state");
            let completion = failures.swap_remove(index);
            self.queued_completions.extend(failures);
            return SubmitResult::Completed(completion);
        }

        SubmitResult::Pending
    }

    /// Submit a timer operation. Uses timerfd on Linux, kqueue timer on macOS/BSD.
    pub fn submit_timer(&mut self, token: IoToken, duration_ns: i64) -> SubmitResult {
        if duration_ns <= 0 {
            return SubmitResult::Completed(Completion {
                token,
                result: Ok(CompletionData::Timer),
            });
        }

        #[cfg(test)]
        if let Some(error) = self.fail_next_timer.take() {
            return SubmitResult::Completed(Completion {
                token,
                result: Err(error),
            });
        }

        #[cfg(target_os = "linux")]
        {
            // Create timerfd
            let timerfd = unsafe {
                libc::timerfd_create(
                    libc::CLOCK_MONOTONIC,
                    libc::TFD_NONBLOCK | libc::TFD_CLOEXEC,
                )
            };
            if timerfd < 0 {
                return SubmitResult::Completed(Completion {
                    token,
                    result: Err(io::Error::last_os_error()),
                });
            }
            let timerfd = unsafe { OwnedFd::from_raw_fd(timerfd) };

            // Set timer
            let secs = duration_ns / 1_000_000_000;
            let nsecs = duration_ns % 1_000_000_000;
            let its = libc::itimerspec {
                it_interval: libc::timespec {
                    tv_sec: 0,
                    tv_nsec: 0,
                },
                it_value: libc::timespec {
                    tv_sec: secs,
                    tv_nsec: nsecs,
                },
            };
            let ret = unsafe {
                libc::timerfd_settime(timerfd.as_raw_fd(), 0, &its, std::ptr::null_mut())
            };
            if ret < 0 {
                return SubmitResult::Completed(Completion {
                    token,
                    result: Err(io::Error::last_os_error()),
                });
            }

            // Register with epoll for read (timer fires = readable)
            let mut ev = libc::epoll_event {
                events: libc::EPOLLIN as u32,
                u64: timerfd.as_raw_fd() as u64,
            };
            let ret = unsafe {
                libc::epoll_ctl(
                    self.epoll_fd.as_raw_fd(),
                    libc::EPOLL_CTL_ADD,
                    timerfd.as_raw_fd(),
                    &mut ev,
                )
            };
            if ret < 0 {
                return SubmitResult::Completed(Completion {
                    token,
                    result: Err(io::Error::last_os_error()),
                });
            }

            self.timers.insert(token, TimerState { token, timerfd });
            SubmitResult::Pending
        }

        #[cfg(any(
            target_os = "macos",
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "netbsd"
        ))]
        {
            // Use kqueue EVFILT_TIMER
            let ms = isize::try_from((duration_ns / 1_000_000).max(1)).unwrap_or(isize::MAX);
            let ev = libc::kevent {
                ident: token as usize,
                filter: libc::EVFILT_TIMER,
                flags: libc::EV_ADD | libc::EV_ONESHOT,
                fflags: 0,
                data: ms,
                udata: std::ptr::null_mut(),
            };
            let ret = unsafe {
                libc::kevent(
                    self.kqueue_fd.as_raw_fd(),
                    &ev,
                    1,
                    std::ptr::null_mut(),
                    0,
                    std::ptr::null(),
                )
            };
            if ret < 0 {
                return SubmitResult::Completed(Completion {
                    token,
                    result: Err(io::Error::last_os_error()),
                });
            }
            self.timers.insert(token, TimerState { token });
            SubmitResult::Pending
        }

        #[cfg(not(any(
            target_os = "linux",
            target_os = "macos",
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "netbsd"
        )))]
        {
            // Fallback: synchronous sleep (not ideal but works)
            std::thread::sleep(std::time::Duration::from_nanos(duration_ns as u64));
            return SubmitResult::Completed(Completion {
                token,
                result: Ok(CompletionData::Timer),
            });
        }
    }

    /// Poll for completed operations. Returns completions.
    pub fn poll(&mut self) -> Vec<Completion> {
        let mut completed = std::mem::take(&mut self.queued_completions);
        completed.extend(self.take_matching_operations(PendingOp::is_cancelled));
        let (ready_fds, ready_timers) = match self.poll_ready() {
            Ok(ready) => ready,
            Err(error) => {
                completed.extend(self.fail_all(&error));
                return completed;
            }
        };
        let mut fds_to_update = Vec::new();
        let mut fds_to_remove = Vec::new();

        // Handle timer completions
        for token in ready_timers {
            if let Some(timer) = self.timers.remove(&token) {
                #[cfg(target_os = "linux")]
                {
                    // Read to clear the timer, then close the fd
                    let mut buf = [0u8; 8];
                    unsafe { libc::read(timer.timerfd.as_raw_fd(), buf.as_mut_ptr() as *mut _, 8) };
                }
                completed.push(Completion {
                    token: timer.token,
                    result: Ok(CompletionData::Timer),
                });
            }
        }

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
                            completed.push(Completion {
                                token: op.token,
                                result: Ok(data),
                            });
                        }
                        TryResult::Error(e) => {
                            completed.push(Completion {
                                token: op.token,
                                result: Err(e),
                            });
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
                            completed.push(Completion {
                                token: op.token,
                                result: Ok(data),
                            });
                        }
                        TryResult::Error(e) => {
                            completed.push(Completion {
                                token: op.token,
                                result: Err(e),
                            });
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
            if let Err(error) = self.update_registration(fd) {
                completed.extend(self.fail_fd(fd, &error));
            }
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

    /// Cancel all operations on a logical source handle and return ordinary
    /// cancellation completions so suspended fibers can resume.
    pub fn cancel(&mut self, key: IoCancelKey) -> Vec<Completion> {
        self.take_matching_operations(|op| op.cancel_key == key)
    }

    pub fn has_pending(&self) -> bool {
        !self.fd_states.is_empty() || !self.timers.is_empty() || !self.queued_completions.is_empty()
    }

    #[cfg(test)]
    pub(crate) fn fail_next_registration_for_test(&mut self, error: io::Error) {
        self.fail_next_registration = Some(error);
    }

    #[cfg(test)]
    pub(crate) fn fail_next_timer_for_test(&mut self, error: io::Error) {
        self.fail_next_timer = Some(error);
    }

    fn take_matching_operations(
        &mut self,
        mut matches: impl FnMut(&PendingOp) -> bool,
    ) -> Vec<Completion> {
        let fds = self.fd_states.keys().copied().collect::<Vec<_>>();
        let mut completed = Vec::new();
        let mut remove = Vec::new();
        let mut update = Vec::new();

        for fd in fds {
            let Some(state) = self.fd_states.get_mut(&fd) else {
                continue;
            };
            if state.read.as_ref().is_some_and(&mut matches) {
                let op = state.read.take().expect("checked pending read");
                completed.push(cancelled_completion(op.token));
            }
            if state.write.as_ref().is_some_and(&mut matches) {
                let op = state.write.take().expect("checked pending write");
                completed.push(cancelled_completion(op.token));
            }
            if state.read.is_none() && state.write.is_none() {
                remove.push(fd);
            } else {
                update.push(fd);
            }
        }

        for fd in update {
            if let Err(error) = self.update_registration(fd) {
                completed.extend(self.fail_fd(fd, &error));
            }
        }
        for fd in remove {
            self.fd_states.remove(&fd);
            self.unregister_fd(fd);
        }
        completed
    }

    fn fail_fd(&mut self, fd: i32, error: &io::Error) -> Vec<Completion> {
        let Some(state) = self.fd_states.remove(&fd) else {
            return Vec::new();
        };
        self.unregister_fd(fd);
        state
            .read
            .into_iter()
            .chain(state.write)
            .map(|op| Completion {
                token: op.token,
                result: Err(copy_io_error(error)),
            })
            .collect()
    }

    fn fail_all(&mut self, error: &io::Error) -> Vec<Completion> {
        let fds = self.fd_states.keys().copied().collect::<Vec<_>>();
        let mut completed = Vec::new();
        for fd in fds {
            completed.extend(self.fail_fd(fd, error));
        }
        completed.extend(
            std::mem::take(&mut self.timers)
                .into_values()
                .map(|timer| Completion {
                    token: timer.token,
                    result: Err(copy_io_error(error)),
                }),
        );
        completed
    }

    fn poll_ready(&mut self) -> io::Result<ReadyBatch> {
        let mut ready = Vec::new();
        let mut ready_timers = Vec::new();

        #[cfg(target_os = "linux")]
        {
            // Build timerfd -> token map for lookup
            let timerfd_to_token: HashMap<i32, IoToken> = self
                .timers
                .iter()
                .map(|(token, state)| (state.timerfd.as_raw_fd(), *token))
                .collect();

            let mut events: [libc::epoll_event; 64] = unsafe { std::mem::zeroed() };
            let n =
                unsafe { libc::epoll_wait(self.epoll_fd.as_raw_fd(), events.as_mut_ptr(), 64, 0) };
            if n < 0 {
                let error = io::Error::last_os_error();
                if error.kind() != io::ErrorKind::Interrupted {
                    return Err(error);
                }
            }
            if n > 0 {
                for event in events.iter().take(n as usize) {
                    let fd = event.u64 as i32;
                    // Check if this is a timer fd
                    if let Some(&token) = timerfd_to_token.get(&fd) {
                        ready_timers.push(token);
                    } else {
                        let ev = event.events;
                        let terminal =
                            libc::EPOLLERR as u32 | libc::EPOLLHUP as u32 | libc::EPOLLRDHUP as u32;
                        let readable = (ev & (libc::EPOLLIN as u32 | terminal)) != 0;
                        let writable = (ev
                            & (libc::EPOLLOUT as u32
                                | libc::EPOLLERR as u32
                                | libc::EPOLLHUP as u32))
                            != 0;
                        ready.push((fd, readable, writable));
                    }
                }
            }
        }

        #[cfg(any(
            target_os = "macos",
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "netbsd"
        ))]
        {
            let mut events: [libc::kevent; 64] = unsafe { std::mem::zeroed() };
            let timeout = libc::timespec {
                tv_sec: 0,
                tv_nsec: 0,
            };
            let n = unsafe {
                libc::kevent(
                    self.kqueue_fd.as_raw_fd(),
                    std::ptr::null(),
                    0,
                    events.as_mut_ptr(),
                    64,
                    &timeout,
                )
            };
            if n < 0 {
                let error = io::Error::last_os_error();
                if error.kind() != io::ErrorKind::Interrupted {
                    return Err(error);
                }
            }
            if n > 0 {
                // kqueue returns separate events for read/write, need to merge
                let mut fd_events: HashMap<i32, (bool, bool)> = HashMap::new();
                for event in &events[..n as usize] {
                    // Check if this is a timer event
                    if event.filter == libc::EVFILT_TIMER {
                        let token = event.ident as IoToken;
                        ready_timers.push(token);
                    } else {
                        let fd = event.ident as i32;
                        let entry = fd_events.entry(fd).or_insert((false, false));
                        if event.filter == libc::EVFILT_READ {
                            entry.0 = true;
                        }
                        if event.filter == libc::EVFILT_WRITE {
                            entry.1 = true;
                        }
                    }
                }
                for (fd, (r, w)) in fd_events {
                    ready.push((fd, r, w));
                }
            }
        }

        Ok((ready, ready_timers))
    }

    fn update_registration(&mut self, fd: i32) -> io::Result<()> {
        #[cfg(test)]
        if let Some(error) = self.fail_next_registration.take() {
            return Err(error);
        }

        let state = match self.fd_states.get(&fd) {
            Some(s) => s,
            None => {
                self.unregister_fd(fd);
                return Ok(());
            }
        };

        let want_read = state.read.is_some();
        let want_write = state.write.is_some();

        if !want_read && !want_write {
            self.unregister_fd(fd);
            return Ok(());
        }

        #[cfg(target_os = "linux")]
        {
            let mut events = libc::EPOLLONESHOT as u32;
            if want_read {
                events |= libc::EPOLLIN as u32 | libc::EPOLLRDHUP as u32;
            }
            if want_write {
                events |= libc::EPOLLOUT as u32;
            }

            let mut event = libc::epoll_event {
                events,
                u64: fd as u64,
            };

            // Try ADD first (most common case for new fd)
            let ret = unsafe {
                libc::epoll_ctl(
                    self.epoll_fd.as_raw_fd(),
                    libc::EPOLL_CTL_ADD,
                    fd,
                    &mut event,
                )
            };
            if ret < 0 {
                let err = io::Error::last_os_error();
                if err.raw_os_error() == Some(libc::EEXIST) {
                    // Already registered, modify it
                    let ret = unsafe {
                        libc::epoll_ctl(
                            self.epoll_fd.as_raw_fd(),
                            libc::EPOLL_CTL_MOD,
                            fd,
                            &mut event,
                        )
                    };
                    if ret < 0 {
                        return Err(io::Error::last_os_error());
                    }
                } else {
                    return Err(err);
                }
            }
        }

        #[cfg(any(
            target_os = "macos",
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "netbsd"
        ))]
        {
            self.kqueue_update_filter(fd, libc::EVFILT_READ, want_read)?;
            self.kqueue_update_filter(fd, libc::EVFILT_WRITE, want_write)?;
        }

        Ok(())
    }

    #[cfg(any(
        target_os = "macos",
        target_os = "freebsd",
        target_os = "openbsd",
        target_os = "netbsd"
    ))]
    fn kqueue_update_filter(&self, fd: i32, filter: i16, enable: bool) -> io::Result<()> {
        let flags = if enable {
            libc::EV_ADD | libc::EV_ONESHOT
        } else {
            libc::EV_DELETE
        };
        let event = libc::kevent {
            ident: fd as usize,
            filter,
            flags,
            fflags: 0,
            data: 0,
            udata: std::ptr::null_mut(),
        };
        let ret = unsafe {
            libc::kevent(
                self.kqueue_fd.as_raw_fd(),
                &event,
                1,
                std::ptr::null_mut(),
                0,
                std::ptr::null(),
            )
        };
        if ret < 0 {
            let error = io::Error::last_os_error();
            if !enable && error.raw_os_error() == Some(libc::ENOENT) {
                return Ok(());
            }
            return Err(error);
        }
        Ok(())
    }

    fn unregister_fd(&mut self, fd: i32) {
        #[cfg(target_os = "linux")]
        unsafe {
            libc::epoll_ctl(
                self.epoll_fd.as_raw_fd(),
                libc::EPOLL_CTL_DEL,
                fd,
                std::ptr::null_mut(),
            );
        }

        #[cfg(any(
            target_os = "macos",
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "netbsd"
        ))]
        {
            let _ = self.kqueue_update_filter(fd, libc::EVFILT_READ, false);
            let _ = self.kqueue_update_filter(fd, libc::EVFILT_WRITE, false);
        }
    }
}

enum TryResult {
    Done(CompletionData),
    Error(io::Error),
    WouldBlock,
}

fn try_complete(op: &PendingOp) -> TryResult {
    if op.is_cancelled() {
        return TryResult::Error(cancelled_error());
    }
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
            #[cfg(any(target_os = "linux", target_os = "android"))]
            let nfd = unsafe {
                libc::accept4(
                    fd,
                    core::ptr::null_mut(),
                    core::ptr::null_mut(),
                    libc::SOCK_NONBLOCK | libc::SOCK_CLOEXEC,
                )
            };
            #[cfg(not(any(target_os = "linux", target_os = "android")))]
            let nfd = unsafe { libc::accept(fd, core::ptr::null_mut(), core::ptr::null_mut()) };
            if nfd >= 0 {
                let accepted = unsafe { OwnedFd::from_raw_fd(nfd) };

                #[cfg(not(any(target_os = "linux", target_os = "android")))]
                {
                    let flags = unsafe { libc::fcntl(accepted.as_raw_fd(), libc::F_GETFL) };
                    if flags == -1 {
                        return TryResult::Error(io::Error::last_os_error());
                    }
                    if unsafe {
                        libc::fcntl(
                            accepted.as_raw_fd(),
                            libc::F_SETFL,
                            flags | libc::O_NONBLOCK,
                        )
                    } == -1
                    {
                        return TryResult::Error(io::Error::last_os_error());
                    }
                    let fd_flags = unsafe { libc::fcntl(accepted.as_raw_fd(), libc::F_GETFD) };
                    if fd_flags == -1 {
                        return TryResult::Error(io::Error::last_os_error());
                    }
                    if unsafe {
                        libc::fcntl(
                            accepted.as_raw_fd(),
                            libc::F_SETFD,
                            fd_flags | libc::FD_CLOEXEC,
                        )
                    } == -1
                    {
                        return TryResult::Error(io::Error::last_os_error());
                    }
                }
                TryResult::Done(CompletionData::Accept(accepted))
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
            let mut addr_len: libc::socklen_t =
                std::mem::size_of::<libc::sockaddr_storage>() as libc::socklen_t;
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
            let Some(addr) = op.dest_addr.as_ref() else {
                return TryResult::Error(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "SendTo requires a destination address",
                ));
            };
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
        OpKind::Timer => TryResult::Error(io::Error::new(
            io::ErrorKind::InvalidInput,
            "timer operation reached the regular I/O submission path",
        )),
    }
}

fn cancelled_error() -> io::Error {
    io::Error::new(
        io::ErrorKind::Interrupted,
        "I/O operation canceled because the resource was closed",
    )
}

fn cancelled_completion(token: IoToken) -> Completion {
    Completion {
        token,
        result: Err(cancelled_error()),
    }
}

fn copy_io_error(error: &io::Error) -> io::Error {
    match error.raw_os_error() {
        Some(code) => io::Error::from_raw_os_error(code),
        None => io::Error::new(error.kind(), error.to_string()),
    }
}

fn sockaddr_to_std(addr: &libc::sockaddr_storage, len: libc::socklen_t) -> std::net::SocketAddr {
    use std::net::{Ipv4Addr, Ipv6Addr, SocketAddr, SocketAddrV4, SocketAddrV6};

    unsafe {
        if addr.ss_family as i32 == libc::AF_INET
            && len as usize >= std::mem::size_of::<libc::sockaddr_in>()
        {
            let addr_in = &*(addr as *const _ as *const libc::sockaddr_in);
            let ip = Ipv4Addr::from(u32::from_be(addr_in.sin_addr.s_addr));
            let port = u16::from_be(addr_in.sin_port);
            SocketAddr::V4(SocketAddrV4::new(ip, port))
        } else if addr.ss_family as i32 == libc::AF_INET6
            && len as usize >= std::mem::size_of::<libc::sockaddr_in6>()
        {
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
            (
                storage,
                std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t,
            )
        }
        SocketAddr::V6(v6) => {
            let addr_in6 = unsafe { &mut *(&mut storage as *mut _ as *mut libc::sockaddr_in6) };
            addr_in6.sin6_family = libc::AF_INET6 as libc::sa_family_t;
            addr_in6.sin6_port = v6.port().to_be();
            addr_in6.sin6_addr.s6_addr = v6.ip().octets();
            (
                storage,
                std::mem::size_of::<libc::sockaddr_in6>() as libc::socklen_t,
            )
        }
    }
}
