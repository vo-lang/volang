use std::collections::HashMap;
use std::io;

use super::{Completion, IoHandle, IoKind, IoToken};

#[derive(Debug)]
pub struct UnixPoller {
    waiters: HashMap<i32, (IoToken, IoKind)>,

    #[cfg(target_os = "linux")]
    epoll_fd: i32,

    #[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd", target_os = "netbsd"))]
    kqueue_fd: i32,
}

impl UnixPoller {
    pub fn new() -> io::Result<Self> {
        #[cfg(target_os = "linux")]
        {
            let epoll_fd = unsafe { libc::epoll_create1(libc::EPOLL_CLOEXEC) };
            if epoll_fd < 0 {
                return Err(io::Error::last_os_error());
            }
            return Ok(Self {
                waiters: HashMap::new(),
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
                waiters: HashMap::new(),
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
                "I/O polling not supported on this platform",
            ))
        }
    }

    pub fn register(&mut self, handle: IoHandle, token: IoToken, kind: IoKind) -> io::Result<()> {
        let fd = i32::try_from(handle).map_err(|_| {
            io::Error::new(io::ErrorKind::InvalidInput, "invalid unix fd handle")
        })?;
        self.waiters.insert(fd, (token, kind));

        #[cfg(target_os = "linux")]
        {
            let events = match kind {
                IoKind::Read | IoKind::Accept => libc::EPOLLIN | libc::EPOLLONESHOT,
                IoKind::Write | IoKind::Connect => libc::EPOLLOUT | libc::EPOLLONESHOT,
            };

            let mut event = libc::epoll_event {
                events: events as u32,
                u64: fd as u64,
            };

            let ret = unsafe { libc::epoll_ctl(self.epoll_fd, libc::EPOLL_CTL_ADD, fd, &mut event) };
            if ret < 0 {
                let err = io::Error::last_os_error();
                if err.raw_os_error() == Some(libc::EEXIST) {
                    let ret =
                        unsafe { libc::epoll_ctl(self.epoll_fd, libc::EPOLL_CTL_MOD, fd, &mut event) };
                    if ret < 0 {
                        self.waiters.remove(&fd);
                        return Err(io::Error::last_os_error());
                    }
                } else {
                    self.waiters.remove(&fd);
                    return Err(err);
                }
            }
        }

        #[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd", target_os = "netbsd"))]
        {
            let filter = match kind {
                IoKind::Read | IoKind::Accept => libc::EVFILT_READ,
                IoKind::Write | IoKind::Connect => libc::EVFILT_WRITE,
            };

            let event = libc::kevent {
                ident: fd as usize,
                filter,
                flags: libc::EV_ADD | libc::EV_ONESHOT,
                fflags: 0,
                data: 0,
                udata: std::ptr::null_mut(),
            };

            let ret = unsafe {
                libc::kevent(
                    self.kqueue_fd,
                    &event,
                    1,
                    std::ptr::null_mut(),
                    0,
                    std::ptr::null(),
                )
            };
            if ret < 0 {
                self.waiters.remove(&fd);
                return Err(io::Error::last_os_error());
            }
        }

        Ok(())
    }

    pub fn unregister(&mut self, handle: IoHandle) {
        let fd = match i32::try_from(handle) {
            Ok(fd) => fd,
            Err(_) => return,
        };
        let kind = self.waiters.remove(&fd).map(|(_, k)| k);

        #[cfg(target_os = "linux")]
        {
            unsafe {
                libc::epoll_ctl(self.epoll_fd, libc::EPOLL_CTL_DEL, fd, std::ptr::null_mut());
            }
            let _ = kind;
        }

        #[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd", target_os = "netbsd"))]
        {
            let filter = match kind {
                Some(IoKind::Read) | Some(IoKind::Accept) => Some(libc::EVFILT_READ),
                Some(IoKind::Write) | Some(IoKind::Connect) => Some(libc::EVFILT_WRITE),
                None => None,
            };

            match filter {
                Some(filter) => {
                    let event = libc::kevent {
                        ident: fd as usize,
                        filter,
                        flags: libc::EV_DELETE,
                        fflags: 0,
                        data: 0,
                        udata: std::ptr::null_mut(),
                    };
                    unsafe {
                        libc::kevent(
                            self.kqueue_fd,
                            &event,
                            1,
                            std::ptr::null_mut(),
                            0,
                            std::ptr::null(),
                        );
                    }
                }
                None => {
                    let event_read = libc::kevent {
                        ident: fd as usize,
                        filter: libc::EVFILT_READ,
                        flags: libc::EV_DELETE,
                        fflags: 0,
                        data: 0,
                        udata: std::ptr::null_mut(),
                    };
                    let event_write = libc::kevent {
                        ident: fd as usize,
                        filter: libc::EVFILT_WRITE,
                        flags: libc::EV_DELETE,
                        fflags: 0,
                        data: 0,
                        udata: std::ptr::null_mut(),
                    };
                    unsafe {
                        libc::kevent(
                            self.kqueue_fd,
                            &event_read,
                            1,
                            std::ptr::null_mut(),
                            0,
                            std::ptr::null(),
                        );
                        libc::kevent(
                            self.kqueue_fd,
                            &event_write,
                            1,
                            std::ptr::null_mut(),
                            0,
                            std::ptr::null(),
                        );
                    }
                }
            }
        }
    }

    pub fn poll(&mut self) -> Vec<Completion> {
        let mut ready = Vec::new();

        #[cfg(target_os = "linux")]
        {
            let mut events: [libc::epoll_event; 64] = unsafe { std::mem::zeroed() };
            let n = unsafe { libc::epoll_wait(self.epoll_fd, events.as_mut_ptr(), 64, 0) };
            if n > 0 {
                for i in 0..n as usize {
                    let fd = events[i].u64 as i32;
                    if let Some((token, kind)) = self.waiters.remove(&fd) {
                        ready.push(Completion {
                            token,
                            kind,
                            result: Ok(0),
                            extra: fd as u64,
                        });
                    }
                }
            }
        }

        #[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd", target_os = "netbsd"))]
        {
            let mut events: [libc::kevent; 64] = unsafe { std::mem::zeroed() };
            let timeout = libc::timespec {
                tv_sec: 0,
                tv_nsec: 0,
            };
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
                for i in 0..n as usize {
                    let fd = events[i].ident as i32;
                    if let Some((token, kind)) = self.waiters.remove(&fd) {
                        ready.push(Completion {
                            token,
                            kind,
                            result: Ok(0),
                            extra: fd as u64,
                        });
                    }
                }
            }
        }

        #[cfg(unix)]
        {
            let fds: Vec<i32> = self.waiters.keys().copied().collect();
            for fd in fds {
                let ret = unsafe { libc::fcntl(fd, libc::F_GETFD) };
                if ret == -1 {
                    let err = io::Error::last_os_error();
                    if err.raw_os_error() == Some(libc::EBADF) {
                        if let Some((token, kind)) = self.waiters.remove(&fd) {
                            ready.push(Completion {
                                token,
                                kind,
                                result: Err(err),
                                extra: fd as u64,
                            });
                        }
                    }
                }
            }
        }

        ready
    }

    pub fn has_waiters(&self) -> bool {
        !self.waiters.is_empty()
    }
}

impl Drop for UnixPoller {
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
