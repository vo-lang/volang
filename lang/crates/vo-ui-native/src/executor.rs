//! A single native VM owner thread with bounded messages and coalesced wakeups.
//!
//! The window thread drains events when notified and returns the exact exchange
//! capability. No VM, renderer or UI object crosses the thread boundary.

use crate::{Error, Exchange, ExchangeId, Exit, Session, Turn};
use std::num::NonZeroUsize;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{mpsc, Arc, Condvar, Mutex, MutexGuard};
use std::thread::{self, JoinHandle};
use std::time::Duration;
use vo_vm::vm::{JitExecutionStats, Vm};

#[derive(Clone, Copy, Debug)]
pub struct Config {
    pub quanta: NonZeroUsize,
    /// The current runtime reports Island readiness but not all native I/O
    /// completions. Poll only while actual I/O waiters exist. Must be nonzero.
    pub io_poll_interval: Duration,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            quanta: NonZeroUsize::new(8).unwrap(),
            io_poll_interval: Duration::from_millis(4),
        }
    }
}

#[derive(Debug)]
pub enum Failure {
    Setup(String),
    Execution(Error),
    WorkerPanicked(String),
}

impl std::fmt::Display for Failure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Setup(message) => write!(formatter, "native UI setup failed: {message}"),
            Self::Execution(error) => error.fmt(formatter),
            Self::WorkerPanicked(message) => {
                write!(formatter, "native UI worker panicked: {message}")
            }
        }
    }
}

impl std::error::Error for Failure {}

#[derive(Debug)]
pub struct Completion {
    /// The VM has already been disposed when this result is published.
    pub result: Result<Exit, Failure>,
    pub stats: JitExecutionStats,
}

#[derive(Debug)]
pub enum Event {
    Exchange(Exchange),
    Finished(Box<Completion>),
}

#[derive(Default)]
struct Inbox {
    pending: Option<ExchangeId>,
    reply: Option<(ExchangeId, Vec<u8>)>,
    ready: bool,
    finished: bool,
}

#[derive(Default)]
struct Control {
    inbox: Mutex<Inbox>,
    wake: Condvar,
    interrupted: Arc<AtomicBool>,
    #[cfg(test)]
    polls: std::sync::atomic::AtomicUsize,
    #[cfg(test)]
    parks: std::sync::atomic::AtomicUsize,
}

impl Control {
    fn lock(&self) -> MutexGuard<'_, Inbox> {
        self.inbox.lock().unwrap_or_else(|error| error.into_inner())
    }

    fn notify(&self) {
        self.lock().ready = true;
        self.wake.notify_one();
    }

    fn stop(&self) {
        self.interrupted.store(true, Ordering::Relaxed);
        self.notify();
    }

    fn stopped(&self) -> bool {
        self.interrupted.load(Ordering::Relaxed)
    }

    fn wait(&self, interval: Option<Duration>) {
        #[cfg(test)]
        self.parks.fetch_add(1, Ordering::Relaxed);
        let predicate =
            |inbox: &mut Inbox| !inbox.ready && inbox.reply.is_none() && !self.stopped();
        let mut inbox = if let Some(interval) = interval {
            self.wake
                .wait_timeout_while(self.lock(), interval, predicate)
                .unwrap_or_else(|error| error.into_inner())
                .0
        } else {
            self.wake
                .wait_while(self.lock(), predicate)
                .unwrap_or_else(|error| error.into_inner())
        };
        inbox.ready = false;
    }
}

/// Owns one worker. Dropping it requests interruption without blocking the UI.
/// `join` is an explicit, potentially blocking teardown for cooperative guests;
/// a host must not join an unresponsive foreign call on its window thread.
pub struct Executor {
    control: Arc<Control>,
    events: mpsc::Receiver<Event>,
    worker: Option<JoinHandle<()>>,
}

impl Executor {
    /// Construct and dispose the VM on its worker thread. The factory must
    /// return a verified, loaded, unstarted VM with the UI provider registered.
    /// `notify` only signals the platform loop; false means that loop is gone.
    pub fn spawn(
        config: Config,
        create: impl FnOnce() -> Result<Vm, String> + Send + 'static,
        notify: impl Fn() -> bool + Send + 'static,
    ) -> std::io::Result<Self> {
        if config.io_poll_interval.is_zero() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "native I/O polling interval must be nonzero",
            ));
        }
        // At most one outstanding exchange and one terminal result. Receiving
        // the exchange is required to obtain the capability for its reply.
        let (sender, events) = mpsc::sync_channel(2);
        let control = Arc::new(Control::default());
        let owner = control.clone();
        let worker = thread::Builder::new()
            .name("volang-ui".into())
            .spawn(move || {
                let completion = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let setup = (|| {
                        let mut vm = create()?;
                        vm.set_interrupt_flag(owner.interrupted.clone());
                        let wake = owner.clone();
                        vm.set_runtime_waker(Arc::new(move || wake.notify()))?;
                        Ok(Session::new(vm, config.quanta))
                    })();
                    match setup {
                        Ok(mut session) => {
                            let result = drive(&mut session, &owner, &sender, &notify, config);
                            session.stop();
                            Completion {
                                result,
                                stats: session.execution_stats(),
                            }
                        }
                        Err(error) => Completion {
                            result: Err(Failure::Setup(error)),
                            stats: JitExecutionStats::default(),
                        },
                    }
                }))
                .unwrap_or_else(|payload| Completion {
                    result: Err(Failure::WorkerPanicked(
                        payload
                            .downcast_ref::<String>()
                            .cloned()
                            .unwrap_or_else(|| {
                                payload
                                    .downcast_ref::<&str>()
                                    .copied()
                                    .unwrap_or("unknown panic")
                                    .into()
                            }),
                    )),
                    stats: JitExecutionStats::default(),
                });
                {
                    let mut inbox = owner.lock();
                    inbox.pending = None;
                    inbox.reply = None;
                    inbox.finished = true;
                }
                if sender
                    .try_send(Event::Finished(Box::new(completion)))
                    .is_ok()
                {
                    notify();
                }
            })?;
        Ok(Self {
            control,
            events,
            worker: Some(worker),
        })
    }

    pub fn try_recv(&self) -> Result<Event, mpsc::TryRecvError> {
        self.events.try_recv()
    }

    /// Reject stale, foreign, duplicate or oversized responses immediately,
    /// preserving the one pending exchange. Accepted bytes occupy one slot.
    pub fn respond(&self, id: &ExchangeId, bytes: Vec<u8>) -> Result<(), Error> {
        let mut inbox = self.control.lock();
        if self.control.stopped()
            || inbox.finished
            || !inbox.pending.as_ref().is_some_and(|pending| {
                Arc::ptr_eq(&pending.owner, &id.owner) && pending.wait == id.wait
            })
        {
            return Err(Error::StaleReply);
        }
        if bytes.len() > vo_ui_bridge::MAX_FRAME_BYTES {
            return Err(Error::ResponseTooLarge);
        }
        inbox.pending = None;
        inbox.reply = Some((id.clone(), bytes));
        self.control.wake.notify_one();
        Ok(())
    }

    pub fn stop(&self) {
        self.control.stop();
    }

    pub fn is_finished(&self) -> bool {
        self.worker.as_ref().is_none_or(JoinHandle::is_finished)
    }

    pub fn join(mut self) -> thread::Result<()> {
        self.stop();
        self.worker.take().expect("executor owns its worker").join()
    }
}

impl Drop for Executor {
    fn drop(&mut self) {
        self.stop();
    }
}

fn drive(
    session: &mut Session,
    control: &Control,
    events: &mpsc::SyncSender<Event>,
    notify: &impl Fn() -> bool,
    config: Config,
) -> Result<Exit, Failure> {
    loop {
        if control.stopped() {
            return Ok(Exit::Stopped);
        }
        let reply = control.lock().reply.take();
        if let Some((id, bytes)) = reply {
            session.respond(&id, bytes).map_err(Failure::Execution)?;
        }
        #[cfg(test)]
        control.polls.fetch_add(1, Ordering::Relaxed);
        let turn = session.poll();
        if control.stopped() {
            return Ok(Exit::Stopped);
        }
        match turn.map_err(Failure::Execution)? {
            Turn::Exchange(exchange) => {
                control.lock().pending = Some(exchange.id.clone());
                if events.try_send(Event::Exchange(exchange)).is_err() || !notify() {
                    return Ok(Exit::Stopped);
                }
            }
            Turn::Yielded => thread::yield_now(),
            Turn::Waiting => {
                control.wait(session.has_pending_io().then_some(config.io_poll_interval))
            }
            Turn::Finished(exit) => return Ok(exit),
        }
    }
}

#[cfg(test)]
mod tests;
