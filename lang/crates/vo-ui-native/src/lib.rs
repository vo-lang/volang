//! Renderer-independent native execution for `ui/next/host.Exchange`.
//!
//! The caller supplies a verified, loaded, unstarted VM and owns the platform
//! event loop. This crate owns its execution and opaque reply identities; the
//! guest and renderer retain ownership of the UI protocol and component state.

use std::fmt;
use std::num::NonZeroUsize;
use std::sync::Arc;
use vo_vm::scheduler::{FiberWakeKey, HostWaitKey};
use vo_vm::vm::{JitExecutionStats, SchedulingOutcome, Vm, VmError};

pub mod executor;

/// A reply capability for one exchange in one live session. Keeping an old
/// capability alive also keeps its owner identity alive, preventing reuse when
/// another window happens to allocate the same VM-local scheduler identities.
#[derive(Clone, Debug)]
pub struct ExchangeId {
    owner: Arc<()>,
    wait: HostWaitKey,
}

#[derive(Debug)]
pub struct Exchange {
    pub id: ExchangeId,
    pub bytes: Vec<u8>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Exit {
    Completed,
    Code(i32),
    Stopped,
    Failed,
}

#[derive(Debug)]
pub enum Turn {
    /// Delivered exactly once. Reply through `Session::respond`.
    Exchange(Exchange),
    /// Runnable work remains; schedule another platform turn.
    Yielded,
    /// Wait for the outstanding reply or native runtime readiness. A native
    /// I/O integration must arrange another poll when its work may be ready.
    Waiting,
    Finished(Exit),
}

#[derive(Debug)]
pub enum Error {
    Vm(VmError),
    InvalidExchange(&'static str),
    StaleReply,
    ResponseTooLarge,
}

impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Vm(error) => write!(formatter, "native UI execution failed: {error:?}"),
            Self::InvalidExchange(reason) => {
                write!(formatter, "invalid native UI exchange: {reason}")
            }
            Self::StaleReply => formatter
                .write_str("native UI reply belongs to no pending exchange in this session"),
            Self::ResponseTooLarge => {
                formatter.write_str("native UI response exceeds the frame limit")
            }
        }
    }
}

impl std::error::Error for Error {}

/// One writer for one native VM/JIT/Native AOT application. There are no
/// renderer, compiler, old UI kernel, window, timer or background-thread owners
/// in this session. Hosts can install VM interruption/readiness hooks before
/// transferring ownership here; callbacks must only notify its owning executor.
pub struct Session {
    vm: Option<Vm>,
    owner: Arc<()>,
    pending: Option<HostWaitKey>,
    writer: Option<FiberWakeKey>,
    started: bool,
    quanta: NonZeroUsize,
    exit: Option<Exit>,
    final_stats: JitExecutionStats,
}

impl Session {
    pub fn new(vm: Vm, quanta: NonZeroUsize) -> Self {
        Self {
            vm: Some(vm),
            owner: Arc::new(()),
            pending: None,
            writer: None,
            started: false,
            quanta,
            exit: None,
            final_stats: JitExecutionStats::default(),
        }
    }

    /// Run at most the configured scheduler turns, spawning the entry exactly
    /// once. This is a cooperative scheduling budget, not a wall-clock deadline.
    /// Polling while a reply is pending advances other runnable goroutines and
    /// native I/O completions without redelivering the outstanding exchange.
    /// Execution/protocol errors end the session and release its VM immediately.
    pub fn poll(&mut self) -> Result<Turn, Error> {
        if let Some(exit) = self.exit {
            return Ok(Turn::Finished(exit));
        }
        let result = self.advance();
        if result.is_err() {
            self.finish(Exit::Failed);
        }
        result
    }

    fn advance(&mut self) -> Result<Turn, Error> {
        let vm = self.vm.as_mut().expect("live session owns its VM");
        let outcome = if self.started {
            vm.run_scheduled_with_budget(self.quanta.get())
        } else {
            self.started = true;
            vm.run_with_budget(self.quanta.get())
        }
        .map_err(Error::Vm)?;

        match outcome {
            SchedulingOutcome::Completed => return Ok(self.finish(Exit::Completed)),
            SchedulingOutcome::Exited(code) => return Ok(self.finish(Exit::Code(code))),
            SchedulingOutcome::Blocked => return Err(Error::Vm(vm.deadlock_err())),
            SchedulingOutcome::Panicked => {
                return Err(vm
                    .take_bounded_panic()
                    .map(Error::Vm)
                    .unwrap_or(Error::InvalidExchange("guest panic has no diagnostics")))
            }
            _ => {}
        }

        // Inspect the complete scheduler snapshot before publishing any bytes:
        // the VM's output slot cannot represent concurrent Exchange writers.
        let waits = vm.take_pending_host_events();
        let bytes = vm.take_host_output();
        if !waits.is_empty() || bytes.is_some() {
            if waits.len() != 1 || !waits[0].source.is_gui_event_replay() {
                return Err(Error::InvalidExchange("expected one UI replay waiter"));
            }
            let wait = waits[0].key;
            if let Some(pending) = self.pending {
                if wait != pending || bytes.is_some() {
                    return Err(Error::InvalidExchange("pending writer changed"));
                }
                return Ok(if vm.has_runnable_fibers() {
                    Turn::Yielded
                } else {
                    Turn::Waiting
                });
            }
            if self.writer.is_some_and(|writer| writer != wait.wake_key) {
                return Err(Error::InvalidExchange("multiple UI writer goroutines"));
            }
            let bytes = bytes.ok_or(Error::InvalidExchange("waiter has no output"))?;
            if bytes.len() > vo_ui_bridge::MAX_FRAME_BYTES {
                return Err(Error::InvalidExchange("output exceeds the frame limit"));
            }
            self.writer = Some(wait.wake_key);
            self.pending = Some(wait);
            return Ok(Turn::Exchange(Exchange {
                id: ExchangeId {
                    owner: self.owner.clone(),
                    wait,
                },
                bytes,
            }));
        }

        match outcome {
            SchedulingOutcome::SuspendedForHostEvents => {
                Err(Error::InvalidExchange("missing UI replay waiter"))
            }
            SchedulingOutcome::Suspended => Ok(if vm.has_runnable_fibers() {
                Turn::Yielded
            } else {
                Turn::Waiting
            }),
            _ => unreachable!("terminal outcomes handled above"),
        }
    }

    /// Resume exactly the exchange issued by this session. Invalid, duplicate,
    /// cross-window and oversized replies leave the pending request intact.
    /// The owning event loop calls `poll` to execute the accepted response.
    pub fn respond(&mut self, id: &ExchangeId, bytes: Vec<u8>) -> Result<(), Error> {
        if !Arc::ptr_eq(&self.owner, &id.owner) || self.pending != Some(id.wait) {
            return Err(Error::StaleReply);
        }
        if bytes.len() > vo_ui_bridge::MAX_FRAME_BYTES {
            return Err(Error::ResponseTooLarge);
        }
        let vm = self.vm.as_mut().expect("pending exchange owns its VM");
        if !vm.wake_host_event_with_data(id.wait, bytes) {
            self.finish(Exit::Failed);
            return Err(Error::InvalidExchange(
                "scheduler rejected its pending reply",
            ));
        }
        self.pending = None;
        Ok(())
    }

    /// Immediately release native execution and invalidate late replies. Hosts
    /// close their renderer/services separately. Normal guest cleanup follows
    /// the protocol's orderly close; forced stop cannot run guest defer bodies.
    pub fn stop(&mut self) {
        if self.exit.is_none() {
            self.finish(Exit::Stopped);
        }
    }

    pub fn execution_stats(&self) -> JitExecutionStats {
        self.vm
            .as_ref()
            .map_or(self.final_stats, Vm::jit_execution_stats)
    }

    /// Native I/O needs a readiness poll even while a renderer reply is pending.
    /// An idle application with only UI input outstanding returns false.
    pub fn has_pending_io(&self) -> bool {
        self.vm.as_ref().is_some_and(Vm::has_pending_io)
    }

    fn finish(&mut self, exit: Exit) -> Turn {
        if let Some(vm) = self.vm.take() {
            self.final_stats = vm.jit_execution_stats();
        }
        self.pending = None;
        self.exit = Some(exit);
        Turn::Finished(exit)
    }
}

#[cfg(test)]
mod tests;
