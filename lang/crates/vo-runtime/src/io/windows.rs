//! Windows completion driver.
//!
//! The stdlib currently performs Windows file and socket operations through
//! its synchronous platform paths. Timers still use the shared completion API,
//! so this driver keeps them non-blocking and reports unsupported raw-handle
//! submissions as explicit completions.

use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashSet};
use std::io;
use std::sync::mpsc::{self, Receiver, RecvTimeoutError, Sender};
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

use super::{Completion, CompletionData, IoCancelKey, IoToken, PendingOp, SubmitResult};

#[derive(Debug)]
pub struct WindowsDriver {
    timer_tx: Sender<TimerCommand>,
    completion_rx: Receiver<Completion>,
    pending_timers: HashSet<IoToken>,
    timer_worker: Option<JoinHandle<()>>,
}

#[derive(Debug)]
enum TimerCommand {
    Schedule { deadline: Instant, token: IoToken },
    Shutdown,
}

fn timer_worker(commands: Receiver<TimerCommand>, completions: Sender<Completion>) {
    let mut deadlines = BinaryHeap::<Reverse<(Instant, IoToken)>>::new();
    loop {
        let now = Instant::now();
        while let Some(Reverse((deadline, token))) = deadlines.peek().copied() {
            if deadline > now {
                break;
            }
            deadlines.pop();
            let _ = completions.send(Completion {
                token,
                result: Ok(CompletionData::Timer),
            });
        }

        let command = if let Some(Reverse((deadline, _))) = deadlines.peek().copied() {
            match commands.recv_timeout(deadline.saturating_duration_since(Instant::now())) {
                Ok(command) => Some(command),
                Err(RecvTimeoutError::Timeout) => None,
                Err(RecvTimeoutError::Disconnected) => break,
            }
        } else {
            match commands.recv() {
                Ok(command) => Some(command),
                Err(_) => break,
            }
        };

        match command {
            Some(TimerCommand::Schedule { deadline, token }) => {
                deadlines.push(Reverse((deadline, token)));
            }
            Some(TimerCommand::Shutdown) => break,
            None => {}
        }
    }
}

impl WindowsDriver {
    pub fn new() -> io::Result<Self> {
        let (completion_tx, completion_rx) = mpsc::channel();
        let (timer_tx, timer_rx) = mpsc::channel();
        let worker_completions = completion_tx.clone();
        let timer_worker = std::thread::Builder::new()
            .name("vo-windows-timer".to_string())
            .spawn(move || timer_worker(timer_rx, worker_completions))?;
        Ok(Self {
            timer_tx,
            completion_rx,
            pending_timers: HashSet::new(),
            timer_worker: Some(timer_worker),
        })
    }

    pub fn submit(&mut self, op: PendingOp) -> SubmitResult {
        SubmitResult::Completed(Completion {
            token: op.token,
            result: Err(io::Error::new(
                io::ErrorKind::Unsupported,
                format!(
                    "raw-handle async {:?} submissions are unavailable on Windows; use a provider-owned operation",
                    op.kind
                ),
            )),
        })
    }

    pub fn submit_timer(&mut self, token: IoToken, duration_ns: i64) -> SubmitResult {
        if duration_ns <= 0 {
            return SubmitResult::Completed(Completion {
                token,
                result: Ok(CompletionData::Timer),
            });
        }

        let duration = Duration::from_nanos(duration_ns as u64);
        let Some(deadline) = Instant::now().checked_add(duration) else {
            return SubmitResult::Completed(Completion {
                token,
                result: Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "timer duration exceeds platform deadline range",
                )),
            });
        };
        self.pending_timers.insert(token);
        if self
            .timer_tx
            .send(TimerCommand::Schedule { deadline, token })
            .is_err()
        {
            self.pending_timers.remove(&token);
            return SubmitResult::Completed(Completion {
                token,
                result: Err(io::Error::new(
                    io::ErrorKind::BrokenPipe,
                    "Windows timer worker is unavailable",
                )),
            });
        }
        SubmitResult::Pending
    }

    pub fn poll(&mut self) -> Vec<Completion> {
        let mut completions = Vec::new();
        while let Ok(completion) = self.completion_rx.try_recv() {
            if self.pending_timers.remove(&completion.token) {
                completions.push(completion);
            }
        }
        completions
    }

    pub fn cancel(&mut self, _key: IoCancelKey) -> Vec<Completion> {
        // The current Windows driver completes regular operations
        // synchronously, so there are no handle-bound pending operations to
        // synthesize cancellation for.
        Vec::new()
    }

    pub fn has_pending(&self) -> bool {
        !self.pending_timers.is_empty()
    }
}

impl Drop for WindowsDriver {
    fn drop(&mut self) {
        let _ = self.timer_tx.send(TimerCommand::Shutdown);
        if let Some(worker) = self.timer_worker.take() {
            let _ = worker.join();
        }
    }
}
