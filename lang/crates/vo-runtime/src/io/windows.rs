//! Windows I/O driver using IOCP.
//!
//! TODO: Not yet implemented.

use std::io;
use std::time::Duration;

use super::{Completion, CompletionData, IoHandle, IoToken, PendingOp, SubmitResult};

#[derive(Debug)]
pub struct WindowsDriver;

impl WindowsDriver {
    pub fn new() -> io::Result<Self> {
        Ok(Self)
    }

    pub fn submit(&mut self, op: PendingOp) -> SubmitResult {
        SubmitResult::Completed(Completion {
            token: op.token,
            result: Err(io::Error::new(
                io::ErrorKind::Unsupported,
                format!("Windows async {:?} not yet implemented", op.kind),
            )),
        })
    }

    pub fn submit_timer(&mut self, token: IoToken, duration_ns: i64) -> SubmitResult {
        if duration_ns > 0 {
            std::thread::sleep(Duration::from_nanos(duration_ns as u64));
        }

        SubmitResult::Completed(Completion {
            token,
            result: Ok(CompletionData::Timer),
        })
    }

    pub fn poll(&mut self) -> Vec<Completion> {
        Vec::new()
    }

    pub fn cancel(&mut self, _handle: IoHandle) {}

    pub fn has_pending(&self) -> bool {
        false
    }
}
