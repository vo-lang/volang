//! Windows I/O driver using IOCP.
//!
//! TODO: Not yet implemented.

use std::collections::HashMap;
use std::io;

use super::{Completion, IoHandle, IoToken, PendingOp, SubmitResult};

#[derive(Debug)]
pub struct WindowsDriver;

impl WindowsDriver {
    pub fn new() -> io::Result<Self> {
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "Windows IOCP driver not yet implemented",
        ))
    }

    pub fn submit(&mut self, _op: PendingOp) -> SubmitResult {
        unreachable!("Windows IOCP driver not yet implemented")
    }

    pub fn poll(&mut self) -> Vec<Completion> {
        unreachable!("Windows IOCP driver not yet implemented")
    }

    pub fn cancel(&mut self, _handle: IoHandle) {
        unreachable!("Windows IOCP driver not yet implemented")
    }

    pub fn has_pending(&self) -> bool {
        false
    }
}
