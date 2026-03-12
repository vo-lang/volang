//! Island transport abstraction for cross-island communication.
//!
//! Implementations:
//! - `InThreadTransport`: wraps `std::sync::mpsc::channel` (existing behavior)
//! - `TauriTransport`: Tauri IPC (Studio native ↔ WebView) — external crate
//! - `WorkerTransport`: `postMessage` (Playground main ↔ Worker) — JS side

#[cfg(feature = "std")]
use crate::island::IslandCommand;

#[cfg(feature = "std")]
use std::sync::mpsc::{Receiver, Sender};
#[cfg(feature = "std")]
use std::time::Duration;

/// Transport errors.
#[derive(Debug)]
pub enum TransportError {
    /// Channel disconnected (peer dropped).
    Disconnected,
    /// Timed out waiting for a message.
    Timeout,
}

#[cfg(feature = "std")]
impl<T> From<std::sync::mpsc::SendError<T>> for TransportError {
    fn from(_: std::sync::mpsc::SendError<T>) -> Self {
        TransportError::Disconnected
    }
}

/// Abstract transport for cross-island communication.
///
/// Each island holds one transport instance for receiving commands.
/// Senders are modeled separately via `IslandSender`.
#[cfg(feature = "std")]
pub trait IslandTransport: Send + 'static {
    /// Try to receive a command without blocking.
    fn try_recv(&self) -> Result<Option<IslandCommand>, TransportError>;

    /// Block until a command arrives or timeout expires.
    fn recv_timeout(&self, timeout: Duration) -> Result<IslandCommand, TransportError>;

    /// Block until a command arrives.
    fn recv(&self) -> Result<IslandCommand, TransportError>;
}

// ---------------------------------------------------------------------------
// InThreadTransport — wraps std::sync::mpsc, same behavior as current islands
// ---------------------------------------------------------------------------

// =============================================================================
// IslandSender trait — unified command dispatch
// =============================================================================

/// Trait for sending commands to an island. Implementations:
/// - InThreadSender: same-process mpsc (existing)
/// - Future: Tauri IPC, Worker postMessage, etc.
///
/// Stored as `Arc<dyn IslandSender>` in island_senders map.
/// No `is_in_process()` — all cross-island ops go through messages.
#[cfg(feature = "std")]
pub trait IslandSender: Send + Sync {
    fn send_command(&self, cmd: IslandCommand) -> Result<(), TransportError>;
}

/// Sender half of an in-thread transport. Cloneable so multiple islands can
/// send commands to the same target.
#[cfg(feature = "std")]
#[derive(Clone)]
pub struct InThreadSender {
    tx: Sender<IslandCommand>,
}

#[cfg(feature = "std")]
impl InThreadSender {
    /// Send a command through the underlying mpsc channel.
    fn send(&self, cmd: IslandCommand) -> Result<(), TransportError> {
        self.tx.send(cmd).map_err(|_| TransportError::Disconnected)
    }
}

#[cfg(feature = "std")]
impl IslandSender for InThreadSender {
    fn send_command(&self, cmd: IslandCommand) -> Result<(), TransportError> {
        self.send(cmd)
    }
}

/// Receiver half of an in-thread transport. Owned by the island thread.
#[cfg(feature = "std")]
pub struct InThreadTransport {
    rx: Receiver<IslandCommand>,
}

#[cfg(feature = "std")]
impl InThreadTransport {
    /// Create a paired (sender, receiver) transport.
    pub fn new() -> (InThreadSender, Self) {
        let (tx, rx) = std::sync::mpsc::channel();
        (InThreadSender { tx }, InThreadTransport { rx })
    }
}

#[cfg(feature = "std")]
impl IslandTransport for InThreadTransport {
    fn try_recv(&self) -> Result<Option<IslandCommand>, TransportError> {
        match self.rx.try_recv() {
            Ok(cmd) => Ok(Some(cmd)),
            Err(std::sync::mpsc::TryRecvError::Empty) => Ok(None),
            Err(std::sync::mpsc::TryRecvError::Disconnected) => Err(TransportError::Disconnected),
        }
    }

    fn recv_timeout(&self, timeout: Duration) -> Result<IslandCommand, TransportError> {
        self.rx
            .recv_timeout(timeout)
            .map_err(|e| match e {
                std::sync::mpsc::RecvTimeoutError::Timeout => TransportError::Timeout,
                std::sync::mpsc::RecvTimeoutError::Disconnected => TransportError::Disconnected,
            })
    }

    fn recv(&self) -> Result<IslandCommand, TransportError> {
        self.rx.recv().map_err(|_| TransportError::Disconnected)
    }
}
