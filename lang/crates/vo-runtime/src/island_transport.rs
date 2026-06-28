//! Island transport abstraction for cross-island communication.
//!
//! Implementations:
//! - `InThreadTransport`: wraps `std::sync::mpsc::channel` (existing behavior)
//! - `TauriTransport`: Tauri IPC (Studio native ↔ WebView) — external crate
//! - `WorkerTransport`: `postMessage` (Playground main ↔ Worker) — JS side

#[cfg(feature = "std")]
use crate::island::{IslandCommand, IslandCommandEnvelope};

#[cfg(feature = "std")]
use std::sync::mpsc::{Receiver, Sender};
#[cfg(feature = "std")]
use std::sync::{Arc, Condvar, Mutex, MutexGuard};
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
    fn try_recv(&self) -> Result<Option<IslandCommandEnvelope>, TransportError>;

    /// Block until a command arrives or timeout expires.
    fn recv_timeout(&self, timeout: Duration) -> Result<IslandCommandEnvelope, TransportError>;

    /// Block until a command arrives.
    fn recv(&self) -> Result<IslandCommandEnvelope, TransportError>;
}

// ---------------------------------------------------------------------------
// InThreadTransport — wraps std::sync::mpsc, same behavior as current islands
// ---------------------------------------------------------------------------

// =============================================================================
// IslandSender trait — unified command dispatch
// =============================================================================

#[cfg(feature = "std")]
pub trait IslandSendReservation: Send {
    fn send(self: Box<Self>, source_island_id: u32, cmd: IslandCommand);
}

/// Trait for sending commands to an island. Implementations:
/// - InThreadSender: same-process mpsc (existing)
/// - Future: Tauri IPC, Worker postMessage, etc.
///
/// Stored as `Arc<dyn IslandSender>` in island_senders map.
/// No `is_in_process()` — all cross-island ops go through messages.
#[cfg(feature = "std")]
pub trait IslandSender: Send + Sync {
    fn reserve_send_command(&self) -> Result<Box<dyn IslandSendReservation>, TransportError>;

    fn preflight_send_command(&self) -> Result<(), TransportError> {
        let reservation = self.reserve_send_command()?;
        drop(reservation);
        Ok(())
    }

    fn send_command(
        &self,
        source_island_id: u32,
        cmd: IslandCommand,
    ) -> Result<(), TransportError> {
        let reservation = self.reserve_send_command()?;
        reservation.send(source_island_id, cmd);
        Ok(())
    }
}

#[cfg(feature = "std")]
#[derive(Default)]
struct InThreadSendState {
    state: Mutex<InThreadSendStateInner>,
    idle: Condvar,
}

#[cfg(feature = "std")]
#[derive(Default)]
struct InThreadSendStateInner {
    closed: bool,
    reservations: usize,
}

#[cfg(feature = "std")]
impl InThreadSendState {
    fn lock(&self) -> MutexGuard<'_, InThreadSendStateInner> {
        match self.state.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        }
    }

    fn reserve(&self) -> Result<(), TransportError> {
        let mut state = self.lock();
        if state.closed {
            return Err(TransportError::Disconnected);
        }
        state.reservations += 1;
        Ok(())
    }

    fn release(&self) {
        let mut state = self.lock();
        state.reservations = state.reservations.saturating_sub(1);
        if state.reservations == 0 {
            self.idle.notify_all();
        }
    }

    fn close(&self) {
        let mut state = self.lock();
        while state.reservations != 0 {
            state = match self.idle.wait(state) {
                Ok(guard) => guard,
                Err(poisoned) => poisoned.into_inner(),
            };
        }
        state.closed = true;
    }
}

#[cfg(feature = "std")]
struct InThreadSendReservation {
    tx: Sender<IslandCommandEnvelope>,
    state: Arc<InThreadSendState>,
    active: bool,
}

#[cfg(feature = "std")]
impl InThreadSendReservation {
    fn new(tx: Sender<IslandCommandEnvelope>, state: Arc<InThreadSendState>) -> Self {
        Self {
            tx,
            state,
            active: true,
        }
    }
}

#[cfg(feature = "std")]
impl Drop for InThreadSendReservation {
    fn drop(&mut self) {
        if self.active {
            self.state.release();
            self.active = false;
        }
    }
}

#[cfg(feature = "std")]
impl IslandSendReservation for InThreadSendReservation {
    fn send(mut self: Box<Self>, source_island_id: u32, cmd: IslandCommand) {
        let result = self
            .tx
            .send(IslandCommandEnvelope::new(source_island_id, cmd));
        debug_assert!(
            result.is_ok(),
            "reserved in-thread island send must not disconnect before commit"
        );
        self.state.release();
        self.active = false;
    }
}

/// Sender half of an in-thread transport. Cloneable so multiple islands can
/// send commands to the same target.
#[cfg(feature = "std")]
#[derive(Clone)]
pub struct InThreadSender {
    tx: Sender<IslandCommandEnvelope>,
    send_state: Arc<InThreadSendState>,
}

#[cfg(feature = "std")]
impl IslandSender for InThreadSender {
    fn reserve_send_command(&self) -> Result<Box<dyn IslandSendReservation>, TransportError> {
        self.send_state.reserve()?;
        Ok(Box::new(InThreadSendReservation::new(
            self.tx.clone(),
            self.send_state.clone(),
        )))
    }
}

/// Receiver half of an in-thread transport. Owned by the island thread.
#[cfg(feature = "std")]
pub struct InThreadTransport {
    rx: Receiver<IslandCommandEnvelope>,
    send_state: Arc<InThreadSendState>,
}

#[cfg(feature = "std")]
impl InThreadTransport {
    /// Create a paired (sender, receiver) transport.
    pub fn new() -> (InThreadSender, Self) {
        let (tx, rx) = std::sync::mpsc::channel();
        let send_state = Arc::new(InThreadSendState::default());
        (
            InThreadSender {
                tx,
                send_state: send_state.clone(),
            },
            InThreadTransport { rx, send_state },
        )
    }
}

#[cfg(feature = "std")]
impl Drop for InThreadTransport {
    fn drop(&mut self) {
        self.send_state.close();
    }
}

#[cfg(feature = "std")]
impl IslandTransport for InThreadTransport {
    fn try_recv(&self) -> Result<Option<IslandCommandEnvelope>, TransportError> {
        match self.rx.try_recv() {
            Ok(envelope) => Ok(Some(envelope)),
            Err(std::sync::mpsc::TryRecvError::Empty) => Ok(None),
            Err(std::sync::mpsc::TryRecvError::Disconnected) => Err(TransportError::Disconnected),
        }
    }

    fn recv_timeout(&self, timeout: Duration) -> Result<IslandCommandEnvelope, TransportError> {
        self.rx.recv_timeout(timeout).map_err(|e| match e {
            std::sync::mpsc::RecvTimeoutError::Timeout => TransportError::Timeout,
            std::sync::mpsc::RecvTimeoutError::Disconnected => TransportError::Disconnected,
        })
    }

    fn recv(&self) -> Result<IslandCommandEnvelope, TransportError> {
        self.rx.recv().map_err(|_| TransportError::Disconnected)
    }
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;
    use std::sync::mpsc;
    use std::time::Duration;

    #[test]
    fn vm_runtime_remote_reservation_058_keeps_in_thread_route_alive_until_commit() {
        let (sender, transport) = InThreadTransport::new();
        let reservation = sender
            .reserve_send_command()
            .expect("receiver is live before transport drop");
        let (started_tx, started_rx) = mpsc::channel();
        let (done_tx, done_rx) = mpsc::channel();

        let dropper = std::thread::spawn(move || {
            started_tx.send(()).expect("signal drop start");
            drop(transport);
            done_tx.send(()).expect("signal drop done");
        });
        started_rx.recv().expect("dropper started");

        assert!(
            done_rx.recv_timeout(Duration::from_millis(50)).is_err(),
            "transport drop must wait until the active send reservation commits or drops"
        );

        reservation.send(3, IslandCommand::Shutdown);
        done_rx
            .recv_timeout(Duration::from_secs(1))
            .expect("transport drop completes after reservation commit");
        dropper.join().expect("dropper thread");
        assert!(matches!(
            sender.reserve_send_command(),
            Err(TransportError::Disconnected)
        ));
    }
}
