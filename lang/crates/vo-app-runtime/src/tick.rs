//! Thread-based tick loop provider for native hosts.
//!
//! Implements [`vo_ext::host::tick::TickProvider`] by spawning a timing thread
//! per active loop.  Each thread sleeps ~16 ms (≈60 fps), accumulates elapsed
//! time, and invokes a user-supplied callback with `(loop_id, dt_ms)`.
//!
//! The callback is responsible for enqueuing whatever event the host's event
//! loop expects (e.g. sending over a channel).

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

// ── Tick-loop state machine ─────────────────────────────────────────────────

#[derive(Default)]
struct TickLoopState {
    pending_event: bool,
    accumulated_dt_ms: f64,
}

/// Per-loop control handle shared between the timing thread and the host
/// event loop.
pub struct TickLoopControl {
    stop: AtomicBool,
    state: Mutex<TickLoopState>,
}

impl TickLoopControl {
    fn new() -> Self {
        Self {
            stop: AtomicBool::new(false),
            state: Mutex::new(TickLoopState::default()),
        }
    }

    pub fn request_stop(&self) {
        self.stop.store(true, Ordering::Relaxed);
    }

    pub fn should_stop(&self) -> bool {
        self.stop.load(Ordering::Relaxed)
    }

    /// Called by the timing thread to accumulate dt.  Returns `true` if this
    /// is the first accumulation since the last `take_pending_tick` (i.e. the
    /// host should be notified).
    fn accumulate_tick(&self, dt_ms: f64) -> bool {
        let mut state = self.state.lock().unwrap();
        state.accumulated_dt_ms += dt_ms;
        if state.pending_event {
            false
        } else {
            state.pending_event = true;
            true
        }
    }

    /// Consume the accumulated dt.  Returns `None` if the loop was stopped or
    /// no time has accumulated.
    pub fn take_pending_tick(&self) -> Option<f64> {
        let mut state = self.state.lock().unwrap();
        if self.should_stop() {
            state.pending_event = false;
            state.accumulated_dt_ms = 0.0;
            return None;
        }
        if state.accumulated_dt_ms <= 0.0 {
            state.pending_event = false;
            return None;
        }
        let dt_ms = state.accumulated_dt_ms;
        state.accumulated_dt_ms = 0.0;
        state.pending_event = false;
        Some(dt_ms)
    }

    /// Put back an unhandled tick (e.g. when the VM was not waiting for
    /// events).  Returns `true` if the host should be re-notified.
    pub fn restore_unhandled_tick(&self, dt_ms: f64) -> bool {
        if dt_ms <= 0.0 {
            return false;
        }
        let mut state = self.state.lock().unwrap();
        if self.should_stop() {
            state.pending_event = false;
            state.accumulated_dt_ms = 0.0;
            return false;
        }
        state.accumulated_dt_ms += dt_ms;
        if state.pending_event {
            false
        } else {
            state.pending_event = true;
            true
        }
    }
}

// ── NativeTickProvider ──────────────────────────────────────────────────────

type TickCallback = Arc<dyn Fn(i32, Arc<TickLoopControl>) + Send + Sync + 'static>;

/// A thread-based tick loop provider that implements
/// [`vo_ext::host::tick::TickProvider`].
///
/// When the guest VM calls `startTickLoop(id)`, a background thread is
/// spawned that fires at ~60 fps.  The user-supplied callback is invoked
/// each time a new tick batch is ready.
///
/// # Example
///
/// ```ignore
/// let tick_provider = NativeTickProvider::new(move |loop_id, control| {
///     let _ = event_tx.send(MyEvent::Tick { loop_id, control });
/// });
/// ```
#[derive(Clone)]
pub struct NativeTickProvider {
    callback: TickCallback,
    loop_controls: Arc<Mutex<HashMap<i32, Arc<TickLoopControl>>>>,
}

impl NativeTickProvider {
    pub fn new<F>(callback: F) -> Self
    where
        F: Fn(i32, Arc<TickLoopControl>) + Send + Sync + 'static,
    {
        Self {
            callback: Arc::new(callback),
            loop_controls: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Re-enqueue a tick event for a loop whose tick was not consumed
    /// (e.g. because the VM was not waiting for events).
    pub fn enqueue_tick(&self, loop_id: i32, control: &Arc<TickLoopControl>) -> bool {
        if control.should_stop() {
            return false;
        }
        (self.callback)(loop_id, Arc::clone(control));
        true
    }
}

impl vo_ext::host::tick::TickProvider for NativeTickProvider {
    fn start_tick_loop(&self, id: i32) {
        let control = Arc::new(TickLoopControl::new());
        if let Some(existing) = self
            .loop_controls
            .lock()
            .unwrap()
            .insert(id, Arc::clone(&control))
        {
            existing.request_stop();
        }
        let provider = self.clone();
        std::thread::spawn(move || {
            let frame_dur = Duration::from_millis(16);
            let mut last = Instant::now();
            loop {
                std::thread::sleep(frame_dur);
                if control.should_stop() {
                    break;
                }
                let now = Instant::now();
                let dt_ms = now.duration_since(last).as_secs_f64() * 1000.0;
                last = now;
                if control.accumulate_tick(dt_ms) {
                    (provider.callback)(id, Arc::clone(&control));
                }
            }
        });
    }

    fn stop_tick_loop(&self, id: i32) {
        if let Some(control) = self.loop_controls.lock().unwrap().remove(&id) {
            control.request_stop();
        }
    }
}
