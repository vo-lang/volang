//! High-level native GUI event loop.
//!
//! Provides [`spawn_native_gui`] which encapsulates the standard pattern of:
//! 1. Creating a [`NativeGuiRuntime`] from a loaded [`Vm`].
//! 2. Installing host capabilities (tick provider, etc.).
//! 3. Spawning a dedicated thread for the guest VM event loop.
//! 4. Returning a [`NativeGuestHandle`] for cross-thread communication
//!    and an `Arc<SyncRenderBuffer>` for async render output.

use std::sync::{mpsc, Arc};

use vo_vm::vm::Vm;

use crate::protocol::event_ids;
use crate::{
    NativeGuiRuntime, NativeTickProvider, NativeTimerProvider, SyncRenderBuffer,
    TickLoopControl, TimerControl,
};

// ── Internal event enum ─────────────────────────────────────────────────────

enum GuestEvent {
    Event {
        handler_id: i32,
        payload: String,
    },
    AsyncEvent {
        handler_id: i32,
        payload: String,
    },
    TimerFired {
        timer_id: i32,
        control: Arc<TimerControl>,
    },
    GameLoopTick {
        loop_id: i32,
        control: Arc<TickLoopControl>,
    },
    IslandData {
        data: Vec<u8>,
    },
    Shutdown,
}

// ── NativeGuestHandle ───────────────────────────────────────────────────────

/// Thread-safe handle for communicating with a running native GUI guest VM.
///
/// Dropping the handle sends a shutdown signal to the guest thread.
pub struct NativeGuestHandle {
    event_tx: mpsc::Sender<GuestEvent>,
    render_rx: mpsc::Receiver<Result<Vec<u8>, String>>,
}

impl Drop for NativeGuestHandle {
    fn drop(&mut self) {
        let _ = self.event_tx.send(GuestEvent::Shutdown);
    }
}

impl NativeGuestHandle {
    /// Send a synchronous event to the guest VM.
    ///
    /// Blocks until the guest produces render output for this event.
    pub fn send_event(&self, handler_id: i32, payload: &str) -> Result<Vec<u8>, String> {
        self.event_tx
            .send(GuestEvent::Event {
                handler_id,
                payload: payload.to_string(),
            })
            .map_err(|_| "guest VM stopped".to_string())?;
        self.render_rx
            .recv()
            .map_err(|_| "guest VM stopped".to_string())?
    }

    /// Send an asynchronous event to the guest VM.
    ///
    /// Returns immediately.  Render output (if any) is pushed to the
    /// [`SyncRenderBuffer`] returned by [`spawn_native_gui`].
    pub fn send_event_async(&self, handler_id: i32, payload: &str) -> Result<(), String> {
        self.event_tx
            .send(GuestEvent::AsyncEvent {
                handler_id,
                payload: payload.to_string(),
            })
            .map_err(|_| "guest VM stopped".to_string())
    }

    /// Push inbound island transport data to the guest VM.
    pub fn push_island_data(&self, data: &[u8]) -> Result<(), String> {
        self.event_tx
            .send(GuestEvent::IslandData {
                data: data.to_vec(),
            })
            .map_err(|_| "guest VM stopped".to_string())
    }
}

// ── Configuration ───────────────────────────────────────────────────────────

type IslandSink = Box<dyn FnMut(Vec<u8>) -> Result<(), String> + Send>;
type StdoutCallback = Box<dyn Fn(&str, &str) + Send>;
type ErrorCallback = Box<dyn Fn(&str) + Send>;

/// Configuration for [`spawn_native_gui`].
pub struct NativeGuiEventLoopConfig {
    /// Callback for outbound island frames.  `None` if the app does not use
    /// external island transport.
    pub island_sink: Option<IslandSink>,
    /// Host capabilities to advertise (e.g. `"external_island_host"`).
    pub capabilities: Vec<String>,
    /// Called when the guest produces stdout.
    ///
    /// Arguments: `(label, trimmed_text)`.  The runtime trims whitespace and
    /// only invokes this callback when the text is non-empty.
    ///
    /// Typical labels: `"init"`, `"event"`, `"tick"`, `"island"`.
    pub on_stdout: Option<StdoutCallback>,
    /// Called when an async dispatch error occurs (async events, ticks, island
    /// data).  Fatal sync-event errors are returned directly from
    /// [`NativeGuestHandle::send_event`].
    pub on_error: Option<ErrorCallback>,
}

impl Default for NativeGuiEventLoopConfig {
    fn default() -> Self {
        Self {
            island_sink: None,
            capabilities: Vec::new(),
            on_stdout: None,
            on_error: None,
        }
    }
}

// ── Public entry point ──────────────────────────────────────────────────────

/// Spawn a native GUI event loop on a dedicated thread.
///
/// `build_vm` is called **on the spawned thread** to construct the VM.  This
/// is necessary because [`Vm`] is not `Send`; it also means expensive work
/// (extension loading, etc.) happens off the caller's thread.
///
/// Returns:
/// - **initial render output** — the first render frame produced by `start()`.
/// - **[`NativeGuestHandle`]** — for sending events to the guest.
/// - **`Arc<SyncRenderBuffer>`** — for polling async render output (from async
///   events, tick updates, and island data).
pub fn spawn_native_gui<F>(
    build_vm: F,
    config: NativeGuiEventLoopConfig,
) -> Result<(Vec<u8>, NativeGuestHandle, Arc<SyncRenderBuffer>), String>
where
    F: FnOnce() -> Result<Vm, String> + Send + 'static,
{
    let (event_tx, event_rx) = mpsc::channel::<GuestEvent>();
    let (render_tx, render_rx) = mpsc::sync_channel::<Result<Vec<u8>, String>>(1);
    let buffer = Arc::new(SyncRenderBuffer::new());
    let buffer_clone = Arc::clone(&buffer);
    let platform_tx = event_tx.clone();

    std::thread::spawn(move || {
        let vm = match build_vm() {
            Ok(vm) => vm,
            Err(error) => {
                let _ = render_tx.send(Err(error));
                return;
            }
        };
        run_event_loop(vm, config, render_tx, buffer_clone, event_rx, platform_tx);
    });

    let initial = render_rx
        .recv()
        .map_err(|_| "guest thread died before producing initial render".to_string())?;
    let initial = initial?;

    let handle = NativeGuestHandle {
        event_tx,
        render_rx,
    };
    Ok((initial, handle, buffer))
}

// ── Event loop implementation ───────────────────────────────────────────────

fn flush_stdout(callback: &Option<StdoutCallback>, label: &str, stdout: Option<&str>) {
    if let Some(cb) = callback {
        if let Some(s) = stdout {
            let trimmed = s.trim();
            if !trimmed.is_empty() {
                cb(label, trimmed);
            }
        }
    }
}

fn report_error(callback: &Option<ErrorCallback>, msg: &str) {
    if let Some(cb) = callback {
        cb(msg);
    }
}

fn run_event_loop(
    vm: Vm,
    config: NativeGuiEventLoopConfig,
    render_tx: mpsc::SyncSender<Result<Vec<u8>, String>>,
    buffer: Arc<SyncRenderBuffer>,
    event_rx: mpsc::Receiver<GuestEvent>,
    platform_tx: mpsc::Sender<GuestEvent>,
) {
    let NativeGuiEventLoopConfig {
        island_sink,
        capabilities,
        on_stdout,
        on_error,
    } = config;

    let mut runtime = NativeGuiRuntime::new(vm, island_sink);

    let tick_tx = platform_tx.clone();
    let tick_provider = NativeTickProvider::new(move |loop_id, control| {
        let _ = tick_tx.send(GuestEvent::GameLoopTick { loop_id, control });
    });
    let timer_provider = NativeTimerProvider::new(move |timer_id, control| {
        let _ = platform_tx.send(GuestEvent::TimerFired { timer_id, control });
    });
    let cap_refs: Vec<&str> = capabilities.iter().map(|s| s.as_str()).collect();
    runtime.install_host_capabilities_with_timer(tick_provider.clone(), timer_provider, &cap_refs);

    // ── start ───────────────────────────────────────────────────────────
    let step = match runtime.start() {
        Ok(step) => step,
        Err(error) => {
            let _ = render_tx.send(Err(error.to_string()));
            return;
        }
    };
    flush_stdout(&on_stdout, "init", step.stdout.as_deref());
    let _ = render_tx.send(Ok(step.render_output.unwrap_or_default()));

    // ── main loop ───────────────────────────────────────────────────────
    while let Ok(event) = event_rx.recv() {
        match event {
            GuestEvent::Shutdown => {
                runtime.shutdown();
                break;
            }
            GuestEvent::Event {
                handler_id,
                payload,
            } => match runtime.dispatch_event(handler_id, &payload) {
                Ok(step) => {
                    flush_stdout(&on_stdout, "event", step.stdout.as_deref());
                    let _ = render_tx.send(Ok(step.render_output.unwrap_or_default()));
                }
                Err(error) => {
                    let _ = render_tx.send(Err(error.to_string()));
                    return;
                }
            },
            GuestEvent::AsyncEvent {
                handler_id,
                payload,
            } => match runtime.try_dispatch_event(handler_id, &payload) {
                Ok(Some(step)) => {
                    flush_stdout(&on_stdout, "event", step.stdout.as_deref());
                    buffer.push(step.render_output.unwrap_or_default());
                }
                Ok(None) => {}
                Err(error) => {
                    report_error(
                        &on_error,
                        &format!("guest VM error on async event: {}", error),
                    );
                    runtime.shutdown();
                    return;
                }
            },
            GuestEvent::TimerFired { timer_id, control } => {
                if !control.take_pending_event() {
                    continue;
                }
                let payload = format!(r#"{{"id":{}}}"#, timer_id);
                match runtime.try_dispatch_event(event_ids::TIMER, &payload) {
                    Ok(Some(step)) => {
                        flush_stdout(&on_stdout, "event", step.stdout.as_deref());
                        buffer.push(step.render_output.unwrap_or_default());
                    }
                    Ok(None) => {}
                    Err(error) => {
                        report_error(
                            &on_error,
                            &format!("guest VM error on timer event: {}", error),
                        );
                        runtime.shutdown();
                        return;
                    }
                }
            },
            GuestEvent::GameLoopTick { loop_id, control } => {
                let Some(dt_ms) = control.take_pending_tick() else {
                    continue;
                };
                let payload = format!(r#"{{"dt":{:.3}}}"#, dt_ms);
                match runtime.try_dispatch_event(event_ids::GAME_LOOP, &payload) {
                    Ok(Some(step)) => {
                        flush_stdout(&on_stdout, "tick", step.stdout.as_deref());
                        buffer.push(step.render_output.unwrap_or_default());
                    }
                    Ok(None) => {
                        if control.restore_unhandled_tick(dt_ms) {
                            tick_provider.enqueue_tick(loop_id, &control);
                        }
                    }
                    Err(error) => {
                        report_error(
                            &on_error,
                            &format!("guest VM error on game loop tick: {}", error),
                        );
                        runtime.shutdown();
                        return;
                    }
                }
            }
            GuestEvent::IslandData { data } => match runtime.dispatch_island_frame(&data) {
                Ok(step) => {
                    flush_stdout(&on_stdout, "island", step.stdout.as_deref());
                    buffer.push(step.render_output.unwrap_or_default());
                }
                Err(error) => {
                    report_error(
                        &on_error,
                        &format!("guest VM error on island data: {}", error),
                    );
                    runtime.shutdown();
                    return;
                }
            },
        }
    }
}
