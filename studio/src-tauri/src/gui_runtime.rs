use std::collections::HashMap;
use std::io::Write;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{mpsc, Arc, Mutex};

use tauri::{AppHandle, Emitter};
use vo_engine::{ensure_toolchain_host_installed, CompileOutput};
use vo_runtime::ext_loader::ExtensionLoader;
use vo_runtime::island_msg::{decode_island_transport_frame, encode_island_transport_frame};
use vo_runtime::output::CaptureSink;
use vo_vm::vm::{SchedulingOutcome, Vm};

// eventIDGameLoop matches canvas.vo constant (-5)
const EVENT_ID_GAME_LOOP: i32 = -5;

// =============================================================================
// StudioTickProvider — tick loop capability for the native Studio GUI VM thread
// =============================================================================

#[derive(Default)]
struct TickLoopState {
    pending_event: bool,
    accumulated_dt_ms: f64,
}

struct TickLoopControl {
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

    fn request_stop(&self) {
        self.stop.store(true, Ordering::Relaxed);
    }

    fn should_stop(&self) -> bool {
        self.stop.load(Ordering::Relaxed)
    }

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

    fn take_pending_tick(&self) -> Option<f64> {
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

    fn restore_unhandled_tick(&self, dt_ms: f64) -> bool {
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

#[derive(Clone)]
struct StudioTickProvider {
    event_tx: mpsc::Sender<GuestEvent>,
    loop_controls: Arc<Mutex<HashMap<i32, Arc<TickLoopControl>>>>,
}

impl StudioTickProvider {
    fn new(event_tx: mpsc::Sender<GuestEvent>) -> Self {
        Self {
            event_tx,
            loop_controls: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    fn enqueue_game_loop_tick(
        &self,
        loop_id: i32,
        control: &Arc<TickLoopControl>,
    ) -> Result<(), mpsc::SendError<GuestEvent>> {
        self.event_tx.send(GuestEvent::GameLoopTick {
            loop_id,
            control: Arc::clone(control),
        })
    }
}

impl vo_ext::host::tick::TickProvider for StudioTickProvider {
    fn start_tick_loop(&self, id: i32) {
        debug_log(&format!("[studio-native] start_tick_loop id={}", id));
        let control = Arc::new(TickLoopControl::new());
        if let Some(existing) = self.loop_controls.lock().unwrap().insert(id, Arc::clone(&control)) {
            existing.request_stop();
        }
        let provider = self.clone();
        std::thread::spawn(move || {
            let frame_dur = std::time::Duration::from_millis(16);
            let mut last = std::time::Instant::now();
            loop {
                std::thread::sleep(frame_dur);
                if control.should_stop() { break; }
                let now = std::time::Instant::now();
                let dt_ms = now.duration_since(last).as_secs_f64() * 1000.0;
                last = now;
                if control.accumulate_tick(dt_ms) {
                    if provider.enqueue_game_loop_tick(id, &control).is_err() {
                        control.request_stop();
                        break;
                    }
                }
            }
        });
    }

    fn stop_tick_loop(&self, id: i32) {
        debug_log(&format!("[studio-native] stop_tick_loop id={}", id));
        if let Some(control) = self.loop_controls.lock().unwrap().remove(&id) {
            control.request_stop();
        }
    }
}

enum GuestEvent {
    Ide { handler_id: i32, payload: String },
    IdeAsync { handler_id: i32, payload: String },
    GameLoopTick { loop_id: i32, control: Arc<TickLoopControl> },
    IslandData { data: Vec<u8> },
    Shutdown,
}

pub(crate) fn debug_log(message: &str) {
    eprintln!("{message}");
    let log_path = ["STUDIO_DEBUG_LOG", "VIBE_STUDIO_DEBUG_LOG"]
        .iter()
        .find_map(|name| std::env::var(name).ok())
        .filter(|path| !path.trim().is_empty());
    if let Some(path) = log_path {
        if let Ok(mut file) = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
        {
            let _ = writeln!(file, "{message}");
        }
    }
}

fn flush_guest_stdout(label: &str, sink: &CaptureSink) {
    let stdout = sink.take();
    if stdout.trim().is_empty() {
        return;
    }
    debug_log(&format!("[guest-stdout][{}] {}", label, stdout.trim_end()));
}

pub struct GuestHandle {
    event_tx: mpsc::Sender<GuestEvent>,
    render_rx: mpsc::Receiver<Result<Vec<u8>, String>>,
}

impl Drop for GuestHandle {
    fn drop(&mut self) {
        let _ = self.event_tx.send(GuestEvent::Shutdown);
    }
}

impl GuestHandle {
    pub fn send_event(&self, handler_id: i32, payload: &str) -> Result<Vec<u8>, String> {
        self.event_tx
            .send(GuestEvent::Ide { handler_id, payload: payload.to_string() })
            .map_err(|_| "guest VM stopped".to_string())?;
        self.render_rx
            .recv()
            .map_err(|_| "guest VM stopped".to_string())?
            .map_err(|error| format!("guest event failed: {}", error))
    }

    pub fn send_event_async(&self, handler_id: i32, payload: &str) -> Result<(), String> {
        self.event_tx
            .send(GuestEvent::IdeAsync { handler_id, payload: payload.to_string() })
            .map_err(|_| "guest VM stopped".to_string())
    }

    pub fn push_island_data(&self, data: &[u8]) -> Result<(), String> {
        self.event_tx
            .send(GuestEvent::IslandData { data: data.to_vec() })
            .map_err(|_| "guest VM stopped".to_string())
    }
}

pub struct PushReceiver {
    latest: Mutex<Option<Vec<u8>>>,
}

impl PushReceiver {
    fn push(&self, bytes: Vec<u8>) {
        if bytes.is_empty() {
            return;
        }
        *self.latest.lock().unwrap() = Some(bytes);
    }

    pub fn poll(&self) -> Option<Vec<u8>> {
        self.latest.lock().unwrap().take()
    }
}

pub fn run_gui(output: CompileOutput, app: AppHandle) -> Result<(Vec<u8>, GuestHandle, Arc<PushReceiver>), String> {
    let (event_tx, event_rx) = mpsc::channel::<GuestEvent>();
    let (render_tx, render_rx) = mpsc::sync_channel::<Result<Vec<u8>, String>>(1);
    let push = Arc::new(PushReceiver { latest: Mutex::new(None) });
    let push_for_thread = Arc::clone(&push);
    let platform_tx = event_tx.clone();
    std::thread::spawn(move || {
        run_gui_thread(output, render_tx, push_for_thread, event_rx, platform_tx, app);
    });
    let initial = render_rx
        .recv()
        .map_err(|error| format!("guest thread died: {}", error))?
        .map_err(|error| format!("guest init failed: {}", error))?;
    let handle = GuestHandle { event_tx, render_rx };
    Ok((initial, handle, push))
}

fn build_gui_vm(output: CompileOutput) -> Result<Vm, String> {
    ensure_toolchain_host_installed();
    debug_log(&format!(
        "[studio-native] compile extensions: {:?}",
        output
            .extensions
            .iter()
            .map(|manifest| format!("{} => {}", manifest.name, manifest.native_path.display()))
            .collect::<Vec<_>>()
    ));
    let ext_loader = if output.extensions.is_empty() {
        None
    } else {
        vo_engine::ensure_extension_manifests_built(&output.extensions, &output.locked_modules)
            .map_err(|error| format!("failed to prepare extensions: {}", error))?;
        Some(
            ExtensionLoader::from_manifests(&output.extensions)
                .map_err(|error| format!("failed to load extensions: {}", error))?
        )
    };
    let mut vm = Vm::new();
    vm.enable_external_island_transport();
    vm.load_with_extensions(output.module, ext_loader);
    Ok(vm)
}

fn run_gui_thread(
    output: CompileOutput,
    render_tx: mpsc::SyncSender<Result<Vec<u8>, String>>,
    push: Arc<PushReceiver>,
    event_rx: mpsc::Receiver<GuestEvent>,
    platform_tx: mpsc::Sender<GuestEvent>,
    app: AppHandle,
) {
    let mut vm = match build_gui_vm(output) {
        Ok(vm) => vm,
        Err(error) => {
            let _ = render_tx.send(Err(error));
            return;
        }
    };
    let capture_sink = CaptureSink::new();
    vm.state.output = capture_sink.clone();
    let tick_provider = StudioTickProvider::new(platform_tx);
    let bridge = vo_ext::host::HostBridge::new()
        .with_capability("external_island_host")
        .with_tick(Box::new(tick_provider.clone()));
    vo_ext::host::install(bridge);
    // Broadcast the bridge pointer into extension dylibs (RTLD_LOCAL means
    // they have their own copy of the BRIDGE thread-local).
    let ptr = vo_ext::host::with_bridge(|b| vo_ext::host::encode_bridge_ptr(b)).unwrap();
    unsafe { vm.broadcast_bridge(ptr); }
    vm.clear_host_output();
    let outcome = match vm.run() {
        Ok(outcome) => outcome,
        Err(error) => {
            let _ = render_tx.send(Err(format!("{:?}", error)));
            return;
        }
    };
    if let Err(error) = handle_guest_outcome(&mut vm, outcome, &app) {
        let _ = render_tx.send(Err(error));
        return;
    }
    flush_guest_stdout("init", capture_sink.as_ref());
    let bytes = vm.take_host_output().unwrap_or_default();
    let _ = render_tx.send(Ok(bytes));
    while let Ok(event) = event_rx.recv() {
        match event {
            GuestEvent::Shutdown => {
                vm.clear_extension_bridges();
                vo_ext::host::clear();
                break;
            },
            GuestEvent::Ide { handler_id, payload } => match dispatch_event(&mut vm, handler_id, &payload, &app, capture_sink.as_ref()) {
                Ok(bytes) => {
                    let _ = render_tx.send(Ok(bytes));
                }
                Err(error) => {
                    let _ = render_tx.send(Err(error));
                    return;
                }
            },
            GuestEvent::IdeAsync { handler_id, payload } => match dispatch_event(&mut vm, handler_id, &payload, &app, capture_sink.as_ref()) {
                Ok(bytes) => {
                    push.push(bytes);
                }
                Err(error) => {
                    if error.contains("not waiting for events") {
                        // VM wasn't ready for this async event (e.g. game loop tick
                        // arrived while another event was being processed) — skip.
                    } else {
                        eprintln!("guest VM error on async IDE event: {}", error);
                        vm.clear_extension_bridges();
                        vo_ext::host::clear();
                        return;
                    }
                }
            },
            GuestEvent::GameLoopTick { loop_id, control } => {
                let Some(dt_ms) = control.take_pending_tick() else { continue; };
                let payload = format!(r#"{{"dt":{:.3}}}"#, dt_ms);
                match dispatch_event(&mut vm, EVENT_ID_GAME_LOOP, &payload, &app, capture_sink.as_ref()) {
                    Ok(bytes) => {
                        push.push(bytes);
                    }
                    Err(error) => {
                        if error.contains("not waiting for events") {
                            if control.restore_unhandled_tick(dt_ms) {
                                let _ = tick_provider.enqueue_game_loop_tick(loop_id, &control);
                            }
                        } else {
                            eprintln!("guest VM error on game loop tick: {}", error);
                            vm.clear_extension_bridges();
                            vo_ext::host::clear();
                            return;
                        }
                    }
                }
            },
            GuestEvent::IslandData { data } => match dispatch_island_data(&mut vm, &data, &app, capture_sink.as_ref()) {
                Ok(bytes) => {
                    push.push(bytes);
                }
                Err(error) => {
                    eprintln!("guest VM error on island data: {}", error);
                    return;
                }
            },
        }
    }
}

fn dispatch_event(vm: &mut Vm, handler_id: i32, payload: &str, app: &AppHandle, capture_sink: &CaptureSink) -> Result<Vec<u8>, String> {
    vm.clear_host_output();
    vo_runtime::output::clear_output();
    let pending = vm.scheduler.take_pending_host_events();
    let token = pending
        .first()
        .map(|event| event.token)
        .ok_or_else(|| "Main fiber not waiting for events".to_string())?;
    let mut data = Vec::with_capacity(4 + payload.len());
    data.extend_from_slice(&handler_id.to_le_bytes());
    data.extend_from_slice(payload.as_bytes());
    vm.wake_host_event_with_data(token, data);
    let outcome = vm.run_scheduled().map_err(|error| format!("{:?}", error))?;
    handle_guest_outcome(vm, outcome, app)?;
    flush_guest_stdout("event", capture_sink);
    Ok(vm.take_host_output().unwrap_or_default())
}

fn dispatch_island_data(vm: &mut Vm, data: &[u8], app: &AppHandle, capture_sink: &CaptureSink) -> Result<Vec<u8>, String> {
    vm.clear_host_output();
    vo_runtime::output::clear_output();
    let (_target_island_id, cmd) = decode_island_transport_frame(data)
        .map_err(|error| format!("invalid island transport frame: {:?}", error))?;
    vm.push_island_command(cmd);
    let outcome = vm.run_scheduled().map_err(|error| format!("{:?}", error))?;
    handle_guest_outcome(vm, outcome, app)?;
    flush_guest_stdout("island", capture_sink);
    Ok(vm.take_host_output().unwrap_or_default())
}

fn handle_guest_outcome(vm: &mut Vm, outcome: SchedulingOutcome, app: &AppHandle) -> Result<(), String> {
    match outcome {
        SchedulingOutcome::Completed | SchedulingOutcome::Suspended | SchedulingOutcome::SuspendedForHostEvents => {
            flush_outbound_island_commands(vm, app)
        }
        SchedulingOutcome::Blocked => Err(format!("{:?}", vm.deadlock_err())),
        SchedulingOutcome::Panicked => Err("unexpected bounded panic outcome".to_string()),
    }
}

fn flush_outbound_island_commands(vm: &mut Vm, app: &AppHandle) -> Result<(), String> {
    for (target_island_id, cmd) in vm.take_outbound_commands() {
        let bytes = encode_island_transport_frame(target_island_id, &cmd);
        app.emit("island_data", bytes)
            .map_err(|error| format!("failed to emit island_data: {}", error))?;
    }
    Ok(())
}
