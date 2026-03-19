use std::collections::HashMap;
use std::io::Write;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{mpsc, Arc, Mutex};

use tauri::{AppHandle, Emitter};
use vo_engine::{ensure_toolchain_host_installed, CompileOutput};
use vo_runtime::ext_loader::ExtensionLoader;
use vo_runtime::island_msg::{decode_island_transport_frame, encode_island_transport_frame};
use vo_vm::vm::{SchedulingOutcome, Vm};

// eventIDGameLoop matches canvas.vo constant (-5)
const EVENT_ID_GAME_LOOP: i32 = -5;

// =============================================================================
// StudioTickProvider — tick loop capability for the native Studio GUI VM thread
// =============================================================================

struct StudioTickProvider {
    event_tx: mpsc::Sender<GuestEvent>,
    loop_stop_flags: Arc<Mutex<HashMap<i32, Arc<AtomicBool>>>>,
}

impl StudioTickProvider {
    fn new(event_tx: mpsc::Sender<GuestEvent>) -> Self {
        Self {
            event_tx,
            loop_stop_flags: Arc::new(Mutex::new(HashMap::new())),
        }
    }
}

impl vo_ext::host::tick::TickProvider for StudioTickProvider {
    fn start_tick_loop(&self, id: i32) {
        debug_log(&format!("[studio-native] start_tick_loop id={}", id));
        let stop = Arc::new(AtomicBool::new(false));
        self.loop_stop_flags.lock().unwrap().insert(id, Arc::clone(&stop));
        let tx = self.event_tx.clone();
        std::thread::spawn(move || {
            let frame_dur = std::time::Duration::from_millis(16);
            let mut last = std::time::Instant::now();
            loop {
                std::thread::sleep(frame_dur);
                if stop.load(Ordering::Relaxed) { break; }
                let now = std::time::Instant::now();
                let dt_ms = now.duration_since(last).as_secs_f64() * 1000.0;
                last = now;
                let payload = format!(r#"{{"dt":{:.3}}}"#, dt_ms);
                if tx.send(GuestEvent::IdeAsync {
                    handler_id: EVENT_ID_GAME_LOOP,
                    payload,
                }).is_err() {
                    break;
                }
            }
        });
    }

    fn stop_tick_loop(&self, id: i32) {
        debug_log(&format!("[studio-native] stop_tick_loop id={}", id));
        if let Some(flag) = self.loop_stop_flags.lock().unwrap().remove(&id) {
            flag.store(true, Ordering::Relaxed);
        }
    }
}

enum GuestEvent {
    Ide { handler_id: i32, payload: String },
    IdeAsync { handler_id: i32, payload: String },
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
    rx: Mutex<mpsc::Receiver<Vec<u8>>>,
}

impl PushReceiver {
    pub fn poll(&self) -> Option<Vec<u8>> {
        let rx = self.rx.lock().unwrap();
        let mut latest = None;
        while let Ok(bytes) = rx.try_recv() {
            if !bytes.is_empty() {
                latest = Some(bytes);
            }
        }
        latest
    }
}

pub fn run_gui(output: CompileOutput, app: AppHandle) -> Result<(Vec<u8>, GuestHandle, Arc<PushReceiver>), String> {
    let (event_tx, event_rx) = mpsc::channel::<GuestEvent>();
    let (render_tx, render_rx) = mpsc::sync_channel::<Result<Vec<u8>, String>>(1);
    let (push_tx, push_rx) = mpsc::channel::<Vec<u8>>();
    let platform_tx = event_tx.clone();
    std::thread::spawn(move || {
        run_gui_thread(output, render_tx, push_tx, event_rx, platform_tx, app);
    });
    let initial = render_rx
        .recv()
        .map_err(|error| format!("guest thread died: {}", error))?
        .map_err(|error| format!("guest init failed: {}", error))?;
    let handle = GuestHandle { event_tx, render_rx };
    let push = Arc::new(PushReceiver { rx: Mutex::new(push_rx) });
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
    push_tx: mpsc::Sender<Vec<u8>>,
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
    let bridge = vo_ext::host::HostBridge::new()
        .with_capability("external_island_host")
        .with_tick(Box::new(StudioTickProvider::new(platform_tx)));
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
    let bytes = vm.take_host_output().unwrap_or_default();
    let _ = render_tx.send(Ok(bytes));
    while let Ok(event) = event_rx.recv() {
        match event {
            GuestEvent::Shutdown => {
                vm.clear_extension_bridges();
                vo_ext::host::clear();
                break;
            },
            GuestEvent::Ide { handler_id, payload } => match dispatch_event(&mut vm, handler_id, &payload, &app) {
                Ok(bytes) => {
                    let _ = render_tx.send(Ok(bytes));
                }
                Err(error) => {
                    let _ = render_tx.send(Err(error));
                    return;
                }
            },
            GuestEvent::IdeAsync { handler_id, payload } => match dispatch_event(&mut vm, handler_id, &payload, &app) {
                Ok(bytes) => {
                    if !bytes.is_empty() {
                        let _ = push_tx.send(bytes);
                    }
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
            GuestEvent::IslandData { data } => match dispatch_island_data(&mut vm, &data, &app) {
                Ok(bytes) => {
                    if !bytes.is_empty() {
                        let _ = push_tx.send(bytes);
                    }
                }
                Err(error) => {
                    eprintln!("guest VM error on island data: {}", error);
                    return;
                }
            },
        }
    }
}

fn dispatch_event(vm: &mut Vm, handler_id: i32, payload: &str, app: &AppHandle) -> Result<Vec<u8>, String> {
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
    Ok(vm.take_host_output().unwrap_or_default())
}

fn dispatch_island_data(vm: &mut Vm, data: &[u8], app: &AppHandle) -> Result<Vec<u8>, String> {
    vm.clear_host_output();
    vo_runtime::output::clear_output();
    let (_target_island_id, cmd) = decode_island_transport_frame(data)
        .map_err(|error| format!("invalid island transport frame: {:?}", error))?;
    vm.push_island_command(cmd);
    let outcome = vm.run_scheduled().map_err(|error| format!("{:?}", error))?;
    handle_guest_outcome(vm, outcome, app)?;
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
