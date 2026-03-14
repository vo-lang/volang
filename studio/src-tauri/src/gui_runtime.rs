use std::collections::HashMap;
use std::io::Write;
use std::sync::{mpsc, Arc, Mutex};
use std::time::{Duration, Instant};
use tauri::{AppHandle, Emitter};
use vo_runtime::ext_loader::ExtensionLoader;
use vo_runtime::island_msg::{decode_island_transport_frame, encode_island_transport_frame};
use vo_vm::vm::{SchedulingOutcome, Vm};
use vo_vox::CompileOutput;

enum GuestEvent {
    Ide { handler_id: i32, payload: String },
    IdeAsync { handler_id: i32, payload: String },
    Platform { handler_id: i32, payload: String },
    IslandData { data: Vec<u8> },
    Shutdown,
}

fn debug_log(message: &str) {
    eprintln!("{message}");
    if let Ok(path) = std::env::var("VIBE_STUDIO_DEBUG_LOG") {
        if let Ok(mut file) = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
        {
            let _ = writeln!(file, "{message}");
        }
    }
}

const EVENT_ID_TIMER: i32 = -1;
const EVENT_ID_ANIM_FRAME: i32 = -4;
const EVENT_ID_GAME_LOOP: i32 = -5;

struct NativeGuiPlatform {
    event_tx: mpsc::Sender<GuestEvent>,
    cancels: Mutex<HashMap<i32, mpsc::Sender<()>>>,
}

impl NativeGuiPlatform {
    fn new(event_tx: mpsc::Sender<GuestEvent>) -> Self {
        Self {
            event_tx,
            cancels: Mutex::new(HashMap::new()),
        }
    }

    fn spawn_oneshot(&self, id: i32, delay: Duration, handler_id: i32, payload: String) {
        let tx = self.event_tx.clone();
        let (cancel_tx, cancel_rx) = mpsc::channel();
        self.cancels.lock().unwrap().insert(id, cancel_tx);
        std::thread::spawn(move || {
            if let Err(mpsc::RecvTimeoutError::Timeout) = cancel_rx.recv_timeout(delay) {
                let _ = tx.send(GuestEvent::Platform { handler_id, payload });
            }
        });
    }

    fn spawn_repeating(&self, id: i32, interval: Duration, handler_id: i32, make_payload: Box<dyn Fn() -> String + Send>) {
        let tx = self.event_tx.clone();
        let (cancel_tx, cancel_rx) = mpsc::channel();
        self.cancels.lock().unwrap().insert(id, cancel_tx);
        std::thread::spawn(move || loop {
            match cancel_rx.recv_timeout(interval) {
                Err(mpsc::RecvTimeoutError::Timeout) => {
                    if tx.send(GuestEvent::Platform { handler_id, payload: make_payload() }).is_err() {
                        return;
                    }
                }
                _ => return,
            }
        });
    }

    fn cancel(&self, id: i32) {
        if let Some(cancel_tx) = self.cancels.lock().unwrap().remove(&id) {
            let _ = cancel_tx.send(());
        }
    }
}

impl vogui::VoguiPlatform for NativeGuiPlatform {
    fn start_timeout(&self, id: i32, ms: i32) {
        let payload = format!("{{\"Id\":{}}}", id);
        self.spawn_oneshot(id, Duration::from_millis(ms.max(0) as u64), EVENT_ID_TIMER, payload);
    }

    fn clear_timeout(&self, id: i32) {
        self.cancel(id);
    }

    fn start_interval(&self, id: i32, ms: i32) {
        let interval = Duration::from_millis(ms.max(1) as u64);
        let timer_id = id;
        self.spawn_repeating(id, interval, EVENT_ID_TIMER, Box::new(move || format!("{{\"Id\":{}}}", timer_id)));
    }

    fn clear_interval(&self, id: i32) {
        self.cancel(id);
    }

    fn navigate(&self, _path: &str) {}

    fn get_current_path(&self) -> String {
        "/".to_string()
    }

    fn start_anim_frame(&self, id: i32) {
        let payload = format!("{{\"Id\":{}}}", id);
        self.spawn_oneshot(id, Duration::from_millis(16), EVENT_ID_ANIM_FRAME, payload);
    }

    fn cancel_anim_frame(&self, id: i32) {
        self.cancel(id);
    }

    fn start_game_loop(&self, id: i32) {
        let tx = self.event_tx.clone();
        let (cancel_tx, cancel_rx) = mpsc::channel();
        self.cancels.lock().unwrap().insert(id, cancel_tx);
        std::thread::spawn(move || {
            let target_frame = Duration::from_micros(16_667);
            let mut last = Instant::now();
            loop {
                let now = Instant::now();
                let elapsed = now.duration_since(last);
                if elapsed < target_frame {
                    match cancel_rx.recv_timeout(target_frame - elapsed) {
                        Err(mpsc::RecvTimeoutError::Timeout) => {}
                        _ => return,
                    }
                }
                let now = Instant::now();
                let dt_ms = now.duration_since(last).as_secs_f64() * 1000.0;
                last = now;
                let payload = format!("{{\"Dt\":{:.3}}}", dt_ms);
                if tx.send(GuestEvent::Platform { handler_id: EVENT_ID_GAME_LOOP, payload }).is_err() {
                    return;
                }
            }
        });
    }

    fn stop_game_loop(&self, id: i32) {
        self.cancel(id);
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
            .map_err(|e| format!("guest event failed: {}", e))
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
        .map_err(|e| format!("guest thread died: {}", e))?
        .map_err(|e| format!("guest init failed: {}", e))?;
    let handle = GuestHandle { event_tx, render_rx };
    let push = Arc::new(PushReceiver { rx: Mutex::new(push_rx) });
    Ok((initial, handle, push))
}

fn build_gui_vm(output: CompileOutput) -> Result<Vm, String> {
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
        let mut loader = ExtensionLoader::new();
        for manifest in &output.extensions {
            debug_log(&format!(
                "[studio-native] loading extension '{}' from {}",
                manifest.name,
                manifest.native_path.display()
            ));
            loader
                .load(&manifest.native_path, &manifest.name)
                .map_err(|e| format!("failed to load extension '{}': {}", manifest.name, e))?;
        }
        debug_log(&format!(
            "[studio-native] loaded extensions: {:?}",
            loader.loaded_extensions()
        ));
        debug_log(&format!(
            "[studio-native] lookup voplay_initSurface = {}",
            loader.lookup("voplay_initSurface").is_some()
        ));
        debug_log(&format!(
            "[studio-native] lookup voplay_scene3d_physicsInit = {}",
            loader.lookup("voplay_scene3d_physicsInit").is_some()
        ));
        debug_log(&format!(
            "[studio-native] lookup voplay_scene3d_physicsQueryAABB = {}",
            loader.lookup("voplay_scene3d_physicsQueryAABB").is_some()
        ));
        let enable_external_island_transport = configure_loaded_extensions(&loader)?;
        Some(loader)
            .map(|loader| (loader, enable_external_island_transport))
    };
    let mut vm = Vm::new();
    let (ext_loader, enable_external_island_transport) = match ext_loader {
        Some((loader, enabled)) => (Some(loader), enabled),
        None => (None, false),
    };
    if enable_external_island_transport {
        vm.enable_external_island_transport();
    }
    vm.load_with_extensions(output.module, ext_loader);
    Ok(vm)
}

fn configure_loaded_extensions(loader: &ExtensionLoader) -> Result<bool, String> {
    let mut enabled_studio_mode = false;
    unsafe {
        if let Some(set_mode) = loader
            .symbol::<unsafe extern "C" fn(bool)>("voplay", b"vo_voplay_set_studio_mode\0")
            .map_err(|e| e.to_string())?
        {
            set_mode(true);
            enabled_studio_mode = true;
        }
    }
    Ok(enabled_studio_mode)
}

fn run_gui_thread(
    output: CompileOutput,
    render_tx: mpsc::SyncSender<Result<Vec<u8>, String>>,
    push_tx: mpsc::Sender<Vec<u8>>,
    event_rx: mpsc::Receiver<GuestEvent>,
    platform_tx: mpsc::Sender<GuestEvent>,
    app: AppHandle,
) {
    vogui::set_platform(Box::new(NativeGuiPlatform::new(platform_tx)));
    let mut vm = match build_gui_vm(output) {
        Ok(vm) => vm,
        Err(e) => {
            let _ = render_tx.send(Err(e));
            return;
        }
    };
    vm.clear_host_output();
    let outcome = match vm.run() {
        Ok(outcome) => outcome,
        Err(e) => {
            let _ = render_tx.send(Err(format!("{:?}", e)));
            return;
        }
    };
    if let Err(e) = handle_guest_outcome(&mut vm, outcome, &app) {
        let _ = render_tx.send(Err(e));
        return;
    }
    let bytes = vm.take_host_output().unwrap_or_default();
    let _ = render_tx.send(Ok(bytes));
    while let Ok(event) = event_rx.recv() {
        match event {
            GuestEvent::Shutdown => {
                vogui::clear_platform();
                break;
            }
            GuestEvent::Ide { handler_id, payload } => match dispatch_event(&mut vm, handler_id, &payload, &app) {
                Ok(bytes) => {
                    let _ = render_tx.send(Ok(bytes));
                }
                Err(e) => {
                    let _ = render_tx.send(Err(e));
                    return;
                }
            },
            GuestEvent::IdeAsync { handler_id, payload } => match dispatch_event(&mut vm, handler_id, &payload, &app) {
                Ok(bytes) => {
                    if !bytes.is_empty() {
                        let _ = push_tx.send(bytes);
                    }
                }
                Err(e) => {
                    eprintln!("guest VM error on async IDE event: {}", e);
                    return;
                }
            },
            GuestEvent::Platform { handler_id, payload } => match dispatch_event(&mut vm, handler_id, &payload, &app) {
                Ok(bytes) => {
                    if !bytes.is_empty() {
                        let _ = push_tx.send(bytes);
                    }
                }
                Err(e) => {
                    eprintln!("guest VM error on platform event: {}", e);
                    return;
                }
            },
            GuestEvent::IslandData { data } => match dispatch_island_data(&mut vm, &data, &app) {
                Ok(bytes) => {
                    if !bytes.is_empty() {
                        let _ = push_tx.send(bytes);
                    }
                }
                Err(e) => {
                    eprintln!("guest VM error on island data: {}", e);
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
        .map(|ev| ev.token)
        .ok_or_else(|| "Main fiber not waiting for events".to_string())?;
    let mut data = Vec::with_capacity(4 + payload.len());
    data.extend_from_slice(&handler_id.to_le_bytes());
    data.extend_from_slice(payload.as_bytes());
    vm.wake_host_event_with_data(token, data);
    let outcome = vm.run_scheduled().map_err(|e| format!("{:?}", e))?;
    handle_guest_outcome(vm, outcome, app)?;
    Ok(vm.take_host_output().unwrap_or_default())
}

fn dispatch_island_data(vm: &mut Vm, data: &[u8], app: &AppHandle) -> Result<Vec<u8>, String> {
    vm.clear_host_output();
    vo_runtime::output::clear_output();
    let (_target_island_id, cmd) = decode_island_transport_frame(data)
        .map_err(|e| format!("invalid island transport frame: {:?}", e))?;
    vm.push_island_command(cmd);
    let outcome = vm.run_scheduled().map_err(|e| format!("{:?}", e))?;
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
            .map_err(|e| format!("failed to emit island_data: {}", e))?;
    }
    Ok(())
}
