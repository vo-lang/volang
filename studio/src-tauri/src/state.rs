use std::collections::HashMap;
use std::env;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use vo_app_runtime::SyncRenderBuffer;
use vo_engine::PreparedNativeExtension;

use crate::commands::pathing::is_module_root;
use crate::gui_runtime::GuestHandle;

// ---------------------------------------------------------------------------
// Domain enums
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum StudioMode {
    Dev,
    Runner,
}

#[derive(Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum SessionOrigin {
    Workspace,
    RunTarget,
    Url,
}

#[derive(Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum ProjectMode {
    SingleFile,
    Module,
}

#[derive(Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
#[allow(dead_code)]
pub enum Platform {
    Native,
    Wasm,
}

// ---------------------------------------------------------------------------
// Serialized types (sent to frontend)
// ---------------------------------------------------------------------------

#[derive(Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BootstrapContext {
    pub workspace_root: String,
    pub launch_url: Option<String>,
    pub initial_path: Option<String>,
    pub initial_url: Option<String>,
    pub initial_run_target: Option<String>,
    pub mode: StudioMode,
    pub platform: Platform,
}

#[derive(Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionInfo {
    pub root: String,
    pub origin: SessionOrigin,
    pub project_mode: ProjectMode,
    pub entry_path: Option<String>,
    pub single_file_run: bool,
}

// ---------------------------------------------------------------------------
// Internal launch configuration (parsed once at startup)
// ---------------------------------------------------------------------------

struct LaunchConfig {
    launch_url: Option<String>,
    initial_path: Option<String>,
    initial_url: Option<String>,
    initial_run_target: Option<String>,
    mode: StudioMode,
}

struct SessionConfig {
    root: PathBuf,
    single_file_run: bool,
}

#[derive(Clone)]
pub struct ConsoleRunHandle {
    slot: Arc<Mutex<Option<Arc<AtomicBool>>>>,
    interrupt: Arc<AtomicBool>,
}

impl ConsoleRunHandle {
    pub fn interrupt_flag(&self) -> Arc<AtomicBool> {
        self.interrupt.clone()
    }

    pub fn clear_current(&self) {
        let mut slot = self.slot.lock().unwrap();
        if slot
            .as_ref()
            .map(|current| Arc::ptr_eq(current, &self.interrupt))
            .unwrap_or(false)
        {
            *slot = None;
        }
    }
}

// ---------------------------------------------------------------------------
// AppState
// ---------------------------------------------------------------------------

struct GuestRuntime {
    handle: GuestHandle,
    render_buffer: Arc<SyncRenderBuffer>,
}

pub struct AppState {
    workspace_root: PathBuf,
    launch: LaunchConfig,
    session: Mutex<SessionConfig>,
    console_run: Arc<Mutex<Option<Arc<AtomicBool>>>>,
    gui_session_id: AtomicU64,
    guest_runtime: Mutex<Option<GuestRuntime>>,
    last_extensions: Mutex<Vec<PreparedNativeExtension>>,
}

impl AppState {
    pub fn new() -> Self {
        let workspace_root = default_workspace();
        let _ = std::fs::create_dir_all(&workspace_root);
        let launch = parse_launch_config();
        Self {
            session: Mutex::new(SessionConfig {
                root: workspace_root.clone(),
                single_file_run: false,
            }),
            console_run: Arc::new(Mutex::new(None)),
            gui_session_id: AtomicU64::new(0),
            guest_runtime: Mutex::new(None),
            last_extensions: Mutex::new(Vec::new()),
            workspace_root,
            launch,
        }
    }

    pub fn bootstrap_context(&self) -> BootstrapContext {
        BootstrapContext {
            workspace_root: self.workspace_root.to_string_lossy().to_string(),
            launch_url: self.launch.launch_url.clone(),
            initial_path: self.launch.initial_path.clone(),
            initial_url: self.launch.initial_url.clone(),
            initial_run_target: self.launch.initial_run_target.clone(),
            mode: self.launch.mode,
            platform: Platform::Native,
        }
    }

    pub fn workspace_root(&self) -> &Path {
        &self.workspace_root
    }

    pub fn session_root(&self) -> PathBuf {
        self.session.lock().unwrap().root.clone()
    }

    pub fn single_file_run(&self) -> bool {
        self.session.lock().unwrap().single_file_run
    }

    pub fn set_session(&self, root: PathBuf, single_file_run: bool) {
        let mut session = self.session.lock().unwrap();
        session.root = root;
        session.single_file_run = single_file_run;
    }

    pub fn begin_console_run(&self) -> ConsoleRunHandle {
        let interrupt = Arc::new(AtomicBool::new(false));
        let mut slot = self.console_run.lock().unwrap();
        *slot = Some(interrupt.clone());
        ConsoleRunHandle {
            slot: self.console_run.clone(),
            interrupt,
        }
    }

    pub fn stop_console_run(&self) {
        if let Some(interrupt) = self.console_run.lock().unwrap().as_ref() {
            interrupt.store(true, Ordering::SeqCst);
        }
    }

    pub fn set_gui_session(&self, session_id: u64) {
        self.gui_session_id.store(session_id, Ordering::SeqCst);
    }

    pub fn gui_session_id(&self) -> u64 {
        self.gui_session_id.load(Ordering::SeqCst)
    }

    pub fn install_guest_runtime(&self, session_id: u64, guest: GuestHandle, render_buffer: Arc<SyncRenderBuffer>) {
        if self.gui_session_id.load(Ordering::SeqCst) != session_id {
            return;
        }
        *self.guest_runtime.lock().unwrap() = Some(GuestRuntime { handle: guest, render_buffer });
    }

    pub fn set_last_extensions(&self, extensions: Vec<PreparedNativeExtension>) {
        *self.last_extensions.lock().unwrap() = extensions;
    }

    pub fn last_extensions(&self) -> Vec<PreparedNativeExtension> {
        self.last_extensions.lock().unwrap().clone()
    }

    pub fn clear_guest_runtime(&self) {
        let _ = self.guest_runtime.lock().unwrap().take();
        self.last_extensions.lock().unwrap().clear();
    }

    pub fn with_guest<R>(&self, f: impl FnOnce(&GuestHandle) -> Result<R, String>) -> Result<R, String> {
        let rt = self.guest_runtime.lock().unwrap();
        let runtime = rt
            .as_ref()
            .ok_or_else(|| "guest VM not running".to_string())?;
        f(&runtime.handle)
    }

    pub fn poll_gui_render(&self) -> Vec<u8> {
        self.guest_runtime
            .lock()
            .unwrap()
            .as_ref()
            .and_then(|rt| rt.render_buffer.poll())
            .unwrap_or_default()
    }
}

pub fn session_info(
    path: &Path,
    origin: SessionOrigin,
    explicit_entry: Option<&Path>,
    single_file_run: bool,
) -> SessionInfo {
    let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let project_mode = if single_file_run {
        ProjectMode::SingleFile
    } else {
        detect_project_mode(&canonical)
    };
    let entry_path = explicit_entry
        .map(|entry| entry.to_string_lossy().to_string())
        .or_else(|| detect_entry_path(&canonical));
    SessionInfo {
        root: canonical.to_string_lossy().to_string(),
        origin,
        project_mode,
        entry_path,
        single_file_run,
    }
}

fn default_workspace() -> PathBuf {
    resolve_workspace_root(
        parse_env_any(&["STUDIO_WORKSPACE", "VIBE_STUDIO_WORKSPACE"]).as_deref(),
        dirs::home_dir().unwrap_or_else(|| PathBuf::from(".")),
    )
}

fn resolve_workspace_root(workspace_override: Option<&str>, home_dir: PathBuf) -> PathBuf {
    if let Some(path) = workspace_override {
        return PathBuf::from(strip_file_prefix(path));
    }
    home_dir.join(".studio").join("workspace")
}

// ---------------------------------------------------------------------------
// Unified launch parameter parsing (env → CLI args → URL query, parsed once)
// ---------------------------------------------------------------------------

fn parse_launch_config() -> LaunchConfig {
    let args: Vec<String> = env::args().skip(1).collect();
    let launch_url = parse_launch_url(&args);
    let query = launch_url.as_deref().and_then(extract_query_map);

    let initial_path = parse_env("STUDIO_PATH")
        .or_else(|| parse_arg(&args, "--path"))
        .or_else(|| query_get(&query, &["path"]))
        .map(|v| strip_file_prefix(&v));

    let initial_url = parse_env("STUDIO_URL")
        .or_else(|| parse_arg(&args, "--url"))
        .or_else(|| query_get(&query, &["project", "url"]));

    let initial_run_target = parse_env("STUDIO_RUN")
        .or_else(|| parse_arg(&args, "--run"))
        .or_else(|| query_get(&query, &["run"]))
        .map(|v| strip_file_prefix(&v));

    let explicit_runner = parse_env("STUDIO_MODE")
        .or_else(|| parse_arg_eq(&args, "--mode"))
        .or_else(|| query_get(&query, &["mode"]))
        .map(|v| v.eq_ignore_ascii_case("runner"))
        .unwrap_or(false)
        || args.iter().any(|a| a == "--runner");

    let mode = if explicit_runner || initial_run_target.is_some() {
        StudioMode::Runner
    } else {
        StudioMode::Dev
    };

    LaunchConfig { launch_url, initial_path, initial_url, initial_run_target, mode }
}

fn parse_launch_url(args: &[String]) -> Option<String> {
    parse_env_any(&["STUDIO_LAUNCH_URL", "VIBE_STUDIO_LAUNCH_URL"])
        .or_else(|| parse_arg_eq(args, "--launch-url"))
        .or_else(|| {
            args.iter()
                .find(|a| a.contains("://") && a.contains('?'))
                .map(|a| a.trim().to_string())
        })
}

fn parse_env(name: &str) -> Option<String> {
    env::var(name)
        .ok()
        .map(|v| v.trim().to_string())
        .filter(|v| !v.is_empty())
}

fn parse_env_any(names: &[&str]) -> Option<String> {
    names.iter().find_map(|name| parse_env(name))
}

fn parse_arg_eq(args: &[String], flag: &str) -> Option<String> {
    let prefix = format!("{}=", flag);
    args.iter()
        .find_map(|arg| arg.strip_prefix(&prefix))
        .map(|v| v.trim().to_string())
        .filter(|v| !v.is_empty())
}

fn parse_arg(args: &[String], flag: &str) -> Option<String> {
    parse_arg_eq(args, flag).or_else(|| {
        args.iter()
            .enumerate()
            .find(|(_, a)| a.trim() == flag)
            .and_then(|(i, _)| args.get(i + 1))
            .map(|v| v.trim().to_string())
            .filter(|v| !v.is_empty())
    })
}

fn strip_file_prefix(value: &str) -> String {
    value.strip_prefix("file://").unwrap_or(value).to_string()
}

fn extract_query_map(url: &str) -> Option<HashMap<String, String>> {
    let query = url.split_once('?')?.1;
    let mut map = HashMap::new();
    for pair in query.split('&') {
        if let Some((key, value)) = pair.split_once('=') {
            let decoded = url_decode(value);
            if !decoded.trim().is_empty() {
                map.entry(key.to_string()).or_insert(decoded);
            }
        }
    }
    if map.is_empty() { None } else { Some(map) }
}

fn query_get(query: &Option<HashMap<String, String>>, keys: &[&str]) -> Option<String> {
    let map = query.as_ref()?;
    keys.iter().find_map(|key| map.get(*key).cloned())
}

fn url_decode(input: &str) -> String {
    let mut bytes = Vec::with_capacity(input.len());
    let mut chars = input.as_bytes().iter();
    while let Some(&b) = chars.next() {
        if b == b'%' {
            let h1 = chars.next().and_then(|c| (*c as char).to_digit(16));
            let h2 = chars.next().and_then(|c| (*c as char).to_digit(16));
            if let (Some(h1), Some(h2)) = (h1, h2) {
                bytes.push((h1 * 16 + h2) as u8);
            } else {
                bytes.push(b);
            }
        } else if b == b'+' {
            bytes.push(b' ');
        } else {
            bytes.push(b);
        }
    }
    String::from_utf8(bytes).unwrap_or_else(|_| input.to_string())
}

fn detect_project_mode(path: &Path) -> ProjectMode {
    if is_module_root(path) {
        return ProjectMode::Module;
    }
    if path.is_file() {
        let parent = path.parent().unwrap_or(path);
        if is_module_root(parent) {
            return ProjectMode::Module;
        }
    }
    ProjectMode::SingleFile
}

fn detect_entry_path(path: &Path) -> Option<String> {
    if path.is_file() {
        return Some(path.to_string_lossy().to_string());
    }
    let main_vo = path.join("main.vo");
    if main_vo.is_file() {
        return Some(main_vo.to_string_lossy().to_string());
    }
    None
}

#[cfg(test)]
mod tests {
    use super::resolve_workspace_root;
    use std::path::PathBuf;

    #[test]
    fn resolve_workspace_root_uses_override_when_present() {
        let resolved = resolve_workspace_root(
            Some("/tmp/vo-studio-workspace"),
            PathBuf::from("/Users/example"),
        );
        assert_eq!(resolved, PathBuf::from("/tmp/vo-studio-workspace"));
    }

    #[test]
    fn resolve_workspace_root_defaults_under_home_directory() {
        let resolved = resolve_workspace_root(None, PathBuf::from("/Users/example"));
        assert_eq!(resolved, PathBuf::from("/Users/example/.studio/workspace"));
    }
}
