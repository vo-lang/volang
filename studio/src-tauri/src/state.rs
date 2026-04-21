use std::collections::HashMap;
use std::env;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use vo_app_runtime::SyncRenderBuffer;
use vo_web::BrowserRuntimePlan;

use crate::commands::pathing::is_module_root;
use crate::gui_runtime::GuestHandle;

// ---------------------------------------------------------------------------
// Domain enums
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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

#[derive(Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LaunchSpec {
    pub proj: Option<String>,
    pub mode: StudioMode,
}

#[derive(Clone, serde::Serialize)]
#[serde(tag = "kind")]
pub enum SessionSource {
    #[serde(rename = "workspace")]
    Workspace,
    #[serde(rename = "path")]
    Path {
        path: String,
    },
    #[serde(rename = "github_repo")]
    GithubRepo {
        owner: String,
        repo: String,
        #[serde(rename = "requestedRef")]
        requested_ref: Option<String>,
        #[serde(rename = "resolvedCommit")]
        resolved_commit: Option<String>,
        subdir: Option<String>,
        #[serde(rename = "htmlUrl")]
        html_url: String,
        #[serde(rename = "sourceCacheRoot")]
        source_cache_root: String,
    },
}

#[derive(Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ShareInfo {
    pub canonical_url: String,
    pub shareable: bool,
    pub reason: Option<String>,
}

// ---------------------------------------------------------------------------
// Serialized types (sent to frontend)
// ---------------------------------------------------------------------------

#[derive(Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BootstrapContext {
    pub workspace_root: String,
    pub launch: Option<LaunchSpec>,
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
    pub source: Option<SessionSource>,
    pub share: Option<ShareInfo>,
}

// ---------------------------------------------------------------------------
// Internal launch configuration (parsed once at startup)
// ---------------------------------------------------------------------------

struct LaunchConfig {
    launch: Option<LaunchSpec>,
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
    last_browser_runtime: Mutex<Option<BrowserRuntimePlan>>,
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
            last_browser_runtime: Mutex::new(None),
            workspace_root,
            launch,
        }
    }

    pub fn bootstrap_context(&self) -> BootstrapContext {
        BootstrapContext {
            workspace_root: self.workspace_root.to_string_lossy().to_string(),
            launch: self.launch.launch.clone(),
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

    pub fn set_last_browser_runtime(&self, runtime: BrowserRuntimePlan) {
        *self.last_browser_runtime.lock().unwrap() = Some(runtime);
    }

    pub fn last_browser_runtime(&self) -> Option<BrowserRuntimePlan> {
        self.last_browser_runtime.lock().unwrap().clone()
    }

    pub fn clear_guest_runtime(&self) {
        let _ = self.guest_runtime.lock().unwrap().take();
        *self.last_browser_runtime.lock().unwrap() = None;
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
    source: Option<SessionSource>,
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
    let share = build_share_info(source.as_ref());
    SessionInfo {
        root: canonical.to_string_lossy().to_string(),
        origin,
        project_mode,
        entry_path,
        single_file_run,
        source,
        share,
    }
}

fn build_share_info(source: Option<&SessionSource>) -> Option<ShareInfo> {
    let SessionSource::GithubRepo {
        owner,
        repo,
        resolved_commit,
        subdir,
        ..
    } = source? else {
        return Some(ShareInfo {
            canonical_url: String::new(),
            shareable: false,
            reason: Some("Only GitHub sessions can be shared".to_string()),
        });
    };
    let Some(commit) = resolved_commit.as_deref() else {
        return Some(ShareInfo {
            canonical_url: String::new(),
            shareable: false,
            reason: Some("GitHub session is not pinned to a commit".to_string()),
        });
    };
    let mut url = url::Url::parse("https://volang.dev/")
        .expect("share base URL must be valid");
    let project_url = build_pinned_github_project_url(owner, repo, commit, subdir.as_deref());
    {
        let mut query = url.query_pairs_mut();
        query.append_pair("mode", "runner");
        query.append_pair("proj", &project_url);
    }
    Some(ShareInfo {
        canonical_url: url.to_string(),
        shareable: true,
        reason: None,
    })
}

fn build_pinned_github_project_url(owner: &str, repo: &str, commit: &str, subdir: Option<&str>) -> String {
    let mut url = url::Url::parse(&format!("https://github.com/{owner}/{repo}"))
        .expect("GitHub project URL must be valid");
    let trimmed_subdir = subdir
        .map(|value| value.trim().trim_matches('/'))
        .filter(|value| !value.is_empty());
    let mut path = format!("/{owner}/{repo}/tree/{commit}");
    if let Some(value) = trimmed_subdir {
        path.push('/');
        path.push_str(value);
    }
    url.set_path(&path);
    url.to_string()
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

    let mode = parse_env_any(&["STUDIO_MODE", "VIBE_STUDIO_MODE"])
        .or_else(|| parse_arg_eq(&args, "--mode"))
        .or_else(|| query_get(&query, &["mode"]))
        .and_then(|value| parse_studio_mode(&value))
        .or_else(|| {
            if args.iter().any(|arg| arg == "--runner") {
                Some(StudioMode::Runner)
            } else {
                None
            }
        })
        .unwrap_or(StudioMode::Dev);

    let proj = parse_env_any(&["STUDIO_PROJ", "VIBE_STUDIO_PROJ"])
        .or_else(|| parse_arg(&args, "--proj"))
        .or_else(|| parse_project_arg(&args))
        .or_else(|| query_get(&query, &["proj"]))
        .map(|value| strip_file_prefix(&value));

    let launch = proj.map(|proj| LaunchSpec {
        proj: Some(proj),
        mode,
    });

    LaunchConfig { launch, mode }
}

fn parse_studio_mode(value: &str) -> Option<StudioMode> {
    match value.trim().to_ascii_lowercase().as_str() {
        "dev" => Some(StudioMode::Dev),
        "runner" => Some(StudioMode::Runner),
        _ => None,
    }
}

fn parse_project_arg(args: &[String]) -> Option<String> {
    let mut skip_value = false;
    for arg in args {
        let trimmed = arg.trim();
        if skip_value {
            skip_value = false;
            continue;
        }
        if matches!(trimmed, "--launch-url" | "--mode" | "--proj") {
            skip_value = true;
            continue;
        }
        if trimmed == "--runner"
            || trimmed.starts_with("--launch-url=")
            || trimmed.starts_with("--mode=")
            || trimmed.starts_with("--proj=")
            || trimmed.starts_with('-')
        {
            continue;
        }
        return Some(trimmed.to_string());
    }
    None
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
    use super::{parse_project_arg, parse_studio_mode, resolve_workspace_root, StudioMode};
    use std::collections::HashMap;
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

    #[test]
    fn parse_studio_mode_accepts_dev_and_runner() {
        assert_eq!(parse_studio_mode("dev"), Some(StudioMode::Dev));
        assert_eq!(parse_studio_mode("runner"), Some(StudioMode::Runner));
        assert_eq!(parse_studio_mode("weird"), None);
    }

    #[test]
    fn parse_project_arg_skips_known_flags_and_returns_positional_project() {
        let args = vec![
            "--runner".to_string(),
            "--mode".to_string(),
            "runner".to_string(),
            "https://github.com/vo-lang/MarbleRush".to_string(),
        ];
        let proj = parse_project_arg(&args);
        assert_eq!(proj.as_deref(), Some("https://github.com/vo-lang/MarbleRush"));
    }

    #[test]
    fn query_map_can_feed_proj_launch() {
        let query = Some(HashMap::from([
            ("proj".to_string(), "https://github.com/vo-lang/MarbleRush/tree/abc123".to_string()),
            ("mode".to_string(), "runner".to_string()),
        ]));
        let proj = super::query_get(&query, &["proj"]);
        let mode = super::query_get(&query, &["mode"]).and_then(|value| parse_studio_mode(&value));
        assert_eq!(proj.as_deref(), Some("https://github.com/vo-lang/MarbleRush/tree/abc123"));
        assert_eq!(mode, Some(StudioMode::Runner));
    }
}
