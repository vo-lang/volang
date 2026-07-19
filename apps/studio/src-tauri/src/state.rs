use std::collections::HashMap;
use std::env;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use vo_app_runtime::SyncRenderBuffer;
use vo_module::project::ProjectContextOptions;
use vo_module::workspace::WorkspaceDiscovery;
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum SessionOrigin {
    Workspace,
    RunTarget,
    Url,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum WorkspaceDiscoveryMode {
    Auto,
    Disabled,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum SessionIsolation {
    SingleFile,
}

impl WorkspaceDiscoveryMode {
    pub fn project_context_options(self) -> ProjectContextOptions {
        let workspace = match self {
            Self::Auto => WorkspaceDiscovery::Auto,
            Self::Disabled => WorkspaceDiscovery::Disabled,
        };
        ProjectContextOptions::new(workspace)
    }
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LaunchSpec {
    pub proj: Option<String>,
    pub mode: StudioMode,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub isolation: Option<SessionIsolation>,
}

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(tag = "kind")]
pub enum SessionSource {
    #[serde(rename = "workspace")]
    Workspace,
    #[serde(rename = "path")]
    Path { path: String },
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

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionInfo {
    pub root: String,
    pub origin: SessionOrigin,
    pub project_mode: ProjectMode,
    pub entry_path: Option<String>,
    pub single_file_run: bool,
    pub workspace_discovery: WorkspaceDiscoveryMode,
    pub source: Option<SessionSource>,
    pub share: Option<ShareInfo>,
}

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PreparedSession {
    pub token: String,
    pub session: SessionInfo,
}

// ---------------------------------------------------------------------------
// Internal launch configuration (parsed once at startup)
// ---------------------------------------------------------------------------

struct LaunchConfig {
    launch: Option<LaunchSpec>,
    mode: StudioMode,
}

#[derive(Clone)]
pub struct SessionSnapshot {
    root: PathBuf,
    single_file_run: bool,
    workspace_discovery: WorkspaceDiscoveryMode,
}

impl SessionSnapshot {
    pub fn new(
        root: PathBuf,
        single_file_run: bool,
        workspace_discovery: WorkspaceDiscoveryMode,
    ) -> Self {
        Self {
            root,
            single_file_run,
            workspace_discovery,
        }
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn single_file_run(&self) -> bool {
        self.single_file_run
    }

    pub fn project_context_options(&self) -> ProjectContextOptions {
        self.workspace_discovery.project_context_options()
    }
}

#[derive(Clone)]
struct ActiveSession {
    info: Option<SessionInfo>,
    snapshot: SessionSnapshot,
}

struct PendingSession {
    candidate: PreparedSession,
    snapshot: SessionSnapshot,
}

struct SessionState {
    active: ActiveSession,
    pending: Option<PendingSession>,
    rollback: Option<ActiveSession>,
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
    session: Mutex<SessionState>,
    session_candidate_id: AtomicU64,
    console_run: Arc<Mutex<Option<Arc<AtomicBool>>>>,
    gui_session_id: AtomicU64,
    guest_runtime: Mutex<Option<GuestRuntime>>,
    last_browser_runtime: Mutex<Option<BrowserRuntimePlan>>,
}

impl AppState {
    pub fn new() -> Self {
        let workspace_root = prepare_workspace_root(default_workspace());
        let launch = parse_launch_config();
        Self {
            session: Mutex::new(SessionState {
                active: ActiveSession {
                    info: None,
                    snapshot: SessionSnapshot::new(
                        workspace_root.clone(),
                        false,
                        WorkspaceDiscoveryMode::Auto,
                    ),
                },
                pending: None,
                rollback: None,
            }),
            session_candidate_id: AtomicU64::new(0),
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
        self.session
            .lock()
            .unwrap()
            .active
            .snapshot
            .root()
            .to_path_buf()
    }

    pub fn session_snapshot(&self) -> SessionSnapshot {
        self.session.lock().unwrap().active.snapshot.clone()
    }

    pub fn prepare_session(&self, info: SessionInfo, snapshot: SessionSnapshot) -> PreparedSession {
        let id = self.session_candidate_id.fetch_add(1, Ordering::Relaxed);
        let candidate = PreparedSession {
            token: format!("{}-{id}", std::process::id()),
            session: info,
        };
        let mut session = self.session.lock().unwrap();
        session.pending = Some(PendingSession {
            candidate: candidate.clone(),
            snapshot,
        });
        session.rollback = None;
        candidate
    }

    pub fn pending_session_snapshot(
        &self,
        candidate: &PreparedSession,
    ) -> Result<SessionSnapshot, String> {
        let session = self.session.lock().unwrap();
        let pending = session
            .pending
            .as_ref()
            .ok_or_else(|| "Prepared session is no longer available".to_string())?;
        if pending.candidate != *candidate {
            return Err("Prepared session does not match the pending candidate".to_string());
        }
        Ok(pending.snapshot.clone())
    }

    pub fn activate_session(&self, candidate: PreparedSession) -> Result<SessionInfo, String> {
        let mut session = self.session.lock().unwrap();
        let pending = session
            .pending
            .as_ref()
            .ok_or_else(|| "Prepared session is no longer available".to_string())?;
        if pending.candidate != candidate {
            return Err("Prepared session does not match the pending candidate".to_string());
        }
        let pending = session
            .pending
            .take()
            .expect("validated pending session must exist");
        session.rollback = session.active.info.as_ref().map(|_| session.active.clone());
        session.active = ActiveSession {
            info: Some(pending.candidate.session.clone()),
            snapshot: pending.snapshot,
        };
        Ok(pending.candidate.session)
    }

    pub fn discard_prepared_session(&self, candidate: &PreparedSession) -> Result<(), String> {
        let mut session = self.session.lock().unwrap();
        let pending = session
            .pending
            .as_ref()
            .ok_or_else(|| "Prepared session is no longer available".to_string())?;
        if pending.candidate != *candidate {
            return Err("Prepared session does not match the pending candidate".to_string());
        }
        session.pending = None;
        Ok(())
    }

    pub fn restore_session(&self, previous: &SessionInfo) -> Result<SessionInfo, String> {
        let mut session = self.session.lock().unwrap();
        let rollback = session
            .rollback
            .as_ref()
            .ok_or_else(|| "Previous session is no longer available".to_string())?;
        if rollback.info.as_ref() != Some(previous) {
            return Err("Session does not match the rollback candidate".to_string());
        }
        session.active = session
            .rollback
            .take()
            .expect("validated rollback session must exist");
        session.pending = None;
        Ok(previous.clone())
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

    pub fn install_guest_runtime(
        &self,
        session_id: u64,
        guest: GuestHandle,
        render_buffer: Arc<SyncRenderBuffer>,
    ) {
        if self.gui_session_id.load(Ordering::SeqCst) != session_id {
            return;
        }
        *self.guest_runtime.lock().unwrap() = Some(GuestRuntime {
            handle: guest,
            render_buffer,
        });
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

    pub fn with_guest<R>(
        &self,
        f: impl FnOnce(&GuestHandle) -> Result<R, String>,
    ) -> Result<R, String> {
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
    workspace_discovery: WorkspaceDiscoveryMode,
    source: Option<SessionSource>,
) -> Result<SessionInfo, String> {
    let canonical = path.canonicalize().map_err(|error| {
        format!(
            "cannot canonicalize session path {}: {error}",
            path.display()
        )
    })?;
    let project_mode = if single_file_run {
        ProjectMode::SingleFile
    } else {
        detect_project_mode(&canonical)?
    };
    let entry_path = explicit_entry
        .map(|entry| entry.to_string_lossy().to_string())
        .or_else(|| detect_entry_path(&canonical));
    let share = build_share_info(source.as_ref());
    Ok(SessionInfo {
        root: canonical.to_string_lossy().to_string(),
        origin,
        project_mode,
        entry_path,
        single_file_run,
        workspace_discovery,
        source,
        share,
    })
}

fn build_share_info(source: Option<&SessionSource>) -> Option<ShareInfo> {
    let SessionSource::GithubRepo {
        owner,
        repo,
        resolved_commit,
        subdir,
        ..
    } = source?
    else {
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
    let mut url = url::Url::parse("https://volang.dev/").expect("share base URL must be valid");
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

fn build_pinned_github_project_url(
    owner: &str,
    repo: &str,
    commit: &str,
    subdir: Option<&str>,
) -> String {
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
        parse_env("STUDIO_WORKSPACE").as_deref(),
        dirs::home_dir().unwrap_or_else(|| PathBuf::from(".")),
    )
}

fn prepare_workspace_root(configured: PathBuf) -> PathBuf {
    if std::fs::create_dir_all(&configured).is_ok() {
        configured.canonicalize().unwrap_or(configured)
    } else {
        configured
    }
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

    let mode = parse_env("STUDIO_MODE")
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

    let proj = parse_env("STUDIO_PROJ")
        .or_else(|| parse_arg(&args, "--proj"))
        .or_else(|| parse_project_arg(&args))
        .or_else(|| query_get(&query, &["proj"]))
        .map(|value| strip_file_prefix(&value));

    let launch = proj.map(|proj| LaunchSpec {
        proj: Some(proj),
        mode,
        isolation: None,
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
    parse_env("STUDIO_LAUNCH_URL")
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
    if map.is_empty() {
        None
    } else {
        Some(map)
    }
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

fn detect_project_mode(path: &Path) -> Result<ProjectMode, String> {
    if is_module_root(path)? {
        return Ok(ProjectMode::Module);
    }
    if path.is_file() {
        let parent = path.parent().unwrap_or(path);
        if is_module_root(parent)? {
            return Ok(ProjectMode::Module);
        }
    }
    Ok(ProjectMode::SingleFile)
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
    use super::{
        parse_project_arg, parse_studio_mode, prepare_workspace_root, resolve_workspace_root,
        session_info, ActiveSession, AppState, LaunchConfig, SessionOrigin, SessionSnapshot,
        SessionState, StudioMode, WorkspaceDiscoveryMode,
    };
    use std::collections::HashMap;
    use std::fs;
    use std::path::PathBuf;
    use std::sync::atomic::AtomicU64;
    use std::sync::{Arc, Mutex};
    use std::time::{SystemTime, UNIX_EPOCH};

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
            "https://github.com/acme/example".to_string(),
        ];
        let proj = parse_project_arg(&args);
        assert_eq!(proj.as_deref(), Some("https://github.com/acme/example"));
    }

    #[test]
    fn query_map_can_feed_proj_launch() {
        let query = Some(HashMap::from([
            (
                "proj".to_string(),
                "https://github.com/acme/example/tree/abc123".to_string(),
            ),
            ("mode".to_string(), "runner".to_string()),
        ]));
        let proj = super::query_get(&query, &["proj"]);
        let mode = super::query_get(&query, &["mode"]).and_then(|value| parse_studio_mode(&value));
        assert_eq!(
            proj.as_deref(),
            Some("https://github.com/acme/example/tree/abc123")
        );
        assert_eq!(mode, Some(StudioMode::Runner));
    }

    #[test]
    fn session_transition_is_atomic_and_rollback_is_one_shot() {
        let root = temp_test_dir("session-transition");
        let first_root = root.join("first");
        let second_root = root.join("second");
        fs::create_dir_all(&first_root).unwrap();
        fs::create_dir_all(&second_root).unwrap();
        let state = test_state(root.clone());

        let first_info = session_info(
            &first_root,
            SessionOrigin::RunTarget,
            None,
            false,
            WorkspaceDiscoveryMode::Auto,
            None,
        )
        .unwrap();
        let first = state.prepare_session(
            first_info.clone(),
            SessionSnapshot::new(first_root.clone(), false, WorkspaceDiscoveryMode::Auto),
        );
        assert_eq!(state.session_root(), root);
        state.activate_session(first).unwrap();
        assert_eq!(state.session_root(), first_root);

        let second_info = session_info(
            &second_root,
            SessionOrigin::RunTarget,
            None,
            true,
            WorkspaceDiscoveryMode::Disabled,
            None,
        )
        .unwrap();
        let second = state.prepare_session(
            second_info,
            SessionSnapshot::new(second_root.clone(), true, WorkspaceDiscoveryMode::Disabled),
        );
        assert_eq!(state.session_root(), first_root);
        assert_eq!(
            state.pending_session_snapshot(&second).unwrap().root(),
            second_root
        );
        state.activate_session(second.clone()).unwrap();
        assert_eq!(state.session_root(), second_root);
        assert!(state.activate_session(second).is_err());

        assert_eq!(state.restore_session(&first_info).unwrap(), first_info);
        assert_eq!(state.session_root(), first_root);
        assert!(state.restore_session(&first_info).is_err());

        let pending = state.prepare_session(
            session_info(
                &second_root,
                SessionOrigin::RunTarget,
                None,
                false,
                WorkspaceDiscoveryMode::Auto,
                None,
            )
            .unwrap(),
            SessionSnapshot::new(second_root, false, WorkspaceDiscoveryMode::Auto),
        );
        let mut forged = pending.clone();
        forged.token.push_str("-forged");
        assert!(state.discard_prepared_session(&forged).is_err());
        state.discard_prepared_session(&pending).unwrap();
        assert_eq!(state.session_root(), first_root);

        fs::remove_dir_all(root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn workspace_root_uses_the_canonical_directory_identity() {
        use std::os::unix::fs::symlink;

        let root = temp_test_dir("canonical-workspace");
        let target = root.join("target");
        let alias = root.join("alias");
        fs::create_dir_all(&target).unwrap();
        symlink(&target, &alias).unwrap();

        assert_eq!(
            prepare_workspace_root(alias),
            target.canonicalize().unwrap()
        );
        fs::remove_dir_all(root).unwrap();
    }

    fn test_state(workspace_root: PathBuf) -> AppState {
        AppState {
            session: Mutex::new(SessionState {
                active: ActiveSession {
                    info: None,
                    snapshot: SessionSnapshot::new(
                        workspace_root.clone(),
                        false,
                        WorkspaceDiscoveryMode::Auto,
                    ),
                },
                pending: None,
                rollback: None,
            }),
            session_candidate_id: AtomicU64::new(0),
            console_run: Arc::new(Mutex::new(None)),
            gui_session_id: AtomicU64::new(0),
            guest_runtime: Mutex::new(None),
            last_browser_runtime: Mutex::new(None),
            workspace_root,
            launch: LaunchConfig {
                launch: None,
                mode: StudioMode::Dev,
            },
        }
    }

    fn temp_test_dir(label: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let path = std::env::temp_dir().join(format!(
            "studio-state-{label}-{}-{nonce}",
            std::process::id()
        ));
        fs::create_dir_all(&path).unwrap();
        path
    }
}
