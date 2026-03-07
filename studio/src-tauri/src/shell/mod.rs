use serde::{Deserialize, Serialize};
use serde_json::Value;

pub mod vo_runner;

pub use vo_runner::VoRunner;

// =============================================================================
// Wire types — mirroring protocol.ts on the Rust side
// =============================================================================

#[derive(Deserialize)]
pub struct ShellRequest {
    pub id:  String,
    pub cwd: String,
    pub op:  Value,  // op is a JSON object with a "kind" discriminant field
}

#[derive(Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum ShellResponse {
    #[serde(rename = "ok")]
    Ok {
        id:   String,
        data: Value,
    },
    #[serde(rename = "stream")]
    Stream {
        id:     String,
        job_id: String,
    },
    #[serde(rename = "error")]
    Error {
        id:      String,
        code:    String,
        message: String,
    },
}

#[derive(Serialize)]
pub struct ShellInitResponse {
    #[serde(rename = "workspaceRoot")]
    pub workspace_root: String,
    pub capabilities:   Vec<String>,
}

// =============================================================================
// ShellError — internal error type; maps to ShellErrorCode in protocol.ts
// =============================================================================

pub struct ShellError {
    pub code:    String,
    pub message: String,
}

// =============================================================================
// StudioError — Serialize-able error type for Tauri GUI commands.
// Sent to the JS frontend as { code, message } so ShellError can be
// constructed on the TypeScript side without string-matching heuristics.
// =============================================================================

#[derive(Serialize, Clone, Debug)]
pub struct StudioError {
    pub code:    String,
    pub message: String,
}

impl std::fmt::Display for StudioError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}] {}", self.code, self.message)
    }
}

impl StudioError {
    pub fn vo_compile(msg: &str) -> Self {
        Self { code: "ERR_VO_COMPILE".into(), message: msg.to_string() }
    }
    pub fn vo_runtime(msg: &str) -> Self {
        Self { code: "ERR_VO_RUNTIME".into(), message: msg.to_string() }
    }
    pub fn access_denied(msg: &str) -> Self {
        Self { code: "ERR_ACCESS_DENIED".into(), message: msg.to_string() }
    }
}

impl ShellError {
    pub fn tool_missing(msg: &str) -> Self {
        Self { code: "ERR_TOOL_MISSING".into(), message: msg.to_string() }
    }
    pub fn internal(msg: &str) -> Self {
        Self { code: "ERR_INTERNAL".into(), message: msg.to_string() }
    }
}

// =============================================================================
// Phase 2 capabilities declared by the local backend
// =============================================================================

pub fn local_capabilities() -> Vec<String> {
    vec![
        "fs".into(),
        "vo.run".into(),
        "vo.check".into(),
        "vo.build".into(),
        "vo.dump".into(),
        "vo.compile".into(),
        "vo.init".into(),
        "vo.clean".into(),
        "vo.version".into(),
        "git".into(),
        "zip".into(),
        "proc.spawn".into(),
        "http".into(),
        "gui".into(),
    ]
}

// =============================================================================
// Tauri commands
// =============================================================================

#[tauri::command]
pub fn cmd_shell_init(state: tauri::State<'_, crate::AppState>) -> Result<ShellInitResponse, String> {
    Ok(ShellInitResponse {
        workspace_root: state.workspace_root.to_string_lossy().to_string(),
        capabilities:   local_capabilities(),
    })
}

#[tauri::command]
pub fn cmd_shell_exec(
    req:   ShellRequest,
    state: tauri::State<'_, crate::AppState>,
    app:   tauri::AppHandle,
) -> Result<ShellResponse, String> {
    Ok(state.shell_runner.handle(req, &app))
}
