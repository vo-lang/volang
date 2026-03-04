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
    #[serde(default)]
    pub env: std::collections::HashMap<String, String>,
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

impl ShellError {
    pub fn not_supported(op: &str) -> Self {
        Self { code: "ERR_NOT_SUPPORTED".into(), message: format!("op '{}' is not supported in local backend", op) }
    }
    pub fn not_found(msg: &str) -> Self {
        Self { code: "ERR_NOT_FOUND".into(), message: msg.to_string() }
    }
    pub fn access_denied(msg: &str) -> Self {
        Self { code: "ERR_ACCESS_DENIED".into(), message: msg.to_string() }
    }
    pub fn already_exists(msg: &str) -> Self {
        Self { code: "ERR_ALREADY_EXISTS".into(), message: msg.to_string() }
    }
    pub fn vo_compile(msg: &str) -> Self {
        Self { code: "ERR_VO_COMPILE".into(), message: msg.to_string() }
    }
    pub fn vo_runtime(msg: &str) -> Self {
        Self { code: "ERR_VO_RUNTIME".into(), message: msg.to_string() }
    }
    pub fn tool_missing(msg: &str) -> Self {
        Self { code: "ERR_TOOL_MISSING".into(), message: msg.to_string() }
    }
    pub fn internal(msg: &str) -> Self {
        Self { code: "ERR_INTERNAL".into(), message: msg.to_string() }
    }

    pub fn fs(err: std::io::Error, path: impl AsRef<std::path::Path>) -> Self {
        use std::io::ErrorKind;
        let msg = format!("{}: {}", path.as_ref().display(), err);
        match err.kind() {
            ErrorKind::NotFound         => Self::not_found(&msg),
            ErrorKind::PermissionDenied => Self::access_denied(&msg),
            ErrorKind::AlreadyExists    => Self::already_exists(&msg),
            _                           => Self::internal(&msg),
        }
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
