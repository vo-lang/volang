use std::path::PathBuf;

use super::{ShellRequest, ShellResponse};
use super::vo_runner::VoRunner;

// =============================================================================
// ShellRouter — thin shim over VoRunner
//
// All routing and operation logic lives in the Vo shell handler programs at
// studio/vo/shell/*.vo.  Rust only provides the VM runtime, output capture,
// and subprocess streaming infrastructure.
// =============================================================================

pub struct ShellRouter {
    runner: VoRunner,
}

impl ShellRouter {
    pub fn new(workspace_root: PathBuf) -> Self {
        Self { runner: VoRunner::new(workspace_root) }
    }

    pub fn handle(&self, req: ShellRequest, app: &tauri::AppHandle) -> ShellResponse {
        self.runner.handle(req, app)
    }
}
