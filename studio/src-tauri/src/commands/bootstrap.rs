#[tauri::command]
pub fn cmd_get_bootstrap_context(state: tauri::State<'_, crate::state::AppState>) -> crate::state::BootstrapContext {
    state.bootstrap_context()
}
