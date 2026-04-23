mod commands;
mod gui_runtime;
mod state;

use state::AppState;

pub fn run() {
    tauri::Builder::default()
        .manage(AppState::new())
        .invoke_handler(tauri::generate_handler![
            // Bootstrap
            commands::bootstrap::cmd_get_bootstrap_context,
            // Session
            commands::session::cmd_open_session,
            // Workspace / Filesystem
            commands::workspace::cmd_discover_projects,
            commands::workspace::cmd_list_dir,
            commands::workspace::cmd_stat_path,
            commands::workspace::cmd_read_file,
            commands::workspace::cmd_read_many,
            commands::workspace::cmd_write_file,
            commands::workspace::cmd_mkdir,
            commands::workspace::cmd_remove_entry,
            commands::workspace::cmd_rename_entry,
            commands::workspace::cmd_copy_entry,
            commands::workspace::cmd_grep,
            // Compiler
            commands::compiler::cmd_check_vo,
            commands::compiler::cmd_compile_vo,
            commands::compiler::cmd_format_vo,
            commands::compiler::cmd_build_vo,
            commands::compiler::cmd_dump_vo,
            commands::compiler::cmd_run_vo,
            commands::compiler::cmd_run_vo_stream,
            commands::compiler::cmd_stop_vo_run,
            // GUI / Runtime
            commands::gui::cmd_run_gui,
            commands::gui::cmd_send_gui_event,
            commands::gui::cmd_send_gui_event_async,
            commands::gui::cmd_push_island_transport,
            commands::gui::cmd_poll_gui_render,
            commands::gui::cmd_stop_gui,
            commands::gui::cmd_get_renderer_bridge_vfs_snapshot,
            // Extension / Toolchain
            commands::extension::cmd_vo_get_stream,
            commands::extension::cmd_vo_init,
            commands::extension::cmd_vo_version,
            commands::extension::cmd_list_installed_modules,
            // Dialog
            commands::dialog::cmd_pick_directory,
            commands::dialog::cmd_pick_file,
            // Project creation (bypass session root)
            commands::workspace::cmd_create_project_files,
            // Workspace discovery (bypass session root)
            commands::workspace::cmd_discover_workspace_projects,
            // Process
            commands::process::cmd_spawn_process,
            // Git
            commands::git::cmd_git_exec,
            // HTTP
            commands::http::cmd_http_request,
        ])
        .run(tauri::generate_context!())
        .expect("failed to run Studio");
}
