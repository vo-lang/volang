mod app_plan;
mod commands;
mod gui_runtime;
mod state;
mod webview_native_smoke;

use state::AppState;

pub fn run() {
    tauri::Builder::default()
        .manage(AppState::new())
        .manage(webview_native_smoke::WebviewNativeSmokeState::default())
        .setup(|app| webview_native_smoke::configure_initial_window(app))
        .invoke_handler(tauri::generate_handler![
            // Bootstrap
            commands::bootstrap::cmd_get_bootstrap_context,
            // Session
            commands::session::cmd_prepare_session,
            commands::session::cmd_activate_session,
            commands::session::cmd_discard_prepared_session,
            commands::session::cmd_restore_session,
            // Workspace / Filesystem
            commands::workspace::cmd_list_dir,
            commands::workspace::cmd_list_prepared_session_dir,
            commands::workspace::cmd_stat_path,
            commands::workspace::cmd_read_file,
            commands::workspace::cmd_read_binary,
            commands::workspace::cmd_read_prepared_session_file,
            commands::workspace::cmd_write_file,
            commands::workspace::cmd_write_binary,
            commands::workspace::cmd_mkdir,
            commands::workspace::cmd_remove_entry,
            commands::workspace::cmd_rename_entry,
            // Compiler
            commands::compiler::cmd_dump_vo,
            commands::compiler::cmd_run_vo_stream,
            commands::compiler::cmd_stop_vo_run,
            // GUI / Runtime
            commands::gui::cmd_run_gui,
            commands::gui::cmd_attach_webview_bridge,
            commands::gui::cmd_poll_webview_bridge,
            commands::gui::cmd_submit_webview_bridge,
            commands::gui::cmd_restart_webview_bridge_with_snapshots,
            commands::gui::cmd_resolve_platform_surface,
            commands::gui::cmd_register_platform_surface_shortcuts,
            commands::gui::cmd_send_gui_event,
            commands::gui::cmd_send_gui_event_async,
            commands::gui::cmd_push_island_transport,
            commands::gui::cmd_open_framework_lane,
            commands::gui::cmd_load_framework_provider,
            commands::gui::cmd_unload_framework_provider,
            commands::gui::cmd_begin_framework_provider,
            commands::gui::cmd_ready_framework_provider,
            commands::gui::cmd_abort_framework_provider,
            commands::gui::cmd_close_framework_provider,
            commands::gui::cmd_poll_framework_lane,
            commands::gui::cmd_submit_framework_lane,
            commands::gui::cmd_submit_framework_lane_batch,
            commands::gui::cmd_poll_display_timing_request,
            commands::gui::cmd_submit_display_pulse,
            commands::gui::cmd_poll_gui_render,
            commands::gui::cmd_poll_game_render,
            commands::gui::cmd_submit_game_render_result,
            commands::gui::cmd_poll_platform_request,
            commands::gui::cmd_poll_vogui_subscriptions,
            commands::gui::cmd_submit_vogui_subscription_event,
            commands::gui::cmd_complete_platform_request,
            commands::gui::cmd_stop_gui,
            // Real Tauri WebView composition smoke
            webview_native_smoke::cmd_webview_native_smoke_begin,
            webview_native_smoke::cmd_webview_native_smoke_owner,
            webview_native_smoke::cmd_webview_native_smoke_attach,
            webview_native_smoke::cmd_webview_native_smoke_poll,
            webview_native_smoke::cmd_webview_native_smoke_submit,
            webview_native_smoke::cmd_webview_native_smoke_take,
            webview_native_smoke::cmd_webview_native_smoke_restart,
            webview_native_smoke::cmd_webview_native_smoke_replace,
            webview_native_smoke::cmd_webview_native_smoke_finish,
            // Module / Toolchain
            commands::extension::cmd_vo_init,
            // Dialog
            commands::dialog::cmd_pick_directory,
            commands::dialog::cmd_pick_file,
            commands::dialog::cmd_save_file,
            // Project creation
            commands::workspace::cmd_create_workspace_files,
            commands::workspace::cmd_create_project_file,
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
