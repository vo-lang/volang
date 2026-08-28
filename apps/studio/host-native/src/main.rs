use std::path::PathBuf;

use vo_engine::RunMode;
use vo_studio_native::{
    apply_studio_automation, launch_preview_artifact, preview_artifact_argument, NativeStudioHost,
};

fn main() {
    if let Err(error) = launch() {
        eprintln!("Volang Studio failed to start: {error}");
        std::process::exit(1);
    }
}

fn launch() -> Result<(), String> {
    let arguments = std::env::args_os().skip(1).collect::<Vec<_>>();
    if let Some(artifact) = preview_artifact_argument(arguments.clone())? {
        return launch_preview_artifact(artifact);
    }
    if let Some(argument) = arguments.first() {
        return Err(format!(
            "unknown Volang Studio option {}",
            argument.to_string_lossy()
        ));
    }
    let application = std::env::var_os("VOLANG_STUDIO_APP")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../entry/host"));
    let workspace = std::env::var_os("VOLANG_STUDIO_WORKSPACE")
        .map(PathBuf::from)
        .unwrap_or(std::env::current_dir().map_err(|error| error.to_string())?);
    let output = vo_engine::compile_path_with_auto_install(&application)
        .map_err(|error| format!("Studio application compilation failed: {error}"))?;
    let vm = vo_engine::build_native_gui_vm_for_mode(output, RunMode::Jit)?;
    let host = NativeStudioHost::open(workspace).map_err(|error| error.to_string())?;
    let mut config = vo_ui_shell_native::NativeDesktopConfig {
        title: "Volang Studio".to_string(),
        width_points: 1440.0,
        height_points: 900.0,
        min_width_points: 720.0,
        min_height_points: 480.0,
        ..vo_ui_shell_native::NativeDesktopConfig::default()
    };
    config.runtime.max_system_requests_per_pump = 4_096;
    apply_studio_automation(&mut config)?;
    vo_ui_shell_native::run_desktop_with_host_invocation(vm, config, host.handler())
        .map_err(|error| error.to_string())
}
