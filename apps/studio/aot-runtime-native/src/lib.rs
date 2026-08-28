//! Native AOT process entry for the Studio-specific capability host.

#[cfg(not(test))]
use std::ffi::c_char;
#[cfg(not(test))]
use std::path::PathBuf;

#[cfg(not(test))]
unsafe fn run_embedded_studio(argc: i32, argv: *const *const c_char) -> Result<i32, String> {
    if let Some(artifact) =
        vo_studio_native::preview_artifact_argument(std::env::args_os().skip(1))?
    {
        vo_studio_native::launch_preview_artifact(artifact)?;
        return Ok(0);
    }
    let vm = unsafe {
        vo_aot_runtime_core::load_embedded_vm(argc, argv, |vm, module| {
            let registry = vm.extern_registry_mut().map_err(|error| {
                format!("failed to configure Studio AOT UI providers: {error:?}")
            })?;
            vo_ui_vm::register_module(registry, module.module())
                .map_err(|error| format!("failed to register Studio AOT UI providers: {error}"))
        })
    }?;
    let workspace = std::env::var_os("VOLANG_STUDIO_WORKSPACE")
        .map(PathBuf::from)
        .unwrap_or(std::env::current_dir().map_err(|error| error.to_string())?);
    let host =
        vo_studio_native::NativeStudioHost::open(workspace).map_err(|error| error.to_string())?;
    let mut config = vo_ui_shell_native::NativeDesktopConfig {
        title: "Volang Studio".to_string(),
        width_points: 1440.0,
        height_points: 900.0,
        min_width_points: 720.0,
        min_height_points: 480.0,
        ..vo_ui_shell_native::NativeDesktopConfig::default()
    };
    config.runtime.max_system_requests_per_pump = 4_096;
    vo_studio_native::apply_studio_automation(&mut config)?;
    vo_ui_shell_native::run_desktop_with_host_invocation(vm, config, host.handler())
        .map_err(|error| error.to_string())?;
    Ok(0)
}

/// C process entry called by the generated Studio object-file trampoline.
///
/// # Safety
///
/// `argv` must follow the platform process-entry ABI for at least `argc`
/// valid C-string pointers.
#[cfg(not(test))]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vo_aot_start(argc: i32, argv: *const *const c_char) -> i32 {
    match std::panic::catch_unwind(|| unsafe { run_embedded_studio(argc, argv) }) {
        Ok(Ok(code)) => code,
        Ok(Err(error)) => {
            eprintln!("Volang Studio AOT runtime error: {error}");
            1
        }
        Err(_) => 101,
    }
}
