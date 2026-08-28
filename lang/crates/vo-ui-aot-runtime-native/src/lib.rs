//! Native process entry for statically linked Volang UI AOT applications.

#[cfg(not(test))]
use std::ffi::c_char;

#[cfg(not(test))]
unsafe fn run_embedded_ui(argc: i32, argv: *const *const c_char) -> Result<i32, String> {
    let automation = std::env::var_os("VO_UI_AUTOMATION_EXIT_AFTER_FRAMES").is_some()
        || std::env::var_os("VO_UI_AUTOMATION_CLICKS").is_some();
    if automation {
        eprintln!("[VO:UI:CERTIFY] loading embedded Native AOT image");
    }
    let vm = unsafe {
        vo_aot_runtime_core::load_embedded_vm(argc, argv, |vm, module| {
            let registry = vm
                .extern_registry_mut()
                .map_err(|error| format!("failed to configure AOT UI providers: {error:?}"))?;
            vo_ui_vm::register_module(registry, module.module())
                .map_err(|error| format!("failed to register AOT UI providers: {error}"))
        })
    }?;
    if automation {
        eprintln!("[VO:UI:CERTIFY] embedded Native AOT VM is ready");
    }
    let mut config = vo_ui_shell_native::NativeDesktopConfig::default();
    if let Some(value) = std::env::var_os("VO_UI_AUTOMATION_EXIT_AFTER_FRAMES") {
        let value = value
            .to_str()
            .ok_or_else(|| "VO_UI_AUTOMATION_EXIT_AFTER_FRAMES must be UTF-8".to_string())?;
        let frames = value.parse::<u64>().map_err(|_| {
            "VO_UI_AUTOMATION_EXIT_AFTER_FRAMES must be a positive integer".to_string()
        })?;
        config.exit_after_presented_frames = std::num::NonZeroU64::new(frames);
        if config.exit_after_presented_frames.is_none() {
            return Err(
                "VO_UI_AUTOMATION_EXIT_AFTER_FRAMES must be a positive integer".to_string(),
            );
        }
    }
    let clicks = automation_values("VO_UI_AUTOMATION_CLICKS")?;
    let expected_text = automation_values("VO_UI_AUTOMATION_EXPECT_TEXT")?;
    match (clicks, expected_text) {
        (Some(clicks), Some(expected_text)) => {
            config.automation = Some(vo_ui_shell_native::NativeDesktopAutomation {
                clicks,
                expected_text,
            });
        }
        (None, None) => {}
        _ => {
            return Err(
                "VO_UI_AUTOMATION_CLICKS and VO_UI_AUTOMATION_EXPECT_TEXT must be set together"
                    .to_string(),
            );
        }
    }
    if automation {
        eprintln!("[VO:UI:CERTIFY] entering native desktop event loop");
    }
    vo_ui_shell_native::run_desktop(vm, config)
        .map_err(|error| format!("native AOT UI failed: {error}"))?;
    Ok(0)
}

#[cfg(not(test))]
fn automation_values(name: &str) -> Result<Option<Vec<String>>, String> {
    let Some(value) = std::env::var_os(name) else {
        return Ok(None);
    };
    let value = value
        .to_str()
        .ok_or_else(|| format!("{name} must be UTF-8"))?;
    let values = value.split('|').map(str::to_string).collect::<Vec<_>>();
    if values.is_empty() || values.iter().any(String::is_empty) {
        return Err(format!("{name} must contain non-empty | separated values"));
    }
    Ok(Some(values))
}

/// C process entry called by the generated object-file `main` trampoline.
///
/// # Safety
///
/// `argv` must follow the platform process-entry ABI for at least `argc`
/// valid C-string pointers.
#[cfg(not(test))]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vo_aot_start(argc: i32, argv: *const *const c_char) -> i32 {
    match std::panic::catch_unwind(|| unsafe { run_embedded_ui(argc, argv) }) {
        Ok(Ok(code)) => code,
        Ok(Err(error)) => {
            eprintln!("vo UI AOT runtime error: {error}");
            1
        }
        Err(_) => 101,
    }
}
