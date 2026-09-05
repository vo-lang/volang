//! Run the executable linked from the unpacked release UI runtime.
use super::{model::CiCommand, native_window, process};
use crate::release_archive::write_text_atomic;
use crate::release_config::sha256_file;
use anyhow::{bail, Result};
use serde::Serialize;
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;
use std::sync::atomic::AtomicBool;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

#[derive(Serialize)]
struct ProbeResult {
    schema: &'static str,
    complete: bool,
    passed: bool,
    source_commit: String,
    target: String,
    executable_sha256: String,
    command: process::CommandResult,
    validation_error: Option<String>,
}

pub(crate) fn probe_native_ui(root: &Path, target: &str, source_commit: String) -> Result<()> {
    let suffix = if target.contains("windows") {
        ".exe"
    } else {
        ""
    };
    let binary = format!("target/{target}/release/ui-native-aot-smoke{suffix}");
    let digest = sha256_file(&root.join(&binary))?;
    let nonce = SystemTime::now().duration_since(UNIX_EPOCH)?.as_millis();
    let directory = root.join(format!(
        "target/ci/release-probes/{target}/{nonce}-{}",
        std::process::id()
    ));
    fs::create_dir_all(&directory)?;
    let mut env = BTreeMap::from([("VO_UI_AUTOMATION_EXIT_AFTER_FRAMES".into(), "1".into())]);
    let mut argv = vec![root.join(&binary).to_string_lossy().into_owned()];
    if target.contains("linux") {
        argv.splice(0..0, ["xvfb-run".into(), "--auto-servernum".into()]);
        env.insert("WGPU_BACKEND".into(), "vulkan".into());
    }
    let spec = CiCommand {
        id: "packaged-ui-native-window".into(),
        argv,
        cwd: ".".into(),
        env,
        repo_env: BTreeMap::new(),
        timeout_seconds: 120,
        failure_kind: "product".into(),
        report: "native-window".into(),
        stdout_result: String::new(),
    };
    let mut command = process::run_command(
        root,
        &spec,
        &directory,
        &BTreeMap::new(),
        &AtomicBool::new(false),
        Duration::from_secs(120),
    )?;
    let validation = (|| -> Result<()> {
        if !command.passed() {
            bail!("packaged native window command failed");
        }
        let read = |relative: &str| -> Result<String> {
            let path = root.join(relative);
            if fs::metadata(&path)?.len() > 1024 * 1024 {
                bail!("native window probe log exceeds 1 MiB");
            }
            Ok(fs::read_to_string(path)?)
        };
        command.test_counts = Some(native_window::validate_streams(
            &read(&command.stdout)?,
            &read(&command.stderr)?,
            &spec.env,
        )?);
        if sha256_file(&root.join(&binary))? != digest {
            bail!("packaged native executable changed during the probe");
        }
        Ok(())
    })();
    let result = ProbeResult {
        schema: "volang.release.native-ui-probe.v1",
        complete: true,
        passed: validation.is_ok(),
        source_commit,
        target: target.into(),
        executable_sha256: digest,
        command,
        validation_error: validation.as_ref().err().map(|e| format!("{e:#}")),
    };
    write_text_atomic(
        &directory.join("result.json"),
        &(serde_json::to_string_pretty(&result)? + "\n"),
    )?;
    println!("native package probe: {}", directory.display());
    validation
}
