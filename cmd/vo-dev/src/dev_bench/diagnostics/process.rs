//! Bounded diagnostic commands with durable failure logs.
use super::evidence;
use crate::process_tree::ProcessTree;
use anyhow::{anyhow, bail, Context, Result};
use serde_json::json;
use std::fs::{self, File};
use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};

pub(super) fn execute(
    mut command: Command,
    directory: &Path,
    cancelled: &AtomicBool,
    timeout: Duration,
) -> Result<()> {
    let argv = std::iter::once(command.get_program())
        .chain(command.get_args())
        .map(|a| a.to_string_lossy().into_owned())
        .collect::<Vec<_>>();
    evidence::write_json(
        &directory.join("command.json"),
        &json!({"argv":argv,"cwd":command.get_current_dir(),"environment_overrides":command.get_envs().map(|(key,value)|(key.to_string_lossy().into_owned(),value.map(|v|v.to_string_lossy().into_owned()))).collect::<Vec<_>>(),"timeout_seconds":timeout.as_secs()}),
    )?;
    let stdout = directory.join("stdout.log");
    let stderr = directory.join("stderr.log");
    command
        .stdin(Stdio::null())
        .stdout(File::create(&stdout)?)
        .stderr(File::create(&stderr)?);
    let start = Instant::now();
    let result = (|| -> Result<i32> {
        if cancelled.load(Ordering::Relaxed) {
            bail!("diagnostic run cancelled");
        }
        let mut tree = ProcessTree::spawn(command)?;
        loop {
            let failure = if cancelled.load(Ordering::Relaxed) {
                Some("diagnostic process cancelled")
            } else if start.elapsed() >= timeout {
                Some("diagnostic process timed out")
            } else if oversized_logs(&stdout, &stderr)? {
                Some("diagnostic process exceeded its log limit")
            } else {
                None
            };
            if let Some(reason) = failure {
                tree.terminate().with_context(|| reason)?;
                bail!(reason);
            }
            if let Some(status) = tree.try_wait()? {
                // The child may have completed its last write between the
                // earlier limit check and exit. Check the final logs as well.
                if oversized_logs(&stdout, &stderr)? {
                    bail!("diagnostic process exceeded its log limit");
                }
                return status
                    .code()
                    .ok_or_else(|| anyhow!("diagnostic process terminated: {status}"));
            }
            std::thread::sleep(Duration::from_millis(20));
        }
    })();
    evidence::write_json(
        &directory.join("process.json"),
        &json!({"exit_code":result.as_ref().ok(),"error":result.as_ref().err().map(|e|format!("{e:#}")),"orchestration_seconds":start.elapsed().as_secs_f64()}),
    )?;
    let code = result?;
    if code != 0 {
        bail!(
            "diagnostic command failed with exit code {code}; logs: {}",
            directory.display()
        );
    }
    Ok(())
}

fn oversized_logs(stdout: &Path, stderr: &Path) -> Result<bool> {
    const MAX_LOG_BYTES: u64 = 128 * 1024 * 1024;
    Ok(fs::metadata(stdout)?.len() > MAX_LOG_BYTES || fs::metadata(stderr)?.len() > MAX_LOG_BYTES)
}
