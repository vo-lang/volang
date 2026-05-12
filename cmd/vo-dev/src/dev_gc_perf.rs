use anyhow::{bail, Result};
use std::path::Path;
use std::process::Command;

pub(crate) fn cmd_gc_perf(root: &Path, args: Vec<String>) -> Result<()> {
    let mut release = false;
    let mut forwarded = Vec::new();
    for arg in args {
        if arg == "--release" {
            release = true;
        } else {
            forwarded.push(arg);
        }
    }

    let mut cmd = Command::new("cargo");
    cmd.current_dir(root)
        .arg("run")
        .arg("-p")
        .arg("vo-runtime")
        .arg("--example")
        .arg("gc_perf");
    if release {
        cmd.arg("--release");
    }
    cmd.arg("--").args(forwarded);

    let status = cmd.status()?;
    if !status.success() {
        bail!("gc-perf failed with status {status}");
    }
    Ok(())
}
