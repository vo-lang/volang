//! Execute and retain bounded release identity and native UI probes.
use super::{model::CiCommand, native_window, process};
use crate::release_archive::write_text_atomic;
use crate::release_config::sha256_file;
use crate::release_identity::ReleaseIdentity;
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

pub(crate) fn probe_cli_identity(
    root: &Path,
    binary: &Path,
    target: &str,
    identity: &ReleaseIdentity,
) -> Result<String> {
    let digest = sha256_file(binary)?;
    let nonce = SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos();
    let directory = root.join(format!(
        "target/ci/release-probes/{target}/cli-identity-{nonce}-{}",
        std::process::id()
    ));
    fs::create_dir_all(&directory)?;
    let spec = CiCommand {
        id: "release-cli-identity".into(),
        argv: vec![binary.to_string_lossy().into_owned(), "--version".into()],
        cwd: ".".into(),
        env: BTreeMap::new(),
        repo_env: BTreeMap::new(),
        timeout_seconds: 60,
        failure_kind: "product".into(),
        report: String::new(),
        stdout_result: String::new(),
    };
    let command = process::run_command(
        root,
        &spec,
        &directory,
        &BTreeMap::new(),
        &AtomicBool::new(false),
        Duration::from_secs(60),
    )?;
    let validation = (|| -> Result<()> {
        if !command.passed() {
            bail!("release CLI identity command failed: {:?}", command.status);
        }
        for path in [&command.stdout, &command.stderr] {
            if fs::metadata(root.join(path))?.len() > 4096 {
                bail!("release CLI identity output exceeds 4 KiB");
            }
        }
        validate_cli_identity_output(&fs::read_to_string(root.join(&command.stdout))?, identity)?;
        if sha256_file(binary)? != digest {
            bail!("release CLI changed during its identity probe");
        }
        Ok(())
    })();
    let result = ProbeResult {
        schema: "volang.release.cli-identity-probe.v1",
        complete: true,
        passed: validation.is_ok(),
        source_commit: identity.commit.clone(),
        target: target.into(),
        executable_sha256: digest.clone(),
        command,
        validation_error: validation.as_ref().err().map(|e| format!("{e:#}")),
    };
    write_text_atomic(
        &directory.join("result.json"),
        &(serde_json::to_string_pretty(&result)? + "\n"),
    )?;
    println!("release CLI identity probe: {}", directory.display());
    validation?;
    Ok(digest)
}

fn validate_cli_identity_output(stdout: &str, identity: &ReleaseIdentity) -> Result<()> {
    let expected = format!(
        "vo version {} ({}) {}",
        identity.version, identity.commit, identity.build_date
    );
    let actual = stdout
        .strip_suffix("\r\n")
        .or_else(|| stdout.strip_suffix('\n'))
        .unwrap_or(stdout);
    if actual != expected {
        bail!("release CLI identity mismatch: expected {expected:?}, got {actual:?}");
    }
    Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::release_identity::ReleasePurpose;
    use std::process::Command;

    fn identity() -> ReleaseIdentity {
        ReleaseIdentity {
            purpose: ReleasePurpose::Candidate,
            tag: None,
            candidate_id: Some("ci-fixture".into()),
            version: "0.1.4".into(),
            commit: "a".repeat(40),
            build_date: "2026-09-06".into(),
            source_date_epoch: 1788652800,
        }
    }

    #[test]
    fn cli_identity_requires_the_complete_exact_version_line() {
        let identity = identity();
        let line = format!("vo version 0.1.4 ({}) 2026-09-06", identity.commit);
        for suffix in ["", "\n", "\r\n"] {
            validate_cli_identity_output(&format!("{line}{suffix}"), &identity).unwrap();
        }
        for invalid in [
            line.replace("0.1.4", "0.1.5"),
            line.replace(&identity.commit, &"b".repeat(40)),
            line.replace(&identity.commit, "aaaaaaa"),
            line.replace("2026-09-06", "2026-09-05"),
            format!("{line}\nextra"),
            format!("{line}\n\n"),
            "vo version 0.1.4".into(),
            String::new(),
        ] {
            assert!(validate_cli_identity_output(&invalid, &identity).is_err());
        }
    }

    #[test]
    fn cli_probe_executes_identity_and_preserves_success_and_failure_evidence() {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root =
            std::env::temp_dir().join(format!("vo-cli-identity-{}-{nonce}", std::process::id()));
        fs::create_dir(&root).unwrap();
        let root = root.canonicalize().unwrap();
        let binary = root.join(format!("identity{}", std::env::consts::EXE_SUFFIX));
        let source = root.join("identity.rs");
        // Compute the commit at runtime: the public version contract cannot
        // depend on a compiler's choice of contiguous string storage.
        fs::write(
            &source,
            r#"
            fn main() {
                assert_eq!(std::env::args().nth(1).as_deref(), Some("--version"));
                let commit = std::hint::black_box("a").repeat(40);
                println!("vo version 0.1.4 ({commit}) 2026-09-06");
                if std::path::Path::new("fail").exists() { std::process::exit(7); }
            }
        "#,
        )
        .unwrap();
        let output = Command::new("rustc")
            .args(["--edition=2021", "-C", "opt-level=3", "-o"])
            .arg(&binary)
            .arg(&source)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        let identity = identity();
        let bytes = fs::read(&binary).unwrap();
        assert!(!bytes
            .windows(40)
            .any(|part| part == identity.commit.as_bytes()));
        let digest = sha256_file(&binary).unwrap();
        assert_eq!(
            probe_cli_identity(&root, &binary, "fixture", &identity).unwrap(),
            digest
        );
        let mut wrong = identity.clone();
        wrong.commit = "b".repeat(40);
        assert!(probe_cli_identity(&root, &binary, "fixture", &wrong).is_err());
        fs::write(root.join("fail"), "").unwrap();
        assert!(probe_cli_identity(&root, &binary, "fixture", &identity).is_err());
        let mut passed = 0;
        let mut rejected = 0;
        for entry in fs::read_dir(root.join("target/ci/release-probes/fixture")).unwrap() {
            let result: serde_json::Value = serde_json::from_slice(
                &fs::read(entry.unwrap().path().join("result.json")).unwrap(),
            )
            .unwrap();
            assert_eq!(result["schema"], "volang.release.cli-identity-probe.v1");
            assert_eq!(result["complete"], true);
            assert_eq!(result["executable_sha256"], digest);
            assert!(root
                .join(result["command"]["stdout"].as_str().unwrap())
                .is_file());
            assert!(root
                .join(result["command"]["stderr"].as_str().unwrap())
                .is_file());
            if result["passed"] == true {
                passed += 1;
                assert_eq!(result["command"]["exit_code"], 0);
                assert!(result["validation_error"].is_null());
            } else {
                rejected += 1;
                assert!(result["validation_error"].as_str().is_some());
            }
        }
        assert_eq!((passed, rejected), (1, 2));
        fs::remove_dir_all(root).unwrap();
    }
}
