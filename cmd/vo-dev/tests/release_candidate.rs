use std::fs;
use std::path::Path;
use std::process::{Command, Output};
use std::time::{SystemTime, UNIX_EPOCH};

fn git(root: &Path, args: &[&str]) -> String {
    let output = Command::new("git")
        .args([
            "-c",
            "user.name=CI fixture",
            "-c",
            "user.email=ci@example.invalid",
            "-c",
            "commit.gpgSign=false",
        ])
        .args(args)
        .current_dir(root)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).unwrap().trim().into()
}

fn candidate(root: &Path, operation: &str, extra: &[(&str, &str)]) -> Output {
    let mut command = Command::new(env!("CARGO_BIN_EXE_vo-dev"));
    command
        .args(["release", "candidate", operation])
        .current_dir(root);
    for name in [
        "VO_RELEASE_TAG",
        "VO_BUILD_COMMIT",
        "VO_BUILD_DATE",
        "SOURCE_DATE_EPOCH",
        "GITHUB_SHA",
        "GITHUB_REF_NAME",
    ] {
        command.env_remove(name);
    }
    command
        .env("GITHUB_REF_NAME", "codex/candidate-fixture")
        .envs(extra.iter().copied());
    command.output().unwrap()
}

#[test]
fn candidate_cli_binds_clean_source_and_rejects_release_or_foreign_identity() {
    let nonce = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let root = std::env::temp_dir().join(format!(
        "vo-release-candidate-{}-{nonce}",
        std::process::id()
    ));
    fs::create_dir_all(root.join("eng")).unwrap();
    fs::write(
        root.join("Cargo.toml"),
        "[workspace.package]\nversion = \"0.1.4\"\n",
    )
    .unwrap();
    fs::write(
        root.join("eng/release.toml"),
        include_str!("../../../eng/release.toml"),
    )
    .unwrap();
    git(&root, &["init", "--quiet"]);
    git(&root, &["add", "Cargo.toml", "eng/release.toml"]);
    git(&root, &["commit", "--quiet", "-m", "candidate fixture"]);
    let head = git(&root, &["rev-parse", "HEAD"]);
    let output = candidate(&root, "metadata", &[("GITHUB_SHA", &head)]);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let metadata: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(metadata["purpose"], "candidate");
    assert!(metadata["tag"].is_null());
    assert_eq!(metadata["commit"], head);
    assert_eq!(metadata["candidate_id"], format!("ci-{head}"));
    assert_eq!(
        metadata["artifact_directory"],
        format!("target/ci/release-candidate/{head}")
    );
    assert!(candidate(&root, "matrix", &[]).status.success());
    for (name, value) in [
        ("VO_RELEASE_TAG", ""),
        ("VO_RELEASE_TAG", "v0.1.4"),
        ("VO_BUILD_COMMIT", "bad"),
        ("GITHUB_SHA", "bad"),
        ("SOURCE_DATE_EPOCH", "0"),
        ("VO_BUILD_DATE", "bad"),
    ] {
        assert!(
            !candidate(&root, "metadata", &[(name, value)])
                .status
                .success(),
            "{name}"
        );
    }
    for operation in ["publish", "update-homebrew", "notes", "verify"] {
        assert!(
            !candidate(&root, operation, &[]).status.success(),
            "{operation}"
        );
    }
    fs::write(root.join("untracked.vo"), "package main").unwrap();
    assert!(!candidate(&root, "metadata", &[]).status.success());
    fs::remove_file(root.join("untracked.vo")).unwrap();
    fs::write(
        root.join("Cargo.toml"),
        "[workspace.package]\nversion = \"0.1.5\"\n",
    )
    .unwrap();
    assert!(!candidate(&root, "metadata", &[]).status.success());
    fs::remove_dir_all(root).unwrap();
}
