use std::fs;
use std::path::Path;

use flate2::read::GzDecoder;
use tar::Archive;
use tempfile::TempDir;
use vo_module::schema::manifest::ReleaseManifest;

use crate::{stage_release, verify_repo, ArtifactInput, ReleaseError, StageReleaseOptions};

const TEST_COMMIT: &str = "0123456789abcdef0123456789abcdef01234567";

fn write_basic_repo(root: &Path) {
    fs::write(
        root.join("vo.mod"),
        "module github.com/acme/app\n\nvo 0.1.0\n",
    )
    .unwrap();
    // Write a minimal valid vo.lock with no resolved modules
    let lock_content = "version = 1\ncreated_by = \"vo test\"\n\n[root]\nmodule = \"github.com/acme/app\"\nvo = \"0.1.0\"\n";
    fs::write(root.join("vo.lock"), lock_content).unwrap();
    fs::write(root.join("main.vo"), "fn main() {}\n").unwrap();
}

#[test]
fn verify_repo_rejects_vo_sum() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(temp.path().join("vo.sum"), "legacy\n").unwrap();

    let err = verify_repo(temp.path()).unwrap_err();
    assert!(matches!(
        err,
        ReleaseError::ForbiddenVoSum { ref paths, .. } if paths == &vec![Path::new("vo.sum").to_path_buf()]
    ));
}

#[test]
fn verify_repo_rejects_legacy_alias_imports_in_blocks() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(
        temp.path().join("main.vo"),
        "import (\n    @\"gin\"\n    api @\"jwt\"\n)\n\nfn main() {}\n",
    )
    .unwrap();

    let err = verify_repo(temp.path()).unwrap_err();
    assert!(matches!(
        err,
        ReleaseError::LegacyAliasImports(ref violations)
            if violations == &vec![
                "main.vo:2: @\"gin\"".to_string(),
                "main.vo:3: api @\"jwt\"".to_string(),
            ]
    ));
}

#[test]
fn stage_release_rejects_orphaned_lock_entry() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());

    // Write a lock file with a resolved module that has no corresponding require in vo.mod
    let lock_content = r#"version = 1
created_by = "vo test"

[root]
module = "github.com/acme/app"
vo = "0.1.0"

[[resolved]]
path = "github.com/acme/lib"
version = "v1.2.0"
vo = "^0.1.0"
commit = "0123456789abcdef0123456789abcdef01234567"
release_manifest = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
source = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
deps = []
artifacts = []
"#;
    fs::write(temp.path().join("vo.lock"), lock_content).unwrap();

    let err = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "v0.1.0".to_string(),
            commit: Some(TEST_COMMIT.to_string()),
            artifacts: Vec::new(),
            out_dir: temp.path().join(".dist"),
        },
    )
    .unwrap_err();

    // The new module system detects orphaned modules in vo.lock
    assert!(matches!(err, ReleaseError::Module(ref msg) if msg.contains("orphaned")));
}

#[test]
fn stage_release_writes_manifest_and_artifacts() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let artifact_path = temp.path().join("demo.wasm");
    fs::write(&artifact_path, b"wasm-bits").unwrap();

    let staged = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "v0.1.0".to_string(),
            commit: Some(TEST_COMMIT.to_string()),
            artifacts: vec![ArtifactInput {
                kind: "extension-wasm".to_string(),
                target: "wasm32-unknown-unknown".to_string(),
                name: "demo.wasm".to_string(),
                path: artifact_path.clone(),
            }],
            out_dir: temp.path().join(".dist"),
        },
    )
    .unwrap();

    assert!(staged.source_path.is_file());
    assert!(staged.manifest_path.is_file());
    assert_eq!(staged.artifacts.len(), 1);
    assert!(staged.artifacts[0].output_path.is_file());

    let manifest_json = fs::read_to_string(&staged.manifest_path).unwrap();
    let manifest = ReleaseManifest::parse(&manifest_json).unwrap();
    assert_eq!(manifest.module.as_str(), "github.com/acme/app");
    assert_eq!(manifest.version.to_string(), "v0.1.0");
    assert_eq!(manifest.commit, TEST_COMMIT);
    assert_eq!(manifest.source.name, "app-v0.1.0.tar.gz");
    assert!(manifest
        .artifacts
        .iter()
        .any(|a| a.id.kind == "extension-wasm" && a.id.target == "wasm32-unknown-unknown"));
}

#[test]
fn stage_release_excludes_output_directory_from_source_package() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(temp.path().join("keep.txt"), "keep\n").unwrap();
    let out_dir = temp.path().join(".dist");
    fs::create_dir_all(&out_dir).unwrap();
    fs::write(out_dir.join("preexisting.txt"), "do not package\n").unwrap();

    let staged = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "v0.1.0".to_string(),
            commit: Some(TEST_COMMIT.to_string()),
            artifacts: Vec::new(),
            out_dir: out_dir.clone(),
        },
    )
    .unwrap();

    let file = fs::File::open(&staged.source_path).unwrap();
    let decoder = GzDecoder::new(file);
    let mut archive = Archive::new(decoder);
    let mut entries = archive
        .entries()
        .unwrap()
        .map(|entry| entry.unwrap().path().unwrap().display().to_string())
        .collect::<Vec<_>>();
    entries.sort();

    assert!(entries.iter().any(|entry| entry.ends_with("/keep.txt")));
    assert!(entries
        .iter()
        .all(|entry| !entry.contains(".dist/preexisting.txt")));
}
