use std::fs;
use std::io::{Read, Write};
use std::path::Path;
use std::process::{Command, Stdio};

use flate2::read::GzDecoder;
use tar::Archive;
use tempfile::TempDir;
use vo_module::ext_manifest::DeclaredArtifactId;
use vo_module::schema::manifest::ReleaseManifest;
use vo_module::schema::{PackageManifest, SourceFileMode};

use crate::{stage_release, verify_repo, ArtifactInput, ReleaseError, StageReleaseOptions};

fn write_basic_repo(root: &Path) {
    fs::write(
        root.join("vo.mod"),
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
    )
    .unwrap();
    fs::write(root.join("main.vo"), "fn main() {}\n").unwrap();
    init_test_git_repo(root);
    git_add(root, Path::new("vo.mod"));
    git_add(root, Path::new("main.vo"));
}

fn write_module_metadata(root: &Path, metadata: String) {
    let mod_path = root.join("vo.mod");
    let mut content = fs::read_to_string(&mod_path).unwrap();
    if !content.ends_with('\n') {
        content.push('\n');
    }
    content.push('\n');
    content.push_str(&metadata);
    if !content.ends_with('\n') {
        content.push('\n');
    }
    fs::write(&mod_path, content).unwrap();
    git_add(root, Path::new("vo.mod"));
}

fn init_test_git_repo(root: &Path) {
    git(root, &["init"]);
    git(
        root,
        &["config", "user.email", "vo-release-tests@example.invalid"],
    );
    git(root, &["config", "user.name", "Vo Release Tests"]);
    git(root, &["config", "core.filemode", "true"]);
}

fn git_add(root: &Path, path: &Path) {
    git(root, &["add", path.to_str().unwrap()]);
    git(root, &["commit", "--no-gpg-sign", "-m", "test fixture"]);
}

fn git(root: &Path, args: &[&str]) {
    let output = Command::new("git")
        .arg("-C")
        .arg(root)
        .args(args)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "git {:?} failed: {}{}",
        args,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

fn git_head(root: &Path) -> String {
    git_stdout(root, &["rev-parse", "HEAD"])
}

fn git_stdout(root: &Path, args: &[&str]) -> String {
    let output = Command::new("git")
        .arg("-C")
        .arg(root)
        .args(args)
        .output()
        .unwrap();
    assert!(output.status.success());
    String::from_utf8(output.stdout).unwrap().trim().to_string()
}

fn git_stdin_stdout(root: &Path, args: &[&str], input: &[u8]) -> String {
    let mut child = Command::new("git")
        .arg("-C")
        .arg(root)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child.stdin.take().unwrap().write_all(input).unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(
        output.status.success(),
        "git {:?} failed: {}{}",
        args,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    String::from_utf8(output.stdout).unwrap().trim().to_string()
}

fn stage_options(temp: &TempDir, artifacts: Vec<ArtifactInput>) -> StageReleaseOptions {
    StageReleaseOptions {
        version: "0.1.0".to_string(),
        commit: Some(git_head(temp.path())),
        artifacts,
        out_dir: temp.path().join(".dist"),
    }
}

fn write_artifact_input(
    root: &Path,
    kind: &str,
    target: &str,
    name: &str,
    bytes: &[u8],
) -> ArtifactInput {
    let path = root.join(name);
    fs::write(&path, bytes).unwrap();
    git_add(root, Path::new(name));
    ArtifactInput {
        kind: kind.to_string(),
        target: target.to_string(),
        name: name.to_string(),
        path,
    }
}

fn standalone_wasm_manifest(name: &str, wasm: &str) -> String {
    format!(
        "[extension]\nname = \"{}\"\n\n[extension.wasm]\nkind = \"standalone\"\nwasm = \"{}\"\n",
        name, wasm,
    )
}

fn bindgen_wasm_manifest(name: &str, wasm: &str, js_glue: &str) -> String {
    format!(
        "[extension]\nname = \"{}\"\n\n[extension.wasm]\nkind = \"bindgen\"\nwasm = \"{}\"\njs = \"{}\"\n",
        name, wasm, js_glue,
    )
}

fn native_manifest(name: &str, native_targets: &[&str]) -> String {
    let targets = native_targets
        .iter()
        .map(|target| format!("\"{target}\""))
        .collect::<Vec<_>>()
        .join(", ");
    let rendered = format!(
        "[extension]\nname = \"{}\"\n\n[extension.native]\ntargets = [{}]\n\n",
        name, targets,
    );
    rendered
}

fn bindgen_manifest(name: &str, native_targets: &[&str], wasm: &str, js_glue: &str) -> String {
    let mut rendered = native_manifest(name, native_targets);
    rendered.push_str(&format!(
        "[extension.wasm]\nkind = \"bindgen\"\nwasm = \"{}\"\njs = \"{}\"\n",
        wasm, js_glue,
    ));
    rendered
}

fn source_archive_entries(path: &Path) -> Vec<String> {
    let mut entries = source_archive_entries_in_order(path);
    entries.sort();
    entries
}

fn source_archive_entries_in_order(path: &Path) -> Vec<String> {
    let file = fs::File::open(path).unwrap();
    let decoder = GzDecoder::new(file);
    let mut archive = Archive::new(decoder);
    archive
        .entries()
        .unwrap()
        .map(|entry| entry.unwrap().path().unwrap().display().to_string())
        .collect()
}

fn source_archive_file(path: &Path, suffix: &str) -> Vec<u8> {
    let file = fs::File::open(path).unwrap();
    let decoder = GzDecoder::new(file);
    let mut archive = Archive::new(decoder);
    for entry in archive.entries().unwrap() {
        let mut entry = entry.unwrap();
        if entry.path().unwrap().to_string_lossy().ends_with(suffix) {
            let mut bytes = Vec::new();
            entry.read_to_end(&mut bytes).unwrap();
            return bytes;
        }
    }
    panic!("source archive is missing {suffix}");
}

fn source_archive_mode(path: &Path, suffix: &str) -> u32 {
    let file = fs::File::open(path).unwrap();
    let decoder = GzDecoder::new(file);
    let mut archive = Archive::new(decoder);
    for entry in archive.entries().unwrap() {
        let entry = entry.unwrap();
        if entry.path().unwrap().to_string_lossy().ends_with(suffix) {
            return entry.header().mode().unwrap();
        }
    }
    panic!("source archive is missing {suffix}");
}

#[test]
fn verify_repo_rejects_vo_sum() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(temp.path().join("vo.sum"), "stale\n").unwrap();

    let err = verify_repo(temp.path()).unwrap_err();
    assert!(matches!(
        err,
        ReleaseError::ForbiddenVoSum { ref paths, .. } if paths == &vec![Path::new("vo.sum").to_path_buf()]
    ));
}

#[test]
fn verify_repo_rejects_noncanonical_case_vo_sum() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(temp.path().join("VO.SUM"), "stale\n").unwrap();

    let error = verify_repo(temp.path()).unwrap_err();

    assert!(error.to_string().contains("VO.SUM"), "{error}");
}

#[test]
fn verify_repo_rejects_full_fold_alias_of_vo_sum() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(temp.path().join("vo.ſum"), "stale\n").unwrap();

    let error = verify_repo(temp.path()).unwrap_err();

    assert!(matches!(
        error,
        ReleaseError::ForbiddenVoSum { ref paths, .. }
            if paths == &vec![Path::new("vo.ſum").to_path_buf()]
    ));
}

#[test]
fn verify_repo_rejects_tracked_project_transaction_lock() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(temp.path().join(".vo-project.lock"), "generated\n").unwrap();
    git_add(temp.path(), Path::new(".vo-project.lock"));

    let error = verify_repo(temp.path()).unwrap_err();

    assert!(error.to_string().contains("transaction state"), "{error}");
    assert!(error.to_string().contains(".vo-project.lock"), "{error}");
}

#[test]
fn verify_repo_rejects_tracked_project_transaction_journal() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(temp.path().join(".vo-project.transaction"), "generated\n").unwrap();
    git_add(temp.path(), Path::new(".vo-project.transaction"));

    let error = verify_repo(temp.path()).unwrap_err();

    assert!(error.to_string().contains("transaction state"), "{error}");
    assert!(
        error.to_string().contains(".vo-project.transaction"),
        "{error}"
    );
}

#[test]
fn verify_repo_requires_exact_tracked_vo_mod_spelling() {
    let temp = TempDir::new().unwrap();
    fs::write(
        temp.path().join("VO.MOD"),
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
    )
    .unwrap();
    fs::write(temp.path().join("main.vo"), "fn main() {}\n").unwrap();
    init_test_git_repo(temp.path());
    git_add(temp.path(), Path::new("VO.MOD"));
    git_add(temp.path(), Path::new("main.vo"));

    let error = verify_repo(temp.path()).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("root module manifest \"VO.MOD\"")
            && error.to_string().contains("portable alias of vo.mod"),
        "{error}",
    );
}

#[test]
fn verify_repo_rejects_case_alias_for_optional_vo_lock() {
    let temp = TempDir::new().unwrap();
    fs::write(
        temp.path().join("vo.mod"),
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
    )
    .unwrap();
    fs::write(
        temp.path().join("VO.LOCK"),
        "invalid but rejected before parsing\n",
    )
    .unwrap();
    fs::write(temp.path().join("main.vo"), "fn main() {}\n").unwrap();
    init_test_git_repo(temp.path());
    git_add(temp.path(), Path::new("vo.mod"));
    git_add(temp.path(), Path::new("VO.LOCK"));
    git_add(temp.path(), Path::new("main.vo"));

    let error = verify_repo(temp.path()).unwrap_err();
    assert!(error.to_string().contains("non-canonical case alias"));
    assert!(error.to_string().contains("VO.LOCK"));
}

#[test]
fn verify_repo_rejects_invalid_alias_imports_in_blocks() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(
        temp.path().join("main.vo"),
        "import (\n    @\"gin\"\n    api @\"jwt\"\n)\n\nfn main() {}\n",
    )
    .unwrap();
    git_add(temp.path(), Path::new("main.vo"));

    let err = verify_repo(temp.path()).unwrap_err();
    assert!(matches!(
        err,
        ReleaseError::InvalidAliasImports(ref violations)
            if violations == &vec![
                "main.vo:2: @\"gin\"".to_string(),
                "main.vo:3: api @\"jwt\"".to_string(),
            ]
    ));
}

#[test]
fn verify_repo_alias_scan_ignores_comments_raw_strings_and_non_import_at_tokens() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(
        temp.path().join("main.vo"),
        concat!(
            "/*\nimport @\"commented\"\n*/\n",
            "const note = `\nimport @\"raw-string\"\n`\n",
            "fn worker() {}\n",
            "fn main() { go @ worker() }\n",
        ),
    )
    .unwrap();
    git_add(temp.path(), Path::new("main.vo"));

    verify_repo(temp.path()).unwrap();
}

#[test]
fn verify_repo_rejects_dirty_tracked_source() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(
        temp.path().join("main.vo"),
        "fn main() { println(\"dirty\") }\n",
    )
    .unwrap();

    let error = verify_repo(temp.path()).unwrap_err();

    assert!(
        error
            .to_string()
            .contains("tracked working-tree files differ from the git index"),
        "{error}"
    );
}

#[test]
fn verify_repo_rejects_missing_declared_build_input() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        concat!(
            "[extension]\n",
            "name = \"app\"\n\n",
            "[extension.native]\n",
            "targets = [\"aarch64-apple-darwin\"]\n\n",
            "[build.native]\n",
            "kind = \"cargo\"\n",
            "manifest = \"rust/Cargo.toml\"\n",
        )
        .to_string(),
    );

    let error = verify_repo(temp.path()).unwrap_err();

    assert!(error.to_string().contains("rust/Cargo.toml"), "{error}");
    assert!(error.to_string().contains("release commit"), "{error}");
}

#[test]
fn stage_release_rejects_invalid_identity_inputs_before_writing_output() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let mut options = stage_options(&temp, Vec::new());
    options.commit = Some("INVALID".to_string());

    let error = stage_release(temp.path(), &options).unwrap_err();
    assert!(matches!(error, ReleaseError::ManifestSerialize(_)));
    assert!(!options.out_dir.exists());
}

#[test]
fn stage_release_rejects_noncanonical_artifact_names_before_writing_output() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let mut options = stage_options(
        &temp,
        vec![ArtifactInput {
            kind: "extension-wasm".to_string(),
            target: "wasm32-unknown-unknown".to_string(),
            name: "../escape.wasm".to_string(),
            path: temp.path().join("main.vo"),
        }],
    );
    options.out_dir = temp.path().join(".dist-invalid-artifact");

    let error = stage_release(temp.path(), &options).unwrap_err();
    assert!(matches!(error, ReleaseError::ManifestSerialize(_)));
    assert!(!options.out_dir.exists());
}

#[cfg(unix)]
#[test]
fn stage_release_rejects_tracked_symbolic_link_sources() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    std::os::unix::fs::symlink("main.vo", temp.path().join("linked.vo")).unwrap();
    git_add(temp.path(), Path::new("linked.vo"));

    let error = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap_err();
    assert!(error.to_string().contains("regular files"));
}

#[test]
fn stage_release_rejects_orphaned_lock_entry() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        "[dependencies]\n\"github.com/acme/lib\" = \"^1.2.0\"\n".to_string(),
    );

    // The first module closes the declared root edge; the second is unreachable.
    let lock_content = r#"version = 3

[root]
module = "github.com/acme/app"
vo = "^0.1.0"

[[module]]
path = "github.com/acme/lib"
version = "1.2.0"
vo = "^0.1.0"
release = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
dependencies = []

[[module]]
path = "github.com/acme/orphan"
version = "1.0.0"
vo = "^0.1.0"
release = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
dependencies = []
"#;
    fs::write(temp.path().join("vo.lock"), lock_content).unwrap();
    git_add(temp.path(), Path::new("vo.lock"));

    let err = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "0.1.0".to_string(),
            commit: Some(git_head(temp.path())),
            artifacts: Vec::new(),
            out_dir: temp.path().join(".dist"),
        },
    )
    .unwrap_err();

    assert!(matches!(err, ReleaseError::Module(ref msg) if msg.contains("orphaned")));
}

#[test]
fn stage_release_writes_manifest_and_artifacts() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(temp.path(), standalone_wasm_manifest("demo", "demo.wasm"));
    let artifact_path = temp.path().join("demo.wasm");
    fs::write(&artifact_path, b"wasm-bits").unwrap();
    git_add(temp.path(), Path::new("demo.wasm"));

    let staged = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "0.1.0".to_string(),
            commit: Some(git_head(temp.path())),
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
    assert!(staged.package_path.is_file());
    assert_eq!(staged.artifacts.len(), 1);
    assert!(staged.artifacts[0].output_path.is_file());
    assert_eq!(
        source_archive_file(&staged.source_path, "/vo.package.json"),
        fs::read(&staged.package_path).unwrap()
    );
    assert_eq!(
        staged.assets(),
        vec![
            staged.manifest_path.as_path(),
            staged.package_path.as_path(),
            staged.source_path.as_path(),
            staged.artifacts[0].output_path.as_path(),
        ]
    );
    let mut staged_names = fs::read_dir(&staged.out_dir)
        .unwrap()
        .map(|entry| entry.unwrap().file_name().into_string().unwrap())
        .collect::<Vec<_>>();
    staged_names.sort();
    let mut asset_names = staged
        .assets()
        .into_iter()
        .map(|path| path.file_name().unwrap().to_str().unwrap().to_string())
        .collect::<Vec<_>>();
    asset_names.sort();
    assert_eq!(staged_names, asset_names);
    assert!(fs::read_dir(temp.path()).unwrap().all(|entry| {
        !entry
            .unwrap()
            .file_name()
            .to_string_lossy()
            .starts_with(".vo-release-stage-")
    }));

    let manifest_json = fs::read_to_string(&staged.manifest_path).unwrap();
    let manifest = ReleaseManifest::parse(&manifest_json).unwrap();
    assert_eq!(manifest.render().unwrap(), manifest_json);
    assert!(manifest_json.ends_with('\n'));
    assert!(!manifest_json.contains("\r\n"));
    assert_eq!(manifest.schema_version, 2);
    for legacy in ["module_root", "files_size", "files_digest", "web_manifest"] {
        assert!(!manifest_json.contains(legacy), "{legacy}");
    }
    assert!(!manifest_json.contains("\"require\""));
    assert_eq!(manifest.module.as_str(), "github.com/acme/app");
    assert_eq!(manifest.version.to_string(), "0.1.0");
    assert_eq!(manifest.commit, staged.commit);
    assert_eq!(manifest.source.name, "source.tar.gz");
    assert_eq!(staged.source_name, "source.tar.gz");
    assert!(source_archive_entries(&staged.source_path)
        .iter()
        .all(|entry| entry.starts_with("source/")));
    let package_bytes = fs::read(&staged.package_path).unwrap();
    assert_eq!(manifest.package.size, staged.package_size);
    assert_eq!(manifest.package.digest.to_string(), staged.package_digest);
    assert_eq!(manifest.package.size, package_bytes.len() as u64);
    assert_eq!(
        manifest.package.digest,
        vo_module::digest::Digest::from_sha256(&package_bytes)
    );
    let package = PackageManifest::parse(&package_bytes).unwrap();
    assert_eq!(package.render().unwrap(), package_bytes);
    assert_eq!(package_bytes.last(), Some(&b'\n'));
    assert!(!package_bytes.windows(2).any(|pair| pair == b"\r\n"));
    assert!(package.files.iter().any(|entry| entry.path == "vo.mod"));
    assert!(package.files.iter().any(|entry| entry.path == "main.vo"));
    assert!(package
        .files
        .iter()
        .all(|entry| entry.mode == SourceFileMode::Regular));
    assert_ne!(manifest.source.digest, manifest.package.digest);
    assert!(manifest
        .artifacts
        .iter()
        .any(|a| a.id.kind == "extension-wasm" && a.id.target == "wasm32-unknown-unknown"));
}

#[test]
fn stage_release_fixed_source_names_support_max_length_legal_versions() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let prefix = "0.1.0-";
    let version = format!(
        "{prefix}{}",
        "a".repeat(vo_module::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES - 1 - prefix.len())
    );
    let mut options = stage_options(&temp, Vec::new());
    options.version = version;

    let staged = stage_release(temp.path(), &options).unwrap();
    let manifest =
        ReleaseManifest::parse(&fs::read_to_string(&staged.manifest_path).unwrap()).unwrap();

    assert_eq!(staged.source_name, "source.tar.gz");
    assert_eq!(staged.source_path.file_name().unwrap(), "source.tar.gz");
    assert_eq!(manifest.source.name, "source.tar.gz");
    assert!(source_archive_entries(&staged.source_path)
        .iter()
        .all(|entry| entry.starts_with("source/")));
}

#[test]
fn release_diagnostic_summaries_are_deterministic_and_bounded() {
    let summary = crate::error::bounded_sorted_diagnostic(
        (0..100_000).rev().map(|index| format!("path-{index:06}")),
    );
    assert!(summary.starts_with(
        "path-000000, path-000001, path-000002, path-000003, path-000004, path-000005, path-000006, path-000007"
    ));
    assert!(summary.ends_with("(and 99992 more)"));
    assert!(!summary.contains("path-000008"));
    assert!(summary.len() < 256);

    let long = crate::error::bounded_sorted_diagnostic((0..32).rev().map(|index| {
        format!(
            "{index:02}-{}",
            "a".repeat(vo_module::schema::MAX_PORTABLE_PATH_BYTES - 4)
        )
    }));
    assert!(long.len() <= 8 * vo_module::schema::MAX_PORTABLE_PATH_BYTES + 128);
    assert!(long.ends_with("(and 24 more)"));

    let aliases = ReleaseError::InvalidAliasImports(
        (0..20)
            .rev()
            .map(|index| format!("alias-{index:02}"))
            .collect(),
    )
    .to_string();
    assert!(aliases.contains("alias-00, alias-01"), "{aliases}");
    assert!(aliases.ends_with("(and 12 more)"), "{aliases}");

    let forbidden = ReleaseError::ForbiddenVoSum {
        repo_root: std::path::PathBuf::from("/repo"),
        paths: (0..20)
            .rev()
            .map(|index| std::path::PathBuf::from(format!("path-{index:02}/vo.sum")))
            .collect(),
    }
    .to_string();
    assert!(forbidden.contains("path-00/vo.sum"), "{forbidden}");
    assert!(forbidden.ends_with("(and 12 more)"), "{forbidden}");

    let artifacts = ReleaseError::ArtifactContractViolation {
        manifest_path: None,
        missing: (0..20)
            .rev()
            .map(|index| DeclaredArtifactId {
                kind: "extension-native".to_string(),
                target: "test-target".to_string(),
                name: format!("artifact-{index:02}"),
            })
            .collect(),
        undeclared: Vec::new(),
    }
    .to_string();
    assert!(artifacts.contains("artifact-00"), "{artifacts}");
    assert!(artifacts.ends_with("(and 12 more)"), "{artifacts}");
}

#[test]
fn stage_release_succeeds_for_pure_source_module_without_ext_manifest() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());

    let staged = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap();

    let manifest_json = fs::read_to_string(&staged.manifest_path).unwrap();
    let manifest = ReleaseManifest::parse(&manifest_json).unwrap();
    assert!(manifest.artifacts.is_empty());
}

#[test]
fn stage_release_succeeds_when_all_declared_artifacts_are_present() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        bindgen_manifest(
            "demo",
            &["aarch64-apple-darwin", "x86_64-unknown-linux-gnu"],
            "demo.wasm",
            "demo.js",
        ),
    );

    let staged = stage_release(
        temp.path(),
        &stage_options(
            &temp,
            vec![
                write_artifact_input(
                    temp.path(),
                    "extension-native",
                    "aarch64-apple-darwin",
                    "libdemo.dylib",
                    b"mac-native",
                ),
                write_artifact_input(
                    temp.path(),
                    "extension-native",
                    "x86_64-unknown-linux-gnu",
                    "libdemo.so",
                    b"linux-native",
                ),
                write_artifact_input(
                    temp.path(),
                    "extension-wasm",
                    "wasm32-unknown-unknown",
                    "demo.wasm",
                    b"wasm-bits",
                ),
                write_artifact_input(
                    temp.path(),
                    "extension-js-glue",
                    "wasm32-unknown-unknown",
                    "demo.js",
                    b"js-bits",
                ),
            ],
        ),
    )
    .unwrap();

    let manifest_json = fs::read_to_string(&staged.manifest_path).unwrap();
    let manifest = ReleaseManifest::parse(&manifest_json).unwrap();
    assert_eq!(manifest.artifacts.len(), 4);

    let package =
        PackageManifest::parse(&fs::read(staged.out_dir.join("vo.package.json")).unwrap()).unwrap();
    assert!(package.files.iter().any(|file| file.path == "vo.mod"));
}

#[test]
fn stage_release_isolates_same_logical_name_across_artifact_identities() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        native_manifest(
            "demo",
            &["aarch64-unknown-linux-gnu", "x86_64-unknown-linux-gnu"],
        ),
    );
    fs::create_dir_all(temp.path().join("build")).unwrap();
    let arm_path = temp.path().join("build/arm.bin");
    let linux_path = temp.path().join("build/linux.bin");
    fs::write(&arm_path, b"arm").unwrap();
    fs::write(&linux_path, b"linux").unwrap();
    git_add(temp.path(), Path::new("build/arm.bin"));
    git_add(temp.path(), Path::new("build/linux.bin"));

    let staged = stage_release(
        temp.path(),
        &stage_options(
            &temp,
            vec![
                ArtifactInput {
                    kind: "extension-native".to_string(),
                    target: "x86_64-unknown-linux-gnu".to_string(),
                    name: "libdemo.so".to_string(),
                    path: linux_path,
                },
                ArtifactInput {
                    kind: "extension-native".to_string(),
                    target: "aarch64-unknown-linux-gnu".to_string(),
                    name: "libdemo.so".to_string(),
                    path: arm_path,
                },
            ],
        ),
    )
    .unwrap();

    assert_eq!(staged.artifacts.len(), 2);
    assert_ne!(
        staged.artifacts[0].asset_name,
        staged.artifacts[1].asset_name
    );
    assert_ne!(
        staged.artifacts[0].output_path,
        staged.artifacts[1].output_path
    );
    assert_eq!(fs::read(&staged.artifacts[0].output_path).unwrap(), b"arm");
    assert_eq!(
        fs::read(&staged.artifacts[1].output_path).unwrap(),
        b"linux"
    );
    let manifest =
        ReleaseManifest::parse(&fs::read_to_string(&staged.manifest_path).unwrap()).unwrap();
    assert_eq!(
        manifest
            .artifacts
            .iter()
            .filter(|artifact| artifact.id.name == "libdemo.so")
            .count(),
        2
    );
}

#[test]
fn stage_release_rejects_duplicate_full_artifact_identity_before_output() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(temp.path(), standalone_wasm_manifest("demo", "demo.wasm"));
    let artifact = write_artifact_input(
        temp.path(),
        "extension-wasm",
        "wasm32-unknown-unknown",
        "demo.wasm",
        b"wasm",
    );
    let out_dir = temp.path().join(".dist");

    let error = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "0.1.0".to_string(),
            commit: Some(git_head(temp.path())),
            artifacts: vec![artifact.clone(), artifact],
            out_dir: out_dir.clone(),
        },
    )
    .unwrap_err();

    assert!(error.to_string().contains("duplicate artifact identity"));
    assert!(!out_dir.exists());
}

#[test]
fn stage_release_rejects_legacy_browser_artifact_path_fields() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        r#"[extension]
name = "demo"

[extension.wasm]
kind = "bindgen"
wasm = "demo.wasm"
js = "demo.js"
local_wasm = "shared.bin"
"#
        .to_string(),
    );
    fs::write(temp.path().join("shared.bin"), b"shared").unwrap();
    git_add(temp.path(), Path::new("shared.bin"));

    let artifacts = vec![
        write_artifact_input(
            temp.path(),
            "extension-wasm",
            "wasm32-unknown-unknown",
            "demo.wasm",
            b"shared",
        ),
        write_artifact_input(
            temp.path(),
            "extension-js-glue",
            "wasm32-unknown-unknown",
            "demo.js",
            b"shared",
        ),
    ];
    let error = stage_release(temp.path(), &stage_options(&temp, artifacts)).unwrap_err();
    assert!(error.to_string().contains("local_wasm"));
}

#[test]
fn stage_release_rejects_missing_declared_native_artifact() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        native_manifest(
            "demo",
            &["aarch64-apple-darwin", "x86_64-unknown-linux-gnu"],
        ),
    );

    let err = stage_release(
        temp.path(),
        &stage_options(
            &temp,
            vec![write_artifact_input(
                temp.path(),
                "extension-native",
                "aarch64-apple-darwin",
                "libdemo.dylib",
                b"mac-native",
            )],
        ),
    )
    .unwrap_err();

    assert!(matches!(
        err,
        ReleaseError::ArtifactContractViolation {
            ref missing,
            ref undeclared,
            ..
        } if missing == &vec![DeclaredArtifactId {
            kind: "extension-native".to_string(),
            target: "x86_64-unknown-linux-gnu".to_string(),
            name: "libdemo.so".to_string(),
        }] && undeclared.is_empty()
    ));
}

#[test]
fn stage_release_rejects_missing_declared_wasm_artifact() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(temp.path(), standalone_wasm_manifest("demo", "demo.wasm"));

    let err = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap_err();

    assert!(matches!(
        err,
        ReleaseError::ArtifactContractViolation {
            ref missing,
            ref undeclared,
            ..
        } if missing == &vec![DeclaredArtifactId {
            kind: "extension-wasm".to_string(),
            target: "wasm32-unknown-unknown".to_string(),
            name: "demo.wasm".to_string(),
        }] && undeclared.is_empty()
    ));
}

#[test]
fn stage_release_rejects_undeclared_artifact() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(temp.path(), standalone_wasm_manifest("demo", "demo.wasm"));

    let err = stage_release(
        temp.path(),
        &stage_options(
            &temp,
            vec![
                write_artifact_input(
                    temp.path(),
                    "extension-wasm",
                    "wasm32-unknown-unknown",
                    "demo.wasm",
                    b"wasm-bits",
                ),
                write_artifact_input(
                    temp.path(),
                    "extension-native",
                    "aarch64-apple-darwin",
                    "libdemo.dylib",
                    b"mac-native",
                ),
            ],
        ),
    )
    .unwrap_err();

    assert!(matches!(
        err,
        ReleaseError::ArtifactContractViolation {
            ref missing,
            ref undeclared,
            ..
        } if missing.is_empty() && undeclared == &vec![DeclaredArtifactId {
            kind: "extension-native".to_string(),
            target: "aarch64-apple-darwin".to_string(),
            name: "libdemo.dylib".to_string(),
        }]
    ));
}

#[test]
fn stage_release_rejects_artifacts_without_vo_mod_declarations() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());

    let err = stage_release(
        temp.path(),
        &stage_options(
            &temp,
            vec![write_artifact_input(
                temp.path(),
                "extension-wasm",
                "wasm32-unknown-unknown",
                "demo.wasm",
                b"wasm-bits",
            )],
        ),
    )
    .unwrap_err();

    assert!(err.to_string().contains("vo.mod"));
    assert!(matches!(
        &err,
        ReleaseError::ArtifactContractViolation {
            missing,
            undeclared,
            ..
        } if missing.is_empty() && undeclared == &vec![DeclaredArtifactId {
            kind: "extension-wasm".to_string(),
            target: "wasm32-unknown-unknown".to_string(),
            name: "demo.wasm".to_string(),
        }]
    ));
}

#[test]
fn stage_release_rejects_invalid_vo_mod_extension_schema() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        concat!(
            "[extension]\n",
            "name = \"demo\"\n",
            "path = \"rust/target/{profile}/libdemo\"\n",
        )
        .to_string(),
    );

    let err = stage_release(
        temp.path(),
        &stage_options(
            &temp,
            vec![write_artifact_input(
                temp.path(),
                "extension-native",
                "aarch64-apple-darwin",
                "libdemo.dylib",
                b"mac-native",
            )],
        ),
    )
    .unwrap_err();

    assert!(matches!(
        err,
        ReleaseError::Module(ref message)
            if message.contains("unsupported key(s) in [extension]: path")
    ));
}

#[test]
fn stage_release_rejects_bindgen_wasm_missing_js_glue_artifact() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        bindgen_manifest("demo", &["aarch64-apple-darwin"], "demo.wasm", "demo.js"),
    );

    let err = stage_release(
        temp.path(),
        &stage_options(
            &temp,
            vec![
                write_artifact_input(
                    temp.path(),
                    "extension-native",
                    "aarch64-apple-darwin",
                    "libdemo.dylib",
                    b"mac-native",
                ),
                write_artifact_input(
                    temp.path(),
                    "extension-wasm",
                    "wasm32-unknown-unknown",
                    "demo.wasm",
                    b"wasm-bits",
                ),
            ],
        ),
    )
    .unwrap_err();

    assert!(matches!(
        err,
        ReleaseError::ArtifactContractViolation {
            ref missing,
            ref undeclared,
            ..
        } if missing == &vec![DeclaredArtifactId {
            kind: "extension-js-glue".to_string(),
            target: "wasm32-unknown-unknown".to_string(),
            name: "demo.js".to_string(),
        }] && undeclared.is_empty()
    ));
}

#[test]
fn stage_release_canonicalizes_manifest_order_from_unsorted_inputs() {
    let temp = TempDir::new().unwrap();
    fs::write(
        temp.path().join("vo.mod"),
        concat!(
            "module = \"github.com/acme/app\"\n",
            "vo = \"^0.1.0\"\n\n",
            "[dependencies]\n",
            "\"github.com/vo-lang/vopack\" = \"0.1.0\"\n",
            "\"github.com/vo-lang/vogui\" = \"0.1.2\"\n",
        ),
    )
    .unwrap();
    fs::write(
        temp.path().join("vo.lock"),
        r#"version = 3

[root]
module = "github.com/acme/app"
vo = "^0.1.0"

[[module]]
path = "github.com/vo-lang/vogui"
version = "0.1.2"
vo = "^0.1.0"
release = "sha256:3333333333333333333333333333333333333333333333333333333333333333"
dependencies = []

[[module]]
path = "github.com/vo-lang/vopack"
version = "0.1.0"
vo = "^0.1.0"
release = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
dependencies = []
"#,
    )
    .unwrap();
    fs::write(temp.path().join("main.vo"), "fn main() {}\n").unwrap();
    let metadata = bindgen_wasm_manifest("demo", "z-demo.wasm", "a-demo.js");
    let mut mod_content = fs::read_to_string(temp.path().join("vo.mod")).unwrap();
    mod_content.push('\n');
    mod_content.push_str(&metadata);
    fs::write(temp.path().join("vo.mod"), mod_content).unwrap();

    let wasm_artifact_path = temp.path().join("z-demo.wasm");
    let js_artifact_path = temp.path().join("a-demo.js");
    fs::write(&wasm_artifact_path, b"wasm-bits").unwrap();
    fs::write(&js_artifact_path, b"js-bits").unwrap();
    init_test_git_repo(temp.path());
    git_add(temp.path(), Path::new("vo.mod"));
    git_add(temp.path(), Path::new("vo.lock"));
    git_add(temp.path(), Path::new("main.vo"));
    git_add(temp.path(), Path::new("z-demo.wasm"));
    git_add(temp.path(), Path::new("a-demo.js"));

    let staged = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "0.1.0".to_string(),
            commit: Some(git_head(temp.path())),
            artifacts: vec![
                ArtifactInput {
                    kind: "extension-wasm".to_string(),
                    target: "wasm32-unknown-unknown".to_string(),
                    name: "z-demo.wasm".to_string(),
                    path: wasm_artifact_path.clone(),
                },
                ArtifactInput {
                    kind: "extension-js-glue".to_string(),
                    target: "wasm32-unknown-unknown".to_string(),
                    name: "a-demo.js".to_string(),
                    path: js_artifact_path.clone(),
                },
            ],
            out_dir: temp.path().join(".dist"),
        },
    )
    .unwrap();

    let manifest_json = fs::read_to_string(&staged.manifest_path).unwrap();
    let vogui_pos = manifest_json
        .find("\"module\": \"github.com/vo-lang/vogui\"")
        .unwrap();
    let vopack_pos = manifest_json
        .find("\"module\": \"github.com/vo-lang/vopack\"")
        .unwrap();
    let js_pos = manifest_json.find("\"name\": \"a-demo.js\"").unwrap();
    let wasm_pos = manifest_json.find("\"name\": \"z-demo.wasm\"").unwrap();

    assert!(vogui_pos < vopack_pos);
    assert!(js_pos < wasm_pos);
    let package = PackageManifest::parse(&fs::read(&staged.package_path).unwrap()).unwrap();
    assert!(package.files.iter().all(|file| file.path != "vo.lock"));
}

#[test]
fn stage_release_includes_all_tracked_files_from_dist_dirs() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(temp.path(), standalone_wasm_manifest("demo", "demo.wasm"));
    fs::create_dir_all(temp.path().join("js/dist")).unwrap();
    fs::write(
        temp.path().join("js/dist/studio_renderer.js"),
        "export const renderer = 1;\n",
    )
    .unwrap();
    fs::write(
        temp.path().join("js/dist/studio_host_bridge.js"),
        "export const hostBridge = 1;\n",
    )
    .unwrap();
    git_add(temp.path(), Path::new("js/dist/studio_renderer.js"));
    git_add(temp.path(), Path::new("js/dist/studio_host_bridge.js"));

    let staged = stage_release(
        temp.path(),
        &stage_options(
            &temp,
            vec![write_artifact_input(
                temp.path(),
                "extension-wasm",
                "wasm32-unknown-unknown",
                "demo.wasm",
                b"wasm-bits",
            )],
        ),
    )
    .unwrap();

    let entries = source_archive_entries(&staged.source_path);
    assert!(entries
        .iter()
        .any(|entry| entry.ends_with("/vo.package.json")));
    assert!(entries
        .iter()
        .any(|entry| entry.ends_with("/js/dist/studio_renderer.js")));
    assert!(entries
        .iter()
        .any(|entry| entry.ends_with("/js/dist/studio_host_bridge.js")));
}

#[test]
fn stage_release_includes_tracked_directories_recursively() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(temp.path(), standalone_wasm_manifest("demo", "demo.wasm"));
    fs::create_dir_all(temp.path().join("js/dist/nested")).unwrap();
    fs::write(
        temp.path().join("js/dist/voplay-render-island.js"),
        "export { bootstrapWebView } from './bootstrap_webview.js';\n",
    )
    .unwrap();
    fs::write(
        temp.path().join("js/dist/bootstrap_webview.js"),
        "export const bootstrapWebView = 1;\n",
    )
    .unwrap();
    fs::write(
        temp.path().join("js/dist/nested/helper.js"),
        "export const helper = 1;\n",
    )
    .unwrap();
    git_add(temp.path(), Path::new("js/dist/voplay-render-island.js"));
    git_add(temp.path(), Path::new("js/dist/bootstrap_webview.js"));
    git_add(temp.path(), Path::new("js/dist/nested/helper.js"));

    let staged = stage_release(
        temp.path(),
        &stage_options(
            &temp,
            vec![write_artifact_input(
                temp.path(),
                "extension-wasm",
                "wasm32-unknown-unknown",
                "demo.wasm",
                b"wasm-bits",
            )],
        ),
    )
    .unwrap();

    let entries = source_archive_entries(&staged.source_path);
    assert!(entries
        .iter()
        .any(|entry| entry.ends_with("/js/dist/voplay-render-island.js")));
    assert!(entries
        .iter()
        .any(|entry| entry.ends_with("/js/dist/bootstrap_webview.js")));
    assert!(entries
        .iter()
        .any(|entry| entry.ends_with("/js/dist/nested/helper.js")));
}

#[test]
fn tracked_source_directory_is_loaded_from_the_commit_tree() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(temp.path(), "[web]\nentry = \"main.vo\"\n".to_string());
    fs::create_dir_all(temp.path().join("js/dist/nested")).unwrap();
    fs::write(
        temp.path().join("js/dist/generated.js"),
        "export const generated = 1;\n",
    )
    .unwrap();
    fs::write(
        temp.path().join("js/dist/nested/helper.js"),
        "export const helper = 1;\n",
    )
    .unwrap();
    git_add(temp.path(), Path::new("js/dist/generated.js"));
    git_add(temp.path(), Path::new("js/dist/nested/helper.js"));

    let root = fs::canonicalize(temp.path()).unwrap();
    let commit = git_head(&root);
    let tree = crate::repo::load_commit_tree(&root, &commit).unwrap();
    let mod_file = crate::repo::read_committed_mod_file(&root, &tree).unwrap();
    fs::remove_dir_all(root.join("js/dist")).unwrap();

    let selected = crate::repo::collect_release_source_files(&root, &mod_file, &tree).unwrap();
    let snapshot = crate::repo::capture_release_source_snapshot(&root, &selected, &tree).unwrap();
    let archive = crate::repo::build_source_package("snapshot-test", &snapshot, b"{}\n").unwrap();
    let archive_path = root.join("snapshot.tar.gz");
    fs::write(&archive_path, archive).unwrap();

    assert_eq!(
        source_archive_file(&archive_path, "/js/dist/generated.js"),
        b"export const generated = 1;\n"
    );
    assert_eq!(
        source_archive_file(&archive_path, "/js/dist/nested/helper.js"),
        b"export const helper = 1;\n"
    );
}

#[test]
fn parent_release_excludes_complete_nested_module_subtrees() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::create_dir_all(temp.path().join("modules/child/assets")).unwrap();
    fs::write(
        temp.path().join("modules/child/vo.mod"),
        "module = \"github.com/acme/app/modules/child\"\nvo = \"^0.1.0\"\n",
    )
    .unwrap();
    fs::write(
        temp.path().join("modules/child/main.vo"),
        "import alias @\"github.com/acme/dep\"\n\npackage child\n",
    )
    .unwrap();
    fs::write(
        temp.path().join("modules/child/assets/data.bin"),
        b"child-only",
    )
    .unwrap();
    fs::write(temp.path().join("modules/child/vo.sum"), "child-only\n").unwrap();
    git_add(temp.path(), Path::new("modules/child/vo.mod"));
    git_add(temp.path(), Path::new("modules/child/main.vo"));
    git_add(temp.path(), Path::new("modules/child/assets/data.bin"));
    git_add(temp.path(), Path::new("modules/child/vo.sum"));

    let staged = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap();
    let entries = source_archive_entries(&staged.source_path);
    assert!(entries.iter().any(|entry| entry.ends_with("/main.vo")));
    assert!(entries.iter().any(|entry| entry.ends_with("/vo.mod")));
    assert!(
        entries
            .iter()
            .all(|entry| !entry.contains("/modules/child/")),
        "{entries:?}",
    );
    let package =
        PackageManifest::parse(&fs::read(staged.out_dir.join("vo.package.json")).unwrap()).unwrap();
    assert!(package
        .files
        .iter()
        .all(|entry| !entry.path.starts_with("modules/child/")),);
}

#[test]
fn nested_module_larger_than_parent_closure_limit_is_pruned_before_accounting() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let shared_path = temp.path().join("shared-blob");
    fs::write(&shared_path, b"nested fixture\n").unwrap();
    let shared_blob = git_stdout(
        temp.path(),
        &["hash-object", "-w", shared_path.to_str().unwrap()],
    );
    let nested_mod_blob = git_stdout(temp.path(), &["rev-parse", "HEAD:vo.mod"]);
    let root_mod_blob = nested_mod_blob.clone();
    let main_blob = git_stdout(temp.path(), &["rev-parse", "HEAD:main.vo"]);

    let excluded_count = vo_common::vfs::MAX_DIRECTORY_ENTRIES + 1;
    let mut child_tree_input = String::new();
    child_tree_input
        .try_reserve(excluded_count.saturating_mul(72))
        .unwrap();
    for index in 0..excluded_count {
        child_tree_input.push_str(&format!("100644 blob {shared_blob}\tignored-{index:06}\n"));
    }
    child_tree_input.push_str(&format!("100644 blob {nested_mod_blob}\tvo.mod\n"));
    let child_tree = git_stdin_stdout(temp.path(), &["mktree"], child_tree_input.as_bytes());
    let modules_tree = git_stdin_stdout(
        temp.path(),
        &["mktree"],
        format!("040000 tree {child_tree}\tchild\n").as_bytes(),
    );
    let root_tree = git_stdin_stdout(
        temp.path(),
        &["mktree"],
        format!(
            "100644 blob {main_blob}\tmain.vo\n040000 tree {modules_tree}\tmodules\n100644 blob {root_mod_blob}\tvo.mod\n"
        )
        .as_bytes(),
    );
    let commit = git_stdout(
        temp.path(),
        &["commit-tree", &root_tree, "-m", "large nested"],
    );
    let root = fs::canonicalize(temp.path()).unwrap();

    let tree = crate::repo::load_commit_tree(&root, &commit).unwrap();
    let mod_file = crate::repo::read_committed_mod_file(&root, &tree).unwrap();
    let selected = crate::repo::collect_release_source_files(&root, &mod_file, &tree).unwrap();
    assert_eq!(selected.len(), 2);
}

#[cfg(unix)]
#[test]
fn nested_boundary_is_detected_before_validating_earlier_tree_entries() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::create_dir_all(temp.path().join("modules/child")).unwrap();
    fs::write(
        temp.path().join("modules/child/vo.mod"),
        "module = \"github.com/acme/app/modules/child\"\nvo = \"^0.1.0\"\n",
    )
    .unwrap();
    std::os::unix::fs::symlink(
        "outside-the-parent-closure",
        temp.path().join("modules/child/a-link"),
    )
    .unwrap();
    git_add(temp.path(), Path::new("modules/child/vo.mod"));
    git_add(temp.path(), Path::new("modules/child/a-link"));

    let staged = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap();
    let entries = source_archive_entries(&staged.source_path);
    assert!(entries
        .iter()
        .all(|entry| !entry.contains("/modules/child/")));
}

#[test]
fn web_entry_cannot_cross_a_nested_module_boundary() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        "[web]\nentry = \"modules/child/main.vo\"\n".to_string(),
    );
    fs::create_dir_all(temp.path().join("modules/child")).unwrap();
    fs::write(
        temp.path().join("modules/child/vo.mod"),
        "module = \"github.com/acme/app/modules/child\"\nvo = \"^0.1.0\"\n",
    )
    .unwrap();
    fs::write(temp.path().join("modules/child/main.vo"), "package child\n").unwrap();
    git_add(temp.path(), Path::new("modules/child/vo.mod"));
    git_add(temp.path(), Path::new("modules/child/main.vo"));
    let options = stage_options(&temp, Vec::new());

    let error = stage_release(temp.path(), &options).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("outside the tracked module source closure"),
        "{error}",
    );
    assert!(!options.out_dir.exists());
}

#[test]
fn release_rejects_nested_module_manifest_portable_aliases() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::create_dir_all(temp.path().join("modules/child")).unwrap();
    fs::write(
        temp.path().join("modules/child/VO.MOD"),
        "module = \"github.com/acme/app/modules/child\"\nvo = \"^0.1.0\"\n",
    )
    .unwrap();
    git_add(temp.path(), Path::new("modules/child/VO.MOD"));
    let options = stage_options(&temp, Vec::new());

    let error = stage_release(temp.path(), &options).unwrap_err();
    assert!(
        error.to_string().contains("portable alias of vo.mod"),
        "{error}",
    );
    assert!(!options.out_dir.exists());
}

#[test]
fn nested_protocol_looking_names_are_ordinary_tracked_source_data() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::create_dir_all(temp.path().join("fixtures")).unwrap();
    fs::write(temp.path().join("fixtures/vo.sum"), "fixture-data\n").unwrap();
    fs::write(temp.path().join("fixtures/vo.lock"), "fixture-lock\n").unwrap();
    fs::write(temp.path().join("fixtures/vo.web.json"), "{}\n").unwrap();
    fs::write(
        temp.path().join("fixtures/source.tar.gz"),
        "fixture archive\n",
    )
    .unwrap();
    fs::write(
        temp.path().join("fixtures/.vo-project.lock"),
        "fixture state\n",
    )
    .unwrap();
    git_add(temp.path(), Path::new("fixtures/vo.sum"));
    git_add(temp.path(), Path::new("fixtures/vo.lock"));
    git_add(temp.path(), Path::new("fixtures/vo.web.json"));
    git_add(temp.path(), Path::new("fixtures/source.tar.gz"));
    git_add(temp.path(), Path::new("fixtures/.vo-project.lock"));

    let staged = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap();
    let package = PackageManifest::parse(&fs::read(&staged.package_path).unwrap()).unwrap();
    for path in [
        "fixtures/.vo-project.lock",
        "fixtures/source.tar.gz",
        "fixtures/vo.lock",
        "fixtures/vo.sum",
        "fixtures/vo.web.json",
    ] {
        assert!(
            package.files.iter().any(|entry| entry.path == path),
            "{path}"
        );
    }
}

#[test]
fn tracked_generated_protocol_file_is_rejected() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(temp.path().join("vo.package.json"), "{}\n").unwrap();
    git_add(temp.path(), Path::new("vo.package.json"));
    let options = stage_options(&temp, Vec::new());

    let error = stage_release(temp.path(), &options).unwrap_err();

    assert!(error.to_string().contains("reserved"), "{error}");
    assert!(!options.out_dir.exists());
}

#[test]
fn stage_release_rejects_removed_browser_manifest_files() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(temp.path().join("VO.WEB.JSON"), "{}\n").unwrap();
    git_add(temp.path(), Path::new("VO.WEB.JSON"));
    let options = stage_options(&temp, Vec::new());

    let error = stage_release(temp.path(), &options).unwrap_err();

    assert!(
        error.to_string().contains("root project protocol state"),
        "{error}"
    );
    assert!(!options.out_dir.exists());
}

#[test]
fn tracked_full_fold_package_manifest_alias_is_rejected() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(temp.path().join("vo.package.jſon"), "{}\n").unwrap();
    git_add(temp.path(), Path::new("vo.package.jſon"));
    let options = stage_options(&temp, Vec::new());

    let error = stage_release(temp.path(), &options).unwrap_err();

    assert!(
        error.to_string().contains("vo.package.jſon")
            || error.to_string().contains("portable spelling"),
        "{error}"
    );
    assert!(!options.out_dir.exists());
}

#[test]
fn tracked_source_archive_name_and_case_alias_are_reserved_at_root() {
    for name in ["source.tar.gz", "SOURCE.TAR.GZ", "ſource.tar.gz"] {
        let temp = TempDir::new().unwrap();
        write_basic_repo(temp.path());
        fs::write(temp.path().join(name), "stale archive\n").unwrap();
        git_add(temp.path(), Path::new(name));
        let options = stage_options(&temp, Vec::new());

        let error = stage_release(temp.path(), &options).unwrap_err();
        assert!(error.to_string().contains("reserved"), "{name}: {error}");
        assert!(!options.out_dir.exists());
    }
}

#[test]
fn tracked_vo_source_cannot_bypass_alias_validation() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::create_dir_all(temp.path().join("dist")).unwrap();
    fs::write(
        temp.path().join("dist/bad.vo"),
        "import (\n  alias @\"gin\"\n)\n\npackage bad\n",
    )
    .unwrap();
    git_add(temp.path(), Path::new("dist/bad.vo"));
    let options = stage_options(&temp, Vec::new());

    let error = stage_release(temp.path(), &options).unwrap_err();

    assert!(matches!(
        error,
        ReleaseError::InvalidAliasImports(ref violations)
            if violations == &vec!["dist/bad.vo:2: alias @\"gin\"".to_string()]
    ));
    assert!(!options.out_dir.exists());
}

#[test]
fn stage_release_fails_when_web_entry_is_missing() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        "[web]\nentry = \"cmd/web/missing.vo\"\n".to_string(),
    );

    let err = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap_err();

    assert!(
        matches!(
            &err,
            ReleaseError::IoError(_, message)
                if message.contains("[web].entry")
                    && message.contains("outside the tracked module source closure")
        ),
        "{err}",
    );
}

#[test]
fn stage_release_fails_when_extension_web_js_file_is_untracked() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        "[extension]\nname = \"demo\"\n\n[extension.web]\ncapabilities = []\n\n[extension.web.js]\nrenderer = \"js/dist/studio_renderer.js\"\n"
            .to_string(),
    );
    fs::create_dir_all(temp.path().join("js/dist")).unwrap();
    fs::write(
        temp.path().join("js/dist/studio_renderer.js"),
        "export const renderer = 1;\n",
    )
    .unwrap();

    let err = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap_err();

    assert!(matches!(
        err,
        ReleaseError::IoError(_, ref message)
            if message.contains("[extension.web.js].renderer")
                && message.contains("outside the tracked module source closure")
    ));
}

#[test]
fn stage_release_rejects_nonempty_output_directory_without_mutating_it() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let out_dir = temp.path().join(".dist");
    fs::create_dir_all(&out_dir).unwrap();
    let sentinel = out_dir.join("preexisting.txt");
    fs::write(&sentinel, "preserve me\n").unwrap();

    let error = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "0.1.0".to_string(),
            commit: Some(git_head(temp.path())),
            artifacts: Vec::new(),
            out_dir: out_dir.clone(),
        },
    )
    .unwrap_err();

    assert!(
        error.to_string().contains("must not already exist"),
        "{error}"
    );
    assert_eq!(fs::read_to_string(&sentinel).unwrap(), "preserve me\n");
    assert_eq!(fs::read_dir(&out_dir).unwrap().count(), 1);
}

#[test]
fn stage_release_rejects_an_existing_empty_output_directory_without_mutating_it() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let out_dir = temp.path().join(".dist");
    fs::create_dir(&out_dir).unwrap();

    let error = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "0.1.0".to_string(),
            commit: Some(git_head(temp.path())),
            artifacts: Vec::new(),
            out_dir: out_dir.clone(),
        },
    )
    .unwrap_err();

    assert!(
        error.to_string().contains("must not already exist"),
        "{error}"
    );
    assert!(out_dir.is_dir());
    assert_eq!(fs::read_dir(out_dir).unwrap().count(), 0);
}

#[test]
fn atomic_release_publish_never_replaces_an_existing_destination() {
    let temp = TempDir::new().unwrap();
    let destination = temp.path().join("published");
    let mut pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
    let staged = pending.path();
    pending.write_new_file("new.txt", b"new\n").unwrap();
    pending.seal().unwrap();
    fs::create_dir(&destination).unwrap();
    fs::write(destination.join("existing.txt"), "existing\n").unwrap();

    let error = pending.publish().unwrap_err();

    assert!(
        matches!(error, ReleaseError::IoError(ref path, _) if path == &destination),
        "unexpected no-replace error: {error}",
    );
    assert_eq!(
        fs::read_to_string(destination.join("existing.txt")).unwrap(),
        "existing\n",
    );
    assert!(!staged.exists());
}

#[cfg(unix)]
#[test]
fn release_stage_names_are_high_entropy_and_reserved() {
    let names = (0..16)
        .map(|_| crate::publish::new_stage_dir_name().unwrap())
        .collect::<std::collections::BTreeSet<_>>();

    assert_eq!(names.len(), 16);
    for name in names {
        assert!(name.starts_with(crate::publish::RELEASE_STAGE_DIR_PREFIX));
        let random = name.rsplit('-').next().unwrap();
        assert_eq!(random.len(), 32);
        assert!(random.bytes().all(|byte| byte.is_ascii_hexdigit()));
    }
}

#[cfg(unix)]
#[test]
fn release_stage_directory_is_private_and_cleanup_is_constrained() {
    use std::os::unix::fs::PermissionsExt;

    let temp = TempDir::new().unwrap();
    let destination = temp.path().join("published");
    let mut pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
    let staged = pending.path();
    assert_eq!(
        fs::metadata(&staged).unwrap().permissions().mode() & 0o777,
        0o700
    );

    pending.write_new_file("owned.txt", b"owned\n").unwrap();
    fs::write(staged.join("foreign.txt"), "preserve\n").unwrap();
    drop(pending);

    assert!(!staged.join("owned.txt").exists());
    assert_eq!(
        fs::read_to_string(staged.join("foreign.txt")).unwrap(),
        "preserve\n"
    );
}

#[cfg(unix)]
#[test]
fn release_cleanup_preserves_a_rebound_stage_name() {
    let temp = TempDir::new().unwrap();
    let destination = temp.path().join("published");
    let mut pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
    let staged = pending.path();
    let moved = temp.path().join("moved-owned-stage");
    pending.write_new_file("owned.txt", b"owned\n").unwrap();

    fs::rename(&staged, &moved).unwrap();
    fs::create_dir(&staged).unwrap();
    drop(pending);

    assert!(staged.is_dir());
    assert_eq!(fs::read_dir(&staged).unwrap().count(), 0);
    assert!(moved.is_dir());
    assert!(!moved.join("owned.txt").exists());
}

#[cfg(unix)]
#[test]
fn release_stage_assets_are_read_only_and_seal_detects_content_drift() {
    use std::os::unix::fs::PermissionsExt;

    let temp = TempDir::new().unwrap();
    let destination = temp.path().join("published");
    let mut pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
    let asset = pending.path().join("asset.txt");
    pending.write_new_file("asset.txt", b"original\n").unwrap();
    assert_eq!(
        fs::metadata(&asset).unwrap().permissions().mode() & 0o222,
        0
    );

    fs::set_permissions(&asset, fs::Permissions::from_mode(0o600)).unwrap();
    fs::write(&asset, "tampered\n").unwrap();
    fs::set_permissions(&asset, fs::Permissions::from_mode(0o400)).unwrap();

    let error = pending.seal().unwrap_err();
    assert!(error.to_string().contains("digest changed"), "{error}");
    assert!(!destination.exists());
}

#[cfg(unix)]
#[test]
fn release_artifact_copy_rejects_a_symbolic_link_source() {
    let temp = TempDir::new().unwrap();
    let destination = temp.path().join("published");
    let artifact = temp.path().join("artifact.wasm");
    let linked_artifact = temp.path().join("linked-artifact.wasm");
    fs::write(&artifact, b"wasm-bits").unwrap();
    std::os::unix::fs::symlink(&artifact, &linked_artifact).unwrap();

    let mut pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
    let error = pending
        .copy_new_file(
            "extension-wasm--wasm32-unknown-unknown--artifact.wasm",
            &linked_artifact,
            vo_module::MAX_MODULE_ARTIFACT_BYTES,
        )
        .unwrap_err();

    assert!(
        error
            .to_string()
            .contains("regular file, not a symbolic link"),
        "{error}"
    );
    assert!(!destination.exists());
}

#[cfg(unix)]
#[test]
fn release_seal_rejects_read_only_mode_drift() {
    use std::os::unix::fs::PermissionsExt;

    let temp = TempDir::new().unwrap();
    let destination = temp.path().join("published");
    let mut pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
    let asset = pending.path().join("asset.txt");
    pending.write_new_file("asset.txt", b"original\n").unwrap();

    fs::set_permissions(&asset, fs::Permissions::from_mode(0o444)).unwrap();

    let error = pending.seal().unwrap_err();
    assert!(error.to_string().contains("mode changed"), "{error}");
    assert!(!destination.exists());
}

#[cfg(unix)]
#[test]
fn release_seal_rejects_an_external_hard_link_to_an_asset() {
    let temp = TempDir::new().unwrap();
    let destination = temp.path().join("published");
    let external_link = temp.path().join("external-link");
    let mut pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
    let asset = pending.path().join("asset.txt");
    pending.write_new_file("asset.txt", b"original\n").unwrap();

    fs::hard_link(&asset, &external_link).unwrap();

    let error = pending.seal().unwrap_err();
    assert!(error.to_string().contains("link count changed"), "{error}");
    assert!(!destination.exists());
    drop(pending);
    assert_eq!(fs::read_to_string(external_link).unwrap(), "original\n");
}

#[cfg(unix)]
#[test]
fn release_stage_entry_enumeration_enforces_the_output_limit() {
    let temp = TempDir::new().unwrap();
    let destination = temp.path().join("published");
    let pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
    let max_entries = vo_module::MAX_MODULE_ARTIFACTS.checked_add(3).unwrap();

    for index in 0..=max_entries {
        fs::File::create(pending.path().join(format!("foreign-{index:05}"))).unwrap();
    }

    let error = pending.entry_names().unwrap_err();
    assert!(
        error
            .to_string()
            .contains(&format!("more than the {max_entries}-entry limit")),
        "{error}"
    );
    assert!(!destination.exists());
}

#[cfg(unix)]
#[test]
fn release_publish_detects_same_name_asset_replacement_after_seal() {
    use std::os::unix::fs::PermissionsExt;

    let temp = TempDir::new().unwrap();
    let destination = temp.path().join("published");
    let mut pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
    let asset = pending.path().join("asset.txt");
    pending.write_new_file("asset.txt", b"original\n").unwrap();
    pending.seal().unwrap();

    fs::remove_file(&asset).unwrap();
    fs::write(&asset, "original\n").unwrap();
    fs::set_permissions(&asset, fs::Permissions::from_mode(0o400)).unwrap();

    let error = pending.publish().unwrap_err();
    assert!(error.to_string().contains("identity changed"), "{error}");
    assert!(!destination.exists());
    assert_eq!(fs::read_to_string(&asset).unwrap(), "original\n");
    assert!(asset.parent().unwrap().is_dir());
}

#[cfg(unix)]
#[test]
fn release_artifact_copy_rejects_oversize_before_creating_output() {
    let temp = TempDir::new().unwrap();
    let source = temp.path().join("oversize.bin");
    let source_file = fs::File::create(&source).unwrap();
    source_file.set_len(5).unwrap();
    let destination = temp.path().join("published");
    let mut pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
    let staged = pending.path();

    let error = pending
        .copy_new_file("artifact.bin", &source, 4)
        .unwrap_err();

    assert!(error.to_string().contains("4-byte limit"), "{error}");
    assert_eq!(fs::read_dir(&staged).unwrap().count(), 0);
    drop(pending);
    assert!(!staged.exists());
}

#[cfg(unix)]
#[test]
fn release_artifact_copy_rejects_links_in_the_complete_input_path() {
    use std::os::unix::fs::symlink;

    let temp = TempDir::new().unwrap();
    let real_parent = temp.path().join("real");
    fs::create_dir(&real_parent).unwrap();
    let source = real_parent.join("artifact.bin");
    fs::write(&source, b"artifact").unwrap();
    let linked_file = temp.path().join("linked-file.bin");
    let linked_parent = temp.path().join("linked-parent");
    symlink(&source, &linked_file).unwrap();
    symlink(&real_parent, &linked_parent).unwrap();

    for linked_source in [linked_file, linked_parent.join("artifact.bin")] {
        let destination = temp
            .path()
            .join(format!("published-{}", destination_suffix(&linked_source)));
        let mut pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
        let staged = pending.path();

        let error = pending
            .copy_new_file("artifact.bin", &linked_source, 64)
            .unwrap_err();

        assert!(
            matches!(error, ReleaseError::IoError(ref path, _) if path == &linked_source),
            "unexpected link rejection: {error}"
        );
        assert_eq!(fs::read_dir(&staged).unwrap().count(), 0);
        drop(pending);
        assert!(!staged.exists());
        assert!(!destination.exists());
    }
}

#[cfg(unix)]
#[test]
fn release_root_alias_resolution_rejects_chained_and_escaping_targets() {
    use std::ffi::OsStr;
    use std::os::unix::fs::symlink;

    let temp = TempDir::new().unwrap();
    let anchor = temp.path().join("anchor");
    let outside = temp.path().join("outside");
    fs::create_dir(&anchor).unwrap();
    fs::create_dir(&outside).unwrap();
    fs::create_dir(anchor.join("real")).unwrap();
    symlink("real", anchor.join("direct")).unwrap();
    symlink("direct", anchor.join("chained")).unwrap();
    symlink("../outside", anchor.join("escaping")).unwrap();

    crate::publish::test_open_alias_target_no_follow(&anchor, OsStr::new("direct")).unwrap();
    assert!(
        crate::publish::test_open_alias_target_no_follow(&anchor, OsStr::new("chained")).is_err()
    );
    let escaping =
        crate::publish::test_open_alias_target_no_follow(&anchor, OsStr::new("escaping"))
            .unwrap_err();
    assert!(escaping.to_string().contains("escapes the filesystem root"));
}

#[cfg(unix)]
#[test]
fn release_artifact_copy_rejects_in_place_source_mutation() {
    let temp = TempDir::new().unwrap();
    let source = temp.path().join("artifact.bin");
    fs::write(&source, b"original").unwrap();
    let destination = temp.path().join("published");
    let mut pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
    let staged = pending.path();
    let source_for_hook = source.clone();
    crate::publish::set_after_first_source_chunk_hook(move || {
        fs::write(source_for_hook, b"tampered").unwrap();
    });

    let error = pending
        .copy_new_file("artifact.bin", &source, 64)
        .unwrap_err();

    assert!(
        error.to_string().contains("changed while being staged"),
        "{error}"
    );
    drop(pending);
    assert!(!staged.exists());
    assert!(!destination.exists());
}

#[cfg(unix)]
fn destination_suffix(path: &Path) -> &str {
    path.file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("artifact")
}

#[cfg(unix)]
#[test]
fn release_seal_rejects_a_rebound_parent_path() {
    let temp = TempDir::new().unwrap();
    let parent = temp.path().join("anchor");
    let moved_parent = temp.path().join("moved-anchor");
    fs::create_dir(&parent).unwrap();
    let destination = parent.join("published");
    let mut pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
    let stage_name = pending.path().file_name().unwrap().to_os_string();
    pending.write_new_file("asset.txt", b"original\n").unwrap();

    fs::rename(&parent, &moved_parent).unwrap();
    fs::create_dir(&parent).unwrap();
    let error = pending.seal().unwrap_err();
    assert!(error.to_string().contains("rebound"), "{error}");
    drop(pending);

    assert!(!destination.exists());
    assert!(!moved_parent.join(stage_name).exists());
}

#[cfg(unix)]
#[test]
fn parent_rebind_after_rename_reports_published_path_uncertainty() {
    let temp = TempDir::new().unwrap();
    let parent = temp.path().join("anchor");
    let moved_parent = temp.path().join("moved-anchor");
    fs::create_dir(&parent).unwrap();
    let destination = parent.join("published");
    let mut pending = crate::publish::PendingOutputDir::create(&destination).unwrap();
    pending.write_new_file("asset.txt", b"original\n").unwrap();
    pending.seal().unwrap();

    let hook_parent = parent.clone();
    let hook_moved_parent = moved_parent.clone();
    crate::publish::set_after_rename_hook(move || {
        fs::rename(hook_parent, &hook_moved_parent).unwrap();
        fs::create_dir(&parent).unwrap();
    });
    let error = pending.publish().unwrap_err();

    assert!(matches!(
        error,
        ReleaseError::PublishedButDurabilityUnconfirmed { ref path, ref message }
            if path == &destination && message.contains("rebound")
    ));
    assert!(!destination.exists());
    assert!(moved_parent.join("published").is_dir());
}

#[test]
fn stage_release_rejects_the_internal_stage_name_prefix() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    for prefix in [
        crate::publish::RELEASE_STAGE_DIR_PREFIX.to_string(),
        crate::publish::RELEASE_STAGE_DIR_PREFIX.to_ascii_uppercase(),
        crate::publish::RELEASE_STAGE_DIR_PREFIX.replace("release", "releaſe"),
    ] {
        let out_dir = temp.path().join(format!("{prefix}user-selected"));
        let error = stage_release(
            temp.path(),
            &StageReleaseOptions {
                version: "0.1.0".to_string(),
                commit: Some(git_head(temp.path())),
                artifacts: Vec::new(),
                out_dir: out_dir.clone(),
            },
        )
        .unwrap_err();

        assert!(error.to_string().contains("internal"), "{error}");
        assert!(!out_dir.exists());
    }
}

#[cfg(unix)]
#[test]
fn parent_sync_failure_reports_published_but_unconfirmed_output() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let options = stage_options(&temp, Vec::new());
    crate::publish::fail_next_parent_sync_after_publish();

    let error = stage_release(temp.path(), &options).unwrap_err();

    assert!(matches!(
        error,
        ReleaseError::PublishedButDurabilityUnconfirmed { ref path, .. }
            if path == &fs::canonicalize(temp.path()).unwrap().join(".dist")
    ));
    assert!(options.out_dir.is_dir());
    assert_eq!(fs::read_dir(&options.out_dir).unwrap().count(), 3);
    assert!(fs::read_dir(temp.path()).unwrap().all(|entry| {
        !entry
            .unwrap()
            .file_name()
            .to_string_lossy()
            .starts_with(crate::publish::RELEASE_STAGE_DIR_PREFIX)
    }));
}

#[test]
fn source_archive_uses_portable_utf8_byte_order() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let bmp = "\u{e000}.vo";
    let supplementary = "\u{10000}.vo";
    fs::write(temp.path().join(bmp), "package order\n").unwrap();
    fs::write(temp.path().join(supplementary), "package order\n").unwrap();
    git_add(temp.path(), Path::new(bmp));
    git_add(temp.path(), Path::new(supplementary));

    let staged = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap();
    let entries = source_archive_entries_in_order(&staged.source_path);
    let bmp_index = entries
        .iter()
        .position(|entry| entry.ends_with(&format!("/{bmp}")))
        .unwrap();
    let supplementary_index = entries
        .iter()
        .position(|entry| entry.ends_with(&format!("/{supplementary}")))
        .unwrap();

    assert!(
        bmp_index < supplementary_index,
        "archive entries must use canonical UTF-8 byte order: {entries:?}"
    );
}

#[cfg(unix)]
#[test]
fn stage_release_rejects_a_symbolic_link_output_directory() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let target = temp.path().join("real-output");
    fs::create_dir(&target).unwrap();
    let out_dir = temp.path().join(".dist");
    std::os::unix::fs::symlink(&target, &out_dir).unwrap();

    let error = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "0.1.0".to_string(),
            commit: Some(git_head(temp.path())),
            artifacts: Vec::new(),
            out_dir,
        },
    )
    .unwrap_err();

    assert!(error.to_string().contains("symbolic link"), "{error}");
    assert_eq!(fs::read_dir(target).unwrap().count(), 0);
}

#[test]
fn stage_release_excludes_untracked_files_from_source_package() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(temp.path().join("tracked.txt"), "tracked\n").unwrap();
    fs::write(temp.path().join("untracked.txt"), "untracked\n").unwrap();
    git_add(temp.path(), Path::new("tracked.txt"));

    let staged = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "0.1.0".to_string(),
            commit: Some(git_head(temp.path())),
            artifacts: Vec::new(),
            out_dir: temp.path().join(".dist"),
        },
    )
    .unwrap();

    let entries = source_archive_entries(&staged.source_path);
    assert!(entries.iter().any(|entry| entry.ends_with("/tracked.txt")));
    assert!(entries
        .iter()
        .all(|entry| !entry.ends_with("/untracked.txt")));
}

#[test]
fn stage_release_roundtrips_text_and_binary_in_the_package_closure() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let readme = "# App\n\nHello, 世界。\n";
    fs::write(temp.path().join("README.md"), readme).unwrap();
    fs::write(temp.path().join("font.ttf"), [0, 159, 146, 150]).unwrap();
    git_add(temp.path(), Path::new("README.md"));
    git_add(temp.path(), Path::new("font.ttf"));

    let staged = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "0.1.0".to_string(),
            commit: Some(git_head(temp.path())),
            artifacts: Vec::new(),
            out_dir: temp.path().join(".dist"),
        },
    )
    .unwrap();

    let entries = source_archive_entries(&staged.source_path);
    assert!(entries.iter().any(|entry| entry.ends_with("/README.md")));
    assert!(entries.iter().any(|entry| entry.ends_with("/font.ttf")));
    assert_eq!(
        source_archive_file(&staged.source_path, "/README.md"),
        readme.as_bytes()
    );
    assert_eq!(
        source_archive_file(&staged.source_path, "/font.ttf"),
        [0, 159, 146, 150]
    );

    let package = PackageManifest::parse(&fs::read(&staged.package_path).unwrap()).unwrap();
    let readme_entry = package
        .files
        .iter()
        .find(|entry| entry.path == "README.md")
        .unwrap();
    let readme_digest = vo_module::digest::Digest::from_sha256(readme.as_bytes());
    assert_eq!(readme_entry.size, readme.len() as u64);
    assert_eq!(readme_entry.digest, readme_digest);
    let binary_entry = package
        .files
        .iter()
        .find(|entry| entry.path == "font.ttf")
        .unwrap();
    assert_eq!(binary_entry.size, 4);
    assert_eq!(
        binary_entry.digest,
        vo_module::digest::Digest::from_sha256(&[0, 159, 146, 150])
    );
}

#[test]
fn stage_release_includes_tracked_directories_regardless_of_output_like_names() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(temp.path().join("keep.txt"), "keep\n").unwrap();
    fs::create_dir_all(temp.path().join(".dist")).unwrap();
    fs::write(temp.path().join(".dist/old.tar.gz"), "old\n").unwrap();
    fs::create_dir_all(temp.path().join(".dist-v0.1.0")).unwrap();
    fs::write(temp.path().join(".dist-v0.1.0/old.tar.gz"), "old\n").unwrap();
    git_add(temp.path(), Path::new("keep.txt"));
    git_add(temp.path(), Path::new(".dist/old.tar.gz"));
    git_add(temp.path(), Path::new(".dist-v0.1.0/old.tar.gz"));

    let staged = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "0.1.0".to_string(),
            commit: Some(git_head(temp.path())),
            artifacts: Vec::new(),
            out_dir: temp.path().join("release-out"),
        },
    )
    .unwrap();

    let entries = source_archive_entries(&staged.source_path);
    assert!(entries.iter().any(|entry| entry.ends_with("/keep.txt")));
    assert!(entries
        .iter()
        .any(|entry| entry.contains("/.dist/old.tar.gz")));
    assert!(entries
        .iter()
        .any(|entry| entry.contains("/.dist-v0.1.0/old.tar.gz")));
}

#[test]
fn stage_release_includes_tracked_names_without_extension_filters() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::create_dir_all(temp.path().join(".diſt-old")).unwrap();
    fs::create_dir_all(temp.path().join(".diﬆ-new")).unwrap();
    fs::write(temp.path().join(".diſt-old/generated.txt"), "old\n").unwrap();
    fs::write(temp.path().join(".diﬆ-new/generated.txt"), "new\n").unwrap();
    fs::write(temp.path().join("lib.ſo"), "binary\n").unwrap();
    fs::write(temp.path().join("Straße.txt"), "keep\n").unwrap();
    git_add(temp.path(), Path::new(".diſt-old/generated.txt"));
    git_add(temp.path(), Path::new(".diﬆ-new/generated.txt"));
    git_add(temp.path(), Path::new("lib.ſo"));
    git_add(temp.path(), Path::new("Straße.txt"));

    let staged = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap();
    let entries = source_archive_entries(&staged.source_path);

    assert!(entries.iter().any(|entry| entry.ends_with("/Straße.txt")));
    assert!(entries.iter().any(|entry| entry.contains("/.diſt-old/")));
    assert!(entries.iter().any(|entry| entry.contains("/.diﬆ-new/")));
    assert!(entries.iter().any(|entry| entry.ends_with("/lib.ſo")));
}

#[test]
fn stage_release_excludes_vo_work_from_source_package() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(
        temp.path().join("vo.work"),
        "version = 1\nmembers = [\"../vopack\"]\n",
    )
    .unwrap();
    git_add(temp.path(), Path::new("vo.work"));

    let staged = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "0.1.0".to_string(),
            commit: Some(git_head(temp.path())),
            artifacts: Vec::new(),
            out_dir: temp.path().join(".dist"),
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

    assert!(entries.iter().all(|entry| !entry.ends_with("/vo.work")));
}

#[test]
fn stage_release_preserves_cargo_patch_bytes_and_manifest_digest() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let cargo = b"[package]\nname = \"demo\"\nversion = \"0.1.0\"\n\n[patch.crates-io]\nsome-dep = { path = \"../some-dep\" }\n";
    fs::write(temp.path().join("Cargo.toml"), cargo).unwrap();
    git_add(temp.path(), Path::new("Cargo.toml"));

    let staged = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap();
    assert_eq!(
        source_archive_file(&staged.source_path, "/Cargo.toml"),
        cargo
    );

    let package = PackageManifest::parse(&fs::read(&staged.package_path).unwrap()).unwrap();
    let entry = package
        .files
        .iter()
        .find(|entry| entry.path == "Cargo.toml")
        .unwrap();
    let digest = vo_module::digest::Digest::from_sha256(cargo);
    assert_eq!(entry.size, cargo.len() as u64);
    assert_eq!(entry.digest, digest);
}

#[test]
fn stage_release_rejects_dirty_tracked_source_before_output() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let options = stage_options(&temp, Vec::new());
    fs::write(temp.path().join("main.vo"), "fn main() { println(1) }\n").unwrap();

    let error = stage_release(temp.path(), &options).unwrap_err();
    assert!(error
        .to_string()
        .contains("tracked working-tree files differ"));
    assert!(!options.out_dir.exists());
}

#[test]
fn stage_release_rejects_staged_source_drift_before_output() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let options = stage_options(&temp, Vec::new());
    fs::write(temp.path().join("main.vo"), "fn main() { println(2) }\n").unwrap();
    git(temp.path(), &["add", "main.vo"]);

    let error = stage_release(temp.path(), &options).unwrap_err();
    assert!(error.to_string().contains("git index differs"));
    assert!(!options.out_dir.exists());
}

#[test]
fn stage_release_rejects_existing_non_head_commit_before_output() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let old_commit = git_stdout(temp.path(), &["rev-list", "--max-parents=0", "HEAD"]);
    assert_ne!(old_commit, git_head(temp.path()));
    let options = StageReleaseOptions {
        version: "0.1.0".to_string(),
        commit: Some(old_commit),
        artifacts: Vec::new(),
        out_dir: temp.path().join(".dist"),
    };

    let error = stage_release(temp.path(), &options).unwrap_err();
    assert!(error.to_string().contains("must equal repository HEAD"));
    assert!(!options.out_dir.exists());
}

#[test]
fn stage_release_rejects_missing_commit_before_output() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let options = StageReleaseOptions {
        version: "0.1.0".to_string(),
        commit: Some("ffffffffffffffffffffffffffffffffffffffff".to_string()),
        artifacts: Vec::new(),
        out_dir: temp.path().join(".dist"),
    };

    let error = stage_release(temp.path(), &options).unwrap_err();
    assert!(error
        .to_string()
        .contains("does not identify an existing commit"));
    assert!(!options.out_dir.exists());
}

#[test]
fn stage_release_rejects_invalid_utf8_vo_source() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(temp.path().join("broken.vo"), [0xff, 0xfe]).unwrap();
    git_add(temp.path(), Path::new("broken.vo"));
    let options = stage_options(&temp, Vec::new());

    let error = stage_release(temp.path(), &options).unwrap_err();
    let message = error.to_string();
    assert!(message.to_ascii_lowercase().contains("utf-8"), "{message}");
    assert!(!options.out_dir.exists());
}

#[test]
fn stage_release_enforces_the_compiler_text_limit_for_vo_sources() {
    fn source_bytes(size: usize) -> Vec<u8> {
        let mut bytes = b"package main\n".to_vec();
        bytes.resize(size, b' ');
        bytes
    }

    let accepted = TempDir::new().unwrap();
    write_basic_repo(accepted.path());
    fs::write(
        accepted.path().join("main.vo"),
        source_bytes(vo_common::vfs::MAX_TEXT_FILE_BYTES),
    )
    .unwrap();
    git_add(accepted.path(), Path::new("main.vo"));
    assert!(stage_release(accepted.path(), &stage_options(&accepted, Vec::new()),).is_ok());

    let rejected = TempDir::new().unwrap();
    write_basic_repo(rejected.path());
    fs::write(
        rejected.path().join("main.vo"),
        source_bytes(vo_common::vfs::MAX_TEXT_FILE_BYTES + 1),
    )
    .unwrap();
    git_add(rejected.path(), Path::new("main.vo"));
    let options = stage_options(&rejected, Vec::new());
    let error = stage_release(rejected.path(), &options).unwrap_err();
    assert!(
        matches!(
            error,
            ReleaseError::ManifestSerialize(ref message)
                if message == &format!(
                    "source file \"main.vo\" exceeds the {}-byte entry limit",
                    vo_common::vfs::MAX_TEXT_FILE_BYTES,
                )
        ),
        "{error}"
    );
    assert!(!options.out_dir.exists());

    let oversized_mod = TempDir::new().unwrap();
    write_basic_repo(oversized_mod.path());
    let mut mod_bytes = b"module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n# ".to_vec();
    mod_bytes.resize(vo_common::vfs::MAX_TEXT_FILE_BYTES + 1, b'x');
    fs::write(oversized_mod.path().join("vo.mod"), mod_bytes).unwrap();
    git_add(oversized_mod.path(), Path::new("vo.mod"));
    let options = stage_options(&oversized_mod, Vec::new());
    let error = stage_release(oversized_mod.path(), &options).unwrap_err();
    assert!(
        matches!(
            error,
            ReleaseError::ManifestSerialize(ref message)
                if message == &format!(
                    "source file \"vo.mod\" exceeds the {}-byte entry limit",
                    vo_common::vfs::MAX_TEXT_FILE_BYTES,
                )
        ),
        "{error}"
    );
    assert!(!options.out_dir.exists());
}

#[test]
fn stage_release_includes_tracked_binary_source_data() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::create_dir_all(temp.path().join("assets")).unwrap();
    fs::write(temp.path().join("assets/payload.bin"), [0xff, 0x00]).unwrap();
    git_add(temp.path(), Path::new("assets/payload.bin"));
    let staged = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap();
    assert_eq!(
        source_archive_file(&staged.source_path, "/assets/payload.bin"),
        [0xff, 0x00]
    );
    let package = PackageManifest::parse(&fs::read(&staged.package_path).unwrap()).unwrap();
    assert!(package
        .files
        .iter()
        .any(|entry| entry.path == "assets/payload.bin"));
}

#[cfg(unix)]
#[test]
fn stage_release_rejects_tracked_mode_drift() {
    use std::os::unix::fs::PermissionsExt;

    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let options = stage_options(&temp, Vec::new());
    let path = temp.path().join("main.vo");
    let mut permissions = fs::metadata(&path).unwrap().permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(path, permissions).unwrap();

    let error = stage_release(temp.path(), &options).unwrap_err();
    assert!(error
        .to_string()
        .contains("tracked working-tree files differ"));
    assert!(!options.out_dir.exists());
}

#[cfg(unix)]
#[test]
fn source_archive_mode_comes_from_commit_tree() {
    use std::os::unix::fs::PermissionsExt;

    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let path = temp.path().join("main.vo");
    let mut permissions = fs::metadata(&path).unwrap().permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(&path, permissions).unwrap();
    git_add(temp.path(), Path::new("main.vo"));

    let staged = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap();
    assert_eq!(
        source_archive_mode(&staged.source_path, "/main.vo") & 0o777,
        0o755
    );
    assert_eq!(
        source_archive_mode(&staged.source_path, "/vo.mod") & 0o777,
        0o644
    );
    assert_eq!(
        source_archive_mode(&staged.source_path, "/vo.package.json") & 0o777,
        0o644
    );
    let package =
        PackageManifest::parse(&fs::read(staged.out_dir.join("vo.package.json")).unwrap()).unwrap();
    assert_eq!(
        package
            .files
            .iter()
            .find(|entry| entry.path == "main.vo")
            .unwrap()
            .mode,
        SourceFileMode::Executable,
    );
    assert_eq!(
        package
            .files
            .iter()
            .find(|entry| entry.path == "vo.mod")
            .unwrap()
            .mode,
        SourceFileMode::Regular,
    );
}

#[cfg(unix)]
#[test]
fn stage_release_rejects_executable_root_mod_file() {
    use std::os::unix::fs::PermissionsExt;

    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let path = temp.path().join("vo.mod");
    let mut permissions = fs::metadata(&path).unwrap().permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(&path, permissions).unwrap();
    git_add(temp.path(), Path::new("vo.mod"));

    let error = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("root vo.mod must have regular mode 0o644"),
        "{error}",
    );
}

#[test]
fn stage_release_roundtrips_nested_module_from_commit_subtree() {
    let temp = TempDir::new().unwrap();
    let module = temp.path().join("graphics/v2");
    fs::create_dir_all(&module).unwrap();
    fs::write(
        module.join("vo.mod"),
        "module = \"github.com/acme/repo/graphics/v2\"\nvo = \"^0.1.0\"\n",
    )
    .unwrap();
    fs::write(module.join("main.vo"), "fn main() {}\n").unwrap();
    fs::write(
        temp.path().join("sibling.txt"),
        "must stay outside module\n",
    )
    .unwrap();
    init_test_git_repo(temp.path());
    git_add(temp.path(), Path::new("graphics/v2/vo.mod"));
    git_add(temp.path(), Path::new("graphics/v2/main.vo"));
    git_add(temp.path(), Path::new("sibling.txt"));

    let staged = stage_release(
        &module,
        &StageReleaseOptions {
            version: "2.1.0".to_string(),
            commit: Some(git_head(temp.path())),
            artifacts: Vec::new(),
            out_dir: module.join(".dist"),
        },
    )
    .unwrap();
    let manifest =
        ReleaseManifest::parse(&fs::read_to_string(&staged.manifest_path).unwrap()).unwrap();
    assert_eq!(manifest.module.as_str(), "github.com/acme/repo/graphics/v2");
    assert!(!fs::read_to_string(&staged.manifest_path)
        .unwrap()
        .contains("module_root"));
    let entries = source_archive_entries(&staged.source_path);
    assert!(entries.iter().any(|entry| entry.ends_with("/vo.mod")));
    assert!(entries.iter().all(|entry| !entry.contains("sibling.txt")));
    assert_eq!(
        source_archive_file(&staged.source_path, "/main.vo"),
        b"fn main() {}\n"
    );
}

#[cfg(unix)]
#[test]
fn stage_release_preserves_trailing_space_in_repository_root_path() {
    let temp = TempDir::new().unwrap();
    let module = temp.path().join("repo ");
    fs::create_dir(&module).unwrap();
    write_basic_repo(&module);

    let staged = stage_release(
        &module,
        &StageReleaseOptions {
            version: "0.1.0".to_string(),
            commit: Some(git_head(&module)),
            artifacts: Vec::new(),
            out_dir: module.join(".dist"),
        },
    )
    .unwrap();

    assert_eq!(staged.repo_root, fs::canonicalize(&module).unwrap());
    assert!(staged.source_path.is_file());
}

// Darwin's path APIs reject ill-formed UTF-8 before the filesystem lookup.
// Other Unix hosts exercise the complete Git/filesystem integration path;
// `repo::internal_tests` covers byte preservation on every Unix target.
#[cfg(all(unix, not(target_vendor = "apple")))]
#[test]
fn stage_release_accepts_non_utf8_repository_parent_component() {
    use std::os::unix::ffi::OsStringExt as _;

    let temp = TempDir::new().unwrap();
    let module = temp
        .path()
        .join(std::ffi::OsString::from_vec(b"repo-\xff".to_vec()));
    fs::create_dir(&module).unwrap();
    write_basic_repo(&module);

    let staged = stage_release(
        &module,
        &StageReleaseOptions {
            version: "0.1.0".to_string(),
            commit: Some(git_head(&module)),
            artifacts: Vec::new(),
            out_dir: module.join(".dist"),
        },
    )
    .unwrap();

    assert_eq!(staged.repo_root, fs::canonicalize(&module).unwrap());
    assert!(staged.manifest_path.is_file());
}

#[test]
fn final_head_validation_detects_allow_empty_commit_drift() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let captured_head = git_head(temp.path());
    git(
        temp.path(),
        &[
            "commit",
            "--allow-empty",
            "--no-gpg-sign",
            "-m",
            "move head without changing tree",
        ],
    );

    let error = crate::repo::validate_release_head(temp.path(), &captured_head).unwrap_err();
    assert!(error.to_string().contains("repository HEAD changed"));
}

#[test]
fn source_package_is_immutable_after_commit_snapshot_capture() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    let root = fs::canonicalize(temp.path()).unwrap();
    let commit = git_head(&root);
    let tree = crate::repo::load_commit_tree(&root, &commit).unwrap();
    let mod_file = crate::repo::read_committed_mod_file(&root, &tree).unwrap();
    let selected = crate::repo::collect_release_source_files(&root, &mod_file, &tree).unwrap();
    let snapshot = crate::repo::capture_release_source_snapshot(&root, &selected, &tree).unwrap();
    let before = crate::repo::build_source_package("app-0.1.0", &snapshot, b"{}\n").unwrap();

    fs::write(
        temp.path().join("main.vo"),
        "fn main() { panic(\"changed\") }\n",
    )
    .unwrap();
    let after = crate::repo::build_source_package("app-0.1.0", &snapshot, b"{}\n").unwrap();
    assert_eq!(before, after);
    let archive_path = temp.path().join("snapshot.tar.gz");
    fs::write(&archive_path, after).unwrap();
    assert_eq!(
        source_archive_file(&archive_path, "/main.vo"),
        b"fn main() {}\n"
    );
}
