use std::fs;
use std::io::Read;
use std::path::Path;
use std::process::Command;

use flate2::read::GzDecoder;
use tar::Archive;
use tempfile::TempDir;
use vo_module::ext_manifest::DeclaredArtifactId;
use vo_module::schema::manifest::ReleaseManifest;

use crate::{stage_release, verify_repo, ArtifactInput, ReleaseError, StageReleaseOptions};

fn write_basic_repo(root: &Path) {
    fs::write(
        root.join("vo.mod"),
        "module github.com/acme/app\n\nvo ^0.1.0\n",
    )
    .unwrap();
    // Write a minimal valid vo.lock with no resolved modules
    let lock_content = "version = 2\ncreated_by = \"vo test\"\n\n[root]\nmodule = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n";
    fs::write(root.join("vo.lock"), lock_content).unwrap();
    fs::write(root.join("main.vo"), "fn main() {}\n").unwrap();
    init_test_git_repo(root);
    git_add(root, Path::new("vo.mod"));
    git_add(root, Path::new("vo.lock"));
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

fn stage_options(temp: &TempDir, artifacts: Vec<ArtifactInput>) -> StageReleaseOptions {
    StageReleaseOptions {
        version: "v0.1.0".to_string(),
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

fn render_include_lines(include: &[&str]) -> String {
    if include.is_empty() {
        return String::new();
    }
    let mut rendered = String::from("include = [\n");
    for path in include {
        rendered.push_str(&format!("  \"{}\",\n", path));
    }
    rendered.push_str("]\n\n");
    rendered
}

fn standalone_wasm_manifest(name: &str, wasm: &str, include: &[&str]) -> String {
    format!(
        "[extension]\nname = \"{}\"\n{}[extension.wasm]\ntype = \"standalone\"\nwasm = \"{}\"\n",
        name,
        render_include_lines(include),
        wasm,
    )
}

fn bindgen_wasm_manifest(name: &str, wasm: &str, js_glue: &str, include: &[&str]) -> String {
    format!(
        "[extension]\nname = \"{}\"\n{}[extension.wasm]\ntype = \"bindgen\"\nwasm = \"{}\"\njs_glue = \"{}\"\n",
        name,
        render_include_lines(include),
        wasm,
        js_glue,
    )
}

fn native_manifest(
    name: &str,
    include: &[&str],
    native_path: &str,
    native_targets: &[(&str, &str)],
) -> String {
    let mut rendered = format!(
        "[extension]\nname = \"{}\"\n{}[extension.native]\npath = \"{}\"\n\n",
        name,
        render_include_lines(include),
        native_path,
    );
    for (target, library) in native_targets {
        rendered.push_str(&format!(
            "[[extension.native.targets]]\ntarget = \"{}\"\nlibrary = \"{}\"\n\n",
            target, library,
        ));
    }
    rendered
}

fn bindgen_manifest(
    name: &str,
    include: &[&str],
    native_path: &str,
    native_targets: &[(&str, &str)],
    wasm: &str,
    js_glue: &str,
) -> String {
    let mut rendered = native_manifest(name, include, native_path, native_targets);
    rendered.push_str(&format!(
        "[extension.wasm]\ntype = \"bindgen\"\nwasm = \"{}\"\njs_glue = \"{}\"\n",
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

    assert!(error.to_string().contains("transaction locks"), "{error}");
    assert!(error.to_string().contains(".vo-project.lock"), "{error}");
}

#[test]
fn verify_repo_requires_exact_tracked_vo_mod_spelling() {
    let temp = TempDir::new().unwrap();
    fs::write(
        temp.path().join("VO.MOD"),
        "module github.com/acme/app\n\nvo ^0.1.0\n",
    )
    .unwrap();
    fs::write(temp.path().join("main.vo"), "fn main() {}\n").unwrap();
    init_test_git_repo(temp.path());
    git_add(temp.path(), Path::new("VO.MOD"));
    git_add(temp.path(), Path::new("main.vo"));

    let error = verify_repo(temp.path()).unwrap_err();
    assert!(error.to_string().contains("non-canonical case alias"));
    assert!(error.to_string().contains("VO.MOD"));
}

#[test]
fn verify_repo_rejects_case_alias_for_optional_vo_lock() {
    let temp = TempDir::new().unwrap();
    fs::write(
        temp.path().join("vo.mod"),
        "module github.com/acme/app\n\nvo ^0.1.0\n",
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

    verify_repo(temp.path()).unwrap();
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

    // Write a lock file with a resolved module that has no corresponding require in vo.mod
    let lock_content = r#"version = 2
created_by = "vo test"

[root]
module = "github.com/acme/app"
vo = "^0.1.0"

[[resolved]]
path = "github.com/acme/lib"
version = "v1.2.0"
vo = "^0.1.0"
commit = "0123456789abcdef0123456789abcdef01234567"
release_manifest = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
source = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
deps = []
"#;
    fs::write(temp.path().join("vo.lock"), lock_content).unwrap();
    git_add(temp.path(), Path::new("vo.lock"));

    let err = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "v0.1.0".to_string(),
            commit: Some(git_head(temp.path())),
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
    write_module_metadata(
        temp.path(),
        standalone_wasm_manifest("demo", "demo.wasm", &[]),
    );
    let artifact_path = temp.path().join("demo.wasm");
    fs::write(&artifact_path, b"wasm-bits").unwrap();
    git_add(temp.path(), Path::new("demo.wasm"));

    let staged = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "v0.1.0".to_string(),
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
    assert!(staged.web_manifest_path.is_file());
    assert_eq!(staged.artifacts.len(), 1);
    assert!(staged.artifacts[0].output_path.is_file());
    assert_eq!(
        source_archive_file(&staged.source_path, "/vo.web.json"),
        fs::read(&staged.web_manifest_path).unwrap()
    );
    assert_eq!(
        staged.assets(),
        vec![
            staged.manifest_path.as_path(),
            staged.web_manifest_path.as_path(),
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
    assert_eq!(manifest.module.as_str(), "github.com/acme/app");
    assert_eq!(manifest.version.to_string(), "v0.1.0");
    assert_eq!(manifest.commit, staged.commit);
    assert_eq!(manifest.source.name, "app-v0.1.0.tar.gz");
    assert_eq!(manifest.source.files_size, staged.source_files_size);
    assert_eq!(
        manifest.source.files_digest.to_string(),
        staged.source_files_digest
    );
    let web_manifest_bytes = fs::read(&staged.web_manifest_path).unwrap();
    assert_eq!(manifest.web_manifest.size, web_manifest_bytes.len() as u64);
    assert_eq!(
        manifest.web_manifest.digest,
        vo_module::digest::Digest::from_sha256(&web_manifest_bytes)
    );
    let web_manifest: serde_json::Value = serde_json::from_slice(&web_manifest_bytes).unwrap();
    let web_size = web_manifest["source"]
        .as_array()
        .unwrap()
        .iter()
        .map(|entry| entry["size"].as_u64().unwrap())
        .sum::<u64>();
    assert_eq!(manifest.source.files_size, web_size);
    assert_eq!(
        manifest.source.files_digest.as_str(),
        web_manifest["source_digest"].as_str().unwrap()
    );
    assert_ne!(manifest.source.digest, manifest.source.files_digest);
    assert!(manifest
        .artifacts
        .iter()
        .any(|a| a.id.kind == "extension-wasm" && a.id.target == "wasm32-unknown-unknown"));
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
            &[],
            "rust/target/{profile}/libdemo",
            &[
                ("aarch64-apple-darwin", "libdemo.dylib"),
                ("x86_64-unknown-linux-gnu", "libdemo.so"),
            ],
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

    let web_json = fs::read_to_string(staged.out_dir.join("vo.web.json")).unwrap();
    let web: serde_json::Value = serde_json::from_str(&web_json).unwrap();
    let artifacts = web
        .get("artifacts")
        .and_then(serde_json::Value::as_array)
        .unwrap();
    assert_eq!(artifacts.len(), 2);
    assert!(artifacts.iter().all(|artifact| {
        matches!(
            artifact.get("kind").and_then(serde_json::Value::as_str),
            Some("extension-wasm" | "extension-js-glue")
        )
    }));
}

#[test]
fn stage_release_isolates_same_logical_name_across_artifact_identities() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        native_manifest(
            "demo",
            &[],
            "rust/target/{profile}/libdemo",
            &[
                ("aarch64-apple-darwin", "libdemo.bin"),
                ("x86_64-unknown-linux-gnu", "libdemo.bin"),
            ],
        ),
    );
    fs::create_dir_all(temp.path().join("build")).unwrap();
    let apple_path = temp.path().join("build/apple.bin");
    let linux_path = temp.path().join("build/linux.bin");
    fs::write(&apple_path, b"apple").unwrap();
    fs::write(&linux_path, b"linux").unwrap();
    git_add(temp.path(), Path::new("build/apple.bin"));
    git_add(temp.path(), Path::new("build/linux.bin"));

    let staged = stage_release(
        temp.path(),
        &stage_options(
            &temp,
            vec![
                ArtifactInput {
                    kind: "extension-native".to_string(),
                    target: "x86_64-unknown-linux-gnu".to_string(),
                    name: "libdemo.bin".to_string(),
                    path: linux_path,
                },
                ArtifactInput {
                    kind: "extension-native".to_string(),
                    target: "aarch64-apple-darwin".to_string(),
                    name: "libdemo.bin".to_string(),
                    path: apple_path,
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
    assert_eq!(
        fs::read(&staged.artifacts[0].output_path).unwrap(),
        b"apple"
    );
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
            .filter(|artifact| artifact.id.name == "libdemo.bin")
            .count(),
        2
    );
}

#[test]
fn stage_release_rejects_duplicate_full_artifact_identity_before_output() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        standalone_wasm_manifest("demo", "demo.wasm", &[]),
    );
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
            version: "v0.1.0".to_string(),
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
fn stage_release_rejects_duplicate_browser_artifact_paths() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        r#"[extension]
name = "demo"

[extension.wasm]
type = "bindgen"
wasm = "demo.wasm"
js_glue = "demo.js"
local_wasm = "shared.bin"
local_js_glue = "shared.bin"
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
    assert!(error.to_string().contains("reuse path"));
}

#[test]
fn stage_release_rejects_missing_declared_native_artifact() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        native_manifest(
            "demo",
            &[],
            "rust/target/{profile}/libdemo",
            &[
                ("aarch64-apple-darwin", "libdemo.dylib"),
                ("x86_64-unknown-linux-gnu", "libdemo.so"),
            ],
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
    write_module_metadata(
        temp.path(),
        standalone_wasm_manifest("demo", "demo.wasm", &[]),
    );

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
    write_module_metadata(
        temp.path(),
        standalone_wasm_manifest("demo", "demo.wasm", &[]),
    );

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
        ReleaseError::Module(ref message) if message.contains("[extension].path is invalid")
    ));
}

#[test]
fn stage_release_rejects_bindgen_wasm_missing_js_glue_artifact() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        bindgen_manifest(
            "demo",
            &[],
            "rust/target/{profile}/libdemo",
            &[("aarch64-apple-darwin", "libdemo.dylib")],
            "demo.wasm",
            "demo.js",
        ),
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
        "module github.com/acme/app\n\nvo ^0.1.0\n\nrequire github.com/vo-lang/vopack v0.1.0\nrequire github.com/vo-lang/vogui v0.1.2\n",
    )
    .unwrap();
    fs::write(
        temp.path().join("vo.lock"),
        r#"version = 2
created_by = "vo test"

[root]
module = "github.com/acme/app"
vo = "^0.1.0"

[[resolved]]
path = "github.com/vo-lang/vogui"
version = "v0.1.2"
vo = "^0.1.0"
commit = "89abcdef0123456789abcdef0123456789abcdef"
release_manifest = "sha256:3333333333333333333333333333333333333333333333333333333333333333"
source = "sha256:4444444444444444444444444444444444444444444444444444444444444444"
deps = []

[[resolved]]
path = "github.com/vo-lang/vopack"
version = "v0.1.0"
vo = "^0.1.0"
commit = "0123456789abcdef0123456789abcdef01234567"
release_manifest = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
source = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
deps = []
"#,
    )
    .unwrap();
    fs::write(temp.path().join("main.vo"), "fn main() {}\n").unwrap();
    let metadata = bindgen_wasm_manifest("demo", "z-demo.wasm", "a-demo.js", &[]);
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
            version: "v0.1.0".to_string(),
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
}

#[test]
fn stage_release_includes_declared_include_files_from_dist_dirs() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        standalone_wasm_manifest(
            "demo",
            "demo.wasm",
            &[
                "js/dist/studio_renderer.js",
                "js/dist/studio_host_bridge.js",
            ],
        ),
    );
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
    assert!(entries.iter().any(|entry| entry.ends_with("/vo.web.json")));
    assert!(entries
        .iter()
        .any(|entry| entry.ends_with("/js/dist/studio_renderer.js")));
    assert!(entries
        .iter()
        .any(|entry| entry.ends_with("/js/dist/studio_host_bridge.js")));
}

#[test]
fn stage_release_includes_declared_include_directories_recursively() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        standalone_wasm_manifest("demo", "demo.wasm", &["js/dist"]),
    );
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
fn declared_include_directory_is_expanded_from_the_commit_tree() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        "[web]\nentry = \"main.vo\"\ninclude = [\"js/dist\"]\n".to_string(),
    );
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

    let selected =
        crate::repo::collect_publish_source_files(&root, &root.join(".dist"), &mod_file, &tree)
            .unwrap();
    let snapshot = crate::repo::capture_release_source_snapshot(&root, &selected, &tree).unwrap();
    let archive = crate::repo::build_source_package("snapshot-test", &snapshot, "{}\n").unwrap();
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
fn explicitly_included_vo_sum_cannot_bypass_release_validation() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        "[web]\nentry = \"main.vo\"\ninclude = [\"dist/vo.sum\"]\n".to_string(),
    );
    fs::create_dir_all(temp.path().join("dist")).unwrap();
    fs::write(temp.path().join("dist/vo.sum"), "stale\n").unwrap();
    git_add(temp.path(), Path::new("dist/vo.sum"));
    let options = stage_options(&temp, Vec::new());

    let error = stage_release(temp.path(), &options).unwrap_err();

    assert!(matches!(
        error,
        ReleaseError::ForbiddenVoSum { ref paths, .. }
            if paths == &vec![Path::new("dist/vo.sum").to_path_buf()]
    ));
    assert!(!options.out_dir.exists());
}

#[test]
fn explicitly_included_generated_protocol_file_is_rejected() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        "[web]\nentry = \"main.vo\"\ninclude = [\"vo.web.json\"]\n".to_string(),
    );
    fs::write(temp.path().join("vo.web.json"), "{}\n").unwrap();
    git_add(temp.path(), Path::new("vo.web.json"));
    let options = stage_options(&temp, Vec::new());

    let error = stage_release(temp.path(), &options).unwrap_err();

    assert!(
        error
            .to_string()
            .contains("generated release protocol path"),
        "{error}"
    );
    assert!(!options.out_dir.exists());
}

#[test]
fn explicitly_included_full_fold_web_manifest_alias_is_rejected() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        "[web]\nentry = \"main.vo\"\ninclude = [\"vo.web.jſon\"]\n".to_string(),
    );
    fs::write(temp.path().join("vo.web.jſon"), "{}\n").unwrap();
    git_add(temp.path(), Path::new("vo.web.jſon"));
    let options = stage_options(&temp, Vec::new());

    let error = stage_release(temp.path(), &options).unwrap_err();

    assert!(
        error.to_string().contains("vo.web.jſon")
            || error.to_string().contains("portable spelling"),
        "{error}"
    );
    assert!(!options.out_dir.exists());
}

#[test]
fn explicitly_included_vo_source_cannot_bypass_alias_validation() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        "[web]\nentry = \"main.vo\"\ninclude = [\"dist/bad.vo\"]\n".to_string(),
    );
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
fn stage_release_fails_when_declared_include_file_is_missing() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        standalone_wasm_manifest("demo", "demo.wasm", &["js/dist/studio_renderer.js"]),
    );

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

    assert!(matches!(
        err,
        ReleaseError::IoError(_, ref message) if message.contains("included path referenced")
    ));
}

#[test]
fn stage_release_fails_when_declared_include_file_is_untracked() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        standalone_wasm_manifest("demo", "demo.wasm", &["js/dist/studio_renderer.js"]),
    );
    fs::create_dir_all(temp.path().join("js/dist")).unwrap();
    fs::write(
        temp.path().join("js/dist/studio_renderer.js"),
        "export const renderer = 1;\n",
    )
    .unwrap();

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

    assert!(matches!(
        err,
        ReleaseError::IoError(_, ref message) if message.contains("must be checked into git")
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
            version: "v0.1.0".to_string(),
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
            version: "v0.1.0".to_string(),
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
                version: "v0.1.0".to_string(),
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
            version: "v0.1.0".to_string(),
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
            version: "v0.1.0".to_string(),
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
fn stage_release_roundtrips_text_and_excludes_binary_from_install_source_set() {
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
            version: "v0.1.0".to_string(),
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

    let web_json = fs::read_to_string(staged.out_dir.join("vo.web.json")).unwrap();
    let web: serde_json::Value = serde_json::from_str(&web_json).unwrap();
    let source = web
        .get("source")
        .and_then(serde_json::Value::as_array)
        .unwrap();
    let readme_entry = source
        .iter()
        .find(|entry| entry.get("path").and_then(serde_json::Value::as_str) == Some("README.md"))
        .unwrap();
    let readme_digest = vo_module::digest::Digest::from_sha256(readme.as_bytes());
    assert_eq!(readme_entry["size"].as_u64(), Some(readme.len() as u64));
    assert_eq!(
        readme_entry["digest"].as_str(),
        Some(readme_digest.as_str())
    );
    assert!(source.iter().all(|entry| {
        entry.get("path").and_then(serde_json::Value::as_str) != Some("font.ttf")
    }));

    let installed =
        vo_module::cache::install::extract_source_entries(&fs::read(&staged.source_path).unwrap())
            .unwrap();
    assert!(installed
        .iter()
        .any(|(path, content)| path == Path::new("README.md") && content == readme));
    assert!(installed
        .iter()
        .all(|(path, _)| path != Path::new("font.ttf")));
}

#[test]
fn stage_release_excludes_committed_release_output_dirs_from_source_package() {
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
            version: "v0.1.0".to_string(),
            commit: Some(git_head(temp.path())),
            artifacts: Vec::new(),
            out_dir: temp.path().join("release-out"),
        },
    )
    .unwrap();

    let entries = source_archive_entries(&staged.source_path);
    assert!(entries.iter().any(|entry| entry.ends_with("/keep.txt")));
    assert!(entries.iter().all(|entry| !entry.contains(".dist/")));
    assert!(entries.iter().all(|entry| !entry.contains(".dist-v")));
}

#[test]
fn stage_release_excludes_full_fold_reserved_path_aliases() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::create_dir_all(temp.path().join(".diſt-old")).unwrap();
    fs::create_dir_all(temp.path().join(".diﬆ-new")).unwrap();
    fs::write(temp.path().join(".diſt-old/generated.txt"), "old\n").unwrap();
    fs::write(temp.path().join(".diﬆ-new/generated.txt"), "new\n").unwrap();
    fs::write(temp.path().join("lib.ſo"), "binary\n").unwrap();
    fs::write(temp.path().join("vo.releaſe.json"), "generated\n").unwrap();
    fs::write(temp.path().join("Straße.txt"), "keep\n").unwrap();
    git_add(temp.path(), Path::new(".diſt-old/generated.txt"));
    git_add(temp.path(), Path::new(".diﬆ-new/generated.txt"));
    git_add(temp.path(), Path::new("lib.ſo"));
    git_add(temp.path(), Path::new("vo.releaſe.json"));
    git_add(temp.path(), Path::new("Straße.txt"));

    let staged = stage_release(temp.path(), &stage_options(&temp, Vec::new())).unwrap();
    let entries = source_archive_entries(&staged.source_path);

    assert!(entries.iter().any(|entry| entry.ends_with("/Straße.txt")));
    assert!(entries.iter().all(|entry| !entry.contains(".diſt-old")));
    assert!(entries.iter().all(|entry| !entry.contains(".diﬆ-new")));
    assert!(entries.iter().all(|entry| !entry.ends_with("/lib.ſo")));
    assert!(entries
        .iter()
        .all(|entry| !entry.ends_with("/vo.releaſe.json")));
}

#[test]
fn stage_release_excludes_vo_work_from_source_package() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(
        temp.path().join("vo.work"),
        "version = 1\n\n[[use]]\npath = \"../vopack\"\n",
    )
    .unwrap();

    let staged = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "v0.1.0".to_string(),
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

    let web: serde_json::Value =
        serde_json::from_slice(&fs::read(&staged.web_manifest_path).unwrap()).unwrap();
    let entry = web["source"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["path"] == "Cargo.toml")
        .unwrap();
    let digest = vo_module::digest::Digest::from_sha256(cargo);
    assert_eq!(entry["size"].as_u64(), Some(cargo.len() as u64));
    assert_eq!(entry["digest"].as_str(), Some(digest.as_str()));

    let extracted =
        vo_module::cache::install::extract_source_entries(&fs::read(&staged.source_path).unwrap())
            .unwrap();
    assert!(extracted
        .iter()
        .any(|(path, content)| { path == Path::new("Cargo.toml") && content.as_bytes() == cargo }));
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
        version: "v0.1.0".to_string(),
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
        version: "v0.1.0".to_string(),
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
fn stage_release_rejects_invalid_utf8_explicit_include() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    write_module_metadata(
        temp.path(),
        "[web]\nentry = \"main.vo\"\ninclude = [\"assets/payload.bin\"]\n".to_string(),
    );
    fs::create_dir_all(temp.path().join("assets")).unwrap();
    fs::write(temp.path().join("assets/payload.bin"), [0xff, 0x00]).unwrap();
    git_add(temp.path(), Path::new("assets/payload.bin"));
    let options = stage_options(&temp, Vec::new());

    let error = stage_release(temp.path(), &options).unwrap_err();
    assert!(error.to_string().contains("explicitly included source"));
    assert!(error.to_string().contains("not valid UTF-8"));
    assert!(!options.out_dir.exists());
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
}

#[test]
fn stage_release_roundtrips_nested_module_from_commit_subtree() {
    let temp = TempDir::new().unwrap();
    let module = temp.path().join("graphics/v2");
    fs::create_dir_all(&module).unwrap();
    fs::write(
        module.join("vo.mod"),
        "module github.com/acme/repo/graphics/v2\n\nvo ^0.1.0\n",
    )
    .unwrap();
    fs::write(
        module.join("vo.lock"),
        "version = 2\ncreated_by = \"vo test\"\n\n[root]\nmodule = \"github.com/acme/repo/graphics/v2\"\nvo = \"^0.1.0\"\n",
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
    git_add(temp.path(), Path::new("graphics/v2/vo.lock"));
    git_add(temp.path(), Path::new("graphics/v2/main.vo"));
    git_add(temp.path(), Path::new("sibling.txt"));

    let staged = stage_release(
        &module,
        &StageReleaseOptions {
            version: "v2.1.0".to_string(),
            commit: Some(git_head(temp.path())),
            artifacts: Vec::new(),
            out_dir: module.join(".dist"),
        },
    )
    .unwrap();
    let manifest =
        ReleaseManifest::parse(&fs::read_to_string(&staged.manifest_path).unwrap()).unwrap();
    assert_eq!(manifest.module_root, "graphics/v2");
    let entries = source_archive_entries(&staged.source_path);
    assert!(entries.iter().any(|entry| entry.ends_with("/vo.mod")));
    assert!(entries.iter().all(|entry| !entry.contains("sibling.txt")));
    let extracted =
        vo_module::cache::install::extract_source_entries(&fs::read(&staged.source_path).unwrap())
            .unwrap();
    assert!(extracted
        .iter()
        .any(|(path, _)| path == Path::new("main.vo")));
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
            version: "v0.1.0".to_string(),
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
            version: "v0.1.0".to_string(),
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
    let selected =
        crate::repo::collect_publish_source_files(&root, &root.join(".dist"), &mod_file, &tree)
            .unwrap();
    let snapshot = crate::repo::capture_release_source_snapshot(&root, &selected, &tree).unwrap();
    let before = crate::repo::build_source_package("app-v0.1.0", &snapshot, "{}\n").unwrap();

    fs::write(
        temp.path().join("main.vo"),
        "fn main() { panic(\"changed\") }\n",
    )
    .unwrap();
    let after = crate::repo::build_source_package("app-v0.1.0", &snapshot, "{}\n").unwrap();
    assert_eq!(before, after);
    let archive_path = temp.path().join("snapshot.tar.gz");
    fs::write(&archive_path, after).unwrap();
    assert_eq!(
        source_archive_file(&archive_path, "/main.vo"),
        b"fn main() {}\n"
    );
}
