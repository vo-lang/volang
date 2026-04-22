use std::fs;
use std::path::Path;
use std::process::Command;

use flate2::read::GzDecoder;
use tar::Archive;
use tempfile::TempDir;
use vo_module::ext_manifest::DeclaredArtifactId;
use vo_module::schema::manifest::ReleaseManifest;

use crate::repo::strip_cargo_patch_sections;
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
    init_test_git_repo(root);
    git_add(root, Path::new("vo.mod"));
    git_add(root, Path::new("vo.lock"));
    git_add(root, Path::new("main.vo"));
}

fn init_test_git_repo(root: &Path) {
    git(root, &["init"]);
    git(
        root,
        &["config", "user.email", "vo-release-tests@example.invalid"],
    );
    git(root, &["config", "user.name", "Vo Release Tests"]);
}

fn git_add(root: &Path, path: &Path) {
    git(root, &["add", path.to_str().unwrap()]);
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

fn stage_options(temp: &TempDir, artifacts: Vec<ArtifactInput>) -> StageReleaseOptions {
    StageReleaseOptions {
        version: "v0.1.0".to_string(),
        commit: Some(TEST_COMMIT.to_string()),
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
    let file = fs::File::open(path).unwrap();
    let decoder = GzDecoder::new(file);
    let mut archive = Archive::new(decoder);
    let mut entries = archive
        .entries()
        .unwrap()
        .map(|entry| entry.unwrap().path().unwrap().display().to_string())
        .collect::<Vec<_>>();
    entries.sort();
    entries
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
    fs::write(
        temp.path().join("vo.ext.toml"),
        standalone_wasm_manifest("demo", "demo.wasm", &[]),
    )
    .unwrap();
    let artifact_path = temp.path().join("demo.wasm");
    fs::write(&artifact_path, b"wasm-bits").unwrap();
    git_add(temp.path(), Path::new("demo.wasm"));

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
    fs::write(
        temp.path().join("vo.ext.toml"),
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
    )
    .unwrap();

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
}

#[test]
fn stage_release_rejects_missing_declared_native_artifact() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(
        temp.path().join("vo.ext.toml"),
        native_manifest(
            "demo",
            &[],
            "rust/target/{profile}/libdemo",
            &[
                ("aarch64-apple-darwin", "libdemo.dylib"),
                ("x86_64-unknown-linux-gnu", "libdemo.so"),
            ],
        ),
    )
    .unwrap();

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
    fs::write(
        temp.path().join("vo.ext.toml"),
        standalone_wasm_manifest("demo", "demo.wasm", &[]),
    )
    .unwrap();

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
    fs::write(
        temp.path().join("vo.ext.toml"),
        standalone_wasm_manifest("demo", "demo.wasm", &[]),
    )
    .unwrap();

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
fn stage_release_rejects_artifacts_without_ext_manifest() {
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

    assert!(matches!(
        err,
        ReleaseError::ArtifactContractViolation {
            manifest_path: None,
            ref missing,
            ref undeclared,
        } if missing.is_empty() && undeclared == &vec![DeclaredArtifactId {
            kind: "extension-wasm".to_string(),
            target: "wasm32-unknown-unknown".to_string(),
            name: "demo.wasm".to_string(),
        }]
    ));
}

#[test]
fn stage_release_rejects_invalid_ext_manifest_schema() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(
        temp.path().join("vo.ext.toml"),
        concat!(
            "[extension]\n",
            "name = \"demo\"\n",
            "path = \"rust/target/{profile}/libdemo\"\n",
        ),
    )
    .unwrap();

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
    fs::write(
        temp.path().join("vo.ext.toml"),
        bindgen_manifest(
            "demo",
            &[],
            "rust/target/{profile}/libdemo",
            &[("aarch64-apple-darwin", "libdemo.dylib")],
            "demo.wasm",
            "demo.js",
        ),
    )
    .unwrap();

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
        "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/vo-lang/vopack v0.1.0\nrequire github.com/vo-lang/vogui v0.1.2\n",
    )
    .unwrap();
    fs::write(
        temp.path().join("vo.lock"),
        r#"version = 1
created_by = "vo test"

[root]
module = "github.com/acme/app"
vo = "0.1.0"

[[resolved]]
path = "github.com/vo-lang/vopack"
version = "v0.1.0"
vo = "^0.1.0"
commit = "0123456789abcdef0123456789abcdef01234567"
release_manifest = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
source = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
deps = []

[[resolved]]
path = "github.com/vo-lang/vogui"
version = "v0.1.2"
vo = "^0.1.0"
commit = "89abcdef0123456789abcdef0123456789abcdef"
release_manifest = "sha256:3333333333333333333333333333333333333333333333333333333333333333"
source = "sha256:4444444444444444444444444444444444444444444444444444444444444444"
deps = []
"#,
    )
    .unwrap();
    fs::write(temp.path().join("main.vo"), "fn main() {}\n").unwrap();
    fs::write(
        temp.path().join("vo.ext.toml"),
        bindgen_wasm_manifest("demo", "z-demo.wasm", "a-demo.js", &[]),
    )
    .unwrap();

    let wasm_artifact_path = temp.path().join("z-demo.wasm");
    let js_artifact_path = temp.path().join("a-demo.js");
    fs::write(&wasm_artifact_path, b"wasm-bits").unwrap();
    fs::write(&js_artifact_path, b"js-bits").unwrap();
    init_test_git_repo(temp.path());
    git_add(temp.path(), Path::new("z-demo.wasm"));
    git_add(temp.path(), Path::new("a-demo.js"));

    let staged = stage_release(
        temp.path(),
        &StageReleaseOptions {
            version: "v0.1.0".to_string(),
            commit: Some(TEST_COMMIT.to_string()),
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
    fs::write(
        temp.path().join("vo.ext.toml"),
        standalone_wasm_manifest(
            "demo",
            "demo.wasm",
            &[
                "js/dist/studio_renderer.js",
                "js/dist/studio_host_bridge.js",
            ],
        ),
    )
    .unwrap();
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
    assert!(entries.iter().any(|entry| entry.ends_with("/vo.ext.toml")));
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
    fs::write(
        temp.path().join("vo.ext.toml"),
        standalone_wasm_manifest("demo", "demo.wasm", &["js/dist"]),
    )
    .unwrap();
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
fn stage_release_fails_when_declared_include_file_is_missing() {
    let temp = TempDir::new().unwrap();
    write_basic_repo(temp.path());
    fs::write(
        temp.path().join("vo.ext.toml"),
        standalone_wasm_manifest("demo", "demo.wasm", &["js/dist/studio_renderer.js"]),
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
        ReleaseError::IoError(_, ref message) if message.contains("included path referenced")
    ));
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
            commit: Some(TEST_COMMIT.to_string()),
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
fn strip_cargo_patch_sections_removes_quoted_patch() {
    let input = "[workspace]\nmembers = [\"ext\"]\n\n[patch.\"https://github.com/vo-lang/volang\"]\nvo-common = { path = \"../../volang/lang/crates/vo-common\" }\nvo-ext = { path = \"../../volang/lang/crates/vo-ext\" }\n\n[dependencies]\nserde = \"1\"\n";
    let result = strip_cargo_patch_sections(input);
    assert!(
        !result.contains("[patch"),
        "patch section should be removed"
    );
    assert!(
        !result.contains("vo-common"),
        "patch entries should be removed"
    );
    assert!(
        result.contains("[workspace]"),
        "workspace section should remain"
    );
    assert!(
        result.contains("[dependencies]"),
        "dependencies section should remain"
    );
    assert!(result.contains("serde"), "non-patch content should remain");
}

#[test]
fn strip_cargo_patch_sections_removes_dotted_patch() {
    let input = "[package]\nname = \"foo\"\n\n[patch.crates-io]\nsome-dep = { path = \"../some-dep\" }\n\n[lib]\ncrate-type = [\"cdylib\"]\n";
    let result = strip_cargo_patch_sections(input);
    assert!(
        !result.contains("[patch"),
        "patch section should be removed"
    );
    assert!(
        !result.contains("some-dep"),
        "patch entries should be removed"
    );
    assert!(
        result.contains("[package]"),
        "package section should remain"
    );
    assert!(result.contains("[lib]"), "lib section should remain");
}

#[test]
fn strip_cargo_patch_sections_preserves_no_patch_file() {
    let input = "[package]\nname = \"foo\"\nversion = \"0.1.0\"\n\n[dependencies]\nvo-vm = { git = \"https://github.com/vo-lang/volang\" }\n";
    let result = strip_cargo_patch_sections(input);
    assert_eq!(result, input);
}
