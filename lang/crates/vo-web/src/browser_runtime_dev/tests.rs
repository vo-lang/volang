use super::*;
use std::time::{SystemTime, UNIX_EPOCH};

use vo_module::digest::Digest;
use vo_module::ext_manifest::parse_ext_manifest_content;
use vo_module::identity::{ArtifactId, ModulePath};
use vo_module::readiness::{ReadyModule, ResolvedArtifact};
use vo_module::schema::lockfile::{LockedArtifact, LockedModule};
use vo_module::schema::manifest::{ManifestArtifact, ManifestSource, ReleaseManifest};
use vo_module::version::{ExactVersion, ToolchainConstraint};

fn temp_dir(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "vo-web-{}-{}-{}",
        name,
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos(),
    ));
    fs::create_dir_all(&dir).unwrap();
    dir
}

fn parse_manifest(content: &str) -> vo_module::ext_manifest::ExtensionManifest {
    parse_ext_manifest_content(content, Path::new("/tmp/vo.mod")).unwrap()
}

fn resolved_artifact(kind: &str, name: &str) -> ResolvedArtifact {
    ResolvedArtifact {
        id: ArtifactId {
            kind: kind.to_string(),
            target: BROWSER_WASM_TARGET.to_string(),
            name: name.to_string(),
        },
        cache_relative_path: Path::new("artifacts").join(name),
        size: 1,
        digest: Digest::parse(
            "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        )
        .unwrap(),
    }
}

fn locked_artifact(kind: &str, target: &str, name: &str, bytes: &[u8]) -> LockedArtifact {
    LockedArtifact {
        id: ArtifactId {
            kind: kind.to_string(),
            target: target.to_string(),
            name: name.to_string(),
        },
        size: bytes.len() as u64,
        digest: Digest::from_sha256(bytes),
    }
}

fn locked_module(
    module: &str,
    version: &str,
    artifacts: Vec<LockedArtifact>,
) -> (LockedModule, String) {
    let source_bytes = b"source-archive";
    let manifest = ReleaseManifest {
        schema_version: 1,
        module: ModulePath::parse(module).unwrap(),
        version: ExactVersion::parse(version).unwrap(),
        commit: "1111111111111111111111111111111111111111".to_string(),
        module_root: ".".to_string(),
        vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
        require: Vec::new(),
        source: ManifestSource {
            name: format!("{}-{}-source.tar.gz", module.replace('/', "-"), version),
            size: source_bytes.len() as u64,
            digest: Digest::from_sha256(source_bytes),
        },
        artifacts: artifacts
            .iter()
            .map(|artifact| ManifestArtifact {
                id: artifact.id.clone(),
                size: artifact.size,
                digest: artifact.digest.clone(),
            })
            .collect(),
    };
    let release_manifest_content = format!("{}\n", manifest.render());
    (
        LockedModule {
            path: manifest.module.clone(),
            version: manifest.version.clone(),
            vo: manifest.vo.clone(),
            commit: manifest.commit.clone(),
            release_manifest: Digest::from_sha256(release_manifest_content.as_bytes()),
            source: manifest.source.digest.clone(),
            deps: Vec::new(),
            artifacts,
        },
        release_manifest_content,
    )
}

fn populate_cached_module(
    cache_root: &Path,
    locked: &LockedModule,
    release_manifest_content: &str,
    ext_manifest_content: Option<&str>,
    files: &[(&str, &[u8])],
) -> PathBuf {
    let module_dir = vo_module::cache::layout::cache_dir(cache_root, &locked.path, &locked.version);
    fs::create_dir_all(&module_dir).unwrap();
    let mut mod_content = format!("module {}\n\nvo {}\n", locked.path, locked.vo);
    if let Some(ext_manifest_content) = ext_manifest_content {
        mod_content.push('\n');
        mod_content.push_str(ext_manifest_content);
    }
    fs::write(module_dir.join("vo.mod"), mod_content).unwrap();
    fs::write(
        module_dir.join(".vo-version"),
        format!("{}\n", locked.version),
    )
    .unwrap();
    fs::write(
        module_dir.join(".vo-source-digest"),
        format!("{}\n", locked.source),
    )
    .unwrap();
    fs::write(
        module_dir.join("vo.release.json"),
        release_manifest_content.as_bytes(),
    )
    .unwrap();
    for (relative_path, bytes) in files {
        let path = module_dir.join(relative_path);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(path, bytes).unwrap();
    }
    module_dir.canonicalize().unwrap_or(module_dir)
}

#[test]
fn browser_artifact_plan_from_fs_plans_local_bindgen_island() {
    let root = temp_dir("artifact-plan-bindgen");
    let rust_root = root.join("rust");
    fs::create_dir_all(&rust_root).unwrap();
    fs::write(
        rust_root.join("Cargo.toml"),
        "[package]\nname = \"voplay\"\nversion = \"0.1.0\"\n[features]\nwasm-island = []\n",
    )
    .unwrap();

    let runtime = browser_runtime_plan_from_manifest(
        &root.to_string_lossy(),
        Some("github.com/vo-lang/voplay"),
        &parse_manifest(
            r#"
[extension]
name = "voplay"

[extension.wasm]
type = "bindgen"
wasm = "voplay_island_bg.wasm"
js_glue = "voplay_island.js"
local_wasm = "rust/pkg-island/voplay_island_bg.wasm"
local_js_glue = "rust/pkg-island/voplay_island.js"

[extension.web]
capabilities = ["widget", "browser_runtime"]

[extension.web.js]
renderer = "js/dist/voplay-renderer.js"
"#,
        ),
    );
    let intent = runtime.artifact_intent().unwrap();
    let plan = browser_artifact_plan_from_fs(&intent, &runtime).unwrap();

    assert_eq!(plan.actions.len(), 1);
    match &plan.actions[0] {
        ArtifactActionSpec::EnsurePkgIsland(action) => {
            assert_eq!(action.module_key, "github.com/vo-lang/voplay");
            assert_eq!(action.extension_name, "voplay");
            assert_eq!(action.rust_root, rust_root);
            assert_eq!(action.crate_root, rust_root);
            assert_eq!(action.out_dir, root.join("rust").join("pkg-island"));
            assert_eq!(action.out_name, "voplay_island");
        }
        _ => panic!("expected bindgen island action"),
    }

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn browser_artifact_plan_from_fs_ignores_ready_module_artifacts() {
    let ready = ReadyModule {
        module: ModulePath::parse("github.com/vo-lang/vogui").unwrap(),
        version: ExactVersion::parse("v0.1.4").unwrap(),
        module_dir: Path::new("github.com@vo-lang@vogui/v0.1.4").to_path_buf(),
        artifacts: vec![resolved_artifact("extension-wasm", "vogui.wasm")],
        ext_manifest: Some(parse_manifest(
            r#"
[extension]
name = "vogui"

[extension.wasm]
type = "standalone"
wasm = "vogui.wasm"

[extension.web]
capabilities = ["widget"]

[extension.web.js]
renderer = "js/dist/studio_renderer.js"
"#,
        )),
    };

    let runtime = plan_ready_browser_runtime_at(&[ready], "/mod-cache").unwrap();
    let intent = runtime.artifact_intent().unwrap();
    let plan = browser_artifact_plan_from_fs(&intent, &runtime).unwrap();

    assert!(plan.actions.is_empty());
}

#[test]
fn published_browser_runtime_plan_from_fs_uses_locked_runtime_only() {
    let cache_root = temp_dir("published-runtime-plan").canonicalize().unwrap();
    let wasm_bytes = b"\0asm";
    let js_glue_bytes = b"export default 1;\n";
    let renderer_bytes = b"export const render = 1;\n";
    let (locked, release_manifest_content) = locked_module(
        "github.com/acme/demo",
        "v1.2.3",
        vec![
            locked_artifact(
                "extension-js-glue",
                BROWSER_WASM_TARGET,
                "demo.js",
                js_glue_bytes,
            ),
            locked_artifact(
                "extension-wasm",
                BROWSER_WASM_TARGET,
                "demo_bg.wasm",
                wasm_bytes,
            ),
        ],
    );
    let module_dir = populate_cached_module(
        &cache_root,
        &locked,
        &release_manifest_content,
        Some(
            r#"
[extension]
name = "demo"

[extension.wasm]
type = "bindgen"
wasm = "demo_bg.wasm"
js_glue = "demo.js"

[extension.web]
entry = "Run"
capabilities = ["widget", "browser_runtime"]

[extension.web.js]
renderer = "js/dist/demo-renderer.js"
"#,
        ),
        &[
            ("artifacts/demo_bg.wasm", wasm_bytes),
            ("artifacts/demo.js", js_glue_bytes),
            ("js/dist/demo-renderer.js", renderer_bytes),
        ],
    );
    let expected_renderer_path = module_dir
        .join("js")
        .join("dist")
        .join("demo-renderer.js")
        .to_string_lossy()
        .to_string();
    let expected_wasm_path = module_dir
        .join("artifacts")
        .join("demo_bg.wasm")
        .to_string_lossy()
        .to_string();
    let expected_js_glue_path = module_dir
        .join("artifacts")
        .join("demo.js")
        .to_string_lossy()
        .to_string();

    let plan = published_browser_runtime_plan_from_fs(&[locked], &cache_root).unwrap();

    assert_eq!(plan.runtime_modules.len(), 1);
    assert_eq!(plan.graph.frameworks.len(), 1);
    assert_eq!(plan.wasm_bindings.len(), 1);
    assert_eq!(plan.wasm_extensions.len(), 1);
    assert_eq!(plan.runtime_modules[0].module_key, "github.com/acme/demo");
    assert_eq!(
        plan.runtime_modules[0].module_root,
        module_dir.to_string_lossy().to_string()
    );
    assert_eq!(plan.wasm_bindings[0].module_key, "github.com/acme/demo");
    assert_eq!(
        plan.wasm_bindings[0].source,
        BrowserArtifactSource::ReadyModule
    );
    assert_eq!(
        plan.runtime_modules[0]
            .contract
            .js_modules
            .get("renderer")
            .unwrap(),
        &expected_renderer_path,
    );
    assert_eq!(plan.wasm_extensions[0].wasm_path, expected_wasm_path);
    assert_eq!(
        plan.wasm_extensions[0].js_glue_path.as_deref(),
        Some(expected_js_glue_path.as_str()),
    );
    assert_eq!(
        plan.primary_framework_split()
            .primary_framework
            .as_ref()
            .unwrap()
            .name,
        "demo",
    );
    let artifact_sources = plan
        .artifact_intent()
        .unwrap()
        .required_artifacts
        .into_iter()
        .map(|artifact| (artifact.module_key, artifact.source))
        .collect::<Vec<_>>();
    assert_eq!(
        artifact_sources,
        vec![(
            "github.com/acme/demo".to_string(),
            BrowserArtifactSource::ReadyModule,
        )],
    );

    fs::remove_dir_all(&cache_root).unwrap();
}

#[test]
fn debug_local_project_browser_runtime_plan_from_fs_reads_local_manifest_only() {
    let project_root = temp_dir("debug-local-runtime-project")
        .canonicalize()
        .unwrap();
    fs::create_dir_all(project_root.join("js").join("dist")).unwrap();
    fs::write(
        project_root.join("vo.mod"),
        r#"module github.com/acme/app
vo 0.1.0

[extension]
name = "app"

[extension.wasm]
type = "standalone"
wasm = "app.wasm"

[extension.web]
entry = "Run"
capabilities = ["widget", "protocol"]

[extension.web.js]
protocol = "js/dist/app-protocol.js"
"#,
    )
    .unwrap();
    fs::write(
        project_root.join("js").join("dist").join("app-protocol.js"),
        "export const protocol = 1;\n",
    )
    .unwrap();

    let plan = debug_local_project_browser_runtime_plan_from_fs(&project_root).unwrap();

    assert_eq!(plan.runtime_modules.len(), 1);
    assert_eq!(plan.graph.frameworks.len(), 1);
    assert_eq!(plan.wasm_bindings.len(), 1);
    assert_eq!(plan.runtime_modules[0].module_key, "github.com/acme/app");
    assert_eq!(
        plan.wasm_bindings[0].source,
        BrowserArtifactSource::LocalManifest
    );

    fs::remove_dir_all(&project_root).unwrap();
}

#[test]
fn native_gui_browser_runtime_plan_from_fs_merges_local_and_locked_modules() {
    let cache_root = temp_dir("native-gui-runtime-cache").canonicalize().unwrap();
    let remote_wasm_bytes = b"\0asm-remote";
    let (locked, release_manifest_content) = locked_module(
        "github.com/acme/remote",
        "v1.2.3",
        vec![locked_artifact(
            "extension-wasm",
            BROWSER_WASM_TARGET,
            "remote.wasm",
            remote_wasm_bytes,
        )],
    );
    populate_cached_module(
        &cache_root,
        &locked,
        &release_manifest_content,
        Some(
            r#"
[extension]
name = "remote"

[extension.wasm]
type = "standalone"
wasm = "remote.wasm"

[extension.web]
capabilities = ["widget", "protocol"]

[extension.web.js]
protocol = "js/dist/remote-protocol.js"
"#,
        ),
        &[
            ("artifacts/remote.wasm", remote_wasm_bytes),
            (
                "js/dist/remote-protocol.js",
                b"export const protocol = 1;\n",
            ),
        ],
    );

    let local_root = temp_dir("native-gui-runtime-local").canonicalize().unwrap();
    fs::create_dir_all(local_root.join("js").join("dist")).unwrap();
    fs::create_dir_all(local_root.join("web-artifacts")).unwrap();
    fs::write(
        local_root.join("vo.mod"),
        r#"module github.com/acme/local
vo 0.1.0

[extension]
name = "local"

[extension.wasm]
type = "bindgen"
wasm = "local_bg.wasm"
js_glue = "local.js"
local_wasm = "web-artifacts/local_bg.wasm"
local_js_glue = "web-artifacts/local.js"

[extension.web]
capabilities = ["widget", "island_transport"]

[extension.web.js]
renderer = "js/dist/local-renderer.js"
"#,
    )
    .unwrap();
    fs::write(
        local_root.join("js").join("dist").join("local-renderer.js"),
        "export const renderer = 1;\n",
    )
    .unwrap();

    let plan = native_gui_browser_runtime_plan_from_fs(
        &[local_root.join("vo.mod")],
        &[locked],
        &cache_root,
    )
    .unwrap();

    let module_keys = plan
        .runtime_modules
        .iter()
        .map(|module| module.module_key.as_str())
        .collect::<Vec<_>>();
    assert!(module_keys.contains(&"github.com/acme/local"));
    assert!(module_keys.contains(&"github.com/acme/remote"));

    let artifact_sources = plan
        .artifact_intent()
        .unwrap()
        .required_artifacts
        .into_iter()
        .map(|artifact| (artifact.module_key, artifact.source))
        .collect::<Vec<_>>();
    assert!(artifact_sources.contains(&(
        "github.com/acme/local".to_string(),
        BrowserArtifactSource::LocalManifest,
    )));
    assert!(artifact_sources.contains(&(
        "github.com/acme/remote".to_string(),
        BrowserArtifactSource::ReadyModule,
    )));

    fs::remove_dir_all(&cache_root).unwrap();
    fs::remove_dir_all(&local_root).unwrap();
}

#[test]
fn browser_artifact_plan_from_fs_accepts_bindgen_runtime_assets_outside_pkg_island() {
    let root = temp_dir("bindgen-runtime-assets");
    let rust_dir = root.join("rust");
    let pkg_dir = rust_dir.join("pkg-island");
    let web_artifacts_dir = root.join("web-artifacts");
    fs::create_dir_all(&pkg_dir).unwrap();
    fs::create_dir_all(&web_artifacts_dir).unwrap();
    fs::write(
        rust_dir.join("Cargo.toml"),
        r#"[package]
name = "demo"
version = "0.1.0"
edition = "2021"

[features]
wasm-island = []
"#,
    )
    .unwrap();
    fs::write(
        root.join("vo.mod"),
        r#"module github.com/vo-lang/demo
vo 0.1.0

[extension]
name = "demo"

[extension.wasm]
type = "bindgen"
wasm = "demo_island_bg.wasm"
js_glue = "demo_island.js"
local_wasm = "web-artifacts/demo_island_bg.wasm"
local_js_glue = "web-artifacts/demo_island.js"

[extension.web]
capabilities = ["widget", "island_transport"]

[extension.web.js]
renderer = "js/dist/demo-renderer.js"
"#,
    )
    .unwrap();

    let runtime = browser_runtime_plan_from_manifest(
        &root.to_string_lossy(),
        Some("github.com/vo-lang/demo"),
        &parse_manifest(
            r#"
[extension]
name = "demo"

[extension.wasm]
type = "bindgen"
wasm = "demo_island_bg.wasm"
js_glue = "demo_island.js"
local_wasm = "web-artifacts/demo_island_bg.wasm"
local_js_glue = "web-artifacts/demo_island.js"

[extension.web]
capabilities = ["widget", "island_transport"]

[extension.web.js]
renderer = "js/dist/demo-renderer.js"
"#,
        ),
    );
    let intent = runtime.artifact_intent().unwrap();
    let plan = browser_artifact_plan_from_fs(&intent, &runtime).unwrap();

    assert_eq!(plan.actions.len(), 1);
    let ArtifactActionSpec::EnsurePkgIsland(action) = &plan.actions[0] else {
        panic!("expected bindgen island action");
    };
    assert_eq!(
        action.runtime_wasm_path,
        web_artifacts_dir.join("demo_island_bg.wasm")
    );
    assert_eq!(
        action.runtime_js_path.as_ref(),
        Some(&web_artifacts_dir.join("demo_island.js"))
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn materialize_browser_snapshot_from_fs_projects_virtual_wasm_paths() {
    let root = temp_dir("snapshot-fs");
    let js_dir = root.join("js").join("dist");
    let wasm_dir = root.join("rust").join("pkg-island");
    fs::create_dir_all(&js_dir).unwrap();
    fs::create_dir_all(&wasm_dir).unwrap();
    let entry_path = root.join("main.vo");
    fs::write(&entry_path, "main").unwrap();
    fs::write(
        js_dir.join("voplay-renderer.js"),
        "export const render = 1;",
    )
    .unwrap();
    fs::write(wasm_dir.join("voplay_island.js"), "export default 1;").unwrap();
    fs::write(wasm_dir.join("voplay_island_bg.wasm"), [0_u8, 97, 115, 109]).unwrap();

    let runtime = browser_runtime_plan_from_manifest(
        &root.to_string_lossy(),
        Some("github.com/vo-lang/voplay"),
        &parse_manifest(
            r#"
[extension]
name = "voplay"

[extension.wasm]
type = "bindgen"
wasm = "voplay_island_bg.wasm"
js_glue = "voplay_island.js"
local_wasm = "rust/pkg-island/voplay_island_bg.wasm"
local_js_glue = "rust/pkg-island/voplay_island.js"

[extension.web]
capabilities = ["widget", "browser_runtime"]

[extension.web.js]
renderer = "js/dist/voplay-renderer.js"
"#,
        ),
    );
    let snapshot = runtime
        .snapshot_plan(crate::browser_runtime::BrowserSnapshotRoot::ProjectRoot)
        .unwrap();
    let files = materialize_browser_snapshot_from_fs(&snapshot, &runtime, Some(&root), &entry_path)
        .unwrap();
    let paths = files.into_iter().map(|file| file.path).collect::<Vec<_>>();

    assert!(paths.contains(&entry_path.to_string_lossy().to_string()));
    assert!(paths.contains(
        &js_dir
            .join("voplay-renderer.js")
            .to_string_lossy()
            .to_string()
    ));
    assert!(paths.contains(&"wasm/voplay_island.js".to_string()));
    assert!(paths.contains(&"wasm/voplay_island_bg.wasm".to_string()));

    fs::remove_dir_all(&root).unwrap();
}
