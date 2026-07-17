use super::*;
use std::time::{SystemTime, UNIX_EPOCH};

use vo_module::digest::Digest;
use vo_module::ext_manifest::parse_ext_manifest_content;
use vo_module::identity::{ArtifactId, ModulePath};
use vo_module::readiness::{ReadyModule, ResolvedArtifact};
use vo_module::schema::lockfile::LockedModule;
use vo_module::schema::manifest::{
    ManifestArtifact, ManifestPackage, ManifestSource, ReleaseManifest,
};
use vo_module::schema::{PackageManifest, SourceFileEntry};
use vo_module::version::{ExactVersion, ToolchainConstraint};

use crate::browser_runtime::MAX_BROWSER_SNAPSHOT_FILE_BYTES;

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

fn write_wasm_candidate(dir: &Path, package: &str, feature: &str) {
    fs::create_dir_all(dir).unwrap();
    fs::write(
        dir.join("Cargo.toml"),
        format!(
            "[package]\nname = {package:?}\nversion = \"0.1.0\"\n\n[features]\n{feature} = []\n"
        ),
    )
    .unwrap();
}

fn parse_manifest(content: &str) -> vo_module::ext_manifest::ExtensionManifest {
    parse_ext_manifest_content(
        &format!("module = \"github.com/acme/test-runtime\"\nvo = \"^0.1.0\"\n\n{content}"),
        Path::new("/tmp/vo.mod"),
    )
    .unwrap()
}

fn resolved_artifact(kind: &str, name: &str) -> ResolvedArtifact {
    ResolvedArtifact::try_new(
        ArtifactId {
            kind: kind.to_string(),
            target: BROWSER_WASM_TARGET.to_string(),
            name: name.to_string(),
        },
        1,
        Digest::parse("sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
            .unwrap(),
    )
    .unwrap()
}

fn manifest_artifact(kind: &str, target: &str, name: &str, bytes: &[u8]) -> ManifestArtifact {
    ManifestArtifact {
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
    artifacts: Vec<ManifestArtifact>,
    ext_manifest_content: Option<&str>,
    files: &[(&str, &[u8])],
) -> (LockedModule, String, Vec<u8>, String) {
    let source_bytes = b"source-archive";
    let module_path = ModulePath::parse(module).unwrap();
    let toolchain = ToolchainConstraint::parse("^0.1.0").unwrap();
    let mut mod_content = format!(
        "module = {:?}\nvo = {:?}\n",
        module_path.as_str(),
        toolchain.to_string(),
    );
    if let Some(ext_manifest_content) = ext_manifest_content {
        mod_content.push('\n');
        mod_content.push_str(ext_manifest_content);
    }
    let mut source_files = vec![("vo.mod".to_string(), mod_content.as_bytes().to_vec())];
    source_files.extend(
        files
            .iter()
            .filter(|(path, _)| !path.starts_with("artifacts/"))
            .map(|(path, bytes)| ((*path).to_string(), (*bytes).to_vec())),
    );
    source_files.sort_by(|left, right| left.0.cmp(&right.0));
    let source_entries = source_files
        .iter()
        .map(|(path, bytes)| SourceFileEntry {
            path: path.clone(),
            mode: vo_module::schema::SourceFileMode::Regular,
            size: bytes.len() as u64,
            digest: Digest::from_sha256(bytes),
        })
        .collect::<Vec<_>>();
    let package_content = PackageManifest {
        schema_version: 1,
        files: source_entries,
    }
    .render()
    .unwrap();
    let version = ExactVersion::parse(version).unwrap();
    let manifest = ReleaseManifest {
        schema_version: 2,
        module: module_path.clone(),
        version: version.clone(),
        commit: "1111111111111111111111111111111111111111".to_string(),
        vo: toolchain.clone(),
        dependencies: Vec::new(),
        source: ManifestSource {
            name: "source.tar.gz".to_string(),
            size: source_bytes.len() as u64,
            digest: Digest::from_sha256(source_bytes),
        },
        package: ManifestPackage {
            size: package_content.len() as u64,
            digest: Digest::from_sha256(&package_content),
        },
        artifacts,
    };
    let release_manifest_content = manifest.render().unwrap();
    let locked = LockedModule {
        path: module_path,
        version,
        vo: toolchain,
        release: Digest::from_sha256(release_manifest_content.as_bytes()),
        dependencies: Vec::new(),
    };
    (
        locked,
        release_manifest_content,
        package_content,
        mod_content,
    )
}

fn populate_cached_module(
    cache_root: &Path,
    locked: &LockedModule,
    release_manifest_content: &str,
    package_content: &[u8],
    mod_content: &str,
    files: &[(&str, &[u8])],
) -> PathBuf {
    let release = ReleaseManifest::parse(release_manifest_content).unwrap();
    let module_dir = vo_module::cache::layout::cache_dir(cache_root, &locked.path, &locked.version);
    fs::create_dir_all(&module_dir).unwrap();
    fs::write(module_dir.join("vo.mod"), mod_content).unwrap();
    fs::write(
        module_dir.join(".vo-version"),
        format!("{}\n", locked.version),
    )
    .unwrap();
    fs::write(
        module_dir.join(".vo-source-digest"),
        format!("{}\n", release.source.digest),
    )
    .unwrap();
    fs::write(
        module_dir.join("vo.release.json"),
        release_manifest_content.as_bytes(),
    )
    .unwrap();
    fs::write(module_dir.join("vo.package.json"), package_content).unwrap();
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
kind = "bindgen"
wasm = "voplay_island_bg.wasm"
js = "voplay_island.js"

[build.wasm]
wasm = "rust/pkg-island/voplay_island_bg.wasm"
js = "rust/pkg-island/voplay_island.js"

[extension.web]
capabilities = ["widget", "browser_runtime"]

[extension.web.js]
renderer = "js/dist/voplay-renderer.js"
"#,
        ),
    )
    .unwrap();
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

    let oversized_intent = BrowserArtifactIntent {
        required_artifacts: vec![
            intent.required_artifacts[0].clone();
            MAX_BROWSER_RUNTIME_ITEMS + 1
        ],
    };
    let error = browser_artifact_plan_from_fs(&oversized_intent, &runtime)
        .expect_err("public artifact intents must be bounded before planning");
    assert!(error.contains("required artifacts"), "{error}");

    let oversized_plan = BrowserArtifactPlan {
        actions: vec![plan.actions[0].clone(); MAX_BROWSER_RUNTIME_ITEMS + 1],
    };
    let error = execute_browser_artifact_plan(&oversized_plan)
        .expect_err("public artifact plans must be bounded before execution");
    assert!(error.contains("actions"), "{error}");

    let duplicate_output_plan = BrowserArtifactPlan {
        actions: vec![plan.actions[0].clone(), plan.actions[0].clone()],
    };
    let error = execute_browser_artifact_plan(&duplicate_output_plan)
        .expect_err("artifact output collisions must fail before starting a build");
    assert!(error.contains("reuse output path"), "{error}");

    let mut aliased_action = plan.actions[0].clone();
    let ArtifactActionSpec::EnsurePkgIsland(action) = &mut aliased_action else {
        unreachable!("fixture must remain a bindgen island action");
    };
    action.runtime_wasm_path = root
        .join("rust")
        .join("pkg-island")
        .join("..")
        .join("pkg-island")
        .join("voplay_island_bg.wasm");
    action.runtime_js_path = Some(root.join("independent.js"));
    let aliased_output_plan = BrowserArtifactPlan {
        actions: vec![plan.actions[0].clone(), aliased_action],
    };
    let error = execute_browser_artifact_plan(&aliased_output_plan)
        .expect_err("lexical aliases of one artifact output must collide");
    assert!(error.contains("reuse output path"), "{error}");

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn native_browser_runtime_rejects_excessive_manifest_inputs_before_cache_io() {
    let manifests = vec![PathBuf::from("missing/vo.mod"); MAX_BROWSER_RUNTIME_ITEMS + 1];
    let error =
        native_gui_browser_runtime_plan_from_fs(&manifests, &[], Path::new("missing-module-cache"))
            .expect_err("native manifest inputs must be bounded before filesystem access");
    assert!(error.contains("local extension manifests"), "{error}");
}

#[test]
fn native_browser_runtime_requires_the_exact_existing_vo_mod_path() {
    let root = temp_dir("native-manifest-identity");
    fs::write(
        root.join("vo.mod"),
        "module = \"github.com/acme/example\"\nvo = \"^0.1.0\"\n",
    )
    .unwrap();
    fs::write(root.join("alternate.toml"), "ignored = true\n").unwrap();

    let error = native_gui_browser_runtime_plan_from_fs(
        &[root.join("alternate.toml")],
        &[],
        Path::new("unused-module-cache"),
    )
    .expect_err("an arbitrary file in a module directory must not identify its manifest");
    assert!(error.contains("must identify"), "{error}");

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn browser_artifact_plan_from_fs_ignores_ready_module_artifacts() {
    let ready = ReadyModule::try_new(
        ModulePath::parse("github.com/vo-lang/vogui").unwrap(),
        ExactVersion::parse("0.1.4").unwrap(),
        BROWSER_WASM_TARGET,
        vec![resolved_artifact("extension-wasm", "vogui.wasm")],
        Some(parse_manifest(
            r#"
[extension]
name = "vogui"

[extension.wasm]
kind = "standalone"
wasm = "vogui.wasm"

[extension.web]
capabilities = ["widget"]

[extension.web.js]
renderer = "js/dist/studio_renderer.js"
"#,
        )),
    )
    .unwrap();

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
    let ext_manifest_content = r#"
[extension]
name = "demo"

[extension.wasm]
kind = "bindgen"
wasm = "demo_bg.wasm"
js = "demo.js"

[extension.web]
entry = "Run"
capabilities = ["widget", "browser_runtime"]

[extension.web.js]
renderer = "js/dist/demo-renderer.js"
"#;
    let files: [(&str, &[u8]); 3] = [
        (
            "artifacts/extension-wasm/wasm32-unknown-unknown/demo_bg.wasm",
            wasm_bytes,
        ),
        (
            "artifacts/extension-js-glue/wasm32-unknown-unknown/demo.js",
            js_glue_bytes,
        ),
        ("js/dist/demo-renderer.js", renderer_bytes),
    ];
    let (locked, release_manifest_content, package_content, mod_content) = locked_module(
        "github.com/acme/demo",
        "1.2.3",
        vec![
            manifest_artifact(
                "extension-js-glue",
                BROWSER_WASM_TARGET,
                "demo.js",
                js_glue_bytes,
            ),
            manifest_artifact(
                "extension-wasm",
                BROWSER_WASM_TARGET,
                "demo_bg.wasm",
                wasm_bytes,
            ),
        ],
        Some(ext_manifest_content),
        &files,
    );
    let module_dir = populate_cached_module(
        &cache_root,
        &locked,
        &release_manifest_content,
        &package_content,
        &mod_content,
        &files,
    );
    let expected_renderer_path = module_dir
        .join("js")
        .join("dist")
        .join("demo-renderer.js")
        .to_string_lossy()
        .to_string();
    let expected_wasm_path = module_dir
        .join(
            vo_module::artifact::artifact_relative_path(&ArtifactId {
                kind: "extension-wasm".to_string(),
                target: BROWSER_WASM_TARGET.to_string(),
                name: "demo_bg.wasm".to_string(),
            })
            .unwrap(),
        )
        .to_string_lossy()
        .to_string();
    let expected_js_glue_path = module_dir
        .join(
            vo_module::artifact::artifact_relative_path(&ArtifactId {
                kind: "extension-js-glue".to_string(),
                target: BROWSER_WASM_TARGET.to_string(),
                name: "demo.js".to_string(),
            })
            .unwrap(),
        )
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
        r#"module = "github.com/acme/app"
vo = "^0.1.0"

[extension]
name = "app"

[extension.wasm]
kind = "standalone"
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
    let ext_manifest_content = r#"
[extension]
name = "remote"

[extension.wasm]
kind = "standalone"
wasm = "remote.wasm"

[extension.web]
capabilities = ["widget", "protocol"]

[extension.web.js]
protocol = "js/dist/remote-protocol.js"
"#;
    let files: [(&str, &[u8]); 2] = [
        (
            "artifacts/extension-wasm/wasm32-unknown-unknown/remote.wasm",
            remote_wasm_bytes,
        ),
        (
            "js/dist/remote-protocol.js",
            b"export const protocol = 1;\n",
        ),
    ];
    let (locked, release_manifest_content, package_content, mod_content) = locked_module(
        "github.com/acme/remote",
        "1.2.3",
        vec![manifest_artifact(
            "extension-wasm",
            BROWSER_WASM_TARGET,
            "remote.wasm",
            remote_wasm_bytes,
        )],
        Some(ext_manifest_content),
        &files,
    );
    populate_cached_module(
        &cache_root,
        &locked,
        &release_manifest_content,
        &package_content,
        &mod_content,
        &files,
    );

    let local_root = temp_dir("native-gui-runtime-local").canonicalize().unwrap();
    fs::create_dir_all(local_root.join("js").join("dist")).unwrap();
    fs::create_dir_all(local_root.join("web-artifacts")).unwrap();
    fs::write(
        local_root.join("vo.mod"),
        r#"module = "github.com/acme/local"
vo = "^0.1.0"

[extension]
name = "local"

[extension.wasm]
kind = "bindgen"
wasm = "local_bg.wasm"
js = "local.js"

[build.wasm]
wasm = "web-artifacts/local_bg.wasm"
js = "web-artifacts/local.js"

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
    let renderer_dir = root.join("js").join("dist");
    fs::create_dir_all(&pkg_dir).unwrap();
    fs::create_dir_all(&web_artifacts_dir).unwrap();
    fs::create_dir_all(&renderer_dir).unwrap();
    let wasm_bytes = [0_u8, 97, 115, 109];
    let js_bytes = b"export default async function init() {}\n";
    fs::write(web_artifacts_dir.join("demo_island_bg.wasm"), wasm_bytes).unwrap();
    fs::write(web_artifacts_dir.join("demo_island.js"), js_bytes).unwrap();
    fs::write(
        renderer_dir.join("demo-renderer.js"),
        "export const renderer = 1;\n",
    )
    .unwrap();
    let entry_path = root.join("main.vo");
    fs::write(&entry_path, "package main\n").unwrap();
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
        r#"module = "github.com/vo-lang/demo"
vo = "^0.1.0"

[extension]
name = "demo"

[extension.wasm]
kind = "bindgen"
wasm = "demo_island_bg.wasm"
js = "demo_island.js"

[build.wasm]
wasm = "web-artifacts/demo_island_bg.wasm"
js = "web-artifacts/demo_island.js"

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
kind = "bindgen"
wasm = "demo_island_bg.wasm"
js = "demo_island.js"

[build.wasm]
wasm = "web-artifacts/demo_island_bg.wasm"
js = "web-artifacts/demo_island.js"

[extension.web]
capabilities = ["widget", "island_transport"]

[extension.web.js]
renderer = "js/dist/demo-renderer.js"
"#,
        ),
    )
    .unwrap();
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

    let snapshot = runtime
        .snapshot_plan(crate::browser_runtime::BrowserSnapshotRoot::ProjectRoot)
        .unwrap();
    let materialized =
        materialize_browser_snapshot_from_fs(&snapshot, &runtime, Some(&root), &entry_path)
            .unwrap();
    let materialized = materialized
        .into_iter()
        .map(|file| (file.path, file.bytes))
        .collect::<std::collections::HashMap<_, _>>();
    assert_eq!(
        materialized
            .get("wasm/demo_island_bg.wasm")
            .map(Vec::as_slice),
        Some(wasm_bytes.as_slice())
    );
    assert_eq!(
        materialized.get("wasm/demo_island.js").map(Vec::as_slice),
        Some(js_bytes.as_slice())
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
kind = "bindgen"
wasm = "voplay_island_bg.wasm"
js = "voplay_island.js"

[build.wasm]
wasm = "rust/pkg-island/voplay_island_bg.wasm"
js = "rust/pkg-island/voplay_island.js"

[extension.web]
capabilities = ["widget", "browser_runtime"]

[extension.web.js]
renderer = "js/dist/voplay-renderer.js"
"#,
        ),
    )
    .unwrap();
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

#[test]
fn materialize_browser_snapshot_from_fs_handles_single_component_assets() {
    let root = temp_dir("snapshot-single-component");
    let entry_path = root.join("main.vo");
    let renderer_path = root.join("renderer.js");
    let wasm_path = root.join("demo.wasm");
    fs::write(&entry_path, "main").unwrap();
    fs::write(&renderer_path, "export const render = 1;").unwrap();
    fs::write(&wasm_path, [0_u8, 97, 115, 109]).unwrap();

    let runtime = browser_runtime_plan_from_manifest(
        &root.to_string_lossy(),
        Some("github.com/vo-lang/demo"),
        &parse_manifest(
            r#"
[extension]
name = "demo"

[extension.wasm]
kind = "standalone"
wasm = "demo.wasm"

[build.wasm]
wasm = "demo.wasm"

[extension.web]
capabilities = ["widget"]

[extension.web.js]
renderer = "renderer.js"
"#,
        ),
    )
    .unwrap();
    let snapshot = runtime
        .snapshot_plan(crate::browser_runtime::BrowserSnapshotRoot::EntryFile)
        .unwrap();
    let owner = vo_module::resolved_extension::ExtensionOwner::new(
        ModulePath::parse("github.com/vo-lang/demo").unwrap(),
    );

    assert!(snapshot.mounts.contains(&BrowserSnapshotMount {
        source: BrowserSnapshotSourceRef::AssetPath(
            vo_module::resolved_extension::AssetRef::module_root(owner.clone(), "").unwrap(),
        ),
        virtual_prefix: String::new(),
        strip_prefix: None,
        kind: BrowserSnapshotMountKind::Directory,
    }));
    assert!(snapshot.mounts.contains(&BrowserSnapshotMount {
        source: BrowserSnapshotSourceRef::AssetPath(
            vo_module::resolved_extension::AssetRef::module_root(owner, "demo.wasm").unwrap(),
        ),
        virtual_prefix: "wasm".to_string(),
        strip_prefix: Some(String::new()),
        kind: BrowserSnapshotMountKind::File,
    }));

    let files =
        materialize_browser_snapshot_from_fs(&snapshot, &runtime, None, &entry_path).unwrap();
    let paths = files.into_iter().map(|file| file.path).collect::<Vec<_>>();
    assert!(paths.contains(&entry_path.to_string_lossy().to_string()));
    assert!(paths.contains(&renderer_path.to_string_lossy().to_string()));
    assert!(paths.contains(&"wasm/demo.wasm".to_string()));

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn materialize_browser_snapshot_from_fs_rejects_nonportable_public_mounts() {
    let root = temp_dir("snapshot-nonportable-mount");
    let entry_path = root.join("main.vo");
    fs::write(&entry_path, "main").unwrap();
    let snapshot = BrowserSnapshotPlan {
        mounts: vec![BrowserSnapshotMount {
            source: BrowserSnapshotSourceRef::EntryFile,
            virtual_prefix: "alias/../escape".to_string(),
            strip_prefix: None,
            kind: BrowserSnapshotMountKind::File,
        }],
    };

    let error = materialize_browser_snapshot_from_fs(
        &snapshot,
        &BrowserRuntimePlan::default(),
        None,
        &entry_path,
    )
    .expect_err("public snapshot mounts must use normalized portable paths");
    assert!(error.contains("virtual_prefix"), "{error}");

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn native_snapshot_paths_share_one_platform_neutral_vfs_mapping() {
    assert_eq!(
        windows_snapshot_vfs_path(r"C:\workspace\demo\main.vo").unwrap(),
        "/C:/workspace/demo/main.vo"
    );
    assert_eq!(
        windows_snapshot_vfs_path(r"\\server\share\demo\main.vo").unwrap(),
        "/UNC/server/share/demo/main.vo"
    );
    assert_eq!(
        windows_snapshot_vfs_path(r"\\?\C:\workspace\main.vo").unwrap(),
        "/C:/workspace/main.vo"
    );
    assert!(windows_snapshot_vfs_path(r"C:relative\main.vo").is_err());
    assert!(windows_snapshot_vfs_path(r"\\.\device").is_err());
    assert!(windows_snapshot_vfs_path(r"\\?\Volume{abc}\main.vo").is_err());
    assert!(windows_snapshot_vfs_path(r"\rooted\main.vo").is_err());
    assert!(windows_snapshot_vfs_path(r"\\server").is_err());
    assert_eq!(
        windows_snapshot_fs_path("/C:/workspace/demo/main.vo").unwrap(),
        "C:/workspace/demo/main.vo"
    );
    assert_eq!(windows_snapshot_fs_path("/C:").unwrap(), "C:/");
    assert_eq!(
        windows_snapshot_fs_path("/UNC/server/share/demo/main.vo").unwrap(),
        "//server/share/demo/main.vo"
    );
    assert!(windows_snapshot_fs_path("/UNC/server").is_err());
    assert!(windows_snapshot_fs_path("/rooted/main.vo").is_err());

    let root = temp_dir("snapshot-vfs-path");
    let entry = root.join("main.vo");
    let root_path = browser_snapshot_vfs_path_from_fs(&root).unwrap();
    let entry_path = browser_snapshot_vfs_path_from_fs(&entry).unwrap();
    assert!(entry_path.starts_with(&format!("{}/", root_path.trim_end_matches('/'))));
    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn materialize_browser_snapshot_from_fs_is_deterministic() {
    let root = temp_dir("snapshot-deterministic");
    fs::create_dir_all(root.join("nested")).unwrap();
    fs::write(root.join("z.txt"), b"z").unwrap();
    fs::write(root.join("a.txt"), b"a").unwrap();
    fs::write(root.join("nested").join("b.txt"), b"b").unwrap();
    fs::write(root.join("nested").join("a.txt"), b"a").unwrap();
    let entry_path = root.join("a.txt");
    let snapshot = BrowserSnapshotPlan {
        mounts: vec![BrowserSnapshotMount {
            source: BrowserSnapshotSourceRef::ProjectRoot,
            virtual_prefix: String::new(),
            strip_prefix: None,
            kind: BrowserSnapshotMountKind::Directory,
        }],
    };

    let files = materialize_browser_snapshot_from_fs(
        &snapshot,
        &BrowserRuntimePlan::default(),
        Some(&root),
        &entry_path,
    )
    .unwrap();
    let paths = files.into_iter().map(|file| file.path).collect::<Vec<_>>();
    let mut sorted = paths.clone();
    sorted.sort_by(|left, right| left.as_bytes().cmp(right.as_bytes()));
    assert_eq!(paths, sorted);

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn materialize_browser_snapshot_from_fs_rejects_oversized_file_before_reading_it() {
    let root = temp_dir("snapshot-oversized");
    let entry_path = root.join("large.bin");
    let file = fs::File::create(&entry_path).unwrap();
    file.set_len(u64::try_from(MAX_BROWSER_SNAPSHOT_FILE_BYTES).unwrap() + 1)
        .unwrap();
    let snapshot = BrowserSnapshotPlan {
        mounts: vec![BrowserSnapshotMount {
            source: BrowserSnapshotSourceRef::EntryFile,
            virtual_prefix: String::new(),
            strip_prefix: None,
            kind: BrowserSnapshotMountKind::File,
        }],
    };

    let error = materialize_browser_snapshot_from_fs(
        &snapshot,
        &BrowserRuntimePlan::default(),
        None,
        &entry_path,
    )
    .expect_err("oversized browser snapshot input must fail");
    assert!(error.contains(&MAX_BROWSER_SNAPSHOT_FILE_BYTES.to_string()));

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn generated_output_publication_replaces_the_destination_without_temp_leaks() {
    let root = temp_dir("generated-output-atomic");
    let source = root.join("built.wasm");
    let destination = root.join("runtime.wasm");
    fs::write(&source, b"new browser artifact").unwrap();
    fs::write(&destination, b"old browser artifact").unwrap();

    copy_generated_output_atomically(&source, &destination, "test artifact").unwrap();

    assert_eq!(fs::read(&destination).unwrap(), b"new browser artifact");
    assert!(fs::read_dir(&root).unwrap().all(|entry| {
        !entry
            .unwrap()
            .file_name()
            .to_string_lossy()
            .starts_with(".vo-browser-output.")
    }));
    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn generated_output_parent_uses_current_directory_for_a_bare_filename() {
    assert_eq!(
        generated_output_parent(Path::new("runtime.wasm")),
        Path::new(".")
    );
}

#[test]
fn generated_output_sync_repairs_same_length_destination_corruption() {
    let root = temp_dir("generated-output-repair");
    let source = root.join("built.wasm");
    let destination = root.join("runtime.wasm");
    fs::write(&source, b"correct").unwrap();
    fs::write(&destination, b"damaged").unwrap();

    sync_generated_output(&source, &destination, "test artifact").unwrap();

    assert_eq!(fs::read(&destination).unwrap(), b"correct");
    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn generated_output_publication_rejects_oversized_sources_before_replacement() {
    let root = temp_dir("generated-output-oversized");
    let source = root.join("built.wasm");
    let destination = root.join("runtime.wasm");
    let source_file = fs::File::create(&source).unwrap();
    source_file
        .set_len(u64::try_from(MAX_BROWSER_SNAPSHOT_FILE_BYTES).unwrap() + 1)
        .unwrap();
    fs::write(&destination, b"stable browser artifact").unwrap();

    let error = copy_generated_output_atomically(&source, &destination, "test artifact")
        .expect_err("oversized generated artifacts must be rejected");

    assert!(error.contains(&MAX_BROWSER_SNAPSHOT_FILE_BYTES.to_string()));
    assert_eq!(fs::read(&destination).unwrap(), b"stable browser artifact");
    assert!(fs::read_dir(&root).unwrap().all(|entry| {
        !entry
            .unwrap()
            .file_name()
            .to_string_lossy()
            .starts_with(".vo-browser-output.")
    }));
    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn select_wasm_build_candidate_rejects_duplicate_exact_package_names() {
    let root = temp_dir("wasm-candidate-duplicate");
    write_wasm_candidate(&root.join("z-crate"), "demo", "wasm-standalone");
    write_wasm_candidate(&root.join("a-crate"), "demo", "wasm-standalone");

    let error = select_wasm_build_candidate(&root, "demo", "wasm-standalone")
        .expect_err("duplicate exact Cargo package names must be ambiguous");
    assert!(error.contains("multiple browser wasm crates"));

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn select_wasm_build_candidate_reports_ambiguous_candidates_in_utf8_order() {
    let root = temp_dir("wasm-candidate-order");
    let z = root.join("z-crate");
    let a = root.join("a-crate");
    write_wasm_candidate(&z, "z-package", "wasm-island");
    write_wasm_candidate(&a, "a-package", "wasm-island");

    let error = select_wasm_build_candidate(&root, "missing", "wasm-island")
        .expect_err("multiple nonmatching Cargo packages must be ambiguous");
    let a_index = error.find(&a.display().to_string()).unwrap();
    let z_index = error.find(&z.display().to_string()).unwrap();
    assert!(a_index < z_index, "{error}");

    fs::remove_dir_all(&root).unwrap();
}
