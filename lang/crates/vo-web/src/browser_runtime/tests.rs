use super::*;
use std::path::Path;

use vo_module::digest::Digest;
use vo_module::ext_manifest::parse_ext_manifest_content;
use vo_module::identity::{ArtifactId, ModulePath};
use vo_module::readiness::{ReadyModule, ResolvedArtifact};
use vo_module::version::ExactVersion;

fn parse_manifest(content: &str) -> ExtensionManifest {
    parse_ext_manifest_content(content, Path::new("/tmp/vo.mod")).unwrap()
}

fn resolved_artifact(kind: &str, name: &str) -> ResolvedArtifact {
    ResolvedArtifact {
        id: ArtifactId {
            kind: kind.to_string(),
            target: "wasm32-unknown-unknown".to_string(),
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

#[test]
fn browser_runtime_module_from_manifest_resolves_relative_paths() {
    let manifest = parse_manifest(
        r#"
[extension]
name = "vogui"

[extension.wasm]
type = "standalone"
wasm = "vogui.wasm"

[extension.web]
entry = "Run"
capabilities = ["widget", "render_surface"]

[extension.web.js]
renderer = "js/dist/studio_renderer.js"
protocol = "js/dist/studio_protocol.js"
host_bridge = "js/dist/studio_host_bridge.js"
"#,
    );

    let module = browser_runtime_module_from_manifest(
        "/github.com@vo-lang@vogui/v0.1.4",
        Some("github.com/vo-lang/vogui"),
        &manifest,
    )
    .unwrap();

    assert_eq!(module.module_key, "github.com/vo-lang/vogui");
    assert_eq!(module.contract.name, "vogui");
    assert_eq!(module.contract.entry.as_deref(), Some("Run"));
    assert_eq!(
        module.contract.js_module_path("renderer"),
        Some("/github.com@vo-lang@vogui/v0.1.4/js/dist/studio_renderer.js")
    );
    assert_eq!(
        module.contract.js_module_path("protocol"),
        Some("/github.com@vo-lang@vogui/v0.1.4/js/dist/studio_protocol.js")
    );
    assert_eq!(
        module.contract.js_module_path("host_bridge"),
        Some("/github.com@vo-lang@vogui/v0.1.4/js/dist/studio_host_bridge.js")
    );
}

#[test]
fn browser_runtime_module_from_manifest_returns_none_without_web_runtime() {
    let manifest = parse_manifest(
        r#"
[extension]
name = "zip"

[extension.wasm]
type = "standalone"
wasm = "zip.wasm"
"#,
    );

    assert!(browser_runtime_module_from_manifest("/zip", None, &manifest).is_none());
}

#[test]
fn browser_wasm_extension_from_manifest_prefers_local_paths() {
    let manifest = parse_manifest(
        r#"
[extension]
name = "voplay"

[extension.wasm]
type = "bindgen"
wasm = "voplay_island_bg.wasm"
js_glue = "voplay_island.js"
local_wasm = "rust/pkg-island/voplay_island_bg.wasm"
local_js_glue = "rust/pkg-island/voplay_island.js"
"#,
    );

    let spec = browser_wasm_extension_from_manifest(
        "/github.com@vo-lang@voplay/local",
        Some("github.com/vo-lang/voplay"),
        &manifest,
    )
    .unwrap();

    assert_eq!(spec.name, "voplay");
    assert_eq!(spec.module_key, "github.com/vo-lang/voplay");
    assert_eq!(
        spec.wasm_path,
        "/github.com@vo-lang@voplay/local/rust/pkg-island/voplay_island_bg.wasm"
    );
    assert_eq!(
        spec.js_glue_path.as_deref(),
        Some("/github.com@vo-lang@voplay/local/rust/pkg-island/voplay_island.js")
    );
}

#[test]
fn browser_runtime_plan_from_manifest_collects_runtime_and_wasm() {
    let manifest = parse_manifest(
        r#"
[extension]
name = "vogui"

[extension.wasm]
type = "standalone"
wasm = "vogui.wasm"

[extension.web]
entry = "Run"
capabilities = ["widget"]

[extension.web.js]
renderer = "js/dist/studio_renderer.js"
"#,
    );

    let plan = browser_runtime_plan_from_manifest(
        "/github.com@vo-lang@vogui/v0.1.4",
        Some("github.com/vo-lang/vogui"),
        &manifest,
    );

    assert_eq!(plan.runtime_modules.len(), 1);
    assert_eq!(plan.graph.frameworks.len(), 1);
    assert_eq!(
        plan.graph.roles.entry_framework.as_ref().unwrap(),
        &BrowserFrameworkId::new("github.com/vo-lang/vogui", "vogui")
    );
    assert_eq!(
        plan.graph.roles.providers_for("renderer"),
        [BrowserFrameworkId::new("github.com/vo-lang/vogui", "vogui")]
    );
    assert_eq!(plan.wasm_bindings.len(), 1);
    assert_eq!(plan.wasm_extensions.len(), 1);
    assert_eq!(
        plan.runtime_modules[0].module_key,
        "github.com/vo-lang/vogui"
    );
    assert_eq!(
        plan.wasm_extensions[0].module_key,
        "github.com/vo-lang/vogui"
    );
}

#[test]
fn browser_artifact_intent_projects_canonical_local_and_published_assets() {
    let manifest = parse_manifest(
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
    );

    let plan = browser_runtime_plan_from_manifest(
        "/github.com@vo-lang@voplay/local",
        Some("github.com/vo-lang/voplay"),
        &manifest,
    );
    let intent = plan.artifact_intent().unwrap();
    let owner = ExtensionOwner::new(ModulePath::parse("github.com/vo-lang/voplay").unwrap());

    assert_eq!(intent.required_artifacts.len(), 1);
    assert_eq!(intent.required_artifacts[0].owner, owner);
    assert_eq!(
        intent.required_artifacts[0].module_key,
        "github.com/vo-lang/voplay"
    );
    assert_eq!(intent.required_artifacts[0].extension_name, "voplay");
    assert_eq!(
        intent.required_artifacts[0].source,
        BrowserArtifactSource::LocalManifest
    );
    assert_eq!(
        intent.required_artifacts[0].family,
        BrowserArtifactFamily::BindgenIsland
    );
    assert_eq!(intent.required_artifacts[0].runtime_roles, vec!["renderer"]);
    assert_eq!(
        intent.required_artifacts[0].wasm.runtime_asset,
        AssetRef::module_root(
            ExtensionOwner::new(ModulePath::parse("github.com/vo-lang/voplay").unwrap()),
            "rust/pkg-island/voplay_island_bg.wasm",
        )
    );
    assert_eq!(
        intent.required_artifacts[0].wasm.published_asset,
        AssetRef::artifact_root(
            ExtensionOwner::new(ModulePath::parse("github.com/vo-lang/voplay").unwrap()),
            "voplay_island_bg.wasm",
        )
    );
    assert_eq!(
        intent.required_artifacts[0].wasm.local_asset,
        Some(AssetRef::module_root(
            ExtensionOwner::new(ModulePath::parse("github.com/vo-lang/voplay").unwrap()),
            "rust/pkg-island/voplay_island_bg.wasm",
        ))
    );
    assert_eq!(
        intent.required_artifacts[0]
            .js_glue
            .as_ref()
            .unwrap()
            .runtime_asset,
        AssetRef::module_root(
            ExtensionOwner::new(ModulePath::parse("github.com/vo-lang/voplay").unwrap()),
            "rust/pkg-island/voplay_island.js",
        )
    );
    assert_eq!(
        intent.required_artifacts[0]
            .js_glue
            .as_ref()
            .unwrap()
            .published_asset,
        AssetRef::artifact_root(
            ExtensionOwner::new(ModulePath::parse("github.com/vo-lang/voplay").unwrap()),
            "voplay_island.js",
        )
    );
}

#[test]
fn browser_snapshot_plan_projects_mounts_from_canonical_bindings() {
    let manifest = parse_manifest(
        r#"
[extension]
name = "vogui"

[extension.wasm]
type = "standalone"
wasm = "vogui.wasm"

[extension.web]
entry = "Run"
capabilities = ["widget"]

[extension.web.js]
renderer = "js/dist/studio_renderer.js"
protocol = "js/dist/studio_protocol.js"
"#,
    );

    let plan = browser_runtime_plan_from_manifest(
        "/github.com@vo-lang@vogui/v0.1.4",
        Some("github.com/vo-lang/vogui"),
        &manifest,
    );
    let snapshot = plan
        .snapshot_plan(BrowserSnapshotRoot::ProjectRoot)
        .unwrap();
    let owner = ExtensionOwner::new(ModulePath::parse("github.com/vo-lang/vogui").unwrap());

    assert!(snapshot.mounts.contains(&BrowserSnapshotMount {
        source: BrowserSnapshotSourceRef::ProjectRoot,
        virtual_prefix: String::new(),
        strip_prefix: None,
        kind: BrowserSnapshotMountKind::Directory,
    }));
    assert!(snapshot.mounts.contains(&BrowserSnapshotMount {
        source: BrowserSnapshotSourceRef::AssetPath(AssetRef::module_root(
            owner.clone(),
            "js/dist",
        )),
        virtual_prefix: String::new(),
        strip_prefix: None,
        kind: BrowserSnapshotMountKind::Directory,
    }));
    assert!(snapshot.mounts.contains(&BrowserSnapshotMount {
        source: BrowserSnapshotSourceRef::AssetPath(AssetRef::artifact_root(owner, "vogui.wasm",)),
        virtual_prefix: "wasm".to_string(),
        strip_prefix: Some(String::new()),
        kind: BrowserSnapshotMountKind::File,
    }));
}

#[test]
fn browser_runtime_view_and_primary_split_project_from_graph() {
    let manifest = parse_manifest(
        r#"
[extension]
name = "voplay"

[extension.wasm]
type = "standalone"
wasm = "voplay.wasm"

[extension.web]
entry = "Run"
capabilities = ["widget", "browser_runtime"]

[extension.web.js]
renderer = "js/dist/voplay-renderer.js"
protocol = "js/dist/voplay-protocol.js"
"#,
    );

    let graph = browser_runtime_graph_from_manifest(
        "/github.com@vo-lang@voplay/v0.1.11",
        Some("github.com/vo-lang/voplay"),
        &manifest,
    );
    let view = browser_runtime_view_from_graph(&graph);
    let split = split_primary_provider_view(&view);

    assert_eq!(view.frameworks.len(), 1);
    assert_eq!(
        view.frameworks[0].id,
        BrowserFrameworkId::new("github.com/vo-lang/voplay", "voplay")
    );
    assert_eq!(
        view.roles.providers_for("protocol"),
        [BrowserFrameworkId::new(
            "github.com/vo-lang/voplay",
            "voplay"
        )]
    );
    assert_eq!(split.primary_framework.as_ref().unwrap().name, "voplay");
    assert!(split.provider_frameworks.is_empty());
}

#[test]
fn ready_browser_runtime_module_uses_ready_module_metadata() {
    let manifest = parse_manifest(
        r#"
[extension]
name = "voplay"

[extension.wasm]
type = "bindgen"
wasm = "voplay_island_bg.wasm"
js_glue = "voplay_island.js"

[extension.web]
capabilities = ["widget", "island_transport", "browser_runtime", "vfs"]

[extension.web.js]
renderer = "js/dist/voplay-render-island.js"
"#,
    );
    let ready = ReadyModule {
        module: ModulePath::parse("github.com/vo-lang/voplay").unwrap(),
        version: ExactVersion::parse("v0.1.11").unwrap(),
        module_dir: Path::new("github.com@vo-lang@voplay/v0.1.11").to_path_buf(),
        artifacts: vec![
            resolved_artifact("extension-wasm", "voplay_island_bg.wasm"),
            resolved_artifact("extension-js-glue", "voplay_island.js"),
        ],
        ext_manifest: Some(manifest),
    };

    let runtime = ready_browser_runtime_module(&ready).unwrap();

    assert_eq!(runtime.module_key, "github.com/vo-lang/voplay");
    assert_eq!(runtime.module_root, "/github.com@vo-lang@voplay/v0.1.11");
    assert_eq!(runtime.contract.name, "voplay");
    assert!(runtime.contract.entry.is_none());
    assert_eq!(
        runtime.contract.capabilities,
        vec!["widget", "island_transport", "browser_runtime", "vfs"]
    );
    assert_eq!(
        runtime.contract.js_module_path("renderer"),
        Some("/github.com@vo-lang@voplay/v0.1.11/js/dist/voplay-render-island.js")
    );
}

#[test]
fn ready_browser_wasm_extension_uses_ready_module_artifacts() {
    let ready = ReadyModule {
        module: ModulePath::parse("github.com/vo-lang/voplay").unwrap(),
        version: ExactVersion::parse("v0.1.11").unwrap(),
        module_dir: Path::new("github.com@vo-lang@voplay/v0.1.11").to_path_buf(),
        artifacts: vec![
            resolved_artifact("extension-wasm", "voplay_island_bg.wasm"),
            resolved_artifact("extension-js-glue", "voplay_island.js"),
        ],
        ext_manifest: Some(parse_manifest(
            r#"
[extension]
name = "voplay"

[extension.wasm]
type = "bindgen"
wasm = "voplay_island_bg.wasm"
js_glue = "voplay_island.js"
"#,
        )),
    };

    let spec = ready_browser_wasm_extension(&ready).unwrap().unwrap();

    assert_eq!(spec.name, "voplay");
    assert_eq!(spec.module_key, "github.com/vo-lang/voplay");
    assert_eq!(spec.module_root, "/github.com@vo-lang@voplay/v0.1.11");
    assert_eq!(
        spec.wasm_path,
        "/github.com@vo-lang@voplay/v0.1.11/artifacts/voplay_island_bg.wasm"
    );
    assert_eq!(
        spec.js_glue_path.as_deref(),
        Some("/github.com@vo-lang@voplay/v0.1.11/artifacts/voplay_island.js")
    );
}

#[test]
fn ready_browser_wasm_extension_rejects_js_glue_without_wasm() {
    let ready = ReadyModule {
        module: ModulePath::parse("github.com/acme/demo").unwrap(),
        version: ExactVersion::parse("v1.2.3").unwrap(),
        module_dir: Path::new("github.com@acme@demo/v1.2.3").to_path_buf(),
        artifacts: vec![resolved_artifact("extension-js-glue", "demo.js")],
        ext_manifest: None,
    };

    let error = ready_browser_wasm_extension(&ready).unwrap_err();

    assert!(error.contains("missing extension-wasm"));
}

#[test]
fn plan_ready_browser_runtime_collects_modules_and_wasm_extensions() {
    let manifest = parse_manifest(
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
    );
    let ready = ReadyModule {
        module: ModulePath::parse("github.com/vo-lang/vogui").unwrap(),
        version: ExactVersion::parse("v0.1.4").unwrap(),
        module_dir: Path::new("github.com@vo-lang@vogui/v0.1.4").to_path_buf(),
        artifacts: vec![resolved_artifact("extension-wasm", "vogui.wasm")],
        ext_manifest: Some(manifest),
    };

    let plan = plan_ready_browser_runtime(&[ready]).unwrap();

    assert_eq!(plan.runtime_modules.len(), 1);
    assert_eq!(plan.graph.frameworks.len(), 1);
    assert_eq!(plan.wasm_bindings.len(), 1);
    assert_eq!(plan.wasm_extensions.len(), 1);
    assert_eq!(
        plan.runtime_modules[0].contract.js_module_path("renderer"),
        Some("/github.com@vo-lang@vogui/v0.1.4/js/dist/studio_renderer.js")
    );
    assert_eq!(
        plan.wasm_extensions[0].wasm_path,
        "/github.com@vo-lang@vogui/v0.1.4/artifacts/vogui.wasm"
    );
}

#[test]
fn plan_ready_browser_runtime_at_prefixes_host_module_root_base() {
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

    let plan = plan_ready_browser_runtime_at(&[ready], "/mod-cache").unwrap();

    assert_eq!(
        plan.runtime_modules[0].module_root,
        "/mod-cache/github.com@vo-lang@vogui/v0.1.4"
    );
    assert_eq!(
        plan.wasm_bindings[0].source,
        BrowserArtifactSource::ReadyModule
    );
    assert_eq!(
        plan.wasm_extensions[0].module_key,
        "github.com/vo-lang/vogui"
    );
    assert_eq!(
        plan.wasm_extensions[0].wasm_path,
        "/mod-cache/github.com@vo-lang@vogui/v0.1.4/artifacts/vogui.wasm"
    );
}
