use super::*;
use std::path::Path;

use vo_module::digest::Digest;
use vo_module::ext_manifest::parse_ext_manifest_content;
use vo_module::identity::{ArtifactId, ModulePath};
use vo_module::readiness::{ReadyModule, ResolvedArtifact};
use vo_module::version::ExactVersion;

const TEST_WASM_TARGET: &str = "wasm32-unknown-unknown";

fn parse_manifest(content: &str) -> ExtensionManifest {
    parse_ext_manifest_content(
        &format!("format = 1\nmodule = \"github.com/acme/test-runtime\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n{content}"),
        Path::new("/tmp/vo.mod"),
    )
    .unwrap()
}

fn resolved_artifact(kind: &str, name: &str) -> ResolvedArtifact {
    ResolvedArtifact::try_new(
        ArtifactId {
            kind: kind.to_string(),
            target: TEST_WASM_TARGET.to_string(),
            name: name.to_string(),
        },
        1,
        Digest::parse("sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
            .unwrap(),
    )
    .unwrap()
}

fn ready_module(
    module: &str,
    version: &str,
    artifacts: Vec<ResolvedArtifact>,
    manifest: Option<ExtensionManifest>,
) -> ReadyModule {
    ReadyModule::try_new(
        ModulePath::parse(module).unwrap(),
        ExactVersion::parse(version).unwrap(),
        TEST_WASM_TARGET,
        artifacts,
        manifest,
    )
    .unwrap()
}

#[test]
fn browser_snapshot_budget_rejects_every_public_limit_boundary() {
    assert!(BrowserSnapshotBudget::new(MAX_BROWSER_SNAPSHOT_MOUNTS).is_ok());
    assert!(BrowserSnapshotBudget::new(MAX_BROWSER_SNAPSHOT_MOUNTS + 1).is_err());

    let mut entries = BrowserSnapshotBudget {
        entries: MAX_BROWSER_SNAPSHOT_ENTRIES,
        ..BrowserSnapshotBudget::default()
    };
    assert!(entries.record_entry("overflow").is_err());

    let files = BrowserSnapshotBudget {
        files: MAX_BROWSER_SNAPSHOT_FILES,
        ..BrowserSnapshotBudget::default()
    };
    assert!(files.next_file_limit("overflow").is_err());

    let mut per_file = BrowserSnapshotBudget::default();
    assert!(per_file
        .record_file("oversized", MAX_BROWSER_SNAPSHOT_FILE_BYTES + 1)
        .is_err());

    let mut aggregate = BrowserSnapshotBudget {
        bytes: MAX_BROWSER_SNAPSHOT_BYTES,
        ..BrowserSnapshotBudget::default()
    };
    assert_eq!(aggregate.next_file_limit("empty"), Ok(0));
    assert!(aggregate.record_file("overflow", 1).is_err());

    let depth = BrowserSnapshotBudget::default();
    assert!(depth
        .validate_depth(MAX_BROWSER_SNAPSHOT_DEPTH, "boundary")
        .is_ok());
    assert!(depth
        .validate_depth(MAX_BROWSER_SNAPSHOT_DEPTH + 1, "overflow")
        .is_err());

    let mount = BrowserSnapshotMount {
        source: BrowserSnapshotSourceRef::EntryFile,
        virtual_prefix: String::new(),
        strip_prefix: None,
        kind: BrowserSnapshotMountKind::File,
    };
    let mut mounts = vec![mount.clone(); MAX_BROWSER_SNAPSHOT_MOUNTS];
    let mut seen_mounts = BTreeSet::new();
    assert!(push_snapshot_mount(&mut mounts, &mut seen_mounts, mount).is_err());

    let runtime = BrowserRuntimePlan {
        wasm_extensions: vec![
            BrowserWasmExtensionSpec {
                name: String::new(),
                module_key: String::new(),
                module_root: String::new(),
                wasm_path: String::new(),
                js_glue_path: None,
            };
            MAX_BROWSER_SNAPSHOT_MOUNTS + 1
        ],
        ..BrowserRuntimePlan::default()
    };
    assert!(
        browser_snapshot_plan_from_runtime_plan(&runtime, BrowserSnapshotRoot::EntryFile).is_err()
    );
}

#[test]
fn browser_snapshot_output_claims_allow_exact_overlap_and_reject_aliases() {
    let mut claimed = BTreeMap::new();
    assert_eq!(
        claim_browser_snapshot_output(&mut claimed, "/virtual/app.js", "/source/app.js"),
        Ok(true)
    );
    assert_eq!(
        claim_browser_snapshot_output(&mut claimed, "/virtual/app.js", "/source/app.js"),
        Ok(false)
    );
    assert!(
        claim_browser_snapshot_output(&mut claimed, "/virtual/app.js", "/other/app.js").is_err()
    );
}

#[test]
fn browser_snapshot_paths_are_portable_bounded_and_canonical() {
    assert_eq!(
        canonical_browser_snapshot_output_path("/project//src/./main.vo").unwrap(),
        "/project/src/main.vo"
    );
    assert_eq!(
        canonical_browser_snapshot_output_path("assets/old/../app.js").unwrap(),
        "assets/app.js"
    );
    assert!(canonical_browser_snapshot_output_path("../escape.vo").is_err());
    assert!(canonical_browser_snapshot_output_path("dir\\alias.vo").is_err());
    assert!(canonical_browser_snapshot_output_path(&format!(
        "{}.vo",
        "x".repeat(MAX_BROWSER_SNAPSHOT_PATH_BYTES)
    ))
    .is_err());

    let valid = BrowserSnapshotMount {
        source: BrowserSnapshotSourceRef::EntryFile,
        virtual_prefix: "wasm/runtime".to_string(),
        strip_prefix: Some(String::new()),
        kind: BrowserSnapshotMountKind::File,
    };
    assert!(validate_browser_snapshot_mount(&valid).is_ok());
    for invalid_prefix in ["/absolute", "../escape", "a//b", "a\\b"] {
        let mut invalid = valid.clone();
        invalid.virtual_prefix = invalid_prefix.to_string();
        assert!(validate_browser_snapshot_mount(&invalid).is_err());
    }
}

#[test]
fn browser_runtime_merge_deduplicates_exact_plans_and_rejects_conflicts() {
    let plan = browser_runtime_plan_from_manifest(
        "/modules/demo",
        Some("github.com/acme/demo"),
        &parse_manifest(
            r#"
[extension]
name = "demo"

[extension.wasm]
kind = "standalone"
wasm = "demo.wasm"

[extension.web]
capabilities = ["widget"]

[extension.web.js]
renderer = "renderer.js"
"#,
        ),
    )
    .unwrap();

    let merged = merge_browser_runtime_plans([plan.clone(), plan.clone()]).unwrap();
    assert_eq!(merged.graph.frameworks.len(), 1);
    assert_eq!(merged.wasm_bindings.len(), 1);
    assert_eq!(merged.wasm_extensions.len(), 1);

    let mut framework_conflict = plan.clone();
    framework_conflict.graph.frameworks[0].contract.name = "conflict".to_string();
    assert!(merge_browser_runtime_plans([plan.clone(), framework_conflict]).is_err());

    let same_identity_other_root = browser_runtime_plan_from_manifest(
        "/modules/other-demo",
        Some("github.com/acme/demo"),
        &parse_manifest(
            r#"
[extension]
name = "demo"

[extension.wasm]
kind = "standalone"
wasm = "demo.wasm"

[extension.web]
capabilities = ["widget"]

[extension.web.js]
renderer = "renderer.js"
"#,
        ),
    )
    .unwrap();
    assert!(merge_browser_runtime_plans([plan.clone(), same_identity_other_root]).is_err());

    let mut specification_conflict = plan.clone();
    specification_conflict.wasm_extensions[0].wasm_path = "/other/demo.wasm".to_string();
    assert!(merge_browser_runtime_plans([plan.clone(), specification_conflict]).is_err());

    let mut unbound_specification = BrowserRuntimePlan::default();
    unbound_specification
        .wasm_extensions
        .push(BrowserWasmExtensionSpec {
            name: "legacy".to_string(),
            module_key: "legacy".to_string(),
            module_root: "/legacy".to_string(),
            wasm_path: "/legacy/legacy.wasm".to_string(),
            js_glue_path: None,
        });
    assert!(merge_browser_runtime_plans([plan, unbound_specification]).is_err());

    assert!(merge_browser_runtime_plans(
        std::iter::repeat_with(BrowserRuntimePlan::default).take(MAX_BROWSER_RUNTIME_ITEMS + 1),
    )
    .is_err());
}

#[test]
fn browser_runtime_module_from_manifest_resolves_relative_paths() {
    let manifest = parse_manifest(
        r#"
[extension]
name = "vogui"

[extension.wasm]
kind = "standalone"
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
        "/github.com@vo-lang@vogui/0.1.4",
        Some("github.com/vo-lang/vogui"),
        &manifest,
    )
    .unwrap()
    .unwrap();

    assert_eq!(module.module_key, "github.com/vo-lang/vogui");
    assert_eq!(module.contract.name, "vogui");
    assert_eq!(module.contract.entry.as_deref(), Some("Run"));
    assert_eq!(
        module.contract.js_module_path("renderer"),
        Some("/github.com@vo-lang@vogui/0.1.4/js/dist/studio_renderer.js")
    );
    assert_eq!(
        module.contract.js_module_path("protocol"),
        Some("/github.com@vo-lang@vogui/0.1.4/js/dist/studio_protocol.js")
    );
    assert_eq!(
        module.contract.js_module_path("host_bridge"),
        Some("/github.com@vo-lang@vogui/0.1.4/js/dist/studio_host_bridge.js")
    );
}

#[test]
fn browser_runtime_module_from_manifest_returns_none_without_web_runtime() {
    let manifest = parse_manifest(
        r#"
[extension]
name = "zip"

[extension.wasm]
kind = "standalone"
wasm = "zip.wasm"
"#,
    );

    assert!(
        browser_runtime_module_from_manifest("/zip", None, &manifest)
            .unwrap()
            .is_none()
    );
}

#[test]
fn browser_wasm_extension_from_manifest_prefers_local_paths() {
    let manifest = parse_manifest(
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
"#,
    );

    let spec = browser_wasm_extension_from_manifest(
        "/github.com@vo-lang@voplay/local",
        Some("github.com/vo-lang/voplay"),
        &manifest,
    )
    .unwrap()
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
kind = "standalone"
wasm = "vogui.wasm"

[extension.web]
entry = "Run"
capabilities = ["widget"]

[extension.web.js]
renderer = "js/dist/studio_renderer.js"
"#,
    );

    let plan = browser_runtime_plan_from_manifest(
        "/github.com@vo-lang@vogui/0.1.4",
        Some("github.com/vo-lang/vogui"),
        &manifest,
    )
    .unwrap();

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
    );

    let plan = browser_runtime_plan_from_manifest(
        "/github.com@vo-lang@voplay/local",
        Some("github.com/vo-lang/voplay"),
        &manifest,
    )
    .unwrap();
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
        .unwrap()
    );
    assert_eq!(
        intent.required_artifacts[0].wasm.published_asset,
        AssetRef::artifact_root(
            ExtensionOwner::new(ModulePath::parse("github.com/vo-lang/voplay").unwrap()),
            "voplay_island_bg.wasm",
        )
        .unwrap()
    );
    assert_eq!(
        intent.required_artifacts[0].wasm.local_asset,
        Some(
            AssetRef::module_root(
                ExtensionOwner::new(ModulePath::parse("github.com/vo-lang/voplay").unwrap()),
                "rust/pkg-island/voplay_island_bg.wasm",
            )
            .unwrap()
        )
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
        .unwrap()
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
        .unwrap()
    );

    let mut oversized = plan;
    oversized.wasm_bindings =
        vec![oversized.wasm_bindings[0].clone(); MAX_BROWSER_RUNTIME_ITEMS + 1];
    assert!(oversized.artifact_intent().is_err());
}

#[test]
fn browser_snapshot_plan_projects_mounts_from_canonical_bindings() {
    let manifest = parse_manifest(
        r#"
[extension]
name = "vogui"

[extension.wasm]
kind = "standalone"
wasm = "vogui.wasm"

[extension.web]
entry = "Run"
capabilities = ["widget"]

[extension.web.js]
renderer = "js/dist/studio_renderer.js"
protocol = "js/dist/studio_protocol.js"
host_bridge = "host_bridge.js"
"#,
    );

    let plan = browser_runtime_plan_from_manifest(
        "/github.com@vo-lang@vogui/0.1.4",
        Some("github.com/vo-lang/vogui"),
        &manifest,
    )
    .unwrap();
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
        source: BrowserSnapshotSourceRef::AssetPath(
            AssetRef::module_root(owner.clone(), "js/dist").unwrap(),
        ),
        virtual_prefix: String::new(),
        strip_prefix: None,
        kind: BrowserSnapshotMountKind::Directory,
    }));
    assert!(snapshot.mounts.contains(&BrowserSnapshotMount {
        source: BrowserSnapshotSourceRef::AssetPath(
            AssetRef::module_root(owner.clone(), "").unwrap(),
        ),
        virtual_prefix: String::new(),
        strip_prefix: None,
        kind: BrowserSnapshotMountKind::Directory,
    }));
    assert!(snapshot.mounts.contains(&BrowserSnapshotMount {
        source: BrowserSnapshotSourceRef::AssetPath(
            AssetRef::artifact_root(owner, "vogui.wasm").unwrap(),
        ),
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
kind = "standalone"
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
        "/github.com@vo-lang@voplay/0.1.11",
        Some("github.com/vo-lang/voplay"),
        &manifest,
    )
    .unwrap();
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
kind = "bindgen"
wasm = "voplay_island_bg.wasm"
js = "voplay_island.js"

[extension.web]
capabilities = ["widget", "island_transport", "browser_runtime", "vfs"]

[extension.web.js]
renderer = "js/dist/voplay-render-island.js"
"#,
    );
    let ready = ready_module(
        "github.com/vo-lang/voplay",
        "0.1.11",
        vec![
            resolved_artifact("extension-wasm", "voplay_island_bg.wasm"),
            resolved_artifact("extension-js-glue", "voplay_island.js"),
        ],
        Some(manifest),
    );

    let runtime = ready_browser_runtime_module(&ready).unwrap();

    assert_eq!(runtime.module_key, "github.com/vo-lang/voplay");
    assert_eq!(runtime.module_root, "/github.com@vo-lang@voplay/0.1.11");
    assert_eq!(runtime.contract.name, "voplay");
    assert!(runtime.contract.entry.is_none());
    assert_eq!(
        runtime.contract.capabilities,
        vec!["widget", "island_transport", "browser_runtime", "vfs"]
    );
    assert_eq!(
        runtime.contract.js_module_path("renderer"),
        Some("/github.com@vo-lang@voplay/0.1.11/js/dist/voplay-render-island.js")
    );
}

#[test]
fn ready_browser_wasm_extension_uses_ready_module_artifacts() {
    let ready = ready_module(
        "github.com/vo-lang/voplay",
        "0.1.11",
        vec![
            resolved_artifact("extension-wasm", "voplay_island_bg.wasm"),
            resolved_artifact("extension-js-glue", "voplay_island.js"),
        ],
        Some(parse_manifest(
            r#"
[extension]
name = "voplay"

[extension.wasm]
kind = "bindgen"
wasm = "voplay_island_bg.wasm"
js = "voplay_island.js"
"#,
        )),
    );

    let spec = ready_browser_wasm_extension(&ready).unwrap().unwrap();

    assert_eq!(spec.name, "voplay");
    assert_eq!(spec.module_key, "github.com/vo-lang/voplay");
    assert_eq!(spec.module_root, "/github.com@vo-lang@voplay/0.1.11");
    assert_eq!(
        spec.wasm_path,
        "/github.com@vo-lang@voplay/0.1.11/artifacts/extension-wasm/wasm32-unknown-unknown/voplay_island_bg.wasm"
    );
    assert_eq!(
        spec.js_glue_path.as_deref(),
        Some("/github.com@vo-lang@voplay/0.1.11/artifacts/extension-js-glue/wasm32-unknown-unknown/voplay_island.js")
    );
}

#[test]
fn ready_module_rejects_js_glue_without_a_declared_wasm_pair() {
    let error = ReadyModule::try_new(
        ModulePath::parse("github.com/acme/demo").unwrap(),
        ExactVersion::parse("1.2.3").unwrap(),
        TEST_WASM_TARGET,
        vec![resolved_artifact("extension-js-glue", "demo.js")],
        None,
    )
    .unwrap_err();

    assert!(error.contains("do not match vo.mod declarations"));
}

#[test]
fn plan_ready_browser_runtime_collects_modules_and_wasm_extensions() {
    let manifest = parse_manifest(
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
    );
    let ready = ready_module(
        "github.com/vo-lang/vogui",
        "0.1.4",
        vec![resolved_artifact("extension-wasm", "vogui.wasm")],
        Some(manifest),
    );

    let plan = plan_ready_browser_runtime(&[ready]).unwrap();

    assert_eq!(plan.runtime_modules.len(), 1);
    assert_eq!(plan.graph.frameworks.len(), 1);
    assert_eq!(plan.wasm_bindings.len(), 1);
    assert_eq!(plan.wasm_extensions.len(), 1);
    assert_eq!(
        plan.runtime_modules[0].contract.js_module_path("renderer"),
        Some("/github.com@vo-lang@vogui/0.1.4/js/dist/studio_renderer.js")
    );
    assert_eq!(
        plan.wasm_extensions[0].wasm_path,
        "/github.com@vo-lang@vogui/0.1.4/artifacts/extension-wasm/wasm32-unknown-unknown/vogui.wasm"
    );
}

#[test]
fn plan_ready_browser_runtime_at_prefixes_host_module_root_base() {
    let ready = ready_module(
        "github.com/vo-lang/vogui",
        "0.1.4",
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
    );

    let plan = plan_ready_browser_runtime_at(&[ready], "/mod-cache").unwrap();

    assert_eq!(
        plan.runtime_modules[0].module_root,
        "/mod-cache/github.com@vo-lang@vogui/0.1.4"
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
        "/mod-cache/github.com@vo-lang@vogui/0.1.4/artifacts/extension-wasm/wasm32-unknown-unknown/vogui.wasm"
    );
}
