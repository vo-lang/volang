use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use vo_module::ext_manifest::{ExtensionManifest, WasmExtensionKind};
use vo_module::identity::ModulePath;
use vo_module::readiness::{ReadyModule, ResolvedArtifact};
use vo_module::resolved_extension::{
    resolve_extension_manifest, resolve_ready_extension, AssetRef, AssetRoot, ExtensionOwner,
    ResolvedExtension,
};
#[cfg(target_arch = "wasm32")]
use vo_module::schema::lockfile::LockedModule;

#[cfg(target_arch = "wasm32")]
use crate::wasm_vfs::WasmVfs;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrowserRuntimeContract {
    pub name: String,
    pub entry: Option<String>,
    pub capabilities: Vec<String>,
    pub js_modules: BTreeMap<String, String>,
}

impl BrowserRuntimeContract {
    pub fn js_module_path(&self, name: &str) -> Option<&str> {
        self.js_modules.get(name).map(|path| path.as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrowserRuntimeModule {
    pub module_key: String,
    pub module_root: String,
    pub contract: BrowserRuntimeContract,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BrowserFrameworkId {
    pub module_key: String,
    pub extension_name: String,
}

impl BrowserFrameworkId {
    pub fn new(module_key: impl Into<String>, extension_name: impl Into<String>) -> Self {
        Self {
            module_key: module_key.into(),
            extension_name: extension_name.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrowserFrameworkBinding {
    pub framework_id: BrowserFrameworkId,
    pub owner: Option<ExtensionOwner>,
    pub module_assets: BTreeMap<String, AssetRef>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrowserFrameworkPlan {
    pub id: BrowserFrameworkId,
    pub module_key: String,
    pub module_root: String,
    pub contract: BrowserRuntimeContract,
    pub binding: BrowserFrameworkBinding,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BrowserRoleIndex {
    pub entry_framework: Option<BrowserFrameworkId>,
    pub module_providers: BTreeMap<String, Vec<BrowserFrameworkId>>,
}

impl BrowserRoleIndex {
    pub fn providers_for(&self, role: &str) -> &[BrowserFrameworkId] {
        self.module_providers
            .get(role)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BrowserRuntimeGraph {
    pub frameworks: Vec<BrowserFrameworkPlan>,
    pub roles: BrowserRoleIndex,
}

impl BrowserRuntimeGraph {
    pub fn runtime_modules(&self) -> Vec<BrowserRuntimeModule> {
        self.frameworks
            .iter()
            .map(project_browser_runtime_module)
            .collect()
    }

    pub fn runtime_view(&self) -> BrowserRuntimeView {
        browser_runtime_view_from_graph(self)
    }

    pub fn primary_framework_split(&self) -> PrimaryFrameworkSplit {
        split_primary_provider_view(&self.runtime_view())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrowserRuntimeViewFramework {
    pub id: BrowserFrameworkId,
    pub contract: BrowserRuntimeContract,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BrowserRuntimeView {
    pub frameworks: Vec<BrowserRuntimeViewFramework>,
    pub roles: BrowserRoleIndex,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrowserWasmExtensionSpec {
    pub name: String,
    pub module_key: String,
    pub module_root: String,
    pub wasm_path: String,
    pub js_glue_path: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrowserWasmExtensionBinding {
    pub name: String,
    pub module_key: String,
    pub module_root: String,
    pub owner: Option<ExtensionOwner>,
    pub source: BrowserArtifactSource,
    pub kind: WasmExtensionKind,
    pub wasm_asset: AssetRef,
    pub js_glue_asset: Option<AssetRef>,
    pub published_wasm_asset: AssetRef,
    pub published_js_glue_asset: Option<AssetRef>,
    pub local_wasm_asset: Option<AssetRef>,
    pub local_js_glue_asset: Option<AssetRef>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BrowserSnapshotRoot {
    ProjectRoot,
    EntryFile,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BrowserSnapshotSourceRef {
    ProjectRoot,
    EntryFile,
    AssetPath(AssetRef),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BrowserSnapshotMountKind {
    Directory,
    File,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BrowserSnapshotMount {
    pub source: BrowserSnapshotSourceRef,
    pub virtual_prefix: String,
    pub strip_prefix: Option<String>,
    pub kind: BrowserSnapshotMountKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BrowserSnapshotPlan {
    pub mounts: Vec<BrowserSnapshotMount>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrowserSnapshotFile {
    pub path: String,
    pub bytes: Vec<u8>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BrowserArtifactSource {
    LocalManifest,
    ReadyModule,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BrowserArtifactFamily {
    StandaloneWasm,
    BindgenIsland,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrowserArtifactAssetBinding {
    pub runtime_asset: AssetRef,
    pub published_asset: AssetRef,
    pub local_asset: Option<AssetRef>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RequiredBrowserArtifact {
    pub owner: ExtensionOwner,
    pub module_key: String,
    pub extension_name: String,
    pub source: BrowserArtifactSource,
    pub family: BrowserArtifactFamily,
    pub runtime_roles: Vec<String>,
    pub wasm: BrowserArtifactAssetBinding,
    pub js_glue: Option<BrowserArtifactAssetBinding>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BrowserArtifactIntent {
    pub required_artifacts: Vec<RequiredBrowserArtifact>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PrimaryFrameworkSplit {
    pub primary_framework: Option<BrowserRuntimeContract>,
    pub provider_frameworks: Vec<BrowserRuntimeContract>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BrowserRuntimePlan {
    pub graph: BrowserRuntimeGraph,
    pub runtime_modules: Vec<BrowserRuntimeModule>,
    pub wasm_bindings: Vec<BrowserWasmExtensionBinding>,
    pub wasm_extensions: Vec<BrowserWasmExtensionSpec>,
}

impl BrowserRuntimePlan {
    pub fn runtime_view(&self) -> BrowserRuntimeView {
        self.graph.runtime_view()
    }

    pub fn primary_framework_split(&self) -> PrimaryFrameworkSplit {
        self.graph.primary_framework_split()
    }

    pub fn snapshot_plan(
        &self,
        root: BrowserSnapshotRoot,
    ) -> std::result::Result<BrowserSnapshotPlan, String> {
        browser_snapshot_plan_from_runtime_plan(self, root)
    }

    pub fn artifact_intent(&self) -> std::result::Result<BrowserArtifactIntent, String> {
        browser_artifact_intent_from_runtime_plan(self)
    }
}

pub fn browser_runtime_graph_from_manifest(
    module_root: &str,
    module_key: Option<&str>,
    manifest: &ExtensionManifest,
) -> BrowserRuntimeGraph {
    let framework = if let Some(owner) = module_key.and_then(parse_module_key_owner) {
        let resolved = resolve_extension_manifest(&owner, manifest);
        browser_framework_plan_from_resolved(module_root, module_key, &resolved)
    } else {
        browser_framework_plan_from_manifest_local(module_root, module_key, manifest)
    };
    browser_runtime_graph_from_frameworks(framework.into_iter().collect())
}

pub fn ready_browser_runtime_graph(ready_modules: &[ReadyModule]) -> BrowserRuntimeGraph {
    ready_browser_runtime_graph_at(ready_modules, "")
}

fn ready_browser_runtime_graph_at(
    ready_modules: &[ReadyModule],
    module_root_base: &str,
) -> BrowserRuntimeGraph {
    browser_runtime_graph_from_frameworks(
        ready_modules
            .iter()
            .filter_map(|ready| {
                let resolved = resolve_ready_extension(ready)?;
                browser_framework_plan_from_resolved(
                    &ready_module_root_at(ready, module_root_base),
                    Some(ready.module.as_str()),
                    &resolved,
                )
            })
            .collect(),
    )
}

pub fn browser_runtime_view_from_graph(graph: &BrowserRuntimeGraph) -> BrowserRuntimeView {
    BrowserRuntimeView {
        frameworks: graph
            .frameworks
            .iter()
            .map(|framework| BrowserRuntimeViewFramework {
                id: framework.id.clone(),
                contract: framework.contract.clone(),
            })
            .collect(),
        roles: graph.roles.clone(),
    }
}

pub fn split_primary_provider_view(view: &BrowserRuntimeView) -> PrimaryFrameworkSplit {
    let primary_id = view
        .roles
        .providers_for("protocol")
        .first()
        .or_else(|| view.roles.providers_for("host_bridge").first())
        .or(view.roles.entry_framework.as_ref())
        .or_else(|| view.frameworks.first().map(|framework| &framework.id));
    let Some(primary_id) = primary_id else {
        return PrimaryFrameworkSplit::default();
    };

    let mut primary_framework = None;
    let mut provider_frameworks = Vec::new();
    for framework in &view.frameworks {
        if framework.id == *primary_id && primary_framework.is_none() {
            primary_framework = Some(framework.contract.clone());
        } else {
            provider_frameworks.push(framework.contract.clone());
        }
    }
    PrimaryFrameworkSplit {
        primary_framework,
        provider_frameworks,
    }
}

pub fn merge_browser_runtime_graphs<I>(graphs: I) -> BrowserRuntimeGraph
where
    I: IntoIterator<Item = BrowserRuntimeGraph>,
{
    let mut frameworks = Vec::new();
    let mut seen_framework_roots = BTreeSet::new();
    for graph in graphs {
        for framework in graph.frameworks {
            if seen_framework_roots.insert(framework.module_root.clone()) {
                frameworks.push(framework);
            }
        }
    }
    browser_runtime_graph_from_frameworks(frameworks)
}

pub fn merge_browser_runtime_plans<I>(plans: I) -> BrowserRuntimePlan
where
    I: IntoIterator<Item = BrowserRuntimePlan>,
{
    let mut graphs = Vec::new();
    let mut wasm_bindings = Vec::new();
    let mut wasm_extensions = Vec::new();
    let mut seen_wasm_roots = BTreeSet::new();
    for plan in plans {
        graphs.push(plan.graph);
        for binding in plan.wasm_bindings {
            if seen_wasm_roots.insert(binding.module_root.clone()) {
                wasm_bindings.push(binding);
            }
        }
        for spec in plan.wasm_extensions {
            if !wasm_bindings
                .iter()
                .any(|binding| binding.module_root == spec.module_root)
                && seen_wasm_roots.insert(spec.module_root.clone())
            {
                wasm_extensions.push(spec);
            }
        }
    }
    let projected_wasm_extensions = project_browser_wasm_extension_specs(&wasm_bindings);
    if projected_wasm_extensions.is_empty() {
        browser_runtime_plan_from_parts(
            merge_browser_runtime_graphs(graphs),
            wasm_bindings,
            wasm_extensions,
        )
    } else {
        browser_runtime_plan_from_parts(
            merge_browser_runtime_graphs(graphs),
            wasm_bindings,
            projected_wasm_extensions,
        )
    }
}

pub fn browser_runtime_module_from_manifest(
    module_root: &str,
    module_key: Option<&str>,
    manifest: &ExtensionManifest,
) -> Option<BrowserRuntimeModule> {
    browser_runtime_graph_from_manifest(module_root, module_key, manifest)
        .runtime_modules()
        .into_iter()
        .next()
}

pub fn browser_wasm_extension_from_manifest(
    module_root: &str,
    module_key: Option<&str>,
    manifest: &ExtensionManifest,
) -> Option<BrowserWasmExtensionSpec> {
    let Some(owner) = module_key.and_then(parse_module_key_owner) else {
        return browser_wasm_extension_from_manifest_local(module_root, module_key, manifest);
    };
    let resolved = resolve_extension_manifest(&owner, manifest);
    browser_wasm_binding_from_resolved(module_root, module_key, &resolved)
        .map(|binding| project_browser_wasm_extension_spec(&binding))
}

pub fn browser_runtime_plan_from_manifest(
    module_root: &str,
    module_key: Option<&str>,
    manifest: &ExtensionManifest,
) -> BrowserRuntimePlan {
    let graph = browser_runtime_graph_from_manifest(module_root, module_key, manifest);
    let wasm_bindings = if let Some(owner) = module_key.and_then(parse_module_key_owner) {
        let resolved = resolve_extension_manifest(&owner, manifest);
        browser_wasm_binding_from_resolved(module_root, module_key, &resolved)
            .into_iter()
            .collect()
    } else {
        Vec::new()
    };
    let wasm_extensions = if wasm_bindings.is_empty() {
        browser_wasm_extension_from_manifest(module_root, module_key, manifest)
            .into_iter()
            .collect()
    } else {
        project_browser_wasm_extension_specs(&wasm_bindings)
    };
    browser_runtime_plan_from_parts(graph, wasm_bindings, wasm_extensions)
}

pub fn ready_browser_runtime_module(ready: &ReadyModule) -> Option<BrowserRuntimeModule> {
    ready_browser_runtime_graph_at(std::slice::from_ref(ready), "")
        .runtime_modules()
        .into_iter()
        .next()
}

pub fn ready_browser_runtime_modules(ready_modules: &[ReadyModule]) -> Vec<BrowserRuntimeModule> {
    ready_browser_runtime_graph_at(ready_modules, "").runtime_modules()
}

pub fn ready_browser_wasm_extension(
    ready: &ReadyModule,
) -> std::result::Result<Option<BrowserWasmExtensionSpec>, String> {
    Ok(ready_browser_wasm_binding(ready)?
        .map(|binding| project_browser_wasm_extension_spec(&binding)))
}

pub fn ready_browser_wasm_extensions(
    ready_modules: &[ReadyModule],
) -> std::result::Result<Vec<BrowserWasmExtensionSpec>, String> {
    Ok(project_browser_wasm_extension_specs(
        &ready_browser_wasm_bindings_at(ready_modules, "")?,
    ))
}

pub fn plan_ready_browser_runtime(
    ready_modules: &[ReadyModule],
) -> std::result::Result<BrowserRuntimePlan, String> {
    plan_ready_browser_runtime_at(ready_modules, "")
}

pub fn plan_ready_browser_runtime_at(
    ready_modules: &[ReadyModule],
    module_root_base: &str,
) -> std::result::Result<BrowserRuntimePlan, String> {
    let wasm_bindings = ready_browser_wasm_bindings_at(ready_modules, module_root_base)?;
    Ok(browser_runtime_plan_from_parts(
        ready_browser_runtime_graph_at(ready_modules, module_root_base),
        wasm_bindings.clone(),
        project_browser_wasm_extension_specs(&wasm_bindings),
    ))
}

fn ready_wasm_artifacts(
    ready: &ReadyModule,
) -> std::result::Result<Option<(&ResolvedArtifact, Option<&ResolvedArtifact>)>, String> {
    let wasm = ready
        .artifacts
        .iter()
        .find(|artifact| artifact.id.kind == "extension-wasm");
    let js_glue = ready
        .artifacts
        .iter()
        .find(|artifact| artifact.id.kind == "extension-js-glue");
    match wasm {
        Some(wasm) => Ok(Some((wasm, js_glue))),
        None if js_glue.is_none() => Ok(None),
        None => Err(format!(
            "resolved wasm extension assets are missing extension-wasm for {}@{}",
            ready.module, ready.version,
        )),
    }
}

fn ready_browser_wasm_binding(
    ready: &ReadyModule,
) -> std::result::Result<Option<BrowserWasmExtensionBinding>, String> {
    ready_browser_wasm_binding_at(ready, "")
}

fn ready_browser_wasm_binding_at(
    ready: &ReadyModule,
    module_root_base: &str,
) -> std::result::Result<Option<BrowserWasmExtensionBinding>, String> {
    let Some((wasm_artifact, js_glue_artifact)) = ready_wasm_artifacts(ready)? else {
        return Ok(None);
    };
    let resolved = resolve_ready_extension(ready);
    let module_root = ready_module_root_at(ready, module_root_base);
    let owner = resolved
        .as_ref()
        .map(|extension| extension.owner.clone())
        .unwrap_or_else(|| ExtensionOwner::new(ready.module.clone()));
    let name = resolved
        .as_ref()
        .map(|extension| extension.manifest.name.clone())
        .unwrap_or_else(|| ready.module.as_str().to_string());
    let kind = resolved
        .as_ref()
        .and_then(|extension| extension.wasm.as_ref().map(|wasm| wasm.kind))
        .unwrap_or_else(|| {
            if js_glue_artifact.is_some() {
                WasmExtensionKind::Bindgen
            } else {
                WasmExtensionKind::Standalone
            }
        });
    let published_wasm_asset = AssetRef::artifact_root(
        owner.clone(),
        artifact_root_relative_path(&wasm_artifact.cache_relative_path),
    );
    let published_js_glue_asset = js_glue_artifact.map(|artifact| {
        AssetRef::artifact_root(
            owner.clone(),
            artifact_root_relative_path(&artifact.cache_relative_path),
        )
    });
    Ok(Some(BrowserWasmExtensionBinding {
        name,
        module_key: ready.module.as_str().to_string(),
        module_root,
        owner: Some(owner),
        source: BrowserArtifactSource::ReadyModule,
        kind,
        wasm_asset: published_wasm_asset.clone(),
        js_glue_asset: published_js_glue_asset.clone(),
        published_wasm_asset,
        published_js_glue_asset,
        local_wasm_asset: None,
        local_js_glue_asset: None,
    }))
}

fn ready_browser_wasm_bindings_at(
    ready_modules: &[ReadyModule],
    module_root_base: &str,
) -> std::result::Result<Vec<BrowserWasmExtensionBinding>, String> {
    ready_modules
        .iter()
        .filter_map(
            |ready| match ready_browser_wasm_binding_at(ready, module_root_base) {
                Ok(Some(binding)) => Some(Ok(binding)),
                Ok(None) => None,
                Err(error) => Some(Err(error)),
            },
        )
        .collect()
}

fn browser_runtime_graph_from_frameworks(
    frameworks: Vec<BrowserFrameworkPlan>,
) -> BrowserRuntimeGraph {
    BrowserRuntimeGraph {
        roles: build_browser_role_index(&frameworks),
        frameworks,
    }
}

fn browser_runtime_plan_from_parts(
    graph: BrowserRuntimeGraph,
    wasm_bindings: Vec<BrowserWasmExtensionBinding>,
    wasm_extensions: Vec<BrowserWasmExtensionSpec>,
) -> BrowserRuntimePlan {
    let runtime_modules = graph.runtime_modules();
    BrowserRuntimePlan {
        graph,
        runtime_modules,
        wasm_bindings,
        wasm_extensions,
    }
}

pub fn browser_snapshot_plan_from_runtime_plan(
    runtime: &BrowserRuntimePlan,
    root: BrowserSnapshotRoot,
) -> std::result::Result<BrowserSnapshotPlan, String> {
    let mut mounts = Vec::new();
    let mut seen_mounts = BTreeSet::new();
    push_snapshot_mount(
        &mut mounts,
        &mut seen_mounts,
        BrowserSnapshotMount {
            source: match root {
                BrowserSnapshotRoot::ProjectRoot => BrowserSnapshotSourceRef::ProjectRoot,
                BrowserSnapshotRoot::EntryFile => BrowserSnapshotSourceRef::EntryFile,
            },
            virtual_prefix: String::new(),
            strip_prefix: None,
            kind: match root {
                BrowserSnapshotRoot::ProjectRoot => BrowserSnapshotMountKind::Directory,
                BrowserSnapshotRoot::EntryFile => BrowserSnapshotMountKind::File,
            },
        },
    );

    for framework in &runtime.graph.frameworks {
        if framework.binding.module_assets.is_empty() {
            if !framework.contract.js_modules.is_empty() {
                return Err(format!(
                    "browser runtime framework {} is missing canonical js module bindings",
                    framework.module_key,
                ));
            }
            continue;
        }
        for asset in framework.binding.module_assets.values() {
            push_snapshot_mount(
                &mut mounts,
                &mut seen_mounts,
                BrowserSnapshotMount {
                    source: BrowserSnapshotSourceRef::AssetPath(asset_parent_ref(asset)),
                    virtual_prefix: String::new(),
                    strip_prefix: None,
                    kind: BrowserSnapshotMountKind::Directory,
                },
            );
        }
    }

    if runtime.wasm_bindings.len() != runtime.wasm_extensions.len() {
        return Err(
            "browser runtime plan includes wasm extensions without canonical bindings".to_string(),
        );
    }
    let mut seen_virtual_wasm_paths = BTreeSet::new();
    for binding in &runtime.wasm_bindings {
        push_wasm_snapshot_mount(
            &mut mounts,
            &mut seen_mounts,
            &mut seen_virtual_wasm_paths,
            &binding.wasm_asset,
        )?;
        if let Some(js_glue_asset) = &binding.js_glue_asset {
            push_wasm_snapshot_mount(
                &mut mounts,
                &mut seen_mounts,
                &mut seen_virtual_wasm_paths,
                js_glue_asset,
            )?;
        }
    }

    Ok(BrowserSnapshotPlan { mounts })
}

pub fn browser_artifact_intent_from_runtime_plan(
    runtime: &BrowserRuntimePlan,
) -> std::result::Result<BrowserArtifactIntent, String> {
    let mut required_artifacts = Vec::new();
    let mut seen = BTreeSet::new();
    for binding in &runtime.wasm_bindings {
        let owner = binding.owner.clone().ok_or_else(|| {
            format!(
                "browser runtime wasm binding {} is missing canonical owner",
                binding.module_key,
            )
        })?;
        let family = match binding.kind {
            WasmExtensionKind::Standalone => BrowserArtifactFamily::StandaloneWasm,
            WasmExtensionKind::Bindgen => BrowserArtifactFamily::BindgenIsland,
        };
        if !seen.insert((owner.clone(), binding.name.clone(), family)) {
            continue;
        }
        required_artifacts.push(RequiredBrowserArtifact {
            owner: owner.clone(),
            module_key: binding.module_key.clone(),
            extension_name: binding.name.clone(),
            source: binding.source,
            family,
            runtime_roles: browser_runtime_roles_for_owner(&runtime.graph, &owner),
            wasm: BrowserArtifactAssetBinding {
                runtime_asset: binding.wasm_asset.clone(),
                published_asset: binding.published_wasm_asset.clone(),
                local_asset: binding.local_wasm_asset.clone(),
            },
            js_glue: binding.js_glue_asset.as_ref().map(|runtime_asset| {
                BrowserArtifactAssetBinding {
                    runtime_asset: runtime_asset.clone(),
                    published_asset: binding
                        .published_js_glue_asset
                        .clone()
                        .unwrap_or_else(|| runtime_asset.clone()),
                    local_asset: binding.local_js_glue_asset.clone(),
                }
            }),
        });
    }
    Ok(BrowserArtifactIntent { required_artifacts })
}

fn browser_runtime_roles_for_owner(
    graph: &BrowserRuntimeGraph,
    owner: &ExtensionOwner,
) -> Vec<String> {
    let mut roles = BTreeSet::new();
    for framework in &graph.frameworks {
        if framework.binding.owner.as_ref() != Some(owner) {
            continue;
        }
        for role in framework.binding.module_assets.keys() {
            roles.insert(role.clone());
        }
    }
    roles.into_iter().collect()
}

fn build_browser_role_index(frameworks: &[BrowserFrameworkPlan]) -> BrowserRoleIndex {
    let entry_framework = frameworks
        .iter()
        .find(|framework| framework.contract.entry.is_some())
        .map(|framework| framework.id.clone());
    let mut module_providers: BTreeMap<String, Vec<BrowserFrameworkId>> = BTreeMap::new();
    for framework in frameworks {
        let provided_roles = if framework.binding.module_assets.is_empty() {
            framework
                .contract
                .js_modules
                .keys()
                .cloned()
                .collect::<Vec<_>>()
        } else {
            framework
                .binding
                .module_assets
                .keys()
                .cloned()
                .collect::<Vec<_>>()
        };
        for role in provided_roles {
            module_providers
                .entry(role)
                .or_default()
                .push(framework.id.clone());
        }
    }
    BrowserRoleIndex {
        entry_framework,
        module_providers,
    }
}

fn browser_framework_plan_from_resolved(
    module_root: &str,
    module_key: Option<&str>,
    resolved: &ResolvedExtension,
) -> Option<BrowserFrameworkPlan> {
    let web = resolved.web.as_ref()?;
    let module_key = module_key.unwrap_or(&resolved.manifest.name).to_string();
    let module_root = normalize_vfs_path(module_root);
    let id = BrowserFrameworkId::new(module_key.clone(), resolved.manifest.name.clone());
    let contract = BrowserRuntimeContract {
        name: resolved.manifest.name.clone(),
        entry: web.entry.clone(),
        capabilities: web.capabilities.clone(),
        js_modules: web
            .js_modules
            .iter()
            .map(|(name, asset)| (name.clone(), resolve_asset_ref(&module_root, asset)))
            .collect(),
    };
    Some(BrowserFrameworkPlan {
        id: id.clone(),
        module_key,
        module_root,
        contract,
        binding: BrowserFrameworkBinding {
            framework_id: id,
            owner: Some(resolved.owner.clone()),
            module_assets: web.js_modules.clone(),
        },
    })
}

fn browser_wasm_binding_from_resolved(
    module_root: &str,
    module_key: Option<&str>,
    resolved: &ResolvedExtension,
) -> Option<BrowserWasmExtensionBinding> {
    let wasm = resolved.wasm.as_ref()?;
    let module_root = normalize_vfs_path(module_root);
    Some(BrowserWasmExtensionBinding {
        name: resolved.manifest.name.clone(),
        module_key: module_key.unwrap_or(&resolved.manifest.name).to_string(),
        module_root,
        owner: Some(resolved.owner.clone()),
        source: BrowserArtifactSource::LocalManifest,
        kind: wasm.kind,
        wasm_asset: wasm.local_or_published_wasm().clone(),
        js_glue_asset: wasm.local_or_published_js_glue().cloned(),
        published_wasm_asset: wasm.wasm.clone(),
        published_js_glue_asset: wasm.js_glue.clone(),
        local_wasm_asset: wasm.local_wasm.clone(),
        local_js_glue_asset: wasm.local_js_glue.clone(),
    })
}

fn project_browser_wasm_extension_spec(
    binding: &BrowserWasmExtensionBinding,
) -> BrowserWasmExtensionSpec {
    BrowserWasmExtensionSpec {
        name: binding.name.clone(),
        module_key: binding.module_key.clone(),
        module_root: binding.module_root.clone(),
        wasm_path: resolve_asset_ref(&binding.module_root, &binding.wasm_asset),
        js_glue_path: binding
            .js_glue_asset
            .as_ref()
            .map(|asset| resolve_asset_ref(&binding.module_root, asset)),
    }
}

fn project_browser_wasm_extension_specs(
    bindings: &[BrowserWasmExtensionBinding],
) -> Vec<BrowserWasmExtensionSpec> {
    bindings
        .iter()
        .map(project_browser_wasm_extension_spec)
        .collect()
}

fn browser_framework_plan_from_manifest_local(
    module_root: &str,
    module_key: Option<&str>,
    manifest: &ExtensionManifest,
) -> Option<BrowserFrameworkPlan> {
    let web = manifest.web_runtime()?;
    let module_key = module_key.unwrap_or(&manifest.name).to_string();
    let module_root = normalize_vfs_path(module_root);
    let id = BrowserFrameworkId::new(module_key.clone(), manifest.name.clone());
    let contract = BrowserRuntimeContract {
        name: manifest.name.clone(),
        entry: web.entry.clone(),
        capabilities: web.capabilities.clone(),
        js_modules: web
            .js_modules
            .iter()
            .map(|(name, path)| (name.clone(), resolve_manifest_path(&module_root, path)))
            .collect(),
    };
    Some(BrowserFrameworkPlan {
        id: id.clone(),
        module_key,
        module_root,
        contract,
        binding: BrowserFrameworkBinding {
            framework_id: id,
            owner: None,
            module_assets: BTreeMap::new(),
        },
    })
}

fn project_browser_runtime_module(framework: &BrowserFrameworkPlan) -> BrowserRuntimeModule {
    BrowserRuntimeModule {
        module_key: framework.module_key.clone(),
        module_root: framework.module_root.clone(),
        contract: framework.contract.clone(),
    }
}

fn browser_wasm_extension_from_manifest_local(
    module_root: &str,
    module_key: Option<&str>,
    manifest: &ExtensionManifest,
) -> Option<BrowserWasmExtensionSpec> {
    let wasm = manifest.wasm.as_ref()?;
    let module_root = normalize_vfs_path(module_root);
    let wasm_path = wasm.local_wasm.as_deref().unwrap_or(&wasm.wasm);
    let js_glue_path = wasm.local_js_glue.as_deref().or(wasm.js_glue.as_deref());
    Some(BrowserWasmExtensionSpec {
        name: manifest.name.clone(),
        module_key: module_key.unwrap_or(&manifest.name).to_string(),
        module_root: module_root.clone(),
        wasm_path: resolve_manifest_path(&module_root, wasm_path),
        js_glue_path: js_glue_path.map(|path| resolve_manifest_path(&module_root, path)),
    })
}

fn parse_module_key_owner(module_key: &str) -> Option<ModulePath> {
    ModulePath::parse(module_key).ok()
}

fn push_snapshot_mount(
    mounts: &mut Vec<BrowserSnapshotMount>,
    seen_mounts: &mut BTreeSet<BrowserSnapshotMount>,
    mount: BrowserSnapshotMount,
) {
    if seen_mounts.insert(mount.clone()) {
        mounts.push(mount);
    }
}

fn push_wasm_snapshot_mount(
    mounts: &mut Vec<BrowserSnapshotMount>,
    seen_mounts: &mut BTreeSet<BrowserSnapshotMount>,
    seen_virtual_paths: &mut BTreeSet<String>,
    asset: &AssetRef,
) -> std::result::Result<(), String> {
    let file_name = asset
        .relative_path()
        .file_name()
        .and_then(|value| value.to_str())
        .ok_or_else(|| {
            format!(
                "invalid browser wasm asset path {}",
                asset.relative_path().display()
            )
        })?;
    let virtual_path = format!("wasm/{}", file_name);
    if !seen_virtual_paths.insert(virtual_path) {
        return Err(format!(
            "duplicate browser wasm snapshot path wasm/{}",
            file_name
        ));
    }
    push_snapshot_mount(
        mounts,
        seen_mounts,
        BrowserSnapshotMount {
            source: BrowserSnapshotSourceRef::AssetPath(asset.clone()),
            virtual_prefix: "wasm".to_string(),
            strip_prefix: Some(asset_parent_relative_path(asset)),
            kind: BrowserSnapshotMountKind::File,
        },
    );
    Ok(())
}

fn ready_module_root_at(ready: &ReadyModule, module_root_base: &str) -> String {
    if module_root_base.is_empty() {
        return normalize_vfs_path(&format!("/{}", ready.module_dir.display()));
    }
    normalize_vfs_path(
        &Path::new(module_root_base)
            .join(&ready.module_dir)
            .to_string_lossy(),
    )
}

fn artifact_root_relative_path(path: &Path) -> std::path::PathBuf {
    path.strip_prefix(Path::new("artifacts"))
        .unwrap_or(path)
        .to_path_buf()
}

fn asset_parent_ref(asset: &AssetRef) -> AssetRef {
    let parent = asset
        .relative_path()
        .parent()
        .unwrap_or_else(|| Path::new(""));
    AssetRef {
        owner: asset.owner.clone(),
        root: asset.root,
        relative_path: parent.to_path_buf(),
    }
}

fn asset_parent_relative_path(asset: &AssetRef) -> String {
    asset
        .relative_path()
        .parent()
        .unwrap_or_else(|| Path::new(""))
        .to_string_lossy()
        .replace('\\', "/")
}

fn resolve_asset_ref(module_root: &str, asset: &AssetRef) -> String {
    let relative_path = asset.relative_path().to_string_lossy();
    match asset.root {
        AssetRoot::ModuleRoot => resolve_manifest_path(module_root, relative_path.as_ref()),
        AssetRoot::ArtifactRoot => {
            let artifact_root = join_vfs_path(module_root, "artifacts");
            join_vfs_path(&artifact_root, relative_path.as_ref())
        }
    }
}

fn resolve_manifest_path(module_root: &str, path: &str) -> String {
    if path.starts_with('/') {
        normalize_vfs_path(path)
    } else {
        join_vfs_path(module_root, path)
    }
}

fn normalize_vfs_path(path: &str) -> String {
    let trimmed = path.trim();
    if trimmed.is_empty() || trimmed == "/" {
        return "/".to_string();
    }

    let absolute = trimmed.starts_with('/');
    let mut parts = Vec::new();
    for part in trimmed.split('/') {
        if part.is_empty() || part == "." {
            continue;
        }
        if part == ".." {
            if !parts.is_empty() {
                parts.pop();
            }
            continue;
        }
        parts.push(part);
    }

    let joined = parts.join("/");
    if absolute {
        format!("/{}", joined)
    } else if joined.is_empty() {
        "/".to_string()
    } else {
        joined
    }
}

fn join_vfs_path(base: &str, child: &str) -> String {
    let normalized_base = normalize_vfs_path(base);
    let normalized_child = child.trim();
    if normalized_base == "/" {
        normalize_vfs_path(&format!("/{}", normalized_child))
    } else {
        normalize_vfs_path(&format!("{}/{}", normalized_base, normalized_child))
    }
}

#[cfg(target_arch = "wasm32")]
/// Debug-only helper for local extension/framework development.
///
/// This reads extension metadata from the project root `vo.mod` and
/// bypasses published dependency resolution. Normal Studio/example/runtime
/// flows must use locked published modules instead.
pub fn debug_local_project_browser_runtime_plan_from_vfs(
    project_root: &str,
) -> std::result::Result<BrowserRuntimePlan, String> {
    let project_root = normalize_vfs_path(project_root);
    let mod_path = join_vfs_path(&project_root, "vo.mod");
    let Some(manifest) = read_browser_runtime_vfs_ext_manifest(&mod_path)? else {
        return Ok(BrowserRuntimePlan::default());
    };
    let module_key = browser_runtime_project_root_module_key_from_vfs(&project_root)?;
    Ok(browser_runtime_plan_from_manifest(
        &project_root,
        Some(&module_key),
        &manifest,
    ))
}

#[cfg(target_arch = "wasm32")]
pub fn locked_browser_runtime_plan_from_vfs(
    locked_modules: &[LockedModule],
    mod_cache_root: &str,
) -> std::result::Result<BrowserRuntimePlan, String> {
    if locked_modules.is_empty() {
        return Ok(BrowserRuntimePlan::default());
    }
    let mod_cache_root = if mod_cache_root.trim().is_empty() {
        String::new()
    } else {
        normalize_vfs_path(mod_cache_root)
    };
    let ready = vo_module::readiness::check_project_readiness(
        &WasmVfs::new(&mod_cache_root),
        locked_modules,
        "wasm32-unknown-unknown",
    )
    .map_err(|error| error.to_string())?;
    plan_ready_browser_runtime_at(&ready, &mod_cache_root)
}

#[cfg(target_arch = "wasm32")]
/// Published runtime discovery for normal browser execution paths.
///
/// This resolves browser runtime state strictly from locked published modules
/// that have already been installed into the module cache. It must not merge
/// debug-only local project manifests.
pub fn published_browser_runtime_plan_from_vfs(
    locked_modules: &[LockedModule],
    mod_cache_root: &str,
) -> std::result::Result<BrowserRuntimePlan, String> {
    locked_browser_runtime_plan_from_vfs(locked_modules, mod_cache_root)
}

#[cfg(target_arch = "wasm32")]
fn read_browser_runtime_vfs_ext_manifest(
    mod_path: &str,
) -> std::result::Result<Option<ExtensionManifest>, String> {
    let (_name, _size, _mode, _mtime, _is_dir, err) = vo_web_runtime_wasm::vfs::stat(mod_path);
    if err.is_some() {
        return Ok(None);
    }
    let content = read_browser_runtime_vfs_text(mod_path)?;
    let mod_file = vo_module::schema::modfile::ModFile::parse(&content)
        .map_err(|error| format!("{}: {}", mod_path, error))?;
    Ok(mod_file.extension)
}

#[cfg(target_arch = "wasm32")]
fn browser_runtime_project_root_module_key_from_vfs(
    project_root: &str,
) -> std::result::Result<String, String> {
    let mod_path = join_vfs_path(project_root, "vo.mod");
    let mod_content = read_browser_runtime_vfs_text(&mod_path)?;
    let mod_file = vo_module::schema::modfile::ModFile::parse(&mod_content)
        .map_err(|error| format!("parse {}: {}", mod_path, error))?;
    let module = mod_file
        .module
        .as_github()
        .ok_or_else(|| format!("{}: root module must be a github module path", project_root))?;
    Ok(module.as_str().to_string())
}

#[cfg(target_arch = "wasm32")]
fn read_browser_runtime_vfs_text(path: &str) -> std::result::Result<String, String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    if let Some(error) = err {
        return Err(format!("read {}: {}", path, error));
    }
    String::from_utf8(data).map_err(|error| format!("read {}: {}", path, error))
}

#[cfg(target_arch = "wasm32")]
pub fn materialize_browser_snapshot_from_vfs(
    snapshot: &BrowserSnapshotPlan,
    runtime: &BrowserRuntimePlan,
    project_root: Option<&str>,
    entry_path: &str,
) -> std::result::Result<Vec<BrowserSnapshotFile>, String> {
    let mut files = Vec::new();
    let mut seen_paths = BTreeSet::new();
    for mount in &snapshot.mounts {
        match mount.kind {
            BrowserSnapshotMountKind::Directory => {
                materialize_browser_snapshot_directory_mount_from_vfs(
                    mount,
                    runtime,
                    project_root,
                    entry_path,
                    &mut files,
                    &mut seen_paths,
                )?
            }
            BrowserSnapshotMountKind::File => materialize_browser_snapshot_file_mount_from_vfs(
                mount,
                runtime,
                project_root,
                entry_path,
                &mut files,
                &mut seen_paths,
            )?,
        }
    }
    Ok(files)
}

#[cfg(target_arch = "wasm32")]
fn materialize_browser_snapshot_directory_mount_from_vfs(
    mount: &BrowserSnapshotMount,
    runtime: &BrowserRuntimePlan,
    project_root: Option<&str>,
    entry_path: &str,
    files: &mut Vec<BrowserSnapshotFile>,
    seen_paths: &mut BTreeSet<String>,
) -> std::result::Result<(), String> {
    fn walk(
        dir: &str,
        mount: &BrowserSnapshotMount,
        runtime: &BrowserRuntimePlan,
        project_root: Option<&str>,
        entry_path: &str,
        files: &mut Vec<BrowserSnapshotFile>,
        seen_paths: &mut BTreeSet<String>,
    ) -> std::result::Result<(), String> {
        let (entries, err) = vo_web_runtime_wasm::vfs::read_dir(dir);
        if let Some(error) = err {
            return Err(format!("read dir '{}': {}", dir, error));
        }
        for (name, is_dir, _mode) in entries {
            let full = if dir == "/" {
                format!("/{}", name)
            } else {
                format!("{}/{}", dir, name)
            };
            if is_dir {
                walk(
                    &full,
                    mount,
                    runtime,
                    project_root,
                    entry_path,
                    files,
                    seen_paths,
                )?;
                continue;
            }
            let output_path = materialized_browser_snapshot_path(
                mount,
                runtime,
                project_root,
                entry_path,
                &full,
            )?;
            if seen_paths.insert(output_path.clone()) {
                files.push(BrowserSnapshotFile {
                    path: output_path,
                    bytes: read_browser_snapshot_vfs_file(&full)?,
                });
            }
        }
        Ok(())
    }

    let root =
        resolve_browser_snapshot_source_path(runtime, &mount.source, project_root, entry_path)?;
    walk(
        &root,
        mount,
        runtime,
        project_root,
        entry_path,
        files,
        seen_paths,
    )
}

#[cfg(target_arch = "wasm32")]
fn materialize_browser_snapshot_file_mount_from_vfs(
    mount: &BrowserSnapshotMount,
    runtime: &BrowserRuntimePlan,
    project_root: Option<&str>,
    entry_path: &str,
    files: &mut Vec<BrowserSnapshotFile>,
    seen_paths: &mut BTreeSet<String>,
) -> std::result::Result<(), String> {
    let full =
        resolve_browser_snapshot_source_path(runtime, &mount.source, project_root, entry_path)?;
    let output_path =
        materialized_browser_snapshot_path(mount, runtime, project_root, entry_path, &full)?;
    if seen_paths.insert(output_path.clone()) {
        files.push(BrowserSnapshotFile {
            path: output_path,
            bytes: read_browser_snapshot_vfs_file(&full)?,
        });
    }
    Ok(())
}

#[cfg(target_arch = "wasm32")]
fn materialized_browser_snapshot_path(
    mount: &BrowserSnapshotMount,
    runtime: &BrowserRuntimePlan,
    project_root: Option<&str>,
    entry_path: &str,
    full_path: &str,
) -> std::result::Result<String, String> {
    let relative_path = if let Some(strip_prefix) = &mount.strip_prefix {
        let resolved_prefix = resolve_browser_snapshot_strip_prefix(
            runtime,
            &mount.source,
            strip_prefix,
            project_root,
            entry_path,
        )?;
        strip_browser_snapshot_prefix(full_path, &resolved_prefix)?
    } else {
        full_path.to_string()
    };
    if mount.virtual_prefix.is_empty() {
        Ok(relative_path)
    } else if relative_path.is_empty() {
        Ok(mount.virtual_prefix.trim_end_matches('/').to_string())
    } else {
        Ok(format!(
            "{}/{}",
            mount.virtual_prefix.trim_end_matches('/'),
            relative_path.trim_start_matches('/'),
        ))
    }
}

#[cfg(target_arch = "wasm32")]
fn resolve_browser_snapshot_source_path(
    runtime: &BrowserRuntimePlan,
    source: &BrowserSnapshotSourceRef,
    project_root: Option<&str>,
    entry_path: &str,
) -> std::result::Result<String, String> {
    match source {
        BrowserSnapshotSourceRef::ProjectRoot => project_root
            .map(normalize_vfs_path)
            .ok_or_else(|| "browser snapshot project root is missing".to_string()),
        BrowserSnapshotSourceRef::EntryFile => Ok(normalize_vfs_path(entry_path)),
        BrowserSnapshotSourceRef::AssetPath(asset) => {
            resolve_browser_snapshot_asset_path(runtime, asset)
        }
    }
}

#[cfg(target_arch = "wasm32")]
fn resolve_browser_snapshot_asset_path(
    runtime: &BrowserRuntimePlan,
    asset: &AssetRef,
) -> std::result::Result<String, String> {
    let module_root = browser_runtime_module_root_for_owner(runtime, &asset.owner)?;
    Ok(resolve_asset_ref(&module_root, asset))
}

pub(crate) fn browser_runtime_module_root_for_owner(
    runtime: &BrowserRuntimePlan,
    owner: &ExtensionOwner,
) -> std::result::Result<String, String> {
    for framework in &runtime.graph.frameworks {
        if framework.binding.owner.as_ref() == Some(owner) {
            return Ok(framework.module_root.clone());
        }
    }
    for binding in &runtime.wasm_bindings {
        if binding.owner.as_ref() == Some(owner) {
            return Ok(binding.module_root.clone());
        }
    }
    Err(format!(
        "browser runtime is missing module root for {}",
        owner.as_module(),
    ))
}

#[cfg(target_arch = "wasm32")]
fn resolve_browser_snapshot_strip_prefix(
    runtime: &BrowserRuntimePlan,
    source: &BrowserSnapshotSourceRef,
    strip_prefix: &str,
    project_root: Option<&str>,
    entry_path: &str,
) -> std::result::Result<String, String> {
    match source {
        BrowserSnapshotSourceRef::ProjectRoot => Ok(join_vfs_path(
            &project_root
                .map(normalize_vfs_path)
                .ok_or_else(|| "browser snapshot project root is missing".to_string())?,
            strip_prefix,
        )),
        BrowserSnapshotSourceRef::EntryFile => {
            if strip_prefix.is_empty() {
                Ok(normalize_vfs_path(entry_path))
            } else {
                Err("browser snapshot entry-file strip prefix is unsupported".to_string())
            }
        }
        BrowserSnapshotSourceRef::AssetPath(asset) => {
            let mut prefix_asset = asset.clone();
            prefix_asset.relative_path = std::path::PathBuf::from(strip_prefix);
            resolve_browser_snapshot_asset_path(runtime, &prefix_asset)
        }
    }
}

#[cfg(target_arch = "wasm32")]
fn strip_browser_snapshot_prefix(path: &str, prefix: &str) -> std::result::Result<String, String> {
    let path = normalize_vfs_path(path);
    let prefix = normalize_vfs_path(prefix);
    if prefix == "/" {
        return Ok(path.trim_start_matches('/').to_string());
    }
    if path == prefix {
        return Ok(String::new());
    }
    let prefix_with_sep = format!("{}/", prefix.trim_end_matches('/'));
    if let Some(relative) = path.strip_prefix(&prefix_with_sep) {
        return Ok(relative.to_string());
    }
    Err(format!(
        "snapshot path '{}' is outside strip prefix '{}'",
        path, prefix
    ))
}

#[cfg(target_arch = "wasm32")]
fn read_browser_snapshot_vfs_file(path: &str) -> std::result::Result<Vec<u8>, String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    if let Some(error) = err {
        return Err(format!("read file '{}': {}", path, error));
    }
    Ok(data)
}

#[cfg(test)]
mod tests {
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
            source: BrowserSnapshotSourceRef::AssetPath(AssetRef::artifact_root(
                owner,
                "vogui.wasm",
            )),
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
}
