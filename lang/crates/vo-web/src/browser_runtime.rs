use std::collections::{BTreeMap, BTreeSet};
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
    pub module_key: String,
    pub name: String,
    pub entry: Option<String>,
    pub provider_role: Option<vo_module::ext_manifest::WebProviderRole>,
    pub provider_roles: Vec<vo_module::ext_manifest::WebProviderRole>,
    pub capabilities: Vec<String>,
    pub roles: Vec<String>,
    pub js_modules: BTreeMap<String, String>,
}

impl BrowserRuntimeContract {
    pub fn js_module_path(&self, name: &str) -> Option<&str> {
        self.js_modules.get(name).map(|path| path.as_str())
    }
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
    pub owner: ExtensionOwner,
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
    pub fn primary_framework_split(&self) -> PrimaryFrameworkSplit {
        let primary_id = self
            .roles
            .providers_for("protocol")
            .first()
            .or_else(|| self.roles.providers_for("host_bridge").first())
            .or(self.roles.entry_framework.as_ref())
            .or_else(|| self.frameworks.first().map(|framework| &framework.id));
        let Some(primary_id) = primary_id else {
            return PrimaryFrameworkSplit::default();
        };

        let mut primary_framework = None;
        let mut provider_frameworks = Vec::new();
        for framework in &self.frameworks {
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrowserWasmExtensionBinding {
    pub name: String,
    pub module_key: String,
    pub module_root: String,
    pub owner: ExtensionOwner,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MaterializedBrowserArtifactRole {
    WasmModule,
    JavaScriptGlue,
    JavaScriptModule,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MaterializedBrowserArtifactFamily {
    StandaloneWasm,
    BindgenIsland,
    FrameworkModule,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MaterializedBrowserArtifact {
    pub module_key: String,
    pub extension_name: String,
    pub family: MaterializedBrowserArtifactFamily,
    pub role: MaterializedBrowserArtifactRole,
    pub runtime_roles: Vec<String>,
    pub source_path: String,
    pub artifact_identity: [u8; 32],
    pub content_digest: [u8; 32],
    pub detached_manifest: Vec<u8>,
    pub detached_manifest_digest: [u8; 32],
    pub development_attestation_digest: [u8; 32],
}

/// Browser snapshots are copied at least once more when they cross the
/// native/JavaScript boundary. Keep one shared, explicit budget for native and
/// WASM materializers so a public `BrowserSnapshotPlan` cannot trigger an
/// unbounded directory walk or allocation.
pub const MAX_BROWSER_RUNTIME_ITEMS: usize = 10_000;
pub const MAX_BROWSER_SNAPSHOT_MOUNTS: usize = MAX_BROWSER_RUNTIME_ITEMS;
pub const MAX_BROWSER_SNAPSHOT_ENTRIES: usize = 100_000;
pub const MAX_BROWSER_SNAPSHOT_FILES: usize = 20_000;
pub const MAX_BROWSER_SNAPSHOT_FILE_BYTES: usize = 256 * 1024 * 1024;
pub const MAX_BROWSER_SNAPSHOT_BYTES: usize = 512 * 1024 * 1024;
pub const MAX_BROWSER_SNAPSHOT_DEPTH: usize = 256;
pub const MAX_BROWSER_SNAPSHOT_PATH_BYTES: usize = 4 * 1024;
pub const MAX_BROWSER_SNAPSHOT_COMPONENT_BYTES: usize = 255;

#[derive(Debug, Default)]
pub(super) struct BrowserSnapshotBudget {
    entries: usize,
    files: usize,
    bytes: usize,
}

impl BrowserSnapshotBudget {
    pub(super) fn new(mounts: usize) -> std::result::Result<Self, String> {
        if mounts > MAX_BROWSER_SNAPSHOT_MOUNTS {
            return Err(format!(
                "browser snapshot contains {mounts} mounts and exceeds the {MAX_BROWSER_SNAPSHOT_MOUNTS}-mount limit"
            ));
        }
        Ok(Self::default())
    }

    pub(super) fn remaining_entries(&self) -> usize {
        MAX_BROWSER_SNAPSHOT_ENTRIES.saturating_sub(self.entries)
    }

    pub(super) fn record_entry(&mut self, path: &str) -> std::result::Result<(), String> {
        let entries = self.entries.checked_add(1).ok_or_else(|| {
            format!("browser snapshot entry count overflow while traversing {path:?}")
        })?;
        if entries > MAX_BROWSER_SNAPSHOT_ENTRIES {
            return Err(format!(
                "browser snapshot contains more than {MAX_BROWSER_SNAPSHOT_ENTRIES} filesystem entries while traversing {path:?}"
            ));
        }
        self.entries = entries;
        Ok(())
    }

    pub(super) fn validate_depth(
        &self,
        depth: usize,
        path: &str,
    ) -> std::result::Result<(), String> {
        if depth > MAX_BROWSER_SNAPSHOT_DEPTH {
            return Err(format!(
                "browser snapshot exceeds the {MAX_BROWSER_SNAPSHOT_DEPTH}-level directory depth limit at {path:?}"
            ));
        }
        Ok(())
    }

    pub(super) fn next_file_limit(&self, path: &str) -> std::result::Result<usize, String> {
        if self.files >= MAX_BROWSER_SNAPSHOT_FILES {
            return Err(format!(
                "browser snapshot contains more than {MAX_BROWSER_SNAPSHOT_FILES} files at {path:?}"
            ));
        }
        Ok(MAX_BROWSER_SNAPSHOT_FILE_BYTES
            .min(MAX_BROWSER_SNAPSHOT_BYTES.saturating_sub(self.bytes)))
    }

    pub(super) fn record_file(
        &mut self,
        path: &str,
        len: usize,
    ) -> std::result::Result<(), String> {
        if len > MAX_BROWSER_SNAPSHOT_FILE_BYTES {
            return Err(format!(
                "browser snapshot file {path:?} has {len} bytes and exceeds the {MAX_BROWSER_SNAPSHOT_FILE_BYTES}-byte per-file limit"
            ));
        }
        let files = self
            .files
            .checked_add(1)
            .ok_or_else(|| format!("browser snapshot file count overflow while adding {path:?}"))?;
        if files > MAX_BROWSER_SNAPSHOT_FILES {
            return Err(format!(
                "browser snapshot contains more than {MAX_BROWSER_SNAPSHOT_FILES} files at {path:?}"
            ));
        }
        let bytes = self
            .bytes
            .checked_add(len)
            .ok_or_else(|| format!("browser snapshot byte count overflow while adding {path:?}"))?;
        if bytes > MAX_BROWSER_SNAPSHOT_BYTES {
            return Err(format!(
                "browser snapshot exceeds the {MAX_BROWSER_SNAPSHOT_BYTES}-byte aggregate limit while adding {path:?}"
            ));
        }
        self.files = files;
        self.bytes = bytes;
        Ok(())
    }
}

pub(super) fn claim_browser_snapshot_output(
    claimed: &mut BTreeMap<String, String>,
    output_path: &str,
    source_path: &str,
) -> std::result::Result<bool, String> {
    match claimed.get(output_path) {
        Some(existing) if existing == source_path => Ok(false),
        Some(existing) => Err(format!(
            "browser snapshot output {output_path:?} is claimed by both {existing:?} and {source_path:?}"
        )),
        None => {
            claimed.insert(output_path.to_string(), source_path.to_string());
            Ok(true)
        }
    }
}

pub(super) fn validate_browser_snapshot_mount(
    mount: &BrowserSnapshotMount,
) -> std::result::Result<(), String> {
    validate_browser_snapshot_relative_path("virtual_prefix", &mount.virtual_prefix, true)?;
    if let Some(strip_prefix) = &mount.strip_prefix {
        validate_browser_snapshot_relative_path("strip_prefix", strip_prefix, true)?;
    }
    Ok(())
}

fn validate_browser_snapshot_relative_path(
    field: &str,
    path: &str,
    allow_empty: bool,
) -> std::result::Result<(), String> {
    if path.is_empty() {
        return if allow_empty {
            Ok(())
        } else {
            Err(format!("browser snapshot {field} must not be empty"))
        };
    }
    if path.starts_with('/') || path.contains('\\') || path.contains('\0') {
        return Err(format!(
            "browser snapshot {field} must be a portable relative path: {path:?}"
        ));
    }
    if path.len() > MAX_BROWSER_SNAPSHOT_PATH_BYTES {
        return Err(format!(
            "browser snapshot {field} exceeds the {MAX_BROWSER_SNAPSHOT_PATH_BYTES}-byte path limit"
        ));
    }
    let mut depth = 0usize;
    for component in path.split('/') {
        if component.is_empty() || component == "." || component == ".." {
            return Err(format!(
                "browser snapshot {field} is not normalized: {path:?}"
            ));
        }
        if component.len() > MAX_BROWSER_SNAPSHOT_COMPONENT_BYTES {
            return Err(format!(
                "browser snapshot {field} component exceeds the {MAX_BROWSER_SNAPSHOT_COMPONENT_BYTES}-byte limit: {component:?}"
            ));
        }
        depth = depth
            .checked_add(1)
            .ok_or_else(|| format!("browser snapshot {field} depth overflow"))?;
        if depth > MAX_BROWSER_SNAPSHOT_DEPTH {
            return Err(format!(
                "browser snapshot {field} exceeds the {MAX_BROWSER_SNAPSHOT_DEPTH}-component path limit"
            ));
        }
    }
    Ok(())
}

pub(super) fn canonical_browser_snapshot_output_path(
    path: &str,
) -> std::result::Result<String, String> {
    if path.is_empty() || path.contains('\\') || path.contains('\0') {
        return Err(format!("invalid browser snapshot output path {path:?}"));
    }
    if path.len() > MAX_BROWSER_SNAPSHOT_PATH_BYTES {
        return Err(format!(
            "browser snapshot output path exceeds the {MAX_BROWSER_SNAPSHOT_PATH_BYTES}-byte limit"
        ));
    }

    let absolute = path.starts_with('/');
    let mut components = Vec::new();
    for component in path.split('/') {
        if component.is_empty() || component == "." {
            continue;
        }
        if component == ".." {
            if components.pop().is_none() {
                return Err(format!(
                    "browser snapshot output path escapes its root: {path:?}"
                ));
            }
            continue;
        }
        if component.len() > MAX_BROWSER_SNAPSHOT_COMPONENT_BYTES {
            return Err(format!(
                "browser snapshot output component exceeds the {MAX_BROWSER_SNAPSHOT_COMPONENT_BYTES}-byte limit: {component:?}"
            ));
        }
        components
            .try_reserve(1)
            .map_err(|_| "failed to reserve browser snapshot path components".to_string())?;
        components.push(component);
        if components.len() > MAX_BROWSER_SNAPSHOT_DEPTH {
            return Err(format!(
                "browser snapshot output exceeds the {MAX_BROWSER_SNAPSHOT_DEPTH}-component path limit"
            ));
        }
    }
    if components.is_empty() {
        return Err(format!(
            "browser snapshot output path does not identify a file: {path:?}"
        ));
    }
    let joined = components.join("/");
    let normalized = if absolute {
        format!("/{joined}")
    } else {
        joined
    };
    if normalized.len() > MAX_BROWSER_SNAPSHOT_PATH_BYTES {
        return Err(format!(
            "browser snapshot output path exceeds the {MAX_BROWSER_SNAPSHOT_PATH_BYTES}-byte limit"
        ));
    }
    Ok(normalized)
}

fn validate_browser_runtime_module_root(module_root: &str) -> std::result::Result<(), String> {
    if module_root == "/" {
        return Ok(());
    }
    let canonical = canonical_browser_snapshot_output_path(module_root)?;
    if canonical != module_root {
        return Err(format!(
            "browser runtime module root is not canonical: {module_root:?}"
        ));
    }
    Ok(())
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
    pub wasm_bindings: Vec<BrowserWasmExtensionBinding>,
}

impl BrowserRuntimePlan {
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

#[allow(clippy::too_many_arguments)]
pub(super) fn materialized_browser_artifact_from_bytes(
    module_key: &str,
    extension_name: &str,
    family: MaterializedBrowserArtifactFamily,
    role: MaterializedBrowserArtifactRole,
    runtime_roles: &[String],
    asset: &AssetRef,
    source_path: String,
    bytes: &[u8],
) -> std::result::Result<MaterializedBrowserArtifact, String> {
    if bytes.is_empty() {
        return Err(format!(
            "materialized browser artifact is empty: {source_path}"
        ));
    }
    let mut runtime_roles = runtime_roles.to_vec();
    runtime_roles.sort();
    runtime_roles.dedup();
    let identity_record = browser_artifact_identity_record(
        module_key,
        extension_name,
        family,
        role,
        asset,
        &runtime_roles,
    )?;
    let content_digest = browser_sha256_bytes(bytes);
    let artifact_identity = browser_sha256_bytes(&identity_record);
    let detached_manifest =
        browser_artifact_manifest_record(&identity_record, content_digest, bytes.len() as u64)?;
    let detached_manifest_digest = browser_sha256_bytes(&detached_manifest);
    let development_attestation_digest = browser_sha256_parts(
        b"vo.browser-artifact.development-attestation.v1",
        &[&detached_manifest_digest, &content_digest],
    );
    Ok(MaterializedBrowserArtifact {
        module_key: module_key.to_string(),
        extension_name: extension_name.to_string(),
        family,
        role,
        runtime_roles,
        source_path,
        artifact_identity,
        content_digest,
        detached_manifest,
        detached_manifest_digest,
        development_attestation_digest,
    })
}

fn browser_artifact_identity_record(
    module_key: &str,
    extension_name: &str,
    family: MaterializedBrowserArtifactFamily,
    role: MaterializedBrowserArtifactRole,
    asset: &AssetRef,
    runtime_roles: &[String],
) -> std::result::Result<Vec<u8>, String> {
    let mut record = Vec::new();
    push_browser_record_bytes(
        &mut record,
        b"vo.browser-artifact.identity.v1",
        "identity domain",
    )?;
    push_browser_record_bytes(&mut record, module_key.as_bytes(), "module key")?;
    push_browser_record_bytes(&mut record, extension_name.as_bytes(), "extension name")?;
    record.push(match family {
        MaterializedBrowserArtifactFamily::StandaloneWasm => 1,
        MaterializedBrowserArtifactFamily::BindgenIsland => 2,
        MaterializedBrowserArtifactFamily::FrameworkModule => 3,
    });
    record.push(match role {
        MaterializedBrowserArtifactRole::WasmModule => 1,
        MaterializedBrowserArtifactRole::JavaScriptGlue => 2,
        MaterializedBrowserArtifactRole::JavaScriptModule => 3,
    });
    record.push(match asset.root() {
        AssetRoot::ModuleRoot => 1,
        AssetRoot::ArtifactRoot => 2,
    });
    push_browser_record_bytes(
        &mut record,
        asset.portable_relative_path().as_bytes(),
        "artifact path",
    )?;
    let role_count = u32::try_from(runtime_roles.len())
        .map_err(|_| String::from("browser artifact runtime role count overflow"))?;
    record.extend_from_slice(&role_count.to_le_bytes());
    for runtime_role in runtime_roles {
        push_browser_record_bytes(&mut record, runtime_role.as_bytes(), "runtime role")?;
    }
    Ok(record)
}

fn browser_artifact_manifest_record(
    identity_record: &[u8],
    content_digest: [u8; 32],
    content_bytes: u64,
) -> std::result::Result<Vec<u8>, String> {
    let mut record = Vec::new();
    push_browser_record_bytes(
        &mut record,
        b"vo.browser-artifact.detached-manifest.v1",
        "manifest domain",
    )?;
    push_browser_record_bytes(&mut record, identity_record, "identity record")?;
    record.extend_from_slice(&content_digest);
    record.extend_from_slice(&content_bytes.to_le_bytes());
    Ok(record)
}

fn push_browser_record_bytes(
    output: &mut Vec<u8>,
    bytes: &[u8],
    label: &str,
) -> std::result::Result<(), String> {
    let length =
        u32::try_from(bytes.len()).map_err(|_| format!("browser artifact {label} is too large"))?;
    output
        .try_reserve(4usize.saturating_add(bytes.len()))
        .map_err(|_| format!("failed to reserve browser artifact {label}"))?;
    output.extend_from_slice(&length.to_le_bytes());
    output.extend_from_slice(bytes);
    Ok(())
}

fn browser_sha256_parts(domain: &[u8], parts: &[&[u8]]) -> [u8; 32] {
    let mut record = Vec::new();
    record.extend_from_slice(domain);
    for part in parts {
        record.extend_from_slice(&(part.len() as u64).to_le_bytes());
        record.extend_from_slice(part);
    }
    browser_sha256_bytes(&record)
}

fn browser_sha256_bytes(bytes: &[u8]) -> [u8; 32] {
    let digest = vo_module::digest::Digest::from_sha256(bytes);
    let hex = digest.hex().as_bytes();
    let mut output = [0u8; 32];
    for (index, byte) in output.iter_mut().enumerate() {
        *byte = (browser_hex_nibble(hex[index * 2]) << 4) | browser_hex_nibble(hex[index * 2 + 1]);
    }
    output
}

fn browser_hex_nibble(byte: u8) -> u8 {
    match byte {
        b'0'..=b'9' => byte - b'0',
        b'a'..=b'f' => byte - b'a' + 10,
        _ => unreachable!("vo-module produced a validated lowercase SHA-256 digest"),
    }
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
                    &resolved,
                )
            })
            .collect(),
    )
}

pub fn merge_browser_runtime_graphs<I>(
    graphs: I,
) -> std::result::Result<BrowserRuntimeGraph, String>
where
    I: IntoIterator<Item = BrowserRuntimeGraph>,
{
    let mut frameworks = Vec::new();
    let mut framework_by_root = BTreeMap::new();
    let mut framework_by_id = BTreeMap::new();
    let mut framework_inputs = 0usize;
    for (graph_index, graph) in graphs.into_iter().enumerate() {
        if graph_index >= MAX_BROWSER_RUNTIME_ITEMS {
            return Err(format!(
                "browser runtime merge contains more than {MAX_BROWSER_RUNTIME_ITEMS} graphs"
            ));
        }
        for framework in graph.frameworks {
            validate_browser_runtime_module_root(&framework.module_root)?;
            framework_inputs = framework_inputs
                .checked_add(1)
                .ok_or_else(|| "browser runtime framework input count overflow".to_string())?;
            if framework_inputs > MAX_BROWSER_RUNTIME_ITEMS {
                return Err(format!(
                    "browser runtime merge contains more than {MAX_BROWSER_RUNTIME_ITEMS} framework inputs"
                ));
            }
            if let Some(existing_root) = framework_by_id.get(&framework.id) {
                if existing_root != &framework.module_root {
                    return Err(format!(
                        "browser runtime framework {:?} is bound to both {:?} and {:?}",
                        framework.id, existing_root, framework.module_root
                    ));
                }
            }
            match framework_by_root.get(&framework.module_root) {
                Some(existing) if existing == &framework => continue,
                Some(_) => {
                    return Err(format!(
                        "conflicting browser runtime frameworks reuse module root {:?}",
                        framework.module_root
                    ));
                }
                None => {
                    frameworks
                        .try_reserve(1)
                        .map_err(|_| "failed to reserve browser runtime frameworks".to_string())?;
                    framework_by_id.insert(framework.id.clone(), framework.module_root.clone());
                    framework_by_root.insert(framework.module_root.clone(), framework.clone());
                    frameworks.push(framework);
                }
            }
        }
    }
    Ok(browser_runtime_graph_from_frameworks(frameworks))
}

pub fn merge_browser_runtime_plans<I>(plans: I) -> std::result::Result<BrowserRuntimePlan, String>
where
    I: IntoIterator<Item = BrowserRuntimePlan>,
{
    let mut graphs = Vec::new();
    let mut wasm_bindings = Vec::new();
    let mut binding_by_root = BTreeMap::new();
    let mut binding_root_by_id = BTreeMap::new();
    let mut binding_inputs = 0usize;
    for (plan_index, plan) in plans.into_iter().enumerate() {
        if plan_index >= MAX_BROWSER_RUNTIME_ITEMS {
            return Err(format!(
                "browser runtime merge contains more than {MAX_BROWSER_RUNTIME_ITEMS} plans"
            ));
        }
        graphs
            .try_reserve(1)
            .map_err(|_| "failed to reserve browser runtime graphs".to_string())?;
        graphs.push(plan.graph);
        for binding in plan.wasm_bindings {
            validate_browser_runtime_module_root(&binding.module_root)?;
            binding_inputs = binding_inputs
                .checked_add(1)
                .ok_or_else(|| "browser runtime wasm binding input count overflow".to_string())?;
            if binding_inputs > MAX_BROWSER_RUNTIME_ITEMS {
                return Err(format!(
                    "browser runtime merge contains more than {MAX_BROWSER_RUNTIME_ITEMS} wasm binding inputs"
                ));
            }
            let binding_id = (binding.module_key.clone(), binding.name.clone());
            if let Some(existing_root) = binding_root_by_id.get(&binding_id) {
                if existing_root != &binding.module_root {
                    return Err(format!(
                        "browser wasm binding {:?} is bound to both {:?} and {:?}",
                        binding_id, existing_root, binding.module_root
                    ));
                }
            }
            match binding_by_root.get(&binding.module_root) {
                Some(existing) if existing == &binding => continue,
                Some(_) => {
                    return Err(format!(
                        "conflicting browser wasm bindings reuse module root {:?}",
                        binding.module_root
                    ));
                }
                None => {
                    wasm_bindings.try_reserve(1).map_err(|_| {
                        "failed to reserve browser runtime wasm bindings".to_string()
                    })?;
                    binding_root_by_id.insert(binding_id, binding.module_root.clone());
                    binding_by_root.insert(binding.module_root.clone(), binding.clone());
                    wasm_bindings.push(binding);
                }
            }
        }
    }
    Ok(browser_runtime_plan_from_parts(
        merge_browser_runtime_graphs(graphs)?,
        wasm_bindings,
    ))
}

pub fn browser_runtime_plan_from_manifest(
    module_root: &str,
    module: &ModulePath,
    manifest: &ExtensionManifest,
) -> std::result::Result<BrowserRuntimePlan, String> {
    let resolved =
        resolve_extension_manifest(module, manifest).map_err(|error| error.to_string())?;
    let graph = browser_runtime_graph_from_frameworks(
        browser_framework_plan_from_resolved(module_root, &resolved)
            .into_iter()
            .collect(),
    );
    let wasm_bindings = browser_wasm_binding_from_resolved(module_root, &resolved)
        .into_iter()
        .collect();
    Ok(browser_runtime_plan_from_parts(graph, wasm_bindings))
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
        wasm_bindings,
    ))
}

fn ready_wasm_artifacts(
    ready: &ReadyModule,
) -> std::result::Result<Option<(&ResolvedArtifact, Option<&ResolvedArtifact>)>, String> {
    let wasm = ready
        .artifacts()
        .iter()
        .find(|artifact| artifact.id().kind == "extension-wasm");
    let js_glue = ready
        .artifacts()
        .iter()
        .find(|artifact| artifact.id().kind == "extension-js-glue");
    match wasm {
        Some(wasm) => Ok(Some((wasm, js_glue))),
        None if js_glue.is_none() => Ok(None),
        None => Err(format!(
            "resolved wasm extension assets are missing extension-wasm for {}@{}",
            ready.module(),
            ready.version(),
        )),
    }
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
        .unwrap_or_else(|| ExtensionOwner::new(ready.module().clone()));
    let name = resolved
        .as_ref()
        .map(|extension| extension.manifest.name.clone())
        .unwrap_or_else(|| ready.module().as_str().to_string());
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
    let published_wasm_asset =
        AssetRef::artifact_root(owner.clone(), artifact_root_relative_path(wasm_artifact))?;
    let published_js_glue_asset = js_glue_artifact
        .map(|artifact| {
            AssetRef::artifact_root(owner.clone(), artifact_root_relative_path(artifact))
        })
        .transpose()?;
    Ok(Some(BrowserWasmExtensionBinding {
        name,
        module_key: ready.module().as_str().to_string(),
        module_root,
        owner,
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
) -> BrowserRuntimePlan {
    BrowserRuntimePlan {
        graph,
        wasm_bindings,
    }
}

pub fn browser_snapshot_plan_from_runtime_plan(
    runtime: &BrowserRuntimePlan,
    root: BrowserSnapshotRoot,
) -> std::result::Result<BrowserSnapshotPlan, String> {
    if runtime.graph.frameworks.len() > MAX_BROWSER_SNAPSHOT_MOUNTS
        || runtime.wasm_bindings.len() > MAX_BROWSER_SNAPSHOT_MOUNTS
    {
        return Err(format!(
            "browser runtime plan exceeds the {MAX_BROWSER_SNAPSHOT_MOUNTS}-item snapshot planning limit"
        ));
    }
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
    )?;

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
            )?;
        }
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
    if runtime.wasm_bindings.len() > MAX_BROWSER_RUNTIME_ITEMS
        || runtime.graph.frameworks.len() > MAX_BROWSER_RUNTIME_ITEMS
    {
        return Err(format!(
            "browser runtime plan exceeds the {MAX_BROWSER_RUNTIME_ITEMS}-item artifact planning limit"
        ));
    }
    let mut required_artifacts = Vec::new();
    let mut seen = BTreeSet::new();
    for binding in &runtime.wasm_bindings {
        let owner = binding.owner.clone();
        let family = match binding.kind {
            WasmExtensionKind::Standalone => BrowserArtifactFamily::StandaloneWasm,
            WasmExtensionKind::Bindgen => BrowserArtifactFamily::BindgenIsland,
        };
        if !seen.insert((owner.clone(), binding.name.clone(), family)) {
            continue;
        }
        required_artifacts
            .try_reserve(1)
            .map_err(|_| "failed to reserve required browser artifacts".to_string())?;
        required_artifacts.push(RequiredBrowserArtifact {
            owner: owner.clone(),
            module_key: binding.module_key.clone(),
            extension_name: binding.name.clone(),
            source: binding.source,
            family,
            runtime_roles: browser_runtime_roles_for_owner(&runtime.graph, &owner)?,
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
) -> std::result::Result<Vec<String>, String> {
    let mut roles = BTreeSet::new();
    let mut role_inputs = 0usize;
    for framework in &graph.frameworks {
        if &framework.binding.owner != owner {
            continue;
        }
        for role in framework.binding.module_assets.keys() {
            role_inputs = role_inputs
                .checked_add(1)
                .ok_or_else(|| "browser runtime role input count overflow".to_string())?;
            if role_inputs > MAX_BROWSER_RUNTIME_ITEMS {
                return Err(format!(
                    "browser runtime contains more than {MAX_BROWSER_RUNTIME_ITEMS} role inputs"
                ));
            }
            roles.insert(role.clone());
        }
    }
    Ok(roles.into_iter().collect())
}

fn build_browser_role_index(frameworks: &[BrowserFrameworkPlan]) -> BrowserRoleIndex {
    let entry_framework = frameworks
        .iter()
        .find(|framework| framework.contract.entry.is_some())
        .map(|framework| framework.id.clone());
    let mut module_providers: BTreeMap<String, Vec<BrowserFrameworkId>> = BTreeMap::new();
    for framework in frameworks {
        if framework.binding.module_assets.is_empty() {
            for role in framework.contract.js_modules.keys() {
                module_providers
                    .entry(role.clone())
                    .or_default()
                    .push(framework.id.clone());
            }
        } else {
            for role in framework.binding.module_assets.keys() {
                module_providers
                    .entry(role.clone())
                    .or_default()
                    .push(framework.id.clone());
            }
        }
    }
    BrowserRoleIndex {
        entry_framework,
        module_providers,
    }
}

fn browser_framework_plan_from_resolved(
    module_root: &str,
    resolved: &ResolvedExtension,
) -> Option<BrowserFrameworkPlan> {
    let web = resolved.web.as_ref()?;
    let module_key = resolved.owner.as_module().as_str().to_string();
    let module_root = normalize_vfs_path(module_root);
    let id = BrowserFrameworkId::new(module_key.clone(), resolved.manifest.name.clone());
    let contract = BrowserRuntimeContract {
        module_key: module_key.clone(),
        name: resolved.manifest.name.clone(),
        entry: web.entry.clone(),
        provider_role: web.provider_role,
        provider_roles: web.provider_roles.clone(),
        capabilities: web.capabilities.clone(),
        roles: web.js_modules.keys().cloned().collect(),
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
            owner: resolved.owner.clone(),
            module_assets: web.js_modules.clone(),
        },
    })
}

fn browser_wasm_binding_from_resolved(
    module_root: &str,
    resolved: &ResolvedExtension,
) -> Option<BrowserWasmExtensionBinding> {
    let wasm = resolved.wasm.as_ref()?;
    let module_root = normalize_vfs_path(module_root);
    Some(BrowserWasmExtensionBinding {
        name: resolved.manifest.name.clone(),
        module_key: resolved.owner.as_module().as_str().to_string(),
        module_root,
        owner: resolved.owner.clone(),
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

fn push_snapshot_mount(
    mounts: &mut Vec<BrowserSnapshotMount>,
    seen_mounts: &mut BTreeSet<BrowserSnapshotMount>,
    mount: BrowserSnapshotMount,
) -> std::result::Result<(), String> {
    if seen_mounts.contains(&mount) {
        return Ok(());
    }
    if mounts.len() >= MAX_BROWSER_SNAPSHOT_MOUNTS {
        return Err(format!(
            "browser snapshot plan exceeds the {MAX_BROWSER_SNAPSHOT_MOUNTS}-mount limit"
        ));
    }
    mounts
        .try_reserve(1)
        .map_err(|_| "failed to reserve browser snapshot mount".to_string())?;
    seen_mounts.insert(mount.clone());
    mounts.push(mount);
    Ok(())
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
    )?;
    Ok(())
}

fn ready_module_root_at(ready: &ReadyModule, module_root_base: &str) -> String {
    let module_dir = ready.module_dir();
    let module_dir = vo_module::schema::portable_relative_path_from_path(&module_dir)
        .expect("ReadyModule cache directories are canonical portable paths");
    if module_root_base.is_empty() {
        return normalize_vfs_path(&format!("/{module_dir}"));
    }
    join_vfs_path(module_root_base, &module_dir)
}

fn artifact_root_relative_path(artifact: &ResolvedArtifact) -> std::path::PathBuf {
    let cache_path = artifact.cache_relative_path();
    let portable = vo_module::schema::portable_relative_path_from_path(&cache_path)
        .expect("ResolvedArtifact cache paths are canonical portable paths");
    let relative = portable
        .strip_prefix("artifacts/")
        .expect("ResolvedArtifact cache paths use the artifacts root");
    std::path::PathBuf::from(relative)
}

fn asset_parent_ref(asset: &AssetRef) -> AssetRef {
    asset.parent()
}

fn asset_parent_relative_path(asset: &AssetRef) -> String {
    asset
        .portable_relative_path()
        .rsplit_once('/')
        .map(|(parent, _)| parent.to_string())
        .unwrap_or_default()
}

pub(crate) fn resolve_asset_ref(module_root: &str, asset: &AssetRef) -> String {
    let relative_path = asset.portable_relative_path();
    if relative_path.is_empty() {
        return match asset.root() {
            AssetRoot::ModuleRoot => normalize_vfs_path(module_root),
            AssetRoot::ArtifactRoot => join_vfs_path(module_root, "artifacts"),
        };
    }
    match asset.root() {
        AssetRoot::ModuleRoot => resolve_manifest_path(module_root, relative_path),
        AssetRoot::ArtifactRoot => {
            let artifact_root = join_vfs_path(module_root, "artifacts");
            join_vfs_path(&artifact_root, relative_path)
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
    let module = browser_runtime_project_root_module_from_vfs(&project_root)?;
    browser_runtime_plan_from_manifest(&project_root, &module, &manifest)
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
fn browser_runtime_project_root_module_from_vfs(
    project_root: &str,
) -> std::result::Result<ModulePath, String> {
    let mod_path = join_vfs_path(project_root, "vo.mod");
    let mod_content = read_browser_runtime_vfs_text(&mod_path)?;
    let mod_file = vo_module::schema::modfile::ModFile::parse(&mod_content)
        .map_err(|error| format!("parse {}: {}", mod_path, error))?;
    let module = mod_file
        .module
        .as_public()
        .ok_or_else(|| format!("{}: root module must be a github module path", project_root))?;
    Ok(module.clone())
}

#[cfg(target_arch = "wasm32")]
fn read_browser_runtime_vfs_text(path: &str) -> std::result::Result<String, String> {
    let (data, err) =
        vo_web_runtime_wasm::vfs::read_file_limited(path, vo_common::vfs::MAX_TEXT_FILE_BYTES);
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
    let mut claimed_paths = BTreeMap::new();
    let mut budget = BrowserSnapshotBudget::new(snapshot.mounts.len())?;
    for mount in &snapshot.mounts {
        validate_browser_snapshot_mount(mount)?;
        match mount.kind {
            BrowserSnapshotMountKind::Directory => {
                materialize_browser_snapshot_directory_mount_from_vfs(
                    mount,
                    runtime,
                    project_root,
                    entry_path,
                    &mut files,
                    &mut claimed_paths,
                    &mut budget,
                )?
            }
            BrowserSnapshotMountKind::File => materialize_browser_snapshot_file_mount_from_vfs(
                mount,
                runtime,
                project_root,
                entry_path,
                &mut files,
                &mut claimed_paths,
                &mut budget,
            )?,
        }
    }
    Ok(files)
}

#[cfg(target_arch = "wasm32")]
pub fn materialized_browser_artifacts_from_vfs(
    intent: &BrowserArtifactIntent,
    runtime: &BrowserRuntimePlan,
    mut retain_payload: impl FnMut(
        &MaterializedBrowserArtifact,
        Vec<u8>,
    ) -> std::result::Result<(), String>,
) -> std::result::Result<Vec<MaterializedBrowserArtifact>, String> {
    if intent.required_artifacts.len() > MAX_BROWSER_RUNTIME_ITEMS
        || runtime.graph.frameworks.len() > MAX_BROWSER_RUNTIME_ITEMS
    {
        return Err(format!(
            "browser runtime exceeds the {MAX_BROWSER_RUNTIME_ITEMS}-item artifact materialization limit"
        ));
    }
    let mut output = Vec::new();
    let mut identities = BTreeSet::new();
    let mut source_paths = BTreeSet::new();
    let mut total_bytes = 0usize;
    for artifact in &intent.required_artifacts {
        let family = match artifact.family {
            BrowserArtifactFamily::StandaloneWasm => {
                MaterializedBrowserArtifactFamily::StandaloneWasm
            }
            BrowserArtifactFamily::BindgenIsland => {
                MaterializedBrowserArtifactFamily::BindgenIsland
            }
        };
        materialize_browser_artifact_from_vfs(
            runtime,
            &artifact.module_key,
            &artifact.extension_name,
            family,
            MaterializedBrowserArtifactRole::WasmModule,
            &artifact.runtime_roles,
            &artifact.wasm.runtime_asset,
            &mut output,
            &mut identities,
            &mut source_paths,
            &mut total_bytes,
            &mut retain_payload,
        )?;
        if let Some(js_glue) = &artifact.js_glue {
            materialize_browser_artifact_from_vfs(
                runtime,
                &artifact.module_key,
                &artifact.extension_name,
                family,
                MaterializedBrowserArtifactRole::JavaScriptGlue,
                &artifact.runtime_roles,
                &js_glue.runtime_asset,
                &mut output,
                &mut identities,
                &mut source_paths,
                &mut total_bytes,
                &mut retain_payload,
            )?;
        }
    }
    for framework in &runtime.graph.frameworks {
        for (runtime_role, asset) in &framework.binding.module_assets {
            materialize_browser_artifact_from_vfs(
                runtime,
                &framework.module_key,
                &framework.id.extension_name,
                MaterializedBrowserArtifactFamily::FrameworkModule,
                MaterializedBrowserArtifactRole::JavaScriptModule,
                core::slice::from_ref(runtime_role),
                asset,
                &mut output,
                &mut identities,
                &mut source_paths,
                &mut total_bytes,
                &mut retain_payload,
            )?;
        }
    }
    output.sort_by(|left, right| {
        left.artifact_identity
            .cmp(&right.artifact_identity)
            .then_with(|| left.role.cmp(&right.role))
    });
    Ok(output)
}

#[cfg(target_arch = "wasm32")]
#[allow(clippy::too_many_arguments)]
fn materialize_browser_artifact_from_vfs(
    runtime: &BrowserRuntimePlan,
    module_key: &str,
    extension_name: &str,
    family: MaterializedBrowserArtifactFamily,
    role: MaterializedBrowserArtifactRole,
    runtime_roles: &[String],
    asset: &AssetRef,
    output: &mut Vec<MaterializedBrowserArtifact>,
    identities: &mut BTreeSet<[u8; 32]>,
    source_paths: &mut BTreeSet<String>,
    total_bytes: &mut usize,
    retain_payload: &mut dyn FnMut(
        &MaterializedBrowserArtifact,
        Vec<u8>,
    ) -> std::result::Result<(), String>,
) -> std::result::Result<(), String> {
    if output.len() >= MAX_BROWSER_RUNTIME_ITEMS {
        return Err(format!(
            "browser runtime contains more than {MAX_BROWSER_RUNTIME_ITEMS} materialized artifacts"
        ));
    }
    let source_path = normalize_vfs_path(&resolve_browser_snapshot_asset_path(runtime, asset)?);
    if !source_paths.insert(source_path.clone()) {
        return Err(format!(
            "multiple browser artifact identities resolve to {source_path:?}"
        ));
    }
    let bytes = read_browser_snapshot_vfs_file(&source_path, MAX_BROWSER_SNAPSHOT_FILE_BYTES)?;
    *total_bytes = total_bytes
        .checked_add(bytes.len())
        .ok_or_else(|| String::from("browser artifact byte count overflow"))?;
    if *total_bytes > MAX_BROWSER_SNAPSHOT_BYTES {
        return Err(format!(
            "browser artifacts exceed the {MAX_BROWSER_SNAPSHOT_BYTES}-byte aggregate limit"
        ));
    }
    let materialized = materialized_browser_artifact_from_bytes(
        module_key,
        extension_name,
        family,
        role,
        runtime_roles,
        asset,
        source_path.clone(),
        &bytes,
    )?;
    if !identities.insert(materialized.artifact_identity) {
        return Err(format!(
            "duplicate materialized browser artifact identity for {source_path:?}"
        ));
    }
    retain_payload(&materialized, bytes)?;
    output
        .try_reserve(1)
        .map_err(|_| String::from("failed to reserve materialized browser artifacts"))?;
    output.push(materialized);
    Ok(())
}

#[cfg(target_arch = "wasm32")]
struct VfsSnapshotDirectoryMaterializer<'a> {
    mount: &'a BrowserSnapshotMount,
    runtime: &'a BrowserRuntimePlan,
    project_root: Option<&'a str>,
    entry_path: &'a str,
    files: &'a mut Vec<BrowserSnapshotFile>,
    claimed_paths: &'a mut BTreeMap<String, String>,
    budget: &'a mut BrowserSnapshotBudget,
}

#[cfg(target_arch = "wasm32")]
impl VfsSnapshotDirectoryMaterializer<'_> {
    fn walk(&mut self, dir: &str, depth: usize) -> std::result::Result<(), String> {
        self.budget.validate_depth(depth, dir)?;
        let (entries, err) = vo_web_runtime_wasm::vfs::read_dir(dir);
        if let Some(error) = err {
            return Err(format!("read dir '{}': {}", dir, error));
        }
        if entries.len() > self.budget.remaining_entries() {
            return Err(format!(
                "browser snapshot contains more than {MAX_BROWSER_SNAPSHOT_ENTRIES} filesystem entries while traversing {dir:?}"
            ));
        }
        for (name, is_dir, _mode) in entries {
            let full = if dir == "/" {
                format!("/{}", name)
            } else {
                format!("{}/{}", dir, name)
            };
            self.budget.record_entry(&full)?;
            if is_dir {
                let child_depth = depth.checked_add(1).ok_or_else(|| {
                    format!("browser snapshot directory depth overflow at {full:?}")
                })?;
                self.walk(&full, child_depth)?;
                continue;
            }
            let output_path = materialized_browser_snapshot_path(
                self.mount,
                self.runtime,
                self.project_root,
                self.entry_path,
                &full,
            )?;
            let source_path = normalize_vfs_path(&full);
            if claim_browser_snapshot_output(self.claimed_paths, &output_path, &source_path)? {
                let read_limit = self.budget.next_file_limit(&source_path)?;
                let bytes = read_browser_snapshot_vfs_file(&source_path, read_limit)?;
                self.budget.record_file(&source_path, bytes.len())?;
                self.files.try_reserve(1).map_err(|_| {
                    format!("failed to reserve browser snapshot entry for {source_path:?}")
                })?;
                self.files.push(BrowserSnapshotFile {
                    path: output_path,
                    bytes,
                });
            }
        }
        Ok(())
    }
}

#[cfg(target_arch = "wasm32")]
fn materialize_browser_snapshot_directory_mount_from_vfs(
    mount: &BrowserSnapshotMount,
    runtime: &BrowserRuntimePlan,
    project_root: Option<&str>,
    entry_path: &str,
    files: &mut Vec<BrowserSnapshotFile>,
    claimed_paths: &mut BTreeMap<String, String>,
    budget: &mut BrowserSnapshotBudget,
) -> std::result::Result<(), String> {
    let root =
        resolve_browser_snapshot_source_path(runtime, &mount.source, project_root, entry_path)?;
    VfsSnapshotDirectoryMaterializer {
        mount,
        runtime,
        project_root,
        entry_path,
        files,
        claimed_paths,
        budget,
    }
    .walk(&root, 0)
}

#[cfg(target_arch = "wasm32")]
fn materialize_browser_snapshot_file_mount_from_vfs(
    mount: &BrowserSnapshotMount,
    runtime: &BrowserRuntimePlan,
    project_root: Option<&str>,
    entry_path: &str,
    files: &mut Vec<BrowserSnapshotFile>,
    claimed_paths: &mut BTreeMap<String, String>,
    budget: &mut BrowserSnapshotBudget,
) -> std::result::Result<(), String> {
    let full =
        resolve_browser_snapshot_source_path(runtime, &mount.source, project_root, entry_path)?;
    let output_path =
        materialized_browser_snapshot_path(mount, runtime, project_root, entry_path, &full)?;
    let source_path = normalize_vfs_path(&full);
    budget.record_entry(&source_path)?;
    if claim_browser_snapshot_output(claimed_paths, &output_path, &source_path)? {
        let read_limit = budget.next_file_limit(&source_path)?;
        let bytes = read_browser_snapshot_vfs_file(&source_path, read_limit)?;
        budget.record_file(&source_path, bytes.len())?;
        files
            .try_reserve(1)
            .map_err(|_| format!("failed to reserve browser snapshot entry for {source_path:?}"))?;
        files.push(BrowserSnapshotFile {
            path: output_path,
            bytes,
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
    let output_path = if mount.virtual_prefix.is_empty() {
        relative_path
    } else if relative_path.is_empty() {
        mount.virtual_prefix.clone()
    } else {
        format!(
            "{}/{}",
            mount.virtual_prefix,
            relative_path.trim_start_matches('/'),
        )
    };
    canonical_browser_snapshot_output_path(&output_path)
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
    let module_root = browser_runtime_module_root_for_owner(runtime, asset.owner())?;
    Ok(resolve_asset_ref(&module_root, asset))
}

pub(crate) fn browser_runtime_module_root_for_owner(
    runtime: &BrowserRuntimePlan,
    owner: &ExtensionOwner,
) -> std::result::Result<String, String> {
    for framework in &runtime.graph.frameworks {
        if &framework.binding.owner == owner {
            return Ok(framework.module_root.clone());
        }
    }
    for binding in &runtime.wasm_bindings {
        if &binding.owner == owner {
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
            let prefix_asset = asset.with_relative_path(strip_prefix)?;
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
fn read_browser_snapshot_vfs_file(
    path: &str,
    max_bytes: usize,
) -> std::result::Result<Vec<u8>, String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file_limited(path, max_bytes);
    if let Some(error) = err {
        return Err(format!("read file '{}': {}", path, error));
    }
    Ok(data)
}

#[cfg(test)]
mod tests;
