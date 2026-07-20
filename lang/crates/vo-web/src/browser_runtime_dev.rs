use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

use toml::Value;
use vo_common::vfs::{read_binary_file, read_text_file, RealFs};
use vo_module::project;
use vo_module::resolved_extension::{AssetRef, AssetRoot};
use vo_module::schema::lockfile::LockedModule;

use crate::browser_runtime::{
    browser_runtime_module_root_for_owner, browser_runtime_plan_from_manifest,
    canonical_browser_snapshot_output_path, claim_browser_snapshot_output,
    merge_browser_runtime_plans, plan_ready_browser_runtime_at, validate_browser_snapshot_mount,
    BrowserArtifactFamily, BrowserArtifactIntent, BrowserArtifactSource, BrowserRuntimePlan,
    BrowserSnapshotBudget, BrowserSnapshotFile, BrowserSnapshotMount, BrowserSnapshotMountKind,
    BrowserSnapshotPlan, BrowserSnapshotSourceRef, RequiredBrowserArtifact,
    MAX_BROWSER_RUNTIME_ITEMS, MAX_BROWSER_SNAPSHOT_FILE_BYTES as MAX_GENERATED_OUTPUT_BYTES,
};

const BROWSER_WASM_TARGET: &str = "wasm32-unknown-unknown";
const MAX_BROWSER_BUILD_SCAN_ENTRIES: usize = 100_000;
const MAX_BROWSER_BUILD_CANDIDATES: usize = 10_000;
static GENERATED_OUTPUT_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BrowserArtifactPlan {
    pub actions: Vec<ArtifactActionSpec>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArtifactActionSpec {
    EnsureStandaloneWasm(EnsureStandaloneWasmAction),
    EnsurePkgIsland(EnsurePkgIslandAction),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnsureStandaloneWasmAction {
    pub module_key: String,
    pub extension_name: String,
    pub rust_root: PathBuf,
    pub crate_root: PathBuf,
    pub workspace_root: PathBuf,
    pub build_output: PathBuf,
    pub runtime_wasm_path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnsurePkgIslandAction {
    pub module_key: String,
    pub extension_name: String,
    pub rust_root: PathBuf,
    pub crate_root: PathBuf,
    pub out_dir: PathBuf,
    pub out_name: String,
    pub runtime_wasm_path: PathBuf,
    pub runtime_js_path: Option<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct WasmBuildCandidate {
    crate_root: PathBuf,
    package_name: Option<String>,
    lib_name: Option<String>,
}

/// Debug-only helper for local extension/framework development.
///
/// This reads extension metadata from the local project's `vo.mod` and bypasses
/// published dependency resolution. Normal Studio/example/runtime flows must
/// use locked published modules instead.
pub fn debug_local_project_browser_runtime_plan_from_fs(
    project_root: &Path,
) -> Result<BrowserRuntimePlan, String> {
    let project_root = canonical_existing_directory(project_root, "local project root")?;
    let mod_file = project::read_mod_file_stable(&project_root)
        .map_err(|error| format!("{}: {}", project_root.join("vo.mod").display(), error))?;
    let Some(manifest) = mod_file.extension.as_ref() else {
        return Ok(BrowserRuntimePlan::default());
    };
    let module = mod_file.module.as_public().ok_or_else(|| {
        format!(
            "{}: root module must be a github module path",
            project_root.display(),
        )
    })?;
    let project_vfs_root = browser_snapshot_vfs_path_from_fs(&project_root)?;
    browser_runtime_plan_from_manifest(&project_vfs_root, Some(module.as_str()), manifest)
}

/// Native Studio GUI runtime discovery for local development paths.
///
/// The native runner already knows exactly which native extensions were linked
/// into the compiled program. Use those manifests as the local source of truth,
/// then merge any remaining published locked-module runtime metadata from the
/// shared module cache.
pub fn native_gui_browser_runtime_plan_from_fs(
    local_extension_manifests: &[PathBuf],
    locked_modules: &[LockedModule],
    mod_cache_root: &Path,
) -> Result<BrowserRuntimePlan, String> {
    if local_extension_manifests.len() > MAX_BROWSER_RUNTIME_ITEMS {
        return Err(format!(
            "native browser runtime contains more than {MAX_BROWSER_RUNTIME_ITEMS} local extension manifests"
        ));
    }
    if locked_modules.len() > MAX_BROWSER_RUNTIME_ITEMS {
        return Err(format!(
            "native browser runtime contains more than {MAX_BROWSER_RUNTIME_ITEMS} locked modules"
        ));
    }
    let published_plan_count = usize::from(!locked_modules.is_empty());
    let plan_input_count = local_extension_manifests
        .len()
        .checked_add(published_plan_count)
        .ok_or_else(|| "native browser runtime plan input count overflow".to_string())?;
    if plan_input_count > MAX_BROWSER_RUNTIME_ITEMS {
        return Err(format!(
            "native browser runtime contains more than {MAX_BROWSER_RUNTIME_ITEMS} plan inputs"
        ));
    }
    let mut plans = Vec::new();
    plans
        .try_reserve(plan_input_count)
        .map_err(|_| "failed to reserve native browser runtime plans".to_string())?;
    if !locked_modules.is_empty() {
        plans.push(published_browser_runtime_plan_from_fs(
            locked_modules,
            mod_cache_root,
        )?);
    }
    let mut seen_module_roots = BTreeSet::new();

    for manifest_path in local_extension_manifests {
        let manifest_path = canonical_existing_regular_file(manifest_path, "extension manifest")?;
        let module_root = manifest_path.parent().ok_or_else(|| {
            format!(
                "extension manifest path has no parent: {}",
                manifest_path.display()
            )
        })?;
        let expected_manifest = module_root.join("vo.mod");
        let expected_manifest =
            canonical_existing_regular_file(&expected_manifest, "extension module manifest")?;
        if manifest_path != expected_manifest {
            return Err(format!(
                "extension manifest path must identify {}: {}",
                module_root.join("vo.mod").display(),
                manifest_path.display()
            ));
        }
        let module_root = module_root.to_path_buf();
        if !seen_module_roots.insert(module_root.clone()) {
            continue;
        }
        let mod_file = project::read_mod_file_stable(&module_root)
            .map_err(|error| format!("{}: {}", module_root.join("vo.mod").display(), error))?;
        let manifest = mod_file.extension.as_ref().ok_or_else(|| {
            format!(
                "{}: missing [extension] metadata for native GUI runtime",
                module_root.join("vo.mod").display()
            )
        })?;
        let module = mod_file.module.as_public().ok_or_else(|| {
            format!(
                "{}: extension module must be a github module path",
                module_root.display()
            )
        })?;
        let module_vfs_root = browser_snapshot_vfs_path_from_fs(&module_root)?;
        plans.push(browser_runtime_plan_from_manifest(
            &module_vfs_root,
            Some(module.as_str()),
            manifest,
        )?);
    }

    merge_browser_runtime_plans(plans)
}

pub fn locked_browser_runtime_plan_from_fs(
    locked_modules: &[LockedModule],
    mod_cache_root: &Path,
) -> Result<BrowserRuntimePlan, String> {
    if locked_modules.is_empty() {
        return Ok(BrowserRuntimePlan::default());
    }
    if locked_modules.len() > MAX_BROWSER_RUNTIME_ITEMS {
        return Err(format!(
            "browser runtime contains more than {MAX_BROWSER_RUNTIME_ITEMS} locked modules"
        ));
    }
    let mod_cache_root = canonical_existing_directory(mod_cache_root, "module cache root")?;
    let ready = vo_module::readiness::check_project_readiness(
        &RealFs::new(&mod_cache_root),
        locked_modules,
        BROWSER_WASM_TARGET,
    )
    .map_err(|error| error.to_string())?;
    let cache_vfs_root = browser_snapshot_vfs_path_from_fs(&mod_cache_root)?;
    plan_ready_browser_runtime_at(&ready, &cache_vfs_root)
}

/// Published runtime discovery for normal browser execution paths.
///
/// This resolves browser runtime state strictly from locked published modules
/// that have already been installed into the module cache. It must not merge
/// debug-only local project manifests.
pub fn published_browser_runtime_plan_from_fs(
    locked_modules: &[LockedModule],
    mod_cache_root: &Path,
) -> Result<BrowserRuntimePlan, String> {
    locked_browser_runtime_plan_from_fs(locked_modules, mod_cache_root)
}

pub fn browser_artifact_plan_from_fs(
    intent: &BrowserArtifactIntent,
    runtime: &BrowserRuntimePlan,
) -> Result<BrowserArtifactPlan, String> {
    if intent.required_artifacts.len() > MAX_BROWSER_RUNTIME_ITEMS {
        return Err(format!(
            "browser artifact intent contains more than {MAX_BROWSER_RUNTIME_ITEMS} required artifacts"
        ));
    }
    let mut actions = Vec::new();
    actions
        .try_reserve(intent.required_artifacts.len())
        .map_err(|_| "failed to reserve browser artifact actions".to_string())?;
    let mut seen = BTreeSet::new();
    for artifact in &intent.required_artifacts {
        if artifact.source != BrowserArtifactSource::LocalManifest {
            continue;
        }
        if !seen.insert((artifact.owner.clone(), artifact.family)) {
            continue;
        }
        let module_root = browser_snapshot_fs_path_from_vfs(
            &browser_runtime_module_root_for_owner(runtime, &artifact.owner)?,
        )?;
        let rust_root = module_root.join("rust");
        if !regular_file_exists(&rust_root.join("Cargo.toml"))? {
            continue;
        }
        match artifact.family {
            BrowserArtifactFamily::StandaloneWasm => {
                actions.push(ArtifactActionSpec::EnsureStandaloneWasm(
                    plan_standalone_wasm_action(artifact, &module_root, &rust_root)?,
                ))
            }
            BrowserArtifactFamily::BindgenIsland => {
                if artifact.runtime_roles.is_empty() {
                    continue;
                }
                actions.push(ArtifactActionSpec::EnsurePkgIsland(plan_pkg_island_action(
                    artifact,
                    &module_root,
                    &rust_root,
                )?));
            }
        }
    }
    Ok(BrowserArtifactPlan { actions })
}

pub fn execute_browser_artifact_plan(plan: &BrowserArtifactPlan) -> Result<(), String> {
    if plan.actions.len() > MAX_BROWSER_RUNTIME_ITEMS {
        return Err(format!(
            "browser artifact plan contains more than {MAX_BROWSER_RUNTIME_ITEMS} actions"
        ));
    }
    let mut output_owners = BTreeMap::new();
    for (action_index, action) in plan.actions.iter().enumerate() {
        let (wasm_path, js_path) = match action {
            ArtifactActionSpec::EnsureStandaloneWasm(action) => (&action.runtime_wasm_path, None),
            ArtifactActionSpec::EnsurePkgIsland(action) => {
                (&action.runtime_wasm_path, action.runtime_js_path.as_ref())
            }
        };
        for output_path in std::iter::once(wasm_path).chain(js_path) {
            let output_key = browser_snapshot_vfs_path_from_fs(output_path)?;
            if let Some(previous_index) = output_owners.insert(output_key, action_index) {
                return Err(format!(
                    "browser artifact actions {previous_index} and {action_index} reuse output path {}",
                    output_path.display()
                ));
            }
        }
    }
    for action in &plan.actions {
        match action {
            ArtifactActionSpec::EnsureStandaloneWasm(action) => {
                execute_standalone_wasm_action(action)?
            }
            ArtifactActionSpec::EnsurePkgIsland(action) => execute_pkg_island_action(action)?,
        }
    }
    Ok(())
}

pub fn materialize_browser_snapshot_from_fs(
    snapshot: &BrowserSnapshotPlan,
    runtime: &BrowserRuntimePlan,
    project_root: Option<&Path>,
    entry_path: &Path,
) -> Result<Vec<BrowserSnapshotFile>, String> {
    let mut files = Vec::new();
    let mut claimed_paths = BTreeMap::new();
    let mut budget = BrowserSnapshotBudget::new(snapshot.mounts.len())?;
    for mount in &snapshot.mounts {
        validate_browser_snapshot_mount(mount)?;
        match mount.kind {
            BrowserSnapshotMountKind::Directory => materialize_snapshot_directory_mount_from_fs(
                mount,
                runtime,
                project_root,
                entry_path,
                &mut files,
                &mut claimed_paths,
                &mut budget,
            )?,
            BrowserSnapshotMountKind::File => materialize_snapshot_file_mount_from_fs(
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

/// Convert a native filesystem path into the canonical path understood by the
/// renderer's platform-neutral VFS. The same mapping must be used for the VFS
/// root and every absolute snapshot file path.
pub fn browser_snapshot_vfs_path_from_fs(path: &Path) -> Result<String, String> {
    let path = normalize_snapshot_output_path(path)?;
    if path == "/" {
        return Ok(path);
    }
    canonical_browser_snapshot_output_path(&path)
}

fn browser_snapshot_fs_path_from_vfs(path: &str) -> Result<PathBuf, String> {
    let canonical = if path == "/" {
        path.to_string()
    } else {
        canonical_browser_snapshot_output_path(path)?
    };
    #[cfg(windows)]
    let canonical = windows_snapshot_fs_path(&canonical)?;
    Ok(PathBuf::from(canonical))
}

fn plan_standalone_wasm_action(
    artifact: &RequiredBrowserArtifact,
    module_root: &Path,
    rust_root: &Path,
) -> Result<EnsureStandaloneWasmAction, String> {
    let build =
        select_wasm_build_candidate(rust_root, &artifact.extension_name, "wasm-standalone")?;
    let workspace_root = select_cargo_workspace_root(rust_root, &build.crate_root)?;
    let build_output = standalone_wasm_build_output_path(&workspace_root, &build)?;
    Ok(EnsureStandaloneWasmAction {
        module_key: artifact.module_key.clone(),
        extension_name: artifact.extension_name.clone(),
        rust_root: rust_root.to_path_buf(),
        crate_root: build.crate_root,
        workspace_root,
        build_output,
        runtime_wasm_path: resolve_fs_asset_path(module_root, &artifact.wasm.runtime_asset),
    })
}

fn plan_pkg_island_action(
    artifact: &RequiredBrowserArtifact,
    module_root: &Path,
    rust_root: &Path,
) -> Result<EnsurePkgIslandAction, String> {
    let build = select_wasm_build_candidate(rust_root, &artifact.extension_name, "wasm-island")?;
    let out_dir = rust_root.join("pkg-island");
    let out_name = format!("{}_island", artifact.extension_name);
    let runtime_wasm_path = resolve_fs_asset_path(module_root, &artifact.wasm.runtime_asset);
    let runtime_js_path = artifact
        .js_glue
        .as_ref()
        .map(|js_glue| resolve_fs_asset_path(module_root, &js_glue.runtime_asset));
    Ok(EnsurePkgIslandAction {
        module_key: artifact.module_key.clone(),
        extension_name: artifact.extension_name.clone(),
        rust_root: rust_root.to_path_buf(),
        crate_root: build.crate_root,
        out_dir,
        out_name,
        runtime_wasm_path,
        runtime_js_path,
    })
}

fn execute_standalone_wasm_action(action: &EnsureStandaloneWasmAction) -> Result<(), String> {
    // Cargo owns the dependency graph and fingerprint. A local mtime shortcut
    // can miss build scripts, workspace members, path dependencies, and config.
    eprintln!(
        "[vo-web] building standalone wasm for {} from {}",
        action.extension_name,
        action.crate_root.display(),
    );
    let status = Command::new("cargo")
        .current_dir(&action.crate_root)
        .args([
            "build",
            "--target",
            BROWSER_WASM_TARGET,
            "--release",
            "--no-default-features",
            "--features",
            "wasm-standalone",
        ])
        .status()
        .map_err(|error| format!("cargo: {}", error))?;
    if !status.success() {
        return Err(format!(
            "failed to build standalone wasm for {}: cargo exited with {}",
            action.extension_name, status,
        ));
    }
    sync_standalone_wasm_output(&action.build_output, &action.runtime_wasm_path)
}

fn execute_pkg_island_action(action: &EnsurePkgIslandAction) -> Result<(), String> {
    // wasm-pack must make the same authoritative freshness decision as Cargo
    // and regenerate its JS/wasm pair as one build operation.
    eprintln!(
        "[vo-web] building render-island wasm for {} from {}",
        action.extension_name,
        action.crate_root.display(),
    );
    let status = Command::new("wasm-pack")
        .arg("build")
        .args(["--target", "web", "--out-dir"])
        .arg(&action.out_dir)
        .arg("--out-name")
        .arg(&action.out_name)
        .arg("--release")
        .arg(&action.crate_root)
        .args(["--", "--no-default-features", "--features", "wasm-island"])
        .status()
        .map_err(|error| format!("wasm-pack: {}", error))?;
    if !status.success() {
        return Err(format!(
            "failed to build render-island wasm for {}: wasm-pack exited with {}",
            action.extension_name, status,
        ));
    }
    let build_wasm_path = action.out_dir.join(format!("{}_bg.wasm", action.out_name));
    sync_generated_output(
        &build_wasm_path,
        &action.runtime_wasm_path,
        "render-island wasm",
    )?;
    if let Some(runtime_js_path) = &action.runtime_js_path {
        let build_js_path = action.out_dir.join(format!("{}.js", action.out_name));
        sync_generated_output(&build_js_path, runtime_js_path, "render-island js glue")?;
    }
    Ok(())
}

struct FsSnapshotDirectoryMaterializer<'a> {
    mount: &'a BrowserSnapshotMount,
    runtime: &'a BrowserRuntimePlan,
    project_root: Option<&'a Path>,
    entry_path: &'a Path,
    files: &'a mut Vec<BrowserSnapshotFile>,
    claimed_paths: &'a mut BTreeMap<String, String>,
    budget: &'a mut BrowserSnapshotBudget,
}

impl FsSnapshotDirectoryMaterializer<'_> {
    fn walk(&mut self, dir: &Path, depth: usize) -> Result<(), String> {
        let dir_label = normalize_snapshot_output_path(dir)?;
        self.budget.validate_depth(depth, &dir_label)?;
        let mut entries = Vec::new();
        entries
            .try_reserve(self.budget.remaining_entries().min(64))
            .map_err(|_| format!("failed to reserve directory entries for {dir_label:?}"))?;
        for entry in fs::read_dir(dir).map_err(|error| format!("{}: {}", dir.display(), error))? {
            let entry = entry.map_err(|error| format!("{}: {}", dir.display(), error))?;
            let path = entry.path();
            let source_path = normalize_snapshot_output_path(&path)?;
            self.budget.record_entry(&source_path)?;
            entries
                .try_reserve(1)
                .map_err(|_| format!("failed to reserve directory entry for {source_path:?}"))?;
            entries.push((source_path, entry));
        }
        entries.sort_by(|left, right| left.0.as_bytes().cmp(right.0.as_bytes()));

        for (source_path, entry) in entries {
            let path = entry.path();
            let file_type = entry
                .file_type()
                .map_err(|error| format!("{}: {}", path.display(), error))?;
            if file_type.is_dir() {
                let child_depth = depth.checked_add(1).ok_or_else(|| {
                    format!("browser snapshot directory depth overflow at {source_path:?}")
                })?;
                self.walk(&path, child_depth)?;
                continue;
            }
            if !file_type.is_file() {
                continue;
            }
            let output_path = materialized_snapshot_path_from_fs(
                self.mount,
                self.runtime,
                self.project_root,
                self.entry_path,
                &path,
            )?;
            if claim_browser_snapshot_output(self.claimed_paths, &output_path, &source_path)? {
                let read_limit = self.budget.next_file_limit(&source_path)?;
                let bytes = read_binary_file(&path, read_limit)
                    .map_err(|error| format!("{}: {}", path.display(), error))?;
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

fn materialize_snapshot_directory_mount_from_fs(
    mount: &BrowserSnapshotMount,
    runtime: &BrowserRuntimePlan,
    project_root: Option<&Path>,
    entry_path: &Path,
    files: &mut Vec<BrowserSnapshotFile>,
    claimed_paths: &mut BTreeMap<String, String>,
    budget: &mut BrowserSnapshotBudget,
) -> Result<(), String> {
    let root =
        resolve_snapshot_source_path_from_fs(runtime, &mount.source, project_root, entry_path)?;
    FsSnapshotDirectoryMaterializer {
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

fn materialize_snapshot_file_mount_from_fs(
    mount: &BrowserSnapshotMount,
    runtime: &BrowserRuntimePlan,
    project_root: Option<&Path>,
    entry_path: &Path,
    files: &mut Vec<BrowserSnapshotFile>,
    claimed_paths: &mut BTreeMap<String, String>,
    budget: &mut BrowserSnapshotBudget,
) -> Result<(), String> {
    let path =
        resolve_snapshot_source_path_from_fs(runtime, &mount.source, project_root, entry_path)?;
    let output_path =
        materialized_snapshot_path_from_fs(mount, runtime, project_root, entry_path, &path)?;
    let source_path = normalize_snapshot_output_path(&path)?;
    budget.record_entry(&source_path)?;
    if claim_browser_snapshot_output(claimed_paths, &output_path, &source_path)? {
        let read_limit = budget.next_file_limit(&source_path)?;
        let bytes = read_binary_file(&path, read_limit)
            .map_err(|error| format!("{}: {}", path.display(), error))?;
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

fn materialized_snapshot_path_from_fs(
    mount: &BrowserSnapshotMount,
    runtime: &BrowserRuntimePlan,
    project_root: Option<&Path>,
    entry_path: &Path,
    full_path: &Path,
) -> Result<String, String> {
    let relative_path = if let Some(strip_prefix) = &mount.strip_prefix {
        let resolved_prefix = resolve_snapshot_strip_prefix_from_fs(
            runtime,
            &mount.source,
            strip_prefix,
            project_root,
            entry_path,
        )?;
        strip_snapshot_prefix_from_fs(full_path, &resolved_prefix)?
    } else {
        normalize_snapshot_output_path(full_path)?
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

fn resolve_snapshot_source_path_from_fs(
    runtime: &BrowserRuntimePlan,
    source: &BrowserSnapshotSourceRef,
    project_root: Option<&Path>,
    entry_path: &Path,
) -> Result<PathBuf, String> {
    match source {
        BrowserSnapshotSourceRef::ProjectRoot => project_root
            .map(Path::to_path_buf)
            .ok_or_else(|| "browser snapshot project root is missing".to_string()),
        BrowserSnapshotSourceRef::EntryFile => Ok(entry_path.to_path_buf()),
        BrowserSnapshotSourceRef::AssetPath(asset) => {
            resolve_snapshot_asset_path_from_fs(runtime, asset)
        }
    }
}

fn resolve_snapshot_asset_path_from_fs(
    runtime: &BrowserRuntimePlan,
    asset: &AssetRef,
) -> Result<PathBuf, String> {
    let module_root = browser_snapshot_fs_path_from_vfs(&browser_runtime_module_root_for_owner(
        runtime,
        asset.owner(),
    )?)?;
    Ok(resolve_fs_asset_path(&module_root, asset))
}

fn resolve_snapshot_strip_prefix_from_fs(
    runtime: &BrowserRuntimePlan,
    source: &BrowserSnapshotSourceRef,
    strip_prefix: &str,
    project_root: Option<&Path>,
    entry_path: &Path,
) -> Result<PathBuf, String> {
    match source {
        BrowserSnapshotSourceRef::ProjectRoot => Ok(project_root
            .ok_or_else(|| "browser snapshot project root is missing".to_string())?
            .join(strip_prefix)),
        BrowserSnapshotSourceRef::EntryFile => {
            if strip_prefix.is_empty() {
                Ok(entry_path.to_path_buf())
            } else {
                Err("browser snapshot entry-file strip prefix is unsupported".to_string())
            }
        }
        BrowserSnapshotSourceRef::AssetPath(asset) => {
            let prefix_asset = asset.with_relative_path(strip_prefix)?;
            resolve_snapshot_asset_path_from_fs(runtime, &prefix_asset)
        }
    }
}

fn strip_snapshot_prefix_from_fs(path: &Path, prefix: &Path) -> Result<String, String> {
    if path == prefix {
        return Ok(String::new());
    }
    path.strip_prefix(prefix)
        .map_err(|error| format!("{}: {}", path.display(), error))
        .and_then(normalize_snapshot_output_path)
}

fn resolve_fs_asset_path(module_root: &Path, asset: &AssetRef) -> PathBuf {
    match asset.root() {
        AssetRoot::ModuleRoot => module_root.join(asset.relative_path()),
        AssetRoot::ArtifactRoot => module_root.join("artifacts").join(asset.relative_path()),
    }
}

fn normalize_snapshot_output_path(path: &Path) -> Result<String, String> {
    let path = path.to_str().ok_or_else(|| {
        format!(
            "browser snapshot path is not valid UTF-8: {}",
            path.display()
        )
    })?;
    #[cfg(windows)]
    let path = windows_snapshot_vfs_path(path)?;
    #[cfg(not(windows))]
    let path = path.to_string();
    Ok(path)
}

#[cfg(any(windows, test))]
fn windows_snapshot_vfs_path(path: &str) -> Result<String, String> {
    let normalized = path.replace('\\', "/");
    let normalized = if let Some(rest) = normalized.strip_prefix("//?/UNC/") {
        validate_windows_unc_path(rest, path)?;
        return Ok(format!("/UNC/{rest}"));
    } else if let Some(rest) = normalized.strip_prefix("//?/") {
        let bytes = rest.as_bytes();
        if !bytes.first().is_some_and(|byte| byte.is_ascii_alphabetic())
            || bytes.get(1) != Some(&b':')
            || bytes.get(2) != Some(&b'/')
        {
            return Err(format!("unsupported Windows snapshot path {path:?}"));
        }
        rest.to_string()
    } else if let Some(rest) = normalized.strip_prefix("//") {
        if rest.starts_with("./") || rest.starts_with("?/") {
            return Err(format!("unsupported Windows snapshot path {path:?}"));
        }
        validate_windows_unc_path(rest, path)?;
        return Ok(format!("/UNC/{rest}"));
    } else {
        normalized
    };

    if normalized.starts_with('/') {
        return Err(format!(
            "Windows root-relative snapshot path is ambiguous: {path:?}"
        ));
    }
    let bytes = normalized.as_bytes();
    if bytes.get(1) == Some(&b':') {
        if !bytes[0].is_ascii_alphabetic() || bytes.get(2) != Some(&b'/') {
            return Err(format!("invalid Windows snapshot path {path:?}"));
        }
        return Ok(format!("/{normalized}"));
    }
    Ok(normalized)
}

#[cfg(any(windows, test))]
fn validate_windows_unc_path(rest: &str, original: &str) -> Result<(), String> {
    let mut components = rest.split('/');
    let server = components.next().filter(|component| !component.is_empty());
    let share = components.next().filter(|component| !component.is_empty());
    if server.is_none() || share.is_none() {
        return Err(format!("invalid Windows UNC snapshot path {original:?}"));
    }
    Ok(())
}

#[cfg(any(windows, test))]
fn windows_snapshot_fs_path(path: &str) -> Result<String, String> {
    if let Some(rest) = path.strip_prefix("/UNC/") {
        let mut components = rest.split('/');
        let server = components.next().filter(|component| !component.is_empty());
        let share = components.next().filter(|component| !component.is_empty());
        if server.is_none() || share.is_none() {
            return Err(format!("invalid Windows UNC snapshot path {path:?}"));
        }
        return Ok(format!("//{rest}"));
    }
    let bytes = path.as_bytes();
    if bytes.first() == Some(&b'/') && bytes.get(2) == Some(&b':') {
        if !bytes[1].is_ascii_alphabetic() || (bytes.len() > 3 && bytes.get(3) != Some(&b'/')) {
            return Err(format!("invalid Windows drive snapshot path {path:?}"));
        }
        return if bytes.len() == 3 {
            Ok(format!("{}/", &path[1..3]))
        } else {
            Ok(path[1..].to_string())
        };
    }
    if path.starts_with('/') {
        return Err(format!(
            "Windows snapshot path has no drive or UNC root: {path:?}"
        ));
    }
    Ok(path.to_string())
}

fn inspect_wasm_build_candidate(
    crate_root: &Path,
    required_feature: &str,
) -> Result<Option<WasmBuildCandidate>, String> {
    let cargo_toml = crate_root.join("Cargo.toml");
    if !regular_file_exists(&cargo_toml)? {
        return Ok(None);
    }
    let content = read_text_file(&cargo_toml)
        .map_err(|error| format!("{}: {}", cargo_toml.display(), error))?;
    let value: Value =
        toml::from_str(&content).map_err(|error| format!("{}: {}", cargo_toml.display(), error))?;
    let package_name = value
        .get("package")
        .and_then(Value::as_table)
        .and_then(|table| table.get("name"))
        .and_then(Value::as_str)
        .map(str::to_string);
    let lib_name = value
        .get("lib")
        .and_then(Value::as_table)
        .and_then(|table| table.get("name"))
        .and_then(Value::as_str)
        .map(str::to_string);
    let has_feature = value
        .get("features")
        .and_then(Value::as_table)
        .map(|table| table.contains_key(required_feature))
        .unwrap_or(false);
    Ok(has_feature.then_some(WasmBuildCandidate {
        crate_root: crate_root.to_path_buf(),
        package_name,
        lib_name,
    }))
}

fn read_sorted_directory_entries(
    dir: &Path,
    entries_seen: &mut usize,
    max_entries: usize,
    context: &str,
) -> Result<Vec<(String, fs::DirEntry)>, String> {
    let mut entries = Vec::new();
    for entry in fs::read_dir(dir).map_err(|error| format!("{}: {}", dir.display(), error))? {
        let entry = entry.map_err(|error| format!("{}: {}", dir.display(), error))?;
        *entries_seen = entries_seen
            .checked_add(1)
            .ok_or_else(|| format!("{context} entry count overflow under {}", dir.display()))?;
        if *entries_seen > max_entries {
            return Err(format!(
                "{context} contains more than {max_entries} filesystem entries under {}",
                dir.display()
            ));
        }
        let name = entry.file_name().into_string().map_err(|name| {
            format!(
                "{context} contains a non-UTF-8 entry name under {}: {:?}",
                dir.display(),
                name
            )
        })?;
        entries
            .try_reserve(1)
            .map_err(|_| format!("failed to reserve {context} directory entries"))?;
        entries.push((name, entry));
    }
    entries.sort_by(|left, right| left.0.as_bytes().cmp(right.0.as_bytes()));
    Ok(entries)
}

fn push_wasm_build_candidate(
    candidates: &mut Vec<WasmBuildCandidate>,
    candidate: WasmBuildCandidate,
) -> Result<(), String> {
    if candidates.len() >= MAX_BROWSER_BUILD_CANDIDATES {
        return Err(format!(
            "browser wasm build discovery contains more than {MAX_BROWSER_BUILD_CANDIDATES} candidates"
        ));
    }
    candidates
        .try_reserve(1)
        .map_err(|_| "failed to reserve browser wasm build candidate".to_string())?;
    candidates.push(candidate);
    Ok(())
}

fn select_wasm_build_candidate(
    rust_root: &Path,
    ext_name: &str,
    required_feature: &str,
) -> Result<WasmBuildCandidate, String> {
    let mut candidates = Vec::new();
    if let Some(candidate) = inspect_wasm_build_candidate(rust_root, required_feature)? {
        push_wasm_build_candidate(&mut candidates, candidate)?;
    }
    let mut entries_seen = 0;
    for (_, entry) in read_sorted_directory_entries(
        rust_root,
        &mut entries_seen,
        MAX_BROWSER_BUILD_SCAN_ENTRIES,
        "browser wasm build discovery",
    )? {
        let path = entry.path();
        if !entry
            .file_type()
            .map_err(|error| format!("{}: {}", path.display(), error))?
            .is_dir()
        {
            continue;
        }
        if let Some(candidate) = inspect_wasm_build_candidate(&path, required_feature)? {
            push_wasm_build_candidate(&mut candidates, candidate)?;
        }
    }
    let mut exact_matches = candidates
        .iter()
        .filter(|candidate| candidate.package_name.as_deref() == Some(ext_name));
    if let Some(candidate) = exact_matches.next() {
        if exact_matches.next().is_some() {
            return Err(format!(
                "multiple browser wasm crates under {} declare package name {}",
                rust_root.display(),
                ext_name
            ));
        }
        return Ok(candidate.clone());
    }
    match candidates.as_slice() {
        [candidate] => Ok(candidate.clone()),
        [] => Err(format!(
            "no {} crate found under {} for {}",
            required_feature,
            rust_root.display(),
            ext_name,
        )),
        _ => {
            let names = candidates
                .iter()
                .take(16)
                .map(|candidate| candidate.crate_root.display().to_string())
                .collect::<Vec<_>>();
            let omitted = candidates.len().saturating_sub(names.len());
            let suffix = if omitted == 0 {
                String::new()
            } else {
                format!(", and {omitted} more")
            };
            Err(format!(
                "multiple {} crate candidates under {} for {}: {}{}",
                required_feature,
                rust_root.display(),
                ext_name,
                names.join(", "),
                suffix,
            ))
        }
    }
}

fn select_cargo_workspace_root(rust_root: &Path, crate_root: &Path) -> Result<PathBuf, String> {
    let mut current = Some(crate_root);
    while let Some(path) = current {
        if !path.starts_with(rust_root) {
            break;
        }
        let cargo_toml = path.join("Cargo.toml");
        if regular_file_exists(&cargo_toml)? {
            let content = read_text_file(&cargo_toml)
                .map_err(|error| format!("{}: {}", cargo_toml.display(), error))?;
            let value: Value = toml::from_str(&content)
                .map_err(|error| format!("{}: {}", cargo_toml.display(), error))?;
            if value.get("workspace").and_then(Value::as_table).is_some() {
                return Ok(path.to_path_buf());
            }
        }
        if path == rust_root {
            break;
        }
        current = path.parent();
    }
    Ok(rust_root.to_path_buf())
}

fn standalone_wasm_build_output_path(
    workspace_root: &Path,
    build: &WasmBuildCandidate,
) -> Result<PathBuf, String> {
    let stem = build
        .lib_name
        .clone()
        .or_else(|| {
            build
                .package_name
                .clone()
                .map(|name| name.replace('-', "_"))
        })
        .ok_or_else(|| {
            format!(
                "{}: missing [package].name and [lib].name",
                build.crate_root.join("Cargo.toml").display(),
            )
        })?;
    Ok(workspace_root
        .join("target")
        .join(BROWSER_WASM_TARGET)
        .join("release")
        .join(format!("{}.wasm", stem)))
}

fn sync_generated_output(
    target_output: &Path,
    runtime_path: &Path,
    label: &str,
) -> Result<(), String> {
    if generated_outputs_match(target_output, runtime_path, label)? {
        return Ok(());
    }
    let parent = generated_output_parent(runtime_path);
    fs::create_dir_all(parent).map_err(|error| format!("{}: {}", parent.display(), error))?;
    copy_generated_output_atomically(target_output, runtime_path, label)
}

fn generated_output_parent(path: &Path) -> &Path {
    path.parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."))
}

fn generated_outputs_match(
    target_output: &Path,
    runtime_path: &Path,
    label: &str,
) -> Result<bool, String> {
    let mut source = fs::File::open(target_output)
        .map_err(|error| format!("{}: {}", target_output.display(), error))?;
    let source_metadata = source
        .metadata()
        .map_err(|error| format!("{}: {}", target_output.display(), error))?;
    if !source_metadata.is_file() {
        return Err(format!(
            "{} build output is not a regular file: {}",
            label,
            target_output.display()
        ));
    }
    let max_bytes = u64::try_from(MAX_GENERATED_OUTPUT_BYTES).unwrap_or(u64::MAX);
    if source_metadata.len() > max_bytes {
        return Err(format!(
            "{} build output {} has {} bytes and exceeds the {}-byte browser file limit",
            label,
            target_output.display(),
            source_metadata.len(),
            MAX_GENERATED_OUTPUT_BYTES
        ));
    }

    let mut destination = match fs::File::open(runtime_path) {
        Ok(file) => file,
        Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(false),
        Err(error) => return Err(format!("{}: {}", runtime_path.display(), error)),
    };
    let destination_metadata = destination
        .metadata()
        .map_err(|error| format!("{}: {}", runtime_path.display(), error))?;
    if !destination_metadata.is_file() || destination_metadata.len() != source_metadata.len() {
        return Ok(false);
    }
    let source_modified = source_metadata
        .modified()
        .map_err(|error| format!("{}: {}", target_output.display(), error))?;
    let destination_modified = destination_metadata
        .modified()
        .map_err(|error| format!("{}: {}", runtime_path.display(), error))?;

    let mut source_buffer = [0_u8; 64 * 1024];
    let mut destination_buffer = [0_u8; 64 * 1024];
    let mut remaining = source_metadata.len();
    while remaining != 0 {
        let buffer_bytes = u64::try_from(source_buffer.len()).unwrap_or(u64::MAX);
        let chunk = usize::try_from(remaining.min(buffer_bytes))
            .map_err(|_| "generated output comparison chunk does not fit usize".to_string())?;
        if let Err(error) = source.read_exact(&mut source_buffer[..chunk]) {
            if error.kind() == io::ErrorKind::UnexpectedEof {
                return Ok(false);
            }
            return Err(format!("{}: {}", target_output.display(), error));
        }
        if let Err(error) = destination.read_exact(&mut destination_buffer[..chunk]) {
            if error.kind() == io::ErrorKind::UnexpectedEof {
                return Ok(false);
            }
            return Err(format!("{}: {}", runtime_path.display(), error));
        }
        if source_buffer[..chunk] != destination_buffer[..chunk] {
            return Ok(false);
        }
        let chunk = u64::try_from(chunk)
            .map_err(|_| "generated output comparison chunk does not fit u64".to_string())?;
        remaining = remaining
            .checked_sub(chunk)
            .ok_or_else(|| "generated output comparison length underflow".to_string())?;
    }

    let source_after = source
        .metadata()
        .map_err(|error| format!("{}: {}", target_output.display(), error))?;
    let destination_after = destination
        .metadata()
        .map_err(|error| format!("{}: {}", runtime_path.display(), error))?;
    let source_modified_after = source_after
        .modified()
        .map_err(|error| format!("{}: {}", target_output.display(), error))?;
    let destination_modified_after = destination_after
        .modified()
        .map_err(|error| format!("{}: {}", runtime_path.display(), error))?;
    Ok(source_after.len() == source_metadata.len()
        && source_modified_after == source_modified
        && destination_after.len() == destination_metadata.len()
        && destination_modified_after == destination_modified)
}

fn copy_generated_output_atomically(
    target_output: &Path,
    runtime_path: &Path,
    label: &str,
) -> Result<(), String> {
    let source = fs::File::open(target_output)
        .map_err(|error| format!("{}: {}", target_output.display(), error))?;
    let source_metadata = source
        .metadata()
        .map_err(|error| format!("{}: {}", target_output.display(), error))?;
    if !source_metadata.is_file() {
        return Err(format!(
            "{} build output is not a regular file: {}",
            label,
            target_output.display()
        ));
    }
    let max_bytes = u64::try_from(MAX_GENERATED_OUTPUT_BYTES).unwrap_or(u64::MAX);
    if source_metadata.len() > max_bytes {
        return Err(format!(
            "{} build output {} has {} bytes and exceeds the {}-byte browser file limit",
            label,
            target_output.display(),
            source_metadata.len(),
            MAX_GENERATED_OUTPUT_BYTES
        ));
    }
    let source_modified = source_metadata
        .modified()
        .map_err(|error| format!("{}: {}", target_output.display(), error))?;

    let parent = generated_output_parent(runtime_path);
    let mut temp_path = None;
    let mut temp_file = None;
    for _ in 0..128 {
        let id = GENERATED_OUTPUT_COUNTER.fetch_add(1, Ordering::Relaxed);
        let candidate = parent.join(format!(
            ".vo-browser-output.{}.{}.tmp",
            std::process::id(),
            id
        ));
        match fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&candidate)
        {
            Ok(file) => {
                temp_path = Some(candidate);
                temp_file = Some(file);
                break;
            }
            Err(error) if error.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(error) => return Err(format!("{}: {}", candidate.display(), error)),
        }
    }
    let temp_path = temp_path.ok_or_else(|| {
        format!(
            "could not allocate a temporary browser output in {}",
            parent.display()
        )
    })?;
    let mut temp_file = temp_file.ok_or_else(|| {
        format!(
            "temporary browser output handle is missing for {}",
            temp_path.display()
        )
    })?;

    let copied = (|| -> io::Result<u64> {
        let mut source = source.take(max_bytes.saturating_add(1));
        let copied = io::copy(&mut source, &mut temp_file)?;
        let final_metadata = source.get_ref().metadata()?;
        if copied != source_metadata.len()
            || final_metadata.len() != source_metadata.len()
            || final_metadata.modified()? != source_modified
        {
            return Err(io::Error::other("build output changed while it was copied"));
        }
        temp_file.sync_all()?;
        Ok(copied)
    })();
    let copy_error = match copied {
        Ok(copied) if copied <= max_bytes => None,
        Ok(_) => Some(format!(
            "{} build output exceeds the {}-byte browser file limit",
            label, MAX_GENERATED_OUTPUT_BYTES
        )),
        Err(error) => Some(format!(
            "{} -> {}: {}",
            target_output.display(),
            runtime_path.display(),
            error
        )),
    };
    drop(temp_file);
    if let Some(error) = copy_error {
        let _ = fs::remove_file(&temp_path);
        return Err(error);
    }

    if let Err(error) = replace_generated_output(&temp_path, runtime_path) {
        let _ = fs::remove_file(&temp_path);
        return Err(format!(
            "{} -> {}: {}",
            target_output.display(),
            runtime_path.display(),
            error
        ));
    }
    sync_generated_output_parent(parent).map_err(|error| {
        format!(
            "{} was published to {}, but its parent directory could not be synchronized: {}",
            label,
            runtime_path.display(),
            error
        )
    })
}

#[cfg(not(windows))]
fn replace_generated_output(from: &Path, to: &Path) -> io::Result<()> {
    fs::rename(from, to)
}

#[cfg(windows)]
fn replace_generated_output(from: &Path, to: &Path) -> io::Result<()> {
    use std::iter::once;
    use std::os::windows::ffi::OsStrExt;

    #[link(name = "kernel32")]
    extern "system" {
        #[link_name = "MoveFileExW"]
        fn move_file_ex_w(existing: *const u16, replacement: *const u16, flags: u32) -> i32;
    }

    const MOVEFILE_REPLACE_EXISTING: u32 = 0x1;
    const MOVEFILE_WRITE_THROUGH: u32 = 0x8;
    let from = from
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    let to = to
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    // SAFETY: both buffers are NUL-terminated and remain alive for the call.
    let result = unsafe {
        move_file_ex_w(
            from.as_ptr(),
            to.as_ptr(),
            MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH,
        )
    };
    if result == 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

fn sync_generated_output_parent(parent: &Path) -> io::Result<()> {
    #[cfg(unix)]
    fs::File::open(parent)?.sync_all()?;
    #[cfg(not(unix))]
    let _ = parent;
    Ok(())
}

fn sync_standalone_wasm_output(target_output: &Path, wasm_path: &Path) -> Result<(), String> {
    sync_generated_output(target_output, wasm_path, "standalone wasm")
}

fn metadata_if_exists(path: &Path) -> Result<Option<fs::Metadata>, String> {
    match fs::metadata(path) {
        Ok(metadata) => Ok(Some(metadata)),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(None),
        Err(error) => Err(format!("{}: {}", path.display(), error)),
    }
}

fn canonical_existing_directory(path: &Path, label: &str) -> Result<PathBuf, String> {
    let canonical = path
        .canonicalize()
        .map_err(|error| format!("{} {}: {}", label, path.display(), error))?;
    let metadata = fs::metadata(&canonical)
        .map_err(|error| format!("{} {}: {}", label, canonical.display(), error))?;
    if !metadata.is_dir() {
        return Err(format!(
            "{} must be a directory: {}",
            label,
            canonical.display()
        ));
    }
    Ok(canonical)
}

fn canonical_existing_regular_file(path: &Path, label: &str) -> Result<PathBuf, String> {
    let canonical = path
        .canonicalize()
        .map_err(|error| format!("{} {}: {}", label, path.display(), error))?;
    let metadata = fs::metadata(&canonical)
        .map_err(|error| format!("{} {}: {}", label, canonical.display(), error))?;
    if !metadata.is_file() {
        return Err(format!(
            "{} must be a regular file: {}",
            label,
            canonical.display()
        ));
    }
    Ok(canonical)
}

fn regular_file_exists(path: &Path) -> Result<bool, String> {
    let Some(metadata) = metadata_if_exists(path)? else {
        return Ok(false);
    };
    if !metadata.is_file() {
        return Err(format!("expected a regular file at {}", path.display()));
    }
    Ok(true)
}

#[cfg(test)]
mod tests;
