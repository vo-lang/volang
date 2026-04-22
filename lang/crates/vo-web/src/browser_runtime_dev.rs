use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::SystemTime;

use toml::Value;
use vo_common::vfs::RealFs;
use vo_module::ext_manifest::parse_ext_manifest_content;
use vo_module::project;
use vo_module::resolved_extension::{AssetRef, AssetRoot};
use vo_module::schema::lockfile::LockedModule;

use crate::browser_runtime::{
    browser_runtime_module_root_for_owner, browser_runtime_plan_from_manifest,
    plan_ready_browser_runtime_at, BrowserArtifactFamily, BrowserArtifactIntent,
    BrowserArtifactSource, BrowserRuntimePlan, BrowserSnapshotFile, BrowserSnapshotMount,
    BrowserSnapshotMountKind, BrowserSnapshotPlan, BrowserSnapshotSourceRef,
    RequiredBrowserArtifact,
};

const BROWSER_WASM_TARGET: &str = "wasm32-unknown-unknown";

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
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct WasmBuildCandidate {
    crate_root: PathBuf,
    package_name: Option<String>,
    lib_name: Option<String>,
}

/// Debug-only helper for local extension/framework development.
///
/// This reads `vo.ext.toml` directly from a local project root and bypasses
/// published dependency resolution. Normal Studio/example/runtime flows must
/// use locked published modules instead.
pub fn debug_local_project_browser_runtime_plan_from_fs(
    project_root: &Path,
) -> Result<BrowserRuntimePlan, String> {
    let project_root = project_root
        .canonicalize()
        .unwrap_or_else(|_| project_root.to_path_buf());
    let manifest_path = project_root.join("vo.ext.toml");
    if !manifest_path.is_file() {
        return Ok(BrowserRuntimePlan::default());
    }
    let mod_file = project::read_mod_file(&project_root)
        .map_err(|error| format!("{}: {}", project_root.join("vo.mod").display(), error))?;
    let module = mod_file.module.as_github().ok_or_else(|| {
        format!(
            "{}: root module must be a github module path",
            project_root.display(),
        )
    })?;
    let content = fs::read_to_string(&manifest_path)
        .map_err(|error| format!("{}: {}", manifest_path.display(), error))?;
    let manifest = parse_ext_manifest_content(&content, &manifest_path)
        .map_err(|error| format!("{}: {}", manifest_path.display(), error))?;
    Ok(browser_runtime_plan_from_manifest(
        &project_root.to_string_lossy(),
        Some(module.as_str()),
        &manifest,
    ))
}

pub fn locked_browser_runtime_plan_from_fs(
    locked_modules: &[LockedModule],
    mod_cache_root: &Path,
) -> Result<BrowserRuntimePlan, String> {
    if locked_modules.is_empty() {
        return Ok(BrowserRuntimePlan::default());
    }
    let mod_cache_root = mod_cache_root
        .canonicalize()
        .unwrap_or_else(|_| mod_cache_root.to_path_buf());
    let ready = vo_module::readiness::check_project_readiness(
        &RealFs::new(&mod_cache_root),
        locked_modules,
        BROWSER_WASM_TARGET,
    )
    .map_err(|error| error.to_string())?;
    plan_ready_browser_runtime_at(&ready, &mod_cache_root.to_string_lossy())
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
    let mut actions = Vec::new();
    let mut seen = BTreeSet::new();
    for artifact in &intent.required_artifacts {
        if artifact.source != BrowserArtifactSource::LocalManifest {
            continue;
        }
        if !seen.insert((artifact.owner.clone(), artifact.family)) {
            continue;
        }
        let module_root = PathBuf::from(browser_runtime_module_root_for_owner(
            runtime,
            &artifact.owner,
        )?);
        let rust_root = module_root.join("rust");
        if !rust_root.join("Cargo.toml").is_file() {
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
    let mut seen_paths = BTreeSet::new();
    for mount in &snapshot.mounts {
        match mount.kind {
            BrowserSnapshotMountKind::Directory => materialize_snapshot_directory_mount_from_fs(
                mount,
                runtime,
                project_root,
                entry_path,
                &mut files,
                &mut seen_paths,
            )?,
            BrowserSnapshotMountKind::File => materialize_snapshot_file_mount_from_fs(
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
    let expected_wasm_path = out_dir.join(format!("{}_bg.wasm", out_name));
    let runtime_wasm_path = resolve_fs_asset_path(module_root, &artifact.wasm.runtime_asset);
    if runtime_wasm_path != expected_wasm_path {
        return Err(format!(
            "bindgen island wasm path for {} must be {}, got {}",
            artifact.extension_name,
            expected_wasm_path.display(),
            runtime_wasm_path.display(),
        ));
    }
    if let Some(js_glue) = &artifact.js_glue {
        let expected_js_path = out_dir.join(format!("{}.js", out_name));
        let runtime_js_path = resolve_fs_asset_path(module_root, &js_glue.runtime_asset);
        if runtime_js_path != expected_js_path {
            return Err(format!(
                "bindgen island js glue path for {} must be {}, got {}",
                artifact.extension_name,
                expected_js_path.display(),
                runtime_js_path.display(),
            ));
        }
    }
    Ok(EnsurePkgIslandAction {
        module_key: artifact.module_key.clone(),
        extension_name: artifact.extension_name.clone(),
        rust_root: rust_root.to_path_buf(),
        crate_root: build.crate_root,
        out_dir,
        out_name,
    })
}

fn execute_standalone_wasm_action(action: &EnsureStandaloneWasmAction) -> Result<(), String> {
    if standalone_wasm_target_needs_build(
        &action.rust_root,
        &action.crate_root,
        &action.build_output,
    )? {
        eprintln!(
            "[vo-web] building standalone wasm for {} from {}",
            action.extension_name,
            action.crate_root.display(),
        );
        let output = Command::new("cargo")
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
            .output()
            .map_err(|error| format!("cargo: {}", error))?;
        if !output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(format!(
                "failed to build standalone wasm for {}:\n{}\n{}",
                action.extension_name,
                stdout.trim_end(),
                stderr.trim_end(),
            ));
        }
    }
    sync_standalone_wasm_output(&action.build_output, &action.runtime_wasm_path)
}

fn execute_pkg_island_action(action: &EnsurePkgIslandAction) -> Result<(), String> {
    if !pkg_island_needs_build(
        &action.rust_root,
        &action.crate_root,
        &action.extension_name,
    )? {
        return Ok(());
    }
    eprintln!(
        "[vo-web] building render-island wasm for {} from {}",
        action.extension_name,
        action.crate_root.display(),
    );
    let output = Command::new("wasm-pack")
        .args([
            "build",
            "--target",
            "web",
            "--out-dir",
            &action.out_dir.to_string_lossy(),
            "--out-name",
            &action.out_name,
            "--release",
            &action.crate_root.to_string_lossy(),
            "--",
            "--no-default-features",
            "--features",
            "wasm-island",
        ])
        .output()
        .map_err(|error| format!("wasm-pack: {}", error))?;
    if output.status.success() {
        return Ok(());
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    Err(format!(
        "failed to build render-island wasm for {}:\n{}\n{}",
        action.extension_name,
        stdout.trim_end(),
        stderr.trim_end(),
    ))
}

fn materialize_snapshot_directory_mount_from_fs(
    mount: &BrowserSnapshotMount,
    runtime: &BrowserRuntimePlan,
    project_root: Option<&Path>,
    entry_path: &Path,
    files: &mut Vec<BrowserSnapshotFile>,
    seen_paths: &mut BTreeSet<String>,
) -> Result<(), String> {
    fn walk(
        dir: &Path,
        mount: &BrowserSnapshotMount,
        runtime: &BrowserRuntimePlan,
        project_root: Option<&Path>,
        entry_path: &Path,
        files: &mut Vec<BrowserSnapshotFile>,
        seen_paths: &mut BTreeSet<String>,
    ) -> Result<(), String> {
        for entry in fs::read_dir(dir).map_err(|error| format!("{}: {}", dir.display(), error))? {
            let entry = entry.map_err(|error| format!("{}: {}", dir.display(), error))?;
            let path = entry.path();
            let file_type = entry
                .file_type()
                .map_err(|error| format!("{}: {}", path.display(), error))?;
            if file_type.is_dir() {
                walk(
                    &path,
                    mount,
                    runtime,
                    project_root,
                    entry_path,
                    files,
                    seen_paths,
                )?;
                continue;
            }
            if !file_type.is_file() {
                continue;
            }
            let output_path = materialized_snapshot_path_from_fs(
                mount,
                runtime,
                project_root,
                entry_path,
                &path,
            )?;
            if seen_paths.insert(output_path.clone()) {
                files.push(BrowserSnapshotFile {
                    path: output_path,
                    bytes: fs::read(&path)
                        .map_err(|error| format!("{}: {}", path.display(), error))?,
                });
            }
        }
        Ok(())
    }

    let root =
        resolve_snapshot_source_path_from_fs(runtime, &mount.source, project_root, entry_path)?;
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

fn materialize_snapshot_file_mount_from_fs(
    mount: &BrowserSnapshotMount,
    runtime: &BrowserRuntimePlan,
    project_root: Option<&Path>,
    entry_path: &Path,
    files: &mut Vec<BrowserSnapshotFile>,
    seen_paths: &mut BTreeSet<String>,
) -> Result<(), String> {
    let path =
        resolve_snapshot_source_path_from_fs(runtime, &mount.source, project_root, entry_path)?;
    let output_path =
        materialized_snapshot_path_from_fs(mount, runtime, project_root, entry_path, &path)?;
    if seen_paths.insert(output_path.clone()) {
        files.push(BrowserSnapshotFile {
            path: output_path,
            bytes: fs::read(&path).map_err(|error| format!("{}: {}", path.display(), error))?,
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
        normalize_snapshot_output_path(full_path)
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
    let module_root = PathBuf::from(browser_runtime_module_root_for_owner(
        runtime,
        &asset.owner,
    )?);
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
            let mut prefix_asset = asset.clone();
            prefix_asset.relative_path = PathBuf::from(strip_prefix);
            resolve_snapshot_asset_path_from_fs(runtime, &prefix_asset)
        }
    }
}

fn strip_snapshot_prefix_from_fs(path: &Path, prefix: &Path) -> Result<String, String> {
    if path == prefix {
        return Ok(String::new());
    }
    path.strip_prefix(prefix)
        .map(normalize_snapshot_output_path)
        .map_err(|error| format!("{}: {}", path.display(), error))
}

fn resolve_fs_asset_path(module_root: &Path, asset: &AssetRef) -> PathBuf {
    match asset.root {
        AssetRoot::ModuleRoot => module_root.join(asset.relative_path()),
        AssetRoot::ArtifactRoot => module_root.join("artifacts").join(asset.relative_path()),
    }
}

fn normalize_snapshot_output_path(path: &Path) -> String {
    path.to_string_lossy().replace('\\', "/")
}

fn inspect_wasm_build_candidate(
    crate_root: &Path,
    required_feature: &str,
) -> Result<Option<WasmBuildCandidate>, String> {
    let cargo_toml = crate_root.join("Cargo.toml");
    if !cargo_toml.is_file() {
        return Ok(None);
    }
    let content = fs::read_to_string(&cargo_toml)
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

fn select_wasm_build_candidate(
    rust_root: &Path,
    ext_name: &str,
    required_feature: &str,
) -> Result<WasmBuildCandidate, String> {
    let mut candidates = Vec::new();
    if let Some(candidate) = inspect_wasm_build_candidate(rust_root, required_feature)? {
        candidates.push(candidate);
    }
    for entry in
        fs::read_dir(rust_root).map_err(|error| format!("{}: {}", rust_root.display(), error))?
    {
        let entry = entry.map_err(|error| format!("{}: {}", rust_root.display(), error))?;
        let path = entry.path();
        if !entry
            .file_type()
            .map_err(|error| format!("{}: {}", path.display(), error))?
            .is_dir()
        {
            continue;
        }
        if let Some(candidate) = inspect_wasm_build_candidate(&path, required_feature)? {
            candidates.push(candidate);
        }
    }
    if let Some(candidate) = candidates
        .iter()
        .find(|candidate| candidate.package_name.as_deref() == Some(ext_name))
    {
        return Ok(WasmBuildCandidate {
            crate_root: candidate.crate_root.clone(),
            package_name: candidate.package_name.clone(),
            lib_name: candidate.lib_name.clone(),
        });
    }
    if candidates.len() == 1 {
        return Ok(candidates.into_iter().next().unwrap());
    }
    if candidates.is_empty() {
        return Err(format!(
            "no {} crate found under {} for {}",
            required_feature,
            rust_root.display(),
            ext_name,
        ));
    }
    let names = candidates
        .iter()
        .map(|candidate| candidate.crate_root.display().to_string())
        .collect::<Vec<_>>();
    Err(format!(
        "multiple {} crate candidates under {} for {}: {}",
        required_feature,
        rust_root.display(),
        ext_name,
        names.join(", "),
    ))
}

fn select_cargo_workspace_root(rust_root: &Path, crate_root: &Path) -> Result<PathBuf, String> {
    let mut current = Some(crate_root);
    while let Some(path) = current {
        if !path.starts_with(rust_root) {
            break;
        }
        let cargo_toml = path.join("Cargo.toml");
        if cargo_toml.is_file() {
            let content = fs::read_to_string(&cargo_toml)
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

fn standalone_wasm_target_needs_build(
    rust_root: &Path,
    crate_root: &Path,
    target_output: &Path,
) -> Result<bool, String> {
    let Some(output_mtime) = file_modified(target_output)? else {
        return Ok(true);
    };
    let mut newest_input = None;
    update_newest_modified(
        &mut newest_input,
        file_modified(&rust_root.join("Cargo.toml"))?,
    );
    update_newest_modified(
        &mut newest_input,
        file_modified(&rust_root.join("Cargo.lock"))?,
    );
    if crate_root != rust_root {
        update_newest_modified(
            &mut newest_input,
            file_modified(&crate_root.join("Cargo.toml"))?,
        );
    }
    update_newest_modified(
        &mut newest_input,
        newest_modified_in_dir(&crate_root.join("src"))?,
    );
    Ok(newest_input
        .map(|mtime| mtime > output_mtime)
        .unwrap_or(false))
}

fn sync_standalone_wasm_output(target_output: &Path, wasm_path: &Path) -> Result<(), String> {
    let Some(target_mtime) = file_modified(target_output)? else {
        return Err(format!(
            "missing standalone wasm build output {}",
            target_output.display(),
        ));
    };
    let wasm_mtime = file_modified(wasm_path)?;
    if wasm_mtime
        .map(|mtime| mtime >= target_mtime)
        .unwrap_or(false)
    {
        return Ok(());
    }
    if let Some(parent) = wasm_path.parent() {
        fs::create_dir_all(parent).map_err(|error| format!("{}: {}", parent.display(), error))?;
    }
    fs::copy(target_output, wasm_path).map_err(|error| {
        format!(
            "{} -> {}: {}",
            target_output.display(),
            wasm_path.display(),
            error,
        )
    })?;
    Ok(())
}

fn pkg_island_needs_build(
    rust_root: &Path,
    crate_root: &Path,
    ext_name: &str,
) -> Result<bool, String> {
    let pkg_island = rust_root.join("pkg-island");
    let wasm_path = pkg_island.join(format!("{}_island_bg.wasm", ext_name));
    let js_path = pkg_island.join(format!("{}_island.js", ext_name));
    let Some(wasm_mtime) = file_modified(&wasm_path)? else {
        return Ok(true);
    };
    let Some(js_mtime) = file_modified(&js_path)? else {
        return Ok(true);
    };
    let oldest_output = if wasm_mtime < js_mtime {
        wasm_mtime
    } else {
        js_mtime
    };

    let mut newest_input = None;
    update_newest_modified(
        &mut newest_input,
        file_modified(&rust_root.join("Cargo.toml"))?,
    );
    update_newest_modified(
        &mut newest_input,
        file_modified(&rust_root.join("Cargo.lock"))?,
    );
    update_newest_modified(
        &mut newest_input,
        file_modified(&crate_root.join("Cargo.toml"))?,
    );
    update_newest_modified(
        &mut newest_input,
        newest_modified_in_dir(&crate_root.join("src"))?,
    );

    Ok(newest_input
        .map(|mtime| mtime > oldest_output)
        .unwrap_or(false))
}

fn update_newest_modified(newest: &mut Option<SystemTime>, candidate: Option<SystemTime>) {
    let Some(candidate) = candidate else {
        return;
    };
    match newest {
        Some(current) if *current >= candidate => {}
        _ => *newest = Some(candidate),
    }
}

fn file_modified(path: &Path) -> Result<Option<SystemTime>, String> {
    if !path.exists() {
        return Ok(None);
    }
    let metadata = fs::metadata(path).map_err(|error| format!("{}: {}", path.display(), error))?;
    metadata
        .modified()
        .map(Some)
        .map_err(|error| format!("{}: {}", path.display(), error))
}

fn newest_modified_in_dir(dir: &Path) -> Result<Option<SystemTime>, String> {
    if !dir.is_dir() {
        return Ok(None);
    }

    fn walk(dir: &Path, newest: &mut Option<SystemTime>) -> Result<(), String> {
        for entry in fs::read_dir(dir).map_err(|error| format!("{}: {}", dir.display(), error))? {
            let entry = entry.map_err(|error| format!("{}: {}", dir.display(), error))?;
            let path = entry.path();
            let file_type = entry
                .file_type()
                .map_err(|error| format!("{}: {}", path.display(), error))?;
            if file_type.is_dir() {
                walk(&path, newest)?;
                continue;
            }
            if !file_type.is_file() {
                continue;
            }
            update_newest_modified(
                newest,
                Some(
                    entry
                        .metadata()
                        .map_err(|error| format!("{}: {}", path.display(), error))?
                        .modified()
                        .map_err(|error| format!("{}: {}", path.display(), error))?,
                ),
            );
        }
        Ok(())
    }

    let mut newest = None;
    walk(dir, &mut newest)?;
    Ok(newest)
}

#[cfg(test)]
mod tests {
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
        parse_ext_manifest_content(content, Path::new("/tmp/vo.ext.toml")).unwrap()
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
        let module_dir =
            vo_module::cache::layout::cache_dir(cache_root, &locked.path, &locked.version);
        fs::create_dir_all(&module_dir).unwrap();
        fs::write(
            module_dir.join("vo.mod"),
            format!("module {}\n\nvo {}\n", locked.path, locked.vo),
        )
        .unwrap();
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
        if let Some(ext_manifest_content) = ext_manifest_content {
            fs::write(
                module_dir.join("vo.ext.toml"),
                ext_manifest_content.as_bytes(),
            )
            .unwrap();
        }
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
            "module github.com/acme/app\n\nvo 0.1.0\n",
        )
        .unwrap();
        fs::write(
            project_root.join("vo.ext.toml"),
            r#"
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
        let files =
            materialize_browser_snapshot_from_fs(&snapshot, &runtime, Some(&root), &entry_path)
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
}
