//! VFS read/write helpers and path computation for module installation.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use vo_common::vfs::MemoryFs;
use vo_module::cache::layout::{cache_key, relative_module_dir, VERSION_MARKER};
use vo_module::schema::lockfile::{LockedArtifact, LockedModule};
use vo_module::schema::manifest::ReleaseManifest;

pub(super) const VFS_ARTIFACT_DIR: &str = ".vo-artifacts";
pub(super) const VFS_WASM_TARGET: &str = "wasm32-unknown-unknown";

// ── Raw VFS IO ──────────────────────────────────────────────────────────────

pub(super) fn read_vfs_text(path: &str) -> Result<String, String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    if let Some(err) = err {
        return Err(format!("read {}: {}", path, err));
    }
    String::from_utf8(data).map_err(|e| format!("read {}: {}", path, e))
}

pub(super) fn read_vfs_bytes(path: &str) -> Result<Vec<u8>, String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    if let Some(err) = err {
        return Err(format!("read {}: {}", path, err));
    }
    Ok(data)
}

fn ensure_vfs_parent_dir(path: &str) -> Result<(), String> {
    if let Some(parent) = Path::new(path).parent() {
        let parent = parent.to_string_lossy();
        if parent != "/" && !parent.is_empty() {
            if let Some(error) = vo_web_runtime_wasm::vfs::mkdir_all(&parent, 0o755) {
                return Err(format!("mkdir {}: {}", parent, error));
            }
        }
    }
    Ok(())
}

pub(super) fn write_vfs_bytes(path: &str, bytes: &[u8]) -> Result<(), String> {
    ensure_vfs_parent_dir(path)?;
    if let Some(error) = vo_web_runtime_wasm::vfs::write_file(path, bytes, 0o644) {
        return Err(format!("write {}: {}", path, error));
    }
    Ok(())
}

pub(super) fn write_vfs_text(path: &str, content: &str) -> Result<(), String> {
    write_vfs_bytes(path, content.as_bytes())
}

// ── VFS path computation ────────────────────────────────────────────────────

pub(super) fn vfs_module_dir(module: &str, version: &str) -> String {
    format!("/{}", relative_module_dir(module, version).display())
}

pub(super) fn vfs_module_file_path(module: &str, version: &str, file_name: &str) -> String {
    format!("{}/{}", vfs_module_dir(module, version), file_name)
}

pub(super) fn vfs_artifact_path(module: &str, version: &str, asset_name: &str) -> String {
    format!(
        "{}/{}/{}",
        vfs_module_dir(module, version),
        VFS_ARTIFACT_DIR,
        asset_name
    )
}

// ── VFS module metadata ─────────────────────────────────────────────────────

pub(super) fn read_vfs_installed_version(module: &str, version: &str) -> Option<String> {
    let path = vfs_module_file_path(module, version, VERSION_MARKER);
    let content = read_vfs_text(&path).ok()?;
    let version = content.trim();
    if version.is_empty() {
        return None;
    }
    Some(version.to_string())
}

pub(super) fn discover_vfs_installed_version(module: &str) -> Option<String> {
    let encoded = cache_key(module);
    let module_dir = format!("/{}", encoded);
    let (entries, err) = vo_web_runtime_wasm::vfs::read_dir(&module_dir);
    if err.is_some() {
        return None;
    }
    for (name, is_dir, _mode) in entries {
        if !is_dir || !name.starts_with('v') {
            continue;
        }
        let version_marker = format!(
            "{}/{}/{}",
            module_dir,
            name,
            VERSION_MARKER
        );
        if let Ok(content) = read_vfs_text(&version_marker) {
            let version = content.trim().to_string();
            if !version.is_empty() {
                return Some(version);
            }
        }
    }
    None
}

// ── Module file helpers ─────────────────────────────────────────────────────

pub(super) fn write_versioned_module_files_to_vfs(
    module: &str,
    version: &str,
    files: &[(PathBuf, String)],
) -> Result<(), String> {
    let module_prefix = Path::new(module);
    let install_root = relative_module_dir(module, version);
    for (vfs_path, content) in files {
        let file_rel = vfs_path.strip_prefix(module_prefix).map_err(|_| {
            format!(
                "fetched file {} is outside module {}",
                vfs_path.display(),
                module,
            )
        })?;
        if file_rel.as_os_str().is_empty() {
            continue;
        }
        let full = format!("/{}", install_root.join(file_rel).display());
        write_vfs_text(&full, content)?;
    }
    Ok(())
}

pub(super) fn read_module_file(
    files: &[(PathBuf, String)],
    module: &str,
    file_name: &str,
) -> Option<String> {
    let wanted = Path::new(module).join(file_name);
    files
        .iter()
        .find(|(path, _)| path == &wanted)
        .map(|(_, content)| content.clone())
}

pub(super) fn find_locked_wasm_artifact<'a>(
    locked: &'a LockedModule,
    asset_name: &str,
) -> Option<&'a LockedArtifact> {
    locked
        .artifacts
        .iter()
        .find(|artifact| artifact.id.target == VFS_WASM_TARGET && artifact.id.name == asset_name)
}

// ── Release manifest helpers ────────────────────────────────────────────────

pub(super) fn release_manifest_from_files(
    module: &str,
    version: &str,
    files: &[(PathBuf, String)],
) -> Result<ReleaseManifest, String> {
    let manifest_content = read_module_file(files, module, "vo.release.json").ok_or_else(|| {
        format!(
            "release manifest missing from fetched source package for {}@{}",
            module, version
        )
    })?;
    ReleaseManifest::parse(&manifest_content).map_err(|error| {
        format!(
            "vo.release.json parse error for {}@{}: {}",
            module, version, error
        )
    })
}

pub(super) fn read_release_manifest_from_vfs(
    module: &str,
    version: &str,
) -> Result<ReleaseManifest, String> {
    let manifest_path = vfs_module_file_path(module, version, "vo.release.json");
    let manifest_content = read_vfs_text(&manifest_path)?;
    ReleaseManifest::parse(&manifest_content).map_err(|error| {
        format!(
            "vo.release.json parse error for {}@{}: {}",
            module, version, error
        )
    })
}

// ── Project deps helpers ────────────────────────────────────────────────────

pub(super) fn inline_project_deps(
    vo_mod_content: &str,
    vo_lock_content: &str,
) -> Result<vo_module::project::ProjectDeps, String> {
    let fs = MemoryFs::new()
        .with_file("vo.mod", vo_mod_content)
        .with_file("vo.lock", vo_lock_content);
    vo_module::project::read_project_deps(&fs, &[]).map_err(|error| error.to_string())
}

pub(super) fn format_module_project_deps_error(
    error: vo_module::project::ProjectDepsError,
    module: &str,
    version: &str,
) -> String {
    match (error.stage, error.kind) {
        (
            vo_module::project::ProjectDepsStage::LockFile,
            vo_module::project::ProjectDepsErrorKind::Missing,
        ) => format!(
            "module {}@{} requires external modules but vo.lock is missing",
            module, version
        ),
        _ => format!("{} for {}@{}", error, module, version),
    }
}

pub(super) fn collect_locked_specs(
    project_deps: &vo_module::project::ProjectDeps,
) -> Vec<(String, String)> {
    project_deps
        .locked_modules
        .iter()
        .map(|module| (module.path.as_str().to_string(), module.version.to_string()))
        .collect()
}

pub(super) fn remember_selected_version(
    selected_versions: &mut HashMap<String, String>,
    module: &str,
    version: &str,
) -> Result<(), String> {
    match selected_versions.get(module) {
        Some(existing) if existing != version => Err(format!(
            "module {} required at both {} and {}",
            module, existing, version
        )),
        Some(_) => Ok(()),
        None => {
            selected_versions.insert(module.to_string(), version.to_string());
            Ok(())
        }
    }
}

pub(super) fn read_vfs_locked_specs(
    module: &str,
    version: &str,
) -> Result<Vec<(String, String)>, String> {
    let vo_mod_path = vfs_module_file_path(module, version, "vo.mod");
    let dep_vo_mod = read_vfs_text(&vo_mod_path).map_err(|_| {
        format!(
            "module {}@{} is missing cached vo.mod in the VFS",
            module, version
        )
    })?;
    let vo_lock_path = vfs_module_file_path(module, version, "vo.lock");
    let dep_vo_lock = read_vfs_text(&vo_lock_path).map_err(|_| {
        format!(
            "module {}@{} requires external modules but vo.lock is missing",
            module, version
        )
    })?;
    let project_deps = vo_module::project::read_project_deps(
        &MemoryFs::new()
            .with_file("vo.mod", dep_vo_mod)
            .with_file("vo.lock", dep_vo_lock),
        &[],
    )
    .map_err(|error| format_module_project_deps_error(error, module, version))?;
    Ok(collect_locked_specs(&project_deps))
}

pub(super) fn validate_vfs_installed_module(locked: &LockedModule) -> Result<(), String> {
    let module = locked.path.as_str();
    let version = locked.version.to_string();
    let vfs_root = vfs_module_dir(module, &version);
    let fs = crate::wasm_vfs::WasmVfs::new(&vfs_root[1..]); // strip leading '/'
    vo_module::cache::validate::validate_installed_module(&fs, std::path::Path::new("."), locked)
        .map_err(|e| e.to_string())
}
