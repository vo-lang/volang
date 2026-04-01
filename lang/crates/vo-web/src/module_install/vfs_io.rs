//! VFS read/write helpers and path computation for module installation.

use std::path::{Path, PathBuf};

use vo_common::vfs::FileSystem;
use vo_module::cache::layout::{relative_module_dir, VERSION_MARKER};
use vo_module::lifecycle::ModuleSelection;
use vo_module::schema::lockfile::{LockedArtifact, LockedModule};
use vo_module::schema::manifest::ReleaseManifest;

use super::{ModuleInstallError, ModuleInstallErrorKind, ModuleInstallResult, ModuleInstallStage};

pub(super) const VFS_ARTIFACT_DIR: &str = ".vo-artifacts";
pub(super) const VFS_WASM_TARGET: &str = "wasm32-unknown-unknown";

// ── Raw VFS IO ──────────────────────────────────────────────────────────────

pub(super) fn read_vfs_text(path: &str) -> ModuleInstallResult<String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    if let Some(err) = err {
        return Err(
            ModuleInstallError::new(
                ModuleInstallStage::Vfs,
                ModuleInstallErrorKind::ReadFailed,
                format!("read {}: {}", path, err),
            )
            .with_path(path),
        );
    }
    String::from_utf8(data).map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Vfs,
            ModuleInstallErrorKind::ParseFailed,
            format!("read {}: {}", path, error),
        )
        .with_path(path)
    })
}

pub(super) fn read_vfs_bytes(path: &str) -> ModuleInstallResult<Vec<u8>> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    if let Some(err) = err {
        return Err(
            ModuleInstallError::new(
                ModuleInstallStage::Vfs,
                ModuleInstallErrorKind::ReadFailed,
                format!("read {}: {}", path, err),
            )
            .with_path(path),
        );
    }
    Ok(data)
}

fn ensure_vfs_parent_dir(path: &str) -> ModuleInstallResult<()> {
    if let Some(parent) = Path::new(path).parent() {
        let parent = parent.to_string_lossy();
        if parent != "/" && !parent.is_empty() {
            if let Some(error) = vo_web_runtime_wasm::vfs::mkdir_all(&parent, 0o755) {
                return Err(
                    ModuleInstallError::new(
                        ModuleInstallStage::Vfs,
                        ModuleInstallErrorKind::WriteFailed,
                        format!("mkdir {}: {}", parent, error),
                    )
                    .with_path(parent.to_string()),
                );
            }
        }
    }
    Ok(())
}

pub(super) fn write_vfs_bytes(path: &str, bytes: &[u8]) -> ModuleInstallResult<()> {
    ensure_vfs_parent_dir(path)?;
    if let Some(error) = vo_web_runtime_wasm::vfs::write_file(path, bytes, 0o644) {
        return Err(
            ModuleInstallError::new(
                ModuleInstallStage::Vfs,
                ModuleInstallErrorKind::WriteFailed,
                format!("write {}: {}", path, error),
            )
            .with_path(path),
        );
    }
    Ok(())
}

pub(super) fn write_vfs_text(path: &str, content: &str) -> ModuleInstallResult<()> {
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

pub(super) fn discover_vfs_installed_version(module: &str) -> ModuleInstallResult<Option<String>> {
    let vfs = crate::wasm_vfs::WasmVfs::new("");
    vo_module::cache::layout::discover_installed_version(&vfs, std::path::Path::new(""), module)
        .map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::Vfs,
                ModuleInstallErrorKind::ReadFailed,
                error,
            )
            .with_module(module)
        })
}

// ── Module file helpers ─────────────────────────────────────────────────────

pub(super) fn write_versioned_module_files_to_vfs(
    module: &str,
    version: &str,
    files: &[(PathBuf, String)],
) -> ModuleInstallResult<()> {
    let module_prefix = Path::new(module);
    let install_root = relative_module_dir(module, version);
    for (vfs_path, content) in files {
        let file_rel = vfs_path.strip_prefix(module_prefix).map_err(|_| {
            ModuleInstallError::new(
                ModuleInstallStage::Vfs,
                ModuleInstallErrorKind::ValidationFailed,
                format!(
                    "fetched file {} is outside module {}",
                    vfs_path.display(),
                    module,
                ),
            )
            .with_module_version(module, version)
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
) -> ModuleInstallResult<ReleaseManifest> {
    let manifest_content = read_module_file(files, module, "vo.release.json").ok_or_else(|| {
        ModuleInstallError::new(
            ModuleInstallStage::Manifest,
            ModuleInstallErrorKind::Missing,
            format!(
                "release manifest missing from fetched source package for {}@{}",
                module, version
            ),
        )
        .with_module_version(module, version)
    })?;
    vo_module::registry::parse_requested_release_manifest_for_spec(module, version, &manifest_content)
    .map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Manifest,
            ModuleInstallErrorKind::ParseFailed,
            format!("vo.release.json parse error for {}@{}: {}", module, version, error),
        )
        .with_module_version(module, version)
    })
}

pub(super) fn read_release_manifest_from_vfs(
    module: &str,
    version: &str,
) -> ModuleInstallResult<ReleaseManifest> {
    let manifest_path = vfs_module_file_path(module, version, "vo.release.json");
    let manifest_content = read_vfs_text(&manifest_path)?;
    vo_module::registry::parse_requested_release_manifest_for_spec(module, version, &manifest_content)
    .map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Manifest,
            ModuleInstallErrorKind::ParseFailed,
            format!("vo.release.json parse error for {}@{}: {}", module, version, error),
        )
        .with_module_version(module, version)
    })
}

// ── Project deps helpers ────────────────────────────────────────────────────

pub(super) fn format_module_project_deps_error(
    error: vo_module::project::ProjectDepsError,
    module: &str,
    version: &str,
) -> ModuleInstallError {
    let mut mapped = ModuleInstallError::from(error.clone()).with_module_version(module, version);
    match (error.stage, error.kind) {
        (
            vo_module::project::ProjectDepsStage::LockFile,
            vo_module::project::ProjectDepsErrorKind::Missing,
        ) => {
            mapped.detail = format!(
                "module {}@{} requires external modules but vo.lock is missing",
                module, version
            );
            mapped
        }
        _ => {
            mapped.detail = format!("{} for {}@{}", error, module, version);
            mapped
        }
    }
}

pub(super) fn read_vfs_locked_selections(
    module: &str,
    version: &str,
) -> ModuleInstallResult<Vec<ModuleSelection>> {
    let vfs_root = vfs_module_dir(module, version);
    let fs = crate::wasm_vfs::WasmVfs::new(&vfs_root[1..]);
    if !fs.exists(Path::new("vo.mod")) {
        return Err(
            ModuleInstallError::new(
                ModuleInstallStage::ModFile,
                ModuleInstallErrorKind::Missing,
                format!(
                    "module {}@{} is missing cached vo.mod in the VFS",
                    module, version
                ),
            )
            .with_module_version(module, version),
        );
    }
    let project_deps = vo_module::project::read_project_deps(
        &fs,
        &[],
    )
    .map_err(|error| format_module_project_deps_error(error, module, version))?;
    Ok(vo_module::lifecycle::locked_module_selections(
        project_deps.locked_modules(),
    ))
}

pub(super) fn validate_vfs_installed_module(locked: &LockedModule) -> ModuleInstallResult<()> {
    let module = locked.path.as_str();
    let version = locked.version.to_string();
    let vfs_root = vfs_module_dir(module, &version);
    let fs = crate::wasm_vfs::WasmVfs::new(&vfs_root[1..]); // strip leading '/'
    vo_module::cache::validate::validate_installed_module(&fs, std::path::Path::new("."), locked)
        .map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::Vfs,
                ModuleInstallErrorKind::ValidationFailed,
                error.to_string(),
            )
            .with_module_version(module, &version)
        })
}
