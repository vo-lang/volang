//! Module installation lifecycle for WASM targets.
//!
//! Handles fetching, installing, and loading Vo modules into the JS VFS.
//! This includes:
//!
//! - **`install_module_to_vfs`** — fetch source + WASM binary from GitHub, write to VFS
//! - **`ensure_vfs_deps`** — ensure all `vo.mod` dependencies are installed
//! - **`ensure_vfs_deps_from_fs`** — same, but find `vo.mod` in a MemoryFs
//! - **`load_ext_if_present`** — load pre-built WASM extension for a module

mod extension;
mod fetch;
mod log;
mod vfs_io;

use vo_common::vfs::MemoryFs;
use vo_module::cache::layout::{SOURCE_DIGEST_MARKER, VERSION_MARKER};
use vo_module::lifecycle::{ModuleSelection, ModuleSelectionPlanner};
use vo_module::operation_error::OperationError;
use vo_module::project::{ProjectDepsError, ProjectDepsErrorKind, ProjectDepsStage};
use vo_module::schema::lockfile::LockedModule;

use extension::*;
use log::*;
use vfs_io::*;

// ── Public re-exports ───────────────────────────────────────────────────────

pub use extension::load_ext_if_present;

type ModuleInstallResult<T> = Result<T, ModuleInstallError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ModuleInstallStage {
    Spec,
    Workspace,
    ModFile,
    LockFile,
    Manifest,
    Fetch,
    VersionResolve,
    Selection,
    Vfs,
    Extension,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ModuleInstallErrorKind {
    Missing,
    ReadFailed,
    WriteFailed,
    ParseFailed,
    ValidationFailed,
    NetworkFailed,
    LoadFailed,
}

pub(super) type ModuleInstallError = OperationError<ModuleInstallStage, ModuleInstallErrorKind>;

fn module_install_error_from_project(error: ProjectDepsError) -> ModuleInstallError {
    fn project_stage(stage: ProjectDepsStage) -> ModuleInstallStage {
        match stage {
            ProjectDepsStage::Workspace => ModuleInstallStage::Workspace,
            ProjectDepsStage::ModFile => ModuleInstallStage::ModFile,
            ProjectDepsStage::LockFile => ModuleInstallStage::LockFile,
        }
    }
    fn project_kind(kind: ProjectDepsErrorKind) -> ModuleInstallErrorKind {
        match kind {
            ProjectDepsErrorKind::Missing => ModuleInstallErrorKind::Missing,
            ProjectDepsErrorKind::ReadFailed => ModuleInstallErrorKind::ReadFailed,
            ProjectDepsErrorKind::ParseFailed => ModuleInstallErrorKind::ParseFailed,
            ProjectDepsErrorKind::ValidationFailed => ModuleInstallErrorKind::ValidationFailed,
        }
    }
    ModuleInstallError::from_other(error, project_stage, project_kind)
}

pub(super) fn stringify_module_install_error(error: ModuleInstallError) -> String {
    error.to_string()
}

// ── Install a single module by spec ─────────────────────────────────────────

pub async fn install_module_to_vfs(spec: &str) -> Result<String, String> {
    install_module_to_vfs_typed(spec)
        .await
        .map_err(stringify_module_install_error)
}

async fn install_module_to_vfs_typed(spec: &str) -> ModuleInstallResult<String> {
    let (module, version) = spec
        .rsplit_once('@')
        .filter(|(m, v)| !m.is_empty() && !v.is_empty())
        .map(|(m, v)| (m.to_string(), v.to_string()))
        .ok_or_else(|| {
            ModuleInstallError::new(
                ModuleInstallStage::Spec,
                ModuleInstallErrorKind::ParseFailed,
                format!("invalid spec {:?}: expected module@version", spec),
            )
        })?;

    let fetch_start = crate::now_ms();
    log_dependency_fetch_start(&module, &version);
    let files = fetch::fetch_module_files(&module, &version).await?;
    log_dependency_fetch_done(&module, &version, fetch_start);
    let manifest = release_manifest_from_files(&module, &version, &files)?;
    write_versioned_module_files_to_vfs(&module, &version, &files)?;
    write_vfs_text(
        &vfs_module_file_path(&module, &version, VERSION_MARKER),
        &format!("{}\n", version),
    )?;
    write_vfs_text(
        &vfs_module_file_path(&module, &version, SOURCE_DIGEST_MARKER),
        &format!("{}\n", manifest.source.digest),
    )?;

    if let Some(wasm_extension) = read_wasm_extension_from_vfs(&module, &version) {
        let assets = fetch_and_cache_extension_assets(
            &module,
            &version,
            &wasm_extension,
            false,
            || {
                fetch::fetch_wasm_binary_from_manifest(
                    &module,
                    &version,
                    &manifest,
                    &wasm_extension.wasm,
                )
            },
            |js_glue_name| {
                let module = module.clone();
                let version = version.clone();
                let manifest = manifest.clone();
                async move {
                    fetch::fetch_wasm_js_glue_text_from_manifest(
                        &module,
                        &version,
                        &manifest,
                        &js_glue_name,
                    )
                    .await
                }
            },
        )
        .await?;
        load_wasm_extension_bytes(&module, &assets.wasm_bytes, assets.js_glue_text.as_deref())
            .await?;
    }

    Ok(vfs_module_dir(&module, &version))
}

// ── Install a locked module (with digest verification) ──────────────────────

async fn install_locked_module_to_vfs(locked: &LockedModule) -> ModuleInstallResult<String> {
    let module = locked.path.as_str();
    let version = locked.version.to_string();

    let fetch_start = crate::now_ms();
    log_dependency_fetch_start(module, &version);
    let files = fetch::fetch_locked_module_files(locked).await?;
    log_dependency_fetch_done(module, &version, fetch_start);
    write_versioned_module_files_to_vfs(module, &version, &files)?;
    write_vfs_text(
        &vfs_module_file_path(module, &version, VERSION_MARKER),
        &format!("{}\n", version),
    )?;
    write_vfs_text(
        &vfs_module_file_path(module, &version, SOURCE_DIGEST_MARKER),
        &format!("{}\n", locked.source),
    )?;

    if let Some(wasm_extension) = read_wasm_extension_from_vfs(module, &version) {
        fetch_and_cache_extension_assets(
            module,
            &version,
            &wasm_extension,
            false,
            || fetch::fetch_locked_wasm_binary(locked, &wasm_extension.wasm),
            |js_glue_name| async move {
                fetch::fetch_locked_wasm_js_glue_text(locked, &js_glue_name).await
            },
        )
        .await?;
        load_locked_ext_from_vfs(locked).await?;
    }

    Ok(vfs_module_dir(module, &version))
}

// ── Ensure locked modules are installed ─────────────────────────────────────

async fn ensure_vfs_locked_modules(initial: Vec<LockedModule>) -> ModuleInstallResult<()> {
    for locked in vo_module::lifecycle::normalize_locked_modules(initial).map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Selection,
            ModuleInstallErrorKind::ValidationFailed,
            error.to_string(),
        )
    })? {
        if validate_vfs_installed_module(&locked).is_ok() {
            load_locked_ext_from_vfs(&locked).await?;
        } else {
            install_locked_module_to_vfs(&locked).await?;
        }
    }
    Ok(())
}

// ── Ensure versioned module closure ─────────────────────────────────────────

async fn ensure_vfs_versioned_module_closure(
    initial: Vec<(String, String)>,
) -> ModuleInstallResult<()> {
    let initial = initial
        .into_iter()
        .map(|(module, version)| {
            ModuleSelection::parse(&module, &version).map_err(|error| {
                ModuleInstallError::new(
                    ModuleInstallStage::Selection,
                    ModuleInstallErrorKind::ParseFailed,
                    error.to_string(),
                )
                .with_module_version(&module, &version)
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut planner = ModuleSelectionPlanner::new(initial).map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Selection,
            ModuleInstallErrorKind::ValidationFailed,
            error.to_string(),
        )
    })?;

    while let Some(selection) = planner.next() {
        let module = selection.module.as_str().to_string();
        let version = selection.version.to_string();

        if read_vfs_installed_version(&module, &version).as_deref() != Some(version.as_str()) {
            let spec = selection.spec();
            install_module_to_vfs_typed(&spec).await?;
        } else {
            load_ext_if_present(&module, &version).await;
        }

        let deps = read_vfs_locked_selections(&module, &version)?;
        planner.push_all(deps).map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::Selection,
                ModuleInstallErrorKind::ValidationFailed,
                error.to_string(),
            )
            .with_module_version(&module, &version)
        })?;
    }

    Ok(())
}

// ── Public dependency ensuring ──────────────────────────────────────────────

pub async fn ensure_vfs_deps(vo_mod_content: &str, vo_lock_content: &str) -> Result<(), String> {
    ensure_vfs_deps_typed(vo_mod_content, vo_lock_content)
        .await
        .map_err(stringify_module_install_error)
}

async fn ensure_vfs_deps_typed(
    vo_mod_content: &str,
    vo_lock_content: &str,
) -> ModuleInstallResult<()> {
    let project_deps =
        vo_module::project::read_inline_project_deps(vo_mod_content, vo_lock_content, &[])
            .map_err(module_install_error_from_project)?;
    if !project_deps.has_mod_file() || project_deps.locked_modules().is_empty() {
        return Ok(());
    }
    ensure_vfs_locked_modules(project_deps.into_locked_modules()).await
}

pub async fn ensure_vfs_deps_from_fs(local_fs: &MemoryFs, entry: &str) -> Result<(), String> {
    ensure_vfs_deps_from_fs_typed(local_fs, entry)
        .await
        .map_err(stringify_module_install_error)
}

async fn ensure_vfs_deps_from_fs_typed(
    local_fs: &MemoryFs,
    entry: &str,
) -> ModuleInstallResult<()> {
    let entry_dir = std::path::Path::new(entry)
        .parent()
        .unwrap_or(std::path::Path::new("."));
    let project_deps = vo_module::project::load_project_context(local_fs, entry_dir)
        .map_err(module_install_error_from_project)?
        .project_deps;
    if !project_deps.has_mod_file() || project_deps.locked_modules().is_empty() {
        return Ok(());
    }
    ensure_vfs_locked_modules(project_deps.into_locked_modules()).await
}

// ── Version resolution ──────────────────────────────────────────────────────

/// Fetch the latest release version for a module from the GitHub API.
async fn fetch_latest_module_version_typed(module: &str) -> ModuleInstallResult<String> {
    let module_path = vo_module::identity::ModulePath::parse(module).map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::VersionResolve,
            ModuleInstallErrorKind::ParseFailed,
            format!("{error}"),
        )
        .with_module(module)
    })?;
    let repo = vo_module::registry::repository_id(&module_path);
    let module_root = module_path.module_root();
    let fetch_start = crate::now_ms();
    log_dependency_version_resolve_start(module);

    // For root-level modules, use the /releases/latest endpoint.
    // For sub-module repos (module_root != "."), list releases and find the matching tag prefix.
    if module_root == "." {
        let api_url = format!(
            "https://api.github.com/repos/{}/{}/releases/latest",
            repo.owner, repo.repo,
        );
        let bytes = fetch::fetch_bytes(&api_url).await?;
        let text = String::from_utf8(bytes).map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::VersionResolve,
                ModuleInstallErrorKind::ParseFailed,
                format!("invalid UTF-8 in GitHub API response: {}", error),
            )
            .with_module(module)
        })?;
        let value: serde_json::Value = serde_json::from_str(&text).map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::VersionResolve,
                ModuleInstallErrorKind::ParseFailed,
                format!("invalid JSON from GitHub API for {}: {}", module, error),
            )
            .with_module(module)
        })?;
        let tag = value["tag_name"].as_str().ok_or_else(|| {
            ModuleInstallError::new(
                ModuleInstallStage::VersionResolve,
                ModuleInstallErrorKind::Missing,
                format!("no tag_name in GitHub latest release for {}", module),
            )
            .with_module(module)
        })?;
        let version = vo_module::registry::version_from_tag(&module_path, tag)
            .ok_or_else(|| {
                ModuleInstallError::new(
                    ModuleInstallStage::VersionResolve,
                    ModuleInstallErrorKind::ParseFailed,
                    format!(
                        "invalid tag_name '{}' in GitHub latest release for {}",
                        tag, module
                    ),
                )
                .with_module(module)
            })?
            .to_string();
        log_dependency_version_resolved(module, &version, fetch_start);
        return Ok(version);
    } else {
        let api_url = format!(
            "https://api.github.com/repos/{}/{}/releases?per_page=20",
            repo.owner, repo.repo,
        );
        let bytes = fetch::fetch_bytes(&api_url).await?;
        let text = String::from_utf8(bytes).map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::VersionResolve,
                ModuleInstallErrorKind::ParseFailed,
                format!("invalid UTF-8 in GitHub API response: {}", error),
            )
            .with_module(module)
        })?;
        let releases: Vec<serde_json::Value> = serde_json::from_str(&text).map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::VersionResolve,
                ModuleInstallErrorKind::ParseFailed,
                format!("invalid JSON from GitHub API for {}: {}", module, error),
            )
            .with_module(module)
        })?;
        let prefix = format!("{}/", module_root);
        for release in &releases {
            if let Some(tag) = release["tag_name"].as_str() {
                if let Some(version) = vo_module::registry::version_from_tag(&module_path, tag) {
                    let version = version.to_string();
                    log_dependency_version_resolved(module, &version, fetch_start);
                    return Ok(version);
                }
            }
        }
        Err(ModuleInstallError::new(
            ModuleInstallStage::VersionResolve,
            ModuleInstallErrorKind::Missing,
            format!(
                "no release found for module {} (tag prefix '{}')",
                module, prefix
            ),
        )
        .with_module(module))
    }
}

pub fn collect_installed_vfs_module_specs(
    modules: &[String],
) -> Result<Vec<(String, String)>, String> {
    collect_installed_vfs_module_specs_typed(modules).map_err(stringify_module_install_error)
}

fn collect_installed_vfs_module_specs_typed(
    modules: &[String],
) -> ModuleInstallResult<Vec<(String, String)>> {
    let mut installed = Vec::new();
    for module in modules {
        let version = discover_vfs_installed_version(module)?.ok_or_else(|| {
            ModuleInstallError::new(
                ModuleInstallStage::Vfs,
                ModuleInstallErrorKind::Missing,
                format!("module {} is not installed in the VFS", module),
            )
            .with_module(module)
        })?;
        installed.push((module.clone(), version));
    }
    Ok(installed)
}

async fn resolve_module_version_typed(module: &str) -> ModuleInstallResult<String> {
    if let Some(version) = discover_vfs_installed_version(module)? {
        log_dependency_version_cached(module, &version);
        return Ok(version);
    }
    fetch_latest_module_version_typed(module).await
}

/// Resolve, install, and return `(module, version)` for a single external module.
pub async fn resolve_and_install_module(module: &str) -> Result<(String, String), String> {
    resolve_and_install_module_typed(module)
        .await
        .map_err(stringify_module_install_error)
}

async fn resolve_and_install_module_typed(module: &str) -> ModuleInstallResult<(String, String)> {
    let version = resolve_module_version_typed(module).await?;
    ensure_vfs_versioned_module_closure(vec![(module.to_string(), version.clone())]).await?;
    Ok((module.to_string(), version))
}

// ── Locked module reading from VFS ──────────────────────────────────────────

fn read_locked_module_from_vfs(module: &str, version: &str) -> ModuleInstallResult<LockedModule> {
    let manifest_path = vfs_module_file_path(module, version, "vo.release.json");
    let manifest_content = read_vfs_text(&manifest_path)?;
    vo_module::lock::locked_module_from_requested_manifest_raw(
        manifest_content.as_bytes(),
        module,
        version,
    )
    .map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Manifest,
            ModuleInstallErrorKind::ParseFailed,
            format!(
                "parse release manifest for {}@{}: {}",
                module, version, error
            ),
        )
        .with_module_version(module, version)
    })
}

// ── Synthetic project files ─────────────────────────────────────────────────

pub fn collect_vfs_locked_module_closure(
    initial: &[(String, String)],
) -> Result<Vec<LockedModule>, String> {
    collect_vfs_locked_module_closure_typed(initial).map_err(stringify_module_install_error)
}

fn collect_vfs_locked_module_closure_typed(
    initial: &[(String, String)],
) -> ModuleInstallResult<Vec<LockedModule>> {
    let initial = initial
        .iter()
        .map(|(module, version)| {
            ModuleSelection::parse(module, version).map_err(|error| {
                ModuleInstallError::new(
                    ModuleInstallStage::Selection,
                    ModuleInstallErrorKind::ParseFailed,
                    error.to_string(),
                )
                .with_module_version(module, version)
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut planner = ModuleSelectionPlanner::new(initial).map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Selection,
            ModuleInstallErrorKind::ValidationFailed,
            error.to_string(),
        )
    })?;
    let mut resolved = Vec::new();

    while let Some(selection) = planner.next() {
        let module = selection.module.as_str().to_string();
        let version = selection.version.to_string();
        let locked = read_locked_module_from_vfs(&module, &version)?;
        let deps = read_vfs_locked_selections(&module, &version)?;
        planner.push_all(deps).map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::Selection,
                ModuleInstallErrorKind::ValidationFailed,
                error.to_string(),
            )
            .with_module_version(&module, &version)
        })?;
        resolved.push(locked);
    }

    resolved.sort_by(|a, b| {
        a.path
            .as_str()
            .cmp(b.path.as_str())
            .then_with(|| a.version.to_string().cmp(&b.version.to_string()))
    });
    Ok(resolved)
}
