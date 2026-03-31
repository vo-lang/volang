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

use std::collections::{HashMap, HashSet};

use vo_common::vfs::MemoryFs;
use vo_module::cache::layout::{SOURCE_DIGEST_MARKER, VERSION_MARKER};
use vo_module::digest::Digest;
use vo_module::schema::lockfile::{LockedArtifact, LockedModule};

use extension::*;
use log::*;
use vfs_io::*;

// ── Public re-exports ───────────────────────────────────────────────────────

pub use extension::load_ext_if_present;

// ── Install a single module by spec ─────────────────────────────────────────

pub async fn install_module_to_vfs(spec: &str) -> Result<String, String> {
    let (module, version) = spec
        .rsplit_once('@')
        .filter(|(m, v)| !m.is_empty() && !v.is_empty())
        .map(|(m, v)| (m.to_string(), v.to_string()))
        .ok_or_else(|| format!("invalid spec {:?}: expected module@version", spec))?;

    let fetch_start = crate::now_ms();
    log_dependency_fetch_start(&module, &version);
    let files = fetch::fetch_module_files(&module, &version)
        .await
        .map_err(|e| format!("fetch module {}: {}", module, e))?;
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
            || fetch::fetch_wasm_binary_from_manifest(&module, &version, &manifest, &wasm_extension.wasm),
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

async fn install_locked_module_to_vfs(locked: &LockedModule) -> Result<String, String> {
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

async fn ensure_vfs_locked_modules(initial: Vec<LockedModule>) -> Result<(), String> {
    let mut visited = HashSet::new();
    let mut selected_versions = HashMap::new();
    for locked in initial {
        let module = locked.path.as_str().to_string();
        let version = locked.version.to_string();
        remember_selected_version(&mut selected_versions, &module, &version)?;
        let visit_key = format!("{}@{}", module, version);
        if !visited.insert(visit_key) {
            continue;
        }

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
) -> Result<(), String> {
    let mut visited = HashSet::new();
    let mut selected_versions = HashMap::new();
    let mut stack = initial;

    while let Some((module, version)) = stack.pop() {
        remember_selected_version(&mut selected_versions, &module, &version)?;
        let visit_key = format!("{}@{}", module, version);
        if !visited.insert(visit_key) {
            continue;
        }

        if read_vfs_installed_version(&module, &version).as_deref() != Some(version.as_str()) {
            let spec = format!("{}@{}", module, version);
            install_module_to_vfs(&spec).await?;
        } else {
            load_ext_if_present(&module, &version).await;
        }

        let specs = read_vfs_locked_specs(&module, &version)?;
        stack.extend(specs);
    }

    Ok(())
}

// ── Public dependency ensuring ──────────────────────────────────────────────

pub async fn ensure_vfs_deps(vo_mod_content: &str, vo_lock_content: &str) -> Result<(), String> {
    let project_deps = inline_project_deps(vo_mod_content, vo_lock_content)?;
    if !project_deps.has_mod_file || project_deps.locked_modules.is_empty() {
        return Ok(());
    }
    ensure_vfs_locked_modules(project_deps.locked_modules).await
}

pub async fn ensure_vfs_deps_from_fs(local_fs: &MemoryFs, entry: &str) -> Result<(), String> {
    let entry_dir = std::path::Path::new(entry)
        .parent()
        .unwrap_or(std::path::Path::new("."));
    let project_deps =
        crate::compile::load_workspace_project_context(local_fs, entry_dir)?.project_deps;
    if !project_deps.has_mod_file || project_deps.locked_modules.is_empty() {
        return Ok(());
    }
    ensure_vfs_locked_modules(project_deps.locked_modules).await
}

// ── Version resolution ──────────────────────────────────────────────────────

/// Fetch the latest release version for a module from the GitHub API.
async fn fetch_latest_module_version(module: &str) -> Result<String, String> {
    let repo = vo_module::compat::module_repository(module)
        .ok_or_else(|| format!("not a github.com module path: {}", module))?;
    let module_root = vo_module::compat::module_root(module).unwrap_or_else(|| ".".to_string());
    let fetch_start = crate::now_ms();
    log_dependency_version_resolve_start(module);

    // For root-level modules, use the /releases/latest endpoint.
    // For sub-module repos (module_root != "."), list releases and find the matching tag prefix.
    if module_root == "." {
        let api_url = format!(
            "https://api.github.com/repos/{}/{}/releases/latest",
            repo.owner, repo.repo,
        );
        let bytes = fetch::fetch_bytes(&api_url)
            .await
            .map_err(|e| format!("failed to fetch latest release for {}: {}", module, e))?;
        let text = String::from_utf8(bytes)
            .map_err(|e| format!("invalid UTF-8 in GitHub API response: {}", e))?;
        let value: serde_json::Value = serde_json::from_str(&text)
            .map_err(|e| format!("invalid JSON from GitHub API for {}: {}", module, e))?;
        let tag = value["tag_name"]
            .as_str()
            .ok_or_else(|| format!("no tag_name in GitHub latest release for {}", module))?;
        log_dependency_version_resolved(module, tag, fetch_start);
        return Ok(tag.to_string());
    } else {
        let api_url = format!(
            "https://api.github.com/repos/{}/{}/releases?per_page=20",
            repo.owner, repo.repo,
        );
        let bytes = fetch::fetch_bytes(&api_url)
            .await
            .map_err(|e| format!("failed to list releases for {}: {}", module, e))?;
        let text = String::from_utf8(bytes)
            .map_err(|e| format!("invalid UTF-8 in GitHub API response: {}", e))?;
        let releases: Vec<serde_json::Value> = serde_json::from_str(&text)
            .map_err(|e| format!("invalid JSON from GitHub API for {}: {}", module, e))?;
        let prefix = format!("{}/", module_root);
        for release in &releases {
            if let Some(tag) = release["tag_name"].as_str() {
                if let Some(version) = tag.strip_prefix(&prefix) {
                    log_dependency_version_resolved(module, version, fetch_start);
                    return Ok(version.to_string());
                }
            }
        }
        Err(format!(
            "no release found for module {} (tag prefix '{}')",
            module, prefix
        ))
    }
}

/// Resolve a module version: check VFS cache first, then fetch latest from GitHub.
pub async fn resolve_module_version(module: &str) -> Result<String, String> {
    if let Some(version) = discover_vfs_installed_version(module) {
        log_dependency_version_cached(module, &version);
        return Ok(version);
    }
    fetch_latest_module_version(module).await
}

/// Resolve, install, and return `(module, version)` for a single external module.
pub async fn resolve_and_install_module(module: &str) -> Result<(String, String), String> {
    let version = resolve_module_version(module).await?;
    ensure_vfs_versioned_module_closure(vec![(module.to_string(), version.clone())]).await?;
    Ok((module.to_string(), version))
}

// ── Locked module reading from VFS ──────────────────────────────────────────

pub fn read_locked_module_from_vfs(module: &str, version: &str) -> Result<LockedModule, String> {
    let manifest_path = vfs_module_file_path(module, version, "vo.release.json");
    let manifest_content = read_vfs_text(&manifest_path)?;
    let manifest = vo_module::schema::manifest::ReleaseManifest::parse(&manifest_content)
        .map_err(|e| format!("parse release manifest for {}@{}: {}", module, version, e))?;

    let manifest_digest = Digest::from_sha256(manifest_content.as_bytes());

    let deps: Vec<vo_module::identity::ModulePath> =
        manifest.require.iter().map(|r| r.module.clone()).collect();

    let artifacts: Vec<LockedArtifact> = manifest
        .artifacts
        .iter()
        .map(|a| LockedArtifact {
            id: a.id.clone(),
            size: a.size,
            digest: a.digest.clone(),
        })
        .collect();

    Ok(LockedModule {
        path: manifest.module,
        version: manifest.version,
        vo: manifest.vo,
        commit: manifest.commit,
        release_manifest: manifest_digest,
        source: manifest.source.digest,
        deps,
        artifacts,
    })
}

// ── Synthetic project files ─────────────────────────────────────────────────

fn collect_vfs_locked_module_closure(
    initial: Vec<(String, String)>,
) -> Result<Vec<LockedModule>, String> {
    let mut visited = HashSet::new();
    let mut selected_versions = HashMap::new();
    let mut stack = initial;
    let mut resolved = Vec::new();

    while let Some((module, version)) = stack.pop() {
        remember_selected_version(&mut selected_versions, &module, &version)?;
        let visit_key = format!("{}@{}", module, version);
        if !visited.insert(visit_key) {
            continue;
        }

        let locked = read_locked_module_from_vfs(&module, &version)?;
        let specs = read_vfs_locked_specs(&module, &version)?;
        stack.extend(specs);
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

pub fn build_synthetic_project_files(
    synthetic_module: &str,
    installed: &[(String, String)],
) -> Result<(String, String), String> {
    use vo_module::schema::lockfile::{LockFile, LockRoot};

    let module = vo_module::identity::ModulePath::parse(synthetic_module).map_err(|e| {
        format!(
            "invalid synthetic module path '{}': {}",
            synthetic_module, e
        )
    })?;
    let vo = vo_module::version::ToolchainConstraint::parse("0.1.0")
        .map_err(|e| format!("invalid toolchain constraint: {}", e))?;

    // Build vo.mod
    let mut mod_content = format!("module {}\nvo {}\n", module, vo);
    if !installed.is_empty() {
        mod_content.push('\n');
        let mut sorted: Vec<&(String, String)> = installed.iter().collect();
        sorted.sort_by(|a, b| a.0.cmp(&b.0));
        for (m, v) in &sorted {
            mod_content.push_str(&format!("require {} {}\n", m, v));
        }
    }

    // Build vo.lock from installed release manifests
    let resolved = collect_vfs_locked_module_closure(installed.to_vec())?;

    let lock_file = LockFile {
        version: 1,
        created_by: "vo-studio".to_string(),
        root: LockRoot { module, vo },
        resolved,
    };
    let lock_content = lock_file.render();

    Ok((mod_content, lock_content))
}
