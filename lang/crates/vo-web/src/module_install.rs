//! Module installation lifecycle for WASM targets.
//!
//! Handles fetching, installing, and loading Vo modules into the JS VFS.
//! This includes:
//!
//! - **`install_module_to_vfs`** — fetch source + WASM binary from GitHub, write to VFS
//! - **`ensure_vfs_deps`** — ensure all `vo.mod` dependencies are installed
//! - **`ensure_vfs_deps_from_fs`** — same, but find `vo.mod` in a MemoryFs
//! - **`prepare_github_modules`** — playground path: detect imports, fetch, return MemoryFs
//! - **`load_ext_if_present`** — load pre-built WASM extension for a module

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use vo_common::vfs::MemoryFs;
use vo_module::digest::Digest;
use vo_module::ext_manifest::{WasmExtensionKind, WasmExtensionManifest};
use vo_module::materialize;
use vo_module::schema::lockfile::{LockFile, LockedArtifact, LockedModule};
use vo_module::schema::manifest::ReleaseManifest;
use vo_module::schema::modfile::ModFile;

use vo_web_runtime_wasm::ext_bridge;

const VFS_ARTIFACT_DIR: &str = ".vo-artifacts";
const VFS_WASM_TARGET: &str = "wasm32-unknown-unknown";

fn read_vfs_text(path: &str) -> Result<String, String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    if let Some(err) = err {
        return Err(format!("read {}: {}", path, err));
    }
    String::from_utf8(data).map_err(|e| format!("read {}: {}", path, e))
}

fn read_vfs_bytes(path: &str) -> Result<Vec<u8>, String> {
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

fn write_vfs_bytes(path: &str, bytes: &[u8]) -> Result<(), String> {
    ensure_vfs_parent_dir(path)?;
    if let Some(error) = vo_web_runtime_wasm::vfs::write_file(path, bytes, 0o644) {
        return Err(format!("write {}: {}", path, error));
    }
    Ok(())
}

fn write_vfs_text(path: &str, content: &str) -> Result<(), String> {
    write_vfs_bytes(path, content.as_bytes())
}

fn vfs_module_dir(module: &str, version: &str) -> String {
    format!("/{}", materialize::relative_module_dir(module, version))
}

fn vfs_module_file_path(module: &str, version: &str, file_name: &str) -> String {
    format!("{}/{}", vfs_module_dir(module, version), file_name)
}

fn vfs_source_digest_path(module: &str, version: &str) -> String {
    format!(
        "{}/{}",
        vfs_module_dir(module, version),
        materialize::SOURCE_DIGEST_MARKER
    )
}

fn vfs_release_manifest_path(module: &str, version: &str) -> String {
    format!("{}/vo.release.json", vfs_module_dir(module, version))
}

fn vfs_artifact_path(module: &str, version: &str, asset_name: &str) -> String {
    format!(
        "{}/{}/{}",
        vfs_module_dir(module, version),
        VFS_ARTIFACT_DIR,
        asset_name
    )
}

fn read_vfs_installed_version(module: &str, version: &str) -> Option<String> {
    let path = vfs_module_file_path(module, version, materialize::VERSION_MARKER);
    let content = read_vfs_text(&path).ok()?;
    let version = content.trim();
    if version.is_empty() {
        return None;
    }
    Some(version.to_string())
}

fn read_vfs_installed_source_digest(module: &str, version: &str) -> Option<String> {
    let content = read_vfs_text(&vfs_source_digest_path(module, version)).ok()?;
    let digest = content.trim();
    if digest.is_empty() {
        return None;
    }
    Some(digest.to_string())
}

fn read_vfs_release_manifest_digest(module: &str, version: &str) -> Option<String> {
    let content = read_vfs_text(&vfs_release_manifest_path(module, version)).ok()?;
    Some(Digest::from_sha256(content.as_bytes()).to_string())
}

fn collect_locked_specs(
    mod_file: &ModFile,
    lock_file: &LockFile,
) -> Result<Vec<(String, String)>, String> {
    vo_module::lock::verify_root_consistency(mod_file, lock_file)
        .map_err(|e| format!("vo.lock validation error: {}", e))?;
    vo_module::lock::verify_graph_completeness(mod_file, lock_file)
        .map_err(|e| format!("vo.lock validation error: {}", e))?;
    Ok(lock_file
        .resolved
        .iter()
        .map(|module| (module.path.as_str().to_string(), module.version.to_string()))
        .collect())
}

fn collect_locked_modules(
    mod_file: &ModFile,
    lock_file: &LockFile,
) -> Result<Vec<LockedModule>, String> {
    vo_module::lock::verify_root_consistency(mod_file, lock_file)
        .map_err(|e| format!("vo.lock validation error: {}", e))?;
    vo_module::lock::verify_graph_completeness(mod_file, lock_file)
        .map_err(|e| format!("vo.lock validation error: {}", e))?;
    Ok(lock_file.resolved.clone())
}

fn requires_registry_modules(mod_file: &ModFile) -> bool {
    !mod_file.require.is_empty()
}

fn remember_selected_version(
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

fn read_module_file(files: &[(PathBuf, String)], module: &str, file_name: &str) -> Option<String> {
    let wanted = Path::new(module).join(file_name);
    files
        .iter()
        .find(|(path, _)| path == &wanted)
        .map(|(_, content)| content.clone())
}

fn find_locked_wasm_artifact<'a>(
    locked: &'a LockedModule,
    asset_name: &str,
) -> Option<&'a LockedArtifact> {
    locked
        .artifacts
        .iter()
        .find(|artifact| artifact.id.target == VFS_WASM_TARGET && artifact.id.name == asset_name)
}

fn js_glue_data_url(js_text: &str) -> Result<String, String> {
    let encoded = wasm_bindgen::JsValue::from(js_sys::encode_uri_component(js_text))
        .as_string()
        .ok_or_else(|| "failed to encode wasm JS glue URL".to_string())?;
    Ok(format!("data:text/javascript;charset=utf-8,{}", encoded))
}

fn emit_log(record: crate::HostLogRecord) {
    let source = record.source.clone();
    let code = record.code.clone();
    let text = record.text.clone();
    crate::emit_host_log(record);
    match text {
        Some(text) => web_sys::console::log_1(&format!("[{}:{}] {}", source, code, text).into()),
        None => web_sys::console::log_1(&format!("[{}:{}]", source, code).into()),
    }
}

fn log_dependency_version_cached(module: &str, version: &str) {
    emit_log(
        crate::HostLogRecord::new("vo-web", "dependency_version_cached", "success")
            .module(module)
            .version(version),
    );
}

fn log_dependency_version_resolve_start(module: &str) {
    emit_log(
        crate::HostLogRecord::new("vo-web", "dependency_version_resolve_start", "system")
            .module(module),
    );
}

fn log_dependency_version_resolved(module: &str, version: &str, start_ms: f64) {
    emit_log(
        crate::HostLogRecord::new("vo-web", "dependency_version_resolved", "success")
            .module(module)
            .version(version)
            .duration_ms(js_sys::Date::now() - start_ms),
    );
}

fn log_dependency_fetch_start(module: &str, version: &str) {
    emit_log(
        crate::HostLogRecord::new("vo-web", "dependency_fetch_start", "system")
            .module(module)
            .version(version),
    );
}

fn log_dependency_fetch_done(module: &str, version: &str, start_ms: f64) {
    emit_log(
        crate::HostLogRecord::new("vo-web", "dependency_fetch_done", "success")
            .module(module)
            .version(version)
            .duration_ms(js_sys::Date::now() - start_ms),
    );
}

fn log_extension_cached(module: &str, version: &str) {
    emit_log(
        crate::HostLogRecord::new("vo-web", "extension_cached", "success")
            .module(module)
            .version(version),
    );
}

fn log_extension_asset_load_start(
    module: &str,
    version: &str,
    asset_kind: &str,
    asset_name: &str,
    cached: bool,
) {
    emit_log(
        crate::HostLogRecord::new("vo-web", "extension_asset_load_start", "system")
            .module(module)
            .version(version)
            .asset(asset_kind, asset_name)
            .cached(cached),
    );
}

fn log_extension_asset_load_done(
    module: &str,
    version: &str,
    asset_kind: &str,
    asset_name: &str,
    cached: bool,
    start_ms: f64,
) {
    emit_log(
        crate::HostLogRecord::new("vo-web", "extension_asset_load_done", "success")
            .module(module)
            .version(version)
            .asset(asset_kind, asset_name)
            .cached(cached)
            .duration_ms(js_sys::Date::now() - start_ms),
    );
}

fn log_extension_load_error(module: &str, version: &str, error: impl std::fmt::Display) {
    web_sys::console::warn_1(
        &format!(
            "[vo-web] ext module {}@{} load failed: {}",
            module, version, error
        )
        .into(),
    );
}

fn log_extension_asset_load_error(
    module: &str,
    version: &str,
    asset_kind: &str,
    asset_name: &str,
    error: impl std::fmt::Display,
) {
    web_sys::console::warn_1(
        &format!(
            "[vo-web] ext {} fetch failed for {}@{} ({}): {}",
            asset_kind, module, version, asset_name, error,
        )
        .into(),
    );
}

fn log_extension_cache_error(
    module: &str,
    version: &str,
    asset_kind: &str,
    asset_name: &str,
    error: impl std::fmt::Display,
) {
    web_sys::console::warn_1(
        &format!(
            "[vo-web] failed to cache ext {} for {}@{} ({}): {}",
            asset_kind, module, version, asset_name, error,
        )
        .into(),
    );
}

async fn load_wasm_extension_bytes(
    module: &str,
    wasm_bytes: &[u8],
    js_glue_text: Option<&str>,
) -> Result<(), String> {
    let js_glue_url = match js_glue_text {
        Some(js_glue_text) => js_glue_data_url(js_glue_text)?,
        None => String::new(),
    };
    ext_bridge::load_wasm_ext_module(module, wasm_bytes, &js_glue_url).await
}

fn write_versioned_module_files_to_vfs(
    module: &str,
    version: &str,
    files: &[(PathBuf, String)],
) -> Result<(), String> {
    let module_prefix = Path::new(module);
    let install_root = PathBuf::from(materialize::relative_module_dir(module, version));
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

fn release_manifest_from_files(
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

fn validate_vfs_installed_module(locked: &LockedModule) -> Result<(), String> {
    let module = locked.path.as_str();
    let version = locked.version.to_string();
    let vo_mod_path = vfs_module_file_path(module, &version, "vo.mod");
    let mod_content = read_vfs_text(&vo_mod_path).map_err(|_| {
        format!(
            "locked module {}@{} is not installed correctly in the VFS (found <missing vo.mod>)",
            module, version,
        )
    })?;
    let mod_file = ModFile::parse(&mod_content).map_err(|error| {
        format!(
            "invalid cached vo.mod for {}@{} in the VFS: {}",
            module, version, error
        )
    })?;
    if mod_file.module != locked.path {
        return Err(format!(
            "cached vo.mod module mismatch for {}@{} in the VFS: expected {}, found {}",
            module, version, module, mod_file.module,
        ));
    }
    if mod_file.vo != locked.vo {
        return Err(format!(
            "cached vo.mod toolchain constraint mismatch for {}@{} in the VFS: expected {}, found {}",
            module, version, locked.vo, mod_file.vo,
        ));
    }
    let installed_version = read_vfs_installed_version(module, &version)
        .ok_or_else(|| format!(
            "locked module {}@{} is not installed correctly in the VFS (found <missing .vo-version>)",
            module, version,
        ))?;
    if installed_version != version {
        return Err(format!(
            "locked module {}@{} is not installed correctly in the VFS (found {})",
            module, version, installed_version,
        ));
    }
    let source_digest = read_vfs_installed_source_digest(module, &version).ok_or_else(|| {
        format!(
            "locked module {}@{} is missing source digest metadata at {}",
            module,
            version,
            vfs_source_digest_path(module, &version),
        )
    })?;
    if source_digest != locked.source.as_str() {
        return Err(format!(
            "locked module {}@{} source digest mismatch in the VFS: expected {}, found {}",
            module, version, locked.source, source_digest,
        ));
    }
    let manifest_digest = read_vfs_release_manifest_digest(module, &version).ok_or_else(|| {
        format!(
            "locked module {}@{} is missing {} in the VFS",
            module,
            version,
            vfs_release_manifest_path(module, &version),
        )
    })?;
    if manifest_digest != locked.release_manifest.as_str() {
        return Err(format!(
            "locked module {}@{} release manifest digest mismatch in the VFS: expected {}, found {}",
            module, version, locked.release_manifest, manifest_digest,
        ));
    }
    Ok(())
}

async fn load_locked_ext_from_vfs(locked: &LockedModule) -> Result<(), String> {
    let module = locked.path.as_str();
    let version = locked.version.to_string();
    let Some(wasm_extension) = read_wasm_extension_from_vfs(module, &version) else {
        return Ok(());
    };

    let wasm_artifact =
        find_locked_wasm_artifact(locked, &wasm_extension.wasm).ok_or_else(|| {
            format!(
                "vo.lock is missing wasm artifact {} for {}@{}",
                wasm_extension.wasm, module, version,
            )
        })?;
    let wasm_path = vfs_artifact_path(module, &version, &wasm_artifact.id.name);
    let wasm_bytes = read_vfs_bytes(&wasm_path)?;
    let wasm_digest = Digest::from_sha256(&wasm_bytes).to_string();
    if wasm_digest != wasm_artifact.digest.as_str() {
        return Err(format!(
            "locked wasm artifact {} for {}@{} digest mismatch in the VFS: expected {}, found {}",
            wasm_artifact.id.name, module, version, wasm_artifact.digest, wasm_digest,
        ));
    }

    let js_glue_text = match wasm_extension.kind {
        WasmExtensionKind::Bindgen => {
            let js_glue_name = wasm_extension.js_glue.as_deref().unwrap_or_default();
            let js_glue_artifact =
                find_locked_wasm_artifact(locked, js_glue_name).ok_or_else(|| {
                    format!(
                        "vo.lock is missing wasm JS glue artifact {} for {}@{}",
                        js_glue_name, module, version,
                    )
                })?;
            let js_glue_path = vfs_artifact_path(module, &version, &js_glue_artifact.id.name);
            let js_glue_bytes = read_vfs_bytes(&js_glue_path)?;
            let js_glue_digest = Digest::from_sha256(&js_glue_bytes).to_string();
            if js_glue_digest != js_glue_artifact.digest.as_str() {
                return Err(format!(
                    "locked wasm JS glue artifact {} for {}@{} digest mismatch in the VFS: expected {}, found {}",
                    js_glue_artifact.id.name, module, version, js_glue_artifact.digest, js_glue_digest,
                ));
            }
            Some(String::from_utf8(js_glue_bytes).map_err(|error| {
                format!(
                    "wasm JS glue for {}@{} is not valid UTF-8: {}",
                    module, version, error
                )
            })?)
        }
        WasmExtensionKind::Standalone => None,
    };

    load_wasm_extension_bytes(module, &wasm_bytes, js_glue_text.as_deref()).await
}

async fn install_locked_module_to_vfs(locked: &LockedModule) -> Result<String, String> {
    let module = locked.path.as_str();
    let version = locked.version.to_string();

    let fetch_start = js_sys::Date::now();
    log_dependency_fetch_start(module, &version);
    let files = wasm_fetch::fetch_locked_module_files(locked).await?;
    log_dependency_fetch_done(module, &version, fetch_start);
    write_versioned_module_files_to_vfs(module, &version, &files)?;
    write_vfs_text(
        &vfs_module_file_path(module, &version, materialize::VERSION_MARKER),
        &format!("{}\n", version),
    )?;
    write_vfs_text(
        &vfs_module_file_path(module, &version, materialize::SOURCE_DIGEST_MARKER),
        &format!("{}\n", locked.source),
    )?;

    if let Some(wasm_extension) = read_wasm_extension_from_vfs(module, &version) {
        let wasm_fetch_start = js_sys::Date::now();
        log_extension_asset_load_start(module, &version, "wasm", &wasm_extension.wasm, false);
        let wasm_bytes = wasm_fetch::fetch_locked_wasm_binary(locked, &wasm_extension.wasm).await?;
        log_extension_asset_load_done(
            module,
            &version,
            "wasm",
            &wasm_extension.wasm,
            false,
            wasm_fetch_start,
        );
        write_vfs_bytes(
            &vfs_artifact_path(module, &version, &wasm_extension.wasm),
            &wasm_bytes,
        )?;
        if let WasmExtensionKind::Bindgen = wasm_extension.kind {
            let js_glue_name = wasm_extension.js_glue.as_deref().unwrap_or_default();
            let js_glue_fetch_start = js_sys::Date::now();
            log_extension_asset_load_start(module, &version, "js_glue", js_glue_name, false);
            let js_glue_text =
                wasm_fetch::fetch_locked_wasm_js_glue_text(locked, js_glue_name).await?;
            log_extension_asset_load_done(
                module,
                &version,
                "js_glue",
                js_glue_name,
                false,
                js_glue_fetch_start,
            );
            write_vfs_text(
                &vfs_artifact_path(module, &version, js_glue_name),
                &js_glue_text,
            )?;
        }
        load_locked_ext_from_vfs(locked).await?;
    }

    Ok(vfs_module_dir(module, &version))
}

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

async fn ensure_vfs_versioned_module_closure(initial: Vec<(String, String)>) -> Result<(), String> {
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

fn read_vfs_locked_specs(module: &str, version: &str) -> Result<Vec<(String, String)>, String> {
    let vo_mod_path = vfs_module_file_path(module, version, "vo.mod");
    let dep_vo_mod = read_vfs_text(&vo_mod_path).map_err(|_| {
        format!(
            "module {}@{} is missing cached vo.mod in the VFS",
            module, version
        )
    })?;
    let dep_mod_file = ModFile::parse(&dep_vo_mod)
        .map_err(|e| format!("vo.mod parse error for {}: {}", module, e))?;
    if !requires_registry_modules(&dep_mod_file) {
        return Ok(Vec::new());
    }

    let vo_lock_path = vfs_module_file_path(module, version, "vo.lock");
    let dep_vo_lock = read_vfs_text(&vo_lock_path).map_err(|_| {
        format!(
            "module {}@{} requires external modules but vo.lock is missing",
            module, version
        )
    })?;
    let dep_lock_file = LockFile::parse(&dep_vo_lock)
        .map_err(|e| format!("vo.lock parse error for {}: {}", module, e))?;
    collect_locked_specs(&dep_mod_file, &dep_lock_file)
}

/// Load the pre-built WASM extension binary for a module if one exists on GitHub.
/// A missing binary (HTTP 404) is silently skipped — it just means the module
/// is pure Vo with no native extension.  Other errors are logged as warnings.
///
/// Determines whether the module uses wasm-bindgen by reading `vo.ext.toml`
/// from the VFS (already installed by fetch or OPFS persistence).  This works
/// correctly for both freshly-fetched and cached modules.
pub(crate) async fn load_ext_if_present(module: &str, version: &str) {
    let Some(wasm_extension) = read_wasm_extension_from_vfs(module, version) else {
        return;
    };

    match load_cached_ext_from_vfs(module, version).await {
        Ok(true) => return,
        Ok(false) => {}
        Err(e) => {
            log_extension_load_error(module, version, e);
        }
    }

    let wasm_fetch_start = js_sys::Date::now();
    log_extension_asset_load_start(module, version, "wasm", &wasm_extension.wasm, true);
    let wasm_bytes =
        match wasm_fetch::fetch_wasm_binary(module, version, &wasm_extension.wasm).await {
            Ok(bytes) => bytes,
            Err(e) => {
                log_extension_asset_load_error(module, version, "wasm", &wasm_extension.wasm, e);
                return;
            }
        };
    log_extension_asset_load_done(
        module,
        version,
        "wasm",
        &wasm_extension.wasm,
        true,
        wasm_fetch_start,
    );
    if let Err(e) = write_vfs_bytes(
        &vfs_artifact_path(module, version, &wasm_extension.wasm),
        &wasm_bytes,
    ) {
        log_extension_cache_error(module, version, "wasm", &wasm_extension.wasm, e);
    }

    let js_glue_text = match wasm_extension.kind {
        WasmExtensionKind::Bindgen => {
            let js_glue_name = wasm_extension.js_glue.as_deref().unwrap_or_default();
            let js_glue_fetch_start = js_sys::Date::now();
            log_extension_asset_load_start(module, version, "js_glue", js_glue_name, true);
            match wasm_fetch::fetch_wasm_js_glue_text(module, version, js_glue_name).await {
                Ok(text) => {
                    log_extension_asset_load_done(
                        module,
                        version,
                        "js_glue",
                        js_glue_name,
                        true,
                        js_glue_fetch_start,
                    );
                    if let Err(e) =
                        write_vfs_text(&vfs_artifact_path(module, version, js_glue_name), &text)
                    {
                        log_extension_cache_error(module, version, "js_glue", js_glue_name, e);
                    }
                    Some(text)
                }
                Err(e) => {
                    log_extension_asset_load_error(module, version, "js_glue", js_glue_name, e);
                    return;
                }
            }
        }
        WasmExtensionKind::Standalone => None,
    };

    if let Err(e) = load_wasm_extension_bytes(module, &wasm_bytes, js_glue_text.as_deref()).await {
        log_extension_load_error(module, version, e);
    }
}

fn read_wasm_extension_from_vfs(module: &str, version: &str) -> Option<WasmExtensionManifest> {
    let path = vfs_module_file_path(module, version, "vo.ext.toml");
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(&path);
    if err.is_some() {
        return None;
    }
    match String::from_utf8(data) {
        Ok(content) => vo_module::ext_manifest::wasm_extension_from_content(&content),
        Err(_) => None,
    }
}

async fn load_cached_ext_from_vfs(module: &str, version: &str) -> Result<bool, String> {
    let Some(wasm_extension) = read_wasm_extension_from_vfs(module, version) else {
        return Ok(false);
    };
    let wasm_path = vfs_artifact_path(module, version, &wasm_extension.wasm);
    let wasm_bytes = match read_vfs_bytes(&wasm_path) {
        Ok(bytes) => bytes,
        Err(_) => return Ok(false),
    };
    let js_glue_text = match wasm_extension.kind {
        WasmExtensionKind::Bindgen => {
            let js_glue_name = wasm_extension.js_glue.as_deref().unwrap_or_default();
            let js_glue_path = vfs_artifact_path(module, version, js_glue_name);
            let js_glue_bytes = match read_vfs_bytes(&js_glue_path) {
                Ok(bytes) => bytes,
                Err(_) => return Ok(false),
            };
            Some(String::from_utf8(js_glue_bytes).map_err(|error| {
                format!(
                    "cached wasm JS glue for {}@{} is not valid UTF-8: {}",
                    module, version, error
                )
            })?)
        }
        WasmExtensionKind::Standalone => None,
    };
    load_wasm_extension_bytes(module, &wasm_bytes, js_glue_text.as_deref()).await?;
    log_extension_cached(module, version);
    Ok(true)
}

pub async fn ensure_vfs_deps(vo_mod_content: &str, vo_lock_content: &str) -> Result<(), String> {
    let mod_file =
        ModFile::parse(vo_mod_content).map_err(|e| format!("vo.mod parse error: {}", e))?;
    if !requires_registry_modules(&mod_file) {
        return Ok(());
    }

    let lock_file =
        LockFile::parse(vo_lock_content).map_err(|e| format!("vo.lock parse error: {}", e))?;
    let initial = collect_locked_modules(&mod_file, &lock_file)?;
    ensure_vfs_locked_modules(initial).await
}

pub async fn ensure_vfs_deps_from_fs(local_fs: &MemoryFs, entry: &str) -> Result<(), String> {
    use vo_common::vfs::FileSystem;

    let entry_dir = std::path::Path::new(entry)
        .parent()
        .unwrap_or(std::path::Path::new("."));
    let mod_candidates = [entry_dir.join("vo.mod"), PathBuf::from("vo.mod")];
    let lock_candidates = [entry_dir.join("vo.lock"), PathBuf::from("vo.lock")];

    for candidate in &mod_candidates {
        if let Ok(mod_content) = local_fs.read_file(candidate) {
            let mod_file =
                ModFile::parse(&mod_content).map_err(|e| format!("vo.mod parse error: {}", e))?;
            if !requires_registry_modules(&mod_file) {
                return Ok(());
            }

            for lock_candidate in &lock_candidates {
                if let Ok(lock_content) = local_fs.read_file(lock_candidate) {
                    return ensure_vfs_deps(&mod_content, &lock_content).await;
                }
            }

            return Err("this build requires external modules but vo.lock is missing".to_string());
        }
    }
    Ok(())
}

pub async fn prepare_github_modules(_source: &str) -> Result<(MemoryFs, String), String> {
    Err(
        "external module imports in web builds now require a project with vo.mod and vo.lock; source-scanned dependency fetching is no longer supported".to_string()
    )
}

pub async fn ensure_vfs_versioned_imports(_source: &str) -> Result<(), String> {
    Err(
        "version-suffixed imports are no longer supported in web builds; declare dependencies in vo.mod and vo.lock instead".to_string()
    )
}

pub async fn install_module_to_vfs(spec: &str) -> Result<String, String> {
    let (module, version) = spec
        .rsplit_once('@')
        .filter(|(m, v)| !m.is_empty() && !v.is_empty())
        .map(|(m, v)| (m.to_string(), v.to_string()))
        .ok_or_else(|| format!("invalid spec {:?}: expected module@version", spec))?;

    let fetch_start = js_sys::Date::now();
    log_dependency_fetch_start(&module, &version);
    let files = wasm_fetch::fetch_module_files(&module, &version)
        .await
        .map_err(|e| format!("fetch module {}: {}", module, e))?;
    log_dependency_fetch_done(&module, &version, fetch_start);
    let manifest = release_manifest_from_files(&module, &version, &files)?;
    write_versioned_module_files_to_vfs(&module, &version, &files)?;
    write_vfs_text(
        &vfs_module_file_path(&module, &version, materialize::VERSION_MARKER),
        &format!("{}\n", version),
    )?;
    write_vfs_text(
        &vfs_module_file_path(&module, &version, materialize::SOURCE_DIGEST_MARKER),
        &format!("{}\n", manifest.source.digest),
    )?;

    if let Some(wasm_extension) = read_wasm_extension_from_vfs(&module, &version) {
        let wasm_fetch_start = js_sys::Date::now();
        log_extension_asset_load_start(&module, &version, "wasm", &wasm_extension.wasm, false);
        let wasm_bytes =
            wasm_fetch::fetch_wasm_binary(&module, &version, &wasm_extension.wasm).await?;
        log_extension_asset_load_done(
            &module,
            &version,
            "wasm",
            &wasm_extension.wasm,
            false,
            wasm_fetch_start,
        );
        write_vfs_bytes(
            &vfs_artifact_path(&module, &version, &wasm_extension.wasm),
            &wasm_bytes,
        )?;
        let js_glue_text = match wasm_extension.kind {
            WasmExtensionKind::Bindgen => {
                let js_glue_name = wasm_extension.js_glue.as_deref().unwrap_or_default();
                let js_glue_fetch_start = js_sys::Date::now();
                log_extension_asset_load_start(&module, &version, "js_glue", js_glue_name, false);
                let js_glue_text =
                    wasm_fetch::fetch_wasm_js_glue_text(&module, &version, js_glue_name).await?;
                log_extension_asset_load_done(
                    &module,
                    &version,
                    "js_glue",
                    js_glue_name,
                    false,
                    js_glue_fetch_start,
                );
                write_vfs_text(
                    &vfs_artifact_path(&module, &version, js_glue_name),
                    &js_glue_text,
                )?;
                Some(js_glue_text)
            }
            WasmExtensionKind::Standalone => None,
        };
        load_wasm_extension_bytes(&module, &wasm_bytes, js_glue_text.as_deref()).await?;
    }

    Ok(vfs_module_dir(&module, &version))
}

pub fn discover_vfs_installed_version(module: &str) -> Option<String> {
    let encoded = vo_module::materialize::cache_key(module);
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
            vo_module::materialize::VERSION_MARKER
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

/// Fetch the latest release version for a module from the GitHub API.
async fn fetch_latest_module_version(module: &str) -> Result<String, String> {
    let repo = vo_module::compat::module_repository(module)
        .ok_or_else(|| format!("not a github.com module path: {}", module))?;
    let module_root = vo_module::compat::module_root(module).unwrap_or_else(|| ".".to_string());
    let fetch_start = js_sys::Date::now();
    log_dependency_version_resolve_start(module);

    // For root-level modules, use the /releases/latest endpoint.
    // For sub-module repos (module_root != "."), list releases and find the matching tag prefix.
    if module_root == "." {
        let api_url = format!(
            "https://api.github.com/repos/{}/{}/releases/latest",
            repo.owner, repo.repo,
        );
        let bytes = wasm_fetch::fetch_bytes(&api_url)
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
        let bytes = wasm_fetch::fetch_bytes(&api_url)
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
///
/// If the module is already installed in VFS, uses the cached version.
/// Otherwise fetches the latest version from GitHub, installs it, and returns.
pub async fn resolve_and_install_module(module: &str) -> Result<(String, String), String> {
    let version = resolve_module_version(module).await?;
    ensure_vfs_versioned_module_closure(vec![(module.to_string(), version.clone())]).await?;
    Ok((module.to_string(), version))
}

///
/// After `install_module_to_vfs` completes, the release manifest is stored at
/// `/<cache_key>/<version>/vo.release.json`.  This function reads it back and
/// builds a `LockedModule` suitable for generating a synthetic `vo.lock`.
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

/// Collects the VFS locked-module closure for a set of initial modules.
///
/// This function is used to generate synthetic `vo.lock` content that includes
/// transitive dependencies.
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

/// Build synthetic `vo.mod` and `vo.lock` content for a set of installed modules.
///
/// `synthetic_module` is the module name for the synthetic project (e.g. `"studio.examples"`).
/// `installed` is a list of `(module_path, version)` pairs.
///
/// Returns `(vo_mod_content, vo_lock_content)`.
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

// ── WASM-specific fetch helpers (browser Fetch API) ──────────────────────────
//
// These were previously in `vo-module-old/src/fetch.rs` under `#[cfg(target_arch = "wasm32")]`.
// They are WASM-platform-specific and belong in vo-web, not in the abstract module system.

mod wasm_fetch {
    use std::path::PathBuf;
    use wasm_bindgen::prelude::*;
    use wasm_bindgen::JsCast;
    use wasm_bindgen_futures::JsFuture;

    use vo_module::schema::lockfile::LockedModule;
    use vo_module::schema::manifest::ReleaseManifest;

    /// Rewrite GitHub release download URLs to go through the dev-server
    /// CORS proxy (`/gh-release/`).  Azure blob storage behind the GitHub
    /// 302 redirect does not send `Access-Control-Allow-Origin`, so direct
    /// browser fetches fail.  The proxy follows the redirect server-side.
    fn cors_proxy_url(url: &str) -> String {
        const GITHUB_PREFIX: &str = "https://github.com/";
        const PROXY_PREFIX: &str = "/gh-release/";
        if url.starts_with(GITHUB_PREFIX) && url.contains("/releases/download/") {
            return format!("{}{}", PROXY_PREFIX, &url[GITHUB_PREFIX.len()..]);
        }
        url.to_string()
    }

    /// Async HTTP GET → raw bytes (uses browser Fetch API).
    pub(super) async fn fetch_bytes(url: &str) -> Result<Vec<u8>, String> {
        let effective_url = cors_proxy_url(url);
        let window = web_sys::window().ok_or("no window object")?;

        let opts = web_sys::RequestInit::new();
        opts.set_method("GET");
        opts.set_cache(web_sys::RequestCache::NoStore);
        let request = web_sys::Request::new_with_str_and_init(&effective_url, &opts)
            .map_err(|e| e.as_string().unwrap_or_else(|| "request error".to_string()))?;

        let resp_value = JsFuture::from(window.fetch_with_request(&request))
            .await
            .map_err(|e| e.as_string().unwrap_or_else(|| "fetch error".to_string()))?;

        let resp: web_sys::Response = resp_value
            .dyn_into()
            .map_err(|_| "response cast error".to_string())?;

        if !resp.ok() {
            return Err(format!("HTTP {} for {}", resp.status(), url));
        }

        let ab_promise = resp.array_buffer().map_err(|e| {
            e.as_string()
                .unwrap_or_else(|| "array_buffer error".to_string())
        })?;
        let ab = JsFuture::from(ab_promise).await.map_err(|e| {
            e.as_string()
                .unwrap_or_else(|| "array_buffer await error".to_string())
        })?;

        let array = js_sys::Uint8Array::new(&ab);
        Ok(array.to_vec())
    }

    fn verify_download_bytes(
        bytes: &[u8],
        expected_size: u64,
        expected_digest: &str,
        label: &str,
    ) -> Result<(), String> {
        if bytes.len() as u64 != expected_size {
            return Err(format!(
                "{} size mismatch: expected {} bytes, got {} bytes",
                label,
                expected_size,
                bytes.len(),
            ));
        }
        let Some(expected_hex) = expected_digest.strip_prefix("sha256:") else {
            return Err(format!(
                "{} declares an invalid digest: {}",
                label, expected_digest
            ));
        };
        let actual = vo_module::digest::Digest::from_sha256(bytes);
        let actual = actual.hex();
        if !expected_hex.eq_ignore_ascii_case(actual) {
            return Err(format!(
                "{} digest mismatch: expected sha256:{}, got sha256:{}",
                label, expected_hex, actual,
            ));
        }
        Ok(())
    }

    // ── URL building ─────────────────────────────────────────────────────────

    fn release_manifest_url(module: &str, version: &str) -> Result<String, String> {
        asset_download_url(module, version, "vo.release.json")
    }

    fn release_tag(module: &str, version: &str) -> String {
        let root = vo_module::compat::module_root(module).unwrap_or_else(|| ".".to_string());
        if root == "." {
            version.to_string()
        } else {
            format!("{}/{}", root, version)
        }
    }

    fn asset_download_url(module: &str, version: &str, asset_name: &str) -> Result<String, String> {
        let repo = vo_module::compat::module_repository(module)
            .ok_or_else(|| format!("not a github.com module path: {}", module))?;
        let tag = release_tag(module, version);
        Ok(format!(
            "https://github.com/{}/{}/releases/download/{}/{}",
            repo.owner, repo.repo, tag, asset_name,
        ))
    }

    fn source_download_url(manifest: &ReleaseManifest) -> String {
        vo_module::registry::release_download_url(
            &manifest.module,
            &manifest.version,
            &manifest.source.name,
        )
    }

    fn manifest_asset_download_url(manifest: &ReleaseManifest, asset_name: &str) -> String {
        vo_module::registry::release_download_url(&manifest.module, &manifest.version, asset_name)
    }

    // ── Release manifest fetch ───────────────────────────────────────────────

    fn parse_requested_release_manifest(
        content: &str,
        module: &str,
        version: &str,
    ) -> Result<ReleaseManifest, String> {
        let manifest = ReleaseManifest::parse(content).map_err(|error| {
            format!(
                "invalid release manifest for {}@{}: {}",
                module, version, error
            )
        })?;
        if manifest.module.as_str() != module {
            return Err(format!(
                "requested {}@{} but release manifest declares module {}",
                module, version, manifest.module,
            ));
        }
        if manifest.version.to_string() != version {
            return Err(format!(
                "requested {}@{} but release manifest declares version {}",
                module, version, manifest.version,
            ));
        }
        Ok(manifest)
    }

    fn validate_locked_manifest(
        manifest: &ReleaseManifest,
        manifest_digest: &str,
        locked: &LockedModule,
    ) -> Result<(), String> {
        let module = locked.path.as_str();
        let version = locked.version.to_string();
        if manifest_digest != locked.release_manifest.as_str() {
            return Err(format!(
                "release manifest digest mismatch for {}@{}: expected {}, found {}",
                module, version, locked.release_manifest, manifest_digest,
            ));
        }
        if manifest.source.digest != locked.source {
            return Err(format!(
                "release manifest source digest mismatch for {}@{}: expected {}, found {}",
                module, version, locked.source, manifest.source.digest,
            ));
        }
        if manifest.vo != locked.vo {
            return Err(format!(
                "release manifest vo constraint mismatch for {}@{}: expected {}, found {}",
                module, version, locked.vo, manifest.vo,
            ));
        }
        for artifact in &locked.artifacts {
            if !manifest
                .artifacts
                .iter()
                .any(|candidate| candidate.id == artifact.id && candidate.digest == artifact.digest)
            {
                return Err(format!(
                    "release manifest for {}@{} is missing locked artifact {} {} {}",
                    module, version, artifact.id.kind, artifact.id.target, artifact.id.name,
                ));
            }
        }
        Ok(())
    }

    async fn fetch_release_manifest_checked(
        module: &str,
        version: &str,
        locked: Option<&LockedModule>,
    ) -> Result<(ReleaseManifest, String), String> {
        let manifest_url = release_manifest_url(module, version)?;
        let manifest_bytes = fetch_bytes(&manifest_url).await.map_err(|e| {
            format!(
                "failed to fetch release manifest for {}@{}: {}",
                module, version, e
            )
        })?;
        let manifest_digest = vo_module::digest::Digest::from_sha256(&manifest_bytes).to_string();
        let manifest_content = String::from_utf8(manifest_bytes).map_err(|error| {
            format!(
                "release manifest for {}@{} is not valid UTF-8: {}",
                module, version, error
            )
        })?;
        let manifest = parse_requested_release_manifest(&manifest_content, module, version)?;
        if let Some(locked) = locked {
            validate_locked_manifest(&manifest, &manifest_digest, locked)?;
        }
        Ok((manifest, manifest_content))
    }

    async fn fetch_release_manifest(
        module: &str,
        version: &str,
    ) -> Result<ReleaseManifest, String> {
        fetch_release_manifest_checked(module, version, None)
            .await
            .map(|(manifest, _)| manifest)
    }

    // ── Source package extraction ─────────────────────────────────────────────

    fn extract_release_source_package_files(
        source_package: &[u8],
        manifest: &ReleaseManifest,
    ) -> Result<Vec<(PathBuf, String)>, String> {
        let entries = vo_module::materialize::extract_source_entries(source_package)
            .map_err(|e| format!("{} for {}@{}", e, manifest.module, manifest.version))?;
        if entries.is_empty() {
            return Err(format!(
                "no Vo files found in source package for {}@{}",
                manifest.module, manifest.version,
            ));
        }
        let module = manifest.module.as_str();
        Ok(entries
            .into_iter()
            .map(|e| {
                (
                    PathBuf::from(format!("{}/{}", module, e.relative_path.display())),
                    e.content,
                )
            })
            .collect())
    }

    fn append_release_manifest_file(
        files: &mut Vec<(PathBuf, String)>,
        module: &str,
        manifest_content: &str,
    ) {
        files.push((
            PathBuf::from(format!("{}/vo.release.json", module)),
            manifest_content.to_string(),
        ));
    }

    // ── Local module override hooks ──────────────────────────────────────────

    fn try_local_module_override(module: &str, version: &str) -> Option<Vec<(PathBuf, String)>> {
        let global = js_sys::global();
        let hook = js_sys::Reflect::get(&global, &JsValue::from_str("_voGetLocalModule")).ok()?;
        if !hook.is_function() {
            return None;
        }
        let func: js_sys::Function = hook.unchecked_into();
        let result: JsValue = func
            .call2(
                &JsValue::NULL,
                &JsValue::from_str(module),
                &JsValue::from_str(version),
            )
            .ok()?;
        if result.is_null() || result.is_undefined() {
            return None;
        }
        let arr = js_sys::Array::from(&result);
        let mut files = Vec::new();
        for i in 0..arr.length() {
            let entry = arr.get(i);
            let name = js_sys::Reflect::get(&entry, &JsValue::from_str("name"))
                .ok()?
                .as_string()?;
            let content = js_sys::Reflect::get(&entry, &JsValue::from_str("content"))
                .ok()?
                .as_string()?;
            let vfs_path = PathBuf::from(format!("{}/{}", module, name));
            files.push((vfs_path, content));
        }
        Some(files)
    }

    fn try_local_wasm_url(module: &str, version: &str) -> Option<String> {
        let global = js_sys::global();
        let hook = js_sys::Reflect::get(&global, &JsValue::from_str("_voGetLocalWasm")).ok()?;
        if !hook.is_function() {
            return None;
        }
        let func: js_sys::Function = hook.unchecked_into();
        let result: JsValue = func
            .call2(
                &JsValue::NULL,
                &JsValue::from_str(module),
                &JsValue::from_str(version),
            )
            .ok()?;
        result.as_string()
    }

    // ── Public fetch functions ────────────────────────────────────────────────

    pub async fn fetch_module_files(
        module: &str,
        version: &str,
    ) -> Result<Vec<(PathBuf, String)>, String> {
        if let Some(files) = try_local_module_override(module, version) {
            return Ok(files);
        }

        let (manifest, manifest_content) =
            fetch_release_manifest_checked(module, version, None).await?;

        let src_url = source_download_url(&manifest);
        let source_package = fetch_bytes(&src_url).await.map_err(|e| {
            format!(
                "failed to fetch source package for {}@{}: {}",
                module, version, e
            )
        })?;
        verify_download_bytes(
            &source_package,
            manifest.source.size,
            manifest.source.digest.as_str(),
            &format!("source package for {}@{}", module, version),
        )?;

        let mut files = extract_release_source_package_files(&source_package, &manifest)?;
        append_release_manifest_file(&mut files, module, &manifest_content);
        Ok(files)
    }

    pub async fn fetch_locked_module_files(
        locked: &LockedModule,
    ) -> Result<Vec<(PathBuf, String)>, String> {
        let module = locked.path.as_str();
        let version = locked.version.to_string();
        let (manifest, manifest_content) =
            fetch_release_manifest_checked(module, &version, Some(locked)).await?;
        let src_url = source_download_url(&manifest);
        let source_package = fetch_bytes(&src_url).await.map_err(|e| {
            format!(
                "failed to fetch source package for {}@{}: {}",
                module, version, e
            )
        })?;
        verify_download_bytes(
            &source_package,
            manifest.source.size,
            manifest.source.digest.as_str(),
            &format!("source package for {}@{}", module, version),
        )?;

        let mut files = extract_release_source_package_files(&source_package, &manifest)?;
        append_release_manifest_file(&mut files, module, &manifest_content);
        Ok(files)
    }

    fn find_manifest_artifact<'a>(
        manifest: &'a ReleaseManifest,
        target: &str,
        asset_name: &str,
    ) -> Option<&'a vo_module::schema::manifest::ManifestArtifact> {
        manifest
            .artifacts
            .iter()
            .find(|artifact| artifact.id.target == target && artifact.id.name == asset_name)
    }

    async fn fetch_locked_target_artifact_bytes(
        locked: &LockedModule,
        target: &str,
        asset_name: &str,
        label: &str,
    ) -> Result<Vec<u8>, String> {
        let module = locked.path.as_str();
        let version = locked.version.to_string();
        let (manifest, _) = fetch_release_manifest_checked(module, &version, Some(locked)).await?;
        let manifest_artifact =
            find_manifest_artifact(&manifest, target, asset_name).ok_or_else(|| {
                format!(
                    "release manifest for {}@{} is missing target artifact {} for {}",
                    module, version, asset_name, target,
                )
            })?;
        let locked_artifact = locked
            .artifacts
            .iter()
            .find(|a| a.id.target == target && a.id.name == asset_name)
            .ok_or_else(|| {
                format!(
                    "vo.lock is missing target artifact {} for {}@{} ({})",
                    asset_name, module, version, target,
                )
            })?;
        if manifest_artifact.digest != locked_artifact.digest {
            return Err(format!(
                "locked artifact digest mismatch for {}@{} {}: expected {}, found {}",
                module, version, asset_name, locked_artifact.digest, manifest_artifact.digest,
            ));
        }
        let bytes = fetch_bytes(&manifest_asset_download_url(
            &manifest,
            &manifest_artifact.id.name,
        ))
        .await
        .map_err(|e| {
            format!(
                "failed to fetch {} for {}@{}: {}",
                label, module, version, e
            )
        })?;
        verify_download_bytes(
            &bytes,
            manifest_artifact.size,
            manifest_artifact.digest.as_str(),
            &format!("{} for {}@{}", label, module, version),
        )?;
        Ok(bytes)
    }

    pub async fn fetch_locked_wasm_binary(
        locked: &LockedModule,
        asset_name: &str,
    ) -> Result<Vec<u8>, String> {
        fetch_locked_target_artifact_bytes(
            locked,
            "wasm32-unknown-unknown",
            asset_name,
            "wasm artifact",
        )
        .await
    }

    pub async fn fetch_locked_wasm_js_glue_text(
        locked: &LockedModule,
        asset_name: &str,
    ) -> Result<String, String> {
        let bytes = fetch_locked_target_artifact_bytes(
            locked,
            "wasm32-unknown-unknown",
            asset_name,
            "wasm JS glue",
        )
        .await?;
        String::from_utf8(bytes).map_err(|error| {
            format!(
                "wasm JS glue for {}@{} is not valid UTF-8: {}",
                locked.path, locked.version, error
            )
        })
    }

    pub async fn fetch_wasm_js_glue_text(
        module: &str,
        version: &str,
        asset_name: &str,
    ) -> Result<String, String> {
        let manifest = fetch_release_manifest(module, version).await?;
        let artifact = find_manifest_artifact(&manifest, "wasm32-unknown-unknown", asset_name)
            .ok_or_else(|| {
                format!(
                    "release manifest for {}@{} is missing wasm JS glue artifact {}",
                    module, version, asset_name,
                )
            })?;
        let bytes = fetch_bytes(&manifest_asset_download_url(&manifest, &artifact.id.name))
            .await
            .map_err(|e| {
                format!(
                    "failed to fetch wasm JS glue for {}@{}: {}",
                    module, version, e
                )
            })?;
        verify_download_bytes(
            &bytes,
            artifact.size,
            artifact.digest.as_str(),
            &format!("wasm JS glue for {}@{}", module, version),
        )?;
        String::from_utf8(bytes).map_err(|error| {
            format!(
                "wasm JS glue for {}@{} is not valid UTF-8: {}",
                module, version, error
            )
        })
    }

    pub async fn fetch_wasm_js_glue_url(
        module: &str,
        version: &str,
        asset_name: &str,
    ) -> Result<String, String> {
        let js_text = fetch_wasm_js_glue_text(module, version, asset_name).await?;
        super::js_glue_data_url(&js_text)
    }

    pub async fn fetch_wasm_binary(
        module: &str,
        version: &str,
        asset_name: &str,
    ) -> Result<Vec<u8>, String> {
        let url = if let Some(local_url) = try_local_wasm_url(module, version) {
            local_url
        } else {
            let manifest = fetch_release_manifest(module, version).await?;
            let artifact = manifest
                .artifacts
                .iter()
                .find(|a| a.id.kind == "extension-wasm" && a.id.target == "wasm32-unknown-unknown")
                .ok_or_else(|| format!(
                    "release manifest for {}@{} is missing extension-wasm artifact for wasm32-unknown-unknown",
                    module, version,
                ))?;
            if artifact.id.name != asset_name {
                return Err(format!(
                    "release manifest for {}@{} declares wasm artifact {}, but vo.ext.toml requests {}",
                    module, version, artifact.id.name, asset_name,
                ));
            }
            let url = manifest_asset_download_url(&manifest, &artifact.id.name);
            let bytes = fetch_bytes(&url).await.map_err(|e| {
                format!(
                    "failed to fetch wasm artifact for {}@{}: {}",
                    module, version, e
                )
            })?;
            verify_download_bytes(
                &bytes,
                artifact.size,
                artifact.digest.as_str(),
                &format!("wasm artifact for {}@{}", module, version),
            )?;
            return Ok(bytes);
        };
        fetch_bytes(&url).await.map_err(|e| {
            format!(
                "failed to fetch local wasm override for {}@{}: {}",
                module, version, e
            )
        })
    }
}
