//! WASM extension loading, caching, and validation for installed modules.

use vo_module::digest::verify_size_and_digest;
use vo_module::ext_manifest::{WasmExtensionKind, WasmExtensionManifest};
use vo_module::schema::lockfile::LockedModule;

use vo_web_runtime_wasm::ext_bridge;

use super::fetch as wasm_fetch;
use super::log::*;
use super::vfs_io::*;
use super::{
    stringify_module_install_error, ModuleInstallError, ModuleInstallErrorKind,
    ModuleInstallResult, ModuleInstallStage,
};

// ── Core loading ────────────────────────────────────────────────────────────

fn js_glue_data_url(js_text: &str) -> ModuleInstallResult<String> {
    let encoded = wasm_bindgen::JsValue::from(js_sys::encode_uri_component(js_text))
        .as_string()
        .ok_or_else(|| {
            ModuleInstallError::new(
                ModuleInstallStage::Extension,
                ModuleInstallErrorKind::ParseFailed,
                "failed to encode wasm JS glue URL",
            )
        })?;
    Ok(format!("data:text/javascript;charset=utf-8,{}", encoded))
}

pub(super) async fn load_wasm_extension_bytes(
    module: &str,
    wasm_bytes: &[u8],
    js_glue_text: Option<&str>,
) -> ModuleInstallResult<()> {
    let js_glue_url = match js_glue_text {
        Some(js_glue_text) => js_glue_data_url(js_glue_text)?,
        None => String::new(),
    };
    ext_bridge::load_wasm_ext_module(module, wasm_bytes, &js_glue_url)
        .await
        .map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::Extension,
                ModuleInstallErrorKind::LoadFailed,
                error,
            )
            .with_module(module)
        })
}

// ── VFS extension manifest reading ──────────────────────────────────────────

pub(super) fn read_wasm_extension_from_vfs(
    module: &str,
    version: &str,
) -> ModuleInstallResult<Option<WasmExtensionManifest>> {
    let path = vfs_module_file_path(module, version, "vo.ext.toml");
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(&path);
    if err.is_some() {
        return Ok(None);
    }
    match String::from_utf8(data) {
        Ok(content) => {
            vo_module::ext_manifest::wasm_extension_from_content(&content).map_err(|error| {
                ModuleInstallError::new(
                    ModuleInstallStage::Extension,
                    ModuleInstallErrorKind::ParseFailed,
                    format!(
                        "failed to parse vo.ext.toml for {}@{}: {}",
                        module, version, error
                    ),
                )
                .with_module_version(module, version)
            })
        }
        Err(error) => Err(ModuleInstallError::new(
            ModuleInstallStage::Extension,
            ModuleInstallErrorKind::ParseFailed,
            format!(
                "cached vo.ext.toml for {}@{} is not valid UTF-8: {}",
                module, version, error
            ),
        )
        .with_module_version(module, version)),
    }
}

// ── VFS js_glue reading ──────────────────────────────────────────────────────

fn try_read_cached_vfs_bytes(path: &str) -> Option<Vec<u8>> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    if err.is_some() {
        return None;
    }
    Some(data)
}

fn read_bindgen_js_glue_from_vfs(
    module: &str,
    version: &str,
    ext: &WasmExtensionManifest,
) -> ModuleInstallResult<Option<String>> {
    match ext.kind {
        WasmExtensionKind::Bindgen => {
            let name = ext.js_glue.as_deref().unwrap_or_default();
            let path = vfs_artifact_path(module, version, name);
            let Some(bytes) = try_read_cached_vfs_bytes(&path) else {
                return Ok(None);
            };
            String::from_utf8(bytes).map(Some).map_err(|error| {
                ModuleInstallError::new(
                    ModuleInstallStage::Extension,
                    ModuleInstallErrorKind::ParseFailed,
                    format!(
                        "cached wasm JS glue for {}@{} is not valid UTF-8: {}",
                        module, version, error
                    ),
                )
                .with_module_version(module, version)
            })
        }
        WasmExtensionKind::Standalone => Ok(None),
    }
}

// ── Cached extension loading ────────────────────────────────────────────────

pub(super) async fn load_cached_ext_from_vfs(
    module: &str,
    version: &str,
) -> ModuleInstallResult<bool> {
    let Some(ext) = read_wasm_extension_from_vfs(module, version)? else {
        return Ok(false);
    };
    let Some(wasm_bytes) =
        try_read_cached_vfs_bytes(&vfs_artifact_path(module, version, &ext.wasm))
    else {
        return Ok(false);
    };
    let js_glue_text = read_bindgen_js_glue_from_vfs(module, version, &ext)?;
    if matches!(ext.kind, WasmExtensionKind::Bindgen) && js_glue_text.is_none() {
        return Ok(false);
    }
    load_wasm_extension_bytes(module, &wasm_bytes, js_glue_text.as_deref()).await?;
    log_extension_cached(module, version);
    Ok(true)
}

// ── Locked extension loading (with digest verification) ─────────────────────

fn verify_locked_artifact_bytes(
    locked: &LockedModule,
    asset_name: &str,
    bytes: &[u8],
) -> ModuleInstallResult<()> {
    let artifact = find_locked_wasm_artifact(locked, asset_name).ok_or_else(|| {
        ModuleInstallError::new(
            ModuleInstallStage::Extension,
            ModuleInstallErrorKind::Missing,
            format!(
                "vo.lock is missing wasm artifact {} for {}@{}",
                asset_name, locked.path, locked.version
            ),
        )
        .with_module_version(locked.path.as_str(), locked.version.to_string())
    })?;
    verify_size_and_digest(
        bytes,
        artifact.size,
        &artifact.digest,
        format!(
            "locked wasm artifact {} for {}@{} in the VFS",
            asset_name, locked.path, locked.version,
        ),
    )
    .map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Extension,
            ModuleInstallErrorKind::ValidationFailed,
            error.to_string(),
        )
        .with_module_version(locked.path.as_str(), locked.version.to_string())
    })
}

pub(super) async fn load_locked_ext_from_vfs(locked: &LockedModule) -> ModuleInstallResult<()> {
    let module = locked.path.as_str();
    let version = locked.version.to_string();
    let Some(ext) = read_wasm_extension_from_vfs(module, &version)? else {
        return Ok(());
    };
    let wasm_bytes = read_vfs_bytes(&vfs_artifact_path(module, &version, &ext.wasm))?;
    verify_locked_artifact_bytes(locked, &ext.wasm, &wasm_bytes)?;
    let js_glue_text = match ext.kind {
        WasmExtensionKind::Bindgen => {
            let name = ext.js_glue.as_deref().unwrap_or_default();
            let path = vfs_artifact_path(module, &version, name);
            let Some(bytes) = try_read_cached_vfs_bytes(&path) else {
                return Err(ModuleInstallError::new(
                    ModuleInstallStage::Extension,
                    ModuleInstallErrorKind::Missing,
                    format!("missing wasm JS glue for {}@{} in the VFS", module, version),
                )
                .with_module_version(module, &version));
            };
            String::from_utf8(bytes).map(Some).map_err(|error| {
                ModuleInstallError::new(
                    ModuleInstallStage::Extension,
                    ModuleInstallErrorKind::ParseFailed,
                    format!(
                        "cached wasm JS glue for {}@{} is not valid UTF-8: {}",
                        module, version, error
                    ),
                )
                .with_module_version(module, &version)
            })?
        }
        WasmExtensionKind::Standalone => None,
    };
    load_wasm_extension_bytes(module, &wasm_bytes, js_glue_text.as_deref()).await
}

// ── Extension asset fetching ────────────────────────────────────────────────

pub(super) struct FetchedExtensionAssets {
    pub wasm_bytes: Vec<u8>,
    pub js_glue_text: Option<String>,
}

pub(super) async fn fetch_and_cache_extension_assets<F, G, FutWasm, FutJs>(
    module: &str,
    version: &str,
    ext: &WasmExtensionManifest,
    cached: bool,
    mut fetch_wasm: F,
    mut fetch_js_glue: G,
) -> ModuleInstallResult<FetchedExtensionAssets>
where
    F: FnMut() -> FutWasm,
    G: FnMut(String) -> FutJs,
    FutWasm: std::future::Future<Output = ModuleInstallResult<Vec<u8>>>,
    FutJs: std::future::Future<Output = ModuleInstallResult<String>>,
{
    let wasm_start = crate::now_ms();
    log_extension_asset_load_start(module, version, "wasm", &ext.wasm, cached);
    let wasm_bytes = fetch_wasm().await?;
    log_extension_asset_load_done(module, version, "wasm", &ext.wasm, cached, wasm_start);

    let js_glue_text = match ext.kind {
        WasmExtensionKind::Bindgen => {
            let name = ext.js_glue.as_deref().unwrap_or_default().to_string();
            let start = crate::now_ms();
            log_extension_asset_load_start(module, version, "js_glue", &name, cached);
            let text = fetch_js_glue(name.clone()).await?;
            log_extension_asset_load_done(module, version, "js_glue", &name, cached, start);
            Some(text)
        }
        WasmExtensionKind::Standalone => None,
    };

    // Cache to VFS
    write_vfs_bytes(&vfs_artifact_path(module, version, &ext.wasm), &wasm_bytes)?;
    if let Some(ref text) = js_glue_text {
        let name = ext.js_glue.as_deref().unwrap_or_default();
        write_vfs_text(&vfs_artifact_path(module, version, name), text)?;
    }

    Ok(FetchedExtensionAssets {
        wasm_bytes,
        js_glue_text,
    })
}

// ── Public extension loading entry point ────────────────────────────────────

/// Load the pre-built WASM extension binary for a module if one exists on GitHub.
/// A missing binary (HTTP 404) is silently skipped — it just means the module
/// is pure Vo with no native extension.  Other errors are logged as warnings.
///
/// Determines whether the module uses wasm-bindgen by reading `vo.ext.toml`
/// from the VFS (already installed by fetch or OPFS persistence).
pub async fn load_ext_if_present(module: &str, version: &str) {
    let wasm_extension = match read_wasm_extension_from_vfs(module, version) {
        Ok(Some(wasm_extension)) => wasm_extension,
        Ok(None) => return,
        Err(error) => {
            log_extension_load_error(module, version, stringify_module_install_error(error));
            return;
        }
    };
    let manifest = match read_release_manifest_from_vfs(module, version) {
        Ok(manifest) => manifest,
        Err(error) => {
            log_extension_load_error(module, version, stringify_module_install_error(error));
            return;
        }
    };

    match load_cached_ext_from_vfs(module, version).await {
        Ok(true) => return,
        Ok(false) => {}
        Err(error) => {
            log_extension_load_error(module, version, stringify_module_install_error(error));
            return;
        }
    }

    let assets = match fetch_and_cache_extension_assets(
        module,
        version,
        &wasm_extension,
        false,
        || {
            wasm_fetch::fetch_wasm_binary_from_manifest(
                module,
                version,
                &manifest,
                &wasm_extension.wasm,
            )
        },
        |js_glue_name| {
            let manifest = manifest.clone();
            async move {
                wasm_fetch::fetch_wasm_js_glue_text_from_manifest(
                    module,
                    version,
                    &manifest,
                    &js_glue_name,
                )
                .await
            }
        },
    )
    .await
    {
        Ok(result) => result,
        Err(error) => {
            log_extension_load_error(module, version, stringify_module_install_error(error));
            return;
        }
    };

    if let Err(error) =
        load_wasm_extension_bytes(module, &assets.wasm_bytes, assets.js_glue_text.as_deref()).await
    {
        log_extension_load_error(module, version, stringify_module_install_error(error));
    }
}
