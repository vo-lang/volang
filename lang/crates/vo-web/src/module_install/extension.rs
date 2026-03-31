//! WASM extension loading, caching, and validation for installed modules.

use vo_module::digest::verify_size_and_digest;
use vo_module::ext_manifest::{WasmExtensionKind, WasmExtensionManifest};
use vo_module::schema::lockfile::LockedModule;

use vo_web_runtime_wasm::ext_bridge;

use super::fetch as wasm_fetch;
use super::log::*;
use super::vfs_io::*;

// ── Core loading ────────────────────────────────────────────────────────────

fn js_glue_data_url(js_text: &str) -> Result<String, String> {
    let encoded = wasm_bindgen::JsValue::from(js_sys::encode_uri_component(js_text))
        .as_string()
        .ok_or_else(|| "failed to encode wasm JS glue URL".to_string())?;
    Ok(format!("data:text/javascript;charset=utf-8,{}", encoded))
}

pub(super) async fn load_wasm_extension_bytes(
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

// ── VFS extension manifest reading ──────────────────────────────────────────

pub(super) fn read_wasm_extension_from_vfs(
    module: &str,
    version: &str,
) -> Option<WasmExtensionManifest> {
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

// ── VFS js_glue reading ──────────────────────────────────────────────────────

fn read_bindgen_js_glue_from_vfs(
    module: &str,
    version: &str,
    ext: &WasmExtensionManifest,
) -> Result<Option<String>, Option<String>> {
    match ext.kind {
        WasmExtensionKind::Bindgen => {
            let name = ext.js_glue.as_deref().unwrap_or_default();
            let bytes = read_vfs_bytes(&vfs_artifact_path(module, version, name))
                .map_err(|_| None)?;
            String::from_utf8(bytes)
                .map(Some)
                .map_err(|e| Some(format!("cached wasm JS glue for {}@{} is not valid UTF-8: {}", module, version, e)))
        }
        WasmExtensionKind::Standalone => Ok(None),
    }
}

// ── Cached extension loading ────────────────────────────────────────────────

pub(super) async fn load_cached_ext_from_vfs(
    module: &str,
    version: &str,
) -> Result<bool, String> {
    let Some(ext) = read_wasm_extension_from_vfs(module, version) else {
        return Ok(false);
    };
    let wasm_bytes = match read_vfs_bytes(&vfs_artifact_path(module, version, &ext.wasm)) {
        Ok(bytes) => bytes,
        Err(_) => return Ok(false),
    };
    let js_glue_text = match read_bindgen_js_glue_from_vfs(module, version, &ext) {
        Ok(text) => text,
        Err(None) => return Ok(false),
        Err(Some(e)) => return Err(e),
    };
    load_wasm_extension_bytes(module, &wasm_bytes, js_glue_text.as_deref()).await?;
    log_extension_cached(module, version);
    Ok(true)
}

// ── Locked extension loading (with digest verification) ─────────────────────

fn verify_locked_artifact_bytes(
    locked: &LockedModule,
    asset_name: &str,
    bytes: &[u8],
) -> Result<(), String> {
    let artifact = find_locked_wasm_artifact(locked, asset_name).ok_or_else(|| {
        format!("vo.lock is missing wasm artifact {} for {}@{}", asset_name, locked.path, locked.version)
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
    .map_err(|error| error.to_string())
}

pub(super) async fn load_locked_ext_from_vfs(locked: &LockedModule) -> Result<(), String> {
    let module = locked.path.as_str();
    let version = locked.version.to_string();
    let Some(ext) = read_wasm_extension_from_vfs(module, &version) else {
        return Ok(());
    };
    let wasm_bytes = read_vfs_bytes(&vfs_artifact_path(module, &version, &ext.wasm))?;
    verify_locked_artifact_bytes(locked, &ext.wasm, &wasm_bytes)?;
    let js_glue_text = match ext.kind {
        WasmExtensionKind::Bindgen => {
            let name = ext.js_glue.as_deref().unwrap_or_default();
            let bytes = read_vfs_bytes(&vfs_artifact_path(module, &version, name))?;
            verify_locked_artifact_bytes(locked, name, &bytes)?;
            Some(String::from_utf8(bytes).map_err(|e| {
                format!("wasm JS glue for {}@{} is not valid UTF-8: {}", module, version, e)
            })?)
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
) -> Result<FetchedExtensionAssets, String>
where
    F: FnMut() -> FutWasm,
    G: FnMut(String) -> FutJs,
    FutWasm: std::future::Future<Output = Result<Vec<u8>, String>>,
    FutJs: std::future::Future<Output = Result<String, String>>,
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

    Ok(FetchedExtensionAssets { wasm_bytes, js_glue_text })
}

// ── Public extension loading entry point ────────────────────────────────────

/// Load the pre-built WASM extension binary for a module if one exists on GitHub.
/// A missing binary (HTTP 404) is silently skipped — it just means the module
/// is pure Vo with no native extension.  Other errors are logged as warnings.
///
/// Determines whether the module uses wasm-bindgen by reading `vo.ext.toml`
/// from the VFS (already installed by fetch or OPFS persistence).
pub async fn load_ext_if_present(module: &str, version: &str) {
    let Some(wasm_extension) = read_wasm_extension_from_vfs(module, version) else {
        return;
    };
    let manifest = match read_release_manifest_from_vfs(module, version) {
        Ok(manifest) => manifest,
        Err(error) => {
            log_extension_load_error(module, version, error);
            return;
        }
    };

    match load_cached_ext_from_vfs(module, version).await {
        Ok(true) => return,
        Ok(false) => {}
        Err(e) => {
            log_extension_load_error(module, version, e);
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
        Err(e) => {
            log_extension_load_error(module, version, e);
            return;
        }
    };

    if let Err(e) = load_wasm_extension_bytes(
        module,
        &assets.wasm_bytes,
        assets.js_glue_text.as_deref(),
    )
    .await
    {
        log_extension_load_error(module, version, e);
    }
}
