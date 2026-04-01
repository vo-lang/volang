//! WASM-specific fetch helpers (browser Fetch API).
//!
//! These were previously in `vo-module-old/src/fetch.rs` under `#[cfg(target_arch = "wasm32")]`.
//! They are WASM-platform-specific and belong in vo-web, not in the abstract module system.

use std::path::PathBuf;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;

use vo_module::identity::ModulePath;
use vo_module::schema::lockfile::LockedModule;
use vo_module::schema::manifest::ReleaseManifest;
use vo_module::version::ExactVersion;

use super::{ModuleInstallError, ModuleInstallErrorKind, ModuleInstallResult, ModuleInstallStage};

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
pub(crate) async fn fetch_bytes(url: &str) -> ModuleInstallResult<Vec<u8>> {
    let effective_url = cors_proxy_url(url);
    let window = web_sys::window().ok_or_else(|| {
        ModuleInstallError::new(
            ModuleInstallStage::Fetch,
            ModuleInstallErrorKind::NetworkFailed,
            "no window object",
        )
        .with_path(url)
    })?;

    let opts = web_sys::RequestInit::new();
    opts.set_method("GET");
    opts.set_cache(web_sys::RequestCache::NoStore);
    let request = web_sys::Request::new_with_str_and_init(&effective_url, &opts)
        .map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::Fetch,
                ModuleInstallErrorKind::NetworkFailed,
                error.as_string().unwrap_or_else(|| "request error".to_string()),
            )
            .with_path(url)
        })?;

    let resp_value = JsFuture::from(window.fetch_with_request(&request))
        .await
        .map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::Fetch,
                ModuleInstallErrorKind::NetworkFailed,
                error.as_string().unwrap_or_else(|| "fetch error".to_string()),
            )
            .with_path(url)
        })?;

    let resp: web_sys::Response = resp_value
        .dyn_into()
        .map_err(|_| {
            ModuleInstallError::new(
                ModuleInstallStage::Fetch,
                ModuleInstallErrorKind::ParseFailed,
                "response cast error",
            )
            .with_path(url)
        })?;

    if !resp.ok() {
        return Err(
            ModuleInstallError::new(
                ModuleInstallStage::Fetch,
                ModuleInstallErrorKind::NetworkFailed,
                format!("HTTP {} for {}", resp.status(), url),
            )
            .with_path(url),
        );
    }

    let ab_promise = resp.array_buffer().map_err(|e| {
        ModuleInstallError::new(
            ModuleInstallStage::Fetch,
            ModuleInstallErrorKind::ReadFailed,
            e.as_string()
                .unwrap_or_else(|| "array_buffer error".to_string()),
        )
        .with_path(url)
    })?;
    let ab = JsFuture::from(ab_promise).await.map_err(|e| {
        ModuleInstallError::new(
            ModuleInstallStage::Fetch,
            ModuleInstallErrorKind::ReadFailed,
            e.as_string()
                .unwrap_or_else(|| "array_buffer await error".to_string()),
        )
        .with_path(url)
    })?;

    let array = js_sys::Uint8Array::new(&ab);
    Ok(array.to_vec())
}

// ── URL building ─────────────────────────────────────────────────────────

fn release_manifest_url(module: &str, version: &str) -> ModuleInstallResult<String> {
    let module = ModulePath::parse(module).map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Manifest,
            ModuleInstallErrorKind::ParseFailed,
            error.to_string(),
        )
        .with_module_version(module, version)
    })?;
    let version = ExactVersion::parse(version).map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Manifest,
            ModuleInstallErrorKind::ParseFailed,
            error.to_string(),
        )
        .with_module_version(module.as_str(), version)
    })?;
    Ok(vo_module::registry::release_download_url(
        &module,
        &version,
        "vo.release.json",
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

async fn fetch_release_manifest_checked(
    module: &str,
    version: &str,
    locked: Option<&LockedModule>,
) -> ModuleInstallResult<(ReleaseManifest, String)> {
    let manifest_url = release_manifest_url(module, version)?;
    let manifest_bytes = fetch_bytes(&manifest_url).await.map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Fetch,
            ModuleInstallErrorKind::NetworkFailed,
            format!(
                "failed to fetch release manifest for {}@{}: {}",
                module, version, error
            ),
        )
        .with_module_version(module, version)
    })?;
    let manifest_digest = vo_module::digest::Digest::from_sha256(&manifest_bytes);
    let manifest_content = String::from_utf8(manifest_bytes).map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Manifest,
            ModuleInstallErrorKind::ParseFailed,
            format!(
                "release manifest for {}@{} is not valid UTF-8: {}",
                module, version, error
            ),
        )
        .with_module_version(module, version)
    })?;
    let manifest = vo_module::registry::parse_requested_release_manifest_for_spec(
        module,
        version,
        &manifest_content,
    )
    .map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Manifest,
            ModuleInstallErrorKind::ParseFailed,
            error.to_string(),
        )
        .with_module_version(module, version)
    })?;
    if let Some(locked) = locked {
        vo_module::lock::validate_locked_module_against_manifest(locked, &manifest, &manifest_digest)
            .map_err(|error| {
                ModuleInstallError::new(
                    ModuleInstallStage::Manifest,
                    ModuleInstallErrorKind::ValidationFailed,
                    error.to_string(),
                )
                .with_module_version(module, version)
            })?;
    }
    Ok((manifest, manifest_content))
}

// ── Source package extraction ─────────────────────────────────────────────

fn extract_release_source_package_files(
    source_package: &[u8],
    manifest: &ReleaseManifest,
) -> ModuleInstallResult<Vec<(PathBuf, String)>> {
    let entries = vo_module::cache::install::extract_source_entries(source_package)
        .map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::Fetch,
                ModuleInstallErrorKind::ParseFailed,
                format!("{} for {}@{}", error, manifest.module, manifest.version),
            )
            .with_module_version(manifest.module.as_str(), manifest.version.to_string())
        })?;
    if entries.is_empty() {
        return Err(
            ModuleInstallError::new(
                ModuleInstallStage::Fetch,
                ModuleInstallErrorKind::Missing,
                format!(
                    "no Vo files found in source package for {}@{}",
                    manifest.module, manifest.version,
                ),
            )
            .with_module_version(manifest.module.as_str(), manifest.version.to_string()),
        );
    }
    let module = manifest.module.as_str();
    Ok(entries
        .into_iter()
        .map(|(relative_path, content)| {
            (
                PathBuf::from(format!("{}/{}", module, relative_path.display())),
                content,
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

async fn fetch_source_files_inner(
    module: &str,
    version: &str,
    locked: Option<&LockedModule>,
) -> ModuleInstallResult<Vec<(PathBuf, String)>> {
    let (manifest, manifest_content) =
        fetch_release_manifest_checked(module, version, locked).await?;
    let src_url = source_download_url(&manifest);
    let source_package = fetch_bytes(&src_url).await.map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Fetch,
            ModuleInstallErrorKind::NetworkFailed,
            format!(
                "failed to fetch source package for {}@{}: {}",
                module, version, error
            ),
        )
        .with_module_version(module, version)
    })?;
    vo_module::digest::verify_size_and_digest(
        &source_package,
        manifest.source.size,
        &manifest.source.digest,
        format!("source package for {}@{}", module, version),
    )
    .map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Fetch,
            ModuleInstallErrorKind::ValidationFailed,
            error.to_string(),
        )
        .with_module_version(module, version)
    })?;
    let mut files = extract_release_source_package_files(&source_package, &manifest)?;
    append_release_manifest_file(&mut files, module, &manifest_content);
    Ok(files)
}

pub(crate) async fn fetch_module_files(
    module: &str,
    version: &str,
) -> ModuleInstallResult<Vec<(PathBuf, String)>> {
    if let Some(files) = try_local_module_override(module, version) {
        return Ok(files);
    }
    fetch_source_files_inner(module, version, None).await
}

pub(crate) async fn fetch_locked_module_files(
    locked: &LockedModule,
) -> ModuleInstallResult<Vec<(PathBuf, String)>> {
    fetch_source_files_inner(locked.path.as_str(), &locked.version.to_string(), Some(locked)).await
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

async fn fetch_artifact_bytes(
    module: &str,
    version: &str,
    manifest: &ReleaseManifest,
    target: &str,
    asset_name: &str,
    label: &str,
) -> ModuleInstallResult<Vec<u8>> {
    let artifact = find_manifest_artifact(manifest, target, asset_name).ok_or_else(|| {
        ModuleInstallError::new(
            ModuleInstallStage::Manifest,
            ModuleInstallErrorKind::Missing,
            format!(
                "release manifest for {}@{} is missing {} artifact {}",
                module, version, label, asset_name,
            ),
        )
        .with_module_version(module, version)
    })?;
    let bytes = fetch_bytes(&manifest_asset_download_url(manifest, &artifact.id.name))
        .await
        .map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::Fetch,
                ModuleInstallErrorKind::NetworkFailed,
                format!(
                    "failed to fetch {} for {}@{}: {}",
                    label, module, version, error
                ),
            )
            .with_module_version(module, version)
        })?;
    vo_module::digest::verify_size_and_digest(
        &bytes,
        artifact.size,
        &artifact.digest,
        format!("{} for {}@{}", label, module, version),
    )
    .map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Fetch,
            ModuleInstallErrorKind::ValidationFailed,
            error.to_string(),
        )
        .with_module_version(module, version)
    })?;
    Ok(bytes)
}

async fn fetch_artifact_bytes_with_locked_manifest(
    locked: &LockedModule,
    target: &str,
    asset_name: &str,
    label: &str,
) -> ModuleInstallResult<Vec<u8>> {
    let module = locked.path.as_str();
    let version = locked.version.to_string();
    let (manifest, _) = fetch_release_manifest_checked(module, &version, Some(locked)).await?;
    fetch_artifact_bytes(module, &version, &manifest, target, asset_name, label).await
}

pub(crate) async fn fetch_locked_wasm_binary(
    locked: &LockedModule,
    asset_name: &str,
) -> ModuleInstallResult<Vec<u8>> {
    fetch_artifact_bytes_with_locked_manifest(locked, "wasm32-unknown-unknown", asset_name, "wasm artifact").await
}

pub(crate) async fn fetch_locked_wasm_js_glue_text(
    locked: &LockedModule,
    asset_name: &str,
) -> ModuleInstallResult<String> {
    let bytes = fetch_artifact_bytes_with_locked_manifest(locked, "wasm32-unknown-unknown", asset_name, "wasm JS glue").await?;
    String::from_utf8(bytes).map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Extension,
            ModuleInstallErrorKind::ParseFailed,
            format!("wasm JS glue for {}@{} is not valid UTF-8: {}", locked.path, locked.version, error),
        )
        .with_module_version(locked.path.as_str(), locked.version.to_string())
    })
}

pub(crate) async fn fetch_wasm_js_glue_text_from_manifest(
    module: &str,
    version: &str,
    manifest: &ReleaseManifest,
    asset_name: &str,
) -> ModuleInstallResult<String> {
    let bytes = fetch_artifact_bytes(module, version, manifest, "wasm32-unknown-unknown", asset_name, "wasm JS glue").await?;
    String::from_utf8(bytes).map_err(|error| {
        ModuleInstallError::new(
            ModuleInstallStage::Extension,
            ModuleInstallErrorKind::ParseFailed,
            format!("wasm JS glue for {}@{} is not valid UTF-8: {}", module, version, error),
        )
        .with_module_version(module, version)
    })
}

pub(crate) async fn fetch_wasm_binary_from_manifest(
    module: &str,
    version: &str,
    manifest: &ReleaseManifest,
    asset_name: &str,
) -> ModuleInstallResult<Vec<u8>> {
    if let Some(local_url) = try_local_wasm_url(module, version) {
        return fetch_bytes(&local_url).await.map_err(|error| {
            ModuleInstallError::new(
                ModuleInstallStage::Fetch,
                ModuleInstallErrorKind::NetworkFailed,
                format!("failed to fetch local wasm override for {}@{}: {}", module, version, error),
            )
            .with_module_version(module, version)
        });
    }
    let artifact = manifest
        .artifacts
        .iter()
        .find(|a| a.id.kind == "extension-wasm" && a.id.target == "wasm32-unknown-unknown")
        .ok_or_else(|| {
            ModuleInstallError::new(
                ModuleInstallStage::Manifest,
                ModuleInstallErrorKind::Missing,
                format!(
                    "release manifest for {}@{} is missing extension-wasm artifact for wasm32-unknown-unknown",
                    module, version,
                ),
            )
            .with_module_version(module, version)
        })?;
    if artifact.id.name != asset_name {
        return Err(
            ModuleInstallError::new(
                ModuleInstallStage::Manifest,
                ModuleInstallErrorKind::ValidationFailed,
                format!(
                    "release manifest for {}@{} declares wasm artifact {}, but vo.ext.toml requests {}",
                    module, version, artifact.id.name, asset_name,
                ),
            )
            .with_module_version(module, version),
        );
    }
    fetch_artifact_bytes(module, version, manifest, "wasm32-unknown-unknown", asset_name, "wasm artifact").await
}
