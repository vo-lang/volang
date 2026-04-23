use std::cell::RefCell;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use serde::de::DeserializeOwned;
use serde::Deserialize;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;

use vo_module::async_install::{AsyncRegistry, BoxFuture, SourcePayload};
use vo_module::digest::Digest;
use vo_module::identity::{ArtifactId, ModulePath};
use vo_module::schema::manifest::{
    ManifestArtifact, ManifestRequire, ManifestSource, ReleaseManifest,
};
use vo_module::version::ExactVersion;
use vo_module::{Error, Result};

const WASM_TARGET: &str = "wasm32-unknown-unknown";
const VO_WEB_FILE: &str = "vo.web.json";

thread_local! {
    static WEB_MANIFEST_CACHE: RefCell<BTreeMap<String, WebManifest>> = RefCell::new(BTreeMap::new());
}

#[derive(Debug, Clone, Copy, Default)]
pub struct BrowserRegistry;

#[derive(Debug, Clone, Deserialize)]
struct GitHubRelease {
    tag_name: String,
}

#[derive(Debug, Clone, Deserialize)]
struct WebManifest {
    schema_version: u64,
    module: String,
    version: String,
    commit: String,
    module_root: String,
    vo: String,
    #[serde(default)]
    require: Vec<WebManifestRequire>,
    source_digest: String,
    #[serde(default)]
    source: Vec<WebManifestSource>,
    #[serde(default)]
    artifacts: Vec<WebManifestArtifact>,
}

#[derive(Debug, Clone, Deserialize)]
struct WebManifestRequire {
    module: String,
    constraint: String,
}

#[derive(Debug, Clone, Deserialize)]
struct WebManifestSource {
    path: String,
    size: u64,
    digest: String,
}

#[derive(Debug, Clone, Deserialize)]
struct WebManifestArtifact {
    kind: String,
    target: String,
    name: String,
    path: String,
    size: u64,
    digest: String,
}

pub async fn fetch_bytes(url: &str) -> std::result::Result<Vec<u8>, String> {
    fetch_bytes_typed(url)
        .await
        .map_err(|error| error.to_string())
}

impl AsyncRegistry for BrowserRegistry {
    fn list_versions<'a>(
        &'a self,
        module: &'a ModulePath,
    ) -> BoxFuture<'a, Result<Vec<ExactVersion>>> {
        Box::pin(async move { fetch_module_release_versions(module).await })
    }

    fn fetch_manifest_raw<'a>(
        &'a self,
        module: &'a ModulePath,
        version: &'a ExactVersion,
    ) -> BoxFuture<'a, Result<(ReleaseManifest, Vec<u8>)>> {
        Box::pin(async move { fetch_release_manifest(module, version).await })
    }

    fn fetch_source<'a>(
        &'a self,
        module: &'a ModulePath,
        version: &'a ExactVersion,
        asset_name: &'a str,
    ) -> BoxFuture<'a, Result<SourcePayload>> {
        Box::pin(async move { fetch_source_files(module, version, asset_name).await })
    }

    fn fetch_artifact<'a>(
        &'a self,
        module: &'a ModulePath,
        version: &'a ExactVersion,
        artifact: &'a ArtifactId,
    ) -> BoxFuture<'a, Result<Vec<u8>>> {
        Box::pin(async move { fetch_web_artifact(module, version, artifact).await })
    }
}

async fn fetch_bytes_typed(url: &str) -> Result<Vec<u8>> {
    let response = fetch_response(url).await?;
    if !response.ok() {
        return Err(http_error(url, &response));
    }
    response_bytes(url, &response).await
}

async fn fetch_response(url: &str) -> Result<web_sys::Response> {
    let window = web_sys::window().ok_or_else(|| Error::Network("no window object".to_string()))?;

    let opts = web_sys::RequestInit::new();
    opts.set_method("GET");
    opts.set_cache(web_sys::RequestCache::NoStore);
    let request = web_sys::Request::new_with_str_and_init(url, &opts).map_err(|error| {
        Error::Network(
            error
                .as_string()
                .unwrap_or_else(|| format!("failed to build request for {}", url)),
        )
    })?;

    let resp_value = JsFuture::from(window.fetch_with_request(&request))
        .await
        .map_err(|error| {
            Error::Network(
                error
                    .as_string()
                    .unwrap_or_else(|| format!("fetch failed for {}", url)),
            )
        })?;

    resp_value
        .dyn_into()
        .map_err(|_| Error::Network(format!("response cast error for {}", url)))
}

async fn response_bytes(url: &str, response: &web_sys::Response) -> Result<Vec<u8>> {
    let array_buffer = response.array_buffer().map_err(|error| {
        Error::Network(
            error
                .as_string()
                .unwrap_or_else(|| format!("array_buffer error for {}", url)),
        )
    })?;
    let array_buffer = JsFuture::from(array_buffer).await.map_err(|error| {
        Error::Network(
            error
                .as_string()
                .unwrap_or_else(|| format!("array_buffer await error for {}", url)),
        )
    })?;

    Ok(js_sys::Uint8Array::new(&array_buffer).to_vec())
}

fn http_error(url: &str, response: &web_sys::Response) -> Error {
    Error::Network(format!("HTTP {} for {}", response.status(), url))
}

async fn fetch_json<T: DeserializeOwned>(url: &str) -> Result<T> {
    let bytes = fetch_bytes_typed(url).await?;
    serde_json::from_slice(&bytes)
        .map_err(|error| Error::RegistryError(format!("invalid JSON from {}: {}", url, error)))
}

async fn fetch_text(url: &str) -> Result<String> {
    let bytes = fetch_bytes_typed(url).await?;
    String::from_utf8(bytes)
        .map_err(|error| Error::RegistryError(format!("invalid UTF-8 from {}: {}", url, error)))
}

async fn fetch_release_manifest(
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<(ReleaseManifest, Vec<u8>)> {
    let web = fetch_web_manifest(module, version).await?;
    validate_web_manifest_identity(module, version, &web)?;
    let source_digest = Digest::parse(&web.source_digest).map_err(|error| {
        Error::InvalidReleaseMetadata(format!(
            "vo.web.json source_digest for {} {} is invalid: {}",
            module, version, error
        ))
    })?;
    let manifest = ReleaseManifest {
        schema_version: 1,
        module: module.clone(),
        version: version.clone(),
        commit: web.commit.clone(),
        module_root: web.module_root.clone(),
        vo: vo_module::version::ToolchainConstraint::parse(&web.vo)
            .map_err(|error| Error::InvalidReleaseMetadata(format!("vo.web.json vo: {}", error)))?,
        require: web
            .require
            .iter()
            .map(|req| {
                Ok(ManifestRequire {
                    module: ModulePath::parse(&req.module).map_err(|error| {
                        Error::InvalidReleaseMetadata(format!(
                            "vo.web.json require module '{}': {}",
                            req.module, error
                        ))
                    })?,
                    constraint: vo_module::version::DepConstraint::parse(&req.constraint).map_err(
                        |error| {
                            Error::InvalidReleaseMetadata(format!(
                                "vo.web.json require constraint '{}': {}",
                                req.constraint, error
                            ))
                        },
                    )?,
                })
            })
            .collect::<Result<Vec<_>>>()?,
        source: ManifestSource {
            name: VO_WEB_FILE.to_string(),
            size: web.source.iter().map(|entry| entry.size).sum(),
            digest: source_digest,
        },
        artifacts: web
            .artifacts
            .iter()
            .map(|artifact| {
                Ok(ManifestArtifact {
                    id: ArtifactId {
                        kind: artifact.kind.clone(),
                        target: artifact.target.clone(),
                        name: artifact.name.clone(),
                    },
                    size: artifact.size,
                    digest: Digest::parse(&artifact.digest).map_err(|error| {
                        Error::InvalidReleaseMetadata(format!(
                            "vo.web.json artifact digest for {}: {}",
                            artifact.name, error
                        ))
                    })?,
                })
            })
            .collect::<Result<Vec<_>>>()?,
    };
    let manifest_raw = format!("{}\n", manifest.render()).into_bytes();
    let manifest = parse_manifest_raw(module, version, &manifest_raw)?;
    Ok((manifest, manifest_raw))
}

fn parse_manifest_raw(
    module: &ModulePath,
    version: &ExactVersion,
    manifest_raw: &[u8],
) -> Result<ReleaseManifest> {
    let manifest_content = std::str::from_utf8(manifest_raw)
        .map_err(|error| Error::ManifestParse(format!("vo.release.json utf-8 error: {error}")))?;
    vo_module::registry::parse_requested_release_manifest(manifest_content, module, version)
}

async fn fetch_source_files(
    module: &ModulePath,
    version: &ExactVersion,
    asset_name: &str,
) -> Result<SourcePayload> {
    if asset_name != VO_WEB_FILE {
        return Err(Error::InvalidReleaseMetadata(format!(
            "browser source payload for {} {} must be keyed by {}",
            module, version, VO_WEB_FILE,
        )));
    }
    let web = fetch_web_manifest(module, version).await?;
    validate_web_manifest_identity(module, version, &web)?;
    if !web.source.iter().any(|entry| entry.path == "vo.mod") {
        return Err(Error::SourceScan(format!(
            "vo.web.json for {} {} is missing vo.mod",
            module, version,
        )));
    }

    let mut files = Vec::new();
    for entry in &web.source {
        validate_web_relative_path(&entry.path)?;
        let content = fetch_module_file_text(module, &web.commit, &entry.path).await?;
        let digest = Digest::from_sha256(content.as_bytes());
        let expected = Digest::parse(&entry.digest).map_err(|error| {
            Error::InvalidReleaseMetadata(format!(
                "vo.web.json source digest for {}: {}",
                entry.path, error
            ))
        })?;
        if content.len() as u64 != entry.size || digest != expected {
            return Err(Error::DigestMismatch {
                context: format!("source file {} for {} {}", entry.path, module, version),
                expected: format!("{} ({} bytes)", expected, entry.size),
                found: format!("{} ({} bytes)", digest, content.len()),
            });
        }
        files.push((PathBuf::from(&entry.path), content));
    }

    Ok(SourcePayload::Files(files))
}

async fn fetch_web_artifact(
    module: &ModulePath,
    version: &ExactVersion,
    artifact: &ArtifactId,
) -> Result<Vec<u8>> {
    let web = fetch_web_manifest(module, version).await?;
    validate_web_manifest_identity(module, version, &web)?;
    if artifact.target != WASM_TARGET {
        return Err(Error::MissingArtifact {
            module: module.as_str().to_string(),
            version: version.to_string(),
            detail: format!(
                "browser registry cannot fetch target-specific artifact {} for {}",
                artifact.name, artifact.target,
            ),
        });
    }
    let web_artifact = web
        .artifacts
        .iter()
        .find(|entry| {
            entry.kind == artifact.kind
                && entry.target == artifact.target
                && entry.name == artifact.name
        })
        .ok_or_else(|| Error::MissingArtifact {
            module: module.as_str().to_string(),
            version: version.to_string(),
            detail: format!("vo.web.json does not declare {}", artifact.name),
        })?;
    validate_web_relative_path(&web_artifact.path)?;
    let bytes = fetch_module_file_bytes(module, &web.commit, &web_artifact.path).await?;
    let expected = Digest::parse(&web_artifact.digest).map_err(|error| {
        Error::InvalidReleaseMetadata(format!(
            "vo.web.json artifact digest for {}: {}",
            web_artifact.name, error
        ))
    })?;
    let found = Digest::from_sha256(&bytes);
    if bytes.len() as u64 != web_artifact.size || found != expected {
        return Err(Error::DigestMismatch {
            context: format!("artifact {} for {} {}", artifact.name, module, version),
            expected: format!("{} ({} bytes)", expected, web_artifact.size),
            found: format!("{} ({} bytes)", found, bytes.len()),
        });
    }
    Ok(bytes)
}

async fn fetch_web_manifest(module: &ModulePath, version: &ExactVersion) -> Result<WebManifest> {
    let cache_key = module_version_cache_key(module, version);
    if let Some(manifest) = WEB_MANIFEST_CACHE.with(|cache| cache.borrow().get(&cache_key).cloned())
    {
        return Ok(manifest);
    }
    let tag = vo_module::registry::release_tag(module, version);
    let url = raw_module_file_url(module, &tag, VO_WEB_FILE);
    let bytes = fetch_bytes_typed(&url).await?;
    let manifest: WebManifest = serde_json::from_slice(&bytes).map_err(|error| {
        Error::InvalidReleaseMetadata(format!("invalid vo.web.json from {}: {}", url, error))
    })?;
    WEB_MANIFEST_CACHE.with(|cache| {
        cache.borrow_mut().insert(cache_key, manifest.clone());
    });
    Ok(manifest)
}

fn validate_web_manifest_identity(
    module: &ModulePath,
    version: &ExactVersion,
    manifest: &WebManifest,
) -> Result<()> {
    if manifest.schema_version != 1 {
        return Err(Error::InvalidReleaseMetadata(format!(
            "vo.web.json for {} {} has unsupported schema_version {}",
            module, version, manifest.schema_version
        )));
    }
    if manifest.module != module.as_str() {
        return Err(Error::InvalidReleaseMetadata(format!(
            "vo.web.json module '{}' does not match requested '{}'",
            manifest.module, module
        )));
    }
    if manifest.version != version.to_string() {
        return Err(Error::InvalidReleaseMetadata(format!(
            "vo.web.json version '{}' does not match requested '{}'",
            manifest.version, version
        )));
    }
    if manifest.module_root != module.module_root() {
        return Err(Error::InvalidReleaseMetadata(format!(
            "vo.web.json module_root '{}' does not match requested '{}'",
            manifest.module_root,
            module.module_root()
        )));
    }
    validate_commit_hash(&manifest.commit)
        .map_err(|error| Error::InvalidReleaseMetadata(format!("vo.web.json commit: {}", error)))
}

fn validate_commit_hash(commit: &str) -> std::result::Result<(), String> {
    if commit.len() != 40 {
        return Err(format!(
            "commit must be 40-char hex, got {} chars",
            commit.len()
        ));
    }
    if !commit
        .chars()
        .all(|c| c.is_ascii_hexdigit() && !c.is_ascii_uppercase())
    {
        return Err("commit must be lowercase hex".to_string());
    }
    Ok(())
}

fn validate_web_relative_path(path: &str) -> Result<()> {
    let path = Path::new(path);
    if path.as_os_str().is_empty() || path.is_absolute() {
        return Err(Error::InvalidReleaseMetadata(format!(
            "vo.web.json path must be module-relative: {}",
            path.display()
        )));
    }
    for component in path.components() {
        match component {
            std::path::Component::Normal(_) => {}
            _ => {
                return Err(Error::InvalidReleaseMetadata(format!(
                    "vo.web.json path must be normalized and stay inside the module: {}",
                    path.display()
                )))
            }
        }
    }
    Ok(())
}

fn module_version_cache_key(module: &ModulePath, version: &ExactVersion) -> String {
    format!("{}@{}", module.as_str(), version)
}

async fn fetch_module_file_text(
    module: &ModulePath,
    commit: &str,
    rel_path: &str,
) -> Result<String> {
    fetch_text(&raw_module_file_url(module, commit, rel_path)).await
}

async fn fetch_module_file_bytes(
    module: &ModulePath,
    commit: &str,
    rel_path: &str,
) -> Result<Vec<u8>> {
    fetch_bytes_typed(&raw_module_file_url(module, commit, rel_path)).await
}

fn raw_module_file_url(module: &ModulePath, commit: &str, rel_path: &str) -> String {
    let repo = vo_module::registry::repository_id(module);
    let repo_path = repo_relative_path(module, rel_path);
    format!(
        "https://raw.githubusercontent.com/{}/{}/{}/{}",
        encode_component(&repo.owner),
        encode_component(&repo.repo),
        encode_component(commit),
        encode_path(&repo_path),
    )
}

fn repo_relative_path(module: &ModulePath, rel_path: &str) -> String {
    let root = module.module_root();
    if root == "." {
        rel_path.to_string()
    } else {
        format!("{}/{}", root.trim_end_matches('/'), rel_path)
    }
}

async fn fetch_module_release_versions(module: &ModulePath) -> Result<Vec<ExactVersion>> {
    let repo = vo_module::registry::repository_id(module);
    let api_url = format!(
        "https://api.github.com/repos/{}/{}/releases?per_page=100",
        encode_component(&repo.owner),
        encode_component(&repo.repo),
    );
    let releases: Vec<GitHubRelease> = fetch_json(&api_url).await?;
    let mut versions = releases
        .iter()
        .filter_map(|release| vo_module::registry::version_from_tag(module, &release.tag_name))
        .collect::<Vec<_>>();
    versions.sort_by(|a, b| b.cmp(a));
    versions.dedup();
    Ok(versions)
}

fn encode_component(value: &str) -> String {
    let mut encoded = String::new();
    for byte in value.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                encoded.push(byte as char)
            }
            _ => {
                encoded.push('%');
                encoded.push(hex_digit(byte >> 4));
                encoded.push(hex_digit(byte & 0x0f));
            }
        }
    }
    encoded
}

fn encode_path(path: &str) -> String {
    path.split('/')
        .map(encode_component)
        .collect::<Vec<_>>()
        .join("/")
}

fn hex_digit(value: u8) -> char {
    match value {
        0..=9 => (b'0' + value) as char,
        10..=15 => (b'A' + (value - 10)) as char,
        _ => unreachable!(),
    }
}
