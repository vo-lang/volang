use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use serde::de::DeserializeOwned;
use serde::Deserialize;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;

use vo_module::async_install::{AsyncRegistry, BoxFuture, SourcePayload};
use vo_module::digest::Digest;
use vo_module::ext_manifest::{parse_ext_manifest_content, ExtensionManifest};
use vo_module::identity::{ArtifactId, ModulePath};
use vo_module::schema::manifest::{
    ManifestArtifact, ManifestRequire, ManifestSource, ReleaseManifest,
};
use vo_module::schema::modfile::ModFile;
use vo_module::version::ExactVersion;
use vo_module::{Error, Result};

const WASM_TARGET: &str = "wasm32-unknown-unknown";
const VO_MOD_FILE: &str = "vo.mod";
const VO_LOCK_FILE: &str = "vo.lock";
const VO_EXT_FILE: &str = "vo.ext.toml";

#[derive(Debug, Clone, Copy, Default)]
pub struct BrowserRegistry;

#[derive(Debug, Clone)]
struct ReleaseContext {
    release: GitHubRelease,
    commit: String,
}

#[derive(Debug, Clone, Deserialize)]
struct GitHubRelease {
    tag_name: String,
    #[serde(default)]
    assets: Vec<GitHubReleaseAsset>,
}

#[derive(Debug, Clone, Deserialize)]
struct GitHubReleaseAsset {
    name: String,
    size: u64,
    digest: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
struct GitHubObject {
    sha: String,
    #[serde(rename = "type")]
    kind: String,
    url: String,
}

#[derive(Debug, Clone, Deserialize)]
struct GitHubRef {
    object: GitHubObject,
}

#[derive(Debug, Clone, Deserialize)]
struct GitHubTag {
    object: GitHubObject,
}

#[derive(Debug, Clone, Deserialize)]
struct GitHubTree {
    #[serde(default)]
    tree: Vec<GitHubTreeEntry>,
    #[serde(default)]
    truncated: bool,
}

#[derive(Debug, Clone, Deserialize)]
struct GitHubTreeEntry {
    path: String,
    #[serde(rename = "type")]
    kind: String,
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

async fn fetch_optional_bytes_typed(url: &str) -> Result<Option<Vec<u8>>> {
    let response = fetch_response(url).await?;
    if response.status() == 404 {
        return Ok(None);
    }
    if !response.ok() {
        return Err(http_error(url, &response));
    }
    response_bytes(url, &response).await.map(Some)
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

async fn fetch_optional_text(url: &str) -> Result<Option<String>> {
    let Some(bytes) = fetch_optional_bytes_typed(url).await? else {
        return Ok(None);
    };
    String::from_utf8(bytes)
        .map(Some)
        .map_err(|error| Error::RegistryError(format!("invalid UTF-8 from {}: {}", url, error)))
}

async fn fetch_release_context(
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<ReleaseContext> {
    let release = fetch_github_release_metadata(module, version).await?;
    let expected_tag = vo_module::registry::release_tag(module, version);
    if release.tag_name != expected_tag {
        return Err(Error::InvalidReleaseMetadata(format!(
            "GitHub release tag '{}' does not match expected '{}'",
            release.tag_name, expected_tag,
        )));
    }
    let commit = fetch_release_commit(module, &expected_tag).await?;
    Ok(ReleaseContext { release, commit })
}

async fn fetch_github_release_metadata(
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<GitHubRelease> {
    let repo = vo_module::registry::repository_id(module);
    let tag = vo_module::registry::release_tag(module, version);
    let api_url = format!(
        "https://api.github.com/repos/{}/{}/releases/tags/{}",
        encode_component(&repo.owner),
        encode_component(&repo.repo),
        encode_component(&tag),
    );
    fetch_json(&api_url).await
}

async fn fetch_release_commit(module: &ModulePath, tag: &str) -> Result<String> {
    let repo = vo_module::registry::repository_id(module);
    let tag_ref = format!("tags/{tag}");
    let api_url = format!(
        "https://api.github.com/repos/{}/{}/git/ref/{}",
        encode_component(&repo.owner),
        encode_component(&repo.repo),
        encode_path(&tag_ref),
    );
    let github_ref: GitHubRef = fetch_json(&api_url).await?;
    resolve_github_object_to_commit(github_ref.object).await
}

async fn resolve_github_object_to_commit(mut object: GitHubObject) -> Result<String> {
    for _ in 0..5 {
        match object.kind.as_str() {
            "commit" => return Ok(object.sha),
            "tag" => {
                let tag: GitHubTag = fetch_json(&object.url).await?;
                object = tag.object;
            }
            other => {
                return Err(Error::InvalidReleaseMetadata(format!(
                    "release tag points to unsupported GitHub object type '{}'",
                    other,
                )));
            }
        }
    }
    Err(Error::InvalidReleaseMetadata(
        "release tag annotation chain is too deep".to_string(),
    ))
}

async fn fetch_release_manifest(
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<(ReleaseManifest, Vec<u8>)> {
    let context = fetch_release_context(module, version).await?;
    let mod_content = fetch_module_file_text(module, &context.commit, VO_MOD_FILE).await?;
    let mod_file = parse_mod_file(module, version, &mod_content)?;
    let ext_manifest = fetch_extension_manifest(module, &context.commit).await?;

    let source_name = source_package_name(module, version)?;
    let source_asset = required_release_asset(&context.release, &source_name, module, version)?;
    let source_digest = asset_digest(source_asset, module, version)?;

    let mut artifacts = Vec::new();
    if let Some(ext_manifest) = ext_manifest.as_ref() {
        for declared in ext_manifest.declared_artifact_ids() {
            let asset = required_release_asset(&context.release, &declared.name, module, version)?;
            artifacts.push(ManifestArtifact {
                id: ArtifactId {
                    kind: declared.kind,
                    target: declared.target,
                    name: declared.name,
                },
                size: asset.size,
                digest: asset_digest(asset, module, version)?,
            });
        }
    }

    let manifest = ReleaseManifest {
        schema_version: 1,
        module: module.clone(),
        version: version.clone(),
        commit: context.commit,
        module_root: module.module_root().to_string(),
        vo: mod_file.vo,
        require: mod_file
            .require
            .into_iter()
            .map(|req| ManifestRequire {
                module: req.module,
                constraint: req.constraint,
            })
            .collect(),
        source: ManifestSource {
            name: source_name,
            size: source_asset.size,
            digest: source_digest,
        },
        artifacts,
    };
    let manifest_raw = format!("{}\n", manifest.render()).into_bytes();
    validate_synthesized_manifest(&context.release, module, version, &manifest_raw)?;
    let manifest = parse_manifest_raw(module, version, &manifest_raw)?;
    Ok((manifest, manifest_raw))
}

fn parse_mod_file(module: &ModulePath, version: &ExactVersion, content: &str) -> Result<ModFile> {
    let mod_file = ModFile::parse(content)?;
    let Some(mod_module) = mod_file.module.as_github() else {
        return Err(Error::InvalidReleaseMetadata(format!(
            "vo.mod for {} {} declares non-publishable module '{}'",
            module, version, mod_file.module,
        )));
    };
    if mod_module != module {
        return Err(Error::InvalidReleaseMetadata(format!(
            "vo.mod module '{}' does not match requested '{}'",
            mod_module, module,
        )));
    }
    Ok(mod_file)
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

fn validate_synthesized_manifest(
    release: &GitHubRelease,
    module: &ModulePath,
    version: &ExactVersion,
    manifest_raw: &[u8],
) -> Result<()> {
    let Some(asset) = release_asset(release, "vo.release.json") else {
        return Ok(());
    };
    let expected_digest = asset_digest(asset, module, version)?;
    let found_digest = Digest::from_sha256(manifest_raw);
    if found_digest != expected_digest {
        return Err(Error::InvalidReleaseMetadata(format!(
            "synthesized vo.release.json for {} {} does not match GitHub release asset digest: expected {}, found {}",
            module, version, expected_digest, found_digest,
        )));
    }
    if asset.size != manifest_raw.len() as u64 {
        return Err(Error::InvalidReleaseMetadata(format!(
            "synthesized vo.release.json for {} {} size does not match GitHub release asset: expected {} bytes, found {} bytes",
            module,
            version,
            asset.size,
            manifest_raw.len(),
        )));
    }
    Ok(())
}

async fn fetch_source_files(
    module: &ModulePath,
    version: &ExactVersion,
    asset_name: &str,
) -> Result<SourcePayload> {
    let context = fetch_release_context(module, version).await?;
    required_release_asset(&context.release, asset_name, module, version)?;
    let ext_manifest = fetch_extension_manifest(module, &context.commit).await?;
    let tree = fetch_repo_tree(module, &context.commit).await?;

    let mut paths = BTreeSet::new();
    for entry in tree.tree {
        if entry.kind != "blob" {
            continue;
        }
        let Some(module_rel) = module_relative_path(module, &entry.path) else {
            continue;
        };
        if source_file_allowed(&module_rel, ext_manifest.as_ref()) {
            paths.insert(module_rel);
        }
    }

    if !paths.contains(VO_MOD_FILE) {
        return Err(Error::SourceScan(format!(
            "GitHub tag for {} {} is missing vo.mod",
            module, version,
        )));
    }

    let mut files = Vec::new();
    for path in paths {
        let content = fetch_module_file_text(module, &context.commit, &path).await?;
        files.push((PathBuf::from(path), content));
    }

    Ok(SourcePayload::Files(files))
}

async fn fetch_web_artifact(
    module: &ModulePath,
    version: &ExactVersion,
    artifact: &ArtifactId,
) -> Result<Vec<u8>> {
    let context = fetch_release_context(module, version).await?;
    let ext_manifest = fetch_extension_manifest(module, &context.commit).await?;
    let ext_manifest = ext_manifest.ok_or_else(|| Error::MissingArtifact {
        module: module.as_str().to_string(),
        version: version.to_string(),
        detail: format!("{} requires vo.ext.toml", artifact.name),
    })?;
    let repo_path = web_artifact_source_path(&ext_manifest, artifact, module, version)?;
    fetch_module_file_bytes(module, &context.commit, &repo_path).await
}

fn web_artifact_source_path(
    ext_manifest: &ExtensionManifest,
    artifact: &ArtifactId,
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<String> {
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
    let wasm = ext_manifest
        .wasm
        .as_ref()
        .ok_or_else(|| Error::MissingArtifact {
            module: module.as_str().to_string(),
            version: version.to_string(),
            detail: format!("vo.ext.toml does not declare {}", artifact.name),
        })?;
    match artifact.kind.as_str() {
        "extension-wasm" if artifact.name == wasm.wasm => {
            Ok(wasm.local_wasm.clone().unwrap_or_else(|| wasm.wasm.clone()))
        }
        "extension-js-glue" => {
            let Some(js_glue) = wasm.js_glue.as_ref() else {
                return Err(Error::MissingArtifact {
                    module: module.as_str().to_string(),
                    version: version.to_string(),
                    detail: format!("vo.ext.toml does not declare {}", artifact.name),
                });
            };
            if artifact.name != *js_glue {
                return Err(Error::MissingArtifact {
                    module: module.as_str().to_string(),
                    version: version.to_string(),
                    detail: format!("vo.ext.toml does not declare {}", artifact.name),
                });
            }
            Ok(wasm
                .local_js_glue
                .clone()
                .unwrap_or_else(|| js_glue.clone()))
        }
        _ => Err(Error::MissingArtifact {
            module: module.as_str().to_string(),
            version: version.to_string(),
            detail: format!("vo.ext.toml does not declare {}", artifact.name),
        }),
    }
}

async fn fetch_extension_manifest(
    module: &ModulePath,
    commit: &str,
) -> Result<Option<ExtensionManifest>> {
    let url = raw_module_file_url(module, commit, VO_EXT_FILE);
    let Some(content) = fetch_optional_text(&url).await? else {
        return Ok(None);
    };
    parse_ext_manifest_content(&content, Path::new(VO_EXT_FILE)).map(Some)
}

async fn fetch_repo_tree(module: &ModulePath, commit: &str) -> Result<GitHubTree> {
    let repo = vo_module::registry::repository_id(module);
    let api_url = format!(
        "https://api.github.com/repos/{}/{}/git/trees/{}?recursive=1",
        encode_component(&repo.owner),
        encode_component(&repo.repo),
        encode_component(commit),
    );
    let tree: GitHubTree = fetch_json(&api_url).await?;
    if tree.truncated {
        return Err(Error::SourceScan(format!(
            "GitHub tree for {} at {} is truncated",
            module, commit,
        )));
    }
    Ok(tree)
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

fn module_relative_path(module: &ModulePath, repo_path: &str) -> Option<String> {
    let root = module.module_root();
    if root == "." {
        return Some(repo_path.to_string());
    }
    let prefix = format!("{}/", root.trim_end_matches('/'));
    repo_path
        .strip_prefix(&prefix)
        .filter(|path| !path.is_empty())
        .map(ToString::to_string)
}

fn source_file_allowed(path: &str, ext_manifest: Option<&ExtensionManifest>) -> bool {
    let name = Path::new(path)
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("");
    if path.ends_with(".vo") || name == VO_MOD_FILE || name == VO_LOCK_FILE || name == VO_EXT_FILE {
        return true;
    }
    let Some(ext_manifest) = ext_manifest else {
        return false;
    };
    let path = Path::new(path);
    ext_manifest
        .include
        .iter()
        .any(|include| path == include || path.starts_with(include))
}

fn required_release_asset<'a>(
    release: &'a GitHubRelease,
    name: &str,
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<&'a GitHubReleaseAsset> {
    release_asset(release, name).ok_or_else(|| {
        Error::InvalidReleaseMetadata(format!(
            "GitHub release for {} {} is missing asset '{}'",
            module, version, name,
        ))
    })
}

fn release_asset<'a>(release: &'a GitHubRelease, name: &str) -> Option<&'a GitHubReleaseAsset> {
    release.assets.iter().find(|asset| asset.name == name)
}

fn asset_digest(
    asset: &GitHubReleaseAsset,
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<Digest> {
    let digest = asset.digest.as_deref().ok_or_else(|| {
        Error::InvalidReleaseMetadata(format!(
            "GitHub release asset '{}' for {} {} has no digest",
            asset.name, module, version,
        ))
    })?;
    Digest::parse(digest).map_err(|error| {
        Error::InvalidReleaseMetadata(format!(
            "GitHub release asset '{}' for {} {} has invalid digest: {}",
            asset.name, module, version, error,
        ))
    })
}

fn source_package_name(module: &ModulePath, version: &ExactVersion) -> Result<String> {
    let base = source_package_base_name(module.as_str()).ok_or_else(|| {
        Error::InvalidReleaseMetadata(format!("invalid module path '{}'", module))
    })?;
    Ok(format!("{}-{}.tar.gz", base, version))
}

fn source_package_base_name(module: &str) -> Option<&str> {
    let parts: Vec<&str> = module.split('/').collect();
    if parts.len() < 3 || parts[0] != "github.com" {
        return None;
    }
    let tail = &parts[3..];
    if tail.is_empty() {
        return Some(parts[2]);
    }
    let last = tail[tail.len() - 1];
    if is_major_version_suffix(last) {
        if tail.len() == 1 {
            Some(parts[2])
        } else {
            Some(tail[tail.len() - 2])
        }
    } else {
        Some(last)
    }
}

fn is_major_version_suffix(value: &str) -> bool {
    value.len() > 1 && value.starts_with('v') && value[1..].chars().all(|ch| ch.is_ascii_digit())
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
