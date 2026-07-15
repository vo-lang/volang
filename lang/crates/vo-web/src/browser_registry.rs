use std::cell::RefCell;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use futures_util::future::join_all;
use serde::de::{self, IgnoredAny, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};
use wasm_bindgen::{closure::Closure, JsCast};
use wasm_bindgen_futures::JsFuture;

use vo_module::async_install::{AsyncRegistry, BoxFuture, SourcePayload};
use vo_module::digest::Digest;
use vo_module::identity::{ArtifactId, ModulePath};
use vo_module::schema::manifest::ReleaseManifest;
use vo_module::schema::{SourceFileEntry, WebManifest};
use vo_module::version::ExactVersion;
use vo_module::{Error, Result};

const WASM_TARGET: &str = "wasm32-unknown-unknown";
const VO_WEB_FILE: &str = "vo.web.json";
const SOURCE_FETCH_BATCH_SIZE: usize = 16;
const MAX_WEB_MANIFEST_CACHE_ENTRIES: usize = 256;
const MAX_RELEASE_MANIFEST_CACHE_ENTRIES: usize = 256;
const MAX_WEB_MANIFEST_CACHE_BYTES: usize = 16 * 1024 * 1024;
const MAX_RELEASE_MANIFEST_CACHE_BYTES: usize = 16 * 1024 * 1024;
const RELEASES_PER_PAGE: usize = 100;
const MAX_RELEASE_PAGES: usize = (vo_module::MAX_REGISTRY_RELEASES - 1) / RELEASES_PER_PAGE + 1;
const REGISTRY_FETCH_IDLE_TIMEOUT_MS: i32 = 30_000;

thread_local! {
    static WEB_MANIFEST_CACHE: RefCell<BTreeMap<String, CachedWebManifest>> = const { RefCell::new(BTreeMap::new()) };
    static RELEASE_MANIFEST_CACHE: RefCell<BTreeMap<String, Vec<u8>>> = const { RefCell::new(BTreeMap::new()) };
}

#[derive(Debug, Clone, Copy, Default)]
pub struct BrowserRegistry;

#[derive(Debug, Clone, Deserialize)]
struct GitHubRelease {
    tag_name: String,
    #[serde(default)]
    draft: bool,
}

struct GitHubReleasePage(Vec<GitHubRelease>);

impl<'de> Deserialize<'de> for GitHubReleasePage {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct GitHubReleasePageVisitor;

        impl<'de> Visitor<'de> for GitHubReleasePageVisitor {
            type Value = GitHubReleasePage;

            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    formatter,
                    "a GitHub release array with at most {RELEASES_PER_PAGE} entries"
                )
            }

            fn visit_seq<A>(self, mut sequence: A) -> std::result::Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                if sequence
                    .size_hint()
                    .is_some_and(|length| length > RELEASES_PER_PAGE)
                {
                    return Err(de::Error::custom(format!(
                        "GitHub release page contains more than {RELEASES_PER_PAGE} entries"
                    )));
                }
                let mut releases = Vec::new();
                releases
                    .try_reserve(sequence.size_hint().unwrap_or(0).min(RELEASES_PER_PAGE))
                    .map_err(|_| de::Error::custom("failed to reserve GitHub release entries"))?;
                loop {
                    if releases.len() == RELEASES_PER_PAGE {
                        if sequence.next_element::<IgnoredAny>()?.is_some() {
                            return Err(de::Error::custom(format!(
                                "GitHub release page contains more than {RELEASES_PER_PAGE} entries"
                            )));
                        }
                        break;
                    }
                    let Some(release) = sequence.next_element::<GitHubRelease>()? else {
                        break;
                    };
                    releases.push(release);
                }
                Ok(GitHubReleasePage(releases))
            }
        }

        deserializer.deserialize_seq(GitHubReleasePageVisitor)
    }
}

fn parse_github_release_page(raw: &[u8], api_url: &str) -> Result<Vec<GitHubRelease>> {
    let mut deserializer = serde_json::Deserializer::from_slice(raw);
    let releases = GitHubReleasePage::deserialize(&mut deserializer)
        .map_err(|error| Error::RegistryError(format!("invalid JSON from {api_url}: {error}")))?
        .0;
    deserializer.end().map_err(|error| {
        Error::RegistryError(format!("invalid trailing data from {api_url}: {error}"))
    })?;
    Ok(releases)
}

#[derive(Debug, Clone)]
struct CachedWebManifest {
    manifest: WebManifest,
    raw: Vec<u8>,
    size: u64,
    digest: Digest,
}

struct FetchDeadline {
    window: web_sys::Window,
    controller: web_sys::AbortController,
    callback: Closure<dyn FnMut()>,
    timeout_id: Option<i32>,
}

impl FetchDeadline {
    fn new(window: &web_sys::Window, url: &str) -> Result<Self> {
        let controller = web_sys::AbortController::new().map_err(|error| {
            js_network_error(error, || {
                format!("failed to create request deadline for {url}")
            })
        })?;
        let abort_controller = controller.clone();
        let callback =
            Closure::wrap(Box::new(move || abort_controller.abort()) as Box<dyn FnMut()>);
        let mut deadline = Self {
            window: window.clone(),
            controller,
            callback,
            timeout_id: None,
        };
        deadline.arm(url)?;
        Ok(deadline)
    }

    fn arm(&mut self, url: &str) -> Result<()> {
        if let Some(timeout_id) = self.timeout_id.take() {
            self.window.clear_timeout_with_handle(timeout_id);
        }
        let timeout_id = self
            .window
            .set_timeout_with_callback_and_timeout_and_arguments_0(
                self.callback.as_ref().unchecked_ref(),
                REGISTRY_FETCH_IDLE_TIMEOUT_MS,
            )
            .map_err(|error| {
                js_network_error(error, || {
                    format!("failed to arm request deadline for {url}")
                })
            })?;
        self.timeout_id = Some(timeout_id);
        Ok(())
    }

    fn signal(&self) -> web_sys::AbortSignal {
        self.controller.signal()
    }
}

impl Drop for FetchDeadline {
    fn drop(&mut self) {
        if let Some(timeout_id) = self.timeout_id.take() {
            self.window.clear_timeout_with_handle(timeout_id);
        }
        self.controller.abort();
    }
}

pub async fn fetch_bytes(url: &str) -> std::result::Result<Vec<u8>, String> {
    fetch_bytes_typed(url, vo_web_runtime_wasm::vfs::MAX_VFS_FILE_BYTES)
        .await
        .map_err(|error| error.to_string())
}

impl AsyncRegistry for BrowserRegistry {
    fn list_version_candidates<'a>(
        &'a self,
        module: &'a ModulePath,
    ) -> BoxFuture<'a, Result<Vec<ExactVersion>>> {
        Box::pin(async move { fetch_module_release_versions(module).await })
    }

    fn fetch_manifest_raw<'a>(
        &'a self,
        module: &'a ModulePath,
        version: &'a ExactVersion,
    ) -> BoxFuture<'a, Result<Vec<u8>>> {
        Box::pin(async move { fetch_release_manifest_raw(module, version).await })
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

async fn fetch_bytes_typed(url: &str, max_bytes: usize) -> Result<Vec<u8>> {
    let window = web_sys::window().ok_or_else(|| Error::Network("no window object".to_string()))?;
    let mut deadline = FetchDeadline::new(&window, url)?;
    let response = fetch_response(url, &window, &deadline.signal()).await?;
    deadline.arm(url)?;
    if !response.ok() {
        return Err(http_error(url, &response));
    }
    response_bytes(url, &response, max_bytes, &mut deadline).await
}

async fn fetch_response(
    url: &str,
    window: &web_sys::Window,
    signal: &web_sys::AbortSignal,
) -> Result<web_sys::Response> {
    let opts = web_sys::RequestInit::new();
    opts.set_method("GET");
    opts.set_cache(web_sys::RequestCache::NoStore);
    opts.set_signal(Some(signal));
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

async fn response_bytes(
    url: &str,
    response: &web_sys::Response,
    max_bytes: usize,
    deadline: &mut FetchDeadline,
) -> Result<Vec<u8>> {
    let declared_length = response
        .headers()
        .get("content-length")
        .map_err(|error| {
            js_network_error(error, || format!("failed to read Content-Length for {url}"))
        })?
        .map(|content_length| {
            content_length.parse::<u64>().map_err(|error| {
                Error::Network(format!(
                    "invalid Content-Length from {url}: {content_length:?}: {error}"
                ))
            })
        })
        .transpose()?;
    if let Some(content_length) = declared_length {
        if content_length > u64::try_from(max_bytes).unwrap_or(u64::MAX) {
            return Err(response_size_error(url, content_length, max_bytes));
        }
    }

    let Some(stream) = response.body() else {
        if declared_length.is_some_and(|content_length| content_length > 0) {
            return Err(Error::Network(format!(
                "response from {url} declared a non-empty body but exposed no response stream"
            )));
        }
        return Ok(Vec::new());
    };
    let reader = web_sys::ReadableStreamDefaultReader::new(&stream).map_err(|error| {
        js_network_error(error, || {
            format!("failed to open response stream for {url}")
        })
    })?;
    let result = async {
        let mut bytes = Vec::new();
        loop {
            let value = JsFuture::from(reader.read()).await.map_err(|error| {
                js_network_error(error, || {
                    format!("failed to read response stream for {url}")
                })
            })?;
            deadline.arm(url)?;
            let result: web_sys::ReadableStreamReadResult = value
                .dyn_into()
                .map_err(|_| Error::Network(format!("invalid response stream result for {url}")))?;
            let done = result.get_done().ok_or_else(|| {
                Error::Network(format!(
                    "response stream result for {url} omitted its completion flag"
                ))
            })?;
            if done {
                return Ok(bytes);
            }
            let chunk: js_sys::Uint8Array = result
                .get_value()
                .dyn_into()
                .map_err(|_| Error::Network(format!("invalid response stream chunk for {url}")))?;
            let chunk_len = usize::try_from(chunk.length()).unwrap_or(usize::MAX);
            let new_len = bytes
                .len()
                .checked_add(chunk_len)
                .ok_or_else(|| response_size_error(url, u64::MAX, max_bytes))?;
            if new_len > max_bytes {
                return Err(response_size_error(
                    url,
                    u64::try_from(new_len).unwrap_or(u64::MAX),
                    max_bytes,
                ));
            }
            bytes.try_reserve(chunk_len).map_err(|_| {
                Error::Network(format!(
                    "failed to reserve {chunk_len} response bytes for {url}"
                ))
            })?;
            let old_len = bytes.len();
            bytes.resize(new_len, 0);
            chunk.copy_to(&mut bytes[old_len..]);
        }
    }
    .await;

    reader.release_lock();
    result
}

fn response_size_error(url: &str, actual: u64, max_bytes: usize) -> Error {
    let _ = actual;
    Error::RegistryResponseTooLarge {
        resource: url.to_string(),
        limit: max_bytes,
    }
}

fn js_network_error(error: wasm_bindgen::JsValue, fallback: impl FnOnce() -> String) -> Error {
    Error::Network(error.as_string().unwrap_or_else(fallback))
}

fn http_error(url: &str, response: &web_sys::Response) -> Error {
    if response.status() == 404 {
        Error::RegistryNotFound {
            resource: url.to_string(),
        }
    } else {
        Error::Network(format!("HTTP {} for {}", response.status(), url))
    }
}

fn release_manifest_fetch_error(
    module: &ModulePath,
    version: &ExactVersion,
    error: Error,
) -> Error {
    match error {
        Error::RegistryNotFound { .. } => Error::InvalidReleaseMetadata(format!(
            "release {module} {version} does not contain vo.release.json"
        )),
        Error::RegistryResponseTooLarge { limit, .. } => Error::InvalidReleaseMetadata(format!(
            "vo.release.json for {module} {version} exceeds the {limit}-byte limit"
        )),
        other => other,
    }
}

async fn fetch_text(url: &str, max_bytes: usize) -> Result<String> {
    let bytes = fetch_bytes_typed(url, max_bytes).await?;
    String::from_utf8(bytes)
        .map_err(|error| Error::RegistryError(format!("invalid UTF-8 from {}: {}", url, error)))
}

async fn fetch_release_manifest(
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<(ReleaseManifest, Vec<u8>)> {
    let manifest_raw = fetch_release_manifest_raw(module, version).await?;
    let manifest = parse_manifest_raw(module, version, &manifest_raw)?;
    let web = fetch_web_manifest(module, version, &manifest).await?;
    validate_web_manifest(module, version, &web)?;
    validate_web_release_contract(module, version, &web, &manifest)?;
    Ok((manifest, manifest_raw))
}

/// Fetch and cache only the exact bounded release-manifest bytes. Parsing and
/// web/source contract I/O belong to the caller after solve-wide byte charge.
async fn fetch_release_manifest_raw(
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<Vec<u8>> {
    let cache_key = module_version_cache_key(module, version);
    if let Some(cached) =
        RELEASE_MANIFEST_CACHE.with(|cache| cache.borrow().get(&cache_key).cloned())
    {
        return Ok(cached);
    }

    let packaged = read_packaged_module_file(
        module,
        version,
        "vo.release.json",
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
    )
    .map_err(|error| release_manifest_fetch_error(module, version, error))?;
    let manifest_raw = match packaged {
        Some(bytes) => bytes,
        None => {
            let url = vo_module::registry::release_download_url(module, version, "vo.release.json");
            fetch_bytes_typed(&url, vo_common::vfs::MAX_TEXT_FILE_BYTES)
                .await
                .map_err(|error| release_manifest_fetch_error(module, version, error))?
        }
    };
    cache_release_manifest(cache_key, manifest_raw.clone());
    Ok(manifest_raw)
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

fn validate_web_release_contract(
    module: &ModulePath,
    version: &ExactVersion,
    web: &WebManifest,
    release: &ReleaseManifest,
) -> Result<()> {
    validate_web_manifest(module, version, web)?;
    web.validate_release_contract(release)
}

async fn fetch_source_files(
    module: &ModulePath,
    version: &ExactVersion,
    asset_name: &str,
) -> Result<SourcePayload> {
    let (release, _) = fetch_release_manifest(module, version).await?;
    if asset_name != release.source.name {
        return Err(Error::InvalidReleaseMetadata(format!(
            "browser source payload for {} {} expected asset {}, found {}",
            module, version, release.source.name, asset_name,
        )));
    }
    let web_snapshot = fetch_web_manifest_snapshot(module, version, &release).await?;
    let web = &web_snapshot.manifest;
    validate_web_manifest(module, version, &web)?;
    if !web.source.iter().any(|entry| entry.path == "vo.mod") {
        return Err(Error::SourceScan(format!(
            "vo.web.json for {} {} is missing vo.mod",
            module, version,
        )));
    }

    let mut files = Vec::new();
    files.try_reserve(web.source.len()).map_err(|_| {
        Error::SourceScan("failed to reserve browser source file entries".to_string())
    })?;
    for batch in web.source.chunks(SOURCE_FETCH_BATCH_SIZE) {
        let results = join_all(
            batch
                .iter()
                .map(|entry| fetch_source_entry(module, version, &web.commit, entry)),
        )
        .await;
        for result in results {
            files.push(result?);
        }
    }
    let mod_content = files
        .iter()
        .find_map(|(path, content)| (path.as_path() == Path::new("vo.mod")).then_some(content))
        .ok_or_else(|| {
            Error::SourceScan(format!(
                "browser source file set for {module} {version} is missing vo.mod"
            ))
        })?;
    let mod_file = vo_module::schema::modfile::ModFile::parse(mod_content)?;
    web.validate_mod_contract(&mod_file)?;
    Ok(SourcePayload::Files {
        source_files: files,
        web_manifest_raw: web_snapshot.raw,
    })
}

async fn fetch_source_entry(
    module: &ModulePath,
    version: &ExactVersion,
    commit: &str,
    entry: &SourceFileEntry,
) -> Result<(PathBuf, String)> {
    validate_web_relative_path(&entry.path)?;
    let max_bytes = usize::try_from(entry.size).map_err(|_| {
        Error::InvalidReleaseMetadata(format!(
            "vo.web.json source size for {} exceeds this platform",
            entry.path
        ))
    })?;
    let content = fetch_module_file_text(module, version, commit, &entry.path, max_bytes).await?;
    let digest = Digest::from_sha256(content.as_bytes());
    if content.len() as u64 != entry.size || digest != entry.digest {
        return Err(Error::DigestMismatch {
            context: format!("source file {} for {} {}", entry.path, module, version),
            expected: format!("{} ({} bytes)", entry.digest, entry.size),
            found: format!("{} ({} bytes)", digest, content.len()),
        });
    }
    Ok((PathBuf::from(&entry.path), content))
}

async fn fetch_web_artifact(
    module: &ModulePath,
    version: &ExactVersion,
    artifact: &ArtifactId,
) -> Result<Vec<u8>> {
    let (release, _) = fetch_release_manifest(module, version).await?;
    let web = fetch_web_manifest(module, version, &release).await?;
    validate_web_manifest(module, version, &web)?;
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
        .find(|entry| entry.id == *artifact)
        .ok_or_else(|| Error::MissingArtifact {
            module: module.as_str().to_string(),
            version: version.to_string(),
            detail: format!("vo.web.json does not declare {}", artifact.name),
        })?;
    validate_web_relative_path(&web_artifact.path)?;
    let max_bytes = usize::try_from(web_artifact.size).map_err(|_| {
        Error::InvalidReleaseMetadata(format!(
            "vo.web.json artifact size for {} exceeds this platform",
            web_artifact.id.name
        ))
    })?;
    let bytes =
        fetch_module_file_bytes(module, version, &web.commit, &web_artifact.path, max_bytes)
            .await?;
    let expected = &web_artifact.digest;
    let found = Digest::from_sha256(&bytes);
    if bytes.len() as u64 != web_artifact.size || found != *expected {
        return Err(Error::DigestMismatch {
            context: format!("artifact {} for {} {}", artifact.name, module, version),
            expected: format!("{} ({} bytes)", expected, web_artifact.size),
            found: format!("{} ({} bytes)", found, bytes.len()),
        });
    }
    Ok(bytes)
}

async fn fetch_web_manifest(
    module: &ModulePath,
    version: &ExactVersion,
    release: &ReleaseManifest,
) -> Result<WebManifest> {
    fetch_web_manifest_snapshot(module, version, release)
        .await
        .map(|snapshot| snapshot.manifest)
}

async fn fetch_web_manifest_snapshot(
    module: &ModulePath,
    version: &ExactVersion,
    release: &ReleaseManifest,
) -> Result<CachedWebManifest> {
    let cache_key = module_version_cache_key(module, version);
    if let Some(bytes) = read_packaged_module_file(
        module,
        version,
        VO_WEB_FILE,
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
    )? {
        let origin = format!(
            "packaged VFS {}",
            packaged_module_file_path(module, version, VO_WEB_FILE)
        );
        let cached = parse_verified_web_manifest(module, version, release, &bytes, &origin)?;
        cache_web_manifest(cache_key, cached.clone());
        return Ok(cached);
    }
    if let Some(cached) = WEB_MANIFEST_CACHE.with(|cache| cache.borrow().get(&cache_key).cloned()) {
        validate_cached_web_manifest(module, version, release, &cached)?;
        return Ok(cached);
    }
    let url = web_manifest_download_url(module, version);
    let bytes = fetch_bytes_typed(&url, vo_common::vfs::MAX_TEXT_FILE_BYTES).await?;
    let cached = parse_verified_web_manifest(module, version, release, &bytes, &url)?;
    cache_web_manifest(cache_key, cached.clone());
    Ok(cached)
}

fn parse_verified_web_manifest(
    module: &ModulePath,
    version: &ExactVersion,
    release: &ReleaseManifest,
    bytes: &[u8],
    origin: &str,
) -> Result<CachedWebManifest> {
    let context = format!("vo.web.json for {module} {version}");
    vo_module::digest::verify_size_and_digest(
        bytes,
        release.web_manifest.size,
        &release.web_manifest.digest,
        &context,
    )?;
    let manifest = WebManifest::parse(bytes).map_err(|error| {
        Error::InvalidReleaseMetadata(format!("invalid vo.web.json from {origin}: {error}"))
    })?;
    validate_web_manifest(module, version, &manifest)?;
    Ok(CachedWebManifest {
        manifest,
        raw: bytes.to_vec(),
        size: bytes.len() as u64,
        digest: release.web_manifest.digest.clone(),
    })
}

fn validate_cached_web_manifest(
    module: &ModulePath,
    version: &ExactVersion,
    release: &ReleaseManifest,
    cached: &CachedWebManifest,
) -> Result<()> {
    if cached.size != release.web_manifest.size || cached.digest != release.web_manifest.digest {
        return Err(Error::DigestMismatch {
            context: format!("cached vo.web.json for {module} {version}"),
            expected: format!(
                "{} ({} bytes)",
                release.web_manifest.digest, release.web_manifest.size
            ),
            found: format!("{} ({} bytes)", cached.digest, cached.size),
        });
    }
    Ok(())
}

fn web_manifest_download_url(module: &ModulePath, version: &ExactVersion) -> String {
    vo_module::registry::release_download_url(module, version, VO_WEB_FILE)
}

fn validate_web_manifest(
    module: &ModulePath,
    version: &ExactVersion,
    manifest: &WebManifest,
) -> Result<()> {
    manifest.validate()?;
    if manifest.module != *module || manifest.version != *version {
        return Err(Error::InvalidReleaseMetadata(format!(
            "vo.web.json identity {} {} does not match requested {module} {version}",
            manifest.module, manifest.version,
        )));
    }
    let browser_limit = u64::try_from(vo_web_runtime_wasm::vfs::MAX_VFS_FILE_BYTES)
        .unwrap_or(u64::MAX)
        .min(vo_module::MAX_MODULE_ARTIFACT_BYTES);
    for (index, artifact) in manifest.artifacts.iter().enumerate() {
        if artifact.size > browser_limit {
            return Err(Error::InvalidReleaseMetadata(format!(
                "vo.web.json artifacts[{index}] size {} exceeds the {browser_limit}-byte browser limit",
                artifact.size
            )));
        }
    }
    Ok(())
}

fn cache_web_manifest(key: String, manifest: CachedWebManifest) {
    WEB_MANIFEST_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        let incoming = manifest.raw.len();
        if incoming > MAX_WEB_MANIFEST_CACHE_BYTES {
            return;
        }
        cache.remove(&key);
        let mut bytes = cache.values().map(|entry| entry.raw.len()).sum::<usize>();
        while cache.len() >= MAX_WEB_MANIFEST_CACHE_ENTRIES
            || bytes.saturating_add(incoming) > MAX_WEB_MANIFEST_CACHE_BYTES
        {
            let Some(oldest_key) = cache.keys().next().cloned() else {
                break;
            };
            if let Some(removed) = cache.remove(&oldest_key) {
                bytes = bytes.saturating_sub(removed.raw.len());
            }
        }
        cache.insert(key, manifest);
    });
}

fn cache_release_manifest(key: String, raw: Vec<u8>) {
    RELEASE_MANIFEST_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        let incoming = raw.len();
        if incoming > MAX_RELEASE_MANIFEST_CACHE_BYTES {
            return;
        }
        cache.remove(&key);
        let mut bytes = cache.values().map(Vec::len).sum::<usize>();
        while cache.len() >= MAX_RELEASE_MANIFEST_CACHE_ENTRIES
            || bytes.saturating_add(incoming) > MAX_RELEASE_MANIFEST_CACHE_BYTES
        {
            let Some(oldest_key) = cache.keys().next().cloned() else {
                break;
            };
            if let Some(removed_raw) = cache.remove(&oldest_key) {
                bytes = bytes.saturating_sub(removed_raw.len());
            }
        }
        cache.insert(key, raw);
    });
}

fn validate_web_relative_path(path: &str) -> Result<()> {
    if vo_module::schema::validate_portable_relative_path(path).is_err() {
        return Err(Error::InvalidReleaseMetadata(format!(
            "vo.web.json path must be module-relative and stay inside the module: {path}"
        )));
    }
    Ok(())
}

fn module_version_cache_key(module: &ModulePath, version: &ExactVersion) -> String {
    format!("{}@{}", module.as_str(), version)
}

async fn fetch_module_file_text(
    module: &ModulePath,
    version: &ExactVersion,
    commit: &str,
    rel_path: &str,
    max_bytes: usize,
) -> Result<String> {
    if let Some(bytes) = read_packaged_module_file(module, version, rel_path, max_bytes)? {
        return String::from_utf8(bytes).map_err(|error| {
            Error::RegistryError(format!(
                "invalid UTF-8 from packaged VFS {}: {}",
                packaged_module_file_path(module, version, rel_path),
                error
            ))
        });
    }
    fetch_text(&raw_module_file_url(module, commit, rel_path), max_bytes).await
}

async fn fetch_module_file_bytes(
    module: &ModulePath,
    version: &ExactVersion,
    commit: &str,
    rel_path: &str,
    max_bytes: usize,
) -> Result<Vec<u8>> {
    if let Some(bytes) = read_packaged_module_file(module, version, rel_path, max_bytes)? {
        return Ok(bytes);
    }
    fetch_bytes_typed(&raw_module_file_url(module, commit, rel_path), max_bytes).await
}

fn read_packaged_module_file(
    module: &ModulePath,
    version: &ExactVersion,
    rel_path: &str,
    max_bytes: usize,
) -> Result<Option<Vec<u8>>> {
    let path = packaged_module_file_path(module, version, rel_path);
    match vo_web_runtime_wasm::vfs::exists(&path) {
        Ok(false) => return Ok(None),
        Ok(true) => {}
        Err(stat_error) => {
            return Err(Error::RegistryError(format!(
                "failed to determine whether packaged VFS file {path} exists: {stat_error}"
            )));
        }
    }
    let (_, size, _, _, is_dir, stat_error) = vo_web_runtime_wasm::vfs::stat(&path);
    if let Some(stat_error) = stat_error {
        return Err(Error::RegistryError(format!(
            "failed to inspect packaged VFS file {path}: {stat_error}"
        )));
    }
    if is_dir {
        return Err(Error::RegistryError(format!(
            "packaged VFS file {path} is a directory"
        )));
    }
    if size < 0 {
        return Err(Error::RegistryError(format!(
            "packaged VFS file {path} reported a negative size"
        )));
    }
    if u64::try_from(size).unwrap_or(u64::MAX) > u64::try_from(max_bytes).unwrap_or(u64::MAX) {
        return Err(Error::RegistryResponseTooLarge {
            resource: path,
            limit: max_bytes,
        });
    }
    let (data, err) = vo_web_runtime_wasm::vfs::read_file_limited(&path, max_bytes);
    if err.is_none() {
        return Ok(Some(data));
    }

    let read_error = err.unwrap_or_else(|| "unknown VFS error".to_string());
    Err(Error::RegistryError(format!(
        "failed to read packaged VFS file {path}: {read_error}"
    )))
}

fn packaged_module_file_path(
    module: &ModulePath,
    version: &ExactVersion,
    rel_path: &str,
) -> String {
    let key = module.as_str().replace('/', "@");
    let rel = rel_path.trim_start_matches('/');
    format!("/{}/{}/{}", key, version, rel)
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
    let mut versions = Vec::new();
    let mut processed_listing_bytes = 0usize;
    for page in 1..=MAX_RELEASE_PAGES + 1 {
        let api_url = format!(
            "https://api.github.com/repos/{}/{}/releases?per_page={RELEASES_PER_PAGE}&page={page}",
            encode_component(&repo.owner),
            encode_component(&repo.repo),
        );
        let raw = fetch_bytes_typed(&api_url, vo_common::vfs::MAX_TEXT_FILE_BYTES).await?;
        processed_listing_bytes =
            vo_module::registry::charge_registry_listing_bytes(processed_listing_bytes, raw.len())?;
        let releases = parse_github_release_page(&raw, &api_url)?;
        if page > MAX_RELEASE_PAGES {
            if releases.is_empty() {
                break;
            }
            return Err(Error::RegistryError(format!(
                "release listing for {} exceeds {} pages",
                module, MAX_RELEASE_PAGES
            )));
        }
        let count = releases.len();
        versions.extend(
            releases
                .iter()
                .filter(|release| !release.draft)
                .filter_map(|release| {
                    vo_module::registry::version_from_tag(module, &release.tag_name)
                        .filter(|version| module.accepts_version(version))
                }),
        );
        if count < RELEASES_PER_PAGE {
            break;
        }
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use vo_module::schema::canonical_source_file_set;
    use vo_module::schema::manifest::{ManifestSource, ManifestWebManifest};
    use vo_module::version::ToolchainConstraint;

    #[test]
    fn github_release_page_is_stream_bounded_and_rejects_trailing_data() {
        let entries = (0..RELEASES_PER_PAGE)
            .map(|index| {
                serde_json::json!({
                    "tag_name": format!("v0.0.{index}"),
                    "draft": false,
                })
            })
            .collect::<Vec<_>>();
        let raw = serde_json::to_vec(&entries).unwrap();
        assert_eq!(
            parse_github_release_page(&raw, "test://releases")
                .unwrap()
                .len(),
            RELEASES_PER_PAGE
        );

        let mut oversized = entries;
        oversized.push(serde_json::json!({
            "tag_name": "v0.0.overflow",
            "draft": false,
        }));
        let error =
            parse_github_release_page(&serde_json::to_vec(&oversized).unwrap(), "test://releases")
                .unwrap_err()
                .to_string();
        assert!(error.contains("more than 100"), "{error}");

        let error = parse_github_release_page(b"[] trailing", "test://releases")
            .unwrap_err()
            .to_string();
        assert!(error.contains("trailing data"), "{error}");
    }

    #[test]
    fn release_manifest_absence_and_oversize_are_candidate_local() {
        let module = ModulePath::parse("github.com/acme/pkg").unwrap();
        let version = ExactVersion::parse("v0.2.0").unwrap();
        for error in [
            Error::RegistryNotFound {
                resource: "https://example.invalid/vo.release.json".to_string(),
            },
            Error::RegistryResponseTooLarge {
                resource: "https://example.invalid/vo.release.json".to_string(),
                limit: vo_common::vfs::MAX_TEXT_FILE_BYTES,
            },
        ] {
            assert!(matches!(
                release_manifest_fetch_error(&module, &version, error),
                Error::InvalidReleaseMetadata(_)
            ));
        }
        assert!(matches!(
            release_manifest_fetch_error(
                &module,
                &version,
                Error::Network("transport".to_string())
            ),
            Error::Network(_)
        ));
    }

    fn matching_contract() -> (ModulePath, ExactVersion, WebManifest, ReleaseManifest) {
        let module = ModulePath::parse("github.com/acme/pkg").unwrap();
        let version = ExactVersion::parse("v0.1.0").unwrap();
        let source = vec![SourceFileEntry {
            path: "vo.mod".to_string(),
            size: 3,
            digest: Digest::from_sha256(b"mod"),
        }];
        let files_digest = canonical_source_file_set(&source).unwrap().digest;
        let web = WebManifest {
            schema_version: 1,
            module: module.clone(),
            version: version.clone(),
            commit: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string(),
            module_root: ".".to_string(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: Vec::new(),
            source_digest: files_digest.clone(),
            source,
            artifacts: Vec::new(),
            web: None,
            extension: None,
        };
        let release = ReleaseManifest {
            schema_version: 1,
            module: module.clone(),
            version: version.clone(),
            commit: web.commit.clone(),
            module_root: web.module_root.clone(),
            vo: web.vo.clone(),
            require: Vec::new(),
            source: ManifestSource {
                name: "pkg-v0.1.0.tar.gz".to_string(),
                size: 7,
                digest: Digest::from_sha256(b"archive"),
                files_size: 3,
                files_digest,
            },
            web_manifest: ManifestWebManifest {
                size: 3,
                digest: Digest::from_sha256(b"web"),
            },
            artifacts: Vec::new(),
        };
        (module, version, web, release)
    }

    #[test]
    fn web_source_set_must_match_release_file_set_fields() {
        let (module, version, web, release) = matching_contract();
        validate_web_manifest(&module, &version, &web).unwrap();
        validate_web_release_contract(&module, &version, &web, &release).unwrap();

        let mut wrong_size = release.clone();
        wrong_size.source.files_size += 1;
        assert!(matches!(
            validate_web_release_contract(&module, &version, &web, &wrong_size),
            Err(Error::DigestMismatch { .. })
        ));

        let mut wrong_digest = release;
        wrong_digest.source.files_digest = Digest::from_sha256(b"different file set");
        assert!(matches!(
            validate_web_release_contract(&module, &version, &web, &wrong_digest),
            Err(Error::DigestMismatch { .. })
        ));
    }

    #[test]
    fn web_source_set_rejects_digest_mismatch_and_cache_metadata_paths() {
        let (module, version, mut web, _) = matching_contract();
        web.source_digest = Digest::from_sha256(b"different file set");
        assert!(matches!(
            validate_web_manifest(&module, &version, &web),
            Err(Error::DigestMismatch { .. })
        ));

        let (_, _, mut web, _) = matching_contract();
        web.source[0].path = "vo.release.json".to_string();
        assert!(matches!(
            validate_web_manifest(&module, &version, &web),
            Err(Error::InvalidReleaseMetadata(detail))
                if detail.contains("source-set-excluded")
        ));
    }

    #[test]
    fn browser_manifest_fallback_uses_the_release_asset() {
        let module = ModulePath::parse("github.com/acme/mono/graphics/v2").unwrap();
        let version = ExactVersion::parse("v2.1.0").unwrap();

        assert_eq!(
            web_manifest_download_url(&module, &version),
            "https://github.com/acme/mono/releases/download/graphics/v2/v2.1.0/vo.web.json"
        );
    }

    #[test]
    fn browser_manifest_bytes_are_verified_before_json_parsing() {
        let (module, version, _, mut release) = matching_contract();
        let invalid_json = br#"{"#;
        release.web_manifest.size = invalid_json.len() as u64;
        release.web_manifest.digest = Digest::from_sha256(b"different bytes");

        assert!(matches!(
            parse_verified_web_manifest(&module, &version, &release, invalid_json, "test fixture"),
            Err(Error::DigestMismatch { .. })
        ));

        release.web_manifest.digest = Digest::from_sha256(invalid_json);
        assert!(matches!(
            parse_verified_web_manifest(
                &module,
                &version,
                &release,
                invalid_json,
                "test fixture"
            ),
            Err(Error::InvalidReleaseMetadata(detail)) if detail.contains("invalid vo.web.json")
        ));
    }

    #[test]
    fn verified_browser_manifest_bytes_parse_successfully() {
        let (module, version, web, mut release) = matching_contract();
        let bytes = serde_json::to_vec(&serde_json::json!({
            "schema_version": web.schema_version,
            "module": web.module.to_string(),
            "version": web.version.to_string(),
            "commit": web.commit,
            "module_root": web.module_root,
            "vo": web.vo.to_string(),
            "require": [],
            "source_digest": web.source_digest.to_string(),
            "source": web.source,
            "artifacts": [],
            "web": null,
            "extension": null,
        }))
        .unwrap();
        release.web_manifest.size = bytes.len() as u64;
        release.web_manifest.digest = Digest::from_sha256(&bytes);

        let cached =
            parse_verified_web_manifest(&module, &version, &release, &bytes, "test fixture")
                .unwrap();
        assert_eq!(cached.size, release.web_manifest.size);
        assert_eq!(cached.digest, release.web_manifest.digest);
        assert_eq!(cached.manifest.module, module);
        assert_eq!(cached.manifest.version, version);
    }
}
