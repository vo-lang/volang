use std::cell::RefCell;
use std::collections::BTreeMap;
use std::path::PathBuf;

use serde::de::{self, IgnoredAny, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};
use wasm_bindgen::{closure::Closure, JsCast};
use wasm_bindgen_futures::JsFuture;

use vo_module::async_install::{AsyncRegistry, BoxFuture, SourcePayload};
use vo_module::cache::install::ExtractedSourceFile;
use vo_module::digest::Digest;
use vo_module::github_provenance::{
    github_provenance_fetch_error, github_release_metadata_url, github_tag_ref_url,
    parse_github_release_provenance, validate_github_tag_commit, GitHubTagResolver, GitHubTagStep,
    GITHUB_API_VERSION, MAX_GITHUB_API_RESPONSE_BYTES,
};
use vo_module::identity::{ArtifactId, ModulePath};
use vo_module::schema::manifest::ReleaseManifest;
use vo_module::schema::PackageManifest;
use vo_module::version::ExactVersion;
use vo_module::{Error, Result};

const WASM_TARGET: &str = "wasm32-unknown-unknown";
const PACKAGE_FILE: &str = "vo.package.json";
const MAX_RELEASE_MANIFEST_CACHE_ENTRIES: usize = 256;
const MAX_RELEASE_MANIFEST_CACHE_BYTES: usize = 16 * 1024 * 1024;
pub const MAX_PACKAGED_RELEASE_CAPABILITIES: usize = 10_000;
const RELEASES_PER_PAGE: usize = 100;
const MAX_RELEASE_PAGES: usize = (vo_module::MAX_REGISTRY_RELEASES - 1) / RELEASES_PER_PAGE + 1;
const REGISTRY_FETCH_IDLE_TIMEOUT_MS: i32 = 30_000;

thread_local! {
    static RELEASE_MANIFEST_CACHE: RefCell<BTreeMap<String, Vec<u8>>> = const { RefCell::new(BTreeMap::new()) };
    static PACKAGED_RELEASE_CAPABILITIES: RefCell<BTreeMap<String, PackagedReleaseCapability>> = const { RefCell::new(BTreeMap::new()) };
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct PackagedReleaseCapability {
    root: String,
    release_digest: Digest,
}

/// Host-authenticated identity and VFS root for one packaged browser release.
///
/// The caller must make every referenced tree visible in the browser VFS for
/// the duration of [`register_packaged_release_capabilities`]. The registry
/// validates all manifests and table conflicts before publishing any entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackagedReleaseCapabilitySpec {
    pub module: String,
    pub version: String,
    pub release_digest: String,
    pub root: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ValidatedPackagedReleaseCapability {
    module: ModulePath,
    version: ExactVersion,
    capability: PackagedReleaseCapability,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct BrowserRegistry;

/// Register one host-authenticated, immutable local release tree.
///
/// A browser VFS path has no authority by itself. The embedding host must bind
/// the exact module, version, raw release-manifest digest, and tree root before
/// [`BrowserRegistry`] will read any packaged bytes from that tree. Repeating
/// an identical registration is idempotent; rebinding either the identity or
/// root is rejected for the lifetime of this WASM instance.
pub fn register_packaged_release_capability(
    module: &str,
    version: &str,
    release_digest: &str,
    root: &str,
) -> Result<()> {
    register_packaged_release_capabilities(&[PackagedReleaseCapabilitySpec {
        module: module.to_string(),
        version: version.to_string(),
        release_digest: release_digest.to_string(),
        root: root.to_string(),
    }])
}

/// Atomically register a complete host-authenticated packaged-release batch.
///
/// Validation reads every `vo.release.json` from the currently visible VFS,
/// authenticates it, and checks the complete candidate table on a private
/// clone. The live authority table changes only after the whole batch passes.
/// Repeating identical bindings is idempotent.
pub fn register_packaged_release_capabilities(
    specs: &[PackagedReleaseCapabilitySpec],
) -> Result<()> {
    if specs.len() > MAX_PACKAGED_RELEASE_CAPABILITIES {
        return Err(Error::InvalidReleaseMetadata(format!(
            "packaged release capability batch exceeds {MAX_PACKAGED_RELEASE_CAPABILITIES} entries",
        )));
    }
    let mut validated = Vec::new();
    validated.try_reserve_exact(specs.len()).map_err(|_| {
        Error::InvalidReleaseMetadata(
            "failed to reserve the packaged release capability batch".to_string(),
        )
    })?;
    for spec in specs {
        validated.push(validate_packaged_release_capability_spec(spec)?);
    }
    insert_packaged_release_capabilities_atomically(&validated)
}

fn validate_packaged_release_capability_spec(
    spec: &PackagedReleaseCapabilitySpec,
) -> Result<ValidatedPackagedReleaseCapability> {
    let module = ModulePath::parse(&spec.module)?;
    let version = ExactVersion::parse(&spec.version)?;
    if !module.accepts_version(&version) {
        return Err(Error::InvalidReleaseMetadata(format!(
            "packaged release capability version {version} is incompatible with module {module}",
        )));
    }
    let release_digest = Digest::parse(&spec.release_digest)?;
    let root = canonical_packaged_release_root(&spec.root)?;
    let manifest_path = packaged_file_path_at_root(&root, "vo.release.json")?;
    let manifest_raw = read_packaged_vfs_file(&manifest_path, vo_common::vfs::MAX_TEXT_FILE_BYTES)?
        .ok_or_else(|| {
            Error::InvalidReleaseMetadata(format!(
                "packaged release capability root {root} is missing vo.release.json"
            ))
        })?;
    validate_packaged_release_capability_raw(&module, &version, &release_digest, &manifest_raw)?;
    Ok(ValidatedPackagedReleaseCapability {
        module,
        version,
        capability: PackagedReleaseCapability {
            root,
            release_digest,
        },
    })
}

#[derive(Debug, Clone, Deserialize)]
struct GitHubRelease {
    tag_name: String,
    draft: bool,
    immutable: bool,
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

fn immutable_release_version(module: &ModulePath, release: &GitHubRelease) -> Option<ExactVersion> {
    if release.draft || !release.immutable {
        return None;
    }
    vo_module::registry::version_from_tag(module, &release.tag_name)
        .filter(|version| module.accepts_version(version))
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
        Box::pin(async move { fetch_source_payload(module, version, asset_name).await })
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
    if url.starts_with("https://api.github.com/") {
        request
            .headers()
            .set("Accept", "application/vnd.github+json")
            .map_err(|error| {
                js_network_error(error, || {
                    format!("failed to set GitHub API Accept header for {url}")
                })
            })?;
        request
            .headers()
            .set("X-GitHub-Api-Version", GITHUB_API_VERSION)
            .map_err(|error| {
                js_network_error(error, || {
                    format!("failed to set GitHub API version header for {url}")
                })
            })?;
    }

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

async fn fetch_release_manifest(
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<(ReleaseManifest, Vec<u8>)> {
    let manifest_raw = fetch_release_manifest_raw(module, version).await?;
    let manifest = parse_manifest_raw(module, version, &manifest_raw)?;
    Ok((manifest, manifest_raw))
}

/// Fetch and cache only the exact bounded release-manifest bytes. Parsing and
/// web/source contract I/O belong to the caller after solve-wide byte charge.
async fn fetch_release_manifest_raw(
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<Vec<u8>> {
    let cache_key = module_version_cache_key(module, version);
    let cached = RELEASE_MANIFEST_CACHE.with(|cache| cache.borrow().get(&cache_key).cloned());
    if let Some(manifest_raw) = read_packaged_module_file(
        module,
        version,
        "vo.release.json",
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
    )
    .map_err(|error| release_manifest_fetch_error(module, version, error))?
    {
        // The embedding host authenticated this exact raw release digest and
        // registered its immutable capability before BrowserRegistry could
        // observe the tree. Package/source/artifact bytes remain digest-bound
        // to that release; path existence carries no authority.
        validate_packaged_release_manifest_raw(module, version, &manifest_raw)?;
        if let Some(cached) = cached {
            validate_immutable_packaged_release_cache(module, version, &cached, &manifest_raw)?;
            return Ok(cached);
        }
        cache_release_manifest(cache_key, manifest_raw.clone());
        return Ok(manifest_raw);
    }
    if let Some(cached) = cached {
        return Ok(cached);
    }

    let transport = WindowProvenanceTransport;
    let manifest_raw = fetch_remote_release_manifest_raw(&transport, module, version).await?;
    cache_release_manifest(cache_key, manifest_raw.clone());
    Ok(manifest_raw)
}

fn validate_packaged_release_manifest_raw(
    module: &ModulePath,
    version: &ExactVersion,
    manifest_raw: &[u8],
) -> Result<()> {
    parse_manifest_raw(module, version, manifest_raw).map(|_| ())
}

fn validate_packaged_release_capability_raw(
    module: &ModulePath,
    version: &ExactVersion,
    release_digest: &Digest,
    manifest_raw: &[u8],
) -> Result<()> {
    vo_module::digest::verify_digest(
        manifest_raw,
        release_digest,
        format!("packaged vo.release.json for {module} {version}"),
    )?;
    validate_packaged_release_manifest_raw(module, version, manifest_raw)
}

fn validate_immutable_packaged_release_cache(
    module: &ModulePath,
    version: &ExactVersion,
    cached: &[u8],
    current: &[u8],
) -> Result<()> {
    vo_module::digest::verify_size_and_digest(
        current,
        u64::try_from(cached.len()).unwrap_or(u64::MAX),
        &Digest::from_sha256(cached),
        format!("immutable packaged vo.release.json for {module} {version}"),
    )
}

trait ProvenanceTransport {
    fn fetch<'a>(&'a self, url: &'a str, max_bytes: usize) -> BoxFuture<'a, Result<Vec<u8>>>;
}

struct WindowProvenanceTransport;

impl ProvenanceTransport for WindowProvenanceTransport {
    fn fetch<'a>(&'a self, url: &'a str, max_bytes: usize) -> BoxFuture<'a, Result<Vec<u8>>> {
        Box::pin(fetch_bytes_typed(url, max_bytes))
    }
}

async fn fetch_remote_release_manifest_raw<T: ProvenanceTransport + ?Sized>(
    transport: &T,
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<Vec<u8>> {
    let release_metadata_url = github_release_metadata_url(module, version);
    let release_metadata_raw = transport
        .fetch(&release_metadata_url, MAX_GITHUB_API_RESPONSE_BYTES)
        .await
        .map_err(|error| {
            github_provenance_fetch_error(module, version, "release metadata", error)
        })?;
    let provenance = parse_github_release_provenance(&release_metadata_raw, module, version)?;
    provenance.require_release_manifest_asset(module, version)?;

    let manifest_url =
        vo_module::registry::release_download_url(module, version, "vo.release.json");
    let manifest_raw = transport
        .fetch(&manifest_url, vo_common::vfs::MAX_TEXT_FILE_BYTES)
        .await
        .map_err(|error| release_manifest_fetch_error(module, version, error))?;
    let manifest = parse_manifest_raw(module, version, &manifest_raw)?;

    let tag_ref_url = github_tag_ref_url(module, version);
    let tag_ref_raw = transport
        .fetch(&tag_ref_url, MAX_GITHUB_API_RESPONSE_BYTES)
        .await
        .map_err(|error| {
            github_provenance_fetch_error(module, version, "release tag reference", error)
        })?;
    let mut resolver = GitHubTagResolver::from_reference(&tag_ref_raw, module, version)?;
    let tag_commit = loop {
        match resolver.next_step()? {
            GitHubTagStep::Complete(commit) => break commit,
            GitHubTagStep::FetchAnnotatedTag { sha, url } => {
                let annotated_raw = transport
                    .fetch(&url, MAX_GITHUB_API_RESPONSE_BYTES)
                    .await
                    .map_err(|error| {
                        github_provenance_fetch_error(
                            module,
                            version,
                            "annotated tag object",
                            error,
                        )
                    })?;
                resolver.accept_annotated_tag(&sha, &annotated_raw)?;
            }
        }
    };
    validate_github_tag_commit(module, version, &tag_commit, &manifest)?;
    provenance.validate_assets(module, version, &manifest, &manifest_raw)?;
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

async fn fetch_source_payload(
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

    // A host-authenticated packaged tree is already materialized as individual
    // files in the browser VFS. Keep that capability closed and offline. Remote
    // releases use the same authenticated source archive and canonical parser
    // as native installs, avoiding a second source authority at the Git commit.
    if packaged_release_capability(module, version).is_some() {
        return read_packaged_source_files(module, version, &release);
    }

    fetch_remote_source_payload(&WindowProvenanceTransport, module, version, &release).await
}

async fn fetch_remote_source_payload<T: ProvenanceTransport + ?Sized>(
    transport: &T,
    module: &ModulePath,
    version: &ExactVersion,
    release: &ReleaseManifest,
) -> Result<SourcePayload> {
    if release.source.size > vo_module::MAX_SOURCE_ARCHIVE_BYTES {
        return Err(Error::InvalidReleaseMetadata(format!(
            "source package for {module} {version} is {} bytes, exceeding the {}-byte archive limit",
            release.source.size,
            vo_module::MAX_SOURCE_ARCHIVE_BYTES,
        )));
    }
    let max_bytes = usize::try_from(release.source.size).map_err(|_| {
        Error::InvalidReleaseMetadata(format!(
            "source package size for {module} {version} exceeds this platform",
        ))
    })?;
    let url = vo_module::registry::release_download_url(module, version, &release.source.name);
    let source_package = transport.fetch(&url, max_bytes).await?;
    vo_module::digest::verify_size_and_digest(
        &source_package,
        release.source.size,
        &release.source.digest,
        format!("source package for {module} {version}"),
    )?;
    Ok(SourcePayload::Package(source_package))
}

fn read_packaged_source_files(
    module: &ModulePath,
    version: &ExactVersion,
    release: &ReleaseManifest,
) -> Result<SourcePayload> {
    packaged_source_payload_from_reader(module, version, release, |path, max_bytes| {
        read_packaged_module_file(module, version, path, max_bytes)?.ok_or_else(|| {
            Error::RegistryNotFound {
                resource: format!(
                    "packaged release tree for {module} {version} is missing {path}",
                ),
            }
        })
    })
}

fn packaged_source_payload_from_reader(
    module: &ModulePath,
    version: &ExactVersion,
    release: &ReleaseManifest,
    mut read: impl FnMut(&str, usize) -> Result<Vec<u8>>,
) -> Result<SourcePayload> {
    let package_raw = read(PACKAGE_FILE, vo_common::vfs::MAX_TEXT_FILE_BYTES)?;
    vo_module::digest::verify_size_and_digest(
        &package_raw,
        release.package.size,
        &release.package.digest,
        format!("vo.package.json for {module} {version}"),
    )?;
    let package = PackageManifest::parse(&package_raw).map_err(|error| {
        Error::InvalidReleaseMetadata(format!(
            "invalid vo.package.json for {module} {version}: {error}",
        ))
    })?;
    if !package.files.iter().any(|entry| entry.path == "vo.mod") {
        return Err(Error::SourceScan(format!(
            "vo.package.json for {} {} is missing vo.mod",
            module, version,
        )));
    }

    let mut files = Vec::new();
    files.try_reserve(package.files.len()).map_err(|_| {
        Error::SourceScan("failed to reserve browser source file entries".to_string())
    })?;
    for entry in &package.files {
        validate_package_relative_path(&entry.path)?;
        let max_bytes = usize::try_from(entry.size).map_err(|_| {
            Error::InvalidReleaseMetadata(format!(
                "vo.package.json source size for {} exceeds this platform",
                entry.path,
            ))
        })?;
        let bytes = read(&entry.path, max_bytes)?;
        let digest = Digest::from_sha256(&bytes);
        if u64::try_from(bytes.len()).unwrap_or(u64::MAX) != entry.size || digest != entry.digest {
            return Err(Error::DigestMismatch {
                context: format!("source file {} for {module} {version}", entry.path),
                expected: format!("{} ({} bytes)", entry.digest, entry.size),
                found: format!("{} ({} bytes)", digest, bytes.len()),
            });
        }
        files.push(ExtractedSourceFile {
            path: PathBuf::from(&entry.path),
            mode: entry.mode,
            bytes,
        });
    }
    Ok(SourcePayload::Files {
        source_files: files,
        package_raw,
    })
}

async fn fetch_web_artifact(
    module: &ModulePath,
    version: &ExactVersion,
    artifact: &ArtifactId,
) -> Result<Vec<u8>> {
    let (release, _) = fetch_release_manifest(module, version).await?;
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
    let published = release
        .artifacts
        .iter()
        .find(|entry| entry.id == *artifact)
        .ok_or_else(|| Error::MissingArtifact {
            module: module.as_str().to_string(),
            version: version.to_string(),
            detail: format!("vo.release.json does not declare {}", artifact.name),
        })?;
    let browser_limit = u64::try_from(vo_web_runtime_wasm::vfs::MAX_VFS_FILE_BYTES)
        .unwrap_or(u64::MAX)
        .min(vo_module::MAX_MODULE_ARTIFACT_BYTES);
    if published.size > browser_limit {
        return Err(Error::InvalidReleaseMetadata(format!(
            "artifact {} size {} exceeds the {browser_limit}-byte browser limit",
            published.id.name, published.size,
        )));
    }
    let max_bytes = usize::try_from(published.size).map_err(|_| {
        Error::InvalidReleaseMetadata(format!(
            "artifact size for {} exceeds this platform",
            published.id.name,
        ))
    })?;
    let relative = vo_module::artifact::artifact_relative_path(artifact)
        .map_err(Error::InvalidReleaseMetadata)?;
    let relative = relative.to_str().ok_or_else(|| {
        Error::InvalidReleaseMetadata("artifact cache path is not valid UTF-8".to_string())
    })?;
    let bytes = match read_packaged_module_file(module, version, relative, max_bytes)? {
        Some(bytes) => bytes,
        None => {
            let asset_name = vo_module::artifact::artifact_release_asset_name(artifact)
                .map_err(Error::InvalidReleaseMetadata)?;
            let url = vo_module::registry::release_download_url(module, version, &asset_name);
            fetch_bytes_typed(&url, max_bytes).await?
        }
    };
    vo_module::digest::verify_size_and_digest(
        &bytes,
        published.size,
        &published.digest,
        format!("artifact {} for {module} {version}", artifact.name),
    )?;
    Ok(bytes)
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

fn validate_package_relative_path(path: &str) -> Result<()> {
    if !vo_module::schema::is_package_file_candidate(path).map_err(Error::InvalidReleaseMetadata)? {
        return Err(Error::InvalidReleaseMetadata(format!(
            "vo.package.json path is reserved by the module protocol: {path}",
        )));
    }
    Ok(())
}

fn module_version_cache_key(module: &ModulePath, version: &ExactVersion) -> String {
    format!("{}@{}", module.as_str(), version)
}

fn canonical_packaged_release_root(root: &str) -> Result<String> {
    if !root.starts_with('/') || root == "/" || root.ends_with('/') {
        return Err(Error::InvalidReleaseMetadata(format!(
            "packaged release capability root must be a non-root canonical absolute path: {root:?}",
        )));
    }
    let relative = root.strip_prefix('/').expect("absolute root has a slash");
    vo_module::schema::validate_portable_relative_path(relative).map_err(|error| {
        Error::InvalidReleaseMetadata(format!(
            "invalid packaged release capability root {root:?}: {error}",
        ))
    })?;
    Ok(root.to_string())
}

#[cfg(test)]
fn insert_packaged_release_capability(
    module: &ModulePath,
    version: &ExactVersion,
    capability: PackagedReleaseCapability,
) -> Result<()> {
    PACKAGED_RELEASE_CAPABILITIES.with(|capabilities| {
        let mut capabilities = capabilities.borrow_mut();
        insert_packaged_release_capability_into(&mut capabilities, module, version, capability)
    })
}

fn insert_packaged_release_capabilities_atomically(
    validated: &[ValidatedPackagedReleaseCapability],
) -> Result<()> {
    PACKAGED_RELEASE_CAPABILITIES.with(|capabilities| {
        let mut candidate_table = capabilities.borrow().clone();
        for validated in validated {
            insert_packaged_release_capability_into(
                &mut candidate_table,
                &validated.module,
                &validated.version,
                validated.capability.clone(),
            )?;
        }
        *capabilities.borrow_mut() = candidate_table;
        Ok(())
    })
}

fn insert_packaged_release_capability_into(
    capabilities: &mut BTreeMap<String, PackagedReleaseCapability>,
    module: &ModulePath,
    version: &ExactVersion,
    capability: PackagedReleaseCapability,
) -> Result<()> {
    let key = module_version_cache_key(module, version);
    if let Some(existing) = capabilities.get(&key) {
        if existing == &capability {
            return Ok(());
        }
        return Err(Error::InvalidReleaseMetadata(format!(
            "packaged release capability for {module} {version} is already bound to {} with digest {}",
            existing.root, existing.release_digest,
        )));
    }
    let root_key = vo_module::schema::portable_case_key(&capability.root);
    if let Some((existing_key, existing)) = capabilities
        .iter()
        .find(|(_, existing)| vo_module::schema::portable_case_key(&existing.root) == root_key)
    {
        return Err(Error::InvalidReleaseMetadata(format!(
            "packaged release capability root {} is already bound to {existing_key} with digest {}",
            capability.root, existing.release_digest,
        )));
    }
    if capabilities.len() >= MAX_PACKAGED_RELEASE_CAPABILITIES {
        return Err(Error::InvalidReleaseMetadata(format!(
            "packaged release capability table exceeds {MAX_PACKAGED_RELEASE_CAPABILITIES} entries",
        )));
    }
    capabilities.insert(key, capability);
    Ok(())
}

fn packaged_release_capability(
    module: &ModulePath,
    version: &ExactVersion,
) -> Option<PackagedReleaseCapability> {
    let key = module_version_cache_key(module, version);
    PACKAGED_RELEASE_CAPABILITIES.with(|capabilities| capabilities.borrow().get(&key).cloned())
}

fn read_packaged_module_file(
    module: &ModulePath,
    version: &ExactVersion,
    rel_path: &str,
    max_bytes: usize,
) -> Result<Option<Vec<u8>>> {
    let Some(capability) = packaged_release_capability(module, version) else {
        return Ok(None);
    };
    let path = packaged_file_path_at_root(&capability.root, rel_path)?;
    let bytes =
        read_packaged_vfs_file(&path, max_bytes)?.ok_or_else(|| Error::RegistryNotFound {
            resource: format!(
                "packaged release tree for {module} {version} is missing {rel_path} at {path}"
            ),
        })?;
    if rel_path == "vo.release.json" {
        vo_module::digest::verify_digest(
            &bytes,
            &capability.release_digest,
            format!("packaged vo.release.json for {module} {version}"),
        )?;
        return Ok(Some(bytes));
    }
    Ok(Some(bytes))
}

#[cfg(target_arch = "wasm32")]
fn read_packaged_vfs_file(path: &str, max_bytes: usize) -> Result<Option<Vec<u8>>> {
    match vo_web_runtime_wasm::vfs::exists(path) {
        Ok(false) => return Ok(None),
        Ok(true) => {}
        Err(stat_error) => {
            return Err(Error::RegistryError(format!(
                "failed to determine whether packaged VFS file {path} exists: {stat_error}"
            )));
        }
    }
    let (_, size, _, _, is_dir, stat_error) = vo_web_runtime_wasm::vfs::stat(path);
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
            resource: path.to_string(),
            limit: max_bytes,
        });
    }
    let (data, err) = vo_web_runtime_wasm::vfs::read_file_limited(path, max_bytes);
    if err.is_none() {
        return Ok(Some(data));
    }

    let read_error = err.unwrap_or_else(|| "unknown VFS error".to_string());
    Err(Error::RegistryError(format!(
        "failed to read packaged VFS file {path}: {read_error}"
    )))
}

#[cfg(not(target_arch = "wasm32"))]
fn read_packaged_vfs_file(path: &str, _max_bytes: usize) -> Result<Option<Vec<u8>>> {
    Err(Error::RegistryError(format!(
        "packaged browser VFS file {path} cannot be read on a non-WASM target"
    )))
}

fn packaged_file_path_at_root(root: &str, rel_path: &str) -> Result<String> {
    vo_module::schema::validate_portable_relative_path(rel_path).map_err(|error| {
        Error::InvalidReleaseMetadata(format!(
            "invalid packaged release relative path {rel_path:?}: {error}",
        ))
    })?;
    Ok(format!("{root}/{rel_path}"))
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
                .filter_map(|release| immutable_release_version(module, release)),
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
    use std::future::Future;
    use std::task::{Context, Poll};

    const RELEASE_COMMIT: &str = "0123456789abcdef0123456789abcdef01234567";
    const OTHER_COMMIT: &str = "89abcdef0123456789abcdef0123456789abcdef";
    const TAG_OBJECT_SHA: &str = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

    #[derive(Default)]
    struct OfflineProvenanceTransport {
        responses: RefCell<BTreeMap<String, Vec<u8>>>,
        requests: RefCell<Vec<String>>,
    }

    impl OfflineProvenanceTransport {
        fn respond(&self, url: String, body: Vec<u8>) {
            self.responses.borrow_mut().insert(url, body);
        }

        fn request_count(&self, url: &str) -> usize {
            self.requests
                .borrow()
                .iter()
                .filter(|request| request.as_str() == url)
                .count()
        }
    }

    impl ProvenanceTransport for OfflineProvenanceTransport {
        fn fetch<'a>(&'a self, url: &'a str, max_bytes: usize) -> BoxFuture<'a, Result<Vec<u8>>> {
            Box::pin(async move {
                self.requests.borrow_mut().push(url.to_string());
                let bytes = self.responses.borrow().get(url).cloned().ok_or_else(|| {
                    Error::RegistryNotFound {
                        resource: url.to_string(),
                    }
                })?;
                if bytes.len() > max_bytes {
                    return Err(Error::RegistryResponseTooLarge {
                        resource: url.to_string(),
                        limit: max_bytes,
                    });
                }
                Ok(bytes)
            })
        }
    }

    enum AssetMutation {
        Exact,
        Remove(String),
        Add(String, u64),
        Resize(String, u64),
        ChangeState(String, String),
        Rename(String, String),
        ChangeDigest(String, Digest),
    }

    struct OfflineReleaseFixture {
        transport: OfflineProvenanceTransport,
        module: ModulePath,
        version: ExactVersion,
        manifest_raw: Vec<u8>,
    }

    fn poll_ready<T>(future: impl Future<Output = T>) -> T {
        let mut future = std::pin::pin!(future);
        let mut context = Context::from_waker(futures_util::task::noop_waker_ref());
        match future.as_mut().poll(&mut context) {
            Poll::Ready(value) => value,
            Poll::Pending => panic!("offline provenance future unexpectedly waited"),
        }
    }

    fn release_manifest_raw(commit: &str) -> Vec<u8> {
        ReleaseManifest {
            schema_version: 2,
            module: ModulePath::parse("github.com/acme/lib").unwrap(),
            version: ExactVersion::parse("1.2.3").unwrap(),
            commit: commit.to_string(),
            vo: vo_module::version::ToolchainConstraint::parse("^0.1.0").unwrap(),
            dependencies: Vec::new(),
            source: vo_module::schema::manifest::ManifestSource {
                name: "source.tar.gz".to_string(),
                size: 13,
                digest: Digest::from_sha256(b"source"),
            },
            package: vo_module::schema::manifest::ManifestPackage {
                size: 17,
                digest: Digest::from_sha256(b"package"),
            },
            artifacts: Vec::new(),
        }
        .render()
        .unwrap()
        .into_bytes()
    }

    fn git_object(kind: &str, sha: &str) -> serde_json::Value {
        serde_json::json!({ "type": kind, "sha": sha })
    }

    fn offline_release_fixture(
        manifest_raw: Vec<u8>,
        tag_target: serde_json::Value,
        annotated_tags: Vec<(String, String, serde_json::Value)>,
        asset_mutation: AssetMutation,
    ) -> OfflineReleaseFixture {
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let version = ExactVersion::parse("1.2.3").unwrap();
        let mut assets = vec![
            serde_json::json!({
                "name": "vo.release.json",
                "size": manifest_raw.len(),
                "state": "uploaded",
                "digest": Digest::from_sha256(&manifest_raw),
            }),
            serde_json::json!({
                "name": "vo.package.json",
                "size": 17,
                "state": "uploaded",
                "digest": Digest::from_sha256(b"package"),
            }),
            serde_json::json!({
                "name": "source.tar.gz",
                "size": 13,
                "state": "uploaded",
                "digest": Digest::from_sha256(b"source"),
            }),
        ];
        match asset_mutation {
            AssetMutation::Exact => {}
            AssetMutation::Remove(name) => {
                assets.retain(|asset| asset["name"].as_str() != Some(name.as_str()));
            }
            AssetMutation::Add(name, size) => assets.push(serde_json::json!({
                "digest": Digest::from_sha256(name.as_bytes()),
                "name": name,
                "size": size,
                "state": "uploaded"
            })),
            AssetMutation::Resize(name, size) => {
                assets
                    .iter_mut()
                    .find(|asset| asset["name"].as_str() == Some(name.as_str()))
                    .unwrap()["size"] = size.into();
            }
            AssetMutation::ChangeState(name, state) => {
                assets
                    .iter_mut()
                    .find(|asset| asset["name"].as_str() == Some(name.as_str()))
                    .unwrap()["state"] = state.into();
            }
            AssetMutation::Rename(name, renamed) => {
                assets
                    .iter_mut()
                    .find(|asset| asset["name"].as_str() == Some(name.as_str()))
                    .unwrap()["name"] = renamed.into();
            }
            AssetMutation::ChangeDigest(name, digest) => {
                assets
                    .iter_mut()
                    .find(|asset| asset["name"].as_str() == Some(name.as_str()))
                    .unwrap()["digest"] = digest.to_string().into();
            }
        }

        let transport = OfflineProvenanceTransport::default();
        transport.respond(
            github_release_metadata_url(&module, &version),
            serde_json::to_vec(&serde_json::json!({
                "tag_name": "v1.2.3",
                "draft": false,
                "immutable": true,
                "assets": assets
            }))
            .unwrap(),
        );
        transport.respond(
            vo_module::registry::release_download_url(&module, &version, "vo.release.json"),
            manifest_raw.clone(),
        );
        transport.respond(
            github_tag_ref_url(&module, &version),
            serde_json::to_vec(&serde_json::json!({
                "ref": "refs/tags/v1.2.3",
                "object": tag_target
            }))
            .unwrap(),
        );
        for (sha, tag, target) in annotated_tags {
            transport.respond(
                vo_module::github_provenance::github_annotated_tag_url(&module, &sha),
                serde_json::to_vec(&serde_json::json!({
                    "sha": sha,
                    "tag": tag,
                    "object": target
                }))
                .unwrap(),
            );
        }
        OfflineReleaseFixture {
            transport,
            module,
            version,
            manifest_raw,
        }
    }

    fn fetch_offline(fixture: &OfflineReleaseFixture) -> Result<Vec<u8>> {
        poll_ready(fetch_remote_release_manifest_raw(
            &fixture.transport,
            &fixture.module,
            &fixture.version,
        ))
    }

    fn release_with_source_bytes(source: &[u8]) -> (ModulePath, ExactVersion, ReleaseManifest) {
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let version = ExactVersion::parse("1.2.3").unwrap();
        let mut release =
            parse_manifest_raw(&module, &version, &release_manifest_raw(RELEASE_COMMIT)).unwrap();
        release.source.size = u64::try_from(source.len()).unwrap();
        release.source.digest = Digest::from_sha256(source);
        (module, version, release)
    }

    #[test]
    fn browser_remote_source_uses_one_authenticated_package_request() {
        let source = b"canonical source package bytes".to_vec();
        let (module, version, release) = release_with_source_bytes(&source);
        let url =
            vo_module::registry::release_download_url(&module, &version, &release.source.name);
        let transport = OfflineProvenanceTransport::default();
        transport.respond(url.clone(), source.clone());

        let payload = poll_ready(fetch_remote_source_payload(
            &transport, &module, &version, &release,
        ))
        .unwrap();

        match payload {
            SourcePayload::Package(bytes) => assert_eq!(bytes, source),
            SourcePayload::Files { .. } => panic!("remote source must use the package payload"),
        }
        assert_eq!(transport.request_count(&url), 1);
        assert_eq!(transport.requests.borrow().len(), 1);
    }

    #[test]
    fn browser_remote_source_rejects_bad_digest_and_oversized_declaration() {
        let expected = b"authenticated source";
        let (module, version, release) = release_with_source_bytes(expected);
        let url =
            vo_module::registry::release_download_url(&module, &version, &release.source.name);
        let transport = OfflineProvenanceTransport::default();
        let mut tampered = expected.to_vec();
        tampered[0] ^= 1;
        transport.respond(url, tampered);
        let error = poll_ready(fetch_remote_source_payload(
            &transport, &module, &version, &release,
        ))
        .unwrap_err();
        assert!(matches!(error, Error::DigestMismatch { .. }), "{error}");

        let mut oversized = release;
        oversized.source.size = vo_module::MAX_SOURCE_ARCHIVE_BYTES + 1;
        let no_fetch = OfflineProvenanceTransport::default();
        let error = poll_ready(fetch_remote_source_payload(
            &no_fetch, &module, &version, &oversized,
        ))
        .unwrap_err();
        assert!(error.to_string().contains("archive limit"), "{error}");
        assert!(no_fetch.requests.borrow().is_empty());
    }

    #[test]
    fn packaged_capability_source_reader_preserves_files_payload_and_modes() {
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let version = ExactVersion::parse("1.2.3").unwrap();
        let mod_bytes = b"module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n".to_vec();
        let tool_bytes = b"tool bytes".to_vec();
        let entries = vec![
            vo_module::schema::SourceFileEntry {
                path: "bin/tool".to_string(),
                mode: vo_module::schema::SourceFileMode::Executable,
                size: u64::try_from(tool_bytes.len()).unwrap(),
                digest: Digest::from_sha256(&tool_bytes),
            },
            vo_module::schema::SourceFileEntry {
                path: "vo.mod".to_string(),
                mode: vo_module::schema::SourceFileMode::Regular,
                size: u64::try_from(mod_bytes.len()).unwrap(),
                digest: Digest::from_sha256(&mod_bytes),
            },
        ];
        let package_raw = PackageManifest {
            schema_version: 1,
            files: entries,
        }
        .render()
        .unwrap();
        let mut release =
            parse_manifest_raw(&module, &version, &release_manifest_raw(RELEASE_COMMIT)).unwrap();
        release.package.size = u64::try_from(package_raw.len()).unwrap();
        release.package.digest = Digest::from_sha256(&package_raw);
        let files = BTreeMap::from([
            (PACKAGE_FILE.to_string(), package_raw.clone()),
            ("bin/tool".to_string(), tool_bytes.clone()),
            ("vo.mod".to_string(), mod_bytes.clone()),
        ]);
        let mut reads = Vec::new();

        let payload =
            packaged_source_payload_from_reader(&module, &version, &release, |path, max_bytes| {
                reads.push(path.to_string());
                let bytes = files
                    .get(path)
                    .cloned()
                    .ok_or_else(|| Error::RegistryNotFound {
                        resource: path.to_string(),
                    })?;
                if bytes.len() > max_bytes {
                    return Err(Error::RegistryResponseTooLarge {
                        resource: path.to_string(),
                        limit: max_bytes,
                    });
                }
                Ok(bytes)
            })
            .unwrap();

        match payload {
            SourcePayload::Files {
                source_files,
                package_raw: found_package,
            } => {
                assert_eq!(found_package, package_raw);
                assert_eq!(source_files.len(), 2);
                assert_eq!(source_files[0].path, PathBuf::from("bin/tool"));
                assert_eq!(
                    source_files[0].mode,
                    vo_module::schema::SourceFileMode::Executable,
                );
                assert_eq!(source_files[0].bytes, tool_bytes);
                assert_eq!(source_files[1].path, PathBuf::from("vo.mod"));
                assert_eq!(source_files[1].bytes, mod_bytes);
            }
            SourcePayload::Package(_) => panic!("packaged capability must retain files payload"),
        }
        assert_eq!(reads, [PACKAGE_FILE, "bin/tool", "vo.mod"]);
    }

    #[test]
    fn browser_source_package_memory_budget_matches_the_shared_installer() {
        let compressed = usize::try_from(vo_module::MAX_SOURCE_ARCHIVE_BYTES).unwrap();
        assert!(compressed <= vo_web_runtime_wasm::vfs::MAX_VFS_FILE_BYTES);
        assert!(compressed + vo_module::MAX_EXTRACTED_SOURCE_BYTES <= 192 * 1024 * 1024,);
    }

    #[test]
    fn browser_remote_provenance_accepts_lightweight_and_multilevel_annotated_tags() {
        let lightweight = offline_release_fixture(
            release_manifest_raw(RELEASE_COMMIT),
            git_object("commit", RELEASE_COMMIT),
            Vec::new(),
            AssetMutation::Exact,
        );
        assert_eq!(
            fetch_offline(&lightweight).unwrap(),
            lightweight.manifest_raw
        );
        for url in [
            github_release_metadata_url(&lightweight.module, &lightweight.version),
            vo_module::registry::release_download_url(
                &lightweight.module,
                &lightweight.version,
                "vo.release.json",
            ),
            github_tag_ref_url(&lightweight.module, &lightweight.version),
        ] {
            assert_eq!(lightweight.transport.request_count(&url), 1, "{url}");
        }

        let second = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
        let annotated = offline_release_fixture(
            release_manifest_raw(RELEASE_COMMIT),
            git_object("tag", TAG_OBJECT_SHA),
            vec![
                (
                    TAG_OBJECT_SHA.to_string(),
                    "v1.2.3".to_string(),
                    git_object("tag", second),
                ),
                (
                    second.to_string(),
                    "inner".to_string(),
                    git_object("commit", RELEASE_COMMIT),
                ),
            ],
            AssetMutation::Exact,
        );
        assert_eq!(fetch_offline(&annotated).unwrap(), annotated.manifest_raw);
    }

    #[test]
    fn browser_remote_provenance_rejects_tag_name_commit_cycle_and_depth_drift() {
        let commit_drift = offline_release_fixture(
            release_manifest_raw(RELEASE_COMMIT),
            git_object("commit", OTHER_COMMIT),
            Vec::new(),
            AssetMutation::Exact,
        );
        let error = fetch_offline(&commit_drift).unwrap_err();
        assert!(error.to_string().contains("resolves to commit"), "{error}");

        let wrong_name = offline_release_fixture(
            release_manifest_raw(RELEASE_COMMIT),
            git_object("tag", TAG_OBJECT_SHA),
            vec![(
                TAG_OBJECT_SHA.to_string(),
                "v9.9.9".to_string(),
                git_object("commit", RELEASE_COMMIT),
            )],
            AssetMutation::Exact,
        );
        let error = fetch_offline(&wrong_name).unwrap_err();
        assert!(error.to_string().contains("declares tag"), "{error}");

        let cycle = offline_release_fixture(
            release_manifest_raw(RELEASE_COMMIT),
            git_object("tag", TAG_OBJECT_SHA),
            vec![(
                TAG_OBJECT_SHA.to_string(),
                "v1.2.3".to_string(),
                git_object("tag", TAG_OBJECT_SHA),
            )],
            AssetMutation::Exact,
        );
        let error = fetch_offline(&cycle).unwrap_err();
        assert!(error.to_string().contains("contains a cycle"), "{error}");

        let mut annotated_tags = Vec::new();
        let mut current = format!("{0:040x}", 1);
        for index in 1..=vo_module::github_provenance::MAX_ANNOTATED_TAG_DEPTH {
            let next = format!("{0:040x}", index + 1);
            annotated_tags.push((
                current.clone(),
                if index == 1 { "v1.2.3" } else { "inner" }.to_string(),
                git_object("tag", &next),
            ));
            current = next;
        }
        let too_deep = offline_release_fixture(
            release_manifest_raw(RELEASE_COMMIT),
            git_object("tag", &format!("{0:040x}", 1)),
            annotated_tags,
            AssetMutation::Exact,
        );
        let error = fetch_offline(&too_deep).unwrap_err();
        assert!(error.to_string().contains("depth limit"), "{error}");
    }

    #[test]
    fn browser_remote_provenance_rejects_incomplete_or_noncanonical_asset_inventory() {
        let cases = [
            (
                AssetMutation::Remove("vo.package.json".to_string()),
                "missing",
            ),
            (AssetMutation::Add("notes.txt".to_string(), 1), "unexpected"),
            (
                AssetMutation::Resize("vo.package.json".to_string(), 18),
                "size mismatches",
            ),
            (
                AssetMutation::ChangeState("vo.package.json".to_string(), "starter".to_string()),
                "expected \"uploaded\"",
            ),
            (
                AssetMutation::Rename(
                    "vo.package.json".to_string(),
                    "../vo.package.json".to_string(),
                ),
                "non-canonical asset name",
            ),
            (
                AssetMutation::ChangeDigest(
                    "vo.package.json".to_string(),
                    Digest::from_sha256(b"tampered"),
                ),
                "digest mismatches",
            ),
        ];
        for (mutation, expected) in cases {
            let fixture = offline_release_fixture(
                release_manifest_raw(RELEASE_COMMIT),
                git_object("commit", RELEASE_COMMIT),
                Vec::new(),
                mutation,
            );
            let error = fetch_offline(&fixture).unwrap_err();
            assert!(error.to_string().contains(expected), "{error}");
        }
    }

    #[test]
    fn packaged_release_trust_root_stays_offline_and_identity_strict() {
        let fixture = offline_release_fixture(
            release_manifest_raw(RELEASE_COMMIT),
            git_object("commit", RELEASE_COMMIT),
            Vec::new(),
            AssetMutation::Exact,
        );
        let release_digest = Digest::from_sha256(&fixture.manifest_raw);
        validate_packaged_release_capability_raw(
            &fixture.module,
            &fixture.version,
            &release_digest,
            &fixture.manifest_raw,
        )
        .unwrap();
        assert!(validate_packaged_release_capability_raw(
            &fixture.module,
            &fixture.version,
            &Digest::from_sha256(b"forged release"),
            &fixture.manifest_raw,
        )
        .is_err());
        validate_immutable_packaged_release_cache(
            &fixture.module,
            &fixture.version,
            &fixture.manifest_raw,
            &fixture.manifest_raw,
        )
        .unwrap();
        let mut changed = fixture.manifest_raw.clone();
        changed.push(b'\n');
        assert!(validate_immutable_packaged_release_cache(
            &fixture.module,
            &fixture.version,
            &fixture.manifest_raw,
            &changed,
        )
        .is_err());
        assert!(fixture.transport.requests.borrow().is_empty());
        let other = ExactVersion::parse("1.2.4").unwrap();
        assert!(validate_packaged_release_capability_raw(
            &fixture.module,
            &other,
            &release_digest,
            &fixture.manifest_raw,
        )
        .is_err());
        assert!(fixture.transport.requests.borrow().is_empty());
    }

    #[test]
    fn github_release_page_is_stream_bounded_and_rejects_trailing_data() {
        let entries = (0..RELEASES_PER_PAGE)
            .map(|index| {
                serde_json::json!({
                    "tag_name": format!("v0.0.{index}"),
                    "draft": false,
                    "immutable": true,
                })
            })
            .collect::<Vec<_>>();
        let raw = serde_json::to_vec(&entries).unwrap();
        assert_eq!(
            parse_github_release_page(&raw, "test://releases")
                .unwrap()
                .len(),
            RELEASES_PER_PAGE,
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

        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        for raw in [
            br#"[{"tag_name":"v1.2.3","draft":false,"immutable":false}]"#.as_slice(),
            br#"[{"tag_name":"v1.2.3","draft":true,"immutable":true}]"#.as_slice(),
        ] {
            let release = &parse_github_release_page(raw, "test://releases").unwrap()[0];
            assert!(immutable_release_version(&module, release).is_none());
        }
        for raw in [
            br#"[{"tag_name":"v1.2.3","immutable":true}]"#.as_slice(),
            br#"[{"tag_name":"v1.2.3","draft":false}]"#.as_slice(),
        ] {
            let error = parse_github_release_page(raw, "test://releases").unwrap_err();
            assert!(error.to_string().contains("missing field"), "{error}");
        }
        let release = &parse_github_release_page(
            br#"[{"tag_name":"v1.2.3","draft":false,"immutable":true}]"#,
            "test://releases",
        )
        .unwrap()[0];
        assert_eq!(
            immutable_release_version(&module, release),
            Some(ExactVersion::parse("1.2.3").unwrap())
        );
    }

    #[test]
    fn release_manifest_transport_errors_are_candidate_local() {
        let module = ModulePath::parse("github.com/acme/pkg").unwrap();
        let version = ExactVersion::parse("0.2.0").unwrap();
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
    }

    #[test]
    fn parses_only_the_release_v2_identity_requested() {
        let module = ModulePath::parse("github.com/acme/pkg").unwrap();
        let version = ExactVersion::parse("0.2.0").unwrap();
        let package = br#"{"schema_version":1,"files":[]}"#;
        let raw = ReleaseManifest {
            schema_version: 2,
            module: module.clone(),
            version: version.clone(),
            commit: "0123456789abcdef0123456789abcdef01234567".to_string(),
            vo: vo_module::version::ToolchainConstraint::parse("^0.1.0").unwrap(),
            dependencies: Vec::new(),
            source: vo_module::schema::manifest::ManifestSource {
                name: "source.tar.gz".to_string(),
                size: 1,
                digest: Digest::from_sha256(b"x"),
            },
            package: vo_module::schema::manifest::ManifestPackage {
                size: u64::try_from(package.len()).unwrap(),
                digest: Digest::from_sha256(package),
            },
            artifacts: Vec::new(),
        }
        .render()
        .unwrap()
        .into_bytes();
        let parsed = parse_manifest_raw(&module, &version, &raw).unwrap();
        assert_eq!(parsed.module, module);
        assert_eq!(parsed.version, version);

        let other = ExactVersion::parse("0.2.1").unwrap();
        assert!(parse_manifest_raw(&module, &other, &raw).is_err());
    }

    #[test]
    fn preseeded_package_paths_have_no_authority_without_an_immutable_capability() {
        assert!(validate_package_relative_path("vo.mod").is_ok());
        assert!(validate_package_relative_path("assets/data.bin").is_ok());
        for reserved in [
            "vo.package.json",
            "vo.release.json",
            "artifacts/demo.wasm",
            ".vo-version",
            "vo.work",
        ] {
            assert!(
                validate_package_relative_path(reserved).is_err(),
                "{reserved}"
            );
        }

        let module = ModulePath::parse("github.com/acme/pkg").unwrap();
        let version = ExactVersion::parse("0.2.0").unwrap();
        PACKAGED_RELEASE_CAPABILITIES.with(|capabilities| capabilities.borrow_mut().clear());
        assert!(packaged_release_capability(&module, &version).is_none());
        assert_eq!(
            read_packaged_module_file(&module, &version, "vo.release.json", 1024).unwrap(),
            None,
        );

        let capability = PackagedReleaseCapability {
            root: "/trusted/releases/acme-pkg-0.2.0".to_string(),
            release_digest: Digest::from_sha256(b"release"),
        };
        insert_packaged_release_capability(&module, &version, capability.clone()).unwrap();
        assert_eq!(
            packaged_release_capability(&module, &version),
            Some(capability.clone()),
        );
        assert_eq!(
            packaged_file_path_at_root(&capability.root, PACKAGE_FILE).unwrap(),
            "/trusted/releases/acme-pkg-0.2.0/vo.package.json",
        );
        let missing = read_packaged_module_file(&module, &version, PACKAGE_FILE, 1024)
            .expect_err("a bound release tree must stay closed when a packaged file is missing");
        #[cfg(target_arch = "wasm32")]
        assert!(matches!(missing, Error::RegistryNotFound { .. }));
        #[cfg(not(target_arch = "wasm32"))]
        assert!(matches!(
            missing,
            Error::RegistryError(message)
                if message.contains("cannot be read on a non-WASM target")
        ));
        insert_packaged_release_capability(&module, &version, capability.clone()).unwrap();

        let replacement = PackagedReleaseCapability {
            root: capability.root.clone(),
            release_digest: Digest::from_sha256(b"replacement"),
        };
        assert!(insert_packaged_release_capability(&module, &version, replacement).is_err());

        let other_module = ModulePath::parse("github.com/acme/other").unwrap();
        assert!(insert_packaged_release_capability(
            &other_module,
            &version,
            PackagedReleaseCapability {
                root: capability.root,
                release_digest: Digest::from_sha256(b"other"),
            },
        )
        .is_err());
        PACKAGED_RELEASE_CAPABILITIES.with(|capabilities| capabilities.borrow_mut().clear());
    }

    #[test]
    fn packaged_release_capability_batches_publish_all_or_none() {
        PACKAGED_RELEASE_CAPABILITIES.with(|capabilities| capabilities.borrow_mut().clear());
        let version = ExactVersion::parse("0.2.0").unwrap();
        let seeded_module = ModulePath::parse("github.com/acme/seeded").unwrap();
        let seeded = PackagedReleaseCapability {
            root: "/trusted/releases/seeded-0.2.0".to_string(),
            release_digest: Digest::from_sha256(b"seeded"),
        };
        insert_packaged_release_capability(&seeded_module, &version, seeded.clone()).unwrap();
        let before =
            PACKAGED_RELEASE_CAPABILITIES.with(|capabilities| capabilities.borrow().clone());

        let first_module = ModulePath::parse("github.com/acme/first").unwrap();
        let conflicting_module = ModulePath::parse("github.com/acme/conflict").unwrap();
        let rejected = vec![
            ValidatedPackagedReleaseCapability {
                module: first_module.clone(),
                version: version.clone(),
                capability: PackagedReleaseCapability {
                    root: "/trusted/releases/first-0.2.0".to_string(),
                    release_digest: Digest::from_sha256(b"first"),
                },
            },
            ValidatedPackagedReleaseCapability {
                module: conflicting_module,
                version: version.clone(),
                capability: PackagedReleaseCapability {
                    root: seeded.root.clone(),
                    release_digest: Digest::from_sha256(b"conflict"),
                },
            },
        ];
        assert!(insert_packaged_release_capabilities_atomically(&rejected).is_err());
        PACKAGED_RELEASE_CAPABILITIES.with(|capabilities| {
            assert_eq!(*capabilities.borrow(), before);
        });

        let second_module = ModulePath::parse("github.com/acme/second").unwrap();
        let accepted = vec![
            rejected[0].clone(),
            ValidatedPackagedReleaseCapability {
                module: second_module.clone(),
                version: version.clone(),
                capability: PackagedReleaseCapability {
                    root: "/trusted/releases/second-0.2.0".to_string(),
                    release_digest: Digest::from_sha256(b"second"),
                },
            },
        ];
        insert_packaged_release_capabilities_atomically(&accepted).unwrap();
        assert!(packaged_release_capability(&first_module, &version).is_some());
        assert!(packaged_release_capability(&second_module, &version).is_some());
        assert_eq!(
            PACKAGED_RELEASE_CAPABILITIES.with(|capabilities| capabilities.borrow().len()),
            before.len() + 2,
        );
        PACKAGED_RELEASE_CAPABILITIES.with(|capabilities| capabilities.borrow_mut().clear());
    }

    #[test]
    fn packaged_release_capability_roots_are_canonical_and_confined_by_construction() {
        assert_eq!(
            canonical_packaged_release_root("/trusted/releases/pkg").unwrap(),
            "/trusted/releases/pkg",
        );
        for invalid in [
            "",
            "/",
            "relative",
            "/trusted/../escape",
            "/trusted//pkg",
            "/trusted/pkg/",
            "/trusted\\pkg",
        ] {
            assert!(
                canonical_packaged_release_root(invalid).is_err(),
                "{invalid}"
            );
        }
        assert!(packaged_file_path_at_root("/trusted/releases/pkg", "../escape").is_err());
    }

    #[test]
    fn registry_urls_keep_git_tags_for_nested_module_assets() {
        let module = ModulePath::parse("github.com/acme/mono/graphics/v2").unwrap();
        let version = ExactVersion::parse("2.1.0").unwrap();
        assert_eq!(
            vo_module::registry::release_download_url(&module, &version, PACKAGE_FILE),
            "https://github.com/acme/mono/releases/download/graphics/v2/v2.1.0/vo.package.json",
        );
        assert_eq!(
            vo_module::registry::release_download_url(&module, &version, "source.tar.gz"),
            "https://github.com/acme/mono/releases/download/graphics/v2/v2.1.0/source.tar.gz",
        );
    }
}
