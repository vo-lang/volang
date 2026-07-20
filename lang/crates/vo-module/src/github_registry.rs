use std::collections::HashMap;
use std::io::Read;
use std::sync::{Arc, Mutex, MutexGuard};
use std::time::Duration;

use serde::Deserialize;

use crate::github_provenance::{
    github_provenance_fetch_error, github_release_metadata_url, parse_github_release_provenance,
    GitHubReleaseProvenance, GITHUB_API_VERSION, MAX_GITHUB_API_RESPONSE_BYTES,
};
use crate::identity::ModulePath;
use crate::registry::{
    parse_manifest_bytes, release_download_url, repository_id, version_from_tag, Registry,
};
use crate::version::ExactVersion;
use crate::Error;

const RELEASES_PER_PAGE: usize = 100;
const MAX_RELEASE_PAGES: usize = (crate::MAX_REGISTRY_RELEASES - 1) / RELEASES_PER_PAGE + 1;
const MAX_REGISTRY_CACHE_ENTRIES: usize = 4_096;
const MAX_VERSION_CACHE_CANDIDATES: usize = crate::MAX_SOLVER_CANDIDATES;
const MAX_MANIFEST_CACHE_BYTES: usize = crate::MAX_SOLVER_MANIFEST_BYTES;
const HTTP_CONNECT_TIMEOUT: Duration = Duration::from_secs(15);
const HTTP_IO_TIMEOUT: Duration = Duration::from_secs(30);
const HTTP_REQUEST_TIMEOUT: Duration = Duration::from_secs(5 * 60);

#[derive(Default)]
struct VersionCache {
    entries: HashMap<String, Vec<ExactVersion>>,
    candidates: usize,
}

impl VersionCache {
    fn insert_with_limits(
        &mut self,
        key: String,
        versions: Vec<ExactVersion>,
        max_entries: usize,
        max_candidates: usize,
    ) -> bool {
        if self.entries.contains_key(&key) || self.entries.len() >= max_entries {
            return false;
        }
        let Some(next_candidates) = self.candidates.checked_add(versions.len()) else {
            return false;
        };
        if next_candidates > max_candidates {
            return false;
        }
        self.entries.insert(key, versions);
        self.candidates = next_candidates;
        true
    }
}

#[derive(Default)]
struct ManifestCache {
    entries: HashMap<(String, String), Vec<u8>>,
    bytes: usize,
}

impl ManifestCache {
    fn insert_with_limits(
        &mut self,
        key: (String, String),
        raw: Vec<u8>,
        max_entries: usize,
        max_bytes: usize,
    ) -> bool {
        if self.entries.contains_key(&key) || self.entries.len() >= max_entries {
            return false;
        }
        let Some(next_bytes) = self.bytes.checked_add(raw.len()) else {
            return false;
        };
        if next_bytes > max_bytes {
            return false;
        }
        self.entries.insert(key, raw);
        self.bytes = next_bytes;
        true
    }
}

fn lock_unpoisoned<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

fn read_response_limited(
    mut reader: impl Read,
    max_bytes: usize,
    url: &str,
) -> Result<Vec<u8>, Error> {
    let mut bytes = Vec::new();
    let mut buffer = [0u8; 8 * 1024];
    loop {
        let remaining = max_bytes.saturating_sub(bytes.len());
        let limit = buffer.len().min(remaining.saturating_add(1));
        let count = match reader.read(&mut buffer[..limit]) {
            Ok(count) => count,
            Err(error) if error.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(error) => {
                return Err(Error::RegistryError(format!(
                    "reading response from {url}: {error}"
                )));
            }
        };
        if count == 0 {
            return Ok(bytes);
        }
        if count > remaining {
            return Err(Error::RegistryResponseTooLarge {
                resource: url.to_string(),
                limit: max_bytes,
            });
        }
        bytes.try_reserve(count).map_err(|_| {
            Error::RegistryError(format!("failed to reserve response buffer for {url}"))
        })?;
        bytes.extend_from_slice(&buffer[..count]);
    }
}

trait GitHubTransport: Send + Sync {
    fn fetch_bytes(&self, url: &str, max_bytes: usize) -> Result<Vec<u8>, Error>;
}

struct HttpGitHubTransport {
    /// Optional GitHub personal access token for authenticated API requests.
    /// Resolves rate-limit issues and enables access to private repositories.
    token: Option<String>,
    agent: ureq::Agent,
}

impl GitHubTransport for HttpGitHubTransport {
    fn fetch_bytes(&self, url: &str, max_bytes: usize) -> Result<Vec<u8>, Error> {
        let mut request = self.agent.get(url).set("User-Agent", "vo-module");
        if url.starts_with("https://api.github.com/") {
            request = request
                .set("Accept", "application/vnd.github+json")
                .set("X-GitHub-Api-Version", GITHUB_API_VERSION);
        }
        if let Some(ref token) = self.token {
            request = request.set("Authorization", &format!("Bearer {}", token));
        }
        let response = match request.call() {
            Ok(response) => response,
            Err(ureq::Error::Status(code, response)) => {
                return Err(http_status_error(url, code, response.status_text()));
            }
            Err(ureq::Error::Transport(error)) => {
                return Err(Error::RegistryError(format!("HTTP GET {url}: {error}")));
            }
        };
        if let Some(content_length) = response.header("Content-Length") {
            let content_length = content_length.parse::<u64>().map_err(|error| {
                Error::RegistryError(format!(
                    "invalid Content-Length from {url}: {content_length:?}: {error}"
                ))
            })?;
            if content_length > u64::try_from(max_bytes).unwrap_or(u64::MAX) {
                return Err(Error::RegistryResponseTooLarge {
                    resource: url.to_string(),
                    limit: max_bytes,
                });
            }
        }
        read_response_limited(response.into_reader(), max_bytes, url)
    }
}

/// Concrete `Registry` implementation backed by the GitHub Releases API.
/// Caches version lists and provenance-verified manifests for the lifetime of
/// the struct.
pub struct GitHubRegistry {
    transport: Arc<dyn GitHubTransport>,
    version_cache: Mutex<VersionCache>,
    manifest_cache: Mutex<ManifestCache>,
}

#[derive(Debug, Deserialize)]
struct GitHubReleaseEntry {
    tag_name: String,
    draft: bool,
    immutable: bool,
}

fn parse_github_release_page(
    body: &[u8],
    module: &ModulePath,
    page: usize,
) -> Result<Vec<GitHubReleaseEntry>, Error> {
    let mut deserializer = serde_json::Deserializer::from_slice(body);
    let releases = crate::schema::deserialize_bounded_vec(
        &mut deserializer,
        RELEASES_PER_PAGE,
        "GitHub release page",
    )
    .map_err(|error| {
        Error::RegistryError(format!(
            "invalid GitHub releases response page {page} for {module}: {error}"
        ))
    })?;
    deserializer.end().map_err(|error| {
        Error::RegistryError(format!(
            "invalid trailing data in GitHub releases response page {page} for {module}: {error}"
        ))
    })?;
    Ok(releases)
}

impl Default for GitHubRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl GitHubRegistry {
    /// Create a new `GitHubRegistry`, automatically resolving a GitHub token
    /// from the environment.
    ///
    /// Token resolution order:
    /// 1. `VO_GITHUB_TOKEN` environment variable
    /// 2. `GITHUB_TOKEN` environment variable
    /// 3. No token (anonymous access)
    pub fn new() -> Self {
        let token = std::env::var("VO_GITHUB_TOKEN")
            .or_else(|_| std::env::var("GITHUB_TOKEN"))
            .ok()
            .filter(|t| !t.is_empty());
        Self::with_transport(Arc::new(HttpGitHubTransport {
            token,
            agent: ureq::AgentBuilder::new()
                .timeout_connect(HTTP_CONNECT_TIMEOUT)
                .timeout_read(HTTP_IO_TIMEOUT)
                .timeout_write(HTTP_IO_TIMEOUT)
                .timeout(HTTP_REQUEST_TIMEOUT)
                .build(),
        }))
    }

    fn with_transport(transport: Arc<dyn GitHubTransport>) -> Self {
        Self {
            transport,
            version_cache: Mutex::new(VersionCache::default()),
            manifest_cache: Mutex::new(ManifestCache::default()),
        }
    }

    fn fetch_versions_uncached(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        let rid = repository_id(module);
        let mut page = 1usize;
        let mut versions = Vec::new();
        let mut processed_listing_bytes = 0usize;
        loop {
            let url = format!(
                "https://api.github.com/repos/{}/{}/releases?per_page={}&page={}",
                rid.owner, rid.repo, RELEASES_PER_PAGE, page,
            );
            let body = self.fetch_bytes(&url, MAX_GITHUB_API_RESPONSE_BYTES)?;
            processed_listing_bytes = crate::registry::charge_registry_listing_bytes(
                processed_listing_bytes,
                body.len(),
            )?;
            let releases = parse_github_release_page(&body, module, page)?;
            let release_count = releases.len();
            if release_count > RELEASES_PER_PAGE {
                return Err(Error::RegistryError(format!(
                    "GitHub release page {page} for {module} contains {release_count} entries, exceeding the requested {RELEASES_PER_PAGE}-entry page size"
                )));
            }
            if page > MAX_RELEASE_PAGES {
                if releases.is_empty() {
                    break;
                }
                return Err(Error::RegistryError(format!(
                    "GitHub release listing for {module} exceeds {MAX_RELEASE_PAGES} pages"
                )));
            }
            for entry in &releases {
                if entry.draft || !entry.immutable {
                    continue;
                }
                if let Some(ev) = version_from_tag(module, &entry.tag_name) {
                    if module.accepts_version(&ev) {
                        versions.push(ev);
                    }
                }
            }
            if release_count < RELEASES_PER_PAGE {
                break;
            }
            page = page.checked_add(1).ok_or_else(|| {
                Error::RegistryError(format!("GitHub release page overflow for {module}"))
            })?;
        }
        Ok(versions)
    }

    fn fetch_manifest_uncached(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<Vec<u8>, Error> {
        let release = self.fetch_release_metadata(module, version)?;
        release.require_release_manifest_asset(module, version)?;

        let url = release_download_url(module, version, "vo.release.json");
        // Preserve downloaded bytes as the sole authority. Callers charge the
        // solve/probe byte budget before parsing and identity validation.
        let raw = self
            .fetch_bytes(&url, vo_common::vfs::MAX_TEXT_FILE_BYTES)
            .map_err(|error| release_manifest_fetch_error(module, version, error))?;
        let manifest = parse_manifest_bytes(&raw, module, version)?;
        release.validate_assets(module, version, &manifest, &raw)?;
        Ok(raw)
    }

    fn fetch_bytes(&self, url: &str, max_bytes: usize) -> Result<Vec<u8>, Error> {
        self.transport.fetch_bytes(url, max_bytes)
    }

    fn fetch_release_metadata(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<GitHubReleaseProvenance, Error> {
        let url = github_release_metadata_url(module, version);
        let body = self
            .fetch_bytes(&url, MAX_GITHUB_API_RESPONSE_BYTES)
            .map_err(|error| {
                github_provenance_fetch_error(module, version, "release metadata", error)
            })?;
        parse_github_release_provenance(&body, module, version)
    }
}

fn http_status_error(url: &str, code: u16, status_text: &str) -> Error {
    if code == 404 {
        Error::RegistryNotFound {
            resource: url.to_string(),
        }
    } else {
        Error::RegistryError(format!("HTTP GET {url}: {code} {status_text}"))
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

impl Registry for GitHubRegistry {
    fn list_version_candidates(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        require_github_module(module)?;
        let key = module.as_str().to_string();
        if let Some(cached) = lock_unpoisoned(&self.version_cache).entries.get(&key) {
            return Ok(cached.clone());
        }
        let versions = self.fetch_versions_uncached(module)?;
        let mut cache = lock_unpoisoned(&self.version_cache);
        cache.insert_with_limits(
            key,
            versions.clone(),
            MAX_REGISTRY_CACHE_ENTRIES,
            MAX_VERSION_CACHE_CANDIDATES,
        );
        Ok(versions)
    }

    fn fetch_manifest_raw(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<Vec<u8>, Error> {
        require_github_module(module)?;
        let key = (module.as_str().to_string(), version.to_string());
        if let Some(cached) = lock_unpoisoned(&self.manifest_cache).entries.get(&key) {
            return Ok(cached.clone());
        }
        let result = self.fetch_manifest_uncached(module, version)?;
        let mut cache = lock_unpoisoned(&self.manifest_cache);
        cache.insert_with_limits(
            key,
            result.clone(),
            MAX_REGISTRY_CACHE_ENTRIES,
            MAX_MANIFEST_CACHE_BYTES,
        );
        Ok(result)
    }

    fn fetch_source_package(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        require_github_module(module)?;
        let manifest_raw = self.fetch_manifest_raw(module, version)?;
        let manifest = parse_manifest_bytes(&manifest_raw, module, version)?;
        if asset_name != manifest.source.name {
            return Err(Error::InvalidReleaseMetadata(format!(
                "source asset request {asset_name:?} for {module} {version} does not match authenticated vo.release.json asset {:?}",
                manifest.source.name
            )));
        }
        let url = release_download_url(module, version, asset_name);
        self.fetch_bytes(
            &url,
            usize::try_from(crate::MAX_SOURCE_ARCHIVE_BYTES).unwrap_or(usize::MAX),
        )
    }

    fn fetch_artifact(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        artifact: &crate::identity::ArtifactId,
    ) -> Result<Vec<u8>, Error> {
        require_github_module(module)?;
        let manifest_raw = self.fetch_manifest_raw(module, version)?;
        let manifest = parse_manifest_bytes(&manifest_raw, module, version)?;
        if !manifest
            .artifacts
            .iter()
            .any(|declared| declared.id == *artifact)
        {
            return Err(Error::InvalidReleaseMetadata(format!(
                "artifact request {artifact} for {module} {version} is absent from authenticated vo.release.json"
            )));
        }
        let asset_name = crate::artifact::artifact_release_asset_name(artifact)
            .map_err(Error::InvalidReleaseMetadata)?;
        let url = release_download_url(module, version, &asset_name);
        self.fetch_bytes(
            &url,
            usize::try_from(crate::MAX_MODULE_ARTIFACT_BYTES).unwrap_or(usize::MAX),
        )
    }
}

fn require_github_module(module: &ModulePath) -> Result<(), Error> {
    if module.host() == "github.com" && !module.is_local() {
        return Ok(());
    }
    Err(Error::RegistryNotFound {
        resource: format!(
            "GitHub registry does not route ModuleId {module}; configure a registry for host {}",
            module.host()
        ),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::{canonical_release_assets, release_tag};
    use crate::schema::manifest::ReleaseManifest;
    use std::sync::Arc;

    const RELEASE_COMMIT: &str = "0123456789abcdef0123456789abcdef01234567";

    #[derive(Default)]
    struct MockGitHubTransport {
        responses: Mutex<HashMap<String, Vec<u8>>>,
        requests: Mutex<Vec<String>>,
    }

    impl MockGitHubTransport {
        fn respond(&self, url: String, body: Vec<u8>) {
            lock_unpoisoned(&self.responses).insert(url, body);
        }

        fn request_count(&self, url: &str) -> usize {
            lock_unpoisoned(&self.requests)
                .iter()
                .filter(|request| request.as_str() == url)
                .count()
        }
    }

    impl GitHubTransport for MockGitHubTransport {
        fn fetch_bytes(&self, url: &str, max_bytes: usize) -> Result<Vec<u8>, Error> {
            lock_unpoisoned(&self.requests).push(url.to_string());
            let body = lock_unpoisoned(&self.responses)
                .get(url)
                .cloned()
                .ok_or_else(|| Error::RegistryError(format!("unmocked GitHub request {url}")))?;
            if body.len() > max_bytes {
                return Err(Error::RegistryResponseTooLarge {
                    resource: url.to_string(),
                    limit: max_bytes,
                });
            }
            Ok(body)
        }
    }

    enum AssetMutation {
        Exact,
        Remove(String),
        Add(String, u64),
        Resize(String, u64),
        ChangeState(String, String),
        ChangeDigest(String, crate::digest::Digest),
    }

    struct OfflineReleaseFixture {
        registry: GitHubRegistry,
        transport: Arc<MockGitHubTransport>,
        module: ModulePath,
        version: ExactVersion,
        manifest_raw: Vec<u8>,
        artifact: crate::identity::ArtifactId,
    }

    fn release_manifest_raw(_commit: &str) -> Vec<u8> {
        ReleaseManifest {
            format: 1,
            module: ModulePath::parse("github.com/acme/lib").unwrap(),
            version: ExactVersion::parse("1.2.3").unwrap(),
            vo: crate::version::ToolchainConstraint::parse("0.1.0").unwrap(),
            intent: crate::digest::Digest::from_sha256(b"github-registry-fixture-intent"),
            dependencies: Vec::new(),
            source: crate::schema::manifest::ManifestSource {
                name: "source.tar.gz".to_string(),
                size: 13,
                digest: crate::digest::Digest::parse(
                    "sha256:1111111111111111111111111111111111111111111111111111111111111111",
                )
                .unwrap(),
                tree: crate::digest::Digest::parse(
                    "sha256:2222222222222222222222222222222222222222222222222222222222222222",
                )
                .unwrap(),
            },
            artifacts: vec![crate::schema::manifest::ManifestArtifact {
                id: crate::identity::ArtifactId {
                    kind: "extension-native".to_string(),
                    target: "aarch64-apple-darwin".to_string(),
                    name: "libdemo.dylib".to_string(),
                },
                size: 19,
                digest: crate::digest::Digest::parse(
                    "sha256:3333333333333333333333333333333333333333333333333333333333333333",
                )
                .unwrap(),
            }],
        }
        .render()
        .unwrap()
        .into_bytes()
    }

    fn offline_release_fixture(
        manifest_raw: Vec<u8>,
        _tag_target: serde_json::Value,
        _annotated_tags: Vec<(String, serde_json::Value)>,
        asset_mutation: AssetMutation,
    ) -> OfflineReleaseFixture {
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let version = ExactVersion::parse("1.2.3").unwrap();
        let manifest = parse_manifest_bytes(&manifest_raw, &module, &version).unwrap();
        let artifact = manifest.artifacts[0].id.clone();
        let mut assets = canonical_release_assets(&manifest, &manifest_raw)
            .unwrap()
            .into_iter()
            .map(|(name, asset)| {
                serde_json::json!({
                    "name": name,
                    "size": asset.size,
                    "state": "uploaded",
                    "digest": asset.digest,
                })
            })
            .collect::<Vec<_>>();
        match asset_mutation {
            AssetMutation::Exact => {}
            AssetMutation::Remove(name) => {
                assets.retain(|asset| asset["name"].as_str() != Some(name.as_str()));
            }
            AssetMutation::Add(name, size) => {
                let digest = crate::digest::Digest::from_sha256(name.as_bytes());
                assets.push(serde_json::json!({
                    "name": name,
                    "size": size,
                    "state": "uploaded",
                    "digest": digest,
                }));
            }
            AssetMutation::Resize(name, size) => {
                let asset = assets
                    .iter_mut()
                    .find(|asset| asset["name"].as_str() == Some(name.as_str()))
                    .unwrap();
                asset["size"] = size.into();
            }
            AssetMutation::ChangeState(name, state) => {
                let asset = assets
                    .iter_mut()
                    .find(|asset| asset["name"].as_str() == Some(name.as_str()))
                    .unwrap();
                asset["state"] = state.into();
            }
            AssetMutation::ChangeDigest(name, digest) => {
                let asset = assets
                    .iter_mut()
                    .find(|asset| asset["name"].as_str() == Some(name.as_str()))
                    .unwrap();
                asset["digest"] = digest.to_string().into();
            }
        }

        let transport = Arc::new(MockGitHubTransport::default());
        transport.respond(
            github_release_metadata_url(&module, &version),
            serde_json::to_vec(&serde_json::json!({
                "tag_name": release_tag(&module, &version),
                "draft": false,
                "immutable": true,
                "assets": assets
            }))
            .unwrap(),
        );
        transport.respond(
            release_download_url(&module, &version, "vo.release.json"),
            manifest_raw.clone(),
        );
        let registry = GitHubRegistry::with_transport(transport.clone());
        OfflineReleaseFixture {
            registry,
            transport,
            module,
            version,
            manifest_raw,
            artifact,
        }
    }

    fn lightweight_target(commit: &str) -> serde_json::Value {
        serde_json::json!({ "type": "commit", "sha": commit })
    }

    #[test]
    fn github_registry_accepts_exact_assets_and_caches_verification() {
        let fixture = offline_release_fixture(
            release_manifest_raw(RELEASE_COMMIT),
            lightweight_target(RELEASE_COMMIT),
            Vec::new(),
            AssetMutation::Exact,
        );
        assert_eq!(
            fixture
                .registry
                .fetch_manifest_raw(&fixture.module, &fixture.version)
                .unwrap(),
            fixture.manifest_raw
        );
        assert_eq!(
            fixture
                .registry
                .fetch_manifest_raw(&fixture.module, &fixture.version)
                .unwrap(),
            fixture.manifest_raw
        );
        for url in [
            github_release_metadata_url(&fixture.module, &fixture.version),
            release_download_url(&fixture.module, &fixture.version, "vo.release.json"),
        ] {
            assert_eq!(fixture.transport.request_count(&url), 1, "{url}");
        }
    }

    #[test]
    fn github_registry_requires_the_complete_exact_release_asset_inventory() {
        let raw = release_manifest_raw(RELEASE_COMMIT);
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let version = ExactVersion::parse("1.2.3").unwrap();
        let manifest = parse_manifest_bytes(&raw, &module, &version).unwrap();
        let artifact_asset =
            crate::artifact::artifact_release_asset_name(&manifest.artifacts[0].id).unwrap();
        let cases = [
            (
                AssetMutation::Remove("vo.release.json".to_string()),
                "missing canonical asset vo.release.json",
            ),
            (
                AssetMutation::Remove(manifest.source.name.clone()),
                "missing [source.tar.gz]",
            ),
            (
                AssetMutation::Remove(artifact_asset.clone()),
                "missing [vo-artifact-v1-",
            ),
            (
                AssetMutation::Add(
                    "vo-artifact-v1-ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                        .to_string(),
                    7,
                ),
                "unexpected [vo-artifact-v1-",
            ),
            (
                AssetMutation::Add("notes.txt".to_string(), 5),
                "unexpected [notes.txt]",
            ),
            (
                AssetMutation::Resize("source.tar.gz".to_string(), 14),
                "size mismatches [\"source.tar.gz\": expected 13 bytes, found 14 bytes]",
            ),
            (
                AssetMutation::ChangeState(
                    "source.tar.gz".to_string(),
                    "starter".to_string(),
                ),
                "has state \"starter\", expected \"uploaded\"",
            ),
            (
                AssetMutation::ChangeDigest(
                    "source.tar.gz".to_string(),
                    crate::digest::Digest::from_sha256(b"tampered"),
                ),
                "digest mismatches",
            ),
        ];
        for (mutation, expected) in cases {
            let fixture = offline_release_fixture(
                raw.clone(),
                lightweight_target(RELEASE_COMMIT),
                Vec::new(),
                mutation,
            );
            let error = fixture
                .registry
                .fetch_manifest_raw(&fixture.module, &fixture.version)
                .unwrap_err();
            assert!(matches!(error, Error::InvalidReleaseMetadata(_)));
            assert!(error.to_string().contains(expected), "{error}");
        }
    }

    #[test]
    fn github_registry_rejects_payload_requests_outside_authenticated_manifest() {
        let fixture = offline_release_fixture(
            release_manifest_raw(RELEASE_COMMIT),
            lightweight_target(RELEASE_COMMIT),
            Vec::new(),
            AssetMutation::Exact,
        );
        let source_error = fixture
            .registry
            .fetch_source_package(&fixture.module, &fixture.version, "other.tar.gz")
            .unwrap_err();
        assert!(
            source_error
                .to_string()
                .contains("does not match authenticated vo.release.json"),
            "{source_error}"
        );

        let undeclared = crate::identity::ArtifactId {
            name: "other.dylib".to_string(),
            ..fixture.artifact
        };
        let artifact_error = fixture
            .registry
            .fetch_artifact(&fixture.module, &fixture.version, &undeclared)
            .unwrap_err();
        assert!(
            artifact_error
                .to_string()
                .contains("absent from authenticated vo.release.json"),
            "{artifact_error}"
        );
    }

    #[test]
    fn github_release_pages_are_bounded_during_deserialization() {
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let body = serde_json::to_vec(
            &(0..=RELEASES_PER_PAGE)
                .map(|index| {
                    serde_json::json!({
                        "tag_name": format!("v1.0.{index}"),
                        "draft": false,
                        "immutable": true,
                    })
                })
                .collect::<Vec<_>>(),
        )
        .unwrap();
        let error = parse_github_release_page(&body, &module, 1).unwrap_err();
        assert!(error.to_string().contains("more than 100"), "{error}");

        let error = parse_github_release_page(b"[] trailing", &module, 1).unwrap_err();
        assert!(error.to_string().contains("trailing"), "{error}");
    }

    #[test]
    fn github_registry_lists_only_published_immutable_releases() {
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let url = format!(
            "https://api.github.com/repos/acme/lib/releases?per_page={RELEASES_PER_PAGE}&page=1"
        );
        let transport = Arc::new(MockGitHubTransport::default());
        transport.respond(
            url,
            serde_json::to_vec(&serde_json::json!([
                {"tag_name": "v1.2.3", "draft": false, "immutable": true},
                {"tag_name": "v1.2.2", "draft": false, "immutable": false},
                {"tag_name": "v1.2.0", "draft": true, "immutable": true}
            ]))
            .unwrap(),
        );
        let registry = GitHubRegistry::with_transport(transport);
        assert_eq!(
            registry.list_version_candidates(&module).unwrap(),
            vec![ExactVersion::parse("1.2.3").unwrap()]
        );

        for body in [
            br#"[{"tag_name":"v1.2.3","immutable":true}]"#.as_slice(),
            br#"[{"tag_name":"v1.2.3","draft":false}]"#.as_slice(),
        ] {
            let error = parse_github_release_page(body, &module, 1).unwrap_err();
            assert!(error.to_string().contains("missing field"), "{error}");
        }
    }

    #[test]
    fn test_version_from_tag_root() {
        let mp = ModulePath::parse("github.com/acme/lib").unwrap();
        let v = version_from_tag(&mp, "v1.4.2").unwrap();
        assert_eq!(v.to_string(), "1.4.2");
    }

    #[test]
    fn test_version_from_tag_nested() {
        let mp = ModulePath::parse("github.com/acme/mono/graphics/v2").unwrap();
        let v = version_from_tag(&mp, "graphics/v2/v2.1.0").unwrap();
        assert_eq!(v.to_string(), "2.1.0");
    }

    #[test]
    fn test_version_from_tag_mismatch() {
        let mp = ModulePath::parse("github.com/acme/mono/graphics").unwrap();
        assert!(version_from_tag(&mp, "v0.8.0").is_none());
    }

    #[test]
    fn test_release_download_url() {
        let mp = ModulePath::parse("github.com/vo-lang/vogui").unwrap();
        let v = ExactVersion::parse("0.4.2").unwrap();
        assert_eq!(
            release_download_url(&mp, &v, "vo.release.json"),
            "https://github.com/vo-lang/vogui/releases/download/v0.4.2/vo.release.json"
        );
    }

    #[test]
    fn test_release_download_url_nested() {
        let mp = ModulePath::parse("github.com/acme/mono/graphics/v2").unwrap();
        let v = ExactVersion::parse("2.1.0").unwrap();
        assert_eq!(
            release_download_url(&mp, &v, "source.tar.gz"),
            "https://github.com/acme/mono/releases/download/graphics/v2/v2.1.0/source.tar.gz"
        );
    }

    #[test]
    fn response_reader_rejects_oversized_bodies_before_retaining_the_extra_byte() {
        let error =
            read_response_limited(std::io::Cursor::new(vec![0; 5]), 4, "test://body").unwrap_err();
        assert!(error.to_string().contains("4-byte limit"));
        assert_eq!(
            read_response_limited(std::io::Cursor::new(vec![1; 4]), 4, "test://body").unwrap(),
            vec![1; 4]
        );
    }

    #[test]
    fn only_http_not_found_receives_typed_absence_semantics() {
        assert!(matches!(
            http_status_error("https://example.invalid/missing", 404, "Not Found"),
            Error::RegistryNotFound { .. }
        ));
        for code in [401, 403, 429, 500, 503] {
            assert!(matches!(
                http_status_error("https://example.invalid/failure", code, "failure"),
                Error::RegistryError(_)
            ));
        }
    }

    #[test]
    fn only_manifest_absence_and_oversize_become_invalid_releases() {
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let version = ExactVersion::parse("1.2.3").unwrap();
        for error in [
            Error::RegistryNotFound {
                resource: "missing".to_string(),
            },
            Error::RegistryResponseTooLarge {
                resource: "oversized".to_string(),
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
                Error::RegistryError("status 503".to_string())
            ),
            Error::RegistryError(_)
        ));
    }

    #[test]
    fn poisoned_registry_cache_lock_is_recovered() {
        let cache = Arc::new(Mutex::new(HashMap::<String, String>::new()));
        let poisoned = Arc::clone(&cache);
        let _ = std::thread::spawn(move || {
            let _guard = poisoned.lock().unwrap();
            panic!("poison test lock");
        })
        .join();

        lock_unpoisoned(&cache).insert("key".to_string(), "value".to_string());
        assert_eq!(
            lock_unpoisoned(&cache).get("key").map(String::as_str),
            Some("value")
        );
    }

    #[test]
    fn aggregate_cache_budgets_skip_excess_entries_without_changing_values() {
        let version = ExactVersion::parse("1.0.0").unwrap();
        let mut versions = VersionCache::default();
        assert!(versions.insert_with_limits(
            "first".to_string(),
            vec![version.clone(), version.clone()],
            4,
            2,
        ));
        assert!(!versions.insert_with_limits("excess".to_string(), vec![version], 4, 2,));
        assert_eq!(versions.candidates, 2);
        assert!(versions.entries.contains_key("first"));
        assert!(!versions.entries.contains_key("excess"));

        let mut manifests = ManifestCache::default();
        assert!(manifests.insert_with_limits(
            ("module".to_string(), "1.0.0".to_string()),
            vec![1, 2],
            4,
            2,
        ));
        assert!(!manifests.insert_with_limits(
            ("module".to_string(), "1.1.0".to_string()),
            vec![3],
            4,
            2,
        ));
        assert_eq!(manifests.bytes, 2);
        assert_eq!(manifests.entries.len(), 1);
    }
}
