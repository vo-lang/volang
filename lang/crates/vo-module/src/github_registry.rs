use std::collections::HashMap;
use std::io::Read;
use std::sync::{Mutex, MutexGuard};
use std::time::Duration;

use serde::Deserialize;

use crate::identity::ModulePath;
use crate::registry::{release_download_url, repository_id, version_from_tag, Registry};
use crate::version::ExactVersion;
use crate::Error;

const MAX_GITHUB_API_RESPONSE_BYTES: usize = 16 * 1024 * 1024;
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

/// Concrete `Registry` implementation backed by the GitHub Releases API.
/// Caches version lists and manifests for the lifetime of the struct.
pub struct GitHubRegistry {
    /// Optional GitHub personal access token for authenticated API requests.
    /// Resolves rate-limit issues and enables access to private repositories.
    token: Option<String>,
    agent: ureq::Agent,
    version_cache: Mutex<VersionCache>,
    manifest_cache: Mutex<ManifestCache>,
}

#[derive(Debug, Deserialize)]
struct GitHubReleaseEntry {
    tag_name: String,
    #[serde(default)]
    draft: bool,
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
        Self {
            token,
            agent: ureq::AgentBuilder::new()
                .timeout_connect(HTTP_CONNECT_TIMEOUT)
                .timeout_read(HTTP_IO_TIMEOUT)
                .timeout_write(HTTP_IO_TIMEOUT)
                .timeout(HTTP_REQUEST_TIMEOUT)
                .build(),
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
                if entry.draft {
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
        let url = release_download_url(module, version, "vo.release.json");
        // Preserve downloaded bytes as the sole authority. Callers charge the
        // solve/probe byte budget before parsing and identity validation.
        self.fetch_bytes(&url, vo_common::vfs::MAX_TEXT_FILE_BYTES)
            .map_err(|error| release_manifest_fetch_error(module, version, error))
    }

    fn fetch_bytes(&self, url: &str, max_bytes: usize) -> Result<Vec<u8>, Error> {
        let mut request = self
            .agent
            .get(url)
            .set("User-Agent", "vo-module")
            .set("Accept", "application/vnd.github+json");
        if let Some(ref token) = self.token {
            request = request.set("Authorization", &format!("Bearer {}", token));
        }
        let response = match request.call() {
            Ok(response) => response,
            Err(ureq::Error::Status(code, response)) => {
                return Err(http_status_error(url, code, response.status_text()));
            }
            Err(ureq::Error::Transport(e)) => {
                return Err(Error::RegistryError(format!("HTTP GET {}: {}", url, e)));
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
        let asset_name = crate::artifact::artifact_release_asset_name(artifact)
            .map_err(Error::InvalidReleaseMetadata)?;
        let url = release_download_url(module, version, &asset_name);
        self.fetch_bytes(
            &url,
            usize::try_from(crate::MAX_MODULE_ARTIFACT_BYTES).unwrap_or(usize::MAX),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn github_release_pages_are_bounded_during_deserialization() {
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let body = serde_json::to_vec(
            &(0..=RELEASES_PER_PAGE)
                .map(|index| serde_json::json!({ "tag_name": format!("v1.0.{index}") }))
                .collect::<Vec<_>>(),
        )
        .unwrap();
        let error = parse_github_release_page(&body, &module, 1).unwrap_err();
        assert!(error.to_string().contains("more than 100"), "{error}");

        let error = parse_github_release_page(b"[] trailing", &module, 1).unwrap_err();
        assert!(error.to_string().contains("trailing"), "{error}");
    }

    #[test]
    fn test_version_from_tag_root() {
        let mp = ModulePath::parse("github.com/acme/lib").unwrap();
        let v = version_from_tag(&mp, "v1.4.2").unwrap();
        assert_eq!(v.to_string(), "v1.4.2");
    }

    #[test]
    fn test_version_from_tag_nested() {
        let mp = ModulePath::parse("github.com/acme/mono/graphics/v2").unwrap();
        let v = version_from_tag(&mp, "graphics/v2/v2.1.0").unwrap();
        assert_eq!(v.to_string(), "v2.1.0");
    }

    #[test]
    fn test_version_from_tag_mismatch() {
        let mp = ModulePath::parse("github.com/acme/mono/graphics").unwrap();
        assert!(version_from_tag(&mp, "v0.8.0").is_none());
    }

    #[test]
    fn test_release_download_url() {
        let mp = ModulePath::parse("github.com/vo-lang/vogui").unwrap();
        let v = ExactVersion::parse("v0.4.2").unwrap();
        assert_eq!(
            release_download_url(&mp, &v, "vo.release.json"),
            "https://github.com/vo-lang/vogui/releases/download/v0.4.2/vo.release.json"
        );
    }

    #[test]
    fn test_release_download_url_nested() {
        let mp = ModulePath::parse("github.com/acme/mono/graphics/v2").unwrap();
        let v = ExactVersion::parse("v2.1.0").unwrap();
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
        let version = ExactVersion::parse("v1.2.3").unwrap();
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
        let version = ExactVersion::parse("v1.0.0").unwrap();
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
            ("module".to_string(), "v1.0.0".to_string()),
            vec![1, 2],
            4,
            2,
        ));
        assert!(!manifests.insert_with_limits(
            ("module".to_string(), "v1.1.0".to_string()),
            vec![3],
            4,
            2,
        ));
        assert_eq!(manifests.bytes, 2);
        assert_eq!(manifests.entries.len(), 1);
    }
}
