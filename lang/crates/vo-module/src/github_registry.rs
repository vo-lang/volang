use std::collections::HashMap;
use std::io::Read;
use std::sync::Mutex;

use serde::Deserialize;

use crate::identity::ModulePath;
use crate::registry::{release_download_url, repository_id, validate_manifest, Registry};
use crate::schema::manifest::ReleaseManifest;
use crate::version::ExactVersion;
use crate::Error;

type ManifestCache = Mutex<HashMap<(String, String), (ReleaseManifest, Vec<u8>)>>;

/// Concrete `Registry` implementation backed by the GitHub Releases API.
/// Caches version lists and manifests for the lifetime of the struct.
pub struct GitHubRegistry {
    /// Optional GitHub personal access token for authenticated API requests.
    /// Resolves rate-limit issues and enables access to private repositories.
    token: Option<String>,
    version_cache: Mutex<HashMap<String, Vec<ExactVersion>>>,
    manifest_cache: ManifestCache,
}

#[derive(Debug, Deserialize)]
struct GitHubReleaseEntry {
    tag_name: String,
    #[serde(default)]
    draft: bool,
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
            version_cache: Mutex::new(HashMap::new()),
            manifest_cache: Mutex::new(HashMap::new()),
        }
    }

    fn fetch_versions_uncached(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        let rid = repository_id(module);
        let page_size = 100usize;
        let mut page = 1usize;
        let mut versions = Vec::new();
        loop {
            let url = format!(
                "https://api.github.com/repos/{}/{}/releases?per_page={}&page={}",
                rid.owner, rid.repo, page_size, page,
            );
            let body = self.fetch_bytes(&url)?;
            let releases: Vec<GitHubReleaseEntry> = serde_json::from_slice(&body).map_err(|e| {
                Error::RegistryError(format!(
                    "invalid GitHub releases response for {}: {}",
                    module, e
                ))
            })?;
            let release_count = releases.len();
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
            if release_count < page_size {
                break;
            }
            page += 1;
        }
        versions.sort();
        versions.dedup();
        versions.reverse(); // newest first
        Ok(versions)
    }

    fn fetch_manifest_uncached(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<(ReleaseManifest, Vec<u8>), Error> {
        let url = release_download_url(module, version, "vo.release.json");
        let bytes = self.fetch_bytes(&url)?;
        let content = String::from_utf8(bytes.clone()).map_err(|e| {
            Error::InvalidReleaseMetadata(format!(
                "release manifest for {}@{} is not valid UTF-8: {}",
                module, version, e
            ))
        })?;
        let manifest = ReleaseManifest::parse(&content).map_err(|e| {
            Error::InvalidReleaseMetadata(format!(
                "invalid vo.release.json for {}@{}: {}",
                module, version, e
            ))
        })?;
        validate_manifest(&manifest, module, version)?;
        Ok((manifest, bytes))
    }

    fn fetch_bytes(&self, url: &str) -> Result<Vec<u8>, Error> {
        let mut request = ureq::get(url)
            .set("User-Agent", "vo-module")
            .set("Accept", "application/vnd.github+json");
        if let Some(ref token) = self.token {
            request = request.set("Authorization", &format!("Bearer {}", token));
        }
        let response = match request.call() {
            Ok(response) => response,
            Err(ureq::Error::Status(code, response)) => {
                return Err(Error::RegistryError(format!(
                    "HTTP GET {}: {} {}",
                    url,
                    code,
                    response.status_text()
                )));
            }
            Err(ureq::Error::Transport(e)) => {
                return Err(Error::RegistryError(format!("HTTP GET {}: {}", url, e)));
            }
        };
        let mut bytes = Vec::new();
        response
            .into_reader()
            .read_to_end(&mut bytes)
            .map_err(|e| Error::RegistryError(format!("reading response from {}: {}", url, e)))?;
        Ok(bytes)
    }
}

impl Registry for GitHubRegistry {
    fn list_versions(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        let key = module.as_str().to_string();
        if let Some(cached) = self.version_cache.lock().unwrap().get(&key) {
            return Ok(cached.clone());
        }
        let versions = self.fetch_versions_uncached(module)?;
        self.version_cache
            .lock()
            .unwrap()
            .insert(key, versions.clone());
        Ok(versions)
    }

    fn probe_module_path(&self, module: &ModulePath) -> Result<bool, Error> {
        match self.list_versions(module) {
            Ok(versions) => {
                Ok(!crate::registry::filter_compatible_versions(module, &versions).is_empty())
            }
            Err(Error::RegistryError(message)) if message.contains(": 404 ") => Ok(false),
            Err(error) => Err(error),
        }
    }

    fn fetch_manifest(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<ReleaseManifest, Error> {
        self.fetch_manifest_raw(module, version).map(|(m, _)| m)
    }

    fn fetch_manifest_raw(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<(ReleaseManifest, Vec<u8>), Error> {
        let key = (module.as_str().to_string(), version.to_string());
        if let Some(cached) = self.manifest_cache.lock().unwrap().get(&key) {
            return Ok(cached.clone());
        }
        let result = self.fetch_manifest_uncached(module, version)?;
        self.manifest_cache
            .lock()
            .unwrap()
            .insert(key, result.clone());
        Ok(result)
    }

    fn fetch_source_package(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        let url = release_download_url(module, version, asset_name);
        self.fetch_bytes(&url)
    }

    fn fetch_artifact(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        let url = release_download_url(module, version, asset_name);
        self.fetch_bytes(&url)
    }
}

/// Extract an `ExactVersion` from a Git tag, accounting for the module's root prefix.
fn version_from_tag(module: &ModulePath, tag: &str) -> Option<ExactVersion> {
    let root = module.module_root();
    let version_str = if root == "." {
        tag
    } else {
        let prefix = format!("{}/", root);
        tag.strip_prefix(&prefix)?
    };
    ExactVersion::parse(version_str).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
