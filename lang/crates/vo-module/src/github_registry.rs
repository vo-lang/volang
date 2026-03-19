use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Read;

use serde::Deserialize;

use crate::identity::ModulePath;
use crate::version::ExactVersion;
use crate::registry::{repository_id, release_download_url, validate_manifest, Registry};
use crate::schema::manifest::ReleaseManifest;
use crate::Error;

/// Concrete `Registry` implementation backed by the GitHub Releases API.
/// Caches version lists and manifests for the lifetime of the struct.
#[derive(Default)]
pub struct GitHubRegistry {
    version_cache: RefCell<HashMap<String, Vec<ExactVersion>>>,
    manifest_cache: RefCell<HashMap<(String, String), (ReleaseManifest, Vec<u8>)>>,
}

#[derive(Debug, Deserialize)]
struct GitHubReleaseEntry {
    tag_name: String,
    #[serde(default)]
    draft: bool,
}

impl GitHubRegistry {
    pub fn new() -> Self {
        Self::default()
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
            let body = fetch_bytes(&url)?;
            let releases: Vec<GitHubReleaseEntry> =
                serde_json::from_slice(&body).map_err(|e| {
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
        let bytes = fetch_bytes(&url)?;
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
}

impl Registry for GitHubRegistry {
    fn list_versions(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        let key = module.as_str().to_string();
        if let Some(cached) = self.version_cache.borrow().get(&key) {
            return Ok(cached.clone());
        }
        let versions = self.fetch_versions_uncached(module)?;
        self.version_cache
            .borrow_mut()
            .insert(key, versions.clone());
        Ok(versions)
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
        if let Some(cached) = self.manifest_cache.borrow().get(&key) {
            return Ok(cached.clone());
        }
        let result = self.fetch_manifest_uncached(module, version)?;
        self.manifest_cache
            .borrow_mut()
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
        fetch_bytes(&url)
    }

    fn fetch_artifact(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        let url = release_download_url(module, version, asset_name);
        fetch_bytes(&url)
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

fn fetch_bytes(url: &str) -> Result<Vec<u8>, Error> {
    let response = match ureq::get(url)
        .set("User-Agent", "vo-module")
        .set("Accept", "application/vnd.github+json")
        .call()
    {
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
