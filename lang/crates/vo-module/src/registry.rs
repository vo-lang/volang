use crate::identity::ModulePath;
use crate::version::ExactVersion;
use crate::schema::manifest::ReleaseManifest;
use crate::Error;

/// Repository identity derived from a module path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RepositoryId {
    pub owner: String,
    pub repo: String,
}

/// Trait for registry access — abstracted for testability.
/// The registry provides version discovery and manifest retrieval.
pub trait Registry {
    /// List all valid published versions for a module path.
    /// Only versions with complete release metadata (tag + release + manifest + source)
    /// are returned. Tags without proper releases are excluded.
    fn list_versions(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error>;

    /// Fetch the release manifest for a specific module version.
    fn fetch_manifest(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<ReleaseManifest, Error>;

    /// Fetch the release manifest and return both the parsed manifest and its
    /// raw bytes (exactly as stored in the registry).  The digest of these bytes
    /// is what gets recorded as `release_manifest` in `vo.lock`.
    ///
    /// The default implementation re-renders via `ReleaseManifest::render()`;
    /// concrete registries that have access to the original bytes should override
    /// this to return those bytes instead.
    fn fetch_manifest_raw(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<(ReleaseManifest, Vec<u8>), Error> {
        let manifest = self.fetch_manifest(module, version)?;
        let raw = format!("{}\n", manifest.render());
        Ok((manifest, raw.into_bytes()))
    }

    /// Fetch the source package bytes for a specific module version.
    fn fetch_source_package(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        asset_name: &str,
    ) -> Result<Vec<u8>, Error>;

    /// Fetch a target-specific artifact by asset name.
    fn fetch_artifact(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        asset_name: &str,
    ) -> Result<Vec<u8>, Error>;
}

/// Compute the GitHub repository identity from a canonical module path.
pub fn repository_id(module: &ModulePath) -> RepositoryId {
    RepositoryId {
        owner: module.owner().to_string(),
        repo: module.repo().to_string(),
    }
}

/// Build the GitHub release asset download URL for a module+version+asset.
///
/// `https://github.com/<owner>/<repo>/releases/download/<tag>/<asset_name>`
///
/// Single source of truth for release asset URLs, used by both the native
/// `GitHubRegistry` and the WASM fetch layer.
pub fn release_download_url(module: &ModulePath, version: &ExactVersion, asset_name: &str) -> String {
    let rid = repository_id(module);
    let tag = module.version_tag(version);
    format!(
        "https://github.com/{}/{}/releases/download/{}/{}",
        rid.owner, rid.repo, tag, asset_name,
    )
}

/// Validate that a release manifest is consistent with the module path and version
/// it was fetched for.
pub fn validate_manifest(
    manifest: &ReleaseManifest,
    expected_module: &ModulePath,
    expected_version: &ExactVersion,
) -> Result<(), Error> {
    if manifest.module != *expected_module {
        return Err(Error::InvalidReleaseMetadata(format!(
            "manifest module '{}' does not match expected '{}'",
            manifest.module, expected_module
        )));
    }
    if manifest.version != *expected_version {
        return Err(Error::InvalidReleaseMetadata(format!(
            "manifest version '{}' does not match expected '{}'",
            manifest.version, expected_version
        )));
    }
    // Verify major version compatibility
    if !expected_module.accepts_version(expected_version) {
        return Err(Error::InvalidReleaseMetadata(format!(
            "version {} is not compatible with module path {}",
            expected_version, expected_module
        )));
    }
    // Verify module_root matches module path
    let expected_root = expected_module.module_root();
    if manifest.module_root != expected_root {
        return Err(Error::InvalidReleaseMetadata(format!(
            "manifest module_root '{}' does not match expected '{}'",
            manifest.module_root, expected_root
        )));
    }
    Ok(())
}

/// Filter versions to only those compatible with the module path's major version rule.
pub fn filter_compatible_versions(
    module: &ModulePath,
    versions: &[ExactVersion],
) -> Vec<ExactVersion> {
    versions
        .iter()
        .filter(|v| module.accepts_version(v))
        .cloned()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_repository_id() {
        let mp = ModulePath::parse("github.com/acme/lib").unwrap();
        let rid = repository_id(&mp);
        assert_eq!(rid.owner, "acme");
        assert_eq!(rid.repo, "lib");
    }

    #[test]
    fn test_repository_id_subdir() {
        let mp = ModulePath::parse("github.com/acme/mono/graphics").unwrap();
        let rid = repository_id(&mp);
        assert_eq!(rid.owner, "acme");
        assert_eq!(rid.repo, "mono");
    }

    #[test]
    fn test_version_tag_root() {
        let mp = ModulePath::parse("github.com/acme/lib").unwrap();
        let v = ExactVersion::parse("v1.4.2").unwrap();
        assert_eq!(mp.version_tag(&v), "v1.4.2");
    }

    #[test]
    fn test_version_tag_nested() {
        let mp = ModulePath::parse("github.com/acme/mono/graphics").unwrap();
        let v = ExactVersion::parse("v0.8.0").unwrap();
        assert_eq!(mp.version_tag(&v), "graphics/v0.8.0");
    }

    #[test]
    fn test_filter_compatible() {
        let mp = ModulePath::parse("github.com/acme/lib").unwrap();
        let versions = vec![
            ExactVersion::parse("v0.1.0").unwrap(),
            ExactVersion::parse("v1.0.0").unwrap(),
            ExactVersion::parse("v2.0.0").unwrap(),
        ];
        let compat = filter_compatible_versions(&mp, &versions);
        assert_eq!(compat.len(), 2);
        assert_eq!(compat[0].to_string(), "v0.1.0");
        assert_eq!(compat[1].to_string(), "v1.0.0");
    }
}
