//! Transport-independent validation for GitHub release provenance.
//!
//! Both the native registry and browser registry fetch GitHub's wire objects,
//! then pass the exact response bytes through this module. Keeping release
//! metadata parsing and asset-inventory validation here prevents either
//! transport from accepting a weaker release identity.

use std::collections::BTreeMap;

use serde::de::DeserializeOwned;
use serde::Deserialize;

use crate::digest::Digest;
use crate::identity::ModulePath;
use crate::registry::{
    canonical_release_assets, encode_url_path, encode_url_path_component, release_tag,
    repository_id,
};
use crate::schema::manifest::ReleaseManifest;
use crate::version::ExactVersion;
use crate::Error;

/// Maximum accepted size for one GitHub API provenance response.
pub const MAX_GITHUB_API_RESPONSE_BYTES: usize = 16 * 1024 * 1024;
/// GitHub REST API version whose release schema includes immutable-release and
/// SHA-256 asset provenance fields.
pub const GITHUB_API_VERSION: &str = "2026-03-10";
const MAX_GITHUB_RELEASE_ASSETS: usize = crate::MAX_MODULE_ARTIFACTS + 2;

#[derive(Debug, Deserialize)]
struct GitHubReleaseMetadata {
    tag_name: String,
    draft: bool,
    immutable: bool,
    #[serde(deserialize_with = "deserialize_github_release_assets")]
    assets: Vec<GitHubReleaseAsset>,
}

#[derive(Debug, Deserialize)]
struct GitHubReleaseAsset {
    name: String,
    size: u64,
    state: String,
    digest: String,
}

fn deserialize_github_release_assets<'de, D>(
    deserializer: D,
) -> Result<Vec<GitHubReleaseAsset>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::schema::deserialize_bounded_vec(
        deserializer,
        MAX_GITHUB_RELEASE_ASSETS,
        "GitHub release assets",
    )
}

/// Validated GitHub release metadata and its complete uploaded-asset view.
#[derive(Debug, Clone)]
pub struct GitHubReleaseProvenance {
    assets: BTreeMap<String, GitHubAssetProvenance>,
}

#[derive(Debug, Clone)]
struct GitHubAssetProvenance {
    size: u64,
    digest: Digest,
}

impl GitHubReleaseProvenance {
    /// Require the fixed release-manifest asset before callers download it.
    pub fn require_release_manifest_asset(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<(), Error> {
        if self.assets.contains_key("vo.release.json") {
            return Ok(());
        }
        Err(Error::InvalidReleaseMetadata(format!(
            "GitHub release {module} {version} is missing canonical asset vo.release.json"
        )))
    }

    /// Compare GitHub's entire uploaded-asset inventory, sizes, and SHA-256
    /// digests with the canonical inventory derived from the authenticated raw
    /// release manifest. This lets native and browser transports authenticate
    /// the same immutable asset set even when they materialize different
    /// payload subsets.
    pub fn validate_assets(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        manifest: &ReleaseManifest,
        manifest_raw: &[u8],
    ) -> Result<(), Error> {
        let expected = canonical_release_assets(manifest, manifest_raw)?;
        let missing = expected
            .keys()
            .filter(|name| !self.assets.contains_key(*name))
            .cloned()
            .collect::<Vec<_>>();
        let unexpected = self
            .assets
            .keys()
            .filter(|name| !expected.contains_key(*name))
            .cloned()
            .collect::<Vec<_>>();
        let size_mismatches = expected
            .iter()
            .filter_map(|(name, expected_asset)| {
                self.assets.get(name).and_then(|actual_asset| {
                    (actual_asset.size != expected_asset.size).then(|| {
                        format!(
                            "{name:?}: expected {} bytes, found {} bytes",
                            expected_asset.size, actual_asset.size
                        )
                    })
                })
            })
            .collect::<Vec<_>>();
        let digest_mismatches = expected
            .iter()
            .filter_map(|(name, expected_asset)| {
                self.assets.get(name).and_then(|actual_asset| {
                    (actual_asset.digest != expected_asset.digest).then(|| {
                        format!(
                            "{name:?}: expected {}, found {}",
                            expected_asset.digest, actual_asset.digest
                        )
                    })
                })
            })
            .collect::<Vec<_>>();
        if missing.is_empty()
            && unexpected.is_empty()
            && size_mismatches.is_empty()
            && digest_mismatches.is_empty()
        {
            return Ok(());
        }

        let mut details = Vec::new();
        if !missing.is_empty() {
            details.push(format!("missing {}", summarize_diagnostics(&missing)));
        }
        if !unexpected.is_empty() {
            details.push(format!("unexpected {}", summarize_diagnostics(&unexpected)));
        }
        if !size_mismatches.is_empty() {
            details.push(format!(
                "size mismatches {}",
                summarize_diagnostics(&size_mismatches)
            ));
        }
        if !digest_mismatches.is_empty() {
            details.push(format!(
                "digest mismatches {}",
                summarize_diagnostics(&digest_mismatches)
            ));
        }
        Err(Error::InvalidReleaseMetadata(format!(
            "GitHub release asset inventory for {module} {version} does not exactly match vo.release.json: {}",
            details.join("; ")
        )))
    }
}

/// Parse and validate the GitHub release-by-tag response for one requested
/// module version.
pub fn parse_github_release_provenance(
    body: &[u8],
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<GitHubReleaseProvenance, Error> {
    let release: GitHubReleaseMetadata =
        parse_provenance_json(body, module, version, "GitHub release metadata")?;
    let expected_tag = release_tag(module, version);
    if release.tag_name != expected_tag {
        return Err(Error::InvalidReleaseMetadata(format!(
            "GitHub release for {module} {version} reports tag {:?}, expected {expected_tag:?}",
            release.tag_name
        )));
    }
    if release.draft {
        return Err(Error::InvalidReleaseMetadata(format!(
            "GitHub release {module} {version} is still a draft"
        )));
    }
    if !release.immutable {
        return Err(Error::InvalidReleaseMetadata(format!(
            "GitHub release {module} {version} must report immutable=true"
        )));
    }

    let mut assets = BTreeMap::new();
    for asset in release.assets {
        if asset.state != "uploaded" {
            return Err(Error::InvalidReleaseMetadata(format!(
                "GitHub release asset {:?} for {module} {version} has state {:?}, expected \"uploaded\"",
                asset.name, asset.state
            )));
        }
        crate::schema::validate_file_name(&asset.name).map_err(|error| {
            Error::InvalidReleaseMetadata(format!(
                "GitHub release {module} {version} contains non-canonical asset name {:?}: {error}",
                asset.name
            ))
        })?;
        let digest = Digest::parse(&asset.digest).map_err(|error| {
            Error::InvalidReleaseMetadata(format!(
                "GitHub release asset {:?} for {module} {version} has a non-canonical digest: {error}",
                asset.name
            ))
        })?;
        if assets
            .insert(
                asset.name.clone(),
                GitHubAssetProvenance {
                    size: asset.size,
                    digest,
                },
            )
            .is_some()
        {
            return Err(Error::InvalidReleaseMetadata(format!(
                "GitHub release {module} {version} contains duplicate asset name {:?}",
                asset.name
            )));
        }
    }
    Ok(GitHubReleaseProvenance { assets })
}

/// Convert missing or oversized GitHub provenance objects into definitive,
/// candidate-local release failures while preserving transient transport
/// failures.
pub fn github_provenance_fetch_error(
    module: &ModulePath,
    version: &ExactVersion,
    resource: &str,
    error: Error,
) -> Error {
    match error {
        Error::RegistryNotFound { .. } => Error::InvalidReleaseMetadata(format!(
            "GitHub release {module} {version} is missing {resource}"
        )),
        Error::RegistryResponseTooLarge { limit, .. } => Error::InvalidReleaseMetadata(format!(
            "GitHub {resource} for {module} {version} exceeds the {limit}-byte limit"
        )),
        other => other,
    }
}

/// GitHub release-by-tag API URL for one canonical module version.
pub fn github_release_metadata_url(module: &ModulePath, version: &ExactVersion) -> String {
    format!(
        "{}/releases/tags/{}",
        github_api_repository_base(module),
        encode_url_path(&release_tag(module, version)),
    )
}

fn github_api_repository_base(module: &ModulePath) -> String {
    let repository = repository_id(module);
    format!(
        "https://api.github.com/repos/{}/{}",
        encode_url_path_component(&repository.owner),
        encode_url_path_component(&repository.repo),
    )
}

fn parse_provenance_json<T: DeserializeOwned>(
    body: &[u8],
    module: &ModulePath,
    version: &ExactVersion,
    resource: &str,
) -> Result<T, Error> {
    let mut deserializer = serde_json::Deserializer::from_slice(body);
    let value = T::deserialize(&mut deserializer).map_err(|error| {
        Error::InvalidReleaseMetadata(format!(
            "invalid {resource} for {module} {version}: {error}"
        ))
    })?;
    deserializer.end().map_err(|error| {
        Error::InvalidReleaseMetadata(format!(
            "invalid trailing data in {resource} for {module} {version}: {error}"
        ))
    })?;
    Ok(value)
}

fn summarize_diagnostics(values: &[String]) -> String {
    const MAX_ITEMS: usize = 8;
    let mut summary = format!(
        "[{}]",
        values
            .iter()
            .take(MAX_ITEMS)
            .cloned()
            .collect::<Vec<_>>()
            .join(", ")
    );
    if values.len() > MAX_ITEMS {
        summary.push_str(&format!(" and {} more", values.len() - MAX_ITEMS));
    }
    summary
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::digest::Digest;
    use crate::registry::parse_manifest_bytes;

    fn identity() -> (ModulePath, ExactVersion) {
        (
            ModulePath::parse("github.com/acme/lib").unwrap(),
            ExactVersion::parse("1.2.3").unwrap(),
        )
    }

    fn manifest_raw() -> Vec<u8> {
        let (module, version) = identity();
        ReleaseManifest {
            format: 1,
            module,
            version,
            vo: crate::version::ToolchainConstraint::parse("0.1.0").unwrap(),
            intent: Digest::from_sha256(b"github-provenance-fixture-intent"),
            dependencies: Vec::new(),
            source: crate::schema::manifest::ManifestSource {
                name: "source.tar.gz".to_string(),
                size: 13,
                digest: Digest::from_sha256(b"source"),
                tree: Digest::from_sha256(b"tree"),
            },
            artifacts: Vec::new(),
        }
        .render()
        .unwrap()
        .into_bytes()
    }

    fn metadata_raw(assets: Vec<serde_json::Value>) -> Vec<u8> {
        serde_json::to_vec(&serde_json::json!({
            "tag_name": "v1.2.3",
            "draft": false,
            "immutable": true,
            "assets": assets
        }))
        .unwrap()
    }

    fn exact_assets(raw: &[u8]) -> Vec<serde_json::Value> {
        let (module, version) = identity();
        let manifest = parse_manifest_bytes(raw, &module, &version).unwrap();
        canonical_release_assets(&manifest, raw)
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
            .collect()
    }

    #[test]
    fn release_metadata_requires_exact_identity_state_names_sizes_and_inventory() {
        let (module, version) = identity();
        let raw = manifest_raw();
        let manifest = parse_manifest_bytes(&raw, &module, &version).unwrap();
        let provenance =
            parse_github_release_provenance(&metadata_raw(exact_assets(&raw)), &module, &version)
                .unwrap();
        provenance
            .require_release_manifest_asset(&module, &version)
            .unwrap();
        provenance
            .validate_assets(&module, &version, &manifest, &raw)
            .unwrap();

        for field in ["tag_name", "draft", "immutable", "assets"] {
            let mut wire: serde_json::Value =
                serde_json::from_slice(&metadata_raw(exact_assets(&raw))).unwrap();
            wire.as_object_mut().unwrap().remove(field);
            let error = parse_github_release_provenance(
                &serde_json::to_vec(&wire).unwrap(),
                &module,
                &version,
            )
            .unwrap_err();
            assert!(error.to_string().contains(field), "{field}: {error}");
        }

        for (field, value, expected) in [
            ("tag_name", serde_json::json!("v1.2.4"), "reports tag"),
            ("draft", serde_json::json!(true), "still a draft"),
            (
                "immutable",
                serde_json::json!(false),
                "must report immutable=true",
            ),
        ] {
            let mut wire: serde_json::Value =
                serde_json::from_slice(&metadata_raw(exact_assets(&raw))).unwrap();
            wire[field] = value;
            let error = parse_github_release_provenance(
                &serde_json::to_vec(&wire).unwrap(),
                &module,
                &version,
            )
            .unwrap_err();
            assert!(error.to_string().contains(expected), "{error}");
        }

        let mutations = [
            (
                "state",
                serde_json::json!("starter"),
                "expected \"uploaded\"",
            ),
            (
                "name",
                serde_json::json!("../bad"),
                "non-canonical asset name",
            ),
            ("size", serde_json::json!(999), "size mismatches"),
        ];
        for (field, value, expected) in mutations {
            let mut assets = exact_assets(&raw);
            assets[0][field] = value;
            let parsed = parse_github_release_provenance(&metadata_raw(assets), &module, &version);
            let error = match parsed {
                Ok(provenance) => provenance
                    .validate_assets(&module, &version, &manifest, &raw)
                    .unwrap_err(),
                Err(error) => error,
            };
            assert!(error.to_string().contains(expected), "{error}");
        }

        let mut missing = exact_assets(&raw);
        missing.retain(|asset| asset["name"] != "source.tar.gz");
        let error = parse_github_release_provenance(&metadata_raw(missing), &module, &version)
            .unwrap()
            .validate_assets(&module, &version, &manifest, &raw)
            .unwrap_err();
        assert!(error.to_string().contains("missing"), "{error}");

        let mut extra = exact_assets(&raw);
        extra.push(serde_json::json!({
            "name": "notes.txt",
            "size": 1,
            "state": "uploaded",
            "digest": Digest::from_sha256(b"notes")
        }));
        let error = parse_github_release_provenance(&metadata_raw(extra), &module, &version)
            .unwrap()
            .validate_assets(&module, &version, &manifest, &raw)
            .unwrap_err();
        assert!(error.to_string().contains("unexpected"), "{error}");

        for field in ["name", "size", "state", "digest"] {
            let mut assets = exact_assets(&raw);
            assets[0].as_object_mut().unwrap().remove(field);
            let error = parse_github_release_provenance(&metadata_raw(assets), &module, &version)
                .unwrap_err();
            assert!(error.to_string().contains(field), "{field}: {error}");
        }

        for (digest, expected) in [
            (serde_json::Value::Null, "expected a string"),
            (serde_json::json!("sha512:abcd"), "digest"),
        ] {
            let mut assets = exact_assets(&raw);
            assets[0]["digest"] = digest;
            let error = parse_github_release_provenance(&metadata_raw(assets), &module, &version)
                .unwrap_err();
            assert!(error.to_string().contains(expected), "{error}");
        }

        let mut assets = exact_assets(&raw);
        assets[0]["digest"] = Digest::from_sha256(b"tampered").to_string().into();
        let error = parse_github_release_provenance(&metadata_raw(assets), &module, &version)
            .unwrap()
            .validate_assets(&module, &version, &manifest, &raw)
            .unwrap_err();
        assert!(error.to_string().contains("digest mismatches"), "{error}");
    }

    #[test]
    fn release_metadata_never_accepts_more_than_githubs_asset_limit() {
        assert_eq!(MAX_GITHUB_RELEASE_ASSETS, 1_000);
        let (module, version) = identity();
        let assets = (0..=MAX_GITHUB_RELEASE_ASSETS)
            .map(|index| {
                serde_json::json!({
                    "name": format!("asset-{index:04}"),
                    "size": 1,
                    "state": "uploaded",
                    "digest": Digest::from_sha256(index.to_string().as_bytes()),
                })
            })
            .collect();
        let error =
            parse_github_release_provenance(&metadata_raw(assets), &module, &version).unwrap_err();
        assert!(error.to_string().contains("more than 1000"), "{error}");
    }
}
