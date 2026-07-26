use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use crate::digest::Digest;
use crate::identity::{ArtifactId, ModulePath};
use crate::version::{DepConstraint, ExactVersion, ToolchainConstraint};
use crate::Error;

pub const SOURCE_ARCHIVE_ASSET_NAME: &str = "source.tar.gz";
pub const SOURCE_ARCHIVE_TOP_LEVEL_DIR: &str = "source";
pub const SOURCE_ARCHIVE_ROOT_DIR: &str = "source";
pub const SOURCE_TREE_MANIFEST_NAME: &str = "vo.tree.json";

/// Immutable registry descriptor. Transport provenance stays outside this
/// language-level object.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ReleaseManifest {
    pub format: u64,
    pub module: ModulePath,
    pub version: ExactVersion,
    pub vo: ToolchainConstraint,
    pub intent: Digest,
    #[serde(default)]
    pub dependencies: Vec<ManifestDependency>,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub profiles: BTreeMap<String, crate::profile::ProfileDeclaration>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub default_profile: Option<String>,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub capabilities: BTreeMap<String, crate::profile::CapabilityDeclaration>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub artifact_variants: Vec<crate::profile::ArtifactVariantDeclaration>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub source_recipes: Vec<crate::profile::SourceRecipeDeclaration>,
    pub source: ManifestSource,
    #[serde(default)]
    pub artifacts: Vec<ManifestArtifact>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ManifestDependency {
    pub module: ModulePath,
    pub constraint: DepConstraint,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub profile: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub capabilities: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ManifestSource {
    pub name: String,
    pub size: u64,
    pub digest: Digest,
    /// Digest of the exact embedded `source/vo.tree.json` bytes.
    pub tree: Digest,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ManifestArtifact {
    #[serde(flatten)]
    pub id: ArtifactId,
    pub size: u64,
    pub digest: Digest,
}

impl ReleaseManifest {
    pub fn parse(json: &str) -> Result<Self, Error> {
        if json.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(Error::ManifestParse(format!(
                "vo.release.json exceeds the {}-byte text limit",
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )));
        }
        let manifest: Self = serde_json::from_str(json)
            .map_err(|error| Error::ManifestParse(format!("JSON parse error: {error}")))?;
        manifest.validate()?;
        Ok(manifest)
    }

    pub fn validate(&self) -> Result<(), Error> {
        self.validate_with_local(false)
    }

    fn validate_with_local(&self, allow_local: bool) -> Result<(), Error> {
        if self.format != 1 {
            return Err(Error::ManifestParse(format!(
                "unsupported release format: {}",
                self.format
            )));
        }
        if self.module.is_local() && !allow_local {
            return Err(Error::ManifestParse(
                "published releases cannot use the local/* namespace".to_string(),
            ));
        }
        if !self.module.accepts_version(&self.version) {
            return Err(Error::ManifestParse(format!(
                "version {} is incompatible with module path {}",
                self.version, self.module
            )));
        }
        if self.dependencies.len() > crate::MAX_MODULE_DEPENDENCIES {
            return Err(Error::ManifestParse(format!(
                "dependencies contains more than {} entries",
                crate::MAX_MODULE_DEPENDENCIES
            )));
        }
        for pair in self.dependencies.windows(2) {
            if pair[0].module >= pair[1].module {
                return Err(Error::ManifestParse(
                    "dependencies must be unique and sorted by module path".to_string(),
                ));
            }
        }
        for (index, dependency) in self.dependencies.iter().enumerate() {
            if dependency.module.is_local() && !allow_local {
                return Err(Error::ManifestParse(format!(
                    "dependencies[{index}] cannot use the unpublished local/* namespace"
                )));
            }
            if dependency.module == self.module {
                return Err(Error::ManifestParse(format!(
                    "dependencies[{index}] must not name the release module"
                )));
            }
            let lower = ExactVersion::from_semver(dependency.constraint.version.clone());
            if !dependency.module.accepts_version(&lower) {
                return Err(Error::ManifestParse(format!(
                    "dependencies[{index}] constraint {} is incompatible with {}",
                    dependency.constraint, dependency.module
                )));
            }
            crate::profile::DependencyCapabilityRequest::new(
                dependency.profile.clone(),
                &dependency.capabilities,
                &format!("dependencies[{index}]"),
            )
            .map_err(|error| Error::ManifestParse(error.to_string()))?;
        }
        let profiles = crate::profile::ProfileCatalog::from_declarations_with_default(
            self.profiles.clone(),
            self.default_profile.clone(),
            "profiles",
        )
        .map_err(|error| Error::ManifestParse(error.to_string()))?;
        let capability_catalog = crate::profile::CapabilityCatalog::from_declarations(
            self.capabilities.clone(),
            "capabilities",
        )
        .map_err(|error| Error::ManifestParse(error.to_string()))?;
        let resolved_variants = crate::profile::resolve_artifact_variants(
            &self.artifact_variants,
            &profiles,
            "artifact_variants",
        )
        .map_err(|error| Error::ManifestParse(error.to_string()))?;
        let resolved_recipes = crate::profile::resolve_source_recipes(
            &self.source_recipes,
            &profiles,
            "source_recipes",
        )
        .map_err(|error| Error::ManifestParse(error.to_string()))?;
        for (index, variant) in resolved_variants.iter().enumerate() {
            capability_catalog
                .validate_set(
                    &variant.capabilities,
                    &variant.target,
                    &format!("artifact_variants[{index}]"),
                )
                .map_err(|error| Error::ManifestParse(error.to_string()))?;
        }
        for (index, recipe) in resolved_recipes.iter().enumerate() {
            capability_catalog
                .validate_set(
                    &recipe.capabilities,
                    &recipe.target,
                    &format!("source_recipes[{index}]"),
                )
                .map_err(|error| Error::ManifestParse(error.to_string()))?;
        }
        if self.source.name != SOURCE_ARCHIVE_ASSET_NAME {
            return Err(Error::ManifestParse(format!(
                "source.name must be {SOURCE_ARCHIVE_ASSET_NAME:?}"
            )));
        }
        if self.source.size == 0 || self.source.size > crate::MAX_SOURCE_ARCHIVE_BYTES {
            return Err(Error::ManifestParse(format!(
                "source.size {} must be within 1..={}",
                self.source.size,
                crate::MAX_SOURCE_ARCHIVE_BYTES
            )));
        }
        if self.artifacts.len() > crate::MAX_MODULE_ARTIFACTS {
            return Err(Error::ManifestParse(format!(
                "artifacts contains more than {} entries",
                crate::MAX_MODULE_ARTIFACTS
            )));
        }
        for pair in self.artifacts.windows(2) {
            if pair[0].id >= pair[1].id {
                return Err(Error::ManifestParse(
                    "artifacts must be unique and sorted by (kind, target, name)".to_string(),
                ));
            }
        }
        let mut names = BTreeSet::from([
            super::portable_case_key("vo.release.json"),
            super::portable_case_key(SOURCE_ARCHIVE_ASSET_NAME),
        ]);
        let mut cache_paths = super::PortablePathSet::default();
        for (index, artifact) in self.artifacts.iter().enumerate() {
            artifact
                .id
                .validate()
                .map_err(|detail| Error::ManifestParse(format!("artifacts[{index}]: {detail}")))?;
            if artifact.size == 0 || artifact.size > crate::MAX_MODULE_ARTIFACT_BYTES {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{index}].size {} must be within 1..={}",
                    artifact.size,
                    crate::MAX_MODULE_ARTIFACT_BYTES
                )));
            }
            let cache_path = crate::artifact::artifact_cache_key(&artifact.id)
                .map_err(|detail| Error::ManifestParse(format!("artifacts[{index}]: {detail}")))?;
            if !cache_paths
                .insert_file(&cache_path)
                .map_err(|detail| Error::ManifestParse(format!("artifacts[{index}]: {detail}")))?
            {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{index}] duplicates cache path {cache_path}"
                )));
            }
            let asset_name = crate::artifact::artifact_release_asset_name(&artifact.id)
                .map_err(|detail| Error::ManifestParse(format!("artifacts[{index}]: {detail}")))?;
            if !names.insert(super::portable_case_key(&asset_name)) {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{index}] collides with release asset {asset_name:?}"
                )));
            }
        }
        let mut artifact_capabilities = BTreeMap::new();
        for (variant_index, variant) in resolved_variants.iter().enumerate() {
            for (role_index, role) in variant.roles.iter().enumerate() {
                let identity = (
                    role.kind.as_str(),
                    variant.target.as_str(),
                    role.name.as_str(),
                );
                if let Some(existing) =
                    artifact_capabilities.insert(identity, variant.capabilities.clone())
                {
                    if existing != variant.capabilities {
                        return Err(Error::ManifestParse(format!(
                            "artifact_variants[{variant_index}].roles[{role_index}] reuses one published artifact for different capability sets"
                        )));
                    }
                }
                let published = self.artifacts.iter().find(|artifact| {
                    artifact.id.kind == role.kind
                        && artifact.id.target == variant.target
                        && artifact.id.name == role.name
                });
                let Some(published) = published else {
                    return Err(Error::ManifestParse(format!(
                        "artifact_variants[{variant_index}].roles[{role_index}] has no matching published artifact"
                    )));
                };
                if published.digest != role.digest {
                    return Err(Error::ManifestParse(format!(
                        "artifact_variants[{variant_index}].roles[{role_index}] digest differs from the published artifact"
                    )));
                }
            }
        }
        Ok(())
    }

    pub fn render(&self) -> Result<String, Error> {
        self.render_with_local(false)
    }

    pub(crate) fn render_resolution_descriptor(&self) -> Result<String, Error> {
        self.render_with_local(true)
    }

    fn render_with_local(&self, allow_local: bool) -> Result<String, Error> {
        self.validate_with_local(allow_local)?;
        let mut value = serde_json::to_string_pretty(self)
            .map_err(|error| Error::ManifestParse(format!("JSON encode error: {error}")))?;
        value.push('\n');
        if value.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(Error::ManifestParse(format!(
                "vo.release.json exceeds the {}-byte text limit",
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )));
        }
        Ok(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample() -> ReleaseManifest {
        ReleaseManifest {
            format: 1,
            module: ModulePath::parse("example.com/acme/render").unwrap(),
            version: ExactVersion::parse("0.2.0").unwrap(),
            vo: ToolchainConstraint::parse("0.1.4").unwrap(),
            intent: Digest::from_sha256(b"intent"),
            dependencies: Vec::new(),
            profiles: BTreeMap::new(),
            default_profile: None,
            capabilities: BTreeMap::new(),
            artifact_variants: Vec::new(),
            source_recipes: Vec::new(),
            source: ManifestSource {
                name: SOURCE_ARCHIVE_ASSET_NAME.to_string(),
                size: 42,
                digest: Digest::from_sha256(b"archive"),
                tree: Digest::from_sha256(b"tree"),
            },
            artifacts: Vec::new(),
        }
    }

    #[test]
    fn roundtrips_and_accepts_equivalent_json() {
        let value = sample();
        let rendered = value.render().unwrap();
        assert_eq!(ReleaseManifest::parse(&rendered).unwrap(), value);
        assert_eq!(
            ReleaseManifest::parse(&serde_json::to_string(&value).unwrap()).unwrap(),
            value
        );
    }
}
