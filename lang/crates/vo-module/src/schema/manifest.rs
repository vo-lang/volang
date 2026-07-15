use crate::digest::Digest;
use crate::identity::{ArtifactId, ModulePath};
use crate::version::{DepConstraint, ExactVersion, ToolchainConstraint};
use crate::Error;
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;

/// Parsed representation of `vo.release.json`.
#[derive(Debug, Clone)]
pub struct ReleaseManifest {
    pub schema_version: u64,
    pub module: ModulePath,
    pub version: ExactVersion,
    pub commit: String,
    pub module_root: String,
    pub vo: ToolchainConstraint,
    pub require: Vec<ManifestRequire>,
    pub source: ManifestSource,
    pub web_manifest: ManifestWebManifest,
    pub artifacts: Vec<ManifestArtifact>,
}

#[derive(Debug, Clone)]
pub struct ManifestRequire {
    pub module: ModulePath,
    pub constraint: DepConstraint,
}

#[derive(Debug, Clone)]
pub struct ManifestSource {
    /// Canonical source-package asset name.
    pub name: String,
    /// Byte size of the compressed source-package asset.
    pub size: u64,
    /// SHA-256 digest of the compressed source-package asset.
    pub digest: Digest,
    /// Total UTF-8 content bytes in the canonical browser-readable file set.
    pub files_size: u64,
    /// SHA-256 digest of the canonical sorted `[{path,size,digest}]` file metadata.
    pub files_digest: Digest,
}

/// Integrity metadata for the fixed `vo.web.json` release asset.
#[derive(Debug, Clone)]
pub struct ManifestWebManifest {
    /// Byte size of `vo.web.json`.
    pub size: u64,
    /// SHA-256 digest of `vo.web.json`.
    pub digest: Digest,
}

#[derive(Debug, Clone)]
pub struct ManifestArtifact {
    pub id: ArtifactId,
    pub size: u64,
    pub digest: Digest,
}

// JSON wire format for serde
#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct RawManifest {
    schema_version: u64,
    module: String,
    version: String,
    commit: String,
    module_root: String,
    vo: String,
    #[serde(deserialize_with = "deserialize_manifest_requirements")]
    require: Vec<RawRequire>,
    source: RawSource,
    web_manifest: RawWebManifest,
    #[serde(default, deserialize_with = "deserialize_manifest_artifacts")]
    artifacts: Vec<RawArtifact>,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct RawRequire {
    module: String,
    constraint: String,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct RawSource {
    name: String,
    size: u64,
    digest: String,
    files_size: u64,
    files_digest: String,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct RawWebManifest {
    size: u64,
    digest: String,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct RawArtifact {
    kind: String,
    target: String,
    name: String,
    size: u64,
    digest: String,
}

fn deserialize_manifest_requirements<'de, D>(deserializer: D) -> Result<Vec<RawRequire>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    super::deserialize_bounded_vec(deserializer, crate::MAX_MODULE_DEPENDENCIES, "require")
}

fn deserialize_manifest_artifacts<'de, D>(deserializer: D) -> Result<Vec<RawArtifact>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    super::deserialize_bounded_vec(deserializer, crate::MAX_MODULE_ARTIFACTS, "artifacts")
}

fn validate_requires_sorted_unique(require: &[ManifestRequire]) -> Result<(), Error> {
    for w in require.windows(2) {
        if w[0].module >= w[1].module {
            return Err(Error::ManifestParse(
                "require must be unique and sorted by module path".into(),
            ));
        }
    }
    Ok(())
}

fn validate_artifacts_sorted_unique(artifacts: &[ManifestArtifact]) -> Result<(), Error> {
    for w in artifacts.windows(2) {
        if w[0].id >= w[1].id {
            return Err(Error::ManifestParse(
                "artifacts must be unique and sorted by (kind, target, name)".into(),
            ));
        }
    }
    Ok(())
}

fn canonicalize_requires(require: &mut [ManifestRequire]) -> Result<(), Error> {
    require.sort_by(|a, b| a.module.cmp(&b.module));
    for w in require.windows(2) {
        if w[0].module == w[1].module {
            return Err(Error::ManifestParse(
                "require must be unique by module path".into(),
            ));
        }
    }
    Ok(())
}

fn canonicalize_artifacts(artifacts: &mut [ManifestArtifact]) -> Result<(), Error> {
    artifacts.sort_by(|a, b| a.id.cmp(&b.id));
    for w in artifacts.windows(2) {
        if w[0].id == w[1].id {
            return Err(Error::ManifestParse(
                "artifacts must be unique by (kind, target, name)".into(),
            ));
        }
    }
    Ok(())
}

impl ReleaseManifest {
    /// Parse from JSON string.
    pub fn parse(json: &str) -> Result<Self, Error> {
        if json.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(Error::ManifestParse(format!(
                "vo.release.json exceeds the {}-byte text limit",
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )));
        }
        let raw: RawManifest = serde_json::from_str(json)
            .map_err(|e| Error::ManifestParse(format!("JSON parse error: {e}")))?;

        if raw.schema_version != 1 {
            return Err(Error::ManifestParse(format!(
                "unsupported schema_version: {}",
                raw.schema_version
            )));
        }

        let module = ModulePath::parse(&raw.module)
            .map_err(|e| Error::ManifestParse(format!("module: {e}")))?;
        let version = ExactVersion::parse(&raw.version)
            .map_err(|e| Error::ManifestParse(format!("version: {e}")))?;
        if !module.accepts_version(&version) {
            return Err(Error::ManifestParse(format!(
                "version {version} is incompatible with module path {module}"
            )));
        }
        if raw.module_root != module.module_root() {
            return Err(Error::ManifestParse(format!(
                "module_root '{}' does not match module path root '{}'",
                raw.module_root,
                module.module_root()
            )));
        }

        super::validate_commit_hash(&raw.commit)
            .map_err(|e| Error::ManifestParse(format!("commit: {e}")))?;

        let vo = ToolchainConstraint::parse(&raw.vo)
            .map_err(|e| Error::ManifestParse(format!("vo: {e}")))?;

        if raw.require.len() > crate::MAX_MODULE_DEPENDENCIES {
            return Err(Error::ManifestParse(format!(
                "require contains more than {} modules",
                crate::MAX_MODULE_DEPENDENCIES
            )));
        }
        let mut require = Vec::new();
        require
            .try_reserve(raw.require.len())
            .map_err(|_| Error::ManifestParse("failed to reserve manifest requirements".into()))?;
        for (i, r) in raw.require.iter().enumerate() {
            let mp = ModulePath::parse(&r.module)
                .map_err(|e| Error::ManifestParse(format!("require[{i}].module: {e}")))?;
            let constraint = DepConstraint::parse(&r.constraint)
                .map_err(|e| Error::ManifestParse(format!("require[{i}].constraint: {e}")))?;
            if mp == module {
                return Err(Error::ManifestParse(format!(
                    "module {module} must not require itself"
                )));
            }
            let lower_bound = ExactVersion::from_semver(constraint.version.clone());
            if !mp.accepts_version(&lower_bound) {
                return Err(Error::ManifestParse(format!(
                    "require[{i}] constraint {constraint} is incompatible with module path {mp}"
                )));
            }
            require.push(ManifestRequire {
                module: mp,
                constraint,
            });
        }
        validate_requires_sorted_unique(&require)?;

        if raw.source.size == 0 || raw.source.size > crate::MAX_SOURCE_ARCHIVE_BYTES {
            return Err(Error::ManifestParse(format!(
                "source.size {} must be within 1..={}",
                raw.source.size,
                crate::MAX_SOURCE_ARCHIVE_BYTES
            )));
        }
        if raw.source.files_size
            > u64::try_from(vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES).unwrap_or(u64::MAX)
        {
            return Err(Error::ManifestParse(format!(
                "source.files_size {} exceeds the {}-byte limit",
                raw.source.files_size,
                vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES
            )));
        }
        if raw.source.files_size == 0 {
            return Err(Error::ManifestParse(
                "source.files_size must include the non-empty published vo.mod".to_string(),
            ));
        }
        super::validate_file_name(&raw.source.name)
            .map_err(|error| Error::ManifestParse(format!("source.name {error}")))?;
        let source = ManifestSource {
            name: raw.source.name.clone(),
            size: raw.source.size,
            digest: Digest::parse(&raw.source.digest)
                .map_err(|e| Error::ManifestParse(format!("source.digest: {e}")))?,
            files_size: raw.source.files_size,
            files_digest: Digest::parse(&raw.source.files_digest)
                .map_err(|e| Error::ManifestParse(format!("source.files_digest: {e}")))?,
        };

        let max_web_manifest_bytes =
            u64::try_from(vo_common::vfs::MAX_TEXT_FILE_BYTES).unwrap_or(u64::MAX);
        if raw.web_manifest.size > max_web_manifest_bytes {
            return Err(Error::ManifestParse(format!(
                "web_manifest.size {} exceeds the {}-byte limit",
                raw.web_manifest.size,
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )));
        }
        if raw.web_manifest.size == 0 {
            return Err(Error::ManifestParse(
                "web_manifest.size must describe a non-empty vo.web.json".to_string(),
            ));
        }
        let web_manifest = ManifestWebManifest {
            size: raw.web_manifest.size,
            digest: Digest::parse(&raw.web_manifest.digest)
                .map_err(|e| Error::ManifestParse(format!("web_manifest.digest: {e}")))?,
        };

        if raw.artifacts.len() > crate::MAX_MODULE_ARTIFACTS {
            return Err(Error::ManifestParse(format!(
                "artifacts contains more than {} entries",
                crate::MAX_MODULE_ARTIFACTS
            )));
        }
        let mut artifacts = Vec::new();
        let mut artifact_paths = super::PortablePathSet::default();
        artifacts
            .try_reserve(raw.artifacts.len())
            .map_err(|_| Error::ManifestParse("failed to reserve manifest artifacts".into()))?;
        for (i, a) in raw.artifacts.iter().enumerate() {
            if a.size > crate::MAX_MODULE_ARTIFACT_BYTES {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{i}].size {} exceeds the {}-byte limit",
                    a.size,
                    crate::MAX_MODULE_ARTIFACT_BYTES
                )));
            }
            let id = ArtifactId {
                kind: a.kind.clone(),
                target: a.target.clone(),
                name: a.name.clone(),
            };
            id.validate()
                .map_err(|error| Error::ManifestParse(format!("artifacts[{i}]: {error}")))?;
            let cache_path = crate::artifact::artifact_cache_key(&id)
                .map_err(|error| Error::ManifestParse(format!("artifacts[{i}]: {error}")))?;
            if !artifact_paths
                .insert_file(&cache_path)
                .map_err(|error| Error::ManifestParse(format!("artifacts[{i}]: {error}")))?
            {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{i}] duplicates cache path {cache_path}"
                )));
            }
            let digest = Digest::parse(&a.digest)
                .map_err(|e| Error::ManifestParse(format!("artifacts[{i}].digest: {e}")))?;
            artifacts.push(ManifestArtifact {
                id,
                size: a.size,
                digest,
            });
        }
        validate_artifacts_sorted_unique(&artifacts)?;

        Ok(ReleaseManifest {
            schema_version: raw.schema_version,
            module,
            version,
            commit: raw.commit,
            module_root: raw.module_root,
            vo,
            require,
            source,
            web_manifest,
            artifacts,
        })
    }

    /// Validate every invariant normally established by parsing a release
    /// manifest, without allocating a duplicate wire representation.
    pub fn validate(&self) -> Result<(), Error> {
        if self.schema_version != 1 {
            return Err(Error::ManifestParse(format!(
                "unsupported schema_version: {}",
                self.schema_version
            )));
        }
        if !self.module.accepts_version(&self.version) {
            return Err(Error::ManifestParse(format!(
                "version {} is incompatible with module path {}",
                self.version, self.module
            )));
        }
        if self.module_root != self.module.module_root() {
            return Err(Error::ManifestParse(format!(
                "module_root '{}' does not match module path root '{}'",
                self.module_root,
                self.module.module_root()
            )));
        }
        super::validate_commit_hash(&self.commit)
            .map_err(|error| Error::ManifestParse(format!("commit: {error}")))?;
        if self.require.len() > crate::MAX_MODULE_DEPENDENCIES {
            return Err(Error::ManifestParse(format!(
                "require contains more than {} modules",
                crate::MAX_MODULE_DEPENDENCIES
            )));
        }
        if self.artifacts.len() > crate::MAX_MODULE_ARTIFACTS {
            return Err(Error::ManifestParse(format!(
                "artifacts contains more than {} entries",
                crate::MAX_MODULE_ARTIFACTS
            )));
        }
        let mut required_modules = BTreeSet::new();
        for (index, requirement) in self.require.iter().enumerate() {
            if requirement.module == self.module {
                return Err(Error::ManifestParse(format!(
                    "module {} must not require itself",
                    self.module
                )));
            }
            if !required_modules.insert(&requirement.module) {
                return Err(Error::ManifestParse(format!(
                    "require[{index}] duplicates module {}",
                    requirement.module
                )));
            }
            let lower_bound = ExactVersion::from_semver(requirement.constraint.version.clone());
            if !requirement.module.accepts_version(&lower_bound) {
                return Err(Error::ManifestParse(format!(
                    "require[{index}] constraint {} is incompatible with module path {}",
                    requirement.constraint, requirement.module
                )));
            }
        }
        if self.source.size == 0 || self.source.size > crate::MAX_SOURCE_ARCHIVE_BYTES {
            return Err(Error::ManifestParse(format!(
                "source.size {} must be within 1..={}",
                self.source.size,
                crate::MAX_SOURCE_ARCHIVE_BYTES
            )));
        }
        if self.source.files_size == 0
            || self.source.files_size
                > u64::try_from(vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES).unwrap_or(u64::MAX)
        {
            return Err(Error::ManifestParse(format!(
                "source.files_size {} must be within 1..={}",
                self.source.files_size,
                vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES
            )));
        }
        super::validate_file_name(&self.source.name)
            .map_err(|error| Error::ManifestParse(format!("source.name {error}")))?;
        if self.web_manifest.size == 0
            || self.web_manifest.size
                > u64::try_from(vo_common::vfs::MAX_TEXT_FILE_BYTES).unwrap_or(u64::MAX)
        {
            return Err(Error::ManifestParse(format!(
                "web_manifest.size {} must be within 1..={}",
                self.web_manifest.size,
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )));
        }
        let mut artifact_ids = BTreeSet::new();
        let mut artifact_paths = super::PortablePathSet::default();
        for (index, artifact) in self.artifacts.iter().enumerate() {
            artifact
                .id
                .validate()
                .map_err(|error| Error::ManifestParse(format!("artifacts[{index}]: {error}")))?;
            if artifact.size > crate::MAX_MODULE_ARTIFACT_BYTES {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{index}].size {} exceeds the {}-byte limit",
                    artifact.size,
                    crate::MAX_MODULE_ARTIFACT_BYTES
                )));
            }
            if !artifact_ids.insert(&artifact.id) {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{index}] duplicates {}",
                    artifact.id
                )));
            }
            let cache_path = crate::artifact::artifact_cache_key(&artifact.id)
                .map_err(|error| Error::ManifestParse(format!("artifacts[{index}]: {error}")))?;
            if !artifact_paths
                .insert_file(&cache_path)
                .map_err(|error| Error::ManifestParse(format!("artifacts[{index}]: {error}")))?
            {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{index}] duplicates cache path {cache_path}"
                )));
            }
        }
        Ok(())
    }

    /// Validate and serialize to canonical JSON.
    pub fn render(&self) -> Result<String, Error> {
        self.validate()?;
        let mut require = self.require.clone();
        canonicalize_requires(&mut require)?;
        let mut artifacts = self.artifacts.clone();
        canonicalize_artifacts(&mut artifacts)?;

        let raw = RawManifest {
            schema_version: self.schema_version,
            module: self.module.as_str().to_string(),
            version: self.version.to_string(),
            commit: self.commit.clone(),
            module_root: self.module_root.clone(),
            vo: self.vo.to_string(),
            require: require
                .iter()
                .map(|r| RawRequire {
                    module: r.module.as_str().to_string(),
                    constraint: r.constraint.to_string(),
                })
                .collect(),
            source: RawSource {
                name: self.source.name.clone(),
                size: self.source.size,
                digest: self.source.digest.as_str().to_string(),
                files_size: self.source.files_size,
                files_digest: self.source.files_digest.as_str().to_string(),
            },
            web_manifest: RawWebManifest {
                size: self.web_manifest.size,
                digest: self.web_manifest.digest.as_str().to_string(),
            },
            artifacts: artifacts
                .iter()
                .map(|a| RawArtifact {
                    kind: a.id.kind.clone(),
                    target: a.id.target.clone(),
                    name: a.id.name.clone(),
                    size: a.size,
                    digest: a.digest.as_str().to_string(),
                })
                .collect(),
        };
        let mut output = super::BoundedBytesOutput::new(vo_common::vfs::MAX_TEXT_FILE_BYTES)
            .map_err(Error::ManifestParse)?;
        let serialized = serde_json::to_writer_pretty(&mut output, &raw);
        if let Err(error) = serialized {
            return Err(Error::ManifestParse(format!("JSON render error: {error}")));
        }
        let rendered = String::from_utf8(output.finish().map_err(Error::ManifestParse)?)
            .map_err(|error| Error::ManifestParse(format!("JSON UTF-8 render error: {error}")))?;

        // Public fields allow callers to construct manifests directly. Run
        // the canonical wire value through the registry validator so render
        // cannot publish an internally inconsistent manifest.
        Self::parse(&rendered)?;
        Ok(rendered)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_json() -> String {
        r#"{
  "schema_version": 1,
  "module": "github.com/vo-lang/vogui",
  "version": "v0.4.2",
  "commit": "9b6d4a8d2d5a6f2d4c0c2d9e6b3a1f0e0c4d1e22",
  "module_root": ".",
  "vo": "^1.0.0",
  "require": [
    { "module": "github.com/vo-lang/voplay", "constraint": "^0.7.0" }
  ],
  "source": {
    "name": "vogui-v0.4.2-source.tar.gz",
    "size": 54321,
    "digest": "sha256:81c181c181c181c181c181c181c181c181c181c181c181c181c181c181c181c1",
    "files_size": 37,
    "files_digest": "sha256:04e07c8d1db1df7c86bc43c5ff9672b6622e7d7b5fef22b4397ca6588073aca5"
  },
  "web_manifest": {
    "size": 321,
    "digest": "sha256:d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3"
  },
  "artifacts": [
    {
      "kind": "extension-wasm",
      "target": "wasm32-unknown-unknown",
      "name": "vogui-wasm.wasm",
      "size": 123456,
      "digest": "sha256:6f926f926f926f926f926f926f926f926f926f926f926f926f926f926f926f92"
    }
  ]
}"#
        .to_string()
    }

    #[test]
    fn test_parse_manifest() {
        let m = ReleaseManifest::parse(&sample_json()).unwrap();
        assert_eq!(m.schema_version, 1);
        assert_eq!(m.module.as_str(), "github.com/vo-lang/vogui");
        assert_eq!(m.version.to_string(), "v0.4.2");
        assert_eq!(m.require.len(), 1);
        assert_eq!(m.artifacts.len(), 1);
        assert_eq!(m.source.files_size, 37);
        assert_eq!(
            m.source.files_digest,
            Digest::parse(
                "sha256:04e07c8d1db1df7c86bc43c5ff9672b6622e7d7b5fef22b4397ca6588073aca5"
            )
            .unwrap()
        );
        assert_eq!(m.web_manifest.size, 321);
        assert_eq!(
            m.web_manifest.digest,
            Digest::parse(
                "sha256:d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3"
            )
            .unwrap()
        );
    }

    #[test]
    fn test_roundtrip() {
        let m = ReleaseManifest::parse(&sample_json()).unwrap();
        let json = m.render().unwrap();
        let m2 = ReleaseManifest::parse(&json).unwrap();
        assert_eq!(m2.module.as_str(), m.module.as_str());
        assert_eq!(m2.version.to_string(), m.version.to_string());
        assert_eq!(m2.web_manifest.size, m.web_manifest.size);
        assert_eq!(m2.web_manifest.digest, m.web_manifest.digest);
    }

    #[test]
    fn test_reject_bad_schema_version() {
        let json = sample_json().replace("\"schema_version\": 1", "\"schema_version\": 99");
        assert!(ReleaseManifest::parse(&json).is_err());
    }

    #[test]
    fn test_reject_unknown_fields() {
        let json = sample_json().replacen(
            "\"schema_version\": 1,",
            "\"schema_version\": 1,\n  \"unknown\": true,",
            1,
        );
        let error = ReleaseManifest::parse(&json).unwrap_err().to_string();
        assert!(error.contains("unknown field `unknown`"));
    }

    #[test]
    fn source_file_set_metadata_is_required_and_bounded() {
        let mut value = serde_json::from_str::<serde_json::Value>(&sample_json()).unwrap();
        value["source"]
            .as_object_mut()
            .unwrap()
            .remove("files_digest");
        let error = ReleaseManifest::parse(&serde_json::to_string(&value).unwrap())
            .unwrap_err()
            .to_string();
        assert!(error.contains("missing field `files_digest`"), "{error}");

        let mut value = serde_json::from_str::<serde_json::Value>(&sample_json()).unwrap();
        value["source"]["files_size"] = serde_json::Value::from(
            u64::try_from(vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES).unwrap() + 1,
        );
        let error = ReleaseManifest::parse(&serde_json::to_string(&value).unwrap())
            .unwrap_err()
            .to_string();
        assert!(error.contains("source.files_size"), "{error}");
    }

    #[test]
    fn source_archive_size_must_be_positive_and_bounded() {
        for invalid_size in [0, crate::MAX_SOURCE_ARCHIVE_BYTES + 1] {
            let mut value = serde_json::from_str::<serde_json::Value>(&sample_json()).unwrap();
            value["source"]["size"] = serde_json::Value::from(invalid_size);
            let error = ReleaseManifest::parse(&serde_json::to_string(&value).unwrap())
                .unwrap_err()
                .to_string();
            assert!(error.contains("source.size"), "{error}");
        }
    }

    #[test]
    fn web_manifest_metadata_is_required() {
        let mut value = serde_json::from_str::<serde_json::Value>(&sample_json()).unwrap();
        value.as_object_mut().unwrap().remove("web_manifest");
        let error = ReleaseManifest::parse(&serde_json::to_string(&value).unwrap())
            .unwrap_err()
            .to_string();
        assert!(error.contains("missing field `web_manifest`"), "{error}");

        for field in ["size", "digest"] {
            let mut value = serde_json::from_str::<serde_json::Value>(&sample_json()).unwrap();
            value["web_manifest"].as_object_mut().unwrap().remove(field);
            let error = ReleaseManifest::parse(&serde_json::to_string(&value).unwrap())
                .unwrap_err()
                .to_string();
            assert!(
                error.contains(&format!("missing field `{field}`")),
                "{error}"
            );
        }
    }

    #[test]
    fn web_manifest_metadata_rejects_invalid_values_and_unknown_fields() {
        let mut value = serde_json::from_str::<serde_json::Value>(&sample_json()).unwrap();
        value["web_manifest"]["digest"] = serde_json::Value::String("invalid".to_string());
        let error = ReleaseManifest::parse(&serde_json::to_string(&value).unwrap())
            .unwrap_err()
            .to_string();
        assert!(error.contains("web_manifest.digest"), "{error}");

        let mut value = serde_json::from_str::<serde_json::Value>(&sample_json()).unwrap();
        value["web_manifest"]["unknown"] = serde_json::Value::Bool(true);
        let error = ReleaseManifest::parse(&serde_json::to_string(&value).unwrap())
            .unwrap_err()
            .to_string();
        assert!(error.contains("unknown field `unknown`"), "{error}");
    }

    #[test]
    fn web_manifest_size_is_bounded() {
        let max_size = u64::try_from(vo_common::vfs::MAX_TEXT_FILE_BYTES).unwrap();

        let mut value = serde_json::from_str::<serde_json::Value>(&sample_json()).unwrap();
        value["web_manifest"]["size"] = serde_json::Value::from(max_size);
        ReleaseManifest::parse(&serde_json::to_string(&value).unwrap()).unwrap();

        value["web_manifest"]["size"] = serde_json::Value::from(max_size + 1);
        let error = ReleaseManifest::parse(&serde_json::to_string(&value).unwrap())
            .unwrap_err()
            .to_string();
        assert!(error.contains("web_manifest.size"), "{error}");
        assert!(error.contains("exceeds"), "{error}");
    }

    #[test]
    fn test_reject_module_root_mismatch() {
        let json = sample_json().replace("\"module_root\": \".\"", "\"module_root\": \"nested\"");
        let error = ReleaseManifest::parse(&json).unwrap_err().to_string();
        assert!(error.contains("module_root 'nested' does not match"));
    }

    #[test]
    fn test_reject_version_incompatible_with_module_path() {
        let json = sample_json().replace("\"version\": \"v0.4.2\"", "\"version\": \"v2.4.2\"");
        let error = ReleaseManifest::parse(&json).unwrap_err().to_string();
        assert!(error.contains("is incompatible with module path"));
    }

    #[test]
    fn test_reject_source_name_with_path_components() {
        let json = sample_json().replace(
            "\"name\": \"vogui-v0.4.2-source.tar.gz\"",
            "\"name\": \"../source.tar.gz\"",
        );
        let error = ReleaseManifest::parse(&json).unwrap_err().to_string();
        assert!(error.contains("source.name must be a non-empty file name"));
    }

    #[test]
    fn test_reject_wasm_artifact_with_non_wasm_target() {
        let json = sample_json().replace(
            "\"target\": \"wasm32-unknown-unknown\"",
            "\"target\": \"x86_64-unknown-linux-gnu\"",
        );
        let error = ReleaseManifest::parse(&json).unwrap_err().to_string();
        assert!(error.contains("must target wasm32-unknown-unknown"));
    }

    #[test]
    fn test_reject_artifact_cache_paths_that_alias_by_case() {
        let mut value = serde_json::from_str::<serde_json::Value>(&sample_json()).unwrap();
        let original = value["artifacts"][0].clone();
        let mut alias = original.clone();
        alias["name"] = serde_json::Value::String("VOGUI-WASM.WASM".to_string());
        value["artifacts"] = serde_json::Value::Array(vec![alias, original]);

        let error = ReleaseManifest::parse(&serde_json::to_string(&value).unwrap())
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("conflicts with portable spelling"),
            "{error}"
        );
    }

    #[test]
    fn test_reject_unsorted_require() {
        let json = r#"{
  "schema_version": 1,
  "module": "github.com/acme/app",
  "version": "v1.0.0",
  "commit": "1111111111111111111111111111111111111111",
  "module_root": ".",
  "vo": "^1.0.0",
  "require": [
    { "module": "github.com/z/z", "constraint": "^1.0.0" },
    { "module": "github.com/a/a", "constraint": "^1.0.0" }
  ],
  "source": {
    "name": "app-v1.0.0-source.tar.gz",
    "size": 100,
    "digest": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "files_size": 37,
    "files_digest": "sha256:04e07c8d1db1df7c86bc43c5ff9672b6622e7d7b5fef22b4397ca6588073aca5"
  },
  "web_manifest": {
    "size": 100,
    "digest": "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
  },
  "artifacts": []
}"#;
        assert!(ReleaseManifest::parse(json).is_err());
    }

    #[test]
    fn test_reject_unsorted_artifacts() {
        let json = r#"{
  "schema_version": 1,
  "module": "github.com/acme/app",
  "version": "v1.0.0",
  "commit": "1111111111111111111111111111111111111111",
  "module_root": ".",
  "vo": "^1.0.0",
  "require": [],
  "source": {
    "name": "app-v1.0.0-source.tar.gz",
    "size": 100,
    "digest": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "files_size": 37,
    "files_digest": "sha256:04e07c8d1db1df7c86bc43c5ff9672b6622e7d7b5fef22b4397ca6588073aca5"
  },
  "web_manifest": {
    "size": 100,
    "digest": "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
  },
  "artifacts": [
    {
      "kind": "extension-wasm",
      "target": "wasm32-unknown-unknown",
      "name": "z-demo.wasm",
      "size": 100,
      "digest": "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    },
    {
      "kind": "extension-js-glue",
      "target": "wasm32-unknown-unknown",
      "name": "a-demo.js",
      "size": 200,
      "digest": "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
    }
  ]
}"#;
        assert!(ReleaseManifest::parse(json).is_err());
    }

    #[test]
    fn test_reject_duplicate_require() {
        let json = r#"{
  "schema_version": 1,
  "module": "github.com/acme/app",
  "version": "v1.0.0",
  "commit": "1111111111111111111111111111111111111111",
  "module_root": ".",
  "vo": "^1.0.0",
  "require": [
    { "module": "github.com/a/a", "constraint": "^1.0.0" },
    { "module": "github.com/a/a", "constraint": "^1.1.0" }
  ],
  "source": {
    "name": "app-v1.0.0-source.tar.gz",
    "size": 100,
    "digest": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "files_size": 37,
    "files_digest": "sha256:04e07c8d1db1df7c86bc43c5ff9672b6622e7d7b5fef22b4397ca6588073aca5"
  },
  "web_manifest": {
    "size": 100,
    "digest": "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
  },
  "artifacts": []
}"#;
        assert!(ReleaseManifest::parse(json).is_err());
    }

    #[test]
    fn test_reject_duplicate_artifacts() {
        let json = r#"{
  "schema_version": 1,
  "module": "github.com/acme/app",
  "version": "v1.0.0",
  "commit": "1111111111111111111111111111111111111111",
  "module_root": ".",
  "vo": "^1.0.0",
  "require": [],
  "source": {
    "name": "app-v1.0.0-source.tar.gz",
    "size": 100,
    "digest": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "files_size": 37,
    "files_digest": "sha256:04e07c8d1db1df7c86bc43c5ff9672b6622e7d7b5fef22b4397ca6588073aca5"
  },
  "web_manifest": {
    "size": 100,
    "digest": "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
  },
  "artifacts": [
    {
      "kind": "extension-wasm",
      "target": "wasm32-unknown-unknown",
      "name": "demo.wasm",
      "size": 100,
      "digest": "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    },
    {
      "kind": "extension-wasm",
      "target": "wasm32-unknown-unknown",
      "name": "demo.wasm",
      "size": 200,
      "digest": "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
    }
  ]
}"#;
        assert!(ReleaseManifest::parse(json).is_err());
    }

    #[test]
    fn test_render_canonicalizes_unsorted_fields() {
        let mut manifest = ReleaseManifest::parse(&sample_json()).unwrap();
        manifest.require = vec![
            ManifestRequire {
                module: ModulePath::parse("github.com/vo-lang/voplay").unwrap(),
                constraint: DepConstraint::parse("^0.7.0").unwrap(),
            },
            ManifestRequire {
                module: ModulePath::parse("github.com/acme/core").unwrap(),
                constraint: DepConstraint::parse("^1.0.0").unwrap(),
            },
        ];
        manifest.artifacts = vec![
            ManifestArtifact {
                id: ArtifactId {
                    kind: "extension-wasm".to_string(),
                    target: "wasm32-unknown-unknown".to_string(),
                    name: "z-demo.wasm".to_string(),
                },
                size: 123,
                digest: Digest::parse(
                    "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                )
                .unwrap(),
            },
            ManifestArtifact {
                id: ArtifactId {
                    kind: "extension-js-glue".to_string(),
                    target: "wasm32-unknown-unknown".to_string(),
                    name: "a-demo.js".to_string(),
                },
                size: 456,
                digest: Digest::parse(
                    "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                )
                .unwrap(),
            },
        ];

        let rendered = manifest.render().unwrap();
        let core_pos = rendered
            .find("\"module\": \"github.com/acme/core\"")
            .unwrap();
        let voplay_pos = rendered
            .find("\"module\": \"github.com/vo-lang/voplay\"")
            .unwrap();
        let js_pos = rendered.find("\"name\": \"a-demo.js\"").unwrap();
        let wasm_pos = rendered.find("\"name\": \"z-demo.wasm\"").unwrap();

        assert!(core_pos < voplay_pos);
        assert!(js_pos < wasm_pos);
    }

    #[test]
    fn render_rejects_invalid_public_values_without_panicking() {
        let mut manifest = ReleaseManifest::parse(&sample_json()).unwrap();
        manifest.commit = "invalid".to_string();
        assert!(manifest.render().is_err());

        let mut manifest = ReleaseManifest::parse(&sample_json()).unwrap();
        manifest.require.push(manifest.require[0].clone());
        assert!(manifest.render().is_err());

        let mut manifest = ReleaseManifest::parse(&sample_json()).unwrap();
        manifest.web_manifest.size =
            u64::try_from(vo_common::vfs::MAX_TEXT_FILE_BYTES).unwrap() + 1;
        assert!(manifest.render().is_err());
    }
}
