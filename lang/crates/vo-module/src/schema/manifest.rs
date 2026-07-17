use std::collections::BTreeSet;

use serde::Deserialize;

use crate::digest::Digest;
use crate::identity::{ArtifactId, ModulePath};
use crate::version::{DepConstraint, ExactVersion, ToolchainConstraint};
use crate::Error;

/// Fixed GitHub Release asset name for the compressed source package.
pub const SOURCE_ARCHIVE_ASSET_NAME: &str = "source.tar.gz";
pub const SOURCE_ARCHIVE_TOP_LEVEL_DIR: &str = "source";
/// Fixed top-level directory inside every source package.
pub const SOURCE_ARCHIVE_ROOT_DIR: &str = "source";

/// Parsed representation of the v2 `vo.release.json` protocol object.
///
/// The release manifest is the registry-facing authority for solving and
/// transport integrity. Exact installed file metadata lives exclusively in
/// `vo.package.json`, whose raw bytes are bound by [`ManifestPackage`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReleaseManifest {
    pub schema_version: u64,
    pub module: ModulePath,
    pub version: ExactVersion,
    pub commit: String,
    pub vo: ToolchainConstraint,
    pub dependencies: Vec<ManifestDependency>,
    pub source: ManifestSource,
    pub package: ManifestPackage,
    pub artifacts: Vec<ManifestArtifact>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ManifestDependency {
    pub module: ModulePath,
    pub constraint: DepConstraint,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ManifestSource {
    /// Canonical source-package release asset name.
    pub name: String,
    /// Byte size of the compressed source-package asset.
    pub size: u64,
    /// SHA-256 digest of the compressed source-package asset.
    pub digest: Digest,
}

/// Integrity metadata for the fixed `vo.package.json` release asset.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ManifestPackage {
    pub size: u64,
    pub digest: Digest,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ManifestArtifact {
    pub id: ArtifactId,
    pub size: u64,
    pub digest: Digest,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawManifest {
    schema_version: u64,
    module: String,
    version: String,
    commit: String,
    vo: String,
    #[serde(deserialize_with = "deserialize_manifest_dependencies")]
    dependencies: Vec<RawDependency>,
    source: RawSource,
    package: RawPackage,
    #[serde(deserialize_with = "deserialize_manifest_artifacts")]
    artifacts: Vec<RawArtifact>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawDependency {
    module: String,
    constraint: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawSource {
    name: String,
    size: u64,
    digest: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawPackage {
    size: u64,
    digest: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawArtifact {
    kind: String,
    target: String,
    name: String,
    size: u64,
    digest: String,
}

fn deserialize_manifest_dependencies<'de, D>(
    deserializer: D,
) -> Result<Vec<RawDependency>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    super::deserialize_bounded_vec(deserializer, crate::MAX_MODULE_DEPENDENCIES, "dependencies")
}

fn deserialize_manifest_artifacts<'de, D>(deserializer: D) -> Result<Vec<RawArtifact>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    super::deserialize_bounded_vec(deserializer, crate::MAX_MODULE_ARTIFACTS, "artifacts")
}

fn validate_dependencies_sorted_unique(dependencies: &[ManifestDependency]) -> Result<(), Error> {
    if dependencies
        .windows(2)
        .any(|pair| pair[0].module >= pair[1].module)
    {
        return Err(Error::ManifestParse(
            "dependencies must be unique and sorted by module path".to_string(),
        ));
    }
    Ok(())
}

fn validate_artifacts_sorted_unique(artifacts: &[ManifestArtifact]) -> Result<(), Error> {
    if artifacts.windows(2).any(|pair| pair[0].id >= pair[1].id) {
        return Err(Error::ManifestParse(
            "artifacts must be unique and sorted by (kind, target, name)".to_string(),
        ));
    }
    Ok(())
}

impl ReleaseManifest {
    pub fn parse(json: &str) -> Result<Self, Error> {
        if json.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(Error::ManifestParse(format!(
                "vo.release.json exceeds the {}-byte text limit",
                vo_common::vfs::MAX_TEXT_FILE_BYTES,
            )));
        }
        let raw: RawManifest = serde_json::from_str(json)
            .map_err(|error| Error::ManifestParse(format!("JSON parse error: {error}")))?;
        if raw.schema_version != 2 {
            return Err(Error::ManifestParse(format!(
                "unsupported schema_version: {}",
                raw.schema_version,
            )));
        }

        let module = ModulePath::parse(&raw.module)
            .map_err(|error| Error::ManifestParse(format!("module: {error}")))?;
        let version = ExactVersion::parse(&raw.version)
            .map_err(|error| Error::ManifestParse(format!("version: {error}")))?;
        if !module.accepts_version(&version) {
            return Err(Error::ManifestParse(format!(
                "version {version} is incompatible with module path {module}",
            )));
        }
        super::validate_commit_hash(&raw.commit)
            .map_err(|error| Error::ManifestParse(format!("commit: {error}")))?;
        let vo = ToolchainConstraint::parse(&raw.vo)
            .map_err(|error| Error::ManifestParse(format!("vo: {error}")))?;

        let mut dependencies = Vec::new();
        dependencies
            .try_reserve(raw.dependencies.len())
            .map_err(|_| {
                Error::ManifestParse("failed to reserve manifest dependencies".to_string())
            })?;
        for (index, dependency) in raw.dependencies.into_iter().enumerate() {
            let dependency_module = ModulePath::parse(&dependency.module).map_err(|error| {
                Error::ManifestParse(format!("dependencies[{index}].module: {error}"))
            })?;
            let constraint = DepConstraint::parse(&dependency.constraint).map_err(|error| {
                Error::ManifestParse(format!("dependencies[{index}].constraint: {error}"))
            })?;
            if dependency_module == module {
                return Err(Error::ManifestParse(format!(
                    "module {module} must not depend on itself",
                )));
            }
            let lower_bound = ExactVersion::from_semver(constraint.version.clone());
            if !dependency_module.accepts_version(&lower_bound) {
                return Err(Error::ManifestParse(format!(
                    "dependencies[{index}] constraint {constraint} is incompatible with module path {dependency_module}",
                )));
            }
            dependencies.push(ManifestDependency {
                module: dependency_module,
                constraint,
            });
        }
        validate_dependencies_sorted_unique(&dependencies)?;

        if raw.source.name != SOURCE_ARCHIVE_ASSET_NAME {
            return Err(Error::ManifestParse(format!(
                "source.name must be the fixed asset name {SOURCE_ARCHIVE_ASSET_NAME:?}"
            )));
        }
        if raw.source.size == 0 || raw.source.size > crate::MAX_SOURCE_ARCHIVE_BYTES {
            return Err(Error::ManifestParse(format!(
                "source.size {} must be within 1..={}",
                raw.source.size,
                crate::MAX_SOURCE_ARCHIVE_BYTES,
            )));
        }
        let source = ManifestSource {
            name: raw.source.name,
            size: raw.source.size,
            digest: Digest::parse(&raw.source.digest)
                .map_err(|error| Error::ManifestParse(format!("source.digest: {error}")))?,
        };

        let max_package_bytes =
            u64::try_from(vo_common::vfs::MAX_TEXT_FILE_BYTES).unwrap_or(u64::MAX);
        if raw.package.size == 0 || raw.package.size > max_package_bytes {
            return Err(Error::ManifestParse(format!(
                "package.size {} must be within 1..={max_package_bytes}",
                raw.package.size,
            )));
        }
        let package = ManifestPackage {
            size: raw.package.size,
            digest: Digest::parse(&raw.package.digest)
                .map_err(|error| Error::ManifestParse(format!("package.digest: {error}")))?,
        };

        let mut artifacts = Vec::new();
        let mut cache_paths = super::PortablePathSet::default();
        let mut release_asset_names = BTreeSet::from([
            super::portable_case_key("vo.release.json"),
            super::portable_case_key("vo.package.json"),
            super::portable_case_key(&source.name),
        ]);
        artifacts.try_reserve(raw.artifacts.len()).map_err(|_| {
            Error::ManifestParse("failed to reserve manifest artifacts".to_string())
        })?;
        for (index, artifact) in raw.artifacts.into_iter().enumerate() {
            if artifact.size == 0 || artifact.size > crate::MAX_MODULE_ARTIFACT_BYTES {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{index}].size {} must be within 1..={}",
                    artifact.size,
                    crate::MAX_MODULE_ARTIFACT_BYTES,
                )));
            }
            let id = ArtifactId {
                kind: artifact.kind,
                target: artifact.target,
                name: artifact.name,
            };
            id.validate()
                .map_err(|error| Error::ManifestParse(format!("artifacts[{index}]: {error}")))?;
            let cache_path = crate::artifact::artifact_cache_key(&id)
                .map_err(|error| Error::ManifestParse(format!("artifacts[{index}]: {error}")))?;
            if !cache_paths
                .insert_file(&cache_path)
                .map_err(|error| Error::ManifestParse(format!("artifacts[{index}]: {error}")))?
            {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{index}] duplicates cache path {cache_path}",
                )));
            }
            let asset_name = crate::artifact::artifact_release_asset_name(&id)
                .map_err(|error| Error::ManifestParse(format!("artifacts[{index}]: {error}")))?;
            if !release_asset_names.insert(super::portable_case_key(&asset_name)) {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{index}] reuses release asset name {asset_name}",
                )));
            }
            artifacts.push(ManifestArtifact {
                id,
                size: artifact.size,
                digest: Digest::parse(&artifact.digest).map_err(|error| {
                    Error::ManifestParse(format!("artifacts[{index}].digest: {error}"))
                })?,
            });
        }
        validate_artifacts_sorted_unique(&artifacts)?;

        let manifest = Self {
            schema_version: raw.schema_version,
            module,
            version,
            commit: raw.commit,
            vo,
            dependencies,
            source,
            package,
            artifacts,
        };
        let canonical = manifest.render()?;
        if canonical.as_bytes() != json.as_bytes() {
            return Err(Error::ManifestParse(
                "vo.release.json must use the canonical JSON encoding with one trailing LF"
                    .to_string(),
            ));
        }
        Ok(manifest)
    }

    fn validate_semantics(&self) -> Result<(), Error> {
        if self.schema_version != 2 {
            return Err(Error::ManifestParse(format!(
                "unsupported schema_version: {}",
                self.schema_version,
            )));
        }
        if !self.module.accepts_version(&self.version) {
            return Err(Error::ManifestParse(format!(
                "version {} is incompatible with module path {}",
                self.version, self.module,
            )));
        }
        self.version
            .validate()
            .map_err(|error| Error::ManifestParse(format!("version: {error}")))?;
        super::validate_commit_hash(&self.commit)
            .map_err(|error| Error::ManifestParse(format!("commit: {error}")))?;
        ToolchainConstraint::parse(&self.vo.to_string())
            .map_err(|error| Error::ManifestParse(format!("vo constraint is invalid: {error}")))?;
        if self.dependencies.len() > crate::MAX_MODULE_DEPENDENCIES {
            return Err(Error::ManifestParse(format!(
                "dependencies contains more than {} modules",
                crate::MAX_MODULE_DEPENDENCIES,
            )));
        }
        for (index, dependency) in self.dependencies.iter().enumerate() {
            DepConstraint::parse(&dependency.constraint.to_string()).map_err(|error| {
                Error::ManifestParse(format!(
                    "dependencies[{index}].constraint is invalid: {error}"
                ))
            })?;
            if dependency.module == self.module {
                return Err(Error::ManifestParse(format!(
                    "module {} must not depend on itself",
                    self.module,
                )));
            }
            let lower_bound = ExactVersion::from_semver(dependency.constraint.version.clone());
            if !dependency.module.accepts_version(&lower_bound) {
                return Err(Error::ManifestParse(format!(
                    "dependencies[{index}] constraint {} is incompatible with module path {}",
                    dependency.constraint, dependency.module,
                )));
            }
        }
        validate_dependencies_sorted_unique(&self.dependencies)?;

        if self.source.name != SOURCE_ARCHIVE_ASSET_NAME {
            return Err(Error::ManifestParse(format!(
                "source.name must be the fixed asset name {SOURCE_ARCHIVE_ASSET_NAME:?}"
            )));
        }
        if self.source.size == 0 || self.source.size > crate::MAX_SOURCE_ARCHIVE_BYTES {
            return Err(Error::ManifestParse(format!(
                "source.size {} must be within 1..={}",
                self.source.size,
                crate::MAX_SOURCE_ARCHIVE_BYTES,
            )));
        }
        let max_package_bytes =
            u64::try_from(vo_common::vfs::MAX_TEXT_FILE_BYTES).unwrap_or(u64::MAX);
        if self.package.size == 0 || self.package.size > max_package_bytes {
            return Err(Error::ManifestParse(format!(
                "package.size {} must be within 1..={max_package_bytes}",
                self.package.size,
            )));
        }
        if self.artifacts.len() > crate::MAX_MODULE_ARTIFACTS {
            return Err(Error::ManifestParse(format!(
                "artifacts contains more than {} entries",
                crate::MAX_MODULE_ARTIFACTS,
            )));
        }
        validate_artifacts_sorted_unique(&self.artifacts)?;
        let mut cache_paths = super::PortablePathSet::default();
        let mut release_asset_names = BTreeSet::from([
            super::portable_case_key("vo.release.json"),
            super::portable_case_key("vo.package.json"),
            super::portable_case_key(&self.source.name),
        ]);
        for (index, artifact) in self.artifacts.iter().enumerate() {
            artifact
                .id
                .validate()
                .map_err(|error| Error::ManifestParse(format!("artifacts[{index}]: {error}")))?;
            if artifact.size == 0 || artifact.size > crate::MAX_MODULE_ARTIFACT_BYTES {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{index}].size {} must be within 1..={}",
                    artifact.size,
                    crate::MAX_MODULE_ARTIFACT_BYTES,
                )));
            }
            let cache_path = crate::artifact::artifact_cache_key(&artifact.id)
                .map_err(|error| Error::ManifestParse(format!("artifacts[{index}]: {error}")))?;
            if !cache_paths
                .insert_file(&cache_path)
                .map_err(|error| Error::ManifestParse(format!("artifacts[{index}]: {error}")))?
            {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{index}] duplicates cache path {cache_path}",
                )));
            }
            let asset_name = crate::artifact::artifact_release_asset_name(&artifact.id)
                .map_err(|error| Error::ManifestParse(format!("artifacts[{index}]: {error}")))?;
            if !release_asset_names.insert(super::portable_case_key(&asset_name)) {
                return Err(Error::ManifestParse(format!(
                    "artifacts[{index}] reuses release asset name {asset_name}",
                )));
            }
        }
        Ok(())
    }

    /// Revalidate a typed value constructed by downstream Rust code,
    /// including the canonical wire-size ceiling.
    pub fn validate(&self) -> Result<(), Error> {
        self.render().map(|_| ())
    }

    /// Validate and serialize to the canonical field and collection order.
    /// Canonical JSON protocol objects end in exactly one LF byte.
    pub fn render(&self) -> Result<String, Error> {
        self.validate_semantics()?;
        let mut output = super::CanonicalJsonWriter::new(vo_common::vfs::MAX_TEXT_FILE_BYTES)
            .map_err(Error::ManifestParse)?;

        macro_rules! raw {
            ($value:expr) => {
                output.push_raw($value).map_err(Error::ManifestParse)?
            };
        }
        macro_rules! string {
            ($value:expr) => {
                output.push_string($value).map_err(Error::ManifestParse)?
            };
        }
        macro_rules! number {
            ($value:expr) => {
                output.push_u64($value).map_err(Error::ManifestParse)?
            };
        }

        raw!("{\n  \"schema_version\": ");
        number!(self.schema_version);
        raw!(",\n  \"module\": ");
        string!(self.module.as_str());
        raw!(",\n  \"version\": ");
        string!(&self.version.to_string());
        raw!(",\n  \"commit\": ");
        string!(&self.commit);
        raw!(",\n  \"vo\": ");
        string!(&self.vo.to_string());
        if self.dependencies.is_empty() {
            raw!(",\n  \"dependencies\": [],\n");
        } else {
            raw!(",\n  \"dependencies\": [\n");
            for (index, dependency) in self.dependencies.iter().enumerate() {
                raw!("    {\n      \"module\": ");
                string!(dependency.module.as_str());
                raw!(",\n      \"constraint\": ");
                string!(&dependency.constraint.to_string());
                raw!("\n    }");
                if index + 1 != self.dependencies.len() {
                    raw!(",");
                }
                raw!("\n");
            }
            raw!("  ],\n");
        }
        raw!("  \"source\": {\n    \"name\": ");
        string!(&self.source.name);
        raw!(",\n    \"size\": ");
        number!(self.source.size);
        raw!(",\n    \"digest\": ");
        string!(self.source.digest.as_str());
        raw!("\n  },\n  \"package\": {\n    \"size\": ");
        number!(self.package.size);
        raw!(",\n    \"digest\": ");
        string!(self.package.digest.as_str());
        if self.artifacts.is_empty() {
            raw!("\n  },\n  \"artifacts\": []\n}\n");
        } else {
            raw!("\n  },\n  \"artifacts\": [\n");
            for (index, artifact) in self.artifacts.iter().enumerate() {
                raw!("    {\n      \"kind\": ");
                string!(&artifact.id.kind);
                raw!(",\n      \"target\": ");
                string!(&artifact.id.target);
                raw!(",\n      \"name\": ");
                string!(&artifact.id.name);
                raw!(",\n      \"size\": ");
                number!(artifact.size);
                raw!(",\n      \"digest\": ");
                string!(artifact.digest.as_str());
                raw!("\n    }");
                if index + 1 != self.artifacts.len() {
                    raw!(",");
                }
                raw!("\n");
            }
            raw!("  ]\n}\n");
        }

        String::from_utf8(output.finish())
            .map_err(|error| Error::ManifestParse(format!("rendered JSON is not UTF-8: {error}")))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn digest(byte: char) -> String {
        format!("sha256:{}", byte.to_string().repeat(64))
    }

    fn sample_json() -> String {
        format!(
            r#"{{
  "schema_version": 2,
  "module": "github.com/acme/lib",
  "version": "1.2.3",
  "commit": "0123456789abcdef0123456789abcdef01234567",
  "vo": "^1.0.0",
  "dependencies": [
    {{
      "module": "github.com/acme/base",
      "constraint": "^1.0.0"
    }}
  ],
  "source": {{
    "name": "source.tar.gz",
    "size": 123,
    "digest": "{}"
  }},
  "package": {{
    "size": 456,
    "digest": "{}"
  }},
  "artifacts": [
    {{
      "kind": "extension-wasm",
      "target": "wasm32-unknown-unknown",
      "name": "数据.wasm",
      "size": 789,
      "digest": "{}"
    }}
  ]
}}
"#,
            digest('a'),
            digest('b'),
            digest('c'),
        )
    }

    #[test]
    fn renders_the_complete_v2_golden_and_round_trips() {
        let parsed = ReleaseManifest::parse(&sample_json()).unwrap();
        assert_eq!(parsed.schema_version, 2);
        assert_eq!(parsed.version.to_string(), "1.2.3");
        assert_eq!(parsed.dependencies.len(), 1);
        assert_eq!(parsed.source.size, 123);
        assert_eq!(parsed.package.size, 456);
        assert_eq!(parsed.artifacts.len(), 1);
        let rendered = parsed.render().unwrap();
        assert_eq!(rendered, sample_json());
        assert_eq!(ReleaseManifest::parse(&rendered).unwrap(), parsed);
        assert!(rendered.ends_with('\n'));
        assert!(!rendered.contains("module_root"));
        assert!(!rendered.contains("files_digest"));
        assert!(!rendered.contains("web_manifest"));
        assert!(!rendered.contains("\"require\""));
    }

    #[test]
    fn rejects_noncanonical_json_and_line_endings() {
        let canonical = sample_json();
        assert!(ReleaseManifest::parse(canonical.trim_end()).is_err());
        assert!(ReleaseManifest::parse(&canonical.replace('\n', "\r\n")).is_err());
        let compact: serde_json::Value = serde_json::from_str(&canonical).unwrap();
        assert!(ReleaseManifest::parse(&format!("{}\n", compact)).is_err());
    }

    #[test]
    fn rejects_prefixed_version_and_legacy_fields() {
        let mut value: serde_json::Value = serde_json::from_str(&sample_json()).unwrap();
        value["version"] = "v1.2.3".into();
        assert!(ReleaseManifest::parse(&serde_json::to_string(&value).unwrap()).is_err());

        let mut value: serde_json::Value = serde_json::from_str(&sample_json()).unwrap();
        value["module_root"] = ".".into();
        assert!(ReleaseManifest::parse(&serde_json::to_string(&value).unwrap()).is_err());
    }

    #[test]
    fn dependencies_and_artifacts_must_be_sorted_and_unique() {
        let mut value: serde_json::Value = serde_json::from_str(&sample_json()).unwrap();
        value["dependencies"] = serde_json::json!([
            {"module":"github.com/acme/z","constraint":"1.0.0"},
            {"module":"github.com/acme/a","constraint":"1.0.0"}
        ]);
        assert!(ReleaseManifest::parse(&serde_json::to_string(&value).unwrap()).is_err());

        let mut value: serde_json::Value = serde_json::from_str(&sample_json()).unwrap();
        value["artifacts"] = serde_json::json!([
            {"kind":"extension-wasm","target":"wasm32-unknown-unknown","name":"z.wasm","size":1,"digest":digest('c')},
            {"kind":"extension-wasm","target":"wasm32-unknown-unknown","name":"a.wasm","size":1,"digest":digest('d')}
        ]);
        assert!(ReleaseManifest::parse(&serde_json::to_string(&value).unwrap()).is_err());
    }

    #[test]
    fn package_binding_is_required_bounded_and_strict() {
        let mut value: serde_json::Value = serde_json::from_str(&sample_json()).unwrap();
        value.as_object_mut().unwrap().remove("package");
        assert!(ReleaseManifest::parse(&serde_json::to_string(&value).unwrap()).is_err());

        let mut value: serde_json::Value = serde_json::from_str(&sample_json()).unwrap();
        value["package"]["size"] = 0.into();
        assert!(ReleaseManifest::parse(&serde_json::to_string(&value).unwrap()).is_err());

        let mut value: serde_json::Value = serde_json::from_str(&sample_json()).unwrap();
        value["package"]["extra"] = true.into();
        assert!(ReleaseManifest::parse(&serde_json::to_string(&value).unwrap()).is_err());
    }

    #[test]
    fn source_archive_uses_the_single_fixed_asset_name() {
        for name in [
            "vo.release.json",
            "VO.PACKAGE.JSON",
            "lib-1.2.3.tar.gz",
            "SOURCE.TAR.GZ",
        ] {
            let mut value: serde_json::Value = serde_json::from_str(&sample_json()).unwrap();
            value["source"]["name"] = name.into();
            assert!(ReleaseManifest::parse(&serde_json::to_string(&value).unwrap()).is_err());
        }
    }

    #[test]
    fn artifacts_must_be_nonempty() {
        let mut value: serde_json::Value = serde_json::from_str(&sample_json()).unwrap();
        value["artifacts"] = serde_json::json!([{
            "kind": "extension-wasm",
            "target": "wasm32-unknown-unknown",
            "name": "demo.wasm",
            "size": 0,
            "digest": digest('c')
        }]);
        assert!(ReleaseManifest::parse(&serde_json::to_string(&value).unwrap()).is_err());
    }

    #[test]
    fn artifact_size_accepts_the_protocol_ceiling_and_rejects_the_next_byte() {
        assert_eq!(crate::MAX_MODULE_ARTIFACT_BYTES, 256 * 1024 * 1024);
        assert_eq!(
            crate::MAX_MODULE_ARTIFACT_BYTES_USIZE as u64,
            crate::MAX_MODULE_ARTIFACT_BYTES,
        );

        let mut manifest = ReleaseManifest::parse(&sample_json()).unwrap();
        manifest.artifacts[0].size = crate::MAX_MODULE_ARTIFACT_BYTES;
        manifest.validate().unwrap();

        manifest.artifacts[0].size = crate::MAX_MODULE_ARTIFACT_BYTES.checked_add(1).unwrap();
        let error = manifest.validate().unwrap_err().to_string();
        assert!(error.contains("must be within 1..=268435456"), "{error}");
    }

    #[test]
    fn artifact_count_reserves_the_three_fixed_github_assets() {
        assert_eq!(crate::MAX_MODULE_ARTIFACTS, 997);
        assert_eq!(crate::MAX_MODULE_ARTIFACTS + 3, 1_000);

        let mut manifest = ReleaseManifest::parse(&sample_json()).unwrap();
        manifest.artifacts = (0..=crate::MAX_MODULE_ARTIFACTS)
            .map(|index| ManifestArtifact {
                id: ArtifactId {
                    kind: "extension-native".to_string(),
                    target: "aarch64-apple-darwin".to_string(),
                    name: format!("artifact-{index:04}.dylib"),
                },
                size: 1,
                digest: Digest::from_sha256(index.to_string().as_bytes()),
            })
            .collect();
        let error = manifest.validate().unwrap_err().to_string();
        assert!(error.contains("more than 997 entries"), "{error}");
    }
}
