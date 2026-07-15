use std::path::PathBuf;

use serde::Deserialize;

use crate::digest::Digest;
use crate::ext_manifest::{
    ExtensionManifest, WasmExtensionManifest, WebProjectManifest, WebRuntimeManifest,
};
use crate::identity::{ArtifactId, ModulePath};
use crate::schema::manifest::{ManifestRequire, ReleaseManifest};
use crate::schema::modfile::{ModFile, Require};
use crate::version::{DepConstraint, ExactVersion, ToolchainConstraint};
use crate::{Error, Result};

use super::{canonical_source_file_set, PortablePathSet, SourceFileEntry};

/// Typed, validated `vo.web.json` protocol object shared by native and browser
/// installers. All fields are public so consumers can fetch declared files and
/// artifacts after parsing through [`WebManifest::parse`].
#[derive(Debug, Clone)]
pub struct WebManifest {
    pub schema_version: u64,
    pub module: ModulePath,
    pub version: ExactVersion,
    pub commit: String,
    pub module_root: String,
    pub vo: ToolchainConstraint,
    pub require: Vec<ManifestRequire>,
    pub source_digest: Digest,
    pub source: Vec<SourceFileEntry>,
    pub artifacts: Vec<WebManifestArtifact>,
    pub web: Option<WebProjectManifest>,
    pub extension: Option<WebManifestExtension>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WebManifestArtifact {
    pub id: ArtifactId,
    pub path: String,
    pub size: u64,
    pub digest: Digest,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WebManifestExtension {
    pub name: String,
    pub include: Vec<PathBuf>,
    pub wasm: Option<WasmExtensionManifest>,
    pub web: Option<WebRuntimeManifest>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawWebManifest {
    schema_version: u64,
    module: String,
    version: String,
    commit: String,
    module_root: String,
    vo: String,
    #[serde(deserialize_with = "deserialize_web_requirements")]
    require: Vec<RawRequire>,
    source_digest: String,
    #[serde(deserialize_with = "deserialize_web_sources")]
    source: Vec<SourceFileEntry>,
    #[serde(deserialize_with = "deserialize_web_artifacts")]
    artifacts: Vec<RawArtifact>,
    web: Option<RawWebProject>,
    extension: Option<RawExtension>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawRequire {
    module: String,
    constraint: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawArtifact {
    kind: String,
    target: String,
    name: String,
    path: String,
    size: u64,
    digest: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawWebProject {
    entry: Option<String>,
    #[serde(deserialize_with = "deserialize_metadata_paths")]
    include: Vec<PathBuf>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawExtension {
    name: String,
    #[serde(deserialize_with = "deserialize_metadata_paths")]
    include: Vec<PathBuf>,
    wasm: Option<WasmExtensionManifest>,
    web: Option<WebRuntimeManifest>,
}

fn deserialize_web_requirements<'de, D>(
    deserializer: D,
) -> std::result::Result<Vec<RawRequire>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    super::deserialize_bounded_vec(
        deserializer,
        crate::MAX_MODULE_DEPENDENCIES,
        "vo.web.json require",
    )
}

fn deserialize_web_sources<'de, D>(
    deserializer: D,
) -> std::result::Result<Vec<SourceFileEntry>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    super::deserialize_bounded_vec(
        deserializer,
        vo_common::vfs::MAX_PACKAGE_SOURCE_FILES,
        "vo.web.json source",
    )
}

fn deserialize_web_artifacts<'de, D>(
    deserializer: D,
) -> std::result::Result<Vec<RawArtifact>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    super::deserialize_bounded_vec(
        deserializer,
        crate::MAX_MODULE_ARTIFACTS,
        "vo.web.json artifacts",
    )
}

fn deserialize_metadata_paths<'de, D>(
    deserializer: D,
) -> std::result::Result<Vec<PathBuf>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    super::deserialize_bounded_vec(
        deserializer,
        crate::MAX_MODULE_METADATA_ENTRIES,
        "vo.web.json metadata path array",
    )
}

impl WebManifest {
    pub fn parse(bytes: &[u8]) -> Result<Self> {
        if bytes.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(invalid(format!(
                "vo.web.json exceeds the {}-byte text limit",
                vo_common::vfs::MAX_TEXT_FILE_BYTES,
            )));
        }
        let raw: RawWebManifest = serde_json::from_slice(bytes)
            .map_err(|source| invalid(format!("vo.web.json JSON parse error: {source}")))?;
        if raw.schema_version != 1 {
            return Err(invalid(format!(
                "vo.web.json has unsupported schema_version {}",
                raw.schema_version,
            )));
        }

        let module = ModulePath::parse(&raw.module)
            .map_err(|source| invalid(format!("vo.web.json module: {source}")))?;
        let version = ExactVersion::parse(&raw.version)
            .map_err(|source| invalid(format!("vo.web.json version: {source}")))?;
        if !module.accepts_version(&version) {
            return Err(invalid(format!(
                "vo.web.json version {version} is incompatible with module path {module}"
            )));
        }
        if raw.module_root != module.module_root() {
            return Err(invalid(format!(
                "vo.web.json module_root {:?} does not match module path root {:?}",
                raw.module_root,
                module.module_root(),
            )));
        }
        super::validate_commit_hash(&raw.commit)
            .map_err(|source| invalid(format!("vo.web.json commit: {source}")))?;
        let vo = ToolchainConstraint::parse(&raw.vo)
            .map_err(|source| invalid(format!("vo.web.json vo: {source}")))?;
        let require = parse_requirements(&module, raw.require)?;
        let source_digest = Digest::parse(&raw.source_digest)
            .map_err(|source| invalid(format!("vo.web.json source_digest: {source}")))?;
        validate_sources(&raw.source, &source_digest)?;
        let artifacts = parse_artifacts(raw.artifacts)?;
        let web = raw.web.map(|web| WebProjectManifest {
            entry: web.entry,
            include: web.include,
        });
        validate_web_project(web.as_ref())?;
        let extension = raw.extension.map(|extension| WebManifestExtension {
            name: extension.name,
            include: extension.include,
            wasm: extension.wasm,
            web: extension.web,
        });
        validate_extension(extension.as_ref())?;

        Ok(Self {
            schema_version: raw.schema_version,
            module,
            version,
            commit: raw.commit,
            module_root: raw.module_root,
            vo,
            require,
            source_digest,
            source: raw.source,
            artifacts,
            web,
            extension,
        })
    }

    /// Revalidate a typed value constructed by downstream Rust code.
    pub fn validate(&self) -> Result<()> {
        if self.schema_version != 1 {
            return Err(invalid(format!(
                "vo.web.json has unsupported schema_version {}",
                self.schema_version,
            )));
        }
        if !self.module.accepts_version(&self.version) {
            return Err(invalid(format!(
                "vo.web.json version {} is incompatible with module path {}",
                self.version, self.module,
            )));
        }
        if self.module_root != self.module.module_root() {
            return Err(invalid(format!(
                "vo.web.json module_root {:?} does not match module path root {:?}",
                self.module_root,
                self.module.module_root(),
            )));
        }
        super::validate_commit_hash(&self.commit)
            .map_err(|source| invalid(format!("vo.web.json commit: {source}")))?;
        validate_typed_requirements(&self.module, &self.require)?;
        validate_sources(&self.source, &self.source_digest)?;
        validate_typed_artifacts(&self.artifacts)?;
        validate_web_project(self.web.as_ref())?;
        validate_extension(self.extension.as_ref())
    }

    /// Verify the fields duplicated in `vo.release.json` and the canonical
    /// source-entry set. This comparison is bidirectional and normalizes the
    /// public typed release fields before comparing their protocol identities.
    pub fn validate_release_contract(&self, release: &ReleaseManifest) -> Result<()> {
        self.validate()?;
        release.validate()?;
        compare("module", &release.module, &self.module)?;
        compare("version", &release.version, &self.version)?;
        compare("commit", &release.commit, &self.commit)?;
        compare("module_root", &release.module_root, &self.module_root)?;
        compare("vo", &release.vo, &self.vo)?;
        compare_requirements(
            "vo.web.json vs vo.release.json",
            &release.require,
            &self.require,
        )?;

        let source_set = canonical_source_file_set(&self.source)
            .map_err(|source| invalid(format!("invalid vo.web.json source set: {source}")))?;
        if source_set.total_size != release.source.files_size
            || source_set.digest != release.source.files_digest
        {
            return Err(Error::DigestMismatch {
                context: "vo.web.json vs vo.release.json source file set".to_string(),
                expected: format!(
                    "{} ({} bytes)",
                    release.source.files_digest, release.source.files_size,
                ),
                found: format!("{} ({} bytes)", source_set.digest, source_set.total_size),
            });
        }

        let mut release_artifacts = release
            .artifacts
            .iter()
            .filter(|artifact| artifact.id.kind != "extension-native")
            .collect::<Vec<_>>();
        release_artifacts.sort_by(|left, right| left.id.cmp(&right.id));
        if release_artifacts.len() != self.artifacts.len()
            || release_artifacts
                .iter()
                .zip(&self.artifacts)
                .any(|(release, web)| {
                    release.id != web.id || release.size != web.size || release.digest != web.digest
                })
        {
            return Err(invalid(
                "vo.web.json artifacts do not exactly match browser artifacts in vo.release.json",
            ));
        }
        Ok(())
    }

    /// Verify all `vo.mod` fields represented by the browser manifest.
    pub fn validate_mod_contract(&self, mod_file: &ModFile) -> Result<()> {
        self.validate()?;
        mod_file.validate()?;
        let Some(module) = mod_file.module.as_github() else {
            return Err(invalid(format!(
                "packaged vo.mod module {} is not a publishable identity",
                mod_file.module,
            )));
        };
        compare("vo.mod module", module, &self.module)?;
        compare("vo.mod vo", &mod_file.vo, &self.vo)?;
        compare_mod_requirements(&mod_file.require, &self.require)?;
        if mod_file.web != self.web {
            return Err(invalid(
                "packaged vo.mod [web] metadata does not match vo.web.json",
            ));
        }
        let extension = mod_file
            .extension
            .as_ref()
            .map(WebManifestExtension::from_extension);
        if extension != self.extension {
            return Err(invalid(
                "packaged vo.mod extension metadata does not match vo.web.json",
            ));
        }
        Ok(())
    }
}

impl WebManifestExtension {
    fn from_extension(extension: &ExtensionManifest) -> Self {
        Self {
            name: extension.name.clone(),
            include: extension.include.clone(),
            wasm: extension.wasm.clone(),
            web: extension.web.clone(),
        }
    }
}

fn parse_requirements(module: &ModulePath, raw: Vec<RawRequire>) -> Result<Vec<ManifestRequire>> {
    if raw.len() > crate::MAX_MODULE_DEPENDENCIES {
        return Err(invalid(format!(
            "vo.web.json require contains more than {} modules",
            crate::MAX_MODULE_DEPENDENCIES,
        )));
    }
    let mut requirements = Vec::new();
    requirements
        .try_reserve(raw.len())
        .map_err(|_| invalid("failed to reserve vo.web.json requirements"))?;
    for (index, requirement) in raw.into_iter().enumerate() {
        let dependency = ModulePath::parse(&requirement.module)
            .map_err(|source| invalid(format!("vo.web.json require[{index}].module: {source}")))?;
        let constraint = DepConstraint::parse(&requirement.constraint).map_err(|source| {
            invalid(format!("vo.web.json require[{index}].constraint: {source}"))
        })?;
        if dependency == *module {
            return Err(invalid(format!(
                "vo.web.json module {module} must not require itself"
            )));
        }
        let lower_bound = ExactVersion::from_semver(constraint.version.clone());
        if !dependency.accepts_version(&lower_bound) {
            return Err(invalid(format!(
                "vo.web.json require[{index}] constraint {constraint} is incompatible with module path {dependency}"
            )));
        }
        requirements.push(ManifestRequire {
            module: dependency,
            constraint,
        });
    }
    if requirements
        .windows(2)
        .any(|pair| pair[0].module >= pair[1].module)
    {
        return Err(invalid(
            "vo.web.json require must be unique and sorted by module path",
        ));
    }
    Ok(requirements)
}

fn validate_sources(entries: &[SourceFileEntry], expected_digest: &Digest) -> Result<()> {
    if entries.is_empty() {
        return Err(invalid("vo.web.json source must contain vo.mod"));
    }
    if entries.windows(2).any(|pair| pair[0].path >= pair[1].path) {
        return Err(invalid(
            "vo.web.json source must be unique and sorted by path",
        ));
    }
    if !entries.iter().any(|entry| entry.path == "vo.mod") {
        return Err(invalid("vo.web.json source is missing vo.mod"));
    }
    for (index, entry) in entries.iter().enumerate() {
        if !super::is_source_file_set_candidate(&entry.path).map_err(invalid)? {
            return Err(invalid(format!(
                "vo.web.json source[{index}] uses source-set-excluded path {:?}",
                entry.path,
            )));
        }
    }
    let canonical = canonical_source_file_set(entries)
        .map_err(|source| invalid(format!("invalid vo.web.json source set: {source}")))?;
    if canonical.digest != *expected_digest {
        return Err(Error::DigestMismatch {
            context: "vo.web.json source entry set".to_string(),
            expected: expected_digest.to_string(),
            found: canonical.digest.to_string(),
        });
    }
    Ok(())
}

fn validate_typed_requirements(
    module: &ModulePath,
    requirements: &[ManifestRequire],
) -> Result<()> {
    if requirements.len() > crate::MAX_MODULE_DEPENDENCIES {
        return Err(invalid(format!(
            "vo.web.json require contains more than {} modules",
            crate::MAX_MODULE_DEPENDENCIES,
        )));
    }
    for (index, requirement) in requirements.iter().enumerate() {
        if requirement.module == *module {
            return Err(invalid(format!(
                "vo.web.json module {module} must not require itself"
            )));
        }
        let lower_bound = ExactVersion::from_semver(requirement.constraint.version.clone());
        if !requirement.module.accepts_version(&lower_bound) {
            return Err(invalid(format!(
                "vo.web.json require[{index}] constraint {} is incompatible with module path {}",
                requirement.constraint, requirement.module,
            )));
        }
    }
    if requirements
        .windows(2)
        .any(|pair| pair[0].module >= pair[1].module)
    {
        return Err(invalid(
            "vo.web.json require must be unique and sorted by module path",
        ));
    }
    Ok(())
}

fn validate_typed_artifacts(artifacts: &[WebManifestArtifact]) -> Result<()> {
    if artifacts.len() > crate::MAX_MODULE_ARTIFACTS {
        return Err(invalid(format!(
            "vo.web.json artifacts contains more than {} entries",
            crate::MAX_MODULE_ARTIFACTS,
        )));
    }
    let mut source_paths = PortablePathSet::default();
    let mut cache_paths = PortablePathSet::default();
    for (index, artifact) in artifacts.iter().enumerate() {
        artifact
            .id
            .validate()
            .map_err(|source| invalid(format!("vo.web.json artifacts[{index}]: {source}")))?;
        if artifact.id.kind == "extension-native" {
            return Err(invalid("vo.web.json must not list native artifacts"));
        }
        super::validate_portable_relative_path(&artifact.path).map_err(|source| {
            invalid(format!(
                "vo.web.json artifacts[{index}].path {:?}: {source}",
                artifact.path,
            ))
        })?;
        if !source_paths.insert_file(&artifact.path).map_err(invalid)? {
            return Err(invalid(format!(
                "vo.web.json artifacts reuse path {:?}",
                artifact.path,
            )));
        }
        let cache_path = crate::artifact::artifact_cache_key(&artifact.id)
            .map_err(|source| invalid(format!("vo.web.json artifacts[{index}]: {source}")))?;
        if !cache_paths.insert_file(&cache_path).map_err(invalid)? {
            return Err(invalid(format!(
                "vo.web.json artifacts[{index}] duplicates cache path {cache_path:?}"
            )));
        }
        if artifact.size > crate::MAX_MODULE_ARTIFACT_BYTES {
            return Err(invalid(format!(
                "vo.web.json artifacts[{index}] size {} exceeds the {}-byte limit",
                artifact.size,
                crate::MAX_MODULE_ARTIFACT_BYTES,
            )));
        }
    }
    if artifacts.windows(2).any(|pair| pair[0].id >= pair[1].id) {
        return Err(invalid(
            "vo.web.json artifacts must be unique and sorted by (kind, target, name)",
        ));
    }
    Ok(())
}

fn parse_artifacts(raw: Vec<RawArtifact>) -> Result<Vec<WebManifestArtifact>> {
    if raw.len() > crate::MAX_MODULE_ARTIFACTS {
        return Err(invalid(format!(
            "vo.web.json artifacts contains more than {} entries",
            crate::MAX_MODULE_ARTIFACTS,
        )));
    }
    let mut artifacts = Vec::new();
    let mut source_paths = PortablePathSet::default();
    let mut cache_paths = PortablePathSet::default();
    artifacts
        .try_reserve(raw.len())
        .map_err(|_| invalid("failed to reserve vo.web.json artifacts"))?;
    for (index, artifact) in raw.into_iter().enumerate() {
        let id = ArtifactId {
            kind: artifact.kind,
            target: artifact.target,
            name: artifact.name,
        };
        id.validate()
            .map_err(|source| invalid(format!("vo.web.json artifacts[{index}]: {source}")))?;
        if id.kind == "extension-native" {
            return Err(invalid("vo.web.json must not list native artifacts"));
        }
        super::validate_portable_relative_path(&artifact.path).map_err(|source| {
            invalid(format!(
                "vo.web.json artifacts[{index}].path {:?}: {source}",
                artifact.path,
            ))
        })?;
        if !source_paths.insert_file(&artifact.path).map_err(invalid)? {
            return Err(invalid(format!(
                "vo.web.json artifacts reuse path {:?}",
                artifact.path,
            )));
        }
        let cache_path = crate::artifact::artifact_cache_key(&id)
            .map_err(|source| invalid(format!("vo.web.json artifacts[{index}]: {source}")))?;
        if !cache_paths.insert_file(&cache_path).map_err(invalid)? {
            return Err(invalid(format!(
                "vo.web.json artifacts[{index}] duplicates cache path {cache_path:?}"
            )));
        }
        if artifact.size > crate::MAX_MODULE_ARTIFACT_BYTES {
            return Err(invalid(format!(
                "vo.web.json artifacts[{index}] size {} exceeds the {}-byte limit",
                artifact.size,
                crate::MAX_MODULE_ARTIFACT_BYTES,
            )));
        }
        artifacts.push(WebManifestArtifact {
            id,
            path: artifact.path,
            size: artifact.size,
            digest: Digest::parse(&artifact.digest).map_err(|source| {
                invalid(format!("vo.web.json artifacts[{index}].digest: {source}"))
            })?,
        });
    }
    if artifacts.windows(2).any(|pair| pair[0].id >= pair[1].id) {
        return Err(invalid(
            "vo.web.json artifacts must be unique and sorted by (kind, target, name)",
        ));
    }
    Ok(artifacts)
}

fn validate_web_project(web: Option<&WebProjectManifest>) -> Result<()> {
    let Some(web) = web else {
        return Ok(());
    };
    if let Some(entry) = web.entry.as_deref() {
        if entry.is_empty()
            || entry.len() > super::MAX_PORTABLE_PATH_BYTES
            || vo_common::identifier::has_unicode_white_space_boundary(entry)
            || entry.chars().any(vo_common::identifier::is_unicode_control)
        {
            return Err(invalid(
                "vo.web.json web.entry must be a non-empty normalized metadata string",
            ));
        }
    }
    if web.include.len() > crate::MAX_MODULE_METADATA_ENTRIES {
        return Err(invalid(format!(
            "vo.web.json web.include contains more than {} paths",
            crate::MAX_MODULE_METADATA_ENTRIES,
        )));
    }
    let mut paths = PortablePathSet::default();
    for path in &web.include {
        let portable = super::portable_relative_path_from_path(path)
            .map_err(|source| invalid(format!("vo.web.json web.include path: {source}")))?;
        if !paths.insert_path(&portable).map_err(invalid)? {
            return Err(invalid(format!(
                "vo.web.json web.include contains duplicate path {portable:?}"
            )));
        }
    }
    Ok(())
}

fn validate_extension(extension: Option<&WebManifestExtension>) -> Result<()> {
    let Some(extension) = extension else {
        return Ok(());
    };
    ExtensionManifest {
        name: extension.name.clone(),
        include: extension.include.clone(),
        native: None,
        wasm: extension.wasm.clone(),
        web: extension.web.clone(),
        manifest_path: PathBuf::from("vo.web.json"),
    }
    .validate()
    .map_err(|source| invalid(format!("vo.web.json extension: {source}")))
}

fn compare<T: std::fmt::Display + PartialEq>(field: &str, expected: &T, found: &T) -> Result<()> {
    if expected == found {
        return Ok(());
    }
    Err(invalid(format!(
        "vo.web.json {field} mismatch: expected {expected}, found {found}"
    )))
}

fn compare_requirements(
    context: &str,
    expected: &[ManifestRequire],
    found: &[ManifestRequire],
) -> Result<()> {
    let expected = requirement_tuples(expected);
    let found = requirement_tuples(found);
    if expected == found {
        return Ok(());
    }
    Err(invalid(format!(
        "{context} requirements mismatch: expected {expected:?}, found {found:?}"
    )))
}

fn compare_mod_requirements(expected: &[Require], found: &[ManifestRequire]) -> Result<()> {
    let mut expected = expected
        .iter()
        .map(|requirement| {
            (
                requirement.module.to_string(),
                requirement.constraint.to_string(),
            )
        })
        .collect::<Vec<_>>();
    expected.sort();
    let found = requirement_tuples(found);
    if expected == found {
        return Ok(());
    }
    Err(invalid(format!(
        "packaged vo.mod requirements do not match vo.web.json: expected {expected:?}, found {found:?}"
    )))
}

fn requirement_tuples(requirements: &[ManifestRequire]) -> Vec<(String, String)> {
    let mut tuples = requirements
        .iter()
        .map(|requirement| {
            (
                requirement.module.to_string(),
                requirement.constraint.to_string(),
            )
        })
        .collect::<Vec<_>>();
    tuples.sort();
    tuples
}

fn invalid(detail: impl Into<String>) -> Error {
    Error::InvalidReleaseMetadata(detail.into())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn minimal_manifest() -> Vec<u8> {
        let mod_bytes = b"module github.com/acme/lib\nvo ^0.1.0\n";
        let source = vec![SourceFileEntry {
            path: "vo.mod".to_string(),
            size: mod_bytes.len() as u64,
            digest: Digest::from_sha256(mod_bytes),
        }];
        let digest = canonical_source_file_set(&source).unwrap().digest;
        format!(
            concat!(
                "{{\"schema_version\":1,\"module\":\"github.com/acme/lib\",",
                "\"version\":\"v1.2.3\",\"commit\":\"0123456789abcdef0123456789abcdef01234567\",",
                "\"module_root\":\".\",\"vo\":\"^0.1.0\",",
                "\"require\":[],\"source_digest\":\"{}\",\"source\":[{{",
                "\"path\":\"vo.mod\",\"size\":{},\"digest\":\"{}\"}}],",
                "\"web\":null,\"extension\":null,\"artifacts\":[]}}"
            ),
            digest,
            mod_bytes.len(),
            Digest::from_sha256(mod_bytes),
        )
        .into_bytes()
    }

    #[test]
    fn parses_a_canonical_nonempty_source_contract() {
        let manifest = WebManifest::parse(&minimal_manifest()).unwrap();
        assert_eq!(manifest.module.as_str(), "github.com/acme/lib");
        assert_eq!(manifest.source.len(), 1);
    }

    #[test]
    fn rejects_empty_duplicate_and_unsorted_source_contracts() {
        let mut value: serde_json::Value = serde_json::from_slice(&minimal_manifest()).unwrap();
        value["source"] = serde_json::json!([]);
        let bytes = serde_json::to_vec(&value).unwrap();
        assert!(WebManifest::parse(&bytes).is_err());

        let entry = value["source"].clone();
        value = serde_json::from_slice(&minimal_manifest()).unwrap();
        let original = value["source"][0].clone();
        value["source"] = serde_json::json!([original.clone(), original]);
        let bytes = serde_json::to_vec(&value).unwrap();
        assert!(WebManifest::parse(&bytes).is_err());
        let _ = entry;
    }

    #[test]
    fn rejects_invalid_web_entry_metadata() {
        for entry in [
            String::new(),
            " entry.vo".to_string(),
            "entry.vo\n".to_string(),
            "x".repeat(crate::schema::MAX_PORTABLE_PATH_BYTES + 1),
        ] {
            let mut value: serde_json::Value = serde_json::from_slice(&minimal_manifest()).unwrap();
            value["web"] = serde_json::json!({"entry": entry, "include": []});
            let bytes = serde_json::to_vec(&value).unwrap();
            assert!(WebManifest::parse(&bytes).is_err());
        }
    }
}
