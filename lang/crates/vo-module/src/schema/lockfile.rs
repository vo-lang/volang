use serde::de::{self, IgnoredAny, SeqAccess, Visitor};
use serde::ser::SerializeStruct;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::digest::Digest;
use crate::identity::{ArtifactId, ModIdentity, ModulePath};
use crate::version::{DepConstraint, ExactVersion, ToolchainConstraint};
use crate::Error;

/// Current canonical `vo.lock` wire-format version.
pub const LOCK_FILE_VERSION: u64 = 2;

struct BoundedLockOutput {
    value: String,
    failure: Option<String>,
    limit: usize,
}

impl BoundedLockOutput {
    fn new() -> Result<Self, Error> {
        Self::with_limit(crate::MAX_LOCK_FILE_BYTES)
    }

    fn with_limit(limit: usize) -> Result<Self, Error> {
        let mut value = String::new();
        value
            .try_reserve(limit.min(4 * 1024))
            .map_err(|_| Error::LockFileParse("failed to reserve lock output".to_string()))?;
        Ok(Self {
            value,
            failure: None,
            limit,
        })
    }

    fn push_str(&mut self, value: &str) {
        if self.failure.is_some() {
            return;
        }
        let Some(next_len) = self.value.len().checked_add(value.len()) else {
            self.failure = Some("canonical vo.lock length overflow".to_string());
            return;
        };
        if next_len > self.limit {
            self.failure = Some(format!(
                "canonical vo.lock exceeds the {}-byte limit",
                self.limit
            ));
            return;
        }
        if self.value.try_reserve(value.len()).is_err() {
            self.failure = Some("failed to reserve canonical vo.lock output".to_string());
            return;
        }
        self.value.push_str(value);
    }

    fn push_toml_string(&mut self, value: &str) {
        self.push_str("\"");
        for character in value.chars() {
            match character {
                '"' => self.push_str(r#"\""#),
                '\\' => self.push_str(r#"\\"#),
                '\u{0008}' => self.push_str(r#"\b"#),
                '\t' => self.push_str(r#"\t"#),
                '\n' => self.push_str(r#"\n"#),
                '\u{000c}' => self.push_str(r#"\f"#),
                '\r' => self.push_str(r#"\r"#),
                character if character.is_control() => {
                    let codepoint = character as u32;
                    if codepoint <= u16::MAX as u32 {
                        self.push_str(&format!(r#"\u{codepoint:04X}"#));
                    } else {
                        self.push_str(&format!(r#"\U{codepoint:08X}"#));
                    }
                }
                character => {
                    let mut bytes = [0u8; 4];
                    self.push_str(character.encode_utf8(&mut bytes));
                }
            }
        }
        self.push_str("\"");
    }

    fn finish(self) -> Result<String, Error> {
        match self.failure {
            Some(failure) => Err(Error::LockFileParse(failure)),
            None => Ok(self.value),
        }
    }
}

/// Parsed representation of a `vo.lock` file.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LockFile {
    pub version: u64,
    pub created_by: String,
    pub root: LockRoot,
    #[serde(default, deserialize_with = "deserialize_resolved_modules")]
    pub resolved: Vec<LockedModule>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LockRoot {
    /// Root module identity: canonical github path for published projects,
    /// or `local/<name>` for toolchain-synthesized ephemeral single-file
    /// module lock files (spec §5.6.2, §10.2).
    pub module: ModIdentity,
    pub vo: ToolchainConstraint,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LockedModule {
    pub path: ModulePath,
    pub version: ExactVersion,
    pub vo: ToolchainConstraint,
    pub commit: String,
    pub release_manifest: Digest,
    pub source: Digest,
    #[serde(deserialize_with = "deserialize_locked_requirements")]
    pub deps: Vec<LockedRequirement>,
    #[serde(
        default,
        rename = "artifact",
        deserialize_with = "deserialize_locked_artifacts"
    )]
    pub artifacts: Vec<LockedArtifact>,
}

/// One dependency edge captured from the selected release manifest.
///
/// Keeping the constraint on the edge lets frozen and offline consumers prove
/// that every selected transitive version still satisfies its importer.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LockedRequirement {
    pub module: ModulePath,
    pub constraint: DepConstraint,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LockedArtifact {
    pub id: ArtifactId,
    pub size: u64,
    pub digest: Digest,
}

impl Serialize for LockedArtifact {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("LockedArtifact", 5)?;
        state.serialize_field("kind", &self.id.kind)?;
        state.serialize_field("target", &self.id.target)?;
        state.serialize_field("name", &self.id.name)?;
        state.serialize_field("size", &self.size)?;
        state.serialize_field("digest", &self.digest)?;
        state.end()
    }
}

impl<'de> Deserialize<'de> for LockedArtifact {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct RawLockedArtifact {
            kind: String,
            target: String,
            name: String,
            size: u64,
            digest: Digest,
        }

        let raw = RawLockedArtifact::deserialize(deserializer)?;
        let id = ArtifactId {
            kind: raw.kind,
            target: raw.target,
            name: raw.name,
        };
        id.validate().map_err(de::Error::custom)?;
        Ok(Self {
            id,
            size: raw.size,
            digest: raw.digest,
        })
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawLockFile {
    version: u64,
    created_by: String,
    root: LockRoot,
    #[serde(default, deserialize_with = "deserialize_resolved_modules")]
    resolved: Vec<LockedModule>,
}

fn deserialize_resolved_modules<'de, D>(deserializer: D) -> Result<Vec<LockedModule>, D::Error>
where
    D: Deserializer<'de>,
{
    struct ResolvedVisitor;

    impl<'de> Visitor<'de> for ResolvedVisitor {
        type Value = Vec<LockedModule>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            formatter.write_str("a bounded resolved-module array")
        }

        fn visit_seq<A>(self, mut sequence: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            let capacity = sequence
                .size_hint()
                .unwrap_or(0)
                .min(crate::MAX_MODULE_DEPENDENCIES);
            let mut modules = Vec::new();
            modules
                .try_reserve(capacity)
                .map_err(|_| de::Error::custom("failed to reserve resolved modules"))?;
            let mut edges = 0usize;
            let mut artifacts = 0usize;
            loop {
                if modules.len() == crate::MAX_MODULE_DEPENDENCIES {
                    if sequence.next_element::<IgnoredAny>()?.is_some() {
                        return Err(de::Error::custom(format!(
                            "resolved contains more than {} modules",
                            crate::MAX_MODULE_DEPENDENCIES
                        )));
                    }
                    break;
                }
                let Some(module) = sequence.next_element::<LockedModule>()? else {
                    break;
                };
                edges = edges
                    .checked_add(module.deps.len())
                    .ok_or_else(|| de::Error::custom("locked graph edge count overflow"))?;
                if edges > crate::MAX_SOLVER_GRAPH_EDGES {
                    return Err(de::Error::custom(format!(
                        "locked graph contains more than {} dependency edges",
                        crate::MAX_SOLVER_GRAPH_EDGES
                    )));
                }
                artifacts = artifacts
                    .checked_add(module.artifacts.len())
                    .ok_or_else(|| de::Error::custom("locked graph artifact count overflow"))?;
                if artifacts > crate::MAX_SOLVER_GRAPH_ARTIFACTS {
                    return Err(de::Error::custom(format!(
                        "locked graph contains more than {} artifacts",
                        crate::MAX_SOLVER_GRAPH_ARTIFACTS
                    )));
                }
                modules.push(module);
            }
            Ok(modules)
        }
    }

    deserializer.deserialize_seq(ResolvedVisitor)
}

fn deserialize_locked_requirements<'de, D>(
    deserializer: D,
) -> Result<Vec<LockedRequirement>, D::Error>
where
    D: Deserializer<'de>,
{
    struct RequirementsVisitor;

    impl<'de> Visitor<'de> for RequirementsVisitor {
        type Value = Vec<LockedRequirement>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            formatter.write_str("a bounded dependency-edge array")
        }

        fn visit_seq<A>(self, mut sequence: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            let capacity = sequence
                .size_hint()
                .unwrap_or(0)
                .min(crate::MAX_MODULE_DEPENDENCIES);
            let mut requirements = Vec::new();
            requirements
                .try_reserve(capacity)
                .map_err(|_| de::Error::custom("failed to reserve dependency edges"))?;
            loop {
                if requirements.len() == crate::MAX_MODULE_DEPENDENCIES {
                    if sequence.next_element::<IgnoredAny>()?.is_some() {
                        return Err(de::Error::custom(format!(
                            "deps contains more than {} modules",
                            crate::MAX_MODULE_DEPENDENCIES
                        )));
                    }
                    break;
                }
                let Some(requirement) = sequence.next_element::<LockedRequirement>()? else {
                    break;
                };
                requirements.push(requirement);
            }
            Ok(requirements)
        }
    }

    deserializer.deserialize_seq(RequirementsVisitor)
}

fn deserialize_locked_artifacts<'de, D>(deserializer: D) -> Result<Vec<LockedArtifact>, D::Error>
where
    D: Deserializer<'de>,
{
    struct ArtifactsVisitor;

    impl<'de> Visitor<'de> for ArtifactsVisitor {
        type Value = Vec<LockedArtifact>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            formatter.write_str("a bounded artifact array")
        }

        fn visit_seq<A>(self, mut sequence: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            let capacity = sequence
                .size_hint()
                .unwrap_or(0)
                .min(crate::MAX_MODULE_ARTIFACTS);
            let mut artifacts = Vec::new();
            artifacts
                .try_reserve(capacity)
                .map_err(|_| de::Error::custom("failed to reserve locked artifacts"))?;
            loop {
                if artifacts.len() == crate::MAX_MODULE_ARTIFACTS {
                    if sequence.next_element::<IgnoredAny>()?.is_some() {
                        return Err(de::Error::custom(format!(
                            "artifact contains more than {} entries",
                            crate::MAX_MODULE_ARTIFACTS
                        )));
                    }
                    break;
                }
                let Some(artifact) = sequence.next_element::<LockedArtifact>()? else {
                    break;
                };
                artifacts.push(artifact);
            }
            Ok(artifacts)
        }
    }

    deserializer.deserialize_seq(ArtifactsVisitor)
}

impl LockFile {
    /// Parse a `vo.lock` TOML string.
    pub fn parse(content: &str) -> Result<Self, Error> {
        if content.len() > crate::MAX_LOCK_FILE_BYTES {
            return Err(Error::LockFileParse(format!(
                "vo.lock exceeds the {}-byte lock-file limit",
                crate::MAX_LOCK_FILE_BYTES
            )));
        }
        // Deserialize directly from the TOML stream. Bounded sequence visitors
        // reject module/edge/artifact excess before building a generic TOML
        // value tree or allocating the remainder of an oversized sequence.
        let raw: RawLockFile = toml::from_str(content)
            .map_err(|error| Error::LockFileParse(format!("TOML parse error: {error}")))?;
        if raw.version != LOCK_FILE_VERSION {
            return Err(Error::LockFileParse(format!(
                "unsupported lock file version: {}",
                raw.version
            )));
        }

        // The v2 wire format has one canonical top-level module order.
        for module in &raw.resolved {
            if raw.root.module.as_github() == Some(&module.path) {
                return Err(Error::LockFileParse(format!(
                    "root module {} must not appear in resolved",
                    module.path
                )));
            }
        }

        let lock_file = LockFile {
            version: raw.version,
            created_by: raw.created_by,
            root: raw.root,
            resolved: raw.resolved,
        };
        validate_locked_module_graph(&lock_file.resolved)?;
        validate_locked_toolchain_coverage(&lock_file.root.vo, &lock_file.resolved)?;
        Ok(lock_file)
    }

    /// Render the lock file as canonical TOML.
    pub fn render(&self) -> Result<String, Error> {
        if self.resolved.len() > crate::MAX_MODULE_DEPENDENCIES {
            return Err(Error::LockFileParse(format!(
                "resolved contains more than {} modules",
                crate::MAX_MODULE_DEPENDENCIES
            )));
        }
        let mut artifact_count = 0usize;
        for (index, module) in self.resolved.iter().enumerate() {
            if module.deps.len() > crate::MAX_MODULE_DEPENDENCIES {
                return Err(Error::LockFileParse(format!(
                    "resolved[{index}].deps contains more than {} modules",
                    crate::MAX_MODULE_DEPENDENCIES
                )));
            }
            if module.artifacts.len() > crate::MAX_MODULE_ARTIFACTS {
                return Err(Error::LockFileParse(format!(
                    "resolved[{index}].artifact contains more than {} entries",
                    crate::MAX_MODULE_ARTIFACTS
                )));
            }
            artifact_count = artifact_count
                .checked_add(module.artifacts.len())
                .ok_or_else(|| Error::ResolutionLimitExceeded {
                    resource: "locked graph artifact count".to_string(),
                    limit: crate::MAX_SOLVER_GRAPH_ARTIFACTS,
                })?;
            if artifact_count > crate::MAX_SOLVER_GRAPH_ARTIFACTS {
                return Err(Error::ResolutionLimitExceeded {
                    resource: "locked graph artifact count".to_string(),
                    limit: crate::MAX_SOLVER_GRAPH_ARTIFACTS,
                });
            }
        }
        if super::rendered_toml_string_len(&self.created_by)
            .is_none_or(|length| length > crate::MAX_LOCK_FILE_BYTES)
        {
            return Err(Error::LockFileParse(format!(
                "created_by cannot fit within the {}-byte lock-file limit",
                crate::MAX_LOCK_FILE_BYTES
            )));
        }
        let mut out = BoundedLockOutput::new()?;
        out.push_str(&format!("version = {}\n", self.version));
        out.push_str("created_by = ");
        out.push_toml_string(&self.created_by);
        out.push_str("\n");
        out.push_str("\n[root]\n");
        out.push_str("module = ");
        out.push_toml_string(self.root.module.as_str());
        out.push_str("\nvo = ");
        out.push_toml_string(&self.root.vo.to_string());
        out.push_str("\n");

        let mut sorted: Vec<&LockedModule> = self.resolved.iter().collect();
        sorted.sort_by(|a, b| a.path.cmp(&b.path));

        for lm in sorted {
            out.push_str("\n[[resolved]]\n");
            out.push_str("path = ");
            out.push_toml_string(lm.path.as_str());
            out.push_str("\nversion = ");
            out.push_toml_string(&lm.version.to_string());
            out.push_str("\nvo = ");
            out.push_toml_string(&lm.vo.to_string());
            out.push_str("\ncommit = ");
            out.push_toml_string(&lm.commit);
            out.push_str("\nrelease_manifest = ");
            out.push_toml_string(lm.release_manifest.as_str());
            out.push_str("\nsource = ");
            out.push_toml_string(lm.source.as_str());
            out.push_str("\n");

            // Dependency edges are canonicalized by module path. Duplicate
            // module paths remain visible to the strict reparse below.
            let mut deps_sorted: Vec<&LockedRequirement> = lm.deps.iter().collect();
            deps_sorted.sort_by(|a, b| a.module.cmp(&b.module));
            if deps_sorted.is_empty() {
                out.push_str("deps = []\n");
            } else {
                out.push_str("deps = [\n");
                for dep in deps_sorted {
                    out.push_str("  { module = ");
                    out.push_toml_string(dep.module.as_str());
                    out.push_str(", constraint = ");
                    out.push_toml_string(&dep.constraint.to_string());
                    out.push_str(" },\n");
                }
                out.push_str("]\n");
            }

            // artifacts: sorted by (kind, target, name)
            let mut arts_sorted: Vec<&LockedArtifact> = lm.artifacts.iter().collect();
            arts_sorted.sort_by(|a, b| a.id.cmp(&b.id));
            for art in arts_sorted {
                out.push_str("\n[[resolved.artifact]]\n");
                out.push_str("kind = ");
                out.push_toml_string(&art.id.kind);
                out.push_str("\ntarget = ");
                out.push_toml_string(&art.id.target);
                out.push_str("\nname = ");
                out.push_toml_string(&art.id.name);
                out.push_str("\n");
                out.push_str(&format!("size = {}\n", art.size));
                out.push_str("digest = ");
                out.push_toml_string(art.digest.as_str());
                out.push_str("\n");
            }
        }
        let out = out.finish()?;
        Self::parse(&out)?;
        Ok(out)
    }

    /// Find a locked module by path.
    pub fn find(&self, path: &ModulePath) -> Option<&LockedModule> {
        self.resolved.iter().find(|lm| lm.path == *path)
    }
}

/// Validate the closed selected-module graph represented by lock entries.
///
/// Root reachability requires `vo.mod` and is checked by `lock.rs`; this layer
/// enforces all invariants that can be proven from the locked modules alone.
pub(crate) fn validate_locked_module_graph(modules: &[LockedModule]) -> Result<(), Error> {
    if modules.len() > crate::MAX_MODULE_DEPENDENCIES {
        return Err(Error::LockFileParse(format!(
            "resolved contains more than {} modules",
            crate::MAX_MODULE_DEPENDENCIES
        )));
    }

    for pair in modules.windows(2) {
        if pair[0].path == pair[1].path {
            return Err(Error::LockFileParse(format!(
                "duplicate resolved module: {}",
                pair[0].path
            )));
        }
        if pair[0].path > pair[1].path {
            return Err(Error::LockFileParse(
                "resolved modules must be sorted by module path".to_string(),
            ));
        }
    }

    let mut selected = std::collections::BTreeMap::new();
    let mut edge_count = 0usize;
    let mut artifact_count = 0usize;
    for (index, module) in modules.iter().enumerate() {
        super::validate_commit_hash(&module.commit)
            .map_err(|error| Error::LockFileParse(format!("resolved[{index}].commit: {error}")))?;
        if !module.path.accepts_version(&module.version) {
            return Err(Error::LockFileParse(format!(
                "resolved[{index}].version {} is incompatible with module path {}",
                module.version, module.path
            )));
        }
        if module.deps.len() > crate::MAX_MODULE_DEPENDENCIES {
            return Err(Error::LockFileParse(format!(
                "resolved[{index}].deps contains more than {} modules",
                crate::MAX_MODULE_DEPENDENCIES
            )));
        }
        edge_count = edge_count.checked_add(module.deps.len()).ok_or_else(|| {
            Error::ResolutionLimitExceeded {
                resource: "locked graph edge count".to_string(),
                limit: crate::MAX_SOLVER_GRAPH_EDGES,
            }
        })?;
        if edge_count > crate::MAX_SOLVER_GRAPH_EDGES {
            return Err(Error::ResolutionLimitExceeded {
                resource: "locked graph edge count".to_string(),
                limit: crate::MAX_SOLVER_GRAPH_EDGES,
            });
        }
        for pair in module.deps.windows(2) {
            if pair[0].module >= pair[1].module {
                return Err(Error::LockFileParse(format!(
                    "resolved[{index}].deps must be unique and sorted by module path"
                )));
            }
        }
        for (dependency_index, dependency) in module.deps.iter().enumerate() {
            if dependency.module == module.path {
                return Err(Error::LockFileParse(format!(
                    "resolved module {} must not require itself",
                    module.path
                )));
            }
            let lower_bound = ExactVersion::from_semver(dependency.constraint.version.clone());
            if !dependency.module.accepts_version(&lower_bound) {
                return Err(Error::LockFileParse(format!(
                    "resolved[{index}].deps[{dependency_index}] constraint {} is incompatible with module path {}",
                    dependency.constraint, dependency.module
                )));
            }
        }
        if module.artifacts.len() > crate::MAX_MODULE_ARTIFACTS {
            return Err(Error::LockFileParse(format!(
                "resolved[{index}].artifact contains more than {} entries",
                crate::MAX_MODULE_ARTIFACTS
            )));
        }
        artifact_count = artifact_count
            .checked_add(module.artifacts.len())
            .ok_or_else(|| Error::ResolutionLimitExceeded {
                resource: "locked graph artifact count".to_string(),
                limit: crate::MAX_SOLVER_GRAPH_ARTIFACTS,
            })?;
        if artifact_count > crate::MAX_SOLVER_GRAPH_ARTIFACTS {
            return Err(Error::ResolutionLimitExceeded {
                resource: "locked graph artifact count".to_string(),
                limit: crate::MAX_SOLVER_GRAPH_ARTIFACTS,
            });
        }
        let mut artifact_paths = super::PortablePathSet::default();
        for (artifact_index, artifact) in module.artifacts.iter().enumerate() {
            artifact.id.validate().map_err(|error| {
                Error::LockFileParse(format!(
                    "resolved[{index}].artifact[{artifact_index}]: {error}"
                ))
            })?;
            if artifact.size > crate::MAX_MODULE_ARTIFACT_BYTES {
                return Err(Error::LockFileParse(format!(
                    "resolved[{index}].artifact[{artifact_index}].size {} exceeds the {}-byte limit",
                    artifact.size,
                    crate::MAX_MODULE_ARTIFACT_BYTES
                )));
            }
            let cache_path =
                crate::artifact::artifact_cache_key(&artifact.id).map_err(|error| {
                    Error::LockFileParse(format!(
                        "resolved[{index}].artifact[{artifact_index}]: {error}"
                    ))
                })?;
            if !artifact_paths.insert_file(&cache_path).map_err(|error| {
                Error::LockFileParse(format!(
                    "resolved[{index}].artifact[{artifact_index}]: {error}"
                ))
            })? {
                return Err(Error::LockFileParse(format!(
                    "resolved[{index}].artifact[{artifact_index}] duplicates cache path {cache_path}"
                )));
            }
        }
        for pair in module.artifacts.windows(2) {
            if pair[0].id >= pair[1].id {
                return Err(Error::LockFileParse(format!(
                    "resolved[{index}].artifact must be unique and sorted by (kind, target, name)"
                )));
            }
        }
        if let Some(existing) = selected.insert(&module.path, module) {
            return Err(Error::LockFileParse(format!(
                "duplicate resolved module {} selected at {} and {}",
                module.path, existing.version, module.version
            )));
        }
    }

    for module in modules {
        for dependency in &module.deps {
            let selected_dependency = selected.get(&dependency.module).ok_or_else(|| {
                Error::LockFileParse(format!(
                    "{} requires {} {} but that module is absent from vo.lock",
                    module.path, dependency.module, dependency.constraint
                ))
            })?;
            if !dependency
                .constraint
                .satisfies(&selected_dependency.version)
            {
                return Err(Error::LockFileParse(format!(
                    "{} requires {} {} but vo.lock selects {}",
                    module.path,
                    dependency.module,
                    dependency.constraint,
                    selected_dependency.version
                )));
            }
        }
    }

    Ok(())
}

/// Enforce the resource dimensions used by APIs that consume an authorized
/// materialized subset of a previously validated lock graph. Such subsets may
/// omit vertices and edges, so full graph closure validation does not apply.
pub(crate) fn validate_materialized_module_limits(modules: &[LockedModule]) -> Result<(), Error> {
    if modules.len() > crate::MAX_MODULE_DEPENDENCIES {
        return Err(Error::LockFileParse(format!(
            "materialized subset contains more than {} modules",
            crate::MAX_MODULE_DEPENDENCIES,
        )));
    }

    let mut artifact_count = 0usize;
    for (index, module) in modules.iter().enumerate() {
        if module.artifacts.len() > crate::MAX_MODULE_ARTIFACTS {
            return Err(Error::LockFileParse(format!(
                "materialized subset module [{index}] contains more than {} artifacts",
                crate::MAX_MODULE_ARTIFACTS,
            )));
        }
        artifact_count = artifact_count
            .checked_add(module.artifacts.len())
            .ok_or_else(|| Error::ResolutionLimitExceeded {
                resource: "materialized artifact count".to_string(),
                limit: crate::MAX_SOLVER_GRAPH_ARTIFACTS,
            })?;
        if artifact_count > crate::MAX_SOLVER_GRAPH_ARTIFACTS {
            return Err(Error::ResolutionLimitExceeded {
                resource: "materialized artifact count".to_string(),
                limit: crate::MAX_SOLVER_GRAPH_ARTIFACTS,
            });
        }
    }
    Ok(())
}

/// Require every selected release to support the root project's complete
/// toolchain range.
pub(crate) fn validate_locked_toolchain_coverage(
    root_vo: &ToolchainConstraint,
    modules: &[LockedModule],
) -> Result<(), Error> {
    for module in modules {
        if !root_vo.is_subset_of(&module.vo) {
            return Err(Error::DependencyToolchainMismatch {
                module: module.path.as_str().to_string(),
                project_constraint: root_vo.to_string(),
                dependency_constraint: module.vo.to_string(),
            });
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const GOLDEN: &str = r#"version = 2
created_by = "vo 1.0.0"

[root]
module = "github.com/acme/app"
vo = "^1.0.0"

[[resolved]]
path = "github.com/vo-lang/vogui"
version = "v0.4.2"
vo = "^1.0.0"
commit = "9b6d4a8d2d5a6f2d4c0c2d9e6b3a1f0e0c4d1e22"
release_manifest = "sha256:2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d"
source = "sha256:81c181c181c181c181c181c181c181c181c181c181c181c181c181c181c181c1"
deps = [
  { module = "github.com/vo-lang/voplay", constraint = "^0.7.0" },
]

[[resolved.artifact]]
kind = "extension-wasm"
target = "wasm32-unknown-unknown"
name = "vogui-wasm.wasm"
size = 123456
digest = "sha256:6f926f926f926f926f926f926f926f926f926f926f926f926f926f926f926f92"

[[resolved]]
path = "github.com/vo-lang/voplay"
version = "v0.7.3"
vo = "^1.0.0"
commit = "1111111111111111111111111111111111111111"
release_manifest = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
source = "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
deps = []
"#;

    #[test]
    fn test_parse_golden() {
        let lf = LockFile::parse(GOLDEN).unwrap();
        assert_eq!(lf.version, LOCK_FILE_VERSION);
        assert_eq!(lf.root.module.as_str(), "github.com/acme/app");
        assert_eq!(lf.resolved.len(), 2);
        assert_eq!(lf.resolved[0].path.as_str(), "github.com/vo-lang/vogui");
        assert_eq!(lf.resolved[0].artifacts.len(), 1);
        assert_eq!(lf.resolved[0].artifacts[0].size, 123456);
    }

    #[test]
    fn test_roundtrip() {
        let lf = LockFile::parse(GOLDEN).unwrap();
        let rendered = lf.render().unwrap();
        let lf2 = LockFile::parse(&rendered).unwrap();
        assert_eq!(lf2, lf);
        assert_eq!(lf2.render().unwrap(), rendered);
    }

    #[test]
    fn serde_roundtrip_preserves_dependency_constraints() {
        let lock = LockFile::parse(GOLDEN).unwrap();
        let json = serde_json::to_string(&lock).unwrap();
        let decoded: LockFile = serde_json::from_str(&json).unwrap();
        assert_eq!(decoded, lock);
        assert!(json.contains("^0.7.0"));
        assert!(json.contains("\"artifact\":"));
        assert!(json.contains("\"kind\":\"extension-wasm\""));
        assert!(!json.contains("\"artifacts\":"));
        assert!(!json.contains("\"id\":"));
    }

    #[test]
    fn rejects_nested_artifact_identity_shape() {
        let nested = GOLDEN.replacen(
            "kind = \"extension-wasm\"\ntarget = \"wasm32-unknown-unknown\"\nname = \"vogui-wasm.wasm\"",
            "id = { kind = \"extension-wasm\", target = \"wasm32-unknown-unknown\", name = \"vogui-wasm.wasm\" }",
            1,
        );
        let error = LockFile::parse(&nested).unwrap_err().to_string();
        assert!(error.contains("id") || error.contains("kind"), "{error}");
    }

    #[test]
    fn render_rejects_invalid_public_values() {
        let mut lock = LockFile::parse(GOLDEN).unwrap();
        lock.resolved[0].commit = "invalid".to_string();
        assert!(lock.render().is_err());

        let mut lock = LockFile::parse(GOLDEN).unwrap();
        lock.resolved.push(lock.resolved[0].clone());
        assert!(lock.render().is_err());
    }

    #[test]
    fn test_find() {
        let lf = LockFile::parse(GOLDEN).unwrap();
        let mp = ModulePath::parse("github.com/vo-lang/vogui").unwrap();
        assert!(lf.find(&mp).is_some());
        let mp2 = ModulePath::parse("github.com/acme/unknown").unwrap();
        assert!(lf.find(&mp2).is_none());
    }

    #[test]
    fn test_reject_duplicate_resolved() {
        let content = r#"version = 2
created_by = "vo 1.0.0"
[root]
module = "github.com/acme/app"
vo = "^1.0.0"
[[resolved]]
path = "github.com/vo-lang/vogui"
version = "v0.4.2"
vo = "^1.0.0"
commit = "9b6d4a8d2d5a6f2d4c0c2d9e6b3a1f0e0c4d1e22"
release_manifest = "sha256:2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d"
source = "sha256:81c181c181c181c181c181c181c181c181c181c181c181c181c181c181c181c1"
deps = []
[[resolved]]
path = "github.com/vo-lang/vogui"
version = "v0.4.3"
vo = "^1.0.0"
commit = "1111111111111111111111111111111111111111"
release_manifest = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
source = "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
deps = []
"#;
        assert!(LockFile::parse(content).is_err());
    }

    #[test]
    fn rejects_noncanonical_resolved_order() {
        let first = GOLDEN.find("\n[[resolved]]\n").unwrap();
        let second = GOLDEN
            .find("\n[[resolved]]\npath = \"github.com/vo-lang/voplay\"")
            .unwrap();
        let reordered = format!(
            "{}{}{}",
            &GOLDEN[..first],
            &GOLDEN[second..],
            &GOLDEN[first..second],
        );
        let error = LockFile::parse(&reordered).unwrap_err().to_string();
        assert!(error.contains("resolved modules must be sorted"), "{error}");
    }

    #[test]
    fn bounded_lock_output_fails_before_exceeding_its_limit() {
        let mut output = BoundedLockOutput::with_limit(4).unwrap();
        output.push_str("1234");
        output.push_str("5");
        let error = output.finish().unwrap_err().to_string();
        assert!(error.contains("exceeds the 4-byte limit"), "{error}");
    }

    #[test]
    fn rejects_legacy_lock_schema() {
        let legacy = GOLDEN.replacen("version = 2", "version = 1", 1);
        let error = LockFile::parse(&legacy).unwrap_err().to_string();
        assert!(error.contains("unsupported lock file version: 1"));
    }

    #[test]
    fn rejects_unsatisfied_transitive_constraint() {
        let content = GOLDEN.replacen("constraint = \"^0.7.0\"", "constraint = \"^0.8.0\"", 1);
        let error = LockFile::parse(&content).unwrap_err().to_string();
        assert!(error.contains("requires github.com/vo-lang/voplay ^0.8.0"));
        assert!(error.contains("selects v0.7.3"));
    }

    #[test]
    fn rejects_dependency_toolchain_constraint_that_does_not_cover_root() {
        let content = GOLDEN.replacen(
            "version = \"v0.4.2\"\nvo = \"^1.0.0\"",
            "version = \"v0.4.2\"\nvo = \"^2.0.0\"",
            1,
        );
        let error = LockFile::parse(&content).unwrap_err();
        assert!(matches!(error, Error::DependencyToolchainMismatch { .. }));
        assert!(error.to_string().contains("github.com/vo-lang/vogui"));
    }

    #[test]
    fn rejects_duplicate_dependency_edge() {
        let content = GOLDEN.replacen(
            "  { module = \"github.com/vo-lang/voplay\", constraint = \"^0.7.0\" },",
            "  { module = \"github.com/vo-lang/voplay\", constraint = \"^0.7.0\" },\n  { module = \"github.com/vo-lang/voplay\", constraint = \"~0.7.0\" },",
            1,
        );
        let error = LockFile::parse(&content).unwrap_err().to_string();
        assert!(error.contains("must be unique and sorted"));
    }

    #[test]
    fn test_reject_unknown_fields() {
        let content = GOLDEN.replacen(
            "created_by = \"vo 1.0.0\"",
            "created_by = \"vo 1.0.0\"\nunknown = true",
            1,
        );
        let error = LockFile::parse(&content).unwrap_err().to_string();
        assert!(error.contains("unknown"), "{error}");
    }

    #[test]
    fn test_reject_unknown_dependency_fields() {
        let content = GOLDEN.replacen(
            "constraint = \"^0.7.0\" }",
            "constraint = \"^0.7.0\", future = true }",
            1,
        );
        let error = LockFile::parse(&content).unwrap_err().to_string();
        assert!(error.contains("future"), "{error}");
    }

    #[test]
    fn test_reject_root_in_resolved() {
        let content = GOLDEN.replacen(
            "module = \"github.com/acme/app\"",
            "module = \"github.com/vo-lang/vogui\"",
            1,
        );
        let error = LockFile::parse(&content).unwrap_err().to_string();
        assert!(error.contains("must not appear in resolved"));
    }

    #[test]
    fn test_reject_version_incompatible_with_module_path() {
        let content = GOLDEN.replacen("version = \"v0.4.2\"", "version = \"v2.4.2\"", 1);
        let error = LockFile::parse(&content).unwrap_err().to_string();
        assert!(error.contains("is incompatible with module path"));
    }

    #[test]
    fn test_render_escapes_toml_strings() {
        let mut lock = LockFile::parse(GOLDEN).unwrap();
        lock.created_by = "quickplay \"v2\"\\selftest\n\u{0085}e\u{0301}🙂".to_string();

        let rendered = lock.render().unwrap();
        assert!(rendered
            .contains("created_by = \"quickplay \\\"v2\\\"\\\\selftest\\n\\u0085e\u{0301}🙂\""));
        let reparsed = LockFile::parse(&rendered).unwrap();
        assert_eq!(reparsed.created_by, lock.created_by);
    }
}
