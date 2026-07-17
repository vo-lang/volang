use serde::de::{self, IgnoredAny, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer, Serialize};

use crate::digest::Digest;
use crate::identity::{ModIdentity, ModulePath};
use crate::version::{DepConstraint, ExactVersion, ToolchainConstraint};
use crate::Error;

/// Current canonical `vo.lock` wire-format version.
pub const LOCK_FILE_VERSION: u64 = 3;

/// A frozen dependency graph.
///
/// Each node stores only data needed to prove graph selection offline. Source,
/// commit, package and artifact metadata are loaded from the exact
/// `vo.release.json` bytes bound by `release` before materialization.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LockFile {
    pub version: u64,
    pub root: LockRoot,
    #[serde(
        default,
        rename = "module",
        deserialize_with = "deserialize_locked_modules"
    )]
    pub modules: Vec<LockedModule>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LockRoot {
    pub module: ModIdentity,
    pub vo: ToolchainConstraint,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LockedModule {
    pub path: ModulePath,
    pub version: ExactVersion,
    pub vo: ToolchainConstraint,
    pub release: Digest,
    #[serde(default, deserialize_with = "deserialize_locked_dependencies")]
    pub dependencies: Vec<LockedDependency>,
}

/// One dependency edge copied from the selected release manifest.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LockedDependency {
    pub module: ModulePath,
    pub constraint: DepConstraint,
}

fn deserialize_locked_modules<'de, D>(deserializer: D) -> Result<Vec<LockedModule>, D::Error>
where
    D: Deserializer<'de>,
{
    struct ModulesVisitor;

    impl<'de> Visitor<'de> for ModulesVisitor {
        type Value = Vec<LockedModule>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            formatter.write_str("a bounded module array")
        }

        fn visit_seq<A>(self, mut sequence: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            if sequence
                .size_hint()
                .is_some_and(|length| length > crate::MAX_MODULE_DEPENDENCIES)
            {
                return Err(de::Error::custom(format!(
                    "module contains more than {} entries",
                    crate::MAX_MODULE_DEPENDENCIES
                )));
            }
            let capacity = sequence
                .size_hint()
                .unwrap_or(0)
                .min(crate::MAX_MODULE_DEPENDENCIES);
            let mut modules = Vec::new();
            modules
                .try_reserve(capacity)
                .map_err(|_| de::Error::custom("failed to reserve locked modules"))?;
            let mut edges = 0usize;
            loop {
                if modules.len() == crate::MAX_MODULE_DEPENDENCIES {
                    if sequence.next_element::<IgnoredAny>()?.is_some() {
                        return Err(de::Error::custom(format!(
                            "module contains more than {} entries",
                            crate::MAX_MODULE_DEPENDENCIES
                        )));
                    }
                    break;
                }
                let Some(module) = sequence.next_element::<LockedModule>()? else {
                    break;
                };
                edges = edges
                    .checked_add(module.dependencies.len())
                    .ok_or_else(|| de::Error::custom("locked graph edge count overflow"))?;
                if edges > crate::MAX_SOLVER_GRAPH_EDGES {
                    return Err(de::Error::custom(format!(
                        "locked graph contains more than {} dependency edges",
                        crate::MAX_SOLVER_GRAPH_EDGES
                    )));
                }
                modules.push(module);
            }
            Ok(modules)
        }
    }

    deserializer.deserialize_seq(ModulesVisitor)
}

fn deserialize_locked_dependencies<'de, D>(
    deserializer: D,
) -> Result<Vec<LockedDependency>, D::Error>
where
    D: Deserializer<'de>,
{
    struct DependenciesVisitor;

    impl<'de> Visitor<'de> for DependenciesVisitor {
        type Value = Vec<LockedDependency>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            formatter.write_str("a bounded dependency-edge array")
        }

        fn visit_seq<A>(self, mut sequence: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            if sequence
                .size_hint()
                .is_some_and(|length| length > crate::MAX_MODULE_DEPENDENCIES)
            {
                return Err(de::Error::custom(format!(
                    "dependencies contains more than {} entries",
                    crate::MAX_MODULE_DEPENDENCIES
                )));
            }
            let capacity = sequence
                .size_hint()
                .unwrap_or(0)
                .min(crate::MAX_MODULE_DEPENDENCIES);
            let mut dependencies = Vec::new();
            dependencies
                .try_reserve(capacity)
                .map_err(|_| de::Error::custom("failed to reserve locked dependencies"))?;
            loop {
                if dependencies.len() == crate::MAX_MODULE_DEPENDENCIES {
                    if sequence.next_element::<IgnoredAny>()?.is_some() {
                        return Err(de::Error::custom(format!(
                            "dependencies contains more than {} entries",
                            crate::MAX_MODULE_DEPENDENCIES
                        )));
                    }
                    break;
                }
                let Some(dependency) = sequence.next_element::<LockedDependency>()? else {
                    break;
                };
                dependencies.push(dependency);
            }
            Ok(dependencies)
        }
    }

    deserializer.deserialize_seq(DependenciesVisitor)
}

impl LockFile {
    pub fn parse(content: &str) -> Result<Self, Error> {
        if content.len() > crate::MAX_LOCK_FILE_BYTES {
            return Err(Error::LockFileParse(format!(
                "vo.lock exceeds the {}-byte lock-file limit",
                crate::MAX_LOCK_FILE_BYTES
            )));
        }
        let lock: Self = toml::from_str(content)
            .map_err(|error| Error::LockFileParse(format!("TOML parse error: {error}")))?;
        lock.validate()?;
        let canonical = lock.render()?;
        if canonical != content {
            return Err(Error::LockFileParse(
                "vo.lock must use the canonical v3 TOML encoding".to_string(),
            ));
        }
        Ok(lock)
    }

    pub fn validate(&self) -> Result<(), Error> {
        if self.version != LOCK_FILE_VERSION {
            return Err(Error::LockFileParse(format!(
                "unsupported lock file version: {}",
                self.version
            )));
        }
        if self.modules.is_empty() {
            return Err(Error::LockFileParse(
                "vo.lock must contain at least one [[module]] entry; omit vo.lock when the external dependency graph is empty"
                    .to_string(),
            ));
        }
        for module in &self.modules {
            if self.root.module.as_github() == Some(&module.path) {
                return Err(Error::LockFileParse(format!(
                    "root module {} must not appear in [[module]]",
                    module.path
                )));
            }
        }
        validate_locked_module_graph(&self.modules)?;
        validate_locked_toolchain_coverage(&self.root.vo, &self.modules)
    }

    /// Render canonical, deterministic TOML.
    pub fn render(&self) -> Result<String, Error> {
        self.validate()?;
        let mut output = super::BoundedTextOutput::new(crate::MAX_LOCK_FILE_BYTES)
            .map_err(Error::LockFileParse)?;

        macro_rules! push {
            ($value:expr) => {
                output.push_str($value).map_err(Error::LockFileParse)?
            };
        }
        macro_rules! quoted {
            ($value:expr) => {
                output
                    .push_toml_string($value)
                    .map_err(Error::LockFileParse)?
            };
        }

        push!(&format!("version = {}\n", self.version));
        push!("\n[root]\nmodule = ");
        quoted!(self.root.module.as_str());
        push!("\nvo = ");
        quoted!(&self.root.vo.to_string());
        push!("\n");

        let mut modules = self.modules.iter().collect::<Vec<_>>();
        modules.sort_by(|left, right| left.path.cmp(&right.path));
        for module in modules {
            push!("\n[[module]]\npath = ");
            quoted!(module.path.as_str());
            push!("\nversion = ");
            quoted!(&module.version.to_string());
            push!("\nvo = ");
            quoted!(&module.vo.to_string());
            push!("\nrelease = ");
            quoted!(module.release.as_str());
            push!("\n");

            let mut dependencies = module.dependencies.iter().collect::<Vec<_>>();
            dependencies.sort_by(|left, right| left.module.cmp(&right.module));
            if dependencies.is_empty() {
                push!("dependencies = []\n");
            } else {
                push!("dependencies = [\n");
                for dependency in dependencies {
                    push!("  { module = ");
                    quoted!(dependency.module.as_str());
                    push!(", constraint = ");
                    quoted!(&dependency.constraint.to_string());
                    push!(" },\n");
                }
                push!("]\n");
            }
        }
        Ok(output.finish())
    }

    pub fn find(&self, path: &ModulePath) -> Option<&LockedModule> {
        self.modules.iter().find(|module| module.path == *path)
    }
}

/// Validate the closed selected-module graph represented by lock entries.
/// Root reachability is checked against `vo.mod` by `lock.rs`.
pub(crate) fn validate_locked_module_graph(modules: &[LockedModule]) -> Result<(), Error> {
    if modules.len() > crate::MAX_MODULE_DEPENDENCIES {
        return Err(Error::LockFileParse(format!(
            "module contains more than {} entries",
            crate::MAX_MODULE_DEPENDENCIES
        )));
    }
    for pair in modules.windows(2) {
        if pair[0].path >= pair[1].path {
            return Err(Error::LockFileParse(
                "[[module]] entries must be unique and sorted by path".to_string(),
            ));
        }
    }

    let mut selected = std::collections::BTreeMap::new();
    let mut edge_count = 0usize;
    for (module_index, module) in modules.iter().enumerate() {
        if !module.path.accepts_version(&module.version) {
            return Err(Error::LockFileParse(format!(
                "module[{module_index}].version {} is incompatible with module path {}",
                module.version, module.path
            )));
        }
        edge_count = edge_count
            .checked_add(module.dependencies.len())
            .ok_or_else(|| Error::ResolutionLimitExceeded {
                resource: "locked graph edge count".to_string(),
                limit: crate::MAX_SOLVER_GRAPH_EDGES,
            })?;
        if edge_count > crate::MAX_SOLVER_GRAPH_EDGES {
            return Err(Error::ResolutionLimitExceeded {
                resource: "locked graph edge count".to_string(),
                limit: crate::MAX_SOLVER_GRAPH_EDGES,
            });
        }
        for pair in module.dependencies.windows(2) {
            if pair[0].module >= pair[1].module {
                return Err(Error::LockFileParse(format!(
                    "module[{module_index}].dependencies must be unique and sorted by module path"
                )));
            }
        }
        for (dependency_index, dependency) in module.dependencies.iter().enumerate() {
            if dependency.module == module.path {
                return Err(Error::LockFileParse(format!(
                    "module {} must not depend on itself",
                    module.path
                )));
            }
            let lower_bound = ExactVersion::from_semver(dependency.constraint.version.clone());
            if !dependency.module.accepts_version(&lower_bound) {
                return Err(Error::LockFileParse(format!(
                    "module[{module_index}].dependencies[{dependency_index}] constraint {} is incompatible with module path {}",
                    dependency.constraint, dependency.module
                )));
            }
        }
        selected.insert(&module.path, module);
    }

    for module in modules {
        for dependency in &module.dependencies {
            let selected_dependency = selected.get(&dependency.module).ok_or_else(|| {
                Error::LockFileParse(format!(
                    "{} depends on {} {} but that module is absent from vo.lock",
                    module.path, dependency.module, dependency.constraint
                ))
            })?;
            if !dependency
                .constraint
                .satisfies(&selected_dependency.version)
            {
                return Err(Error::LockFileParse(format!(
                    "{} depends on {} {} but vo.lock selects {}",
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

pub(crate) fn validate_materialized_module_limits(modules: &[LockedModule]) -> Result<(), Error> {
    if modules.len() > crate::MAX_MODULE_DEPENDENCIES {
        return Err(Error::LockFileParse(format!(
            "materialized subset contains more than {} modules",
            crate::MAX_MODULE_DEPENDENCIES
        )));
    }
    let edge_count = modules.iter().try_fold(0usize, |total, module| {
        total
            .checked_add(module.dependencies.len())
            .ok_or_else(|| Error::ResolutionLimitExceeded {
                resource: "materialized dependency edge count".to_string(),
                limit: crate::MAX_SOLVER_GRAPH_EDGES,
            })
    })?;
    if edge_count > crate::MAX_SOLVER_GRAPH_EDGES {
        return Err(Error::ResolutionLimitExceeded {
            resource: "materialized dependency edge count".to_string(),
            limit: crate::MAX_SOLVER_GRAPH_EDGES,
        });
    }
    Ok(())
}

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

    const GOLDEN: &str = r#"version = 3

[root]
module = "github.com/acme/app"
vo = "^1.0.0"

[[module]]
path = "github.com/vo-lang/vogui"
version = "0.4.2"
vo = "^1.0.0"
release = "sha256:2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d"
dependencies = [
  { module = "github.com/vo-lang/voplay", constraint = "^0.7.0" },
]

[[module]]
path = "github.com/vo-lang/voplay"
version = "0.7.3"
vo = "^1.0.0"
release = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
dependencies = []
"#;

    #[test]
    fn parses_and_roundtrips_canonical_lock() {
        let lock = LockFile::parse(GOLDEN).unwrap();
        assert_eq!(lock.version, LOCK_FILE_VERSION);
        assert_eq!(lock.modules.len(), 2);
        assert_eq!(lock.render().unwrap(), GOLDEN);
    }

    #[test]
    fn rejects_noncanonical_lock_text() {
        assert!(LockFile::parse(&GOLDEN.replace("version = 3", "version=3")).is_err());
        assert!(LockFile::parse(&GOLDEN.replace(
            "path = \"github.com/vo-lang/vogui\"",
            "path = 'github.com/vo-lang/vogui'"
        ))
        .is_err());
        assert!(LockFile::parse(&format!("# generated\n{GOLDEN}")).is_err());
        assert!(LockFile::parse(GOLDEN.trim_end()).is_err());
    }

    #[test]
    fn rejects_removed_v2_fields_and_versions() {
        assert!(LockFile::parse(&GOLDEN.replace("version = 3", "version = 2")).is_err());
        assert!(LockFile::parse(&GOLDEN.replace(
            "release = \"sha256:2f7d",
            "commit = \"1111111111111111111111111111111111111111\"\nrelease = \"sha256:2f7d"
        ))
        .is_err());
        assert!(
            LockFile::parse(&GOLDEN.replace("version = \"0.4.2\"", "version = \"v0.4.2\""))
                .is_err()
        );
    }

    #[test]
    fn rejects_unsorted_modules_and_edges() {
        let unsorted_modules = GOLDEN
            .replace("github.com/vo-lang/vogui", "github.com/vo-lang/zz")
            .replace("github.com/vo-lang/voplay", "github.com/vo-lang/aa");
        assert!(LockFile::parse(&unsorted_modules).is_err());

        let duplicate_edge = GOLDEN.replace(
            "  { module = \"github.com/vo-lang/voplay\", constraint = \"^0.7.0\" },",
            "  { module = \"github.com/vo-lang/voplay\", constraint = \"^0.7.0\" },\n  { module = \"github.com/vo-lang/voplay\", constraint = \"^0.7.0\" },",
        );
        assert!(LockFile::parse(&duplicate_edge).is_err());
    }

    #[test]
    fn rejects_open_graph() {
        let missing = GOLDEN
            .split("\n[[module]]\npath = \"github.com/vo-lang/voplay\"")
            .next()
            .unwrap();
        assert!(LockFile::parse(missing).is_err());
    }

    #[test]
    fn rejects_empty_lock_graphs() {
        let empty = r#"version = 3

[root]
module = "github.com/acme/app"
vo = "^1.0.0"
"#;
        let error = LockFile::parse(empty).unwrap_err();
        assert!(
            error.to_string().contains("omit vo.lock"),
            "unexpected error: {error}"
        );
    }
}
