//! Stable, read-only project graph export for first-party tooling.
//!
//! Studio, Quickplay and other hosts consume this typed projection instead of
//! reimplementing `vo.mod`, `vo.lock`, workspace and cache-layout rules.

use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::path::Path;
#[cfg(test)]
use std::sync::{Mutex, OnceLock};

use serde::{Deserialize, Serialize};
use vo_common::vfs::{normalize_fs_path, RealFs};

use crate::project::ProjectContextOptions;
use crate::schema::modfile::Dependency;
use crate::workspace::WorkspaceDiscovery;
use crate::Error;

pub const PROJECT_GRAPH_FORMAT: u64 = 1;
const MAX_PROJECT_SNAPSHOT_ATTEMPTS: usize = 8;

#[cfg(test)]
struct ProjectSnapshotPause {
    reached: std::sync::mpsc::Sender<()>,
    resume: std::sync::mpsc::Receiver<()>,
}

#[cfg(test)]
static PROJECT_SNAPSHOT_CONTEXT_PAUSES: OnceLock<
    Mutex<BTreeMap<std::path::PathBuf, ProjectSnapshotPause>>,
> = OnceLock::new();

#[cfg(test)]
fn pause_project_snapshot_after_context_for_test(project_dir: &Path) {
    let pause = PROJECT_SNAPSHOT_CONTEXT_PAUSES
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(project_dir);
    if let Some(pause) = pause {
        pause.reached.send(()).unwrap();
        pause.resume.recv().unwrap();
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum GraphMode {
    Declared,
    Effective,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SnapshotOptions {
    pub mode: GraphMode,
    pub workspace: WorkspaceDiscovery,
}

impl SnapshotOptions {
    pub fn declared() -> Self {
        Self {
            mode: GraphMode::Declared,
            workspace: WorkspaceDiscovery::Disabled,
        }
    }

    pub fn effective() -> Self {
        Self {
            mode: GraphMode::Effective,
            workspace: WorkspaceDiscovery::Auto,
        }
    }
}

impl Default for SnapshotOptions {
    fn default() -> Self {
        Self::effective()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ProjectSnapshot {
    pub format: u64,
    pub mode: GraphMode,
    pub authority: SnapshotAuthority,
    pub root: SnapshotRoot,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace: Option<SnapshotWorkspace>,
    #[serde(deserialize_with = "deserialize_snapshot_modules")]
    pub modules: Vec<SnapshotModule>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SnapshotAuthority {
    Empty,
    Lock,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SnapshotRoot {
    pub module: String,
    pub vo: String,
    #[serde(deserialize_with = "deserialize_snapshot_dependencies")]
    pub dependencies: Vec<SnapshotDependency>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SnapshotWorkspace {
    pub file: String,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SnapshotModule {
    pub module: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
    pub vo: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub release: Option<String>,
    pub source: SnapshotSource,
    #[serde(deserialize_with = "deserialize_snapshot_dependencies")]
    pub dependencies: Vec<SnapshotDependency>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum SnapshotSource {
    Registry { directory: String },
    Workspace { directory: String },
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SnapshotDependency {
    pub module: String,
    pub constraint: String,
}

fn deserialize_snapshot_modules<'de, D>(deserializer: D) -> Result<Vec<SnapshotModule>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::schema::deserialize_bounded_vec(
        deserializer,
        crate::MAX_MODULE_DEPENDENCIES,
        "project snapshot modules",
    )
}

fn deserialize_snapshot_dependencies<'de, D>(
    deserializer: D,
) -> Result<Vec<SnapshotDependency>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::schema::deserialize_bounded_vec(
        deserializer,
        crate::MAX_MODULE_DEPENDENCIES,
        "snapshot dependencies",
    )
}

impl ProjectSnapshot {
    pub fn capture(
        project_dir: &Path,
        cache_root: &Path,
        options: &SnapshotOptions,
    ) -> Result<Self, Error> {
        let fs = RealFs::new(".");
        let context_options = ProjectContextOptions::new(match options.mode {
            GraphMode::Declared => WorkspaceDiscovery::Disabled,
            GraphMode::Effective => options.workspace.clone(),
        });
        for _ in 0..MAX_PROJECT_SNAPSHOT_ATTEMPTS {
            let context = load_snapshot_project_context(&fs, project_dir, &context_options)?;

            #[cfg(test)]
            pause_project_snapshot_after_context_for_test(project_dir);

            let snapshot = Self::from_project_context(&context, cache_root, options)?;
            let final_context = load_snapshot_project_context(&fs, project_dir, &context_options)?;
            if !same_project_context_generation(&context, &final_context) {
                continue;
            }
            let final_snapshot = Self::from_project_context(&final_context, cache_root, options)?;
            if snapshot == final_snapshot {
                return Ok(snapshot);
            }
        }
        Err(Error::SourceScan(
            "project snapshot inputs changed during every bounded capture attempt".to_string(),
        ))
    }

    fn from_project_context(
        context: &crate::project::ProjectContext,
        cache_root: &Path,
        options: &SnapshotOptions,
    ) -> Result<Self, Error> {
        let project_plan = context.project_plan();
        let mod_file = project_plan.mod_file().cloned().ok_or_else(|| {
            Error::ModFileParse("project snapshot requires a vo.mod project".to_string())
        })?;

        let mut modules = match options.mode {
            GraphMode::Declared => declared_modules(project_plan.lock_file(), cache_root)?,
            GraphMode::Effective => effective_modules(context, cache_root)?,
        };
        modules.sort_by(|left, right| left.module.cmp(&right.module));

        let snapshot = Self {
            format: PROJECT_GRAPH_FORMAT,
            mode: options.mode,
            authority: match context.authority() {
                crate::project::ProjectAuthority::Empty => SnapshotAuthority::Empty,
                crate::project::ProjectAuthority::Lock => SnapshotAuthority::Lock,
            },
            root: SnapshotRoot {
                module: mod_file.module.as_str().to_string(),
                vo: mod_file.vo.to_string(),
                dependencies: declared_dependencies(&mod_file.dependencies),
            },
            workspace: match (options.mode, context.workspace_file()) {
                (GraphMode::Effective, Some(file)) => Some(SnapshotWorkspace {
                    file: utf8_path(file, "workspace file")?,
                }),
                _ => None,
            },
            modules,
        };
        snapshot.validate()?;
        Ok(snapshot)
    }

    pub fn parse(bytes: &[u8]) -> Result<Self, Error> {
        if bytes.len() > crate::MAX_LOCK_FILE_BYTES {
            return Err(Error::LockFileParse(format!(
                "project snapshot exceeds the {}-byte limit",
                crate::MAX_LOCK_FILE_BYTES
            )));
        }
        let snapshot: Self = serde_json::from_slice(bytes)
            .map_err(|error| Error::LockFileParse(format!("snapshot JSON parse error: {error}")))?;
        snapshot.validate()?;
        if snapshot.render()? != bytes {
            return Err(Error::LockFileParse(
                "project snapshot must use the canonical JSON encoding".to_string(),
            ));
        }
        Ok(snapshot)
    }

    pub fn render(&self) -> Result<Vec<u8>, Error> {
        self.validate()?;
        let mut output = crate::schema::CanonicalJsonWriter::new(crate::MAX_LOCK_FILE_BYTES)
            .map_err(Error::LockFileParse)?;

        macro_rules! raw {
            ($value:expr) => {
                output.push_raw($value).map_err(Error::LockFileParse)?
            };
        }
        macro_rules! string {
            ($value:expr) => {
                output.push_string($value).map_err(Error::LockFileParse)?
            };
        }

        raw!("{\n  \"format\": ");
        output.push_u64(self.format).map_err(Error::LockFileParse)?;
        raw!(",\n  \"mode\": ");
        string!(match self.mode {
            GraphMode::Declared => "declared",
            GraphMode::Effective => "effective",
        });
        raw!(",\n  \"authority\": ");
        string!(match self.authority {
            SnapshotAuthority::Empty => "empty",
            SnapshotAuthority::Lock => "lock",
        });
        raw!(",\n  \"root\": {\n    \"module\": ");
        string!(&self.root.module);
        raw!(",\n    \"vo\": ");
        string!(&self.root.vo);
        raw!(",\n    \"dependencies\": ");
        render_snapshot_dependencies(
            &mut output,
            &self.root.dependencies,
            "    ",
            "      ",
            "        ",
        )
        .map_err(Error::LockFileParse)?;
        raw!("\n  }");
        if let Some(workspace) = &self.workspace {
            raw!(",\n  \"workspace\": {\n    \"file\": ");
            string!(&workspace.file);
            raw!("\n  }");
        }
        if self.modules.is_empty() {
            raw!(",\n  \"modules\": []\n}\n");
        } else {
            raw!(",\n  \"modules\": [\n");
            for (index, module) in self.modules.iter().enumerate() {
                raw!("    {\n      \"module\": ");
                string!(&module.module);
                if let Some(version) = &module.version {
                    raw!(",\n      \"version\": ");
                    string!(version);
                }
                raw!(",\n      \"vo\": ");
                string!(&module.vo);
                if let Some(release) = &module.release {
                    raw!(",\n      \"release\": ");
                    string!(release);
                }
                raw!(",\n      \"source\": {\n        \"kind\": ");
                let (kind, directory) = match &module.source {
                    SnapshotSource::Registry { directory } => ("registry", directory),
                    SnapshotSource::Workspace { directory } => ("workspace", directory),
                };
                string!(kind);
                raw!(",\n        \"directory\": ");
                string!(directory);
                raw!("\n      },\n      \"dependencies\": ");
                render_snapshot_dependencies(
                    &mut output,
                    &module.dependencies,
                    "      ",
                    "        ",
                    "          ",
                )
                .map_err(Error::LockFileParse)?;
                raw!("\n    }");
                if index + 1 != self.modules.len() {
                    raw!(",");
                }
                raw!("\n");
            }
            raw!("  ]\n}\n");
        }
        Ok(output.finish())
    }

    /// Return the deterministic shortest dependency chain from the root to a
    /// selected module in this exact graph authority.
    pub fn dependency_chain(&self, target: &str) -> Result<Vec<String>, Error> {
        self.validate()?;
        let target = crate::identity::ModulePath::parse(target)?;
        if self.root.module == target.as_str() {
            return Ok(vec![self.root.module.clone()]);
        }

        let modules = self
            .modules
            .iter()
            .map(|module| (module.module.as_str(), module))
            .collect::<BTreeMap<_, _>>();
        if !modules.contains_key(target.as_str()) {
            return Err(Error::DependencyGraph(format!(
                "{target} is not in the module dependency graph"
            )));
        }

        let mut queue = VecDeque::<&str>::new();
        let mut predecessor = BTreeMap::<&str, Option<&str>>::new();
        for dependency in &self.root.dependencies {
            let module = dependency.module.as_str();
            if predecessor.insert(module, None).is_none() {
                queue.push_back(module);
            }
        }

        while let Some(module) = queue.pop_front() {
            if module == target.as_str() {
                let mut chain = vec![module.to_string()];
                let mut cursor = module;
                while let Some(Some(parent)) = predecessor.get(cursor) {
                    cursor = parent;
                    chain.push(cursor.to_string());
                }
                chain.push(self.root.module.clone());
                chain.reverse();
                return Ok(chain);
            }
            let selected = modules.get(module).ok_or_else(|| {
                Error::DependencyGraph(format!(
                    "snapshot dependency {module} has no selected module"
                ))
            })?;
            for dependency in &selected.dependencies {
                let child = dependency.module.as_str();
                if predecessor.contains_key(child) {
                    continue;
                }
                predecessor.insert(child, Some(module));
                queue.push_back(child);
            }
        }

        Err(Error::DependencyGraph(format!(
            "{target} is selected but unreachable from the root module"
        )))
    }

    pub fn validate(&self) -> Result<(), Error> {
        if self.format != PROJECT_GRAPH_FORMAT {
            return Err(Error::LockFileParse(format!(
                "unsupported project graph format: {}",
                self.format
            )));
        }
        if self.mode == GraphMode::Declared && self.workspace.is_some() {
            return Err(Error::LockFileParse(
                "declared graph snapshots must not contain workspace state".to_string(),
            ));
        }
        if let Some(workspace) = &self.workspace {
            validate_export_path(&workspace.file, "workspace file")?;
        }
        let root_module = crate::identity::ModIdentity::parse(&self.root.module)?;
        let root_vo = crate::version::ToolchainConstraint::parse(&self.root.vo)?;
        validate_dependencies(&self.root.dependencies, "root dependencies")?;
        match self.authority {
            SnapshotAuthority::Empty
                if !self.root.dependencies.is_empty() || !self.modules.is_empty() =>
            {
                return Err(Error::DependencyGraph(
                    "empty snapshot authority requires an empty dependency graph".to_string(),
                ));
            }
            SnapshotAuthority::Lock
                if self.root.dependencies.is_empty() || self.modules.is_empty() =>
            {
                return Err(Error::DependencyGraph(
                    "lock snapshot authority requires a non-empty locked dependency graph"
                        .to_string(),
                ));
            }
            _ => {}
        }

        if self.modules.len() > crate::MAX_MODULE_DEPENDENCIES {
            return Err(Error::LockFileParse(format!(
                "project snapshot contains more than {} modules",
                crate::MAX_MODULE_DEPENDENCIES
            )));
        }
        let mut previous = None::<&str>;
        let mut paths = BTreeSet::new();
        for module in &self.modules {
            let path = crate::identity::ModulePath::parse(&module.module)?;
            if root_module.as_str() == path.as_str() {
                return Err(Error::LockFileParse(format!(
                    "root module {} appears in snapshot modules",
                    module.module
                )));
            }
            if previous.is_some_and(|previous| previous >= module.module.as_str()) {
                return Err(Error::LockFileParse(
                    "project snapshot modules must be strictly sorted by module".to_string(),
                ));
            }
            previous = Some(&module.module);
            paths.insert(module.module.as_str());
            validate_module_selection(module, &path, &root_vo, self.authority)?;
            validate_source(
                module,
                &module.source,
                self.mode,
                self.authority,
                &path,
                self.workspace.is_some(),
            )?;
            validate_dependencies(
                &module.dependencies,
                &format!("{} dependencies", module.module),
            )?;
        }

        let modules_by_path = self
            .modules
            .iter()
            .map(|module| (module.module.as_str(), module))
            .collect::<BTreeMap<_, _>>();
        let mut edge_count = self.root.dependencies.len();
        for dependency in &self.root.dependencies {
            if !paths.contains(dependency.module.as_str()) {
                return Err(Error::DependencyGraph(format!(
                    "snapshot root dependency {} is absent from modules",
                    dependency.module
                )));
            }
            validate_edge_selection(dependency, &modules_by_path, self.authority)?;
        }
        for module in &self.modules {
            edge_count = edge_count
                .checked_add(module.dependencies.len())
                .ok_or_else(|| {
                    Error::DependencyGraph("snapshot dependency edge count overflow".to_string())
                })?;
            if edge_count > crate::MAX_SOLVER_GRAPH_EDGES {
                return Err(Error::DependencyGraph(format!(
                    "snapshot contains more than {} dependency edges",
                    crate::MAX_SOLVER_GRAPH_EDGES
                )));
            }
            for dependency in &module.dependencies {
                if dependency.module == module.module {
                    return Err(Error::DependencyGraph(format!(
                        "snapshot module {} must not depend on itself",
                        module.module
                    )));
                }
                if dependency.module == self.root.module {
                    return Err(Error::DependencyGraph(format!(
                        "snapshot module {} must not depend on root module {}",
                        module.module, self.root.module
                    )));
                }
                if !paths.contains(dependency.module.as_str()) {
                    return Err(Error::DependencyGraph(format!(
                        "snapshot dependency {} -> {} is open",
                        module.module, dependency.module
                    )));
                }
                validate_edge_selection(dependency, &modules_by_path, self.authority)?;
            }
        }
        let mut reachable = BTreeSet::new();
        let mut queue = self
            .root
            .dependencies
            .iter()
            .map(|dependency| dependency.module.as_str())
            .collect::<VecDeque<_>>();
        while let Some(module) = queue.pop_front() {
            if !reachable.insert(module) {
                continue;
            }
            if let Some(selected) = modules_by_path.get(module) {
                queue.extend(
                    selected
                        .dependencies
                        .iter()
                        .map(|dependency| dependency.module.as_str()),
                );
            }
        }
        if reachable.len() != self.modules.len() {
            let orphaned = self
                .modules
                .iter()
                .find(|module| !reachable.contains(module.module.as_str()))
                .map(|module| module.module.as_str())
                .unwrap_or("<unknown>");
            return Err(Error::DependencyGraph(format!(
                "snapshot module {orphaned} is unreachable from the root"
            )));
        }
        Ok(())
    }
}

fn render_snapshot_dependencies(
    output: &mut crate::schema::CanonicalJsonWriter,
    dependencies: &[SnapshotDependency],
    field_indent: &str,
    item_indent: &str,
    value_indent: &str,
) -> Result<(), String> {
    if dependencies.is_empty() {
        return output.push_raw("[]");
    }
    output.push_raw("[\n")?;
    for (index, dependency) in dependencies.iter().enumerate() {
        output.push_raw(item_indent)?;
        output.push_raw("{\n")?;
        output.push_raw(value_indent)?;
        output.push_raw("\"module\": ")?;
        output.push_string(&dependency.module)?;
        output.push_raw(",\n")?;
        output.push_raw(value_indent)?;
        output.push_raw("\"constraint\": ")?;
        output.push_string(&dependency.constraint)?;
        output.push_raw("\n")?;
        output.push_raw(item_indent)?;
        output.push_raw("}")?;
        if index + 1 != dependencies.len() {
            output.push_raw(",")?;
        }
        output.push_raw("\n")?;
    }
    output.push_raw(field_indent)?;
    output.push_raw("]")
}

fn load_snapshot_project_context(
    fs: &RealFs,
    project_dir: &Path,
    options: &ProjectContextOptions,
) -> Result<crate::project::ProjectContext, Error> {
    crate::project::load_project_context_with_options(fs, project_dir, options)
        .map_err(|error| Error::SourceScan(format!("project snapshot failed: {error}")))
}

fn same_project_context_generation(
    initial: &crate::project::ProjectContext,
    current: &crate::project::ProjectContext,
) -> bool {
    initial.has_same_root_authority(current)
        && initial.workspace_modules() == current.workspace_modules()
        && initial.workspace_file().map(normalize_fs_path)
            == current.workspace_file().map(normalize_fs_path)
        && initial.workspace_generation() == current.workspace_generation()
        && normalized_workspace_sources(initial.workspace_sources())
            == normalized_workspace_sources(current.workspace_sources())
        && normalized_input_files(initial.validated_input_files())
            == normalized_input_files(current.validated_input_files())
}

fn normalized_workspace_sources(
    sources: &std::collections::HashMap<String, std::path::PathBuf>,
) -> BTreeMap<String, std::path::PathBuf> {
    sources
        .iter()
        .map(|(module, path)| (module.clone(), normalize_fs_path(path)))
        .collect()
}

fn normalized_input_files(paths: &[std::path::PathBuf]) -> BTreeSet<std::path::PathBuf> {
    paths.iter().map(|path| normalize_fs_path(path)).collect()
}

fn validate_edge_selection(
    dependency: &SnapshotDependency,
    modules: &BTreeMap<&str, &SnapshotModule>,
    authority: SnapshotAuthority,
) -> Result<(), Error> {
    let target = modules.get(dependency.module.as_str()).ok_or_else(|| {
        Error::DependencyGraph(format!(
            "snapshot dependency {} has no selected module",
            dependency.module
        ))
    })?;
    if authority != SnapshotAuthority::Lock {
        return Err(Error::DependencyGraph(
            "empty snapshot authority cannot contain dependency edges".to_string(),
        ));
    }
    let constraint = crate::version::DepConstraint::parse(&dependency.constraint)?;
    let version = crate::version::ExactVersion::parse(
        target
            .version
            .as_deref()
            .ok_or_else(|| Error::LockFileParse("lock module is missing version".to_string()))?,
    )?;
    if !constraint.satisfies(&version) {
        return Err(Error::DependencyGraph(format!(
            "snapshot selects {}@{}, which does not satisfy {}",
            dependency.module, version, dependency.constraint
        )));
    }
    Ok(())
}

fn declared_modules(
    lock_file: Option<&crate::schema::lockfile::LockFile>,
    cache_root: &Path,
) -> Result<Vec<SnapshotModule>, Error> {
    lock_file
        .map(|lock| {
            lock.modules
                .iter()
                .map(|locked| registry_snapshot_module(locked, cache_root))
                .collect()
        })
        .transpose()
        .map(Option::unwrap_or_default)
}

fn effective_modules(
    context: &crate::project::ProjectContext,
    cache_root: &Path,
) -> Result<Vec<SnapshotModule>, Error> {
    match context.authority() {
        crate::project::ProjectAuthority::Empty => Ok(Vec::new()),
        crate::project::ProjectAuthority::Lock => effective_locked_modules(
            context.project_plan().lock_file().ok_or_else(|| {
                Error::DependencyGraph("lock authority is missing its captured vo.lock".to_string())
            })?,
            context.workspace_modules(),
            cache_root,
        ),
    }
}

/// Preserve the root lock as the sole graph authority and change only the
/// physical source selected for matching workspace modules.
fn effective_locked_modules(
    lock_file: &crate::schema::lockfile::LockFile,
    workspace_sources: &[crate::project::WorkspaceModule],
    cache_root: &Path,
) -> Result<Vec<SnapshotModule>, Error> {
    let workspace_sources = workspace_sources
        .iter()
        .map(|module| (module.module().as_str(), module))
        .collect::<BTreeMap<_, _>>();
    lock_file
        .modules
        .iter()
        .map(|locked| {
            let Some(workspace) = workspace_sources.get(locked.path.as_str()) else {
                return registry_snapshot_module(locked, cache_root);
            };
            workspace_snapshot_module(locked, workspace)
        })
        .collect()
}

fn workspace_snapshot_module(
    locked: &crate::schema::lockfile::LockedModule,
    workspace: &crate::project::WorkspaceModule,
) -> Result<SnapshotModule, Error> {
    Ok(SnapshotModule {
        module: locked.path.to_string(),
        version: Some(locked.version.to_string()),
        vo: workspace.mod_file().vo.to_string(),
        release: None,
        source: SnapshotSource::Workspace {
            directory: utf8_path(workspace.directory(), "workspace member")?,
        },
        dependencies: declared_dependencies(&workspace.mod_file().dependencies),
    })
}

fn registry_snapshot_module(
    locked: &crate::schema::lockfile::LockedModule,
    cache_root: &Path,
) -> Result<SnapshotModule, Error> {
    let directory = crate::cache::layout::cache_dir(cache_root, &locked.path, &locked.version);
    let raw = std::fs::read_to_string(directory.join("vo.release.json"))?;
    let release =
        crate::registry::parse_requested_release_manifest(&raw, &locked.path, &locked.version)?;
    Ok(SnapshotModule {
        module: locked.path.as_str().to_string(),
        version: Some(locked.version.to_string()),
        vo: release.vo.to_string(),
        release: locked.release.as_ref().map(ToString::to_string),
        source: SnapshotSource::Registry {
            directory: utf8_path(&directory, "module cache directory")?,
        },
        dependencies: release
            .dependencies
            .iter()
            .map(|dependency| SnapshotDependency {
                module: dependency.module.to_string(),
                constraint: dependency.constraint.to_string(),
            })
            .collect(),
    })
}

fn declared_dependencies(dependencies: &[Dependency]) -> Vec<SnapshotDependency> {
    let mut dependencies = dependencies
        .iter()
        .map(|dependency| SnapshotDependency {
            module: dependency.module.as_str().to_string(),
            constraint: dependency.constraint.to_string(),
        })
        .collect::<Vec<_>>();
    dependencies.sort();
    dependencies
}

fn validate_dependencies(dependencies: &[SnapshotDependency], label: &str) -> Result<(), Error> {
    if dependencies.len() > crate::MAX_MODULE_DEPENDENCIES {
        return Err(Error::DependencyGraph(format!(
            "{label} contains more than {} entries",
            crate::MAX_MODULE_DEPENDENCIES
        )));
    }
    let mut previous_module = None::<&str>;
    for dependency in dependencies {
        let module = crate::identity::ModulePath::parse(&dependency.module)?;
        let constraint = crate::version::DepConstraint::parse(&dependency.constraint)?;
        let lower_bound = crate::version::ExactVersion::from_semver(constraint.version.clone());
        if !module.accepts_version(&lower_bound) {
            return Err(Error::DependencyGraph(format!(
                "{label} constraint {constraint} is incompatible with module {module}"
            )));
        }
        if previous_module.is_some_and(|previous| previous >= dependency.module.as_str()) {
            return Err(Error::DependencyGraph(format!(
                "{label} must be unique and strictly sorted by module"
            )));
        }
        previous_module = Some(&dependency.module);
    }
    Ok(())
}

fn validate_module_selection(
    selected: &SnapshotModule,
    module: &crate::identity::ModulePath,
    root_vo: &crate::version::ToolchainConstraint,
    authority: SnapshotAuthority,
) -> Result<(), Error> {
    let vo = crate::version::ToolchainConstraint::parse(&selected.vo)?;
    if !root_vo.is_subset_of(&vo) {
        return Err(Error::DependencyToolchainMismatch {
            module: module.to_string(),
            project_constraint: root_vo.to_string(),
            dependency_constraint: vo.to_string(),
        });
    }
    match authority {
        SnapshotAuthority::Lock => {
            let Some(version) = selected.version.as_deref() else {
                return Err(Error::LockFileParse(format!(
                    "lock snapshot module {module} requires a version"
                )));
            };
            let version = crate::version::ExactVersion::parse(version)?;
            if !module.accepts_version(&version) {
                return Err(Error::LockFileParse(format!(
                    "snapshot version {version} is incompatible with module {module}"
                )));
            }
            if let Some(release) = selected.release.as_deref() {
                crate::digest::Digest::parse(release)?;
            }
            Ok(())
        }
        SnapshotAuthority::Empty => Err(Error::DependencyGraph(
            "empty snapshot authority cannot contain modules".to_string(),
        )),
    }
}

fn validate_source(
    selected: &SnapshotModule,
    source: &SnapshotSource,
    mode: GraphMode,
    authority: SnapshotAuthority,
    module: &crate::identity::ModulePath,
    has_workspace_provenance: bool,
) -> Result<(), Error> {
    match source {
        SnapshotSource::Registry { directory } => {
            if authority != SnapshotAuthority::Lock {
                return Err(Error::LockFileParse(format!(
                    "workspace snapshot authority requires workspace source for {module}"
                )));
            }
            if selected.release.is_none() {
                return Err(Error::LockFileParse(format!(
                    "registry source for {module} requires a release digest"
                )));
            }
            validate_export_path(directory, "cache directory")?;
            Ok(())
        }
        SnapshotSource::Workspace { directory } => {
            if mode != GraphMode::Effective {
                return Err(Error::LockFileParse(
                    "declared graph snapshots cannot select workspace sources".to_string(),
                ));
            }
            if !has_workspace_provenance {
                return Err(Error::LockFileParse(format!(
                    "workspace source for {module} requires workspace provenance"
                )));
            }
            if selected.release.is_some() {
                return Err(Error::LockFileParse(format!(
                    "workspace source for {module} must omit a release digest"
                )));
            }
            validate_export_path(directory, "workspace directory")?;
            Ok(())
        }
    }
}

fn validate_export_path(path: &str, label: &str) -> Result<(), Error> {
    if path.is_empty() || path.chars().any(vo_common::identifier::is_unicode_control) {
        return Err(Error::LockFileParse(format!(
            "snapshot {label} must be a non-empty path without control characters"
        )));
    }
    Ok(())
}

fn utf8_path(path: &Path, label: &str) -> Result<String, Error> {
    path.to_str().map(str::to_string).ok_or_else(|| {
        Error::SourceScan(format!(
            "{label} is not valid UTF-8 and cannot be exported: {}",
            path.display()
        ))
    })
}

pub fn render_graph(snapshot: &ProjectSnapshot) -> Result<String, Error> {
    snapshot.validate()?;
    let mut output = crate::schema::BoundedTextOutput::new(crate::MAX_LOCK_FILE_BYTES)
        .map_err(Error::LockFileParse)?;
    push_graph_text(&mut output, "root ")?;
    push_graph_text(&mut output, &snapshot.root.module)?;
    push_graph_text(&mut output, " vo ")?;
    push_graph_text(&mut output, &snapshot.root.vo)?;
    push_graph_text(&mut output, "\n")?;
    if let Some(workspace) = &snapshot.workspace {
        push_graph_text(&mut output, "workspace ")?;
        push_graph_text(&mut output, &workspace.file)?;
        push_graph_text(&mut output, "\n")?;
    }
    for dependency in &snapshot.root.dependencies {
        push_graph_text(&mut output, &snapshot.root.module)?;
        push_graph_text(&mut output, " -> ")?;
        push_graph_text(&mut output, &dependency.module)?;
        push_graph_text(&mut output, " ")?;
        push_graph_text(&mut output, &dependency.constraint)?;
        push_graph_text(&mut output, "\n")?;
    }
    for module in &snapshot.modules {
        push_graph_text(&mut output, "module ")?;
        push_graph_text(&mut output, &module.module)?;
        match &module.source {
            SnapshotSource::Registry { directory } => {
                push_graph_text(&mut output, "@")?;
                push_graph_text(
                    &mut output,
                    module.version.as_deref().unwrap_or("<missing>"),
                )?;
                push_graph_text(&mut output, " registry ")?;
                push_graph_text(&mut output, directory)?;
            }
            SnapshotSource::Workspace { directory } => match module.version.as_deref() {
                Some(version) => {
                    push_graph_text(&mut output, "@")?;
                    push_graph_text(&mut output, version)?;
                    push_graph_text(&mut output, " workspace ")?;
                    push_graph_text(&mut output, directory)?;
                }
                None => {
                    push_graph_text(&mut output, " workspace ")?;
                    push_graph_text(&mut output, directory)?;
                }
            },
        }
        push_graph_text(&mut output, "\n")?;
        for dependency in &module.dependencies {
            push_graph_text(&mut output, &module.module)?;
            push_graph_text(&mut output, " -> ")?;
            push_graph_text(&mut output, &dependency.module)?;
            push_graph_text(&mut output, " ")?;
            push_graph_text(&mut output, &dependency.constraint)?;
            push_graph_text(&mut output, "\n")?;
        }
    }
    Ok(output.finish())
}

fn push_graph_text(
    output: &mut crate::schema::BoundedTextOutput,
    value: &str,
) -> Result<(), Error> {
    output.push_str(value).map_err(Error::LockFileParse)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshot_options_default_to_the_effective_build_authority() {
        assert_eq!(SnapshotOptions::default().mode, GraphMode::Effective);
    }

    #[test]
    fn canonical_snapshot_json_has_explicit_field_order_and_string_spelling() {
        let mut snapshot = ProjectSnapshot {
            format: PROJECT_GRAPH_FORMAT,
            mode: GraphMode::Declared,
            authority: SnapshotAuthority::Lock,
            root: SnapshotRoot {
                module: "github.com/acme/app".into(),
                vo: "0.2.0".into(),
                dependencies: vec![SnapshotDependency {
                    module: "github.com/acme/lib".into(),
                    constraint: "^1.0.0".into(),
                }],
            },
            workspace: None,
            modules: vec![SnapshotModule {
                module: "github.com/acme/lib".into(),
                version: Some("1.0.0".into()),
                vo: "0.2.0".into(),
                release: Some(
                    concat!(
                        "sha256:",
                        "0000000000000000000000000000000000000000000000000000000000000000"
                    )
                    .into(),
                ),
                source: SnapshotSource::Registry {
                    directory: "缓存/\"库\"\\数据".into(),
                },
                dependencies: vec![],
            }],
        };
        let expected = concat!(
            "{\n",
            "  \"format\": 1,\n",
            "  \"mode\": \"declared\",\n",
            "  \"authority\": \"lock\",\n",
            "  \"root\": {\n",
            "    \"module\": \"github.com/acme/app\",\n",
            "    \"vo\": \"0.2.0\",\n",
            "    \"dependencies\": [\n",
            "      {\n",
            "        \"module\": \"github.com/acme/lib\",\n",
            "        \"constraint\": \"^1.0.0\"\n",
            "      }\n",
            "    ]\n",
            "  },\n",
            "  \"modules\": [\n",
            "    {\n",
            "      \"module\": \"github.com/acme/lib\",\n",
            "      \"version\": \"1.0.0\",\n",
            "      \"vo\": \"0.2.0\",\n",
            "      \"release\": \"sha256:",
            "0000000000000000000000000000000000000000000000000000000000000000\",\n",
            "      \"source\": {\n",
            "        \"kind\": \"registry\",\n",
            "        \"directory\": \"缓存/\\\"库\\\"\\\\数据\"\n",
            "      },\n",
            "      \"dependencies\": []\n",
            "    }\n",
            "  ]\n",
            "}\n",
        );

        let rendered = snapshot.render().unwrap();
        assert_eq!(rendered, expected.as_bytes());
        assert_eq!(ProjectSnapshot::parse(&rendered).unwrap(), snapshot);

        snapshot.modules[0].source = SnapshotSource::Registry {
            directory: "cache/\u{0001}".into(),
        };
        let error = snapshot.render().unwrap_err();
        assert!(error.to_string().contains("control characters"), "{error}");
    }

    #[test]
    fn captures_and_round_trips_a_lockless_declared_graph_without_lock_side_effects() {
        let project = tempfile::tempdir().unwrap();
        std::fs::write(
            project.path().join("vo.mod"),
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        let cache = tempfile::tempdir().unwrap();

        let snapshot =
            ProjectSnapshot::capture(project.path(), cache.path(), &SnapshotOptions::declared())
                .unwrap();
        assert_eq!(snapshot.mode, GraphMode::Declared);
        assert_eq!(snapshot.authority, SnapshotAuthority::Empty);
        assert!(snapshot.modules.is_empty());
        assert!(snapshot.workspace.is_none());
        assert!(!project.path().join(".vo-project.lock").exists());

        let bytes = snapshot.render().unwrap();
        assert_eq!(ProjectSnapshot::parse(&bytes).unwrap(), snapshot);
        assert_eq!(
            render_graph(&snapshot).unwrap(),
            "root github.com/acme/app vo 0.1.0\n"
        );
    }

    #[test]
    fn parser_rejects_unknown_fields_and_noncanonical_module_order() {
        let unknown_schema = br#"{
  "unexpected": 2,
  "mode": "declared",
  "authority": "empty",
  "root": {
    "module": "github.com/acme/app",
    "vo": "0.2.0",
    "dependencies": []
  },
  "modules": []
}
"#;
        let error = ProjectSnapshot::parse(unknown_schema).unwrap_err();
        assert!(error.to_string().contains("unknown field"), "{error}");

        let unknown = br#"{
          "format": 1,
          "mode": "declared",
          "authority": "empty",
          "root": {"module":"github.com/acme/app","vo":"^0.2.0","dependencies":[]},
          "modules": [],
          "extra": true
        }"#;
        assert!(ProjectSnapshot::parse(unknown).is_err());

        let snapshot = ProjectSnapshot {
            format: PROJECT_GRAPH_FORMAT,
            mode: GraphMode::Declared,
            authority: SnapshotAuthority::Lock,
            root: SnapshotRoot {
                module: "github.com/acme/app".into(),
                vo: "0.2.0".into(),
                dependencies: vec![
                    SnapshotDependency {
                        module: "github.com/acme/a".into(),
                        constraint: "^1.0.0".into(),
                    },
                    SnapshotDependency {
                        module: "github.com/acme/z".into(),
                        constraint: "^1.0.0".into(),
                    },
                ],
            },
            workspace: None,
            modules: vec![
                fixture_module("github.com/acme/z"),
                fixture_module("github.com/acme/a"),
            ],
        };
        assert!(snapshot.validate().is_err());
    }

    #[test]
    fn deserializer_rejects_oversized_collections_before_semantic_validation() {
        fn repeated_json_array(value: &str, count: usize) -> String {
            let mut output = String::with_capacity(
                value
                    .len()
                    .checked_add(1)
                    .and_then(|size| size.checked_mul(count))
                    .and_then(|size| size.checked_add(2))
                    .unwrap(),
            );
            output.push('[');
            for index in 0..count {
                if index != 0 {
                    output.push(',');
                }
                output.push_str(value);
            }
            output.push(']');
            output
        }

        let dependency = r#"{"module":"github.com/acme/a","constraint":"^1.0.0"}"#;
        let root = format!(
            r#"{{"module":"github.com/acme/app","vo":"^0.1.0","dependencies":{}}}"#,
            repeated_json_array(dependency, crate::MAX_MODULE_DEPENDENCIES + 1)
        );
        let error = serde_json::from_str::<SnapshotRoot>(&root)
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("snapshot dependencies contains more than"),
            "{error}"
        );

        let module = concat!(
            r#"{"module":"github.com/acme/a","version":"1.0.0","vo":"^0.1.0","release":""#,
            "0000000000000000000000000000000000000000000000000000000000000000",
            r#"","source":{"kind":"registry","directory":"cache/a"},"dependencies":[]}"#,
        );
        let snapshot = format!(
            concat!(
                r#"{{"format":1,"mode":"declared","authority":"lock","root":{{"module":"github.com/acme/app","vo":"0.1.0","dependencies":[]}},"modules":{} }}"#,
            ),
            repeated_json_array(module, crate::MAX_MODULE_DEPENDENCIES + 1)
        );
        let error = serde_json::from_str::<ProjectSnapshot>(&snapshot)
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("project snapshot modules contains more than"),
            "{error}"
        );
    }

    #[test]
    fn validator_requires_workspace_provenance_and_rejects_self_edges() {
        let mut module = fixture_module("github.com/acme/a");
        module.release = None;
        module.source = SnapshotSource::Workspace {
            directory: "workspace/a".into(),
        };
        let mut snapshot = ProjectSnapshot {
            format: PROJECT_GRAPH_FORMAT,
            mode: GraphMode::Effective,
            authority: SnapshotAuthority::Lock,
            root: SnapshotRoot {
                module: "github.com/acme/app".into(),
                vo: "0.2.0".into(),
                dependencies: vec![SnapshotDependency {
                    module: "github.com/acme/a".into(),
                    constraint: "^1.0.0".into(),
                }],
            },
            workspace: None,
            modules: vec![module],
        };
        assert!(snapshot
            .validate()
            .unwrap_err()
            .to_string()
            .contains("workspace provenance"));

        snapshot.workspace = Some(SnapshotWorkspace {
            file: "workspace/vo.work".into(),
        });
        snapshot.modules[0].dependencies.push(SnapshotDependency {
            module: "github.com/acme/a".into(),
            constraint: "^1.0.0".into(),
        });
        assert!(snapshot
            .validate()
            .unwrap_err()
            .to_string()
            .contains("must not depend on itself"));
    }

    #[test]
    fn workspace_snapshot_schema_requires_locked_versions_and_omits_releases() {
        let mut snapshot = ProjectSnapshot {
            format: PROJECT_GRAPH_FORMAT,
            mode: GraphMode::Effective,
            authority: SnapshotAuthority::Lock,
            root: SnapshotRoot {
                module: "github.com/acme/app".into(),
                vo: "0.2.0".into(),
                dependencies: vec![SnapshotDependency {
                    module: "github.com/acme/a".into(),
                    constraint: "^1.0.0".into(),
                }],
            },
            workspace: Some(SnapshotWorkspace {
                file: "workspace/vo.work".into(),
            }),
            modules: vec![SnapshotModule {
                module: "github.com/acme/a".into(),
                version: Some("1.0.0".into()),
                vo: "0.2.0".into(),
                release: None,
                source: SnapshotSource::Workspace {
                    directory: "workspace/a".into(),
                },
                dependencies: vec![],
            }],
        };
        snapshot.validate().unwrap();
        let rendered = snapshot.render().unwrap();
        assert_eq!(ProjectSnapshot::parse(&rendered).unwrap(), snapshot);

        snapshot.modules[0].release =
            Some(crate::digest::Digest::from_sha256(b"workspace-release").to_string());
        assert!(snapshot
            .validate()
            .unwrap_err()
            .to_string()
            .contains("must omit a release digest"));
        snapshot.modules[0].release = None;
        snapshot.modules[0].source = SnapshotSource::Registry {
            directory: "cache/a".into(),
        };
        assert!(snapshot
            .validate()
            .unwrap_err()
            .to_string()
            .contains("requires a release digest"));
    }

    #[test]
    fn workspace_snapshot_schema_rejects_disjoint_incoming_constraints() {
        let workspace_module =
            |module: &str, dependencies: Vec<SnapshotDependency>| SnapshotModule {
                module: module.into(),
                version: Some("1.0.0".into()),
                vo: "0.2.0".into(),
                release: None,
                source: SnapshotSource::Workspace {
                    directory: format!("workspace/{module}"),
                },
                dependencies,
            };
        let snapshot = ProjectSnapshot {
            format: PROJECT_GRAPH_FORMAT,
            mode: GraphMode::Effective,
            authority: SnapshotAuthority::Lock,
            root: SnapshotRoot {
                module: "github.com/acme/app".into(),
                vo: "0.2.0".into(),
                dependencies: vec![
                    SnapshotDependency {
                        module: "github.com/acme/a".into(),
                        constraint: "^1.0.0".into(),
                    },
                    SnapshotDependency {
                        module: "github.com/acme/b".into(),
                        constraint: "^1.0.0".into(),
                    },
                ],
            },
            workspace: Some(SnapshotWorkspace {
                file: "workspace/vo.work".into(),
            }),
            modules: vec![
                workspace_module(
                    "github.com/acme/a",
                    vec![SnapshotDependency {
                        module: "github.com/acme/c".into(),
                        constraint: "~1.0.0".into(),
                    }],
                ),
                workspace_module(
                    "github.com/acme/b",
                    vec![SnapshotDependency {
                        module: "github.com/acme/c".into(),
                        constraint: "~1.1.0".into(),
                    }],
                ),
                workspace_module("github.com/acme/c", vec![]),
            ],
        };

        let error = snapshot.validate().unwrap_err();
        assert!(
            error.to_string().contains("does not satisfy ~1.1.0"),
            "{error}"
        );
    }

    #[test]
    fn effective_snapshot_uses_exact_workspace_lock_nodes() {
        let root = tempfile::tempdir().unwrap();
        let root_path = std::fs::canonicalize(root.path()).unwrap();
        let app = root_path.join("app");
        let a = root_path.join("a");
        let b = root_path.join("b");
        for directory in [&app, &a, &b] {
            std::fs::create_dir(directory).unwrap();
        }
        let app_mod = concat!(
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\n",
            "vo = \"0.1.0\"\n[dependencies]\n",
            "\"github.com/acme/a\" = \"0.1.0\"\n",
        );
        let a_mod = concat!(
            "format = 1\nmodule = \"github.com/acme/a\"\nversion = \"0.1.0\"\n",
            "vo = \"0.1.0\"\n[dependencies]\n",
            "\"github.com/acme/b\" = \"0.1.0\"\n",
        );
        let b_mod =
            "format = 1\nmodule = \"github.com/acme/b\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n";
        std::fs::write(
            root_path.join("vo.work"),
            "format = 1\nmembers = [\"app\", \"a\", \"b\"]\n",
        )
        .unwrap();
        std::fs::write(app.join("vo.mod"), app_mod).unwrap();
        std::fs::write(a.join("vo.mod"), a_mod).unwrap();
        std::fs::write(b.join("vo.mod"), b_mod).unwrap();

        let workspace_node = |content: &str| {
            let mod_file = crate::schema::modfile::ModFile::parse(content).unwrap();
            crate::schema::lockfile::LockedModule {
                path: crate::identity::ModulePath::parse(mod_file.module.as_str()).unwrap(),
                version: mod_file.version.clone(),
                origin: crate::schema::lockfile::LockOrigin::Workspace,
                release: None,
                intent: Some(crate::lock::module_intent_digest(&mod_file).unwrap()),
            }
        };
        let root_mod = crate::schema::modfile::ModFile::parse(app_mod).unwrap();
        let lock = crate::schema::lockfile::LockFile {
            format: crate::schema::lockfile::LOCK_FILE_VERSION,
            root: crate::lock::module_intent_digest(&root_mod).unwrap(),
            modules: vec![workspace_node(a_mod), workspace_node(b_mod)],
        };
        std::fs::write(app.join("vo.lock"), lock.render().unwrap()).unwrap();

        let snapshot = ProjectSnapshot::capture(
            &app,
            &root_path.join("cache"),
            &SnapshotOptions::effective(),
        )
        .unwrap();
        assert_eq!(snapshot.authority, SnapshotAuthority::Lock);
        assert_eq!(snapshot.modules.len(), 2);
        assert!(snapshot.modules.iter().all(|module| {
            module.version.as_deref() == Some("0.1.0")
                && module.release.is_none()
                && matches!(module.source, SnapshotSource::Workspace { .. })
        }));
        assert_eq!(
            snapshot.dependency_chain("github.com/acme/b").unwrap(),
            [
                "github.com/acme/app",
                "github.com/acme/a",
                "github.com/acme/b",
            ]
        );
        assert_eq!(
            ProjectSnapshot::parse(&snapshot.render().unwrap()).unwrap(),
            snapshot
        );
    }

    fn fixture_module(module: &str) -> SnapshotModule {
        SnapshotModule {
            module: module.into(),
            version: Some("1.0.0".into()),
            vo: "0.2.0".into(),
            release: Some(crate::digest::Digest::from_sha256(module.as_bytes()).to_string()),
            source: SnapshotSource::Registry {
                directory: format!("cache/{module}/1.0.0"),
            },
            dependencies: vec![],
        }
    }
}
