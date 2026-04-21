use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};

use vo_common::vfs::{normalize_fs_path, FileSystem, MemoryFs, RealFs};

use crate::inline_mod::{self, InlineMod, INLINE_MOD_OPEN};
use crate::lock;
use crate::operation_error::OperationError;
use crate::schema::lockfile::{LockFile, LockRoot, LockedModule};
use crate::schema::modfile::ModFile;
use crate::Error;

#[derive(Debug, Clone, Default)]
pub struct ProjectDeps {
    kind: ProjectDepsKind,
}

#[derive(Debug, Clone, Default)]
enum ProjectDepsKind {
    #[default]
    NoModule,
    WithModule(Box<ProjectModuleContext>),
}

#[derive(Debug, Clone)]
struct ProjectModuleContext {
    mod_file: ModFile,
    current_module: String,
    lock_state: LockState,
}

#[derive(Debug, Clone)]
enum LockState {
    NoLock,
    Locked(Box<LockedProjectContext>),
}

#[derive(Debug, Clone)]
struct LockedProjectContext {
    lock_file: LockFile,
    allowed_modules: Vec<String>,
    locked_modules: Vec<LockedModule>,
}

impl ProjectDeps {
    fn no_module() -> Self {
        Self {
            kind: ProjectDepsKind::NoModule,
        }
    }

    fn with_module(context: ProjectModuleContext) -> Self {
        Self {
            kind: ProjectDepsKind::WithModule(Box::new(context)),
        }
    }

    fn module_context(&self) -> Option<&ProjectModuleContext> {
        match &self.kind {
            ProjectDepsKind::NoModule => None,
            ProjectDepsKind::WithModule(context) => Some(context.as_ref()),
        }
    }

    pub fn has_mod_file(&self) -> bool {
        self.module_context().is_some()
    }

    pub fn current_module(&self) -> Option<&str> {
        self.module_context()
            .map(|context| context.current_module.as_str())
    }

    pub fn mod_file(&self) -> Option<&ModFile> {
        self.module_context().map(|context| &context.mod_file)
    }

    pub fn allowed_modules(&self) -> &[String] {
        match self.module_context() {
            Some(context) => context.lock_state.allowed_modules(),
            None => &[],
        }
    }

    pub fn locked_modules(&self) -> &[LockedModule] {
        match self.module_context() {
            Some(context) => context.lock_state.locked_modules(),
            None => &[],
        }
    }

    pub fn lock_file(&self) -> Option<&LockFile> {
        self.module_context()
            .and_then(|context| context.lock_state.lock_file())
    }

    pub fn into_locked_modules(self) -> Vec<LockedModule> {
        match self.kind {
            ProjectDepsKind::NoModule => Vec::new(),
            ProjectDepsKind::WithModule(context) => context.lock_state.into_locked_modules(),
        }
    }
}

impl LockState {
    fn locked_context(&self) -> Option<&LockedProjectContext> {
        match self {
            LockState::NoLock => None,
            LockState::Locked(context) => Some(context.as_ref()),
        }
    }

    fn allowed_modules(&self) -> &[String] {
        match self.locked_context() {
            Some(context) => &context.allowed_modules,
            None => &[],
        }
    }

    fn locked_modules(&self) -> &[LockedModule] {
        match self.locked_context() {
            Some(context) => &context.locked_modules,
            None => &[],
        }
    }

    fn lock_file(&self) -> Option<&LockFile> {
        self.locked_context().map(|context| &context.lock_file)
    }

    fn into_locked_modules(self) -> Vec<LockedModule> {
        match self {
            LockState::NoLock => Vec::new(),
            LockState::Locked(context) => context.locked_modules,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectDepsStage {
    Workspace,
    ModFile,
    LockFile,
}

impl ProjectDepsStage {
    fn as_str(self) -> &'static str {
        match self {
            ProjectDepsStage::Workspace => "workspace",
            ProjectDepsStage::ModFile => "mod_file",
            ProjectDepsStage::LockFile => "lock_file",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectDepsErrorKind {
    Missing,
    ReadFailed,
    ParseFailed,
    ValidationFailed,
}

pub type ProjectDepsError = OperationError<ProjectDepsStage, ProjectDepsErrorKind>;

pub fn read_mod_file(project_dir: &Path) -> Result<ModFile, Error> {
    let path = project_dir.join("vo.mod");
    let content = std::fs::read_to_string(&path)?;
    ModFile::parse(&content)
}

pub fn read_lock_file(project_dir: &Path) -> Result<LockFile, Error> {
    let path = project_dir.join("vo.lock");
    let content = std::fs::read_to_string(&path)?;
    LockFile::parse(&content)
}

pub(crate) fn write_mod_file(project_dir: &Path, mod_file: &ModFile) -> Result<(), Error> {
    let path = project_dir.join("vo.mod");
    std::fs::write(&path, mod_file.render())?;
    Ok(())
}

pub(crate) fn write_lock_file(project_dir: &Path, lock_file: &LockFile) -> Result<(), Error> {
    let path = project_dir.join("vo.lock");
    std::fs::write(&path, lock_file.render())?;
    Ok(())
}

pub(crate) fn remove_lock_file_if_exists(project_dir: &Path) -> Result<(), Error> {
    let path = project_dir.join("vo.lock");
    if path.exists() {
        std::fs::remove_file(path)?;
    }
    Ok(())
}

pub(crate) fn write_or_remove_lock_file(
    project_dir: &Path,
    lock_file: Option<&LockFile>,
) -> Result<(), Error> {
    match lock_file {
        Some(lock_file) => write_lock_file(project_dir, lock_file),
        None => remove_lock_file_if_exists(project_dir),
    }
}

pub fn read_project_deps<F: FileSystem>(
    fs: &F,
    excluded_modules: &[String],
) -> Result<ProjectDeps, ProjectDepsError> {
    read_project_deps_with_candidates(
        fs,
        &[PathBuf::from("vo.mod")],
        &[PathBuf::from("vo.lock")],
        excluded_modules,
    )
}

fn read_project_deps_at_root_in<F: FileSystem>(
    fs: &F,
    dir: &Path,
    excluded_modules: &[String],
) -> Result<ProjectDeps, ProjectDepsError> {
    let mod_path = if dir == Path::new(".") || dir.as_os_str().is_empty() {
        PathBuf::from("vo.mod")
    } else {
        dir.join("vo.mod")
    };
    let Some(mod_content) = read_optional_file(fs, &mod_path, ProjectDepsStage::ModFile)? else {
        return Err(ProjectDepsError::new(
            ProjectDepsStage::ModFile,
            ProjectDepsErrorKind::Missing,
            "this operation requires vo.mod at the selected project root",
        )
        .with_path(&mod_path));
    };
    let mod_file = ModFile::parse(&mod_content).map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::ModFile,
            ProjectDepsErrorKind::ParseFailed,
            format!("vo.mod parse error: {}", error),
        )
        .with_path(&mod_path)
    })?;
    let lock_path = if dir == Path::new(".") || dir.as_os_str().is_empty() {
        PathBuf::from("vo.lock")
    } else {
        dir.join("vo.lock")
    };
    let lock_candidates = [lock_path];
    read_project_deps_from_mod_file(fs, mod_file, &lock_candidates, excluded_modules)
}

pub fn read_project_deps_at_root(
    dir: &Path,
    excluded_modules: &[String],
) -> Result<ProjectDeps, ProjectDepsError> {
    let fs = RealFs::new(".");
    read_project_deps_at_root_in(&fs, dir, excluded_modules)
}

pub fn read_inline_project_deps(
    vo_mod_content: &str,
    vo_lock_content: &str,
    excluded_modules: &[String],
) -> Result<ProjectDeps, ProjectDepsError> {
    let fs = MemoryFs::new()
        .with_file("vo.mod", vo_mod_content)
        .with_file("vo.lock", vo_lock_content);
    read_project_deps(&fs, excluded_modules)
}

pub fn build_lock_file_from_mod_file(
    mod_file: &ModFile,
    mut locked_modules: Vec<LockedModule>,
    created_by: &str,
) -> LockFile {
    locked_modules.sort_by(|a, b| a.path.cmp(&b.path));
    locked_modules.dedup_by(|a, b| a.path == b.path && a.version == b.version);
    LockFile {
        version: 1,
        created_by: created_by.to_string(),
        root: LockRoot {
            module: mod_file.module.clone(),
            vo: mod_file.vo.clone(),
        },
        resolved: locked_modules,
    }
}

pub fn build_lock_file_for_project_deps(
    project_deps: &ProjectDeps,
    locked_modules: Vec<LockedModule>,
    created_by: &str,
) -> Result<LockFile, Error> {
    let Some(mod_file) = project_deps.mod_file() else {
        return Err(Error::ModFileParse(
            "cannot build lock file without vo.mod project metadata".to_string(),
        ));
    };
    Ok(build_lock_file_from_mod_file(
        mod_file,
        locked_modules,
        created_by,
    ))
}

fn read_project_deps_near<F: FileSystem>(
    fs: &F,
    dir: &Path,
    excluded_modules: &[String],
) -> Result<ProjectDeps, ProjectDepsError> {
    let mod_candidates = candidate_paths(dir, "vo.mod");
    let lock_candidates = candidate_paths(dir, "vo.lock");
    read_project_deps_with_candidates(fs, &mod_candidates, &lock_candidates, excluded_modules)
}

fn candidate_paths(dir: &Path, file_name: &str) -> Vec<PathBuf> {
    let near = if dir == Path::new(".") || dir.as_os_str().is_empty() {
        PathBuf::from(file_name)
    } else {
        dir.join(file_name)
    };
    let root = PathBuf::from(file_name);
    if near == root {
        vec![root]
    } else {
        vec![near, root]
    }
}

fn read_project_deps_with_candidates<F: FileSystem>(
    fs: &F,
    mod_candidates: &[PathBuf],
    lock_candidates: &[PathBuf],
    excluded_modules: &[String],
) -> Result<ProjectDeps, ProjectDepsError> {
    for mod_candidate in mod_candidates {
        let Some(mod_content) = read_optional_file(fs, mod_candidate, ProjectDepsStage::ModFile)?
        else {
            continue;
        };

        let mod_file = ModFile::parse(&mod_content).map_err(|error| {
            ProjectDepsError::new(
                ProjectDepsStage::ModFile,
                ProjectDepsErrorKind::ParseFailed,
                format!("vo.mod parse error: {}", error),
            )
            .with_path(mod_candidate)
        })?;
        return read_project_deps_from_mod_file(fs, mod_file, lock_candidates, excluded_modules);
    }

    Ok(ProjectDeps::no_module())
}

fn read_project_deps_from_mod_file<F: FileSystem>(
    fs: &F,
    mod_file: ModFile,
    lock_candidates: &[PathBuf],
    excluded_modules: &[String],
) -> Result<ProjectDeps, ProjectDepsError> {
    let excluded_modules = excluded_modules
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    let current_module = mod_file.module.as_str().to_string();

    let has_unexcluded_external = mod_file
        .require
        .iter()
        .any(|req| !excluded_modules.contains(req.module.as_str()));

    for lock_candidate in lock_candidates {
        let Some(lock_content) =
            read_optional_file(fs, lock_candidate, ProjectDepsStage::LockFile)?
        else {
            continue;
        };

        let lock_file = LockFile::parse(&lock_content).map_err(|error| {
            ProjectDepsError::new(
                ProjectDepsStage::LockFile,
                ProjectDepsErrorKind::ParseFailed,
                format!("vo.lock parse error: {}", error),
            )
            .with_path(lock_candidate)
        })?;

        lock::verify_root_consistency(&mod_file, &lock_file).map_err(|error| {
            ProjectDepsError::new(
                ProjectDepsStage::LockFile,
                ProjectDepsErrorKind::ValidationFailed,
                format!("vo.lock validation error: {}", error),
            )
            .with_path(lock_candidate)
        })?;
        let excluded_modules_vec = excluded_modules
            .iter()
            .map(|module| (*module).to_string())
            .collect::<Vec<_>>();
        lock::verify_graph_completeness(&mod_file, &lock_file, &excluded_modules_vec).map_err(
            |error| {
                ProjectDepsError::new(
                    ProjectDepsStage::LockFile,
                    ProjectDepsErrorKind::ValidationFailed,
                    format!("vo.lock validation error: {}", error),
                )
                .with_path(lock_candidate)
            },
        )?;

        let mut allowed_modules = Vec::new();
        let mut locked_modules = Vec::new();
        for locked in &lock_file.resolved {
            let module_path = locked.path.as_str();
            if excluded_modules.contains(module_path) {
                continue;
            }
            allowed_modules.push(module_path.to_string());
            locked_modules.push(locked.clone());
        }

        return Ok(ProjectDeps::with_module(ProjectModuleContext {
            mod_file,
            current_module,
            lock_state: LockState::Locked(Box::new(LockedProjectContext {
                lock_file,
                allowed_modules,
                locked_modules,
            })),
        }));
    }

    if has_unexcluded_external {
        let mut error = ProjectDepsError::new(
            ProjectDepsStage::LockFile,
            ProjectDepsErrorKind::Missing,
            "this build requires external modules but vo.lock is missing",
        );
        if let Some(lock_candidate) = lock_candidates.first() {
            error = error.with_path(lock_candidate);
        }
        return Err(error);
    }

    Ok(ProjectDeps::with_module(ProjectModuleContext {
        mod_file,
        current_module,
        lock_state: LockState::NoLock,
    }))
}

fn read_optional_file<F: FileSystem>(
    fs: &F,
    path: &Path,
    stage: ProjectDepsStage,
) -> Result<Option<String>, ProjectDepsError> {
    match fs.read_file(path) {
        Ok(content) => Ok(Some(content)),
        Err(_) if !fs.exists(path) => Ok(None),
        Err(error) => Err(ProjectDepsError::new(
            stage,
            ProjectDepsErrorKind::ReadFailed,
            format!("{} read error: {}", stage.as_str(), error),
        )
        .with_path(path)),
    }
}

/// Find the nearest project root by walking up from `dir` looking for `vo.mod`.
/// Returns `dir` itself (normalized) if no `vo.mod` is found.
fn find_project_root_in<F: FileSystem>(fs: &F, dir: &Path) -> PathBuf {
    try_find_project_root_in(fs, dir).unwrap_or_else(|| normalize_fs_path(dir))
}

pub fn find_project_root(dir: &Path) -> Option<PathBuf> {
    let fs = RealFs::new(".");
    try_find_project_root_in(&fs, dir)
}

fn try_find_project_root_in<F: FileSystem>(fs: &F, dir: &Path) -> Option<PathBuf> {
    let mut current = normalize_fs_path(dir);
    loop {
        let candidate = if current == Path::new(".") || current.as_os_str().is_empty() {
            PathBuf::from("vo.mod")
        } else {
            current.join("vo.mod")
        };
        if fs.exists(&candidate) && !fs.is_dir(&candidate) {
            return Some(current);
        }
        if !current.pop() {
            return None;
        }
    }
}

/// Read a `vo.mod` file from a directory, returning `None` if the file does not exist.
fn read_mod_file_in<F: FileSystem>(
    fs: &F,
    dir: &Path,
) -> Result<Option<ModFile>, ProjectDepsError> {
    let mod_path = if dir == Path::new(".") || dir.as_os_str().is_empty() {
        PathBuf::from("vo.mod")
    } else {
        dir.join("vo.mod")
    };
    match fs.read_file(&mod_path) {
        Ok(content) => ModFile::parse(&content).map(Some).map_err(|error| {
            ProjectDepsError::new(
                ProjectDepsStage::ModFile,
                ProjectDepsErrorKind::ParseFailed,
                format!("vo.mod parse error: {}", error),
            )
            .with_path(&mod_path)
        }),
        Err(_) if !fs.exists(&mod_path) => Ok(None),
        Err(error) => Err(ProjectDepsError::new(
            ProjectDepsStage::ModFile,
            ProjectDepsErrorKind::ReadFailed,
            format!("vo.mod read error: {}", error),
        )
        .with_path(&mod_path)),
    }
}

/// Resolved project context: project root, dependencies, and workspace replaces.
///
/// This is the canonical result of project discovery and should be used by both
/// native (vo-engine) and web (vo-web) compilation paths.
#[derive(Debug)]
pub struct ProjectContext {
    project_root: PathBuf,
    project_deps: ProjectDeps,
    workspace_replaces: HashMap<String, PathBuf>,
}

impl ProjectContext {
    pub fn project_root(&self) -> &Path {
        &self.project_root
    }

    pub fn project_deps(&self) -> &ProjectDeps {
        &self.project_deps
    }

    pub fn workspace_replaces(&self) -> &HashMap<String, PathBuf> {
        &self.workspace_replaces
    }

    pub fn into_parts(self) -> (PathBuf, ProjectDeps, HashMap<String, PathBuf>) {
        (
            self.project_root,
            self.project_deps,
            self.workspace_replaces,
        )
    }
}

/// Load the full project context for a directory.
///
/// Discovers the nearest `vo.mod` ancestor, loads workspace replaces,
/// and reads project dependencies (excluding workspace-replaced modules).
pub fn load_project_context<F: FileSystem>(
    fs: &F,
    dir: &Path,
) -> Result<ProjectContext, ProjectDepsError> {
    let project_root = find_project_root_in(fs, dir);
    let root_mod = read_mod_file_in(fs, &project_root)?;
    let mod_path = if project_root == Path::new(".") || project_root.as_os_str().is_empty() {
        PathBuf::from("vo.mod")
    } else {
        project_root.join("vo.mod")
    };
    let mod_file_replaces = if let Some(root_mod) = root_mod.as_ref() {
        crate::workspace::load_mod_file_replaces(fs, root_mod, &project_root).map_err(|error| {
            let kind = match &error {
                crate::Error::Io(_) => ProjectDepsErrorKind::ReadFailed,
                crate::Error::ModFileParse(_) | crate::Error::WorkFileParse(_) => {
                    ProjectDepsErrorKind::ParseFailed
                }
                _ => ProjectDepsErrorKind::ValidationFailed,
            };
            ProjectDepsError::new(ProjectDepsStage::ModFile, kind, error.to_string())
                .with_path(&mod_path)
        })?
    } else {
        HashMap::new()
    };
    let mod_file_overrides = if let Some(root_mod) = root_mod.as_ref() {
        root_mod
            .replace
            .iter()
            .map(|replace| crate::workspace::Override {
                module: replace.module.clone(),
                local_dir: mod_file_replaces
                    .get(replace.module.as_str())
                    .cloned()
                    .expect("validated vo.mod replace must exist in replace map"),
            })
            .collect::<Vec<_>>()
    } else {
        Vec::new()
    };
    let workspace_overrides = crate::workspace::load_workspace_overrides_in(
        fs,
        &project_root,
        root_mod.as_ref().map(|mf| &mf.module),
    )
    .map_err(|error| {
        let kind = match &error {
            crate::Error::Io(_) => ProjectDepsErrorKind::ReadFailed,
            crate::Error::WorkFileParse(_) => ProjectDepsErrorKind::ParseFailed,
            _ => ProjectDepsErrorKind::ValidationFailed,
        };
        ProjectDepsError::new(ProjectDepsStage::Workspace, kind, error.to_string())
    })?;
    let mut excluded_modules = mod_file_overrides
        .iter()
        .map(|override_entry| override_entry.module.as_str().to_string())
        .collect::<Vec<_>>();
    excluded_modules.extend(
        workspace_overrides
            .iter()
            .map(|override_entry| override_entry.module.as_str().to_string()),
    );
    let project_deps = read_project_deps_near(fs, &project_root, &excluded_modules)?;
    if let Some(root_mod) = root_mod.as_ref() {
        let locked_modules = project_deps
            .locked_modules()
            .iter()
            .map(|locked| locked.path.clone())
            .collect::<Vec<_>>();
        let mut active_overrides = mod_file_overrides.clone();
        active_overrides.extend(workspace_overrides.iter().cloned());
        crate::workspace::validate_override_external_imports(
            fs,
            &root_mod.module,
            &mod_file_overrides,
            &active_overrides,
            &locked_modules,
            "vo.mod replace",
        )
        .map_err(|error| {
            let kind = match &error {
                crate::Error::Io(_) => ProjectDepsErrorKind::ReadFailed,
                crate::Error::ModFileParse(_)
                | crate::Error::WorkFileParse(_)
                | crate::Error::SourceScan(_) => ProjectDepsErrorKind::ParseFailed,
                _ => ProjectDepsErrorKind::ValidationFailed,
            };
            ProjectDepsError::new(ProjectDepsStage::ModFile, kind, error.to_string())
                .with_path(&mod_path)
        })?;
        crate::workspace::validate_override_external_imports(
            fs,
            &root_mod.module,
            &workspace_overrides,
            &active_overrides,
            &locked_modules,
            "workspace override",
        )
        .map_err(|error| {
            let kind = match &error {
                crate::Error::Io(_) => ProjectDepsErrorKind::ReadFailed,
                crate::Error::WorkFileParse(_) | crate::Error::SourceScan(_) => {
                    ProjectDepsErrorKind::ParseFailed
                }
                _ => ProjectDepsErrorKind::ValidationFailed,
            };
            ProjectDepsError::new(ProjectDepsStage::Workspace, kind, error.to_string())
        })?;
    }
    let mut workspace_replaces = mod_file_replaces;
    for override_entry in workspace_overrides {
        workspace_replaces.insert(
            override_entry.module.as_str().to_string(),
            normalize_fs_path(&override_entry.local_dir),
        );
    }
    Ok(ProjectContext {
        project_root,
        project_deps,
        workspace_replaces,
    })
}

// ============================================================
// Single-file project classification (spec §5.6, §10)
// ============================================================

/// Classification of a single `.vo` source file relative to the module system.
///
/// Implements the precedence rules in spec §5.6.4:
///
/// - If an ancestor directory contains `vo.mod`, the file belongs to that
///   project. An inline mod block inside the file is a hard error.
/// - Else, if the file begins with a `/*vo:mod ... */` block, the file is a
///   *single-file ephemeral module* (spec §10.2).
/// - Else, the file is an *ad hoc program* (spec §10.1).
#[derive(Debug)]
pub enum SingleFileContext {
    /// The file is inside a project with a `vo.mod` ancestor.
    Project(ProjectContext),

    /// The file is a single-file ephemeral module carrying inline mod metadata.
    EphemeralInlineMod {
        /// Directory containing the file, used as the compile root.
        project_root: PathBuf,
        /// Leaf file name of the source file (relative to `project_root`).
        file_name: PathBuf,
        /// Parsed inline mod declaration.
        inline_mod: InlineMod,
    },

    /// The file is an ad hoc program with no module metadata of any kind.
    AdHoc {
        /// Directory containing the file, used as the compile root.
        project_root: PathBuf,
        /// Leaf file name of the source file (relative to `project_root`).
        file_name: PathBuf,
    },
}

impl SingleFileContext {
    pub fn has_inline_mod(&self) -> bool {
        matches!(self, SingleFileContext::EphemeralInlineMod { .. })
    }

    pub fn inline_mod(&self) -> Option<&InlineMod> {
        match self {
            SingleFileContext::EphemeralInlineMod { inline_mod, .. } => Some(inline_mod),
            _ => None,
        }
    }
}

/// Classify a single source file and return its `SingleFileContext` per spec
/// §5.6.4 precedence rules.
///
/// `file_path` is the path of the `.vo` source file as seen by `fs`. It must
/// point to a regular file, not a directory.
pub fn load_single_file_context<F: FileSystem>(
    fs: &F,
    file_path: &Path,
) -> Result<SingleFileContext, ProjectDepsError> {
    let file_name_os = file_path.file_name().ok_or_else(|| {
        ProjectDepsError::new(
            ProjectDepsStage::ModFile,
            ProjectDepsErrorKind::ReadFailed,
            "single-file source path has no file name",
        )
        .with_path(file_path)
    })?;
    let file_name = PathBuf::from(file_name_os);
    let file_dir = file_path
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."));
    let file_dir = normalize_fs_path(&file_dir);

    let source = fs.read_file(file_path).map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::ModFile,
            ProjectDepsErrorKind::ReadFailed,
            format!("single-file source read error: {}", error),
        )
        .with_path(file_path)
    })?;
    let leading_reserved_span = inline_mod::leading_reserved_block_span(&source, 0);

    if let Some(project_root) = try_find_project_root_in(fs, &file_dir) {
        if let Some(span) = leading_reserved_span {
            return Err(ProjectDepsError::new(
                ProjectDepsStage::ModFile,
                ProjectDepsErrorKind::ValidationFailed,
                format!(
                    "inline '{}' block is not allowed in a file inside a project with vo.mod",
                    INLINE_MOD_OPEN
                ),
            )
            .with_path(file_path)
            .with_span(span));
        }
        let context = load_project_context(fs, &project_root)?;
        return Ok(SingleFileContext::Project(context));
    }

    let inline_mod =
        inline_mod::parse_inline_mod_from_source_with_span(&source, 0).map_err(|error| {
            ProjectDepsError::new(
                ProjectDepsStage::ModFile,
                ProjectDepsErrorKind::ParseFailed,
                format!("inline vo.mod parse error: {}", error),
            )
            .with_path(file_path)
            .with_span(error.span)
        })?;

    if inline_mod.is_some() {
        let ext_manifest_path = normalize_fs_path(&file_dir.join("vo.ext.toml"));
        if fs.exists(&ext_manifest_path) {
            return Err(ProjectDepsError::new(
                ProjectDepsStage::ModFile,
                ProjectDepsErrorKind::ValidationFailed,
                format!(
                    "inline '{}' single-file module cannot coexist with '{}' in the same directory",
                    INLINE_MOD_OPEN,
                    ext_manifest_path.display(),
                ),
            )
            .with_path(file_path)
            .with_span(
                leading_reserved_span
                    .expect("inline mod classification requires a reserved-block span"),
            ));
        }
    }

    Ok(match inline_mod {
        Some(inline_mod) => SingleFileContext::EphemeralInlineMod {
            project_root: file_dir,
            file_name,
            inline_mod,
        },
        None => SingleFileContext::AdHoc {
            project_root: file_dir,
            file_name,
        },
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::lockfile::LockedModule;
    use vo_common::vfs::MemoryFs;

    fn lock_file_for_workspace_replace() -> String {
        r#"version = 1
created_by = "vo 0.1.0"
[root]
module = "github.com/acme/app"
vo = "^0.1.0"

[[resolved]]
path = "github.com/vo-lang/core"
version = "v0.1.0"
vo = "^0.1.0"
commit = "cccccccccccccccccccccccccccccccccccccccc"
release_manifest = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
source = "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
deps = []
artifacts = []

[[resolved]]
path = "github.com/vo-lang/voplay"
version = "v0.1.0"
vo = "^0.1.0"
commit = "dddddddddddddddddddddddddddddddddddddddd"
release_manifest = "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
source = "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
deps = ["github.com/vo-lang/core"]
artifacts = []
"#
        .to_string()
    }

    fn root_project_fs() -> MemoryFs {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "vo.mod",
            "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/vo-lang/voplay ^0.1.0\n",
        );
        fs
    }

    #[test]
    fn read_project_deps_reports_missing_lock_when_external_modules_present() {
        let fs = root_project_fs();
        let error = read_project_deps(&fs, &[]).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::LockFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::Missing);
        assert_eq!(error.path.as_deref(), Some("vo.lock"));
        assert!(error
            .detail
            .contains("this build requires external modules but vo.lock is missing"));
    }

    #[test]
    fn read_project_deps_allows_missing_lock_when_all_direct_external_modules_are_excluded() {
        let fs = root_project_fs();
        let deps = read_project_deps(&fs, &["github.com/vo-lang/voplay".to_string()])
            .unwrap_or_else(|error| panic!("unexpected error: {error}"));
        assert!(deps.has_mod_file());
        assert_eq!(deps.current_module(), Some("github.com/acme/app"));
        assert!(deps.allowed_modules().is_empty());
        assert!(deps.locked_modules().is_empty());
    }

    #[test]
    fn read_project_deps_keeps_unexcluded_transitive_locked_modules() {
        let mut fs = root_project_fs();
        fs.add_file("vo.lock", lock_file_for_workspace_replace());
        let deps = read_project_deps(&fs, &["github.com/vo-lang/voplay".to_string()])
            .unwrap_or_else(|error| panic!("unexpected error: {error}"));
        assert_eq!(
            deps.allowed_modules(),
            &["github.com/vo-lang/core".to_string()]
        );
        assert_eq!(deps.locked_modules().len(), 1);
        let locked: &LockedModule = &deps.locked_modules()[0];
        assert_eq!(locked.path.as_str(), "github.com/vo-lang/core");
    }

    #[test]
    fn read_project_deps_rejects_orphaned_lock_when_root_has_no_requires() {
        let mut fs = MemoryFs::new();
        fs.add_file("vo.mod", "module github.com/acme/app\nvo ^0.1.0\n");
        fs.add_file(
            "vo.lock",
            "version = 1\ncreated_by = \"vo 0.1.0\"\n[root]\nmodule = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[[resolved]]\npath = \"github.com/acme/lib\"\nversion = \"v0.1.0\"\nvo = \"^0.1.0\"\ncommit = \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\nrelease_manifest = \"sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\nsource = \"sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"\ndeps = []\nartifacts = []\n",
        );

        let error = read_project_deps(&fs, &[]).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::LockFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert_eq!(error.path.as_deref(), Some("vo.lock"));
        assert!(error.detail.contains("orphaned"), "{}", error.detail);
    }

    #[test]
    fn read_project_deps_near_prefers_nearest_project_files() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "vo.mod",
            "module github.com/root/app\nvo ^0.1.0\nrequire github.com/vo-lang/root ^0.1.0\n",
        );
        fs.add_file(
            "vo.lock",
            "version = 1\ncreated_by = \"vo 0.1.0\"\n[root]\nmodule = \"github.com/root/app\"\nvo = \"^0.1.0\"\n\n[[resolved]]\npath = \"github.com/vo-lang/root\"\nversion = \"v0.1.0\"\nvo = \"^0.1.0\"\ncommit = \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\nrelease_manifest = \"sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\nsource = \"sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"\ndeps = []\nartifacts = []\n",
        );
        fs.add_file(
            "workspace/app/vo.mod",
            "module github.com/workspace/app\nvo ^0.1.0\nrequire github.com/vo-lang/local ^0.1.0\n",
        );
        fs.add_file(
            "workspace/app/vo.lock",
            "version = 1\ncreated_by = \"vo 0.1.0\"\n[root]\nmodule = \"github.com/workspace/app\"\nvo = \"^0.1.0\"\n\n[[resolved]]\npath = \"github.com/vo-lang/local\"\nversion = \"v0.1.0\"\nvo = \"^0.1.0\"\ncommit = \"cccccccccccccccccccccccccccccccccccccccc\"\nrelease_manifest = \"sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc\"\nsource = \"sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd\"\ndeps = []\nartifacts = []\n",
        );

        let deps = read_project_deps_near(&fs, Path::new("workspace/app"), &[])
            .unwrap_or_else(|error| panic!("unexpected error: {error}"));
        assert_eq!(deps.current_module(), Some("github.com/workspace/app"));
        assert_eq!(
            deps.allowed_modules(),
            &["github.com/vo-lang/local".to_string()]
        );
    }

    #[test]
    fn read_inline_project_deps_matches_root_file_parsing() {
        let mut fs = root_project_fs();
        fs.add_file("vo.lock", lock_file_for_workspace_replace());
        let mod_content = fs.read_file(Path::new("vo.mod")).unwrap();
        let lock_content = fs.read_file(Path::new("vo.lock")).unwrap();

        let deps = read_inline_project_deps(
            &mod_content,
            &lock_content,
            &["github.com/vo-lang/voplay".to_string()],
        )
        .unwrap();

        assert_eq!(
            deps.allowed_modules(),
            &["github.com/vo-lang/core".to_string()]
        );
        assert_eq!(deps.locked_modules().len(), 1);
        assert_eq!(
            deps.locked_modules()[0].path.as_str(),
            "github.com/vo-lang/core"
        );
    }

    #[test]
    fn load_project_context_allows_missing_lock_when_vo_mod_replaces_all_direct_external_modules() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/lib/vo.mod",
            "module github.com/vo-lang/lib\nvo ^0.1.0\n",
        );
        fs.add_file(
            "workspace/tests/vo.mod",
            "module github.com/vo-lang/lib/tests\nvo ^0.1.0\nrequire github.com/vo-lang/lib v0.1.0\nreplace github.com/vo-lang/lib => ../lib\n",
        );

        let context = load_project_context(&fs, Path::new("workspace/tests"))
            .unwrap_or_else(|error| panic!("unexpected error: {error}"));
        assert!(context.project_deps().has_mod_file());
        assert!(context.project_deps().locked_modules().is_empty());
    }

    #[test]
    fn load_project_context_rejects_unlocked_external_import_from_vo_mod_replace() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/lib/vo.mod",
            "module github.com/vo-lang/lib\nvo ^0.1.0\nrequire github.com/vo-lang/core v0.1.0\n",
        );
        fs.add_file(
            "workspace/lib/lib.vo",
            "package lib\nimport \"github.com/vo-lang/core\"\nfunc Hello(){core.Hello()}\n",
        );
        fs.add_file(
            "workspace/tests/vo.mod",
            "module github.com/vo-lang/lib/tests\nvo ^0.1.0\nrequire github.com/vo-lang/lib v0.1.0\nreplace github.com/vo-lang/lib => ../lib\n",
        );

        let error = match load_project_context(&fs, Path::new("workspace/tests")) {
            Ok(_) => panic!("expected unlocked external import validation error"),
            Err(error) => error,
        };
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert_eq!(error.path.as_deref(), Some("workspace/tests/vo.mod"));
        assert!(
            error
                .detail
                .contains("vo.mod replace github.com/vo-lang/lib imports github.com/vo-lang/core"),
            "{}",
            error.detail
        );
    }

    #[test]
    fn load_project_context_merges_vo_mod_replace_into_resolver_map() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/lib/vo.mod",
            "module github.com/vo-lang/lib\nvo ^0.1.0\n",
        );
        fs.add_file(
            "workspace/tests/vo.mod",
            "module github.com/vo-lang/lib/tests\nvo ^0.1.0\nrequire github.com/vo-lang/core v0.1.0\nrequire github.com/vo-lang/lib v0.1.0\nreplace github.com/vo-lang/lib => ../lib\n",
        );
        fs.add_file(
            "workspace/tests/vo.lock",
            "version = 1\ncreated_by = \"vo test\"\n[root]\nmodule = \"github.com/vo-lang/lib/tests\"\nvo = \"^0.1.0\"\n\n[[resolved]]\npath = \"github.com/vo-lang/core\"\nversion = \"v0.1.0\"\nvo = \"^0.1.0\"\ncommit = \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\nrelease_manifest = \"sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\nsource = \"sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"\ndeps = []\nartifacts = []\n",
        );

        let context = load_project_context(&fs, Path::new("workspace/tests"))
            .unwrap_or_else(|error| panic!("unexpected error: {error}"));
        assert_eq!(context.project_root(), Path::new("workspace/tests"));
        assert_eq!(
            context.workspace_replaces().get("github.com/vo-lang/lib"),
            Some(&PathBuf::from("workspace/lib"))
        );
        assert_eq!(context.project_deps().locked_modules().len(), 1);
        assert_eq!(
            context.project_deps().locked_modules()[0].path.as_str(),
            "github.com/vo-lang/core"
        );
    }

    #[test]
    fn load_single_file_context_ad_hoc_when_no_mod_and_no_inline() {
        let mut fs = MemoryFs::new();
        fs.add_file("main.vo", "package main\nfunc main() {}\n");
        let ctx = load_single_file_context(&fs, Path::new("main.vo")).unwrap();
        match ctx {
            SingleFileContext::AdHoc {
                project_root,
                file_name,
            } => {
                assert_eq!(project_root, PathBuf::from("."));
                assert_eq!(file_name, PathBuf::from("main.vo"));
            }
            other => panic!("expected AdHoc, got {:?}", other),
        }
    }

    #[test]
    fn load_single_file_context_parses_ephemeral_inline_mod() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "main.vo",
            "/*vo:mod\nmodule local/demo\nvo ^0.1.0\n*/\npackage main\nfunc main() {}\n",
        );
        let ctx = load_single_file_context(&fs, Path::new("main.vo")).unwrap();
        match ctx {
            SingleFileContext::EphemeralInlineMod {
                project_root,
                file_name,
                inline_mod,
            } => {
                assert_eq!(project_root, PathBuf::from("."));
                assert_eq!(file_name, PathBuf::from("main.vo"));
                assert_eq!(inline_mod.module.as_str(), "local/demo");
                assert!(inline_mod.require.is_empty());
            }
            other => panic!("expected EphemeralInlineMod, got {:?}", other),
        }
    }

    #[test]
    fn load_single_file_context_rejects_inline_mod_inside_project() {
        let mut fs = root_project_fs();
        fs.add_file(
            "scripts/tool.vo",
            "/*vo:mod\nmodule local/tool\nvo ^0.1.0\n*/\npackage main\n",
        );
        let error = load_single_file_context(&fs, Path::new("scripts/tool.vo")).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert!(
            error
                .detail
                .contains("not allowed in a file inside a project with vo.mod"),
            "{}",
            error.detail
        );
        assert_eq!(error.span().map(|span| span.start.0), Some(0));
    }

    #[test]
    fn load_single_file_context_project_when_ancestor_mod_exists_and_no_inline() {
        let mut fs = root_project_fs();
        fs.add_file("vo.lock", lock_file_for_workspace_replace());
        fs.add_file("scripts/tool.vo", "package main\nfunc main() {}\n");
        let ctx = load_single_file_context(&fs, Path::new("scripts/tool.vo")).unwrap();
        match ctx {
            SingleFileContext::Project(context) => {
                assert_eq!(
                    context.project_deps().current_module(),
                    Some("github.com/acme/app")
                );
            }
            other => panic!("expected Project, got {:?}", other),
        }
    }

    #[test]
    fn load_single_file_context_surfaces_malformed_inline_mod() {
        let mut fs = MemoryFs::new();
        fs.add_file("main.vo", "/*vo:mod\nmodule local/demo\n*/\npackage main\n");
        let error = load_single_file_context(&fs, Path::new("main.vo")).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ParseFailed);
        assert!(
            error.detail.contains("inline vo.mod parse error"),
            "{}",
            error.detail
        );
        assert!(error.span().is_some());
    }

    #[test]
    fn load_single_file_context_surfaces_reserved_sentinel_error() {
        let mut fs = MemoryFs::new();
        fs.add_file("main.vo", "/*vo:script\n*/\npackage main\n");
        let error = load_single_file_context(&fs, Path::new("main.vo")).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ParseFailed);
        assert_eq!(error.span().map(|span| span.start.0), Some(0));
    }

    #[test]
    fn load_single_file_context_surfaces_duplicate_inline_directive() {
        let mut fs = MemoryFs::new();
        let source =
            "/*vo:mod\nmodule local/demo\nmodule local/other\nvo ^0.1.0\n*/\npackage main\n";
        fs.add_file("main.vo", source);

        let error = load_single_file_context(&fs, Path::new("main.vo")).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ParseFailed);
        assert!(error.detail.contains("duplicate 'module' directive"));
        assert_eq!(
            error.span().map(|span| &source[span.to_range()]),
            Some("module")
        );
    }

    #[test]
    fn load_single_file_context_rejects_inline_mod_with_ext_manifest_in_same_dir() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "main.vo",
            "/*vo:mod\nmodule local/demo\nvo ^0.1.0\n*/\npackage main\nfunc main() {}\n",
        );
        fs.add_file(
            "vo.ext.toml",
            "[extension]\nname = \"demo\"\n\n[extension.native]\npath = \"rust/target/{profile}/libdemo\"\n\n[[extension.native.targets]]\ntarget = \"aarch64-apple-darwin\"\nlibrary = \"libdemo.dylib\"\n",
        );

        let error = load_single_file_context(&fs, Path::new("main.vo")).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert!(error.detail.contains("vo.ext.toml"), "{}", error.detail);
        assert_eq!(error.path.as_deref(), Some("main.vo"));
        assert_eq!(error.span().map(|span| span.start.0), Some(0));
    }
}
