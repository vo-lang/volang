use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};

use vo_common::vfs::{normalize_fs_path, FileSystem, MemoryFs, RealFs};

use crate::lock;
use crate::operation_error::OperationError;
use crate::schema::lockfile::{LockFile, LockRoot, LockedModule};
use crate::schema::modfile::{ModFile, Require};
use crate::Error;

#[derive(Debug, Clone)]
pub enum ProjectDeps {
    NoModule,
    WithModule {
        current_module: String,
        mod_file_path: PathBuf,
        mod_file: ModFile,
        lock_state: LockState,
    },
}

#[derive(Debug, Clone)]
pub enum LockState {
    NoLock,
    Locked {
        lock_file_path: PathBuf,
        lock_file: LockFile,
        allowed_modules: Vec<String>,
        locked_modules: Vec<LockedModule>,
    },
}

impl Default for ProjectDeps {
    fn default() -> Self {
        ProjectDeps::NoModule
    }
}

impl ProjectDeps {
    pub fn has_mod_file(&self) -> bool {
        matches!(self, ProjectDeps::WithModule { .. })
    }

    pub fn current_module(&self) -> Option<&str> {
        match self {
            ProjectDeps::NoModule => None,
            ProjectDeps::WithModule { current_module, .. } => Some(current_module),
        }
    }

    pub fn allowed_modules(&self) -> &[String] {
        match self {
            ProjectDeps::NoModule => &[],
            ProjectDeps::WithModule { lock_state: LockState::Locked { allowed_modules, .. }, .. } => allowed_modules,
            ProjectDeps::WithModule { lock_state: LockState::NoLock, .. } => &[],
        }
    }

    pub fn locked_modules(&self) -> &[LockedModule] {
        match self {
            ProjectDeps::NoModule => &[],
            ProjectDeps::WithModule { lock_state: LockState::Locked { locked_modules, .. }, .. } => locked_modules,
            ProjectDeps::WithModule { lock_state: LockState::NoLock, .. } => &[],
        }
    }

    pub fn mod_file(&self) -> Option<&ModFile> {
        match self {
            ProjectDeps::NoModule => None,
            ProjectDeps::WithModule { mod_file, .. } => Some(mod_file),
        }
    }

    pub fn mod_file_path(&self) -> Option<&Path> {
        match self {
            ProjectDeps::NoModule => None,
            ProjectDeps::WithModule { mod_file_path, .. } => Some(mod_file_path),
        }
    }

    pub fn lock_file(&self) -> Option<&LockFile> {
        match self {
            ProjectDeps::WithModule { lock_state: LockState::Locked { lock_file, .. }, .. } => Some(lock_file),
            _ => None,
        }
    }

    pub fn lock_file_path(&self) -> Option<&Path> {
        match self {
            ProjectDeps::WithModule { lock_state: LockState::Locked { lock_file_path, .. }, .. } => Some(lock_file_path),
            _ => None,
        }
    }

    pub fn into_locked_modules(self) -> Vec<LockedModule> {
        match self {
            ProjectDeps::WithModule { lock_state: LockState::Locked { locked_modules, .. }, .. } => locked_modules,
            _ => Vec::new(),
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
    pub fn as_str(self) -> &'static str {
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

impl ProjectDepsErrorKind {
    pub fn as_str(self) -> &'static str {
        match self {
            ProjectDepsErrorKind::Missing => "missing",
            ProjectDepsErrorKind::ReadFailed => "read_failed",
            ProjectDepsErrorKind::ParseFailed => "parse_failed",
            ProjectDepsErrorKind::ValidationFailed => "validation_failed",
        }
    }
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

pub fn write_mod_file(project_dir: &Path, mod_file: &ModFile) -> Result<(), Error> {
    let path = project_dir.join("vo.mod");
    std::fs::write(&path, mod_file.render())?;
    Ok(())
}

pub fn write_lock_file(project_dir: &Path, lock_file: &LockFile) -> Result<(), Error> {
    let path = project_dir.join("vo.lock");
    std::fs::write(&path, lock_file.render())?;
    Ok(())
}

pub fn remove_lock_file_if_exists(project_dir: &Path) -> Result<(), Error> {
    let path = project_dir.join("vo.lock");
    if path.exists() {
        std::fs::remove_file(path)?;
    }
    Ok(())
}

pub fn write_or_remove_lock_file(
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

pub fn read_project_deps_at_root_in<F: FileSystem>(
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
        return Err(
            ProjectDepsError::new(
                ProjectDepsStage::ModFile,
                ProjectDepsErrorKind::Missing,
                "this operation requires vo.mod at the selected project root",
            )
            .with_path(&mod_path),
        );
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
    read_project_deps_from_mod_file(fs, &mod_path, mod_file, &lock_candidates, excluded_modules)
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

pub fn read_project_deps_near<F: FileSystem>(
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
        let Some(mod_content) = read_optional_file(fs, mod_candidate, ProjectDepsStage::ModFile)? else {
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
        return read_project_deps_from_mod_file(
            fs,
            mod_candidate,
            mod_file,
            lock_candidates,
            excluded_modules,
        );
    }

    Ok(ProjectDeps::NoModule)
}

fn read_project_deps_from_mod_file<F: FileSystem>(
    fs: &F,
    mod_candidate: &Path,
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
        let Some(lock_content) = read_optional_file(fs, lock_candidate, ProjectDepsStage::LockFile)? else {
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
        lock::verify_graph_completeness(&mod_file, &lock_file).map_err(|error| {
            ProjectDepsError::new(
                ProjectDepsStage::LockFile,
                ProjectDepsErrorKind::ValidationFailed,
                format!("vo.lock validation error: {}", error),
            )
            .with_path(lock_candidate)
        })?;

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

        return Ok(ProjectDeps::WithModule {
            current_module,
            mod_file_path: mod_candidate.to_path_buf(),
            mod_file,
            lock_state: LockState::Locked {
                lock_file_path: lock_candidate.clone(),
                lock_file,
                allowed_modules,
                locked_modules,
            },
        });
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

    Ok(ProjectDeps::WithModule {
        current_module,
        mod_file_path: mod_candidate.to_path_buf(),
        mod_file,
        lock_state: LockState::NoLock,
    })
}

fn read_optional_file<F: FileSystem>(
    fs: &F,
    path: &Path,
    stage: ProjectDepsStage,
) -> Result<Option<String>, ProjectDepsError> {
    match fs.read_file(path) {
        Ok(content) => Ok(Some(content)),
        Err(_) if !fs.exists(path) => Ok(None),
        Err(error) => Err(
            ProjectDepsError::new(
                stage,
                ProjectDepsErrorKind::ReadFailed,
                format!("{} read error: {}", stage.as_str(), error),
            )
            .with_path(path),
        ),
    }
}

/// Find the nearest project root by walking up from `dir` looking for `vo.mod`.
/// Returns `dir` itself (normalized) if no `vo.mod` is found.
pub fn find_project_root_in<F: FileSystem>(fs: &F, dir: &Path) -> PathBuf {
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
        Err(error) => Err(
            ProjectDepsError::new(
                ProjectDepsStage::ModFile,
                ProjectDepsErrorKind::ReadFailed,
                format!("vo.mod read error: {}", error),
            )
            .with_path(&mod_path),
        ),
    }
}

/// Resolved project context: project root, dependencies, and workspace replaces.
///
/// This is the canonical result of project discovery and should be used by both
/// native (vo-engine) and web (vo-web) compilation paths.
pub struct ProjectContext {
    pub project_root: PathBuf,
    pub project_deps: ProjectDeps,
    pub workspace_replaces: HashMap<String, PathBuf>,
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
    let workspace_replaces = crate::workspace::load_workspace_replaces(
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
    let excluded_modules = workspace_replaces.keys().cloned().collect::<Vec<_>>();
    let project_deps = read_project_deps_near(fs, &project_root, &excluded_modules)
        ?;
    Ok(ProjectContext {
        project_root,
        project_deps,
        workspace_replaces,
    })
}

pub fn build_synthetic_project_files(
    synthetic_module: &str,
    synthetic_vo: &str,
    created_by: &str,
    installed: &[(String, String)],
    locked_modules: &[LockedModule],
) -> Result<(String, String), crate::Error> {
    let module = crate::identity::ModulePath::parse(synthetic_module)?;
    let vo = crate::version::ToolchainConstraint::parse(synthetic_vo)?;
    let require = installed
        .iter()
        .map(|(module, version)| {
            Ok(Require {
                module: crate::identity::ModulePath::parse(module)?,
                constraint: crate::version::DepConstraint::parse(version)?,
            })
        })
        .collect::<Result<Vec<_>, crate::Error>>()?;
    let mod_file = ModFile {
        module: module.clone(),
        vo: vo.clone(),
        require,
    };
    let lock_file = LockFile {
        version: 1,
        created_by: created_by.to_string(),
        root: LockRoot { module, vo },
        resolved: locked_modules.to_vec(),
    };
    Ok((mod_file.render(), lock_file.render()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::digest::Digest;
    use crate::schema::lockfile::LockedModule;
    use crate::version::{ExactVersion, ToolchainConstraint};
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

    fn sample_locked_module() -> LockedModule {
        LockedModule {
            path: crate::identity::ModulePath::parse("github.com/vo-lang/voplay").unwrap(),
            version: ExactVersion::parse("v0.1.0").unwrap(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            commit: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string(),
            release_manifest: Digest::parse(
                "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            )
            .unwrap(),
            source: Digest::parse(
                "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
            )
            .unwrap(),
            deps: Vec::new(),
            artifacts: Vec::new(),
        }
    }

    #[test]
    fn read_project_deps_requires_lock_for_unexcluded_external_modules() {
        let fs = root_project_fs();
        let result = read_project_deps(&fs, &[]);
        let error = match result {
            Ok(_) => panic!("expected missing vo.lock error"),
            Err(error) => error,
        };
        assert_eq!(error.stage, ProjectDepsStage::LockFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::Missing);
        assert_eq!(error.path.as_deref(), Some("vo.lock"));
        assert!(
            error
                .detail
                .contains("this build requires external modules but vo.lock is missing")
        );
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
        assert_eq!(deps.mod_file_path(), Some(Path::new("vo.mod")));
        assert_eq!(deps.lock_file_path(), None);
    }

    #[test]
    fn read_project_deps_keeps_unexcluded_transitive_locked_modules() {
        let mut fs = root_project_fs();
        fs.add_file("vo.lock", lock_file_for_workspace_replace());
        let deps = read_project_deps(&fs, &["github.com/vo-lang/voplay".to_string()])
            .unwrap_or_else(|error| panic!("unexpected error: {error}"));
        assert_eq!(deps.allowed_modules(), &["github.com/vo-lang/core".to_string()]);
        assert_eq!(deps.locked_modules().len(), 1);
        let locked: &LockedModule = &deps.locked_modules()[0];
        assert_eq!(locked.path.as_str(), "github.com/vo-lang/core");
        assert_eq!(deps.lock_file_path(), Some(Path::new("vo.lock")));
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
        assert_eq!(deps.allowed_modules(), &["github.com/vo-lang/local".to_string()]);
        assert_eq!(deps.mod_file_path(), Some(Path::new("workspace/app/vo.mod")));
        assert_eq!(deps.lock_file_path(), Some(Path::new("workspace/app/vo.lock")));
    }

    #[test]
    fn build_synthetic_project_files_renders_canonical_files() {
        let locked = sample_locked_module();
        let (mod_content, lock_content) = build_synthetic_project_files(
            "github.com/vo-lang/studio-examples",
            "0.1.0",
            "vo-studio",
            &[(locked.path.as_str().to_string(), locked.version.to_string())],
            std::slice::from_ref(&locked),
        )
        .unwrap();

        let mod_file = ModFile::parse(&mod_content).unwrap();
        assert_eq!(mod_file.module.as_str(), "github.com/vo-lang/studio-examples");
        assert_eq!(mod_file.require.len(), 1);
        assert_eq!(mod_file.require[0].module, locked.path);
        assert_eq!(mod_file.require[0].constraint.to_string(), locked.version.to_string());

        let lock_file = LockFile::parse(&lock_content).unwrap();
        assert_eq!(lock_file.created_by, "vo-studio");
        assert_eq!(lock_file.root.module.as_str(), "github.com/vo-lang/studio-examples");
        assert_eq!(lock_file.resolved.len(), 1);
        assert_eq!(lock_file.resolved[0].path.as_str(), "github.com/vo-lang/voplay");
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

        assert_eq!(deps.allowed_modules(), &["github.com/vo-lang/core".to_string()]);
        assert_eq!(deps.locked_modules().len(), 1);
        assert_eq!(deps.locked_modules()[0].path.as_str(), "github.com/vo-lang/core");
    }
}
