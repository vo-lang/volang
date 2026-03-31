use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};

use vo_common::vfs::{normalize_fs_path, FileSystem};

use crate::lock;
use crate::schema::lockfile::{LockFile, LockedModule};
use crate::schema::modfile::ModFile;

#[derive(Debug, Clone, Default)]
pub struct ProjectDeps {
    pub has_mod_file: bool,
    pub current_module: Option<String>,
    pub allowed_modules: Vec<String>,
    pub locked_modules: Vec<LockedModule>,
    pub mod_file_path: Option<PathBuf>,
    pub lock_file_path: Option<PathBuf>,
    pub mod_file: Option<ModFile>,
    pub lock_file: Option<LockFile>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectDepsStage {
    ModFile,
    LockFile,
}

impl ProjectDepsStage {
    pub fn as_str(self) -> &'static str {
        match self {
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

#[derive(Debug, Clone)]
pub struct ProjectDepsError {
    pub stage: ProjectDepsStage,
    pub kind: ProjectDepsErrorKind,
    pub path: Option<PathBuf>,
    pub detail: String,
}

impl ProjectDepsError {
    fn new(
        stage: ProjectDepsStage,
        kind: ProjectDepsErrorKind,
        detail: impl Into<String>,
    ) -> Self {
        Self {
            stage,
            kind,
            path: None,
            detail: detail.into(),
        }
    }

    fn with_path(mut self, path: &Path) -> Self {
        self.path = Some(path.to_path_buf());
        self
    }
}

impl std::fmt::Display for ProjectDepsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.detail)
    }
}

impl std::error::Error for ProjectDepsError {}

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
    let excluded_modules = excluded_modules
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();

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

        let current_module = Some(mod_file.module.as_str().to_string());
        let mut project_deps = ProjectDeps {
            has_mod_file: true,
            current_module,
            mod_file_path: Some(mod_candidate.clone()),
            mod_file: Some(mod_file.clone()),
            ..ProjectDeps::default()
        };

        if mod_file.require.is_empty() {
            return Ok(project_deps);
        }

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

            project_deps.allowed_modules = allowed_modules;
            project_deps.locked_modules = locked_modules;
            project_deps.lock_file_path = Some(lock_candidate.clone());
            project_deps.lock_file = Some(lock_file);
            return Ok(project_deps);
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

        return Ok(project_deps);
    }

    Ok(ProjectDeps::default())
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
    let mut current = normalize_fs_path(dir);
    loop {
        let candidate = if current == Path::new(".") || current.as_os_str().is_empty() {
            PathBuf::from("vo.mod")
        } else {
            current.join("vo.mod")
        };
        if fs.exists(&candidate) && !fs.is_dir(&candidate) {
            return current;
        }
        if !current.pop() {
            return normalize_fs_path(dir);
        }
    }
}

/// Read a `vo.mod` file from a directory, returning `None` if the file does not exist.
fn read_mod_file_in<F: FileSystem>(fs: &F, dir: &Path) -> Result<Option<ModFile>, String> {
    let mod_path = if dir == Path::new(".") || dir.as_os_str().is_empty() {
        PathBuf::from("vo.mod")
    } else {
        dir.join("vo.mod")
    };
    match fs.read_file(&mod_path) {
        Ok(content) => ModFile::parse(&content)
            .map(Some)
            .map_err(|error| format!("vo.mod parse error: {}", error)),
        Err(_) if !fs.exists(&mod_path) => Ok(None),
        Err(error) => Err(format!("vo.mod read error: {}", error)),
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
) -> Result<ProjectContext, String> {
    let project_root = find_project_root_in(fs, dir);
    let root_mod = read_mod_file_in(fs, &project_root)?;
    let workspace_replaces = crate::workspace::load_workspace_replaces(
        fs,
        &project_root,
        root_mod.as_ref().map(|mf| &mf.module),
    )
    .map_err(|error| error.to_string())?;
    let excluded_modules = workspace_replaces.keys().cloned().collect::<Vec<_>>();
    let project_deps = read_project_deps_near(fs, &project_root, &excluded_modules)
        .map_err(|error| error.to_string())?;
    Ok(ProjectContext {
        project_root,
        project_deps,
        workspace_replaces,
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
    fn read_project_deps_requires_lock_for_unexcluded_external_modules() {
        let fs = root_project_fs();
        let result = read_project_deps(&fs, &[]);
        let error = match result {
            Ok(_) => panic!("expected missing vo.lock error"),
            Err(error) => error,
        };
        assert_eq!(error.stage, ProjectDepsStage::LockFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::Missing);
        assert_eq!(error.path, Some(PathBuf::from("vo.lock")));
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
        assert!(deps.has_mod_file);
        assert_eq!(deps.current_module.as_deref(), Some("github.com/acme/app"));
        assert!(deps.allowed_modules.is_empty());
        assert!(deps.locked_modules.is_empty());
        assert_eq!(deps.mod_file_path, Some(PathBuf::from("vo.mod")));
        assert_eq!(deps.lock_file_path, None);
    }

    #[test]
    fn read_project_deps_keeps_unexcluded_transitive_locked_modules() {
        let mut fs = root_project_fs();
        fs.add_file("vo.lock", lock_file_for_workspace_replace());
        let deps = read_project_deps(&fs, &["github.com/vo-lang/voplay".to_string()])
            .unwrap_or_else(|error| panic!("unexpected error: {error}"));
        assert_eq!(deps.allowed_modules, vec!["github.com/vo-lang/core".to_string()]);
        assert_eq!(deps.locked_modules.len(), 1);
        let locked: &LockedModule = &deps.locked_modules[0];
        assert_eq!(locked.path.as_str(), "github.com/vo-lang/core");
        assert_eq!(deps.lock_file_path, Some(PathBuf::from("vo.lock")));
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
        assert_eq!(deps.current_module.as_deref(), Some("github.com/workspace/app"));
        assert_eq!(deps.allowed_modules, vec!["github.com/vo-lang/local".to_string()]);
        assert_eq!(deps.mod_file_path, Some(PathBuf::from("workspace/app/vo.mod")));
        assert_eq!(deps.lock_file_path, Some(PathBuf::from("workspace/app/vo.lock")));
    }
}
