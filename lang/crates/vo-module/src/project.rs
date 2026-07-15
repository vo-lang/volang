use std::collections::{BTreeSet, HashMap};
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
#[cfg(test)]
use std::sync::{Mutex, OnceLock};

use vo_common::vfs::{normalize_fs_path, FileSystem, MemoryFs, RealFs};

use crate::inline_mod::{self, InlineMod, INLINE_MOD_OPEN};
use crate::lock;
use crate::operation_error::OperationError;
use crate::schema::lockfile::{LockFile, LockRoot, LockedModule, LOCK_FILE_VERSION};
use crate::schema::modfile::ModFile;
use crate::version::{ToolchainConstraint, ToolchainVersion};
use crate::workspace::WorkspaceDiscovery;
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

    /// Registry modules that must be materialized for this build. Workspace
    /// override targets are omitted only from this source-selection view.
    pub fn locked_modules(&self) -> &[LockedModule] {
        match self.module_context() {
            Some(context) => context.lock_state.locked_modules(),
            None => &[],
        }
    }

    /// The complete, already-validated root lock graph, including any modules
    /// whose source is supplied by a workspace override.
    pub fn lock_file(&self) -> Option<&LockFile> {
        self.module_context()
            .and_then(|context| context.lock_state.lock_file())
    }

    /// Consume this context and return its registry materialization subset.
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectContextOptions {
    pub workspace: WorkspaceDiscovery,
}

impl ProjectContextOptions {
    pub fn new(workspace: WorkspaceDiscovery) -> Self {
        Self { workspace }
    }

    pub fn from_environment() -> Self {
        Self {
            workspace: crate::workspace::workspace_discovery_from_environment(),
        }
    }
}

impl Default for ProjectContextOptions {
    fn default() -> Self {
        Self {
            workspace: WorkspaceDiscovery::Auto,
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

fn validate_current_toolchain(
    constraint: &ToolchainConstraint,
    path: &Path,
) -> Result<(), ProjectDepsError> {
    let current = ToolchainVersion::parse(env!("CARGO_PKG_VERSION"))
        .expect("the workspace package version must be valid semantic versioning");
    if constraint.satisfies(&current) {
        return Ok(());
    }

    Err(ProjectDepsError::new(
        ProjectDepsStage::ModFile,
        ProjectDepsErrorKind::ValidationFailed,
        format!(
            "project requires Vo toolchain {}, but this compiler is {}",
            constraint, current
        ),
    )
    .with_path(path))
}

pub fn read_mod_file(project_dir: &Path) -> Result<ModFile, Error> {
    let path = project_dir.join("vo.mod");
    let content = vo_common::vfs::read_text_file(&path)?;
    ModFile::parse(&content)
}

pub fn read_lock_file(project_dir: &Path) -> Result<LockFile, Error> {
    let path = project_dir.join("vo.lock");
    let bytes = vo_common::vfs::read_binary_file(&path, crate::MAX_LOCK_FILE_BYTES)?;
    let content = std::str::from_utf8(&bytes).map_err(|error| {
        Error::LockFileParse(format!("{} is not valid UTF-8: {error}", path.display()))
    })?;
    LockFile::parse(content)
}

/// Exclusive advisory lock spanning one project mutation's read, solve,
/// materialize, and commit phases. Keeping the lock file stable allows
/// independent CLI processes to serialize without a racy sentinel protocol.
pub(crate) struct ProjectMutationLock {
    _file: File,
    #[cfg(windows)]
    overlapped: Box<WindowsOverlapped>,
}

pub(crate) fn lock_project_mutation(project_dir: &Path) -> Result<ProjectMutationLock, Error> {
    acquire_project_file_lock(project_dir, true, true)?.ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "project mutation locking is unavailable on this platform",
        )
        .into()
    })
}

fn lock_existing_project_for_read(
    project_dir: &Path,
) -> Result<Option<ProjectMutationLock>, Error> {
    acquire_project_file_lock(project_dir, false, false)
}

fn acquire_project_file_lock(
    project_dir: &Path,
    create: bool,
    exclusive: bool,
) -> Result<Option<ProjectMutationLock>, Error> {
    let path = project_dir.join(".vo-project.lock");
    let mut options = std::fs::OpenOptions::new();
    options.read(true).write(create).create(create);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options
            .mode(0o600)
            .custom_flags(libc::O_CLOEXEC | libc::O_NOFOLLOW);
    }
    #[cfg(windows)]
    {
        use std::os::windows::fs::OpenOptionsExt;
        const FILE_FLAG_OPEN_REPARSE_POINT: u32 = 0x0020_0000;
        options.custom_flags(FILE_FLAG_OPEN_REPARSE_POINT);
    }
    let file = match options.open(&path) {
        Ok(file) => file,
        Err(error) if !create && error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(error) => return Err(error.into()),
    };

    #[cfg(unix)]
    {
        use std::os::fd::AsRawFd;
        use std::os::unix::fs::MetadataExt;
        let operation = if exclusive {
            libc::LOCK_EX
        } else {
            libc::LOCK_SH
        };
        loop {
            if unsafe { libc::flock(file.as_raw_fd(), operation) } == 0 {
                break;
            }
            let error = std::io::Error::last_os_error();
            if error.kind() != std::io::ErrorKind::Interrupted {
                return Err(error.into());
            }
        }
        let descriptor = file.metadata()?;
        let linked = std::fs::symlink_metadata(&path)?;
        if !descriptor.is_file()
            || !linked.is_file()
            || descriptor.nlink() != 1
            || descriptor.dev() != linked.dev()
            || descriptor.ino() != linked.ino()
        {
            return Err(Error::SourceScan(format!(
                "project lock {} must be one stable, non-linked regular file",
                path.display()
            )));
        }
        Ok(Some(ProjectMutationLock { _file: file }))
    }

    #[cfg(windows)]
    {
        use std::os::windows::fs::MetadataExt;
        use std::os::windows::io::AsRawHandle;
        let mut overlapped = Box::new(WindowsOverlapped::default());
        let flags = if exclusive {
            WINDOWS_LOCKFILE_EXCLUSIVE_LOCK
        } else {
            0
        };
        let acquired = unsafe {
            LockFileEx(
                file.as_raw_handle(),
                flags,
                0,
                u32::MAX,
                u32::MAX,
                overlapped.as_mut(),
            )
        };
        if acquired == 0 {
            return Err(std::io::Error::last_os_error().into());
        }
        const FILE_ATTRIBUTE_REPARSE_POINT: u32 = 0x0000_0400;
        let descriptor = file.metadata()?;
        let linked = std::fs::symlink_metadata(&path)?;
        if !descriptor.is_file()
            || !linked.is_file()
            || descriptor.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0
            || linked.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0
            || descriptor.number_of_links() != Some(1)
            || descriptor.volume_serial_number() != linked.volume_serial_number()
            || descriptor.file_index() != linked.file_index()
        {
            return Err(Error::SourceScan(format!(
                "project lock {} must be one stable, non-linked regular file",
                path.display()
            )));
        }
        return Ok(Some(ProjectMutationLock {
            _file: file,
            overlapped,
        }));
    }

    #[cfg(not(any(unix, windows)))]
    {
        let _ = file;
        if exclusive {
            Err(std::io::Error::new(
                std::io::ErrorKind::Unsupported,
                "project mutation locking is unavailable on this platform",
            )
            .into())
        } else {
            Ok(None)
        }
    }
}

#[cfg(windows)]
const WINDOWS_LOCKFILE_EXCLUSIVE_LOCK: u32 = 0x0000_0002;

#[cfg(windows)]
#[repr(C)]
#[derive(Default)]
struct WindowsOverlapped {
    internal: usize,
    internal_high: usize,
    offset: u32,
    offset_high: u32,
    event: *mut std::ffi::c_void,
}

#[cfg(windows)]
#[link(name = "kernel32")]
unsafe extern "system" {
    fn LockFileEx(
        file: std::os::windows::io::RawHandle,
        flags: u32,
        reserved: u32,
        bytes_low: u32,
        bytes_high: u32,
        overlapped: *mut WindowsOverlapped,
    ) -> i32;
    fn UnlockFileEx(
        file: std::os::windows::io::RawHandle,
        reserved: u32,
        bytes_low: u32,
        bytes_high: u32,
        overlapped: *mut WindowsOverlapped,
    ) -> i32;
}

impl Drop for ProjectMutationLock {
    fn drop(&mut self) {
        #[cfg(unix)]
        {
            use std::os::fd::AsRawFd;
            let _ = unsafe { libc::flock(self._file.as_raw_fd(), libc::LOCK_UN) };
        }
        #[cfg(windows)]
        {
            use std::os::windows::io::AsRawHandle;
            let _ = unsafe {
                UnlockFileEx(
                    self._file.as_raw_handle(),
                    0,
                    u32::MAX,
                    u32::MAX,
                    self.overlapped.as_mut(),
                )
            };
        }
    }
}

static PROJECT_TEMP_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[cfg(test)]
static FAIL_PROJECT_PARENT_SYNCS: OnceLock<Mutex<BTreeSet<PathBuf>>> = OnceLock::new();

#[cfg(test)]
fn fail_project_parent_sync_for_test(path: &Path) {
    FAIL_PROJECT_PARENT_SYNCS
        .get_or_init(|| Mutex::new(BTreeSet::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert(path.to_path_buf());
}

#[cfg(test)]
fn should_fail_project_parent_sync_for_test(path: &Path) -> bool {
    FAIL_PROJECT_PARENT_SYNCS
        .get_or_init(|| Mutex::new(BTreeSet::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(path)
}

fn sync_parent_directory(path: &Path) -> Result<(), Error> {
    #[cfg(test)]
    if should_fail_project_parent_sync_for_test(path) {
        return Err(std::io::Error::other("injected project parent sync failure").into());
    }
    #[cfg(unix)]
    {
        let parent = path.parent().unwrap_or_else(|| Path::new("."));
        std::fs::File::open(parent)?.sync_all()?;
    }
    #[cfg(not(unix))]
    let _ = path;
    Ok(())
}

fn write_temp_file(path: &Path, contents: &[u8]) -> Result<PathBuf, Error> {
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    let stem = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("vo-project-file");

    for _ in 0..128 {
        let id = PROJECT_TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
        let temp_path = parent.join(format!(".{stem}.{}.{}.tmp", std::process::id(), id));
        let mut file = match std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&temp_path)
        {
            Ok(file) => file,
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(error) => return Err(error.into()),
        };
        if let Err(error) = (|| {
            file.write_all(contents)?;
            file.sync_all()
        })() {
            let _ = std::fs::remove_file(&temp_path);
            return Err(error.into());
        }
        return Ok(temp_path);
    }

    Err(Error::SourceScan(format!(
        "could not allocate a temporary project file in {}",
        parent.display()
    )))
}

#[cfg(not(windows))]
fn replace_file_atomically(from: &Path, to: &Path) -> std::io::Result<()> {
    std::fs::rename(from, to)
}

#[cfg(windows)]
fn replace_file_atomically(from: &Path, to: &Path) -> std::io::Result<()> {
    use std::iter::once;
    use std::os::windows::ffi::OsStrExt;

    #[link(name = "kernel32")]
    extern "system" {
        fn MoveFileExW(existing: *const u16, replacement: *const u16, flags: u32) -> i32;
    }

    const MOVEFILE_REPLACE_EXISTING: u32 = 0x1;
    const MOVEFILE_WRITE_THROUGH: u32 = 0x8;
    let from = from
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    let to = to
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    let result = unsafe {
        MoveFileExW(
            from.as_ptr(),
            to.as_ptr(),
            MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH,
        )
    };
    if result == 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(())
    }
}

#[derive(Debug)]
enum ProjectFileMutationOutcome {
    Durable,
    CommittedWithPostCommitFailure { path: String, message: String },
}

impl ProjectFileMutationOutcome {
    fn into_result(self) -> Result<(), Error> {
        match self {
            Self::Durable => Ok(()),
            Self::CommittedWithPostCommitFailure { path, message } => {
                Err(Error::ProjectPublicationPostCommitFailure { path, message })
            }
        }
    }

    fn record_post_commit_failure(self, failures: &mut Vec<String>) {
        if let Self::CommittedWithPostCommitFailure { path, message } = self {
            failures.push(format!("{path}: {message}"));
        }
    }
}

fn committed_sync_failure(path: &Path, error: Error) -> ProjectFileMutationOutcome {
    ProjectFileMutationOutcome::CommittedWithPostCommitFailure {
        path: path.display().to_string(),
        message: error.to_string(),
    }
}

fn write_bytes_atomically(
    path: &Path,
    contents: &[u8],
) -> Result<ProjectFileMutationOutcome, Error> {
    let temp_path = write_temp_file(path, contents)?;
    if let Err(error) = replace_file_atomically(&temp_path, path) {
        let _ = std::fs::remove_file(&temp_path);
        return Err(error.into());
    }
    match sync_parent_directory(path) {
        Ok(()) => Ok(ProjectFileMutationOutcome::Durable),
        Err(error) => Ok(committed_sync_failure(path, error)),
    }
}

fn write_bytes_create_new(
    path: &Path,
    contents: &[u8],
) -> Result<ProjectFileMutationOutcome, Error> {
    let temp_path = write_temp_file(path, contents)?;
    if let Err(error) = std::fs::hard_link(&temp_path, path) {
        let _ = std::fs::remove_file(&temp_path);
        return Err(error.into());
    }
    let cleanup_error = std::fs::remove_file(&temp_path).err();
    let sync_error = sync_parent_directory(path).err();
    match (cleanup_error, sync_error) {
        (None, None) => Ok(ProjectFileMutationOutcome::Durable),
        (cleanup_error, sync_error) => {
            let mut messages = Vec::new();
            if let Some(error) = cleanup_error {
                messages.push(format!(
                    "temporary publication link {} could not be removed: {error}",
                    temp_path.display()
                ));
            }
            if let Some(error) = sync_error {
                messages.push(error.to_string());
            }
            Ok(ProjectFileMutationOutcome::CommittedWithPostCommitFailure {
                path: path.display().to_string(),
                message: messages.join("; "),
            })
        }
    }
}

fn remove_file_atomically(path: &Path) -> Result<ProjectFileMutationOutcome, Error> {
    match std::fs::remove_file(path) {
        Ok(()) => match sync_parent_directory(path) {
            Ok(()) => Ok(ProjectFileMutationOutcome::Durable),
            Err(error) => Ok(committed_sync_failure(path, error)),
        },
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            Ok(ProjectFileMutationOutcome::Durable)
        }
        Err(error) => Err(error.into()),
    }
}

fn write_mod_file_outcome(
    project_dir: &Path,
    mod_file: &ModFile,
) -> Result<ProjectFileMutationOutcome, Error> {
    let path = project_dir.join("vo.mod");
    let rendered = mod_file.render()?;
    write_bytes_atomically(&path, rendered.as_bytes())
}

fn write_lock_file_outcome(
    project_dir: &Path,
    lock_file: &LockFile,
) -> Result<ProjectFileMutationOutcome, Error> {
    let path = project_dir.join("vo.lock");
    let rendered = lock_file.render()?;
    write_bytes_atomically(&path, rendered.as_bytes())
}

fn write_or_remove_lock_file_outcome(
    project_dir: &Path,
    lock_file: Option<&LockFile>,
) -> Result<ProjectFileMutationOutcome, Error> {
    match lock_file {
        Some(lock_file) => write_lock_file_outcome(project_dir, lock_file),
        None => remove_file_atomically(&project_dir.join("vo.lock")),
    }
}

pub(crate) fn write_new_mod_file(project_dir: &Path, mod_file: &ModFile) -> Result<(), Error> {
    let path = project_dir.join("vo.mod");
    let rendered = mod_file.render()?;
    write_bytes_create_new(&path, rendered.as_bytes())?.into_result()
}

#[cfg(test)]
pub(crate) fn remove_lock_file_if_exists(project_dir: &Path) -> Result<(), Error> {
    remove_file_atomically(&project_dir.join("vo.lock"))?.into_result()
}

/// Commit the authored module graph as a rollback-protected operation. Each file
/// replacement is atomic. If the second replacement fails, the first one is
/// restored before the error is returned.
pub(crate) fn write_project_files(
    project_dir: &Path,
    mod_file: &ModFile,
    lock_file: Option<&LockFile>,
) -> Result<(), Error> {
    let lock_path = project_dir.join("vo.lock");
    let previous_lock =
        match vo_common::vfs::read_binary_file(&lock_path, crate::MAX_LOCK_FILE_BYTES) {
            Ok(contents) => Some(contents),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
            Err(error) => return Err(error.into()),
        };

    let mut post_commit_failures = Vec::new();
    write_or_remove_lock_file_outcome(project_dir, lock_file)?
        .record_post_commit_failure(&mut post_commit_failures);
    match write_mod_file_outcome(project_dir, mod_file) {
        Ok(outcome) => outcome.record_post_commit_failure(&mut post_commit_failures),
        Err(commit_error) => {
            let rollback = match previous_lock {
                Some(contents) => write_bytes_atomically(&lock_path, &contents),
                None => remove_file_atomically(&lock_path),
            };
            match rollback {
                Ok(outcome) => {
                    let mut rollback_failures = Vec::new();
                    outcome.record_post_commit_failure(&mut rollback_failures);
                    if rollback_failures.is_empty() {
                        return Err(commit_error);
                    }
                    return Err(Error::ProjectPublicationPostCommitFailure {
                        path: lock_path.display().to_string(),
                        message: format!(
                            "project commit failed ({commit_error}); lock rollback committed with a post-commit failure ({})",
                            rollback_failures.join("; ")
                        ),
                    });
                }
                Err(rollback_error) => {
                    return Err(Error::SourceScan(format!(
                        "failed to commit project files ({commit_error}); lock rollback also failed ({rollback_error})"
                    )));
                }
            }
        }
    }
    if post_commit_failures.is_empty() {
        Ok(())
    } else {
        Err(Error::ProjectPublicationPostCommitFailure {
            path: project_dir.display().to_string(),
            message: post_commit_failures.join("; "),
        })
    }
}

pub(crate) fn write_or_remove_lock_file(
    project_dir: &Path,
    lock_file: Option<&LockFile>,
) -> Result<(), Error> {
    write_or_remove_lock_file_outcome(project_dir, lock_file)?.into_result()
}

/// Load and validate the complete root graph when a lock is present, then omit
/// `excluded_modules` only from the registry-source materialization view. A
/// lock may be absent when every direct external requirement is excluded by a
/// local replacement; otherwise frozen builds require one. Any retained lock
/// always contains the full graph, including workspace-overridden modules.
pub fn read_project_deps<F: FileSystem>(
    fs: &F,
    excluded_modules: &[String],
) -> Result<ProjectDeps, ProjectDepsError> {
    let candidates = [(PathBuf::from("vo.mod"), PathBuf::from("vo.lock"))];
    read_project_deps_with_candidates(fs, &candidates, excluded_modules)
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
    validate_current_toolchain(&mod_file.vo, &mod_path)?;
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
    for _ in 0..8 {
        let read_guard = lock_existing_project_for_read(dir).map_err(|error| {
            ProjectDepsError::new(
                ProjectDepsStage::LockFile,
                ProjectDepsErrorKind::ReadFailed,
                format!("project snapshot lock failed: {error}"),
            )
            .with_path(&dir.join(".vo-project.lock"))
        })?;
        let snapshot = read_project_deps_at_root_in(&fs, dir, excluded_modules);
        if read_guard.is_some() || !dir.join(".vo-project.lock").exists() {
            return snapshot;
        }
        drop(read_guard);
    }
    Err(ProjectDepsError::new(
        ProjectDepsStage::LockFile,
        ProjectDepsErrorKind::ReadFailed,
        "project metadata changed during every bounded snapshot attempt",
    ))
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
) -> Result<LockFile, Error> {
    locked_modules.sort_by(|a, b| a.path.cmp(&b.path));
    let lock_file = LockFile {
        version: LOCK_FILE_VERSION,
        created_by: created_by.to_string(),
        root: LockRoot {
            module: mod_file.module.clone(),
            vo: mod_file.vo.clone(),
        },
        resolved: locked_modules,
    };
    lock_file.render()?;
    lock::verify_root_consistency(mod_file, &lock_file)?;
    lock::verify_graph_completeness(mod_file, &lock_file)?;
    Ok(lock_file)
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
    build_lock_file_from_mod_file(mod_file, locked_modules, created_by)
}

fn read_project_deps_near<F: FileSystem>(
    fs: &F,
    dir: &Path,
    excluded_modules: &[String],
) -> Result<ProjectDeps, ProjectDepsError> {
    let candidates = project_metadata_candidates(dir);
    read_project_deps_with_candidates(fs, &candidates, excluded_modules)
}

fn project_metadata_candidates(dir: &Path) -> Vec<(PathBuf, PathBuf)> {
    let near = if dir == Path::new(".") || dir.as_os_str().is_empty() {
        (PathBuf::from("vo.mod"), PathBuf::from("vo.lock"))
    } else {
        (dir.join("vo.mod"), dir.join("vo.lock"))
    };
    let root = (PathBuf::from("vo.mod"), PathBuf::from("vo.lock"));
    if near == root {
        vec![root]
    } else {
        vec![near, root]
    }
}

fn read_project_deps_with_candidates<F: FileSystem>(
    fs: &F,
    candidates: &[(PathBuf, PathBuf)],
    excluded_modules: &[String],
) -> Result<ProjectDeps, ProjectDepsError> {
    for (mod_candidate, lock_candidate) in candidates {
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
        validate_current_toolchain(&mod_file.vo, mod_candidate)?;
        return read_project_deps_from_mod_file(
            fs,
            mod_file,
            std::slice::from_ref(lock_candidate),
            excluded_modules,
        );
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
        .any(|requirement| !excluded_modules.contains(requirement.module.as_str()));

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
    let limit = if stage == ProjectDepsStage::LockFile {
        crate::MAX_LOCK_FILE_BYTES
    } else {
        vo_common::vfs::MAX_TEXT_FILE_BYTES
    };
    match fs.read_text_limited(path, limit) {
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
    match fs.read_text_limited(&mod_path, vo_common::vfs::MAX_TEXT_FILE_BYTES) {
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
    workspace_file: Option<PathBuf>,
    workspace_replaces: HashMap<String, PathBuf>,
}

impl ProjectContext {
    pub fn project_root(&self) -> &Path {
        &self.project_root
    }

    pub fn project_deps(&self) -> &ProjectDeps {
        &self.project_deps
    }

    /// Exact workspace file selected while this context was loaded.
    ///
    /// This is retained alongside the resolved replacement map so snapshot
    /// consumers can distinguish no workspace from an empty workspace and
    /// detect concurrent workspace appearance, disappearance, or relocation.
    pub fn workspace_file(&self) -> Option<&Path> {
        self.workspace_file.as_deref()
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
/// and reads project dependencies. Workspace replacements select local source
/// materialization. If every direct external requirement is replaced, the
/// frozen build can proceed without a lock; otherwise the root lock remains
/// authoritative for the complete dependency graph.
pub fn load_project_context<F: FileSystem>(
    fs: &F,
    dir: &Path,
) -> Result<ProjectContext, ProjectDepsError> {
    load_project_context_with_options(fs, dir, &ProjectContextOptions::from_environment())
}

pub fn load_project_context_with_options<F: FileSystem>(
    fs: &F,
    dir: &Path,
    options: &ProjectContextOptions,
) -> Result<ProjectContext, ProjectDepsError> {
    const MAX_SNAPSHOT_ATTEMPTS: usize = 8;
    for _ in 0..MAX_SNAPSHOT_ATTEMPTS {
        let project_root = find_project_root_in(fs, dir);
        let physical_project_dir = physical_project_directory(fs, &project_root);
        let read_guard = match physical_project_dir.as_ref() {
            Some(project_dir) => lock_existing_project_for_read(project_dir).map_err(|error| {
                ProjectDepsError::new(
                    ProjectDepsStage::LockFile,
                    ProjectDepsErrorKind::ReadFailed,
                    format!("project snapshot lock failed: {error}"),
                )
                .with_path(&project_dir.join(".vo-project.lock"))
            })?,
            None => None,
        };
        let snapshot = load_project_context_snapshot(fs, project_root.clone(), options);
        let final_root = find_project_root_in(fs, dir);
        let lock_appeared = read_guard.is_none()
            && physical_project_dir
                .as_ref()
                .is_some_and(|project_dir| project_dir.join(".vo-project.lock").exists());
        if final_root == project_root && !lock_appeared {
            return snapshot;
        }
        drop(read_guard);
    }
    Err(ProjectDepsError::new(
        ProjectDepsStage::LockFile,
        ProjectDepsErrorKind::ReadFailed,
        "project metadata changed during every bounded snapshot attempt",
    ))
}

fn physical_project_directory<F: FileSystem>(fs: &F, project_root: &Path) -> Option<PathBuf> {
    let fs_root = fs.root()?;
    let physical = if fs_root == Path::new(".") || fs_root.as_os_str().is_empty() {
        project_root.to_path_buf()
    } else if project_root.is_absolute() {
        return None;
    } else {
        fs_root.join(project_root)
    };
    physical.is_dir().then_some(physical)
}

fn load_project_context_snapshot<F: FileSystem>(
    fs: &F,
    project_root: PathBuf,
    options: &ProjectContextOptions,
) -> Result<ProjectContext, ProjectDepsError> {
    let root_mod = read_mod_file_in(fs, &project_root)?;
    let (workspace_file, workspace_overrides) =
        crate::workspace::load_workspace_overrides_in_with_provenance(
            fs,
            &project_root,
            root_mod.as_ref().map(|mf| &mf.module),
            &options.workspace,
        )
        .map_err(|error| {
            let kind = match &error {
                crate::Error::Io(_) => ProjectDepsErrorKind::ReadFailed,
                crate::Error::WorkFileParse(_) => ProjectDepsErrorKind::ParseFailed,
                _ => ProjectDepsErrorKind::ValidationFailed,
            };
            ProjectDepsError::new(ProjectDepsStage::Workspace, kind, error.to_string())
        })?;
    let excluded_modules = workspace_overrides
        .iter()
        .map(|override_entry| override_entry.module.as_str().to_string())
        .collect::<Vec<_>>();
    let project_deps = read_project_deps_near(fs, &project_root, &excluded_modules)?;
    if let Some(root_mod) = root_mod.as_ref() {
        let locked_modules = project_deps
            .locked_modules()
            .iter()
            .map(|locked| locked.path.clone())
            .collect::<Vec<_>>();
        crate::workspace::validate_override_external_imports(
            fs,
            &root_mod.module,
            &workspace_overrides,
            &workspace_overrides,
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
    let mut workspace_replaces = HashMap::new();
    for override_entry in workspace_overrides {
        workspace_replaces.insert(
            override_entry.module.as_str().to_string(),
            normalize_fs_path(&override_entry.local_dir),
        );
    }
    Ok(ProjectContext {
        project_root,
        project_deps,
        workspace_file,
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
    load_single_file_context_with_options(fs, file_path, &ProjectContextOptions::from_environment())
}

pub fn load_single_file_context_with_options<F: FileSystem>(
    fs: &F,
    file_path: &Path,
    options: &ProjectContextOptions,
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

    let source = fs
        .read_text_limited(file_path, vo_common::vfs::MAX_TEXT_FILE_BYTES)
        .map_err(|error| {
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
        let context = load_project_context_with_options(fs, &project_root, options)?;
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

    Ok(match inline_mod {
        Some(inline_mod) => {
            validate_current_toolchain(&inline_mod.vo, file_path)?;
            SingleFileContext::EphemeralInlineMod {
                project_root: file_dir,
                file_name,
                inline_mod,
            }
        }
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
        r#"version = 2
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

[[resolved]]
path = "github.com/vo-lang/voplay"
version = "v0.1.0"
vo = "^0.1.0"
commit = "dddddddddddddddddddddddddddddddddddddddd"
release_manifest = "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
source = "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
deps = [
  { module = "github.com/vo-lang/core", constraint = "^0.1.0" },
]
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
        assert!(deps.lock_file().is_none());
    }

    #[test]
    fn read_project_deps_requires_lock_when_any_direct_external_module_is_unexcluded() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "vo.mod",
            "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/vo-lang/voplay ^0.1.0\nrequire github.com/vo-lang/vogui ^0.1.0\n",
        );

        let error = read_project_deps(&fs, &["github.com/vo-lang/voplay".to_string()])
            .expect_err("an unreplaced external requirement must keep the build frozen");
        assert_eq!(error.stage, ProjectDepsStage::LockFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::Missing);
        assert_eq!(error.path.as_deref(), Some("vo.lock"));
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
        assert_eq!(deps.lock_file().unwrap().resolved.len(), 2);
    }

    #[test]
    fn build_lock_file_requires_complete_root_graph() {
        let mod_file = ModFile::parse(
            "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/vo-lang/voplay ^0.1.0\n",
        )
        .unwrap();
        let error = build_lock_file_from_mod_file(&mod_file, Vec::new(), "vo test").unwrap_err();
        assert!(error.to_string().contains("github.com/vo-lang/voplay"));

        let valid = LockFile::parse(&lock_file_for_workspace_replace()).unwrap();
        let lock_file =
            build_lock_file_from_mod_file(&mod_file, valid.resolved, "vo test").unwrap();
        assert_eq!(lock_file.resolved.len(), 2);
    }

    #[test]
    fn build_lock_file_rejects_orphans_and_duplicate_selections() {
        let valid = LockFile::parse(&lock_file_for_workspace_replace()).unwrap();
        let empty_root = ModFile::parse("module github.com/acme/app\nvo ^0.1.0\n").unwrap();
        let orphan_error =
            build_lock_file_from_mod_file(&empty_root, valid.resolved.clone(), "vo test")
                .unwrap_err();
        assert!(orphan_error.to_string().contains("orphaned"));

        let rooted = ModFile::parse(
            "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/vo-lang/voplay ^0.1.0\n",
        )
        .unwrap();
        let mut duplicate = valid.resolved;
        duplicate.push(duplicate[0].clone());
        let duplicate_error =
            build_lock_file_from_mod_file(&rooted, duplicate, "vo test").unwrap_err();
        assert!(duplicate_error
            .to_string()
            .contains("duplicate resolved module"));
    }

    #[test]
    fn read_project_deps_rejects_orphaned_lock_when_root_has_no_requires() {
        let mut fs = MemoryFs::new();
        fs.add_file("vo.mod", "module github.com/acme/app\nvo ^0.1.0\n");
        fs.add_file(
            "vo.lock",
            "version = 2\ncreated_by = \"vo 0.1.0\"\n[root]\nmodule = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[[resolved]]\npath = \"github.com/acme/lib\"\nversion = \"v0.1.0\"\nvo = \"^0.1.0\"\ncommit = \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\nrelease_manifest = \"sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\nsource = \"sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"\ndeps = []\n",
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
            "version = 2\ncreated_by = \"vo 0.1.0\"\n[root]\nmodule = \"github.com/root/app\"\nvo = \"^0.1.0\"\n\n[[resolved]]\npath = \"github.com/vo-lang/root\"\nversion = \"v0.1.0\"\nvo = \"^0.1.0\"\ncommit = \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\nrelease_manifest = \"sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\nsource = \"sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"\ndeps = []\n",
        );
        fs.add_file(
            "workspace/app/vo.mod",
            "module github.com/workspace/app\nvo ^0.1.0\nrequire github.com/vo-lang/local ^0.1.0\n",
        );
        fs.add_file(
            "workspace/app/vo.lock",
            "version = 2\ncreated_by = \"vo 0.1.0\"\n[root]\nmodule = \"github.com/workspace/app\"\nvo = \"^0.1.0\"\n\n[[resolved]]\npath = \"github.com/vo-lang/local\"\nversion = \"v0.1.0\"\nvo = \"^0.1.0\"\ncommit = \"cccccccccccccccccccccccccccccccccccccccc\"\nrelease_manifest = \"sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc\"\nsource = \"sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd\"\ndeps = []\n",
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
    fn read_project_deps_near_does_not_pair_nested_mod_with_root_lock() {
        let mut fs = root_project_fs();
        fs.add_file("vo.lock", lock_file_for_workspace_replace());
        fs.add_file(
            "workspace/app/vo.mod",
            "module github.com/workspace/app\nvo ^0.1.0\nrequire github.com/vo-lang/local ^0.1.0\n",
        );

        let deps = read_project_deps_near(
            &fs,
            Path::new("workspace/app"),
            &["github.com/vo-lang/local".to_string()],
        )
        .unwrap_or_else(|error| panic!("unexpected error: {error}"));

        assert_eq!(deps.current_module(), Some("github.com/workspace/app"));
        assert!(deps.lock_file().is_none());
        assert!(deps.locked_modules().is_empty());
    }

    #[test]
    fn project_context_without_nested_mod_uses_root_metadata_pair() {
        let mut fs = root_project_fs();
        fs.add_file("vo.lock", lock_file_for_workspace_replace());

        let context = load_project_context_with_options(
            &fs,
            Path::new("workspace/app"),
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap_or_else(|error| panic!("unexpected error: {error}"));
        let deps = context.project_deps();

        assert_eq!(deps.current_module(), Some("github.com/acme/app"));
        assert_eq!(deps.lock_file().unwrap().resolved.len(), 2);
        assert_eq!(deps.locked_modules().len(), 2);
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
    fn load_project_context_allows_missing_lock_when_vo_work_overrides_all_direct_external_modules()
    {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/lib/vo.mod",
            "module github.com/vo-lang/lib\nvo ^0.1.0\n",
        );
        fs.add_file(
            "workspace/tests/vo.mod",
            "module github.com/vo-lang/lib/tests\nvo ^0.1.0\nrequire github.com/vo-lang/lib v0.1.0\n",
        );
        fs.add_file(
            "workspace/tests/vo.work",
            "version = 1\n[[use]]\npath = \"../lib\"\n",
        );

        let context = load_project_context(&fs, Path::new("workspace/tests"))
            .unwrap_or_else(|error| panic!("unexpected error: {error}"));
        assert!(context.project_deps().has_mod_file());
        assert!(context.project_deps().locked_modules().is_empty());
        assert!(context.project_deps().lock_file().is_none());
        assert_eq!(
            context.workspace_replaces().get("github.com/vo-lang/lib"),
            Some(&PathBuf::from("workspace/lib"))
        );
    }

    #[test]
    fn load_project_context_with_disabled_workspace_requires_locked_external_modules() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/lib/vo.mod",
            "module github.com/vo-lang/lib\nvo ^0.1.0\n",
        );
        fs.add_file(
            "workspace/tests/vo.mod",
            "module github.com/vo-lang/lib/tests\nvo ^0.1.0\nrequire github.com/vo-lang/lib v0.1.0\n",
        );
        fs.add_file(
            "workspace/tests/vo.work",
            "version = 1\n[[use]]\npath = \"../lib\"\n",
        );

        let error = load_project_context_with_options(
            &fs,
            Path::new("workspace/tests"),
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap_err();

        assert_eq!(error.stage, ProjectDepsStage::LockFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::Missing);
    }

    #[test]
    fn load_project_context_without_lock_rejects_external_import_outside_all_local_graph() {
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
            "module github.com/vo-lang/lib/tests\nvo ^0.1.0\nrequire github.com/vo-lang/lib v0.1.0\n",
        );
        fs.add_file(
            "workspace/tests/vo.work",
            "version = 1\n[[use]]\npath = \"../lib\"\n",
        );

        let error = match load_project_context(&fs, Path::new("workspace/tests")) {
            Ok(_) => panic!("expected unlocked external import validation error"),
            Err(error) => error,
        };
        assert_eq!(error.stage, ProjectDepsStage::Workspace);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert!(
            error.detail.contains(
                "workspace override github.com/vo-lang/lib imports github.com/vo-lang/core"
            ),
            "{}",
            error.detail
        );
    }

    #[test]
    fn load_project_context_merges_vo_work_override_into_resolver_map() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/lib/vo.mod",
            "module github.com/vo-lang/lib\nvo ^0.1.0\n",
        );
        fs.add_file(
            "workspace/tests/vo.mod",
            "module github.com/vo-lang/lib/tests\nvo ^0.1.0\nrequire github.com/vo-lang/core v0.1.0\nrequire github.com/vo-lang/lib v0.1.0\n",
        );
        fs.add_file(
            "workspace/tests/vo.work",
            "version = 1\n[[use]]\npath = \"../lib\"\n",
        );
        fs.add_file(
            "workspace/tests/vo.lock",
            "version = 2\ncreated_by = \"vo test\"\n[root]\nmodule = \"github.com/vo-lang/lib/tests\"\nvo = \"^0.1.0\"\n\n[[resolved]]\npath = \"github.com/vo-lang/core\"\nversion = \"v0.1.0\"\nvo = \"^0.1.0\"\ncommit = \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\nrelease_manifest = \"sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\nsource = \"sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"\ndeps = []\n\n[[resolved]]\npath = \"github.com/vo-lang/lib\"\nversion = \"v0.1.0\"\nvo = \"^0.1.0\"\ncommit = \"cccccccccccccccccccccccccccccccccccccccc\"\nrelease_manifest = \"sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc\"\nsource = \"sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd\"\ndeps = []\n",
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
    fn read_project_deps_rejects_incompatible_toolchain() {
        let mut fs = MemoryFs::new();
        fs.add_file("vo.mod", "module github.com/acme/app\nvo ^9.0.0\n");

        let error = read_project_deps(&fs, &[]).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert_eq!(error.path.as_deref(), Some("vo.mod"));
        assert!(error.detail.contains("requires Vo toolchain ^9.0.0"));
        assert!(error.detail.contains(env!("CARGO_PKG_VERSION")));
    }

    #[test]
    fn load_single_file_context_rejects_incompatible_inline_toolchain() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "main.vo",
            "/*vo:mod\nmodule local/demo\nvo ^9.0.0\n*/\npackage main\nfunc main() {}\n",
        );

        let error = load_single_file_context(&fs, Path::new("main.vo")).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert_eq!(error.path.as_deref(), Some("main.vo"));
        assert!(error.detail.contains("requires Vo toolchain ^9.0.0"));
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
    fn authored_project_files_are_replaced_atomically() {
        let temp = tempfile::tempdir().unwrap();
        let mod_file = ModFile::parse("module github.com/acme/atomic\nvo ^0.1.0\n").unwrap();
        let lock_file = build_lock_file_from_mod_file(&mod_file, Vec::new(), "vo test").unwrap();

        write_project_files(temp.path(), &mod_file, Some(&lock_file)).unwrap();

        assert_eq!(read_mod_file(temp.path()).unwrap().module, mod_file.module);
        assert_eq!(
            read_lock_file(temp.path()).unwrap().root.module,
            lock_file.root.module
        );
        let names: Vec<_> = std::fs::read_dir(temp.path())
            .unwrap()
            .map(|entry| entry.unwrap().file_name())
            .collect();
        assert!(names
            .iter()
            .all(|name| !name.to_string_lossy().ends_with(".tmp")));
    }

    #[test]
    fn authored_project_transaction_rolls_back_lock_on_manifest_failure() {
        let temp = tempfile::tempdir().unwrap();
        let old_lock = b"previous lock contents";
        std::fs::write(temp.path().join("vo.lock"), old_lock).unwrap();
        std::fs::create_dir(temp.path().join("vo.mod")).unwrap();
        let mod_file = ModFile::parse("module github.com/acme/rollback\nvo ^0.1.0\n").unwrap();
        let lock_file = build_lock_file_from_mod_file(&mod_file, Vec::new(), "vo test").unwrap();

        assert!(write_project_files(temp.path(), &mod_file, Some(&lock_file)).is_err());

        assert_eq!(
            std::fs::read(temp.path().join("vo.lock")).unwrap(),
            old_lock
        );
    }

    #[test]
    fn create_new_reports_that_the_manifest_committed_when_parent_sync_fails() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("vo.mod");
        let mod_file = ModFile::parse("module github.com/acme/new\nvo ^0.1.0\n").unwrap();
        fail_project_parent_sync_for_test(&path);

        let error = write_new_mod_file(temp.path(), &mod_file).unwrap_err();

        assert!(matches!(
            error,
            Error::ProjectPublicationPostCommitFailure { ref path, .. }
                if path.ends_with("vo.mod")
        ));
        assert_eq!(read_mod_file(temp.path()).unwrap().module, mod_file.module);
    }

    #[test]
    fn remove_reports_that_the_lock_was_removed_when_parent_sync_fails() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("vo.lock");
        std::fs::write(&path, b"old lock").unwrap();
        fail_project_parent_sync_for_test(&path);

        let error = remove_lock_file_if_exists(temp.path()).unwrap_err();

        assert!(matches!(
            error,
            Error::ProjectPublicationPostCommitFailure { ref path, .. }
                if path.ends_with("vo.lock")
        ));
        assert!(!path.exists());
    }

    #[test]
    fn project_pair_finishes_after_committed_lock_sync_failure() {
        let temp = tempfile::tempdir().unwrap();
        let mod_file = ModFile::parse("module github.com/acme/pair\nvo ^0.1.0\n").unwrap();
        let lock_file = build_lock_file_from_mod_file(&mod_file, Vec::new(), "vo test").unwrap();
        fail_project_parent_sync_for_test(&temp.path().join("vo.lock"));

        let error = write_project_files(temp.path(), &mod_file, Some(&lock_file)).unwrap_err();

        assert!(matches!(
            error,
            Error::ProjectPublicationPostCommitFailure { .. }
        ));
        assert_eq!(read_mod_file(temp.path()).unwrap().module, mod_file.module);
        assert_eq!(read_lock_file(temp.path()).unwrap(), lock_file);
    }

    #[test]
    fn project_pair_keeps_committed_manifest_after_its_sync_failure() {
        let temp = tempfile::tempdir().unwrap();
        let mod_file = ModFile::parse("module github.com/acme/pair-mod\nvo ^0.1.0\n").unwrap();
        let lock_file = build_lock_file_from_mod_file(&mod_file, Vec::new(), "vo test").unwrap();
        fail_project_parent_sync_for_test(&temp.path().join("vo.mod"));

        let error = write_project_files(temp.path(), &mod_file, Some(&lock_file)).unwrap_err();

        assert!(matches!(
            error,
            Error::ProjectPublicationPostCommitFailure { .. }
        ));
        assert_eq!(read_mod_file(temp.path()).unwrap().module, mod_file.module);
        assert_eq!(read_lock_file(temp.path()).unwrap(), lock_file);
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn project_context_reader_waits_for_pair_commit_guard() {
        use std::sync::mpsc;

        let temp = tempfile::tempdir().unwrap();
        let old_mod = ModFile::parse("module github.com/acme/old\nvo ^0.1.0\n").unwrap();
        let old_lock = build_lock_file_from_mod_file(&old_mod, Vec::new(), "vo test").unwrap();
        write_project_files(temp.path(), &old_mod, Some(&old_lock)).unwrap();

        let project_dir = temp.path().to_path_buf();
        let writer_dir = project_dir.clone();
        let (locked_tx, locked_rx) = mpsc::channel();
        let (release_tx, release_rx) = mpsc::channel();
        let writer = std::thread::spawn(move || {
            let guard = lock_project_mutation(&writer_dir).unwrap();
            locked_tx.send(()).unwrap();
            release_rx.recv().unwrap();
            let new_mod =
                ModFile::parse("module github.com/acme/new-snapshot\nvo ^0.1.0\n").unwrap();
            let new_lock = build_lock_file_from_mod_file(&new_mod, Vec::new(), "vo test").unwrap();
            write_project_files(&writer_dir, &new_mod, Some(&new_lock)).unwrap();
            drop(guard);
        });
        locked_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();

        let reader_dir = project_dir.clone();
        let (result_tx, result_rx) = mpsc::channel();
        let reader = std::thread::spawn(move || {
            let fs = RealFs::new(".");
            let context = load_project_context(&fs, &reader_dir).unwrap();
            result_tx
                .send(context.project_deps().current_module().unwrap().to_string())
                .unwrap();
        });
        std::thread::sleep(std::time::Duration::from_millis(30));
        assert!(matches!(
            result_rx.try_recv(),
            Err(mpsc::TryRecvError::Empty)
        ));

        release_tx.send(()).unwrap();
        assert_eq!(
            result_rx
                .recv_timeout(std::time::Duration::from_secs(5))
                .unwrap(),
            "github.com/acme/new-snapshot"
        );
        writer.join().unwrap();
        reader.join().unwrap();
    }
}
