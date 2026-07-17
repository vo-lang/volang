use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
#[cfg(any(unix, windows))]
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
#[cfg(test)]
use std::sync::{Mutex, OnceLock};

use sha2::{Digest, Sha256};
use vo_common::stable_hash::StableHasher;
use vo_common::vfs::{
    normalize_ancestor_discovery_start, normalize_fs_path, FileSystem, FileSystemEntryKind,
    MemoryFs, RealFs,
};

use crate::inline_mod::{self, InlineMod, INLINE_MOD_OPEN};
use crate::lock;
use crate::operation_error::OperationError;
use crate::schema::lockfile::{LockFile, LockRoot, LockedModule, LOCK_FILE_VERSION};
use crate::schema::modfile::ModFile;
use crate::version::{ToolchainConstraint, ToolchainVersion};
use crate::workspace::WorkspaceDiscovery;
use crate::Error;

const PROJECT_LOCK_FILE: &str = ".vo-project.lock";
const PROJECT_TRANSACTION_FILE: &str = ".vo-project.transaction";
const PROJECT_PUBLIC_PROTOCOL_FILES: &[&str] = &["vo.mod", "vo.lock"];
const PROJECT_CONTROL_FILES: &[&str] = &[
    "vo.mod",
    "vo.lock",
    PROJECT_LOCK_FILE,
    PROJECT_TRANSACTION_FILE,
];
const PROJECT_TRANSACTION_MAGIC: &[u8] = b"vo-project-transaction-v1\0";
const PROJECT_TRANSACTION_NONE: u64 = u64::MAX;
const PROJECT_TRANSACTION_DIGEST_BYTES: usize = 32;
const MAX_PROJECT_TRANSACTION_BYTES: usize = PROJECT_TRANSACTION_MAGIC.len()
    + 16
    + vo_common::vfs::MAX_TEXT_FILE_BYTES
    + crate::MAX_LOCK_FILE_BYTES
    + PROJECT_TRANSACTION_DIGEST_BYTES;

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

    /// Registry modules that must be materialized for this build. Modules
    /// supplied by workspace sources are omitted from this materialization view.
    pub fn locked_modules(&self) -> &[LockedModule] {
        match self.module_context() {
            Some(context) => context.lock_state.locked_modules(),
            None => &[],
        }
    }

    /// The complete, already-validated root lock graph, including modules
    /// whose source is supplied by the selected workspace.
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

    fn select_workspace_sources(&mut self, workspace_modules: &BTreeSet<String>) {
        let Some(context) = self.module_context_mut() else {
            return;
        };
        let LockState::Locked(locked) = &mut context.lock_state else {
            return;
        };
        locked
            .allowed_modules
            .retain(|module| !workspace_modules.contains(module));
        locked
            .locked_modules
            .retain(|module| !workspace_modules.contains(module.path.as_str()));
    }

    fn module_context_mut(&mut self) -> Option<&mut ProjectModuleContext> {
        match &mut self.kind {
            ProjectDepsKind::NoModule => None,
            ProjectDepsKind::WithModule(context) => Some(context.as_mut()),
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

fn project_snapshot_lock_error(project_dir: &Path, error: Error) -> ProjectDepsError {
    ProjectDepsError::new(
        ProjectDepsStage::LockFile,
        ProjectDepsErrorKind::ReadFailed,
        format!("project snapshot lock failed: {error}"),
    )
    .with_path(&project_dir.join(".vo-project.lock"))
}

fn validate_current_toolchain(
    constraint: &ToolchainConstraint,
    path: &Path,
) -> Result<(), ProjectDepsError> {
    let current = ToolchainVersion::parse(crate::TOOLCHAIN_VERSION)
        .expect("the module-protocol toolchain version must be valid semantic versioning");
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

fn reject_project_portable_aliases(
    project_dir: &Path,
    canonical_names: &[&str],
) -> Result<(), Error> {
    let fs = RealFs::new(".");
    let entries =
        crate::schema::read_canonical_directory_entries(&fs, project_dir).map_err(|error| {
            Error::SourceScan(format!(
                "cannot enumerate project directory {} for portable protocol aliases: {error}",
                project_dir.display(),
            ))
        })?;
    if let Some((alias, canonical)) =
        crate::schema::first_portable_name_alias(&entries, canonical_names)
    {
        return Err(Error::SourceScan(format!(
            "project directory {} contains portable alias {} for canonical protocol file {canonical}",
            project_dir.display(),
            alias.display(),
        )));
    }
    Ok(())
}

fn reject_project_portable_aliases_in<F: FileSystem>(
    fs: &F,
    project_dir: &Path,
    canonical_names: &[&str],
) -> Result<(), ProjectDepsError> {
    if fs.entry_kind(project_dir).map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::ModFile,
            ProjectDepsErrorKind::ReadFailed,
            format!("cannot inspect project directory for portable protocol aliases: {error}"),
        )
        .with_path(project_dir)
    })? == FileSystemEntryKind::Missing
    {
        // Ancestor discovery may begin below the virtual or host filesystem's
        // existing namespace. Missing levels contain no aliases and must be
        // skipped so the walk can still reach a real parent project.
        return Ok(());
    }
    let entries =
        crate::schema::read_canonical_directory_entries(fs, project_dir).map_err(|error| {
            ProjectDepsError::new(
                ProjectDepsStage::ModFile,
                ProjectDepsErrorKind::ReadFailed,
                format!(
                    "cannot enumerate project directory for portable protocol aliases: {error}"
                ),
            )
            .with_path(project_dir)
        })?;
    if let Some((alias, canonical)) =
        crate::schema::first_portable_name_alias(&entries, canonical_names)
    {
        let stage = if canonical == "vo.mod" {
            ProjectDepsStage::ModFile
        } else {
            ProjectDepsStage::LockFile
        };
        return Err(ProjectDepsError::new(
            stage,
            ProjectDepsErrorKind::ValidationFailed,
            format!(
                "project directory contains portable alias {} for canonical protocol file {canonical}",
                alias.display(),
            ),
        )
        .with_path(&alias));
    }
    Ok(())
}

pub(crate) fn read_mod_file(project_dir: &Path) -> Result<ModFile, Error> {
    reject_project_portable_aliases(project_dir, PROJECT_PUBLIC_PROTOCOL_FILES)?;
    let path = project_dir.join("vo.mod");
    let fs = RealFs::new(".");
    match fs.entry_kind(&path)? {
        FileSystemEntryKind::Missing => {
            return Err(Error::Io(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("project vo.mod does not exist: {}", path.display()),
            )))
        }
        FileSystemEntryKind::RegularFile => {}
        found => {
            return Err(Error::ModFileParse(format!(
                "{} must be a regular file without links or special entries; found {found:?}",
                path.display()
            )))
        }
    }
    let content = crate::workspace::read_stable_regular_text_file(
        &fs,
        &path,
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
        "project vo.mod",
    )?;
    ModFile::parse(&content)
}

/// Read one module declaration without creating project state.
///
/// An existing mutation lock is acquired in shared mode. If a writer creates
/// the lock while an initially lock-free read is in progress, the read is
/// retried under that lock so callers never publish a torn declaration.
pub fn read_mod_file_stable(project_dir: &Path) -> Result<ModFile, Error> {
    const MAX_SNAPSHOT_ATTEMPTS: usize = 8;
    for _ in 0..MAX_SNAPSHOT_ATTEMPTS {
        let read_guard = lock_existing_project_for_read(project_dir)?;
        let mod_file = read_mod_file(project_dir);
        #[cfg(test)]
        if read_guard.is_none() {
            pause_stable_mod_read_after_raw_read_for_test(project_dir);
        }
        if let Some(guard) = read_guard.as_ref() {
            guard.validate_for(project_dir)?;
            return mod_file;
        }
        if !project_dir.join(".vo-project.lock").exists() {
            return mod_file;
        }
        drop(read_guard);
    }
    Err(Error::SourceScan(
        "project declaration changed during every bounded snapshot attempt".to_string(),
    ))
}

pub(crate) fn read_lock_file(project_dir: &Path) -> Result<LockFile, Error> {
    reject_project_portable_aliases(project_dir, PROJECT_PUBLIC_PROTOCOL_FILES)?;
    let path = project_dir.join("vo.lock");
    let fs = RealFs::new(".");
    match fs.entry_kind(&path)? {
        FileSystemEntryKind::Missing => {
            return Err(Error::Io(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("project vo.lock does not exist: {}", path.display()),
            )))
        }
        FileSystemEntryKind::RegularFile => {}
        found => {
            return Err(Error::LockFileParse(format!(
                "{} must be a regular file without links or special entries; found {found:?}",
                path.display()
            )))
        }
    }
    let content = crate::workspace::read_stable_regular_text_file(
        &fs,
        &path,
        crate::MAX_LOCK_FILE_BYTES,
        "project vo.lock",
    )?;
    LockFile::parse(&content)
}

/// Read one generated lock without observing a partially committed project
/// metadata transaction.
pub fn read_lock_file_stable(project_dir: &Path) -> Result<LockFile, Error> {
    const MAX_SNAPSHOT_ATTEMPTS: usize = 8;
    for _ in 0..MAX_SNAPSHOT_ATTEMPTS {
        let read_guard = lock_existing_project_for_read(project_dir)?;
        let lock_file = read_lock_file(project_dir);
        if let Some(guard) = read_guard.as_ref() {
            guard.validate_for(project_dir)?;
            return lock_file;
        }
        if !project_dir.join(".vo-project.lock").exists() {
            return lock_file;
        }
        drop(read_guard);
    }
    Err(Error::SourceScan(
        "project lock changed during every bounded snapshot attempt".to_string(),
    ))
}

/// Exclusive advisory lock spanning one project mutation's read, solve,
/// materialize, and commit phases. Keeping the lock file stable allows
/// independent CLI processes to serialize without a racy sentinel protocol.
pub(crate) struct ProjectMutationLock {
    #[cfg(any(unix, windows))]
    file: File,
    path: PathBuf,
    #[cfg(windows)]
    overlapped: Box<WindowsOverlapped>,
}

impl ProjectMutationLock {
    fn validate_for(&self, project_dir: &Path) -> Result<(), Error> {
        reject_project_portable_aliases(project_dir, PROJECT_CONTROL_FILES)?;
        let expected = project_dir.join(PROJECT_LOCK_FILE);
        if self.path != expected {
            return Err(Error::SourceScan(format!(
                "project lock {} cannot authorize mutation of {}",
                self.path.display(),
                project_dir.display()
            )));
        }
        self.validate_binding()
    }

    fn validate_binding(&self) -> Result<(), Error> {
        #[cfg(unix)]
        {
            use std::os::unix::fs::MetadataExt;
            let descriptor = self.file.metadata()?;
            let linked = std::fs::symlink_metadata(&self.path)?;
            if !descriptor.is_file()
                || !linked.file_type().is_file()
                || linked.file_type().is_symlink()
                || descriptor.nlink() != 1
                || linked.nlink() != 1
                || descriptor.dev() != linked.dev()
                || descriptor.ino() != linked.ino()
            {
                return Err(Error::SourceScan(format!(
                    "project lock {} was rebound while held",
                    self.path.display()
                )));
            }
        }
        #[cfg(windows)]
        {
            use std::os::windows::fs::{MetadataExt, OpenOptionsExt};
            const FILE_ATTRIBUTE_REPARSE_POINT: u32 = 0x0000_0400;
            const FILE_FLAG_OPEN_REPARSE_POINT: u32 = 0x0020_0000;
            let descriptor = self.file.metadata()?;
            let linked = std::fs::symlink_metadata(&self.path)?;
            let path_file = std::fs::OpenOptions::new()
                .read(true)
                .custom_flags(FILE_FLAG_OPEN_REPARSE_POINT)
                .open(&self.path)?;
            let path_metadata = path_file.metadata()?;
            let descriptor_information = crate::windows_file::file_information(&self.file)?;
            let path_information = crate::windows_file::file_information(&path_file)?;
            if !descriptor.is_file()
                || !linked.file_type().is_file()
                || !path_metadata.is_file()
                || descriptor.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0
                || linked.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0
                || path_information.attributes & FILE_ATTRIBUTE_REPARSE_POINT != 0
                || descriptor_information.directory
                || path_information.directory
                || descriptor_information.delete_pending
                || path_information.delete_pending
                || descriptor_information.links != 1
                || path_information.links != 1
                || descriptor_information.volume != path_information.volume
                || descriptor_information.file != path_information.file
            {
                return Err(Error::SourceScan(format!(
                    "project lock {} was rebound while held",
                    self.path.display()
                )));
            }
        }
        Ok(())
    }
}

pub(crate) fn lock_project_mutation(project_dir: &Path) -> Result<ProjectMutationLock, Error> {
    let guard = acquire_project_file_lock(project_dir, true, true)?.ok_or_else(|| {
        Error::from(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "project mutation locking is unavailable on this platform",
        ))
    })?;
    guard.validate_for(project_dir)?;
    recover_project_transaction_locked(project_dir, &guard)?;
    guard.validate_for(project_dir)?;
    Ok(guard)
}

fn lock_existing_project_for_read(
    project_dir: &Path,
) -> Result<Option<ProjectMutationLock>, Error> {
    const MAX_RECOVERY_ATTEMPTS: usize = 8;
    for _ in 0..MAX_RECOVERY_ATTEMPTS {
        let read_guard = acquire_project_file_lock(project_dir, false, false)?;
        if !project_transaction_journal_exists(project_dir, read_guard.as_ref())? {
            if let Some(guard) = read_guard.as_ref() {
                guard.validate_for(project_dir)?;
            }
            return Ok(read_guard);
        }

        // A shared guard freezes writers while the journal is inspected. Drop
        // it before taking the exclusive recovery lock, then retry from a new
        // shared snapshot after recovery completes.
        drop(read_guard);
        let recovery_guard =
            acquire_project_file_lock(project_dir, true, true)?.ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::Unsupported,
                    "project transaction recovery locking is unavailable on this platform",
                )
            })?;
        recovery_guard.validate_for(project_dir)?;
        recover_project_transaction_locked(project_dir, &recovery_guard)?;
        recovery_guard.validate_for(project_dir)?;
        drop(recovery_guard);
    }
    Err(Error::SourceScan(
        "project transaction journal reappeared during every bounded read attempt".to_string(),
    ))
}

fn project_transaction_journal_exists(
    project_dir: &Path,
    guard: Option<&ProjectMutationLock>,
) -> Result<bool, Error> {
    if let Some(guard) = guard {
        guard.validate_for(project_dir)?;
    }
    let journal_path = project_dir.join(PROJECT_TRANSACTION_FILE);
    let result = match std::fs::symlink_metadata(&journal_path) {
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(false),
        Err(error) => Err(error.into()),
        Ok(metadata) if metadata.file_type().is_file() => Ok(true),
        Ok(_) => Err(Error::SourceScan(format!(
            "project transaction journal {} must be a regular file",
            journal_path.display()
        ))),
    };
    if let Some(guard) = guard {
        guard.validate_for(project_dir)?;
    }
    result
}

fn acquire_project_file_lock(
    project_dir: &Path,
    create: bool,
    exclusive: bool,
) -> Result<Option<ProjectMutationLock>, Error> {
    reject_project_portable_aliases(project_dir, PROJECT_CONTROL_FILES)?;
    let path = project_dir.join(PROJECT_LOCK_FILE);
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
    reject_project_portable_aliases(project_dir, PROJECT_CONTROL_FILES)?;
    #[cfg(test)]
    notify_project_lock_attempt_for_test(&path);

    #[cfg(unix)]
    {
        use std::os::fd::AsRawFd;
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
        let guard = ProjectMutationLock { file, path };
        guard.validate_binding()?;
        Ok(Some(guard))
    }

    #[cfg(windows)]
    {
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
        let guard = ProjectMutationLock {
            file,
            path,
            overlapped,
        };
        guard.validate_binding()?;
        return Ok(Some(guard));
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
            let _ = unsafe { libc::flock(self.file.as_raw_fd(), libc::LOCK_UN) };
        }
        #[cfg(windows)]
        {
            use std::os::windows::io::AsRawHandle;
            let _ = unsafe {
                UnlockFileEx(
                    self.file.as_raw_handle(),
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
struct StableModReadPause {
    reached: std::sync::mpsc::Sender<()>,
    resume: std::sync::mpsc::Receiver<()>,
}

#[cfg(test)]
static STABLE_MOD_READ_PAUSES: OnceLock<Mutex<HashMap<PathBuf, StableModReadPause>>> =
    OnceLock::new();

#[cfg(test)]
type ProjectLockAttempt = (PathBuf, std::sync::mpsc::Sender<()>);

#[cfg(test)]
static PROJECT_LOCK_ATTEMPT_HOOK: OnceLock<Mutex<Option<ProjectLockAttempt>>> = OnceLock::new();

#[cfg(test)]
static PROJECT_CONTEXT_ROOT_READ_PAUSES: OnceLock<Mutex<HashMap<PathBuf, StableModReadPause>>> =
    OnceLock::new();

#[cfg(test)]
static PROJECT_CONTEXT_WORKSPACE_VALIDATE_PAUSES: OnceLock<
    Mutex<HashMap<PathBuf, StableModReadPause>>,
> = OnceLock::new();

#[cfg(test)]
static SINGLE_FILE_CONTEXT_VALIDATE_PAUSES: OnceLock<Mutex<HashMap<PathBuf, StableModReadPause>>> =
    OnceLock::new();

#[cfg(test)]
fn pause_stable_mod_read_after_raw_read_for_test(project_dir: &Path) {
    let pause = STABLE_MOD_READ_PAUSES
        .get_or_init(|| Mutex::new(HashMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(project_dir);
    if let Some(pause) = pause {
        pause.reached.send(()).unwrap();
        pause.resume.recv().unwrap();
    }
}

#[cfg(test)]
fn pause_project_context_after_root_read_for_test(project_dir: &Path) {
    let pause = PROJECT_CONTEXT_ROOT_READ_PAUSES
        .get_or_init(|| Mutex::new(HashMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(project_dir);
    if let Some(pause) = pause {
        pause.reached.send(()).unwrap();
        pause.resume.recv().unwrap();
    }
}

#[cfg(test)]
fn pause_project_context_before_workspace_validation_for_test(project_dir: &Path) {
    let pause = PROJECT_CONTEXT_WORKSPACE_VALIDATE_PAUSES
        .get_or_init(|| Mutex::new(HashMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(project_dir);
    if let Some(pause) = pause {
        pause.reached.send(()).unwrap();
        pause.resume.recv().unwrap();
    }
}

#[cfg(test)]
fn pause_single_file_context_before_validation_for_test(file_path: &Path) {
    let pause = SINGLE_FILE_CONTEXT_VALIDATE_PAUSES
        .get_or_init(|| Mutex::new(HashMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(file_path);
    if let Some(pause) = pause {
        pause.reached.send(()).unwrap();
        pause.resume.recv().unwrap();
    }
}

#[cfg(test)]
fn notify_project_lock_attempt_for_test(path: &Path) {
    let sender = {
        let mut slot = PROJECT_LOCK_ATTEMPT_HOOK
            .get_or_init(|| Mutex::new(None))
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if slot.as_ref().is_some_and(|(expected, _)| expected == path) {
            slot.take().map(|(_, sender)| sender)
        } else {
            None
        }
    };
    if let Some(sender) = sender {
        sender.send(()).unwrap();
    }
}

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

#[derive(Debug)]
struct PendingProjectTransaction {
    mod_bytes: Vec<u8>,
    lock_bytes: Option<Vec<u8>>,
}

fn render_project_transaction(
    mod_bytes: &[u8],
    lock_bytes: Option<&[u8]>,
) -> Result<Vec<u8>, Error> {
    if mod_bytes.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
        return Err(Error::SourceScan(format!(
            "vo.mod exceeds the {}-byte project transaction limit",
            vo_common::vfs::MAX_TEXT_FILE_BYTES
        )));
    }
    if lock_bytes.is_some_and(|bytes| bytes.len() > crate::MAX_LOCK_FILE_BYTES) {
        return Err(Error::SourceScan(format!(
            "vo.lock exceeds the {}-byte project transaction limit",
            crate::MAX_LOCK_FILE_BYTES
        )));
    }
    let lock_len = match lock_bytes {
        Some(bytes) => u64::try_from(bytes.len())
            .map_err(|_| Error::SourceScan("vo.lock length exceeds u64".to_string()))?,
        None => PROJECT_TRANSACTION_NONE,
    };
    let capacity = PROJECT_TRANSACTION_MAGIC
        .len()
        .checked_add(16)
        .and_then(|size| size.checked_add(mod_bytes.len()))
        .and_then(|size| size.checked_add(lock_bytes.map_or(0, <[u8]>::len)))
        .and_then(|size| size.checked_add(PROJECT_TRANSACTION_DIGEST_BYTES))
        .ok_or_else(|| Error::SourceScan("project transaction size overflow".to_string()))?;
    if capacity > MAX_PROJECT_TRANSACTION_BYTES {
        return Err(Error::SourceScan(format!(
            "project transaction exceeds the {MAX_PROJECT_TRANSACTION_BYTES}-byte limit"
        )));
    }
    let mut bytes = Vec::with_capacity(capacity);
    bytes.extend_from_slice(PROJECT_TRANSACTION_MAGIC);
    bytes.extend_from_slice(
        &u64::try_from(mod_bytes.len())
            .map_err(|_| Error::SourceScan("vo.mod length exceeds u64".to_string()))?
            .to_le_bytes(),
    );
    bytes.extend_from_slice(&lock_len.to_le_bytes());
    bytes.extend_from_slice(mod_bytes);
    if let Some(lock_bytes) = lock_bytes {
        bytes.extend_from_slice(lock_bytes);
    }
    let digest = Sha256::digest(&bytes);
    bytes.extend_from_slice(&digest);
    Ok(bytes)
}

fn parse_project_transaction(bytes: &[u8]) -> Result<PendingProjectTransaction, Error> {
    let header_len = PROJECT_TRANSACTION_MAGIC.len() + 16;
    let minimum_len = header_len + PROJECT_TRANSACTION_DIGEST_BYTES;
    if bytes.len() < minimum_len || bytes.len() > MAX_PROJECT_TRANSACTION_BYTES {
        return Err(Error::SourceScan(format!(
            "project transaction journal has invalid size {}",
            bytes.len()
        )));
    }
    if !bytes.starts_with(PROJECT_TRANSACTION_MAGIC) {
        return Err(Error::SourceScan(
            "project transaction journal has an unsupported format".to_string(),
        ));
    }
    let digest_offset = bytes.len() - PROJECT_TRANSACTION_DIGEST_BYTES;
    let expected_digest = Sha256::digest(&bytes[..digest_offset]);
    if expected_digest[..] != bytes[digest_offset..] {
        return Err(Error::SourceScan(
            "project transaction journal digest mismatch".to_string(),
        ));
    }
    let mod_len = u64::from_le_bytes(
        bytes[PROJECT_TRANSACTION_MAGIC.len()..PROJECT_TRANSACTION_MAGIC.len() + 8]
            .try_into()
            .expect("fixed project transaction length field"),
    );
    let lock_len = u64::from_le_bytes(
        bytes[PROJECT_TRANSACTION_MAGIC.len() + 8..header_len]
            .try_into()
            .expect("fixed project transaction length field"),
    );
    let mod_len = usize::try_from(mod_len)
        .map_err(|_| Error::SourceScan("project transaction vo.mod length overflow".to_string()))?;
    if mod_len > vo_common::vfs::MAX_TEXT_FILE_BYTES {
        return Err(Error::SourceScan(
            "project transaction vo.mod exceeds its size limit".to_string(),
        ));
    }
    let lock_len = if lock_len == PROJECT_TRANSACTION_NONE {
        None
    } else {
        let length = usize::try_from(lock_len).map_err(|_| {
            Error::SourceScan("project transaction vo.lock length overflow".to_string())
        })?;
        if length > crate::MAX_LOCK_FILE_BYTES {
            return Err(Error::SourceScan(
                "project transaction vo.lock exceeds its size limit".to_string(),
            ));
        }
        Some(length)
    };
    let payload_len = mod_len
        .checked_add(lock_len.unwrap_or(0))
        .ok_or_else(|| Error::SourceScan("project transaction payload overflow".to_string()))?;
    if header_len + payload_len != digest_offset {
        return Err(Error::SourceScan(
            "project transaction journal length fields do not match its payload".to_string(),
        ));
    }
    let mod_end = header_len + mod_len;
    let mod_bytes = bytes[header_len..mod_end].to_vec();
    let mod_text = std::str::from_utf8(&mod_bytes).map_err(|error| {
        Error::SourceScan(format!(
            "project transaction vo.mod is not valid UTF-8: {error}"
        ))
    })?;
    let mod_file = ModFile::parse(mod_text)?;
    let mut parsed_lock = None;
    let lock_bytes = match lock_len {
        Some(_) => {
            let lock_bytes = bytes[mod_end..digest_offset].to_vec();
            let lock_text = std::str::from_utf8(&lock_bytes).map_err(|error| {
                Error::SourceScan(format!(
                    "project transaction vo.lock is not valid UTF-8: {error}"
                ))
            })?;
            let lock_file = LockFile::parse(lock_text)?;
            parsed_lock = Some(lock_file);
            Some(lock_bytes)
        }
        None => None,
    };
    validate_project_file_pair(&mod_file, parsed_lock.as_ref())?;
    Ok(PendingProjectTransaction {
        mod_bytes,
        lock_bytes,
    })
}

pub(crate) fn validate_project_file_pair(
    mod_file: &ModFile,
    lock_file: Option<&LockFile>,
) -> Result<(), Error> {
    match (mod_file.dependencies.is_empty(), lock_file) {
        (true, None) => Ok(()),
        (true, Some(_)) => Err(Error::DependencyGraph(
            "vo.lock must be absent when vo.mod has no external dependencies".to_string(),
        )),
        (false, None) => Err(Error::DependencyGraph(
            "vo.lock is required whenever vo.mod declares external dependencies".to_string(),
        )),
        (false, Some(lock_file)) => {
            lock::verify_root_consistency(mod_file, lock_file)?;
            lock::verify_graph_completeness(mod_file, lock_file)
        }
    }
}

fn recover_project_transaction_locked(
    project_dir: &Path,
    guard: &ProjectMutationLock,
) -> Result<(), Error> {
    guard.validate_for(project_dir)?;
    let journal_path = project_dir.join(PROJECT_TRANSACTION_FILE);
    let metadata = match std::fs::symlink_metadata(&journal_path) {
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(error) => return Err(error.into()),
        Ok(metadata) => metadata,
    };
    if !metadata.file_type().is_file() || metadata.file_type().is_symlink() {
        return Err(Error::SourceScan(format!(
            "project transaction journal {} must be one regular non-symlink file",
            journal_path.display()
        )));
    }
    let fs = RealFs::new(".");
    let bytes = crate::workspace::read_stable_regular_file_bytes(
        &fs,
        &journal_path,
        MAX_PROJECT_TRANSACTION_BYTES,
        "project transaction journal",
    )?;
    let transaction = parse_project_transaction(&bytes)?;
    guard.validate_for(project_dir)?;
    write_or_remove_lock_bytes_outcome(project_dir, transaction.lock_bytes.as_deref())?
        .into_result()?;
    guard.validate_for(project_dir)?;
    write_bytes_atomically(&project_dir.join("vo.mod"), &transaction.mod_bytes)?.into_result()?;
    guard.validate_for(project_dir)?;
    remove_file_atomically(&journal_path)?.into_result()?;
    guard.validate_for(project_dir)
}

fn write_or_remove_lock_bytes_outcome(
    project_dir: &Path,
    lock_bytes: Option<&[u8]>,
) -> Result<ProjectFileMutationOutcome, Error> {
    match lock_bytes {
        Some(bytes) => write_bytes_atomically(&project_dir.join("vo.lock"), bytes),
        None => remove_file_atomically(&project_dir.join("vo.lock")),
    }
}

fn validate_project_replacement_target(path: &Path) -> Result<(), Error> {
    match std::fs::symlink_metadata(path) {
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error.into()),
        Ok(metadata) if metadata.file_type().is_file() && !metadata.file_type().is_symlink() => {
            Ok(())
        }
        Ok(_) => Err(Error::SourceScan(format!(
            "project protocol path {} must be absent or a regular non-symlink file",
            path.display()
        ))),
    }
}

pub(crate) fn write_new_mod_file(
    project_dir: &Path,
    guard: &ProjectMutationLock,
    mod_file: &ModFile,
) -> Result<(), Error> {
    guard.validate_for(project_dir)?;
    validate_project_file_pair(mod_file, None)?;
    let path = project_dir.join("vo.mod");
    let lock_path = project_dir.join("vo.lock");
    match std::fs::symlink_metadata(&lock_path) {
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => return Err(error.into()),
        Ok(_) => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::AlreadyExists,
                format!(
                    "cannot initialize {} while project lock {} already exists",
                    path.display(),
                    lock_path.display(),
                ),
            )
            .into())
        }
    }
    let rendered = mod_file.render()?;
    write_bytes_create_new(&path, rendered.as_bytes())?.into_result()?;
    guard.validate_for(project_dir)
}

#[cfg(test)]
pub(crate) fn remove_lock_file_if_exists(project_dir: &Path) -> Result<(), Error> {
    remove_file_atomically(&project_dir.join("vo.lock"))?.into_result()
}

/// Commit the authored module graph with a durable redo journal. Each file
/// replacement is atomic, and every stable reader completes an interrupted
/// transaction before exposing either file.
pub(crate) fn write_project_files(
    project_dir: &Path,
    guard: &ProjectMutationLock,
    mod_file: &ModFile,
    lock_file: Option<&LockFile>,
) -> Result<(), Error> {
    guard.validate_for(project_dir)?;
    validate_project_file_pair(mod_file, lock_file)?;
    let mod_path = project_dir.join("vo.mod");
    let lock_path = project_dir.join("vo.lock");
    let journal_path = project_dir.join(PROJECT_TRANSACTION_FILE);
    validate_project_replacement_target(&mod_path)?;
    validate_project_replacement_target(&lock_path)?;
    match std::fs::symlink_metadata(&journal_path) {
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => return Err(error.into()),
        Ok(_) => {
            return Err(Error::SourceScan(format!(
                "project transaction journal {} already exists; acquire the project mutation lock or perform recovery before writing",
                journal_path.display()
            )))
        }
    }
    let mod_bytes = mod_file.render()?.into_bytes();
    let lock_bytes = lock_file
        .map(LockFile::render)
        .transpose()?
        .map(String::into_bytes);
    let journal_bytes = render_project_transaction(&mod_bytes, lock_bytes.as_deref())?;

    // Never mutate either public file until the recovery record and its
    // directory entry are known durable.
    write_bytes_create_new(&journal_path, &journal_bytes)?.into_result()?;
    guard.validate_for(project_dir)?;
    write_or_remove_lock_bytes_outcome(project_dir, lock_bytes.as_deref())?.into_result()?;
    guard.validate_for(project_dir)?;
    write_bytes_atomically(&mod_path, &mod_bytes)?.into_result()?;
    guard.validate_for(project_dir)?;
    remove_file_atomically(&journal_path)?.into_result()?;
    guard.validate_for(project_dir)?;
    Ok(())
}

/// Load and validate the complete root graph when a lock is present.
/// Every project with external dependencies requires a complete root lock.
/// Workspace source selection is available only through [`ProjectContext`],
/// after the workspace has been authorized against that lock graph.
pub fn read_project_deps<F: FileSystem>(fs: &F) -> Result<ProjectDeps, ProjectDepsError> {
    const MAX_SNAPSHOT_ATTEMPTS: usize = 8;
    let root = Path::new(".");
    for _ in 0..MAX_SNAPSHOT_ATTEMPTS {
        let metadata = read_root_project_metadata(fs, root)?;
        let final_metadata = read_root_project_metadata(fs, root)?;
        if metadata == final_metadata {
            return project_deps_from_root_metadata(&metadata, root, false);
        }
    }
    Err(ProjectDepsError::new(
        ProjectDepsStage::LockFile,
        ProjectDepsErrorKind::ReadFailed,
        "project metadata changed during every bounded snapshot attempt",
    ))
}

pub fn read_project_deps_at_root(dir: &Path) -> Result<ProjectDeps, ProjectDepsError> {
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
        let metadata = read_root_project_metadata(&fs, dir)?;
        let final_metadata = read_root_project_metadata(&fs, dir)?;
        if let Some(guard) = read_guard.as_ref() {
            guard
                .validate_for(dir)
                .map_err(|error| project_snapshot_lock_error(dir, error))?;
            if metadata == final_metadata {
                return project_deps_from_root_metadata(&metadata, dir, true);
            }
            continue;
        }
        if metadata == final_metadata && !dir.join(".vo-project.lock").exists() {
            return project_deps_from_root_metadata(&metadata, dir, true);
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
    vo_lock_content: Option<&str>,
) -> Result<ProjectDeps, ProjectDepsError> {
    read_inline_project_deps_with(
        vo_mod_content,
        vo_lock_content,
        ModFile::parse_project,
        "vo.mod",
    )
}

/// Validate a synthesized standard-library-only `local/*` manifest.
///
/// This is the only string-based project-dependency entry point that accepts
/// the reserved ephemeral identity namespace. Ordinary on-disk projects must
/// use [`read_inline_project_deps`]. Ephemeral projects never participate in a
/// workspace or external dependency graph, so `vo.lock` must be absent.
pub fn read_inline_ephemeral_project_deps(
    vo_mod_content: &str,
    vo_lock_content: Option<&str>,
) -> Result<ProjectDeps, ProjectDepsError> {
    read_inline_project_deps_with(
        vo_mod_content,
        vo_lock_content,
        ModFile::parse_ephemeral,
        "ephemeral vo.mod",
    )
}

fn read_inline_project_deps_with(
    vo_mod_content: &str,
    vo_lock_content: Option<&str>,
    parse_mod_file: fn(&str) -> Result<ModFile, Error>,
    manifest_label: &str,
) -> Result<ProjectDeps, ProjectDepsError> {
    let mod_path = Path::new("vo.mod");
    let mod_file = parse_mod_file(vo_mod_content).map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::ModFile,
            ProjectDepsErrorKind::ParseFailed,
            format!("{manifest_label} parse error: {error}"),
        )
        .with_path(mod_path)
    })?;
    validate_current_toolchain(&mod_file.vo, mod_path)?;
    let mut fs = MemoryFs::new();
    if let Some(vo_lock_content) = vo_lock_content {
        fs.add_file("vo.lock", vo_lock_content);
    }
    read_project_deps_from_mod_file(&fs, mod_file, &[PathBuf::from("vo.lock")])
}

pub fn build_lock_file_from_mod_file(
    mod_file: &ModFile,
    mut locked_modules: Vec<LockedModule>,
) -> Result<LockFile, Error> {
    if mod_file.dependencies.is_empty() {
        return Err(Error::DependencyGraph(
            "vo.lock is undefined when vo.mod has no external dependencies".to_string(),
        ));
    }
    locked_modules.sort_by(|a, b| a.path.cmp(&b.path));
    let lock_file = LockFile {
        version: LOCK_FILE_VERSION,
        root: LockRoot {
            module: mod_file.module.clone(),
            vo: mod_file.vo.clone(),
        },
        modules: locked_modules,
    };
    lock::verify_root_consistency(mod_file, &lock_file)?;
    lock::verify_graph_completeness(mod_file, &lock_file)?;
    lock_file.render()?;
    Ok(lock_file)
}

pub fn build_lock_file_for_project_deps(
    project_deps: &ProjectDeps,
    locked_modules: Vec<LockedModule>,
) -> Result<LockFile, Error> {
    let Some(mod_file) = project_deps.mod_file() else {
        return Err(Error::ModFileParse(
            "cannot build lock file without vo.mod project metadata".to_string(),
        ));
    };
    build_lock_file_from_mod_file(mod_file, locked_modules)
}

fn read_project_deps_from_mod_file<F: FileSystem>(
    fs: &F,
    mod_file: ModFile,
    lock_candidates: &[PathBuf],
) -> Result<ProjectDeps, ProjectDepsError> {
    for lock_candidate in lock_candidates {
        let Some(lock_bytes) =
            read_optional_file_bytes(fs, lock_candidate, ProjectDepsStage::LockFile)?
        else {
            continue;
        };
        let lock_content =
            project_metadata_text(&lock_bytes, lock_candidate, ProjectDepsStage::LockFile)?;
        return project_deps_from_captured_metadata(mod_file, Some(lock_content), lock_candidate);
    }
    let lock_path = lock_candidates
        .first()
        .cloned()
        .unwrap_or_else(|| PathBuf::from("vo.lock"));
    project_deps_from_captured_metadata(mod_file, None, &lock_path)
}

fn project_deps_from_captured_metadata(
    mod_file: ModFile,
    lock_content: Option<&str>,
    lock_path: &Path,
) -> Result<ProjectDeps, ProjectDepsError> {
    let current_module = mod_file.module.as_str().to_string();
    let Some(lock_content) = lock_content else {
        if mod_file.dependencies.is_empty() {
            return Ok(ProjectDeps::with_module(ProjectModuleContext {
                mod_file,
                current_module,
                lock_state: LockState::NoLock,
            }));
        }
        let mut error = ProjectDepsError::new(
            ProjectDepsStage::LockFile,
            ProjectDepsErrorKind::Missing,
            "vo.lock is required whenever vo.mod declares external dependencies",
        );
        error = error.with_path(lock_path);
        return Err(error);
    };
    if mod_file.dependencies.is_empty() {
        return Err(ProjectDepsError::new(
            ProjectDepsStage::LockFile,
            ProjectDepsErrorKind::ValidationFailed,
            "vo.lock must be absent when vo.mod has no external dependencies",
        )
        .with_path(lock_path));
    }

    let lock_file = LockFile::parse(lock_content).map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::LockFile,
            ProjectDepsErrorKind::ParseFailed,
            format!("vo.lock parse error: {error}"),
        )
        .with_path(lock_path)
    })?;
    lock::verify_root_consistency(&mod_file, &lock_file).map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::LockFile,
            ProjectDepsErrorKind::ValidationFailed,
            format!("vo.lock validation error: {error}"),
        )
        .with_path(lock_path)
    })?;
    lock::verify_graph_completeness(&mod_file, &lock_file).map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::LockFile,
            ProjectDepsErrorKind::ValidationFailed,
            format!("vo.lock validation error: {error}"),
        )
        .with_path(lock_path)
    })?;
    let allowed_modules = lock_file
        .modules
        .iter()
        .map(|locked| locked.path.as_str().to_string())
        .collect();
    let locked_modules = lock_file.modules.clone();
    Ok(ProjectDeps::with_module(ProjectModuleContext {
        mod_file,
        current_module,
        lock_state: LockState::Locked(Box::new(LockedProjectContext {
            lock_file,
            allowed_modules,
            locked_modules,
        })),
    }))
}

fn read_optional_file_bytes<F: FileSystem>(
    fs: &F,
    path: &Path,
    stage: ProjectDepsStage,
) -> Result<Option<Vec<u8>>, ProjectDepsError> {
    let limit = if stage == ProjectDepsStage::LockFile {
        crate::MAX_LOCK_FILE_BYTES
    } else {
        vo_common::vfs::MAX_TEXT_FILE_BYTES
    };
    match fs.entry_kind(path).map_err(|error| {
        ProjectDepsError::new(
            stage,
            ProjectDepsErrorKind::ReadFailed,
            format!("cannot inspect {}: {error}", stage.as_str()),
        )
        .with_path(path)
    })? {
        FileSystemEntryKind::Missing => Ok(None),
        FileSystemEntryKind::RegularFile => {
            crate::workspace::read_stable_regular_file_bytes(fs, path, limit, stage.as_str())
                .map(Some)
                .map_err(|error| {
                    ProjectDepsError::new(
                        stage,
                        ProjectDepsErrorKind::ReadFailed,
                        format!("{} stable read error: {error}", stage.as_str()),
                    )
                    .with_path(path)
                })
        }
        found => Err(ProjectDepsError::new(
            stage,
            ProjectDepsErrorKind::ValidationFailed,
            format!(
                "{} must be a regular file without symbolic links, reparse points, or special entries; found {found:?}",
                stage.as_str(),
            ),
        )
        .with_path(path)),
    }
}

fn project_metadata_text<'a>(
    bytes: &'a [u8],
    path: &Path,
    stage: ProjectDepsStage,
) -> Result<&'a str, ProjectDepsError> {
    std::str::from_utf8(bytes).map_err(|error| {
        ProjectDepsError::new(
            stage,
            ProjectDepsErrorKind::ReadFailed,
            format!("{} is not valid UTF-8: {error}", stage.as_str()),
        )
        .with_path(path)
    })
}

/// Find the nearest project root by walking up from `dir` looking for `vo.mod`.
/// Returns `dir` itself (normalized) if no `vo.mod` is found.
fn find_project_root_in<F: FileSystem>(fs: &F, dir: &Path) -> Result<PathBuf, ProjectDepsError> {
    Ok(try_find_project_root_in(fs, dir)?.unwrap_or_else(|| normalize_fs_path(dir)))
}

/// Find the nearest project root without suppressing malformed manifest entries
/// or filesystem inspection failures encountered during ancestor discovery.
pub fn find_project_root(dir: &Path) -> Result<Option<PathBuf>, ProjectDepsError> {
    let fs = RealFs::new(".");
    try_find_project_root_in(&fs, dir)
}

fn try_find_project_root_in<F: FileSystem>(
    fs: &F,
    dir: &Path,
) -> Result<Option<PathBuf>, ProjectDepsError> {
    let Some(mut current) = normalize_ancestor_discovery_start(fs, dir).map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::ModFile,
            ProjectDepsErrorKind::ReadFailed,
            format!("cannot normalize project discovery start: {error}"),
        )
        .with_path(dir)
    })?
    else {
        return Ok(None);
    };
    loop {
        reject_project_portable_aliases_in(fs, &current, &["vo.mod"])?;
        let candidate = if current == Path::new(".") || current.as_os_str().is_empty() {
            PathBuf::from("vo.mod")
        } else {
            current.join("vo.mod")
        };
        match fs.entry_kind(&candidate).map_err(|error| {
            ProjectDepsError::new(
                ProjectDepsStage::ModFile,
                ProjectDepsErrorKind::ReadFailed,
                format!("cannot inspect project manifest: {error}"),
            )
            .with_path(&candidate)
        })? {
            FileSystemEntryKind::RegularFile => return Ok(Some(current)),
            FileSystemEntryKind::Missing => {}
            found => {
                return Err(ProjectDepsError::new(
                    ProjectDepsStage::ModFile,
                    ProjectDepsErrorKind::ValidationFailed,
                    format!(
                        "vo.mod must be a regular file without links or special entries; found {found:?}"
                    ),
                )
                .with_path(&candidate));
            }
        }
        if !current.pop() {
            return Ok(None);
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct RootProjectMetadata {
    mod_bytes: Option<Vec<u8>>,
    lock_bytes: Option<Vec<u8>>,
}

fn read_root_project_metadata<F: FileSystem>(
    fs: &F,
    dir: &Path,
) -> Result<RootProjectMetadata, ProjectDepsError> {
    reject_project_portable_aliases_in(fs, dir, PROJECT_PUBLIC_PROTOCOL_FILES)?;
    let mod_path = root_metadata_path(dir, "vo.mod");
    let lock_path = root_metadata_path(dir, "vo.lock");
    Ok(RootProjectMetadata {
        mod_bytes: read_optional_file_bytes(fs, &mod_path, ProjectDepsStage::ModFile)?,
        lock_bytes: read_optional_file_bytes(fs, &lock_path, ProjectDepsStage::LockFile)?,
    })
}

fn root_metadata_path(dir: &Path, file_name: &str) -> PathBuf {
    if dir == Path::new(".") || dir.as_os_str().is_empty() {
        PathBuf::from(file_name)
    } else {
        dir.join(file_name)
    }
}

fn project_deps_from_root_metadata(
    metadata: &RootProjectMetadata,
    project_root: &Path,
    require_mod_file: bool,
) -> Result<ProjectDeps, ProjectDepsError> {
    let mod_path = root_metadata_path(project_root, "vo.mod");
    let Some(mod_bytes) = metadata.mod_bytes.as_deref() else {
        if metadata.lock_bytes.is_some() {
            return Err(ProjectDepsError::new(
                ProjectDepsStage::LockFile,
                ProjectDepsErrorKind::ValidationFailed,
                "vo.lock must be absent when the selected root has no vo.mod",
            )
            .with_path(&root_metadata_path(project_root, "vo.lock")));
        }
        if require_mod_file {
            return Err(ProjectDepsError::new(
                ProjectDepsStage::ModFile,
                ProjectDepsErrorKind::Missing,
                "this operation requires vo.mod at the selected project root",
            )
            .with_path(&mod_path));
        }
        return Ok(ProjectDeps::no_module());
    };
    let mod_content = project_metadata_text(mod_bytes, &mod_path, ProjectDepsStage::ModFile)?;
    let mod_file = ModFile::parse(mod_content).map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::ModFile,
            ProjectDepsErrorKind::ParseFailed,
            format!("vo.mod parse error: {error}"),
        )
        .with_path(&mod_path)
    })?;
    validate_current_toolchain(&mod_file.vo, &mod_path)?;
    let lock_path = root_metadata_path(project_root, "vo.lock");
    let lock_content = metadata
        .lock_bytes
        .as_deref()
        .map(|bytes| project_metadata_text(bytes, &lock_path, ProjectDepsStage::LockFile))
        .transpose()?;
    project_deps_from_captured_metadata(mod_file, lock_content, &lock_path)
}

fn parse_root_mod_file(
    metadata: &RootProjectMetadata,
    project_root: &Path,
) -> Result<Option<ModFile>, ProjectDepsError> {
    let Some(bytes) = metadata.mod_bytes.as_deref() else {
        return Ok(None);
    };
    let mod_path = root_metadata_path(project_root, "vo.mod");
    let content = project_metadata_text(bytes, &mod_path, ProjectDepsStage::ModFile)?;
    ModFile::parse(content).map(Some).map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::ModFile,
            ProjectDepsErrorKind::ParseFailed,
            format!("vo.mod parse error: {error}"),
        )
        .with_path(&mod_path)
    })
}

/// Resolved project context: project root, dependencies, and workspace sources.
///
/// This is the canonical result of project discovery and should be used by both
/// native (vo-engine) and web (vo-web) compilation paths.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectAuthority {
    Empty,
    Lock,
    Workspace,
}

/// One reachable workspace module in the effective dependency graph.
///
/// The declaration is the exact stable `vo.mod` generation captured during
/// project loading. Consumers must use this value instead of rereading the
/// member path and creating a second authority generation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkspaceModule {
    module: crate::identity::ModulePath,
    directory: PathBuf,
    mod_file: ModFile,
}

impl WorkspaceModule {
    pub fn module(&self) -> &crate::identity::ModulePath {
        &self.module
    }

    pub fn directory(&self) -> &Path {
        &self.directory
    }

    pub fn mod_file(&self) -> &ModFile {
        &self.mod_file
    }
}

#[derive(Debug, Clone)]
pub struct ProjectContext {
    project_root: PathBuf,
    project_deps: ProjectDeps,
    authority: ProjectAuthority,
    workspace_modules: Vec<WorkspaceModule>,
    project_metadata_generation: String,
    workspace_file: Option<PathBuf>,
    workspace_sources: HashMap<String, PathBuf>,
    workspace_generation: String,
    validated_input_files: Vec<PathBuf>,
}

impl ProjectContext {
    pub fn project_root(&self) -> &Path {
        &self.project_root
    }

    pub fn project_deps(&self) -> &ProjectDeps {
        &self.project_deps
    }

    /// Graph authority selected for this effective project context.
    pub fn authority(&self) -> ProjectAuthority {
        self.authority
    }

    /// Reachable workspace modules together with the exact declarations that
    /// authorize their outgoing dependency edges.
    pub fn workspace_modules(&self) -> &[WorkspaceModule] {
        &self.workspace_modules
    }

    /// Exact root `vo.mod`/`vo.lock` byte-pair generation used to construct
    /// `project_deps`, including typed absence of either file.
    pub fn project_metadata_generation(&self) -> &str {
        &self.project_metadata_generation
    }

    /// Return whether two contexts use the same exact root project authority.
    ///
    /// This binds the normalized project root, the raw `vo.mod`/`vo.lock`
    /// generation (including file absence), the parsed root metadata, and the
    /// selected graph authority. Workspace generation and selected source
    /// directories are intentionally excluded so a two-pass consumer can
    /// materialize an already-authorized source closure and compare that
    /// closure separately.
    pub fn has_same_root_authority(&self, other: &Self) -> bool {
        self.project_root == other.project_root
            && self.project_metadata_generation == other.project_metadata_generation
            && self.authority == other.authority
            && self.project_deps.mod_file() == other.project_deps.mod_file()
            && self.project_deps.lock_file() == other.project_deps.lock_file()
    }

    /// Exact workspace file selected while this context was loaded.
    ///
    /// This is retained alongside the resolved source map so snapshot
    /// consumers can distinguish no workspace from an empty workspace and
    /// detect concurrent workspace appearance, disappearance, or relocation.
    pub fn workspace_file(&self) -> Option<&Path> {
        self.workspace_file.as_deref()
    }

    pub fn workspace_sources(&self) -> &HashMap<String, PathBuf> {
        &self.workspace_sources
    }

    /// Stable identity of the selected workspace and every discovered member
    /// directory generation used while authorizing this context.
    pub fn workspace_generation(&self) -> &str {
        &self.workspace_generation
    }

    /// Every stable project file read to authorize this context.
    ///
    /// This includes root metadata, the selected workspace file, every
    /// discovered member manifest, and every Vo source inspected by external
    /// import validation. Build-time consumers should track the whole set so
    /// metadata and source-only edits both invalidate incremental results.
    pub fn validated_input_files(&self) -> &[PathBuf] {
        &self.validated_input_files
    }

    pub fn into_parts(self) -> (PathBuf, ProjectDeps, HashMap<String, PathBuf>) {
        (self.project_root, self.project_deps, self.workspace_sources)
    }
}

/// Load the full project context for a directory.
///
/// Discovers the nearest `vo.mod` ancestor, loads workspace members, and
/// selects one explicit graph authority. A present lock remains authoritative
/// for the complete graph. Without a lock, an enabled selected `vo.work` may
/// authorize a graph only when every reachable owner is a local member.
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
    for attempt in 0..MAX_SNAPSHOT_ATTEMPTS {
        let project_root = find_project_root_in(fs, dir)?;
        let physical_project_dir = physical_project_directory(fs, &project_root)?;
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
        let root_metadata = read_root_project_metadata(fs, &project_root)?;
        #[cfg(test)]
        if read_guard.is_none() {
            if let Some(project_dir) = physical_project_dir.as_deref() {
                pause_project_context_after_root_read_for_test(project_dir);
            }
        }
        let snapshot =
            load_project_context_snapshot(fs, project_root.clone(), &root_metadata, options);
        #[cfg(test)]
        if read_guard.is_none() && snapshot.is_ok() {
            if let Some(project_dir) = physical_project_dir.as_deref() {
                pause_project_context_before_workspace_validation_for_test(project_dir);
            }
        }
        let final_root = find_project_root_in(fs, dir)?;
        let final_root_metadata = read_root_project_metadata(fs, &project_root)?;
        let lock_appeared = read_guard.is_none()
            && physical_project_dir
                .as_ref()
                .is_some_and(|project_dir| project_dir.join(".vo-project.lock").exists());
        if final_root == project_root && final_root_metadata == root_metadata && !lock_appeared {
            if let (Some(guard), Some(project_dir)) =
                (read_guard.as_ref(), physical_project_dir.as_ref())
            {
                guard
                    .validate_for(project_dir)
                    .map_err(|error| project_snapshot_lock_error(project_dir, error))?;
            }
            let (context, selected_workfile, workspace_candidates) = match snapshot {
                Ok(snapshot) => snapshot,
                Err(error) if error.stage == ProjectDepsStage::Workspace => {
                    if attempt + 1 == MAX_SNAPSHOT_ATTEMPTS {
                        return Err(error);
                    }
                    continue;
                }
                Err(error) => return Err(error),
            };
            let workspace_validation = validate_workspace_selection_generation(
                fs,
                &project_root,
                &options.workspace,
                selected_workfile.as_ref(),
            )
            .and_then(|()| {
                validate_workspace_candidate_generations(fs, &workspace_candidates).map_err(
                    |error| {
                        ProjectDepsError::new(
                            ProjectDepsStage::Workspace,
                            ProjectDepsErrorKind::ValidationFailed,
                            error.to_string(),
                        )
                    },
                )
            });
            match workspace_validation {
                Ok(()) => return Ok(context),
                Err(error) => {
                    if attempt + 1 == MAX_SNAPSHOT_ATTEMPTS {
                        return Err(error);
                    }
                    continue;
                }
            }
        }
        drop(read_guard);
    }
    Err(ProjectDepsError::new(
        ProjectDepsStage::LockFile,
        ProjectDepsErrorKind::ReadFailed,
        "project metadata changed during every bounded snapshot attempt",
    ))
}

fn validate_workspace_selection_generation<F: FileSystem>(
    fs: &F,
    project_root: &Path,
    discovery: &WorkspaceDiscovery,
    expected: Option<&crate::workspace::SelectedWorkfileGeneration>,
) -> Result<(), ProjectDepsError> {
    let selected = crate::workspace::discover_workfile_in_with(fs, project_root, discovery)
        .map_err(project_workspace_error)?;
    let expected_path = expected.map(crate::workspace::SelectedWorkfileGeneration::path);
    if selected.as_deref().map(normalize_fs_path) != expected_path.map(normalize_fs_path) {
        return Err(ProjectDepsError::new(
            ProjectDepsStage::Workspace,
            ProjectDepsErrorKind::ValidationFailed,
            "workspace selection changed while loading the project; retrying requires one stable workspace generation",
        ));
    }
    if let Some(expected) = expected {
        expected.validate(fs).map_err(project_workspace_error)?;
    }
    Ok(())
}

fn project_workspace_error(error: Error) -> ProjectDepsError {
    project_source_error(ProjectDepsStage::Workspace, error)
}

fn project_source_error_kind(error: &Error) -> ProjectDepsErrorKind {
    match error {
        Error::Io(_) => ProjectDepsErrorKind::ReadFailed,
        Error::SourceRead(error) if error.kind() == std::io::ErrorKind::InvalidData => {
            ProjectDepsErrorKind::ValidationFailed
        }
        Error::SourceRead(_) => ProjectDepsErrorKind::ReadFailed,
        Error::WorkFileParse(_) | Error::SourceScan(_) => ProjectDepsErrorKind::ParseFailed,
        _ => ProjectDepsErrorKind::ValidationFailed,
    }
}

fn project_source_error(stage: ProjectDepsStage, error: Error) -> ProjectDepsError {
    let kind = project_source_error_kind(&error);
    ProjectDepsError::new(stage, kind, error.to_string())
}

fn physical_project_directory<F: FileSystem>(
    fs: &F,
    project_root: &Path,
) -> Result<Option<PathBuf>, ProjectDepsError> {
    let physical = fs.resolve_host_path(project_root).map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::LockFile,
            ProjectDepsErrorKind::ReadFailed,
            format!("cannot resolve project directory on the host filesystem: {error}"),
        )
        .with_path(project_root)
    })?;
    Ok(physical.filter(|path| path.is_dir()))
}

fn validate_workspace_member_manifest(
    root_mod: &ModFile,
    source_entry: &crate::workspace::WorkspaceMember,
) -> Result<(), Error> {
    let mod_file = source_entry.mod_file();
    if mod_file.module.as_github() != Some(&source_entry.module) {
        return Err(Error::DependencyGraph(format!(
            "workspace directory {} declaration does not match {}",
            source_entry.local_dir.display(),
            source_entry.module
        )));
    }
    if !root_mod.vo.is_subset_of(&mod_file.vo) {
        return Err(Error::DependencyToolchainMismatch {
            module: source_entry.module.to_string(),
            project_constraint: root_mod.vo.to_string(),
            dependency_constraint: mod_file.vo.to_string(),
        });
    }
    Ok(())
}

fn authorize_locked_workspace_sources(
    root_mod: &ModFile,
    project_deps: &ProjectDeps,
    discovered: &[crate::workspace::WorkspaceMember],
) -> Result<Vec<crate::workspace::WorkspaceMember>, Error> {
    let Some(lock_file) = project_deps.lock_file() else {
        return Ok(Vec::new());
    };
    let locked_modules = lock_file
        .modules
        .iter()
        .map(|module| module.path.as_str())
        .collect::<BTreeSet<_>>();
    let mut authorized = Vec::new();
    for source_entry in discovered {
        if !locked_modules.contains(source_entry.module.as_str()) {
            continue;
        }
        validate_workspace_member_manifest(root_mod, source_entry)?;
        authorized.push(source_entry.clone());
    }
    authorized.sort_by(|left, right| left.module.cmp(&right.module));
    Ok(authorized)
}

/// Build the complete local dependency closure from stable workspace member
/// declarations. Missing owners keep registry intent explicit: they require a
/// complete v3 lock and cannot be silently omitted or partially resolved.
fn authorize_lockless_workspace_sources(
    root_mod: &ModFile,
    discovered: &[crate::workspace::WorkspaceMember],
) -> Result<Vec<crate::workspace::WorkspaceMember>, Error> {
    let mut candidates = BTreeMap::new();
    for member in discovered {
        if candidates.insert(member.module.as_str(), member).is_some() {
            return Err(Error::DependencyGraph(format!(
                "workspace contains duplicate module owner {}",
                member.module
            )));
        }
    }

    let mut incoming = BTreeMap::<String, Vec<(String, crate::version::DepConstraint)>>::new();
    let mut pending = VecDeque::new();
    for dependency in &root_mod.dependencies {
        incoming
            .entry(dependency.module.as_str().to_string())
            .or_default()
            .push((
                root_mod.module.as_str().to_string(),
                dependency.constraint.clone(),
            ));
        pending.push_back(dependency.module.as_str().to_string());
    }

    let mut reachable = BTreeMap::<String, crate::workspace::WorkspaceMember>::new();
    let mut edge_count = root_mod.dependencies.len();
    while let Some(module) = pending.pop_front() {
        if reachable.contains_key(&module) {
            continue;
        }
        let member = candidates.get(module.as_str()).copied().ok_or_else(|| {
            Error::DependencyGraph(format!(
                "workspace dependency graph requires {module}, but the selected vo.work has no member that owns it; any registry dependency requires a complete v3 vo.lock"
            ))
        })?;
        validate_workspace_member_manifest(root_mod, member)?;
        let manifest = member.mod_file();
        edge_count = edge_count
            .checked_add(manifest.dependencies.len())
            .ok_or_else(|| Error::ResolutionLimitExceeded {
                resource: "workspace dependency edge count".to_string(),
                limit: crate::MAX_SOLVER_GRAPH_EDGES,
            })?;
        if edge_count > crate::MAX_SOLVER_GRAPH_EDGES {
            return Err(Error::ResolutionLimitExceeded {
                resource: "workspace dependency edge count".to_string(),
                limit: crate::MAX_SOLVER_GRAPH_EDGES,
            });
        }
        for dependency in &manifest.dependencies {
            if dependency.module == member.module {
                return Err(Error::DependencyGraph(format!(
                    "workspace module {} must not depend on itself",
                    member.module
                )));
            }
            if root_mod.module.as_github() == Some(&dependency.module) {
                return Err(Error::DependencyGraph(format!(
                    "workspace module {} must not depend on root module {}",
                    member.module, root_mod.module
                )));
            }
            incoming
                .entry(dependency.module.as_str().to_string())
                .or_default()
                .push((
                    member.module.as_str().to_string(),
                    dependency.constraint.clone(),
                ));
            pending.push_back(dependency.module.as_str().to_string());
        }
        reachable.insert(module, member.clone());
    }

    for (module, constraints) in &incoming {
        if crate::version::dependency_constraints_have_common_version(
            constraints.iter().map(|(_, constraint)| constraint),
        ) {
            continue;
        }
        return Err(Error::ConflictingConstraints {
            module: module.clone(),
            detail: summarize_incoming_constraints(constraints),
        });
    }

    Ok(reachable.into_values().collect())
}

pub(crate) fn summarize_incoming_constraints(
    constraints: &[(String, crate::version::DepConstraint)],
) -> String {
    crate::summarize_diagnostic_items(
        constraints
            .iter()
            .map(|(importer, constraint)| format!("{importer} requires {constraint}")),
        "incoming requirement(s)",
    )
}

fn validate_workspace_candidate_generations<F: FileSystem>(
    fs: &F,
    candidates: &[crate::workspace::WorkspaceMember],
) -> Result<(), Error> {
    for candidate in candidates {
        candidate.validate_generation(fs)?;
    }
    Ok(())
}

fn calculate_workspace_generation(
    project_root: &Path,
    selected_workfile: Option<&crate::workspace::SelectedWorkfileGeneration>,
    candidates: &[crate::workspace::WorkspaceMember],
) -> String {
    let mut hasher = StableHasher::new("vo-project-workspace-generation-v3");
    hasher.update_path("project_root", &normalize_fs_path(project_root));
    hasher.update_bool("workspace_selected", selected_workfile.is_some());
    if let Some(selected_workfile) = selected_workfile {
        hasher.update_path(
            "workspace_file",
            &normalize_fs_path(selected_workfile.path()),
        );
        hasher.update_str(
            "workspace_file_generation",
            &selected_workfile.generation_key(),
        );
    }

    let mut members = candidates
        .iter()
        .map(|member| {
            (
                member.module.as_str(),
                normalize_fs_path(&member.local_dir),
                member.directory_generation_key(),
                member.manifest_generation_key(),
            )
        })
        .collect::<Vec<_>>();
    members.sort();
    hasher.update_bytes("member_count", &(members.len() as u64).to_le_bytes());
    for (module, local_dir, directory_generation, manifest_generation) in members {
        hasher.update_str("member_module", module);
        hasher.update_path("member_directory", &local_dir);
        hasher.update_str("member_directory_generation", &directory_generation);
        hasher.update_str("member_manifest_generation", &manifest_generation);
    }
    hasher.finish()
}

fn calculate_project_metadata_generation(
    project_root: &Path,
    metadata: &RootProjectMetadata,
) -> String {
    let mut hasher = StableHasher::new("vo-project-metadata-generation-v1");
    hasher.update_path("project_root", &normalize_fs_path(project_root));
    hasher.update_bool("mod_present", metadata.mod_bytes.is_some());
    if let Some(bytes) = metadata.mod_bytes.as_deref() {
        hasher.update_bytes("mod_bytes", bytes);
    }
    hasher.update_bool("lock_present", metadata.lock_bytes.is_some());
    if let Some(bytes) = metadata.lock_bytes.as_deref() {
        hasher.update_bytes("lock_bytes", bytes);
    }
    hasher.finish()
}

fn load_project_context_snapshot<F: FileSystem>(
    fs: &F,
    project_root: PathBuf,
    root_metadata: &RootProjectMetadata,
    options: &ProjectContextOptions,
) -> Result<
    (
        ProjectContext,
        Option<crate::workspace::SelectedWorkfileGeneration>,
        Vec<crate::workspace::WorkspaceMember>,
    ),
    ProjectDepsError,
> {
    let root_mod = parse_root_mod_file(root_metadata, &project_root)?;
    let (selected_workfile, workspace_candidates) =
        crate::workspace::discover_workspace_candidates_in_with_generation(
            fs,
            &project_root,
            root_mod.as_ref().map(|mf| &mf.module),
            &options.workspace,
        )
        .map_err(project_workspace_error)?;
    let workspace_file = selected_workfile
        .as_ref()
        .map(|generation| generation.path().to_path_buf());
    let mut project_deps = match root_mod.clone() {
        Some(mod_file) => {
            let mod_path = project_root.join("vo.mod");
            let lock_path = project_root.join("vo.lock");
            validate_current_toolchain(&mod_file.vo, &mod_path)?;
            let lock_content = root_metadata
                .lock_bytes
                .as_deref()
                .map(|bytes| project_metadata_text(bytes, &lock_path, ProjectDepsStage::LockFile))
                .transpose()?;
            if lock_content.is_none()
                && !mod_file.dependencies.is_empty()
                && selected_workfile.is_some()
            {
                let current_module = mod_file.module.as_str().to_string();
                ProjectDeps::with_module(ProjectModuleContext {
                    mod_file,
                    current_module,
                    lock_state: LockState::NoLock,
                })
            } else {
                project_deps_from_captured_metadata(
                    mod_file,
                    lock_content,
                    &normalize_fs_path(&lock_path),
                )?
            }
        }
        None => project_deps_from_root_metadata(root_metadata, &project_root, false)?,
    };
    validate_workspace_candidate_generations(fs, &workspace_candidates).map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::Workspace,
            ProjectDepsErrorKind::ValidationFailed,
            error.to_string(),
        )
    })?;
    let authority = match root_mod.as_ref() {
        None => ProjectAuthority::Empty,
        Some(_) if project_deps.lock_file().is_some() => ProjectAuthority::Lock,
        Some(root_mod) if root_mod.dependencies.is_empty() => ProjectAuthority::Empty,
        Some(_) if selected_workfile.is_some() => ProjectAuthority::Workspace,
        Some(_) => {
            return Err(ProjectDepsError::new(
                ProjectDepsStage::LockFile,
                ProjectDepsErrorKind::Missing,
                "vo.lock is required whenever vo.mod declares a dependency that is not closed by the selected workspace",
            )
            .with_path(&project_root.join("vo.lock")))
        }
    };
    let authorized_sources = match (root_mod.as_ref(), authority) {
        (Some(root_mod), ProjectAuthority::Lock) => {
            authorize_locked_workspace_sources(root_mod, &project_deps, &workspace_candidates)
        }
        (Some(root_mod), ProjectAuthority::Workspace) => {
            authorize_lockless_workspace_sources(root_mod, &workspace_candidates)
        }
        _ => Ok(Vec::new()),
    }
    .map_err(project_workspace_error)?;
    let authorized_modules = authorized_sources
        .iter()
        .map(|entry| entry.module.as_str().to_string())
        .collect::<BTreeSet<_>>();
    project_deps.select_workspace_sources(&authorized_modules);
    let validated_source_files = if let Some(root_mod) = root_mod.as_ref() {
        crate::workspace::validate_project_external_imports(
            fs,
            &project_root,
            root_mod,
            &workspace_candidates,
            &authorized_sources,
            project_deps.lock_file(),
        )
        .map_err(|error| {
            let stage = match &error {
                crate::Error::WorkspaceSourceOutsideGraph { importer, .. }
                    if importer == root_mod.module.as_str() =>
                {
                    ProjectDepsStage::ModFile
                }
                _ => ProjectDepsStage::Workspace,
            };
            project_source_error(stage, error)
        })?
    } else {
        Vec::new()
    };
    validate_workspace_candidate_generations(fs, &workspace_candidates).map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::Workspace,
            ProjectDepsErrorKind::ValidationFailed,
            error.to_string(),
        )
    })?;
    let mut workspace_sources = HashMap::new();
    for source_entry in &authorized_sources {
        workspace_sources.insert(
            source_entry.module.as_str().to_string(),
            normalize_fs_path(&source_entry.local_dir),
        );
    }
    let workspace_modules = authorized_sources
        .iter()
        .map(|source_entry| WorkspaceModule {
            module: source_entry.module.clone(),
            directory: normalize_fs_path(&source_entry.local_dir),
            mod_file: source_entry.mod_file().clone(),
        })
        .collect::<Vec<_>>();
    let mut validated_input_files = BTreeSet::new();
    if root_mod.is_some() {
        validated_input_files.insert(normalize_fs_path(&project_root.join("vo.mod")));
    }
    if project_deps.lock_file().is_some() {
        validated_input_files.insert(normalize_fs_path(&project_root.join("vo.lock")));
    }
    if let Some(workspace_file) = workspace_file.as_ref() {
        validated_input_files.insert(normalize_fs_path(workspace_file));
    }
    validated_input_files.extend(
        workspace_candidates
            .iter()
            .map(|candidate| normalize_fs_path(&candidate.local_dir.join("vo.mod"))),
    );
    validated_input_files.extend(validated_source_files);
    let workspace_generation = calculate_workspace_generation(
        &project_root,
        selected_workfile.as_ref(),
        &workspace_candidates,
    );
    let project_metadata_generation =
        calculate_project_metadata_generation(&project_root, root_metadata);
    Ok((
        ProjectContext {
            project_root,
            project_deps,
            authority,
            workspace_modules,
            project_metadata_generation,
            workspace_file,
            workspace_sources,
            workspace_generation,
            validated_input_files: validated_input_files.into_iter().collect(),
        },
        selected_workfile,
        workspace_candidates,
    ))
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

/// Exact source identity used to classify one real or virtual single-file
/// program. Consumers that capture source bytes after classification must
/// compare them with this value before applying the returned module context.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SingleFileSourceGeneration {
    path: PathBuf,
    source_digest: String,
    generation: String,
    authority: SingleFileAuthorityGeneration,
    options: ProjectContextOptions,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SingleFileAuthorityGeneration {
    Project {
        project_root: PathBuf,
        project_metadata: String,
        workspace_file: Option<PathBuf>,
        workspace_generation: String,
        workspace_sources: BTreeMap<String, PathBuf>,
        validated_input_files: BTreeSet<PathBuf>,
    },
    EphemeralInlineMod,
    AdHoc,
}

impl SingleFileSourceGeneration {
    fn from_classification(
        path: &Path,
        source: &str,
        context: &SingleFileContext,
        options: &ProjectContextOptions,
    ) -> Self {
        let path = normalize_fs_path(path);
        let mut source_hasher = StableHasher::new("vo-single-file-source-bytes-v1");
        source_hasher.update_path("path", &path);
        source_hasher.update_bytes("source", source.as_bytes());
        let source_digest = source_hasher.finish();
        let authority = match context {
            SingleFileContext::Project(context) => SingleFileAuthorityGeneration::Project {
                project_root: normalize_fs_path(context.project_root()),
                project_metadata: context.project_metadata_generation().to_string(),
                workspace_file: context.workspace_file().map(normalize_fs_path),
                workspace_generation: context.workspace_generation().to_string(),
                workspace_sources: context
                    .workspace_sources()
                    .iter()
                    .map(|(module, path)| (module.clone(), normalize_fs_path(path)))
                    .collect(),
                validated_input_files: context
                    .validated_input_files()
                    .iter()
                    .map(|path| normalize_fs_path(path))
                    .collect(),
            },
            SingleFileContext::EphemeralInlineMod { .. } => {
                SingleFileAuthorityGeneration::EphemeralInlineMod
            }
            SingleFileContext::AdHoc { .. } => SingleFileAuthorityGeneration::AdHoc,
        };
        let generation =
            single_file_classification_generation_key(&path, &source_digest, &authority, options);
        Self {
            path,
            source_digest,
            generation,
            authority,
            options: options.clone(),
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn generation_key(&self) -> &str {
        &self.generation
    }

    /// Check captured source bytes without reopening the live path.
    pub fn matches_source(&self, path: &Path, source: &str) -> bool {
        let path = normalize_fs_path(path);
        let mut hasher = StableHasher::new("vo-single-file-source-bytes-v1");
        hasher.update_path("path", &path);
        hasher.update_bytes("source", source.as_bytes());
        path == self.path && hasher.finish() == self.source_digest
    }

    /// Re-run source classification with the original workspace policy and
    /// require the complete source/project/workspace authority generation.
    /// Snapshot consumers should also call `matches_source` against the exact
    /// bytes they captured for analysis.
    pub fn validate<F: FileSystem>(&self, fs: &F) -> Result<(), ProjectDepsError> {
        let (_, current) =
            load_single_file_context_with_options_and_generation(fs, &self.path, &self.options)?;
        if &current == self {
            return Ok(());
        }
        Err(single_file_source_generation_changed(&self.path))
    }
}

fn single_file_classification_generation_key(
    path: &Path,
    source_digest: &str,
    authority: &SingleFileAuthorityGeneration,
    options: &ProjectContextOptions,
) -> String {
    let mut hasher = StableHasher::new("vo-single-file-classification-generation-v1");
    hasher.update_path("path", path);
    hasher.update_str("source_digest", source_digest);
    match authority {
        SingleFileAuthorityGeneration::Project {
            project_root,
            project_metadata,
            workspace_file,
            workspace_generation,
            workspace_sources,
            validated_input_files,
        } => {
            hasher.update_str("authority", "project");
            hasher.update_path("project_root", project_root);
            hasher.update_str("project_metadata", project_metadata);
            hasher.update_bool("workspace_file_present", workspace_file.is_some());
            if let Some(workspace_file) = workspace_file {
                hasher.update_path("workspace_file", workspace_file);
            }
            hasher.update_str("workspace_generation", workspace_generation);
            for (module, path) in workspace_sources {
                hasher.update_str("workspace_module", module);
                hasher.update_path("workspace_source", path);
            }
            for input in validated_input_files {
                hasher.update_path("validated_input", input);
            }
        }
        SingleFileAuthorityGeneration::EphemeralInlineMod => {
            hasher.update_str("authority", "ephemeral-inline");
        }
        SingleFileAuthorityGeneration::AdHoc => {
            hasher.update_str("authority", "ad-hoc");
        }
    }
    match &options.workspace {
        WorkspaceDiscovery::Auto => hasher.update_str("workspace_policy", "auto"),
        WorkspaceDiscovery::Disabled => hasher.update_str("workspace_policy", "disabled"),
        WorkspaceDiscovery::Explicit(path) => {
            hasher.update_str("workspace_policy", "explicit");
            hasher.update_path("workspace_policy_path", &normalize_fs_path(path));
        }
    }
    hasher.finish()
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
    load_single_file_context_with_options_and_generation(fs, file_path, options)
        .map(|(context, _)| context)
}

/// Classify one source and return the exact source generation behind that
/// decision. The bounded double read prevents project/inline/ad-hoc policy
/// from being assembled from two different source generations.
pub fn load_single_file_context_with_options_and_generation<F: FileSystem>(
    fs: &F,
    file_path: &Path,
    options: &ProjectContextOptions,
) -> Result<(SingleFileContext, SingleFileSourceGeneration), ProjectDepsError> {
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

    const MAX_SOURCE_SNAPSHOT_ATTEMPTS: usize = 8;
    for _ in 0..MAX_SOURCE_SNAPSHOT_ATTEMPTS {
        let source = read_single_file_source_stable(fs, file_path)?;
        let initial = classify_single_file_context_from_source(
            fs, file_path, &file_name, &file_dir, &source, options,
        );

        #[cfg(test)]
        pause_single_file_context_before_validation_for_test(file_path);

        let final_source = read_single_file_source_stable(fs, file_path)?;
        let current = classify_single_file_context_from_source(
            fs,
            file_path,
            &file_name,
            &file_dir,
            &final_source,
            options,
        );
        match (initial, current) {
            (Ok(initial), Ok(current)) => {
                let initial_generation = SingleFileSourceGeneration::from_classification(
                    file_path, &source, &initial, options,
                );
                let current_generation = SingleFileSourceGeneration::from_classification(
                    file_path,
                    &final_source,
                    &current,
                    options,
                );
                if initial_generation == current_generation {
                    return Ok((current, current_generation));
                }
            }
            (Err(_), Err(current_error)) if source == final_source => {
                return Err(current_error);
            }
            _ => {}
        }
    }

    Err(single_file_source_kept_changing(file_path))
}

fn read_single_file_source_stable<F: FileSystem>(
    fs: &F,
    file_path: &Path,
) -> Result<String, ProjectDepsError> {
    crate::workspace::read_stable_regular_text_file(
        fs,
        file_path,
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
        "single-file source",
    )
    .map_err(|error| {
        ProjectDepsError::new(
            ProjectDepsStage::ModFile,
            ProjectDepsErrorKind::ReadFailed,
            format!("single-file source stable read error: {error}"),
        )
        .with_path(file_path)
    })
}

fn single_file_source_generation_changed(file_path: &Path) -> ProjectDepsError {
    ProjectDepsError::new(
        ProjectDepsStage::ModFile,
        ProjectDepsErrorKind::ValidationFailed,
        "single-file classification generation no longer matches the captured source and project authority",
    )
    .with_path(file_path)
}

fn single_file_source_kept_changing(file_path: &Path) -> ProjectDepsError {
    ProjectDepsError::new(
        ProjectDepsStage::ModFile,
        ProjectDepsErrorKind::ReadFailed,
        "single-file source or project authority changed during every bounded classification attempt",
    )
    .with_path(file_path)
}

fn classify_single_file_context_from_source<F: FileSystem>(
    fs: &F,
    file_path: &Path,
    file_name: &Path,
    file_dir: &Path,
    source: &str,
    options: &ProjectContextOptions,
) -> Result<SingleFileContext, ProjectDepsError> {
    let leading_reserved_span = inline_mod::leading_reserved_block_span(source, 0);

    if let Some(project_root) = try_find_project_root_in(fs, file_dir)? {
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
        inline_mod::parse_inline_mod_from_source_with_span(source, 0).map_err(|error| {
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
                project_root: file_dir.to_path_buf(),
                file_name: file_name.to_path_buf(),
                inline_mod,
            }
        }
        None => SingleFileContext::AdHoc {
            project_root: file_dir.to_path_buf(),
            file_name: file_name.to_path_buf(),
        },
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::digest::Digest;
    use crate::identity::{ModIdentity, ModulePath};
    use crate::schema::lockfile::{LockedDependency, LockedModule};
    use crate::version::{DepConstraint, ExactVersion};
    use vo_common::vfs::MemoryFs;

    struct FailingDiscoveryFs;

    impl FileSystem for FailingDiscoveryFs {
        fn read_file(&self, _path: &Path) -> std::io::Result<String> {
            unreachable!("project-root discovery must not read source files")
        }

        fn read_bytes(&self, _path: &Path) -> std::io::Result<Vec<u8>> {
            unreachable!("project-root discovery must not read source files")
        }

        fn read_dir(&self, _path: &Path) -> std::io::Result<Vec<PathBuf>> {
            unreachable!("project-root discovery must not enumerate directories")
        }

        fn exists(&self, _path: &Path) -> bool {
            false
        }

        fn is_dir(&self, _path: &Path) -> bool {
            false
        }

        fn root(&self) -> Option<&Path> {
            Some(Path::new("."))
        }

        fn resolve_host_path(&self, _path: &Path) -> std::io::Result<Option<PathBuf>> {
            Err(std::io::Error::new(
                std::io::ErrorKind::PermissionDenied,
                "project discovery denied",
            ))
        }
    }

    fn locked_module(path: &str, dependencies: &[(&str, &str)]) -> LockedModule {
        LockedModule {
            path: ModulePath::parse(path).unwrap(),
            version: ExactVersion::parse("0.1.0").unwrap(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            release: Digest::from_sha256(path.as_bytes()),
            dependencies: dependencies
                .iter()
                .map(|(module, constraint)| LockedDependency {
                    module: ModulePath::parse(module).unwrap(),
                    constraint: DepConstraint::parse(constraint).unwrap(),
                })
                .collect(),
        }
    }

    fn rendered_lock(root: &str, mut modules: Vec<LockedModule>) -> String {
        modules.sort_by(|left, right| left.path.cmp(&right.path));
        LockFile {
            version: LOCK_FILE_VERSION,
            root: LockRoot {
                module: ModIdentity::parse(root).unwrap(),
                vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            },
            modules,
        }
        .render()
        .unwrap()
    }

    fn module_with_lock(identity: &str) -> (ModFile, LockFile) {
        let mod_file = ModFile::parse(&format!(
            "module = {identity:?}\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/lib\" = \"^0.1.0\"\n"
        ))
        .unwrap();
        let lock_file = build_lock_file_from_mod_file(
            &mod_file,
            vec![locked_module("github.com/acme/lib", &[])],
        )
        .unwrap();
        (mod_file, lock_file)
    }

    fn commit_project_files(
        project_dir: &Path,
        mod_file: &ModFile,
        lock_file: Option<&LockFile>,
    ) -> Result<(), Error> {
        let guard = lock_project_mutation(project_dir)?;
        write_project_files(project_dir, &guard, mod_file, lock_file)
    }

    fn lock_file_for_workspace_source() -> String {
        rendered_lock(
            "github.com/acme/app",
            vec![
                locked_module("github.com/vo-lang/core", &[]),
                locked_module(
                    "github.com/vo-lang/voplay",
                    &[("github.com/vo-lang/core", "^0.1.0")],
                ),
            ],
        )
    }

    fn root_project_fs() -> MemoryFs {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/vo-lang/voplay\" = \"^0.1.0\"\n",
        );
        fs
    }

    #[test]
    fn project_root_discovery_propagates_start_normalization_failures() {
        let error = try_find_project_root_in(&FailingDiscoveryFs, Path::new("../outside/project"))
            .expect_err("discovery-start failures must be reported");
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ReadFailed);
        assert!(error.detail.contains("project discovery denied"));
    }

    #[test]
    fn project_root_discovery_rejects_child_manifest_directory_masking_parent() {
        let fs = MemoryFs::new()
            .with_file(
                "repo/vo.mod",
                "module = \"github.com/acme/root\"\nvo = \"^0.1.0\"\n",
            )
            .with_dir("repo/child/vo.mod");

        let error = try_find_project_root_in(&fs, Path::new("repo/child"))
            .expect_err("a child vo.mod directory must block parent discovery");
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert_eq!(error.path.as_deref(), Some("repo/child/vo.mod"));
    }

    #[test]
    fn project_root_and_metadata_discovery_reject_portable_protocol_aliases() {
        let fs = MemoryFs::new()
            .with_file(
                "repo/vo.mod",
                "module = \"github.com/acme/root\"\nvo = \"^0.1.0\"\n",
            )
            .with_file("repo/child/VO.MOD", "alias")
            .with_dir("repo/child/grandchild");
        let error = try_find_project_root_in(&fs, Path::new("repo/child/grandchild"))
            .expect_err("a child portable alias must block parent discovery");
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert_eq!(error.path.as_deref(), Some("repo/child/VO.MOD"));
        assert!(error.detail.contains("portable alias"), "{error}");

        let fs = MemoryFs::new()
            .with_file(
                "vo.mod",
                "module = \"github.com/acme/root\"\nvo = \"^0.1.0\"\n",
            )
            .with_file("VO.LOCK", "alias");
        let error = read_project_deps(&fs).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::LockFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert_eq!(error.path.as_deref(), Some("VO.LOCK"));
        assert!(error.detail.contains("portable alias"), "{error}");
    }

    #[test]
    fn public_project_root_discovery_surfaces_invalid_manifest_entries() {
        let temp = tempfile::tempdir().unwrap();
        std::fs::write(
            temp.path().join("vo.mod"),
            "module = \"github.com/acme/root\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();
        let child = temp.path().join("child");
        std::fs::create_dir_all(child.join("vo.mod")).unwrap();

        let error = find_project_root(&child)
            .expect_err("the public API must propagate an invalid child manifest entry");
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        let invalid_manifest = child.join("vo.mod");
        assert_eq!(error.path.as_deref(), invalid_manifest.to_str());
    }

    #[test]
    fn read_project_deps_reports_missing_lock_when_external_modules_present() {
        let fs = root_project_fs();
        let error = read_project_deps(&fs).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::LockFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::Missing);
        assert_eq!(error.path.as_deref(), Some("vo.lock"));
        assert!(error
            .detail
            .contains("vo.lock is required whenever vo.mod declares external dependencies"));
    }

    #[test]
    fn read_project_deps_requires_lock_for_multiple_direct_dependencies() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/vo-lang/vogui\" = \"^0.1.0\"\n\"github.com/vo-lang/voplay\" = \"^0.1.0\"\n",
        );

        let error = read_project_deps(&fs)
            .expect_err("every non-empty external graph must have a frozen lock");
        assert_eq!(error.stage, ProjectDepsStage::LockFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::Missing);
        assert_eq!(error.path.as_deref(), Some("vo.lock"));
    }

    #[test]
    fn read_project_deps_returns_the_complete_locked_graph() {
        let mut fs = root_project_fs();
        fs.add_file("vo.lock", lock_file_for_workspace_source());
        let deps =
            read_project_deps(&fs).unwrap_or_else(|error| panic!("unexpected error: {error}"));
        assert_eq!(
            deps.allowed_modules(),
            &[
                "github.com/vo-lang/core".to_string(),
                "github.com/vo-lang/voplay".to_string(),
            ]
        );
        assert_eq!(deps.locked_modules().len(), 2);
        assert_eq!(deps.lock_file().unwrap().modules.len(), 2);
    }

    #[test]
    fn build_lock_file_requires_complete_root_graph() {
        let lockless =
            ModFile::parse("module = \"github.com/acme/lockless\"\nvo = \"^0.1.0\"\n").unwrap();
        let lockless_error = build_lock_file_from_mod_file(&lockless, Vec::new()).unwrap_err();
        assert!(lockless_error.to_string().contains("undefined"));

        let mod_file = ModFile::parse(
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/vo-lang/voplay\" = \"^0.1.0\"\n",
        )
        .unwrap();
        let error = build_lock_file_from_mod_file(&mod_file, Vec::new()).unwrap_err();
        assert!(error.to_string().contains("github.com/vo-lang/voplay"));

        let valid = LockFile::parse(&lock_file_for_workspace_source()).unwrap();
        let lock_file = build_lock_file_from_mod_file(&mod_file, valid.modules).unwrap();
        assert_eq!(lock_file.modules.len(), 2);
    }

    #[test]
    fn build_lock_file_rejects_orphans_and_duplicate_selections() {
        let valid = LockFile::parse(&lock_file_for_workspace_source()).unwrap();
        let rooted = ModFile::parse(
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/vo-lang/voplay\" = \"^0.1.0\"\n",
        )
        .unwrap();
        let mut with_orphan = valid.modules.clone();
        with_orphan.push(locked_module("github.com/acme/orphan", &[]));
        let orphan_error = build_lock_file_from_mod_file(&rooted, with_orphan).unwrap_err();
        assert!(orphan_error.to_string().contains("orphaned"));

        let mut duplicate = valid.modules;
        duplicate.push(duplicate[0].clone());
        let duplicate_error = build_lock_file_from_mod_file(&rooted, duplicate).unwrap_err();
        assert!(duplicate_error
            .to_string()
            .contains("[[module]] entries must be unique and sorted by path"));
    }

    #[test]
    fn read_project_deps_rejects_a_nonempty_lock_for_the_lockless_graph() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "vo.lock",
            rendered_lock(
                "github.com/acme/app",
                vec![locked_module("github.com/acme/lib", &[])],
            ),
        );

        let error = read_project_deps(&fs).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::LockFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert_eq!(error.path.as_deref(), Some("vo.lock"));
        assert!(error.detail.contains("must be absent"), "{}", error.detail);
    }

    #[test]
    fn read_project_deps_rejects_an_empty_lock_for_the_lockless_graph() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "vo.lock",
            "version = 3\n\n[root]\nmodule = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
        );

        let error = read_project_deps(&fs).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::LockFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert_eq!(error.path.as_deref(), Some("vo.lock"));
        assert!(error.detail.contains("must be absent"), "{}", error.detail);
    }

    #[test]
    fn project_context_without_nested_mod_uses_root_metadata_pair() {
        let mut fs = root_project_fs();
        fs.add_file("vo.lock", lock_file_for_workspace_source());

        let context = load_project_context_with_options(
            &fs,
            Path::new("workspace/app"),
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap_or_else(|error| panic!("unexpected error: {error}"));
        let deps = context.project_deps();

        assert_eq!(deps.current_module(), Some("github.com/acme/app"));
        assert_eq!(deps.lock_file().unwrap().modules.len(), 2);
        assert_eq!(deps.locked_modules().len(), 2);
    }

    #[test]
    fn project_context_classifies_oversized_source_as_validation_failure() {
        let project = tempfile::tempdir().unwrap();
        std::fs::write(
            project.path().join("vo.mod"),
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();
        std::fs::File::create(project.path().join("main.vo"))
            .unwrap()
            .set_len(u64::try_from(vo_common::vfs::MAX_TEXT_FILE_BYTES).unwrap() + 1)
            .unwrap();

        let error = load_project_context_with_options(
            &RealFs::new("."),
            project.path(),
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap_err();

        assert_eq!(error.stage, ProjectDepsStage::Workspace);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert!(error.detail.contains("size limit"), "{error}");
    }

    #[test]
    fn project_context_classifies_invalid_source_utf8_as_validation_failure() {
        let project = tempfile::tempdir().unwrap();
        std::fs::write(
            project.path().join("vo.mod"),
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(project.path().join("main.vo"), [0xff]).unwrap();

        let error = load_project_context_with_options(
            &RealFs::new("."),
            project.path(),
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap_err();

        assert_eq!(error.stage, ProjectDepsStage::Workspace);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert!(error.detail.contains("UTF-8"), "{error}");
    }

    #[test]
    fn project_context_keeps_workspace_syntax_errors_as_parse_failures() {
        let project = tempfile::tempdir().unwrap();
        std::fs::write(
            project.path().join("vo.mod"),
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(project.path().join("vo.work"), "version = [\n").unwrap();

        let error = load_project_context_with_options(
            &RealFs::new("."),
            project.path(),
            &ProjectContextOptions::new(WorkspaceDiscovery::Auto),
        )
        .unwrap_err();

        assert_eq!(error.stage, ProjectDepsStage::Workspace);
        assert_eq!(error.kind, ProjectDepsErrorKind::ParseFailed);
    }

    #[test]
    fn project_context_classifies_active_root_identity_ambiguity_as_validation_failure() {
        let mut fs = vo_common::vfs::MemoryFs::new();
        fs.add_file(
            "workspace/vo.work",
            "version = 1\nmembers = [\"app-copy\"]\n",
        );
        let manifest = "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n";
        fs.add_file("workspace/app/vo.mod", manifest);
        fs.add_file("workspace/app-copy/vo.mod", manifest);

        let error = load_project_context_with_options(
            &fs,
            Path::new("workspace/app"),
            &ProjectContextOptions::new(WorkspaceDiscovery::Auto),
        )
        .unwrap_err();

        assert_eq!(error.stage, ProjectDepsStage::Workspace);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert!(error.detail.contains("active root identity"), "{error}");
        assert!(error.detail.contains("app-copy"), "{error}");
    }

    #[test]
    fn absolute_directory_without_a_project_cannot_absorb_relative_root_metadata() {
        let mut fs = MemoryFs::new();
        let (relative_mod, relative_lock) = module_with_lock("github.com/acme/relative-root");
        fs.add_file("vo.mod", relative_mod.render().unwrap());
        fs.add_file("vo.lock", relative_lock.render().unwrap());
        let detached = PathBuf::from("/detached/no-project");

        let context = load_project_context_with_options(
            &fs,
            &detached,
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap_or_else(|error| panic!("unexpected error: {error}"));

        assert_eq!(context.project_root(), detached);
        assert!(!context.project_deps().has_mod_file());
        assert!(context.validated_input_files().is_empty());
    }

    #[test]
    fn parent_relative_directory_cannot_absorb_virtual_root_metadata() {
        let mut fs = MemoryFs::new();
        let (relative_mod, relative_lock) = module_with_lock("github.com/acme/relative-root");
        fs.add_file("vo.mod", relative_mod.render().unwrap());
        fs.add_file("vo.lock", relative_lock.render().unwrap());
        let detached = PathBuf::from("../detached/no-project");

        let context = load_project_context_with_options(
            &fs,
            &detached,
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap_or_else(|error| panic!("unexpected error: {error}"));

        assert_eq!(context.project_root(), detached);
        assert!(!context.project_deps().has_mod_file());
        assert!(context.validated_input_files().is_empty());
    }

    #[test]
    fn workspace_generation_distinguishes_no_workspace_from_an_empty_selection() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "project/vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file("project/vo.work", "version = 1\nmembers = []\n");

        let disabled = load_project_context_with_options(
            &fs,
            Path::new("project"),
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap();
        let disabled_again = load_project_context_with_options(
            &fs,
            Path::new("project"),
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap();
        let selected = load_project_context_with_options(
            &fs,
            Path::new("project"),
            &ProjectContextOptions::new(WorkspaceDiscovery::Auto),
        )
        .unwrap();

        assert_eq!(
            disabled.workspace_generation(),
            disabled_again.workspace_generation()
        );
        assert_ne!(
            disabled.workspace_generation(),
            selected.workspace_generation()
        );
        assert!(disabled.has_same_root_authority(&selected));
    }

    #[test]
    fn root_authority_survives_two_pass_workspace_source_materialization() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/app/vo.mod",
            concat!(
                "module = \"github.com/acme/app\"\n",
                "vo = \"^0.1.0\"\n\n",
                "[dependencies]\n",
                "\"github.com/acme/lib\" = \"0.1.0\"\n",
            ),
        );
        fs.add_file(
            "workspace/app/main.vo",
            "package main\nimport \"github.com/acme/lib\"\n",
        );
        fs.add_file(
            "workspace/app/vo.work",
            "version = 1\nmembers = [\"../lib\"]\n",
        );
        fs.add_file(
            "workspace/lib/vo.mod",
            "module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n",
        );

        let metadata_only = load_project_context(&fs, Path::new("workspace/app")).unwrap();
        assert_eq!(metadata_only.authority(), ProjectAuthority::Workspace);
        assert_eq!(metadata_only.workspace_sources().len(), 1);

        fs.add_file("workspace/lib/lib.vo", "package lib\n");
        let materialized = load_project_context(&fs, Path::new("workspace/app")).unwrap();

        assert!(metadata_only.has_same_root_authority(&materialized));
        assert_eq!(
            metadata_only.workspace_sources(),
            materialized.workspace_sources()
        );
        assert_ne!(
            metadata_only.validated_input_files(),
            materialized.validated_input_files()
        );
    }

    #[test]
    fn root_authority_detects_exact_manifest_and_lock_drift() {
        let project_root = Path::new("workspace/app");
        let canonical_mod = "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n";
        let canonical_fs = MemoryFs::new().with_file(project_root.join("vo.mod"), canonical_mod);
        let canonical = load_project_context_with_options(
            &canonical_fs,
            project_root,
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap();
        let equivalent_fs =
            MemoryFs::new().with_file(project_root.join("vo.mod"), format!("{canonical_mod}\n"));
        let equivalent = load_project_context_with_options(
            &equivalent_fs,
            project_root,
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap();
        assert_eq!(
            canonical.project_deps().mod_file(),
            equivalent.project_deps().mod_file()
        );
        assert!(!canonical.has_same_root_authority(&equivalent));

        let (mod_file, lock_file) = module_with_lock("github.com/acme/locked-app");
        let locked_root = Path::new("workspace/locked");
        let original_fs = MemoryFs::new().with_files([
            (locked_root.join("vo.mod"), mod_file.render().unwrap()),
            (locked_root.join("vo.lock"), lock_file.render().unwrap()),
        ]);
        let original = load_project_context_with_options(
            &original_fs,
            locked_root,
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap();

        let mut changed_lock = lock_file;
        changed_lock.modules[0].release = Digest::from_sha256(b"changed-release");
        let changed_fs = MemoryFs::new().with_files([
            (locked_root.join("vo.mod"), mod_file.render().unwrap()),
            (locked_root.join("vo.lock"), changed_lock.render().unwrap()),
        ]);
        let changed = load_project_context_with_options(
            &changed_fs,
            locked_root,
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap();
        assert!(!original.has_same_root_authority(&changed));
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    #[test]
    fn workspace_generation_changes_when_a_member_directory_is_rebound_in_place() {
        let temp = tempfile::tempdir().unwrap();
        let root = std::fs::canonicalize(temp.path()).unwrap();
        let app = root.join("app");
        let member = root.join("lib");
        std::fs::create_dir_all(&app).unwrap();
        std::fs::create_dir_all(&member).unwrap();
        std::fs::write(
            app.join("vo.mod"),
            concat!(
                "module = \"github.com/acme/app\"\n",
                "vo = \"^0.1.0\"\n\n",
                "[dependencies]\n",
                "\"github.com/acme/lib\" = \"0.1.0\"\n",
            ),
        )
        .unwrap();
        std::fs::write(
            app.join("vo.lock"),
            rendered_lock(
                "github.com/acme/app",
                vec![locked_module("github.com/acme/lib", &[])],
            ),
        )
        .unwrap();
        std::fs::write(app.join("main.vo"), "package main\nfunc main() {}\n").unwrap();
        let member_mod = "module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n";
        let member_source = "package lib\nfunc Value() int { return 1 }\n";
        std::fs::write(member.join("vo.mod"), member_mod).unwrap();
        std::fs::write(member.join("lib.vo"), member_source).unwrap();
        std::fs::write(
            root.join("vo.work"),
            "version = 1\nmembers = [\"app\", \"lib\"]\n",
        )
        .unwrap();

        let fs = RealFs::new(".");
        let first = load_project_context(&fs, &app).unwrap();
        let detached = root.join("detached-lib");
        std::fs::rename(&member, &detached).unwrap();
        std::fs::create_dir(&member).unwrap();
        std::fs::write(member.join("vo.mod"), member_mod).unwrap();
        std::fs::write(member.join("lib.vo"), member_source).unwrap();
        let rebound = load_project_context(&fs, &app).unwrap();

        assert_eq!(first.workspace_sources(), rebound.workspace_sources());
        assert_ne!(first.workspace_generation(), rebound.workspace_generation());
    }

    #[test]
    fn read_inline_project_deps_matches_root_file_parsing() {
        let mut fs = root_project_fs();
        fs.add_file("vo.lock", lock_file_for_workspace_source());
        let mod_content = fs.read_file(Path::new("vo.mod")).unwrap();
        let lock_content = fs.read_file(Path::new("vo.lock")).unwrap();

        let deps = read_inline_project_deps(&mod_content, Some(&lock_content)).unwrap();

        assert_eq!(
            deps.allowed_modules(),
            &[
                "github.com/vo-lang/core".to_string(),
                "github.com/vo-lang/voplay".to_string(),
            ]
        );
        assert_eq!(deps.locked_modules().len(), 2);
    }

    #[test]
    fn inline_project_and_ephemeral_entry_points_have_disjoint_identity_domains() {
        let dependency = locked_module("github.com/vo-lang/core", &[]);
        let project_mod = concat!(
            "module = \"github.com/acme/app\"\n",
            "vo = \"^0.1.0\"\n\n",
            "[dependencies]\n",
            "\"github.com/vo-lang/core\" = \"0.1.0\"\n",
        );
        let ephemeral_mod = concat!("module = \"local/script\"\n", "vo = \"^0.1.0\"\n",);
        let project_lock = rendered_lock("github.com/acme/app", vec![dependency]);

        assert!(read_inline_project_deps(project_mod, Some(&project_lock)).is_ok());
        assert!(read_inline_ephemeral_project_deps(ephemeral_mod, None).is_ok());

        let project_error = read_inline_project_deps(ephemeral_mod, None).unwrap_err();
        assert_eq!(project_error.stage, ProjectDepsStage::ModFile);
        assert_eq!(project_error.kind, ProjectDepsErrorKind::ParseFailed);

        let ephemeral_error =
            read_inline_ephemeral_project_deps(project_mod, Some(&project_lock)).unwrap_err();
        assert_eq!(ephemeral_error.stage, ProjectDepsStage::ModFile);
        assert_eq!(ephemeral_error.kind, ProjectDepsErrorKind::ParseFailed);
    }

    #[test]
    fn load_project_context_uses_workspace_authority_for_a_closed_local_graph() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/lib/vo.mod",
            "module = \"github.com/vo-lang/lib\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "workspace/tests/vo.mod",
            "module = \"github.com/vo-lang/lib/tests\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/vo-lang/lib\" = \"0.1.0\"\n",
        );
        fs.add_file(
            "workspace/tests/vo.work",
            "version = 1\nmembers = [\"../lib\", \".\"]\n",
        );

        let context = load_project_context(&fs, Path::new("workspace/tests")).unwrap();
        assert_eq!(context.authority(), ProjectAuthority::Workspace);
        assert!(context.project_deps().lock_file().is_none());
        assert!(context.project_deps().locked_modules().is_empty());
        assert_eq!(context.workspace_modules().len(), 1);
        assert_eq!(
            context.workspace_modules()[0].module().as_str(),
            "github.com/vo-lang/lib"
        );
    }

    #[test]
    fn load_project_context_with_disabled_workspace_requires_locked_external_modules() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/lib/vo.mod",
            "module = \"github.com/vo-lang/lib\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "workspace/tests/vo.mod",
            "module = \"github.com/vo-lang/lib/tests\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/vo-lang/lib\" = \"0.1.0\"\n",
        );
        fs.add_file(
            "workspace/tests/vo.work",
            "version = 1\nmembers = [\"../lib\", \".\"]\n",
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
    fn workspace_authority_requires_every_reachable_owner_to_be_local() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/app/vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/a\" = \"^1.0.0\"\n",
        );
        fs.add_file("workspace/app/vo.work", "version = 1\nmembers = []\n");

        let error = load_project_context(&fs, Path::new("workspace/app")).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::Workspace);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert!(error.detail.contains("github.com/acme/a"), "{error}");
        assert!(error.detail.contains("complete v3 vo.lock"), "{error}");
    }

    #[test]
    fn workspace_authority_rejects_a_transitive_registry_edge() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/app/vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/a\" = \"^1.0.0\"\n",
        );
        fs.add_file(
            "workspace/a/vo.mod",
            "module = \"github.com/acme/a\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/registry\" = \"^1.0.0\"\n",
        );
        fs.add_file(
            "workspace/app/vo.work",
            "version = 1\nmembers = [\"../a\"]\n",
        );

        let error = load_project_context(&fs, Path::new("workspace/app")).unwrap_err();
        assert!(error.detail.contains("github.com/acme/registry"), "{error}");
        assert!(error.detail.contains("complete v3 vo.lock"), "{error}");
    }

    #[test]
    fn workspace_authority_requires_member_toolchain_coverage() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/app/vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/a\" = \"^1.0.0\"\n",
        );
        fs.add_file(
            "workspace/a/vo.mod",
            "module = \"github.com/acme/a\"\nvo = \"^0.2.0\"\n",
        );
        fs.add_file(
            "workspace/app/vo.work",
            "version = 1\nmembers = [\"../a\"]\n",
        );

        let error = load_project_context(&fs, Path::new("workspace/app")).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::Workspace);
        assert!(error.detail.contains("toolchain"), "{error}");
        assert!(error.detail.contains("github.com/acme/a"), "{error}");
    }

    #[test]
    fn workspace_authority_allows_member_cycles_and_rejects_root_back_edges() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/app/vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/a\" = \"^1.0.0\"\n",
        );
        fs.add_file(
            "workspace/a/vo.mod",
            "module = \"github.com/acme/a\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/b\" = \"^1.0.0\"\n",
        );
        fs.add_file(
            "workspace/b/vo.mod",
            "module = \"github.com/acme/b\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/a\" = \"^1.0.0\"\n",
        );
        fs.add_file(
            "workspace/app/vo.work",
            "version = 1\nmembers = [\"../a\", \"../b\"]\n",
        );

        let context = load_project_context(&fs, Path::new("workspace/app")).unwrap();
        assert_eq!(context.authority(), ProjectAuthority::Workspace);
        assert_eq!(context.workspace_modules().len(), 2);

        fs.add_file(
            "workspace/b/vo.mod",
            "module = \"github.com/acme/b\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/app\" = \"^1.0.0\"\n",
        );
        let error = load_project_context(&fs, Path::new("workspace/app")).unwrap_err();
        assert!(
            error.detail.contains("must not depend on root module"),
            "{error}"
        );
    }

    #[test]
    fn workspace_authority_requires_one_common_incoming_version() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/app/vo.mod",
            concat!(
                "module = \"github.com/acme/app\"\n",
                "vo = \"^0.1.0\"\n\n",
                "[dependencies]\n",
                "\"github.com/acme/a\" = \"^1.0.0\"\n",
                "\"github.com/acme/b\" = \"^1.0.0\"\n",
            ),
        );
        for (member, constraint) in [("a", "~1.0.0"), ("b", "~1.1.0")] {
            fs.add_file(
                format!("workspace/{member}/vo.mod"),
                format!(
                    "module = \"github.com/acme/{member}\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/c\" = \"{constraint}\"\n"
                ),
            );
        }
        fs.add_file(
            "workspace/c/vo.mod",
            "module = \"github.com/acme/c\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "workspace/app/vo.work",
            "version = 1\nmembers = [\"../a\", \"../b\", \"../c\"]\n",
        );

        let error = load_project_context(&fs, Path::new("workspace/app")).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::Workspace);
        assert!(error.detail.contains("github.com/acme/c"), "{error}");
        assert!(error.detail.contains("conflicting constraints"), "{error}");
    }

    #[test]
    fn incoming_constraint_diagnostic_is_deterministic_and_bounded_at_maximum_fan_in() {
        let constraint = crate::version::DepConstraint::parse("^1.0.0").unwrap();
        let constraints = (0..crate::MAX_SOLVER_GRAPH_EDGES)
            .rev()
            .map(|index| {
                (
                    format!("github.com/acme/importer-{index:06}"),
                    constraint.clone(),
                )
            })
            .collect::<Vec<_>>();

        let detail = summarize_incoming_constraints(&constraints);
        let mut reversed = constraints.clone();
        reversed.reverse();

        assert_eq!(detail, summarize_incoming_constraints(&reversed));
        assert!(detail.len() < 1_024, "diagnostic is {} bytes", detail.len());
        assert!(detail.contains("github.com/acme/importer-000000 requires ^1.0.0"));
        assert!(detail.contains("github.com/acme/importer-000007 requires ^1.0.0"));
        assert!(!detail.contains("github.com/acme/importer-000008 requires ^1.0.0"));
        assert!(detail.contains("and 99992 more incoming requirement(s)"));
    }

    #[test]
    fn workspace_authority_ignores_unreachable_member_edges() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/app/vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/a\" = \"^1.0.0\"\n",
        );
        fs.add_file(
            "workspace/a/vo.mod",
            "module = \"github.com/acme/a\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "workspace/orphan/vo.mod",
            "module = \"github.com/acme/orphan\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/registry\" = \"^1.0.0\"\n",
        );
        fs.add_file(
            "workspace/app/vo.work",
            "version = 1\nmembers = [\"../a\", \"../orphan\"]\n",
        );

        let context = load_project_context(&fs, Path::new("workspace/app")).unwrap();
        assert_eq!(context.authority(), ProjectAuthority::Workspace);
        assert_eq!(context.workspace_modules().len(), 1);
        assert_eq!(
            context.workspace_modules()[0].module().as_str(),
            "github.com/acme/a"
        );
        assert!(!context
            .workspace_sources()
            .contains_key("github.com/acme/orphan"));
    }

    #[test]
    fn nested_workspace_members_use_the_nearest_manifest_boundary() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/app/vo.mod",
            concat!(
                "module = \"github.com/acme/app\"\n",
                "vo = \"^0.1.0\"\n\n",
                "[dependencies]\n",
                "\"github.com/acme/child\" = \"^1.0.0\"\n",
                "\"github.com/acme/parent\" = \"^1.0.0\"\n",
            ),
        );
        fs.add_file(
            "workspace/parent/vo.mod",
            "module = \"github.com/acme/parent\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file("workspace/parent/parent.vo", "package parent\n");
        fs.add_file(
            "workspace/parent/child/vo.mod",
            "module = \"github.com/acme/child\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/util\" = \"^1.0.0\"\n",
        );
        fs.add_file(
            "workspace/parent/child/child.vo",
            "package child\nimport \"github.com/acme/util\"\n",
        );
        fs.add_file(
            "workspace/util/vo.mod",
            "module = \"github.com/acme/util\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "workspace/app/vo.work",
            "version = 1\nmembers = [\"../parent\", \"../parent/child\", \"../util\"]\n",
        );

        let context = load_project_context(&fs, Path::new("workspace/app")).unwrap();
        assert_eq!(context.authority(), ProjectAuthority::Workspace);
        assert_eq!(context.workspace_modules().len(), 3);
    }

    #[test]
    fn workspace_source_rejects_an_import_absent_from_its_locked_edges() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/lib/vo.mod",
            "module = \"github.com/vo-lang/lib\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/vo-lang/core\" = \"0.1.0\"\n",
        );
        fs.add_file(
            "workspace/lib/lib.vo",
            "package lib\nimport \"github.com/vo-lang/core\"\nfunc Hello(){core.Hello()}\n",
        );
        fs.add_file(
            "workspace/tests/vo.mod",
            "module = \"github.com/vo-lang/lib/tests\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/vo-lang/lib\" = \"0.1.0\"\n",
        );
        fs.add_file(
            "workspace/tests/vo.work",
            "version = 1\nmembers = [\"../lib\", \".\"]\n",
        );
        fs.add_file(
            "workspace/tests/vo.lock",
            rendered_lock(
                "github.com/vo-lang/lib/tests",
                vec![locked_module("github.com/vo-lang/lib", &[])],
            ),
        );

        let error = match load_project_context(&fs, Path::new("workspace/tests")) {
            Ok(_) => panic!("expected importer-specific edge validation error"),
            Err(error) => error,
        };
        assert_eq!(error.stage, ProjectDepsStage::Workspace);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert!(
            error.detail.contains(
                "workspace source github.com/vo-lang/lib imports github.com/vo-lang/core"
            ),
            "{}",
            error.detail
        );
    }

    #[test]
    fn load_project_context_exposes_vo_work_sources_to_the_resolver() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/lib/vo.mod",
            "module = \"github.com/vo-lang/lib\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file("workspace/lib/lib.vo", "package lib\n");
        fs.add_file(
            "workspace/tests/vo.mod",
            "module = \"github.com/vo-lang/lib/tests\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/vo-lang/core\" = \"0.1.0\"\n\"github.com/vo-lang/lib\" = \"0.1.0\"\n",
        );
        fs.add_file("workspace/tests/main.vo", "package tests\n");
        fs.add_file(
            "workspace/tests/vo.work",
            "version = 1\nmembers = [\"../lib\", \".\"]\n",
        );
        fs.add_file(
            "workspace/tests/vo.lock",
            rendered_lock(
                "github.com/vo-lang/lib/tests",
                vec![
                    locked_module("github.com/vo-lang/core", &[]),
                    locked_module("github.com/vo-lang/lib", &[]),
                ],
            ),
        );

        let context = load_project_context(&fs, Path::new("workspace/tests"))
            .unwrap_or_else(|error| panic!("unexpected error: {error}"));
        assert_eq!(context.authority(), ProjectAuthority::Lock);
        assert_eq!(context.project_root(), Path::new("workspace/tests"));
        assert_eq!(
            context.workspace_sources().get("github.com/vo-lang/lib"),
            Some(&PathBuf::from("workspace/lib"))
        );
        assert_eq!(context.project_deps().locked_modules().len(), 1);
        assert_eq!(
            context.project_deps().locked_modules()[0].path.as_str(),
            "github.com/vo-lang/core"
        );
        assert_eq!(
            context
                .validated_input_files()
                .iter()
                .cloned()
                .collect::<BTreeSet<_>>(),
            [
                PathBuf::from("workspace/lib/lib.vo"),
                PathBuf::from("workspace/lib/vo.mod"),
                PathBuf::from("workspace/tests/main.vo"),
                PathBuf::from("workspace/tests/vo.lock"),
                PathBuf::from("workspace/tests/vo.mod"),
                PathBuf::from("workspace/tests/vo.work"),
            ]
            .into_iter()
            .collect()
        );
    }

    #[test]
    fn root_source_cannot_import_an_undeclared_workspace_member() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/lib/vo.mod",
            "module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "workspace/app/vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "workspace/app/main.vo",
            "package main\nimport \"github.com/acme/lib\"\nfunc main() {}\n",
        );
        fs.add_file(
            "workspace/app/vo.work",
            "version = 1\nmembers = [\"../lib\", \".\"]\n",
        );

        let error = load_project_context(&fs, Path::new("workspace/app")).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert!(error
            .detail
            .contains("github.com/acme/app imports github.com/acme/lib"));
    }

    #[test]
    fn root_source_cannot_borrow_a_transitive_locked_dependency() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/a\" = \"0.1.0\"\n",
        );
        fs.add_file(
            "vo.lock",
            rendered_lock(
                "github.com/acme/app",
                vec![
                    locked_module("github.com/acme/a", &[("github.com/acme/b", "0.1.0")]),
                    locked_module("github.com/acme/b", &[]),
                ],
            ),
        );
        fs.add_file(
            "main.vo",
            "package main\nimport \"github.com/acme/b\"\nfunc main() {}\n",
        );

        let error = load_project_context(&fs, Path::new(".")).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert!(error
            .detail
            .contains("github.com/acme/app imports github.com/acme/b"));
    }

    #[test]
    fn lock_external_workspace_members_are_ignored_and_not_scanned() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/lib/vo.mod",
            "module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "workspace/unrelated/vo.mod",
            "module = \"github.com/acme/unrelated\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file("workspace/unrelated/broken.vo", "this is not Vo source");
        fs.add_file(
            "workspace/app/vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/lib\" = \"0.1.0\"\n",
        );
        fs.add_file(
            "workspace/app/vo.work",
            "version = 1\nmembers = [\"../lib\", \"../unrelated\", \".\"]\n",
        );
        fs.add_file(
            "workspace/app/vo.lock",
            rendered_lock(
                "github.com/acme/app",
                vec![locked_module("github.com/acme/lib", &[])],
            ),
        );

        let context = load_project_context(&fs, Path::new("workspace/app")).unwrap();
        assert!(context
            .workspace_sources()
            .contains_key("github.com/acme/lib"));
        assert!(!context
            .workspace_sources()
            .contains_key("github.com/acme/unrelated"));
    }

    #[test]
    fn workspace_member_cannot_borrow_an_unrelated_locked_branch() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/a/vo.mod",
            "module = \"github.com/acme/a\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "workspace/a/a.vo",
            "package a\nimport \"github.com/acme/b\"\nfunc A() {}\n",
        );
        fs.add_file(
            "workspace/b/vo.mod",
            "module = \"github.com/acme/b\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "workspace/app/vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/a\" = \"0.1.0\"\n\"github.com/acme/b\" = \"0.1.0\"\n",
        );
        fs.add_file(
            "workspace/app/vo.work",
            "version = 1\nmembers = [\"../a\", \"../b\", \".\"]\n",
        );
        fs.add_file(
            "workspace/app/vo.lock",
            rendered_lock(
                "github.com/acme/app",
                vec![
                    locked_module("github.com/acme/a", &[]),
                    locked_module("github.com/acme/b", &[]),
                ],
            ),
        );

        let error = load_project_context(&fs, Path::new("workspace/app")).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::Workspace);
        assert!(error
            .detail
            .contains("workspace source github.com/acme/a imports github.com/acme/b"));
    }

    #[test]
    fn workspace_member_can_import_its_exact_locked_dependency_edge() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "workspace/a/vo.mod",
            "module = \"github.com/acme/a\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "workspace/a/a.vo",
            "package a\nimport \"github.com/acme/b\"\nfunc A() {}\n",
        );
        fs.add_file(
            "workspace/b/vo.mod",
            "module = \"github.com/acme/b\"\nvo = \"^0.1.0\"\n",
        );
        fs.add_file(
            "workspace/app/vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/a\" = \"0.1.0\"\n",
        );
        fs.add_file(
            "workspace/app/vo.work",
            "version = 1\nmembers = [\"../a\", \"../b\", \".\"]\n",
        );
        fs.add_file(
            "workspace/app/vo.lock",
            rendered_lock(
                "github.com/acme/app",
                vec![
                    locked_module("github.com/acme/a", &[("github.com/acme/b", "0.1.0")]),
                    locked_module("github.com/acme/b", &[]),
                ],
            ),
        );

        let context = load_project_context(&fs, Path::new("workspace/app")).unwrap();
        assert_eq!(context.workspace_sources().len(), 2);
        assert!(context.project_deps().locked_modules().is_empty());
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
            "/*vo:mod\nmodule = \"local/demo\"\nvo = \"^0.1.0\"\n*/\npackage main\nfunc main() {}\n",
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
                assert_eq!(inline_mod.vo.to_string(), "^0.1.0");
            }
            other => panic!("expected EphemeralInlineMod, got {:?}", other),
        }
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn single_file_context_retries_when_source_changes_after_classification() {
        use std::sync::mpsc;

        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let source_path = root.join("main.vo");
        std::fs::write(&source_path, "package main\nfunc main() {}\n").unwrap();

        let (reached_tx, reached_rx) = mpsc::channel();
        let (resume_tx, resume_rx) = mpsc::channel();
        assert!(SINGLE_FILE_CONTEXT_VALIDATE_PAUSES
            .get_or_init(|| Mutex::new(HashMap::new()))
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(
                source_path.clone(),
                StableModReadPause {
                    reached: reached_tx,
                    resume: resume_rx,
                },
            )
            .is_none());

        let reader_path = source_path.clone();
        let reader = std::thread::spawn(move || {
            load_single_file_context_with_options_and_generation(
                &RealFs::new("."),
                &reader_path,
                &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
            )
            .unwrap()
        });
        reached_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();
        let replacement = concat!(
            "/*vo:mod\n",
            "module = \"local/reclassified\"\n",
            "vo = \"^0.1.0\"\n",
            "*/\n",
            "package main\n",
        );
        std::fs::write(&source_path, replacement).unwrap();
        resume_tx.send(()).unwrap();

        let (context, generation) = reader.join().unwrap();
        assert!(matches!(
            context,
            SingleFileContext::EphemeralInlineMod { ref inline_mod, .. }
                if inline_mod.module.as_str() == "local/reclassified"
        ));
        assert!(generation.matches_source(&source_path, replacement));
        generation.validate(&RealFs::new(".")).unwrap();
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn single_file_context_retries_when_an_ancestor_manifest_appears() {
        use std::sync::mpsc;

        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let source_path = root.join("main.vo");
        std::fs::write(&source_path, "package main\nfunc main() {}\n").unwrap();

        let (reached_tx, reached_rx) = mpsc::channel();
        let (resume_tx, resume_rx) = mpsc::channel();
        assert!(SINGLE_FILE_CONTEXT_VALIDATE_PAUSES
            .get_or_init(|| Mutex::new(HashMap::new()))
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(
                source_path.clone(),
                StableModReadPause {
                    reached: reached_tx,
                    resume: resume_rx,
                },
            )
            .is_none());

        let reader_path = source_path.clone();
        let reader = std::thread::spawn(move || {
            load_single_file_context_with_options_and_generation(
                &RealFs::new("."),
                &reader_path,
                &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
            )
            .unwrap()
        });
        reached_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();
        std::fs::write(
            root.join("vo.mod"),
            "module = \"github.com/acme/appeared\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();
        resume_tx.send(()).unwrap();

        let (context, generation) = reader.join().unwrap();
        assert!(matches!(
            context,
            SingleFileContext::Project(ref project)
                if project.project_deps().current_module() == Some("github.com/acme/appeared")
        ));
        generation.validate(&RealFs::new(".")).unwrap();
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn single_file_context_retries_when_the_project_manifest_disappears() {
        use std::sync::mpsc;

        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let source_path = root.join("main.vo");
        let mod_path = root.join("vo.mod");
        std::fs::write(&source_path, "package main\nfunc main() {}\n").unwrap();
        std::fs::write(
            &mod_path,
            "module = \"github.com/acme/disappearing\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();

        let (reached_tx, reached_rx) = mpsc::channel();
        let (resume_tx, resume_rx) = mpsc::channel();
        assert!(SINGLE_FILE_CONTEXT_VALIDATE_PAUSES
            .get_or_init(|| Mutex::new(HashMap::new()))
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(
                source_path.clone(),
                StableModReadPause {
                    reached: reached_tx,
                    resume: resume_rx,
                },
            )
            .is_none());

        let reader_path = source_path.clone();
        let reader = std::thread::spawn(move || {
            load_single_file_context_with_options_and_generation(
                &RealFs::new("."),
                &reader_path,
                &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
            )
            .unwrap()
        });
        reached_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();
        std::fs::remove_file(mod_path).unwrap();
        resume_tx.send(()).unwrap();

        let (context, generation) = reader.join().unwrap();
        assert!(matches!(context, SingleFileContext::AdHoc { .. }));
        generation.validate(&RealFs::new(".")).unwrap();
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn single_file_context_retries_when_the_nearest_project_root_relocates() {
        use std::sync::mpsc;

        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let nested = root.join("nested");
        std::fs::create_dir(&nested).unwrap();
        let source_path = nested.join("main.vo");
        let outer_mod = root.join("vo.mod");
        std::fs::write(&source_path, "package main\nfunc main() {}\n").unwrap();
        std::fs::write(
            &outer_mod,
            "module = \"github.com/acme/outer\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();

        let (reached_tx, reached_rx) = mpsc::channel();
        let (resume_tx, resume_rx) = mpsc::channel();
        assert!(SINGLE_FILE_CONTEXT_VALIDATE_PAUSES
            .get_or_init(|| Mutex::new(HashMap::new()))
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(
                source_path.clone(),
                StableModReadPause {
                    reached: reached_tx,
                    resume: resume_rx,
                },
            )
            .is_none());

        let reader_path = source_path.clone();
        let reader = std::thread::spawn(move || {
            load_single_file_context_with_options_and_generation(
                &RealFs::new("."),
                &reader_path,
                &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
            )
            .unwrap()
        });
        reached_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();
        std::fs::write(
            nested.join("vo.mod"),
            "module = \"github.com/acme/inner\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();
        std::fs::remove_file(outer_mod).unwrap();
        resume_tx.send(()).unwrap();

        let (context, generation) = reader.join().unwrap();
        assert!(matches!(
            context,
            SingleFileContext::Project(ref project)
                if project.project_deps().current_module() == Some("github.com/acme/inner")
                    && project.project_root() == nested
        ));
        generation.validate(&RealFs::new(".")).unwrap();
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn single_file_context_rejects_a_linked_source_leaf() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let target = root.join("target.vo");
        let source = root.join("main.vo");
        std::fs::write(&target, "package main\n").unwrap();
        #[cfg(unix)]
        std::os::unix::fs::symlink(&target, &source).unwrap();
        #[cfg(windows)]
        if std::os::windows::fs::symlink_file(&target, &source).is_err() {
            return;
        }

        let error = load_single_file_context_with_options_and_generation(
            &RealFs::new("."),
            &source,
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap_err();

        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ReadFailed);
        assert!(error.detail.contains("single-file source"), "{error}");
    }

    #[test]
    fn single_file_generation_validation_reports_authority_drift_structurally() {
        let mut fs = MemoryFs::new();
        fs.add_file("main.vo", "package main\n");
        let (_, generation) = load_single_file_context_with_options_and_generation(
            &fs,
            Path::new("main.vo"),
            &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
        )
        .unwrap();
        fs.add_file(
            "vo.mod",
            "module = \"github.com/acme/appeared\"\nvo = \"^0.1.0\"\n",
        );

        let error = generation.validate(&fs).unwrap_err();

        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert_eq!(error.path.as_deref(), Some("main.vo"));
        assert!(
            error.detail.contains("classification generation"),
            "{error}"
        );
    }

    #[test]
    fn read_project_deps_rejects_incompatible_toolchain() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "vo.mod",
            "module = \"github.com/acme/app\"\nvo = \"^9.0.0\"\n",
        );

        let error = read_project_deps(&fs).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert_eq!(error.path.as_deref(), Some("vo.mod"));
        assert!(error.detail.contains("requires Vo toolchain ^9.0.0"));
        assert!(error.detail.contains(crate::TOOLCHAIN_VERSION));
    }

    #[test]
    fn load_single_file_context_rejects_incompatible_inline_toolchain() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "main.vo",
            "/*vo:mod\nmodule = \"local/demo\"\nvo = \"^9.0.0\"\n*/\npackage main\nfunc main() {}\n",
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
            "/*vo:mod\nmodule = \"local/tool\"\nvo = \"^0.1.0\"\n*/\npackage main\n",
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
    fn project_rejects_inline_mod_after_ordinary_leading_comments() {
        let mut fs = root_project_fs();
        let prefix = "// license\n/* ordinary */\n";
        fs.add_file(
            "scripts/tool.vo",
            format!(
                "{prefix}/*vo:mod\nmodule = \"local/tool\"\nvo = \"^0.1.0\"\n*/\npackage main\n"
            ),
        );
        let error = load_single_file_context(&fs, Path::new("scripts/tool.vo")).unwrap_err();
        assert_eq!(error.kind, ProjectDepsErrorKind::ValidationFailed);
        assert_eq!(
            error.span().map(|span| span.start.0),
            Some(u32::try_from(prefix.len()).unwrap())
        );
    }

    #[test]
    fn load_single_file_context_project_when_ancestor_mod_exists_and_no_inline() {
        let mut fs = root_project_fs();
        fs.add_file("vo.lock", lock_file_for_workspace_source());
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
        fs.add_file(
            "main.vo",
            "/*vo:mod\nmodule = \"local/demo\"\n*/\npackage main\n",
        );
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
    fn load_single_file_context_surfaces_duplicate_inline_toml_key() {
        let mut fs = MemoryFs::new();
        let source = "/*vo:mod\nmodule = \"local/demo\"\nmodule = \"local/other\"\nvo = \"^0.1.0\"\n*/\npackage main\n";
        fs.add_file("main.vo", source);

        let error = load_single_file_context(&fs, Path::new("main.vo")).unwrap_err();
        assert_eq!(error.stage, ProjectDepsStage::ModFile);
        assert_eq!(error.kind, ProjectDepsErrorKind::ParseFailed);
        assert!(error.detail.contains("duplicate key"), "{}", error.detail);
        assert!(error
            .span()
            .map(|span| source[span.to_range()].contains("module = \"local/other\""))
            .unwrap_or(false));
    }

    #[test]
    fn authored_project_files_are_replaced_atomically() {
        let temp = tempfile::tempdir().unwrap();
        let (mod_file, lock_file) = module_with_lock("github.com/acme/atomic");

        commit_project_files(temp.path(), &mod_file, Some(&lock_file)).unwrap();

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

    #[cfg(unix)]
    #[test]
    fn project_metadata_reads_reject_linked_and_special_entries() {
        use std::os::unix::ffi::OsStrExt;
        use std::os::unix::fs::symlink;

        let temp = tempfile::tempdir().unwrap();
        let linked_root = temp.path().join("linked-root");
        std::fs::create_dir(&linked_root).unwrap();
        let target = temp.path().join("target.mod");
        std::fs::write(
            &target,
            "module = \"github.com/acme/linked\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();
        symlink(&target, linked_root.join("vo.mod")).unwrap();

        let linked_error = read_project_deps_at_root(&linked_root).unwrap_err();
        assert_eq!(linked_error.stage, ProjectDepsStage::ModFile);
        assert_eq!(linked_error.kind, ProjectDepsErrorKind::ValidationFailed);

        let fifo_root = temp.path().join("fifo-root");
        std::fs::create_dir(&fifo_root).unwrap();
        let fifo_path = fifo_root.join("vo.mod");
        let fifo_c_path = std::ffi::CString::new(fifo_path.as_os_str().as_bytes()).unwrap();
        assert_eq!(unsafe { libc::mkfifo(fifo_c_path.as_ptr(), 0o600) }, 0);

        let fifo_error = read_project_deps_at_root(&fifo_root).unwrap_err();
        assert_eq!(fifo_error.stage, ProjectDepsStage::ModFile);
        assert_eq!(fifo_error.kind, ProjectDepsErrorKind::ValidationFailed);

        let real_root = temp.path().join("real-root");
        std::fs::create_dir(&real_root).unwrap();
        std::fs::write(
            real_root.join("vo.mod"),
            "module = \"github.com/acme/parent-link\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();
        let alias_root = temp.path().join("alias-root");
        symlink(&real_root, &alias_root).unwrap();

        let parent_error = read_project_deps_at_root(&alias_root).unwrap_err();
        assert_eq!(parent_error.stage, ProjectDepsStage::ModFile);
        assert!(
            parent_error.detail.contains("stable read"),
            "{}",
            parent_error.detail
        );
    }

    #[test]
    fn authored_project_transaction_rejects_manifest_directory_before_changing_lock() {
        let temp = tempfile::tempdir().unwrap();
        let old_lock = b"previous lock contents";
        std::fs::write(temp.path().join("vo.lock"), old_lock).unwrap();
        std::fs::create_dir(temp.path().join("vo.mod")).unwrap();
        let (mod_file, lock_file) = module_with_lock("github.com/acme/rollback");

        let guard = lock_project_mutation(temp.path()).unwrap();
        assert!(write_project_files(temp.path(), &guard, &mod_file, Some(&lock_file)).is_err());

        assert_eq!(
            std::fs::read(temp.path().join("vo.lock")).unwrap(),
            old_lock
        );
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn stable_reader_completes_interrupted_project_transaction() {
        let temp = tempfile::tempdir().unwrap();
        let (old_mod, old_lock) = module_with_lock("github.com/acme/old");
        commit_project_files(temp.path(), &old_mod, Some(&old_lock)).unwrap();

        let (new_mod, new_lock) = module_with_lock("github.com/acme/new");
        let new_mod_bytes = new_mod.render().unwrap().into_bytes();
        let new_lock_bytes = new_lock.render().unwrap().into_bytes();
        let journal = render_project_transaction(&new_mod_bytes, Some(&new_lock_bytes)).unwrap();
        std::fs::write(temp.path().join(PROJECT_TRANSACTION_FILE), journal).unwrap();
        std::fs::write(temp.path().join("vo.lock"), &new_lock_bytes).unwrap();

        let recovered = read_mod_file_stable(temp.path()).unwrap();

        assert_eq!(recovered, new_mod);
        assert_eq!(read_lock_file(temp.path()).unwrap(), new_lock);
        assert!(!temp.path().join(PROJECT_TRANSACTION_FILE).exists());
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn stable_reader_rejects_corrupt_project_transaction() {
        let temp = tempfile::tempdir().unwrap();
        let (mod_file, lock_file) = module_with_lock("github.com/acme/app");
        commit_project_files(temp.path(), &mod_file, Some(&lock_file)).unwrap();
        std::fs::write(
            temp.path().join(PROJECT_TRANSACTION_FILE),
            b"corrupt transaction",
        )
        .unwrap();

        let error = read_mod_file_stable(temp.path()).unwrap_err();

        assert!(error.to_string().contains("transaction journal"), "{error}");
        assert!(temp.path().join(PROJECT_TRANSACTION_FILE).exists());
    }

    #[test]
    fn project_pair_does_not_change_public_files_before_journal_is_durable() {
        let temp = tempfile::tempdir().unwrap();
        let (old_mod, old_lock) = module_with_lock("github.com/acme/old");
        commit_project_files(temp.path(), &old_mod, Some(&old_lock)).unwrap();

        let (new_mod, new_lock) = module_with_lock("github.com/acme/new");
        fail_project_parent_sync_for_test(&temp.path().join(PROJECT_TRANSACTION_FILE));

        let guard = lock_project_mutation(temp.path()).unwrap();
        let error =
            write_project_files(temp.path(), &guard, &new_mod, Some(&new_lock)).unwrap_err();

        assert!(matches!(
            error,
            Error::ProjectPublicationPostCommitFailure { .. }
        ));
        assert_eq!(read_mod_file(temp.path()).unwrap(), old_mod);
        assert_eq!(read_lock_file(temp.path()).unwrap(), old_lock);
        assert!(temp.path().join(PROJECT_TRANSACTION_FILE).exists());
    }

    #[test]
    fn create_new_reports_that_the_manifest_committed_when_parent_sync_fails() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("vo.mod");
        let mod_file =
            ModFile::parse("module = \"github.com/acme/new\"\nvo = \"^0.1.0\"\n").unwrap();
        fail_project_parent_sync_for_test(&path);

        let guard = lock_project_mutation(temp.path()).unwrap();
        let error = write_new_mod_file(temp.path(), &guard, &mod_file).unwrap_err();

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
    fn project_pair_retains_redo_journal_after_committed_lock_sync_failure() {
        let temp = tempfile::tempdir().unwrap();
        let (mod_file, lock_file) = module_with_lock("github.com/acme/pair");
        fail_project_parent_sync_for_test(&temp.path().join("vo.lock"));

        let guard = lock_project_mutation(temp.path()).unwrap();
        let error =
            write_project_files(temp.path(), &guard, &mod_file, Some(&lock_file)).unwrap_err();

        assert!(matches!(
            error,
            Error::ProjectPublicationPostCommitFailure { .. }
        ));
        assert!(!temp.path().join("vo.mod").exists());
        assert_eq!(read_lock_file(temp.path()).unwrap(), lock_file);
        assert!(temp.path().join(PROJECT_TRANSACTION_FILE).exists());

        drop(guard);
        assert_eq!(
            read_mod_file_stable(temp.path()).unwrap().module,
            mod_file.module
        );
        assert_eq!(read_lock_file(temp.path()).unwrap(), lock_file);
        assert!(!temp.path().join(PROJECT_TRANSACTION_FILE).exists());
    }

    #[test]
    fn project_pair_retains_redo_journal_after_committed_manifest_sync_failure() {
        let temp = tempfile::tempdir().unwrap();
        let (mod_file, lock_file) = module_with_lock("github.com/acme/pair-mod");
        fail_project_parent_sync_for_test(&temp.path().join("vo.mod"));

        let guard = lock_project_mutation(temp.path()).unwrap();
        let error =
            write_project_files(temp.path(), &guard, &mod_file, Some(&lock_file)).unwrap_err();

        assert!(matches!(
            error,
            Error::ProjectPublicationPostCommitFailure { .. }
        ));
        assert_eq!(read_mod_file(temp.path()).unwrap().module, mod_file.module);
        assert_eq!(read_lock_file(temp.path()).unwrap(), lock_file);
        assert!(temp.path().join(PROJECT_TRANSACTION_FILE).exists());

        drop(guard);
        assert_eq!(
            read_mod_file_stable(temp.path()).unwrap().module,
            mod_file.module
        );
        assert_eq!(read_lock_file(temp.path()).unwrap(), lock_file);
        assert!(!temp.path().join(PROJECT_TRANSACTION_FILE).exists());
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn initially_lock_free_reader_retries_after_a_writer_creates_the_lock() {
        use std::sync::mpsc;

        let temp = tempfile::tempdir().unwrap();
        let old_mod =
            ModFile::parse("module = \"github.com/acme/old\"\nvo = \"^0.1.0\"\n").unwrap();
        std::fs::write(temp.path().join("vo.mod"), old_mod.render().unwrap()).unwrap();

        let (reached_tx, reached_rx) = mpsc::channel();
        let (resume_tx, resume_rx) = mpsc::channel();
        assert!(STABLE_MOD_READ_PAUSES
            .get_or_init(|| Mutex::new(HashMap::new()))
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(
                temp.path().to_path_buf(),
                StableModReadPause {
                    reached: reached_tx,
                    resume: resume_rx,
                },
            )
            .is_none());

        let reader_dir = temp.path().to_path_buf();
        let reader = std::thread::spawn(move || read_mod_file_stable(&reader_dir).unwrap());
        reached_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();

        let (new_mod, new_lock) = module_with_lock("github.com/acme/new");
        commit_project_files(temp.path(), &new_mod, Some(&new_lock)).unwrap();
        resume_tx.send(()).unwrap();

        assert_eq!(reader.join().unwrap(), new_mod);
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn project_mutation_preflight_rejects_portable_control_file_aliases() {
        for (alias, canonical) in [
            (".VO-PROJECT.LOCK", PROJECT_LOCK_FILE),
            (".VO-PROJECT.TRANSACTION", PROJECT_TRANSACTION_FILE),
        ] {
            let temp = tempfile::tempdir().unwrap();
            std::fs::write(temp.path().join(alias), b"alias").unwrap();

            let error = match lock_project_mutation(temp.path()) {
                Ok(_) => panic!("portable control-file alias was accepted: {alias}"),
                Err(error) => error,
            };
            assert!(error.to_string().contains("portable alias"), "{error}");
            assert!(error.to_string().contains(alias), "{error}");
            assert!(error.to_string().contains(canonical), "{error}");
            let names = std::fs::read_dir(temp.path())
                .unwrap()
                .map(|entry| entry.unwrap().file_name())
                .collect::<Vec<_>>();
            assert_eq!(names, [std::ffi::OsString::from(alias)]);
        }
    }

    #[cfg(unix)]
    #[test]
    fn held_project_lock_rejects_path_rebinding_before_publication() {
        let temp = tempfile::tempdir().unwrap();
        let guard = lock_project_mutation(temp.path()).unwrap();
        let lock_path = temp.path().join(".vo-project.lock");
        std::fs::rename(&lock_path, temp.path().join("detached.lock")).unwrap();
        std::fs::write(&lock_path, b"replacement").unwrap();
        let (mod_file, lock_file) = module_with_lock("github.com/acme/rebound");

        let error =
            write_project_files(temp.path(), &guard, &mod_file, Some(&lock_file)).unwrap_err();

        assert!(error.to_string().contains("rebound"), "{error}");
        assert!(!temp.path().join("vo.mod").exists());
        assert!(!temp.path().join(PROJECT_TRANSACTION_FILE).exists());
    }

    #[test]
    fn project_transaction_journal_publication_is_no_clobber() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join(PROJECT_TRANSACTION_FILE);
        let barrier = std::sync::Arc::new(std::sync::Barrier::new(3));
        let mut writers = Vec::new();
        for contents in [b"first".as_slice(), b"second".as_slice()] {
            let path = path.clone();
            let barrier = barrier.clone();
            let contents = contents.to_vec();
            writers.push(std::thread::spawn(move || {
                barrier.wait();
                write_bytes_create_new(&path, &contents)
            }));
        }
        barrier.wait();
        let results = writers
            .into_iter()
            .map(|writer| writer.join().unwrap())
            .collect::<Vec<_>>();

        assert_eq!(results.iter().filter(|result| result.is_ok()).count(), 1);
        let published = std::fs::read(path).unwrap();
        assert!(published == b"first" || published == b"second");
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn project_context_retries_an_unlocked_root_metadata_pair_change() {
        use std::sync::mpsc;

        let temp = tempfile::tempdir().unwrap();
        let (old_mod, old_lock) = module_with_lock("github.com/acme/old-context");
        std::fs::write(temp.path().join("vo.mod"), old_mod.render().unwrap()).unwrap();
        std::fs::write(temp.path().join("vo.lock"), old_lock.render().unwrap()).unwrap();

        let (reached_tx, reached_rx) = mpsc::channel();
        let (resume_tx, resume_rx) = mpsc::channel();
        assert!(PROJECT_CONTEXT_ROOT_READ_PAUSES
            .get_or_init(|| Mutex::new(HashMap::new()))
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(
                temp.path().to_path_buf(),
                StableModReadPause {
                    reached: reached_tx,
                    resume: resume_rx,
                },
            )
            .is_none());

        let reader_dir = temp.path().to_path_buf();
        let reader = std::thread::spawn(move || {
            let fs = RealFs::new(".");
            load_project_context_with_options(
                &fs,
                &reader_dir,
                &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
            )
            .unwrap()
        });
        reached_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();

        let (new_mod, new_lock) = module_with_lock("github.com/acme/new-context");
        std::fs::write(temp.path().join("vo.mod"), new_mod.render().unwrap()).unwrap();
        std::fs::write(temp.path().join("vo.lock"), new_lock.render().unwrap()).unwrap();
        resume_tx.send(()).unwrap();

        let context = reader.join().unwrap();
        assert_eq!(
            context.project_deps().current_module(),
            Some("github.com/acme/new-context")
        );
        assert_eq!(
            context
                .project_deps()
                .lock_file()
                .map(|lock_file| lock_file.root.module.as_str()),
            Some("github.com/acme/new-context")
        );
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn project_context_retries_an_unlocked_lock_only_change() {
        use std::sync::mpsc;

        let temp = tempfile::tempdir().unwrap();
        let (mod_file, old_lock) = module_with_lock("github.com/acme/lock-drift");
        std::fs::write(temp.path().join("vo.mod"), mod_file.render().unwrap()).unwrap();
        std::fs::write(temp.path().join("vo.lock"), old_lock.render().unwrap()).unwrap();

        let (reached_tx, reached_rx) = mpsc::channel();
        let (resume_tx, resume_rx) = mpsc::channel();
        assert!(PROJECT_CONTEXT_ROOT_READ_PAUSES
            .get_or_init(|| Mutex::new(HashMap::new()))
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(
                temp.path().to_path_buf(),
                StableModReadPause {
                    reached: reached_tx,
                    resume: resume_rx,
                },
            )
            .is_none());

        let reader_dir = temp.path().to_path_buf();
        let reader = std::thread::spawn(move || {
            load_project_context_with_options(
                &RealFs::new("."),
                &reader_dir,
                &ProjectContextOptions::new(WorkspaceDiscovery::Disabled),
            )
            .unwrap()
        });
        reached_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();

        let mut new_lock = old_lock;
        let expected_release = Digest::from_sha256(b"new-lock-generation");
        new_lock.modules[0].release = expected_release.clone();
        std::fs::write(temp.path().join("vo.lock"), new_lock.render().unwrap()).unwrap();
        resume_tx.send(()).unwrap();

        let context = reader.join().unwrap();
        assert_eq!(
            context.project_deps().locked_modules()[0].release,
            expected_release
        );
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn project_context_retries_when_auto_workspace_appears_before_return() {
        use std::sync::mpsc;

        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        std::fs::write(
            root.join("vo.mod"),
            "module = \"github.com/acme/workspace-appearance\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();

        let (reached_tx, reached_rx) = mpsc::channel();
        let (resume_tx, resume_rx) = mpsc::channel();
        assert!(PROJECT_CONTEXT_WORKSPACE_VALIDATE_PAUSES
            .get_or_init(|| Mutex::new(HashMap::new()))
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(
                root.clone(),
                StableModReadPause {
                    reached: reached_tx,
                    resume: resume_rx,
                },
            )
            .is_none());

        let reader_root = root.clone();
        let reader = std::thread::spawn(move || {
            load_project_context_with_options(
                &RealFs::new("."),
                &reader_root,
                &ProjectContextOptions::new(WorkspaceDiscovery::Auto),
            )
            .unwrap()
        });
        reached_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();
        std::fs::write(root.join("vo.work"), "version = 1\nmembers = []\n").unwrap();
        resume_tx.send(()).unwrap();

        let context = reader.join().unwrap();
        assert_eq!(
            context.workspace_file().map(normalize_fs_path),
            Some(normalize_fs_path(&root.join("vo.work")))
        );
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn project_context_retries_when_a_member_manifest_changes_before_return() {
        use std::sync::mpsc;

        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let app = root.join("app");
        let library = root.join("lib");
        let utility = root.join("util");
        for directory in [&app, &library, &utility] {
            std::fs::create_dir(directory).unwrap();
        }
        std::fs::write(
            root.join("vo.work"),
            "version = 1\nmembers = [\"lib\", \"util\"]\n",
        )
        .unwrap();
        std::fs::write(
            app.join("vo.mod"),
            "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n[dependencies]\n\"github.com/acme/lib\" = \"^1.0.0\"\n",
        )
        .unwrap();
        std::fs::write(
            library.join("vo.mod"),
            "module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(
            utility.join("vo.mod"),
            "module = \"github.com/acme/util\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();

        let (reached_tx, reached_rx) = mpsc::channel();
        let (resume_tx, resume_rx) = mpsc::channel();
        assert!(PROJECT_CONTEXT_WORKSPACE_VALIDATE_PAUSES
            .get_or_init(|| Mutex::new(HashMap::new()))
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(
                app.clone(),
                StableModReadPause {
                    reached: reached_tx,
                    resume: resume_rx,
                },
            )
            .is_none());

        let reader_app = app.clone();
        let reader = std::thread::spawn(move || {
            load_project_context_with_options(
                &RealFs::new("."),
                &reader_app,
                &ProjectContextOptions::new(WorkspaceDiscovery::Auto),
            )
            .unwrap()
        });
        reached_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();
        std::fs::write(
            library.join("vo.mod"),
            "module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n[dependencies]\n\"github.com/acme/util\" = \"^1.0.0\"\n",
        )
        .unwrap();
        resume_tx.send(()).unwrap();

        let context = reader.join().unwrap();
        assert_eq!(context.authority(), ProjectAuthority::Workspace);
        assert_eq!(context.workspace_modules().len(), 2);
        let library = context
            .workspace_modules()
            .iter()
            .find(|module| module.module().as_str() == "github.com/acme/lib")
            .unwrap();
        assert_eq!(
            library.mod_file().dependencies[0].module.as_str(),
            "github.com/acme/util"
        );
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn project_context_reader_waits_for_pair_commit_guard() {
        use std::sync::mpsc;

        let temp = tempfile::tempdir().unwrap();
        let (old_mod, old_lock) = module_with_lock("github.com/acme/old");
        commit_project_files(temp.path(), &old_mod, Some(&old_lock)).unwrap();

        let project_dir = temp.path().to_path_buf();
        let writer_dir = project_dir.clone();
        let (locked_tx, locked_rx) = mpsc::channel();
        let (release_tx, release_rx) = mpsc::channel();
        let writer = std::thread::spawn(move || {
            let guard = lock_project_mutation(&writer_dir).unwrap();
            locked_tx.send(()).unwrap();
            release_rx.recv().unwrap();
            let (new_mod, new_lock) = module_with_lock("github.com/acme/new-snapshot");
            write_project_files(&writer_dir, &guard, &new_mod, Some(&new_lock)).unwrap();
            drop(guard);
        });
        locked_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();

        let (attempt_tx, attempt_rx) = mpsc::channel();
        *PROJECT_LOCK_ATTEMPT_HOOK
            .get_or_init(|| Mutex::new(None))
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) =
            Some((project_dir.join(".vo-project.lock"), attempt_tx));

        let reader_dir = project_dir.clone();
        let (result_tx, result_rx) = mpsc::channel();
        let reader = std::thread::spawn(move || {
            let fs = RealFs::new(".");
            let context = load_project_context(&fs, &reader_dir).unwrap();
            result_tx
                .send(context.project_deps().current_module().unwrap().to_string())
                .unwrap();
        });
        attempt_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();
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
