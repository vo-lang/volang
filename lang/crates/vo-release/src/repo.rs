use std::cell::Cell;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::ffi::OsStr;
use std::fs;
use std::io::{self, BufRead, BufReader, Cursor, Read, Write};
use std::path::{Component, Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::rc::Rc;

use flate2::{bufread::GzDecoder, Compression, GzBuilder};
use sha2::{Digest, Sha256};
use tar::{Builder, EntryType, Header};
use vo_module::digest::Digest as ModDigest;
use vo_module::ext_manifest::NativeBuildManifest;
use vo_module::identity::ArtifactId;
use vo_module::project;
use vo_module::schema::manifest::{
    ManifestArtifact, ManifestDependency, ManifestPackage, ManifestSource, ReleaseManifest,
    SOURCE_ARCHIVE_ASSET_NAME, SOURCE_ARCHIVE_TOP_LEVEL_DIR,
};
use vo_module::schema::modfile::ModFile;
use vo_module::schema::{PackageManifest, SourceFileEntry, SourceFileMode};
use vo_module::version::ExactVersion;
use vo_syntax::{Lexer, TokenKind};

use crate::error::bounded_sorted_diagnostic;
use crate::publish::{PendingOutputDir, RELEASE_STAGE_DIR_PREFIX};
use crate::{ReleaseError, ReleaseResult};

const MAX_GIT_REVISION_OUTPUT_BYTES: usize = 8 * 1024;
const MAX_GIT_STDERR_BYTES: usize = 64 * 1024;
const MAX_GIT_PATH_RECORD_BYTES: usize = vo_module::schema::MAX_PORTABLE_PATH_BYTES + 128;
const MAX_GIT_TREE_WALK_REQUESTS: usize = 100_000;
const MAX_GIT_TREE_WALK_RECORDS: usize = 1_000_000;
const MAX_GIT_TREE_WALK_BYTES: usize = 256 * 1024 * 1024;
const MAX_DIAGNOSTIC_ITEMS: usize = 8;

fn portable_name_eq(left: &str, right: &str) -> bool {
    vo_module::schema::portable_case_key(left) == vo_module::schema::portable_case_key(right)
}

fn portable_name_matches_any(value: &str, candidates: &[&str]) -> bool {
    let key = vo_module::schema::portable_case_key(value);
    candidates
        .iter()
        .any(|candidate| key == vo_module::schema::portable_case_key(candidate))
}

fn portable_name_starts_with(value: &str, prefix: &str) -> bool {
    vo_module::schema::portable_case_key(value)
        .starts_with(&vo_module::schema::portable_case_key(prefix))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArtifactInput {
    pub kind: String,
    pub target: String,
    pub name: String,
    pub path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StageReleaseOptions {
    pub version: String,
    pub commit: Option<String>,
    pub artifacts: Vec<ArtifactInput>,
    pub out_dir: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StagedArtifact {
    pub kind: String,
    pub target: String,
    pub name: String,
    /// Canonical flat registry asset name derived from `(kind, target, name)`.
    pub asset_name: String,
    pub size: u64,
    pub digest: String,
    pub source_path: PathBuf,
    pub output_path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct StagedRelease {
    pub repo_root: PathBuf,
    pub out_dir: PathBuf,
    pub version: String,
    pub commit: String,
    pub source_name: String,
    pub source_path: PathBuf,
    pub source_size: u64,
    pub source_digest: String,
    pub package_size: u64,
    pub package_digest: String,
    pub manifest_path: PathBuf,
    pub package_path: PathBuf,
    pub manifest_digest: String,
    pub manifest_json: String,
    pub artifacts: Vec<StagedArtifact>,
}

impl StagedRelease {
    /// Exact GitHub Release upload set in stable protocol order:
    /// `vo.release.json`, `vo.package.json`, source package, then artifacts sorted
    /// by their complete `(kind, target, name)` identity.
    pub fn assets(&self) -> Vec<&Path> {
        let mut assets = Vec::with_capacity(3 + self.artifacts.len());
        assets.push(self.manifest_path.as_path());
        assets.push(self.package_path.as_path());
        assets.push(self.source_path.as_path());
        assets.extend(
            self.artifacts
                .iter()
                .map(|artifact| artifact.output_path.as_path()),
        );
        assets
    }
}

struct PreparedArtifact {
    staged: StagedArtifact,
    manifest_artifact: ManifestArtifact,
}

#[derive(Debug, Clone)]
struct CommitTreeFile {
    repository_path: String,
    module_path: String,
    object_id: String,
    mode: u32,
}

#[derive(Debug)]
struct GitTreeEntry {
    name: String,
    object_id: String,
    git_mode: String,
    git_kind: String,
    archive_mode: Option<u32>,
}

pub(crate) struct CommitTree {
    repository_root: PathBuf,
    module_prefix: String,
    files: BTreeMap<String, CommitTreeFile>,
}

#[derive(Debug, Clone)]
pub(crate) struct SelectedSourceFile {
    tree_file: CommitTreeFile,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ReleaseSourceFile {
    path: String,
    bytes: Vec<u8>,
    mode: u32,
}

/// One bounded, immutable view of every byte that can enter the source
/// archive. Both `vo.package.json.files` and the tar writer consume this value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ReleaseSourceSnapshot {
    files: Vec<ReleaseSourceFile>,
    extracted_size: usize,
}

/// Immutable release payload shared by staging and the post-write verifier.
///
/// Keeping these fields together prevents the writer and verifier from being
/// called with digests or manifests belonging to different release builds.
struct PreparedReleasePayload<'a> {
    source_top_dir: &'a str,
    source_name: &'a str,
    source_size: u64,
    source_digest: &'a str,
    manifest_json: &'a str,
    manifest_digest: &'a str,
    package_bytes: &'a [u8],
    source_snapshot: &'a ReleaseSourceSnapshot,
    artifacts: &'a [PreparedArtifact],
}

#[cfg(test)]
struct BoundedVecWriter {
    bytes: Vec<u8>,
    max_bytes: usize,
    context: &'static str,
}

#[derive(Debug)]
struct BoundedCommandOutput {
    status: ExitStatus,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
}

#[derive(Clone, Default)]
struct TarPaddingGuard {
    remaining: Rc<Cell<usize>>,
}

impl TarPaddingGuard {
    fn expect_after(&self, size: u64, source_path: &Path) -> ReleaseResult<()> {
        if self.remaining.get() != 0 {
            return Err(ReleaseError::IoError(
                source_path.to_path_buf(),
                "source archive parser did not consume the preceding tar padding".to_string(),
            ));
        }
        let remainder = usize::try_from(size % 512).expect("tar block remainder fits usize");
        self.remaining.set((512 - remainder) % 512);
        Ok(())
    }
}

struct CanonicalTarReader<R> {
    inner: R,
    padding: TarPaddingGuard,
}

impl<R> CanonicalTarReader<R> {
    fn new(inner: R) -> (Self, TarPaddingGuard) {
        let padding = TarPaddingGuard::default();
        (
            Self {
                inner,
                padding: padding.clone(),
            },
            padding,
        )
    }

    fn into_inner(self) -> R {
        self.inner
    }
}

impl<R: Read> Read for CanonicalTarReader<R> {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
        let read = self.inner.read(buffer)?;
        let checked = read.min(self.padding.remaining.get());
        if buffer[..checked].iter().any(|byte| *byte != 0) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "source archive tar padding must be zero",
            ));
        }
        self.padding
            .remaining
            .set(self.padding.remaining.get() - checked);
        Ok(read)
    }
}

#[cfg(test)]
impl BoundedVecWriter {
    fn new(max_bytes: usize, context: &'static str) -> Self {
        Self {
            bytes: Vec::new(),
            max_bytes,
            context,
        }
    }

    fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }
}

#[cfg(test)]
impl Write for BoundedVecWriter {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
        let new_len = self
            .bytes
            .len()
            .checked_add(buffer.len())
            .ok_or_else(|| io::Error::other(format!("{} size overflow", self.context)))?;
        if new_len > self.max_bytes {
            return Err(io::Error::other(format!(
                "{} exceeds the {}-byte limit",
                self.context, self.max_bytes
            )));
        }
        self.bytes
            .try_reserve(buffer.len())
            .map_err(|_| io::Error::other(format!("failed to reserve {} storage", self.context)))?;
        self.bytes.extend_from_slice(buffer);
        Ok(buffer.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

fn read_pipe_bounded<R: Read>(
    mut reader: R,
    max_bytes: usize,
    stream_name: &'static str,
) -> io::Result<Vec<u8>> {
    let mut bytes = Vec::new();
    bytes.try_reserve(max_bytes.min(64 * 1024)).map_err(|_| {
        io::Error::other(format!("failed to reserve bounded {stream_name} storage"))
    })?;
    let mut exceeded = false;
    let mut buffer = [0_u8; 16 * 1024];
    loop {
        let read = reader.read(&mut buffer)?;
        if read == 0 {
            break;
        }
        let remaining = max_bytes.saturating_sub(bytes.len());
        let retained = remaining.min(read);
        if retained > 0 {
            bytes.try_reserve(retained).map_err(|_| {
                io::Error::other(format!("failed to grow bounded {stream_name} storage"))
            })?;
            bytes.extend_from_slice(&buffer[..retained]);
        }
        exceeded |= retained != read;
    }
    if exceeded {
        return Err(io::Error::other(format!(
            "{stream_name} exceeds the {max_bytes}-byte limit"
        )));
    }
    Ok(bytes)
}

fn read_delimited_record_bounded<R: BufRead>(
    reader: &mut R,
    delimiter: u8,
    max_bytes: usize,
    record: &mut Vec<u8>,
) -> io::Result<bool> {
    record.clear();
    loop {
        let available = reader.fill_buf()?;
        if available.is_empty() {
            if record.is_empty() {
                return Ok(false);
            }
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "delimited record is unterminated",
            ));
        }
        let terminator = available.iter().position(|byte| *byte == delimiter);
        let data_len = terminator.unwrap_or(available.len());
        let new_len = record
            .len()
            .checked_add(data_len)
            .ok_or_else(|| io::Error::other("delimited record size overflow"))?;
        if new_len > max_bytes {
            return Err(io::Error::other(format!(
                "delimited record exceeds the {max_bytes}-byte limit"
            )));
        }
        record
            .try_reserve(data_len)
            .map_err(|_| io::Error::other("failed to reserve delimited record storage"))?;
        record.extend_from_slice(&available[..data_len]);
        reader.consume(data_len + usize::from(terminator.is_some()));
        if terminator.is_some() {
            return Ok(true);
        }
    }
}

fn join_bounded_git_pipe(
    reader: std::thread::JoinHandle<io::Result<Vec<u8>>>,
    repo_root: &Path,
    stream_name: &str,
) -> ReleaseResult<Vec<u8>> {
    reader
        .join()
        .map_err(|_| ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: format!("{stream_name} reader thread panicked"),
        })?
        .map_err(|error| ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: error.to_string(),
        })
}

fn git_stdout_record<'a>(text: &'a str, repo_root: &Path, context: &str) -> ReleaseResult<&'a str> {
    let record = text
        .strip_suffix('\n')
        .ok_or_else(|| ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: format!("{context} returned output without its record terminator"),
        })?;
    #[cfg(windows)]
    let record = record.strip_suffix('\r').unwrap_or(record);
    if record.is_empty() {
        return Err(ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: format!("{context} returned an empty record"),
        });
    }
    Ok(record)
}

fn git_stdout_path_record(
    mut bytes: Vec<u8>,
    repo_root: &Path,
    context: &str,
) -> ReleaseResult<PathBuf> {
    if bytes.pop() != Some(b'\n') {
        return Err(ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: format!("{context} returned output without its record terminator"),
        });
    }
    #[cfg(windows)]
    if bytes.last() == Some(&b'\r') {
        bytes.pop();
    }
    if bytes.is_empty() {
        return Err(ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: format!("{context} returned an empty path"),
        });
    }

    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStringExt as _;
        Ok(PathBuf::from(std::ffi::OsString::from_vec(bytes)))
    }
    #[cfg(not(unix))]
    {
        String::from_utf8(bytes)
            .map(PathBuf::from)
            .map_err(|error| ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: format!("{context} returned a non-UTF-8 path: {error}"),
            })
    }
}

fn run_command_output_bounded(
    command: &mut Command,
    repo_root: &Path,
    context: &str,
    max_stdout_bytes: usize,
) -> ReleaseResult<BoundedCommandOutput> {
    let mut child = command
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|error| ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: format!("failed to start {context}: {error}"),
        })?;
    let stdout = child.stdout.take().ok_or_else(|| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: format!("{context} did not expose its configured stdout pipe"),
    })?;
    let stderr = child.stderr.take().ok_or_else(|| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: format!("{context} did not expose its configured stderr pipe"),
    })?;
    let stdout_reader =
        std::thread::spawn(move || read_pipe_bounded(stdout, max_stdout_bytes, "git stdout"));
    let stderr_reader =
        std::thread::spawn(move || read_pipe_bounded(stderr, MAX_GIT_STDERR_BYTES, "git stderr"));
    let status_result = child.wait();
    let stdout_result = join_bounded_git_pipe(stdout_reader, repo_root, "git stdout");
    let stderr_result = join_bounded_git_pipe(stderr_reader, repo_root, "git stderr");
    let stdout = stdout_result?;
    let stderr = stderr_result?;
    let status = status_result.map_err(|error| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: format!("failed to wait for {context}: {error}"),
    })?;
    Ok(BoundedCommandOutput {
        status,
        stdout,
        stderr,
    })
}

fn run_git_nul_records_with_limits(
    command: &mut Command,
    repo_root: &Path,
    context: &str,
    max_records: usize,
    max_bytes: usize,
    mut handle_record: impl FnMut(&[u8]) -> ReleaseResult<()>,
) -> ReleaseResult<()> {
    let mut child = command
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|error| ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: format!("failed to start {context}: {error}"),
        })?;
    let stdout = child.stdout.take().ok_or_else(|| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: format!("{context} did not expose its configured stdout pipe"),
    })?;
    let stderr = child.stderr.take().ok_or_else(|| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: format!("{context} did not expose its configured stderr pipe"),
    })?;
    let stderr_reader =
        std::thread::spawn(move || read_pipe_bounded(stderr, MAX_GIT_STDERR_BYTES, "git stderr"));
    let mut stdout = BufReader::new(stdout);
    let parse_result = (|| {
        let mut record = Vec::new();
        let mut record_count = 0usize;
        let mut total_bytes = 0usize;
        loop {
            let found = read_delimited_record_bounded(
                &mut stdout,
                0,
                MAX_GIT_PATH_RECORD_BYTES,
                &mut record,
            )
            .map_err(|error| ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: format!("failed to read {context} path record: {error}"),
            })?;
            if !found {
                break;
            }
            if record.is_empty() {
                return Err(ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("{context} returned an empty path record"),
                });
            }
            record_count = record_count
                .checked_add(1)
                .ok_or_else(|| ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("{context} path record count overflow"),
                })?;
            if record_count > max_records {
                return Err(ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("{context} returned more than {max_records} path records"),
                });
            }
            total_bytes = total_bytes
                .checked_add(record.len().saturating_add(1))
                .ok_or_else(|| ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("{context} output size overflow"),
                })?;
            if total_bytes > max_bytes {
                return Err(ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("{context} output exceeds the {max_bytes}-byte limit"),
                });
            }
            handle_record(&record)?;
        }
        Ok(())
    })();
    drop(stdout);
    if parse_result.is_err() {
        let _ = child.kill();
    }
    let status_result = child.wait();
    let stderr_result = join_bounded_git_pipe(stderr_reader, repo_root, "git stderr");
    parse_result?;
    let status = status_result.map_err(|error| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: format!("failed to wait for {context}: {error}"),
    })?;
    let stderr = stderr_result?;
    if !status.success() {
        let detail = String::from_utf8_lossy(&stderr).trim().to_string();
        return Err(ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: if detail.is_empty() {
                format!("{context} failed with status {status}")
            } else {
                detail
            },
        });
    }
    Ok(())
}

pub fn verify_repo(repo_root: &Path) -> ReleaseResult<()> {
    let repo_root = canonical_repo_root(repo_root)?;
    let commit = resolve_release_commit(&repo_root, None)?;
    validate_clean_tracked_worktree(&repo_root, &commit)?;
    let commit_tree = load_commit_tree(&repo_root, &commit)?;
    let committed_mod_file = read_committed_mod_file(&repo_root, &commit_tree)?;
    validate_committed_repo(&repo_root, &commit_tree, &committed_mod_file)?;
    let live_mod_file = validate_live_repo_policy(&repo_root, &commit_tree)?;
    let module_path = committed_mod_file.module.as_github().ok_or_else(|| {
        ReleaseError::IoError(
            repo_root.clone(),
            format!(
                "release verification requires a canonical github module path; vo.mod declares \
                 the ephemeral '{}' identity",
                committed_mod_file.module,
            ),
        )
    })?;
    validate_module_root_location(&repo_root, module_path.module_root(), &commit_tree)?;
    if live_mod_file != committed_mod_file {
        return Err(ReleaseError::GitError {
            repo_root: repo_root.clone(),
            message: "tracked vo.mod bytes changed while release verification was running"
                .to_string(),
        });
    }
    validate_local_build_inputs(&repo_root, &committed_mod_file, &commit_tree)?;
    let source_files = collect_release_source_files(&repo_root, &committed_mod_file, &commit_tree)?;
    let source_snapshot = capture_release_source_snapshot(&repo_root, &source_files, &commit_tree)?;
    validate_release_source_snapshot(&repo_root, &source_snapshot)?;
    validate_clean_tracked_worktree(&repo_root, &commit)?;
    validate_release_head(&repo_root, &commit)?;
    Ok(())
}

fn validate_live_repo_policy(repo_root: &Path, commit_tree: &CommitTree) -> ReleaseResult<ModFile> {
    let tracked = commit_tree_paths(commit_tree);
    let mut tracked_project_state = tracked
        .iter()
        .filter(|path| {
            path.parent()
                .is_some_and(|parent| parent.as_os_str().is_empty())
                && path
                    .file_name()
                    .and_then(OsStr::to_str)
                    .is_some_and(|name| {
                        portable_name_matches_any(
                            name,
                            &[".vo-project.lock", ".vo-project.transaction"],
                        )
                    })
        })
        .collect::<Vec<_>>();
    tracked_project_state.sort();
    if !tracked_project_state.is_empty() {
        let displayed = bounded_sorted_diagnostic(
            tracked_project_state
                .iter()
                .map(|path| path.display().to_string()),
        );
        return Err(ReleaseError::ManifestSerialize(format!(
            "generated project transaction state must not be tracked or released: {displayed}"
        )));
    }
    let mod_file = read_mod_file(repo_root)?;
    if !mod_file.dependencies.is_empty() {
        validate_exact_tracked_root_file(&tracked, "vo.lock", true)?;
    }

    let vo_sum_paths = find_root_named_entries(repo_root, "vo.sum")?;
    if !vo_sum_paths.is_empty() {
        return Err(ReleaseError::ForbiddenVoSum {
            repo_root: repo_root.to_path_buf(),
            paths: vo_sum_paths,
        });
    }

    for name in ["vo.web.json", ".vo-project.lock", ".vo-project.transaction"] {
        let paths = find_root_named_entries(repo_root, name)?;
        if !paths.is_empty() {
            return Err(ReleaseError::ManifestSerialize(format!(
                "root project protocol state must be removed before release: {}",
                bounded_sorted_diagnostic(paths.iter().map(|path| path.display().to_string())),
            )));
        }
    }

    project::read_project_deps_at_root(repo_root)
        .map_err(map_project_deps_error_for_release_verify)?;
    Ok(mod_file)
}

/// Builds and atomically publishes a release directory on supported filesystems.
///
/// A process crash may leave a private `.vo-release-stage-*` directory beside
/// the requested output. Such directories are intentionally left untouched:
/// after confirming that no release process is active, an operator may inspect
/// and remove only abandoned matching directories owned by the current user.
pub fn stage_release(
    repo_root: &Path,
    options: &StageReleaseOptions,
) -> ReleaseResult<StagedRelease> {
    let repo_root = canonical_repo_root(repo_root)?;
    let commit = resolve_release_commit(&repo_root, options.commit.as_deref())?;
    validate_clean_tracked_worktree(&repo_root, &commit)?;
    let commit_tree = load_commit_tree(&repo_root, &commit)?;
    let mod_file = read_committed_mod_file(&repo_root, &commit_tree)?;
    validate_committed_repo(&repo_root, &commit_tree, &mod_file)?;
    let live_mod_file = validate_live_repo_policy(&repo_root, &commit_tree)?;
    if live_mod_file != mod_file {
        return Err(ReleaseError::GitError {
            repo_root: repo_root.clone(),
            message: "tracked vo.mod bytes changed while release staging was running".to_string(),
        });
    }

    let out_dir = resolve_output_dir(&repo_root, &options.out_dir)?;
    // Spec §5.6.2: only canonical github-hosted modules are publishable;
    // `local/*` ephemeral identities are toolchain-internal and MUST NOT
    // reach release staging.
    let module_path = mod_file.module.as_github().ok_or_else(|| {
        ReleaseError::IoError(
            repo_root.clone(),
            format!(
                "release staging requires a canonical github module path; vo.mod declares an \
                 ephemeral '{}' identity which cannot be published",
                mod_file.module,
            ),
        )
    })?;
    validate_module_root_location(&repo_root, module_path.module_root(), &commit_tree)?;
    let version = ExactVersion::parse(&options.version)
        .map_err(|error| ReleaseError::ManifestSerialize(format!("invalid version: {error}")))?;
    if !module_path.accepts_version(&version) {
        return Err(ReleaseError::ManifestSerialize(format!(
            "version {version} is incompatible with module path {module_path}"
        )));
    }
    let version_string = version.to_string();
    let source_name = SOURCE_ARCHIVE_ASSET_NAME.to_string();
    let mut pending = PendingOutputDir::create(&out_dir)?;
    let prepared_artifacts = prepare_artifacts(
        &options.artifacts,
        &out_dir,
        &[
            "vo.release.json",
            "vo.package.json",
            SOURCE_ARCHIVE_ASSET_NAME,
        ],
        &mut pending,
    )?;
    validate_artifact_contract(&repo_root, &mod_file, &prepared_artifacts)?;
    let source_files = collect_release_source_files(&repo_root, &mod_file, &commit_tree)?;
    let source_snapshot = capture_release_source_snapshot(&repo_root, &source_files, &commit_tree)?;
    validate_release_source_snapshot(&repo_root, &source_snapshot)?;
    let package_manifest = PackageManifest {
        schema_version: 1,
        files: package_file_entries(&source_snapshot)?,
    };
    let package_bytes = package_manifest
        .render()
        .map_err(|error| ReleaseError::ManifestSerialize(error.to_string()))?;
    let package_size = u64::try_from(package_bytes.len()).map_err(|_| {
        ReleaseError::ManifestSerialize("vo.package.json size exceeds u64".to_string())
    })?;
    let package_digest_typed = ModDigest::from_sha256(&package_bytes);
    let package_digest = package_digest_typed.to_string();

    let (source_size, source_digest) = pending.write_new_file_streaming(
        SOURCE_ARCHIVE_ASSET_NAME,
        vo_module::MAX_SOURCE_ARCHIVE_BYTES,
        |target| {
            write_source_package(
                SOURCE_ARCHIVE_TOP_LEVEL_DIR,
                &source_snapshot,
                &package_bytes,
                target,
            )
        },
    )?;
    let source_digest_typed = ModDigest::parse(&source_digest)
        .map_err(|error| ReleaseError::ManifestSerialize(error.to_string()))?;

    let mut dependencies = mod_file
        .dependencies
        .iter()
        .map(|dependency| ManifestDependency {
            module: dependency.module.clone(),
            constraint: dependency.constraint.clone(),
        })
        .collect::<Vec<_>>();
    dependencies.sort_by(|left, right| left.module.cmp(&right.module));

    let manifest = ReleaseManifest {
        schema_version: 2,
        module: module_path.clone(),
        version,
        commit: commit.clone(),
        vo: mod_file.vo.clone(),
        dependencies,
        source: ManifestSource {
            name: source_name.clone(),
            size: source_size,
            digest: source_digest_typed,
        },
        package: ManifestPackage {
            size: package_size,
            digest: package_digest_typed,
        },
        artifacts: prepared_artifacts
            .iter()
            .map(|a| a.manifest_artifact.clone())
            .collect(),
    };
    let manifest_json = manifest
        .render()
        .map_err(|error| ReleaseError::ManifestSerialize(error.to_string()))?;
    let manifest_digest = sha256_digest(manifest_json.as_bytes());

    let source_path = out_dir.join(&source_name);
    let manifest_path = out_dir.join("vo.release.json");
    let package_path = out_dir.join("vo.package.json");
    let payload = PreparedReleasePayload {
        source_top_dir: SOURCE_ARCHIVE_TOP_LEVEL_DIR,
        source_name: &source_name,
        source_size,
        source_digest: &source_digest,
        manifest_json: &manifest_json,
        manifest_digest: &manifest_digest,
        package_bytes: &package_bytes,
        source_snapshot: &source_snapshot,
        artifacts: &prepared_artifacts,
    };
    write_release_atomically(pending, &payload, &repo_root, &commit)?;

    Ok(StagedRelease {
        repo_root,
        out_dir,
        version: version_string,
        commit,
        source_name,
        source_path,
        source_size,
        source_digest,
        package_size,
        package_digest,
        manifest_path,
        package_path,
        manifest_digest,
        manifest_json,
        artifacts: prepared_artifacts
            .into_iter()
            .map(|artifact| artifact.staged)
            .collect(),
    })
}

fn canonical_repo_root(repo_root: &Path) -> ReleaseResult<PathBuf> {
    if !repo_root.is_dir() {
        return Err(ReleaseError::RepoRootNotDirectory(repo_root.to_path_buf()));
    }
    let canonical = fs::canonicalize(repo_root)
        .map_err(|error| ReleaseError::IoError(repo_root.to_path_buf(), error.to_string()))?;
    if !canonical.is_dir() {
        return Err(ReleaseError::RepoRootNotDirectory(canonical));
    }
    Ok(canonical)
}

fn resolve_output_dir(repo_root: &Path, out_dir: &Path) -> ReleaseResult<PathBuf> {
    let raw = if out_dir.is_absolute() {
        out_dir.to_path_buf()
    } else {
        repo_root.join(out_dir)
    };
    match fs::symlink_metadata(&raw) {
        Ok(metadata) => {
            let detail = if metadata.file_type().is_symlink() {
                "release output path must not already exist and must not be a symbolic link"
            } else {
                "release output path must not already exist"
            };
            return Err(ReleaseError::IoError(raw, detail.to_string()));
        }
        Err(error) if error.kind() == io::ErrorKind::NotFound => {}
        Err(error) => return Err(ReleaseError::IoError(raw, error.to_string())),
    }
    let parent = raw.parent().ok_or_else(|| {
        ReleaseError::IoError(
            raw.clone(),
            "output directory must have a parent".to_string(),
        )
    })?;
    let parent = fs::canonicalize(parent)
        .map_err(|error| ReleaseError::IoError(parent.to_path_buf(), error.to_string()))?;
    let file_name = raw.file_name().ok_or_else(|| {
        ReleaseError::IoError(
            raw.clone(),
            "output directory must end in a normal directory name".to_string(),
        )
    })?;
    if file_name.to_str().is_some_and(has_release_stage_dir_prefix) {
        return Err(ReleaseError::IoError(
            raw,
            format!(
                "release output directory name must not use the internal {RELEASE_STAGE_DIR_PREFIX:?} prefix"
            ),
        ));
    }
    let resolved = parent.join(file_name);
    match fs::symlink_metadata(&resolved) {
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(resolved),
        Ok(_) => Err(ReleaseError::IoError(
            resolved,
            "release output path must not already exist".to_string(),
        )),
        Err(error) => Err(ReleaseError::IoError(resolved, error.to_string())),
    }
}

fn write_release_atomically(
    mut pending: PendingOutputDir,
    payload: &PreparedReleasePayload<'_>,
    repo_root: &Path,
    commit: &str,
) -> ReleaseResult<()> {
    pending.write_new_file("vo.package.json", payload.package_bytes)?;
    pending.write_new_file("vo.release.json", payload.manifest_json.as_bytes())?;

    validate_staged_release(&pending, payload)?;
    pending.seal()?;
    validate_clean_tracked_worktree(repo_root, commit)?;
    validate_release_head(repo_root, commit)?;
    pending.publish()
}

fn validate_staged_release(
    stage_dir: &PendingOutputDir,
    payload: &PreparedReleasePayload<'_>,
) -> ReleaseResult<()> {
    let mut expected_names = BTreeSet::from([
        payload.source_name.to_string(),
        "vo.release.json".to_string(),
        "vo.package.json".to_string(),
    ]);
    for artifact in payload.artifacts {
        expected_names.insert(artifact.staged.asset_name.clone());
    }

    let found_names = stage_dir.entry_names()?;
    if found_names != expected_names {
        let missing = bounded_sorted_diagnostic(expected_names.difference(&found_names).cloned());
        let extra = bounded_sorted_diagnostic(found_names.difference(&expected_names).cloned());
        return Err(ReleaseError::IoError(
            stage_dir.path(),
            format!(
                "staged release asset set mismatch; missing [{}], extra [{}]",
                missing, extra
            ),
        ));
    }

    let source_path = stage_dir.path().join(payload.source_name);
    stage_dir.validate_file_digest(
        payload.source_name,
        vo_module::MAX_SOURCE_ARCHIVE_BYTES,
        payload.source_size,
        payload.source_digest,
    )?;
    stage_dir.with_file_reader(
        payload.source_name,
        vo_module::MAX_SOURCE_ARCHIVE_BYTES,
        payload.source_size,
        |source| validate_staged_source_package(&source_path, source, payload),
    )?;

    let manifest_path = stage_dir.path().join("vo.release.json");
    let staged_manifest_bytes =
        stage_dir.read_file("vo.release.json", vo_common::vfs::MAX_TEXT_FILE_BYTES)?;
    let expected_manifest_size = u64::try_from(payload.manifest_json.len()).map_err(|_| {
        ReleaseError::IoError(
            manifest_path.clone(),
            "staged vo.release.json size exceeds u64".to_string(),
        )
    })?;
    validate_staged_payload(
        &manifest_path,
        &staged_manifest_bytes,
        expected_manifest_size,
        payload.manifest_digest,
    )?;
    let staged_manifest = std::str::from_utf8(&staged_manifest_bytes).map_err(|error| {
        ReleaseError::IoError(
            manifest_path.clone(),
            format!("staged vo.release.json is not valid UTF-8: {error}"),
        )
    })?;
    let parsed_manifest = ReleaseManifest::parse(staged_manifest)
        .map_err(|error| ReleaseError::IoError(manifest_path.clone(), error.to_string()))?;
    let canonical_manifest = parsed_manifest
        .render()
        .map_err(|error| ReleaseError::IoError(manifest_path.clone(), error.to_string()))?;
    if canonical_manifest != payload.manifest_json {
        return Err(ReleaseError::IoError(
            manifest_path,
            "staged vo.release.json is not canonical".to_string(),
        ));
    }
    if parsed_manifest.source.name != payload.source_name
        || parsed_manifest.source.size != payload.source_size
        || parsed_manifest.source.digest.as_str() != payload.source_digest
    {
        return Err(ReleaseError::IoError(
            source_path.clone(),
            "vo.release.json source binding differs from the staged source asset".to_string(),
        ));
    }
    if parsed_manifest.artifacts.len() != payload.artifacts.len()
        || parsed_manifest
            .artifacts
            .iter()
            .zip(payload.artifacts)
            .any(|(manifest, prepared)| manifest != &prepared.manifest_artifact)
    {
        return Err(ReleaseError::IoError(
            stage_dir.path().join("vo.release.json"),
            "vo.release.json artifact bindings differ from the staged artifact set".to_string(),
        ));
    }

    let package_path = stage_dir.path().join("vo.package.json");
    let staged_package_bytes =
        stage_dir.read_file("vo.package.json", vo_common::vfs::MAX_TEXT_FILE_BYTES)?;
    validate_staged_payload(
        &package_path,
        &staged_package_bytes,
        parsed_manifest.package.size,
        parsed_manifest.package.digest.as_str(),
    )?;
    if staged_package_bytes != payload.package_bytes {
        return Err(ReleaseError::IoError(
            package_path.clone(),
            "staged vo.package.json bytes do not match the generated package manifest".to_string(),
        ));
    }
    let parsed_package = PackageManifest::parse(&staged_package_bytes)
        .map_err(|error| ReleaseError::IoError(package_path.clone(), error.to_string()))?;
    let canonical_package = parsed_package
        .render()
        .map_err(|error| ReleaseError::IoError(package_path.clone(), error.to_string()))?;
    if canonical_package != staged_package_bytes {
        return Err(ReleaseError::IoError(
            package_path,
            "staged vo.package.json is not canonical".to_string(),
        ));
    }

    for artifact in payload.artifacts {
        stage_dir.validate_file_digest(
            &artifact.staged.asset_name,
            vo_module::MAX_MODULE_ARTIFACT_BYTES,
            artifact.staged.size,
            &artifact.staged.digest,
        )?;
    }
    Ok(())
}

#[derive(Default)]
struct BoundedDiagnostic {
    first: Vec<String>,
    total: usize,
}

impl BoundedDiagnostic {
    fn push(&mut self, value: &str) {
        self.total = self.total.saturating_add(1);
        if self.first.len() < MAX_DIAGNOSTIC_ITEMS {
            self.first.push(value.to_string());
        }
    }

    fn render(&self) -> String {
        let shown = self.first.join(", ");
        let remaining = self.total.saturating_sub(self.first.len());
        if remaining == 0 {
            shown
        } else if shown.is_empty() {
            format!("and {remaining} more")
        } else {
            format!("{shown} (and {remaining} more)")
        }
    }

    fn is_empty(&self) -> bool {
        self.total == 0
    }
}

fn validate_package_snapshot_binding(
    source_path: &Path,
    payload: &PreparedReleasePayload<'_>,
) -> ReleaseResult<()> {
    let package = PackageManifest::parse(payload.package_bytes)
        .map_err(|error| ReleaseError::IoError(source_path.to_path_buf(), error.to_string()))?;
    if package.files.len() == payload.source_snapshot.files.len() {
        let mut matches = true;
        for (entry, source) in package.files.iter().zip(&payload.source_snapshot.files) {
            if entry.path != source.path
                || entry.mode != source_file_mode(source.mode)?
                || u64::try_from(source.bytes.len()).ok() != Some(entry.size)
                || entry.digest != ModDigest::from_sha256(&source.bytes)
            {
                matches = false;
                break;
            }
        }
        if matches {
            return Ok(());
        }
    }
    Err(ReleaseError::IoError(
        source_path.to_path_buf(),
        "vo.package.json does not describe the immutable source snapshot".to_string(),
    ))
}

fn read_archive_entry_matches<R: Read>(
    entry: &mut R,
    source_path: &Path,
    path: &str,
    advertised_size: usize,
    expected: Option<&[u8]>,
) -> ReleaseResult<bool> {
    let mut total = 0usize;
    let mut equal = expected.is_some_and(|bytes| bytes.len() == advertised_size);
    let mut buffer = [0_u8; 64 * 1024];
    loop {
        let read = entry.read(&mut buffer).map_err(|error| {
            ReleaseError::IoError(
                source_path.to_path_buf(),
                format!("failed to read source archive entry {path:?}: {error}"),
            )
        })?;
        if read == 0 {
            break;
        }
        if let Some(expected) = expected {
            let end = total.saturating_add(read);
            if end > expected.len() || expected[total..end] != buffer[..read] {
                equal = false;
            }
        }
        total = total.checked_add(read).ok_or_else(|| {
            ReleaseError::IoError(
                source_path.to_path_buf(),
                format!("source archive entry {path:?} read size overflow"),
            )
        })?;
    }
    if total != advertised_size {
        return Err(ReleaseError::IoError(
            source_path.to_path_buf(),
            format!(
                "source archive entry {path:?} advertised {advertised_size} bytes and yielded {total}"
            ),
        ));
    }
    Ok(equal)
}

fn canonical_gnu_long_name_header(path_size: usize) -> ReleaseResult<Header> {
    let path_size = u64::try_from(path_size).map_err(|_| {
        ReleaseError::ManifestSerialize("GNU long-name path size exceeds u64".to_string())
    })?;
    let payload_size = path_size.checked_add(1).ok_or_else(|| {
        ReleaseError::ManifestSerialize("GNU long-name payload size overflow".to_string())
    })?;
    let mut header = Header::new_gnu();
    let name = b"././@LongLink";
    header.as_gnu_mut().expect("new GNU header").name[..name.len()].copy_from_slice(name);
    header.set_mode(0o644);
    header.set_uid(0);
    header.set_gid(0);
    header.set_mtime(0);
    header.set_size(payload_size);
    header.set_entry_type(EntryType::GNULongName);
    header.set_cksum();
    Ok(header)
}

fn canonical_source_file_header(path: &str, size: u64, mode: u32) -> ReleaseResult<Header> {
    let mut header = Header::new_gnu();
    header.set_size(size);
    header.set_mode(mode);
    header.set_uid(0);
    header.set_gid(0);
    header.set_mtime(0);
    if path.len() <= header.as_old().name.len() {
        header.set_path(path).map_err(|error| {
            ReleaseError::ManifestSerialize(format!(
                "failed to construct canonical source archive header for {path:?}: {error}"
            ))
        })?;
    } else {
        let limit = header.as_old().name.len();
        let valid = std::str::from_utf8(&path.as_bytes()[..limit])
            .map(|_| limit)
            .unwrap_or_else(|error| error.valid_up_to());
        if valid == 0 {
            return Err(ReleaseError::ManifestSerialize(format!(
                "source archive path {path:?} has no complete UTF-8 scalar in its GNU header prefix"
            )));
        }
        header.as_gnu_mut().expect("new GNU header").name[..valid]
            .copy_from_slice(&path.as_bytes()[..valid]);
    }
    header.set_cksum();
    Ok(header)
}

fn read_canonical_gnu_long_name<R: Read>(
    entry: &mut R,
    header: &Header,
    source_path: &Path,
    max_payload_size: usize,
) -> ReleaseResult<String> {
    let advertised_size = usize::try_from(header.size().map_err(|error| {
        ReleaseError::IoError(
            source_path.to_path_buf(),
            format!("invalid GNU long-name size: {error}"),
        )
    })?)
    .map_err(|_| {
        ReleaseError::IoError(
            source_path.to_path_buf(),
            "GNU long-name payload size exceeds usize".to_string(),
        )
    })?;
    if advertised_size < 2 || advertised_size > max_payload_size {
        return Err(ReleaseError::IoError(
            source_path.to_path_buf(),
            format!(
                "GNU long-name payload is {advertised_size} bytes; expected 2..={max_payload_size}"
            ),
        ));
    }
    let expected_header = canonical_gnu_long_name_header(advertised_size - 1)?;
    if header.as_bytes() != expected_header.as_bytes() {
        return Err(ReleaseError::IoError(
            source_path.to_path_buf(),
            "source archive GNU long-name header is not canonical".to_string(),
        ));
    }
    let mut bytes = Vec::new();
    bytes.try_reserve_exact(advertised_size).map_err(|_| {
        ReleaseError::IoError(
            source_path.to_path_buf(),
            "failed to reserve bounded GNU long-name payload".to_string(),
        )
    })?;
    bytes.resize(advertised_size, 0);
    entry.read_exact(&mut bytes).map_err(|error| {
        ReleaseError::IoError(
            source_path.to_path_buf(),
            format!("failed to read GNU long-name payload: {error}"),
        )
    })?;
    let mut trailing = [0_u8; 1];
    if entry.read(&mut trailing).map_err(|error| {
        ReleaseError::IoError(
            source_path.to_path_buf(),
            format!("failed to finish GNU long-name payload: {error}"),
        )
    })? != 0
    {
        return Err(ReleaseError::IoError(
            source_path.to_path_buf(),
            "GNU long-name payload exceeds its advertised size".to_string(),
        ));
    }
    if bytes.last() != Some(&0) || bytes[..bytes.len() - 1].contains(&0) {
        return Err(ReleaseError::IoError(
            source_path.to_path_buf(),
            "GNU long-name payload must contain exactly one trailing NUL".to_string(),
        ));
    }
    bytes.pop();
    let path = String::from_utf8(bytes).map_err(|error| {
        ReleaseError::IoError(
            source_path.to_path_buf(),
            format!("GNU long-name path is not valid UTF-8: {error}"),
        )
    })?;
    if path.len() <= Header::new_gnu().as_old().name.len() {
        return Err(ReleaseError::IoError(
            source_path.to_path_buf(),
            "GNU long-name extension is noncanonical for a path of at most 100 bytes".to_string(),
        ));
    }
    Ok(path)
}

fn validate_staged_source_package(
    source_path: &Path,
    compressed: &mut impl Read,
    payload: &PreparedReleasePayload<'_>,
) -> ReleaseResult<()> {
    validate_package_snapshot_binding(source_path, payload)?;
    let decoder = GzDecoder::new(BufReader::new(compressed));
    let (tar_reader, padding_guard) = CanonicalTarReader::new(decoder);
    let mut archive = tar::Archive::new(tar_reader);
    let entries = archive.entries().map_err(|error| {
        ReleaseError::IoError(
            source_path.to_path_buf(),
            format!("invalid tar archive: {error}"),
        )
    })?;
    let mut entries = entries.raw(true);
    let mut paths = vo_module::schema::PortablePathSet::default();
    let mut total_size = 0usize;
    let mut entry_count = 0usize;
    let mut previous_path: Option<String> = None;
    let mut expected = Vec::new();
    expected
        .try_reserve(payload.source_snapshot.files.len().saturating_add(1))
        .map_err(|_| {
            ReleaseError::IoError(
                source_path.to_path_buf(),
                "failed to reserve source archive verification index".to_string(),
            )
        })?;
    for file in &payload.source_snapshot.files {
        expected.push((
            file.path.as_str(),
            file.bytes.as_slice(),
            source_archive_mode(source_file_mode(file.mode)?),
        ));
    }
    expected.push(("vo.package.json", payload.package_bytes, 0o644));
    expected.sort_unstable_by(|left, right| left.0.cmp(right.0));
    let mut expected_index = 0usize;
    let mut missing = BoundedDiagnostic::default();
    let mut extra = BoundedDiagnostic::default();
    let mut changed = BoundedDiagnostic::default();
    let max_long_name_payload = payload
        .source_top_dir
        .len()
        .checked_add(1)
        .and_then(|length| length.checked_add(vo_module::schema::MAX_PORTABLE_PATH_BYTES))
        .and_then(|length| length.checked_add(1))
        .ok_or_else(|| {
            ReleaseError::IoError(
                source_path.to_path_buf(),
                "GNU long-name payload bound overflow".to_string(),
            )
        })?;
    let mut pending_long_name: Option<String> = None;

    for entry in entries.by_ref() {
        let mut entry = entry.map_err(|error| {
            ReleaseError::IoError(
                source_path.to_path_buf(),
                format!("invalid tar entry: {error}"),
            )
        })?;
        let entry_type = entry.header().entry_type();
        if entry_type.is_gnu_longname() {
            if pending_long_name.is_some() {
                return Err(ReleaseError::IoError(
                    source_path.to_path_buf(),
                    "GNU long-name headers must be followed immediately by one regular file"
                        .to_string(),
                ));
            }
            let header = entry.header().clone();
            pending_long_name = Some(read_canonical_gnu_long_name(
                &mut entry,
                &header,
                source_path,
                max_long_name_payload,
            )?);
            padding_guard.expect_after(entry.size(), source_path)?;
            continue;
        }
        if !entry_type.is_file() {
            return Err(ReleaseError::IoError(
                source_path.to_path_buf(),
                format!(
                    "source archive rejects noncanonical tar entry type {entry_type:?}; only regular files and bounded GNU long-name headers are allowed"
                ),
            ));
        }
        if entry_count == vo_module::MAX_SOURCE_ARCHIVE_ENTRIES {
            return Err(ReleaseError::IoError(
                source_path.to_path_buf(),
                format!(
                    "source archive contains more than {} entries",
                    vo_module::MAX_SOURCE_ARCHIVE_ENTRIES
                ),
            ));
        }
        entry_count += 1;
        let used_long_name = pending_long_name.is_some();
        let archive_path = if let Some(long_name) = pending_long_name.take() {
            long_name
        } else {
            let bytes = entry.header().path_bytes();
            std::str::from_utf8(bytes.as_ref())
                .map_err(|error| {
                    ReleaseError::IoError(
                        source_path.to_path_buf(),
                        format!("source archive path is not valid UTF-8: {error}"),
                    )
                })?
                .to_string()
        };
        let relative = Path::new(&archive_path)
            .strip_prefix(Path::new(payload.source_top_dir))
            .map_err(|_| {
                ReleaseError::IoError(
                    source_path.to_path_buf(),
                    format!(
                        "source archive entry {} is outside the expected top directory {:?}",
                        archive_path, payload.source_top_dir
                    ),
                )
            })?;
        let path =
            vo_module::schema::portable_relative_path_from_path(relative).map_err(|error| {
                ReleaseError::IoError(
                    source_path.to_path_buf(),
                    format!("invalid source archive entry path: {error}"),
                )
            })?;
        let canonical_archive_path = format!("{}/{}", payload.source_top_dir, path);
        if archive_path != canonical_archive_path {
            return Err(ReleaseError::IoError(
                source_path.to_path_buf(),
                format!(
                    "source archive entry path {archive_path:?} is not canonical; expected {canonical_archive_path:?}"
                ),
            ));
        }
        if path != "vo.package.json"
            && !vo_module::schema::is_package_file_candidate(&path)
                .map_err(ReleaseError::ManifestSerialize)?
        {
            return Err(ReleaseError::IoError(
                source_path.to_path_buf(),
                format!("source archive entry {path:?} is reserved by the module protocol"),
            ));
        }
        if !paths
            .insert_file(&path)
            .map_err(ReleaseError::ManifestSerialize)?
        {
            return Err(ReleaseError::IoError(
                source_path.to_path_buf(),
                format!("source archive contains duplicate path {path:?}"),
            ));
        }
        if previous_path
            .as_deref()
            .is_some_and(|previous| previous >= path.as_str())
        {
            return Err(ReleaseError::IoError(
                source_path.to_path_buf(),
                format!(
                    "source archive entries must be strictly sorted; {path:?} follows {:?}",
                    previous_path.as_deref().unwrap_or_default()
                ),
            ));
        }
        previous_path = Some(path.clone());

        let archive_size = entry.size();
        let advertised_size = usize::try_from(archive_size).map_err(|_| {
            ReleaseError::IoError(
                source_path.to_path_buf(),
                format!("source archive entry {path:?} size exceeds usize"),
            )
        })?;
        if advertised_size > vo_module::MAX_SOURCE_ARCHIVE_ENTRY_BYTES {
            return Err(ReleaseError::IoError(
                source_path.to_path_buf(),
                format!(
                    "source archive entry {path:?} exceeds the {}-byte limit",
                    vo_module::MAX_SOURCE_ARCHIVE_ENTRY_BYTES
                ),
            ));
        }
        total_size = total_size.checked_add(advertised_size).ok_or_else(|| {
            ReleaseError::IoError(
                source_path.to_path_buf(),
                "source archive extracted size overflow".to_string(),
            )
        })?;
        if total_size > vo_module::MAX_EXTRACTED_SOURCE_BYTES {
            return Err(ReleaseError::IoError(
                source_path.to_path_buf(),
                format!(
                    "source archive exceeds the {}-byte extracted limit",
                    vo_module::MAX_EXTRACTED_SOURCE_BYTES
                ),
            ));
        }
        while expected_index < expected.len() && expected[expected_index].0 < path.as_str() {
            missing.push(expected[expected_index].0);
            expected_index += 1;
        }
        let expected_entry =
            if expected_index < expected.len() && expected[expected_index].0 == path.as_str() {
                let expected_entry = expected[expected_index];
                expected_index += 1;
                Some(expected_entry)
            } else {
                extra.push(&path);
                None
            };
        let archive_mode = entry.header().mode().map_err(|error| {
            ReleaseError::IoError(
                source_path.to_path_buf(),
                format!("invalid source archive entry {path:?} mode: {error}"),
            )
        })?;
        if !matches!(archive_mode, 0o644 | 0o755) {
            return Err(ReleaseError::IoError(
                source_path.to_path_buf(),
                format!(
                    "source archive entry {path:?} has noncanonical mode {archive_mode:#o}; expected 0o644 or 0o755"
                ),
            ));
        }
        if used_long_name != (archive_path.len() > Header::new_gnu().as_old().name.len()) {
            return Err(ReleaseError::IoError(
                source_path.to_path_buf(),
                format!(
                    "source archive entry {path:?} does not use the canonical GNU long-name form"
                ),
            ));
        }
        let expected_header =
            canonical_source_file_header(&archive_path, archive_size, archive_mode)?;
        if entry.header().as_bytes() != expected_header.as_bytes() {
            return Err(ReleaseError::IoError(
                source_path.to_path_buf(),
                format!("source archive entry {path:?} header is not canonical"),
            ));
        }
        if let Some((_, _, expected_mode)) = expected_entry {
            if archive_mode != expected_mode {
                return Err(ReleaseError::IoError(
                    source_path.to_path_buf(),
                    format!(
                        "source archive entry {path:?} mode mismatch: expected {expected_mode:#o}, found {archive_mode:#o}"
                    ),
                ));
            }
        }
        let expected_bytes = expected_entry.map(|(_, bytes, _)| bytes);
        if !read_archive_entry_matches(
            &mut entry,
            source_path,
            &path,
            advertised_size,
            expected_bytes,
        )? && expected_bytes.is_some()
        {
            changed.push(&path);
        }
        padding_guard.expect_after(archive_size, source_path)?;
    }
    if pending_long_name.is_some() {
        return Err(ReleaseError::IoError(
            source_path.to_path_buf(),
            "GNU long-name header is not followed by a regular file".to_string(),
        ));
    }
    let tar_reader = archive.into_inner();
    if padding_guard.remaining.get() != 0 {
        return Err(ReleaseError::IoError(
            source_path.to_path_buf(),
            "source archive ended before its final tar padding was consumed".to_string(),
        ));
    }
    let mut decoder = tar_reader.into_inner();
    let mut canonical_tar_end = [0_u8; 512];
    decoder
        .read_exact(&mut canonical_tar_end)
        .map_err(|error| {
            ReleaseError::IoError(
                source_path.to_path_buf(),
                format!("source archive is missing its canonical tar end block: {error}"),
            )
        })?;
    if canonical_tar_end.iter().any(|byte| *byte != 0) {
        return Err(ReleaseError::IoError(
            source_path.to_path_buf(),
            "source archive has nonzero bytes after the first tar end block".to_string(),
        ));
    }
    let mut trailing = [0_u8; 1];
    if decoder.read(&mut trailing).map_err(|error| {
        ReleaseError::IoError(
            source_path.to_path_buf(),
            format!("source archive gzip trailer is invalid: {error}"),
        )
    })? != 0
    {
        return Err(ReleaseError::IoError(
            source_path.to_path_buf(),
            "source archive has decompressed bytes after the canonical tar ending".to_string(),
        ));
    }
    let mut compressed_input = decoder.into_inner();
    if compressed_input.read(&mut trailing).map_err(|error| {
        ReleaseError::IoError(
            source_path.to_path_buf(),
            format!("failed to check source archive compressed closure: {error}"),
        )
    })? != 0
    {
        return Err(ReleaseError::IoError(
            source_path.to_path_buf(),
            "source archive contains a concatenated gzip member or trailing compressed bytes"
                .to_string(),
        ));
    }
    while expected_index < expected.len() {
        missing.push(expected[expected_index].0);
        expected_index += 1;
    }
    if missing.is_empty() && extra.is_empty() && changed.is_empty() {
        return Ok(());
    }
    Err(ReleaseError::IoError(
        source_path.to_path_buf(),
        format!(
            "source archive differs from the immutable snapshot; missing [{}], extra [{}], changed [{}]",
            missing.render(),
            extra.render(),
            changed.render(),
        ),
    ))
}

fn validate_staged_payload(
    path: &Path,
    bytes: &[u8],
    expected_size: u64,
    expected_digest: &str,
) -> ReleaseResult<()> {
    let found_size = u64::try_from(bytes.len()).map_err(|_| {
        ReleaseError::IoError(
            path.to_path_buf(),
            "release asset size exceeds u64".to_string(),
        )
    })?;
    if found_size != expected_size {
        return Err(ReleaseError::IoError(
            path.to_path_buf(),
            format!(
                "staged release asset size mismatch: expected {expected_size}, found {}",
                bytes.len()
            ),
        ));
    }
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let found_digest = format!("sha256:{:x}", hasher.finalize());
    if found_digest != expected_digest {
        return Err(ReleaseError::IoError(
            path.to_path_buf(),
            format!(
                "staged release asset digest mismatch: expected {expected_digest}, found {found_digest}"
            ),
        ));
    }
    Ok(())
}

fn infer_commit(repo_root: &Path) -> ReleaseResult<String> {
    let mut command = Command::new("git");
    command
        .arg("-C")
        .arg(repo_root)
        .arg("rev-parse")
        .arg("HEAD");
    let output = run_command_output_bounded(
        &mut command,
        repo_root,
        "git rev-parse HEAD",
        MAX_GIT_REVISION_OUTPUT_BYTES,
    )?;
    if !output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        let message = [stdout, stderr]
            .into_iter()
            .filter(|part| !part.is_empty())
            .collect::<Vec<_>>()
            .join("\n");
        return Err(ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: if message.is_empty() {
                format!("git rev-parse HEAD failed with status {}", output.status)
            } else {
                message
            },
        });
    }
    let commit = String::from_utf8(output.stdout).map_err(|error| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: format!("git returned a non-UTF-8 HEAD commit id: {error}"),
    })?;
    git_stdout_record(&commit, repo_root, "git rev-parse HEAD").map(str::to_string)
}

fn resolve_release_commit(repo_root: &Path, requested: Option<&str>) -> ReleaseResult<String> {
    let head = infer_commit(repo_root)?;
    validate_release_commit(&head)?;
    let candidate = requested.unwrap_or(&head);
    validate_release_commit(candidate)?;

    let revision = format!("{candidate}^{{commit}}");
    let mut command = Command::new("git");
    command
        .arg("-C")
        .arg(repo_root)
        .args(["rev-parse", "--verify", &revision]);
    let output = run_command_output_bounded(
        &mut command,
        repo_root,
        "git rev-parse --verify",
        MAX_GIT_REVISION_OUTPUT_BYTES,
    )?;
    if !output.status.success() {
        return Err(ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: format!(
                "release commit {candidate} does not identify an existing commit: {}",
                String::from_utf8_lossy(&output.stderr).trim(),
            ),
        });
    }
    let resolved = String::from_utf8(output.stdout).map_err(|error| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: format!("git returned a non-UTF-8 commit id: {error}"),
    })?;
    let resolved = git_stdout_record(&resolved, repo_root, "git rev-parse --verify")?;
    if resolved != candidate {
        return Err(ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: format!("release commit {candidate} resolved to unexpected commit {resolved}"),
        });
    }
    if candidate != head {
        return Err(ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: format!("release commit {candidate} must equal repository HEAD {head}"),
        });
    }
    Ok(candidate.to_string())
}

fn validate_clean_tracked_worktree(repo_root: &Path, commit: &str) -> ReleaseResult<()> {
    validate_git_diff_is_clean(
        repo_root,
        &["diff", "--quiet", "--cached", commit, "--", "."],
        "git index differs from the release commit",
    )?;
    validate_git_diff_is_clean(
        repo_root,
        &["diff", "--quiet", "--", "."],
        "tracked working-tree files differ from the git index",
    )
}

pub(crate) fn validate_release_head(repo_root: &Path, expected: &str) -> ReleaseResult<()> {
    let found = infer_commit(repo_root)?;
    validate_release_commit(&found)?;
    if found == expected {
        return Ok(());
    }
    Err(ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: format!(
            "repository HEAD changed from release commit {expected} to {found} while staging"
        ),
    })
}

fn validate_git_diff_is_clean(
    repo_root: &Path,
    args: &[&str],
    dirty_message: &str,
) -> ReleaseResult<()> {
    let status = Command::new("git")
        .arg("-C")
        .arg(repo_root)
        .args(args)
        .status()
        .map_err(|error| ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: error.to_string(),
        })?;
    match status.code() {
        Some(0) => Ok(()),
        Some(1) => Err(ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: format!(
                "{dirty_message}; commit or restore tracked changes before staging a release"
            ),
        }),
        _ => Err(ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: format!("git {:?} failed with status {status}", args),
        }),
    }
}

fn validate_release_commit(commit: &str) -> ReleaseResult<()> {
    if commit.len() != 40
        || !commit
            .chars()
            .all(|character| character.is_ascii_hexdigit() && !character.is_ascii_uppercase())
    {
        return Err(ReleaseError::ManifestSerialize(
            "commit must be exactly 40 lowercase hexadecimal characters".to_string(),
        ));
    }
    Ok(())
}

fn list_git_tree_entries(
    repo_root: &Path,
    repository_root: &Path,
    treeish: &str,
    walked_records: &mut usize,
    walked_bytes: &mut usize,
) -> ReleaseResult<Vec<GitTreeEntry>> {
    let remaining_records = MAX_GIT_TREE_WALK_RECORDS
        .checked_sub(*walked_records)
        .ok_or_else(|| {
            ReleaseError::ManifestSerialize(
                "release Git tree walk record budget is exhausted".to_string(),
            )
        })?;
    let remaining_bytes = MAX_GIT_TREE_WALK_BYTES
        .checked_sub(*walked_bytes)
        .ok_or_else(|| {
            ReleaseError::ManifestSerialize(
                "release Git tree walk byte budget is exhausted".to_string(),
            )
        })?;
    if remaining_records == 0 || remaining_bytes == 0 {
        return Err(ReleaseError::ManifestSerialize(
            "release Git tree walk budget is exhausted".to_string(),
        ));
    }

    let mut command = Command::new("git");
    command
        .arg("-C")
        .arg(repository_root)
        .args(["ls-tree", "-z", treeish]);
    let mut entries = Vec::new();
    run_git_nul_records_with_limits(
        &mut command,
        repo_root,
        "git ls-tree directory walk",
        remaining_records,
        remaining_bytes,
        |raw_record| {
            *walked_records = walked_records.checked_add(1).ok_or_else(|| {
                ReleaseError::ManifestSerialize(
                    "release Git tree walk record count overflow".to_string(),
                )
            })?;
            *walked_bytes = walked_bytes
                .checked_add(raw_record.len().saturating_add(1))
                .ok_or_else(|| {
                    ReleaseError::ManifestSerialize(
                        "release Git tree walk byte count overflow".to_string(),
                    )
                })?;
            let tab = raw_record
                .iter()
                .position(|byte| *byte == b'\t')
                .ok_or_else(|| ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: "git ls-tree returned a malformed entry".to_string(),
                })?;
            let header = std::str::from_utf8(&raw_record[..tab]).map_err(|error| {
                ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("git ls-tree returned a non-UTF-8 header: {error}"),
                }
            })?;
            let name = std::str::from_utf8(&raw_record[tab + 1..]).map_err(|error| {
                ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("release commit contains a non-UTF-8 path: {error}"),
                }
            })?;
            if name.is_empty() || name.contains('/') {
                return Err(ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("git ls-tree returned invalid direct child name {name:?}"),
                });
            }
            vo_module::schema::validate_portable_relative_path(name).map_err(|error| {
                ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!(
                        "release commit path component {name:?} is not portable: {error}"
                    ),
                }
            })?;
            let mut fields = header.split_whitespace();
            let git_mode = fields.next().unwrap_or_default();
            let git_kind = fields.next().unwrap_or_default();
            let object_id = fields.next().unwrap_or_default();
            if fields.next().is_some()
                || !object_id.bytes().all(|byte| byte.is_ascii_hexdigit())
                || !matches!(object_id.len(), 40 | 64)
            {
                return Err(ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("git ls-tree returned malformed metadata for {name:?}"),
                });
            }
            let archive_mode = match (git_mode, git_kind) {
                ("100644", "blob") => Some(0o644),
                ("100755", "blob") => Some(0o755),
                _ => None,
            };
            entries.try_reserve(1).map_err(|_| {
                ReleaseError::ManifestSerialize(
                    "failed to reserve release Git tree entries".to_string(),
                )
            })?;
            entries.push(GitTreeEntry {
                name: name.to_string(),
                object_id: object_id.to_string(),
                git_mode: git_mode.to_string(),
                git_kind: git_kind.to_string(),
                archive_mode,
            });
            Ok(())
        },
    )?;
    Ok(entries)
}

fn walk_commit_tree(
    repo_root: &Path,
    repository_root: &Path,
    module_prefix: &str,
    commit: &str,
) -> ReleaseResult<BTreeMap<String, CommitTreeFile>> {
    let root_treeish = if module_prefix.is_empty() {
        format!("{commit}^{{tree}}")
    } else {
        format!("{commit}:{module_prefix}")
    };
    let mut pending_directories = vec![(String::new(), root_treeish)];
    let mut walked_requests = 0usize;
    let mut walked_records = 0usize;
    let mut walked_bytes = 0usize;
    let mut portable_paths = vo_module::schema::PortablePathSet::default();
    let mut boundary_spellings = BTreeMap::new();
    let mut files = BTreeMap::new();

    while let Some((directory, treeish)) = pending_directories.pop() {
        walked_requests = walked_requests.checked_add(1).ok_or_else(|| {
            ReleaseError::ManifestSerialize(
                "release Git tree walk request count overflow".to_string(),
            )
        })?;
        if walked_requests > MAX_GIT_TREE_WALK_REQUESTS {
            return Err(ReleaseError::ManifestSerialize(format!(
                "release Git tree walk requires more than {MAX_GIT_TREE_WALK_REQUESTS} directory requests"
            )));
        }
        let entries = list_git_tree_entries(
            repo_root,
            repository_root,
            &treeish,
            &mut walked_records,
            &mut walked_bytes,
        )?;

        let mut boundary = false;
        for entry in &entries {
            if !portable_name_eq(&entry.name, "vo.mod") {
                continue;
            }
            let marker_path = if directory.is_empty() {
                entry.name.clone()
            } else {
                format!("{directory}/{}", entry.name)
            };
            if entry.name != "vo.mod" {
                let scope = if directory.is_empty() {
                    "root"
                } else {
                    "nested"
                };
                return Err(ReleaseError::ManifestSerialize(format!(
                    "{scope} module manifest {marker_path:?} is a portable alias of vo.mod; module boundaries must use the exact basename \"vo.mod\""
                )));
            }
            if directory.is_empty() {
                continue;
            }
            if entry.archive_mode.is_none() {
                return Err(ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!(
                        "nested module manifest {marker_path:?} has unsupported git kind {:?} and mode {:?}; a module boundary must be a regular vo.mod blob",
                        entry.git_kind, entry.git_mode,
                    ),
                });
            }
            let key = vo_module::schema::portable_case_key(&marker_path);
            if let Some(previous) = boundary_spellings.insert(key, marker_path.clone()) {
                return Err(ReleaseError::ManifestSerialize(format!(
                    "nested module boundaries {previous:?} and {marker_path:?} conflict under portable spelling rules"
                )));
            }
            boundary = true;
        }
        if boundary {
            continue;
        }

        for entry in entries.into_iter().rev() {
            let module_path = if directory.is_empty() {
                entry.name.clone()
            } else {
                format!("{directory}/{}", entry.name)
            };
            vo_module::schema::validate_portable_relative_path(&module_path).map_err(|error| {
                ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("module source path {module_path:?} is not portable: {error}"),
                }
            })?;
            if entry.git_mode == "040000" && entry.git_kind == "tree" {
                pending_directories.push((module_path, entry.object_id));
                continue;
            }
            let mode = entry.archive_mode.ok_or_else(|| ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: format!(
                    "release commit entry {module_path:?} has unsupported git kind {:?} and mode {:?}; only regular files are publishable",
                    entry.git_kind, entry.git_mode,
                ),
            })?;
            if files.len() >= vo_common::vfs::MAX_DIRECTORY_ENTRIES {
                return Err(ReleaseError::ManifestSerialize(format!(
                    "release commit tree contains more than {} entries inside the module boundary",
                    vo_common::vfs::MAX_DIRECTORY_ENTRIES
                )));
            }
            if !insert_release_source_path(
                &mut portable_paths,
                &module_path,
                "release commit tree",
            )? {
                return Err(ReleaseError::ManifestSerialize(format!(
                    "release commit contains duplicate source path {module_path:?}"
                )));
            }
            let repository_path = if module_prefix.is_empty() {
                module_path.clone()
            } else {
                format!("{module_prefix}/{module_path}")
            };
            files.insert(
                module_path.clone(),
                CommitTreeFile {
                    repository_path,
                    module_path,
                    object_id: entry.object_id,
                    mode,
                },
            );
        }
    }
    Ok(files)
}

pub(crate) fn load_commit_tree(repo_root: &Path, commit: &str) -> ReleaseResult<CommitTree> {
    let mut root_command = Command::new("git");
    root_command
        .arg("-C")
        .arg(repo_root)
        .args(["rev-parse", "--show-toplevel"]);
    let output = run_command_output_bounded(
        &mut root_command,
        repo_root,
        "git rev-parse --show-toplevel",
        vo_module::schema::MAX_PORTABLE_PATH_BYTES,
    )?;
    if !output.status.success() {
        return Err(ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: String::from_utf8_lossy(&output.stderr).trim().to_string(),
        });
    }
    let repository_root =
        git_stdout_path_record(output.stdout, repo_root, "git rev-parse --show-toplevel")?;
    let repository_root = fs::canonicalize(&repository_root).map_err(|error| {
        ReleaseError::IoError(
            repo_root.to_path_buf(),
            format!("invalid git root: {error}"),
        )
    })?;
    let module_relative =
        repo_root
            .strip_prefix(&repository_root)
            .map_err(|_| ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: format!(
                    "module root is outside git repository {}",
                    repository_root.display()
                ),
            })?;
    let module_prefix = if module_relative.as_os_str().is_empty() {
        String::new()
    } else {
        vo_module::schema::portable_relative_path_from_path(module_relative).map_err(|error| {
            ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: format!("module root is not a portable git path: {error}"),
            }
        })?
    };

    let files = walk_commit_tree(repo_root, &repository_root, &module_prefix, commit)?;
    Ok(CommitTree {
        repository_root,
        module_prefix,
        files,
    })
}

fn insert_release_source_path(
    paths: &mut vo_module::schema::PortablePathSet,
    path: &str,
    context: &str,
) -> ReleaseResult<bool> {
    paths.insert_file(path).map_err(|error| {
        ReleaseError::ManifestSerialize(format!(
            "{context} path {path:?} violates the portable source-path budget: {error}"
        ))
    })
}

fn validate_module_root_location(
    repo_root: &Path,
    expected_module_dir: &str,
    tree: &CommitTree,
) -> ReleaseResult<()> {
    let actual = if tree.module_prefix.is_empty() {
        "."
    } else {
        tree.module_prefix.as_str()
    };
    if actual == expected_module_dir {
        return Ok(());
    }
    Err(ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: format!(
            "the vo.mod module path implies repository subdirectory {expected_module_dir:?}, but the staged module directory is {actual:?}"
        ),
    })
}

pub(crate) fn read_committed_mod_file(
    repo_root: &Path,
    tree: &CommitTree,
) -> ReleaseResult<ModFile> {
    let tracked = commit_tree_paths(tree);
    validate_exact_tracked_root_file(&tracked, "vo.mod", true)?;
    let tree_file = tree.files.get("vo.mod").ok_or_else(|| {
        ReleaseError::ManifestSerialize(
            "release commit must contain the exact module-root path vo.mod".to_string(),
        )
    })?;
    let mut blobs = read_commit_blobs(
        repo_root,
        &tree.repository_root,
        std::slice::from_ref(tree_file),
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
    )?;
    let bytes = blobs.pop().ok_or_else(|| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: "git returned no blob for committed vo.mod".to_string(),
    })?;
    let content = std::str::from_utf8(&bytes).map_err(|error| {
        ReleaseError::ManifestSerialize(format!("committed vo.mod is not valid UTF-8: {error}"))
    })?;
    ModFile::parse(content).map_err(ReleaseError::from)
}

fn validate_committed_repo(
    repo_root: &Path,
    tree: &CommitTree,
    mod_file: &ModFile,
) -> ReleaseResult<()> {
    let tracked = commit_tree_paths(tree);
    validate_exact_tracked_root_file(&tracked, "vo.mod", true)?;
    validate_exact_tracked_root_file(&tracked, "vo.lock", !mod_file.dependencies.is_empty())?;

    let mut fs = vo_common::vfs::MemoryFs::new();
    let mod_bytes = read_committed_file(repo_root, tree, "vo.mod")?.ok_or_else(|| {
        ReleaseError::ManifestSerialize(
            "release commit must contain the exact module-root path vo.mod".to_string(),
        )
    })?;
    fs.add_bytes("vo.mod", mod_bytes);
    if let Some(lock_bytes) = read_committed_file(repo_root, tree, "vo.lock")? {
        fs.add_bytes("vo.lock", lock_bytes);
    }
    project::read_project_deps(&fs).map_err(map_project_deps_error_for_release_verify)?;
    Ok(())
}

fn commit_tree_paths(tree: &CommitTree) -> HashSet<PathBuf> {
    tree.files.keys().map(PathBuf::from).collect()
}

fn validate_local_build_inputs(
    repo_root: &Path,
    mod_file: &ModFile,
    tree: &CommitTree,
) -> ReleaseResult<()> {
    let Some(extension) = &mod_file.extension else {
        return Ok(());
    };
    if let Some(native) = extension.native_build() {
        match native {
            NativeBuildManifest::Cargo { manifest, .. } => {
                validate_local_build_input(repo_root, manifest, true, tree)?;
            }
            NativeBuildManifest::Prebuilt { path } => {
                validate_local_build_input(repo_root, path, false, tree)?;
            }
        }
    }
    if let Some(build) = extension
        .build
        .as_ref()
        .and_then(|build| build.wasm.as_ref())
    {
        validate_local_build_input(repo_root, &build.wasm, false, tree)?;
        if let Some(js) = &build.js {
            validate_local_build_input(repo_root, js, false, tree)?;
        }
    }
    Ok(())
}

fn validate_local_build_input(
    repo_root: &Path,
    relative: &str,
    must_be_committed: bool,
    tree: &CommitTree,
) -> ReleaseResult<()> {
    if must_be_committed && !tree.files.contains_key(relative) {
        return Err(ReleaseError::ManifestSerialize(format!(
            "local build input {relative:?} must be a regular file in the release commit"
        )));
    }
    let path = repo_root.join(relative);
    let mut current = repo_root.to_path_buf();
    let components = Path::new(relative).components().collect::<Vec<_>>();
    if components.is_empty() {
        return Err(ReleaseError::IoError(
            path,
            "local build input path must not be empty".to_string(),
        ));
    }
    for (index, component) in components.iter().enumerate() {
        let Component::Normal(component) = component else {
            return Err(ReleaseError::IoError(
                path.clone(),
                "local build input must be a normalized module-relative path".to_string(),
            ));
        };
        current.push(component);
        let metadata = fs::symlink_metadata(&current)
            .map_err(|error| ReleaseError::IoError(current.clone(), error.to_string()))?;
        if metadata.file_type().is_symlink() {
            return Err(ReleaseError::IoError(
                current,
                "local build input path must not contain a symbolic link".to_string(),
            ));
        }
        let is_final = index + 1 == components.len();
        if is_final {
            if !metadata.file_type().is_file() {
                return Err(ReleaseError::IoError(
                    current,
                    "local build input must be a regular file".to_string(),
                ));
            }
            if metadata.len() == 0 || metadata.len() > vo_module::MAX_MODULE_ARTIFACT_BYTES {
                return Err(ReleaseError::IoError(
                    current,
                    format!(
                        "local build input must contain 1..={} bytes",
                        vo_module::MAX_MODULE_ARTIFACT_BYTES
                    ),
                ));
            }
        } else if !metadata.file_type().is_dir() {
            return Err(ReleaseError::IoError(
                current,
                "local build input parent must be a directory".to_string(),
            ));
        }
    }
    Ok(())
}

fn read_committed_file(
    repo_root: &Path,
    tree: &CommitTree,
    path: &str,
) -> ReleaseResult<Option<Vec<u8>>> {
    let Some(file) = tree.files.get(path) else {
        return Ok(None);
    };
    let mut blobs = read_commit_blobs(
        repo_root,
        &tree.repository_root,
        std::slice::from_ref(file),
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
    )?;
    Ok(blobs.pop())
}

fn read_commit_blobs(
    repo_root: &Path,
    repository_root: &Path,
    files: &[CommitTreeFile],
    max_entry_bytes: usize,
    max_total_bytes: usize,
) -> ReleaseResult<Vec<Vec<u8>>> {
    let mut child = Command::new("git")
        .arg("-C")
        .arg(repository_root)
        .args(["cat-file", "--batch"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|error| ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: error.to_string(),
        })?;
    let mut stdin = child.stdin.take().ok_or_else(|| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: "git cat-file did not expose its configured stdin pipe".to_string(),
    })?;
    let stdout = child.stdout.take().ok_or_else(|| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: "git cat-file did not expose its configured stdout pipe".to_string(),
    })?;
    let mut stdout = BufReader::new(stdout);
    let stderr = child.stderr.take().ok_or_else(|| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: "git cat-file did not expose its configured stderr pipe".to_string(),
    })?;
    let stderr_reader =
        std::thread::spawn(move || read_pipe_bounded(stderr, MAX_GIT_STDERR_BYTES, "git stderr"));
    let result = (|| {
        let mut blobs = Vec::new();
        blobs.try_reserve(files.len()).map_err(|_| {
            ReleaseError::ManifestSerialize("failed to reserve release source blobs".to_string())
        })?;
        let mut total_bytes = 0usize;
        for file in files {
            stdin
                .write_all(file.object_id.as_bytes())
                .and_then(|_| stdin.write_all(b"\n"))
                .and_then(|_| stdin.flush())
                .map_err(|error| ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("failed to request git blob {}: {error}", file.object_id),
                })?;

            let mut header = Vec::new();
            let found_header = read_delimited_record_bounded(&mut stdout, b'\n', 255, &mut header)
                .map_err(|error| ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("failed to read git blob header: {error}"),
                })?;
            if !found_header {
                return Err(ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!(
                        "git returned a malformed blob header for {:?}",
                        file.module_path
                    ),
                });
            }
            let header = std::str::from_utf8(&header).map_err(|error| ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: format!("git returned a non-UTF-8 blob header: {error}"),
            })?;
            let mut fields = header.split_whitespace();
            let object_id = fields.next().unwrap_or_default();
            let kind = fields.next().unwrap_or_default();
            let size = fields.next().unwrap_or_default();
            if fields.next().is_some() || object_id != file.object_id || kind != "blob" {
                return Err(ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!(
                        "git returned an unexpected blob header for {:?}",
                        file.module_path
                    ),
                });
            }
            let size = size
                .parse::<usize>()
                .map_err(|error| ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!(
                        "git returned an invalid blob size for {:?}: {error}",
                        file.module_path
                    ),
                })?;
            let entry_limit = if file.module_path == "vo.mod"
                || Path::new(&file.module_path)
                    .extension()
                    .and_then(OsStr::to_str)
                    == Some("vo")
            {
                max_entry_bytes.min(vo_common::vfs::MAX_TEXT_FILE_BYTES)
            } else {
                max_entry_bytes
            };
            if size > entry_limit {
                return Err(ReleaseError::ManifestSerialize(format!(
                    "source file {:?} exceeds the {}-byte entry limit",
                    file.module_path, entry_limit
                )));
            }
            total_bytes = total_bytes.checked_add(size).ok_or_else(|| {
                ReleaseError::ManifestSerialize("release source size overflow".to_string())
            })?;
            if total_bytes > max_total_bytes {
                return Err(ReleaseError::ManifestSerialize(format!(
                    "release source files exceed the {max_total_bytes}-byte total limit"
                )));
            }
            let mut bytes = Vec::new();
            bytes.try_reserve_exact(size).map_err(|_| {
                ReleaseError::ManifestSerialize(format!(
                    "failed to reserve source bytes for {:?}",
                    file.module_path
                ))
            })?;
            bytes.resize(size, 0);
            stdout
                .read_exact(&mut bytes)
                .map_err(|error| ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!(
                        "failed to read git blob for {:?}: {error}",
                        file.module_path
                    ),
                })?;
            let mut separator = [0u8; 1];
            stdout
                .read_exact(&mut separator)
                .map_err(|error| ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("git blob for {:?} is truncated: {error}", file.module_path),
                })?;
            if separator != [b'\n'] {
                return Err(ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!(
                        "git blob for {:?} has an invalid terminator",
                        file.module_path
                    ),
                });
            }
            blobs.push(bytes);
        }
        Ok(blobs)
    })();
    drop(stdin);
    drop(stdout);
    if result.is_err() {
        let _ = child.kill();
    }
    let status_result = child.wait().map_err(|error| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: error.to_string(),
    });
    let stderr_result = join_bounded_git_pipe(stderr_reader, repo_root, "git stderr");
    let blobs = result?;
    let status = status_result?;
    let stderr = stderr_result?;
    if !status.success() {
        return Err(ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: String::from_utf8_lossy(&stderr).trim().to_string(),
        });
    }
    Ok(blobs)
}

fn prepare_artifacts(
    inputs: &[ArtifactInput],
    out_dir: &Path,
    reserved_output_names: &[&str],
    pending: &mut PendingOutputDir,
) -> ReleaseResult<Vec<PreparedArtifact>> {
    if inputs.len() > vo_module::MAX_MODULE_ARTIFACTS {
        return Err(ReleaseError::ManifestSerialize(format!(
            "release declares {} artifacts, exceeding the {}-artifact limit",
            inputs.len(),
            vo_module::MAX_MODULE_ARTIFACTS
        )));
    }
    let mut seen_ids = BTreeSet::new();
    let mut output_names = vo_module::schema::PortablePathSet::default();
    for name in reserved_output_names {
        output_names
            .insert_file(name)
            .map_err(ReleaseError::ManifestSerialize)?;
    }
    let mut plans = Vec::new();
    plans.try_reserve(inputs.len()).map_err(|_| {
        ReleaseError::ManifestSerialize("failed to reserve release artifact plans".into())
    })?;
    for input in inputs {
        if !input.path.is_file() {
            return Err(ReleaseError::InvalidArtifactPath(input.path.clone()));
        }
        let id = ArtifactId {
            kind: input.kind.clone(),
            target: input.target.clone(),
            name: input.name.clone(),
        };
        id.validate().map_err(ReleaseError::ManifestSerialize)?;
        if !seen_ids.insert(id.clone()) {
            return Err(ReleaseError::ManifestSerialize(format!(
                "release declares duplicate artifact identity ({}, {}, {})",
                id.kind, id.target, id.name
            )));
        }
        let asset_name = vo_module::artifact::artifact_release_asset_name(&id)
            .map_err(ReleaseError::ManifestSerialize)?;
        if !output_names
            .insert_file(&asset_name)
            .map_err(ReleaseError::ManifestSerialize)?
        {
            return Err(ReleaseError::ManifestSerialize(format!(
                "release artifact identity ({}, {}, {}) reuses output asset {asset_name}",
                id.kind, id.target, id.name
            )));
        }
        plans.push((id, input, asset_name));
    }
    plans.sort_by(|left, right| left.0.cmp(&right.0));

    let mut prepared = Vec::new();
    prepared.try_reserve(plans.len()).map_err(|_| {
        ReleaseError::ManifestSerialize("failed to reserve staged artifacts".into())
    })?;
    for (id, input, asset_name) in plans {
        let (size, digest) = pending.copy_new_file(
            &asset_name,
            &input.path,
            vo_module::MAX_MODULE_ARTIFACT_BYTES,
        )?;
        let output_path = out_dir.join(&asset_name);
        let digest_typed = ModDigest::parse(&digest).map_err(|e| {
            ReleaseError::IoError(input.path.clone(), format!("invalid digest: {e}"))
        })?;
        prepared.push(PreparedArtifact {
            staged: StagedArtifact {
                kind: input.kind.clone(),
                target: input.target.clone(),
                name: input.name.clone(),
                asset_name,
                size,
                digest: digest.clone(),
                source_path: input.path.clone(),
                output_path,
            },
            manifest_artifact: ManifestArtifact {
                id,
                size,
                digest: digest_typed,
            },
        });
    }
    Ok(prepared)
}

fn validate_artifact_contract(
    repo_root: &Path,
    mod_file: &ModFile,
    prepared: &[PreparedArtifact],
) -> ReleaseResult<()> {
    let manifest_path = repo_root.join("vo.mod");
    let declared = mod_file
        .extension
        .as_ref()
        .map(vo_module::ext_manifest::ExtensionManifest::declared_artifact_ids)
        .unwrap_or_default();

    let declared = declared.into_iter().collect::<BTreeSet<_>>();
    let staged = prepared
        .iter()
        .map(|artifact| vo_module::ext_manifest::DeclaredArtifactId {
            kind: artifact.staged.kind.clone(),
            target: artifact.staged.target.clone(),
            name: artifact.staged.name.clone(),
        })
        .collect::<BTreeSet<_>>();

    let missing = declared.difference(&staged).cloned().collect::<Vec<_>>();
    let undeclared = staged.difference(&declared).cloned().collect::<Vec<_>>();

    if missing.is_empty() && undeclared.is_empty() {
        return Ok(());
    }

    Err(ReleaseError::ArtifactContractViolation {
        manifest_path: Some(manifest_path),
        missing,
        undeclared,
    })
}

pub(crate) fn collect_release_source_files(
    repo_root: &Path,
    mod_file: &ModFile,
    commit_tree: &CommitTree,
) -> ReleaseResult<Vec<SelectedSourceFile>> {
    let mut portable_paths = vo_module::schema::PortablePathSet::default();
    let mut selected = BTreeSet::new();
    for rel in commit_tree.files.keys() {
        let at_root = !rel.contains('/');
        let root_key = at_root.then(|| vo_module::schema::portable_case_key(rel));
        if root_key.as_deref() == Some("vo.lock") {
            if rel != "vo.lock" {
                return Err(ReleaseError::ManifestSerialize(format!(
                    "root protocol path {rel:?} is a portable alias of vo.lock"
                )));
            }
            continue;
        }
        if root_key.as_deref() == Some("vo.work") {
            if rel != "vo.work" {
                return Err(ReleaseError::ManifestSerialize(format!(
                    "root protocol path {rel:?} is a portable alias of vo.work"
                )));
            }
            continue;
        }
        if root_key.as_deref().is_some_and(|key| {
            matches!(
                key,
                "vo.sum" | "vo.web.json" | ".vo-project.lock" | ".vo-project.transaction"
            )
        }) {
            return Err(ReleaseError::ManifestSerialize(format!(
                "root project protocol state {rel:?} must be removed before release"
            )));
        }
        if vo_module::schema::is_reserved_module_cache_path(rel) {
            return Err(ReleaseError::ManifestSerialize(format!(
                "tracked source path {rel:?} is reserved for generated package/cache state"
            )));
        }
        insert_release_source_path(&mut portable_paths, rel, "release source set")?;
        selected.insert(rel.clone());
    }
    if !selected.contains("vo.mod") {
        return Err(ReleaseError::ManifestSerialize(
            "release source closure must contain the exact root vo.mod".to_string(),
        ));
    }
    validate_runtime_source_references(repo_root, mod_file, &selected)?;
    if selected.len() >= vo_module::MAX_SOURCE_ARCHIVE_ENTRIES {
        return Err(ReleaseError::ManifestSerialize(format!(
            "release source set leaves no room for vo.package.json within the {}-entry archive limit",
            vo_module::MAX_SOURCE_ARCHIVE_ENTRIES
        )));
    }
    insert_release_source_path(&mut portable_paths, "vo.package.json", "release source set")?;
    selected
        .into_iter()
        .map(|path| {
            let tree_file =
                commit_tree
                    .files
                    .get(&path)
                    .cloned()
                    .ok_or_else(|| ReleaseError::GitError {
                        repo_root: repo_root.to_path_buf(),
                        message: format!(
                            "selected source path {path:?} is absent from commit tree {}",
                            commit_tree.repository_root.display()
                        ),
                    })?;
            Ok(SelectedSourceFile { tree_file })
        })
        .collect()
}

fn validate_runtime_source_references(
    repo_root: &Path,
    mod_file: &ModFile,
    selected: &BTreeSet<String>,
) -> ReleaseResult<()> {
    let require_source = |path: &str, field: &str| {
        if selected.contains(path) {
            return Ok(());
        }
        Err(ReleaseError::IoError(
            repo_root.join("vo.mod"),
            format!(
                "{field} references {path:?}, which is outside the tracked module source closure"
            ),
        ))
    };
    if let Some(entry) = mod_file.web.as_ref().and_then(|web| web.entry.as_deref()) {
        require_source(entry, "[web].entry")?;
    }
    if let Some(web) = mod_file
        .extension
        .as_ref()
        .and_then(|extension| extension.web.as_ref())
    {
        for (name, path) in &web.js_modules {
            require_source(path, &format!("[extension.web.js].{name}"))?;
        }
    }
    Ok(())
}

pub(crate) fn capture_release_source_snapshot(
    repo_root: &Path,
    files: &[SelectedSourceFile],
    commit_tree: &CommitTree,
) -> ReleaseResult<ReleaseSourceSnapshot> {
    let tree_files = files
        .iter()
        .map(|file| file.tree_file.clone())
        .collect::<Vec<_>>();
    let blobs = read_commit_blobs(
        repo_root,
        &commit_tree.repository_root,
        &tree_files,
        vo_module::MAX_SOURCE_ARCHIVE_ENTRY_BYTES,
        vo_module::MAX_EXTRACTED_SOURCE_BYTES,
    )?;
    let mut snapshot_files = Vec::new();
    snapshot_files.try_reserve(files.len()).map_err(|_| {
        ReleaseError::ManifestSerialize("failed to reserve release source snapshot".to_string())
    })?;
    let mut extracted_size = 0usize;
    for (selected, bytes) in files.iter().zip(blobs) {
        if Path::new(&selected.tree_file.module_path)
            .extension()
            .and_then(OsStr::to_str)
            == Some("vo")
        {
            if bytes.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
                return Err(ReleaseError::ManifestSerialize(format!(
                    "Vo source {:?} is {} bytes, exceeding the {}-byte compiler text limit",
                    selected.tree_file.module_path,
                    bytes.len(),
                    vo_common::vfs::MAX_TEXT_FILE_BYTES,
                )));
            }
            std::str::from_utf8(&bytes).map_err(|error| {
                ReleaseError::ManifestSerialize(format!(
                    "Vo source {:?} in commit tree path {:?} is not valid UTF-8: {error}",
                    selected.tree_file.module_path, selected.tree_file.repository_path,
                ))
            })?;
        }
        extracted_size = extracted_size.checked_add(bytes.len()).ok_or_else(|| {
            ReleaseError::ManifestSerialize("release source size overflow".to_string())
        })?;
        snapshot_files.push(ReleaseSourceFile {
            path: selected.tree_file.module_path.clone(),
            bytes,
            mode: selected.tree_file.mode,
        });
    }
    Ok(ReleaseSourceSnapshot {
        files: snapshot_files,
        extracted_size,
    })
}

fn validate_release_source_snapshot(
    repo_root: &Path,
    snapshot: &ReleaseSourceSnapshot,
) -> ReleaseResult<()> {
    let mut vo_sum_paths = Vec::new();
    let mut alias_imports = Vec::new();
    for file in &snapshot.files {
        let path = Path::new(&file.path);
        let root_name = if path
            .parent()
            .is_some_and(|parent| parent.as_os_str().is_empty())
        {
            path.file_name().and_then(OsStr::to_str)
        } else {
            None
        };
        if let Some(name) = root_name {
            if portable_name_eq(name, "vo.sum") {
                vo_sum_paths.push(path.to_path_buf());
            }
            if portable_name_matches_any(name, &[".vo-project.lock", ".vo-project.transaction"]) {
                return Err(ReleaseError::ManifestSerialize(format!(
                    "generated root project transaction state {:?} must not enter a module release",
                    file.path
                )));
            }
            if portable_name_eq(name, "vo.work") {
                return Err(ReleaseError::ManifestSerialize(
                    "the root workspace file vo.work must not enter a module release".to_string(),
                ));
            }
            if portable_name_eq(name, "vo.release.json")
                || portable_name_eq(name, "vo.package.json")
            {
                return Err(ReleaseError::ManifestSerialize(format!(
                    "generated release protocol path {name:?} must not come from the source commit"
                )));
            }
        }
        if path.extension().and_then(OsStr::to_str) == Some("vo") {
            let content = std::str::from_utf8(&file.bytes).map_err(|error| {
                ReleaseError::ManifestSerialize(format!(
                    "Vo source {:?} in the immutable release snapshot is not valid UTF-8: {error}",
                    file.path,
                ))
            })?;
            collect_alias_import_violations(path, content, &mut alias_imports)?;
        }
    }
    if !vo_sum_paths.is_empty() {
        return Err(ReleaseError::ForbiddenVoSum {
            repo_root: repo_root.to_path_buf(),
            paths: vo_sum_paths,
        });
    }
    if !alias_imports.is_empty() {
        return Err(ReleaseError::InvalidAliasImports(alias_imports));
    }
    Ok(())
}

fn package_file_entries(snapshot: &ReleaseSourceSnapshot) -> ReleaseResult<Vec<SourceFileEntry>> {
    let mut entries = Vec::new();
    entries.try_reserve(snapshot.files.len()).map_err(|_| {
        ReleaseError::ManifestSerialize("failed to reserve package file entries".into())
    })?;
    for file in &snapshot.files {
        if !vo_module::schema::is_package_file_candidate(&file.path)
            .map_err(ReleaseError::ManifestSerialize)?
        {
            return Err(ReleaseError::ManifestSerialize(format!(
                "release package path {:?} is owned by another release protocol",
                file.path
            )));
        }
        let mode = source_file_mode(file.mode)?;
        if file.path == "vo.mod" && mode != SourceFileMode::Regular {
            return Err(ReleaseError::ManifestSerialize(
                "root vo.mod must have regular mode 0o644 in the immutable release commit"
                    .to_string(),
            ));
        }
        entries.push(SourceFileEntry {
            path: file.path.clone(),
            mode,
            size: u64::try_from(file.bytes.len()).map_err(|_| {
                ReleaseError::ManifestSerialize("package file size exceeds u64".into())
            })?,
            digest: ModDigest::from_sha256(&file.bytes),
        });
    }
    entries.sort_by(|left, right| left.path.cmp(&right.path));
    Ok(entries)
}

fn source_file_mode(mode: u32) -> ReleaseResult<SourceFileMode> {
    match mode {
        0o644 => Ok(SourceFileMode::Regular),
        0o755 => Ok(SourceFileMode::Executable),
        _ => Err(ReleaseError::ManifestSerialize(format!(
            "immutable release source has unsupported mode {mode:#o}; expected 0o644 or 0o755"
        ))),
    }
}

fn source_archive_mode(mode: SourceFileMode) -> u32 {
    match mode {
        SourceFileMode::Regular => 0o644,
        SourceFileMode::Executable => 0o755,
    }
}

#[cfg(test)]
pub(crate) fn build_source_package(
    top_dir: &str,
    snapshot: &ReleaseSourceSnapshot,
    package_bytes: &[u8],
) -> ReleaseResult<Vec<u8>> {
    let compressed_limit =
        usize::try_from(vo_module::MAX_SOURCE_ARCHIVE_BYTES).unwrap_or(usize::MAX);
    let mut output = BoundedVecWriter::new(compressed_limit, "compressed source package");
    write_source_package(top_dir, snapshot, package_bytes, &mut output)?;
    Ok(output.into_bytes())
}

fn write_source_package(
    top_dir: &str,
    snapshot: &ReleaseSourceSnapshot,
    package_bytes: &[u8],
    target: &mut dyn Write,
) -> ReleaseResult<()> {
    let entry_count = snapshot
        .files
        .len()
        .checked_add(1)
        .ok_or_else(|| ReleaseError::ManifestSerialize("source entry count overflow".into()))?;
    if entry_count > vo_module::MAX_SOURCE_ARCHIVE_ENTRIES {
        return Err(ReleaseError::ManifestSerialize(format!(
            "source package contains {entry_count} entries, exceeding the {}-entry limit",
            vo_module::MAX_SOURCE_ARCHIVE_ENTRIES
        )));
    }
    if package_bytes.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
        return Err(ReleaseError::ManifestSerialize(format!(
            "vo.package.json exceeds the {}-byte text limit",
            vo_common::vfs::MAX_TEXT_FILE_BYTES
        )));
    }
    let extracted_bytes = snapshot
        .extracted_size
        .checked_add(package_bytes.len())
        .ok_or_else(|| {
            ReleaseError::ManifestSerialize("source package extracted size overflow".into())
        })?;
    if extracted_bytes > vo_module::MAX_EXTRACTED_SOURCE_BYTES {
        return Err(ReleaseError::ManifestSerialize(format!(
            "source package extracted content exceeds the {}-byte limit",
            vo_module::MAX_EXTRACTED_SOURCE_BYTES
        )));
    }
    let encoder = GzBuilder::new()
        .mtime(0)
        .write(target, Compression::default());
    let mut builder = Builder::new(encoder);
    if snapshot.extracted_size > vo_module::MAX_EXTRACTED_SOURCE_BYTES {
        return Err(ReleaseError::ManifestSerialize(format!(
            "source package extracted content exceeds the {}-byte limit",
            vo_module::MAX_EXTRACTED_SOURCE_BYTES
        )));
    }
    let mut package_appended = false;
    for file in &snapshot.files {
        if !package_appended && file.path.as_str() > "vo.package.json" {
            append_virtual_file(
                &mut builder,
                top_dir,
                Path::new("vo.package.json"),
                package_bytes,
            )?;
            package_appended = true;
        }
        let mut header = Header::new_gnu();
        header.set_size(u64::try_from(file.bytes.len()).map_err(|_| {
            ReleaseError::ManifestSerialize(format!("source file {:?} size exceeds u64", file.path))
        })?);
        header.set_mode(source_archive_mode(source_file_mode(file.mode)?));
        header.set_uid(0);
        header.set_gid(0);
        header.set_mtime(0);
        header.set_cksum();
        builder
            .append_data(
                &mut header,
                format!("{}/{}", top_dir, file.path),
                Cursor::new(file.bytes.as_slice()),
            )
            .map_err(|error| {
                ReleaseError::ManifestSerialize(format!(
                    "failed to archive source file {:?}: {error}",
                    file.path
                ))
            })?;
    }
    if !package_appended {
        append_virtual_file(
            &mut builder,
            top_dir,
            Path::new("vo.package.json"),
            package_bytes,
        )?;
    }
    let encoder = builder
        .into_inner()
        .map_err(|error| ReleaseError::ManifestSerialize(error.to_string()))?;
    encoder
        .finish()
        .map(|_| ())
        .map_err(|error| ReleaseError::ManifestSerialize(error.to_string()))
}

fn append_virtual_file<W: std::io::Write>(
    builder: &mut Builder<W>,
    top_dir: &str,
    rel: &Path,
    data: &[u8],
) -> ReleaseResult<()> {
    let mut header = Header::new_gnu();
    header.set_size(u64::try_from(data.len()).map_err(|_| {
        ReleaseError::ManifestSerialize("virtual source file size exceeds u64".to_string())
    })?);
    header.set_mode(0o644);
    header.set_uid(0);
    header.set_gid(0);
    header.set_mtime(0);
    header.set_cksum();
    let rel = vo_module::schema::portable_relative_path_from_path(rel).map_err(|_| {
        ReleaseError::ManifestSerialize(
            "virtual source path must be a normalized portable relative path".to_string(),
        )
    })?;
    builder
        .append_data(
            &mut header,
            format!("{}/{}", top_dir, rel),
            Cursor::new(data),
        )
        .map_err(|error| ReleaseError::ManifestSerialize(error.to_string()))
}

fn validate_exact_tracked_root_file(
    tracked: &HashSet<PathBuf>,
    expected: &str,
    required: bool,
) -> ReleaseResult<()> {
    let aliases = bounded_sorted_diagnostic(tracked.iter().filter_map(|path| {
        let is_alias = path
            .parent()
            .is_some_and(|parent| parent.as_os_str().is_empty())
            && path
                .file_name()
                .and_then(OsStr::to_str)
                .is_some_and(|name| portable_name_eq(name, expected))
            && path.as_path() != Path::new(expected);
        is_alias.then(|| path.display().to_string())
    }));
    if !aliases.is_empty() {
        return Err(ReleaseError::ManifestSerialize(format!(
            "tracked protocol file {expected} has non-canonical case alias(es): {aliases}"
        )));
    }
    if required && !tracked.contains(Path::new(expected)) {
        return Err(ReleaseError::ManifestSerialize(format!(
            "release repository must track the exact root path {expected}"
        )));
    }
    Ok(())
}

fn find_root_named_entries(repo_root: &Path, file_name: &str) -> ReleaseResult<Vec<PathBuf>> {
    let mut entry_count = 0usize;
    let mut matches = Vec::new();
    let entries = read_sorted_directory_entries(repo_root, &mut entry_count)?;
    for path in entries {
        if path
            .file_name()
            .and_then(OsStr::to_str)
            .is_some_and(|name| portable_name_eq(name, file_name))
        {
            let rel = path
                .strip_prefix(repo_root)
                .map_err(|error| ReleaseError::IoError(path.clone(), error.to_string()))?;
            matches.push(rel.to_path_buf());
        }
    }
    Ok(matches)
}

fn collect_alias_import_violations(
    path: &Path,
    content: &str,
    violations: &mut Vec<String>,
) -> ReleaseResult<()> {
    #[derive(Clone, Copy)]
    enum ImportState {
        Outside,
        Single,
        Group,
    }

    let mut lexer = Lexer::new(content, 0);
    let mut state = ImportState::Outside;
    let mut scanned_until = 0usize;
    let mut line_start = 0usize;
    let mut line_number = 1usize;
    let mut last_reported_line = None;
    let max_tokens = content
        .len()
        .checked_mul(2)
        .and_then(|count| count.checked_add(1))
        .ok_or_else(|| {
            ReleaseError::ManifestSerialize("alias import token limit overflow".to_string())
        })?;
    let mut token_count = 0usize;
    loop {
        let token = lexer.next_token();
        token_count = token_count.checked_add(1).ok_or_else(|| {
            ReleaseError::ManifestSerialize("alias import token count overflow".to_string())
        })?;
        if token_count > max_tokens {
            return Err(ReleaseError::ManifestSerialize(format!(
                "alias import scan exceeded the {max_tokens}-token source bound"
            )));
        }
        if token_count.is_multiple_of(1024) {
            drop(lexer.take_diagnostics());
        }
        let invalid_offset = match (state, token.kind) {
            (_, TokenKind::Eof) => break,
            (ImportState::Outside, TokenKind::Import) => {
                state = ImportState::Single;
                None
            }
            (ImportState::Single, TokenKind::LParen) => {
                state = ImportState::Group;
                None
            }
            (ImportState::Single, TokenKind::Semicolon)
            | (ImportState::Group, TokenKind::RParen) => {
                state = ImportState::Outside;
                None
            }
            (ImportState::Single, TokenKind::At) | (ImportState::Group, TokenKind::At) => {
                Some(token.span.start.to_usize())
            }
            _ => None,
        };
        if let Some(offset) = invalid_offset {
            let offset = offset.min(content.len());
            for (relative, byte) in content.as_bytes()[scanned_until..offset]
                .iter()
                .copied()
                .enumerate()
            {
                if byte == b'\n' {
                    line_number = line_number.checked_add(1).ok_or_else(|| {
                        ReleaseError::ManifestSerialize(
                            "alias import line number overflow".to_string(),
                        )
                    })?;
                    line_start = scanned_until + relative + 1;
                }
            }
            scanned_until = offset;
            let line_end = content.as_bytes()[offset..]
                .iter()
                .position(|byte| *byte == b'\n')
                .map_or(content.len(), |relative| offset + relative);
            let line = content.get(line_start..line_end).ok_or_else(|| {
                ReleaseError::ManifestSerialize(
                    "alias import token has an invalid source span".to_string(),
                )
            })?;
            if last_reported_line == Some(line_number) {
                continue;
            }
            if violations.len() >= vo_module::MAX_MODULE_METADATA_ENTRIES {
                return Err(ReleaseError::ManifestSerialize(format!(
                    "alias import scan found more than {} violations",
                    vo_module::MAX_MODULE_METADATA_ENTRIES
                )));
            }
            violations.push(format!(
                "{}:{}: {}",
                path.display(),
                line_number,
                line.trim()
            ));
            last_reported_line = Some(line_number);
        }
    }
    Ok(())
}

fn read_sorted_directory_entries(
    dir: &Path,
    total_entries: &mut usize,
) -> ReleaseResult<Vec<PathBuf>> {
    let mut entries = Vec::new();
    for entry in fs::read_dir(dir)
        .map_err(|error| ReleaseError::IoError(dir.to_path_buf(), error.to_string()))?
    {
        *total_entries = total_entries.checked_add(1).ok_or_else(|| {
            ReleaseError::ManifestSerialize("repository scan entry count overflow".into())
        })?;
        if *total_entries > vo_common::vfs::MAX_DIRECTORY_ENTRIES {
            return Err(ReleaseError::ManifestSerialize(format!(
                "repository scan exceeds the {}-entry limit",
                vo_common::vfs::MAX_DIRECTORY_ENTRIES
            )));
        }
        entries.try_reserve(1).map_err(|_| {
            ReleaseError::ManifestSerialize("failed to reserve repository scan entries".into())
        })?;
        entries.push(
            entry
                .map_err(|error| ReleaseError::IoError(dir.to_path_buf(), error.to_string()))?
                .path(),
        );
    }
    entries.sort();
    Ok(entries)
}

fn has_release_stage_dir_prefix(name: &str) -> bool {
    portable_name_starts_with(name, RELEASE_STAGE_DIR_PREFIX)
}

fn sha256_digest(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("sha256:{:x}", hasher.finalize())
}

fn read_mod_file(repo_root: &Path) -> ReleaseResult<ModFile> {
    project::read_mod_file_stable(repo_root)
        .map_err(|error| map_project_file_error(repo_root.join("vo.mod"), error))
}

fn map_project_file_error(path: PathBuf, error: vo_module::Error) -> ReleaseError {
    match error {
        vo_module::Error::Io(error) => ReleaseError::IoError(path, error.to_string()),
        other => other.into(),
    }
}

fn map_project_deps_error_for_release_verify(error: project::ProjectDepsError) -> ReleaseError {
    if error.stage == project::ProjectDepsStage::LockFile
        && error.kind == project::ProjectDepsErrorKind::Missing
    {
        return ReleaseError::Module(
            "vo.mod has dependencies but vo.lock is missing; run: vo mod sync".to_string(),
        );
    }
    match error.kind {
        project::ProjectDepsErrorKind::ReadFailed => match error.path {
            Some(path) => ReleaseError::IoError(PathBuf::from(path), error.detail),
            None => ReleaseError::Module(error.detail),
        },
        _ => ReleaseError::Module(error.detail),
    }
}

#[cfg(test)]
mod portable_path_tests {
    use super::*;

    #[test]
    fn release_path_semantics_use_unicode_full_case_folding() {
        assert!(portable_name_eq("artifactſ", "artifacts"));
        assert!(portable_name_eq("Straße", "STRASSE"));
        assert!(portable_name_eq("İ", "i\u{0307}"));
        assert!(portable_name_matches_any(
            "vo.package.jſon",
            &["vo.package.json"]
        ));
        assert!(portable_name_starts_with(".diſt-output", ".dist-"));
        assert!(portable_name_starts_with(".diﬆ-output", ".dist-"));
        assert!(!portable_name_eq("i", "ı"));
    }

    #[test]
    fn release_producer_rejects_path_closure_budget_overflow_atomically() {
        let deep_tail = std::iter::repeat_n("d", 244)
            .chain(std::iter::once("f.vo"))
            .collect::<Vec<_>>()
            .join("/");
        let mut paths = vo_module::schema::PortablePathSet::default();
        let mut accepted = 0usize;
        let error = loop {
            let path = format!("branch-{accepted}/{deep_tail}");
            let before = paths.path_key_bytes();
            match insert_release_source_path(&mut paths, &path, "release test producer") {
                Ok(true) => accepted += 1,
                Ok(false) => panic!("generated release path unexpectedly duplicated"),
                Err(error) => {
                    assert_eq!(paths.path_key_bytes(), before);
                    break error;
                }
            }
        };
        assert!(accepted > 100);
        assert!(
            error.to_string().contains(&format!(
                "{}-byte path-key limit",
                vo_module::MAX_PACKAGE_PATH_KEY_BYTES
            )),
            "{error}"
        );
    }
}

#[cfg(test)]
mod source_archive_tests {
    use super::*;
    use flate2::read::GzDecoder as ReadGzDecoder;

    fn snapshot(extra_path: Option<String>) -> ReleaseSourceSnapshot {
        let mut files = vec![ReleaseSourceFile {
            path: "vo.mod".to_string(),
            bytes: b"module = \"github.com/acme/archive-test\"\nvo = \"^0.1.0\"\n".to_vec(),
            mode: 0o644,
        }];
        if let Some(path) = extra_path {
            files.push(ReleaseSourceFile {
                path,
                bytes: b"x".to_vec(),
                mode: 0o644,
            });
        }
        files.sort_by(|left, right| left.path.cmp(&right.path));
        let extracted_size = files.iter().map(|file| file.bytes.len()).sum();
        ReleaseSourceSnapshot {
            files,
            extracted_size,
        }
    }

    fn package_bytes(snapshot: &ReleaseSourceSnapshot) -> Vec<u8> {
        PackageManifest {
            schema_version: 1,
            files: package_file_entries(snapshot).unwrap(),
        }
        .render()
        .unwrap()
    }

    fn validate_archive(
        archive: &[u8],
        snapshot: &ReleaseSourceSnapshot,
        package: &[u8],
    ) -> ReleaseResult<()> {
        let payload = PreparedReleasePayload {
            source_top_dir: SOURCE_ARCHIVE_TOP_LEVEL_DIR,
            source_name: SOURCE_ARCHIVE_ASSET_NAME,
            source_size: archive.len() as u64,
            source_digest: "unused",
            manifest_json: "unused",
            manifest_digest: "unused",
            package_bytes: package,
            source_snapshot: snapshot,
            artifacts: &[],
        };
        validate_staged_source_package(
            Path::new(SOURCE_ARCHIVE_ASSET_NAME),
            &mut Cursor::new(archive),
            &payload,
        )
    }

    fn gzip(raw_tar: &[u8]) -> Vec<u8> {
        let mut encoder = GzBuilder::new()
            .mtime(0)
            .write(Vec::new(), Compression::default());
        encoder.write_all(raw_tar).unwrap();
        encoder.finish().unwrap()
    }

    fn gunzip(archive: &[u8]) -> Vec<u8> {
        let mut raw = Vec::new();
        ReadGzDecoder::new(archive).read_to_end(&mut raw).unwrap();
        raw
    }

    fn custom_archive(build: impl FnOnce(&mut Builder<&mut Vec<u8>>)) -> Vec<u8> {
        let mut raw = Vec::new();
        {
            let mut builder = Builder::new(&mut raw);
            build(&mut builder);
            builder.finish().unwrap();
        }
        gzip(&raw)
    }

    fn append_extension(builder: &mut Builder<&mut Vec<u8>>, entry_type: EntryType, data: &[u8]) {
        let mut header = Header::new_gnu();
        header.set_size(data.len() as u64);
        header.set_mode(0o644);
        header.set_uid(0);
        header.set_gid(0);
        header.set_mtime(0);
        header.set_entry_type(entry_type);
        header.set_cksum();
        builder
            .append_data(&mut header, "metadata", Cursor::new(data))
            .unwrap();
    }

    fn append_long_name(builder: &mut Builder<&mut Vec<u8>>, data: &[u8]) {
        let header = canonical_gnu_long_name_header(data.len() - 1).unwrap();
        builder.append(&header, Cursor::new(data)).unwrap();
    }

    fn maximal_portable_path() -> String {
        let mut components = vec!["a".repeat(255); 15];
        components.push("b".repeat(252));
        components.push("c".to_string());
        components.push("f".to_string());
        let path = components.join("/");
        assert_eq!(path.len(), vo_module::schema::MAX_PORTABLE_PATH_BYTES);
        path
    }

    #[test]
    fn canonical_source_archive_roundtrips_the_maximum_wire_path() {
        let path = maximal_portable_path();
        let snapshot = snapshot(Some(path.clone()));
        let package = package_bytes(&snapshot);
        let archive =
            build_source_package(SOURCE_ARCHIVE_TOP_LEVEL_DIR, &snapshot, &package).unwrap();

        validate_archive(&archive, &snapshot, &package).unwrap();

        let raw = gunzip(&archive);
        let wire_path = format!("{SOURCE_ARCHIVE_TOP_LEVEL_DIR}/{path}");
        let long_header = Header::from_byte_slice(&raw[..512]);
        assert_eq!(
            long_header.as_bytes(),
            canonical_gnu_long_name_header(wire_path.len())
                .unwrap()
                .as_bytes()
        );
        assert_eq!(long_header.size().unwrap(), 4_104);
        assert_eq!(&raw[512..512 + wire_path.len()], wire_path.as_bytes());
        assert_eq!(raw[512 + wire_path.len()], 0);
        let regular_offset = 512 + 4_608;
        let regular_header = Header::from_byte_slice(&raw[regular_offset..regular_offset + 512]);
        assert_eq!(
            regular_header.as_bytes(),
            canonical_source_file_header(&wire_path, 1, 0o644)
                .unwrap()
                .as_bytes(),
        );
    }

    #[test]
    fn canonical_source_archive_uses_the_exact_longname_threshold_and_utf8_prefix() {
        let direct_path = "a".repeat(93);
        let direct_snapshot = snapshot(Some(direct_path.clone()));
        let direct_package = package_bytes(&direct_snapshot);
        let direct_archive = build_source_package(
            SOURCE_ARCHIVE_TOP_LEVEL_DIR,
            &direct_snapshot,
            &direct_package,
        )
        .unwrap();
        validate_archive(&direct_archive, &direct_snapshot, &direct_package).unwrap();
        let direct_raw = gunzip(&direct_archive);
        let direct_header = Header::from_byte_slice(&direct_raw[..512]);
        assert!(direct_header.entry_type().is_file());
        assert_eq!(direct_header.path_bytes().len(), 100);

        let utf8_path = format!("{}é", "a".repeat(92));
        let long_snapshot = snapshot(Some(utf8_path.clone()));
        let long_package = package_bytes(&long_snapshot);
        let long_archive =
            build_source_package(SOURCE_ARCHIVE_TOP_LEVEL_DIR, &long_snapshot, &long_package)
                .unwrap();
        validate_archive(&long_archive, &long_snapshot, &long_package).unwrap();
        let long_raw = gunzip(&long_archive);
        let long_header = Header::from_byte_slice(&long_raw[..512]);
        assert!(long_header.entry_type().is_gnu_longname());
        let regular_header = Header::from_byte_slice(&long_raw[1_024..1_536]);
        let wire_prefix = format!("{SOURCE_ARCHIVE_TOP_LEVEL_DIR}/{}", "a".repeat(92));
        assert_eq!(wire_prefix.len(), 99);
        assert_eq!(&regular_header.as_old().name[..99], wire_prefix.as_bytes());
        assert!(regular_header.as_old().name[99..]
            .iter()
            .all(|byte| *byte == 0));
    }

    #[test]
    fn source_archive_rejects_every_non_longname_extension_and_link_type() {
        let snapshot = snapshot(None);
        let package = package_bytes(&snapshot);
        for entry_type in [
            EntryType::XHeader,
            EntryType::XGlobalHeader,
            EntryType::GNULongLink,
            EntryType::GNUSparse,
            EntryType::Link,
            EntryType::Symlink,
            EntryType::Directory,
            EntryType::new(b'Z'),
        ] {
            let archive = custom_archive(|builder| {
                append_extension(builder, entry_type, b"");
            });
            let error = validate_archive(&archive, &snapshot, &package).unwrap_err();
            assert!(
                error.to_string().contains("entry type"),
                "{entry_type:?}: {error}"
            );
        }
    }

    #[test]
    fn source_archive_rejects_dangling_repeated_and_malformed_longnames() {
        let snapshot = snapshot(None);
        let package = package_bytes(&snapshot);
        let valid = [vec![b'a'; 101], vec![0]].concat();

        let dangling = custom_archive(|builder| append_long_name(builder, &valid));
        let dangling_raw = gunzip(&dangling);
        let dangling_header = Header::from_byte_slice(&dangling_raw[..512]);
        let dangling_expected = canonical_gnu_long_name_header(valid.len() - 1).unwrap();
        let mismatch = dangling_header
            .as_bytes()
            .iter()
            .zip(dangling_expected.as_bytes())
            .position(|(actual, expected)| actual != expected);
        assert_eq!(mismatch, None, "unexpected long-name header mismatch");
        let dangling_error = validate_archive(&dangling, &snapshot, &package).unwrap_err();
        assert!(
            dangling_error.to_string().contains("not followed"),
            "{dangling_error}"
        );

        let repeated = custom_archive(|builder| {
            append_long_name(builder, &valid);
            append_long_name(builder, &valid);
        });
        assert!(validate_archive(&repeated, &snapshot, &package)
            .unwrap_err()
            .to_string()
            .contains("immediately"));

        for malformed in [
            [vec![b'a'; 101], vec![b'x']].concat(),
            [vec![b'a'; 50], vec![0], vec![b'a'; 50], vec![0]].concat(),
            [vec![b'a'; 100], vec![0xff], vec![0]].concat(),
        ] {
            let archive = custom_archive(|builder| append_long_name(builder, &malformed));
            assert!(validate_archive(&archive, &snapshot, &package).is_err());
        }

        let noncanonical_header = custom_archive(|builder| {
            let mut header = canonical_gnu_long_name_header(101).unwrap();
            header.set_mode(0o755);
            header.set_cksum();
            builder.append(&header, Cursor::new(&valid)).unwrap();
        });
        assert!(validate_archive(&noncanonical_header, &snapshot, &package)
            .unwrap_err()
            .to_string()
            .contains("header is not canonical"));
    }

    #[test]
    fn source_archive_rejects_noncanonical_longname_prefix_and_padding() {
        let path = "a".repeat(101);
        let snapshot = snapshot(Some(path));
        let package = package_bytes(&snapshot);
        let archive =
            build_source_package(SOURCE_ARCHIVE_TOP_LEVEL_DIR, &snapshot, &package).unwrap();
        let raw = gunzip(&archive);
        let long_size = Header::from_byte_slice(&raw[..512]).size().unwrap() as usize;
        let regular_offset = 512 + long_size.div_ceil(512) * 512;

        let mut bad_prefix = raw.clone();
        let mut header = Header::new_gnu();
        header
            .as_mut_bytes()
            .copy_from_slice(&bad_prefix[regular_offset..regular_offset + 512]);
        header.as_mut_bytes()[0] ^= 1;
        header.set_cksum();
        bad_prefix[regular_offset..regular_offset + 512].copy_from_slice(header.as_bytes());
        assert!(validate_archive(&gzip(&bad_prefix), &snapshot, &package)
            .unwrap_err()
            .to_string()
            .contains("header is not canonical"));

        let mut bad_padding = raw;
        bad_padding[512 + long_size] = 1;
        assert!(validate_archive(&gzip(&bad_padding), &snapshot, &package)
            .unwrap_err()
            .to_string()
            .contains("padding must be zero"));
    }

    #[test]
    fn source_archive_enforces_exact_tar_and_gzip_closure() {
        let snapshot = snapshot(None);
        let package = package_bytes(&snapshot);
        let archive =
            build_source_package(SOURCE_ARCHIVE_TOP_LEVEL_DIR, &snapshot, &package).unwrap();
        let raw = gunzip(&archive);

        let mut missing_end = raw.clone();
        missing_end.truncate(missing_end.len() - 512);
        assert!(validate_archive(&gzip(&missing_end), &snapshot, &package).is_err());

        let mut extra_end = raw;
        extra_end.extend_from_slice(&[0; 512]);
        assert!(validate_archive(&gzip(&extra_end), &snapshot, &package)
            .unwrap_err()
            .to_string()
            .contains("decompressed bytes"));

        let mut trailing = archive.clone();
        trailing.extend_from_slice(b"trailing");
        assert!(validate_archive(&trailing, &snapshot, &package)
            .unwrap_err()
            .to_string()
            .contains("trailing compressed bytes"));

        let mut concatenated = archive.clone();
        concatenated.extend_from_slice(&archive);
        assert!(validate_archive(&concatenated, &snapshot, &package)
            .unwrap_err()
            .to_string()
            .contains("concatenated gzip member"));

        let mut corrupt_crc = archive;
        let crc_byte = corrupt_crc.len() - 8;
        corrupt_crc[crc_byte] ^= 1;
        assert!(validate_archive(&corrupt_crc, &snapshot, &package).is_err());
    }
}

#[cfg(all(test, unix))]
mod internal_tests {
    use super::*;

    #[test]
    fn git_path_record_preserves_non_utf8_unix_bytes() {
        use std::os::unix::ffi::OsStrExt as _;

        let path = git_stdout_path_record(
            b"repo-\xff\n".to_vec(),
            Path::new("."),
            "non-UTF-8 path regression",
        )
        .unwrap();

        assert_eq!(path.as_os_str().as_bytes(), b"repo-\xff");
    }

    #[test]
    fn bounded_command_drains_oversized_stdout_and_stderr_before_reaping() {
        let mut command = Command::new("sh");
        command.args([
            "-c",
            "i=0; while [ \"$i\" -lt 20000 ]; do printf 'oversized-stdout-record\\n'; printf 'oversized-stderr-record\\n' >&2; i=$((i + 1)); done",
        ]);

        let error = run_command_output_bounded(
            &mut command,
            Path::new("."),
            "bounded output regression child",
            1024,
        )
        .unwrap_err();

        assert!(error.to_string().contains("git stdout exceeds"), "{error}");
    }
}
