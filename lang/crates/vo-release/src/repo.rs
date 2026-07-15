use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::ffi::OsStr;
use std::fs;
use std::io::{self, BufRead, BufReader, Cursor, Read, Write};
use std::path::{Component, Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};

use flate2::{Compression, GzBuilder};
use sha2::{Digest, Sha256};
use tar::{Builder, Header};
use vo_module::digest::Digest as ModDigest;
use vo_module::identity::ArtifactId;
use vo_module::project;
use vo_module::schema::manifest::{
    ManifestArtifact, ManifestRequire, ManifestSource, ManifestWebManifest, ReleaseManifest,
};
use vo_module::schema::modfile::ModFile;
use vo_module::schema::{canonical_source_file_set, SourceFileEntry};
use vo_module::version::ExactVersion;
use vo_syntax::{Lexer, TokenKind};

use crate::publish::{PendingOutputDir, RELEASE_STAGE_DIR_PREFIX};
use crate::{ReleaseError, ReleaseResult};

const IGNORED_DIR_NAMES: &[&str] = &[
    ".git",
    ".github",
    ".vo-cache",
    ".vodeps",
    "node_modules",
    "target",
    "dist",
    "pkg",
    "__pycache__",
];
const IGNORED_FILE_NAMES: &[&str] = &[
    "vo.release.json",
    "vo.web.json",
    "vo.work",
    ".vo-project.lock",
    ".DS_Store",
];
const IGNORED_SUFFIXES: &[&str] = &[".a", ".dll", ".dylib", ".lib", ".pdb", ".so", ".wasm"];
const WASM_TARGET: &str = "wasm32-unknown-unknown";
const MAX_RELEASE_SCAN_DEPTH: usize = 256;
const MAX_GIT_REVISION_OUTPUT_BYTES: usize = 8 * 1024;
const MAX_GIT_STDERR_BYTES: usize = 64 * 1024;
const MAX_GIT_PATH_RECORD_BYTES: usize = vo_module::schema::MAX_PORTABLE_PATH_BYTES + 128;
const MAX_GIT_PATH_STREAM_BYTES: usize = vo_module::MAX_EXTRACTED_SOURCE_BYTES;

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

fn portable_name_ends_with(value: &str, suffix: &str) -> bool {
    vo_module::schema::portable_case_key(value)
        .ends_with(&vo_module::schema::portable_case_key(suffix))
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
    pub source_files_size: u64,
    pub source_files_digest: String,
    pub manifest_path: PathBuf,
    pub web_manifest_path: PathBuf,
    pub manifest_digest: String,
    pub manifest_json: String,
    pub artifacts: Vec<StagedArtifact>,
}

impl StagedRelease {
    /// Exact GitHub Release upload set in stable protocol order:
    /// `vo.release.json`, `vo.web.json`, source package, then artifacts sorted
    /// by their complete `(kind, target, name)` identity.
    pub fn assets(&self) -> Vec<&Path> {
        let mut assets = Vec::with_capacity(3 + self.artifacts.len());
        assets.push(self.manifest_path.as_path());
        assets.push(self.web_manifest_path.as_path());
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

pub(crate) struct CommitTree {
    repository_root: PathBuf,
    module_prefix: String,
    files: BTreeMap<String, CommitTreeFile>,
}

#[derive(Debug, Clone)]
pub(crate) struct SelectedSourceFile {
    tree_file: CommitTreeFile,
    explicitly_included: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ReleaseSourceFile {
    path: String,
    bytes: Vec<u8>,
    mode: u32,
}

/// One bounded, immutable view of every byte that can enter the source
/// archive. Both `vo.web.json.source` and the tar writer consume this value.
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
    source_name: &'a str,
    source_bytes: &'a [u8],
    source_digest: &'a str,
    manifest_json: &'a str,
    manifest_digest: &'a str,
    web_manifest_json: &'a str,
    source_snapshot: &'a ReleaseSourceSnapshot,
    artifacts: &'a [PreparedArtifact],
}

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

fn run_git_nul_records(
    command: &mut Command,
    repo_root: &Path,
    context: &str,
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
            if record_count > vo_common::vfs::MAX_DIRECTORY_ENTRIES {
                return Err(ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!(
                        "{context} returned more than {} path records",
                        vo_common::vfs::MAX_DIRECTORY_ENTRIES
                    ),
                });
            }
            total_bytes = total_bytes
                .checked_add(record.len().saturating_add(1))
                .ok_or_else(|| ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!("{context} output size overflow"),
                })?;
            if total_bytes > MAX_GIT_PATH_STREAM_BYTES {
                return Err(ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!(
                        "{context} output exceeds the {MAX_GIT_PATH_STREAM_BYTES}-byte limit"
                    ),
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
    let tracked = validate_tracked_protocol_file_names(&repo_root)?;
    let mut tracked_project_locks = tracked
        .iter()
        .filter(|path| {
            path.file_name()
                .and_then(OsStr::to_str)
                .is_some_and(|name| portable_name_eq(name, ".vo-project.lock"))
        })
        .collect::<Vec<_>>();
    tracked_project_locks.sort();
    if !tracked_project_locks.is_empty() {
        let total = tracked_project_locks.len();
        let displayed = tracked_project_locks
            .iter()
            .take(16)
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>()
            .join(", ");
        let omitted = total.saturating_sub(16);
        return Err(ReleaseError::ManifestSerialize(format!(
            "generated project transaction locks must not be tracked or released: {displayed}{}",
            if omitted == 0 {
                String::new()
            } else {
                format!(" (and {omitted} more)")
            }
        )));
    }
    let mod_file = read_mod_file(&repo_root)?;
    if !mod_file.require.is_empty() {
        validate_exact_tracked_root_file(&tracked, "vo.lock", true)?;
    }

    let vo_sum_paths = find_named_files(&repo_root, "vo.sum")?;
    if !vo_sum_paths.is_empty() {
        return Err(ReleaseError::ForbiddenVoSum {
            repo_root: repo_root.clone(),
            paths: vo_sum_paths,
        });
    }

    let alias_imports = find_alias_imports(&repo_root)?;
    if !alias_imports.is_empty() {
        return Err(ReleaseError::InvalidAliasImports(alias_imports));
    }
    project::read_project_deps_at_root(&repo_root, &[])
        .map_err(map_project_deps_error_for_release_verify)?;
    Ok(())
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
    verify_repo(&repo_root)?;

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
    let module_str = module_path.as_str();
    let source_top_dir = source_package_base_name(module_str).ok_or_else(|| {
        ReleaseError::IoError(
            repo_root.clone(),
            format!("invalid module path: {}", module_str),
        )
    })?;
    let source_name = format!("{}-{}.tar.gz", source_top_dir, version_string);
    let mut pending = PendingOutputDir::create(&out_dir)?;
    let prepared_artifacts = prepare_artifacts(
        &options.artifacts,
        &out_dir,
        &["vo.release.json", "vo.web.json", &source_name],
        &mut pending,
    )?;
    validate_artifact_contract(&repo_root, &mod_file, &prepared_artifacts)?;
    validate_web_artifact_sources(&repo_root, &mod_file, &commit_tree, &prepared_artifacts)?;
    let source_files = collect_publish_source_files(&repo_root, &out_dir, &mod_file, &commit_tree)?;
    let source_snapshot = capture_release_source_snapshot(&repo_root, &source_files, &commit_tree)?;
    validate_release_source_snapshot(&repo_root, &source_snapshot)?;
    let source_entries = web_source_entries(&source_snapshot)?;
    let source_set =
        canonical_source_file_set(&source_entries).map_err(ReleaseError::ManifestSerialize)?;
    let source_files_size = source_set.total_size;
    let source_set_digest = source_set.digest.to_string();
    let web_manifest_json = build_web_manifest_json(
        &mod_file,
        &version_string,
        &commit,
        &prepared_artifacts,
        &source_entries,
        &source_set_digest,
    )?;
    let web_manifest_size = u64::try_from(web_manifest_json.len())
        .map_err(|_| ReleaseError::ManifestSerialize("vo.web.json size exceeds u64".to_string()))?;
    let web_manifest_digest = ModDigest::from_sha256(web_manifest_json.as_bytes());
    let source_bytes = build_source_package(source_top_dir, &source_snapshot, &web_manifest_json)?;
    let source_size = u64::try_from(source_bytes.len()).map_err(|_| {
        ReleaseError::ManifestSerialize("compressed source package size exceeds u64".to_string())
    })?;
    let source_digest = sha256_digest(&source_bytes);

    let source_digest_typed = ModDigest::parse(&source_digest)
        .map_err(|e| ReleaseError::ManifestSerialize(format!("invalid source digest: {e}")))?;
    let source_files_digest_typed = source_set.digest;

    let manifest = ReleaseManifest {
        schema_version: 1,
        module: module_path.clone(),
        version,
        commit: commit.clone(),
        module_root: module_path.module_root().to_string(),
        vo: mod_file.vo.clone(),
        require: mod_file
            .require
            .iter()
            .map(|req| ManifestRequire {
                module: req.module.clone(),
                constraint: req.constraint.clone(),
            })
            .collect(),
        source: ManifestSource {
            name: source_name.clone(),
            size: source_size,
            digest: source_digest_typed,
            files_size: source_files_size,
            files_digest: source_files_digest_typed,
        },
        web_manifest: ManifestWebManifest {
            size: web_manifest_size,
            digest: web_manifest_digest,
        },
        artifacts: prepared_artifacts
            .iter()
            .map(|a| a.manifest_artifact.clone())
            .collect(),
    };
    let mut manifest_json = manifest
        .render()
        .map_err(|error| ReleaseError::ManifestSerialize(error.to_string()))?;
    if manifest_json.len() >= vo_common::vfs::MAX_TEXT_FILE_BYTES {
        return Err(ReleaseError::ManifestSerialize(format!(
            "canonical vo.release.json leaves no room for its final newline within the {}-byte text limit",
            vo_common::vfs::MAX_TEXT_FILE_BYTES
        )));
    }
    manifest_json.try_reserve(1).map_err(|_| {
        ReleaseError::ManifestSerialize(
            "failed to reserve canonical vo.release.json terminator".to_string(),
        )
    })?;
    manifest_json.push('\n');
    let manifest_digest = sha256_digest(manifest_json.as_bytes());

    let source_path = out_dir.join(&source_name);
    let manifest_path = out_dir.join("vo.release.json");
    let web_manifest_path = out_dir.join("vo.web.json");
    let payload = PreparedReleasePayload {
        source_name: &source_name,
        source_bytes: &source_bytes,
        source_digest: &source_digest,
        manifest_json: &manifest_json,
        manifest_digest: &manifest_digest,
        web_manifest_json: &web_manifest_json,
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
        source_files_size,
        source_files_digest: source_set_digest,
        manifest_path,
        web_manifest_path,
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
    pending.write_new_file(payload.source_name, payload.source_bytes)?;
    pending.write_new_file("vo.release.json", payload.manifest_json.as_bytes())?;
    pending.write_new_file("vo.web.json", payload.web_manifest_json.as_bytes())?;

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
        "vo.web.json".to_string(),
    ]);
    for artifact in payload.artifacts {
        expected_names.insert(artifact.staged.asset_name.clone());
    }

    let found_names = stage_dir.entry_names()?;
    if found_names != expected_names {
        let missing = expected_names
            .difference(&found_names)
            .cloned()
            .collect::<Vec<_>>();
        let extra = found_names
            .difference(&expected_names)
            .cloned()
            .collect::<Vec<_>>();
        return Err(ReleaseError::IoError(
            stage_dir.path(),
            format!(
                "staged release asset set mismatch; missing [{}], extra [{}]",
                missing.join(", "),
                extra.join(", ")
            ),
        ));
    }

    let source_path = stage_dir.path().join(payload.source_name);
    let expected_source_size = u64::try_from(payload.source_bytes.len()).map_err(|_| {
        ReleaseError::IoError(
            source_path.clone(),
            "staged source package size exceeds u64".to_string(),
        )
    })?;
    stage_dir.validate_file_digest(
        payload.source_name,
        vo_module::MAX_SOURCE_ARCHIVE_BYTES,
        expected_source_size,
        payload.source_digest,
    )?;
    let extracted_source = vo_module::cache::install::extract_source_entries(payload.source_bytes)
        .map_err(|error| {
            ReleaseError::IoError(
                source_path.clone(),
                format!("module installer rejected the immutable source archive: {error}"),
            )
        })?;
    validate_extracted_source_snapshot(&source_path, payload, &extracted_source)?;

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
        .map_err(|error| ReleaseError::IoError(manifest_path.clone(), error.to_string()))?
        + "\n";
    if canonical_manifest != payload.manifest_json {
        return Err(ReleaseError::IoError(
            manifest_path,
            "staged vo.release.json is not canonical".to_string(),
        ));
    }

    let web_manifest_path = stage_dir.path().join("vo.web.json");
    let staged_web_manifest_bytes =
        stage_dir.read_file("vo.web.json", vo_common::vfs::MAX_TEXT_FILE_BYTES)?;
    validate_staged_payload(
        &web_manifest_path,
        &staged_web_manifest_bytes,
        parsed_manifest.web_manifest.size,
        parsed_manifest.web_manifest.digest.as_str(),
    )?;
    let staged_web_manifest = std::str::from_utf8(&staged_web_manifest_bytes).map_err(|error| {
        ReleaseError::IoError(
            web_manifest_path.clone(),
            format!("staged vo.web.json is not valid UTF-8: {error}"),
        )
    })?;
    if staged_web_manifest != payload.web_manifest_json {
        return Err(ReleaseError::IoError(
            web_manifest_path,
            "staged vo.web.json bytes do not match the generated browser manifest".to_string(),
        ));
    }
    validate_staged_install_model(&source_path, payload, &parsed_manifest, extracted_source)?;

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

fn validate_extracted_source_snapshot(
    source_path: &Path,
    payload: &PreparedReleasePayload<'_>,
    extracted: &[(PathBuf, String)],
) -> ReleaseResult<()> {
    let mut expected = BTreeMap::new();
    for file in &payload.source_snapshot.files {
        if std::str::from_utf8(&file.bytes).is_err() {
            continue;
        }
        expected.insert(Path::new(&file.path), file.bytes.as_slice());
    }
    expected.insert(
        Path::new("vo.web.json"),
        payload.web_manifest_json.as_bytes(),
    );
    let mut found = BTreeMap::new();
    for (path, content) in extracted {
        if found.insert(path.as_path(), content.as_str()).is_some() {
            return Err(ReleaseError::IoError(
                source_path.to_path_buf(),
                format!("module installer returned duplicate source path {path:?}"),
            ));
        }
    }
    let matches = found.len() == expected.len()
        && expected.iter().all(|(path, bytes)| {
            found
                .get(path)
                .is_some_and(|content| content.as_bytes() == *bytes)
        });
    if matches {
        return Ok(());
    }
    let missing = expected
        .keys()
        .filter(|path| !found.contains_key(*path))
        .map(|path| path.display().to_string())
        .collect::<Vec<_>>();
    let extra = found
        .keys()
        .filter(|path| !expected.contains_key(*path))
        .map(|path| path.display().to_string())
        .collect::<Vec<_>>();
    let changed = expected
        .iter()
        .filter(|(path, bytes)| {
            found
                .get(*path)
                .is_some_and(|content| content.as_bytes() != **bytes)
        })
        .map(|(path, _)| path.display().to_string())
        .collect::<Vec<_>>();
    Err(ReleaseError::IoError(
        source_path.to_path_buf(),
        format!(
            "module installer source set differs from the immutable snapshot; missing [{}], extra [{}], changed [{}]",
            missing.join(", "),
            extra.join(", "),
            changed.join(", "),
        ),
    ))
}

fn validate_staged_install_model(
    source_path: &Path,
    payload: &PreparedReleasePayload<'_>,
    manifest: &ReleaseManifest,
    extracted: Vec<(PathBuf, String)>,
) -> ReleaseResult<()> {
    let module_dir =
        vo_module::cache::layout::relative_module_dir(&manifest.module, &manifest.version);
    let mut fs = vo_common::vfs::MemoryFs::new();
    for (path, content) in extracted {
        fs.add_file(module_dir.join(path), content);
    }
    fs.add_file(
        module_dir.join("vo.release.json"),
        payload.manifest_json.to_string(),
    );
    fs.add_file(
        module_dir.join(vo_module::cache::layout::VERSION_MARKER),
        format!("{}\n", manifest.version),
    );
    fs.add_file(
        module_dir.join(vo_module::cache::layout::SOURCE_DIGEST_MARKER),
        format!("{}\n", manifest.source.digest),
    );
    let locked = vo_module::schema::lockfile::LockedModule {
        path: manifest.module.clone(),
        version: manifest.version.clone(),
        vo: manifest.vo.clone(),
        commit: manifest.commit.clone(),
        release_manifest: ModDigest::from_sha256(payload.manifest_json.as_bytes()),
        source: manifest.source.digest.clone(),
        deps: manifest
            .require
            .iter()
            .map(|require| vo_module::schema::lockfile::LockedRequirement {
                module: require.module.clone(),
                constraint: require.constraint.clone(),
            })
            .collect(),
        artifacts: manifest
            .artifacts
            .iter()
            .map(|artifact| vo_module::schema::lockfile::LockedArtifact {
                id: artifact.id.clone(),
                size: artifact.size,
                digest: artifact.digest.clone(),
            })
            .collect(),
    };
    vo_module::cache::validate::validate_installed_module(&fs, &module_dir, &locked).map_err(
        |error| {
            ReleaseError::IoError(
                source_path.to_path_buf(),
                format!("staged source fails installed-module validation: {error}"),
            )
        },
    )
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

    let mut command = Command::new("git");
    command.arg("-C").arg(&repository_root).args([
        "ls-tree",
        "-r",
        "-z",
        "--full-tree",
        commit,
        "--",
    ]);
    if !module_prefix.is_empty() {
        command.arg(&module_prefix);
    }
    let mut files = BTreeMap::new();
    let mut portable_paths = vo_module::schema::PortablePathSet::default();
    run_git_nul_records(&mut command, repo_root, "git ls-tree", |raw_record| {
        if raw_record.is_empty() {
            return Err(ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: "git ls-tree returned an empty entry".to_string(),
            });
        }
        if files.len() >= vo_common::vfs::MAX_DIRECTORY_ENTRIES {
            return Err(ReleaseError::ManifestSerialize(format!(
                "release commit tree contains more than {} entries below the module root",
                vo_common::vfs::MAX_DIRECTORY_ENTRIES
            )));
        }
        let tab = raw_record
            .iter()
            .position(|byte| *byte == b'\t')
            .ok_or_else(|| ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: "git ls-tree returned a malformed entry".to_string(),
            })?;
        let header =
            std::str::from_utf8(&raw_record[..tab]).map_err(|error| ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: format!("git ls-tree returned a non-UTF-8 header: {error}"),
            })?;
        let repository_path = std::str::from_utf8(&raw_record[tab + 1..]).map_err(|error| {
            ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: format!("release commit contains a non-UTF-8 path: {error}"),
            }
        })?;
        vo_module::schema::validate_portable_relative_path(repository_path).map_err(|error| {
            ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: format!(
                    "release commit path {repository_path:?} is not portable: {error}"
                ),
            }
        })?;
        let module_path = if module_prefix.is_empty() {
            repository_path
        } else {
            repository_path
                .strip_prefix(&module_prefix)
                .and_then(|path| path.strip_prefix('/'))
                .ok_or_else(|| ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!(
                        "git ls-tree returned path {repository_path:?} outside module prefix {module_prefix:?}"
                    ),
                })?
        };
        vo_module::schema::validate_portable_relative_path(module_path).map_err(|error| {
            ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: format!("module source path {module_path:?} is not portable: {error}"),
            }
        })?;

        let mut fields = header.split_whitespace();
        let mode = fields.next().unwrap_or_default();
        let kind = fields.next().unwrap_or_default();
        let object_id = fields.next().unwrap_or_default();
        if fields.next().is_some()
            || !object_id.bytes().all(|byte| byte.is_ascii_hexdigit())
            || !matches!(object_id.len(), 40 | 64)
        {
            return Err(ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: format!("git ls-tree returned malformed metadata for {module_path:?}"),
            });
        }
        let archive_mode = match (mode, kind) {
            ("100644", "blob") => 0o644,
            ("100755", "blob") => 0o755,
            _ => {
                return Err(ReleaseError::GitError {
                    repo_root: repo_root.to_path_buf(),
                    message: format!(
                        "release commit entry {module_path:?} has unsupported git kind {kind:?} and mode {mode:?}; only regular files are publishable"
                    ),
                })
            }
        };
        if !portable_paths
            .insert_file(module_path)
            .map_err(ReleaseError::ManifestSerialize)?
        {
            return Err(ReleaseError::ManifestSerialize(format!(
                "release commit contains duplicate source path {module_path:?}"
            )));
        }
        let tree_file = CommitTreeFile {
            repository_path: repository_path.to_string(),
            module_path: module_path.to_string(),
            object_id: object_id.to_string(),
            mode: archive_mode,
        };
        if files.insert(module_path.to_string(), tree_file).is_some() {
            return Err(ReleaseError::ManifestSerialize(format!(
                "release commit contains duplicate source path {module_path:?}"
            )));
        }
        Ok(())
    })?;
    Ok(CommitTree {
        repository_root,
        module_prefix,
        files,
    })
}

fn validate_module_root_location(
    repo_root: &Path,
    declared_module_root: &str,
    tree: &CommitTree,
) -> ReleaseResult<()> {
    let actual = if tree.module_prefix.is_empty() {
        "."
    } else {
        tree.module_prefix.as_str()
    };
    if actual == declared_module_root {
        return Ok(());
    }
    Err(ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: format!(
            "vo.mod declares repository module_root {declared_module_root:?}, but the staged module directory is {actual:?}"
        ),
    })
}

pub(crate) fn read_committed_mod_file(
    repo_root: &Path,
    tree: &CommitTree,
) -> ReleaseResult<ModFile> {
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
    let tracked = tree.files.keys().map(PathBuf::from).collect::<HashSet<_>>();
    validate_exact_tracked_root_file(&tracked, "vo.mod", true)?;
    validate_exact_tracked_root_file(&tracked, "vo.lock", !mod_file.require.is_empty())?;

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
    project::read_project_deps(&fs, &[]).map_err(map_project_deps_error_for_release_verify)?;
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
            if size > max_entry_bytes {
                return Err(ReleaseError::ManifestSerialize(format!(
                    "source file {:?} exceeds the {}-byte entry limit",
                    file.module_path, max_entry_bytes
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

fn validate_web_artifact_sources(
    repo_root: &Path,
    mod_file: &ModFile,
    commit_tree: &CommitTree,
    prepared: &[PreparedArtifact],
) -> ReleaseResult<()> {
    let manifest_path = repo_root.join("vo.mod");
    let Some(manifest) = mod_file.extension.as_ref() else {
        return Ok(());
    };
    let Some(wasm) = manifest.wasm.as_ref() else {
        return Ok(());
    };

    validate_web_artifact_source(
        repo_root,
        &manifest_path,
        commit_tree,
        prepared,
        "extension-wasm",
        &wasm.wasm,
        wasm.local_wasm.as_deref().unwrap_or(&wasm.wasm),
    )?;
    if let Some(js_glue) = wasm.js_glue.as_deref() {
        validate_web_artifact_source(
            repo_root,
            &manifest_path,
            commit_tree,
            prepared,
            "extension-js-glue",
            js_glue,
            wasm.local_js_glue.as_deref().unwrap_or(js_glue),
        )?;
    }
    Ok(())
}

fn validate_web_artifact_source(
    repo_root: &Path,
    manifest_path: &Path,
    commit_tree: &CommitTree,
    prepared: &[PreparedArtifact],
    kind: &str,
    artifact_name: &str,
    source_rel: &str,
) -> ReleaseResult<()> {
    let source_rel = normalize_include_path(manifest_path, Path::new(source_rel))?;
    let source_path = repo_root.join(&source_rel);
    let source_rel =
        vo_module::schema::portable_relative_path_from_path(&source_rel).map_err(|error| {
            ReleaseError::IoError(
                source_path.clone(),
                format!("web artifact source path must be portable: {error}"),
            )
        })?;
    let tree_file = commit_tree.files.get(&source_rel).ok_or_else(|| {
        ReleaseError::IoError(
            source_path.clone(),
            "web artifact source must be a regular file in the release commit so Studio web can fetch it from the release tag".to_string(),
        )
    })?;
    let artifact_limit =
        usize::try_from(vo_module::MAX_MODULE_ARTIFACT_BYTES).unwrap_or(usize::MAX);
    let mut source_blobs = read_commit_blobs(
        repo_root,
        &commit_tree.repository_root,
        std::slice::from_ref(tree_file),
        artifact_limit,
        artifact_limit,
    )?;
    let source_bytes = source_blobs.pop().ok_or_else(|| ReleaseError::GitError {
        repo_root: repo_root.to_path_buf(),
        message: format!("git returned no blob for web artifact source {source_rel:?}"),
    })?;
    let staged = prepared
        .iter()
        .find(|artifact| {
            artifact.staged.kind == kind
                && artifact.staged.target == WASM_TARGET
                && artifact.staged.name == artifact_name
        })
        .ok_or_else(|| {
            ReleaseError::IoError(
                manifest_path.to_path_buf(),
                format!(
                    "missing staged web artifact {}:{}:{}",
                    kind, WASM_TARGET, artifact_name
                ),
            )
        })?;
    let source_size = u64::try_from(source_bytes.len()).map_err(|_| {
        ReleaseError::IoError(
            source_path.clone(),
            "web artifact source size exceeds u64".to_string(),
        )
    })?;
    let source_digest = sha256_digest(&source_bytes);
    if source_size != staged.staged.size || source_digest != staged.staged.digest {
        return Err(ReleaseError::IoError(
            source_path,
            format!(
                "web artifact source declared by {} must byte-match staged release artifact {}:{}:{} from {}",
                manifest_path.display(),
                kind,
                WASM_TARGET,
                artifact_name,
                staged.staged.source_path.display(),
            ),
        ));
    }
    Ok(())
}

fn included_source_files(
    repo_root: &Path,
    mod_file: &ModFile,
    commit_tree: &CommitTree,
) -> ReleaseResult<Vec<String>> {
    let manifest_path = repo_root.join("vo.mod");
    let mut include_paths = Vec::new();
    if let Some(web) = &mod_file.web {
        include_paths.extend(web.include.iter().cloned());
    }
    if let Some(extension) = &mod_file.extension {
        include_paths.extend(extension.include.iter().cloned());
    }
    if include_paths.is_empty() {
        return Ok(Vec::new());
    }
    let mut files = BTreeSet::new();
    for include_path in &include_paths {
        let normalized = normalize_include_path(&manifest_path, include_path)?;
        let portable =
            vo_module::schema::portable_relative_path_from_path(&normalized).map_err(|error| {
                ReleaseError::IoError(
                    manifest_path.clone(),
                    format!("included path must be portable: {error}"),
                )
            })?;
        if commit_tree.files.contains_key(&portable) {
            files.insert(portable);
            continue;
        }
        let prefix = format!("{portable}/");
        let mut matched = false;
        for (path, _) in commit_tree.files.range(prefix.clone()..) {
            if !path.starts_with(&prefix) {
                break;
            }
            matched = true;
            files.insert(path.clone());
        }
        if !matched {
            return Err(ReleaseError::IoError(
                repo_root.join(&normalized),
                format!(
                    "included path referenced by {} must identify a regular file or non-empty directory in the release commit and must be checked into git",
                    manifest_path.display(),
                ),
            ));
        }
    }
    Ok(files.into_iter().collect())
}

fn normalize_include_path(manifest_path: &Path, path: &Path) -> ReleaseResult<PathBuf> {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Normal(part) => normalized.push(part),
            Component::CurDir => {}
            _ => {
                return Err(ReleaseError::IoError(
                    manifest_path.to_path_buf(),
                    format!(
                        "include path must be a relative path inside the module: {}",
                        path.display()
                    ),
                ))
            }
        }
    }
    if normalized.as_os_str().is_empty() {
        return Err(ReleaseError::IoError(
            manifest_path.to_path_buf(),
            "include path must not be empty".to_string(),
        ));
    }
    Ok(normalized)
}

pub(crate) fn collect_publish_source_files(
    repo_root: &Path,
    out_dir: &Path,
    mod_file: &ModFile,
    commit_tree: &CommitTree,
) -> ReleaseResult<Vec<SelectedSourceFile>> {
    let tracked_files = commit_tree
        .files
        .keys()
        .map(PathBuf::from)
        .collect::<HashSet<_>>();
    let mut files = collect_source_files(repo_root, out_dir, &tracked_files)?;
    let included_files = included_source_files(repo_root, mod_file, commit_tree)?;
    let explicitly_included = included_files
        .iter()
        .map(PathBuf::from)
        .collect::<HashSet<_>>();
    files.extend(included_files.iter().map(|path| repo_root.join(path)));
    let mut portable_paths = vo_module::schema::PortablePathSet::default();
    let mut selected = BTreeMap::new();
    for path in files {
        let rel = path
            .strip_prefix(repo_root)
            .map_err(|error| ReleaseError::IoError(path.clone(), error.to_string()))?;
        let rel = vo_module::schema::portable_relative_path_from_path(rel).map_err(|error| {
            ReleaseError::IoError(
                path.clone(),
                format!("published source path must be portable: {error}"),
            )
        })?;
        if vo_module::schema::is_reserved_module_cache_path(&rel) {
            return Err(ReleaseError::ManifestSerialize(format!(
                "published source path {rel:?} is reserved for module-cache metadata"
            )));
        }
        let is_explicit = explicitly_included.contains(Path::new(&rel));
        if let Some(previous) = selected.get_mut(&rel) {
            *previous |= is_explicit;
            continue;
        }
        portable_paths
            .insert_file(&rel)
            .map_err(ReleaseError::ManifestSerialize)?;
        selected.insert(rel, is_explicit);
    }
    if selected.len() >= vo_module::MAX_SOURCE_ARCHIVE_ENTRIES {
        return Err(ReleaseError::ManifestSerialize(format!(
            "published source set leaves no room for vo.web.json within the {}-entry archive limit",
            vo_module::MAX_SOURCE_ARCHIVE_ENTRIES
        )));
    }
    portable_paths
        .insert_file("vo.web.json")
        .map_err(ReleaseError::ManifestSerialize)?;
    selected
        .into_iter()
        .map(|(path, explicitly_included)| {
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
            Ok(SelectedSourceFile {
                tree_file,
                explicitly_included,
            })
        })
        .collect()
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
        let invalid_utf8 = std::str::from_utf8(&bytes).err();
        let is_vo_source = Path::new(&selected.tree_file.module_path)
            .extension()
            .and_then(OsStr::to_str)
            == Some("vo");
        if let Some(error) = invalid_utf8 {
            if is_vo_source || selected.explicitly_included {
                let reason = if is_vo_source {
                    "Vo source"
                } else {
                    "explicitly included source"
                };
                return Err(ReleaseError::ManifestSerialize(format!(
                    "{reason} {:?} in commit tree path {:?} is not valid UTF-8: {error}",
                    selected.tree_file.module_path, selected.tree_file.repository_path,
                )));
            }
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
        if path
            .file_name()
            .and_then(OsStr::to_str)
            .is_some_and(|name| portable_name_eq(name, "vo.sum"))
        {
            vo_sum_paths.push(path.to_path_buf());
        }
        if path
            .file_name()
            .and_then(OsStr::to_str)
            .is_some_and(|name| portable_name_eq(name, ".vo-project.lock"))
        {
            return Err(ReleaseError::ManifestSerialize(format!(
                "generated project transaction lock {:?} must not enter a module release",
                file.path
            )));
        }
        if let Some(name) = root_name {
            if portable_name_eq(name, "vo.work") {
                return Err(ReleaseError::ManifestSerialize(
                    "the root workspace file vo.work must not enter a module release".to_string(),
                ));
            }
            if portable_name_eq(name, "vo.release.json") || portable_name_eq(name, "vo.web.json") {
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

fn web_source_entries(snapshot: &ReleaseSourceSnapshot) -> ReleaseResult<Vec<SourceFileEntry>> {
    let mut entries = Vec::new();
    entries.try_reserve(snapshot.files.len()).map_err(|_| {
        ReleaseError::ManifestSerialize("failed to reserve browser source entries".into())
    })?;
    let mut total_bytes = 0usize;
    for file in &snapshot.files {
        if std::str::from_utf8(&file.bytes).is_err() {
            continue;
        }
        if file.bytes.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(ReleaseError::ManifestSerialize(format!(
                "browser text source {:?} exceeds the {}-byte text limit",
                file.path,
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )));
        }
        if entries.len() >= vo_common::vfs::MAX_PACKAGE_SOURCE_FILES {
            return Err(ReleaseError::ManifestSerialize(format!(
                "browser source set contains more than {} text files",
                vo_common::vfs::MAX_PACKAGE_SOURCE_FILES
            )));
        }
        if !vo_module::schema::is_source_file_set_candidate(&file.path)
            .map_err(ReleaseError::ManifestSerialize)?
        {
            return Err(ReleaseError::ManifestSerialize(format!(
                "published browser source path {:?} is owned by another release protocol",
                file.path
            )));
        }
        total_bytes = total_bytes.checked_add(file.bytes.len()).ok_or_else(|| {
            ReleaseError::ManifestSerialize("browser source size total overflow".into())
        })?;
        if total_bytes > vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES {
            return Err(ReleaseError::ManifestSerialize(format!(
                "browser source set exceeds the {}-byte package limit",
                vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES
            )));
        }
        entries.push(SourceFileEntry {
            path: file.path.clone(),
            size: u64::try_from(file.bytes.len()).map_err(|_| {
                ReleaseError::ManifestSerialize("browser source file size exceeds u64".into())
            })?,
            digest: ModDigest::from_sha256(&file.bytes),
        });
    }
    entries.sort_by(|left, right| left.path.cmp(&right.path));
    Ok(entries)
}

fn build_web_manifest_json(
    mod_file: &ModFile,
    version: &str,
    commit: &str,
    prepared_artifacts: &[PreparedArtifact],
    source_entries: &[SourceFileEntry],
    source_set_digest: &str,
) -> ReleaseResult<String> {
    let module_path = mod_file.module.as_github().ok_or_else(|| {
        ReleaseError::ManifestSerialize(format!(
            "vo.web.json requires a canonical github module path; found {}",
            mod_file.module
        ))
    })?;
    let mut require = mod_file
        .require
        .iter()
        .map(|req| {
            serde_json::json!({
                "module": req.module.as_str(),
                "constraint": req.constraint.to_string(),
            })
        })
        .collect::<Vec<_>>();
    require.sort_by(|left, right| {
        left.get("module")
            .and_then(serde_json::Value::as_str)
            .cmp(&right.get("module").and_then(serde_json::Value::as_str))
    });
    let artifacts = web_manifest_artifacts(mod_file, prepared_artifacts)?;
    let web = web_manifest_project_metadata(mod_file)?;
    let extension = web_manifest_extension_metadata(mod_file)?;
    let manifest = serde_json::json!({
        "schema_version": 1,
        "module": module_path.as_str(),
        "version": version,
        "commit": commit,
        "module_root": module_path.module_root(),
        "vo": mod_file.vo.to_string(),
        "require": require,
        "source_digest": source_set_digest,
        "source": source_entries,
        "web": web,
        "extension": extension,
        "artifacts": artifacts,
    });
    let mut output = BoundedVecWriter::new(vo_common::vfs::MAX_TEXT_FILE_BYTES, "vo.web.json");
    serde_json::to_writer_pretty(&mut output, &manifest)
        .map_err(|error| ReleaseError::ManifestSerialize(error.to_string()))?;
    output
        .write_all(b"\n")
        .map_err(|error| ReleaseError::ManifestSerialize(error.to_string()))?;
    String::from_utf8(output.into_bytes()).map_err(|error| {
        ReleaseError::ManifestSerialize(format!(
            "generated vo.web.json is not valid UTF-8: {error}"
        ))
    })
}

fn web_manifest_project_metadata(mod_file: &ModFile) -> ReleaseResult<serde_json::Value> {
    match &mod_file.web {
        Some(web) => Ok(serde_json::json!({
            "entry": web.entry.as_ref(),
            "include": portable_manifest_paths(&web.include, "web.include")?,
        })),
        None => Ok(serde_json::Value::Null),
    }
}

fn web_manifest_extension_metadata(mod_file: &ModFile) -> ReleaseResult<serde_json::Value> {
    let Some(extension) = &mod_file.extension else {
        return Ok(serde_json::Value::Null);
    };
    Ok(serde_json::json!({
        "name": extension.name.as_str(),
        "include": portable_manifest_paths(&extension.include, "extension.include")?,
        "wasm": extension.wasm.as_ref(),
        "web": extension.web.as_ref(),
    }))
}

fn portable_manifest_paths(paths: &[PathBuf], field: &str) -> ReleaseResult<Vec<String>> {
    let mut portable = Vec::new();
    portable.try_reserve(paths.len()).map_err(|_| {
        ReleaseError::ManifestSerialize(format!(
            "failed to reserve generated vo.web.json {field} paths"
        ))
    })?;
    for path in paths {
        portable.push(
            vo_module::schema::portable_relative_path_from_path(path).map_err(|error| {
                ReleaseError::ManifestSerialize(format!(
                    "generated vo.web.json {field} path {} is not portable: {error}",
                    path.display()
                ))
            })?,
        );
    }
    Ok(portable)
}

fn web_manifest_artifacts(
    mod_file: &ModFile,
    prepared_artifacts: &[PreparedArtifact],
) -> ReleaseResult<Vec<serde_json::Value>> {
    let wasm = mod_file
        .extension
        .as_ref()
        .and_then(|extension| extension.wasm.as_ref());
    let mut artifacts = prepared_artifacts
        .iter()
        .filter(|artifact| {
            artifact.staged.target == WASM_TARGET
                && matches!(
                    artifact.staged.kind.as_str(),
                    "extension-wasm" | "extension-js-glue"
                )
        })
        .map(|artifact| {
            serde_json::json!({
                "kind": artifact.staged.kind.as_str(),
                "target": artifact.staged.target.as_str(),
                "name": artifact.staged.name.as_str(),
                "path": web_manifest_artifact_path(wasm, artifact),
                "size": artifact.staged.size,
                "digest": artifact.staged.digest.as_str(),
            })
        })
        .collect::<Vec<_>>();
    artifacts.sort_by(|a, b| {
        (
            a.get("kind").and_then(serde_json::Value::as_str),
            a.get("target").and_then(serde_json::Value::as_str),
            a.get("name").and_then(serde_json::Value::as_str),
        )
            .cmp(&(
                b.get("kind").and_then(serde_json::Value::as_str),
                b.get("target").and_then(serde_json::Value::as_str),
                b.get("name").and_then(serde_json::Value::as_str),
            ))
    });
    let mut paths = vo_module::schema::PortablePathSet::default();
    for (index, artifact) in artifacts.iter().enumerate() {
        let path = artifact
            .get("path")
            .and_then(serde_json::Value::as_str)
            .ok_or_else(|| {
                ReleaseError::ManifestSerialize(format!(
                    "vo.web.json artifact[{index}] is missing its path"
                ))
            })?;
        validate_portable_relative_path(path).map_err(|detail| {
            ReleaseError::ManifestSerialize(format!(
                "vo.web.json artifact[{index}] path {path:?}: {detail}"
            ))
        })?;
        if !paths
            .insert_file(path)
            .map_err(ReleaseError::ManifestSerialize)?
        {
            return Err(ReleaseError::ManifestSerialize(format!(
                "vo.web.json artifacts reuse path {path:?}"
            )));
        }
    }
    Ok(artifacts)
}

fn validate_portable_relative_path(path: &str) -> Result<(), &'static str> {
    vo_module::schema::validate_portable_relative_path(path)
        .map_err(|_| "must be a normalized portable module-relative path")
}

fn web_manifest_artifact_path(
    wasm: Option<&vo_module::ext_manifest::WasmExtensionManifest>,
    artifact: &PreparedArtifact,
) -> String {
    let Some(wasm) = wasm else {
        return artifact.staged.name.clone();
    };
    if artifact.staged.kind == "extension-wasm"
        && artifact.staged.target == WASM_TARGET
        && artifact.staged.name == wasm.wasm
    {
        return wasm.local_wasm.clone().unwrap_or_else(|| wasm.wasm.clone());
    }
    if artifact.staged.kind == "extension-js-glue"
        && artifact.staged.target == WASM_TARGET
        && wasm.js_glue.as_ref() == Some(&artifact.staged.name)
    {
        return wasm
            .local_js_glue
            .clone()
            .unwrap_or_else(|| artifact.staged.name.clone());
    }
    artifact.staged.name.clone()
}

pub(crate) fn build_source_package(
    top_dir: &str,
    snapshot: &ReleaseSourceSnapshot,
    web_manifest_json: &str,
) -> ReleaseResult<Vec<u8>> {
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
    let compressed_limit =
        usize::try_from(vo_module::MAX_SOURCE_ARCHIVE_BYTES).unwrap_or(usize::MAX);
    let encoder = GzBuilder::new().mtime(0).write(
        BoundedVecWriter::new(compressed_limit, "compressed source package"),
        Compression::default(),
    );
    let mut builder = Builder::new(encoder);
    if snapshot.extracted_size > vo_module::MAX_EXTRACTED_SOURCE_BYTES {
        return Err(ReleaseError::ManifestSerialize(format!(
            "source package extracted content exceeds the {}-byte limit",
            vo_module::MAX_EXTRACTED_SOURCE_BYTES
        )));
    }
    for file in &snapshot.files {
        let mut header = Header::new_gnu();
        header.set_size(u64::try_from(file.bytes.len()).map_err(|_| {
            ReleaseError::ManifestSerialize(format!("source file {:?} size exceeds u64", file.path))
        })?);
        header.set_mode(file.mode);
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
    if web_manifest_json.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
        return Err(ReleaseError::ManifestSerialize(format!(
            "vo.web.json exceeds the {}-byte text limit",
            vo_common::vfs::MAX_TEXT_FILE_BYTES
        )));
    }
    let extracted_bytes = snapshot
        .extracted_size
        .checked_add(web_manifest_json.len())
        .ok_or_else(|| {
            ReleaseError::ManifestSerialize("source package extracted size overflow".into())
        })?;
    if extracted_bytes > vo_module::MAX_EXTRACTED_SOURCE_BYTES {
        return Err(ReleaseError::ManifestSerialize(format!(
            "source package extracted content exceeds the {}-byte limit",
            vo_module::MAX_EXTRACTED_SOURCE_BYTES
        )));
    }
    append_virtual_file(
        &mut builder,
        top_dir,
        Path::new("vo.web.json"),
        web_manifest_json.as_bytes(),
    )?;
    let encoder = builder
        .into_inner()
        .map_err(|error| ReleaseError::ManifestSerialize(error.to_string()))?;
    encoder
        .finish()
        .map(BoundedVecWriter::into_bytes)
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

fn git_tracked_files(repo_root: &Path) -> ReleaseResult<HashSet<PathBuf>> {
    let mut command = Command::new("git");
    command.arg("-C").arg(repo_root).arg("ls-files").arg("-z");
    let mut tracked = HashSet::new();
    let mut portable_paths = vo_module::schema::PortablePathSet::default();
    run_git_nul_records(&mut command, repo_root, "git ls-files", |raw| {
        if raw.is_empty() {
            return Err(ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: "git ls-files returned an empty path".to_string(),
            });
        }
        let path = std::str::from_utf8(raw).map_err(|error| ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: format!("git ls-files returned a non-UTF-8 path: {error}"),
        })?;
        vo_module::schema::validate_portable_relative_path(path).map_err(|error| {
            ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: format!("tracked path {path:?} is not portable: {error}"),
            }
        })?;
        if !portable_paths
            .insert_file(path)
            .map_err(ReleaseError::ManifestSerialize)?
        {
            return Err(ReleaseError::ManifestSerialize(format!(
                "repository contains a duplicate portable tracked path {path:?}"
            )));
        }
        if !tracked.insert(PathBuf::from(path)) {
            return Err(ReleaseError::GitError {
                repo_root: repo_root.to_path_buf(),
                message: format!("git ls-files returned duplicate path {path:?}"),
            });
        }
        Ok(())
    })?;
    Ok(tracked)
}

fn validate_tracked_protocol_file_names(repo_root: &Path) -> ReleaseResult<HashSet<PathBuf>> {
    let tracked = git_tracked_files(repo_root)?;
    validate_exact_tracked_root_file(&tracked, "vo.mod", true)?;
    validate_exact_tracked_root_file(&tracked, "vo.lock", false)?;
    Ok(tracked)
}

fn validate_exact_tracked_root_file(
    tracked: &HashSet<PathBuf>,
    expected: &str,
    required: bool,
) -> ReleaseResult<()> {
    let mut aliases = tracked
        .iter()
        .filter(|path| {
            path.parent()
                .is_some_and(|parent| parent.as_os_str().is_empty())
                && path
                    .file_name()
                    .and_then(OsStr::to_str)
                    .is_some_and(|name| portable_name_eq(name, expected))
                && path.as_path() != Path::new(expected)
        })
        .map(|path| path.display().to_string())
        .collect::<Vec<_>>();
    aliases.sort();
    if !aliases.is_empty() {
        return Err(ReleaseError::ManifestSerialize(format!(
            "tracked protocol file {expected} has non-canonical case alias(es): {}",
            aliases.join(", ")
        )));
    }
    if required && !tracked.contains(Path::new(expected)) {
        return Err(ReleaseError::ManifestSerialize(format!(
            "release repository must track the exact root path {expected}"
        )));
    }
    Ok(())
}

fn collect_source_files(
    root: &Path,
    out_dir: &Path,
    tracked_files: &HashSet<PathBuf>,
) -> ReleaseResult<Vec<PathBuf>> {
    let mut files = Vec::new();
    let mut tracked = tracked_files.iter().collect::<Vec<_>>();
    tracked.sort();
    for rel in tracked {
        let path = root.join(rel);
        if path.starts_with(out_dir) {
            continue;
        }
        if should_include_source_file(root, &path) {
            files.push(path);
        }
    }
    Ok(files)
}

fn find_named_files(repo_root: &Path, file_name: &str) -> ReleaseResult<Vec<PathBuf>> {
    let mut entry_count = 0usize;
    find_named_files_inner(repo_root, repo_root, file_name, 0, &mut entry_count)
}

fn find_named_files_inner(
    repo_root: &Path,
    dir: &Path,
    file_name: &str,
    depth: usize,
    entry_count: &mut usize,
) -> ReleaseResult<Vec<PathBuf>> {
    if depth > MAX_RELEASE_SCAN_DEPTH {
        return Err(ReleaseError::IoError(
            dir.to_path_buf(),
            format!("repository scan exceeds the {MAX_RELEASE_SCAN_DEPTH}-level depth limit"),
        ));
    }
    let mut matches = Vec::new();
    let entries = read_sorted_directory_entries(dir, entry_count)?;
    for path in entries {
        let metadata = fs::symlink_metadata(&path)
            .map_err(|error| ReleaseError::IoError(path.clone(), error.to_string()))?;
        if metadata.file_type().is_symlink() {
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
            continue;
        }
        if metadata.file_type().is_dir() {
            if is_ignored_dir(&path) {
                continue;
            }
            matches.extend(find_named_files_inner(
                repo_root,
                &path,
                file_name,
                depth + 1,
                entry_count,
            )?);
            continue;
        }
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

fn find_alias_imports(repo_root: &Path) -> ReleaseResult<Vec<String>> {
    let mut entry_count = 0usize;
    find_alias_imports_inner(repo_root, repo_root, 0, &mut entry_count)
}

fn find_alias_imports_inner(
    repo_root: &Path,
    dir: &Path,
    depth: usize,
    entry_count: &mut usize,
) -> ReleaseResult<Vec<String>> {
    if depth > MAX_RELEASE_SCAN_DEPTH {
        return Err(ReleaseError::IoError(
            dir.to_path_buf(),
            format!("repository scan exceeds the {MAX_RELEASE_SCAN_DEPTH}-level depth limit"),
        ));
    }
    let mut violations = Vec::new();
    let entries = read_sorted_directory_entries(dir, entry_count)?;
    for path in entries {
        let metadata = fs::symlink_metadata(&path)
            .map_err(|error| ReleaseError::IoError(path.clone(), error.to_string()))?;
        if metadata.file_type().is_symlink() {
            continue;
        }
        if metadata.file_type().is_dir() {
            if is_ignored_dir(&path) {
                continue;
            }
            violations.extend(find_alias_imports_inner(
                repo_root,
                &path,
                depth + 1,
                entry_count,
            )?);
            if violations.len() > vo_module::MAX_MODULE_METADATA_ENTRIES {
                return Err(ReleaseError::ManifestSerialize(format!(
                    "alias import scan found more than {} violations",
                    vo_module::MAX_MODULE_METADATA_ENTRIES
                )));
            }
            continue;
        }
        if path.extension().and_then(OsStr::to_str) != Some("vo") {
            continue;
        }
        let content = vo_common::vfs::read_text_file(&path)
            .map_err(|error| ReleaseError::IoError(path.clone(), error.to_string()))?;
        let rel = path
            .strip_prefix(repo_root)
            .map_err(|error| ReleaseError::IoError(path.clone(), error.to_string()))?;
        collect_alias_import_violations(rel, &content, &mut violations)?;
    }
    Ok(violations)
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

fn should_include_source_file(repo_root: &Path, path: &Path) -> bool {
    let Ok(rel) = path.strip_prefix(repo_root) else {
        return false;
    };
    if rel
        .parent()
        .map(|parent| {
            parent.components().any(|component| {
                let Some(name) = component.as_os_str().to_str() else {
                    return false;
                };
                portable_name_matches_any(name, IGNORED_DIR_NAMES)
                    || is_release_output_dir_name(name)
            })
        })
        .unwrap_or(false)
    {
        return false;
    }
    let Some(file_name) = path.file_name().and_then(OsStr::to_str) else {
        return false;
    };
    if portable_name_matches_any(file_name, IGNORED_FILE_NAMES) {
        return false;
    }
    !IGNORED_SUFFIXES
        .iter()
        .any(|suffix| portable_name_ends_with(file_name, suffix))
}

fn is_ignored_dir(path: &Path) -> bool {
    path.file_name()
        .and_then(OsStr::to_str)
        .map(|name| {
            portable_name_matches_any(name, IGNORED_DIR_NAMES) || is_release_output_dir_name(name)
        })
        .unwrap_or(false)
}

fn is_release_output_dir_name(name: &str) -> bool {
    portable_name_eq(name, ".dist")
        || portable_name_starts_with(name, ".dist-")
        || has_release_stage_dir_prefix(name)
}

fn has_release_stage_dir_prefix(name: &str) -> bool {
    portable_name_starts_with(name, RELEASE_STAGE_DIR_PREFIX)
}

fn sha256_digest(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("sha256:{:x}", hasher.finalize())
}

fn source_package_base_name(module: &str) -> Option<&str> {
    let parts: Vec<&str> = module.split('/').collect();
    if parts.len() < 3 || parts[0] != "github.com" {
        return None;
    }
    let tail = &parts[3..];
    if tail.is_empty() {
        return Some(parts[2]);
    }
    let last = tail[tail.len() - 1];
    if is_major_version_suffix(last) {
        if tail.len() == 1 {
            Some(parts[2])
        } else {
            Some(tail[tail.len() - 2])
        }
    } else {
        Some(last)
    }
}

fn is_major_version_suffix(value: &str) -> bool {
    value.len() > 1
        && value.starts_with('v')
        && value[1..].chars().all(|char| char.is_ascii_digit())
}

fn read_mod_file(repo_root: &Path) -> ReleaseResult<ModFile> {
    project::read_mod_file(repo_root)
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
            "vo.mod has require directives but vo.lock is missing; run: vo mod sync".to_string(),
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
        assert!(portable_name_matches_any("vo.web.jſon", &["vo.web.json"]));
        assert!(portable_name_starts_with(".diſt-output", ".dist-"));
        assert!(portable_name_starts_with(".diﬆ-output", ".dist-"));
        assert!(portable_name_ends_with("lib.ſo", ".so"));
        assert!(!portable_name_eq("i", "ı"));
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
