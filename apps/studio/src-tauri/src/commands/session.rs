use std::fs;
use std::io::Read;
use std::path::{Component, Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use flate2::read::GzDecoder;
use serde::de::DeserializeOwned;
use tar::Archive;
use url::Url;
use vo_module::schema::PortablePathSet;

use super::pathing::{find_project_root, is_link_or_reparse_point, is_module_root};
use crate::state::{
    session_info, AppState, LaunchSpec, PreparedSession, SessionInfo, SessionIsolation,
    SessionOrigin, SessionSnapshot, SessionSource, WorkspaceDiscoveryMode,
};

#[derive(serde::Deserialize)]
struct GitHubRepoMetadata {
    default_branch: Option<String>,
    html_url: Option<String>,
}

#[derive(serde::Deserialize)]
struct GitHubCommitMetadata {
    sha: Option<String>,
}

struct GitHubRepoInput {
    owner: String,
    repo: String,
    ref_: Option<String>,
    commit: Option<String>,
    subdir: Option<String>,
}

struct ResolvedGitHubSource {
    owner: String,
    repo: String,
    requested_ref: Option<String>,
    resolved_commit: String,
    subdir: Option<String>,
    html_url: String,
    source_cache_root: PathBuf,
    session_root: PathBuf,
    project_root: PathBuf,
}

const MAX_GITHUB_ARCHIVE_FILES: usize = 20_000;
const MAX_GITHUB_ARCHIVE_RESPONSE_BYTES: usize = 256 * 1024 * 1024;
const MAX_GITHUB_ARCHIVE_FILE_BYTES: usize = 256 * 1024 * 1024;
const MAX_GITHUB_ARCHIVE_TOTAL_BYTES: usize = 512 * 1024 * 1024;

#[tauri::command]
pub fn cmd_prepare_session(
    spec: LaunchSpec,
    state: tauri::State<'_, AppState>,
) -> Result<PreparedSession, String> {
    let prepared = prepare_project_impl(spec.proj.as_deref(), spec.isolation, state.inner())?;
    Ok(state.prepare_session(prepared.info, prepared.snapshot))
}

#[tauri::command]
pub fn cmd_activate_session(
    candidate: PreparedSession,
    state: tauri::State<'_, AppState>,
) -> Result<SessionInfo, String> {
    state.activate_session(candidate)
}

#[tauri::command]
pub fn cmd_discard_prepared_session(
    candidate: PreparedSession,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    state.discard_prepared_session(&candidate)
}

#[tauri::command]
pub fn cmd_restore_session(
    previous: SessionInfo,
    state: tauri::State<'_, AppState>,
) -> Result<SessionInfo, String> {
    state.restore_session(&previous)
}

struct PreparedSessionData {
    info: SessionInfo,
    snapshot: SessionSnapshot,
}

fn prepare_project_impl(
    proj: Option<&str>,
    isolation: Option<SessionIsolation>,
    state: &AppState,
) -> Result<PreparedSessionData, String> {
    let Some(proj) = proj.map(str::trim).filter(|value| !value.is_empty()) else {
        if isolation.is_some() {
            return Err("Session isolation requires a project path".to_string());
        }
        return open_workspace_session_impl(state);
    };
    if isolation == Some(SessionIsolation::SingleFile) {
        if !is_local_project_path(proj) {
            return Err("Single-file isolation requires a local Vo file".to_string());
        }
        return open_isolated_file_session_impl(strip_file_prefix(proj));
    }
    if is_local_project_path(proj) {
        return open_run_session_impl(strip_file_prefix(proj));
    }
    if let Some(source) = parse_github_repo_url(proj) {
        return open_github_session_impl(
            source.owner,
            source.repo,
            source.ref_,
            source.commit,
            source.subdir,
            state,
        );
    }
    Err(format!("Unsupported project URL: {proj}"))
}

struct IsolatedFileTarget {
    root: PathBuf,
    entry: PathBuf,
}

fn resolve_isolated_file_target(path: String) -> Result<IsolatedFileTarget, String> {
    let target = PathBuf::from(&path);
    let canonical = target
        .canonicalize()
        .map_err(|error| format!("{}: {}", target.display(), error))?;
    if !canonical.is_file()
        || canonical
            .extension()
            .and_then(|extension| extension.to_str())
            != Some("vo")
    {
        return Err(format!(
            "Isolated session target must be a Vo file: {}",
            canonical.display()
        ));
    }
    let root = canonical
        .parent()
        .ok_or_else(|| format!("No parent for {}", canonical.display()))?
        .to_path_buf();
    Ok(IsolatedFileTarget {
        root,
        entry: canonical,
    })
}

fn open_isolated_file_session_impl(path: String) -> Result<PreparedSessionData, String> {
    let target = resolve_isolated_file_target(path)?;
    let info = isolated_file_session_info(&target)?;
    Ok(PreparedSessionData {
        info,
        snapshot: SessionSnapshot::new(target.root, true, WorkspaceDiscoveryMode::Disabled),
    })
}

fn isolated_file_session_info(target: &IsolatedFileTarget) -> Result<SessionInfo, String> {
    let source_path = target.entry.to_string_lossy().to_string();
    session_info(
        &target.root,
        SessionOrigin::RunTarget,
        Some(&target.entry),
        true,
        WorkspaceDiscoveryMode::Disabled,
        Some(SessionSource::Path { path: source_path }),
    )
}

fn open_run_session_impl(path: String) -> Result<PreparedSessionData, String> {
    let target = PathBuf::from(&path);
    if !target.exists() {
        return Err(format!("Path not found: {}", target.display()));
    }
    let canonical = target
        .canonicalize()
        .map_err(|err| format!("{}: {}", target.display(), err))?;

    let is_vo_file =
        canonical.is_file() && canonical.extension().and_then(|e| e.to_str()) == Some("vo");

    let module_root = if is_vo_file {
        find_project_root(&canonical)?
    } else if is_module_root(&canonical)? {
        Some(canonical.clone())
    } else {
        None
    };

    let (session_root, is_single_file) = if let Some(mod_root) = module_root {
        // Inside a module project — compile the whole module.
        (mod_root, false)
    } else if is_vo_file {
        // Standalone single file — isolate it.
        let parent = canonical
            .parent()
            .ok_or_else(|| format!("No parent for {}", canonical.display()))?
            .to_path_buf();
        (parent, true)
    } else {
        // Directory without vo.mod
        (canonical.clone(), false)
    };

    let entry = if canonical.is_file() {
        Some(canonical.as_path())
    } else {
        None
    };
    let source_path = canonical.to_string_lossy().to_string();
    let info = session_info(
        &session_root,
        SessionOrigin::RunTarget,
        entry,
        is_single_file,
        WorkspaceDiscoveryMode::Auto,
        Some(SessionSource::Path { path: source_path }),
    )?;
    Ok(PreparedSessionData {
        info,
        snapshot: SessionSnapshot::new(session_root, is_single_file, WorkspaceDiscoveryMode::Auto),
    })
}

fn open_workspace_session_impl(state: &AppState) -> Result<PreparedSessionData, String> {
    let root = state.workspace_root().to_path_buf();
    std::fs::create_dir_all(&root).map_err(|err| format!("{}: {}", root.display(), err))?;
    let info = session_info(
        &root,
        SessionOrigin::Workspace,
        None,
        false,
        WorkspaceDiscoveryMode::Auto,
        Some(SessionSource::Workspace),
    )?;
    Ok(PreparedSessionData {
        info,
        snapshot: SessionSnapshot::new(root, false, WorkspaceDiscoveryMode::Auto),
    })
}

fn is_local_project_path(value: &str) -> bool {
    value.starts_with('/')
        || value.starts_with("file://")
        || (!value.contains("://") && Path::new(value).exists())
}

fn strip_file_prefix(value: &str) -> String {
    value.strip_prefix("file://").unwrap_or(value).to_string()
}

fn open_github_session_impl(
    owner: String,
    repo: String,
    ref_: Option<String>,
    commit: Option<String>,
    subdir: Option<String>,
    state: &AppState,
) -> Result<PreparedSessionData, String> {
    let resolved = resolve_github_source(
        &owner,
        &repo,
        ref_.as_deref(),
        commit.as_deref(),
        subdir.as_deref(),
        state.workspace_root(),
    )?;
    populate_github_source_cache(&resolved)?;
    materialize_github_session(&resolved)?;
    let entry_path = detect_import_entry(&resolved.project_root);
    let info = session_info(
        &resolved.project_root,
        SessionOrigin::Url,
        entry_path.as_deref(),
        false,
        WorkspaceDiscoveryMode::Disabled,
        Some(SessionSource::GithubRepo {
            owner: resolved.owner.clone(),
            repo: resolved.repo.clone(),
            requested_ref: resolved.requested_ref.clone(),
            resolved_commit: Some(resolved.resolved_commit.clone()),
            subdir: resolved.subdir.clone(),
            html_url: resolved.html_url.clone(),
            source_cache_root: resolved.source_cache_root.to_string_lossy().to_string(),
        }),
    )?;
    Ok(PreparedSessionData {
        info,
        snapshot: SessionSnapshot::new(
            resolved.project_root,
            false,
            WorkspaceDiscoveryMode::Disabled,
        ),
    })
}

fn fetch_bytes_blocking(url: &str, max_bytes: usize) -> Result<Vec<u8>, String> {
    fetch_bytes_with_headers(url, &[], max_bytes)
}

fn fetch_bytes_with_headers(
    url: &str,
    headers: &[(&str, &str)],
    max_bytes: usize,
) -> Result<Vec<u8>, String> {
    let mut request = ureq::get(url).set("User-Agent", "Vo-Studio");
    for (name, value) in headers {
        request = request.set(name, value);
    }
    let response = request
        .call()
        .map_err(|e| format!("HTTP fetch failed for {}: {}", url, e))?;
    if let Some(content_length) = response.header("Content-Length") {
        let content_length = content_length.parse::<u64>().map_err(|error| {
            format!("invalid Content-Length from {url}: {content_length:?}: {error}")
        })?;
        if content_length > u64::try_from(max_bytes).unwrap_or(u64::MAX) {
            return Err(format!(
                "response body from {url} exceeds the {max_bytes}-byte limit"
            ));
        }
    }
    read_response_limited(response.into_reader(), max_bytes, url)
}

fn read_response_limited(
    mut reader: impl Read,
    max_bytes: usize,
    url: &str,
) -> Result<Vec<u8>, String> {
    let mut bytes = Vec::new();
    let mut buffer = [0_u8; 8 * 1024];
    loop {
        let remaining = max_bytes.saturating_sub(bytes.len());
        let read_limit = buffer.len().min(remaining.saturating_add(1));
        let count = match reader.read(&mut buffer[..read_limit]) {
            Ok(count) => count,
            Err(error) if error.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(error) => {
                return Err(format!("failed to read response body from {url}: {error}"));
            }
        };
        if count == 0 {
            return Ok(bytes);
        }
        if count > remaining {
            return Err(format!(
                "response body from {url} exceeds the {max_bytes}-byte limit"
            ));
        }
        bytes
            .try_reserve(count)
            .map_err(|_| format!("failed to reserve response buffer for {url}"))?;
        bytes.extend_from_slice(&buffer[..count]);
    }
}

fn fetch_json_blocking<T: DeserializeOwned>(url: &str) -> Result<T, String> {
    let bytes = fetch_bytes_with_headers(
        url,
        &[("Accept", "application/vnd.github+json")],
        vo_module::github_provenance::MAX_GITHUB_API_RESPONSE_BYTES,
    )?;
    serde_json::from_slice(&bytes)
        .map_err(|err| format!("failed to decode JSON response from {}: {}", url, err))
}

fn resolve_github_source(
    owner: &str,
    repo: &str,
    ref_: Option<&str>,
    commit: Option<&str>,
    subdir: Option<&str>,
    workspace_root: &Path,
) -> Result<ResolvedGitHubSource, String> {
    let repo_api = format!("https://api.github.com/repos/{}/{}", owner, repo);
    let repo_url = format!("https://github.com/{}/{}", owner, repo);
    let mut requested_ref = ref_.map(|value| value.to_string());
    let mut resolved_commit = commit.map(|value| value.to_string());
    let mut html_url = repo_url.clone();
    if resolved_commit.is_none() {
        let repo_info: GitHubRepoMetadata = fetch_json_blocking(&repo_api)?;
        if let Some(value) = repo_info.html_url {
            if !value.trim().is_empty() {
                html_url = value;
            }
        }
        if requested_ref.is_none() {
            requested_ref = repo_info
                .default_branch
                .filter(|value| !value.trim().is_empty());
        }
        let target_ref = requested_ref
            .as_deref()
            .ok_or_else(|| format!("could not resolve a GitHub ref for {}/{}", owner, repo))?;
        let commit_info: GitHubCommitMetadata =
            fetch_json_blocking(&format!("{}/commits/{}", repo_api, target_ref))?;
        resolved_commit = commit_info.sha.filter(|value| !value.trim().is_empty());
    }
    let resolved_commit = resolved_commit
        .ok_or_else(|| format!("could not resolve commit for {}/{}", owner, repo))?;
    let normalized_subdir = match subdir {
        Some(value) => Some(normalize_relative_path(value)?),
        None => None,
    };
    let source_cache_root = workspace_root
        .join(".volang/apps/studio/sources")
        .join("github")
        .join(owner)
        .join(repo)
        .join(&resolved_commit);
    let session_root = workspace_root
        .join(".volang/apps/studio/sessions")
        .join("github")
        .join(owner)
        .join(repo)
        .join(&resolved_commit)
        .join(session_nonce());
    let project_root = normalized_subdir
        .as_ref()
        .map(|value| session_root.join(value))
        .unwrap_or_else(|| session_root.clone());
    Ok(ResolvedGitHubSource {
        owner: owner.to_string(),
        repo: repo.to_string(),
        requested_ref,
        resolved_commit,
        subdir: normalized_subdir.map(|value| value.to_string_lossy().to_string()),
        html_url,
        source_cache_root,
        session_root,
        project_root,
    })
}

fn populate_github_source_cache(resolved: &ResolvedGitHubSource) -> Result<(), String> {
    if path_has_tree(&resolved.source_cache_root)? {
        return Ok(());
    }
    remove_path_no_follow(&resolved.source_cache_root)?;
    fs::create_dir_all(&resolved.source_cache_root)
        .map_err(|err| format!("{}: {}", resolved.source_cache_root.display(), err))?;
    let archive_url = format!(
        "https://api.github.com/repos/{}/{}/tarball/{}",
        resolved.owner, resolved.repo, resolved.resolved_commit,
    );
    let archive_bytes = fetch_bytes_blocking(&archive_url, MAX_GITHUB_ARCHIVE_RESPONSE_BYTES)?;
    extract_tar_gz_project(&archive_bytes, &resolved.source_cache_root)
}

fn materialize_github_session(resolved: &ResolvedGitHubSource) -> Result<(), String> {
    remove_path_no_follow(&resolved.session_root)?;
    copy_dir_recursive(&resolved.source_cache_root, &resolved.session_root)?;
    if !path_has_tree(&resolved.project_root)? {
        return Err(format!(
            "GitHub project root not found: {}",
            resolved.project_root.display()
        ));
    }
    let workfile = resolved.project_root.join("vo.work");
    if workfile.is_file() {
        fs::remove_file(&workfile).map_err(|err| format!("{}: {}", workfile.display(), err))?;
    }
    Ok(())
}

fn copy_dir_recursive(source_root: &Path, target_root: &Path) -> Result<(), String> {
    let metadata = fs::symlink_metadata(source_root)
        .map_err(|err| format!("{}: {}", source_root.display(), err))?;
    if is_link_or_reparse_point(&metadata) || !metadata.file_type().is_dir() {
        return Err(format!(
            "GitHub source cache root must be a real directory: {}",
            source_root.display(),
        ));
    }
    let mut portable_paths = PortablePathSet::default();
    copy_dir_recursive_inner(source_root, source_root, target_root, &mut portable_paths)
}

fn copy_dir_recursive_inner(
    source_root: &Path,
    source_dir: &Path,
    target_dir: &Path,
    portable_paths: &mut PortablePathSet,
) -> Result<(), String> {
    fs::create_dir_all(target_dir).map_err(|err| format!("{}: {}", target_dir.display(), err))?;
    let mut entries = fs::read_dir(source_dir)
        .map_err(|err| format!("{}: {}", source_dir.display(), err))?
        .collect::<Result<Vec<_>, _>>()
        .map_err(|err| format!("{}: {}", source_dir.display(), err))?;
    entries.sort_by_key(|entry| entry.file_name());
    for entry in entries {
        let source_path = entry.path();
        let relative = source_path.strip_prefix(source_root).map_err(|_| {
            format!(
                "GitHub source entry escapes cache root: {}",
                source_path.display(),
            )
        })?;
        let portable =
            vo_module::schema::portable_relative_path_from_path(relative).map_err(|error| {
                format!(
                    "invalid GitHub source path '{}': {error}",
                    relative.display()
                )
            })?;
        let target_path = target_dir.join(entry.file_name());
        let metadata = fs::symlink_metadata(&source_path)
            .map_err(|err| format!("{}: {}", source_path.display(), err))?;
        let file_type = metadata.file_type();
        if is_link_or_reparse_point(&metadata) {
            return Err(format!(
                "GitHub source tree contains unsupported symbolic link or reparse point: {}",
                source_path.display(),
            ));
        }
        if file_type.is_dir() {
            if !portable_paths
                .insert_directory(&portable)
                .map_err(|error| format!("invalid GitHub source path '{portable}': {error}"))?
            {
                return Err(format!("duplicate GitHub source directory: {portable}"));
            }
            copy_dir_recursive_inner(source_root, &source_path, &target_path, portable_paths)?;
        } else if file_type.is_file() {
            if !portable_paths
                .insert_file(&portable)
                .map_err(|error| format!("invalid GitHub source path '{portable}': {error}"))?
            {
                return Err(format!("duplicate GitHub source file: {portable}"));
            }
            if let Some(parent) = target_path.parent() {
                fs::create_dir_all(parent)
                    .map_err(|err| format!("{}: {}", parent.display(), err))?;
            }
            fs::copy(&source_path, &target_path)
                .map(|_| ())
                .map_err(|err| {
                    format!(
                        "{} -> {}: {}",
                        source_path.display(),
                        target_path.display(),
                        err
                    )
                })?;
        } else {
            return Err(format!(
                "GitHub source tree contains unsupported special file: {}",
                source_path.display(),
            ));
        }
    }
    Ok(())
}

fn remove_path_no_follow(path: &Path) -> Result<(), String> {
    let metadata = match fs::symlink_metadata(path) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(error) => return Err(format!("{}: {}", path.display(), error)),
    };
    if metadata.file_type().is_dir() {
        fs::remove_dir_all(path).map_err(|error| format!("{}: {}", path.display(), error))
    } else {
        fs::remove_file(path).map_err(|error| format!("{}: {}", path.display(), error))
    }
}

fn path_has_tree(path: &Path) -> Result<bool, String> {
    let metadata = match fs::symlink_metadata(path) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(false),
        Err(error) => return Err(format!("{}: {}", path.display(), error)),
    };
    let file_type = metadata.file_type();
    if is_link_or_reparse_point(&metadata) {
        return Err(format!(
            "path must not be a symbolic link or reparse point: {}",
            path.display()
        ));
    }
    if file_type.is_file() {
        return Ok(true);
    }
    if !file_type.is_dir() {
        return Err(format!(
            "path is not a regular file or directory: {}",
            path.display()
        ));
    }
    let mut entries = fs::read_dir(path).map_err(|err| format!("{}: {}", path.display(), err))?;
    Ok(entries
        .next()
        .transpose()
        .map_err(|err| format!("{}: {}", path.display(), err))?
        .is_some())
}

fn normalize_relative_path(path: &str) -> Result<PathBuf, String> {
    sanitize_archive_path(Path::new(path)).ok_or_else(|| format!("invalid GitHub subdir: {}", path))
}

fn session_nonce() -> String {
    let nonce = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    format!("{:x}", nonce)
}

fn parse_github_repo_url(value: &str) -> Option<GitHubRepoInput> {
    let parsed = Url::parse(value).ok()?;
    let host = parsed.host_str()?.to_ascii_lowercase();
    if host != "github.com" && host != "www.github.com" {
        return None;
    }
    let parts: Vec<_> = parsed
        .path_segments()?
        .filter(|segment| !segment.is_empty())
        .collect();
    if parts.len() < 2 {
        return None;
    }
    let owner = parts[0].to_string();
    let repo = parts[1].trim_end_matches(".git").to_string();
    if owner.is_empty() || repo.is_empty() {
        return None;
    }
    if parts.len() == 2 {
        return Some(GitHubRepoInput {
            owner,
            repo,
            ref_: None,
            commit: None,
            subdir: None,
        });
    }
    if parts[2] != "tree" || parts.len() < 4 {
        return None;
    }
    Some(GitHubRepoInput {
        owner,
        repo,
        ref_: Some(parts[3].to_string()),
        commit: None,
        subdir: if parts.len() > 4 {
            Some(parts[4..].join("/"))
        } else {
            None
        },
    })
}

fn extract_tar_gz_project(bytes: &[u8], session_root: &Path) -> Result<(), String> {
    let gz = GzDecoder::new(bytes);
    let mut archive = Archive::new(gz);
    let mut files = Vec::<(PathBuf, Vec<u8>)>::new();
    let mut directories = Vec::<PathBuf>::new();

    let mut total_bytes = 0usize;
    for entry in archive.entries().map_err(|err| err.to_string())? {
        let entry = entry.map_err(|err| err.to_string())?;
        let entry_type = entry.header().entry_type();
        if entry_type.is_dir() {
            if files.len().saturating_add(directories.len()) >= MAX_GITHUB_ARCHIVE_FILES * 2 {
                return Err(format!(
                    "archive contains more than {} entries",
                    MAX_GITHUB_ARCHIVE_FILES * 2,
                ));
            }
            let raw_path = entry.path().map_err(|err| err.to_string())?.into_owned();
            let path = sanitize_archive_path(&raw_path)
                .ok_or_else(|| format!("archive contains invalid path: {}", raw_path.display()))?;
            directories.push(path);
            continue;
        }
        if !entry_type.is_file() {
            return Err("archive contains unsupported link or special entry".to_string());
        }
        if files.len() >= MAX_GITHUB_ARCHIVE_FILES {
            return Err(format!(
                "archive contains more than {MAX_GITHUB_ARCHIVE_FILES} files"
            ));
        }
        let raw_path = entry.path().map_err(|err| err.to_string())?.into_owned();
        let path = sanitize_archive_path(&raw_path)
            .ok_or_else(|| format!("archive contains invalid path: {}", raw_path.display()))?;
        let advertised = usize::try_from(entry.size()).unwrap_or(usize::MAX);
        if advertised > MAX_GITHUB_ARCHIVE_FILE_BYTES {
            return Err(format!(
                "archive entry {} exceeds the {}-byte file limit",
                path.display(),
                MAX_GITHUB_ARCHIVE_FILE_BYTES,
            ));
        }
        let mut buf = Vec::new();
        entry
            .take(u64::try_from(MAX_GITHUB_ARCHIVE_FILE_BYTES).unwrap_or(u64::MAX) + 1)
            .read_to_end(&mut buf)
            .map_err(|err| err.to_string())?;
        if buf.len() > MAX_GITHUB_ARCHIVE_FILE_BYTES {
            return Err(format!(
                "archive entry {} exceeds the {}-byte file limit",
                path.display(),
                MAX_GITHUB_ARCHIVE_FILE_BYTES,
            ));
        }
        total_bytes = total_bytes
            .checked_add(buf.len())
            .ok_or_else(|| "archive content size overflow".to_string())?;
        if total_bytes > MAX_GITHUB_ARCHIVE_TOTAL_BYTES {
            return Err(format!(
                "archive content exceeds the {MAX_GITHUB_ARCHIVE_TOTAL_BYTES}-byte limit"
            ));
        }
        files.push((path, buf));
    }

    if files.is_empty() {
        return Err("archive contains no files".to_string());
    }

    let strip_prefix = shared_archive_prefix(&files);
    let mut portable_paths = PortablePathSet::default();
    for directory in directories {
        let relative = match &strip_prefix {
            Some(prefix) => directory
                .strip_prefix(prefix)
                .unwrap_or(directory.as_path()),
            None => directory.as_path(),
        };
        if relative.as_os_str().is_empty() {
            continue;
        }
        let portable = vo_module::schema::portable_relative_path_from_path(relative)
            .map_err(|error| format!("invalid archive path '{}': {error}", relative.display()))?;
        if !portable_paths
            .insert_directory(&portable)
            .map_err(|error| format!("invalid archive path '{portable}': {error}"))?
        {
            return Err(format!(
                "archive contains duplicate directory path: {portable}"
            ));
        }
    }
    let mut materialized = Vec::with_capacity(files.len());
    for (path, bytes) in files {
        let relative = match &strip_prefix {
            Some(prefix) => path.strip_prefix(prefix).unwrap_or(path.as_path()),
            None => path.as_path(),
        };
        if relative.as_os_str().is_empty() {
            continue;
        }
        let portable = vo_module::schema::portable_relative_path_from_path(relative)
            .map_err(|error| format!("invalid archive path '{}': {error}", relative.display()))?;
        if !portable_paths
            .insert_file(&portable)
            .map_err(|error| format!("invalid archive path '{portable}': {error}"))?
        {
            return Err(format!("archive contains duplicate path: {portable}"));
        }
        materialized.push((portable, bytes));
    }

    for (portable, bytes) in materialized {
        let target = session_root.join(Path::new(&portable));
        if let Some(parent) = target.parent() {
            fs::create_dir_all(parent).map_err(|err| format!("{}: {}", parent.display(), err))?;
        }
        fs::write(&target, bytes).map_err(|err| format!("{}: {}", target.display(), err))?;
    }

    Ok(())
}

fn sanitize_archive_path(path: &Path) -> Option<PathBuf> {
    let mut clean = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Normal(part) => clean.push(part),
            Component::CurDir => {}
            Component::ParentDir | Component::Prefix(_) | Component::RootDir => return None,
        }
    }
    if clean.as_os_str().is_empty() {
        None
    } else {
        Some(clean)
    }
}

fn shared_archive_prefix<T>(files: &[(PathBuf, T)]) -> Option<PathBuf> {
    let first_component = files.first()?.0.components().next()?;
    let Component::Normal(first_name) = first_component else {
        return None;
    };
    for (path, _) in files {
        let mut components = path.components();
        let Some(Component::Normal(name)) = components.next() else {
            return None;
        };
        if name != first_name || components.next().is_none() {
            return None;
        }
    }
    Some(PathBuf::from(first_name))
}

fn detect_import_entry(session_root: &Path) -> Option<PathBuf> {
    let main = session_root.join("main.vo");
    if main.is_file() {
        return Some(main);
    }
    let mut stack = vec![session_root.to_path_buf()];
    let mut vo_files = Vec::new();
    while let Some(dir) = stack.pop() {
        let entries = fs::read_dir(&dir).ok()?;
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().and_then(|ext| ext.to_str()) == Some("vo") {
                vo_files.push(path);
            }
        }
    }
    vo_files.sort();
    vo_files.into_iter().next()
}

#[cfg(test)]
mod tests {
    use super::{
        extract_tar_gz_project, isolated_file_session_info, read_response_limited,
        resolve_isolated_file_target,
    };
    use crate::state::{ProjectMode, SessionOrigin, WorkspaceDiscoveryMode};
    use flate2::write::GzEncoder;
    use flate2::Compression;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};
    use tar::Builder;

    #[test]
    fn isolated_file_target_ignores_ancestor_module_manifest() {
        let root = temp_test_dir("isolated-file");
        fs::write(root.join("vo.mod"), "malformed ancestor manifest")
            .expect("expected ancestor manifest fixture");
        let example_root = root.join(".volang/apps/studio/sessions/examples/channels");
        fs::create_dir_all(&example_root).expect("expected example directory fixture");
        let entry = example_root.join("channels.vo");
        fs::write(&entry, "package main\n").expect("expected example fixture");

        let target = resolve_isolated_file_target(entry.to_string_lossy().to_string())
            .expect("isolated target resolution must ignore ancestors");
        assert_eq!(target.root, example_root.canonicalize().unwrap());
        assert_eq!(target.entry, entry.canonicalize().unwrap());
        let info = isolated_file_session_info(&target).expect("expected isolated session info");
        assert_eq!(info.root, target.root.to_string_lossy());
        assert_eq!(
            info.entry_path.as_deref(),
            Some(target.entry.to_string_lossy().as_ref())
        );
        assert!(info.origin == SessionOrigin::RunTarget);
        assert!(info.project_mode == ProjectMode::SingleFile);
        assert!(info.single_file_run);
        assert!(info.workspace_discovery == WorkspaceDiscoveryMode::Disabled);

        fs::remove_dir_all(&root).expect("expected cleanup to succeed");
    }

    #[test]
    fn response_reader_enforces_its_exact_byte_limit() {
        let exact = vec![7_u8; 16];
        assert_eq!(
            read_response_limited(exact.as_slice(), exact.len(), "fixture").unwrap(),
            exact,
        );

        let oversized = vec![9_u8; 17];
        let error = read_response_limited(oversized.as_slice(), 16, "fixture")
            .expect_err("the host must reject response bodies past its declared limit");
        assert!(error.contains("16-byte limit"), "{error}");
    }

    #[test]
    fn extract_tar_gz_project_preserves_binary_files() {
        let archive_bytes = build_archive(vec![
            ("bundle/main.vo", b"package main\n".to_vec()),
            ("bundle/assets/logo.bin", vec![0, 159, 146, 150, 255, 1]),
        ]);
        let root = temp_test_dir("extract-binary");
        extract_tar_gz_project(&archive_bytes, &root)
            .expect("expected archive extraction to succeed");
        let text =
            fs::read_to_string(root.join("main.vo")).expect("expected main.vo to be readable");
        assert_eq!(text, "package main\n");
        let bytes = fs::read(root.join("assets/logo.bin")).expect("expected binary file to exist");
        assert_eq!(bytes, vec![0, 159, 146, 150, 255, 1]);
        fs::remove_dir_all(&root).expect("expected cleanup to succeed");
    }

    #[test]
    fn extract_tar_gz_project_rejects_portable_path_collisions() {
        let archive_bytes = build_archive(vec![
            ("bundle/Straße.vo", b"package one\n".to_vec()),
            ("bundle/STRASSE.vo", b"package two\n".to_vec()),
        ]);
        let root = temp_test_dir("extract-portable-collision");
        let error = extract_tar_gz_project(&archive_bytes, &root)
            .expect_err("portable case-fold collision must be rejected");
        assert!(
            error.contains("conflicts with portable spelling"),
            "{error}"
        );
        assert!(fs::read_dir(&root).unwrap().next().is_none());
        fs::remove_dir_all(&root).expect("expected cleanup to succeed");
    }

    #[test]
    fn extract_tar_gz_project_rejects_links() {
        let encoder = GzEncoder::new(Vec::new(), Compression::default());
        let mut builder = Builder::new(encoder);
        let mut header = tar::Header::new_gnu();
        header.set_entry_type(tar::EntryType::Symlink);
        header.set_size(0);
        header.set_mode(0o777);
        header.set_link_name("../outside").unwrap();
        header.set_cksum();
        builder
            .append_data(&mut header, "bundle/linked.vo", &[][..])
            .expect("expected tar symlink append to succeed");
        let archive_bytes = builder
            .into_inner()
            .expect("expected tar finalize to succeed")
            .finish()
            .expect("expected gzip finalize to succeed");
        let root = temp_test_dir("extract-link");

        let error = extract_tar_gz_project(&archive_bytes, &root)
            .expect_err("archive link must be rejected");
        assert!(
            error.contains("unsupported link or special entry"),
            "{error}"
        );
        assert!(fs::read_dir(&root).unwrap().next().is_none());
        fs::remove_dir_all(&root).expect("expected cleanup to succeed");
    }

    fn temp_test_dir(label: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("expected monotonic system time")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("studio-session-{label}-{nonce}"));
        fs::create_dir_all(&dir).expect("expected temp dir creation to succeed");
        dir
    }

    fn build_archive(entries: Vec<(&str, Vec<u8>)>) -> Vec<u8> {
        let encoder = GzEncoder::new(Vec::new(), Compression::default());
        let mut builder = Builder::new(encoder);
        for (path, bytes) in entries {
            let mut header = tar::Header::new_gnu();
            header.set_size(bytes.len() as u64);
            header.set_mode(0o644);
            header.set_cksum();
            builder
                .append_data(&mut header, path, bytes.as_slice())
                .expect("expected tar append to succeed");
        }
        let encoder = builder
            .into_inner()
            .expect("expected tar finalize to succeed");
        encoder.finish().expect("expected gzip finalize to succeed")
    }
}
