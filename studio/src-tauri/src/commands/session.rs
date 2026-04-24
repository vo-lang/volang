use std::fs;
use std::io::Read;
use std::path::{Component, Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use flate2::read::GzDecoder;
use serde::de::DeserializeOwned;
use tar::Archive;
use url::Url;

use super::pathing::{find_project_root, is_module_root};
use crate::state::{
    session_info, AppState, LaunchSpec, SessionInfo, SessionOrigin, SessionSource,
    WorkspaceDiscoveryMode,
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

#[tauri::command]
pub fn cmd_open_session(
    spec: LaunchSpec,
    state: tauri::State<'_, AppState>,
) -> Result<SessionInfo, String> {
    open_project_impl(spec.proj.as_deref(), state.inner())
}

fn open_project_impl(proj: Option<&str>, state: &AppState) -> Result<SessionInfo, String> {
    let Some(proj) = proj.map(str::trim).filter(|value| !value.is_empty()) else {
        return open_workspace_session_impl(state);
    };
    if is_local_project_path(proj) {
        return open_run_session_impl(strip_file_prefix(proj), state);
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

fn open_run_session_impl(path: String, state: &AppState) -> Result<SessionInfo, String> {
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
        find_project_root(&canonical)
    } else if is_module_root(&canonical) {
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

    state.set_session(
        session_root.clone(),
        is_single_file,
        WorkspaceDiscoveryMode::Auto,
    );

    let entry = if canonical.is_file() {
        Some(canonical.as_path())
    } else {
        None
    };
    let source_path = canonical.to_string_lossy().to_string();
    Ok(session_info(
        &session_root,
        SessionOrigin::RunTarget,
        entry,
        is_single_file,
        WorkspaceDiscoveryMode::Auto,
        Some(SessionSource::Path { path: source_path }),
    ))
}

fn open_workspace_session_impl(state: &AppState) -> Result<SessionInfo, String> {
    let root = state.workspace_root().to_path_buf();
    std::fs::create_dir_all(&root).map_err(|err| format!("{}: {}", root.display(), err))?;
    state.set_session(root.clone(), false, WorkspaceDiscoveryMode::Auto);
    Ok(session_info(
        &root,
        SessionOrigin::Workspace,
        None,
        false,
        WorkspaceDiscoveryMode::Auto,
        Some(SessionSource::Workspace),
    ))
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
) -> Result<SessionInfo, String> {
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
    state.set_session(
        resolved.project_root.clone(),
        false,
        WorkspaceDiscoveryMode::Disabled,
    );
    Ok(session_info(
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
    ))
}

fn fetch_bytes_blocking(url: &str) -> Result<Vec<u8>, String> {
    fetch_bytes_with_headers(url, &[])
}

fn fetch_bytes_with_headers(url: &str, headers: &[(&str, &str)]) -> Result<Vec<u8>, String> {
    let mut request = ureq::get(url).set("User-Agent", "Vo-Studio");
    for (name, value) in headers {
        request = request.set(name, value);
    }
    let response = request
        .call()
        .map_err(|e| format!("HTTP fetch failed for {}: {}", url, e))?;
    let mut bytes = Vec::new();
    response
        .into_reader()
        .read_to_end(&mut bytes)
        .map_err(|e| format!("failed to read response body from {}: {}", url, e))?;
    Ok(bytes)
}

fn fetch_json_blocking<T: DeserializeOwned>(url: &str) -> Result<T, String> {
    let bytes = fetch_bytes_with_headers(url, &[("Accept", "application/vnd.github+json")])?;
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
        .join(".studio-sources")
        .join("github")
        .join(owner)
        .join(repo)
        .join(&resolved_commit);
    let session_root = workspace_root
        .join(".studio-sessions")
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
    if resolved.source_cache_root.exists() {
        fs::remove_dir_all(&resolved.source_cache_root)
            .map_err(|err| format!("{}: {}", resolved.source_cache_root.display(), err))?;
    }
    fs::create_dir_all(&resolved.source_cache_root)
        .map_err(|err| format!("{}: {}", resolved.source_cache_root.display(), err))?;
    let archive_url = format!(
        "https://api.github.com/repos/{}/{}/tarball/{}",
        resolved.owner, resolved.repo, resolved.resolved_commit,
    );
    let archive_bytes = fetch_bytes_blocking(&archive_url)?;
    extract_tar_gz_project(&archive_bytes, &resolved.source_cache_root)
}

fn materialize_github_session(resolved: &ResolvedGitHubSource) -> Result<(), String> {
    if resolved.session_root.exists() {
        fs::remove_dir_all(&resolved.session_root)
            .map_err(|err| format!("{}: {}", resolved.session_root.display(), err))?;
    }
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
    fs::create_dir_all(target_root).map_err(|err| format!("{}: {}", target_root.display(), err))?;
    for entry in
        fs::read_dir(source_root).map_err(|err| format!("{}: {}", source_root.display(), err))?
    {
        let entry = entry.map_err(|err| format!("{}: {}", source_root.display(), err))?;
        let source_path = entry.path();
        let target_path = target_root.join(entry.file_name());
        let file_type = entry
            .file_type()
            .map_err(|err| format!("{}: {}", source_path.display(), err))?;
        if file_type.is_dir() {
            copy_dir_recursive(&source_path, &target_path)?;
        } else if file_type.is_file() {
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
        }
    }
    Ok(())
}

fn path_has_tree(path: &Path) -> Result<bool, String> {
    if path.is_file() {
        return Ok(true);
    }
    if !path.is_dir() {
        return Ok(false);
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

    for entry in archive.entries().map_err(|err| err.to_string())? {
        let mut entry = entry.map_err(|err| err.to_string())?;
        if !entry.header().entry_type().is_file() {
            continue;
        }
        let raw_path = entry.path().map_err(|err| err.to_string())?.into_owned();
        let Some(path) = sanitize_archive_path(&raw_path) else {
            continue;
        };
        let mut buf = Vec::new();
        entry.read_to_end(&mut buf).map_err(|err| err.to_string())?;
        files.push((path, buf));
    }

    if files.is_empty() {
        return Err("archive contains no files".to_string());
    }

    let strip_prefix = shared_archive_prefix(&files);
    for (path, bytes) in files {
        let relative = match &strip_prefix {
            Some(prefix) => path.strip_prefix(prefix).unwrap_or(path.as_path()),
            None => path.as_path(),
        };
        if relative.as_os_str().is_empty() {
            continue;
        }
        let target = session_root.join(relative);
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
    use super::extract_tar_gz_project;
    use flate2::write::GzEncoder;
    use flate2::Compression;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};
    use tar::Builder;

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
