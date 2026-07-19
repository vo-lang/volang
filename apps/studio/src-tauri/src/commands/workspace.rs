use super::pathing::{is_link_or_reparse_point, resolve_path};
use super::run_blocking;
use crate::state::{AppState, PreparedSession};
use std::collections::HashSet;
use std::ffi::OsString;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

static CREATE_PROJECT_TEMP_NONCE: AtomicU64 = AtomicU64::new(0);

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FsEntry {
    pub name: String,
    pub path: String,
    pub is_dir: bool,
    pub size: Option<u64>,
    pub modified_ms: Option<u64>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FsStat {
    pub path: String,
    pub is_dir: bool,
    pub is_file: bool,
    pub size: u64,
    pub modified_ms: u64,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ReadManyResult {
    pub path: String,
    pub content: Option<String>,
    pub error: Option<String>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct GrepMatch {
    pub path: String,
    pub line: u32,
    pub column: u32,
    pub text: String,
}

#[tauri::command]
pub async fn cmd_list_dir(
    path: String,
    state: tauri::State<'_, AppState>,
) -> Result<Vec<FsEntry>, String> {
    let session_root = state.session_root();
    run_blocking(move || list_dir_impl(session_root, path)).await
}

#[tauri::command]
pub async fn cmd_list_prepared_session_dir(
    candidate: PreparedSession,
    path: String,
    state: tauri::State<'_, AppState>,
) -> Result<Vec<FsEntry>, String> {
    let root = state
        .pending_session_snapshot(&candidate)?
        .root()
        .to_path_buf();
    run_blocking(move || list_dir_impl(root, path)).await
}

fn list_dir_impl(session_root: PathBuf, path: String) -> Result<Vec<FsEntry>, String> {
    let resolved = resolve_path(&session_root, &path)?;
    if !resolved.is_dir() {
        return Err(format!("Not a directory: {}", resolved.display()));
    }
    let mut entries = Vec::new();
    for entry in
        std::fs::read_dir(&resolved).map_err(|err| format!("{}: {}", resolved.display(), err))?
    {
        let entry = entry.map_err(|err| format!("{}: {}", resolved.display(), err))?;
        let path = entry.path();
        let metadata = std::fs::symlink_metadata(&path)
            .map_err(|err| format!("{}: {}", path.display(), err))?;
        let file_type = metadata.file_type();
        if is_link_or_reparse_point(&metadata) || (!file_type.is_file() && !file_type.is_dir()) {
            continue;
        }
        let modified_ms = metadata
            .modified()
            .ok()
            .and_then(|value| value.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|value| value.as_millis() as u64);
        entries.push(FsEntry {
            name: entry.file_name().to_string_lossy().to_string(),
            path: path.to_string_lossy().to_string(),
            is_dir: metadata.is_dir(),
            size: if metadata.is_file() {
                Some(metadata.len())
            } else {
                None
            },
            modified_ms,
        });
    }
    entries.sort_by(|left, right| match (left.is_dir, right.is_dir) {
        (true, false) => std::cmp::Ordering::Less,
        (false, true) => std::cmp::Ordering::Greater,
        _ => left.name.cmp(&right.name),
    });
    Ok(entries)
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DiscoveredProject {
    pub name: String,
    #[serde(rename = "type")]
    pub project_type: String,
    pub local_path: String,
    pub entry_path: Option<String>,
}

const SKIP_DIRS: &[&str] = &[".volang", ".vo-cache", ".git", "node_modules"];

#[tauri::command]
pub async fn cmd_discover_projects(
    root: String,
    state: tauri::State<'_, AppState>,
) -> Result<Vec<DiscoveredProject>, String> {
    let session_root = state.session_root();
    run_blocking(move || {
        let resolved = resolve_path(&session_root, &root)?;
        scan_projects_in_dir(&resolved)
    })
    .await
}

#[tauri::command]
pub async fn cmd_discover_workspace_projects(
    state: tauri::State<'_, AppState>,
) -> Result<Vec<DiscoveredProject>, String> {
    let workspace_root = state.workspace_root().to_path_buf();
    run_blocking(move || scan_projects_in_dir(&workspace_root)).await
}

fn scan_projects_in_dir(dir: &Path) -> Result<Vec<DiscoveredProject>, String> {
    if !dir.is_dir() {
        return Err(format!("Not a directory: {}", dir.display()));
    }

    let mut projects = Vec::new();
    let entries = std::fs::read_dir(dir).map_err(|err| format!("{}: {}", dir.display(), err))?;

    for entry in entries {
        let entry = match entry {
            Ok(e) => e,
            Err(_) => continue,
        };
        let name_os = entry.file_name();
        let name = name_os.to_string_lossy();
        if name.starts_with('.') {
            continue;
        }
        let path = entry.path();
        let metadata = match std::fs::symlink_metadata(&path) {
            Ok(metadata) if !is_link_or_reparse_point(&metadata) => metadata,
            Err(_) => continue,
            _ => continue,
        };
        let file_type = metadata.file_type();

        if file_type.is_file() && name.ends_with(".vo") {
            let project_name = name.trim_end_matches(".vo").to_string();
            let path_str = path.to_string_lossy().to_string();
            projects.push(DiscoveredProject {
                name: project_name,
                project_type: "single".to_string(),
                local_path: path_str.clone(),
                entry_path: Some(path_str),
            });
        } else if file_type.is_dir() && !SKIP_DIRS.contains(&name.as_ref()) {
            if let Ok(children) = std::fs::read_dir(&path) {
                let child_names: Vec<String> = children
                    .filter_map(|c| c.ok())
                    .filter(|c| c.file_type().map(|kind| kind.is_file()).unwrap_or(false))
                    .map(|c| c.file_name().to_string_lossy().to_string())
                    .collect();

                if child_names.iter().any(|n| n == "vo.mod") {
                    let dir_str = path.to_string_lossy().to_string();
                    let entry_path = if child_names.iter().any(|n| n == "main.vo") {
                        Some(format!("{}/main.vo", dir_str))
                    } else if child_names.iter().any(|n| n == "app.vo") {
                        Some(format!("{}/app.vo", dir_str))
                    } else {
                        Some(format!("{}/vo.mod", dir_str))
                    };
                    projects.push(DiscoveredProject {
                        name: name.to_string(),
                        project_type: "module".to_string(),
                        local_path: dir_str,
                        entry_path,
                    });
                }
            }
        }
    }

    projects.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(projects)
}

#[tauri::command]
pub async fn cmd_stat_path(
    path: String,
    state: tauri::State<'_, AppState>,
) -> Result<FsStat, String> {
    let session_root = state.session_root();
    run_blocking(move || stat_path_impl(session_root, path)).await
}

fn stat_path_impl(session_root: PathBuf, path: String) -> Result<FsStat, String> {
    let resolved = resolve_path(&session_root, &path)?;
    let metadata =
        std::fs::metadata(&resolved).map_err(|err| format!("{}: {}", resolved.display(), err))?;
    let modified_ms = metadata
        .modified()
        .ok()
        .and_then(|value| value.duration_since(std::time::UNIX_EPOCH).ok())
        .map(|value| value.as_millis() as u64)
        .unwrap_or(0);
    Ok(FsStat {
        path: resolved.to_string_lossy().to_string(),
        is_dir: metadata.is_dir(),
        is_file: metadata.is_file(),
        size: if metadata.is_file() {
            metadata.len()
        } else {
            0
        },
        modified_ms,
    })
}

#[tauri::command]
pub async fn cmd_read_file(
    path: String,
    state: tauri::State<'_, AppState>,
) -> Result<String, String> {
    let session_root = state.session_root();
    run_blocking(move || read_file_impl(session_root, path)).await
}

#[tauri::command]
pub async fn cmd_read_prepared_session_file(
    candidate: PreparedSession,
    path: String,
    state: tauri::State<'_, AppState>,
) -> Result<String, String> {
    let root = state
        .pending_session_snapshot(&candidate)?
        .root()
        .to_path_buf();
    run_blocking(move || read_file_impl(root, path)).await
}

fn read_file_impl(session_root: PathBuf, path: String) -> Result<String, String> {
    let resolved = resolve_path(&session_root, &path)?;
    if !resolved.is_file() {
        return Err(format!("Not a file: {}", resolved.display()));
    }
    std::fs::read_to_string(&resolved).map_err(|err| format!("{}: {}", resolved.display(), err))
}

#[tauri::command]
pub async fn cmd_read_many(
    paths: Vec<String>,
    state: tauri::State<'_, AppState>,
) -> Result<Vec<ReadManyResult>, String> {
    let session_root = state.session_root();
    run_blocking(move || read_many_impl(session_root, paths)).await
}

fn read_many_impl(
    session_root: PathBuf,
    paths: Vec<String>,
) -> Result<Vec<ReadManyResult>, String> {
    Ok(paths
        .into_iter()
        .map(|path| {
            let resolved = match resolve_path(&session_root, &path) {
                Ok(r) => r,
                Err(err) => {
                    return ReadManyResult {
                        path,
                        content: None,
                        error: Some(err),
                    }
                }
            };
            match std::fs::read_to_string(&resolved) {
                Ok(content) => ReadManyResult {
                    path: resolved.to_string_lossy().to_string(),
                    content: Some(content),
                    error: None,
                },
                Err(err) => ReadManyResult {
                    path,
                    content: None,
                    error: Some(err.to_string()),
                },
            }
        })
        .collect())
}

#[tauri::command]
pub async fn cmd_write_file(
    path: String,
    content: String,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    let session_root = state.session_root();
    run_blocking(move || write_file_impl(session_root, path, content)).await
}

fn write_file_impl(session_root: PathBuf, path: String, content: String) -> Result<(), String> {
    let resolved = resolve_path(&session_root, &path)?;
    if let Some(parent) = resolved.parent() {
        std::fs::create_dir_all(parent).map_err(|err| format!("{}: {}", parent.display(), err))?;
    }
    std::fs::write(&resolved, content).map_err(|err| format!("{}: {}", resolved.display(), err))
}

#[tauri::command]
pub async fn cmd_mkdir(path: String, state: tauri::State<'_, AppState>) -> Result<(), String> {
    let session_root = state.session_root();
    run_blocking(move || mkdir_impl(session_root, path)).await
}

fn mkdir_impl(session_root: PathBuf, path: String) -> Result<(), String> {
    let resolved = resolve_path(&session_root, &path)?;
    std::fs::create_dir_all(&resolved).map_err(|err| format!("{}: {}", resolved.display(), err))
}

#[tauri::command]
pub async fn cmd_remove_entry(
    path: String,
    recursive: bool,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    let session_root = state.session_root();
    run_blocking(move || remove_entry_impl(session_root, path, recursive)).await
}

fn remove_entry_impl(session_root: PathBuf, path: String, recursive: bool) -> Result<(), String> {
    let resolved = resolve_path(&session_root, &path)?;
    if resolved.is_dir() {
        if recursive {
            std::fs::remove_dir_all(&resolved)
        } else {
            std::fs::remove_dir(&resolved)
        }
        .map_err(|err| format!("{}: {}", resolved.display(), err))
    } else {
        std::fs::remove_file(&resolved).map_err(|err| format!("{}: {}", resolved.display(), err))
    }
}

#[tauri::command]
pub async fn cmd_rename_entry(
    old_path: String,
    new_path: String,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    let session_root = state.session_root();
    run_blocking(move || rename_entry_impl(session_root, old_path, new_path)).await
}

fn rename_entry_impl(
    session_root: PathBuf,
    old_path: String,
    new_path: String,
) -> Result<(), String> {
    let old_resolved = resolve_path(&session_root, &old_path)?;
    let new_resolved = resolve_path(&session_root, &new_path)?;
    std::fs::rename(&old_resolved, &new_resolved).map_err(|err| {
        format!(
            "{} -> {}: {}",
            old_resolved.display(),
            new_resolved.display(),
            err
        )
    })
}

#[tauri::command]
pub async fn cmd_copy_entry(
    src: String,
    dst: String,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    let session_root = state.session_root();
    run_blocking(move || copy_entry_impl(session_root, src, dst)).await
}

fn copy_entry_impl(session_root: PathBuf, src: String, dst: String) -> Result<(), String> {
    let src_resolved = resolve_path(&session_root, &src)?;
    let dst_resolved = resolve_path(&session_root, &dst)?;
    if let Some(parent) = dst_resolved.parent() {
        std::fs::create_dir_all(parent).map_err(|err| format!("{}: {}", parent.display(), err))?;
    }
    std::fs::copy(&src_resolved, &dst_resolved)
        .map(|_| ())
        .map_err(|err| {
            format!(
                "{} -> {}: {}",
                src_resolved.display(),
                dst_resolved.display(),
                err
            )
        })
}

#[tauri::command]
pub async fn cmd_grep(
    path: String,
    pattern: String,
    case_sensitive: Option<bool>,
    max_results: Option<usize>,
    state: tauri::State<'_, AppState>,
) -> Result<Vec<GrepMatch>, String> {
    let session_root = state.session_root();
    let case_sensitive = case_sensitive.unwrap_or(false);
    let max_results = max_results.unwrap_or(500);
    run_blocking(move || grep_impl(session_root, path, pattern, case_sensitive, max_results)).await
}

fn grep_impl(
    session_root: PathBuf,
    path: String,
    pattern: String,
    case_sensitive: bool,
    max_results: usize,
) -> Result<Vec<GrepMatch>, String> {
    let resolved = resolve_path(&session_root, &path)?;
    let mut results = Vec::new();
    grep_dir(
        &resolved,
        &pattern,
        case_sensitive,
        max_results,
        &mut results,
    )?;
    Ok(results)
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CreateFileEntry {
    pub path: String,
    pub content: String,
}

#[tauri::command]
pub async fn cmd_create_workspace_files(
    files: Vec<CreateFileEntry>,
    state: tauri::State<'_, AppState>,
) -> Result<(), String> {
    let workspace_root = state.workspace_root().to_path_buf();
    run_blocking(move || create_workspace_files_impl(&workspace_root, files)).await
}

#[tauri::command]
pub async fn cmd_create_project_files(files: Vec<CreateFileEntry>) -> Result<(), String> {
    run_blocking(move || create_external_project_file_impl(files)).await
}

fn create_workspace_files_impl(
    workspace_root: &Path,
    files: Vec<CreateFileEntry>,
) -> Result<(), String> {
    std::fs::create_dir_all(workspace_root)
        .map_err(|error| format!("{}: {}", workspace_root.display(), error))?;
    create_project_files_in_root(workspace_root, files, true)
}

fn create_external_project_file_impl(mut files: Vec<CreateFileEntry>) -> Result<(), String> {
    if files.len() != 1 {
        return Err(format!(
            "External project creation requires exactly one file, received {}",
            files.len()
        ));
    }
    let file = files.pop().expect("length checked above");
    let path = Path::new(&file.path);
    if !path.is_absolute() {
        return Err(format!("Path must be absolute: {}", file.path));
    }
    if path.extension().and_then(|extension| extension.to_str()) != Some("vo") {
        return Err(format!(
            "External project file must use the .vo extension: {}",
            path.display()
        ));
    }
    let parent = path
        .parent()
        .ok_or_else(|| format!("Project file has no parent: {}", path.display()))?;
    match std::fs::symlink_metadata(parent) {
        Ok(metadata) if is_link_or_reparse_point(&metadata) => {
            return Err(format!(
                "External project directory is a symbolic link: {}",
                parent.display()
            ))
        }
        Ok(metadata) if metadata.file_type().is_dir() => {}
        Ok(_) => {
            return Err(format!(
                "External project directory is not a directory: {}",
                parent.display()
            ))
        }
        Err(error) => return Err(format!("{}: {}", parent.display(), error)),
    }
    let authorized_root = parent.to_path_buf();
    create_project_files_in_root(&authorized_root, vec![file], false)
}

fn create_project_files_in_root(
    authorized_root: &Path,
    files: Vec<CreateFileEntry>,
    create_missing_parents: bool,
) -> Result<(), String> {
    let canonical_root = authorized_root
        .canonicalize()
        .map_err(|error| format!("{}: {}", authorized_root.display(), error))?;
    if !canonical_root.is_dir() {
        return Err(format!(
            "Project file root is not a directory: {}",
            canonical_root.display()
        ));
    }

    // Validate the complete request before creating anything. This prevents an
    // invalid later entry from leaving an earlier project file behind.
    let mut seen = HashSet::with_capacity(files.len());
    let mut prepared = Vec::with_capacity(files.len());
    for CreateFileEntry { path, content } in files {
        if !Path::new(&path).is_absolute() {
            return Err(format!("Path must be absolute: {path}"));
        }
        let resolved = resolve_path(authorized_root, &path)?;
        if resolved == canonical_root {
            return Err(format!(
                "Project file path names the workspace root: {path}"
            ));
        }
        if !seen.insert(resolved.clone()) {
            return Err(format!(
                "Duplicate project file path: {}",
                resolved.display()
            ));
        }
        prepared.push((resolved, content));
    }

    for (path, content) in prepared {
        write_project_file(
            &canonical_root,
            &path,
            content.as_bytes(),
            create_missing_parents,
        )?;
    }
    Ok(())
}

fn write_project_file(
    authorized_root: &Path,
    path: &Path,
    content: &[u8],
    create_missing_parents: bool,
) -> Result<(), String> {
    let parent = path
        .parent()
        .ok_or_else(|| format!("Project file has no parent: {}", path.display()))?;
    if create_missing_parents {
        std::fs::create_dir_all(parent)
            .map_err(|error| format!("{}: {}", parent.display(), error))?;
    }
    verify_project_file_path(authorized_root, path)?;

    let (temporary_path, mut temporary_file) = create_project_temp_file(parent)?;
    let result = (|| {
        temporary_file
            .write_all(content)
            .map_err(|error| format!("{}: {}", temporary_path.display(), error))?;
        temporary_file
            .flush()
            .map_err(|error| format!("{}: {}", temporary_path.display(), error))?;
        drop(temporary_file);

        // Re-check after directory creation and immediately before commit. A
        // symbolic-link target is never opened, and rename replaces its
        // directory entry rather than writing through it.
        verify_project_file_path(authorized_root, path)?;
        if let Some(permissions) = existing_regular_file_permissions(path)? {
            std::fs::set_permissions(&temporary_path, permissions)
                .map_err(|error| format!("{}: {}", temporary_path.display(), error))?;
        }
        replace_project_file(&temporary_path, path)
    })();

    if result.is_err() {
        let _ = remove_project_artifact(&temporary_path);
    }
    result
}

fn verify_project_file_path(authorized_root: &Path, path: &Path) -> Result<(), String> {
    let parent = path
        .parent()
        .ok_or_else(|| format!("Project file has no parent: {}", path.display()))?;
    let canonical_parent = parent
        .canonicalize()
        .map_err(|error| format!("{}: {}", parent.display(), error))?;
    if !canonical_parent.starts_with(authorized_root) {
        return Err(format!(
            "Project file parent escapes its authorized root: {}",
            parent.display()
        ));
    }

    let original = path.to_string_lossy();
    let resolved = resolve_path(authorized_root, &original)?;
    if resolved != path {
        return Err(format!(
            "Project file path escapes its authorized root: {}",
            path.display()
        ));
    }
    existing_regular_file_permissions(path).map(|_| ())
}

fn existing_regular_file_permissions(path: &Path) -> Result<Option<std::fs::Permissions>, String> {
    match std::fs::symlink_metadata(path) {
        Ok(metadata) if is_link_or_reparse_point(&metadata) => Err(format!(
            "Project file target is a symbolic link or reparse point: {}",
            path.display()
        )),
        Ok(metadata) if metadata.file_type().is_file() => Ok(Some(metadata.permissions())),
        Ok(_) => Err(format!(
            "Project file target is not a regular file: {}",
            path.display()
        )),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(error) => Err(format!("{}: {}", path.display(), error)),
    }
}

fn create_project_temp_file(parent: &Path) -> Result<(PathBuf, std::fs::File), String> {
    for _ in 0..128 {
        let nonce = CREATE_PROJECT_TEMP_NONCE.fetch_add(1, Ordering::Relaxed);
        let mut name = OsString::from(".vo-studio-create-");
        name.push(std::process::id().to_string());
        name.push("-");
        name.push(nonce.to_string());
        name.push(".tmp");
        let path = parent.join(name);
        match std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&path)
        {
            Ok(file) => return Ok((path, file)),
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(error) => return Err(format!("{}: {}", path.display(), error)),
        }
    }
    Err(format!(
        "Could not allocate a temporary project file in {}",
        parent.display()
    ))
}

#[cfg(not(windows))]
fn replace_project_file(temporary_path: &Path, path: &Path) -> Result<(), String> {
    std::fs::rename(temporary_path, path).map_err(|error| {
        format!(
            "{} -> {}: {}",
            temporary_path.display(),
            path.display(),
            error
        )
    })
}

#[cfg(windows)]
fn replace_project_file(temporary_path: &Path, path: &Path) -> Result<(), String> {
    replace_project_file_via_backup(temporary_path, path)
}

#[cfg(any(windows, test))]
fn replace_project_file_via_backup(temporary_path: &Path, path: &Path) -> Result<(), String> {
    let original_permissions = match std::fs::symlink_metadata(path) {
        Ok(metadata) => Some(metadata.permissions()),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
        Err(error) => return Err(format!("{}: {}", path.display(), error)),
    };
    let Some(original_permissions) = original_permissions else {
        return std::fs::rename(temporary_path, path).map_err(|error| {
            format!(
                "{} -> {}: {}",
                temporary_path.display(),
                path.display(),
                error
            )
        });
    };

    let backup_path = move_project_file_to_backup(path)?;
    if let Err(prepare_error) = make_project_file_removable(&backup_path) {
        return match restore_project_backup(&backup_path, path, &original_permissions) {
            Ok(()) => Err(prepare_error),
            Err(restore_error) => Err(format!(
                "{prepare_error}; restoring {} failed: {restore_error}",
                backup_path.display()
            )),
        };
    }
    match std::fs::rename(temporary_path, path) {
        Ok(()) => match std::fs::remove_file(&backup_path) {
            Ok(()) => Ok(()),
            Err(cleanup_error) => {
                let cleanup_message = format!("{}: {}", backup_path.display(), cleanup_error);
                match rollback_published_project(
                    temporary_path,
                    path,
                    &backup_path,
                    &original_permissions,
                ) {
                    Ok(()) => Err(cleanup_message),
                    Err(rollback_error) => Err(format!(
                        "{cleanup_message}; rolling back published project file failed: {rollback_error}"
                    )),
                }
            }
        },
        Err(publish_error) => {
            match restore_project_backup(&backup_path, path, &original_permissions) {
                Ok(()) => Err(format!(
                    "{} -> {}: {}",
                    temporary_path.display(),
                    path.display(),
                    publish_error
                )),
                Err(restore_error) => Err(format!(
                    "{} -> {}: {}; restoring {} failed: {}",
                    temporary_path.display(),
                    path.display(),
                    publish_error,
                    backup_path.display(),
                    restore_error
                )),
            }
        }
    }
}

#[cfg(any(windows, test))]
fn restore_project_backup(
    backup_path: &Path,
    path: &Path,
    permissions: &std::fs::Permissions,
) -> Result<(), String> {
    std::fs::rename(backup_path, path)
        .map_err(|error| format!("{} -> {}: {}", backup_path.display(), path.display(), error))?;
    std::fs::set_permissions(path, permissions.clone())
        .map_err(|error| format!("{}: {}", path.display(), error))
}

#[cfg(any(windows, test))]
fn rollback_published_project(
    temporary_path: &Path,
    path: &Path,
    backup_path: &Path,
    permissions: &std::fs::Permissions,
) -> Result<(), String> {
    make_project_file_removable(path)?;
    std::fs::rename(path, temporary_path).map_err(|error| {
        format!(
            "{} -> {}: {}",
            path.display(),
            temporary_path.display(),
            error
        )
    })?;
    if let Err(restore_error) = restore_project_backup(backup_path, path, permissions) {
        if backup_path.exists() {
            let _ = std::fs::rename(temporary_path, path);
        }
        return Err(restore_error);
    }
    remove_project_artifact(temporary_path)
}

fn remove_project_artifact(path: &Path) -> Result<(), String> {
    match std::fs::symlink_metadata(path) {
        Ok(_) => {
            make_project_file_removable(path)?;
            std::fs::remove_file(path).map_err(|error| format!("{}: {}", path.display(), error))
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(format!("{}: {}", path.display(), error)),
    }
}

#[cfg(windows)]
fn make_project_file_removable(path: &Path) -> Result<(), String> {
    let mut permissions = std::fs::metadata(path)
        .map_err(|error| format!("{}: {}", path.display(), error))?
        .permissions();
    if permissions.readonly() {
        // On Windows this clears FILE_ATTRIBUTE_READONLY. The Unix permission-bit
        // alternative suggested by this lint does not apply to this cfg-only path.
        #[allow(clippy::permissions_set_readonly_false)]
        permissions.set_readonly(false);
        std::fs::set_permissions(path, permissions)
            .map_err(|error| format!("{}: {}", path.display(), error))?;
    }
    Ok(())
}

#[cfg(not(windows))]
fn make_project_file_removable(_path: &Path) -> Result<(), String> {
    Ok(())
}

#[cfg(any(windows, test))]
fn move_project_file_to_backup(path: &Path) -> Result<PathBuf, String> {
    let parent = path
        .parent()
        .ok_or_else(|| format!("Project file has no parent: {}", path.display()))?;
    for _ in 0..128 {
        let nonce = CREATE_PROJECT_TEMP_NONCE.fetch_add(1, Ordering::Relaxed);
        let backup_path = parent.join(format!(
            ".vo-studio-replaced-{}-{nonce}.tmp",
            std::process::id()
        ));
        match std::fs::symlink_metadata(&backup_path) {
            Ok(_) => continue,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => return Err(format!("{}: {}", backup_path.display(), error)),
        }
        match std::fs::rename(path, &backup_path) {
            Ok(()) => return Ok(backup_path),
            Err(error) => match std::fs::symlink_metadata(&backup_path) {
                Ok(_) => continue,
                Err(inspect_error) if inspect_error.kind() == std::io::ErrorKind::NotFound => {
                    return Err(format!(
                        "{} -> {}: {}",
                        path.display(),
                        backup_path.display(),
                        error
                    ))
                }
                Err(inspect_error) => {
                    return Err(format!("{}: {}", backup_path.display(), inspect_error))
                }
            },
        }
    }
    Err(format!(
        "Could not allocate a project replacement backup in {}",
        parent.display()
    ))
}

fn grep_dir(
    path: &Path,
    pattern: &str,
    case_sensitive: bool,
    max_results: usize,
    results: &mut Vec<GrepMatch>,
) -> Result<(), String> {
    if results.len() >= max_results {
        return Ok(());
    }
    let metadata = fs_metadata_no_follow(path)?;
    if metadata.file_type().is_file() {
        grep_file(path, pattern, case_sensitive, max_results, results)?;
    } else if metadata.file_type().is_dir() {
        let mut entries: Vec<_> = std::fs::read_dir(path)
            .map_err(|err| format!("{}: {}", path.display(), err))?
            .filter_map(|e| e.ok())
            .collect();
        entries.sort_by_key(|e| e.file_name());
        for entry in entries {
            if results.len() >= max_results {
                break;
            }
            let entry_path = entry.path();
            let metadata = std::fs::symlink_metadata(&entry_path)
                .map_err(|error| format!("{}: {}", entry_path.display(), error))?;
            let file_type = metadata.file_type();
            if is_link_or_reparse_point(&metadata) || (!file_type.is_file() && !file_type.is_dir())
            {
                continue;
            }
            let name = entry.file_name();
            let name_str = name.to_string_lossy();
            if name_str.starts_with('.') {
                continue;
            }
            grep_dir(&entry_path, pattern, case_sensitive, max_results, results)?;
        }
    }
    Ok(())
}

fn fs_metadata_no_follow(path: &Path) -> Result<std::fs::Metadata, String> {
    let metadata = std::fs::symlink_metadata(path)
        .map_err(|error| format!("{}: {}", path.display(), error))?;
    if is_link_or_reparse_point(&metadata) {
        return Err(format!(
            "Path contains unsupported symbolic link or reparse point: {}",
            path.display(),
        ));
    }
    Ok(metadata)
}

fn grep_file(
    path: &Path,
    pattern: &str,
    case_sensitive: bool,
    max_results: usize,
    results: &mut Vec<GrepMatch>,
) -> Result<(), String> {
    let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");
    if !matches!(
        ext,
        "vo" | "md" | "txt" | "toml" | "json" | "ts" | "js" | "rs" | "html" | "css" | "svelte"
    ) {
        return Ok(());
    }
    let content = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return Ok(()),
    };
    let pattern_cmp = if case_sensitive {
        pattern.to_string()
    } else {
        pattern.to_lowercase()
    };
    for (line_idx, line) in content.lines().enumerate() {
        if results.len() >= max_results {
            break;
        }
        let line_cmp = if case_sensitive {
            line.to_string()
        } else {
            line.to_lowercase()
        };
        if let Some(col) = line_cmp.find(&pattern_cmp) {
            results.push(GrepMatch {
                path: path.to_string_lossy().to_string(),
                line: (line_idx + 1) as u32,
                column: col as u32,
                text: line.to_string(),
            });
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{
        create_external_project_file_impl, create_workspace_files_impl,
        replace_project_file_via_backup, CreateFileEntry,
    };
    #[cfg(unix)]
    use super::{grep_dir, list_dir_impl};
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn create_project_files_stays_within_workspace_and_preserves_legal_overwrite() {
        let root = temp_dir("create-project");
        let entry = root.join("examples/channels/main.vo");

        create_workspace_files_impl(
            &root,
            vec![create_file(&entry, "package main\nfunc main() {}\n")],
        )
        .unwrap();
        assert_eq!(
            fs::read_to_string(&entry).unwrap(),
            "package main\nfunc main() {}\n"
        );

        create_workspace_files_impl(
            &root,
            vec![create_file(
                &entry,
                "package main\nfunc main() { println(\"updated\") }\n",
            )],
        )
        .unwrap();
        assert_eq!(
            fs::read_to_string(&entry).unwrap(),
            "package main\nfunc main() { println(\"updated\") }\n"
        );

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn create_project_files_creates_the_bootstrap_workspace_root_on_first_use() {
        let container = temp_dir("create-project-container");
        let root = container.join("workspace");
        let entry = root.join("first/main.vo");
        assert!(!root.exists());

        create_workspace_files_impl(&root, vec![create_file(&entry, "package main\n")]).unwrap();
        assert!(root.is_dir());
        assert_eq!(fs::read_to_string(entry).unwrap(), "package main\n");

        let _ = fs::remove_dir_all(container);
    }

    #[test]
    fn windows_replacement_strategy_overwrites_an_existing_regular_file() {
        let root = temp_dir("create-project-windows-replace");
        let target = root.join("main.vo");
        let temporary = root.join("new.tmp");
        fs::write(&target, "old\n").unwrap();
        fs::write(&temporary, "new\n").unwrap();

        replace_project_file_via_backup(&temporary, &target).unwrap();
        assert_eq!(fs::read_to_string(&target).unwrap(), "new\n");
        assert!(!temporary.exists());
        assert_eq!(fs::read_dir(&root).unwrap().count(), 1);

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn replacement_strategy_restores_the_original_after_publish_failure() {
        let root = temp_dir("create-project-replace-rollback");
        let target = root.join("main.vo");
        let missing_temporary = root.join("missing.tmp");
        fs::write(&target, "old\n").unwrap();
        let original_permissions = fs::metadata(&target).unwrap().permissions();

        let error = replace_project_file_via_backup(&missing_temporary, &target).unwrap_err();
        assert!(error.contains("missing.tmp"), "{error}");
        assert_eq!(fs::read_to_string(&target).unwrap(), "old\n");
        assert_eq!(
            fs::metadata(&target).unwrap().permissions().readonly(),
            original_permissions.readonly()
        );
        assert_eq!(fs::read_dir(&root).unwrap().count(), 1);

        let _ = fs::remove_dir_all(root);
    }

    #[cfg(windows)]
    #[test]
    fn windows_replacement_preserves_readonly_files_without_backup_leaks() {
        let root = temp_dir("create-project-windows-readonly");
        let target = root.join("main.vo");
        let temporary = root.join("new.tmp");
        fs::write(&target, "old\n").unwrap();
        fs::write(&temporary, "new\n").unwrap();
        let mut permissions = fs::metadata(&target).unwrap().permissions();
        permissions.set_readonly(true);
        fs::set_permissions(&target, permissions.clone()).unwrap();
        fs::set_permissions(&temporary, permissions).unwrap();

        replace_project_file_via_backup(&temporary, &target).unwrap();
        assert_eq!(fs::read_to_string(&target).unwrap(), "new\n");
        assert!(fs::metadata(&target).unwrap().permissions().readonly());
        assert!(!temporary.exists());
        assert_eq!(fs::read_dir(&root).unwrap().count(), 1);

        super::make_project_file_removable(&target).unwrap();
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn create_project_files_rejects_escape_before_writing_any_entry() {
        let root = temp_dir("create-project-root");
        let outside = temp_dir("create-project-outside");
        let legal = root.join("legal/main.vo");
        let victim = outside.join("victim.vo");
        fs::write(&victim, "keep\n").unwrap();

        let error = create_workspace_files_impl(
            &root,
            vec![
                create_file(&legal, "created\n"),
                create_file(&victim, "overwritten\n"),
            ],
        )
        .unwrap_err();
        assert!(error.contains("escapes session root"), "{error}");
        assert!(!legal.exists());
        assert_eq!(fs::read_to_string(&victim).unwrap(), "keep\n");

        let traversal = root
            .join("nested/../../")
            .join(outside.file_name().expect("temporary directory has a name"));
        let traversal_target = traversal.join("victim.vo");
        let error = create_workspace_files_impl(
            &root,
            vec![create_file(&traversal_target, "overwritten\n")],
        )
        .unwrap_err();
        assert!(error.contains("escapes session root"), "{error}");
        assert_eq!(fs::read_to_string(&victim).unwrap(), "keep\n");

        let _ = fs::remove_dir_all(root);
        let _ = fs::remove_dir_all(outside);
    }

    #[cfg(unix)]
    #[test]
    fn create_project_files_rejects_parent_and_target_symbolic_links() {
        use std::os::unix::fs::symlink;

        let root = temp_dir("create-project-links");
        let outside = temp_dir("create-project-link-target");
        let victim = outside.join("victim.vo");
        fs::write(&victim, "keep\n").unwrap();

        symlink(&outside, root.join("linked-parent")).unwrap();
        let error = create_workspace_files_impl(
            &root,
            vec![create_file(
                &root.join("linked-parent/victim.vo"),
                "overwritten\n",
            )],
        )
        .unwrap_err();
        assert!(error.contains("symbolic link"), "{error}");
        assert_eq!(fs::read_to_string(&victim).unwrap(), "keep\n");

        let project = root.join("project");
        fs::create_dir_all(&project).unwrap();
        symlink(&victim, project.join("main.vo")).unwrap();
        let error = create_workspace_files_impl(
            &root,
            vec![create_file(&project.join("main.vo"), "overwritten\n")],
        )
        .unwrap_err();
        assert!(error.contains("symbolic link"), "{error}");
        assert_eq!(fs::read_to_string(&victim).unwrap(), "keep\n");

        let _ = fs::remove_dir_all(root);
        let _ = fs::remove_dir_all(outside);
    }

    #[test]
    fn create_project_files_replaces_hard_links_without_touching_outside_inode() {
        let root = temp_dir("create-project-hard-link");
        let outside = temp_dir("create-project-hard-link-target");
        let victim = outside.join("victim.vo");
        let entry = root.join("project/main.vo");
        fs::create_dir_all(entry.parent().unwrap()).unwrap();
        fs::write(&victim, "keep\n").unwrap();
        fs::hard_link(&victim, &entry).unwrap();

        create_workspace_files_impl(&root, vec![create_file(&entry, "workspace\n")]).unwrap();
        assert_eq!(fs::read_to_string(&entry).unwrap(), "workspace\n");
        assert_eq!(fs::read_to_string(&victim).unwrap(), "keep\n");

        let _ = fs::remove_dir_all(root);
        let _ = fs::remove_dir_all(outside);
    }

    #[test]
    fn external_project_creation_is_limited_to_one_vo_file_in_an_existing_directory() {
        let root = temp_dir("create-external-project");
        let entry = root.join("main.vo");

        create_external_project_file_impl(vec![create_file(&entry, "package main\n")]).unwrap();
        assert_eq!(fs::read_to_string(&entry).unwrap(), "package main\n");

        let error = create_external_project_file_impl(Vec::new()).unwrap_err();
        assert!(error.contains("exactly one file"), "{error}");
        let error = create_external_project_file_impl(vec![
            create_file(&root.join("one.vo"), "one\n"),
            create_file(&root.join("two.vo"), "two\n"),
        ])
        .unwrap_err();
        assert!(error.contains("exactly one file"), "{error}");
        assert!(!root.join("one.vo").exists());
        assert!(!root.join("two.vo").exists());

        let missing = root.join("missing/project.vo");
        let error = create_external_project_file_impl(vec![create_file(&missing, "missing\n")])
            .unwrap_err();
        assert!(error.contains("missing"), "{error}");
        assert!(!missing.exists());

        let invalid_extension = root.join("project.txt");
        let error =
            create_external_project_file_impl(vec![create_file(&invalid_extension, "invalid\n")])
                .unwrap_err();
        assert!(error.contains(".vo extension"), "{error}");
        assert!(!invalid_extension.exists());

        let _ = fs::remove_dir_all(root);
    }

    #[cfg(unix)]
    #[test]
    fn external_project_creation_rejects_symbolic_link_directory_and_target() {
        use std::os::unix::fs::symlink;

        let container = temp_dir("create-external-links");
        let outside = temp_dir("create-external-link-target");
        let victim = outside.join("victim.vo");
        fs::write(&victim, "keep\n").unwrap();

        let linked_directory = container.join("linked-directory");
        symlink(&outside, &linked_directory).unwrap();
        let error = create_external_project_file_impl(vec![create_file(
            &linked_directory.join("created.vo"),
            "created\n",
        )])
        .unwrap_err();
        assert!(error.contains("symbolic link"), "{error}");
        assert!(!outside.join("created.vo").exists());

        let linked_target = container.join("linked-target.vo");
        symlink(&victim, &linked_target).unwrap();
        let error =
            create_external_project_file_impl(vec![create_file(&linked_target, "overwritten\n")])
                .unwrap_err();
        assert!(error.contains("symbolic link"), "{error}");
        assert_eq!(fs::read_to_string(&victim).unwrap(), "keep\n");

        let _ = fs::remove_dir_all(container);
        let _ = fs::remove_dir_all(outside);
    }

    #[test]
    fn external_project_creation_replaces_hard_link_without_writing_through_it() {
        let root = temp_dir("create-external-hard-link");
        let outside = temp_dir("create-external-hard-link-target");
        let victim = outside.join("victim.vo");
        let entry = root.join("main.vo");
        fs::write(&victim, "keep\n").unwrap();
        fs::hard_link(&victim, &entry).unwrap();

        create_external_project_file_impl(vec![create_file(&entry, "project\n")]).unwrap();
        assert_eq!(fs::read_to_string(&entry).unwrap(), "project\n");
        assert_eq!(fs::read_to_string(&victim).unwrap(), "keep\n");

        let _ = fs::remove_dir_all(root);
        let _ = fs::remove_dir_all(outside);
    }

    #[cfg(unix)]
    #[test]
    fn workspace_listing_and_grep_do_not_follow_nested_symbolic_links() {
        use std::os::unix::fs::symlink;

        let root = temp_dir("nofollow");
        let outside = temp_dir("outside");
        fs::write(root.join("main.vo"), "package main\nneedle\n").unwrap();
        fs::write(outside.join("secret.vo"), "package secret\nneedle\n").unwrap();
        symlink(&outside, root.join("linked")).unwrap();

        let listed = list_dir_impl(root.clone(), ".".to_string()).unwrap();
        assert_eq!(listed.len(), 1);
        assert_eq!(listed[0].name, "main.vo");

        let mut matches = Vec::new();
        grep_dir(&root, "needle", true, 10, &mut matches).unwrap();
        assert_eq!(matches.len(), 1);
        assert!(matches[0].path.ends_with("main.vo"));

        let _ = fs::remove_dir_all(root);
        let _ = fs::remove_dir_all(outside);
    }

    fn create_file(path: &Path, content: &str) -> CreateFileEntry {
        CreateFileEntry {
            path: path.to_string_lossy().into_owned(),
            content: content.to_string(),
        }
    }

    fn temp_dir(label: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root = std::env::temp_dir().join(format!("studio-workspace-{label}-{nonce}"));
        fs::create_dir_all(&root).unwrap();
        root
    }
}
