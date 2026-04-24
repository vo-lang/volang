use super::pathing::resolve_path;
use super::run_blocking;
use crate::state::AppState;
use std::path::{Path, PathBuf};

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
        let metadata = entry
            .metadata()
            .map_err(|err| format!("{}: {}", path.display(), err))?;
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

const SKIP_DIRS: &[&str] = &[".vo-cache", ".git", "node_modules"];

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
        let meta = match entry.metadata() {
            Ok(m) => m,
            Err(_) => continue,
        };

        if meta.is_file() && name.ends_with(".vo") {
            let project_name = name.trim_end_matches(".vo").to_string();
            let path_str = path.to_string_lossy().to_string();
            projects.push(DiscoveredProject {
                name: project_name,
                project_type: "single".to_string(),
                local_path: path_str.clone(),
                entry_path: Some(path_str),
            });
        } else if meta.is_dir() && !SKIP_DIRS.contains(&name.as_ref()) {
            if let Ok(children) = std::fs::read_dir(&path) {
                let child_names: Vec<String> = children
                    .filter_map(|c| c.ok())
                    .filter(|c| c.metadata().map(|m| m.is_file()).unwrap_or(false))
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
pub async fn cmd_create_project_files(files: Vec<CreateFileEntry>) -> Result<(), String> {
    run_blocking(move || {
        for file in &files {
            let path = PathBuf::from(&file.path);
            if !path.is_absolute() {
                return Err(format!("Path must be absolute: {}", file.path));
            }
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)
                    .map_err(|err| format!("{}: {}", parent.display(), err))?;
            }
            std::fs::write(&path, &file.content)
                .map_err(|err| format!("{}: {}", path.display(), err))?;
        }
        Ok(())
    })
    .await
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
    if path.is_file() {
        grep_file(path, pattern, case_sensitive, max_results, results)?;
    } else if path.is_dir() {
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
