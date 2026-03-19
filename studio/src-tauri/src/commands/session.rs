use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::io::Read;
use std::path::{Component, Path, PathBuf};

use flate2::read::GzDecoder;
use tar::Archive;

use crate::state::{session_info, AppState, SessionInfo, SessionOrigin};
use super::pathing::find_project_root;

#[tauri::command]
pub fn cmd_open_workspace_session(state: tauri::State<'_, AppState>) -> Result<SessionInfo, String> {
    let root = state.workspace_root().to_path_buf();
    std::fs::create_dir_all(&root).map_err(|err| format!("{}: {}", root.display(), err))?;
    state.set_session(root.clone(), false);
    Ok(session_info(&root, SessionOrigin::Workspace, None, false))
}

/// Runner-mode session: accepts any local path (file or directory).
///
/// - If the target is a directory, it becomes the session root directly.
/// - If the target is a .vo file inside a module project (vo.mod found in an
///   ancestor), the module root becomes the session root and the file is the
///   entry point.  `single_file_run` stays false so the whole module compiles.
/// - If the target is a standalone .vo file (no vo.mod), `single_file_run` is
///   set to true so the compiler only compiles that one file.
#[tauri::command]
pub fn cmd_open_run_session(path: String, state: tauri::State<'_, AppState>) -> Result<SessionInfo, String> {
    let target = PathBuf::from(&path);
    if !target.exists() {
        return Err(format!("Path not found: {}", target.display()));
    }
    let canonical = target
        .canonicalize()
        .map_err(|err| format!("{}: {}", target.display(), err))?;

    let is_vo_file = canonical.is_file()
        && canonical.extension().and_then(|e| e.to_str()) == Some("vo");

    let module_root = if is_vo_file {
        find_project_root(&canonical)
    } else if canonical.is_dir() && canonical.join("vo.mod").is_file() {
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

    state.set_session(session_root.clone(), is_single_file);

    let entry = if canonical.is_file() { Some(canonical.as_path()) } else { None };
    Ok(session_info(&session_root, SessionOrigin::RunTarget, entry, is_single_file))
}

#[tauri::command]
pub fn cmd_open_url_session(url: String, state: tauri::State<'_, AppState>) -> Result<SessionInfo, String> {
    let session_root = import_url_project(&url, state.workspace_root())?;
    let entry_path = detect_import_entry(&session_root);
    state.set_session(session_root.clone(), false);
    Ok(session_info(&session_root, SessionOrigin::Url, entry_path.as_deref(), false))
}

fn import_url_project(url: &str, workspace_root: &Path) -> Result<PathBuf, String> {
    let bytes = fetch_bytes_blocking(url)?;
    let session_root = build_url_session_root(workspace_root, url);
    if session_root.exists() {
        fs::remove_dir_all(&session_root).map_err(|err| format!("{}: {}", session_root.display(), err))?;
    }
    fs::create_dir_all(&session_root).map_err(|err| format!("{}: {}", session_root.display(), err))?;
    if looks_like_tar_gz(url, &bytes) {
        extract_tar_gz_project(&bytes, &session_root)?;
    } else {
        write_text_import(url, &bytes, &session_root)?;
    }
    Ok(session_root)
}

fn fetch_bytes_blocking(url: &str) -> Result<Vec<u8>, String> {
    let response = ureq::get(url)
        .call()
        .map_err(|e| format!("HTTP fetch failed for {}: {}", url, e))?;
    let mut bytes = Vec::new();
    response
        .into_reader()
        .read_to_end(&mut bytes)
        .map_err(|e| format!("failed to read response body from {}: {}", url, e))?;
    Ok(bytes)
}

fn build_url_session_root(workspace_root: &Path, url: &str) -> PathBuf {
    let mut hasher = DefaultHasher::new();
    url.hash(&mut hasher);
    let slug = sanitize_slug(file_name_from_url(url).unwrap_or("project"));
    workspace_root.join(".studio-sessions").join("url").join(format!("{}-{:016x}", slug, hasher.finish()))
}

fn sanitize_slug(input: &str) -> String {
    let stem = input
        .trim_end_matches(".tar.gz")
        .trim_end_matches(".tgz")
        .trim_end_matches(".zip")
        .trim_end_matches(".vo");
    let mut out = String::new();
    for ch in stem.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch.to_ascii_lowercase());
        } else if !out.ends_with('-') {
            out.push('-');
        }
    }
    let trimmed = out.trim_matches('-');
    if trimmed.is_empty() {
        "project".to_string()
    } else {
        trimmed.to_string()
    }
}

fn file_name_from_url(url: &str) -> Option<&str> {
    let path = url.split('?').next().unwrap_or(url);
    let name = path.rsplit('/').next()?;
    if name.is_empty() { None } else { Some(name) }
}

fn looks_like_tar_gz(url: &str, bytes: &[u8]) -> bool {
    let lowered = url.to_ascii_lowercase();
    lowered.ends_with(".tar.gz") || lowered.ends_with(".tgz") || bytes.starts_with(&[0x1f, 0x8b])
}

fn write_text_import(url: &str, bytes: &[u8], session_root: &Path) -> Result<(), String> {
    let content = String::from_utf8(bytes.to_vec())
        .map_err(|err| format!("non-UTF-8 URL import '{}': {}", url, err))?;
    let mut file_name = file_name_from_url(url).unwrap_or("main.vo").to_string();
    if !file_name.contains('.') {
        file_name.push_str(".vo");
    }
    let target = session_root.join(file_name);
    fs::write(&target, content).map_err(|err| format!("{}: {}", target.display(), err))
}

fn extract_tar_gz_project(bytes: &[u8], session_root: &Path) -> Result<(), String> {
    let gz = GzDecoder::new(bytes);
    let mut archive = Archive::new(gz);
    let mut files = Vec::<(PathBuf, String)>::new();

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
        let Ok(content) = String::from_utf8(buf) else {
            continue;
        };
        files.push((path, content));
    }

    if files.is_empty() {
        return Err("archive contains no UTF-8 files".to_string());
    }

    let strip_prefix = shared_archive_prefix(&files);
    for (path, content) in files {
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
        fs::write(&target, content).map_err(|err| format!("{}: {}", target.display(), err))?;
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

fn shared_archive_prefix(files: &[(PathBuf, String)]) -> Option<PathBuf> {
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
