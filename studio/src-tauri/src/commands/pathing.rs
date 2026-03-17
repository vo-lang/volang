use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Component, Path, PathBuf};

#[derive(Debug, Clone)]
pub struct ResolvedRunTarget {
    pub compile_path: PathBuf,
    pub source_root: PathBuf,
}

pub fn resolve_path(root: &Path, path: &str) -> Result<PathBuf, String> {
    let canonical_root = root
        .canonicalize()
        .map_err(|err| format!("{}: {}", root.display(), err))?;
    let input = Path::new(path);
    let relative = if input.is_absolute() {
        canonicalize_absolute_input(input)?
            .strip_prefix(&canonical_root)
            .map_err(|_| format!("Path escapes session root: {}", path))?
            .to_path_buf()
    } else {
        PathBuf::from(path)
    };
    let mut normalized = PathBuf::new();
    for component in relative.components() {
        match component {
            Component::CurDir => {}
            Component::Normal(segment) => normalized.push(segment),
            Component::ParentDir => {
                if !normalized.pop() {
                    return Err(format!("Path escapes session root: {}", path));
                }
            }
            Component::RootDir | Component::Prefix(_) => {
                return Err(format!("Path escapes session root: {}", path));
            }
        }
    }
    let resolved = canonical_root.join(&normalized);
    let mut probe = resolved.as_path();
    while !probe.exists() {
        probe = probe
            .parent()
            .ok_or_else(|| format!("Path escapes session root: {}", path))?;
    }
    let canonical_probe = probe
        .canonicalize()
        .map_err(|err| format!("{}: {}", probe.display(), err))?;
    if !canonical_probe.starts_with(&canonical_root) {
        return Err(format!("Path escapes session root: {}", path));
    }
    Ok(resolved)
}

fn canonicalize_absolute_input(path: &Path) -> Result<PathBuf, String> {
    if !path.is_absolute() {
        return Ok(path.to_path_buf());
    }
    if path.exists() {
        return path
            .canonicalize()
            .map_err(|err| format!("{}: {}", path.display(), err));
    }

    let mut missing_parts = Vec::new();
    let mut probe = path;
    while !probe.exists() {
        let name = probe
            .file_name()
            .ok_or_else(|| format!("Path escapes session root: {}", path.display()))?;
        missing_parts.push(name.to_os_string());
        probe = probe
            .parent()
            .ok_or_else(|| format!("Path escapes session root: {}", path.display()))?;
    }

    let mut canonical = probe
        .canonicalize()
        .map_err(|err| format!("{}: {}", probe.display(), err))?;
    for part in missing_parts.iter().rev() {
        canonical.push(part);
    }
    Ok(canonical)
}

pub fn find_project_root(path: &Path) -> Option<PathBuf> {
    let mut current = if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent()?.to_path_buf()
    };

    loop {
        if current.join("vo.mod").is_file() {
            return Some(current);
        }
        if !current.pop() {
            return None;
        }
    }
}

pub fn resolve_compile_path(entry: &Path) -> PathBuf {
    if let Some(project_root) = find_project_root(entry) {
        return project_root;
    }
    entry.to_path_buf()
}

pub fn resolve_compile_target(root: &Path, entry_path: &str) -> Result<PathBuf, String> {
    let abs = resolve_path(root, entry_path)?;
    Ok(resolve_compile_path(&abs))
}

pub fn source_root_for_target(target_path: &Path) -> PathBuf {
    if let Some(project_root) = find_project_root(target_path) {
        return project_root;
    }
    if target_path.is_file() {
        return target_path.parent().unwrap_or(target_path).to_path_buf();
    }
    target_path.to_path_buf()
}

pub fn resolve_run_target(session_root: &Path, workspace_root: &Path, entry_path: &str, single_file_run: bool) -> Result<ResolvedRunTarget, String> {
    let abs = resolve_path(session_root, entry_path)?;

    // single_file_run: compile only this file, regardless of vo.mod presence.
    // This is the runner-mode path for `run=file://path/to/file.vo`.
    if single_file_run && abs.is_file() {
        let source_root = abs.parent().unwrap_or(&abs).to_path_buf();
        return Ok(ResolvedRunTarget { compile_path: abs, source_root });
    }

    if is_standalone_single_file_target(&abs) {
        return resolve_standalone_single_file_target(workspace_root, &abs);
    }
    let compile_path = resolve_compile_path(&abs);
    let source_root = source_root_for_target(&compile_path);
    Ok(ResolvedRunTarget { compile_path, source_root })
}

fn is_standalone_single_file_target(path: &Path) -> bool {
    path.is_file()
        && path.extension().and_then(|ext| ext.to_str()) == Some("vo")
        && find_project_root(path).is_none()
}

fn resolve_standalone_single_file_target(workspace_root: &Path, target_path: &Path) -> Result<ResolvedRunTarget, String> {
    let canonical_target = target_path
        .canonicalize()
        .map_err(|err| format!("{}: {}", target_path.display(), err))?;
    let source_root = source_root_for_target(&canonical_target);
    let materialized_root = standalone_single_file_run_root(workspace_root, &canonical_target);

    remove_existing_path(&materialized_root)?;
    copy_path_recursive(&source_root, &materialized_root)?;

    let relative = canonical_target
        .strip_prefix(&source_root)
        .map_err(|err| err.to_string())?;
    let materialized_entry = materialized_root.join(relative);
    let compile_path = resolve_compile_path(&materialized_entry);
    let source_root = source_root_for_target(&materialized_entry);
    Ok(ResolvedRunTarget { compile_path, source_root })
}

fn standalone_single_file_run_root(workspace_root: &Path, target_path: &Path) -> PathBuf {
    let canonical_workspace = workspace_root.canonicalize().unwrap_or_else(|_| workspace_root.to_path_buf());
    let canonical_target = target_path.canonicalize().unwrap_or_else(|_| target_path.to_path_buf());

    let mut workspace_hasher = DefaultHasher::new();
    canonical_workspace.to_string_lossy().hash(&mut workspace_hasher);

    let mut target_hasher = DefaultHasher::new();
    canonical_target.to_string_lossy().hash(&mut target_hasher);

    let mut root = dirs::cache_dir().unwrap_or_else(std::env::temp_dir);
    root.push("vo-studio");
    root.push(format!("workspace-{:016x}", workspace_hasher.finish()));
    root.push("single-file-run");

    if let Some(parent) = canonical_target.parent() {
        for component in parent.components() {
            if let Component::Normal(segment) = component {
                root.push(sanitize_path_component(&segment.to_string_lossy()));
            }
        }
    }

    let file_name = canonical_target
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("entry.vo");
    root.push(format!(
        "__single__{}-{:016x}",
        sanitize_path_component(file_name),
        target_hasher.finish(),
    ));
    root
}

fn sanitize_path_component(input: &str) -> String {
    let mut out = String::new();
    for ch in input.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch.to_ascii_lowercase());
        } else if !out.ends_with('-') {
            out.push('-');
        }
    }
    let trimmed = out.trim_matches('-');
    if trimmed.is_empty() {
        "entry".to_string()
    } else {
        trimmed.to_string()
    }
}

fn remove_existing_path(path: &Path) -> Result<(), String> {
    if !path.exists() {
        return Ok(());
    }
    let metadata = fs::symlink_metadata(path)
        .map_err(|err| format!("{}: {}", path.display(), err))?;
    if metadata.is_dir() {
        fs::remove_dir_all(path)
            .map_err(|err| format!("{}: {}", path.display(), err))?;
    } else {
        fs::remove_file(path)
            .map_err(|err| format!("{}: {}", path.display(), err))?;
    }
    Ok(())
}

fn copy_path_recursive(src: &Path, dst: &Path) -> Result<(), String> {
    if src.is_dir() {
        fs::create_dir_all(dst)
            .map_err(|err| format!("{}: {}", dst.display(), err))?;
        for entry in fs::read_dir(src)
            .map_err(|err| format!("{}: {}", src.display(), err))?
        {
            let entry = entry
                .map_err(|err| format!("{}: {}", src.display(), err))?;
            let src_path = entry.path();
            let dst_path = dst.join(entry.file_name());
            copy_path_recursive(&src_path, &dst_path)?;
        }
        return Ok(());
    }

    if let Some(parent) = dst.parent() {
        fs::create_dir_all(parent)
            .map_err(|err| format!("{}: {}", parent.display(), err))?;
    }
    fs::copy(src, dst)
        .map(|_| ())
        .map_err(|err| format!("{} -> {}: {}", src.display(), dst.display(), err))
}

#[cfg(test)]
mod tests {
    use super::{resolve_run_target, standalone_single_file_run_root};
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn resolve_run_target_materializes_standalone_single_files_per_file() {
        let root = make_temp_dir("single-file-run");
        let workspace_root = root.join("workspace");
        let session_root = root.join("session");
        fs::create_dir_all(&workspace_root).unwrap();
        fs::create_dir_all(&session_root).unwrap();

        let alpha = session_root.join("alpha.vo");
        let beta = session_root.join("beta.vo");
        let shared = session_root.join("shared.vo");
        fs::write(&alpha, "package main\nfunc main() {}\n").unwrap();
        fs::write(&beta, "package main\nfunc main() {}\n").unwrap();
        fs::write(&shared, "package shared\n").unwrap();

        let alpha_target = resolve_run_target(&session_root, &workspace_root, alpha.to_string_lossy().as_ref(), false).unwrap();
        let beta_target = resolve_run_target(&session_root, &workspace_root, beta.to_string_lossy().as_ref(), false).unwrap();

        assert_ne!(alpha_target.compile_path, beta_target.compile_path);
        assert_ne!(alpha_target.source_root, beta_target.source_root);
        assert!(alpha_target.compile_path.is_file());
        assert!(beta_target.compile_path.is_file());
        assert!(alpha_target.source_root.join("shared.vo").is_file());
        assert!(beta_target.source_root.join("shared.vo").is_file());

        let alpha_cache_root = standalone_single_file_run_root(&workspace_root, &alpha);
        let beta_cache_root = standalone_single_file_run_root(&workspace_root, &beta);
        let _ = fs::remove_dir_all(alpha_cache_root);
        let _ = fs::remove_dir_all(beta_cache_root);
        remove_temp_dir(&root);
    }

    #[test]
    fn resolve_run_target_keeps_module_entries_on_project_root() {
        let root = make_temp_dir("module-run");
        let workspace_root = root.join("workspace");
        let project_root = root.join("project");
        fs::create_dir_all(&workspace_root).unwrap();
        fs::create_dir_all(&project_root).unwrap();
        fs::write(project_root.join("vo.mod"), "module main\n\nvo 1.0\n").unwrap();
        let main_file = project_root.join("main.vo");
        fs::write(&main_file, "package main\nfunc main() {}\n").unwrap();

        let resolved = resolve_run_target(&project_root, &workspace_root, main_file.to_string_lossy().as_ref(), false).unwrap();
        let canonical_project_root = project_root.canonicalize().unwrap();

        assert_eq!(resolved.compile_path, canonical_project_root);
        assert_eq!(resolved.source_root, canonical_project_root);

        remove_temp_dir(&root);
    }

    fn make_temp_dir(name: &str) -> PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("studio-{}-{}", name, unique));
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn remove_temp_dir(path: &Path) {
        let _ = fs::remove_dir_all(path);
    }
}
