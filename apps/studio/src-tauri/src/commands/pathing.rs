use std::fs;
use std::path::{Component, Path, PathBuf};

use vo_module::project;

#[cfg(windows)]
const FILE_ATTRIBUTE_REPARSE_POINT: u32 = 0x0000_0400;

#[derive(Debug, Clone)]
pub struct ResolvedTarget {
    pub compile_path: PathBuf,
    pub output_base_path: PathBuf,
    pub source_root: PathBuf,
}

pub fn resolve_path(root: &Path, path: &str) -> Result<PathBuf, String> {
    let canonical_root = root
        .canonicalize()
        .map_err(|err| format!("{}: {}", root.display(), err))?;
    let input = Path::new(path);
    let relative = if input.is_absolute() {
        input
            .strip_prefix(&canonical_root)
            .or_else(|_| input.strip_prefix(root))
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
    reject_nested_links(&canonical_root, &normalized, path)?;
    Ok(resolved)
}

fn reject_nested_links(root: &Path, relative: &Path, original: &str) -> Result<(), String> {
    let components = relative.components().collect::<Vec<_>>();
    let mut current = root.to_path_buf();
    for (index, component) in components.iter().enumerate() {
        let Component::Normal(segment) = component else {
            return Err(format!("Path escapes session root: {original}"));
        };
        current.push(segment);
        let metadata = match fs::symlink_metadata(&current) {
            Ok(metadata) => metadata,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
            Err(error) => return Err(format!("{}: {}", current.display(), error)),
        };
        let file_type = metadata.file_type();
        if is_link_or_reparse_point(&metadata) {
            return Err(format!(
                "Path contains unsupported symbolic link or reparse point: {}",
                current.display(),
            ));
        }
        let last = index + 1 == components.len();
        if !last && !file_type.is_dir() {
            return Err(format!(
                "Path component is not a directory: {}",
                current.display()
            ));
        }
        if last && !file_type.is_dir() && !file_type.is_file() {
            return Err(format!(
                "Path is not a regular file or directory: {}",
                current.display()
            ));
        }
    }
    Ok(())
}

pub fn is_link_or_reparse_point(metadata: &fs::Metadata) -> bool {
    #[cfg(windows)]
    {
        use std::os::windows::fs::MetadataExt;
        metadata.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0
    }
    #[cfg(not(windows))]
    {
        metadata.file_type().is_symlink()
    }
}

pub fn find_project_root(path: &Path) -> Result<Option<PathBuf>, String> {
    let metadata = fs::symlink_metadata(path)
        .map_err(|error| format!("cannot inspect target path {}: {error}", path.display()))?;
    let file_type = metadata.file_type();
    if is_link_or_reparse_point(&metadata) {
        return Err(format!(
            "target path must not be a symbolic link or reparse point: {}",
            path.display()
        ));
    }
    let current = if file_type.is_dir() {
        path.to_path_buf()
    } else if file_type.is_file() {
        path.parent()
            .ok_or_else(|| format!("target file has no parent directory: {}", path.display()))?
            .to_path_buf()
    } else {
        return Err(format!(
            "target path must be a regular file or directory: {}",
            path.display()
        ));
    };
    project::find_project_root(&current).map_err(|error| error.to_string())
}

pub fn is_module_root(path: &Path) -> Result<bool, String> {
    let metadata = fs::symlink_metadata(path)
        .map_err(|error| format!("cannot inspect target path {}: {error}", path.display()))?;
    let file_type = metadata.file_type();
    if is_link_or_reparse_point(&metadata) {
        return Err(format!(
            "target path must not be a symbolic link or reparse point: {}",
            path.display()
        ));
    }
    if file_type.is_file() {
        return Ok(false);
    }
    if !file_type.is_dir() {
        return Err(format!(
            "target path must be a regular file or directory: {}",
            path.display()
        ));
    }
    project::find_project_root(path)
        .map(|root| root.is_some_and(|root| root == path))
        .map_err(|error| error.to_string())
}

pub fn resolve_compile_path(entry: &Path) -> Result<PathBuf, String> {
    Ok(find_project_root(entry)?.unwrap_or_else(|| entry.to_path_buf()))
}

pub fn source_root_for_target(target_path: &Path) -> Result<PathBuf, String> {
    if let Some(project_root) = find_project_root(target_path)? {
        return Ok(project_root);
    }
    if target_path.is_file() {
        return Ok(target_path.parent().unwrap_or(target_path).to_path_buf());
    }
    Ok(target_path.to_path_buf())
}

pub fn resolve_target(
    session_root: &Path,
    _workspace_root: &Path,
    entry_path: &str,
    single_file_run: bool,
) -> Result<ResolvedTarget, String> {
    let abs = resolve_path(session_root, entry_path)?;

    // single_file_run: compile only this file, regardless of vo.mod presence.
    // This is the runner-mode path for `run=file://path/to/file.vo`.
    if single_file_run && abs.is_file() {
        let source_root = abs.parent().unwrap_or(&abs).to_path_buf();
        return Ok(ResolvedTarget {
            output_base_path: abs.clone(),
            compile_path: abs,
            source_root,
        });
    }

    let output_base_path = resolve_compile_path(&abs)?;
    if is_standalone_single_file_target(&abs)? {
        return resolve_standalone_single_file_target(&abs, output_base_path);
    }
    let compile_path = resolve_compile_path(&abs)?;
    let source_root = source_root_for_target(&compile_path)?;
    Ok(ResolvedTarget {
        compile_path,
        output_base_path,
        source_root,
    })
}

pub fn resolve_run_target(
    session_root: &Path,
    workspace_root: &Path,
    entry_path: &str,
    single_file_run: bool,
) -> Result<ResolvedTarget, String> {
    resolve_target(session_root, workspace_root, entry_path, single_file_run)
}

fn is_standalone_single_file_target(path: &Path) -> Result<bool, String> {
    if !path.is_file() || path.extension().and_then(|ext| ext.to_str()) != Some("vo") {
        return Ok(false);
    }
    Ok(find_project_root(path)?.is_none())
}

fn resolve_standalone_single_file_target(
    target_path: &Path,
    output_base_path: PathBuf,
) -> Result<ResolvedTarget, String> {
    let canonical_target = target_path
        .canonicalize()
        .map_err(|err| format!("{}: {}", target_path.display(), err))?;
    let source_root = source_root_for_target(&canonical_target)?;
    Ok(ResolvedTarget {
        compile_path: canonical_target,
        output_base_path,
        source_root,
    })
}

#[cfg(test)]
mod tests {
    use super::{resolve_path, resolve_run_target, resolve_target};
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn resolve_run_target_keeps_standalone_single_files_on_their_original_paths() {
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

        let alpha_target = resolve_run_target(
            &session_root,
            &workspace_root,
            alpha.to_string_lossy().as_ref(),
            false,
        )
        .unwrap();
        let beta_target = resolve_run_target(
            &session_root,
            &workspace_root,
            beta.to_string_lossy().as_ref(),
            false,
        )
        .unwrap();
        let alpha_output_base = alpha.canonicalize().unwrap();
        let beta_output_base = beta.canonicalize().unwrap();

        assert_ne!(alpha_target.compile_path, beta_target.compile_path);
        assert_eq!(alpha_target.compile_path, alpha_output_base);
        assert_eq!(beta_target.compile_path, beta_output_base);
        assert_eq!(
            alpha_target.source_root,
            session_root.canonicalize().unwrap()
        );
        assert_eq!(
            beta_target.source_root,
            session_root.canonicalize().unwrap()
        );
        assert_eq!(alpha_target.output_base_path, alpha_output_base);
        assert_eq!(beta_target.output_base_path, beta_output_base);
        assert!(alpha_target.source_root.join("shared.vo").is_file());
        assert!(beta_target.source_root.join("shared.vo").is_file());
        remove_temp_dir(&root);
    }

    #[test]
    fn resolve_target_keeps_original_output_base_for_standalone_single_files() {
        let root = make_temp_dir("single-file-output-base");
        let workspace_root = root.join("workspace");
        let session_root = root.join("session");
        fs::create_dir_all(&workspace_root).unwrap();
        fs::create_dir_all(&session_root).unwrap();

        let entry = session_root.join("main.vo");
        fs::write(&entry, "package main\nfunc main() {}\n").unwrap();
        fs::write(session_root.join("shared.vo"), "package shared\n").unwrap();

        let resolved = resolve_target(
            &session_root,
            &workspace_root,
            entry.to_string_lossy().as_ref(),
            false,
        )
        .unwrap();
        let output_base = entry.canonicalize().unwrap();

        assert!(resolved.compile_path.is_file());
        assert_eq!(resolved.compile_path, output_base);
        assert_eq!(resolved.output_base_path, output_base);
        assert!(resolved.source_root.join("shared.vo").is_file());
        remove_temp_dir(&root);
    }

    #[test]
    fn resolve_run_target_keeps_module_entries_on_project_root() {
        let root = make_temp_dir("module-run");
        let workspace_root = root.join("workspace");
        let project_root = root.join("project");
        fs::create_dir_all(&workspace_root).unwrap();
        fs::create_dir_all(&project_root).unwrap();
        fs::write(
            project_root.join("vo.mod"),
            "module = \"github.com/acme/main\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();
        let main_file = project_root.join("main.vo");
        fs::write(&main_file, "package main\nfunc main() {}\n").unwrap();

        let resolved = resolve_run_target(
            &project_root,
            &workspace_root,
            main_file.to_string_lossy().as_ref(),
            false,
        )
        .unwrap();
        let canonical_project_root = project_root.canonicalize().unwrap();

        assert_eq!(resolved.compile_path, canonical_project_root);
        assert_eq!(resolved.output_base_path, canonical_project_root);
        assert_eq!(resolved.source_root, canonical_project_root);

        remove_temp_dir(&root);
    }

    #[test]
    fn isolated_single_file_keeps_its_output_inside_the_isolated_entry() {
        let root = make_temp_dir("isolated-output-base");
        let workspace_root = root.join("workspace");
        let project_root = root.join("project");
        let isolated_root = project_root.join("examples/demo");
        fs::create_dir_all(&workspace_root).unwrap();
        fs::create_dir_all(&isolated_root).unwrap();
        fs::write(
            project_root.join("vo.mod"),
            "module = \"github.com/acme/main\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();
        let entry = isolated_root.join("demo.vo");
        fs::write(&entry, "package main\nfunc main() {}\n").unwrap();

        let resolved = resolve_target(
            &isolated_root,
            &workspace_root,
            entry.to_string_lossy().as_ref(),
            true,
        )
        .unwrap();
        let canonical_entry = entry.canonicalize().unwrap();

        assert_eq!(resolved.compile_path, canonical_entry);
        assert_eq!(resolved.output_base_path, canonical_entry);
        assert_eq!(resolved.source_root, isolated_root.canonicalize().unwrap());
        assert_eq!(
            resolved.output_base_path.with_extension("vob"),
            canonical_entry.with_extension("vob")
        );

        remove_temp_dir(&root);
    }

    #[test]
    fn resolve_target_rejects_invalid_child_manifest_masking_parent_project() {
        let root = make_temp_dir("invalid-child-manifest");
        let workspace_root = root.join("workspace");
        let project_root = root.join("project");
        let child = project_root.join("child");
        fs::create_dir_all(&workspace_root).unwrap();
        fs::create_dir_all(child.join("vo.mod")).unwrap();
        fs::write(
            project_root.join("vo.mod"),
            "module = \"github.com/acme/main\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();
        let main_file = child.join("main.vo");
        fs::write(&main_file, "package main\nfunc main() {}\n").unwrap();

        let error = resolve_target(
            &project_root,
            &workspace_root,
            main_file.to_string_lossy().as_ref(),
            false,
        )
        .expect_err("invalid child manifests must remain visible to Studio callers");
        assert!(error.contains("vo.mod"), "{error}");
        assert!(error.contains("Directory"), "{error}");

        remove_temp_dir(&root);
    }

    #[test]
    fn standalone_single_file_resolution_does_not_materialize_workspace_protocol_state() {
        let root = make_temp_dir("single-file-workfile");
        let workspace_root = root.join("workspace");
        let repo_root = root.join("repo");
        let volang_root = repo_root.join("volang");
        let examples_root = volang_root.join("examples");
        fs::create_dir_all(&workspace_root).unwrap();
        fs::create_dir_all(&examples_root).unwrap();
        fs::write(
            volang_root.join("vo.work"),
            "version = 1\nmembers = [\".\"]\n",
        )
        .unwrap();
        let entry = examples_root.join("tetris.vo");
        fs::write(&entry, "package main\nfunc main() {}\n").unwrap();

        let resolved = resolve_run_target(
            &volang_root,
            &workspace_root,
            entry.to_string_lossy().as_ref(),
            false,
        )
        .unwrap();
        assert_eq!(resolved.compile_path, entry.canonicalize().unwrap());
        assert_eq!(resolved.source_root, examples_root.canonicalize().unwrap());
        assert!(!resolved.source_root.join("vo.work").exists());

        remove_temp_dir(&root);
    }

    #[cfg(unix)]
    #[test]
    fn path_resolution_allows_an_authorized_root_link_and_rejects_nested_links() {
        use std::os::unix::fs::symlink;

        let root = make_temp_dir("path-link-boundary");
        let real = root.join("real");
        let alias = root.join("alias");
        let outside = root.join("outside");
        fs::create_dir_all(&real).unwrap();
        fs::create_dir_all(&outside).unwrap();
        fs::write(real.join("main.vo"), "package main\n").unwrap();
        fs::write(outside.join("secret.vo"), "package secret\n").unwrap();
        symlink(&real, &alias).unwrap();
        symlink(&outside, real.join("linked")).unwrap();

        assert_eq!(
            resolve_path(&alias, "main.vo").unwrap(),
            real.canonicalize().unwrap().join("main.vo"),
        );
        let error = resolve_path(&alias, "linked/secret.vo")
            .expect_err("nested symbolic link must be rejected");
        assert!(error.contains("unsupported symbolic link"), "{error}");

        remove_temp_dir(&root);
    }

    #[cfg(windows)]
    #[test]
    fn path_resolution_rejects_nested_directory_junctions() {
        use std::process::Command;

        let root = make_temp_dir("path-junction-boundary");
        let workspace = root.join("workspace");
        let outside = root.join("outside");
        let junction = workspace.join("linked");
        fs::create_dir_all(&workspace).unwrap();
        fs::create_dir_all(&outside).unwrap();
        let output = Command::new("cmd")
            .args(["/C", "mklink", "/J"])
            .arg(&junction)
            .arg(&outside)
            .output()
            .expect("cmd must be available on Windows");
        assert!(
            output.status.success(),
            "mklink failed: {}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );

        let error = resolve_path(&workspace, "linked/created/main.vo")
            .expect_err("nested junction must be rejected");
        assert!(error.contains("reparse point"), "{error}");
        assert!(!outside.join("created").exists());

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
