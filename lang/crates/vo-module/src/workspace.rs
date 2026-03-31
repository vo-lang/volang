use std::collections::HashMap;
use std::path::{Path, PathBuf};

use vo_common::vfs::{normalize_fs_path, FileSystem, RealFs};

use crate::identity::ModulePath;
use crate::schema::modfile::ModFile;
use crate::schema::workfile::WorkFile;
use crate::Error;

/// A resolved workspace override: canonical module path mapped to a local directory.
#[derive(Debug, Clone)]
pub struct Override {
    pub module: ModulePath,
    pub local_dir: PathBuf,
}

/// Discover the nearest ancestor `vo.work` file starting from `project_dir`.
/// Returns None if no `vo.work` is found or if `VOWORK=off`.
pub fn discover_workfile(project_dir: &Path) -> Option<PathBuf> {
    if std::env::var("VOWORK").ok().as_deref() == Some("off") {
        return None;
    }
    discover_workfile_in(&RealFs::new("."), project_dir)
}

pub fn discover_workfile_in<F: FileSystem>(fs: &F, project_dir: &Path) -> Option<PathBuf> {
    let mut dir = normalize_fs_path(project_dir);
    loop {
        let candidate = dir.join("vo.work");
        if fs.exists(&candidate) && !fs.is_dir(&candidate) {
            return Some(candidate);
        }
        if !dir.pop() {
            return None;
        }
    }
}

/// Parse a `vo.work` file and resolve all override entries.
/// For entries without an explicit `module` field, reads `<path>/vo.mod` to
/// determine the canonical module path.
pub fn resolve_overrides(workfile: &WorkFile, workfile_dir: &Path) -> Result<Vec<Override>, Error> {
    resolve_overrides_in(&RealFs::new("."), workfile, workfile_dir)
}

pub fn resolve_overrides_in<F: FileSystem>(
    fs: &F,
    workfile: &WorkFile,
    workfile_dir: &Path,
) -> Result<Vec<Override>, Error> {
    let mut overrides = Vec::new();
    let mut seen = std::collections::HashSet::new();

    for (i, entry) in workfile.uses.iter().enumerate() {
        let local_dir = resolve_path(workfile_dir, &entry.path);

        let module = match &entry.module {
            Some(mp) => mp.clone(),
            None => {
                let mod_path = local_dir.join("vo.mod");
                let content = fs.read_file(&mod_path).map_err(|e| {
                    Error::WorkFileParse(format!(
                        "use[{i}]: cannot read {}: {e}",
                        mod_path.display()
                    ))
                })?;
                let mf = ModFile::parse(&content).map_err(|e| {
                    Error::WorkFileParse(format!(
                        "use[{i}]: error parsing {}: {e}",
                        mod_path.display()
                    ))
                })?;
                mf.module
            }
        };

        if !seen.insert(module.as_str().to_string()) {
            return Err(Error::WorkFileParse(format!(
                "use[{i}]: duplicate override for {}",
                module
            )));
        }

        overrides.push(Override { module, local_dir });
    }

    Ok(overrides)
}

pub fn validate_overrides(
    overrides: &[Override],
    root_module: Option<&ModulePath>,
) -> Result<(), Error> {
    validate_overrides_in(&RealFs::new("."), overrides, root_module)
}

pub fn validate_overrides_in<F: FileSystem>(
    fs: &F,
    overrides: &[Override],
    root_module: Option<&ModulePath>,
) -> Result<(), Error> {
    verify_override_identity_in(fs, overrides)?;
    if let Some(root_module) = root_module {
        check_no_self_override(root_module, overrides)?;
    }
    Ok(())
}

pub fn resolve_validated_overrides(
    workfile: &WorkFile,
    workfile_dir: &Path,
    root_module: Option<&ModulePath>,
) -> Result<Vec<Override>, Error> {
    resolve_validated_overrides_in(&RealFs::new("."), workfile, workfile_dir, root_module)
}

pub fn resolve_validated_overrides_in<F: FileSystem>(
    fs: &F,
    workfile: &WorkFile,
    workfile_dir: &Path,
    root_module: Option<&ModulePath>,
) -> Result<Vec<Override>, Error> {
    let overrides = resolve_overrides_in(fs, workfile, workfile_dir)?;
    validate_overrides_in(fs, &overrides, root_module)?;
    Ok(overrides)
}

pub fn load_workspace_overrides_in<F: FileSystem>(
    fs: &F,
    project_dir: &Path,
    root_module: Option<&ModulePath>,
) -> Result<Vec<Override>, Error> {
    let Some(workfile_path) = discover_workfile_in(fs, project_dir) else {
        return Ok(Vec::new());
    };
    let workfile_dir = workfile_path.parent().unwrap_or(project_dir);
    let content = fs.read_file(&workfile_path).map_err(|e| {
        Error::WorkFileParse(format!("cannot read {}: {e}", workfile_path.display()))
    })?;
    let workfile = WorkFile::parse(&content).map_err(|e| {
        Error::WorkFileParse(format!("error parsing {}: {e}", workfile_path.display()))
    })?;
    resolve_validated_overrides_in(fs, &workfile, workfile_dir, root_module)
}

/// Verify that each override directory's `vo.mod` declares the expected module path.
/// Per spec §11.1: the local directory must identify the same canonical module path.
pub fn verify_override_identity(overrides: &[Override]) -> Result<(), Error> {
    verify_override_identity_in(&RealFs::new("."), overrides)
}

pub fn verify_override_identity_in<F: FileSystem>(
    fs: &F,
    overrides: &[Override],
) -> Result<(), Error> {
    for ov in overrides {
        let mod_path = ov.local_dir.join("vo.mod");
        let content = fs.read_file(&mod_path).map_err(Error::Io)?;
        let mf = ModFile::parse(&content).map_err(|e| {
            Error::WorkFileParse(format!("override {}: error parsing vo.mod: {e}", ov.module))
        })?;
        if mf.module != ov.module {
            return Err(Error::WorkspaceIdentityMismatch {
                expected: ov.module.as_str().to_string(),
                found: mf.module.as_str().to_string(),
                path: ov.local_dir.display().to_string(),
            });
        }
    }
    Ok(())
}

/// Check that the root module does not override itself.
/// Per spec §11.1: the root module MUST NOT override itself via vo.work.
pub fn check_no_self_override(
    root_module: &ModulePath,
    overrides: &[Override],
) -> Result<(), Error> {
    for ov in overrides {
        if ov.module == *root_module {
            return Err(Error::SelfOverride);
        }
    }
    Ok(())
}

/// Check that all external imports from override sources are either:
/// - owned by the root module
/// - owned by another active override
/// - present in the root vo.lock
///
/// Per spec §11.2: if override source imports an external package not covered
/// by the above, the build fails.
pub fn check_override_imports_covered(
    import_path: &str,
    root_module: &ModulePath,
    overrides: &[Override],
    locked_modules: &[ModulePath],
) -> Result<(), Error> {
    // Check if owned by root module
    if root_module.owns_import(import_path).is_some() {
        return Ok(());
    }

    // Check if owned by an active override
    if overrides
        .iter()
        .any(|ov| ov.module.owns_import(import_path).is_some())
    {
        return Ok(());
    }

    // Check if owned by a locked module
    if crate::identity::find_owning_module(import_path, locked_modules).is_some() {
        return Ok(());
    }

    Err(Error::OverrideUnlockedDep {
        importer: "workspace override".to_string(),
        import_path: import_path.to_string(),
    })
}

/// Load workspace replaces as a module-path → local-directory map.
///
/// Discovers the nearest `vo.work`, resolves and validates overrides,
/// normalizes paths, and excludes the current project root from the map.
/// Returns an empty map if `VOWORK=off` or no `vo.work` is found.
///
/// This is the canonical API for obtaining workspace replaces and should be
/// used by both native (vo-engine) and web (vo-web) compilation paths.
pub fn load_workspace_replaces<F: FileSystem>(
    fs: &F,
    project_root: &Path,
    root_module: Option<&ModulePath>,
) -> Result<HashMap<String, PathBuf>, Error> {
    if std::env::var("VOWORK").ok().as_deref() == Some("off") {
        return Ok(HashMap::new());
    }
    let auto_root_module;
    let effective_root_module = match root_module {
        Some(m) => Some(m),
        None => {
            let mod_path = if project_root == Path::new(".") || project_root.as_os_str().is_empty()
            {
                PathBuf::from("vo.mod")
            } else {
                project_root.join("vo.mod")
            };
            auto_root_module = fs
                .read_file(&mod_path)
                .ok()
                .and_then(|content| ModFile::parse(&content).ok())
                .map(|mf| mf.module);
            auto_root_module.as_ref()
        }
    };
    let overrides = load_workspace_overrides_in(fs, project_root, effective_root_module)?;
    let normalized_root = normalize_fs_path(project_root);
    let mut replaces = HashMap::new();
    for ov in overrides {
        let local_dir = normalize_fs_path(&ov.local_dir);
        if local_dir == normalized_root {
            continue;
        }
        replaces.insert(ov.module.as_str().to_string(), local_dir);
    }
    Ok(replaces)
}

fn resolve_path(base: &Path, relative: &str) -> PathBuf {
    let p = Path::new(relative);
    if p.is_absolute() {
        normalize_fs_path(p)
    } else {
        normalize_fs_path(&base.join(p))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_path_relative() {
        let base = Path::new("/home/user/project");
        let resolved = resolve_path(base, "../vogui");
        assert_eq!(resolved, PathBuf::from("/home/user/vogui"));
    }

    #[test]
    fn test_resolve_path_absolute() {
        let base = Path::new("/home/user/project");
        let resolved = resolve_path(base, "/opt/vogui");
        assert_eq!(resolved, PathBuf::from("/opt/vogui"));
    }

    #[test]
    fn test_check_no_self_override_ok() {
        let root = ModulePath::parse("github.com/acme/app").unwrap();
        let overrides = vec![Override {
            module: ModulePath::parse("github.com/vo-lang/vogui").unwrap(),
            local_dir: PathBuf::from("../vogui"),
        }];
        assert!(check_no_self_override(&root, &overrides).is_ok());
    }

    #[test]
    fn test_check_no_self_override_fail() {
        let root = ModulePath::parse("github.com/acme/app").unwrap();
        let overrides = vec![Override {
            module: ModulePath::parse("github.com/acme/app").unwrap(),
            local_dir: PathBuf::from("."),
        }];
        assert!(check_no_self_override(&root, &overrides).is_err());
    }

    #[test]
    fn test_check_override_imports_covered_by_root() {
        let root = ModulePath::parse("github.com/acme/app").unwrap();
        let overrides = vec![];
        let locked = vec![];
        assert!(check_override_imports_covered(
            "github.com/acme/app/util",
            &root,
            &overrides,
            &locked
        )
        .is_ok());
    }

    #[test]
    fn test_check_override_imports_covered_by_lock() {
        let root = ModulePath::parse("github.com/acme/app").unwrap();
        let overrides = vec![];
        let locked = vec![ModulePath::parse("github.com/vo-lang/vogui").unwrap()];
        assert!(check_override_imports_covered(
            "github.com/vo-lang/vogui/widget",
            &root,
            &overrides,
            &locked
        )
        .is_ok());
    }

    #[test]
    fn test_check_override_imports_not_covered() {
        let root = ModulePath::parse("github.com/acme/app").unwrap();
        let overrides = vec![];
        let locked = vec![];
        assert!(check_override_imports_covered(
            "github.com/acme/newdep/pkg",
            &root,
            &overrides,
            &locked
        )
        .is_err());
    }
}
