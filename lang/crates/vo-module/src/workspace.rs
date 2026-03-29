use std::path::{Path, PathBuf};

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
    let mut dir = project_dir.to_path_buf();
    loop {
        let candidate = dir.join("vo.work");
        if candidate.is_file() {
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
    let mut overrides = Vec::new();
    let mut seen = std::collections::HashSet::new();

    for (i, entry) in workfile.uses.iter().enumerate() {
        let local_dir = resolve_path(workfile_dir, &entry.path);

        let module = match &entry.module {
            Some(mp) => mp.clone(),
            None => {
                // Read vo.mod from the target directory
                let mod_path = local_dir.join("vo.mod");
                let content = std::fs::read_to_string(&mod_path).map_err(|e| {
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

        // Duplicate check
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

/// Verify that each override directory's `vo.mod` declares the expected module path.
/// Per spec §11.1: the local directory must identify the same canonical module path.
pub fn verify_override_identity(overrides: &[Override]) -> Result<(), Error> {
    for ov in overrides {
        let mod_path = ov.local_dir.join("vo.mod");
        let content = std::fs::read_to_string(&mod_path).map_err(|e| Error::Io(e))?;
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

fn resolve_path(base: &Path, relative: &str) -> PathBuf {
    let p = Path::new(relative);
    if p.is_absolute() {
        p.to_path_buf()
    } else {
        base.join(p)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_path_relative() {
        let base = Path::new("/home/user/project");
        let resolved = resolve_path(base, "../vogui");
        assert_eq!(resolved, PathBuf::from("/home/user/project/../vogui"));
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
