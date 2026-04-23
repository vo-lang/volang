use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};

use vo_common::vfs::{normalize_fs_path, FileSystem, RealFs};

use crate::identity::{classify_import, ImportClass, ModIdentity, ModulePath};
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

pub fn load_workspace_overrides_in<F: FileSystem>(
    fs: &F,
    project_dir: &Path,
    root_module: Option<&ModIdentity>,
) -> Result<Vec<Override>, Error> {
    if std::env::var("VOWORK").ok().as_deref() == Some("off") {
        return Ok(Vec::new());
    }
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
                // Workspace overrides can only target canonical github modules
                // (spec §11). Ephemeral `local/*` roots are never published and
                // thus never referenced from `vo.work`.
                mf.module.as_github().cloned().ok_or_else(|| {
                    Error::WorkFileParse(format!(
                        "use[{i}]: {} declares an ephemeral 'local/*' module \
                         which cannot be a workspace override target",
                        local_dir.display(),
                    ))
                })?
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

    for ov in &overrides {
        let mod_path = ov.local_dir.join("vo.mod");
        let content = fs.read_file(&mod_path).map_err(Error::Io)?;
        let mf = ModFile::parse(&content).map_err(|e| {
            Error::WorkFileParse(format!("override {}: error parsing vo.mod: {e}", ov.module))
        })?;
        if mf.module.as_github() != Some(&ov.module) {
            return Err(Error::WorkspaceIdentityMismatch {
                expected: ov.module.as_str().to_string(),
                found: mf.module.as_str().to_string(),
                path: ov.local_dir.display().to_string(),
            });
        }
    }
    if let Some(root_module) = root_module {
        // A `local/*` root cannot collide with any canonical override path, so
        // the self-override check only runs when the root is a github module.
        if let Some(root_github) = root_module.as_github() {
            check_no_self_override(root_github, &overrides)?;
        }
    }
    Ok(overrides)
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
    root_module: &ModIdentity,
    overrides: &[Override],
    locked_modules: &[ModulePath],
) -> Result<(), Error> {
    check_override_imports_covered_by(
        "workspace override",
        import_path,
        root_module,
        overrides,
        locked_modules,
    )
}

fn check_override_imports_covered_by(
    importer: &str,
    import_path: &str,
    root_module: &ModIdentity,
    overrides: &[Override],
    locked_modules: &[ModulePath],
) -> Result<(), Error> {
    // Check if owned by root module. Only canonical github roots can own
    // external imports; `local/*` ephemeral roots own nothing in the external
    // namespace and always fall through to override/lock lookup.
    if let Some(root_github) = root_module.as_github() {
        if root_github.owns_import(import_path).is_some() {
            return Ok(());
        }
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
        importer: importer.to_string(),
        import_path: import_path.to_string(),
    })
}

pub fn validate_override_external_imports<F: FileSystem>(
    fs: &F,
    root_module: &ModIdentity,
    overrides_to_scan: &[Override],
    active_overrides: &[Override],
    locked_modules: &[ModulePath],
    importer_prefix: &str,
) -> Result<(), Error> {
    for override_entry in overrides_to_scan {
        let imports = scan_external_imports_in(fs, &override_entry.local_dir)?;
        let importer = format!("{} {}", importer_prefix, override_entry.module);
        for import_path in imports {
            check_override_imports_covered_by(
                &importer,
                &import_path,
                root_module,
                active_overrides,
                locked_modules,
            )?;
        }
    }
    Ok(())
}

fn scan_external_imports_in<F: FileSystem>(fs: &F, dir: &Path) -> Result<BTreeSet<String>, Error> {
    let mut imports = BTreeSet::new();
    scan_external_imports_dir_in(fs, dir, &mut imports)?;
    Ok(imports)
}

fn scan_external_imports_dir_in<F: FileSystem>(
    fs: &F,
    dir: &Path,
    imports: &mut BTreeSet<String>,
) -> Result<(), Error> {
    for entry in fs.read_dir(dir).map_err(Error::Io)? {
        if fs.is_dir(&entry) {
            let name = entry
                .file_name()
                .and_then(|value| value.to_str())
                .unwrap_or("");
            if name.starts_with('.')
                || name == "vendor"
                || name == "testdata"
                || name == "node_modules"
                || name == "target"
                || name == "dist"
            {
                continue;
            }
            scan_external_imports_dir_in(fs, &entry, imports)?;
            continue;
        }
        if entry
            .extension()
            .map(|value| value == "vo")
            .unwrap_or(false)
        {
            scan_external_imports_file_in(fs, &entry, imports)?;
        }
    }
    Ok(())
}

fn scan_external_imports_file_in<F: FileSystem>(
    fs: &F,
    path: &Path,
    imports: &mut BTreeSet<String>,
) -> Result<(), Error> {
    let content = fs.read_file(path).map_err(Error::Io)?;
    let (file, diagnostics, _) = vo_syntax::parse(&content, 0);
    if diagnostics.has_errors() {
        let detail = diagnostics
            .iter()
            .map(|diagnostic| diagnostic.message.as_str())
            .collect::<Vec<_>>()
            .join("; ");
        return Err(Error::SourceScan(format!(
            "failed to parse {} while scanning imports: {}",
            path.display(),
            detail,
        )));
    }
    for import in &file.imports {
        let import_path = import.path.value.clone();
        if classify_import(&import_path)? == ImportClass::External {
            imports.insert(import_path);
        }
    }
    Ok(())
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
    root_module: Option<&ModIdentity>,
) -> Result<HashMap<String, PathBuf>, Error> {
    if std::env::var("VOWORK").ok().as_deref() == Some("off") {
        return Ok(HashMap::new());
    }
    let auto_root_module;
    let effective_root_module = match root_module {
        Some(module) => Some(module),
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
    static ENV_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

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
        let root: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();
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
        let root: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();
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
        let root: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();
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

    #[test]
    fn test_load_workspace_overrides_in_honors_vowork_off() {
        let _guard = ENV_LOCK.lock().unwrap();
        let old = std::env::var("VOWORK").ok();
        std::env::set_var("VOWORK", "off");

        let mut fs = vo_common::vfs::MemoryFs::new();
        fs.add_file(
            "workspace/vo.work",
            "version = 1\n\n[[use]]\npath = \".\"\n",
        );
        fs.add_file(
            "workspace/vo.mod",
            "module github.com/acme/app\nvo ^0.1.0\n",
        );

        let root: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();
        let result = load_workspace_overrides_in(&fs, Path::new("workspace"), Some(&root)).unwrap();

        match old {
            Some(value) => std::env::set_var("VOWORK", value),
            None => std::env::remove_var("VOWORK"),
        }

        assert!(result.is_empty());
    }
}
