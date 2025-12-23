//! Module resolution and dependency closure computation.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::error::{ModuleError, ModuleResult};
use crate::modfile::ModFile;

/// The dependency cache directory name.
const DEPS_DIR: &str = ".vodeps";

/// Standard library import prefix.
const STD_PREFIX: &str = "std/";

/// A resolved module with its version and filesystem location.
#[derive(Debug, Clone)]
pub struct ResolvedModule {
    /// Module path (e.g., "github.com/foo/bar").
    pub path: String,
    /// Version (e.g., "v1.2.3").
    pub version: String,
    /// Filesystem root directory.
    pub root: PathBuf,
    /// The dependency chain that led to this module (for error reporting).
    pub chain: Vec<String>,
}

/// The computed module closure: all modules required for a build.
#[derive(Debug, Clone)]
pub struct ModuleClosure {
    /// The root module path.
    pub root_module: String,
    /// The root module directory.
    pub root_dir: PathBuf,
    /// All resolved modules (module path -> resolved module).
    pub modules: HashMap<String, ResolvedModule>,
}

impl ModuleClosure {
    /// Returns the module that owns the given import path using longest prefix match.
    pub fn find_owning_module(&self, import_path: &str) -> Option<&ResolvedModule> {
        // Check root module first
        if import_path == self.root_module || import_path.starts_with(&format!("{}/", self.root_module)) {
            return None; // Owned by root module, handled separately
        }

        // Find longest prefix match among dependencies
        let mut best_match: Option<&ResolvedModule> = None;
        let mut best_len = 0;

        for (mod_path, resolved) in &self.modules {
            if import_path == mod_path || import_path.starts_with(&format!("{}/", mod_path)) {
                if mod_path.len() > best_len {
                    best_len = mod_path.len();
                    best_match = Some(resolved);
                }
            }
        }

        best_match
    }

    /// Checks if the import path belongs to the root module.
    pub fn is_root_module_path(&self, import_path: &str) -> bool {
        import_path == self.root_module || import_path.starts_with(&format!("{}/", self.root_module))
    }
}

/// A resolved package location.
#[derive(Debug, Clone)]
pub struct ResolvedPackage {
    /// The import path.
    pub import_path: String,
    /// The filesystem directory containing the package.
    pub dir: PathBuf,
    /// Whether this is a standard library package.
    pub is_std: bool,
}

/// Module resolver for computing closures and resolving imports.
pub struct ModuleResolver {
    /// Project root directory (where vo.mod is located).
    project_root: PathBuf,
    /// Standard library root (if set).
    std_root: Option<PathBuf>,
}

impl ModuleResolver {
    /// Creates a new resolver for the given project root.
    pub fn new<P: AsRef<Path>>(project_root: P) -> Self {
        ModuleResolver {
            project_root: project_root.as_ref().to_path_buf(),
            std_root: None,
        }
    }

    /// Sets the standard library root directory.
    pub fn with_std_root<P: AsRef<Path>>(mut self, std_root: P) -> Self {
        self.std_root = Some(std_root.as_ref().to_path_buf());
        self
    }

    /// Returns the path to the vo.mod file.
    pub fn mod_file_path(&self) -> PathBuf {
        self.project_root.join("vo.mod")
    }

    /// Returns the path to the .vodeps directory.
    pub fn deps_dir(&self) -> PathBuf {
        self.project_root.join(DEPS_DIR)
    }

    /// Computes the transitive dependency closure.
    ///
    /// This reads all vo.mod files recursively and builds a complete
    /// module closure. Returns an error if:
    /// - A required module is not found in .vodeps
    /// - The same module is required at different versions
    pub fn compute_closure(&self, root_mod: &ModFile) -> ModuleResult<ModuleClosure> {
        let mut closure = ModuleClosure {
            root_module: root_mod.module.clone(),
            root_dir: self.project_root.clone(),
            modules: HashMap::new(),
        };

        // Track visited modules to detect cycles and avoid re-processing
        let mut visited: HashSet<String> = HashSet::new();

        // Process queue: (module_path, version, dependency_chain)
        let mut queue: Vec<(String, String, Vec<String>)> = Vec::new();

        // Initialize with root module's direct dependencies
        let root_chain = vec![root_mod.module.clone()];
        for req in &root_mod.requires {
            queue.push((req.module.clone(), req.version.clone(), root_chain.clone()));
        }

        while let Some((mod_path, version, chain)) = queue.pop() {
            // Check if already visited
            if visited.contains(&mod_path) {
                // Check for version conflict
                if let Some(existing) = closure.modules.get(&mod_path) {
                    if existing.version != version {
                        return Err(ModuleError::VersionConflict {
                            module: mod_path,
                            version1: existing.version.clone(),
                            chain1: existing.chain.clone(),
                            version2: version,
                            chain2: chain,
                        });
                    }
                }
                continue;
            }
            visited.insert(mod_path.clone());

            // Find the module in .vodeps
            let versioned_dir = self.find_module_dir(&mod_path, &version)?;

            // Parse the module's vo.mod
            let dep_mod_file_path = versioned_dir.join("vo.mod");
            let dep_mod = ModFile::parse_file(&dep_mod_file_path)?;

            // Add to closure
            closure.modules.insert(mod_path.clone(), ResolvedModule {
                path: mod_path.clone(),
                version: version.clone(),
                root: versioned_dir,
                chain: chain.clone(),
            });

            // Add transitive dependencies to queue
            let mut new_chain = chain.clone();
            new_chain.push(format!("{}@{}", mod_path, version));
            
            for req in &dep_mod.requires {
                if !visited.contains(&req.module) {
                    queue.push((req.module.clone(), req.version.clone(), new_chain.clone()));
                } else if let Some(existing) = closure.modules.get(&req.module) {
                    // Check for version conflict
                    if existing.version != req.version {
                        return Err(ModuleError::VersionConflict {
                            module: req.module.clone(),
                            version1: existing.version.clone(),
                            chain1: existing.chain.clone(),
                            version2: req.version.clone(),
                            chain2: new_chain.clone(),
                        });
                    }
                }
            }
        }

        Ok(closure)
    }

    /// Finds the directory for a module at a specific version.
    fn find_module_dir(&self, module: &str, version: &str) -> ModuleResult<PathBuf> {
        // Expected path: .vodeps/<module>@<version>/
        let dir_name = format!("{}@{}", module, version);
        let mod_dir = self.deps_dir().join(&dir_name);

        if !mod_dir.exists() {
            return Err(ModuleError::ModuleNotFound {
                module: module.to_string(),
                version: version.to_string(),
            });
        }

        Ok(mod_dir)
    }

    /// Resolves an import path to a filesystem location.
    ///
    /// Resolution order:
    /// 1. Standard library (if path starts with "std/")
    /// 2. Root module (if path starts with root module path)
    /// 3. Dependencies (longest prefix match)
    pub fn resolve_import(
        &self,
        import_path: &str,
        closure: &ModuleClosure,
    ) -> ModuleResult<ResolvedPackage> {
        // 1. Standard library
        if import_path.starts_with(STD_PREFIX) {
            return self.resolve_std_import(import_path);
        }

        // 2. Root module
        if closure.is_root_module_path(import_path) {
            let rest = if import_path == closure.root_module {
                ""
            } else {
                &import_path[closure.root_module.len() + 1..]
            };

            let pkg_dir = if rest.is_empty() {
                closure.root_dir.clone()
            } else {
                closure.root_dir.join(rest)
            };

            if !pkg_dir.exists() {
                return Err(ModuleError::PackageNotFound {
                    import_path: import_path.to_string(),
                    expected_dir: pkg_dir,
                });
            }

            return Ok(ResolvedPackage {
                import_path: import_path.to_string(),
                dir: pkg_dir,
                is_std: false,
            });
        }

        // 3. Dependencies
        if let Some(resolved) = closure.find_owning_module(import_path) {
            let rest = if import_path == resolved.path {
                ""
            } else {
                &import_path[resolved.path.len() + 1..]
            };

            let pkg_dir = if rest.is_empty() {
                resolved.root.clone()
            } else {
                resolved.root.join(rest)
            };

            if !pkg_dir.exists() {
                return Err(ModuleError::PackageNotFound {
                    import_path: import_path.to_string(),
                    expected_dir: pkg_dir,
                });
            }

            return Ok(ResolvedPackage {
                import_path: import_path.to_string(),
                dir: pkg_dir,
                is_std: false,
            });
        }

        Err(ModuleError::UnownedImportPath(import_path.to_string()))
    }

    /// Resolves a standard library import.
    fn resolve_std_import(&self, import_path: &str) -> ModuleResult<ResolvedPackage> {
        let std_root = self.std_root.as_ref().ok_or_else(|| {
            ModuleError::StdPackageNotFound(import_path.to_string())
        })?;

        // Remove "std/" prefix
        let pkg_path = &import_path[STD_PREFIX.len()..];
        let pkg_dir = std_root.join(pkg_path);

        if !pkg_dir.exists() {
            return Err(ModuleError::StdPackageNotFound(import_path.to_string()));
        }

        Ok(ResolvedPackage {
            import_path: import_path.to_string(),
            dir: pkg_dir,
            is_std: true,
        })
    }

    /// Checks if an import violates internal package rules.
    ///
    /// A package with `/internal/` in its path can only be imported by
    /// packages that share the path prefix before `/internal/`.
    pub fn check_internal_access(
        &self,
        importer_path: &str,
        import_path: &str,
    ) -> ModuleResult<()> {
        // Find /internal/ in the import path
        if let Some(idx) = import_path.find("/internal/") {
            let internal_parent = &import_path[..idx];
            
            // Importer must share this prefix
            if !importer_path.starts_with(internal_parent) {
                return Err(ModuleError::InternalPackageViolation {
                    importer: importer_path.to_string(),
                    internal_pkg: import_path.to_string(),
                });
            }
        } else if import_path.ends_with("/internal") {
            // Edge case: package is exactly "something/internal"
            let internal_parent = &import_path[..import_path.len() - "/internal".len()];
            if !importer_path.starts_with(internal_parent) {
                return Err(ModuleError::InternalPackageViolation {
                    importer: importer_path.to_string(),
                    internal_pkg: import_path.to_string(),
                });
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn setup_test_project() -> (TempDir, ModuleResolver) {
        let temp = TempDir::new().unwrap();
        let resolver = ModuleResolver::new(temp.path());
        
        // Create vo.mod
        fs::write(
            temp.path().join("vo.mod"),
            "module github.com/test/project\n",
        ).unwrap();

        // Create a local package
        fs::create_dir_all(temp.path().join("util")).unwrap();
        fs::write(temp.path().join("util/util.vo"), "package util\n").unwrap();

        (temp, resolver)
    }

    #[test]
    fn test_compute_closure_no_deps() {
        let (temp, resolver) = setup_test_project();
        
        let mod_file = ModFile::parse_file(temp.path().join("vo.mod")).unwrap();
        let closure = resolver.compute_closure(&mod_file).unwrap();

        assert_eq!(closure.root_module, "github.com/test/project");
        assert!(closure.modules.is_empty());
    }

    #[test]
    fn test_resolve_root_module_package() {
        let (temp, resolver) = setup_test_project();
        
        let mod_file = ModFile::parse_file(temp.path().join("vo.mod")).unwrap();
        let closure = resolver.compute_closure(&mod_file).unwrap();

        let resolved = resolver.resolve_import(
            "github.com/test/project/util",
            &closure,
        ).unwrap();

        assert_eq!(resolved.import_path, "github.com/test/project/util");
        assert_eq!(resolved.dir, temp.path().join("util"));
        assert!(!resolved.is_std);
    }

    #[test]
    fn test_resolve_unowned_path() {
        let (temp, resolver) = setup_test_project();
        
        let mod_file = ModFile::parse_file(temp.path().join("vo.mod")).unwrap();
        let closure = resolver.compute_closure(&mod_file).unwrap();

        let result = resolver.resolve_import(
            "github.com/unknown/pkg",
            &closure,
        );

        assert!(matches!(result, Err(ModuleError::UnownedImportPath(_))));
    }

    #[test]
    fn test_check_internal_access_allowed() {
        let resolver = ModuleResolver::new("/tmp");

        // Same module can access internal
        resolver.check_internal_access(
            "github.com/foo/bar/cmd",
            "github.com/foo/bar/internal/secret",
        ).unwrap();

        // Subpackage can access internal
        resolver.check_internal_access(
            "github.com/foo/bar/cmd/tool",
            "github.com/foo/bar/internal/secret",
        ).unwrap();
    }

    #[test]
    fn test_check_internal_access_denied() {
        let resolver = ModuleResolver::new("/tmp");

        let result = resolver.check_internal_access(
            "github.com/other/project",
            "github.com/foo/bar/internal/secret",
        );

        assert!(matches!(result, Err(ModuleError::InternalPackageViolation { .. })));
    }

    #[test]
    fn test_is_root_module_path() {
        let closure = ModuleClosure {
            root_module: "github.com/test/project".to_string(),
            root_dir: PathBuf::from("/tmp"),
            modules: HashMap::new(),
        };

        assert!(closure.is_root_module_path("github.com/test/project"));
        assert!(closure.is_root_module_path("github.com/test/project/util"));
        assert!(closure.is_root_module_path("github.com/test/project/a/b/c"));
        
        assert!(!closure.is_root_module_path("github.com/test/projectx"));
        assert!(!closure.is_root_module_path("github.com/other/project"));
    }
}
