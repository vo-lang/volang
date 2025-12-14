//! Multi-package project analysis.
//!
//! Handles:
//! - Cross-package imports with "./path" syntax
//! - Dependency graph and topological sort for init order
//! - Package exports (capitalized names)
//! - Package init: implicit (var decls) + explicit (init funcs)

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use gox_common::vfs::FileSet;
use gox_common::{DiagnosticSink, FileId, Symbol, SymbolInterner};
use gox_module::vfs::Vfs;
use gox_syntax::{parse, parse_with_interner, ast};
use gox_syntax::ast::Decl;

use crate::types::Type;
use crate::scope::Entity;
use crate::{typecheck_files, typecheck_files_with_imports, TypeCheckResult};

/// Import declaration with resolved package reference.
#[derive(Debug, Clone)]
pub struct ResolvedImport {
    /// The import path as written (e.g., "./math")
    pub path: String,
    /// The resolved package name (e.g., "math")
    pub package_name: String,
    /// Optional alias (e.g., `import m "./math"` -> alias = "m")
    pub alias: Option<String>,
}

/// Import path classification.
#[derive(Debug, Clone)]
pub enum ImportPath {
    /// "./mylib" - relative to current package directory
    Local(PathBuf),
    /// "fmt", "os" - standard library package
    Stdlib(String),
    /// "github.com/user/pkg" - external module (not yet supported)
    External(String),
}

impl ImportPath {
    pub fn parse(import_str: &str) -> Self {
        if import_str.starts_with("./") || import_str.starts_with("../") {
            ImportPath::Local(PathBuf::from(import_str))
        } else if Self::is_stdlib(import_str) {
            ImportPath::Stdlib(import_str.to_string())
        } else {
            ImportPath::External(import_str.to_string())
        }
    }
    
    /// Check if import path is a standard library package.
    /// Go convention: stdlib has no dots (domain names like github.com have dots).
    /// Examples: "fmt", "os", "encoding/json" are stdlib
    ///           "github.com/user/pkg" is external
    fn is_stdlib(path: &str) -> bool {
        !path.contains('.')
    }
}

/// A parsed package (before type checking).
#[derive(Debug)]
pub struct ParsedPackage {
    pub name: String,
    pub dir: PathBuf,
    pub files: Vec<(PathBuf, ast::File)>,
    pub interner: SymbolInterner,
    pub imports: Vec<ResolvedImport>,
    /// Has init() function(s)
    pub has_init: bool,
    /// Has package-level var declarations (implicit init)
    pub has_var_decls: bool,
}

/// Exported symbol from a package.
#[derive(Debug, Clone)]
pub struct ExportedSymbol {
    pub name: String,
    pub ty: Type,
    pub kind: ExportKind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExportKind {
    Func,
    Var,
    Const,
    Type,
}

/// A type-checked package.
#[derive(Debug)]
pub struct TypedPackage {
    pub name: String,
    pub dir: PathBuf,
    pub files: Vec<ast::File>,
    pub interner: SymbolInterner,
    pub types: TypeCheckResult,
    /// Exported symbols (capitalized names)
    pub exports: HashMap<String, ExportedSymbol>,
    /// Resolved imports for this package
    pub imports: Vec<ResolvedImport>,
    /// Init order index (0 = first to init)
    pub init_order: usize,
    /// Has init() function(s)
    pub has_init: bool,
    /// Has package-level var declarations
    pub has_var_decls: bool,
}

/// A complete project with all packages analyzed.
#[derive(Debug)]
pub struct Project {
    /// Packages in init order (dependencies first)
    pub packages: Vec<TypedPackage>,
    /// Package name -> index in packages vec
    pub package_index: HashMap<String, usize>,
    /// Symbol interner shared across all packages
    pub interner: SymbolInterner,
    /// Main package name
    pub main_package: String,
}

/// Project analysis error.
#[derive(Debug)]
pub enum ProjectError {
    /// IO error
    Io(std::io::Error),
    /// Parse error
    Parse(String),
    /// Circular dependency detected
    CircularDependency(Vec<String>),
    /// External imports not supported
    ExternalImport(String),
    /// Package not found
    PackageNotFound(String),
    /// Type check error
    TypeCheck(String),
    /// No main package found
    NoMainPackage,
}

impl std::fmt::Display for ProjectError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProjectError::Io(e) => write!(f, "IO error: {}", e),
            ProjectError::Parse(e) => write!(f, "Parse error: {}", e),
            ProjectError::CircularDependency(pkgs) => {
                write!(f, "Circular dependency: {}", pkgs.join(" -> "))
            }
            ProjectError::ExternalImport(path) => {
                write!(f, "External imports not yet supported: {}", path)
            }
            ProjectError::PackageNotFound(name) => write!(f, "Package not found: {}", name),
            ProjectError::TypeCheck(e) => write!(f, "Type check error: {}", e),
            ProjectError::NoMainPackage => write!(f, "No main package found"),
        }
    }
}

impl std::error::Error for ProjectError {}

impl From<std::io::Error> for ProjectError {
    fn from(e: std::io::Error) -> Self {
        ProjectError::Io(e)
    }
}

/// Analyze a complete project from a file set.
/// 
/// The VFS is used to resolve imports (std, local, external modules).
pub fn analyze_project(file_set: FileSet, vfs: &Vfs) -> Result<Project, ProjectError> {
    let mut interner = SymbolInterner::new();
    
    // Step 1: Parse all files and group by package (by directory)
    let mut parsed_packages = parse_packages(&file_set, &mut interner)?;
    
    // Step 2: Resolve imports and load stdlib packages
    resolve_imports(&mut parsed_packages)?;
    load_stdlib_packages(&mut parsed_packages, vfs, &mut interner)?;
    
    // Step 3: Build dependency graph and topologically sort for init order
    let sorted_names = topological_sort(&parsed_packages)?;
    
    // Step 4: Type check packages in dependency order
    let mut typed_packages = Vec::new();
    let mut package_exports: HashMap<String, HashMap<String, ExportedSymbol>> = HashMap::new();
    
    for (init_order, pkg_name) in sorted_names.iter().enumerate() {
        let parsed = parsed_packages.remove(pkg_name).unwrap();
        let typed = typecheck_package(parsed, &package_exports, &interner, init_order)?;
        package_exports.insert(pkg_name.clone(), typed.exports.clone());
        typed_packages.push(typed);
    }
    
    // Build package index
    let package_index: HashMap<String, usize> = typed_packages
        .iter()
        .enumerate()
        .map(|(i, p)| (p.name.clone(), i))
        .collect();
    
    // Find main package
    let main_package = typed_packages
        .iter()
        .find(|p| p.name == "main")
        .map(|p| p.name.clone())
        .ok_or(ProjectError::NoMainPackage)?;
    
    Ok(Project {
        packages: typed_packages,
        package_index,
        interner,
        main_package,
    })
}

/// Parse all files and group them by package.
fn parse_packages(
    file_set: &FileSet,
    _interner: &mut SymbolInterner,
) -> Result<HashMap<String, ParsedPackage>, ProjectError> {
    // First, group files by directory
    let mut files_by_dir: HashMap<PathBuf, Vec<(PathBuf, String)>> = HashMap::new();
    for (path, content) in &file_set.files {
        let pkg_dir = path.parent().unwrap_or(Path::new("")).to_path_buf();
        files_by_dir.entry(pkg_dir).or_default().push((path.clone(), content.clone()));
    }
    
    // Parse each package with a shared interner
    let mut result = HashMap::new();
    let mut file_id_counter = 0u32;
    
    for (dir, files) in files_by_dir {
        if files.is_empty() {
            continue;
        }
        
        // Parse all files in this package with a shared interner
        let mut shared_interner = SymbolInterner::new();
        let mut parsed_files = Vec::new();
        let mut raw_imports = Vec::new();
        let mut has_init = false;
        let mut has_var_decls = false;
        
        for (path, content) in files {
            let file_id = FileId::new(file_id_counter);
            file_id_counter += 1;
            
            let (ast, _diag, interner) = parse_with_interner(file_id, &content, shared_interner);
            shared_interner = interner;
            
            // Extract imports (no alias support in current AST)
            for imp in &ast.imports {
                if let Some(raw) = shared_interner.resolve(imp.path.raw) {
                    let path_str = raw.trim_matches('"').to_string();
                    raw_imports.push((path_str, None));
                }
            }
            
            // Check for init() and var declarations
            for decl in &ast.decls {
                match decl {
                    Decl::Func(f) => {
                        if shared_interner.resolve(f.name.symbol) == Some("init") {
                            has_init = true;
                        }
                    }
                    Decl::Var(_) => {
                        has_var_decls = true;
                    }
                    _ => {}
                }
            }
            
            parsed_files.push((path, ast));
        }
        
        // Get package name
        let pkg_name = parsed_files[0].1.package.as_ref()
            .and_then(|p| shared_interner.resolve(p.symbol))
            .unwrap_or("main")
            .to_string();
        
        // Create unresolved imports (will be resolved later)
        let imports: Vec<ResolvedImport> = raw_imports.into_iter()
            .map(|(path, alias)| ResolvedImport {
                path: path.clone(),
                package_name: String::new(), // Will be resolved
                alias,
            })
            .collect();
        
        let parsed = ParsedPackage {
            name: pkg_name.clone(),
            dir: dir.clone(),
            files: parsed_files,
            interner: shared_interner,
            imports,
            has_init,
            has_var_decls,
        };
        
        result.insert(pkg_name, parsed);
    }
    
    Ok(result)
}

/// Resolve import paths to package names.
fn resolve_imports(packages: &mut HashMap<String, ParsedPackage>) -> Result<(), ProjectError> {
    // Build dir -> package name mapping
    let dir_to_pkg: HashMap<PathBuf, String> = packages.iter()
        .map(|(name, pkg)| (pkg.dir.clone(), name.clone()))
        .collect();
    
    // Resolve each package's imports
    let pkg_dirs: Vec<(String, PathBuf)> = packages.iter()
        .map(|(name, pkg)| (name.clone(), pkg.dir.clone()))
        .collect();
    
    for (pkg_name, pkg_dir) in pkg_dirs {
        let pkg = packages.get_mut(&pkg_name).unwrap();
        
        for import in &mut pkg.imports {
            let import_path = ImportPath::parse(&import.path);
            
            match import_path {
                ImportPath::Local(rel_path) => {
                    // Resolve relative to package directory
                    let target_dir = pkg_dir.join(&rel_path);
                    let target_dir = normalize_path(&target_dir);
                    
                    // Find matching package
                    if let Some(target_pkg) = dir_to_pkg.get(&target_dir) {
                        import.package_name = target_pkg.clone();
                    } else {
                        // Try matching by path suffix
                        for (dir, name) in &dir_to_pkg {
                            if dir.ends_with(&rel_path) {
                                import.package_name = name.clone();
                                break;
                            }
                        }
                        
                        if import.package_name.is_empty() {
                            return Err(ProjectError::PackageNotFound(import.path.clone()));
                        }
                    }
                }
                ImportPath::Stdlib(pkg_name) => {
                    // Standard library: use package name directly
                    // The actual implementation is via native functions
                    import.package_name = pkg_name;
                }
                ImportPath::External(path) => {
                    return Err(ProjectError::ExternalImport(path));
                }
            }
        }
    }
    
    Ok(())
}

/// Load stdlib packages referenced by imports.
fn load_stdlib_packages(
    packages: &mut HashMap<String, ParsedPackage>,
    vfs: &Vfs,
    _interner: &mut SymbolInterner,
) -> Result<(), ProjectError> {
    // Collect all stdlib imports
    let mut stdlib_imports: HashSet<String> = HashSet::new();
    for pkg in packages.values() {
        for import in &pkg.imports {
            if let ImportPath::Stdlib(_) = ImportPath::parse(&import.path) {
                stdlib_imports.insert(import.package_name.clone());
            }
        }
    }
    
    // Load each stdlib package through VFS
    for pkg_name in stdlib_imports {
        if packages.contains_key(&pkg_name) {
            continue; // Already loaded
        }
        
        if let Some(vfs_pkg) = vfs.resolve(&pkg_name) {
            let mut pkg_interner = SymbolInterner::new();
            let mut files = Vec::new();
            let file_id = FileId::new(0); // Dummy file ID for stdlib
            
            for vfs_file in &vfs_pkg.files {
                let content = &vfs_file.content;
                let (ast, _diag, new_interner) = parse_with_interner(file_id, content, pkg_interner);
                pkg_interner = new_interner;
                files.push((vfs_file.path.clone(), ast));
            }
            
            if !files.is_empty() {
                let parsed = ParsedPackage {
                    name: pkg_name.clone(),
                    dir: PathBuf::from(format!("std/{}", pkg_name)),
                    files,
                    interner: pkg_interner,
                    imports: Vec::new(),
                    has_init: false,
                    has_var_decls: false,
                };
                packages.insert(pkg_name, parsed);
            }
        }
        // If VFS can't resolve, that's OK - natives don't need source files
    }
    
    Ok(())
}

/// Normalize a path by resolving . and ..
fn normalize_path(path: &Path) -> PathBuf {
    let mut result = PathBuf::new();
    for component in path.components() {
        match component {
            std::path::Component::ParentDir => { result.pop(); }
            std::path::Component::CurDir => {}
            c => result.push(c),
        }
    }
    result
}

/// Topologically sort packages by dependencies (for init order).
/// Dependencies come first in the result.
fn topological_sort(packages: &HashMap<String, ParsedPackage>) -> Result<Vec<String>, ProjectError> {
    let mut in_degree: HashMap<String, usize> = HashMap::new();
    let mut dependents: HashMap<String, Vec<String>> = HashMap::new();
    
    // Initialize
    for name in packages.keys() {
        in_degree.insert(name.clone(), 0);
        dependents.insert(name.clone(), Vec::new());
    }
    
    // Build dependency graph using resolved imports
    for (name, pkg) in packages {
        for import in &pkg.imports {
            if !import.package_name.is_empty() {
                // Skip stdlib imports - they don't exist as real packages
                if !packages.contains_key(&import.package_name) {
                    continue;
                }
                // This package depends on import.package_name
                *in_degree.get_mut(name).unwrap() += 1;
                if let Some(deps) = dependents.get_mut(&import.package_name) {
                    deps.push(name.clone());
                }
            }
        }
    }
    
    // Kahn's algorithm - packages with no dependencies come first
    let mut queue: Vec<String> = in_degree.iter()
        .filter(|(_, &deg)| deg == 0)
        .map(|(name, _)| name.clone())
        .collect();
    
    // Sort queue for deterministic ordering
    queue.sort();
    
    let mut sorted = Vec::new();
    
    while let Some(name) = queue.pop() {
        sorted.push(name.clone());
        
        let deps = dependents.get(&name).cloned().unwrap_or_default();
        for dependent in deps {
            let deg = in_degree.get_mut(&dependent).unwrap();
            *deg -= 1;
            if *deg == 0 {
                // Insert in sorted position for determinism
                let pos = queue.binary_search(&dependent).unwrap_or_else(|e| e);
                queue.insert(pos, dependent);
            }
        }
    }
    
    if sorted.len() != packages.len() {
        // Find cycle
        let remaining: Vec<String> = packages.keys()
            .filter(|k| !sorted.contains(k))
            .cloned()
            .collect();
        return Err(ProjectError::CircularDependency(remaining));
    }
    
    Ok(sorted)
}

/// Type check a single package and collect exports.
fn typecheck_package(
    parsed: ParsedPackage,
    package_exports: &HashMap<String, HashMap<String, ExportedSymbol>>,
    _interner: &SymbolInterner,
    init_order: usize,
) -> Result<TypedPackage, ProjectError> {
    if parsed.files.is_empty() {
        return Ok(TypedPackage {
            name: parsed.name,
            dir: parsed.dir,
            files: Vec::new(),
            interner: parsed.interner,
            types: TypeCheckResult::default(),
            exports: HashMap::new(),
            imports: parsed.imports,
            init_order,
            has_init: parsed.has_init,
            has_var_decls: parsed.has_var_decls,
        });
    }
    
    // Collect all ASTs
    let file_refs: Vec<&ast::File> = parsed.files.iter().map(|(_, ast)| ast).collect();
    
    // Type check all files together with the shared interner
    // Pass imported package exports for cross-package symbol resolution
    let mut diag = DiagnosticSink::new();
    let types = typecheck_files_with_imports(
        &file_refs, 
        &parsed.interner, 
        &mut diag,
        &parsed.imports,
        package_exports,
    );
    
    if diag.has_errors() {
        let first_path = &parsed.files[0].0;
        return Err(ProjectError::TypeCheck(format!(
            "Type errors in package {:?}", first_path.parent().unwrap_or(first_path.as_path())
        )));
    }
    
    // Collect exported symbols (capitalized names)
    let exports = collect_exports(&parsed.files, &parsed.interner, &types);
    
    let all_files: Vec<ast::File> = parsed.files
        .into_iter()
        .map(|(_, ast)| ast)
        .collect();
    
    Ok(TypedPackage {
        name: parsed.name,
        dir: parsed.dir,
        files: all_files,
        interner: parsed.interner,
        types,
        exports,
        imports: parsed.imports,
        init_order,
        has_init: parsed.has_init,
        has_var_decls: parsed.has_var_decls,
    })
}

/// Collect exported symbols (capitalized names) from a package.
fn collect_exports(
    files: &[(PathBuf, ast::File)],
    interner: &SymbolInterner,
    types: &TypeCheckResult,
) -> HashMap<String, ExportedSymbol> {
    let mut exports = HashMap::new();
    
    for (_, file) in files {
        for decl in &file.decls {
            match decl {
                Decl::Func(f) => {
                    if let Some(name) = interner.resolve(f.name.symbol) {
                        if is_exported(name) && name != "init" {
                            // Look up function type from scope
                            let ty = types.scope.lookup(f.name.symbol)
                                .and_then(|e| match e {
                                    Entity::Func(fe) => Some(Type::Func(fe.sig.clone())),
                                    _ => None,
                                })
                                .unwrap_or(Type::Invalid);
                            
                            exports.insert(name.to_string(), ExportedSymbol {
                                name: name.to_string(),
                                ty,
                                kind: ExportKind::Func,
                            });
                        }
                    }
                }
                Decl::Var(v) => {
                    for spec in &v.specs {
                        for ident in &spec.names {
                            if let Some(name) = interner.resolve(ident.symbol) {
                                if is_exported(name) {
                                    let ty = types.scope.lookup(ident.symbol)
                                        .and_then(|e| match e {
                                            Entity::Var(ve) => Some(ve.ty.clone()),
                                            _ => None,
                                        })
                                        .unwrap_or(Type::Invalid);
                                    
                                    exports.insert(name.to_string(), ExportedSymbol {
                                        name: name.to_string(),
                                        ty,
                                        kind: ExportKind::Var,
                                    });
                                }
                            }
                        }
                    }
                }
                Decl::Const(c) => {
                    for spec in &c.specs {
                        for ident in &spec.names {
                            if let Some(name) = interner.resolve(ident.symbol) {
                                if is_exported(name) {
                                    let ty = types.scope.lookup(ident.symbol)
                                        .and_then(|e| match e {
                                            Entity::Var(ve) => Some(ve.ty.clone()),
                                            _ => None,
                                        })
                                        .unwrap_or(Type::Invalid);
                                    
                                    exports.insert(name.to_string(), ExportedSymbol {
                                        name: name.to_string(),
                                        ty,
                                        kind: ExportKind::Const,
                                    });
                                }
                            }
                        }
                    }
                }
                Decl::Type(t) => {
                    if let Some(name) = interner.resolve(t.name.symbol) {
                        if is_exported(name) {
                            // For type exports, just use Invalid for now
                            // Full type info would require looking up named type
                            exports.insert(name.to_string(), ExportedSymbol {
                                name: name.to_string(),
                                ty: Type::Invalid,
                                kind: ExportKind::Type,
                            });
                        }
                    }
                }
                _ => {}
            }
        }
    }
    
    exports
}

/// Check if a name is exported (starts with uppercase).
fn is_exported(name: &str) -> bool {
    name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use gox_common::vfs::MemoryFs;
    
    #[test]
    fn test_single_package_project() {
        let fs = MemoryFs::new()
            .with_file("/project/main.gox", r#"
package main

func main() {
    println(42)
}
"#);
        
        let file_set = FileSet::collect(&fs, Path::new("/project")).unwrap();
        let vfs = Vfs::with_fs_roots(
            PathBuf::from("/std"),
            PathBuf::from("/project"),
            PathBuf::from("/mod"),
        );
        let project = analyze_project(file_set, &vfs).unwrap();
        
        assert_eq!(project.main_package, "main");
        assert_eq!(project.packages.len(), 1);
    }
}
