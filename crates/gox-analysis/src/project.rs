//! Multi-package project analysis.
//!
//! Handles:
//! - Cross-package imports with "./path" syntax
//! - Dependency graph and topological sort for init order
//! - Package exports (capitalized names)
//! - Package init: implicit (var decls) + explicit (init funcs)

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use gox_common::vfs::FileSet;
use gox_common::diagnostics::DiagnosticEmitter;
use gox_common::{DiagnosticSink, SourceMap, SymbolInterner};
use gox_module::vfs::Vfs;
use gox_syntax::{parse_with_interner, ast};
use gox_syntax::ast::Decl;

use crate::types::Type;
use crate::scope::Entity;
use crate::{typecheck_files_with_imports, TypeCheckResult};

/// Import declaration with resolved package reference.
#[derive(Debug, Clone)]
pub struct ResolvedImport {
    /// The import path as written (e.g., "./math" or "gin" for external)
    pub path: String,
    /// The resolved package name (e.g., "math")
    pub package_name: String,
    /// Optional alias (e.g., `import m "./math"` -> alias = "m")
    pub alias: Option<String>,
    /// Whether this is an external import (@"alias" syntax)
    pub is_external: bool,
}

/// Import path classification.
#[derive(Debug, Clone)]
pub enum ImportPath {
    /// "./mylib" - relative to current package directory
    Local(PathBuf),
    /// "fmt", "os" - standard library package
    Stdlib(String),
    /// External module via @"alias" syntax (resolved from gox.mod)
    External { alias: String, module: String, version: String },
}

impl ImportPath {
    /// Parse a standard import path (without @ marker).
    /// For external imports, use parse_external instead.
    pub fn parse(import_str: &str) -> Self {
        if import_str.starts_with("./") || import_str.starts_with("../") {
            ImportPath::Local(PathBuf::from(import_str))
        } else if Self::is_stdlib(import_str) {
            ImportPath::Stdlib(import_str.to_string())
        } else {
            // For non-external imports without ./ prefix, treat as local project path
            ImportPath::Local(PathBuf::from(import_str))
        }
    }
    
    /// Check if import path is a standard library package.
    /// Known stdlib packages from the spec.
    fn is_stdlib(path: &str) -> bool {
        // Check if it's a known stdlib package or under a stdlib parent
        let base = path.split('/').next().unwrap_or(path);
        matches!(base,
            "fmt" | "os" | "io" | "strings" | "strconv" | "math" | "time" |
            "sync" | "context" | "errors" | "log" | "sort" | "bytes" | "bufio" |
            "encoding" | "net" | "path" | "regexp" | "reflect" | "runtime" | "testing" |
            "unicode" | "rand" | "crypto" | "hash" | "compress" | "archive" | "image" |
            "html" | "text" | "database" | "debug" | "go" | "plugin" | "syscall" | "unsafe"
        )
    }
}

/// A parsed package (before type checking).
pub struct ParsedPackage {
    pub name: String,
    pub dir: PathBuf,
    pub files: Vec<(PathBuf, ast::File)>,
    pub imports: Vec<ResolvedImport>,
    /// init() function indices (file_idx, decl_idx) in source order
    pub init_funcs: Vec<(usize, usize)>,
    /// Has package-level var declarations (implicit init)
    pub has_var_decls: bool,
    /// Source map for error reporting
    pub source_map: SourceMap,
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

/// Exported type with its methods.
#[derive(Debug, Clone)]
pub struct ExportedType {
    pub name: String,
    pub methods: HashMap<String, Type>,
}

/// A type-checked package.
#[derive(Debug)]
pub struct TypedPackage {
    pub name: String,
    pub dir: PathBuf,
    pub files: Vec<ast::File>,
    pub types: TypeCheckResult,
    /// Exported symbols (capitalized names)
    pub exports: HashMap<String, ExportedSymbol>,
    /// Exported types with their methods
    pub exported_types: HashMap<String, ExportedType>,
    /// Resolved imports for this package
    pub imports: Vec<ResolvedImport>,
    /// Init order index (0 = first to init)
    pub init_order: usize,
    /// init() function indices (file_idx, decl_idx) in source order
    pub init_funcs: Vec<(usize, usize)>,
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
    let mut package_exported_types: HashMap<String, HashMap<String, ExportedType>> = HashMap::new();
    
    for (init_order, pkg_name) in sorted_names.iter().enumerate() {
        let parsed = parsed_packages.remove(pkg_name).unwrap();
        let typed = typecheck_package(parsed, &package_exports, &package_exported_types, &mut interner, init_order)?;
        package_exports.insert(pkg_name.clone(), typed.exports.clone());
        package_exported_types.insert(pkg_name.clone(), typed.exported_types.clone());
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
    shared_interner: &mut SymbolInterner,
) -> Result<HashMap<String, ParsedPackage>, ProjectError> {
    // First, group files by directory
    let mut files_by_dir: HashMap<PathBuf, Vec<(PathBuf, String)>> = HashMap::new();
    for (path, content) in &file_set.files {
        let pkg_dir = path.parent().unwrap_or(Path::new("")).to_path_buf();
        files_by_dir.entry(pkg_dir).or_default().push((path.clone(), content.clone()));
    }
    
    // Parse each package with a shared interner
    let mut result = HashMap::new();
    let mut base_offset = 0u32;
    
    for (dir, files) in files_by_dir {
        if files.is_empty() {
            continue;
        }
        
        // Use the shared interner across all packages
        let mut parsed_files = Vec::new();
        let mut raw_imports = Vec::new();
        let mut init_funcs = Vec::new();
        let mut has_var_decls = false;
        let mut source_map = SourceMap::new();
        
        // Sort files by filename for deterministic order (Go spec)
        let mut files = files;
        files.sort_by(|a, b| a.0.file_name().cmp(&b.0.file_name()));
        
        for (path, content) in files {
            // Add file to source map for error reporting
            let file_name = path.to_string_lossy().to_string();
            source_map.add_file(file_name, content.clone());
            
            // Take ownership temporarily, then put it back
            let interner_owned = std::mem::take(shared_interner);
            let (ast, diag, interner) = parse_with_interner(&content, base_offset, interner_owned);
            *shared_interner = interner;
            base_offset += content.len() as u32 + 1; // Add 1 for gap between files
            
            // Check for parse errors
            if diag.has_errors() {
                let emitter = DiagnosticEmitter::new(&source_map);
                let error_details = emitter.emit_all_to_string(&diag);
                return Err(ProjectError::Parse(error_details));
            }
            
            // Extract imports with kind and alias info from AST
            for imp in &ast.imports {
                if let Some(raw) = shared_interner.resolve(imp.path.raw) {
                    let path_str = raw.trim_matches('"').to_string();
                    let is_external = imp.kind == ast::ImportKind::External;
                    let alias = imp.alias.as_ref()
                        .and_then(|a| shared_interner.resolve(a.symbol))
                        .map(|s| s.to_string());
                    raw_imports.push((path_str, alias, is_external));
                }
            }
            
            let file_idx = parsed_files.len();
            
            // Check for init() and var declarations
            for (decl_idx, decl) in ast.decls.iter().enumerate() {
                match decl {
                    Decl::Func(f) => {
                        if shared_interner.resolve(f.name.symbol) == Some("init") {
                            init_funcs.push((file_idx, decl_idx));
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
            .and_then(|p| {
                let resolved = shared_interner.resolve(p.symbol);
                    resolved
            })
            .unwrap_or("main")
            .to_string();
        
        // Create unresolved imports (will be resolved later)
        let imports: Vec<ResolvedImport> = raw_imports.into_iter()
            .map(|(path, alias, is_external)| ResolvedImport {
                path: path.clone(),
                package_name: String::new(), // Will be resolved
                alias,
                is_external,
            })
            .collect();
        
        let parsed = ParsedPackage {
            name: pkg_name.clone(),
            dir: dir.clone(),
            files: parsed_files,
            imports,
            init_funcs,
            has_var_decls,
            source_map,
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
            // External imports (@"alias") are resolved via gox.mod lookup
            // The path contains the alias, resolution happens at build time
            if import.is_external {
                // For external imports, package_name will be set during
                // dependency resolution from gox.mod. For now, use the alias
                // as a placeholder package name.
                import.package_name = import.path.clone();
                continue;
            }
            
            // Standard imports: check stdlib first, then local
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
                ImportPath::Stdlib(pkg_path) => {
                    // Standard library: use last segment as package name
                    // e.g., "encoding/hex" -> "hex"
                    import.package_name = pkg_path.rsplit('/').next()
                        .unwrap_or(&pkg_path).to_string();
                }
                ImportPath::External { .. } => {
                    // This case won't happen for non-external imports
                    unreachable!()
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
    shared_interner: &mut SymbolInterner,
) -> Result<(), ProjectError> {
    // Collect all stdlib imports: (full_path, short_name)
    // e.g., ("encoding/hex", "hex")
    let mut stdlib_imports: HashMap<String, String> = HashMap::new();
    for pkg in packages.values() {
        for import in &pkg.imports {
            if let ImportPath::Stdlib(_) = ImportPath::parse(&import.path) {
                // path is full path for VFS, package_name is short name
                stdlib_imports.insert(import.path.clone(), import.package_name.clone());
            }
        }
    }
    
    // Load each stdlib package through VFS
    for (full_path, pkg_name) in stdlib_imports {
        if packages.contains_key(&pkg_name) {
            continue; // Already loaded
        }
        
        if let Some(vfs_pkg) = vfs.resolve(&full_path) {
            let mut files = Vec::new();
            let mut stdlib_base = 0u32;
            let mut source_map = SourceMap::new();
            
            // Use shared interner to ensure symbols are consistent across packages
            let mut current_interner = std::mem::take(shared_interner);
            
            for vfs_file in &vfs_pkg.files {
                let content = &vfs_file.content;
                let file_name = vfs_file.path.to_string_lossy().to_string();
                source_map.add_file(file_name.clone(), content.clone());
                let (ast, diag, new_interner) = parse_with_interner(content, stdlib_base, current_interner);
                current_interner = new_interner;
                stdlib_base += content.len() as u32 + 1;
                
                // Check for parse errors in stdlib
                if diag.has_errors() {
                    *shared_interner = current_interner;
                    let emitter = DiagnosticEmitter::new(&source_map);
                    let error_details = emitter.emit_all_to_string(&diag);
                    return Err(ProjectError::Parse(format!("in stdlib/{}: {}", pkg_name, error_details)));
                }
                
                files.push((vfs_file.path.clone(), ast));
            }
            
            // Put the interner back
            *shared_interner = current_interner;
            
            if !files.is_empty() {
                let parsed = ParsedPackage {
                    name: pkg_name.clone(),
                    dir: PathBuf::from(format!("stdlib/{}", pkg_name)),
                    files,
                    imports: Vec::new(),
                    init_funcs: Vec::new(),
                    has_var_decls: false,
                    source_map,
                };
                packages.insert(pkg_name, parsed);
            }
        }
        // If VFS can't resolve, that's OK - externs don't need source files
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
    package_exported_types: &HashMap<String, HashMap<String, ExportedType>>,
    interner: &mut SymbolInterner,
    init_order: usize,
) -> Result<TypedPackage, ProjectError> {
    if parsed.files.is_empty() {
        return Ok(TypedPackage {
            name: parsed.name,
            dir: parsed.dir,
            files: Vec::new(),
            types: TypeCheckResult::default(),
            exported_types: HashMap::new(),
            exports: HashMap::new(),
            imports: parsed.imports,
            init_order,
            init_funcs: parsed.init_funcs,
            has_var_decls: parsed.has_var_decls,
        });
    }
    
    // Collect all ASTs
    let file_refs: Vec<&ast::File> = parsed.files.iter().map(|(_, ast)| ast).collect();
    
    // Pre-intern all imported package names so they can be found during type checking
    // This is necessary because import paths are string literals, not identifiers
    for import in &parsed.imports {
        let local_name = import.alias.as_ref().unwrap_or(&import.package_name);
        if !local_name.is_empty() {
            interner.intern(local_name);
        }
    }
    
    // Type check all files together with the shared interner
    // Pass imported package exports for cross-package symbol resolution
    let mut diag = DiagnosticSink::new();
    let types = typecheck_files_with_imports(
        &file_refs, 
        interner, 
        &mut diag,
        &parsed.imports,
        package_exports,
        package_exported_types,
    );
    
    if diag.has_errors() {
        let emitter = DiagnosticEmitter::new(&parsed.source_map);
        let error_details = emitter.emit_all_to_string(&diag);
        return Err(ProjectError::TypeCheck(error_details));
    }
    
    // Collect exported symbols (capitalized names)
    let exports = collect_exports(&parsed.files, interner, &types);
    
    // Collect exported types with their methods
    let exported_types = collect_exported_types(&parsed.files, interner, &types);
    
    let all_files: Vec<ast::File> = parsed.files
        .into_iter()
        .map(|(_, ast)| ast)
        .collect();
    
    Ok(TypedPackage {
        name: parsed.name,
        dir: parsed.dir,
        files: all_files,
        types,
        exports,
        exported_types,
        imports: parsed.imports,
        init_order,
        init_funcs: parsed.init_funcs,
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
    
    for (_path, file) in files {
        for decl in &file.decls {
            match decl {
                Decl::Func(f) => {
                    // Skip methods (functions with receivers)
                    if f.receiver.is_some() {
                        continue;
                    }
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

/// Collect exported types with their methods from a package.
fn collect_exported_types(
    files: &[(PathBuf, ast::File)],
    interner: &SymbolInterner,
    types: &TypeCheckResult,
) -> HashMap<String, ExportedType> {
    let mut exported_types: HashMap<String, ExportedType> = HashMap::new();
    
    // First pass: collect exported type names
    for (_path, file) in files {
        for decl in &file.decls {
            if let Decl::Type(t) = decl {
                if let Some(name) = interner.resolve(t.name.symbol) {
                    if is_exported(name) {
                        exported_types.insert(name.to_string(), ExportedType {
                            name: name.to_string(),
                            methods: HashMap::new(),
                        });
                    }
                }
            }
        }
    }
    
    // Second pass: collect methods for exported types
    for (_path, file) in files {
        for decl in &file.decls {
            if let Decl::Func(f) = decl {
                // Only process methods (functions with receivers)
                if let Some(ref recv) = f.receiver {
                    // Get the receiver type name
                    let recv_type_name = interner.resolve(recv.ty.symbol).map(|s| s.to_string());
                    if let Some(type_name) = recv_type_name {
                        // Check if this is an exported type
                        if let Some(exported_type) = exported_types.get_mut(&type_name) {
                            // Get method name
                            if let Some(method_name) = interner.resolve(f.name.symbol) {
                                // Find method signature from named_types
                                let method_ty = types.named_types.iter()
                                    .find(|info| interner.resolve(info.name) == Some(&type_name))
                                    .and_then(|info| {
                                        info.methods.iter()
                                            .find(|m| m.name == f.name.symbol)
                                            .map(|m| Type::Func(m.sig.clone()))
                                    })
                                    .unwrap_or(Type::Invalid);
                                
                                exported_type.methods.insert(method_name.to_string(), method_ty);
                            }
                        }
                    }
                }
            }
        }
    }
    
    exported_types
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
