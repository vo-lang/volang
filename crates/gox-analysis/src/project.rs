//! Multi-package project analysis.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use gox_common::vfs::FileSet;
use gox_common::{DiagnosticSink, FileId, SymbolInterner};
use gox_syntax::ast;
use gox_syntax::parse;

use crate::types::Type;
use crate::{typecheck_file, TypeCheckResult};

/// Import path classification.
#[derive(Debug, Clone)]
pub enum ImportPath {
    /// "./mylib" - relative to current package directory
    Local(PathBuf),
    /// "github.com/user/pkg" - external module (not yet supported)
    External(String),
}

impl ImportPath {
    pub fn parse(import_str: &str) -> Self {
        if import_str.starts_with("./") || import_str.starts_with("../") {
            ImportPath::Local(PathBuf::from(import_str))
        } else {
            ImportPath::External(import_str.to_string())
        }
    }
}

/// A parsed package (before type checking).
#[derive(Debug)]
pub struct ParsedPackage {
    pub name: String,
    pub dir: PathBuf,
    pub files: Vec<(PathBuf, ast::File, SymbolInterner)>,
    pub imports: Vec<ImportPath>,
}

/// A type-checked package.
#[derive(Debug)]
pub struct TypedPackage {
    pub name: String,
    pub dir: PathBuf,
    pub files: Vec<(ast::File, SymbolInterner)>,
    pub types: TypeCheckResult,
    pub exports: HashMap<String, Type>,
}

/// A complete project with all packages analyzed.
#[derive(Debug)]
pub struct Project {
    /// Packages in dependency order (dependencies first)
    pub packages: Vec<TypedPackage>,
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
pub fn analyze_project(file_set: FileSet) -> Result<Project, ProjectError> {
    let mut interner = SymbolInterner::new();
    
    // Step 1: Parse all files and group by package
    let mut parsed_packages = parse_packages(&file_set, &mut interner)?;
    
    // Step 2: Build dependency graph and topologically sort
    let sorted_names = topological_sort(&parsed_packages)?;
    
    // Step 3: Type check packages in dependency order
    let mut typed_packages = Vec::new();
    let mut package_exports: HashMap<String, HashMap<String, Type>> = HashMap::new();
    
    for pkg_name in &sorted_names {
        let parsed = parsed_packages.remove(pkg_name).unwrap();
        let typed = typecheck_package(parsed, &package_exports, &interner)?;
        package_exports.insert(pkg_name.clone(), typed.exports.clone());
        typed_packages.push(typed);
    }
    
    // Find main package
    let main_package = typed_packages
        .iter()
        .find(|p| p.name == "main")
        .map(|p| p.name.clone())
        .ok_or(ProjectError::NoMainPackage)?;
    
    Ok(Project {
        packages: typed_packages,
        interner,
        main_package,
    })
}

/// Parse all files and group them by package.
fn parse_packages(
    file_set: &FileSet,
    _interner: &mut SymbolInterner,
) -> Result<HashMap<String, ParsedPackage>, ProjectError> {
    let mut packages: HashMap<PathBuf, Vec<(PathBuf, ast::File, SymbolInterner, Vec<ImportPath>)>> = HashMap::new();
    
    let mut file_id_counter = 0u32;
    
    for (path, content) in &file_set.files {
        let file_id = FileId::new(file_id_counter);
        file_id_counter += 1;
        
        let (ast, _diag, file_interner) = parse(file_id, content);
        
        // Extract imports
        let imports: Vec<ImportPath> = ast.imports.iter()
            .filter_map(|imp| {
                let raw = file_interner.resolve(imp.path.raw)?;
                let path_str = raw.trim_matches('"');
                Some(ImportPath::parse(path_str))
            })
            .collect();
        
        let pkg_dir = path.parent().unwrap_or(Path::new("")).to_path_buf();
        packages.entry(pkg_dir).or_default().push((path.clone(), ast, file_interner, imports));
    }
    
    // Convert to ParsedPackage
    let mut result = HashMap::new();
    for (dir, files) in packages {
        if files.is_empty() {
            continue;
        }
        
        // Get package name from first file's interner
        let pkg_name = files[0].1.package.as_ref()
            .and_then(|p| files[0].2.resolve(p.symbol))
            .unwrap_or("main")
            .to_string();
        
        let all_imports: Vec<ImportPath> = files.iter()
            .flat_map(|(_, _, _, imports)| imports.clone())
            .collect();
        
        let parsed = ParsedPackage {
            name: pkg_name.clone(),
            dir: dir.clone(),
            files: files.into_iter().map(|(p, ast, interner, _)| (p, ast, interner)).collect(),
            imports: all_imports,
        };
        
        result.insert(pkg_name, parsed);
    }
    
    Ok(result)
}

/// Topologically sort packages by dependencies.
fn topological_sort(packages: &HashMap<String, ParsedPackage>) -> Result<Vec<String>, ProjectError> {
    let mut in_degree: HashMap<String, usize> = HashMap::new();
    let mut dependents: HashMap<String, Vec<String>> = HashMap::new();
    
    // Initialize
    for name in packages.keys() {
        in_degree.insert(name.clone(), 0);
        dependents.insert(name.clone(), Vec::new());
    }
    
    // Build dependency graph
    for (name, pkg) in packages {
        for import in &pkg.imports {
            match import {
                ImportPath::Local(path) => {
                    // Find the package this import refers to
                    let import_dir = pkg.dir.join(path);
                    if let Some((dep_name, _)) = packages.iter()
                        .find(|(_, p)| p.dir == import_dir || p.dir.ends_with(path))
                    {
                        *in_degree.get_mut(name).unwrap() += 1;
                        dependents.get_mut(dep_name).unwrap().push(name.clone());
                    }
                }
                ImportPath::External(path) => {
                    return Err(ProjectError::ExternalImport(path.clone()));
                }
            }
        }
    }
    
    // Kahn's algorithm
    let mut queue: Vec<String> = in_degree.iter()
        .filter(|(_, &deg)| deg == 0)
        .map(|(name, _)| name.clone())
        .collect();
    
    let mut sorted = Vec::new();
    
    while let Some(name) = queue.pop() {
        sorted.push(name.clone());
        
        for dependent in dependents.get(&name).unwrap_or(&Vec::new()) {
            let deg = in_degree.get_mut(dependent).unwrap();
            *deg -= 1;
            if *deg == 0 {
                queue.push(dependent.clone());
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

/// Type check a single package.
fn typecheck_package(
    parsed: ParsedPackage,
    _package_exports: &HashMap<String, HashMap<String, Type>>,
    _interner: &SymbolInterner,
) -> Result<TypedPackage, ProjectError> {
    let mut all_files = Vec::new();
    let mut combined_types = TypeCheckResult::default();
    let exports = HashMap::new();
    
    for (path, ast, file_interner) in parsed.files {
        let mut diag = DiagnosticSink::new();
        let types = typecheck_file(&ast, &file_interner, &mut diag);
        
        if diag.has_errors() {
            return Err(ProjectError::TypeCheck(format!(
                "Type errors in {:?}", path
            )));
        }
        
        all_files.push((ast, file_interner));
        combined_types = types;
    }
    
    Ok(TypedPackage {
        name: parsed.name,
        dir: parsed.dir,
        files: all_files,
        types: combined_types,
        exports,
    })
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
        let project = analyze_project(file_set).unwrap();
        
        assert_eq!(project.main_package, "main");
        assert_eq!(project.packages.len(), 1);
    }
}
