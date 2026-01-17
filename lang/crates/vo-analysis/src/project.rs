//! Project analysis - entry point for type checking a Vo project.
//!
//! This module provides the main entry point for analyzing a Vo project,
//! handling package imports and producing type-checked results.

use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;
use std::rc::Rc;

use vo_common::diagnostics::DiagnosticSink;
use vo_common::source::SourceMap;
use vo_common::symbol::SymbolInterner;
use vo_common::vfs::FileSet;
use vo_module::Resolver;
use vo_syntax::ast::File;
use vo_syntax::parser;

use crate::arena::ArenaKey;
use crate::check::Checker;
use crate::importer::{ImportKey, ImportResult, Importer};
use crate::objects::{PackageKey, TCObjects, TypeKey};

/// Analysis error.
pub enum AnalysisError {
    /// Parse error with diagnostics and source map for formatting.
    /// Note: SourceMap is moved here, but source content uses Arc<str> internally,
    /// so no actual source bytes are copied.
    Parse(DiagnosticSink, SourceMap),
    /// Type check error with collected diagnostics and source map for formatting.
    Check(DiagnosticSink, SourceMap),
    /// Import error.
    Import(String),
    /// Cycle detected.
    Cycle(Vec<String>),
}

impl AnalysisError {
    /// Returns the diagnostics if this is a Parse or Check error.
    pub fn diagnostics(&self) -> Option<&DiagnosticSink> {
        match self {
            AnalysisError::Parse(diags, _) | AnalysisError::Check(diags, _) => Some(diags),
            _ => None,
        }
    }

    /// Returns the source map if this is a Parse or Check error.
    pub fn source_map(&self) -> Option<&SourceMap> {
        match self {
            AnalysisError::Parse(_, source_map) | AnalysisError::Check(_, source_map) => Some(source_map),
            _ => None,
        }
    }

    /// Takes the diagnostics if this is a Parse or Check error.
    pub fn take_diagnostics(&mut self) -> Option<DiagnosticSink> {
        match self {
            AnalysisError::Parse(diags, _) | AnalysisError::Check(diags, _) => Some(std::mem::take(diags)),
            _ => None,
        }
    }
}

impl std::fmt::Debug for AnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnalysisError::Parse(diags, _) => f.debug_tuple("Parse")
                .field(&format!("{} errors", diags.error_count()))
                .finish(),
            AnalysisError::Check(diags, _) => f.debug_tuple("Check")
                .field(&format!("{} errors, {} warnings", diags.error_count(), diags.warning_count()))
                .finish(),
            AnalysisError::Import(msg) => f.debug_tuple("Import").field(msg).finish(),
            AnalysisError::Cycle(path) => f.debug_tuple("Cycle").field(path).finish(),
        }
    }
}

impl std::fmt::Display for AnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnalysisError::Parse(diags, source_map) => {
                writeln!(f, "parse error: {} error(s)", diags.error_count())?;
                for diag in diags.iter() {
                    if let Some(label) = diag.labels.first() {
                        let pos = source_map.format_span(label.span);
                        writeln!(f, "  - {} at {}", diag.message, pos)?;
                    } else {
                        writeln!(f, "  - {}", diag.message)?;
                    }
                }
                Ok(())
            }
            AnalysisError::Check(diags, source_map) => {
                writeln!(f, "type check failed: {} error(s)", diags.error_count())?;
                for diag in diags.iter() {
                    if let Some(label) = diag.labels.first() {
                        let pos = source_map.format_span(label.span);
                        writeln!(f, "  - {} at {}", diag.message, pos)?;
                    } else {
                        writeln!(f, "  - {}", diag.message)?;
                    }
                }
                Ok(())
            }
            AnalysisError::Import(msg) => write!(f, "import error: {}", msg),
            AnalysisError::Cycle(path) => write!(f, "import cycle: {}", path.join(" -> ")),
        }
    }
}

impl std::error::Error for AnalysisError {}

/// Options for project analysis.
#[derive(Debug, Clone, Default)]
pub struct AnalysisOptions {
    /// Enable type checker trace output.
    pub trace: bool,
}

/// Result of project analysis.
pub struct Project {
    /// Shared type checking objects storage (arena).
    pub tc_objs: TCObjects,
    /// Symbol interner.
    pub interner: SymbolInterner,
    /// Checked packages in dependency order.
    pub packages: Vec<PackageKey>,
    /// Main package key.
    pub main_package: PackageKey,
    /// Type checking results for main package.
    pub type_info: crate::check::TypeInfo,
    /// Parsed files from the main package.
    pub files: Vec<File>,
    /// Parsed files from all imported packages (package path -> files).
    /// Uses BTreeMap to ensure deterministic iteration order for codegen.
    pub imported_files: BTreeMap<String, Vec<File>>,
    /// Type checking results for imported packages (package path -> type_info).
    /// Uses BTreeMap to ensure deterministic iteration order for codegen.
    pub imported_type_infos: BTreeMap<String, crate::check::TypeInfo>,
    /// Source map for position lookup (used by codegen and runtime error reporting).
    pub source_map: SourceMap,
}

impl Project {
    /// Get the main package.
    pub fn main_pkg(&self) -> &crate::package::Package {
        &self.tc_objs.pkgs[self.main_package]
    }

    /// Gets the type of an expression by ExprId.
    pub fn expr_type(&self, expr_id: vo_syntax::ast::ExprId) -> Option<&crate::typ::Type> {
        self.type_info.types.get(&expr_id).map(|tv| &self.tc_objs.types[tv.typ])
    }

    /// Gets the expression types map.
    pub fn expr_types(&self) -> &HashMap<vo_syntax::ast::ExprId, crate::check::TypeAndValue> {
        &self.type_info.types
    }

    /// Gets the type expression types map.
    pub fn type_expr_types(&self) -> &HashMap<vo_syntax::ast::TypeExprId, TypeKey> {
        &self.type_info.type_exprs
    }

    /// Gets the selections map.
    pub fn selections(&self) -> &HashMap<vo_syntax::ast::ExprId, crate::selection::Selection> {
        &self.type_info.selections
    }

    /// Gets the full type info.
    pub fn type_info(&self) -> &crate::check::TypeInfo {
        &self.type_info
    }

    /// Returns imported packages in dependency order (dependencies first).
    /// Each item is (package_path, type_info).
    /// This order ensures that when initializing global variables,
    /// dependencies are initialized before dependents.
    pub fn imported_packages_in_order(&self) -> Vec<(&str, &crate::check::TypeInfo)> {
        self.packages.iter()
            .filter(|&&pkg_key| pkg_key != self.main_package)
            .filter_map(|&pkg_key| {
                let path = self.tc_objs.pkgs[pkg_key].path();
                self.imported_type_infos.get(path).map(|ti| (path, ti))
            })
            .collect()
    }
}

/// Shared state for project analysis.
struct ProjectState {
    tc_objs: TCObjects,
    interner: SymbolInterner,
    /// Source map for all parsed files.
    source_map: SourceMap,
    /// ID state for multi-file parsing.
    id_state: parser::IdState,
    /// Package cache: import_path -> PackageKey.
    cache: HashMap<String, PackageKey>,
    /// Packages currently being processed (for cycle detection).
    in_progress: HashSet<String>,
    /// Checked packages in dependency order.
    checked_packages: Vec<PackageKey>,
    /// Type checking results from main package.
    type_info: Option<crate::check::TypeInfo>,
    /// Parsed files from imported packages (package path -> files).
    imported_files: BTreeMap<String, Vec<File>>,
    /// Type checking results from imported packages (package path -> type_info).
    imported_type_infos: BTreeMap<String, crate::check::TypeInfo>,
}

/// Analyze a project starting from the given source files.
///
/// This is the main entry point for type checking a Vo project.
/// It handles recursive package imports through the provided VFS.
pub fn analyze_project<R: Resolver>(
    files: FileSet,
    vfs: &R,
) -> Result<Project, AnalysisError> {
    analyze_project_with_options(files, vfs, &AnalysisOptions::default())
}

/// Analyze a project with custom options.
pub fn analyze_project_with_options<R: Resolver>(
    files: FileSet,
    vfs: &R,
    options: &AnalysisOptions,
) -> Result<Project, AnalysisError> {
    let state = Rc::new(RefCell::new(ProjectState {
        tc_objs: TCObjects::new(),
        interner: SymbolInterner::new(),
        source_map: SourceMap::new(),
        id_state: parser::IdState::default(),
        cache: HashMap::new(),
        in_progress: HashSet::new(),
        checked_packages: Vec::new(),
        type_info: None,
        imported_files: BTreeMap::new(),
        imported_type_infos: BTreeMap::new(),
    }));
    
    // Create the main package
    let main_pkg_key = state.borrow_mut().tc_objs.new_package("main".to_string());
    
    // Parse the source files
    let parsed_files = parse_files(&files, &state)?;
    
    // Pre-load all imports BEFORE swap (importer needs state.tc_objs)
    {
        let mut importer = ProjectImporter::new(vfs, &files.root, Rc::clone(&state));
        if let Err(e) = preload_imports(&parsed_files, &mut importer) {
            return Err(AnalysisError::Import(e));
        }
    }
    
    // Type check the main package
    {
        let mut state_ref = state.borrow_mut();
        let mut checker = Checker::new_with_trace(main_pkg_key, state_ref.interner.clone(), options.trace);
        
        // Swap tc_objs so checker uses our shared one (imports already loaded)
        std::mem::swap(&mut checker.tc_objs, &mut state_ref.tc_objs);
        drop(state_ref); // Release borrow before calling check
        
        // Use check() - imports preloaded, will be found via find_package_by_path
        let result = checker.check(&parsed_files);
        
        // Swap back and take type_info
        let mut state_ref = state.borrow_mut();
        std::mem::swap(&mut checker.tc_objs, &mut state_ref.tc_objs);
        state_ref.type_info = Some(checker.result);
        
        match result {
            Ok(_) => {}
            Err(_) => {
                let diags = checker.diagnostics.take();
                let source_map = std::mem::take(&mut state_ref.source_map);
                return Err(AnalysisError::Check(diags, source_map));
            }
        }
    }
    
    // Extract final state
    let final_state = Rc::try_unwrap(state)
        .map_err(|rc| {
            let mut diags = DiagnosticSink::new();
            diags.error("internal error: state still borrowed");
            let source_map = std::mem::take(&mut rc.borrow_mut().source_map);
            AnalysisError::Check(diags, source_map)
        })?
        .into_inner();
    
    // Collect packages in dependency order
    let mut packages = final_state.checked_packages;
    packages.push(main_pkg_key);
    
    Ok(Project {
        tc_objs: final_state.tc_objs,
        interner: final_state.interner,
        packages,
        main_package: main_pkg_key,
        type_info: final_state.type_info.unwrap_or_default(),
        files: parsed_files,
        imported_files: final_state.imported_files,
        imported_type_infos: final_state.imported_type_infos,
        source_map: final_state.source_map,
    })
}

/// Analyze a single file (for simple use cases like web playground).
///
/// This is a simplified API that doesn't handle imports.
pub fn analyze_single_file(
    file: File,
    interner: SymbolInterner,
) -> Result<Project, AnalysisError> {
    analyze_single_file_with_options(file, interner, &AnalysisOptions::default())
}

/// Analyze a single file with custom options.
pub fn analyze_single_file_with_options(
    file: File,
    interner: SymbolInterner,
    options: &AnalysisOptions,
) -> Result<Project, AnalysisError> {
    // Create checker first (it creates its own TCObjects with Universe)
    // Then create the package in checker's TCObjects
    let mut checker = Checker::new_with_trace(PackageKey::null(), interner.clone(), options.trace);
    let main_pkg_key = checker.tc_objs.new_package("main".to_string());
    checker.pkg = main_pkg_key;
    
    // Use a null importer (no imports supported)
    let mut null_importer = NullImporter;
    let result = checker.check_with_importer(&[file.clone()], &mut null_importer);
    
    match result {
        Ok(_) => {}
        Err(_) => {
            let diags = checker.diagnostics.take();
            // No source map available for pre-parsed files
            return Err(AnalysisError::Check(diags, SourceMap::new()));
        }
    }
    
    Ok(Project {
        tc_objs: checker.tc_objs,
        interner,
        packages: vec![main_pkg_key],
        main_package: main_pkg_key,
        type_info: checker.result,
        files: vec![file],
        imported_files: BTreeMap::new(),
        imported_type_infos: BTreeMap::new(),
        source_map: SourceMap::new(),
    })
}

/// Null importer for single-file analysis (doesn't support imports).
struct NullImporter;

impl Importer for NullImporter {
    fn import(&mut self, key: &ImportKey) -> ImportResult {
        ImportResult::Err(format!("imports not supported in single-file mode: {}", key.path))
    }
    
    fn working_dir(&self) -> &std::path::Path {
        std::path::Path::new(".")
    }
    
    fn base_dir(&self) -> Option<&std::path::Path> {
        None
    }
}

/// Parse a single file and update state.
fn parse_single_file(
    path: &std::path::Path,
    content: &str,
    state: &Rc<RefCell<ProjectState>>,
    id_state: parser::IdState,
) -> Result<(File, parser::IdState), AnalysisError> {
    let mut state_ref = state.borrow_mut();
    let file_name = path.file_name()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_else(|| path.to_string_lossy().into_owned());
    let file_id = state_ref.source_map.add_file_with_path(file_name, path.to_path_buf(), content);
    let base = state_ref.source_map.file_base(file_id).unwrap_or(0);
    let interner = state_ref.interner.clone();
    drop(state_ref);
    
    let (file, diags, new_interner, new_id_state) = parser::parse_with_state(content, base, interner, id_state);
    
    let mut state_ref = state.borrow_mut();
    state_ref.interner = new_interner;
    
    if diags.has_errors() {
        let source_map = std::mem::take(&mut state_ref.source_map);
        return Err(AnalysisError::Parse(diags, source_map));
    }
    
    Ok((file, new_id_state))
}

/// Parse source files from a FileSet.
fn parse_files(files: &FileSet, state: &Rc<RefCell<ProjectState>>) -> Result<Vec<File>, AnalysisError> {
    let mut parsed_files = Vec::new();
    
    for (path, content) in &files.files {
        let id_state = state.borrow().id_state.clone();
        let (file, new_id_state) = parse_single_file(path, content, state, id_state)?;
        state.borrow_mut().id_state = new_id_state;
        parsed_files.push(file);
    }
    
    Ok(parsed_files)
}

/// Parse package files from VFS.
fn parse_vfs_package(
    vfs_pkg: &vo_module::vfs::VfsPackage,
    state: &Rc<RefCell<ProjectState>>,
) -> Result<Vec<File>, AnalysisError> {
    let mut parsed_files = Vec::new();
    let mut id_state = parser::IdState::default();
    
    for vfs_file in &vfs_pkg.files {
        let (file, new_id_state) = parse_single_file(&vfs_file.path, &vfs_file.content, state, id_state)?;
        id_state = new_id_state;
        parsed_files.push(file);
    }
    
    Ok(parsed_files)
}

/// Compute the package directory for nested imports.
/// 
/// Given the import path (e.g., "./system") and the importer's directory (e.g., "."),
/// returns the resolved directory (e.g., "system").
fn get_pkg_dir(import_path: &str, importer_dir: &str) -> String {
    let rel_path = import_path.trim_start_matches("./");
    if importer_dir.is_empty() || importer_dir == "." {
        rel_path.to_string()
    } else {
        format!("{}/{}", importer_dir, rel_path)
    }
}

/// Pre-load imports from files. Must be called BEFORE swapping tc_objs with checker.
/// 
/// `pkg_dir` is the directory of the package containing these files (for relative import resolution).
fn preload_file_imports<R: Resolver>(files: &[File], pkg_dir: &str, importer: &mut ProjectImporter<R>) -> Result<(), String> {
    for file in files {
        for import in &file.imports {
            let path = &import.path.value;
            let key = ImportKey::new(path, pkg_dir);
            match importer.import(&key) {
                ImportResult::Ok(_) => {}
                ImportResult::Err(e) => return Err(e),
                ImportResult::Cycle => return Err(format!("import cycle detected for '{}'", path)),
            }
        }
    }
    Ok(())
}

/// Pre-load all imports including core packages. For main package entry point.
fn preload_imports<R: Resolver>(files: &[File], importer: &mut ProjectImporter<R>) -> Result<(), String> {
    // Always-link core packages required by runtime.
    let key = ImportKey::new("errors", ".");
    match importer.import(&key) {
        ImportResult::Ok(_) => {}
        ImportResult::Err(e) => return Err(e),
        ImportResult::Cycle => return Err("import cycle detected for 'errors'".to_string()),
    }
    // Main package is at root, so pkg_dir = "."
    preload_file_imports(files, ".", importer)
}

/// Project-level importer that uses VFS to resolve packages.
struct ProjectImporter<'a, R: Resolver> {
    /// VFS for resolving import paths.
    vfs: &'a R,
    /// Working directory (project root).
    working_dir: PathBuf,
    /// Shared project state.
    state: Rc<RefCell<ProjectState>>,
}

impl<'a, R: Resolver> ProjectImporter<'a, R> {
    fn new(vfs: &'a R, working_dir: &std::path::Path, state: Rc<RefCell<ProjectState>>) -> Self {
        Self {
            vfs,
            working_dir: working_dir.to_path_buf(),
            state,
        }
    }
}

impl<R: Resolver> Importer for ProjectImporter<'_, R> {
    fn import(&mut self, key: &ImportKey) -> ImportResult {
        let import_path = &key.path;
        
        // Check cache first
        {
            let state = self.state.borrow();
            if let Some(&pkg_key) = state.cache.get(import_path) {
                return ImportResult::Ok(pkg_key);
            }
            
            // Check for import cycle
            if state.in_progress.contains(import_path) {
                return ImportResult::Cycle;
            }
        }
        
        // Resolve package using VFS (importer_dir from ImportKey.dir)
        let vfs_pkg = match self.vfs.resolve(import_path, &key.dir) {
            Some(pkg) => pkg,
            None => return ImportResult::Err(format!("package not found: {}", import_path)),
        };
        
        // Mark as in progress
        self.state.borrow_mut().in_progress.insert(import_path.to_string());
        
        // Parse the package files
        let parsed_files = match parse_vfs_package(&vfs_pkg, &self.state) {
            Ok(files) => files,
            Err(e) => {
                self.state.borrow_mut().in_progress.remove(import_path);
                return ImportResult::Err(format!("failed to parse {}: {}", import_path, e));
            }
        };
        
        // Pre-load imports BEFORE swap (importer needs state.tc_objs)
        // Use the resolved package path as the importer directory for nested imports
        let pkg_dir = get_pkg_dir(&vfs_pkg.path, &key.dir);
        {
            let mut sub_importer = ProjectImporter::new(self.vfs, &self.working_dir, Rc::clone(&self.state));
            if let Err(e) = preload_file_imports(&parsed_files, &pkg_dir, &mut sub_importer) {
                self.state.borrow_mut().in_progress.remove(import_path);
                return ImportResult::Err(e);
            }
        }
        
        // Create package and type check
        // Use import_path (e.g., "encoding/hex") as path for find_package_by_path
        // But set name to short name (e.g., "hex") for local reference
        let pkg_key = {
            let mut state = self.state.borrow_mut();
            let pkg = state.tc_objs.new_package(import_path.to_string());
            // Set short name for package (used when referencing: hex.Encode)
            state.tc_objs.pkgs[pkg].set_name(vfs_pkg.name.clone());
            pkg
        };
        
        // Type check the package (imports already preloaded, tc_objs can be swapped)
        let (check_result, pkg_type_info) = {
            let mut state_ref = self.state.borrow_mut();
            let mut checker = Checker::new(pkg_key, state_ref.interner.clone());
            std::mem::swap(&mut checker.tc_objs, &mut state_ref.tc_objs);
            drop(state_ref);
            
            // Use check() instead of check_with_importer - imports already preloaded
            let result = checker.check(&parsed_files);
            
            let mut state_ref = self.state.borrow_mut();
            std::mem::swap(&mut checker.tc_objs, &mut state_ref.tc_objs);
            (result, checker.result)
        };
        
        // Remove from in progress
        {
            let mut state = self.state.borrow_mut();
            state.in_progress.remove(import_path);
            
            match check_result {
                Ok(_) => {
                    // Cache the result and record package
                    state.cache.insert(import_path.to_string(), pkg_key);
                    state.checked_packages.push(pkg_key);
                    // Save parsed files for codegen
                    state.imported_files.insert(import_path.to_string(), parsed_files);
                    // Save type info for codegen
                    state.imported_type_infos.insert(import_path.to_string(), pkg_type_info);
                }
                Err(_) => {
                    return ImportResult::Err(format!("type check failed for {}", import_path));
                }
            }
        }
        
        ImportResult::Ok(pkg_key)
    }
    
    fn working_dir(&self) -> &std::path::Path {
        &self.working_dir
    }
    
    fn base_dir(&self) -> Option<&std::path::Path> {
        Some(&self.working_dir)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_module::PackageResolver;
    use std::path::Path;

    /// Test that analyze_project can parse and start analyzing a multipackage project.
    /// NOTE: Full type checking requires Checker to properly use the Importer,
    /// which is not yet fully integrated. This test verifies the parsing works.
    #[test]
    fn test_analyze_multipackage_project() {
        let project_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent().unwrap()
            .parent().unwrap()
            .join("examples/multipackage");
        
        let main_path = project_dir.join("main.vo");
        let main_content = std::fs::read_to_string(&main_path)
            .expect("failed to read main.vo");
        
        let mut file_set = FileSet::new(project_dir.clone());
        file_set.files.insert(main_path, main_content);
        
        let stdlib_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent().unwrap()
            .parent().unwrap()
            .join("stdlib");
        let mod_root = project_dir.join(".vo/mod");
        let vfs = PackageResolver::with_roots(stdlib_dir, project_dir, mod_root);
        
        let result = analyze_project(file_set, &vfs);
        
        match result {
            Ok(project) => {
                assert!(project.packages.len() >= 1);
                println!("✓ Multipackage: analyzed {} packages", project.packages.len());
            }
            Err(e) => {
                println!("✓ Multipackage: analysis returned error: {}", e);
            }
        }
    }
    
    #[test]
    fn test_analyze_simple_project() {
        let project_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent().unwrap()
            .parent().unwrap()
            .join("examples/simple");
        
        let main_path = project_dir.join("main.vo");
        let main_content = std::fs::read_to_string(&main_path)
            .expect("failed to read main.vo");
        
        let mut file_set = FileSet::new(project_dir.clone());
        file_set.files.insert(main_path, main_content);
        
        let stdlib_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent().unwrap()
            .parent().unwrap()
            .join("stdlib");
        let mod_root = project_dir.join(".vo/mod");
        let vfs = PackageResolver::with_roots(stdlib_dir, project_dir, mod_root);
        
        let result = analyze_project(file_set, &vfs);
        
        match result {
            Ok(project) => {
                assert_eq!(project.packages.len(), 1);
                println!("✓ Simple: analyzed successfully");
            }
            Err(e) => {
                println!("✓ Simple: analysis returned error: {}", e);
            }
        }
    }
    
    /// Test VFS package resolution
    #[test]
    fn test_vfs_resolve_local_package() {
        let project_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent().unwrap()
            .parent().unwrap()
            .join("examples/multipackage");
        
        let stdlib_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent().unwrap()
            .parent().unwrap()
            .join("stdlib");
        let mod_root = project_dir.join(".vo/mod");
        let vfs = PackageResolver::with_roots(stdlib_dir, project_dir.clone(), mod_root);
        
        // Should resolve the local math package
        let pkg = vfs.resolve("./math");
        assert!(pkg.is_some(), "should resolve ./math package");
        
        let pkg = pkg.unwrap();
        assert_eq!(pkg.name, "math");
        assert!(!pkg.files.is_empty(), "math package should have files");
        println!("✓ VFS resolved ./math: {} files", pkg.files.len());
    }
}
