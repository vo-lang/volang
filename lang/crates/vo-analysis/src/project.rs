//! Project analysis - entry point for type checking a Vo project.
//!
//! This module provides the main entry point for analyzing a Vo project,
//! handling package imports and producing type-checked results.

use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::vfs::{find_module_metadata_abs, Resolver, VfsPackage};
use vo_common::diagnostics::{DiagnosticEmitter, DiagnosticSink};
use vo_common::source::SourceMap;
use vo_common::symbol::SymbolInterner;
use vo_common::vfs::{
    normalize_fs_path, sort_fs_paths, FileSet, MAX_PACKAGE_SOURCE_BYTES, MAX_PACKAGE_SOURCE_FILES,
    MAX_TEXT_FILE_BYTES,
};
use vo_module::ext_manifest::ExtensionManifest;
use vo_module::identity::{self, LocalName};
use vo_syntax::ast::File;
use vo_syntax::parser;

use crate::check::Checker;
use crate::importer::{ImportKey, ImportResult, Importer};
use crate::objects::{PackageKey, TCObjects, TypeKey};

/// Borrowed metadata for one imported package in dependency order.
pub type ImportedPackageRef<'a> = (&'a str, PackageKey, &'a crate::check::TypeInfo, &'a [File]);

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
            AnalysisError::Parse(_, source_map) | AnalysisError::Check(_, source_map) => {
                Some(source_map)
            }
            _ => None,
        }
    }

    /// Takes the diagnostics if this is a Parse or Check error.
    pub fn take_diagnostics(&mut self) -> Option<DiagnosticSink> {
        match self {
            AnalysisError::Parse(diags, _) | AnalysisError::Check(diags, _) => {
                Some(std::mem::take(diags))
            }
            _ => None,
        }
    }
}

impl std::fmt::Debug for AnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnalysisError::Parse(diags, _) => f
                .debug_tuple("Parse")
                .field(&format!("{} errors", diags.error_count()))
                .finish(),
            AnalysisError::Check(diags, _) => f
                .debug_tuple("Check")
                .field(&format!(
                    "{} errors, {} warnings",
                    diags.error_count(),
                    diags.warning_count()
                ))
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

/// Canonical identity of the package supplied as the analysis root.
///
/// Project and in-memory frontends should pass this explicitly. Keeping the
/// source package identity separate from its declared short name is required
/// for `internal` visibility, unexported object identity, and stable runtime
/// type names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageIdentity {
    path: String,
    abi_path: String,
}

impl PackageIdentity {
    pub fn new(path: impl Into<String>) -> Result<Self, String> {
        let path = path.into();
        if path.starts_with(vo_module::identity::LOCAL_NAMESPACE_PREFIX) {
            LocalName::parse(&path).map_err(|error| error.to_string())?;
        } else {
            identity::classify_import(&path).map_err(|error| error.to_string())?;
        }
        let abi_path = vo_common::abi::package_abi_path(&path);
        Ok(Self { path, abi_path })
    }

    /// Identity used for a source set that deliberately has no module
    /// context. Frontends should pass this explicitly so an in-memory or
    /// ephemeral source set cannot inherit an unrelated host `vo.mod`.
    pub fn ad_hoc() -> Self {
        Self {
            path: "main".to_string(),
            abi_path: "main".to_string(),
        }
    }

    pub fn path(&self) -> &str {
        &self.path
    }

    pub fn abi_path(&self) -> &str {
        &self.abi_path
    }
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
    /// Native extension manifests discovered from imported packages.
    pub extensions: Vec<ExtensionManifest>,
}

impl Project {
    /// Get the main package.
    pub fn main_pkg(&self) -> &crate::package::Package {
        &self.tc_objs.pkgs[self.main_package]
    }

    /// Gets the type of an expression by ExprId.
    pub fn expr_type(&self, expr_id: vo_syntax::ast::ExprId) -> Option<&crate::typ::Type> {
        self.type_info
            .types
            .get(&expr_id)
            .map(|tv| &self.tc_objs.types[tv.typ])
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
    /// Each item contains the canonical path, package key, type information,
    /// and parsed files. Inconsistent project metadata is reported explicitly
    /// so downstream compilation cannot silently omit a package.
    /// This order ensures that when initializing global variables,
    /// dependencies are initialized before dependents.
    pub fn imported_packages_in_order(&self) -> Result<Vec<ImportedPackageRef<'_>>, String> {
        let mut packages = Vec::with_capacity(self.packages.len().saturating_sub(1));
        let mut seen = HashSet::new();
        for &package in &self.packages {
            if package == self.main_package {
                continue;
            }
            let path = self.tc_objs.pkgs[package].path();
            if !seen.insert(path) {
                return Err(format!(
                    "duplicate imported package in dependency order: {path}"
                ));
            }
            let type_info = self
                .imported_type_infos
                .get(path)
                .ok_or_else(|| format!("missing type information for imported package {path}"))?;
            let files = self
                .imported_files
                .get(path)
                .ok_or_else(|| format!("missing parsed files for imported package {path}"))?;
            packages.push((path, package, type_info, files.as_slice()));
        }

        if packages.len() != self.imported_type_infos.len()
            || packages.len() != self.imported_files.len()
        {
            return Err(format!(
                "imported package metadata cardinality mismatch: order={}, type_info={}, files={}",
                packages.len(),
                self.imported_type_infos.len(),
                self.imported_files.len()
            ));
        }
        Ok(packages)
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
    extensions: Vec<ExtensionManifest>,
}

/// Analyze a project starting from the given source files.
///
/// This is the main entry point for type checking a Vo project.
/// It handles recursive package imports through the provided VFS.
pub fn analyze_project<R: Resolver>(files: FileSet, vfs: &R) -> Result<Project, AnalysisError> {
    analyze_project_with_options(files, vfs, &AnalysisOptions::default())
}

/// Analyze a project with custom options.
pub fn analyze_project_with_options<R: Resolver>(
    files: FileSet,
    vfs: &R,
    options: &AnalysisOptions,
) -> Result<Project, AnalysisError> {
    let (identity, root_extensions) = current_package_context_from_root(&files.root)?;
    analyze_project_with_identity_and_options(files, vfs, identity, root_extensions, options)
}

/// Analyze a project using the canonical identity supplied by its module
/// frontend. This avoids deriving semantic identity from host filesystem
/// paths, which is unavailable for memory and archive-backed projects and is
/// incomplete for a package below the module root. This entry point also does
/// not probe `FileSet::root` for extension metadata; filesystem-aware
/// frontends must use the VFS analysis entry points so identity, sources, and
/// extension metadata come from one captured view.
pub fn analyze_project_with_identity<R: Resolver>(
    files: FileSet,
    vfs: &R,
    identity: PackageIdentity,
) -> Result<Project, AnalysisError> {
    analyze_project_with_identity_and_options(
        files,
        vfs,
        Some(identity),
        Vec::new(),
        &AnalysisOptions::default(),
    )
}

/// Analyze a project whose root extension metadata was obtained by the
/// frontend from the same filesystem view as the source files.
pub(crate) fn analyze_project_with_identity_and_extension<R: Resolver>(
    files: FileSet,
    vfs: &R,
    identity: PackageIdentity,
    extension: Option<ExtensionManifest>,
) -> Result<Project, AnalysisError> {
    analyze_project_with_identity_and_options(
        files,
        vfs,
        Some(identity),
        extension.into_iter().collect(),
        &AnalysisOptions::default(),
    )
}

fn analyze_project_with_identity_and_options<R: Resolver>(
    files: FileSet,
    vfs: &R,
    identity: Option<PackageIdentity>,
    root_extensions: Vec<ExtensionManifest>,
    options: &AnalysisOptions,
) -> Result<Project, AnalysisError> {
    validate_root_file_set(&files).map_err(AnalysisError::Import)?;
    let PackageIdentity {
        path: main_package_path,
        abi_path: main_package_abi_path,
    } = identity.unwrap_or_else(PackageIdentity::ad_hoc);
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
        extensions: Vec::new(),
    }));

    // Create the main package
    let main_pkg_key = state
        .borrow_mut()
        .tc_objs
        .new_package(main_package_path.clone(), main_package_abi_path);

    // Parse the source files
    let parsed_files = parse_files(&files, &state)?;

    for extension in root_extensions {
        record_extension(&state, extension).map_err(AnalysisError::Import)?;
    }

    // Pre-load all imports BEFORE swap (importer needs state.tc_objs)
    {
        // The root participates in cycle detection too. Without this marker a
        // canonical self-import could create a second Package object and
        // overwrite the root's path-cache entry.
        state
            .borrow_mut()
            .in_progress
            .insert(main_package_path.clone());
        let mut importer = ProjectImporter::new(
            vfs,
            &files.root,
            Some(main_package_path.clone()),
            Rc::clone(&state),
        );
        let preload_result = preload_imports(&parsed_files, &mut importer);
        state.borrow_mut().in_progress.remove(&main_package_path);
        if let Err(e) = preload_result {
            return Err(AnalysisError::Import(e));
        }
    }

    // Type check the main package
    {
        let mut state_ref = state.borrow_mut();
        let mut checker =
            Checker::new_with_trace(main_pkg_key, state_ref.interner.clone(), options.trace);

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
        extensions: final_state.extensions,
    })
}

/// Seal the public `FileSet` boundary before syntax processing. Files loaded
/// through a `FileSystem` already satisfy the same byte limits, but memory and
/// embedding frontends can construct a `FileSet` directly.
fn validate_root_file_set(files: &FileSet) -> Result<(), String> {
    if files.files.is_empty() {
        return Err("root package contains no Vo source files".to_string());
    }
    if files.files.len() > MAX_PACKAGE_SOURCE_FILES {
        return Err(format!(
            "root package contains {} source files, exceeding the {MAX_PACKAGE_SOURCE_FILES}-file limit",
            files.files.len()
        ));
    }

    let mut package_dir = None::<PathBuf>;
    let mut source_paths = vo_module::schema::PortablePathSet::default();
    let mut total_source_bytes = 0usize;
    for (path, content) in &files.files {
        let portable = vo_module::schema::portable_relative_path_from_path(path)
            .map_err(|error| format!("invalid root source path '{}': {error}", path.display()))?;
        if path.extension() != Some(std::ffi::OsStr::new("vo")) {
            return Err(format!(
                "root source '{}' must be a canonical .vo file",
                path.display()
            ));
        }
        if !source_paths
            .insert_file(&portable)
            .map_err(|error| format!("invalid root source path '{portable}': {error}"))?
        {
            return Err(format!("duplicate root source path '{portable}'"));
        }

        let parent = normalize_fs_path(path.parent().unwrap_or_else(|| Path::new(".")));
        if let Some(expected) = package_dir.as_ref() {
            if expected != &parent {
                return Err(format!(
                    "root source '{}' is outside package directory '{}'",
                    path.display(),
                    expected.display()
                ));
            }
        } else {
            package_dir = Some(parent);
        }

        if content.len() > MAX_TEXT_FILE_BYTES {
            return Err(format!(
                "root source '{}' exceeds the {MAX_TEXT_FILE_BYTES}-byte text-file limit",
                path.display()
            ));
        }
        total_source_bytes = total_source_bytes
            .checked_add(content.len())
            .ok_or_else(|| "root package source size overflow".to_string())?;
        if total_source_bytes > MAX_PACKAGE_SOURCE_BYTES {
            return Err(format!(
                "root package exceeds the {MAX_PACKAGE_SOURCE_BYTES}-byte source limit"
            ));
        }
    }
    Ok(())
}

fn record_extension(
    state: &Rc<RefCell<ProjectState>>,
    mut extension: ExtensionManifest,
) -> Result<(), String> {
    extension
        .validate()
        .map_err(|error| format!("invalid extension metadata: {error}"))?;
    let manifest_path = normalize_fs_path(&extension.manifest_path);
    extension.manifest_path = manifest_path.clone();
    let mut state = state.borrow_mut();
    if let Some(existing) = state
        .extensions
        .iter()
        .find(|existing| normalize_fs_path(&existing.manifest_path) == manifest_path)
    {
        if existing == &extension {
            return Ok(());
        }
        return Err(format!(
            "conflicting extension metadata was resolved from '{}'",
            manifest_path.display()
        ));
    }
    state.extensions.push(extension);
    Ok(())
}

/// Parse a single file and update state.
fn parse_single_file(
    path: &std::path::Path,
    content: &str,
    state: &Rc<RefCell<ProjectState>>,
    id_state: parser::IdState,
) -> Result<(File, parser::IdState), AnalysisError> {
    let mut state_ref = state.borrow_mut();
    let file_name = path
        .file_name()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_else(|| path.to_string_lossy().into_owned());
    let file_id = state_ref
        .source_map
        .try_add_file_with_path(file_name, path.to_path_buf(), content)
        .map_err(|error| {
            AnalysisError::Import(format!(
                "cannot add source file '{}': {error}",
                path.display()
            ))
        })?;
    let base = state_ref.source_map.file_base(file_id).unwrap_or(0);
    let interner = state_ref.interner.clone();
    drop(state_ref);

    let (file, diags, new_interner, new_id_state) =
        parser::parse_with_state(content, base, interner, id_state);

    let mut state_ref = state.borrow_mut();
    state_ref.interner = new_interner;

    if diags.has_errors() {
        let source_map = std::mem::take(&mut state_ref.source_map);
        return Err(AnalysisError::Parse(diags, source_map));
    }

    Ok((file, new_id_state))
}

/// Parse source files from a FileSet.
fn parse_files(
    files: &FileSet,
    state: &Rc<RefCell<ProjectState>>,
) -> Result<Vec<File>, AnalysisError> {
    let mut parsed_files = Vec::new();

    let mut paths: Vec<_> = files.files.keys().cloned().collect();
    sort_fs_paths(&mut paths);
    for path in paths {
        let content = &files.files[&path];
        let id_state = state.borrow().id_state.clone();
        let (file, new_id_state) = parse_single_file(&path, content, state, id_state)?;
        state.borrow_mut().id_state = new_id_state;
        parsed_files.push(file);
    }

    Ok(parsed_files)
}

/// Parse package files from VFS.
fn parse_vfs_package(
    vfs_pkg: &VfsPackage,
    state: &Rc<RefCell<ProjectState>>,
) -> Result<Vec<File>, AnalysisError> {
    let mut parsed_files = Vec::new();
    let mut id_state = parser::IdState::default();

    let mut files = vfs_pkg.files().iter().collect::<Vec<_>>();
    files.sort_by_cached_key(|file| {
        (
            normalize_fs_path(&file.path)
                .to_string_lossy()
                .replace('\\', "/"),
            file.path.clone(),
        )
    });
    for vfs_file in files {
        let (file, new_id_state) =
            parse_single_file(&vfs_file.path, &vfs_file.content, state, id_state)?;
        id_state = new_id_state;
        parsed_files.push(file);
    }

    Ok(parsed_files)
}

fn current_package_context_from_root(
    root: &Path,
) -> Result<(Option<PackageIdentity>, Vec<ExtensionManifest>), AnalysisError> {
    let root = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
    let Some(metadata) = find_module_metadata_abs(&root).map_err(AnalysisError::Import)? else {
        return Ok((None, Vec::new()));
    };
    let path = if metadata.sub_path.is_empty() {
        metadata.module_path.clone()
    } else {
        format!("{}/{}", metadata.module_path, metadata.sub_path)
    };
    let identity = PackageIdentity::new(path).map_err(AnalysisError::Import)?;
    Ok((Some(identity), metadata.extension.into_iter().collect()))
}

fn declared_package_name(files: &[File], interner: &SymbolInterner) -> Option<String> {
    for file in files {
        if let Some(package) = &file.package {
            if let Some(name) = interner.resolve(package.symbol) {
                return Some(name.to_string());
            }
        }
    }
    None
}

/// Pre-load imports from files. Must be called BEFORE swapping tc_objs with checker.
fn preload_file_imports<R: Resolver>(
    files: &[File],
    importer: &mut ProjectImporter<R>,
) -> Result<(), String> {
    for file in files {
        for import in &file.imports {
            let path = &import.path.value;
            let key = ImportKey::new(path);
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
fn preload_imports<R: Resolver>(
    files: &[File],
    importer: &mut ProjectImporter<R>,
) -> Result<(), String> {
    // Always-link core packages required by runtime.
    let key = ImportKey::new("errors");
    match importer.import(&key) {
        ImportResult::Ok(_) => {}
        ImportResult::Err(e) => return Err(e),
        ImportResult::Cycle => return Err("import cycle detected for 'errors'".to_string()),
    }
    preload_file_imports(files, importer)
}

/// Maximum dependency edges followed recursively by the project importer.
const MAX_IMPORT_DEPTH: usize = 128;

/// Project-level importer that uses VFS to resolve packages.
struct ProjectImporter<'a, R: Resolver> {
    /// VFS for resolving import paths.
    vfs: &'a R,
    /// Working directory (project root).
    working_dir: PathBuf,
    current_package_path: Option<String>,
    /// Number of dependency edges from the root package currently being loaded.
    depth: usize,
    /// Shared project state.
    state: Rc<RefCell<ProjectState>>,
}

impl<'a, R: Resolver> ProjectImporter<'a, R> {
    fn new(
        vfs: &'a R,
        working_dir: &std::path::Path,
        current_package_path: Option<String>,
        state: Rc<RefCell<ProjectState>>,
    ) -> Self {
        Self {
            vfs,
            working_dir: working_dir.to_path_buf(),
            current_package_path,
            depth: 0,
            state,
        }
    }
}

impl<R: Resolver> Importer for ProjectImporter<'_, R> {
    fn import(&mut self, key: &ImportKey) -> ImportResult {
        let import_path = &key.path;

        if let Err(e) = identity::classify_import(import_path) {
            return ImportResult::Err(format!("invalid import path \"{}\": {}", import_path, e));
        }
        if let Some(current_package_path) = self.current_package_path.as_deref() {
            if !identity::check_internal_visibility(current_package_path, import_path) {
                return ImportResult::Err(format!(
                    "use of internal package not allowed: {} cannot import {}",
                    current_package_path, import_path,
                ));
            }
        }

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

        let vfs_pkg = match self.vfs.resolve(import_path) {
            Ok(Some(pkg)) => pkg,
            Ok(None) => return ImportResult::Err(format!("package not found: {}", import_path)),
            Err(error) => {
                return ImportResult::Err(format!(
                    "failed to resolve package {}: {}",
                    import_path, error,
                ))
            }
        };

        if vfs_pkg.path() != import_path.as_str() {
            return ImportResult::Err(format!(
                "import path '{}' resolved to package '{}'; imports must use the canonical package path",
                import_path,
                vfs_pkg.path()
            ));
        }
        // Mark as in progress
        self.state
            .borrow_mut()
            .in_progress
            .insert(import_path.to_string());

        // Parse the package files
        let parsed_files = match parse_vfs_package(&vfs_pkg, &self.state) {
            Ok(files) => files,
            Err(e) => {
                self.state.borrow_mut().in_progress.remove(import_path);
                return ImportResult::Err(format!("failed to parse {}: {}", import_path, e));
            }
        };
        let package_name = {
            let state = self.state.borrow();
            declared_package_name(&parsed_files, &state.interner)
        };
        if package_name.as_deref() == Some("main") {
            self.state.borrow_mut().in_progress.remove(import_path);
            return ImportResult::Err(format!(
                "cannot import package {}: package clause is main",
                import_path,
            ));
        }

        if let Some(extension) = vfs_pkg.extension().cloned() {
            if let Err(error) = record_extension(&self.state, extension) {
                self.state.borrow_mut().in_progress.remove(import_path);
                return ImportResult::Err(error);
            }
        }

        // Pre-load imports BEFORE swap (importer needs state.tc_objs)
        {
            let Some(child_depth) = self.depth.checked_add(1) else {
                self.state.borrow_mut().in_progress.remove(import_path);
                return ImportResult::Err(format!(
                    "import graph depth overflow while loading '{}'",
                    import_path
                ));
            };
            if child_depth > MAX_IMPORT_DEPTH {
                self.state.borrow_mut().in_progress.remove(import_path);
                return ImportResult::Err(format!(
                    "import graph depth exceeds the supported limit of {MAX_IMPORT_DEPTH} while loading '{}'",
                    import_path
                ));
            }
            let mut sub_importer = ProjectImporter::new(
                self.vfs,
                &self.working_dir,
                Some(vfs_pkg.path().to_string()),
                Rc::clone(&self.state),
            );
            sub_importer.depth = child_depth;
            if let Err(e) = preload_file_imports(&parsed_files, &mut sub_importer) {
                self.state.borrow_mut().in_progress.remove(import_path);
                return ImportResult::Err(e);
            }
        }

        // Create package and type check.
        // Use vfs_pkg.path (canonical module path, possibly from vo.mod) so that the
        // typechecker Package.path() always holds the authoritative module path.
        // This guarantees extern lookup names in bytecode match what the Rust macro
        // registers, regardless of whether the import used a relative or full path.
        let pkg_key = {
            let mut state = self.state.borrow_mut();
            let pkg = state
                .tc_objs
                .new_package(vfs_pkg.path().to_string(), vfs_pkg.abi_path().to_string());
            // Set short name for package (used when referencing: hex.Encode)
            let fallback_name = vfs_pkg
                .path()
                .rsplit('/')
                .next()
                .unwrap_or(vfs_pkg.path())
                .to_string();
            state.tc_objs.pkgs[pkg].set_name(package_name.unwrap_or(fallback_name));
            pkg
        };

        // Type check the package (imports already preloaded, tc_objs can be swapped)
        let (check_result, pkg_type_info, check_diagnostics) = {
            let mut state_ref = self.state.borrow_mut();
            let mut checker = Checker::new(pkg_key, state_ref.interner.clone());
            std::mem::swap(&mut checker.tc_objs, &mut state_ref.tc_objs);
            drop(state_ref);

            // Use check() instead of check_with_importer - imports already preloaded
            let result = checker.check(&parsed_files);

            let mut state_ref = self.state.borrow_mut();
            let diagnostics = if result.is_err() && checker.diagnostics.borrow().has_errors() {
                let emitter = DiagnosticEmitter::new(&state_ref.source_map);
                Some(emitter.emit_all_to_string(&checker.diagnostics.borrow()))
            } else {
                None
            };
            std::mem::swap(&mut checker.tc_objs, &mut state_ref.tc_objs);
            (result, checker.result, diagnostics)
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
                    state
                        .imported_files
                        .insert(import_path.to_string(), parsed_files);
                    // Save type info for codegen
                    state
                        .imported_type_infos
                        .insert(import_path.to_string(), pkg_type_info);
                }
                Err(_) => {
                    if let Some(diag) = check_diagnostics {
                        let diag = diag.trim();
                        if !diag.is_empty() {
                            return ImportResult::Err(format!(
                                "type check failed for {}:\n{}",
                                import_path, diag
                            ));
                        }
                    }
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
    use crate::vfs::{ModSource, PackageResolver, StdSource};
    use std::path::PathBuf;
    use vo_common::vfs::{FileSet, MemoryFs};

    #[test]
    fn package_identity_rejects_noncanonical_paths_at_construction() {
        for path in [
            "github.com/Acme/app",
            "github.com/acme/app/../other",
            "github.com/acme/app/e\u{301}",
            "github.com/acme/app/CON",
            "local/demo/child",
        ] {
            assert!(
                PackageIdentity::new(path).is_err(),
                "invalid package identity {path:?} must be rejected"
            );
        }
        assert!(PackageIdentity::new("local/demo").is_ok());
        assert!(PackageIdentity::new("github.com/acme/app/\u{56fe}\u{5f62}/\u{00e9}").is_ok());
    }

    #[test]
    fn root_file_set_boundary_enforces_one_portable_bounded_package() {
        let empty = FileSet::new(PathBuf::from("."));
        assert!(validate_root_file_set(&empty)
            .unwrap_err()
            .contains("contains no Vo source files"));

        for invalid_path in ["../main.vo", "CON.vo", "main.txt"] {
            let mut files = FileSet::new(PathBuf::from("."));
            files
                .files
                .insert(PathBuf::from(invalid_path), "package main\n".to_string());
            assert!(
                validate_root_file_set(&files).is_err(),
                "invalid root source path {invalid_path:?} must be rejected"
            );
        }

        let mut mixed_directories = FileSet::new(PathBuf::from("."));
        mixed_directories
            .files
            .insert(PathBuf::from("app/main.vo"), "package main\n".to_string());
        mixed_directories
            .files
            .insert(PathBuf::from("lib/helper.vo"), "package main\n".to_string());
        assert!(validate_root_file_set(&mixed_directories)
            .unwrap_err()
            .contains("outside package directory"));

        let mut colliding_names = FileSet::new(PathBuf::from("."));
        colliding_names
            .files
            .insert(PathBuf::from("Main.vo"), "package main\n".to_string());
        colliding_names
            .files
            .insert(PathBuf::from("main.vo"), "package main\n".to_string());
        assert!(validate_root_file_set(&colliding_names).is_err());

        let mut unicode = FileSet::new(PathBuf::from("."));
        unicode.files.insert(
            PathBuf::from("src/\u{00e9}.vo"),
            "package main\n".to_string(),
        );
        unicode.files.insert(
            PathBuf::from("src/\u{56fe}\u{5f62}.vo"),
            "package main\n".to_string(),
        );
        validate_root_file_set(&unicode).unwrap();
    }

    #[test]
    fn explicit_identity_analysis_does_not_probe_live_module_metadata() {
        static NEXT_ID: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        let id = NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let root = std::env::temp_dir().join(format!(
            "vo-analysis-explicit-identity-{}-{id}",
            std::process::id()
        ));
        std::fs::create_dir_all(&root).unwrap();
        std::fs::write(root.join("vo.mod"), "this is not a module = \"manifest\"\n").unwrap();

        let mut files = FileSet::new(root.clone());
        files.files.insert(
            PathBuf::from("main.vo"),
            "package main\nfunc main() {}\n".to_string(),
        );
        let resolver = PackageResolver {
            std: StdSource::with_fs(
                MemoryFs::new().with_file("errors/errors.vo", "package errors\n"),
            ),
            r#mod: ModSource::with_fs(MemoryFs::new()),
        };
        let project = analyze_project_with_identity(
            files,
            &resolver,
            PackageIdentity::new("github.com/acme/app").unwrap(),
        )
        .unwrap();
        assert!(project.extensions.is_empty());

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn test_analyze_project_rejects_relative_imports() {
        let mut files = FileSet::new(PathBuf::from("."));
        files.files.insert(
            PathBuf::from("main.vo"),
            concat!("package main\n", "import \"./codec\"\n", "func main() {}\n",).to_string(),
        );

        let mut std_fs = MemoryFs::new();
        std_fs.add_file("errors/errors.vo", "package errors\n");
        let resolver = PackageResolver {
            std: StdSource::with_fs(std_fs),
            r#mod: ModSource::with_fs(MemoryFs::new()),
        };

        let result = analyze_project(files, &resolver);
        match result {
            Err(AnalysisError::Import(msg)) => {
                assert!(msg.contains("relative or absolute import paths are not allowed"));
            }
            _ => panic!("expected relative import rejection"),
        }
    }

    #[test]
    fn test_analyze_project_rejects_imported_main_package() {
        let mut files = FileSet::new(PathBuf::from("."));
        files.files.insert(
            PathBuf::from("main.vo"),
            concat!(
                "package main\n",
                "import \"github.com/acme/tool\"\n",
                "func main() {}\n",
            )
            .to_string(),
        );

        let std_fs = MemoryFs::new().with_file("errors/errors.vo", "package errors\n");
        let mod_fs = MemoryFs::new()
            .with_file(
                "github.com/acme/tool/vo.mod",
                "module = \"github.com/acme/tool\"\nvo = \"^0.1.0\"\n",
            )
            .with_file(
                "github.com/acme/tool/tool.vo",
                "package main\nfunc Run() {}\n",
            );
        let resolver = PackageResolver {
            std: StdSource::with_fs(std_fs),
            r#mod: ModSource::with_fs(mod_fs),
        };

        let result = analyze_project(files, &resolver);
        match result {
            Err(AnalysisError::Import(msg)) => {
                assert!(msg.contains("package clause is main"), "{msg}");
            }
            _ => panic!("expected imported main package rejection"),
        }
    }

    #[test]
    fn test_analyze_project_enforces_internal_visibility_in_import_pipeline() {
        let mut files = FileSet::new(PathBuf::from("."));
        files.files.insert(
            PathBuf::from("main.vo"),
            concat!(
                "package main\n",
                "import \"github.com/acme/lib\"\n",
                "func main() {}\n",
            )
            .to_string(),
        );

        let std_fs = MemoryFs::new().with_file("errors/errors.vo", "package errors\n");
        let mod_fs = MemoryFs::new()
            .with_file(
                "github.com/acme/lib/vo.mod",
                "module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n",
            )
            .with_file(
                "github.com/acme/lib/lib.vo",
                concat!(
                    "package lib\n",
                    "import \"github.com/acme/secret/internal/secret\"\n",
                ),
            )
            .with_file(
                "github.com/acme/secret/vo.mod",
                "module = \"github.com/acme/secret\"\nvo = \"^0.1.0\"\n",
            )
            .with_file(
                "github.com/acme/secret/internal/secret/secret.vo",
                "package secret\n",
            );
        let resolver = PackageResolver {
            std: StdSource::with_fs(std_fs),
            r#mod: ModSource::with_fs(mod_fs),
        };

        let result = analyze_project(files, &resolver);
        match result {
            Err(AnalysisError::Import(msg)) => {
                assert!(msg.contains("use of internal package not allowed"), "{msg}");
                assert!(msg.contains("github.com/acme/lib"), "{msg}");
                assert!(
                    msg.contains("github.com/acme/secret/internal/secret"),
                    "{msg}"
                );
            }
            _ => panic!("expected internal package rejection"),
        }
    }

    #[test]
    fn test_analyze_project_rejects_import_graphs_deeper_than_host_safe_limit() {
        let mut files = FileSet::new(PathBuf::from("."));
        files.files.insert(
            PathBuf::from("main.vo"),
            "package main\nimport \"p0\"\nfunc main() {}\n".to_string(),
        );

        let mut std_fs = MemoryFs::new().with_file("errors/errors.vo", "package errors\n");
        for depth in 0..=MAX_IMPORT_DEPTH {
            let source = if depth == MAX_IMPORT_DEPTH {
                format!("package p{depth}\n")
            } else {
                format!("package p{depth}\nimport \"p{}\"\n", depth + 1)
            };
            std_fs.add_file(format!("p{depth}/p{depth}.vo"), source);
        }
        let resolver = PackageResolver {
            std: StdSource::with_fs(std_fs),
            r#mod: ModSource::with_fs(MemoryFs::new()),
        };

        let result = analyze_project(files, &resolver);
        match result {
            Err(AnalysisError::Import(message)) => {
                assert!(
                    message.contains("import graph depth exceeds the supported limit of 128"),
                    "{message}"
                );
            }
            _ => panic!("expected deep import graph rejection"),
        }
    }

    #[test]
    fn explicit_root_identity_controls_internal_visibility_and_package_identity() {
        let mut files = FileSet::new(PathBuf::from("virtual-project"));
        files.files.insert(
            PathBuf::from("cmd/tool/main.vo"),
            concat!(
                "package main\n",
                "import \"github.com/acme/app/cmd/internal/secret\"\n",
                "var Seen = secret.Value\n",
                "func main() {}\n",
            )
            .to_string(),
        );

        let std_fs = MemoryFs::new().with_file("errors/errors.vo", "package errors\n");
        let mod_fs = MemoryFs::new()
            .with_file(
                "github.com/acme/app/vo.mod",
                "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
            )
            .with_file(
                "github.com/acme/app/cmd/internal/secret/secret.vo",
                "package secret\nconst Value = 7\n",
            );
        let resolver = PackageResolver {
            std: StdSource::with_fs(std_fs),
            r#mod: ModSource::with_fs(mod_fs),
        };
        let identity = PackageIdentity::new("github.com/acme/app/cmd/tool").unwrap();

        let project = analyze_project_with_identity(files, &resolver, identity).unwrap();
        assert_eq!(project.main_pkg().path(), "github.com/acme/app/cmd/tool");
        assert_eq!(
            project.main_pkg().abi_path(),
            "github.com/acme/app/cmd/tool"
        );
    }

    #[test]
    fn explicit_root_identity_rejects_foreign_internal_package() {
        let mut files = FileSet::new(PathBuf::from("virtual-project"));
        files.files.insert(
            PathBuf::from("main.vo"),
            concat!(
                "package main\n",
                "import \"github.com/acme/app/cmd/internal/secret\"\n",
                "var Seen = secret.Value\n",
                "func main() {}\n",
            )
            .to_string(),
        );

        let std_fs = MemoryFs::new().with_file("errors/errors.vo", "package errors\n");
        let mod_fs = MemoryFs::new()
            .with_file(
                "github.com/acme/app/vo.mod",
                "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
            )
            .with_file(
                "github.com/acme/app/cmd/internal/secret/secret.vo",
                "package secret\nconst Value = 7\n",
            );
        let resolver = PackageResolver {
            std: StdSource::with_fs(std_fs),
            r#mod: ModSource::with_fs(mod_fs),
        };

        let result = analyze_project_with_identity(
            files,
            &resolver,
            PackageIdentity::new("github.com/acme/other/tool").unwrap(),
        );
        match result {
            Err(AnalysisError::Import(message)) => {
                assert!(
                    message.contains("use of internal package not allowed"),
                    "{message}"
                );
                assert!(message.contains("github.com/acme/other/tool"), "{message}");
            }
            _ => panic!("expected root internal visibility rejection"),
        }
    }

    #[test]
    fn canonical_root_self_import_is_reported_as_a_cycle() {
        let mut files = FileSet::new(PathBuf::from("virtual-project"));
        files.files.insert(
            PathBuf::from("main.vo"),
            concat!(
                "package main\n",
                "import \"github.com/acme/app\"\n",
                "func main() {}\n",
            )
            .to_string(),
        );
        let resolver = PackageResolver {
            std: StdSource::with_fs(
                MemoryFs::new().with_file("errors/errors.vo", "package errors\n"),
            ),
            r#mod: ModSource::with_fs(MemoryFs::new()),
        };

        let result = analyze_project_with_identity(
            files,
            &resolver,
            PackageIdentity::new("github.com/acme/app").unwrap(),
        );
        match result {
            Err(AnalysisError::Import(message)) => {
                assert!(message.contains("import cycle detected"), "{message}");
                assert!(message.contains("github.com/acme/app"), "{message}");
            }
            _ => panic!("expected canonical self-import cycle"),
        }
    }

    #[test]
    fn checked_packages_are_recorded_once_in_dependency_first_order() {
        let mut files = FileSet::new(PathBuf::from("virtual-project"));
        files.files.insert(
            PathBuf::from("main.vo"),
            concat!(
                "package main\n",
                "import (\n",
                "  \"github.com/acme/graph/right\"\n",
                "  \"github.com/acme/graph/left\"\n",
                ")\n",
                "var Value = right.Value + left.Value\n",
                "func main() {}\n",
            )
            .to_string(),
        );
        let std_fs = MemoryFs::new().with_file("errors/errors.vo", "package errors\n");
        let mod_fs = MemoryFs::new()
            .with_file(
                "github.com/acme/graph/vo.mod",
                "module = \"github.com/acme/graph\"\nvo = \"^0.1.0\"\n",
            )
            .with_file(
                "github.com/acme/graph/shared/shared.vo",
                "package shared\nconst Value = 1\n",
            )
            .with_file(
                "github.com/acme/graph/right/right.vo",
                concat!(
                    "package right\n",
                    "import \"github.com/acme/graph/shared\"\n",
                    "const Value = shared.Value\n",
                ),
            )
            .with_file(
                "github.com/acme/graph/left/left.vo",
                concat!(
                    "package left\n",
                    "import \"github.com/acme/graph/shared\"\n",
                    "const Value = shared.Value\n",
                ),
            );
        let resolver = PackageResolver {
            std: StdSource::with_fs(std_fs),
            r#mod: ModSource::with_fs(mod_fs),
        };

        let mut project = analyze_project_with_identity(
            files,
            &resolver,
            PackageIdentity::new("github.com/acme/graph").unwrap(),
        )
        .unwrap();
        let paths: Vec<_> = project
            .packages
            .iter()
            .map(|&package| project.tc_objs.pkgs[package].path())
            .collect();
        assert_eq!(
            paths,
            vec![
                "errors",
                "github.com/acme/graph/shared",
                "github.com/acme/graph/right",
                "github.com/acme/graph/left",
                "github.com/acme/graph",
            ]
        );

        project
            .imported_type_infos
            .remove("github.com/acme/graph/shared");
        assert_eq!(
            project.imported_packages_in_order().unwrap_err(),
            "missing type information for imported package github.com/acme/graph/shared"
        );
    }
}
