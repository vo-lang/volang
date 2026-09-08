//! Project analysis - entry point for type checking a Vo project.
//!
//! This module provides the main entry point for analyzing a Vo project,
//! handling package imports and producing type-checked results.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::vfs::{find_module_metadata_abs, Resolver, VfsPackage};
use vo_common::diagnostics::DiagnosticSink;
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
        if let Some(local_path) = path.strip_prefix(vo_module::identity::LOCAL_NAMESPACE_PREFIX) {
            let module_name = local_path.split('/').next().unwrap_or_default();
            let local_module = format!(
                "{}{}",
                vo_module::identity::LOCAL_NAMESPACE_PREFIX,
                module_name
            );
            LocalName::parse(&local_module).map_err(|error| error.to_string())?;
            identity::classify_import(&path).map_err(|error| error.to_string())?;
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

/// One complete package, with source and semantic facts kept together.
pub struct AnalyzedPackage {
    pub key: PackageKey,
    pub files: Vec<File>,
    pub type_info: crate::check::TypeInfo,
}

/// Result of project analysis. Packages are frozen in dependency order and the
/// final package is the root; files and semantic facts cannot become unpaired.
pub struct Project {
    pub tc_objs: TCObjects,
    pub interner: SymbolInterner,
    packages: Vec<AnalyzedPackage>,
    package_indices: HashMap<PackageKey, usize>,
    pub source_map: SourceMap,
    pub diagnostics: DiagnosticSink,
    pub extensions: Vec<ExtensionManifest>,
}

impl Project {
    /// Assemble complete checked packages, dependencies first and root last.
    pub fn from_packages(
        tc_objs: TCObjects,
        interner: SymbolInterner,
        packages: Vec<AnalyzedPackage>,
        source_map: SourceMap,
        diagnostics: DiagnosticSink,
        extensions: Vec<ExtensionManifest>,
    ) -> Result<Self, String> {
        if packages.is_empty() {
            return Err("analysis contains no root package".into());
        }
        let mut package_indices = HashMap::new();
        let mut paths = HashSet::new();
        for (index, package) in packages.iter().enumerate() {
            let identity = tc_objs
                .pkgs
                .get(package.key)
                .ok_or("invalid analyzed package key")?;
            if package_indices.insert(package.key, index).is_some()
                || !paths.insert(identity.path())
            {
                return Err(format!("duplicate analyzed package {}", identity.path()));
            }
        }
        for (index, package) in packages.iter().enumerate() {
            for dependency in tc_objs.pkgs[package.key].imports() {
                if !package_indices
                    .get(dependency)
                    .is_some_and(|&position| position < index)
                {
                    return Err(format!(
                        "missing or out-of-order dependency of {}",
                        tc_objs.pkgs[package.key].path()
                    ));
                }
            }
        }
        Ok(Self {
            tc_objs,
            interner,
            packages,
            package_indices,
            source_map,
            diagnostics,
            extensions,
        })
    }

    pub fn packages(&self) -> &[AnalyzedPackage] {
        &self.packages
    }

    pub fn main(&self) -> &AnalyzedPackage {
        self.packages
            .last()
            .expect("Project construction requires a root package")
    }

    pub fn package(&self, key: PackageKey) -> Option<&AnalyzedPackage> {
        self.package_indices
            .get(&key)
            .map(|&index| &self.packages[index])
    }

    pub fn package_by_path(&self, path: &str) -> Option<&AnalyzedPackage> {
        self.tc_objs
            .find_package_by_path(path)
            .and_then(|key| self.package(key))
    }

    /// Get the main package.
    pub fn main_pkg(&self) -> &crate::package::Package {
        &self.tc_objs.pkgs[self.main().key]
    }

    /// Gets the type of an expression by ExprId.
    pub fn expr_type(&self, expr_id: vo_syntax::ast::ExprId) -> Option<&crate::typ::Type> {
        self.main()
            .type_info
            .types
            .get(&expr_id)
            .map(|tv| &self.tc_objs.types[tv.typ])
    }

    /// Gets the expression types map.
    pub fn expr_types(&self) -> &HashMap<vo_syntax::ast::ExprId, crate::check::TypeAndValue> {
        &self.main().type_info.types
    }

    /// Gets the type expression types map.
    pub fn type_expr_types(&self) -> &HashMap<vo_syntax::ast::TypeExprId, TypeKey> {
        &self.main().type_info.type_exprs
    }

    /// Gets the selections map.
    pub fn selections(&self) -> &HashMap<vo_syntax::ast::ExprId, crate::selection::Selection> {
        &self.main().type_info.selections
    }

    /// Gets the full type info.
    pub fn type_info(&self) -> &crate::check::TypeInfo {
        &self.main().type_info
    }

    /// Complete imported packages in their checked dependency order.
    pub fn imported_packages_in_order(&self) -> impl Iterator<Item = ImportedPackageRef<'_>> {
        self.packages[..self.packages.len() - 1]
            .iter()
            .map(|package| {
                (
                    self.tc_objs.pkgs[package.key].path(),
                    package.key,
                    &package.type_info,
                    package.files.as_slice(),
                )
            })
    }
}

/// Shared state for project analysis.
struct ProjectState {
    // Temporarily owned by Checker during a package check. Loading is complete
    // before that phase, so no importer can observe the arena in transit.
    tc_objs: Option<TCObjects>,
    interner: SymbolInterner,
    source_map: SourceMap,
    diagnostics: DiagnosticSink,
    id_state: parser::IdState,
    cache: HashMap<String, PackageKey>,
    in_progress: HashSet<String>,
    checked_packages: Vec<AnalyzedPackage>,
    extensions: Vec<ExtensionManifest>,
}

impl ProjectState {
    fn objects(&mut self) -> &mut TCObjects {
        self.tc_objs.as_mut().unwrap()
    }

    fn check_package(
        &mut self,
        key: PackageKey,
        files: &[File],
        trace: bool,
    ) -> Result<crate::check::TypeInfo, AnalysisError> {
        let mut checker = Checker::with_objects(
            key,
            std::mem::take(&mut self.interner),
            trace,
            self.tc_objs.take().unwrap(),
        );
        let result = checker.check(files);
        self.tc_objs = Some(checker.tc_objs);
        self.interner = checker.interner;
        self.diagnostics.extend(checker.diagnostics.into_inner());
        if result.is_err() {
            return Err(AnalysisError::Check(
                std::mem::take(&mut self.diagnostics),
                std::mem::take(&mut self.source_map),
            ));
        }
        Ok(checker.result)
    }
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
    let mut state = ProjectState {
        tc_objs: Some(TCObjects::new()),
        interner: SymbolInterner::new(),
        source_map: SourceMap::new(),
        diagnostics: DiagnosticSink::new(),
        id_state: parser::IdState::default(),
        cache: HashMap::new(),
        in_progress: HashSet::new(),
        checked_packages: Vec::new(),
        extensions: Vec::new(),
    };
    let main_pkg_key = state
        .objects()
        .new_package(main_package_path.clone(), main_package_abi_path);
    let parsed_files = parse_files(&files, &mut state)?;
    for extension in root_extensions {
        record_extension(&mut state, extension).map_err(AnalysisError::Import)?;
    }

    state.in_progress.insert(main_package_path.clone());
    {
        let mut loader = PackageLoader {
            vfs,
            state: &mut state,
        };
        loader.load("errors", Some(&main_package_path), 1)?;
        loader.load_imports(&parsed_files, &main_package_path, 1)?;
    }
    state.in_progress.remove(&main_package_path);
    let type_info = state.check_package(main_pkg_key, &parsed_files, options.trace)?;
    state.checked_packages.push(AnalyzedPackage {
        key: main_pkg_key,
        files: parsed_files,
        type_info,
    });
    Project::from_packages(
        state.tc_objs.unwrap(),
        state.interner,
        state.checked_packages,
        state.source_map,
        state.diagnostics,
        state.extensions,
    )
    .map_err(AnalysisError::Import)
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
    state: &mut ProjectState,
    mut extension: ExtensionManifest,
) -> Result<(), String> {
    extension
        .validate()
        .map_err(|error| format!("invalid extension metadata: {error}"))?;
    let manifest_path = normalize_fs_path(&extension.manifest_path);
    extension.manifest_path = manifest_path.clone();
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
    state: &mut ProjectState,
    id_state: parser::IdState,
) -> Result<(File, parser::IdState), AnalysisError> {
    let file_name = path
        .file_name()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_else(|| path.to_string_lossy().into_owned());
    let file_id = state
        .source_map
        .try_add_file_with_path(file_name, path.to_path_buf(), content)
        .map_err(|error| {
            AnalysisError::Import(format!(
                "cannot add source file '{}': {error}",
                path.display()
            ))
        })?;
    let base = state.source_map.file_base(file_id).unwrap_or(0);
    let interner = std::mem::take(&mut state.interner);

    let (file, diags, new_interner, new_id_state) =
        parser::parse_with_state(content, base, interner, id_state);

    state.interner = new_interner;

    let failed = diags.has_errors();
    state.diagnostics.extend(diags);
    if failed {
        return Err(AnalysisError::Parse(
            std::mem::take(&mut state.diagnostics),
            std::mem::take(&mut state.source_map),
        ));
    }
    Ok((file, new_id_state))
}

/// Parse source files from a FileSet.
fn parse_files(files: &FileSet, state: &mut ProjectState) -> Result<Vec<File>, AnalysisError> {
    let mut parsed_files = Vec::new();

    let mut paths: Vec<_> = files.files.keys().cloned().collect();
    sort_fs_paths(&mut paths);
    for path in paths {
        let content = &files.files[&path];
        let id_state = state.id_state.clone();
        let (file, new_id_state) = parse_single_file(&path, content, state, id_state)?;
        state.id_state = new_id_state;
        parsed_files.push(file);
    }

    Ok(parsed_files)
}

/// Parse package files from VFS.
fn parse_vfs_package(
    vfs_pkg: &VfsPackage,
    state: &mut ProjectState,
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

/// Maximum dependency edges followed from the root package.
const MAX_IMPORT_DEPTH: usize = 128;

/// Module loading owns dependency discovery; Checker only sees complete imports.
/// Structured syntax/type errors cross this boundary without being formatted.
struct PackageLoader<'a, R: Resolver> {
    vfs: &'a R,
    state: &'a mut ProjectState,
}

impl<R: Resolver> PackageLoader<'_, R> {
    fn load_imports(
        &mut self,
        files: &[File],
        parent: &str,
        depth: usize,
    ) -> Result<(), AnalysisError> {
        for file in files {
            for import in &file.imports {
                self.load(&import.path.value, Some(parent), depth)?;
            }
        }
        Ok(())
    }

    fn load(
        &mut self,
        path: &str,
        parent: Option<&str>,
        depth: usize,
    ) -> Result<PackageKey, AnalysisError> {
        identity::classify_import(path)
            .map_err(|e| AnalysisError::Import(format!("invalid import path \"{path}\": {e}")))?;
        if let Some(parent) = parent {
            if !identity::check_internal_visibility(parent, path) {
                return Err(AnalysisError::Import(format!(
                    "use of internal package not allowed: {parent} cannot import {path}"
                )));
            }
        }
        if let Some(&key) = self.state.cache.get(path) {
            return Ok(key);
        }
        if self.state.in_progress.contains(path) {
            return Err(AnalysisError::Import(format!(
                "import cycle detected for '{path}'"
            )));
        }
        if depth > MAX_IMPORT_DEPTH {
            return Err(AnalysisError::Import(format!("import graph depth exceeds the supported limit of {MAX_IMPORT_DEPTH} while loading '{path}'")));
        }
        self.state.in_progress.insert(path.to_owned());
        let result = self.load_package(path, depth);
        self.state.in_progress.remove(path);
        result
    }

    fn load_package(&mut self, path: &str, depth: usize) -> Result<PackageKey, AnalysisError> {
        let package = self
            .vfs
            .resolve(path)
            .map_err(|e| AnalysisError::Import(format!("failed to resolve package {path}: {e}")))?
            .ok_or_else(|| AnalysisError::Import(format!("package not found: {path}")))?;
        if package.path() != path {
            return Err(AnalysisError::Import(format!("import path '{path}' resolved to package '{}'; imports must use the canonical package path", package.path())));
        }
        let files = parse_vfs_package(&package, self.state)?;
        let name = declared_package_name(&files, &self.state.interner);
        if name.as_deref() == Some("main") {
            return Err(AnalysisError::Import(format!(
                "cannot import package {path}: package clause is main"
            )));
        }
        if let Some(extension) = package.extension().cloned() {
            record_extension(self.state, extension).map_err(AnalysisError::Import)?;
        }
        self.load_imports(&files, path, depth + 1)?;
        let key = self
            .state
            .objects()
            .new_package(path.to_owned(), package.abi_path().to_owned());
        self.state.objects().pkgs[key]
            .set_name(name.unwrap_or_else(|| path.rsplit('/').next().unwrap_or(path).to_owned()));
        let type_info = self.state.check_package(key, &files, false)?;
        self.state.cache.insert(path.to_owned(), key);
        self.state.checked_packages.push(AnalyzedPackage {
            key,
            files,
            type_info,
        });
        Ok(key)
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
        ] {
            assert!(
                PackageIdentity::new(path).is_err(),
                "invalid package identity {path:?} must be rejected"
            );
        }
        assert!(PackageIdentity::new("local/demo").is_ok());
        assert!(PackageIdentity::new("local/demo/child").is_ok());
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
                "format = 1\nmodule = \"github.com/acme/tool\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
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
                "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
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
                "format = 1\nmodule = \"github.com/acme/secret\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
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
                "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
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
                "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
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
                "format = 1\nmodule = \"github.com/acme/graph\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
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

        let project = analyze_project_with_identity(
            files,
            &resolver,
            PackageIdentity::new("github.com/acme/graph").unwrap(),
        )
        .unwrap();
        let paths: Vec<_> = project
            .packages()
            .iter()
            .map(|package| project.tc_objs.pkgs[package.key].path())
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

        let imported: Vec<_> = project.imported_packages_in_order().collect();
        assert_eq!(imported.len(), 4);
        assert!(imported.iter().all(|(_, _, _, files)| !files.is_empty()));
    }
}
