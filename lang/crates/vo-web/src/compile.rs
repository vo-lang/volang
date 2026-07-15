//! Vo source compilation pipeline for WASM targets.
//!
//! Provides multiple compilation entry points:
//! - Single-file compilation (`compile_source_with_*`)
//! - Multi-file package compilation (`compile_entry_with_*`)
//! - Module source backed by `MemoryFs` or `WasmVfs`

use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

use vo_analysis::vfs::{
    analyze_file_set_with_current_module, project_package_resolver_with_layout_and_replaces,
    ProjectModLayout,
};
use vo_common::vfs::{FileSet, FileSystem, MemoryFs};
use vo_module::inline_mod::InlineMod;
use vo_module::operation_error::OperationError;
#[cfg(any(test, target_arch = "wasm32"))]
use vo_module::project::ProjectContextOptions;
use vo_module::project::{
    ProjectDeps, ProjectDepsError, ProjectDepsErrorKind, ProjectDepsStage, SingleFileContext,
};

use crate::js_types::CompileResult;
#[cfg(target_arch = "wasm32")]
use crate::wasm_vfs::WasmVfs;

/// Build the standard library filesystem. Exported for libraries to extend.
pub fn build_stdlib_fs() -> MemoryFs {
    fn build_embedded_stdlib_fs() -> MemoryFs {
        let stdlib = vo_stdlib::EmbeddedStdlib::new();
        let mut fs = MemoryFs::new();

        fn add_dir_recursive(stdlib: &vo_stdlib::EmbeddedStdlib, fs: &mut MemoryFs, path: &Path) {
            use vo_common::vfs::FileSystem;
            if let Ok(entries) = stdlib.read_dir(path) {
                for entry in entries {
                    if stdlib.is_dir(&entry) {
                        add_dir_recursive(stdlib, fs, &entry);
                    } else if entry.to_str().map(|s| s.ends_with(".vo")).unwrap_or(false) {
                        if let Ok(content) = stdlib.read_file(&entry) {
                            fs.add_file(entry, content);
                        }
                    }
                }
            }
        }

        add_dir_recursive(&stdlib, &mut fs, Path::new("."));
        fs
    }

    static STDLIB_FS: OnceLock<MemoryFs> = OnceLock::new();
    STDLIB_FS.get_or_init(build_embedded_stdlib_fs).clone()
}

// ── Internal helpers ─────────────────────────────────────────────────────────

#[cfg(any(test, target_arch = "wasm32"))]
fn entry_package_dir(entry: &str) -> &Path {
    Path::new(entry).parent().unwrap_or(Path::new("."))
}

struct PreparedCompileInput {
    local_fs: MemoryFs,
    file_set: FileSet,
    project_root: PathBuf,
    project_deps: vo_module::project::ProjectDeps,
    workspace_replaces: HashMap<String, PathBuf>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WebCompileStage {
    Policy,
    FileSet,
    Analysis,
    Codegen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WebCompileErrorKind {
    Read,
    Validation,
    Analysis,
    Codegen,
}

type WebCompileError = OperationError<WebCompileStage, WebCompileErrorKind>;

fn web_compile_error_from_project(error: ProjectDepsError) -> WebCompileError {
    fn project_stage(stage: ProjectDepsStage) -> WebCompileStage {
        match stage {
            ProjectDepsStage::Workspace
            | ProjectDepsStage::ModFile
            | ProjectDepsStage::LockFile => WebCompileStage::Policy,
        }
    }
    fn project_kind(kind: ProjectDepsErrorKind) -> WebCompileErrorKind {
        match kind {
            ProjectDepsErrorKind::Missing | ProjectDepsErrorKind::ReadFailed => {
                WebCompileErrorKind::Read
            }
            ProjectDepsErrorKind::ParseFailed | ProjectDepsErrorKind::ValidationFailed => {
                WebCompileErrorKind::Validation
            }
        }
    }
    WebCompileError::from_other(error, project_stage, project_kind)
}

fn prepare_single_file_input(
    source: &str,
    filename: &str,
) -> Result<PreparedCompileInput, WebCompileError> {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(PathBuf::from(filename), source.to_string());

    // Spec §5.6: classify the file before compiling so that inline
    // `/*vo:mod*/` metadata and the reserved `/*vo:` prefix are handled
    // uniformly with the native engine (`vo-engine::compile`). This replaces
    // the older ad hoc external-import pre-check that did not recognize
    // inline mod blocks at all.
    let ctx = vo_module::project::load_single_file_context(&local_fs, Path::new(filename))
        .map_err(web_compile_error_from_project)?;

    let file_set = FileSet::from_file(&local_fs, Path::new(filename), PathBuf::from(".")).map_err(
        |error| {
            WebCompileError::new(
                WebCompileStage::FileSet,
                WebCompileErrorKind::Read,
                format!("Failed to read file: {}", error),
            )
            .with_path(Path::new(filename))
        },
    )?;

    match ctx {
        SingleFileContext::Project(project_context) => {
            // A caller-supplied `local_fs` cannot realistically contain an
            // ancestor `vo.mod` (this MemoryFs is created fresh inside this
            // helper), but if a future caller adds one we respect it.
            let (project_root, project_deps, workspace_replaces) = project_context.into_parts();
            Ok(PreparedCompileInput {
                local_fs,
                file_set,
                project_root,
                project_deps,
                workspace_replaces,
            })
        }
        SingleFileContext::EphemeralInlineMod { inline_mod, .. } => {
            ensure_inline_mod_dependencies_are_resolved(&inline_mod)?;
            // With empty `require`, an ephemeral single-file module sees only
            // the stdlib. Any `github.com/...` import that is not declared is
            // a spec §5.6.3 / §10.2 violation.
            reject_external_imports_in_source(source, "single-file ephemeral module")?;
            Ok(PreparedCompileInput {
                local_fs,
                file_set,
                project_root: PathBuf::from("."),
                project_deps: ProjectDeps::default(),
                workspace_replaces: HashMap::new(),
            })
        }
        SingleFileContext::AdHoc { .. } => {
            // Spec §10.1: ad hoc programs see only the stdlib. Keep the
            // long-standing "requires a project with vo.mod and vo.lock"
            // diagnostic so existing callers (apps/studio/playground) can continue
            // to match on it.
            reject_external_imports_in_source(source, "ad hoc program")?;
            Ok(PreparedCompileInput {
                local_fs,
                file_set,
                project_root: PathBuf::from("."),
                project_deps: ProjectDeps::default(),
                workspace_replaces: HashMap::new(),
            })
        }
    }
}

/// Reject external dependencies in the web source-only compilation API.
///
/// This entry point receives one source string and an immutable stdlib view;
/// it has no registry or resolved ephemeral lock. Project-oriented web flows
/// can supply a frozen dependency graph through their dedicated pipeline.
fn ensure_inline_mod_dependencies_are_resolved(inline: &InlineMod) -> Result<(), WebCompileError> {
    if inline.require.is_empty() {
        return Ok(());
    }
    Err(WebCompileError::new(
        WebCompileStage::Policy,
        WebCompileErrorKind::Validation,
        format!(
            "inline '/*vo:mod*/' declares {} require entry(ies), but web source-only \
             compilation has no registry or resolved ephemeral lock; use a project flow \
             with a frozen vo.mod/vo.lock dependency graph",
            inline.require.len()
        ),
    ))
}

/// Reject any `github.com/...` import in a single-file source that is not
/// backed by a resolved dependency graph.
///
/// `context_kind` is interpolated into the diagnostic so the caller can see
/// whether the file was classified as an ad hoc program or an ephemeral
/// single-file module. The phrasing "requires a project with vo.mod and
/// vo.lock" is preserved from the previous web compile diagnostic so that
/// downstream string-matching callers continue to function.
fn reject_external_imports_in_source(
    source: &str,
    context_kind: &str,
) -> Result<(), WebCompileError> {
    let (file, diagnostics, _) = vo_syntax::parser::parse(source, 0);
    if diagnostics.has_errors() {
        // Let the normal parse/analyze pipeline surface syntactic errors.
        return Ok(());
    }
    for import in &file.imports {
        let import_path = import.path.value.as_str();
        if is_external_import_path(import_path) {
            return Err(WebCompileError::new(
                WebCompileStage::Policy,
                WebCompileErrorKind::Validation,
                format!(
                    "external import \"{}\" from a {} requires a project with vo.mod and vo.lock; \
                     single-file web compilation does not resolve third-party modules",
                    import_path, context_kind,
                ),
            ));
        }
    }
    Ok(())
}

#[cfg(any(test, target_arch = "wasm32"))]
fn prepare_entry_input_with_options(
    entry: &str,
    local_fs: MemoryFs,
    options: &ProjectContextOptions,
) -> Result<PreparedCompileInput, WebCompileError> {
    let file_set = FileSet::collect(&local_fs, entry_package_dir(entry), PathBuf::from("."))
        .map_err(|error| {
            WebCompileError::new(
                WebCompileStage::FileSet,
                WebCompileErrorKind::Read,
                format!("Failed to collect package files: {}", error),
            )
            .with_path(entry_package_dir(entry))
        })?;
    let context = vo_module::project::load_project_context_with_options(
        &local_fs,
        entry_package_dir(entry),
        options,
    )
    .map_err(web_compile_error_from_project)?;
    let (project_root, project_deps, workspace_replaces) = context.into_parts();
    Ok(PreparedCompileInput {
        local_fs,
        file_set,
        project_root,
        project_deps,
        workspace_replaces,
    })
}

fn compile_with_package_resolver<R: vo_analysis::vfs::Resolver>(
    input: PreparedCompileInput,
    package_resolver: R,
) -> Result<Vec<u8>, WebCompileError> {
    let current_module = input.project_deps.current_module().map(str::to_string);
    let local_fs = input.local_fs;
    let project = analyze_file_set_with_current_module(
        input.file_set,
        package_resolver,
        local_fs,
        input.project_root,
        current_module,
    )
    .map_err(|error| {
        WebCompileError::new(
            WebCompileStage::Analysis,
            WebCompileErrorKind::Analysis,
            format!("{}", error),
        )
    })?;
    let module = vo_codegen::compile_project(&project).map_err(|error| {
        WebCompileError::new(
            WebCompileStage::Codegen,
            WebCompileErrorKind::Codegen,
            format!("{}", error),
        )
    })?;
    vo_common_core::verifier::verify_module(&module).map_err(|error| {
        WebCompileError::new(
            WebCompileStage::Codegen,
            WebCompileErrorKind::Codegen,
            format!("generated invalid bytecode: {}", error),
        )
    })?;
    module.serialize().map_err(|error| {
        WebCompileError::new(
            WebCompileStage::Codegen,
            WebCompileErrorKind::Codegen,
            format!("failed to serialize generated bytecode: {error}"),
        )
    })
}

fn compile_with_fs_modules<M: FileSystem + Send + Sync>(
    input: PreparedCompileInput,
    std_fs: MemoryFs,
    mod_fs: M,
    layout: ProjectModLayout,
) -> Result<Vec<u8>, WebCompileError> {
    let package_resolver = project_package_resolver_with_layout_and_replaces(
        std_fs,
        mod_fs,
        input.local_fs.clone(),
        &input.project_deps,
        input.workspace_replaces.clone(),
        layout,
    );
    compile_with_package_resolver(input, package_resolver)
}

fn is_external_import_path(import_path: &str) -> bool {
    matches!(
        vo_module::identity::classify_import(import_path),
        Ok(vo_module::identity::ImportClass::External)
    )
}

pub fn extract_external_module_paths(source: &str) -> Vec<String> {
    let (file, diagnostics, _) = vo_syntax::parser::parse(source, 0);
    if diagnostics.has_errors() {
        return Vec::new();
    }
    let mut imports = BTreeSet::new();
    for import in &file.imports {
        let import_path = import.path.value.as_str();
        if is_external_import_path(import_path) {
            imports.insert(import_path.to_string());
        }
    }
    imports.into_iter().collect()
}

#[cfg(test)]
fn compile_entry_with_external_fs<M: FileSystem + Send + Sync>(
    entry: &str,
    local_fs: MemoryFs,
    std_fs: MemoryFs,
    mod_fs: M,
    layout: ProjectModLayout,
) -> Result<Vec<u8>, String> {
    compile_entry_with_external_fs_with_options(
        entry,
        local_fs,
        std_fs,
        mod_fs,
        layout,
        &ProjectContextOptions::default(),
    )
}

#[cfg(any(test, target_arch = "wasm32"))]
fn compile_entry_with_external_fs_with_options<M: FileSystem + Send + Sync>(
    entry: &str,
    local_fs: MemoryFs,
    std_fs: MemoryFs,
    mod_fs: M,
    layout: ProjectModLayout,
    options: &ProjectContextOptions,
) -> Result<Vec<u8>, String> {
    let input = prepare_entry_input_with_options(entry, local_fs, options)
        .map_err(|error| error.to_string())?;
    compile_with_fs_modules(input, std_fs, mod_fs, layout).map_err(|error| error.to_string())
}

// ── Public compilation functions ─────────────────────────────────────────────

/// Compile Vo source code to bytecode (WASM export).
#[wasm_bindgen::prelude::wasm_bindgen]
pub fn compile(source: &str, filename: Option<String>) -> CompileResult {
    let filename = filename.unwrap_or_else(|| "main.vo".to_string());

    match compile_source_with_std_fs(source, &filename, build_stdlib_fs()) {
        Ok(bytecode) => CompileResult {
            success: true,
            bytecode: Some(bytecode),
            error_message: None,
            error_line: None,
            error_column: None,
        },
        Err(msg) => CompileResult {
            success: false,
            bytecode: None,
            error_message: Some(msg),
            error_line: None,
            error_column: None,
        },
    }
}

/// Compile source with a custom stdlib filesystem.
/// Exported for libraries (like vogui) that need to add extra packages.
pub fn compile_source_with_std_fs(
    source: &str,
    filename: &str,
    std_fs: MemoryFs,
) -> Result<Vec<u8>, String> {
    let input = prepare_single_file_input(source, filename).map_err(|error| error.to_string())?;
    compile_with_fs_modules(
        input,
        std_fs,
        MemoryFs::new(),
        ProjectModLayout::ImportPaths,
    )
    .map_err(|error| error.to_string())
}

/// Compile source with separate stdlib and external module filesystems.
///
/// `mod_fs` must have module files at paths matching the canonical module path, e.g.
/// `github.com/vo-lang/resvg/resvg.vo` for `import "github.com/vo-lang/resvg"`.
pub fn compile_source_with_mod_fs(
    source: &str,
    filename: &str,
    std_fs: MemoryFs,
    mod_fs: MemoryFs,
) -> Result<Vec<u8>, String> {
    let input = prepare_single_file_input(source, filename).map_err(|error| error.to_string())?;
    compile_with_fs_modules(input, std_fs, mod_fs, ProjectModLayout::ImportPaths)
        .map_err(|error| error.to_string())
}

#[cfg(target_arch = "wasm32")]
pub fn compile_source_with_vfs(
    source: &str,
    filename: &str,
    mod_root: &str,
) -> Result<Vec<u8>, String> {
    let input = prepare_single_file_input(source, filename).map_err(|error| error.to_string())?;
    compile_with_fs_modules(
        input,
        build_stdlib_fs(),
        WasmVfs::new(mod_root),
        ProjectModLayout::VersionedCache,
    )
    .map_err(|error| error.to_string())
}

/// Compile a multi-file Vo package given a pre-populated local filesystem.
///
/// `entry` is the path to the package entry file inside `local_fs`
/// (e.g. `"apps/studio/main.vo"`). `local_fs` must contain all package source files.
/// `std_fs` must contain stdlib packages only.
/// Third-party modules (imports containing `.`) are not resolved by this variant;
/// use `compile_entry_with_mod_fs` if the package has external module dependencies.
#[cfg(test)]
fn compile_entry_with_mod_fs(
    entry: &str,
    local_fs: MemoryFs,
    std_fs: MemoryFs,
    mod_fs: MemoryFs,
) -> Result<Vec<u8>, String> {
    compile_entry_with_external_fs(
        entry,
        local_fs,
        std_fs,
        mod_fs,
        ProjectModLayout::ImportPaths,
    )
}

#[cfg(test)]
fn compile_entry_with_versioned_mod_fs(
    entry: &str,
    local_fs: MemoryFs,
    std_fs: MemoryFs,
    mod_fs: MemoryFs,
) -> Result<Vec<u8>, String> {
    compile_entry_with_external_fs(
        entry,
        local_fs,
        std_fs,
        mod_fs,
        ProjectModLayout::VersionedCache,
    )
}

#[cfg(target_arch = "wasm32")]
pub fn compile_entry_with_vfs(
    entry: &str,
    local_fs: MemoryFs,
    mod_root: &str,
) -> Result<Vec<u8>, String> {
    compile_entry_with_vfs_with_options(
        entry,
        local_fs,
        mod_root,
        &ProjectContextOptions::default(),
    )
}

#[cfg(target_arch = "wasm32")]
pub fn compile_entry_with_vfs_with_options(
    entry: &str,
    local_fs: MemoryFs,
    mod_root: &str,
    options: &ProjectContextOptions,
) -> Result<Vec<u8>, String> {
    compile_entry_with_external_fs_with_options(
        entry,
        local_fs,
        build_stdlib_fs(),
        WasmVfs::new(mod_root),
        ProjectModLayout::VersionedCache,
        options,
    )
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests;
