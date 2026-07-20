//! Vo source compilation pipeline for WASM targets.
//!
//! Provides two explicit compilation boundaries:
//! - stdlib-only single-file compilation (`compile_source_with_std_fs`)
//! - validated project compilation (`compile_entry_with_mod_fs` or `compile_entry_with_vfs`)

use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

#[cfg(any(test, target_arch = "wasm32"))]
use vo_analysis::vfs::analyze_file_set_with_synthesized_ephemeral_module;
use vo_analysis::vfs::{
    analyze_file_set_with_current_module, project_package_resolver_with_workspace_sources,
};
use vo_common::vfs::{FileSet, FileSystem, MemoryFs};
use vo_module::operation_error::OperationError;
use vo_module::project::ProjectContextOptions;
use vo_module::project::{
    ProjectPlan, ProjectPlanError, ProjectPlanErrorKind, ProjectPlanStage, SingleFileContext,
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

fn entry_package_dir(entry: &str) -> &Path {
    Path::new(entry).parent().unwrap_or(Path::new("."))
}

struct PreparedCompileInput {
    local_fs: MemoryFs,
    file_set: FileSet,
    project_root: PathBuf,
    project_plan: vo_module::project::ProjectPlan,
    workspace_modules: Vec<vo_module::project::WorkspaceModule>,
    workspace_sources: HashMap<String, PathBuf>,
    module_authority: ModuleAnalysisAuthority,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ModuleAnalysisAuthority {
    Project,
    #[cfg(any(test, target_arch = "wasm32"))]
    SynthesizedEphemeral,
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

fn web_compile_error_from_project(error: ProjectPlanError) -> WebCompileError {
    fn project_stage(stage: ProjectPlanStage) -> WebCompileStage {
        match stage {
            ProjectPlanStage::Workspace
            | ProjectPlanStage::ModFile
            | ProjectPlanStage::LockFile => WebCompileStage::Policy,
        }
    }
    fn project_kind(kind: ProjectPlanErrorKind) -> WebCompileErrorKind {
        match kind {
            ProjectPlanErrorKind::Missing | ProjectPlanErrorKind::ReadFailed => {
                WebCompileErrorKind::Read
            }
            ProjectPlanErrorKind::ParseFailed | ProjectPlanErrorKind::ValidationFailed => {
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
            let workspace_modules = project_context.workspace_modules().to_vec();
            let (project_root, project_plan, workspace_sources) = project_context.into_parts();
            Ok(PreparedCompileInput {
                local_fs,
                file_set,
                project_root,
                project_plan,
                workspace_modules,
                workspace_sources,
                module_authority: ModuleAnalysisAuthority::Project,
            })
        }
        SingleFileContext::EphemeralInlineMod { .. } => {
            // An ephemeral single-file module sees only the stdlib.
            reject_external_imports_in_source(source, "single-file ephemeral module")?;
            Ok(PreparedCompileInput {
                local_fs,
                file_set,
                project_root: PathBuf::from("."),
                project_plan: ProjectPlan::default(),
                workspace_modules: Vec::new(),
                workspace_sources: HashMap::new(),
                module_authority: ModuleAnalysisAuthority::Project,
            })
        }
        SingleFileContext::AdHoc { .. } => {
            // Spec §10.1: ad hoc programs see only the stdlib.
            reject_external_imports_in_source(source, "ad hoc program")?;
            Ok(PreparedCompileInput {
                local_fs,
                file_set,
                project_root: PathBuf::from("."),
                project_plan: ProjectPlan::default(),
                workspace_modules: Vec::new(),
                workspace_sources: HashMap::new(),
                module_authority: ModuleAnalysisAuthority::Project,
            })
        }
    }
}

/// Reject any `github.com/...` import in a single-file source that is not
/// backed by a resolved dependency graph.
///
/// `context_kind` is interpolated into the diagnostic so the caller can see
/// whether the file was classified as an ad hoc program or an ephemeral
/// single-file module.
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
                    "external import \"{}\" cannot be resolved from a {}; single files support \
                     only the standard library, so create a project with vo.mod and commit its \
                     generated vo.lock",
                    import_path, context_kind,
                ),
            ));
        }
    }
    Ok(())
}

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
    let workspace_modules = context.workspace_modules().to_vec();
    let (project_root, project_plan, workspace_sources) = context.into_parts();
    Ok(PreparedCompileInput {
        local_fs,
        file_set,
        project_root,
        project_plan,
        workspace_modules,
        workspace_sources,
        module_authority: ModuleAnalysisAuthority::Project,
    })
}

/// Prepare a toolchain-synthesized `local/*` single-file project.
///
/// This boundary is deliberately separate from ordinary project discovery:
/// ephemeral identities are valid only for compiler-created single-file
/// manifests, never for user-authored projects or workspace members.
#[cfg(any(test, target_arch = "wasm32"))]
fn prepare_ephemeral_entry_input(
    entry: &str,
    local_fs: MemoryFs,
) -> Result<PreparedCompileInput, WebCompileError> {
    let project_root = entry_package_dir(entry).to_path_buf();
    let file_set =
        FileSet::collect(&local_fs, &project_root, PathBuf::from(".")).map_err(|error| {
            WebCompileError::new(
                WebCompileStage::FileSet,
                WebCompileErrorKind::Read,
                format!("Failed to collect ephemeral package files: {error}"),
            )
            .with_path(&project_root)
        })?;
    let mod_path = project_root.join("vo.mod");
    let mod_content = local_fs
        .read_text_limited(&mod_path, vo_common::vfs::MAX_TEXT_FILE_BYTES)
        .map_err(|error| {
            WebCompileError::new(
                WebCompileStage::Policy,
                WebCompileErrorKind::Read,
                format!("failed to read synthesized ephemeral vo.mod: {error}"),
            )
            .with_path(&mod_path)
        })?;
    let lock_path = project_root.join("vo.lock");
    if local_fs.exists(&lock_path) {
        return Err(WebCompileError::new(
            WebCompileStage::Policy,
            WebCompileErrorKind::Validation,
            "single-file modules cannot carry vo.lock; use a vo.mod project for third-party dependencies",
        )
        .with_path(&lock_path));
    }
    let project_plan = vo_module::project::read_inline_ephemeral_project_plan(&mod_content, None)
        .map_err(web_compile_error_from_project)?;
    let mod_file = project_plan
        .mod_file()
        .expect("ephemeral project dependency reader always returns a module");
    let entry_path = Path::new(entry);
    let entry_source = local_fs
        .read_text_limited(entry_path, vo_common::vfs::MAX_TEXT_FILE_BYTES)
        .map_err(|error| {
            WebCompileError::new(
                WebCompileStage::Policy,
                WebCompileErrorKind::Read,
                format!("failed to read ephemeral entry source: {error}"),
            )
            .with_path(entry_path)
        })?;
    let inline_mod = vo_module::inline_mod::parse_inline_mod_from_source(&entry_source)
        .map_err(|error| {
            WebCompileError::new(
                WebCompileStage::Policy,
                WebCompileErrorKind::Validation,
                format!("ephemeral entry has invalid inline module metadata: {error}"),
            )
            .with_path(entry_path)
        })?
        .ok_or_else(|| {
            WebCompileError::new(
                WebCompileStage::Policy,
                WebCompileErrorKind::Validation,
                "ephemeral compilation requires a leading /*vo:mod ... */ block",
            )
            .with_path(entry_path)
        })?;
    let synthesized = vo_module::inline_mod::synthesize_mod_file(&inline_mod);
    if &synthesized != mod_file {
        return Err(WebCompileError::new(
            WebCompileStage::Policy,
            WebCompileErrorKind::Validation,
            "synthesized ephemeral vo.mod does not match the entry's inline module authority",
        )
        .with_path(&mod_path));
    }
    if file_set.files.len() != 1 {
        return Err(WebCompileError::new(
            WebCompileStage::Policy,
            WebCompileErrorKind::Validation,
            "ephemeral compilation accepts exactly one source file in the entry package",
        )
        .with_path(&project_root));
    }
    vo_module::workspace::validate_project_external_imports(
        &local_fs,
        &project_root,
        mod_file,
        &[],
        &[],
        project_plan.lock_file(),
    )
    .map_err(|error| {
        WebCompileError::new(
            WebCompileStage::Policy,
            WebCompileErrorKind::Validation,
            format!("ephemeral source dependency validation failed: {error}"),
        )
        .with_path(&project_root)
    })?;

    Ok(PreparedCompileInput {
        local_fs,
        file_set,
        project_root,
        project_plan,
        workspace_modules: Vec::new(),
        workspace_sources: HashMap::new(),
        module_authority: ModuleAnalysisAuthority::SynthesizedEphemeral,
    })
}

fn compile_with_package_resolver<R: vo_analysis::vfs::Resolver>(
    input: PreparedCompileInput,
    package_resolver: R,
) -> Result<Vec<u8>, WebCompileError> {
    let current_module = input.project_plan.current_module().map(str::to_string);
    let local_fs = input.local_fs;
    let project = match input.module_authority {
        ModuleAnalysisAuthority::Project => analyze_file_set_with_current_module(
            input.file_set,
            package_resolver,
            local_fs,
            input.project_root,
            current_module,
        ),
        #[cfg(any(test, target_arch = "wasm32"))]
        ModuleAnalysisAuthority::SynthesizedEphemeral => {
            let current_module = current_module.ok_or_else(|| {
                WebCompileError::new(
                    WebCompileStage::Policy,
                    WebCompileErrorKind::Validation,
                    "synthesized ephemeral compilation lost its module identity",
                )
            })?;
            analyze_file_set_with_synthesized_ephemeral_module(
                input.file_set,
                package_resolver,
                local_fs,
                input.project_root,
                current_module,
            )
        }
    }
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
) -> Result<Vec<u8>, WebCompileError> {
    vo_module::readiness::validate_materialized_graph(
        &mod_fs,
        &input.project_plan,
        &input.workspace_modules,
    )
    .map_err(|error| {
        WebCompileError::new(
            WebCompileStage::Policy,
            WebCompileErrorKind::Validation,
            error.to_string(),
        )
    })?;
    let package_resolver = project_package_resolver_with_workspace_sources(
        std_fs,
        mod_fs,
        input.local_fs.clone(),
        &input.project_plan,
        input.workspace_sources.clone(),
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

fn compile_entry_with_external_fs<M: FileSystem + Send + Sync>(
    entry: &str,
    local_fs: MemoryFs,
    std_fs: MemoryFs,
    mod_fs: M,
) -> Result<Vec<u8>, String> {
    compile_entry_with_external_fs_with_options(
        entry,
        local_fs,
        std_fs,
        mod_fs,
        &ProjectContextOptions::default(),
    )
}

fn compile_entry_with_external_fs_with_options<M: FileSystem + Send + Sync>(
    entry: &str,
    local_fs: MemoryFs,
    std_fs: MemoryFs,
    mod_fs: M,
    options: &ProjectContextOptions,
) -> Result<Vec<u8>, String> {
    let input = prepare_entry_input_with_options(entry, local_fs, options)
        .map_err(|error| error.to_string())?;
    compile_with_fs_modules(input, std_fs, mod_fs).map_err(|error| error.to_string())
}

// ── Public compilation functions ─────────────────────────────────────────────

/// Compile one stdlib-only Vo source file to bytecode (WASM export).
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

/// Compile one source file against the standard-library filesystem supplied by
/// the host. This source-only API never resolves third-party modules.
pub fn compile_source_with_std_fs(
    source: &str,
    filename: &str,
    std_fs: MemoryFs,
) -> Result<Vec<u8>, String> {
    let input = prepare_single_file_input(source, filename).map_err(|error| error.to_string())?;
    compile_with_fs_modules(input, std_fs, MemoryFs::new()).map_err(|error| error.to_string())
}

/// Compile a validated Vo project from a pre-populated local filesystem.
///
/// `entry` is the path to the package entry file inside `local_fs`
/// (e.g. `"apps/studio/main.vo"`). `local_fs` must contain the project source,
/// its canonical `vo.mod` and a complete format-1 `vo.lock` whenever its
/// selected graph is non-empty. `mod_fs` uses the authenticated versioned
/// cache layout for registry modules; selected workspace modules are read from
/// `local_fs` and must match their workspace-origin lock records.
pub fn compile_entry_with_mod_fs(
    entry: &str,
    local_fs: MemoryFs,
    std_fs: MemoryFs,
    mod_fs: MemoryFs,
) -> Result<Vec<u8>, String> {
    compile_entry_with_external_fs(entry, local_fs, std_fs, mod_fs)
}

#[cfg(test)]
fn compile_ephemeral_entry_with_versioned_mod_fs(
    entry: &str,
    local_fs: MemoryFs,
    std_fs: MemoryFs,
    mod_fs: MemoryFs,
) -> Result<Vec<u8>, String> {
    let input =
        prepare_ephemeral_entry_input(entry, local_fs).map_err(|error| error.to_string())?;
    compile_with_fs_modules(input, std_fs, mod_fs).map_err(|error| error.to_string())
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
        options,
    )
}

/// Compile a compiler-synthesized single-file project with a reserved
/// `local/*` identity and standard-library-only dependency scope.
///
/// The explicit entry point keeps ephemeral identity authority separate from
/// ordinary project and workspace discovery. It never consults `vo.work`.
#[cfg(target_arch = "wasm32")]
pub fn compile_ephemeral_entry_with_vfs(
    entry: &str,
    local_fs: MemoryFs,
    mod_root: &str,
) -> Result<Vec<u8>, String> {
    let input =
        prepare_ephemeral_entry_input(entry, local_fs).map_err(|error| error.to_string())?;
    compile_with_fs_modules(input, build_stdlib_fs(), WasmVfs::new(mod_root))
        .map_err(|error| error.to_string())
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests;
