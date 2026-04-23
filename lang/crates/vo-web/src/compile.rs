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
            ensure_inline_mod_ephemeral_build_is_supported(&inline_mod)?;
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
            // diagnostic so existing callers (studio/playground) can continue
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

/// Reject ephemeral single-file modules whose inline mod declares external
/// `require` entries, until the web toolchain grows ephemeral dependency
/// resolution (spec §5.6.5, follow-up plan P3).
fn ensure_inline_mod_ephemeral_build_is_supported(
    inline: &InlineMod,
) -> Result<(), WebCompileError> {
    if inline.require.is_empty() {
        return Ok(());
    }
    Err(WebCompileError::new(
        WebCompileStage::Policy,
        WebCompileErrorKind::Validation,
        format!(
            "inline '/*vo:mod*/' declares {} require entry(ies), but ephemeral dependency \
             resolution is not yet implemented in the web toolchain; remove the 'require' \
             lines or promote the script to a project with vo.mod and vo.lock",
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
fn prepare_entry_input(
    entry: &str,
    local_fs: MemoryFs,
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
    let context = vo_module::project::load_project_context(&local_fs, entry_package_dir(entry))
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
    Ok(module.serialize())
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

#[cfg(any(test, target_arch = "wasm32"))]
fn compile_entry_with_external_fs<M: FileSystem + Send + Sync>(
    entry: &str,
    local_fs: MemoryFs,
    std_fs: MemoryFs,
    mod_fs: M,
    layout: ProjectModLayout,
) -> Result<Vec<u8>, String> {
    let input = prepare_entry_input(entry, local_fs).map_err(|error| error.to_string())?;
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
/// (e.g. `"studio/main.vo"`). `local_fs` must contain all package source files.
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
    compile_entry_with_external_fs(
        entry,
        local_fs,
        build_stdlib_fs(),
        WasmVfs::new(mod_root),
        ProjectModLayout::VersionedCache,
    )
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_entry_with_mod_fs_uses_vo_lock() {
        let std_fs = build_stdlib_fs();
        let mut local_fs = MemoryFs::new();
        let mut mod_fs = MemoryFs::new();

        local_fs.add_file(
            "vo.mod",
            "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/acme/lib v0.1.0\n",
        );
        local_fs.add_file(
            "vo.lock",
            concat!(
                "version = 1\n",
                "created_by = \"vo test\"\n\n",
                "[root]\n",
                "module = \"github.com/acme/app\"\n",
                "vo = \"0.1.0\"\n\n",
                "[[resolved]]\n",
                "path = \"github.com/acme/lib\"\n",
                "version = \"v0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "commit = \"0123456789abcdef0123456789abcdef01234567\"\n",
                "release_manifest = \"sha256:1111111111111111111111111111111111111111111111111111111111111111\"\n",
                "source = \"sha256:2222222222222222222222222222222222222222222222222222222222222222\"\n",
                "deps = []\n",
            ),
        );
        local_fs.add_file(
            "app/main.vo",
            concat!(
                "package main\n",
                "import \"github.com/acme/lib\"\n",
                "func main() {\n",
                "    lib.Hello()\n",
                "}\n",
            ),
        );

        mod_fs.add_file(
            "github.com/acme/lib/vo.mod",
            "module github.com/acme/lib\n\nvo 0.1.0\n",
        );
        mod_fs.add_file(
            "github.com/acme/lib/lib.vo",
            concat!("package lib\n", "func Hello() {}\n",),
        );

        let result = compile_entry_with_mod_fs("app/main.vo", local_fs, std_fs, mod_fs);
        match &result {
            Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
            Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
        }
    }

    #[test]
    fn test_compile_entry_with_versioned_mod_fs_uses_vo_lock() {
        let std_fs = build_stdlib_fs();
        let mut local_fs = MemoryFs::new();
        let mut mod_fs = MemoryFs::new();

        local_fs.add_file(
            "vo.mod",
            "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/acme/lib v0.1.0\n",
        );
        local_fs.add_file(
            "vo.lock",
            concat!(
                "version = 1\n",
                "created_by = \"vo test\"\n\n",
                "[root]\n",
                "module = \"github.com/acme/app\"\n",
                "vo = \"0.1.0\"\n\n",
                "[[resolved]]\n",
                "path = \"github.com/acme/lib\"\n",
                "version = \"v0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "commit = \"0123456789abcdef0123456789abcdef01234567\"\n",
                "release_manifest = \"sha256:1111111111111111111111111111111111111111111111111111111111111111\"\n",
                "source = \"sha256:2222222222222222222222222222222222222222222222222222222222222222\"\n",
                "deps = []\n",
            ),
        );
        local_fs.add_file(
            "app/main.vo",
            concat!(
                "package main\n",
                "import \"github.com/acme/lib\"\n",
                "func main() {\n",
                "    lib.Hello()\n",
                "}\n",
            ),
        );

        let module_dir =
            vo_module::cache::layout::relative_module_dir("github.com/acme/lib", "v0.1.0");
        mod_fs.add_file(
            module_dir.join("vo.mod"),
            "module github.com/acme/lib\n\nvo 0.1.0\n",
        );
        mod_fs.add_file(
            module_dir.join("lib.vo"),
            concat!("package lib\n", "func Hello() {}\n",),
        );

        let result = compile_entry_with_versioned_mod_fs("app/main.vo", local_fs, std_fs, mod_fs);
        match &result {
            Err(e) => panic!("compile_entry_with_versioned_mod_fs failed: {}", e),
            Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
        }
    }

    #[test]
    fn test_compile_entry_resolves_current_module_canonical_imports() {
        let mut local_fs = MemoryFs::new();
        local_fs.add_file("vo.mod", "module github.com/acme/app\n\nvo 0.1.0\n");
        local_fs.add_file(
            "main.vo",
            concat!(
                "package main\n",
                "import \"github.com/acme/app/util\"\n",
                "func main() {\n",
                "    util.Hello()\n",
                "}\n",
            ),
        );
        local_fs.add_file(
            "util/util.vo",
            concat!("package util\n", "func Hello() {}\n",),
        );

        let result =
            compile_entry_with_mod_fs("main.vo", local_fs, build_stdlib_fs(), MemoryFs::new());
        match &result {
            Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
            Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
        }
    }

    #[test]
    fn test_compile_entry_with_mod_fs_honors_workspace_replace_without_vo_lock() {
        let mut local_fs = MemoryFs::new();
        local_fs.add_file(
            "workspace/app/vo.mod",
            "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/acme/replaced v0.1.0\n",
        );
        local_fs.add_file(
            "workspace/vo.work",
            "version = 1\n\n[[use]]\npath = \"./replaced\"\n",
        );
        local_fs.add_file(
            "workspace/app/main.vo",
            concat!(
                "package main\n",
                "import \"github.com/acme/replaced\"\n",
                "func main() {\n",
                "    replaced.Hello()\n",
                "}\n",
            ),
        );
        local_fs.add_file(
            "workspace/replaced/vo.mod",
            "module github.com/acme/replaced\n\nvo 0.1.0\n",
        );
        local_fs.add_file(
            "workspace/replaced/replaced.vo",
            concat!("package replaced\n", "func Hello() {}\n",),
        );

        let result = compile_entry_with_mod_fs(
            "workspace/app/main.vo",
            local_fs,
            build_stdlib_fs(),
            MemoryFs::new(),
        );
        match &result {
            Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
            Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
        }
    }

    #[test]
    fn test_compile_entry_with_mod_fs_honors_vo_work_without_vo_lock() {
        let mut local_fs = MemoryFs::new();
        local_fs.add_file(
            "workspace/app/vo.mod",
            "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/acme/replaced v0.1.0\n",
        );
        local_fs.add_file(
            "workspace/app/vo.work",
            "version = 1\n[[use]]\npath = \"../replaced\"\n",
        );
        local_fs.add_file(
            "workspace/app/main.vo",
            concat!(
                "package main\n",
                "import \"github.com/acme/replaced\"\n",
                "func main() {\n",
                "    replaced.Hello()\n",
                "}\n",
            ),
        );
        local_fs.add_file(
            "workspace/replaced/vo.mod",
            "module github.com/acme/replaced\n\nvo 0.1.0\n",
        );
        local_fs.add_file(
            "workspace/replaced/replaced.vo",
            concat!("package replaced\n", "func Hello() {}\n",),
        );

        let result = compile_entry_with_mod_fs(
            "workspace/app/main.vo",
            local_fs,
            build_stdlib_fs(),
            MemoryFs::new(),
        );
        match &result {
            Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
            Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
        }
    }

    #[test]
    fn test_compile_entry_with_mod_fs_keeps_locked_transitive_modules_for_workspace_replace() {
        let std_fs = build_stdlib_fs();
        let mut local_fs = MemoryFs::new();
        let mut mod_fs = MemoryFs::new();

        local_fs.add_file(
            "workspace/app/vo.mod",
            "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/acme/replaced v0.1.0\n",
        );
        local_fs.add_file(
            "workspace/app/vo.lock",
            concat!(
                "version = 1\n",
                "created_by = \"vo test\"\n\n",
                "[root]\n",
                "module = \"github.com/acme/app\"\n",
                "vo = \"0.1.0\"\n\n",
                "[[resolved]]\n",
                "path = \"github.com/acme/replaced\"\n",
                "version = \"v0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "commit = \"0123456789abcdef0123456789abcdef01234567\"\n",
                "release_manifest = \"sha256:1111111111111111111111111111111111111111111111111111111111111111\"\n",
                "source = \"sha256:2222222222222222222222222222222222222222222222222222222222222222\"\n",
                "deps = [\"github.com/acme/core\"]\n",
                "artifacts = []\n\n",
                "[[resolved]]\n",
                "path = \"github.com/acme/core\"\n",
                "version = \"v0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "commit = \"fedcba9876543210fedcba9876543210fedcba98\"\n",
                "release_manifest = \"sha256:3333333333333333333333333333333333333333333333333333333333333333\"\n",
                "source = \"sha256:4444444444444444444444444444444444444444444444444444444444444444\"\n",
                "deps = []\n",
                "artifacts = []\n",
            ),
        );
        local_fs.add_file(
            "workspace/vo.work",
            "version = 1\n\n[[use]]\npath = \"./replaced\"\n",
        );
        local_fs.add_file(
            "workspace/app/main.vo",
            concat!(
                "package main\n",
                "import \"github.com/acme/replaced\"\n",
                "func main() {\n",
                "    replaced.Hello()\n",
                "}\n",
            ),
        );
        local_fs.add_file(
            "workspace/replaced/vo.mod",
            "module github.com/acme/replaced\n\nvo 0.1.0\n\nrequire github.com/acme/core v0.1.0\n",
        );
        local_fs.add_file(
            "workspace/replaced/replaced.vo",
            concat!(
                "package replaced\n",
                "import \"github.com/acme/core\"\n",
                "func Hello() {\n",
                "    core.Hello()\n",
                "}\n",
            ),
        );

        mod_fs.add_file(
            "github.com/acme/core/vo.mod",
            "module github.com/acme/core\n\nvo 0.1.0\n",
        );
        mod_fs.add_file(
            "github.com/acme/core/core.vo",
            concat!("package core\n", "func Hello() {}\n",),
        );

        let result = compile_entry_with_mod_fs("workspace/app/main.vo", local_fs, std_fs, mod_fs);
        match &result {
            Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
            Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
        }
    }

    #[test]
    fn test_compile_entry_with_mod_fs_keeps_locked_transitive_modules_for_workspace_override() {
        let std_fs = build_stdlib_fs();
        let mut local_fs = MemoryFs::new();
        let mut mod_fs = MemoryFs::new();

        local_fs.add_file(
            "workspace/app/vo.mod",
            "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/acme/replaced v0.1.0\n",
        );
        local_fs.add_file(
            "workspace/app/vo.work",
            "version = 1\n[[use]]\npath = \"../replaced\"\n",
        );
        local_fs.add_file(
            "workspace/app/vo.lock",
            concat!(
                "version = 1\n",
                "created_by = \"vo test\"\n\n",
                "[root]\n",
                "module = \"github.com/acme/app\"\n",
                "vo = \"0.1.0\"\n\n",
                "[[resolved]]\n",
                "path = \"github.com/acme/replaced\"\n",
                "version = \"v0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "commit = \"0123456789abcdef0123456789abcdef01234567\"\n",
                "release_manifest = \"sha256:1111111111111111111111111111111111111111111111111111111111111111\"\n",
                "source = \"sha256:2222222222222222222222222222222222222222222222222222222222222222\"\n",
                "deps = [\"github.com/acme/core\"]\n",
                "artifacts = []\n\n",
                "[[resolved]]\n",
                "path = \"github.com/acme/core\"\n",
                "version = \"v0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "commit = \"fedcba9876543210fedcba9876543210fedcba98\"\n",
                "release_manifest = \"sha256:3333333333333333333333333333333333333333333333333333333333333333\"\n",
                "source = \"sha256:4444444444444444444444444444444444444444444444444444444444444444\"\n",
                "deps = []\n",
                "artifacts = []\n",
            ),
        );
        local_fs.add_file(
            "workspace/app/main.vo",
            concat!(
                "package main\n",
                "import \"github.com/acme/replaced\"\n",
                "func main() {\n",
                "    replaced.Hello()\n",
                "}\n",
            ),
        );
        local_fs.add_file(
            "workspace/replaced/vo.mod",
            "module github.com/acme/replaced\n\nvo 0.1.0\n\nrequire github.com/acme/core v0.1.0\n",
        );
        local_fs.add_file(
            "workspace/replaced/replaced.vo",
            concat!(
                "package replaced\n",
                "import \"github.com/acme/core\"\n",
                "func Hello() {\n",
                "    core.Hello()\n",
                "}\n",
            ),
        );

        mod_fs.add_file(
            "github.com/acme/core/vo.mod",
            "module github.com/acme/core\n\nvo 0.1.0\n",
        );
        mod_fs.add_file(
            "github.com/acme/core/core.vo",
            concat!("package core\n", "func Hello() {}\n",),
        );

        let result = compile_entry_with_mod_fs("workspace/app/main.vo", local_fs, std_fs, mod_fs);
        match &result {
            Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
            Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
        }
    }

    #[test]
    fn test_compile_source_with_mod_fs_rejects_external_imports_without_project_files() {
        let source = concat!(
            "package main\n",
            "import \"github.com/acme/lib\"\n",
            "func main() {\n",
            "    lib.Hello()\n",
            "}\n",
        );
        let std_fs = build_stdlib_fs();
        let mod_fs = MemoryFs::new()
            .with_file(
                "github.com/acme/lib/vo.mod",
                "module github.com/acme/lib\n\nvo 0.1.0\n",
            )
            .with_file(
                "github.com/acme/lib/lib.vo",
                concat!("package lib\n", "func Hello() {}\n",),
            );

        let result = compile_source_with_mod_fs(source, "main.vo", std_fs, mod_fs);
        match result {
            Err(message) => {
                assert!(
                    message.contains("requires a project with vo.mod and vo.lock"),
                    "{message}"
                );
                assert!(message.contains("github.com/acme/lib"), "{message}");
                assert!(message.contains("ad hoc program"), "{message}");
            }
            Ok(_) => panic!("expected single-file external import rejection"),
        }
    }

    #[test]
    fn test_compile_single_file_inline_mod_without_require_is_ephemeral_and_compiles() {
        // Spec §5.6, §10.2: single-file ephemeral module with inline mod but
        // no `require` compiles as if it were ad hoc (stdlib only).
        let source = "\
/*vo:mod
module local/demo
vo ^0.1.0
*/
package main
func main() {}
";
        let std_fs = build_stdlib_fs();
        let bytes = compile_source_with_std_fs(source, "main.vo", std_fs).unwrap_or_else(|msg| {
            panic!("expected ephemeral inline-mod compile to succeed: {msg}")
        });
        assert!(!bytes.is_empty());
    }

    #[test]
    fn test_compile_single_file_inline_mod_with_require_rejected_as_unsupported() {
        // Spec §10.2 frozen-build rule: ephemeral single-file modules with
        // external requires need a resolved graph; the web toolchain does
        // not yet materialize a cache-local ephemeral lock.
        let source = "\
/*vo:mod
module local/demo
vo ^0.1.0
require github.com/vo-lang/vogui ^0.4.0
*/
package main
func main() {}
";
        let std_fs = build_stdlib_fs();
        let message = compile_source_with_std_fs(source, "main.vo", std_fs).expect_err(
            "expected ephemeral require to be rejected until resolution is implemented",
        );
        assert!(
            message.contains("ephemeral dependency resolution is not yet implemented"),
            "{message}"
        );
        assert!(message.contains("1 require"), "{message}");
    }

    #[test]
    fn test_compile_single_file_inline_mod_external_import_requires_declaration() {
        // Even with an inline mod block, undeclared external imports are
        // rejected the same way ad hoc programs reject them: single-file
        // web compilation does not resolve third-party modules.
        let source = "\
/*vo:mod
module local/demo
vo ^0.1.0
*/
package main
import \"github.com/vo-lang/vogui\"
func main() { vogui.Hello() }
";
        let std_fs = build_stdlib_fs();
        let message = compile_source_with_std_fs(source, "main.vo", std_fs)
            .expect_err("expected external import rejection");
        assert!(
            message.contains("requires a project with vo.mod and vo.lock"),
            "{message}"
        );
        assert!(message.contains("github.com/vo-lang/vogui"), "{message}");
        assert!(
            message.contains("single-file ephemeral module"),
            "{message}"
        );
    }

    #[test]
    fn test_compile_single_file_rejects_reserved_sentinel() {
        // Spec §5.6.1: a leading `/*vo:` block other than `/*vo:mod` is a
        // reserved-namespace error surfaced by the single-file classifier.
        let source = "/*vo:script\n*/\npackage main\nfunc main() {}\n";
        let std_fs = build_stdlib_fs();
        let message = compile_source_with_std_fs(source, "main.vo", std_fs)
            .expect_err("expected reserved sentinel rejection");
        assert!(
            message.contains("reserved sentinel '/*vo:' at start of file must be '/*vo:mod'"),
            "{message}"
        );
    }

    #[test]
    fn test_compile_single_file_rejects_local_require_in_inline_mod() {
        // Spec §3.5, §5.6.3: `local/*` paths MUST NOT appear in `require`.
        let source = "\
/*vo:mod
module local/demo
vo ^0.1.0
require local/other ^0.1.0
*/
package main
func main() {}
";
        let std_fs = build_stdlib_fs();
        let message = compile_source_with_std_fs(source, "main.vo", std_fs)
            .expect_err("expected local-require rejection");
        assert!(
            message.contains("'local/*' paths are not allowed in require"),
            "{message}"
        );
    }
}
