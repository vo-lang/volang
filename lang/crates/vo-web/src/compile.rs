//! Vo source compilation pipeline for WASM targets.
//!
//! Provides multiple compilation entry points:
//! - Single-file compilation (`compile_source_with_*`)
//! - Multi-file package compilation (`compile_entry_with_*`)
//! - Module source backed by `MemoryFs` or `WasmVfs`

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

use vo_analysis::vfs::{
    analyze_file_set_with_current_module, project_package_resolver_with_layout_and_replaces,
    project_package_resolver_with_replaces, ProjectModLayout,
};
use vo_common::vfs::{FileSet, MemoryFs};
use vo_module::operation_error::OperationError;
use vo_module::project::{ProjectDepsError, ProjectDepsErrorKind, ProjectDepsStage};

use crate::js_types::CompileResult;
use crate::wasm_vfs::WasmVfs;

// Re-export stdlib filesystem
pub use vo_stdlib::EmbeddedStdlib;

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

fn build_single_file_fs(source: &str, filename: &str) -> MemoryFs {
    let mut fs = MemoryFs::new();
    fs.add_file(PathBuf::from(filename), source.to_string());
    fs
}

fn build_single_file_set(fs: &MemoryFs, filename: &str) -> Result<FileSet, WebCompileError> {
    FileSet::from_file(fs, Path::new(filename), PathBuf::from(".")).map_err(|error| {
        WebCompileError::new(
            WebCompileStage::FileSet,
            WebCompileErrorKind::ReadFailed,
            format!("Failed to read file: {}", error),
        )
        .with_path(Path::new(filename))
    })
}

fn build_entry_file_set(local_fs: &MemoryFs, entry: &str) -> Result<FileSet, WebCompileError> {
    FileSet::collect(local_fs, entry_package_dir(entry), PathBuf::from(".")).map_err(|error| {
        WebCompileError::new(
            WebCompileStage::FileSet,
            WebCompileErrorKind::ReadFailed,
            format!("Failed to collect package files: {}", error),
        )
        .with_path(entry_package_dir(entry))
    })
}

struct PreparedCompileInput {
    local_fs: MemoryFs,
    file_set: FileSet,
    project_root: PathBuf,
    project_deps: vo_module::project::ProjectDeps,
    workspace_replaces: HashMap<String, PathBuf>,
}

pub enum CompileSource {
    SingleFile { source: String, filename: String },
    Entry { entry: String, local_fs: MemoryFs },
}

pub enum ModuleSource {
    Fs { std_fs: MemoryFs, mod_fs: MemoryFs },
    Vfs { vfs_mod_root: String },
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
    ReadFailed,
    ValidationFailed,
    AnalysisFailed,
    CodegenFailed,
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
                WebCompileErrorKind::ReadFailed
            }
            ProjectDepsErrorKind::ParseFailed | ProjectDepsErrorKind::ValidationFailed => {
                WebCompileErrorKind::ValidationFailed
            }
        }
    }
    WebCompileError::from_other(error, project_stage, project_kind)
}

fn prepare_compile_input(source: CompileSource) -> Result<PreparedCompileInput, WebCompileError> {
    match source {
        CompileSource::SingleFile { source, filename } => {
            reject_single_file_external_imports_typed(&source)?;
            let local_fs = build_single_file_fs(&source, &filename);
            let file_set = build_single_file_set(&local_fs, &filename)?;
            let context =
                vo_module::project::load_project_context(&local_fs, entry_package_dir(&filename))
                    .map_err(web_compile_error_from_project)?;
            Ok(PreparedCompileInput {
                local_fs,
                file_set,
                project_root: context.project_root,
                project_deps: context.project_deps,
                workspace_replaces: context.workspace_replaces,
            })
        }
        CompileSource::Entry { entry, local_fs } => {
            let file_set = build_entry_file_set(&local_fs, &entry)?;
            let context =
                vo_module::project::load_project_context(&local_fs, entry_package_dir(&entry))
                    .map_err(web_compile_error_from_project)?;
            Ok(PreparedCompileInput {
                local_fs,
                file_set,
                project_root: context.project_root,
                project_deps: context.project_deps,
                workspace_replaces: context.workspace_replaces,
            })
        }
    }
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
            WebCompileErrorKind::AnalysisFailed,
            format!("{}", error),
        )
    })?;
    let module = vo_codegen::compile_project(&project).map_err(|error| {
        WebCompileError::new(
            WebCompileStage::Codegen,
            WebCompileErrorKind::CodegenFailed,
            format!("{}", error),
        )
    })?;
    Ok(module.serialize())
}

fn compile_web_typed(
    source: CompileSource,
    module_source: ModuleSource,
) -> Result<Vec<u8>, WebCompileError> {
    let input = prepare_compile_input(source)?;
    match module_source {
        ModuleSource::Fs { std_fs, mod_fs } => {
            let package_resolver = project_package_resolver_with_layout_and_replaces(
                std_fs,
                mod_fs,
                input.local_fs.clone(),
                &input.project_deps,
                input.workspace_replaces.clone(),
                ProjectModLayout::ImportPaths,
            );
            compile_with_package_resolver(input, package_resolver)
        }
        ModuleSource::Vfs { vfs_mod_root } => {
            let package_resolver = project_package_resolver_with_replaces(
                build_stdlib_fs(),
                WasmVfs::new(&vfs_mod_root),
                input.local_fs.clone(),
                &input.project_deps,
                input.workspace_replaces.clone(),
            );
            compile_with_package_resolver(input, package_resolver)
        }
    }
}

pub fn compile_web(source: CompileSource, module_source: ModuleSource) -> Result<Vec<u8>, String> {
    compile_web_typed(source, module_source).map_err(stringify_web_compile_error)
}

fn stringify_web_compile_error(error: WebCompileError) -> String {
    error.to_string()
}

fn is_external_import_path(import_path: &str) -> bool {
    matches!(
        vo_module::identity::classify_import(import_path),
        Ok(vo_module::identity::ImportClass::External)
    )
}

/// Extract unique external module root paths from source code imports.
///
/// For each `import "github.com/owner/repo/..."`, extracts the 3-segment
/// module root `github.com/owner/repo`. Returns deduplicated module paths.
pub fn extract_external_module_paths(source: &str) -> Vec<String> {
    let (file, diagnostics, _) = vo_syntax::parser::parse(source, 0);
    if diagnostics.has_errors() {
        return Vec::new();
    }
    let mut modules = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for import in &file.imports {
        let import_path = import.path.value.as_str();
        if is_external_import_path(import_path) {
            if let Some(module_root) = vo_module::identity::extract_module_root(import_path) {
                if seen.insert(module_root.clone()) {
                    modules.push(module_root);
                }
            }
        }
    }
    modules
}

fn reject_single_file_external_imports_typed(source: &str) -> Result<(), WebCompileError> {
    let (file, diagnostics, _) = vo_syntax::parser::parse(source, 0);
    if diagnostics.has_errors() {
        return Ok(());
    }
    for import in &file.imports {
        let import_path = import.path.value.as_str();
        if is_external_import_path(import_path) {
            return Err(WebCompileError::new(
                WebCompileStage::Policy,
                WebCompileErrorKind::ValidationFailed,
                format!(
                    "external import \"{}\" requires a project with vo.mod and vo.lock; single-file web compilation no longer resolves third-party modules",
                    import_path,
                ),
            ));
        }
    }
    Ok(())
}

pub fn reject_single_file_external_imports(source: &str) -> Result<(), String> {
    reject_single_file_external_imports_typed(source).map_err(stringify_web_compile_error)
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
    compile_web(
        CompileSource::SingleFile {
            source: source.to_string(),
            filename: filename.to_string(),
        },
        ModuleSource::Fs {
            std_fs,
            mod_fs: MemoryFs::new(),
        },
    )
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
    compile_web(
        CompileSource::SingleFile {
            source: source.to_string(),
            filename: filename.to_string(),
        },
        ModuleSource::Fs { std_fs, mod_fs },
    )
}

/// Compile a multi-file Vo package given a pre-populated local filesystem.
///
/// `entry` is the path to the package entry file inside `local_fs`
/// (e.g. `"studio/main.vo"`). `local_fs` must contain all package source files.
/// `std_fs` must contain stdlib packages only.
/// Third-party modules (imports containing `.`) are not resolved by this variant;
/// use `compile_entry_with_mod_fs` if the package has external module dependencies.
pub fn compile_entry_with_std_fs(
    entry: &str,
    local_fs: MemoryFs,
    std_fs: MemoryFs,
) -> Result<Vec<u8>, String> {
    compile_web(
        CompileSource::Entry {
            entry: entry.to_string(),
            local_fs,
        },
        ModuleSource::Fs {
            std_fs,
            mod_fs: MemoryFs::new(),
        },
    )
}

/// Compile a multi-file Vo package with separate stdlib and module filesystems.
///
/// `entry` is the path to the package entry file inside `local_fs`.
/// `std_fs` contains stdlib packages (fmt, strings, etc.).
/// `mod_fs` contains third-party module dependencies at their canonical paths
/// (e.g. `github.com/vo-lang/vogui/app.vo`). Imports containing `.` are
/// resolved from `mod_fs`.
pub fn compile_entry_with_mod_fs(
    entry: &str,
    local_fs: MemoryFs,
    std_fs: MemoryFs,
    mod_fs: MemoryFs,
) -> Result<Vec<u8>, String> {
    compile_web(
        CompileSource::Entry {
            entry: entry.to_string(),
            local_fs,
        },
        ModuleSource::Fs { std_fs, mod_fs },
    )
}

/// Compile a multi-file Vo package, resolving third-party modules from the JS VFS.
///
/// This is the preferred WASM compilation path: stdlib is embedded, local files
/// come from `local_fs`, and third-party modules (imports containing `.`) are
/// resolved from the JS VirtualFS at `vfs_mod_root` (e.g. `"/.vo/mod"`).
///
/// Modules must be installed into the VFS beforehand (via `install_module_to_vfs`).
pub fn compile_entry_with_vfs(
    entry: &str,
    local_fs: MemoryFs,
    vfs_mod_root: &str,
) -> Result<Vec<u8>, String> {
    compile_web(
        CompileSource::Entry {
            entry: entry.to_string(),
            local_fs,
        },
        ModuleSource::Vfs {
            vfs_mod_root: vfs_mod_root.to_string(),
        },
    )
}

/// Compile a single-file Vo source, resolving third-party modules from the JS VFS.
pub fn compile_source_with_vfs(
    source: &str,
    filename: &str,
    vfs_mod_root: &str,
) -> Result<Vec<u8>, String> {
    compile_web(
        CompileSource::SingleFile {
            source: source.to_string(),
            filename: filename.to_string(),
        },
        ModuleSource::Vfs {
            vfs_mod_root: vfs_mod_root.to_string(),
        },
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
            }
            Ok(_) => panic!("expected single-file external import rejection"),
        }
    }
}
