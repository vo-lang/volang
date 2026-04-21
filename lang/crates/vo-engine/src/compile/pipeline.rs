use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use vo_analysis::project::Project as AnalysisProject;
use vo_analysis::vfs::{
    analyze_file_set_with_current_module, project_package_resolver_with_replaces,
};
use vo_codegen::compile_project;
use vo_common::vfs::{FileSet, FileSystem, RealFs, ZipFs};
use vo_module::inline_mod::InlineMod;
use vo_module::project::{ProjectDeps, SingleFileContext};
use vo_module::readiness::ReadyModule;
use vo_stdlib::EmbeddedStdlib;

use super::native::{
    check_frozen_dependency_readiness, prepare_native_extension_specs_with_readiness,
};
use super::{
    CompileError, CompileOutput, ModuleSystemError, ModuleSystemErrorKind, ModuleSystemStage,
};

struct PreparedProject<F> {
    fs: F,
    file_set: FileSet,
    source_root: PathBuf,
    local_root: PathBuf,
    mod_cache: PathBuf,
    replaces: HashMap<String, PathBuf>,
    project_deps: ProjectDeps,
    ready_modules: Vec<ReadyModule>,
}

struct AnalyzedCompilation {
    project: AnalysisProject,
    source_root: PathBuf,
    mod_cache: PathBuf,
    locked_modules: Vec<vo_module::schema::lockfile::LockedModule>,
    ready_modules: Vec<ReadyModule>,
}

pub(super) struct ProjectCompileContext {
    pub(super) project_root: PathBuf,
    pub(super) mod_cache: PathBuf,
    pub(super) source_root: PathBuf,
    pub(super) package_dir: PathBuf,
    pub(super) single_file: Option<PathBuf>,
    pub(super) project_deps: ProjectDeps,
    pub(super) replaces: HashMap<String, PathBuf>,
}

impl<F: FileSystem> PreparedProject<F> {
    fn load_prepared(
        fs: F,
        context: ProjectCompileContext,
        empty_message: &'static str,
    ) -> Result<Self, CompileError> {
        let file_set = collect_file_set(
            &fs,
            &context.package_dir,
            context.single_file.as_deref(),
            context.project_root.clone(),
            empty_message,
        )?;
        let ready_modules = check_frozen_dependency_readiness(
            context.project_deps.locked_modules(),
            &context.mod_cache,
        )
        .map_err(CompileError::ModuleSystem)?;
        Ok(Self {
            fs,
            file_set,
            source_root: context.source_root,
            local_root: PathBuf::from("."),
            mod_cache: context.mod_cache,
            replaces: context.replaces,
            project_deps: context.project_deps,
            ready_modules,
        })
    }

    fn analyze(self) -> Result<AnalyzedCompilation, CompileError> {
        let current_module = self.project_deps.current_module().map(str::to_string);
        let locked_modules = self.project_deps.locked_modules().to_vec();
        let resolver = project_package_resolver_with_replaces(
            EmbeddedStdlib::new(),
            RealFs::new(&self.mod_cache),
            RealFs::new("."),
            &self.project_deps,
            self.replaces,
        );
        let project = analyze_file_set_with_current_module(
            self.file_set,
            resolver,
            self.fs,
            self.local_root,
            current_module,
        )
        .map_err(|e| CompileError::Analysis(format!("{}", e)))?;
        Ok(AnalyzedCompilation {
            project,
            source_root: self.source_root,
            mod_cache: self.mod_cache,
            locked_modules,
            ready_modules: self.ready_modules,
        })
    }

    fn check(self) -> Result<(), CompileError> {
        self.analyze()?.prepare_extensions_for_frozen_build()
    }

    fn compile(self) -> Result<CompileOutput, CompileError> {
        self.analyze()?.into_output()
    }
}

impl AnalyzedCompilation {
    fn prepare_extensions_for_frozen_build(&self) -> Result<(), CompileError> {
        prepare_native_extension_specs_with_readiness(
            &self.project.extensions,
            &self.ready_modules,
            &self.mod_cache,
        )
        .map_err(CompileError::ModuleSystem)?;
        Ok(())
    }

    fn into_output(self) -> Result<CompileOutput, CompileError> {
        let extensions = prepare_native_extension_specs_with_readiness(
            &self.project.extensions,
            &self.ready_modules,
            &self.mod_cache,
        )
        .map_err(CompileError::ModuleSystem)?;

        let module =
            compile_project(&self.project).map_err(|e| CompileError::Codegen(format!("{}", e)))?;

        Ok(CompileOutput {
            module,
            source_root: self.source_root,
            extensions,
            locked_modules: self.locked_modules,
        })
    }
}

fn collect_file_set<F: FileSystem>(
    fs: &F,
    dir: &Path,
    single_file: Option<&Path>,
    abs_root: PathBuf,
    empty_message: &'static str,
) -> Result<FileSet, CompileError> {
    let file_set = if let Some(file_path) = single_file {
        FileSet::from_file(fs, file_path, abs_root)?
    } else {
        FileSet::collect(fs, dir, abs_root)?
    };

    if file_set.files.is_empty() {
        return Err(CompileError::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            empty_message,
        )));
    }

    Ok(file_set)
}

pub(super) fn source_root(path: &Path) -> PathBuf {
    if path.is_dir() {
        path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
    } else {
        path.canonicalize()
            .unwrap_or_else(|_| path.to_path_buf())
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf()
    }
}

pub(super) fn load_bytecode(path: &Path) -> Result<CompileOutput, CompileError> {
    let bytes = fs::read(path)?;
    let module = vo_vm::bytecode::Module::deserialize(&bytes).map_err(|e| {
        CompileError::Io(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("{:?}", e),
        ))
    })?;
    Ok(CompileOutput {
        module,
        source_root: path.parent().unwrap_or(Path::new(".")).to_path_buf(),
        extensions: Vec::new(),
        locked_modules: Vec::new(),
    })
}

pub(super) fn check_with_project_context<F: FileSystem>(
    fs: F,
    context: ProjectCompileContext,
) -> Result<(), CompileError> {
    PreparedProject::load_prepared(fs, context, "no .vo files found")?.check()
}

pub(super) fn compile_prepared_project<F: FileSystem>(
    fs: F,
    root: &Path,
    single_file: Option<&OsStr>,
) -> Result<CompileOutput, CompileError> {
    let mod_cache = super::default_mod_cache_root();

    // Single-file entries go through the spec §5.6 single-file classifier so
    // that inline `/*vo:mod ... */` metadata is recognized and the spec §5.6.4
    // precedence rules are enforced.
    if let Some(single_file_os) = single_file {
        let file_path = PathBuf::from(single_file_os);
        let ctx = vo_module::project::load_single_file_context(&fs, &file_path)
            .map_err(super::module_system_error_from_project)?;
        return compile_from_single_file_context(fs, ctx, root, file_path, mod_cache);
    }

    let context = vo_module::project::load_project_context(&fs, root)
        .map_err(super::module_system_error_from_project)?;
    let (_, project_deps, workspace_replaces) = context.into_parts();
    PreparedProject::load_prepared(
        fs,
        ProjectCompileContext {
            project_root: root.to_path_buf(),
            mod_cache,
            source_root: root.to_path_buf(),
            package_dir: PathBuf::from("."),
            single_file: None,
            project_deps,
            replaces: workspace_replaces,
        },
        "no .vo files found",
    )?
    .compile()
}

fn compile_from_single_file_context<F: FileSystem>(
    fs: F,
    ctx: SingleFileContext,
    compile_root: &Path,
    file_path: PathBuf,
    mod_cache: PathBuf,
) -> Result<CompileOutput, CompileError> {
    let project_context =
        single_file_context_to_project_compile_context(ctx, compile_root, file_path, mod_cache)?;
    PreparedProject::load_prepared(fs, project_context, "no .vo files found")?.compile()
}

fn single_file_context_to_project_compile_context(
    ctx: SingleFileContext,
    compile_root: &Path,
    file_path: PathBuf,
    mod_cache: PathBuf,
) -> Result<ProjectCompileContext, CompileError> {
    match ctx {
        SingleFileContext::Project(project_context) => {
            let (_, project_deps, workspace_replaces) = project_context.into_parts();
            Ok(ProjectCompileContext {
                project_root: compile_root.to_path_buf(),
                mod_cache,
                source_root: compile_root.to_path_buf(),
                package_dir: PathBuf::from("."),
                single_file: Some(file_path),
                project_deps,
                replaces: workspace_replaces,
            })
        }
        SingleFileContext::EphemeralInlineMod { inline_mod, .. } => {
            ensure_inline_mod_ephemeral_build_is_supported(&inline_mod)?;
            Ok(ProjectCompileContext {
                project_root: compile_root.to_path_buf(),
                mod_cache,
                source_root: compile_root.to_path_buf(),
                package_dir: PathBuf::from("."),
                single_file: Some(file_path),
                project_deps: ProjectDeps::default(),
                replaces: HashMap::new(),
            })
        }
        SingleFileContext::AdHoc { .. } => Ok(ProjectCompileContext {
            project_root: compile_root.to_path_buf(),
            mod_cache,
            source_root: compile_root.to_path_buf(),
            package_dir: PathBuf::from("."),
            single_file: Some(file_path),
            project_deps: ProjectDeps::default(),
            replaces: HashMap::new(),
        }),
    }
}

/// Reject ephemeral single-file modules whose inline mod declares external
/// `require` entries, until ephemeral dependency resolution is wired up.
///
/// Spec §10.2 requires a resolved graph for every frozen build; because the
/// toolchain does not yet materialize a cache-local ephemeral lock (spec
/// §5.6.5), we fail fast with a diagnostic instead of silently compiling
/// against stdlib only.
pub(super) fn ensure_inline_mod_ephemeral_build_is_supported(
    inline: &InlineMod,
) -> Result<(), CompileError> {
    if inline.require.is_empty() {
        return Ok(());
    }
    Err(CompileError::ModuleSystem(ModuleSystemError::new(
        ModuleSystemStage::ModFile,
        ModuleSystemErrorKind::Missing,
        format!(
            "inline '/*vo:mod*/' declares {} require entry(ies), but ephemeral dependency \
             resolution is not yet implemented in this toolchain; remove the 'require' lines \
             or promote the script to a project with vo.mod",
            inline.require.len()
        ),
    )))
}

pub(super) fn compile_with_project_context<F: FileSystem>(
    fs: F,
    context: ProjectCompileContext,
) -> Result<CompileOutput, CompileError> {
    PreparedProject::load_prepared(fs, context, "no .vo files found")?.compile()
}

pub(super) fn compile_zip(
    zip_path: &Path,
    internal_root: Option<&str>,
) -> Result<CompileOutput, CompileError> {
    let zip_fs = match internal_root {
        Some(root) => ZipFs::from_path_with_root(zip_path, root),
        None => ZipFs::from_path(zip_path),
    }?;

    let abs_root = zip_path
        .canonicalize()
        .unwrap_or_else(|_| zip_path.to_path_buf());
    let mod_cache = super::default_mod_cache_root();
    let project_deps = vo_module::project::read_project_deps(&zip_fs, &[])
        .map_err(super::module_system_error_from_project)?;
    PreparedProject::load_prepared(
        zip_fs,
        ProjectCompileContext {
            project_root: abs_root.clone(),
            mod_cache,
            source_root: abs_root,
            package_dir: PathBuf::from("."),
            single_file: None,
            project_deps,
            replaces: HashMap::new(),
        },
        "no .vo files found in zip",
    )?
    .compile()
}

pub(super) fn parse_zip_path(path: &str) -> Option<(String, Option<String>)> {
    if path.ends_with(".zip") {
        Some((path.to_string(), None))
    } else if path.contains(".zip:") {
        let parts: Vec<&str> = path.splitn(2, ".zip:").collect();
        if parts.len() == 2 {
            Some((format!("{}.zip", parts[0]), Some(parts[1].to_string())))
        } else {
            None
        }
    } else {
        None
    }
}
