use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use vo_analysis::project::Project as AnalysisProject;
use vo_analysis::vfs::{CurrentModuleResolver, ReplacingResolver};
use vo_analysis::analyze_project;
use vo_codegen::compile_project;
use vo_common::vfs::{FileSet, FileSystem, ZipFs};
use vo_module::project::ProjectDeps;

use super::native::{prepare_extension_manifests_for_frozen_build, validate_locked_modules_installed};
use super::project_prepare::{load_project_deps_for_engine, read_all_replaces, replacing_resolver};
use super::{CompileError, CompileOutput};

struct PreparedProject<F> {
    fs: F,
    file_set: FileSet,
    source_root: PathBuf,
    mod_cache: PathBuf,
    replaces: HashMap<String, PathBuf>,
    project_deps: ProjectDeps,
}

struct AnalyzedCompilation {
    project: AnalysisProject,
    source_root: PathBuf,
    mod_cache: PathBuf,
    locked_modules: Vec<vo_module::schema::lockfile::LockedModule>,
}

impl<F: FileSystem> PreparedProject<F> {
    fn load(
        fs: F,
        source_root: PathBuf,
        single_file: Option<&OsStr>,
        replaces: HashMap<String, PathBuf>,
        empty_message: &'static str,
    ) -> Result<Self, CompileError> {
        let file_set = collect_file_set(&fs, &source_root, single_file, empty_message)?;
        let project_deps = load_project_deps_for_engine(&fs, &replaces)?;
        let mod_cache = super::default_mod_cache_root();
        validate_locked_modules_installed(&project_deps.locked_modules, &mod_cache)?;
        Ok(Self {
            fs,
            file_set,
            source_root,
            mod_cache,
            replaces,
            project_deps,
        })
    }

    fn analyze(self) -> Result<AnalyzedCompilation, CompileError> {
        let current_module = self.project_deps.current_module.clone();
        let locked_modules = self.project_deps.locked_modules.clone();
        let resolver = build_current_module_resolver(self.fs, &self.project_deps, self.replaces, current_module);
        let project = analyze_project(self.file_set, &resolver)
            .map_err(|e| CompileError::Analysis(format!("{}", e)))?;
        Ok(AnalyzedCompilation {
            project,
            source_root: self.source_root,
            mod_cache: self.mod_cache,
            locked_modules,
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
        prepare_extension_manifests_for_frozen_build(
            &self.project.extensions,
            &self.locked_modules,
            &self.mod_cache,
        )
        .map_err(CompileError::ModuleSystem)?;
        Ok(())
    }

    fn into_output(self) -> Result<CompileOutput, CompileError> {
        let extensions = prepare_extension_manifests_for_frozen_build(
            &self.project.extensions,
            &self.locked_modules,
            &self.mod_cache,
        )
        .map_err(CompileError::ModuleSystem)?;

        let module = compile_project(&self.project)
            .map_err(|e| CompileError::Codegen(format!("{}", e)))?;

        Ok(CompileOutput {
            module,
            source_root: self.source_root,
            extensions,
            locked_modules: self.locked_modules,
        })
    }
}

fn build_current_module_resolver<F: FileSystem>(
    fs: F,
    project_deps: &ProjectDeps,
    replaces: HashMap<String, PathBuf>,
    current_module: Option<String>,
) -> CurrentModuleResolver<ReplacingResolver<vo_analysis::vfs::PackageResolverMixed<vo_stdlib::EmbeddedStdlib, vo_common::vfs::RealFs>>, F> {
    let replaced = replacing_resolver(project_deps, replaces);
    CurrentModuleResolver::new(replaced, fs, current_module)
}

fn collect_file_set<F: FileSystem>(
    fs: &F,
    root: &Path,
    single_file: Option<&OsStr>,
    empty_message: &'static str,
) -> Result<FileSet, CompileError> {
    let file_set = if let Some(file_name) = single_file {
        FileSet::from_file(fs, Path::new(file_name), root.to_path_buf())?
    } else {
        FileSet::collect(fs, Path::new("."), root.to_path_buf())?
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

pub(super) fn check_with_fs<F: FileSystem>(
    fs: F,
    root: &Path,
    single_file: Option<&OsStr>,
) -> Result<(), CompileError> {
    let replaces = read_all_replaces(root)?;
    PreparedProject::load(fs, root.to_path_buf(), single_file, replaces, "no .vo files found")?.check()
}

pub(super) fn compile_with_fs<F: FileSystem>(
    fs: F,
    root: &Path,
    single_file: Option<&OsStr>,
) -> Result<CompileOutput, CompileError> {
    let replaces = read_all_replaces(root)?;
    PreparedProject::load(fs, root.to_path_buf(), single_file, replaces, "no .vo files found")?.compile()
}

pub(super) fn compile_zip(
    zip_path: &Path,
    internal_root: Option<&str>,
) -> Result<CompileOutput, CompileError> {
    let zip_fs = match internal_root {
        Some(root) => ZipFs::from_path_with_root(zip_path, root),
        None => ZipFs::from_path(zip_path),
    }?;

    let abs_root = zip_path.canonicalize().unwrap_or_else(|_| zip_path.to_path_buf());
    PreparedProject::load(
        zip_fs,
        abs_root,
        None,
        HashMap::new(),
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
