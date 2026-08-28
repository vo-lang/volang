use std::collections::{BTreeSet, HashMap};
use std::ffi::OsStr;
use std::io::Cursor;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use sha2::{Digest, Sha256};
use vo_analysis::objects::PackageKey;
use vo_analysis::project::{PackageIdentity, Project as AnalysisProject};
use vo_analysis::vfs::{
    analyze_file_set_with_package_identity, package_identity_for_module_path,
    project_package_resolver_with_workspace_sources,
};
use vo_codegen::{
    compile_project_with_adapters, ExpressionEvaluatorSpec, ExternalizedLocalSpec, ScopedCallSpec,
};
use vo_common::vfs::{
    normalize_fs_path, FileSet, FileSystem, RealFs, ZipFs, MAX_ZIP_ARCHIVE_BYTES,
};
use vo_module::project::{
    ProjectContext, ProjectContextOptions, ProjectPlan, SingleFileContext, WorkspaceModule,
};
use vo_module::readiness::ReadyModule;
use vo_module::workspace::WorkspaceDiscovery;
use vo_stdlib::EmbeddedStdlib;
use vo_syntax::ast::{self, Expr, ExprId, Visitor};
use vo_ui_artifact::{
    ArtifactLimits, BundleLimits, COMPONENT_ARTIFACT_NAME, COMPONENT_ARTIFACT_VERSION,
    COMPONENT_BUNDLE_ARTIFACT_NAME, COMPONENT_BUNDLE_ARTIFACT_VERSION,
};
use vo_ui_compiler::{
    compile_project_ui, discover_project_ui_runtime, encode_ui_component_bundle_with_functions,
    encode_ui_program_with_functions, RuntimeCellKind, UI_MODULE_PATH,
};
use vo_ui_plan::PlanLimits;

use super::native::{
    check_materialized_dependency_readiness_with_fs,
    prepare_native_extension_specs_with_readiness_and_workspace,
};
use super::snapshot::{CompileInputSnapshot, ResolverFs};
use super::{
    CompileError, CompileOutput, ModuleSystemError, ModuleSystemErrorKind, ModuleSystemStage,
    COMPILE_PACKAGES_ARTIFACT_NAME, COMPILE_PACKAGES_ARTIFACT_VERSION,
};

// In-memory compilation rejects every host-backed dependency before analysis.
// This virtual root is carried only to keep the shared analysis/output types
// uniform; its resolver is an immutable empty snapshot and never touches the
// host filesystem.
const IN_MEMORY_MODULE_CACHE_ROOT: &str = ".vo-in-memory-module-cache";

struct PackageExpressionIndexer<'a> {
    package: PackageKey,
    expressions: &'a mut HashMap<(PackageKey, ExprId), Expr>,
}

impl Visitor for PackageExpressionIndexer<'_> {
    fn visit_expr(&mut self, expression: &Expr) {
        self.expressions
            .insert((self.package, expression.id), expression.clone());
        ast::walk_expr(self, expression);
    }
}

fn index_project_expressions(
    project: &AnalysisProject,
) -> Result<HashMap<(PackageKey, ExprId), Expr>, String> {
    let mut expressions = HashMap::new();
    {
        let mut indexer = PackageExpressionIndexer {
            package: project.main_package,
            expressions: &mut expressions,
        };
        for file in &project.files {
            indexer.visit_file(file);
        }
    }
    for (_, package, _, files) in project.imported_packages_in_order()? {
        let mut indexer = PackageExpressionIndexer {
            package,
            expressions: &mut expressions,
        };
        for file in files {
            indexer.visit_file(file);
        }
    }
    Ok(expressions)
}

struct PreparedProject<F> {
    fs: F,
    stdlib: Option<EmbeddedStdlib>,
    module_fs: ResolverFs,
    workspace_source_fs: ResolverFs,
    native_input_fs: ResolverFs,
    file_set: FileSet,
    source_root: PathBuf,
    local_root: PathBuf,
    mod_cache: PathBuf,
    workspace_sources: HashMap<String, PathBuf>,
    project_plan: ProjectPlan,
    current_module: Option<String>,
    current_package: Option<PackageIdentity>,
    ready_modules: Vec<ReadyModule>,
    workspace: super::WorkspaceCompileContext,
}

struct AnalyzedCompilation {
    project: AnalysisProject,
    source_root: PathBuf,
    mod_cache: PathBuf,
    locked_modules: Vec<vo_module::schema::lockfile::LockedModule>,
    ready_modules: Vec<ReadyModule>,
    workspace: super::WorkspaceCompileContext,
    native_input_fs: ResolverFs,
}

pub(super) struct ProjectCompileContext {
    pub(super) project_root: PathBuf,
    pub(super) mod_cache: PathBuf,
    pub(super) source_root: PathBuf,
    pub(super) package_dir: PathBuf,
    pub(super) single_file: Option<PathBuf>,
    pub(super) graph: super::ProjectGraphContext,
    pub(super) project_plan: ProjectPlan,
    /// Explicit module identity for ephemeral inline modules whose dependency
    /// context is intentionally otherwise empty.
    pub(super) current_module_override: Option<String>,
    pub(super) workspace_sources: HashMap<String, PathBuf>,
    pub(super) workspace: super::WorkspaceCompileContext,
}

pub(super) struct ProjectSnapshotInputs<'a> {
    pub(super) project_root: &'a Path,
    pub(super) mod_cache: &'a Path,
    pub(super) graph: &'a super::ProjectGraphContext,
    pub(super) project_plan: &'a ProjectPlan,
    pub(super) current_module_override: Option<&'a str>,
    pub(super) workspace_sources: &'a HashMap<String, PathBuf>,
    pub(super) workspace: &'a super::WorkspaceCompileContext,
}

impl<'a> From<&'a ProjectCompileContext> for ProjectSnapshotInputs<'a> {
    fn from(context: &'a ProjectCompileContext) -> Self {
        Self {
            project_root: &context.project_root,
            mod_cache: &context.mod_cache,
            graph: &context.graph,
            project_plan: &context.project_plan,
            current_module_override: context.current_module_override.as_deref(),
            workspace_sources: &context.workspace_sources,
            workspace: &context.workspace,
        }
    }
}

pub(super) struct PreparedProjectSnapshot {
    snapshot: Arc<CompileInputSnapshot>,
    project_plan: ProjectPlan,
    workspace_sources: HashMap<String, PathBuf>,
    ready_modules: Vec<ReadyModule>,
}

impl PreparedProjectSnapshot {
    pub(super) fn context_fs(&self) -> ResolverFs {
        ResolverFs::snapshot_global(Arc::clone(&self.snapshot))
    }

    pub(super) fn ready_modules(&self) -> &[ReadyModule] {
        &self.ready_modules
    }
}

fn compilation_identities(
    project_plan: &ProjectPlan,
    current_module_override: Option<&str>,
    package_dir: &Path,
) -> Result<(Option<String>, Option<PackageIdentity>), CompileError> {
    let declared_module = project_plan.current_module();
    if let (Some(override_module), Some(declared_module)) =
        (current_module_override, declared_module)
    {
        if override_module != declared_module {
            return Err(CompileError::Analysis(format!(
                "compile context module identity mismatch: {override_module} != {declared_module}"
            )));
        }
    }

    let current_module = current_module_override
        .or(declared_module)
        .map(str::to_string);
    let Some(module) = current_module.as_deref() else {
        return Ok((None, None));
    };

    let package_identity = package_identity_for_module_path(module, Path::new("."), package_dir)
        .map_err(|error| {
            CompileError::Analysis(format!("invalid current package identity: {error}"))
        })?;
    Ok((current_module, Some(package_identity)))
}

impl<F: FileSystem> PreparedProject<F> {
    fn load_memory_prepared(
        fs: F,
        context: ProjectCompileContext,
        empty_message: &'static str,
    ) -> Result<Self, CompileError> {
        reject_unfrozen_memory_inputs(&context)?;
        let empty_snapshot = Arc::new(CompileInputSnapshot::default());
        let module_fs = ResolverFs::snapshot(Arc::clone(&empty_snapshot), &context.mod_cache);
        let workspace_source_fs = ResolverFs::snapshot_global(empty_snapshot);
        Self::load_prepared_with_inputs(
            fs,
            context,
            empty_message,
            None,
            module_fs,
            workspace_source_fs,
            Vec::new(),
        )
    }

    fn load_prepared_with_inputs(
        fs: F,
        context: ProjectCompileContext,
        empty_message: &'static str,
        stdlib: Option<EmbeddedStdlib>,
        module_fs: ResolverFs,
        workspace_source_fs: ResolverFs,
        ready_modules: Vec<ReadyModule>,
    ) -> Result<Self, CompileError> {
        let (current_module, current_package) = compilation_identities(
            &context.project_plan,
            context.current_module_override.as_deref(),
            &context.package_dir,
        )?;
        let file_set = collect_file_set(
            &fs,
            &context.package_dir,
            context.single_file.as_deref(),
            context.project_root.clone(),
            empty_message,
        )?;
        let native_input_fs = workspace_source_fs.clone();
        Ok(Self {
            fs,
            stdlib,
            module_fs,
            workspace_source_fs,
            native_input_fs,
            file_set,
            source_root: context.source_root,
            local_root: PathBuf::from("."),
            mod_cache: context.mod_cache,
            workspace_sources: context.workspace_sources,
            project_plan: context.project_plan,
            current_module,
            current_package,
            ready_modules,
            workspace: context.workspace,
        })
    }

    fn analyze(self) -> Result<AnalyzedCompilation, CompileError> {
        let locked_modules = self.project_plan.locked_modules().to_vec();
        let resolver = project_package_resolver_with_workspace_sources(
            self.stdlib.unwrap_or_default(),
            self.module_fs,
            self.workspace_source_fs,
            &self.project_plan,
            self.workspace_sources,
        );
        let project = analyze_file_set_with_package_identity(
            self.file_set,
            resolver,
            self.fs,
            self.local_root,
            self.current_module,
            self.current_package,
        )
        .map_err(|e| CompileError::Analysis(format!("{}", e)))?;
        let imported_packages = project
            .imported_packages_in_order()
            .map_err(CompileError::Analysis)?
            .into_iter()
            .map(|(path, _, _, _)| path)
            .collect::<Vec<_>>();
        for ready in &self.ready_modules {
            ready
                .validate_import_capabilities(imported_packages.iter().copied())
                .map_err(|error| CompileError::Analysis(error.to_string()))?;
        }
        Ok(AnalyzedCompilation {
            project,
            source_root: self.source_root,
            mod_cache: self.mod_cache,
            locked_modules,
            ready_modules: self.ready_modules,
            workspace: self.workspace,
            native_input_fs: self.native_input_fs,
        })
    }

    fn check(self) -> Result<(), CompileError> {
        self.analyze()?.prepare_extensions_for_frozen_build()
    }

    fn compile(self) -> Result<CompileOutput, CompileError> {
        self.analyze()?.into_output()
    }
}

fn reject_unfrozen_memory_inputs(context: &ProjectCompileContext) -> Result<(), CompileError> {
    let mod_file = context.project_plan.mod_file();
    let has_external_dependencies = mod_file
        .is_some_and(|mod_file| !mod_file.dependencies.is_empty() || mod_file.extension.is_some())
        || context.project_plan.lock_file().is_some()
        || !context.project_plan.locked_modules().is_empty()
        || !context.workspace_sources.is_empty();
    if !has_external_dependencies {
        return Ok(());
    }
    Err(CompileError::ModuleSystem(
        ModuleSystemError::new(
            ModuleSystemStage::CompileInputs,
            ModuleSystemErrorKind::ValidationFailed,
            "in-memory compilation accepts only self-contained source graphs; external dependencies, workspace sources, and extension metadata require a real project path so every host input can be frozen",
        )
        .with_path(&context.project_root),
    ))
}

impl AnalyzedCompilation {
    fn prepare_extensions_for_frozen_build(&self) -> Result<(), CompileError> {
        prepare_native_extension_specs_with_readiness_and_workspace(
            &self.project.extensions,
            &self.ready_modules,
            &self.mod_cache,
            &self.workspace.options.workspace,
            Some(&self.native_input_fs),
        )
        .map_err(CompileError::ModuleSystem)?;
        Ok(())
    }

    fn into_output(self) -> Result<CompileOutput, CompileError> {
        let extensions = prepare_native_extension_specs_with_readiness_and_workspace(
            &self.project.extensions,
            &self.ready_modules,
            &self.mod_cache,
            &self.workspace.options.workspace,
            Some(&self.native_input_fs),
        )
        .map_err(CompileError::ModuleSystem)?;

        let module = compile_analyzed_project(&self.project)?;
        let module = vo_common_core::verifier::verify_loaded_module(module)
            .map(Arc::new)
            .map_err(|err| CompileError::Codegen(format!("generated invalid bytecode: {err}")))?;

        Ok(CompileOutput {
            module,
            source_root: self.source_root,
            extensions,
            locked_modules: self.locked_modules,
        })
    }
}

pub(super) fn compile_analyzed_project(
    project: &AnalysisProject,
) -> Result<vo_common_core::Module, CompileError> {
    let plan_limits = PlanLimits::default();
    let ui_program = compile_project_ui(project, plan_limits)
        .map_err(|error| CompileError::Codegen(error.to_string()))?;
    let ui_runtime_discovery = discover_project_ui_runtime(project)
        .map_err(|error| CompileError::Codegen(error.to_string()))?;
    let discovered_ui_states = ui_runtime_discovery
        .as_ref()
        .map(|discovery| discovery.state_bindings.as_slice())
        .unwrap_or_default();
    let ui_extern_name = |function| {
        vo_common_core::extern_key::ExternKeyRef::new(UI_MODULE_PATH, function)
            .encode()
            .map_err(|error| CompileError::Codegen(error.to_string()))
    };
    let evaluator_specs = ui_program
        .as_ref()
        .map(|program| {
            program
                .components
                .iter()
                .chain(std::iter::once(&program.root))
                .flat_map(|component| {
                    let state_object = |index: u32| component.state_bindings[index as usize].object;
                    let prop_object = |index: u16| {
                        component.props[usize::from(index)]
                            .expect("referenced component props have compiler object identity")
                    };
                    component
                        .state_bindings
                        .iter()
                        .filter_map(|state| {
                            state.initializer.map(|expression| ExpressionEvaluatorSpec {
                                package: component.package,
                                expression,
                                parameters: state
                                    .initializer_dependencies
                                    .iter()
                                    .map(|dependency| state_object(dependency.index()))
                                    .chain(
                                        state
                                            .initializer_prop_dependencies
                                            .iter()
                                            .map(|dependency| prop_object(*dependency)),
                                    )
                                    .collect(),
                            })
                        })
                        .chain(component.slot_bindings.iter().map(|binding| {
                            ExpressionEvaluatorSpec {
                                package: component.package,
                                expression: binding.expression,
                                parameters: binding
                                    .dependencies
                                    .iter()
                                    .map(|dependency| state_object(dependency.index()))
                                    .chain(
                                        binding
                                            .prop_dependencies
                                            .iter()
                                            .map(|dependency| prop_object(*dependency)),
                                    )
                                    .collect(),
                            }
                        }))
                        .chain(component.handler_bindings.iter().map(|handler| {
                            ExpressionEvaluatorSpec {
                                package: component.package,
                                expression: handler.expression,
                                parameters: handler
                                    .captured_state
                                    .iter()
                                    .map(|dependency| state_object(dependency.index()))
                                    .chain(
                                        handler
                                            .captured_props
                                            .iter()
                                            .map(|dependency| prop_object(*dependency)),
                                    )
                                    .collect(),
                            }
                        }))
                        .chain(component.component_calls.iter().flat_map(|call| {
                            call.props.iter().chain(call.key.iter()).map(|prop| {
                                ExpressionEvaluatorSpec {
                                    package: component.package,
                                    expression: prop.expression,
                                    parameters: prop
                                        .dependencies
                                        .iter()
                                        .map(|dependency| state_object(dependency.index()))
                                        .chain(
                                            prop.prop_dependencies
                                                .iter()
                                                .map(|dependency| prop_object(*dependency)),
                                        )
                                        .collect(),
                                }
                            })
                        }))
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    let scoped_calls = if let Some(discovery) = &ui_runtime_discovery {
        let expressions = index_project_expressions(project)
            .map_err(|error| CompileError::Codegen(error.to_string()))?;
        discovery
            .component_scopes
            .iter()
            .map(|scope| {
                let source = |expression: ExprId| {
                    expressions
                        .get(&(scope.package, expression))
                        .cloned()
                        .ok_or_else(|| {
                            CompileError::Codegen(format!(
                                "component call expression {:?} is missing from its source package",
                                expression
                            ))
                        })
                };
                Ok(ScopedCallSpec {
                    package: scope.package,
                    expression: scope.expression,
                    target: source(scope.target)?,
                    key: scope.key.map(source).transpose()?,
                    identity: scope.identity.clone(),
                    call_site: scope.call_site.value(),
                    enter_extern: ui_extern_name("runtimeEnterComponent")?,
                    exit_extern: ui_extern_name("runtimeExitComponent")?,
                    key_extern: scope
                        .key
                        .is_some()
                        .then(|| ui_extern_name("Key"))
                        .transpose()?,
                })
            })
            .collect::<Result<Vec<_>, CompileError>>()?
    } else {
        Vec::new()
    };
    let externalized_locals = {
        let states = ui_program.as_ref().map_or_else(
            || discovered_ui_states.iter().collect::<Vec<_>>(),
            |program| {
                program
                    .components
                    .iter()
                    .chain(std::iter::once(&program.root))
                    .flat_map(|component| component.state_bindings.iter())
                    .collect::<Vec<_>>()
            },
        );
        if ui_program.is_some() && states.iter().any(|state| state.runtime_cell.is_none()) {
            Vec::new()
        } else {
            states
                .iter()
                .filter(|state| state.automatic_cell)
                .map(|state| {
                    let (initialize, read, write) = match state
                        .runtime_cell
                        .expect("automatic cells have a supported runtime kind")
                    {
                        RuntimeCellKind::String => {
                            ("UseStringState", "StringStateValue", "SetStringState")
                        }
                        RuntimeCellKind::Bool => ("UseBoolState", "BoolStateValue", "SetBoolState"),
                        RuntimeCellKind::Int => ("UseIntState", "IntStateValue", "SetIntState"),
                        RuntimeCellKind::Float => {
                            ("UseFloatState", "FloatStateValue", "SetFloatState")
                        }
                    };
                    Ok(ExternalizedLocalSpec {
                        object: state.object,
                        initialize_extern: ui_extern_name(initialize)?,
                        read_extern: ui_extern_name(read)?,
                        write_extern: ui_extern_name(write)?,
                    })
                })
                .collect::<Result<Vec<_>, CompileError>>()?
        }
    };
    let (mut module, codegen_report) = compile_project_with_adapters(
        project,
        &evaluator_specs,
        &externalized_locals,
        &scoped_calls,
    )
    .map_err(|error| CompileError::Codegen(error.to_string()))?;
    if let Some(ui_program) = ui_program {
        if ui_program.root.component_calls.is_empty() && ui_program.components.is_empty() {
            let payload = encode_ui_program_with_functions(
                &ui_program,
                ArtifactLimits::default(),
                plan_limits,
                |expression| {
                    codegen_report.function_for_expression(ui_program.root.package, expression)
                },
            )
            .map_err(|error| CompileError::Codegen(error.to_string()))?;
            module.set_artifact(vo_common_core::ModuleArtifact::new(
                COMPONENT_ARTIFACT_NAME,
                COMPONENT_ARTIFACT_VERSION,
                payload,
            ));
        }
        let bundle_payload = encode_ui_component_bundle_with_functions(
            &ui_program,
            BundleLimits::default(),
            plan_limits,
            |package, expression| codegen_report.function_for_expression(package, expression),
        )
        .map_err(|error| CompileError::Codegen(error.to_string()))?;
        module.set_artifact(vo_common_core::ModuleArtifact::new(
            COMPONENT_BUNDLE_ARTIFACT_NAME,
            COMPONENT_BUNDLE_ARTIFACT_VERSION,
            bundle_payload,
        ));
    }
    let package_payload = project
        .packages
        .iter()
        .map(|package| project.tc_objs.pkgs[*package].path())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect::<Vec<_>>()
        .join("\n")
        .into_bytes();
    module.set_artifact(vo_common_core::ModuleArtifact::new(
        COMPILE_PACKAGES_ARTIFACT_NAME,
        COMPILE_PACKAGES_ARTIFACT_VERSION,
        package_payload,
    ));
    Ok(module)
}

fn invalid_bytecode_error(err: impl std::fmt::Display) -> CompileError {
    CompileError::Io(std::io::Error::new(
        std::io::ErrorKind::InvalidData,
        format!("invalid bytecode: {err}"),
    ))
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
    let bytes = super::host_input::read_stable_regular_file(
        path,
        vo_common_core::serialize::MAX_VOB_BYTES,
    )?;
    let module = vo_vm::bytecode::Module::deserialize(&bytes).map_err(|e| {
        CompileError::Io(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("{:?}", e),
        ))
    })?;
    let module = vo_common_core::verifier::verify_loaded_module(module)
        .map(Arc::new)
        .map_err(invalid_bytecode_error)?;
    Ok(CompileOutput {
        module,
        source_root: path.parent().unwrap_or(Path::new(".")).to_path_buf(),
        extensions: Vec::new(),
        locked_modules: Vec::new(),
    })
}

pub(super) fn compile_prepared_project<F: FileSystem>(
    fs: F,
    root: &Path,
    single_file: Option<&OsStr>,
) -> Result<CompileOutput, CompileError> {
    let mod_cache = PathBuf::from(IN_MEMORY_MODULE_CACHE_ROOT);
    let options = ProjectContextOptions::new(WorkspaceDiscovery::Disabled);

    // Single-file entries go through the spec §5.6 single-file classifier so
    // that inline `/*vo:mod ... */` metadata is recognized and the spec §5.6.4
    // precedence rules are enforced.
    if let Some(single_file_os) = single_file {
        let file_path = PathBuf::from(single_file_os);
        let ctx =
            vo_module::project::load_single_file_context_with_options(&fs, &file_path, &options)
                .map_err(super::module_system_error_from_project)?;
        return compile_from_single_file_context(fs, ctx, root, file_path, mod_cache);
    }

    let context = vo_module::project::load_project_context_with_options(&fs, root, &options)
        .map_err(super::module_system_error_from_project)?;
    let graph = super::ProjectGraphContext::from_project(&context);
    let (_, project_plan, workspace_sources) = context.into_parts();
    PreparedProject::load_memory_prepared(
        fs,
        ProjectCompileContext {
            project_root: root.to_path_buf(),
            mod_cache,
            source_root: root.to_path_buf(),
            package_dir: PathBuf::from("."),
            single_file: None,
            graph,
            project_plan,
            current_module_override: None,
            workspace_sources,
            workspace: super::WorkspaceCompileContext::disabled(),
        },
        "no .vo files found",
    )?
    .compile()
}

fn with_zip_project<T>(
    zip_path: &Path,
    internal_root: Option<&str>,
    operation: impl FnOnce(PreparedProject<ZipFs>) -> Result<T, CompileError>,
) -> Result<T, CompileError> {
    let archive =
        super::host_input::read_stable_regular_file_snapshot(zip_path, MAX_ZIP_ARCHIVE_BYTES)?;
    let archive_generation = archive.generation.clone();
    let archive_digest: [u8; 32] = Sha256::digest(&archive.bytes).into();
    let zip_fs = ZipFs::from_reader_with_root(
        Cursor::new(archive.bytes.as_slice()),
        internal_root.unwrap_or(""),
    )?;
    drop(archive);

    let archive_root = zip_path
        .canonicalize()
        .unwrap_or_else(|_| zip_path.to_path_buf());
    let virtual_root = Path::new(".");
    let options = ProjectContextOptions::new(WorkspaceDiscovery::Disabled);
    let context =
        vo_module::project::load_project_context_with_options(&zip_fs, virtual_root, &options)
            .map_err(super::module_system_error_from_project)?;
    let graph = super::ProjectGraphContext::from_project(&context);
    let (_, project_plan, workspace_sources) = context.into_parts();
    let project = PreparedProject::load_memory_prepared(
        zip_fs,
        ProjectCompileContext {
            project_root: archive_root.clone(),
            mod_cache: PathBuf::from(IN_MEMORY_MODULE_CACHE_ROOT),
            source_root: archive_root,
            package_dir: PathBuf::from("."),
            single_file: None,
            graph,
            project_plan,
            current_module_override: None,
            workspace_sources,
            workspace: super::WorkspaceCompileContext::disabled(),
        },
        "no .vo files found in zip",
    )?;
    let result = operation(project)?;

    let live_archive =
        super::host_input::read_stable_regular_file_snapshot(zip_path, MAX_ZIP_ARCHIVE_BYTES)?;
    let live_digest: [u8; 32] = Sha256::digest(&live_archive.bytes).into();
    if live_archive.generation != archive_generation || live_digest != archive_digest {
        return Err(CompileError::ModuleSystem(
            ModuleSystemError::new(
                ModuleSystemStage::CompileInputs,
                ModuleSystemErrorKind::Mismatch,
                "zip archive changed while its immutable compile snapshot was in use",
            )
            .with_path(zip_path),
        ));
    }

    Ok(result)
}

pub(super) fn compile_zip(
    zip_path: &Path,
    internal_root: Option<&str>,
) -> Result<CompileOutput, CompileError> {
    with_zip_project(zip_path, internal_root, PreparedProject::compile)
}

pub(super) fn check_zip(zip_path: &Path, internal_root: Option<&str>) -> Result<(), CompileError> {
    with_zip_project(zip_path, internal_root, PreparedProject::check)
}

pub(super) fn parse_zip_path(path: &str) -> Option<(String, Option<String>)> {
    if let Some((prefix, internal_root)) = path.rsplit_once(".zip:") {
        return Some((format!("{prefix}.zip"), Some(internal_root.to_string())));
    }
    path.ends_with(".zip").then(|| (path.to_string(), None))
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
    PreparedProject::load_memory_prepared(fs, project_context, "no .vo files found")?.compile()
}

fn single_file_context_to_project_compile_context(
    ctx: SingleFileContext,
    compile_root: &Path,
    file_path: PathBuf,
    mod_cache: PathBuf,
) -> Result<ProjectCompileContext, CompileError> {
    match ctx {
        SingleFileContext::Project(project_context) => {
            let graph = super::ProjectGraphContext::from_project(&project_context);
            let (_, project_plan, workspace_sources) = project_context.into_parts();
            let package_dir = file_path
                .parent()
                .filter(|parent| !parent.as_os_str().is_empty())
                .unwrap_or_else(|| Path::new("."))
                .to_path_buf();
            Ok(ProjectCompileContext {
                project_root: compile_root.to_path_buf(),
                mod_cache,
                source_root: compile_root.to_path_buf(),
                package_dir,
                single_file: Some(file_path),
                graph,
                project_plan,
                current_module_override: None,
                workspace_sources,
                workspace: super::WorkspaceCompileContext::disabled(),
            })
        }
        SingleFileContext::EphemeralInlineMod { inline_mod, .. } => {
            let current_module = inline_mod.module.as_str().to_string();
            Ok(ProjectCompileContext {
                project_root: compile_root.to_path_buf(),
                mod_cache,
                source_root: compile_root.to_path_buf(),
                package_dir: PathBuf::from("."),
                single_file: Some(file_path),
                graph: super::ProjectGraphContext::empty(),
                project_plan: ProjectPlan::default(),
                current_module_override: Some(current_module),
                workspace_sources: HashMap::new(),
                workspace: super::WorkspaceCompileContext::disabled(),
            })
        }
        SingleFileContext::AdHoc { .. } => Ok(ProjectCompileContext {
            project_root: compile_root.to_path_buf(),
            mod_cache,
            source_root: compile_root.to_path_buf(),
            package_dir: PathBuf::from("."),
            single_file: Some(file_path),
            graph: super::ProjectGraphContext::empty(),
            project_plan: ProjectPlan::default(),
            current_module_override: None,
            workspace_sources: HashMap::new(),
            workspace: super::WorkspaceCompileContext::disabled(),
        }),
    }
}

pub(super) fn compile_with_project_snapshot(
    context: ProjectCompileContext,
    stdlib: EmbeddedStdlib,
    snapshot: Arc<CompileInputSnapshot>,
) -> Result<CompileOutput, CompileError> {
    compile_with_project_snapshot_and_generated_inputs(context, stdlib, snapshot, &BTreeSet::new())
}

pub(super) fn compile_with_prepared_project_snapshot(
    context: ProjectCompileContext,
    stdlib: EmbeddedStdlib,
    prepared: PreparedProjectSnapshot,
) -> Result<CompileOutput, CompileError> {
    load_prepared_project_snapshot(context, stdlib, prepared)?.compile()
}

pub(super) fn compile_with_project_snapshot_and_generated_inputs(
    context: ProjectCompileContext,
    stdlib: EmbeddedStdlib,
    snapshot: Arc<CompileInputSnapshot>,
    generated_inputs: &BTreeSet<PathBuf>,
) -> Result<CompileOutput, CompileError> {
    load_project_from_snapshot(context, stdlib, snapshot, generated_inputs)?.compile()
}

pub(super) fn check_with_project_snapshot(
    context: ProjectCompileContext,
    stdlib: EmbeddedStdlib,
    snapshot: Arc<CompileInputSnapshot>,
) -> Result<(), CompileError> {
    load_project_from_snapshot(context, stdlib, snapshot, &BTreeSet::new())?.check()
}

fn load_project_from_snapshot(
    context: ProjectCompileContext,
    stdlib: EmbeddedStdlib,
    snapshot: Arc<CompileInputSnapshot>,
    generated_inputs: &BTreeSet<PathBuf>,
) -> Result<PreparedProject<ResolverFs>, CompileError> {
    let prepared = prepare_project_snapshot_with_generated_inputs(
        ProjectSnapshotInputs::from(&context),
        snapshot,
        generated_inputs,
    )?;
    load_prepared_project_snapshot(context, stdlib, prepared)
}

fn load_prepared_project_snapshot(
    mut context: ProjectCompileContext,
    stdlib: EmbeddedStdlib,
    prepared: PreparedProjectSnapshot,
) -> Result<PreparedProject<ResolverFs>, CompileError> {
    let PreparedProjectSnapshot {
        snapshot,
        project_plan,
        workspace_sources,
        ready_modules,
    } = prepared;
    context.project_plan = project_plan;
    context.workspace_sources = workspace_sources;
    let project_fs = ResolverFs::snapshot(Arc::clone(&snapshot), &context.project_root);
    let module_fs = ResolverFs::snapshot(Arc::clone(&snapshot), &context.mod_cache);
    let workspace_source_fs = ResolverFs::snapshot_global(snapshot);
    PreparedProject::load_prepared_with_inputs(
        project_fs,
        context,
        "no .vo files found",
        Some(stdlib),
        module_fs,
        workspace_source_fs,
        ready_modules,
    )
}

pub(super) fn prepare_project_snapshot(
    inputs: ProjectSnapshotInputs<'_>,
    snapshot: Arc<CompileInputSnapshot>,
) -> Result<PreparedProjectSnapshot, CompileError> {
    prepare_project_snapshot_with_generated_inputs(inputs, snapshot, &BTreeSet::new())
}

fn prepare_project_snapshot_with_generated_inputs(
    inputs: ProjectSnapshotInputs<'_>,
    snapshot: Arc<CompileInputSnapshot>,
    generated_inputs: &BTreeSet<PathBuf>,
) -> Result<PreparedProjectSnapshot, CompileError> {
    let context_fs = ResolverFs::snapshot_global(Arc::clone(&snapshot));
    let captured_context = validate_captured_project_context_with_generated_inputs(
        &context_fs,
        inputs.project_root,
        inputs.graph,
        inputs.project_plan,
        inputs.workspace_sources,
        inputs.current_module_override,
        inputs.workspace,
        generated_inputs,
    )?;
    let module_fs = ResolverFs::snapshot(Arc::clone(&snapshot), inputs.mod_cache);
    let ready_modules = prepare_materialized_modules(
        &module_fs,
        captured_context.project_plan(),
        captured_context.workspace_modules(),
    )?;
    let (_, project_plan, workspace_sources) = captured_context.into_parts();
    Ok(PreparedProjectSnapshot {
        snapshot,
        project_plan,
        workspace_sources,
        ready_modules,
    })
}

pub(super) fn prepare_materialized_modules<F: FileSystem>(
    module_fs: &F,
    project_plan: &ProjectPlan,
    workspace_modules: &[WorkspaceModule],
) -> Result<Vec<ReadyModule>, CompileError> {
    vo_module::readiness::validate_materialized_graph(module_fs, project_plan, workspace_modules)
        .map_err(|error| {
        CompileError::ModuleSystem(ModuleSystemError::new(
            ModuleSystemStage::CachedModule,
            ModuleSystemErrorKind::ValidationFailed,
            error.to_string(),
        ))
    })?;
    check_materialized_dependency_readiness_with_fs(module_fs, project_plan.locked_modules())
        .map_err(CompileError::ModuleSystem)
}

#[allow(clippy::too_many_arguments)]
fn validate_captured_project_context_with_generated_inputs<F: FileSystem>(
    snapshot_fs: &F,
    project_root: &Path,
    expected_graph: &super::ProjectGraphContext,
    expected: &ProjectPlan,
    workspace_sources: &HashMap<String, PathBuf>,
    current_module_override: Option<&str>,
    workspace: &super::WorkspaceCompileContext,
    generated_inputs: &BTreeSet<PathBuf>,
) -> Result<ProjectContext, CompileError> {
    let captured_context = vo_module::project::load_project_context_with_options(
        snapshot_fs,
        project_root,
        &workspace.options,
    )
    .map_err(captured_context_reload_error)?;

    if normalize_fs_path(captured_context.project_root()) != normalize_fs_path(project_root) {
        return Err(captured_context_mismatch(
            ModuleSystemStage::ModFile,
            "project root",
        ));
    }
    if normalized_optional_path(captured_context.workspace_file())
        != normalized_optional_path(workspace.file.as_deref())
    {
        return Err(captured_context_mismatch(
            ModuleSystemStage::Workspace,
            "vo.work provenance",
        ));
    }
    if !workspace_source_maps_match(
        snapshot_fs,
        captured_context.workspace_sources(),
        workspace_sources,
    )? {
        return Err(captured_context_mismatch(
            ModuleSystemStage::Workspace,
            "vo.work source map",
        ));
    }
    validate_captured_project_graph(&captured_context, expected_graph, generated_inputs)?;

    // Inline ephemeral dependencies live in a cache-local project, while this
    // filesystem is rooted beside the source file. Their typed dependency
    // context is fingerprinted separately. Here we enforce the classifier
    // invariant that a host vo.mod did not appear after inline selection.
    if current_module_override.is_some() {
        if captured_context.project_plan().has_mod_file() {
            return Err(captured_context_mismatch(
                ModuleSystemStage::ModFile,
                "vo.mod",
            ));
        }
        return Ok(captured_context);
    }

    let expected_mod = render_project_mod(expected)?;
    let captured_mod = render_project_mod(captured_context.project_plan())?;
    if expected_mod != captured_mod {
        return Err(captured_context_mismatch(
            ModuleSystemStage::ModFile,
            "vo.mod",
        ));
    }

    let expected_lock = render_project_lock(expected)?;
    let captured_lock = render_project_lock(captured_context.project_plan())?;
    if expected_lock != captured_lock {
        return Err(captured_context_mismatch(
            ModuleSystemStage::LockFile,
            "vo.lock",
        ));
    }
    Ok(captured_context)
}

fn validate_captured_project_graph(
    captured: &ProjectContext,
    expected: &super::ProjectGraphContext,
    generated_inputs: &BTreeSet<PathBuf>,
) -> Result<(), CompileError> {
    // Ephemeral/ad-hoc contexts have no ProjectContext graph. Their source
    // classification generation is validated during bounded input capture.
    if expected.project_metadata_generation.is_empty() {
        return Ok(());
    }
    if captured.authority() != expected.authority {
        return Err(captured_context_mismatch(
            ModuleSystemStage::Workspace,
            "project dependency authority",
        ));
    }
    if captured.project_metadata_generation() != expected.project_metadata_generation {
        return Err(captured_context_mismatch(
            ModuleSystemStage::ModFile,
            "project metadata generation",
        ));
    }
    if normalized_workspace_modules(captured.workspace_modules())?
        != normalized_workspace_modules(&expected.workspace_modules)?
    {
        return Err(captured_context_mismatch(
            ModuleSystemStage::Workspace,
            "workspace source graph",
        ));
    }
    let generated_inputs = generated_inputs
        .iter()
        .map(|path| normalize_fs_path(path))
        .collect::<BTreeSet<_>>();
    let expected_inputs = normalized_input_files(&expected.validated_input_files);
    let expected_input_set = expected_inputs.iter().cloned().collect::<BTreeSet<_>>();
    let captured_inputs = normalized_input_files(captured.validated_input_files())
        .into_iter()
        .filter(|path| expected_input_set.contains(path) || !generated_inputs.contains(path))
        .collect::<Vec<_>>();
    if captured_inputs != expected_inputs {
        return Err(project_input_closure_mismatch(
            &captured_inputs,
            &expected_inputs,
        ));
    }
    Ok(())
}

pub(super) fn validate_live_workspace_generation(
    project_root: &Path,
    workspace: &super::WorkspaceCompileContext,
) -> Result<(), CompileError> {
    if workspace.generation.is_empty() {
        return Ok(());
    }
    if workspace.file.is_none() {
        let live_workspace = vo_module::workspace::discover_workfile_in_with(
            &RealFs::new("."),
            project_root,
            &workspace.options.workspace,
        )
        .map_err(live_workspace_reload_error)?;
        if live_workspace.is_some() {
            return Err(captured_context_mismatch(
                ModuleSystemStage::Workspace,
                "workspace directory generation",
            ));
        }
        return Ok(());
    }
    let live_context = vo_module::project::load_project_context_with_options(
        &RealFs::new("."),
        project_root,
        &workspace.options,
    )
    .map_err(captured_context_reload_error)?;
    if live_context.workspace_generation() != workspace.generation {
        return Err(captured_context_mismatch(
            ModuleSystemStage::Workspace,
            "workspace directory generation",
        ));
    }
    Ok(())
}

fn live_workspace_reload_error(error: vo_module::Error) -> CompileError {
    let kind = match &error {
        vo_module::Error::Io(_) => ModuleSystemErrorKind::ReadFailed,
        vo_module::Error::WorkFileParse(_) => ModuleSystemErrorKind::ParseFailed,
        _ => ModuleSystemErrorKind::ValidationFailed,
    };
    CompileError::ModuleSystem(ModuleSystemError::new(
        ModuleSystemStage::Workspace,
        kind,
        format!("live workspace generation cannot be reloaded: {error}"),
    ))
}

fn captured_context_reload_error(error: vo_module::project::ProjectPlanError) -> CompileError {
    let stage = match error.stage() {
        vo_module::project::ProjectPlanStage::Workspace => ModuleSystemStage::Workspace,
        vo_module::project::ProjectPlanStage::ModFile => ModuleSystemStage::ModFile,
        vo_module::project::ProjectPlanStage::LockFile => ModuleSystemStage::LockFile,
    };
    CompileError::ModuleSystem(ModuleSystemError::new(
        stage,
        ModuleSystemErrorKind::Mismatch,
        format!(
            "captured project context cannot be reloaded from one metadata generation: {error}"
        ),
    ))
}

fn normalized_optional_path(path: Option<&Path>) -> Option<PathBuf> {
    path.map(normalize_fs_path)
}

fn workspace_source_maps_match<F: FileSystem>(
    snapshot_fs: &F,
    captured: &HashMap<String, PathBuf>,
    expected: &HashMap<String, PathBuf>,
) -> Result<bool, CompileError> {
    if captured.len() != expected.len() {
        return Ok(false);
    }
    for (module, expected_path) in expected {
        let Some(captured_path) = captured.get(module) else {
            return Ok(false);
        };
        let captured_path = normalize_fs_path(captured_path);
        let expected_path = normalize_fs_path(expected_path);
        if captured_path == expected_path {
            continue;
        }
        let captured_identity = snapshot_workspace_directory_identity(snapshot_fs, &captured_path)?;
        let expected_identity = snapshot_workspace_directory_identity(snapshot_fs, &expected_path)?;
        if captured_identity.is_none() || captured_identity != expected_identity {
            return Ok(false);
        }
    }
    Ok(true)
}

fn snapshot_workspace_directory_identity<F: FileSystem>(
    snapshot_fs: &F,
    path: &Path,
) -> Result<Option<Vec<u8>>, CompileError> {
    let identity = snapshot_fs
        .opaque_directory_identity(path)
        .map_err(|error| {
            CompileError::ModuleSystem(ModuleSystemError::new(
                ModuleSystemStage::Workspace,
                ModuleSystemErrorKind::ReadFailed,
                format!(
                    "failed to read captured workspace source directory identity for {}: {error}",
                    path.display()
                ),
            ))
        })?;
    if identity
        .as_ref()
        .is_some_and(|identity| identity.is_empty() || identity.len() > 64)
    {
        return Err(CompileError::ModuleSystem(ModuleSystemError::new(
            ModuleSystemStage::Workspace,
            ModuleSystemErrorKind::ValidationFailed,
            format!(
                "captured workspace source directory {} has an invalid identity",
                path.display()
            ),
        )));
    }
    Ok(identity)
}

fn normalized_workspace_modules(
    modules: &[WorkspaceModule],
) -> Result<Vec<(String, PathBuf, String)>, CompileError> {
    let mut entries = modules
        .iter()
        .map(|module| {
            let declaration = module.mod_file().render().map_err(|error| {
                CompileError::ModuleSystem(ModuleSystemError::new(
                    ModuleSystemStage::Workspace,
                    ModuleSystemErrorKind::ValidationFailed,
                    format!(
                        "failed to render authorized workspace manifest for {}: {error}",
                        module.module()
                    ),
                ))
            })?;
            Ok((
                module.module().as_str().to_string(),
                normalize_fs_path(module.directory()),
                declaration,
            ))
        })
        .collect::<Result<Vec<_>, CompileError>>()?;
    entries.sort();
    Ok(entries)
}

fn normalized_input_files(paths: &[PathBuf]) -> Vec<PathBuf> {
    let mut entries = paths
        .iter()
        .map(|path| normalize_fs_path(path))
        .collect::<Vec<_>>();
    entries.sort();
    entries.dedup();
    entries
}

fn render_project_mod(project_plan: &ProjectPlan) -> Result<Option<String>, CompileError> {
    project_plan
        .mod_file()
        .map(|mod_file| mod_file.render())
        .transpose()
        .map_err(|error| {
            CompileError::ModuleSystem(ModuleSystemError::new(
                ModuleSystemStage::ModFile,
                ModuleSystemErrorKind::ValidationFailed,
                format!("failed to render loaded vo.mod metadata: {error}"),
            ))
        })
}

fn render_project_lock(project_plan: &ProjectPlan) -> Result<Option<String>, CompileError> {
    project_plan
        .lock_file()
        .map(|lock_file| lock_file.render())
        .transpose()
        .map_err(|error| {
            CompileError::ModuleSystem(ModuleSystemError::new(
                ModuleSystemStage::LockFile,
                ModuleSystemErrorKind::ValidationFailed,
                format!("failed to render loaded vo.lock metadata: {error}"),
            ))
        })
}

fn captured_context_mismatch(stage: ModuleSystemStage, file: &str) -> CompileError {
    CompileError::ModuleSystem(ModuleSystemError::new(
        stage,
        ModuleSystemErrorKind::Mismatch,
        format!(
            "captured {file} does not match the project context loaded before snapshot capture; retry the build after concurrent project metadata updates finish"
        ),
    ))
}

fn project_input_closure_mismatch(captured: &[PathBuf], expected: &[PathBuf]) -> CompileError {
    let captured = captured.iter().collect::<BTreeSet<_>>();
    let expected = expected.iter().collect::<BTreeSet<_>>();
    let added = captured
        .difference(&expected)
        .take(4)
        .map(|path| path.display().to_string())
        .collect::<Vec<_>>();
    let missing = expected
        .difference(&captured)
        .take(4)
        .map(|path| path.display().to_string())
        .collect::<Vec<_>>();
    CompileError::ModuleSystem(ModuleSystemError::new(
        ModuleSystemStage::Workspace,
        ModuleSystemErrorKind::Mismatch,
        format!(
            "captured project authority input closure does not match the project context loaded before snapshot capture (added: {added:?}; missing: {missing:?}); retry the build after concurrent project metadata updates finish"
        ),
    ))
}

#[cfg(test)]
mod workspace_source_map_tests {
    use super::*;
    use std::io;

    #[derive(Default)]
    struct IdentityFs {
        identities: HashMap<PathBuf, Vec<u8>>,
    }

    impl FileSystem for IdentityFs {
        fn read_file(&self, path: &Path) -> io::Result<String> {
            Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("{} is unavailable", path.display()),
            ))
        }

        fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
            Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("{} is unavailable", path.display()),
            ))
        }

        fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
            Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("{} is unavailable", path.display()),
            ))
        }

        fn exists(&self, path: &Path) -> bool {
            self.identities.contains_key(&normalize_fs_path(path))
        }

        fn is_dir(&self, path: &Path) -> bool {
            self.exists(path)
        }

        fn opaque_directory_identity(&self, path: &Path) -> io::Result<Option<Vec<u8>>> {
            Ok(self.identities.get(&normalize_fs_path(path)).cloned())
        }
    }

    fn source_map(path: &str) -> HashMap<String, PathBuf> {
        HashMap::from([("github.com/vo-lang/ui".to_string(), PathBuf::from(path))])
    }

    #[test]
    fn workspace_source_map_accepts_authenticated_path_aliases() {
        let mut fs = IdentityFs::default();
        fs.identities
            .insert(PathBuf::from("C:/RUNNER~1/workspace/ui"), vec![7; 24]);
        fs.identities.insert(
            PathBuf::from("C:/Users/runneradmin/workspace/ui"),
            vec![7; 24],
        );

        assert!(workspace_source_maps_match(
            &fs,
            &source_map("C:/RUNNER~1/workspace/ui"),
            &source_map("C:/Users/runneradmin/workspace/ui"),
        )
        .unwrap());
    }

    #[test]
    fn workspace_source_map_rejects_unauthenticated_path_changes() {
        let mut fs = IdentityFs::default();
        fs.identities
            .insert(PathBuf::from("C:/RUNNER~1/workspace/ui"), vec![7; 24]);
        fs.identities.insert(
            PathBuf::from("C:/Users/runneradmin/workspace/ui"),
            vec![8; 24],
        );

        assert!(!workspace_source_maps_match(
            &fs,
            &source_map("C:/RUNNER~1/workspace/ui"),
            &source_map("C:/Users/runneradmin/workspace/ui"),
        )
        .unwrap());
    }
}
