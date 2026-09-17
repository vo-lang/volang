//! Typed UI compilation and artifact attachment.
use std::collections::HashMap;
use vo_analysis::objects::PackageKey;
use vo_analysis::project::Project as AnalysisProject;
use vo_codegen::{
    compile_project_with_adapters, ExpressionEvaluatorSpec, ExternalizedLocalSpec, ScopedCallSpec,
};
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

fn index_project_expressions(project: &AnalysisProject) -> HashMap<(PackageKey, ExprId), Expr> {
    let mut expressions = HashMap::new();
    for package in project.packages() {
        let mut indexer = PackageExpressionIndexer {
            package: package.key,
            expressions: &mut expressions,
        };
        for file in &package.files {
            indexer.visit_file(file);
        }
    }
    expressions
}

pub fn compile_project(project: &AnalysisProject) -> Result<vo_common_core::Module, String> {
    let plan_limits = PlanLimits::default();
    let ui_program = compile_project_ui(project, plan_limits).map_err(|error| error.to_string())?;
    let ui_runtime_discovery =
        discover_project_ui_runtime(project).map_err(|error| error.to_string())?;
    let discovered_ui_states = ui_runtime_discovery
        .as_ref()
        .map(|discovery| discovery.state_bindings.as_slice())
        .unwrap_or_default();
    let ui_extern_name = |function| {
        vo_common_core::extern_key::ExternKeyRef::new(UI_MODULE_PATH, function)
            .encode()
            .map_err(|error| error.to_string())
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
        let expressions = index_project_expressions(project);
        discovery
            .component_scopes
            .iter()
            .map(|scope| {
                let source = |expression: ExprId| {
                    expressions
                        .get(&(scope.package, expression))
                        .cloned()
                        .ok_or_else(|| {
                            format!(
                                "component call expression {:?} is missing from its source package",
                                expression
                            )
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
            .collect::<Result<Vec<_>, String>>()?
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
                .collect::<Result<Vec<_>, String>>()?
        }
    };
    let (mut module, codegen_report) = compile_project_with_adapters(
        project,
        &evaluator_specs,
        &externalized_locals,
        &scoped_calls,
    )
    .map_err(|error| error.to_string())?;
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
            .map_err(|error| error.to_string())?;
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
        .map_err(|error| error.to_string())?;
        module.set_artifact(vo_common_core::ModuleArtifact::new(
            COMPONENT_BUNDLE_ARTIFACT_NAME,
            COMPONENT_BUNDLE_ARTIFACT_VERSION,
            bundle_payload,
        ));
    }
    Ok(module)
}
