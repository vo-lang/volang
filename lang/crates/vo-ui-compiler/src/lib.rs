use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt;

use sha2::{Digest, Sha256};
use vo_analysis::check::TypeInfo;
use vo_analysis::objects::{ObjKey, PackageKey};
use vo_analysis::typ::{deep_underlying_type, BasicType, Type};
use vo_analysis::Project;
use vo_common::Span;
use vo_syntax::ast::{
    self, Block, CallExpr, Decl, Expr, ExprId, ExprKind, FuncDecl, StmtKind, Visitor,
};
use vo_ui_artifact::{
    encode_component_artifact, encode_component_bundle, ArtifactError, ArtifactLimits,
    BindingDefinition, BindingId, BundleError, BundleLimits, BundleSourceMetadata,
    ComponentArtifact, ComponentBundle, ComponentCallMode, ComponentCallSite, ComponentCallSiteId,
    ComponentDefinition, ComponentInterface, ComponentTypeId, ExecutionMode, HandlerArtifact,
    HandlerDefinition, HandlerSiteId, LifecycleDefinition, SlotArtifact, StateArtifact,
    StateFieldDefinition, StateFieldId, StateValueKind, COMPONENT_BUNDLE_ABI_VERSION,
};
use vo_ui_core::{
    EventType, HandlerId, Listener, ListenerOptions, Primitive, Property, PropertyId,
};
use vo_ui_plan::{
    ComponentPlan, LocalNodeId, PlanError, PlanLimits, SlotId, SlotKind, TemplateNode,
    TemplateNodeKind, UpdateSite, ValidatedPlan,
};
use vo_ui_reload::{ComponentSchema, ReloadError, ReloadLimits, StateField};

pub const UI_MODULE_PATH: &str = "github.com/vo-lang/ui";

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct StateId(u32);

impl StateId {
    pub const fn new(index: u32) -> Self {
        Self(index)
    }

    pub const fn index(self) -> u32 {
        self.0
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StateBinding {
    pub id: StateId,
    /// Compiler object identity used by codegen instrumentation. It never
    /// crosses the serialized component-plan boundary.
    pub object: ObjKey,
    /// Source-level key used for state-preserving development reloads.
    pub key: String,
    pub definition_span: Span,
    pub initializer: Option<ExprId>,
    pub initializer_span: Option<Span>,
    /// Earlier component state cells referenced by the initializer. Direct
    /// bootstrap currently accepts only independent initializers; retaining
    /// the edge makes that restriction explicit and keeps future lowering
    /// deterministic.
    pub initializer_dependencies: Vec<StateId>,
    /// Component props referenced by the initializer, ordered by prop index.
    pub initializer_prop_dependencies: Vec<u16>,
    /// Identifies declarations backed by the official stable state-cell ABI.
    /// The code generator externalizes eligible ordinary locals through the
    /// same ABI, so source reads and assignments stay idiomatic.
    pub runtime_cell: Option<RuntimeCellKind>,
    /// True when codegen must externalize an ordinary source local. Explicit
    /// Use*State declarations already contain their storage calls in source.
    pub automatic_cell: bool,
    pub dependent_slots: Vec<SlotId>,
    pub captured_by_handlers: Vec<HandlerId>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuntimeCellKind {
    String,
    Bool,
    Int,
    Float,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SlotBinding {
    pub expression: ExprId,
    pub expression_span: Span,
    pub slots: Vec<SlotId>,
    pub dependencies: Vec<StateId>,
    pub prop_dependencies: Vec<u16>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HandlerBinding {
    pub handler: HandlerId,
    pub expression: ExprId,
    pub expression_span: Span,
    pub captured_state: Vec<StateId>,
    pub captured_props: Vec<u16>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ComponentPropBinding {
    pub expression: ExprId,
    pub expression_span: Span,
    pub dependencies: Vec<StateId>,
    pub prop_dependencies: Vec<u16>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompiledComponentCall {
    pub id: ComponentCallSiteId,
    /// Source expression for the component function call itself.
    pub expression: ExprId,
    /// When present, codegen enters the component scope at this surrounding
    /// `ui.Key` expression so the key is evaluated before component state.
    pub wrapper_expression: Option<ExprId>,
    pub callee_object: ObjKey,
    pub callee: ComponentTypeId,
    pub span: Span,
    pub mount_parent: LocalNodeId,
    pub mount_before: Option<LocalNodeId>,
    pub props: Vec<ComponentPropBinding>,
    pub key: Option<ComponentPropBinding>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompiledComponent {
    /// Owning source package. Expression identities are scoped by this key.
    pub package: PackageKey,
    pub name: String,
    pub identity: String,
    pub type_id: ComponentTypeId,
    pub props_arity: u16,
    /// Canonical ordered logical prop layout, independent of import spelling.
    pub props_type_fingerprint: u64,
    /// Parameter objects by ABI position. Unnamed parameters have no object.
    pub props: Vec<Option<ObjKey>>,
    pub declaration_span: Span,
    pub plan: ValidatedPlan,
    pub state_bindings: Vec<StateBinding>,
    pub reload_schema: ComponentSchema,
    pub slot_bindings: Vec<SlotBinding>,
    pub handler_bindings: Vec<HandlerBinding>,
    pub component_calls: Vec<CompiledComponentCall>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UiProgram {
    pub mount_span: Span,
    pub root: CompiledComponent,
    /// Reachable same-package definitions in canonical component-type order.
    pub components: Vec<CompiledComponent>,
}

/// Runtime scope metadata discovered independently from static component-plan
/// lowering. Generic VM/JIT/AOT rendering consumes these source identities so
/// dynamic branches and collections retain the same keyed component state.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DiscoveredComponentScope {
    pub package: PackageKey,
    pub expression: ExprId,
    pub target: ExprId,
    pub key: Option<ExprId>,
    pub identity: String,
    pub call_site: ComponentCallSiteId,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UiRuntimeDiscovery {
    pub state_bindings: Vec<StateBinding>,
    pub component_scopes: Vec<DiscoveredComponentScope>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompileErrorKind {
    MultipleMounts,
    InvalidMountArity,
    InvalidMountTarget,
    ImportedRootUnsupported,
    ComponentBodyRequired,
    ComponentBodyMustBeSingleReturn,
    UnsupportedComponentPrelude,
    StateInitializerArity,
    StateIdentityExhausted,
    ComponentReturnArity,
    ExpectedUiCall,
    UnsupportedUiCall(String),
    InvalidCallArity {
        function: String,
        expected: &'static str,
        found: usize,
    },
    ModifierTargetMustBeElement(String),
    HandlerIdentityExhausted,
    ComponentCallIdentityCollision,
    ComponentPropArityExceeded,
    ComponentGraphCycle,
    Plan(PlanError),
    Reload(ReloadError),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompileError {
    pub span: Span,
    pub kind: CompileErrorKind,
}

impl fmt::Display for CompileError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "UI component compilation failed at {:?}: {:?}",
            self.span, self.kind
        )
    }
}

impl std::error::Error for CompileError {}

#[derive(Clone, Debug, PartialEq)]
pub enum UiCompileOutcome {
    NoMount,
    Compiled(Box<UiProgram>),
    /// The program is valid and will use generic keyed reconciliation. Tooling
    /// can surface the retained compiler reason without turning it into an
    /// application build failure.
    GenericFallback(CompileError),
}

/// Finds the typed `ui.Mount` boundary in the main package and compiles its
/// root component. Projects without a UI mount remain ordinary Volang builds.
pub fn compile_project_ui(
    project: &Project,
    limits: PlanLimits,
) -> Result<Option<UiProgram>, CompileError> {
    compile_project_ui_outcome(project, limits).map(|outcome| match outcome {
        UiCompileOutcome::Compiled(program) => Some(*program),
        UiCompileOutcome::NoMount | UiCompileOutcome::GenericFallback(_) => None,
    })
}

/// Detailed discovery result for diagnostics, inspectors, and size analysis.
/// Generic fallbacks carry the exact source span and unsupported construct.
pub fn compile_project_ui_outcome(
    project: &Project,
    limits: PlanLimits,
) -> Result<UiCompileOutcome, CompileError> {
    let mut finder = MountFinder {
        project,
        mounts: Vec::new(),
    };
    for file in &project.files {
        finder.visit_file(file);
    }
    let mut mounts = finder.mounts.into_iter();
    let Some(mount) = mounts.next() else {
        return Ok(UiCompileOutcome::NoMount);
    };
    if let Some(second) = mounts.next() {
        return Err(error(second.span, CompileErrorKind::MultipleMounts));
    }
    if mount.call.spread || mount.call.args.len() != 1 {
        return Err(error(mount.span, CompileErrorKind::InvalidMountArity));
    }

    let resolved = match resolve_component_body(project, &mount.call.args[0]) {
        Ok(component) => component,
        Err(error) if supports_generic_runtime_fallback(&error.kind) => {
            return Ok(UiCompileOutcome::GenericFallback(error));
        }
        Err(error) => return Err(error),
    };
    let root = match compile_resolved_component(project, &resolved, limits) {
        Ok(component) => component,
        Err(error) if supports_generic_runtime_fallback(&error.kind) => {
            return Ok(UiCompileOutcome::GenericFallback(error));
        }
        Err(error) => return Err(error),
    };

    let mut seen = HashSet::new();
    if let Some(object) = resolved.object {
        seen.insert(object);
    }
    let mut pending = root
        .component_calls
        .iter()
        .map(|call| call.callee_object)
        .collect::<Vec<_>>();
    let mut components = Vec::new();
    let mut cursor = 0;
    while let Some(object) = pending.get(cursor).copied() {
        cursor += 1;
        if !seen.insert(object) {
            continue;
        }
        let Some(source) = find_component_function(project, object) else {
            return Ok(UiCompileOutcome::GenericFallback(error(
                root.declaration_span,
                CompileErrorKind::ImportedRootUnsupported,
            )));
        };
        let function = source.function;
        let name = project
            .interner
            .resolve(function.name.symbol)
            .unwrap_or("<component>")
            .to_string();
        let Some(body) = function.body.as_ref() else {
            return Err(error(
                function.span,
                CompileErrorKind::ComponentBodyRequired,
            ));
        };
        let resolved = ResolvedComponent {
            package: source.package,
            name,
            object: Some(object),
            props_arity: component_props_arity(function)?,
            props_type_fingerprint: component_props_fingerprint(
                project,
                source.type_info,
                &function.sig,
            ),
            props: component_props(source.type_info, function),
            package_path: source.package_path.to_string(),
            declaration_span: function.span,
            body: body.clone(),
        };
        let component = match compile_resolved_component(project, &resolved, limits) {
            Ok(component) => component,
            Err(error) if supports_generic_runtime_fallback(&error.kind) => {
                return Ok(UiCompileOutcome::GenericFallback(error));
            }
            Err(error) => return Err(error),
        };
        pending.extend(
            component
                .component_calls
                .iter()
                .map(|call| call.callee_object),
        );
        components.push(component);
    }
    components.sort_by(|left, right| left.type_id.cmp(&right.type_id));
    Ok(UiCompileOutcome::Compiled(Box::new(UiProgram {
        mount_span: mount.span,
        root,
        components,
    })))
}

/// Discovers component-local state independently from static tree lowering.
/// The engine uses this metadata to externalize eligible locals even when a
/// library call or dynamic tree sends rendering through generic reconciliation.
pub fn discover_project_ui_state_bindings(
    project: &Project,
) -> Result<Option<Vec<StateBinding>>, CompileError> {
    Ok(discover_project_ui_runtime(project)?.map(|discovery| discovery.state_bindings))
}

/// Discovers every reachable authored component call and eligible top-level
/// component state declaration, including calls inside ordinary control flow.
/// Static-plan compilation remains free to fail over to generic reconciliation
/// without losing component instance boundaries.
pub fn discover_project_ui_runtime(
    project: &Project,
) -> Result<Option<UiRuntimeDiscovery>, CompileError> {
    let mut finder = MountFinder {
        project,
        mounts: Vec::new(),
    };
    for file in &project.files {
        finder.visit_file(file);
    }
    let mut mounts = finder.mounts.into_iter();
    let Some(mount) = mounts.next() else {
        return Ok(None);
    };
    if let Some(second) = mounts.next() {
        return Err(error(second.span, CompileErrorKind::MultipleMounts));
    }
    if mount.call.spread || mount.call.args.len() != 1 {
        return Err(error(mount.span, CompileErrorKind::InvalidMountArity));
    }
    let component = match resolve_component_body(project, &mount.call.args[0]) {
        Ok(component) => component,
        Err(error) if supports_generic_runtime_fallback(&error.kind) => return Ok(None),
        Err(error) => return Err(error),
    };

    let root_type = component.object.map_or_else(
        || {
            ComponentTypeId::new(
                project.main_pkg().path(),
                format!("<mount@{}>", component.declaration_span.start.to_u32()),
            )
        },
        |object| component_type_id(project, object),
    );
    let mut state_bindings = Vec::new();
    let mut component_scopes = Vec::new();
    let mut pending = Vec::new();
    let mut seen = HashSet::new();
    if let Some(object) = component.object {
        seen.insert(object);
    }
    discover_component_runtime(
        project,
        component.package,
        &component.package_path,
        &component.body,
        &component.props,
        &root_type,
        &mut state_bindings,
        &mut component_scopes,
        &mut pending,
    )?;

    let mut cursor = 0;
    while let Some(object) = pending.get(cursor).copied() {
        cursor += 1;
        if !seen.insert(object) {
            continue;
        }
        let Some(source) = find_component_function(project, object) else {
            continue;
        };
        let Some(body) = source.function.body.as_ref() else {
            return Err(error(
                source.function.span,
                CompileErrorKind::ComponentBodyRequired,
            ));
        };
        let props = component_props(source.type_info, source.function);
        discover_component_runtime(
            project,
            source.package,
            source.package_path,
            body,
            &props,
            &component_type_id(project, object),
            &mut state_bindings,
            &mut component_scopes,
            &mut pending,
        )?;
    }
    Ok(Some(UiRuntimeDiscovery {
        state_bindings,
        component_scopes,
    }))
}

#[allow(clippy::too_many_arguments)]
fn discover_component_runtime(
    project: &Project,
    package: PackageKey,
    package_path: &str,
    body: &Block,
    props: &[Option<ObjKey>],
    component_type: &ComponentTypeId,
    state_bindings: &mut Vec<StateBinding>,
    component_scopes: &mut Vec<DiscoveredComponentScope>,
    pending: &mut Vec<ObjKey>,
) -> Result<(), CompileError> {
    let type_info = package_type_info(project, package_path)
        .ok_or_else(|| error(body.span, CompileErrorKind::InvalidMountTarget))?;
    state_bindings.extend(discover_top_level_component_states(
        project, type_info, body, props,
    )?);
    let mut finder = ComponentScopeFinder {
        project,
        type_info,
        package,
        component_type,
        claimed_targets: HashSet::new(),
        scopes: Vec::new(),
        callees: Vec::new(),
    };
    for statement in &body.stmts {
        finder.visit_stmt(statement);
    }
    component_scopes.extend(finder.scopes);
    pending.extend(finder.callees);
    Ok(())
}

fn discover_top_level_component_states(
    project: &Project,
    type_info: &TypeInfo,
    body: &Block,
    props: &[Option<ObjKey>],
) -> Result<Vec<StateBinding>, CompileError> {
    let prop_ids = props
        .iter()
        .enumerate()
        .filter_map(|(index, object)| {
            object.map(|object| {
                (
                    object,
                    u16::try_from(index).expect("component prop arity was bounded"),
                )
            })
        })
        .collect::<HashMap<_, _>>();
    let mut states = Vec::new();
    for statement in &body.stmts {
        match &statement.kind {
            StmtKind::ShortVar(declaration) => {
                if declaration.names.len() != declaration.values.len() {
                    // A single call may initialize several names from a
                    // multi-result function. Those locals execute correctly
                    // in the generic component body, but they cannot be
                    // externalized as independent scalar state cells without
                    // tuple projections. Keep discovering nested components
                    // and leave this declaration owned by ordinary Volang.
                    continue;
                }
                for (name, initializer) in declaration.names.iter().zip(&declaration.values) {
                    push_state(
                        project,
                        type_info,
                        &mut states,
                        &prop_ids,
                        name,
                        Some(initializer),
                    )?;
                }
            }
            StmtKind::Var(declaration) => {
                for spec in &declaration.specs {
                    if !spec.values.is_empty() && spec.names.len() != spec.values.len() {
                        continue;
                    }
                    for (index, name) in spec.names.iter().enumerate() {
                        push_state(
                            project,
                            type_info,
                            &mut states,
                            &prop_ids,
                            name,
                            spec.values.get(index),
                        )?;
                    }
                }
            }
            _ => {}
        }
    }
    Ok(states)
}

struct ComponentScopeFinder<'a> {
    project: &'a Project,
    type_info: &'a TypeInfo,
    package: PackageKey,
    component_type: &'a ComponentTypeId,
    claimed_targets: HashSet<ExprId>,
    scopes: Vec<DiscoveredComponentScope>,
    callees: Vec<ObjKey>,
}

impl ComponentScopeFinder<'_> {
    fn push_scope(&mut self, expression: ExprId, target: &Expr, key: Option<ExprId>) -> bool {
        let Some(callee_object) = component_call_object(self.project, self.type_info, target)
        else {
            return false;
        };
        let callee = component_type_id(self.project, callee_object);
        self.scopes.push(DiscoveredComponentScope {
            package: self.package,
            expression,
            target: strip_parens(target).id,
            key,
            identity: callee.to_string(),
            call_site: stable_component_call_site_id(self.component_type, &callee, target.span),
        });
        self.callees.push(callee_object);
        true
    }
}

impl Visitor for ComponentScopeFinder<'_> {
    fn visit_expr(&mut self, expression: &Expr) {
        let stripped = strip_parens(expression);
        if let ExprKind::Call(call) = &stripped.kind {
            if ui_function_name(self.project, self.type_info, &call.func) == Some("Key")
                && !call.spread
                && call.args.len() == 2
            {
                let target = strip_parens(&call.args[0]);
                if self.push_scope(stripped.id, target, Some(call.args[1].id)) {
                    self.claimed_targets.insert(target.id);
                }
            } else if !self.claimed_targets.contains(&stripped.id) {
                self.push_scope(stripped.id, stripped, None);
            }
        }
        ast::walk_expr(self, expression);
    }
}

fn component_call_object(
    project: &Project,
    type_info: &TypeInfo,
    expression: &Expr,
) -> Option<ObjKey> {
    let expression = strip_parens(expression);
    let ExprKind::Call(call) = &expression.kind else {
        return None;
    };
    if ui_function_name(project, type_info, &call.func).is_some()
        || !is_ui_view_expression(project, type_info, expression)
    {
        return None;
    }
    let object = called_function_object(type_info, &call.func)?;
    find_component_function(project, object).map(|_| object)
}

fn is_ui_view_expression(project: &Project, type_info: &TypeInfo, expression: &Expr) -> bool {
    let Some(typ) = type_info.expr_type(expression.id) else {
        return false;
    };
    let Some(named) = project.tc_objs.types[typ].try_as_named() else {
        return false;
    };
    let Some(object) = *named.obj() else {
        return false;
    };
    let object = &project.tc_objs.lobjs[object];
    object.name() == "View"
        && object
            .pkg()
            .is_some_and(|package| project.tc_objs.pkgs[package].path() == UI_MODULE_PATH)
}

/// Static component plans are an optimization. Valid Volang control flow and
/// library composition continue through the generic reconciler when the typed
/// adapter cannot prove a fixed tree shape. Contract violations and bounded
/// identity/plan failures still stop compilation.
fn supports_generic_runtime_fallback(kind: &CompileErrorKind) -> bool {
    matches!(
        kind,
        CompileErrorKind::ImportedRootUnsupported
            | CompileErrorKind::ComponentBodyMustBeSingleReturn
            | CompileErrorKind::UnsupportedComponentPrelude
            | CompileErrorKind::ComponentReturnArity
            | CompileErrorKind::ExpectedUiCall
            | CompileErrorKind::UnsupportedUiCall(_)
            | CompileErrorKind::ModifierTargetMustBeElement(_)
    )
}

/// Converts compiler-owned expression identities into the stable artifact
/// model consumed by runtimes and AOT packagers. Direct mode bootstraps the
/// root once, then evaluates affected slots without reconstructing `View`s.
/// Components outside that proven subset retain root evaluation.
pub fn build_component_artifact(program: &UiProgram) -> ComponentArtifact {
    build_component_artifact_with_functions(program, |_| None)
}

pub fn build_component_artifact_with_functions(
    program: &UiProgram,
    function_for_expression: impl Fn(ExprId) -> Option<u32>,
) -> ComponentArtifact {
    let component = &program.root;
    let initializer_functions = component
        .state_bindings
        .iter()
        .map(|binding| binding.initializer.and_then(&function_for_expression))
        .collect::<Vec<_>>();
    let slot_functions = component
        .slot_bindings
        .iter()
        .map(|binding| function_for_expression(binding.expression))
        .collect::<Vec<_>>();
    let handler_functions = component
        .handler_bindings
        .iter()
        .map(|binding| function_for_expression(binding.expression))
        .collect::<Vec<_>>();
    let direct = component
        .state_bindings
        .iter()
        .zip(&initializer_functions)
        .all(|(binding, function)| {
            binding.runtime_cell.is_some()
                && binding.initializer_dependencies.is_empty()
                && (binding.initializer.is_none() || function.is_some())
        })
        && slot_functions.iter().all(Option::is_some)
        && handler_functions.iter().all(Option::is_some);
    ComponentArtifact {
        identity: component.identity.clone(),
        component_name: component.name.clone(),
        mode: if direct {
            ExecutionMode::Direct
        } else {
            ExecutionMode::RootFallback
        },
        plan: component.plan.clone(),
        states: component
            .state_bindings
            .iter()
            .zip(&component.reload_schema.state)
            .zip(initializer_functions)
            .map(|((binding, schema), initializer_func)| StateArtifact {
                key: schema.key.clone(),
                type_fingerprint: schema.type_fingerprint,
                has_initializer: binding.initializer.is_some(),
                initializer_func,
                dependent_slots: binding.dependent_slots.clone(),
                captured_by_handlers: binding.captured_by_handlers.clone(),
            })
            .collect(),
        slots: component
            .slot_bindings
            .iter()
            .zip(slot_functions)
            .map(|(binding, evaluator_func)| SlotArtifact {
                evaluator_func,
                slots: binding.slots.clone(),
                dependencies: binding
                    .dependencies
                    .iter()
                    .map(|state| state.index())
                    .collect(),
            })
            .collect(),
        handlers: component
            .handler_bindings
            .iter()
            .zip(handler_functions)
            .map(|(binding, evaluator_func)| HandlerArtifact {
                handler: binding.handler,
                evaluator_func,
                captured_state: binding
                    .captured_state
                    .iter()
                    .map(|state| state.index())
                    .collect(),
            })
            .collect(),
    }
}

pub fn encode_ui_program(
    program: &UiProgram,
    artifact_limits: ArtifactLimits,
    plan_limits: PlanLimits,
) -> Result<Vec<u8>, ArtifactError> {
    encode_component_artifact(
        &build_component_artifact(program),
        artifact_limits,
        plan_limits,
    )
}

pub fn encode_ui_program_with_functions(
    program: &UiProgram,
    artifact_limits: ArtifactLimits,
    plan_limits: PlanLimits,
    function_for_expression: impl Fn(ExprId) -> Option<u32>,
) -> Result<Vec<u8>, ArtifactError> {
    encode_component_artifact(
        &build_component_artifact_with_functions(program, function_for_expression),
        artifact_limits,
        plan_limits,
    )
}

pub fn build_component_bundle(program: &UiProgram) -> Result<ComponentBundle, BundleError> {
    build_component_bundle_with_functions(program, |_, _| None)
}

pub fn build_component_bundle_with_functions(
    program: &UiProgram,
    function_for_expression: impl Fn(PackageKey, ExprId) -> Option<u32>,
) -> Result<ComponentBundle, BundleError> {
    let mut components = program.components.iter().collect::<Vec<_>>();
    components.push(&program.root);
    components.sort_by(|left, right| left.type_id.cmp(&right.type_id));
    let mut definitions = Vec::new();
    definitions
        .try_reserve_exact(components.len())
        .map_err(|_| BundleError::AllocationFailed)?;
    for component in components {
        definitions.push(build_component_definition(
            component,
            &function_for_expression,
        )?);
    }

    let mut digest = Sha256::new();
    digest.update(b"volang-ui-component-bundle-source-v1\0");
    digest.update(projected_bundle_source_bytes(program));
    let source_digest: [u8; 32] = digest.finalize().into();
    let mut linked_modules = definitions
        .iter()
        .map(|definition| definition.type_id.module())
        .filter(|module| *module != program.root.type_id.module())
        .map(str::to_string)
        .collect::<Vec<_>>();
    linked_modules.sort();
    linked_modules.dedup();
    Ok(ComponentBundle {
        abi_version: COMPONENT_BUNDLE_ABI_VERSION,
        module_identity: program.root.type_id.module().to_string(),
        root: program.root.type_id.clone(),
        linked_modules,
        definitions,
        imports: Vec::new(),
        capabilities: vec!["ui.component-v2".to_string()],
        source: BundleSourceMetadata {
            source_digest,
            compiler_identity: format!("vo-ui-compiler/{}", env!("CARGO_PKG_VERSION")),
            reload_schema_version: 1,
        },
    })
}

pub fn encode_ui_component_bundle_with_functions(
    program: &UiProgram,
    bundle_limits: BundleLimits,
    plan_limits: PlanLimits,
    function_for_expression: impl Fn(PackageKey, ExprId) -> Option<u32>,
) -> Result<Vec<u8>, BundleError> {
    let bundle = build_component_bundle_with_functions(program, function_for_expression)?;
    encode_component_bundle(&bundle, bundle_limits, plan_limits)
}

fn build_component_definition(
    component: &CompiledComponent,
    function_for_expression: &impl Fn(PackageKey, ExprId) -> Option<u32>,
) -> Result<ComponentDefinition, BundleError> {
    let initializer_functions = component
        .state_bindings
        .iter()
        .map(|binding| {
            binding
                .initializer
                .and_then(|expression| function_for_expression(component.package, expression))
        })
        .collect::<Vec<_>>();
    let slot_functions = component
        .slot_bindings
        .iter()
        .map(|binding| function_for_expression(component.package, binding.expression))
        .collect::<Vec<_>>();
    let handler_functions = component
        .handler_bindings
        .iter()
        .map(|binding| function_for_expression(component.package, binding.expression))
        .collect::<Vec<_>>();

    let mut bindings = Vec::new();
    for (index, (binding, evaluator_func)) in component
        .slot_bindings
        .iter()
        .zip(slot_functions.iter().copied())
        .enumerate()
    {
        bindings.push(BindingDefinition {
            id: BindingId::new(u32::try_from(index).map_err(|_| BundleError::LengthOverflow)?),
            evaluator_func,
            slots: binding.slots.clone(),
            dependencies: binding
                .dependencies
                .iter()
                .map(|state| StateFieldId::new(state.index()))
                .collect(),
            prop_dependencies: binding.prop_dependencies.clone(),
        });
    }

    let mut call_sites = Vec::new();
    let mut all_prop_functions_present = true;
    for call in &component.component_calls {
        let mut props_bindings = Vec::new();
        for prop in &call.props {
            let id = BindingId::new(
                u32::try_from(bindings.len()).map_err(|_| BundleError::LengthOverflow)?,
            );
            let evaluator_func = function_for_expression(component.package, prop.expression);
            all_prop_functions_present &= evaluator_func.is_some();
            bindings.push(BindingDefinition {
                id,
                evaluator_func,
                slots: Vec::new(),
                dependencies: prop
                    .dependencies
                    .iter()
                    .map(|state| StateFieldId::new(state.index()))
                    .collect(),
                prop_dependencies: prop.prop_dependencies.clone(),
            });
            props_bindings.push(id);
        }
        let key_binding = call
            .key
            .as_ref()
            .map(|key| {
                let id = BindingId::new(
                    u32::try_from(bindings.len()).map_err(|_| BundleError::LengthOverflow)?,
                );
                let evaluator_func = function_for_expression(component.package, key.expression);
                all_prop_functions_present &= evaluator_func.is_some();
                bindings.push(BindingDefinition {
                    id,
                    evaluator_func,
                    slots: Vec::new(),
                    dependencies: key
                        .dependencies
                        .iter()
                        .map(|state| StateFieldId::new(state.index()))
                        .collect(),
                    prop_dependencies: key.prop_dependencies.clone(),
                });
                Ok(id)
            })
            .transpose()?;
        call_sites.push(ComponentCallSite {
            id: call.id,
            mode: ComponentCallMode::Static,
            callee: Some(call.callee.clone()),
            mount_parent: call.mount_parent,
            mount_before: call.mount_before,
            props_bindings,
            key_binding,
        });
    }

    let direct = component
        .state_bindings
        .iter()
        .zip(&initializer_functions)
        .all(|(binding, function)| {
            binding.runtime_cell.is_some() && (binding.initializer.is_none() || function.is_some())
        })
        && slot_functions.iter().all(Option::is_some)
        && handler_functions.iter().all(Option::is_some)
        && all_prop_functions_present;

    Ok(ComponentDefinition {
        type_id: component.type_id.clone(),
        display_name: component.name.clone(),
        mode: if direct {
            ExecutionMode::Direct
        } else {
            ExecutionMode::RootFallback
        },
        interface: ComponentInterface {
            props_arity: component.props_arity,
            props_type_fingerprint: stable_props_fingerprint(component),
            child_contract_fingerprint: 0,
            slot_contract_fingerprint: 0,
        },
        plan: component.plan.clone(),
        call_sites,
        states: component
            .state_bindings
            .iter()
            .zip(&component.reload_schema.state)
            .zip(initializer_functions)
            .map(
                |((binding, schema), initializer_func)| StateFieldDefinition {
                    id: StateFieldId::new(binding.id.index()),
                    key: schema.key.clone(),
                    type_fingerprint: schema.type_fingerprint,
                    value_kind: match binding.runtime_cell {
                        Some(RuntimeCellKind::String) => StateValueKind::String,
                        Some(RuntimeCellKind::Bool) => StateValueKind::Bool,
                        Some(RuntimeCellKind::Int) => StateValueKind::Int,
                        Some(RuntimeCellKind::Float) => StateValueKind::Float,
                        None => StateValueKind::Opaque,
                    },
                    has_initializer: binding.initializer.is_some(),
                    initializer_func,
                    initializer_dependencies: binding
                        .initializer_dependencies
                        .iter()
                        .map(|state| StateFieldId::new(state.index()))
                        .collect(),
                    initializer_props: binding.initializer_prop_dependencies.clone(),
                },
            )
            .collect(),
        bindings,
        handlers: component
            .handler_bindings
            .iter()
            .zip(handler_functions)
            .enumerate()
            .map(|(index, (binding, evaluator_func))| {
                Ok(HandlerDefinition {
                    id: HandlerSiteId::new(
                        u32::try_from(index).map_err(|_| BundleError::LengthOverflow)?,
                    ),
                    plan_handler: binding.handler,
                    evaluator_func,
                    captured_state: binding
                        .captured_state
                        .iter()
                        .map(|state| StateFieldId::new(state.index()))
                        .collect(),
                    captured_props: binding.captured_props.clone(),
                })
            })
            .collect::<Result<Vec<_>, BundleError>>()?,
        effects: Vec::new(),
        tasks: Vec::new(),
        lifecycle: LifecycleDefinition::empty(),
        reload_schema_fingerprint: stable_reload_schema_fingerprint(component),
    })
}

fn stable_props_fingerprint(component: &CompiledComponent) -> u64 {
    component.props_type_fingerprint
}

fn stable_reload_schema_fingerprint(component: &CompiledComponent) -> u64 {
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for byte in b"volang-ui-reload-schema-v1\0" {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    for field in &component.reload_schema.state {
        for byte in field
            .key
            .bytes()
            .chain([0])
            .chain(field.type_fingerprint.to_le_bytes())
        {
            hash ^= u64::from(byte);
            hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
        }
    }
    hash
}

fn projected_bundle_source_bytes(program: &UiProgram) -> Vec<u8> {
    let mut components = program.components.iter().collect::<Vec<_>>();
    components.push(&program.root);
    components.sort_by(|left, right| left.type_id.cmp(&right.type_id));
    let mut bytes = Vec::new();
    for component in components {
        bytes.extend_from_slice(component.type_id.module().as_bytes());
        bytes.push(0);
        bytes.extend_from_slice(component.type_id.object().as_bytes());
        bytes.push(0);
        bytes.extend_from_slice(&component.declaration_span.start.to_u32().to_le_bytes());
        bytes.extend_from_slice(&component.declaration_span.end.to_u32().to_le_bytes());
        bytes.extend_from_slice(&component.props_type_fingerprint.to_le_bytes());
        bytes.extend_from_slice(&stable_reload_schema_fingerprint(component).to_le_bytes());
    }
    bytes
}

fn build_reload_schema(
    project: &Project,
    identity: &str,
    states: &[StateBinding],
) -> Result<ComponentSchema, CompileError> {
    let fields = states
        .iter()
        .map(|state| {
            let object = &project.tc_objs.lobjs[state.object];
            let type_name = object
                .typ()
                .map(|typ| vo_analysis::display::type_string(typ, &project.tc_objs))
                .unwrap_or_else(|| "<invalid>".to_string());
            StateField::new(state.key.clone(), stable_type_fingerprint(&type_name))
        })
        .collect();
    let schema = ComponentSchema::new(identity, fields);
    schema
        .validate(ReloadLimits::default())
        .map_err(|reload_error| {
            error(
                states
                    .first()
                    .map(|state| state.definition_span)
                    .unwrap_or_else(Span::dummy),
                CompileErrorKind::Reload(reload_error),
            )
        })?;
    Ok(schema)
}

fn stable_type_fingerprint(type_name: &str) -> u64 {
    // Versioned FNV-1a keeps the reload ABI deterministic across host targets.
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for byte in b"volang-ui-state-type-v1\0"
        .iter()
        .chain(type_name.as_bytes())
    {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    hash
}

#[derive(Clone)]
struct MountCall {
    span: Span,
    call: CallExpr,
}

struct MountFinder<'a> {
    project: &'a Project,
    mounts: Vec<MountCall>,
}

impl Visitor for MountFinder<'_> {
    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::Call(call) = &expr.kind {
            if ui_function_name(self.project, &self.project.type_info, &call.func) == Some("Mount")
            {
                self.mounts.push(MountCall {
                    span: expr.span,
                    call: (**call).clone(),
                });
            }
        }
        ast::walk_expr(self, expr);
    }
}

struct ResolvedComponent {
    package: PackageKey,
    name: String,
    object: Option<ObjKey>,
    props_arity: u16,
    props_type_fingerprint: u64,
    props: Vec<Option<ObjKey>>,
    package_path: String,
    declaration_span: Span,
    body: Block,
}

fn resolve_component_body(
    project: &Project,
    expression: &Expr,
) -> Result<ResolvedComponent, CompileError> {
    let expression = strip_parens(expression);
    match &expression.kind {
        ExprKind::FuncLit(function) => Ok(ResolvedComponent {
            package: project.main_package,
            name: "<mount>".to_string(),
            object: None,
            props_arity: u16::try_from(
                function
                    .sig
                    .params
                    .iter()
                    .map(|parameter| parameter.names.len().max(1))
                    .sum::<usize>(),
            )
            .map_err(|_| {
                error(
                    expression.span,
                    CompileErrorKind::ComponentPropArityExceeded,
                )
            })?,
            props_type_fingerprint: component_props_fingerprint(
                project,
                &project.type_info,
                &function.sig,
            ),
            props: component_props_from_sig(&project.type_info, &function.sig),
            package_path: project.main_pkg().path().to_string(),
            declaration_span: expression.span,
            body: function.body.clone(),
        }),
        ExprKind::Ident(_) | ExprKind::Selector(_) => {
            let Some(object) = called_function_object(&project.type_info, expression) else {
                return Err(error(expression.span, CompileErrorKind::InvalidMountTarget));
            };
            find_component_function(project, object)
                .map(|source| {
                    let function = source.function;
                    let name = project
                        .interner
                        .resolve(function.name.symbol)
                        .unwrap_or("<component>")
                        .to_string();
                    function.body.as_ref().map_or_else(
                        || {
                            Err(error(
                                function.span,
                                CompileErrorKind::ComponentBodyRequired,
                            ))
                        },
                        |body| {
                            Ok(ResolvedComponent {
                                package: source.package,
                                name,
                                object: Some(object),
                                props_arity: component_props_arity(function)?,
                                props_type_fingerprint: component_props_fingerprint(
                                    project,
                                    source.type_info,
                                    &function.sig,
                                ),
                                props: component_props(source.type_info, function),
                                package_path: source.package_path.to_string(),
                                declaration_span: function.span,
                                body: body.clone(),
                            })
                        },
                    )
                })
                .ok_or_else(|| error(expression.span, CompileErrorKind::InvalidMountTarget))?
        }
        _ => Err(error(expression.span, CompileErrorKind::InvalidMountTarget)),
    }
}

fn compile_resolved_component(
    project: &Project,
    resolved: &ResolvedComponent,
    limits: PlanLimits,
) -> Result<CompiledComponent, CompileError> {
    let type_info = package_type_info(project, &resolved.package_path).ok_or_else(|| {
        error(
            resolved.declaration_span,
            CompileErrorKind::InvalidMountTarget,
        )
    })?;
    let (mut state_bindings, root_expr) =
        component_body(project, type_info, &resolved.body, &resolved.props)?;
    let type_id = resolved.object.map_or_else(
        || {
            ComponentTypeId::new(
                project.main_pkg().path(),
                format!("<mount@{}>", resolved.declaration_span.start.to_u32()),
            )
        },
        |object| component_type_id(project, object),
    );
    let identity = type_id.to_string();
    let mut lowerer = Lowerer::new(
        project,
        type_info,
        &state_bindings,
        &resolved.props,
        type_id.clone(),
    );
    let root = lowerer.lower_view(root_expr)?;
    lowerer.plan.root = root;
    let plan = lowerer
        .plan
        .validate(limits)
        .map_err(|plan_error| error(root_expr.span, CompileErrorKind::Plan(plan_error)))?;
    populate_state_consumers(
        &mut state_bindings,
        &lowerer.slot_bindings,
        &lowerer.handler_bindings,
    );
    lowerer
        .component_calls
        .sort_by_key(|call_site| call_site.id);
    if lowerer
        .component_calls
        .windows(2)
        .any(|pair| pair[0].id == pair[1].id)
    {
        return Err(error(
            resolved.declaration_span,
            CompileErrorKind::ComponentCallIdentityCollision,
        ));
    }
    let reload_schema = build_reload_schema(project, &identity, &state_bindings)?;
    Ok(CompiledComponent {
        package: resolved.package,
        name: resolved.name.clone(),
        identity,
        type_id,
        props_arity: resolved.props_arity,
        props_type_fingerprint: resolved.props_type_fingerprint,
        props: resolved.props.clone(),
        declaration_span: resolved.declaration_span,
        plan,
        state_bindings,
        reload_schema,
        slot_bindings: lowerer.slot_bindings,
        handler_bindings: lowerer.handler_bindings,
        component_calls: lowerer.component_calls,
    })
}

fn package_type_info<'a>(project: &'a Project, package_path: &str) -> Option<&'a TypeInfo> {
    if package_path == project.main_pkg().path() {
        Some(&project.type_info)
    } else {
        project.imported_type_infos.get(package_path)
    }
}

fn component_props_arity(function: &FuncDecl) -> Result<u16, CompileError> {
    let arity = function
        .sig
        .params
        .iter()
        .map(|parameter| parameter.names.len().max(1))
        .sum::<usize>();
    u16::try_from(arity).map_err(|_| {
        error(
            function.sig.span,
            CompileErrorKind::ComponentPropArityExceeded,
        )
    })
}

fn component_props_fingerprint(
    project: &Project,
    type_info: &TypeInfo,
    signature: &ast::FuncSig,
) -> u64 {
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for byte in b"volang-ui-props-layout-v1\0" {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    for parameter in &signature.params {
        let type_name = type_info
            .type_exprs
            .get(&parameter.ty.id)
            .map(|typ| vo_analysis::display::type_string(*typ, &project.tc_objs))
            .unwrap_or_else(|| "<invalid>".to_string());
        for _ in 0..parameter.names.len().max(1) {
            for byte in type_name.bytes().chain([0]) {
                hash ^= u64::from(byte);
                hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
            }
        }
    }
    hash
}

fn component_props(type_info: &TypeInfo, function: &FuncDecl) -> Vec<Option<ObjKey>> {
    component_props_from_sig(type_info, &function.sig)
}

fn component_props_from_sig(type_info: &TypeInfo, signature: &ast::FuncSig) -> Vec<Option<ObjKey>> {
    let mut props = Vec::new();
    for parameter in &signature.params {
        if parameter.names.is_empty() {
            props.push(None);
        } else {
            props.extend(parameter.names.iter().map(|name| type_info.get_def(name)));
        }
    }
    props
}

fn component_type_id(project: &Project, object: ObjKey) -> ComponentTypeId {
    let object = &project.tc_objs.lobjs[object];
    let module = object
        .pkg()
        .map(|package| project.tc_objs.pkgs[package].path())
        .unwrap_or_else(|| project.main_pkg().path());
    ComponentTypeId::new(module, object.name())
}

struct ComponentFunctionRef<'a> {
    package: PackageKey,
    package_path: &'a str,
    type_info: &'a TypeInfo,
    function: &'a FuncDecl,
}

fn find_component_function(project: &Project, object: ObjKey) -> Option<ComponentFunctionRef<'_>> {
    let owner = project.tc_objs.lobjs[object].pkg()?;
    let package_path = project.tc_objs.pkgs[owner].path();
    let (type_info, files) = if owner == project.main_package {
        (&project.type_info, project.files.as_slice())
    } else {
        (
            project.imported_type_infos.get(package_path)?,
            project.imported_files.get(package_path)?.as_slice(),
        )
    };
    let function = files.iter().find_map(|file| {
        file.decls.iter().find_map(|decl| match decl {
            Decl::Func(function) if type_info.get_def(&function.name) == Some(object) => {
                Some(function)
            }
            _ => None,
        })
    })?;
    Some(ComponentFunctionRef {
        package: owner,
        package_path,
        type_info,
        function,
    })
}

fn component_body<'a>(
    project: &Project,
    type_info: &TypeInfo,
    body: &'a Block,
    props: &[Option<ObjKey>],
) -> Result<(Vec<StateBinding>, &'a Expr), CompileError> {
    let prop_ids = props
        .iter()
        .enumerate()
        .filter_map(|(index, object)| {
            object.map(|object| {
                (
                    object,
                    u16::try_from(index).expect("component prop arity was bounded"),
                )
            })
        })
        .collect::<HashMap<_, _>>();
    let statements = body
        .stmts
        .iter()
        .filter(|statement| !matches!(statement.kind, StmtKind::Empty))
        .collect::<Vec<_>>();
    let Some((return_statement, prelude)) = statements.split_last() else {
        return Err(error(
            body.span,
            CompileErrorKind::ComponentBodyMustBeSingleReturn,
        ));
    };
    let StmtKind::Return(return_) = &return_statement.kind else {
        return Err(error(
            return_statement.span,
            CompileErrorKind::ComponentBodyMustBeSingleReturn,
        ));
    };
    let [value] = return_.values.as_slice() else {
        return Err(error(
            return_statement.span,
            CompileErrorKind::ComponentReturnArity,
        ));
    };
    let mut states = Vec::new();
    for statement in prelude {
        match &statement.kind {
            StmtKind::ShortVar(declaration) => {
                if declaration.names.len() != declaration.values.len() {
                    return Err(error(
                        statement.span,
                        CompileErrorKind::UnsupportedComponentPrelude,
                    ));
                }
                for (name, initializer) in declaration.names.iter().zip(&declaration.values) {
                    push_state(
                        project,
                        type_info,
                        &mut states,
                        &prop_ids,
                        name,
                        Some(initializer),
                    )?;
                }
            }
            StmtKind::Var(declaration) => {
                for spec in &declaration.specs {
                    if !spec.values.is_empty() && spec.names.len() != spec.values.len() {
                        return Err(error(
                            spec.span,
                            CompileErrorKind::UnsupportedComponentPrelude,
                        ));
                    }
                    for (index, name) in spec.names.iter().enumerate() {
                        push_state(
                            project,
                            type_info,
                            &mut states,
                            &prop_ids,
                            name,
                            spec.values.get(index),
                        )?;
                    }
                }
            }
            StmtKind::Const(_) | StmtKind::Type(_) => {}
            _ => {
                return Err(error(
                    statement.span,
                    CompileErrorKind::UnsupportedComponentPrelude,
                ));
            }
        }
    }
    Ok((states, value))
}

fn push_state(
    project: &Project,
    type_info: &TypeInfo,
    states: &mut Vec<StateBinding>,
    prop_ids: &HashMap<ObjKey, u16>,
    name: &ast::Ident,
    initializer: Option<&Expr>,
) -> Result<(), CompileError> {
    let Some(object) = type_info.get_def(name) else {
        // Blank identifiers deliberately have no persistent state identity.
        return Ok(());
    };
    let index = u32::try_from(states.len())
        .map_err(|_| error(name.span, CompileErrorKind::StateIdentityExhausted))?;
    let explicit_cell =
        initializer.and_then(|expression| runtime_cell_initializer(project, type_info, expression));
    let value_initializer = explicit_cell
        .as_ref()
        .map_or(initializer, |(_, expression)| Some(*expression));
    let initializer_dependencies = value_initializer
        .map(|expression| {
            let state_ids = states
                .iter()
                .map(|state| (state.object, state.id))
                .collect::<HashMap<_, _>>();
            let mut finder = StateUseFinder {
                type_info,
                state_ids: &state_ids,
                found: BTreeSet::new(),
            };
            finder.visit_expr(expression);
            finder.found.into_iter().collect()
        })
        .unwrap_or_default();
    let initializer_prop_dependencies = value_initializer
        .map(|expression| expression_props(type_info, prop_ids, expression))
        .unwrap_or_default();
    let derived = initializer
        .is_some_and(|expression| derived_value_kind(project, type_info, expression).is_some());
    let automatic_cell = explicit_cell.is_none() && initializer.is_some() && !derived;
    let runtime_cell = explicit_cell.map(|(kind, _)| kind).or_else(|| {
        automatic_cell
            .then(|| source_cell_kind(project, object))
            .flatten()
    });
    states.push(StateBinding {
        id: StateId::new(index),
        object,
        key: project
            .interner
            .resolve(name.symbol)
            .unwrap_or("<state>")
            .to_string(),
        definition_span: name.span,
        initializer: value_initializer.map(|expression| expression.id),
        initializer_span: value_initializer.map(|expression| expression.span),
        initializer_dependencies,
        initializer_prop_dependencies,
        runtime_cell,
        automatic_cell: automatic_cell && runtime_cell.is_some(),
        dependent_slots: Vec::new(),
        captured_by_handlers: Vec::new(),
    });
    Ok(())
}

fn derived_value_kind(
    project: &Project,
    type_info: &TypeInfo,
    expression: &Expr,
) -> Option<RuntimeCellKind> {
    let ExprKind::Call(call) = &strip_parens(expression).kind else {
        return None;
    };
    Some(match ui_function_name(project, type_info, &call.func)? {
        "DerivedString" => RuntimeCellKind::String,
        "DerivedBool" => RuntimeCellKind::Bool,
        "DerivedInt" => RuntimeCellKind::Int,
        "DerivedFloat" => RuntimeCellKind::Float,
        _ => return None,
    })
}

fn source_cell_kind(project: &Project, object: ObjKey) -> Option<RuntimeCellKind> {
    let type_key = project.tc_objs.lobjs[object].typ()?;
    let underlying = deep_underlying_type(type_key, &project.tc_objs);
    let Type::Basic(basic) = &project.tc_objs.types[underlying] else {
        return None;
    };
    Some(match basic.typ() {
        BasicType::Str => RuntimeCellKind::String,
        BasicType::Bool => RuntimeCellKind::Bool,
        BasicType::Int | BasicType::Int64 => RuntimeCellKind::Int,
        BasicType::Float64 => RuntimeCellKind::Float,
        _ => return None,
    })
}

fn runtime_cell_initializer<'a>(
    project: &Project,
    type_info: &TypeInfo,
    expression: &'a Expr,
) -> Option<(RuntimeCellKind, &'a Expr)> {
    let ExprKind::Call(call) = &strip_parens(expression).kind else {
        return None;
    };
    if call.spread || call.args.len() != 1 {
        return None;
    }
    let kind = match ui_function_name(project, type_info, &call.func)? {
        "UseStringState" => RuntimeCellKind::String,
        "UseBoolState" => RuntimeCellKind::Bool,
        "UseIntState" => RuntimeCellKind::Int,
        "UseFloatState" => RuntimeCellKind::Float,
        _ => return None,
    };
    Some((kind, &call.args[0]))
}

fn populate_state_consumers(
    states: &mut [StateBinding],
    slots: &[SlotBinding],
    handlers: &[HandlerBinding],
) {
    for binding in slots {
        for dependency in &binding.dependencies {
            states[dependency.index() as usize]
                .dependent_slots
                .extend(binding.slots.iter().copied());
        }
    }
    for binding in handlers {
        for state in &binding.captured_state {
            states[state.index() as usize]
                .captured_by_handlers
                .push(binding.handler);
        }
    }
    for state in states {
        state.dependent_slots.sort_unstable();
        state.dependent_slots.dedup();
        state.captured_by_handlers.sort_unstable();
        state.captured_by_handlers.dedup();
    }
}

struct Lowerer<'a> {
    project: &'a Project,
    type_info: &'a TypeInfo,
    component_type: ComponentTypeId,
    state_ids: HashMap<ObjKey, StateId>,
    prop_ids: HashMap<ObjKey, u16>,
    plan: ComponentPlan,
    slot_bindings: Vec<SlotBinding>,
    handler_bindings: Vec<HandlerBinding>,
    component_calls: Vec<CompiledComponentCall>,
}

impl<'a> Lowerer<'a> {
    fn new(
        project: &'a Project,
        type_info: &'a TypeInfo,
        states: &[StateBinding],
        props: &[Option<ObjKey>],
        component_type: ComponentTypeId,
    ) -> Self {
        Self {
            project,
            type_info,
            component_type,
            state_ids: states
                .iter()
                .map(|state| (state.object, state.id))
                .collect(),
            prop_ids: props
                .iter()
                .enumerate()
                .filter_map(|(index, object)| {
                    object.map(|object| {
                        (
                            object,
                            u16::try_from(index).expect("component prop arity was bounded"),
                        )
                    })
                })
                .collect(),
            plan: ComponentPlan::new(LocalNodeId::new(0)),
            slot_bindings: Vec::new(),
            handler_bindings: Vec::new(),
            component_calls: Vec::new(),
        }
    }

    fn lower_view(&mut self, expression: &Expr) -> Result<LocalNodeId, CompileError> {
        let expression = strip_parens(expression);
        let ExprKind::Call(call) = &expression.kind else {
            return Err(error(expression.span, CompileErrorKind::ExpectedUiCall));
        };
        let Some(function) = ui_function_name(self.project, self.type_info, &call.func) else {
            return self.lower_component_call(expression, call);
        };
        match function {
            "Fragment" => self.lower_container(expression.span, call, Primitive::Fragment),
            "Box" => self.lower_container(expression.span, call, Primitive::Box),
            "Row" => self.lower_container(expression.span, call, Primitive::Row),
            "Column" => self.lower_container(expression.span, call, Primitive::Column),
            "Stack" => self.lower_container(expression.span, call, Primitive::Stack),
            "Grid" => self.lower_container(expression.span, call, Primitive::Grid),
            "Scroll" => self.lower_container(expression.span, call, Primitive::Scroll),
            "Text" => self.lower_text(expression.span, call),
            "Image" => self.lower_image(expression.span, call),
            "Canvas" => self.lower_canvas(expression.span, call),
            "PlatformView" => self.lower_platform_view(expression.span, call),
            "Button" => self.lower_button(expression.span, call),
            "TextInput" => self.lower_text_input(expression.span, call),
            "TextArea" => self.lower_text_area(expression.span, call),
            "Toggle" => self.lower_toggle(expression.span, call),
            "Slider" => self.lower_slider(expression.span, call),
            "Width" => self.lower_modifier(expression.span, call, PropertyId::WIDTH, function),
            "Height" => self.lower_modifier(expression.span, call, PropertyId::HEIGHT, function),
            "MinWidth" => {
                self.lower_modifier(expression.span, call, PropertyId::MIN_WIDTH, function)
            }
            "MinHeight" => {
                self.lower_modifier(expression.span, call, PropertyId::MIN_HEIGHT, function)
            }
            "MaxWidth" => {
                self.lower_modifier(expression.span, call, PropertyId::MAX_WIDTH, function)
            }
            "MaxHeight" => {
                self.lower_modifier(expression.span, call, PropertyId::MAX_HEIGHT, function)
            }
            "Flex" => self.lower_modifier(expression.span, call, PropertyId::FLEX, function),
            "Gap" => self.lower_modifier(expression.span, call, PropertyId::GAP, function),
            "Padding" => self.lower_modifier(expression.span, call, PropertyId::PADDING, function),
            "Background" => {
                self.lower_modifier(expression.span, call, PropertyId::BACKGROUND, function)
            }
            "Foreground" => {
                self.lower_modifier(expression.span, call, PropertyId::FOREGROUND, function)
            }
            "FontSize" => {
                self.lower_modifier(expression.span, call, PropertyId::FONT_SIZE, function)
            }
            "FontWeight" => {
                self.lower_modifier(expression.span, call, PropertyId::FONT_WEIGHT, function)
            }
            "Align" => self.lower_modifier(expression.span, call, PropertyId::ALIGN, function),
            "Justify" => self.lower_modifier(expression.span, call, PropertyId::JUSTIFY, function),
            "GridColumns" => {
                self.lower_modifier(expression.span, call, PropertyId::GRID_COLUMNS, function)
            }
            "GridTemplateAreas" => self.lower_modifier(
                expression.span,
                call,
                PropertyId::GRID_TEMPLATE_AREAS,
                function,
            ),
            "GridArea" => {
                self.lower_modifier(expression.span, call, PropertyId::GRID_AREA, function)
            }
            "Overflow" => {
                self.lower_modifier(expression.span, call, PropertyId::OVERFLOW, function)
            }
            "Radius" => self.lower_modifier(expression.span, call, PropertyId::RADIUS, function),
            "BorderColor" => {
                self.lower_modifier(expression.span, call, PropertyId::BORDER_COLOR, function)
            }
            "BorderWidth" => {
                self.lower_modifier(expression.span, call, PropertyId::BORDER_WIDTH, function)
            }
            "ScrollX" => self.lower_modifier(expression.span, call, PropertyId::SCROLL_X, function),
            "ScrollY" => self.lower_modifier(expression.span, call, PropertyId::SCROLL_Y, function),
            "Disabled" => {
                self.lower_modifier(expression.span, call, PropertyId::DISABLED, function)
            }
            "Checked" => self.lower_modifier(expression.span, call, PropertyId::CHECKED, function),
            "Role" => self.lower_modifier(expression.span, call, PropertyId::ROLE, function),
            "AccessibleName" => {
                self.lower_modifier(expression.span, call, PropertyId::ACCESSIBLE_NAME, function)
            }
            "AccessibleDescription" => self.lower_modifier(
                expression.span,
                call,
                PropertyId::ACCESSIBLE_DESCRIPTION,
                function,
            ),
            "AccessibleValue" => {
                self.lower_modifier(expression.span, call, PropertyId::VALUE, function)
            }
            "Required" => {
                self.lower_modifier(expression.span, call, PropertyId::REQUIRED, function)
            }
            "Invalid" => self.lower_modifier(expression.span, call, PropertyId::INVALID, function),
            "Selected" => {
                self.lower_modifier(expression.span, call, PropertyId::SELECTED, function)
            }
            "Expanded" => {
                self.lower_modifier(expression.span, call, PropertyId::EXPANDED, function)
            }
            "Pressed" => self.lower_modifier(expression.span, call, PropertyId::PRESSED, function),
            "Current" => self.lower_modifier(expression.span, call, PropertyId::CURRENT, function),
            "Hidden" => self.lower_modifier(expression.span, call, PropertyId::HIDDEN, function),
            "AccessibilityHidden" => self.lower_modifier(
                expression.span,
                call,
                PropertyId::ACCESSIBILITY_HIDDEN,
                function,
            ),
            "Focusable" => {
                self.lower_modifier(expression.span, call, PropertyId::FOCUSABLE, function)
            }
            "Source" => self.lower_modifier(expression.span, call, PropertyId::SOURCE, function),
            "ContentType" => {
                self.lower_modifier(expression.span, call, PropertyId::CONTENT_TYPE, function)
            }
            "Fit" => self.lower_modifier(expression.span, call, PropertyId::FIT, function),
            "Opacity" => self.lower_modifier(expression.span, call, PropertyId::OPACITY, function),
            "Transform" => {
                self.lower_modifier(expression.span, call, PropertyId::TRANSFORM, function)
            }
            "GraphicsProgram" => self.lower_modifier(
                expression.span,
                call,
                PropertyId::GRAPHICS_PROGRAM,
                function,
            ),
            "MediaState" => {
                self.lower_modifier(expression.span, call, PropertyId::MEDIA_STATE, function)
            }
            "Poster" => self.lower_modifier(expression.span, call, PropertyId::POSTER, function),
            "Modal" => self.lower_modifier(expression.span, call, PropertyId::MODAL, function),
            "AutoFocus" => {
                self.lower_modifier(expression.span, call, PropertyId::AUTO_FOCUS, function)
            }
            "PointerEvents" => {
                self.lower_modifier(expression.span, call, PropertyId::POINTER_EVENTS, function)
            }
            "CapturePointer" => {
                self.lower_modifier(expression.span, call, PropertyId::POINTER_CAPTURE, function)
            }
            "FlowDirection" => {
                self.lower_modifier(expression.span, call, PropertyId::FLOW_DIRECTION, function)
            }
            "Portal" => {
                self.lower_modifier(expression.span, call, PropertyId::PORTAL_LAYER, function)
            }
            "FocusRequest" => {
                self.lower_modifier(expression.span, call, PropertyId::FOCUS_REQUEST, function)
            }
            "Key" => self.lower_component_key(expression, call),
            "SelectionStartUTF16" => self.lower_modifier(
                expression.span,
                call,
                PropertyId::SELECTION_START_UTF16,
                function,
            ),
            "SelectionLengthUTF16" => self.lower_modifier(
                expression.span,
                call,
                PropertyId::SELECTION_LENGTH_UTF16,
                function,
            ),
            "TestID" => self.lower_modifier(expression.span, call, PropertyId::TEST_ID, function),
            "OnClick" => {
                self.lower_listener_modifier(expression.span, call, EventType::CLICK, function)
            }
            "OnSubmit" => {
                self.lower_listener_modifier(expression.span, call, EventType::SUBMIT, function)
            }
            "OnFocus" => {
                self.lower_listener_modifier(expression.span, call, EventType::FOCUS, function)
            }
            "OnBlur" => {
                self.lower_listener_modifier(expression.span, call, EventType::BLUR, function)
            }
            "OnKeyDown" => {
                self.lower_listener_modifier(expression.span, call, EventType::KEY_DOWN, function)
            }
            "OnKeyDownCapture" => self.lower_capture_listener_modifier(
                expression.span,
                call,
                EventType::KEY_DOWN,
                function,
            ),
            "OnKeyUp" => {
                self.lower_listener_modifier(expression.span, call, EventType::KEY_UP, function)
            }
            "OnPointerDown" => self.lower_listener_modifier(
                expression.span,
                call,
                EventType::POINTER_DOWN,
                function,
            ),
            "OnPointerMove" => self.lower_listener_modifier(
                expression.span,
                call,
                EventType::POINTER_MOVE,
                function,
            ),
            "OnPointerUp" => {
                self.lower_listener_modifier(expression.span, call, EventType::POINTER_UP, function)
            }
            "OnPointerCancel" => self.lower_listener_modifier(
                expression.span,
                call,
                EventType::POINTER_CANCEL,
                function,
            ),
            "OnScroll" => {
                self.lower_listener_modifier(expression.span, call, EventType::SCROLL, function)
            }
            "OnCompositionStart" => self.lower_listener_modifier(
                expression.span,
                call,
                EventType::COMPOSITION_START,
                function,
            ),
            "OnCompositionUpdate" => self.lower_listener_modifier(
                expression.span,
                call,
                EventType::COMPOSITION_UPDATE,
                function,
            ),
            "OnCompositionEnd" => self.lower_listener_modifier(
                expression.span,
                call,
                EventType::COMPOSITION_END,
                function,
            ),
            "OnSelectionChange" => self.lower_listener_modifier(
                expression.span,
                call,
                EventType::SELECTION_CHANGE,
                function,
            ),
            "OnWheel" => {
                self.lower_listener_modifier(expression.span, call, EventType::WHEEL, function)
            }
            "OnLayout" => {
                self.lower_listener_modifier(expression.span, call, EventType::LAYOUT, function)
            }
            _ => Err(error(
                expression.span,
                CompileErrorKind::UnsupportedUiCall(function.to_string()),
            )),
        }
    }

    fn lower_component_call(
        &mut self,
        expression: &Expr,
        call: &CallExpr,
    ) -> Result<LocalNodeId, CompileError> {
        let span = expression.span;
        if call.spread {
            return Err(error(span, CompileErrorKind::ExpectedUiCall));
        }
        let Some(callee_object) = called_function_object(self.type_info, &call.func) else {
            return Err(error(span, CompileErrorKind::ExpectedUiCall));
        };
        if find_component_function(self.project, callee_object).is_none() {
            return Err(error(span, CompileErrorKind::ImportedRootUnsupported));
        }
        let callee = component_type_id(self.project, callee_object);
        let anchor = self.next_node_id()?;
        self.push_node(TemplateNode::element(anchor, Primitive::Fragment));
        let props = call
            .args
            .iter()
            .map(|argument| ComponentPropBinding {
                expression: argument.id,
                expression_span: argument.span,
                dependencies: self.expression_state(argument),
                prop_dependencies: self.expression_props(argument),
            })
            .collect();
        self.component_calls.push(CompiledComponentCall {
            id: stable_component_call_site_id(&self.component_type, &callee, span),
            expression: expression.id,
            wrapper_expression: None,
            callee_object,
            callee,
            span,
            mount_parent: anchor,
            mount_before: None,
            props,
            key: None,
        });
        Ok(anchor)
    }

    fn lower_component_key(
        &mut self,
        expression: &Expr,
        call: &CallExpr,
    ) -> Result<LocalNodeId, CompileError> {
        let span = expression.span;
        expect_arity(span, "Key", call, 2)?;
        let previous_calls = self.component_calls.len();
        let target = self.lower_view(&call.args[0])?;
        if self.component_calls.len() != previous_calls.saturating_add(1) {
            return Err(error(
                call.args[0].span,
                CompileErrorKind::UnsupportedUiCall("Key component target".to_string()),
            ));
        }
        let key = &call.args[1];
        let key_binding = ComponentPropBinding {
            expression: key.id,
            expression_span: key.span,
            dependencies: self.expression_state(key),
            prop_dependencies: self.expression_props(key),
        };
        let component = self
            .component_calls
            .last_mut()
            .expect("one component call was just lowered");
        component.wrapper_expression = Some(expression.id);
        component.key = Some(key_binding);
        Ok(target)
    }

    fn lower_container(
        &mut self,
        span: Span,
        call: &CallExpr,
        primitive: Primitive,
    ) -> Result<LocalNodeId, CompileError> {
        if call.spread {
            return Err(error(
                span,
                CompileErrorKind::UnsupportedUiCall("spread children".to_string()),
            ));
        }
        let id = self.push_node(TemplateNode::element(self.next_node_id()?, primitive));
        for child in &call.args {
            let child = self.lower_view(child)?;
            self.plan.nodes[id.index() as usize].children.push(child);
        }
        Ok(id)
    }

    fn lower_text(&mut self, span: Span, call: &CallExpr) -> Result<LocalNodeId, CompileError> {
        expect_arity(span, "Text", call, 1)?;
        let element = self.next_node_id()?;
        self.push_node(TemplateNode::element(element, Primitive::Text));
        let value = self.lower_text_value(&call.args[0], None)?;
        self.plan.nodes[element.index() as usize]
            .children
            .push(value);
        Ok(element)
    }

    fn lower_image(&mut self, span: Span, call: &CallExpr) -> Result<LocalNodeId, CompileError> {
        expect_arity(span, "Image", call, 2)?;
        let image = self.next_node_id()?;
        self.push_node(TemplateNode::element(image, Primitive::Image));
        self.bind_string_property(&call.args[0], image, PropertyId::SOURCE)?;
        self.bind_string_property(&call.args[1], image, PropertyId::ACCESSIBLE_NAME)?;
        Ok(image)
    }

    fn lower_canvas(&mut self, span: Span, call: &CallExpr) -> Result<LocalNodeId, CompileError> {
        expect_arity(span, "Canvas", call, 2)?;
        let canvas = self.next_node_id()?;
        self.push_node(
            TemplateNode::element(canvas, Primitive::Canvas)
                .property(Property::new(PropertyId::ROLE, "img")),
        );
        self.bind_string_property(&call.args[0], canvas, PropertyId::GRAPHICS_PROGRAM)?;
        self.bind_string_property(&call.args[1], canvas, PropertyId::ACCESSIBLE_NAME)?;
        Ok(canvas)
    }

    fn lower_platform_view(
        &mut self,
        span: Span,
        call: &CallExpr,
    ) -> Result<LocalNodeId, CompileError> {
        expect_arity(span, "PlatformView", call, 3)?;
        let platform = self.next_node_id()?;
        self.push_node(
            TemplateNode::element(platform, Primitive::PlatformView)
                .property(Property::new(PropertyId::ROLE, "group")),
        );
        self.bind_string_property(&call.args[0], platform, PropertyId::CONTENT_TYPE)?;
        self.bind_string_property(&call.args[1], platform, PropertyId::MEDIA_STATE)?;
        self.bind_string_property(&call.args[2], platform, PropertyId::ACCESSIBLE_NAME)?;
        Ok(platform)
    }

    fn lower_button(&mut self, span: Span, call: &CallExpr) -> Result<LocalNodeId, CompileError> {
        expect_arity(span, "Button", call, 2)?;
        let button = self.next_node_id()?;
        let handler_index = u32::try_from(self.handler_bindings.len()).map_err(|_| {
            error(
                call.args[1].span,
                CompileErrorKind::HandlerIdentityExhausted,
            )
        })?;
        let handler = HandlerId::new(handler_index, 1);
        let mut node = TemplateNode::element(button, Primitive::Button)
            .property(Property::new(PropertyId::ROLE, "button"))
            .listener(Listener::new(EventType::CLICK, handler));
        if let ExprKind::StringLit(literal) = &strip_parens(&call.args[0]).kind {
            node.properties.push(Property::new(
                PropertyId::ACCESSIBLE_NAME,
                literal.value.clone(),
            ));
        }
        self.push_node(node);
        let label = self.lower_text_value(&call.args[0], Some(button))?;
        self.plan.nodes[button.index() as usize]
            .children
            .push(label);
        let captured_state = self.handler_state(&call.args[1]);
        let captured_props = self.handler_props(&call.args[1]);
        self.handler_bindings.push(HandlerBinding {
            handler,
            expression: call.args[1].id,
            expression_span: call.args[1].span,
            captured_state,
            captured_props,
        });
        Ok(button)
    }

    fn lower_text_input(
        &mut self,
        span: Span,
        call: &CallExpr,
    ) -> Result<LocalNodeId, CompileError> {
        expect_arity(span, "TextInput", call, 3)?;
        let input = self.next_node_id()?;
        self.push_node(
            TemplateNode::element(input, Primitive::TextInput)
                .property(Property::new(PropertyId::ROLE, "textbox")),
        );
        self.bind_string_property(&call.args[0], input, PropertyId::VALUE)?;
        self.bind_string_property(&call.args[1], input, PropertyId::PLACEHOLDER)?;
        self.bind_handler(&call.args[2], input, EventType::INPUT)?;
        Ok(input)
    }

    fn lower_text_area(
        &mut self,
        span: Span,
        call: &CallExpr,
    ) -> Result<LocalNodeId, CompileError> {
        expect_arity(span, "TextArea", call, 3)?;
        let input = self.next_node_id()?;
        self.push_node(
            TemplateNode::element(input, Primitive::TextArea)
                .property(Property::new(PropertyId::ROLE, "textbox")),
        );
        self.bind_string_property(&call.args[0], input, PropertyId::VALUE)?;
        self.bind_string_property(&call.args[1], input, PropertyId::PLACEHOLDER)?;
        self.bind_handler(&call.args[2], input, EventType::INPUT)?;
        Ok(input)
    }

    fn lower_toggle(&mut self, span: Span, call: &CallExpr) -> Result<LocalNodeId, CompileError> {
        expect_arity(span, "Toggle", call, 3)?;
        let toggle = self.next_node_id()?;
        self.push_node(
            TemplateNode::element(toggle, Primitive::Toggle)
                .property(Property::new(PropertyId::ROLE, "switch")),
        );
        self.bind_property_slot(&call.args[0], toggle, PropertyId::CHECKED, true)?;
        self.bind_string_property(&call.args[1], toggle, PropertyId::ACCESSIBLE_NAME)?;
        self.bind_handler(&call.args[2], toggle, EventType::CHANGE)?;
        Ok(toggle)
    }

    fn lower_slider(&mut self, span: Span, call: &CallExpr) -> Result<LocalNodeId, CompileError> {
        expect_arity(span, "Slider", call, 6)?;
        let slider = self.next_node_id()?;
        self.push_node(
            TemplateNode::element(slider, Primitive::Slider)
                .property(Property::new(PropertyId::ROLE, "slider")),
        );
        self.bind_property_slot(&call.args[0], slider, PropertyId::VALUE, true)?;
        self.bind_property_slot(&call.args[1], slider, PropertyId::MIN_VALUE, true)?;
        self.bind_property_slot(&call.args[2], slider, PropertyId::MAX_VALUE, true)?;
        self.bind_property_slot(&call.args[3], slider, PropertyId::STEP_VALUE, true)?;
        self.bind_string_property(&call.args[4], slider, PropertyId::ACCESSIBLE_NAME)?;
        self.bind_handler(&call.args[5], slider, EventType::INPUT)?;
        Ok(slider)
    }

    fn bind_string_property(
        &mut self,
        expression: &Expr,
        target: LocalNodeId,
        property: PropertyId,
    ) -> Result<(), CompileError> {
        if let ExprKind::StringLit(literal) = &strip_parens(expression).kind {
            self.plan.nodes[target.index() as usize]
                .properties
                .push(Property::new(property, literal.value.clone()));
        } else {
            self.bind_property_slot(expression, target, property, true)?;
        }
        Ok(())
    }

    fn bind_handler(
        &mut self,
        expression: &Expr,
        target: LocalNodeId,
        event: EventType,
    ) -> Result<HandlerId, CompileError> {
        self.bind_handler_with_options(expression, target, event, ListenerOptions::default())
    }

    fn bind_handler_with_options(
        &mut self,
        expression: &Expr,
        target: LocalNodeId,
        event: EventType,
        options: ListenerOptions,
    ) -> Result<HandlerId, CompileError> {
        let handler_index = u32::try_from(self.handler_bindings.len())
            .map_err(|_| error(expression.span, CompileErrorKind::HandlerIdentityExhausted))?;
        let handler = HandlerId::new(handler_index, 1);
        self.plan.nodes[target.index() as usize]
            .listeners
            .push(Listener::new(event, handler).with_options(options));
        let captured_state = self.handler_state(expression);
        let captured_props = self.handler_props(expression);
        self.handler_bindings.push(HandlerBinding {
            handler,
            expression: expression.id,
            expression_span: expression.span,
            captured_state,
            captured_props,
        });
        Ok(handler)
    }

    fn lower_text_value(
        &mut self,
        expression: &Expr,
        accessible_target: Option<LocalNodeId>,
    ) -> Result<LocalNodeId, CompileError> {
        let id = self.next_node_id()?;
        if let ExprKind::StringLit(literal) = &strip_parens(expression).kind {
            self.push_node(TemplateNode::text(id, literal.value.clone()));
            return Ok(id);
        }
        self.push_node(TemplateNode::text(id, ""));
        let text_slot = self.bind_slot(expression, SlotKind::Text, UpdateSite::text, id)?;
        let mut slots = vec![text_slot];
        if let Some(target) = accessible_target {
            let property_slot =
                self.bind_property_slot(expression, target, PropertyId::ACCESSIBLE_NAME, false)?;
            slots.push(property_slot);
        }
        let dependencies = self.expression_state(expression);
        let prop_dependencies = self.expression_props(expression);
        self.slot_bindings.push(SlotBinding {
            expression: expression.id,
            expression_span: expression.span,
            slots,
            dependencies,
            prop_dependencies,
        });
        Ok(id)
    }

    fn lower_modifier(
        &mut self,
        span: Span,
        call: &CallExpr,
        property: PropertyId,
        function: &str,
    ) -> Result<LocalNodeId, CompileError> {
        expect_arity(span, function, call, 2)?;
        let target = self.lower_view(&call.args[0])?;
        if !matches!(
            self.plan.nodes[target.index() as usize].kind,
            TemplateNodeKind::Element(_)
        ) {
            return Err(error(
                call.args[0].span,
                CompileErrorKind::ModifierTargetMustBeElement(function.to_string()),
            ));
        }
        self.bind_property_slot(&call.args[1], target, property, true)?;
        Ok(target)
    }

    fn lower_listener_modifier(
        &mut self,
        span: Span,
        call: &CallExpr,
        event: EventType,
        function: &str,
    ) -> Result<LocalNodeId, CompileError> {
        expect_arity(span, function, call, 2)?;
        let target = self.lower_view(&call.args[0])?;
        if !matches!(
            self.plan.nodes[target.index() as usize].kind,
            TemplateNodeKind::Element(_)
        ) {
            return Err(error(
                call.args[0].span,
                CompileErrorKind::ModifierTargetMustBeElement(function.to_string()),
            ));
        }
        self.bind_handler(&call.args[1], target, event)?;
        Ok(target)
    }

    fn lower_capture_listener_modifier(
        &mut self,
        span: Span,
        call: &CallExpr,
        event: EventType,
        function: &str,
    ) -> Result<LocalNodeId, CompileError> {
        expect_arity(span, function, call, 2)?;
        let target = self.lower_view(&call.args[0])?;
        if !matches!(
            self.plan.nodes[target.index() as usize].kind,
            TemplateNodeKind::Element(_)
        ) {
            return Err(error(
                call.args[0].span,
                CompileErrorKind::ModifierTargetMustBeElement(function.to_string()),
            ));
        }
        self.bind_handler_with_options(
            &call.args[1],
            target,
            event,
            ListenerOptions {
                capture: true,
                passive: false,
                once: false,
            },
        )?;
        Ok(target)
    }

    fn bind_property_slot(
        &mut self,
        expression: &Expr,
        target: LocalNodeId,
        property: PropertyId,
        record_binding: bool,
    ) -> Result<SlotId, CompileError> {
        let slot = self.next_slot(SlotKind::Property)?;
        self.plan
            .updates
            .push(UpdateSite::property(slot, target, property));
        if record_binding {
            let dependencies = self.expression_state(expression);
            let prop_dependencies = self.expression_props(expression);
            self.slot_bindings.push(SlotBinding {
                expression: expression.id,
                expression_span: expression.span,
                slots: vec![slot],
                dependencies,
                prop_dependencies,
            });
        }
        Ok(slot)
    }

    fn bind_slot(
        &mut self,
        _expression: &Expr,
        kind: SlotKind,
        update: impl FnOnce(SlotId, LocalNodeId) -> UpdateSite,
        target: LocalNodeId,
    ) -> Result<SlotId, CompileError> {
        let slot = self.next_slot(kind)?;
        self.plan.updates.push(update(slot, target));
        Ok(slot)
    }

    fn next_node_id(&self) -> Result<LocalNodeId, CompileError> {
        u32::try_from(self.plan.nodes.len())
            .map(LocalNodeId::new)
            .map_err(|_| {
                error(
                    Span::dummy(),
                    CompileErrorKind::Plan(PlanError::NodeLimitExceeded),
                )
            })
    }

    fn push_node(&mut self, node: TemplateNode) -> LocalNodeId {
        let id = node.id;
        self.plan.nodes.push(node);
        id
    }

    fn next_slot(&mut self, kind: SlotKind) -> Result<SlotId, CompileError> {
        let index = u32::try_from(self.plan.slots.len()).map_err(|_| {
            error(
                Span::dummy(),
                CompileErrorKind::Plan(PlanError::SlotLimitExceeded),
            )
        })?;
        self.plan.slots.push(kind);
        Ok(SlotId::new(index))
    }

    fn expression_state(&self, expression: &Expr) -> Vec<StateId> {
        let mut finder = StateUseFinder {
            type_info: self.type_info,
            state_ids: &self.state_ids,
            found: BTreeSet::new(),
        };
        finder.visit_expr(expression);
        finder.found.into_iter().collect()
    }

    fn expression_props(&self, expression: &Expr) -> Vec<u16> {
        expression_props(self.type_info, &self.prop_ids, expression)
    }

    fn handler_state(&self, expression: &Expr) -> Vec<StateId> {
        let mut state = self.expression_state(expression);
        state.extend(
            handler_captures(self.type_info, expression)
                .into_iter()
                .filter_map(|object| self.state_ids.get(&object).copied()),
        );
        state.sort_unstable();
        state.dedup();
        state
    }

    fn handler_props(&self, expression: &Expr) -> Vec<u16> {
        let mut props = self.expression_props(expression);
        props.extend(
            handler_captures(self.type_info, expression)
                .into_iter()
                .filter_map(|object| self.prop_ids.get(&object).copied()),
        );
        props.sort_unstable();
        props.dedup();
        props
    }
}

/// Handler expressions may be a closure directly or a typed public adapter
/// such as `ui.Action(func() { ... })`. Collect captures from every nested
/// function literal so ergonomic adapters retain the exact dependency graph
/// used by direct VM/JIT/AOT component updates.
fn handler_captures(type_info: &TypeInfo, expression: &Expr) -> HashSet<ObjKey> {
    struct Finder<'a> {
        type_info: &'a TypeInfo,
        found: HashSet<ObjKey>,
    }

    impl Visitor for Finder<'_> {
        fn visit_expr(&mut self, expression: &Expr) {
            if matches!(expression.kind, ExprKind::FuncLit(_)) {
                for capture in self
                    .type_info
                    .closure_captures
                    .get(&expression.id)
                    .into_iter()
                    .flatten()
                {
                    self.found.insert(*capture);
                }
            }
            ast::walk_expr(self, expression);
        }
    }

    let mut finder = Finder {
        type_info,
        found: HashSet::new(),
    };
    finder.visit_expr(expression);
    finder.found
}

struct StateUseFinder<'a> {
    type_info: &'a TypeInfo,
    state_ids: &'a HashMap<ObjKey, StateId>,
    found: BTreeSet<StateId>,
}

impl Visitor for StateUseFinder<'_> {
    fn visit_expr(&mut self, expression: &Expr) {
        if let ExprKind::Ident(identifier) = &expression.kind {
            if let Some(state) = self
                .type_info
                .get_use(identifier)
                .and_then(|object| self.state_ids.get(&object))
            {
                self.found.insert(*state);
            }
        }
        ast::walk_expr(self, expression);
    }
}

fn expression_props(
    type_info: &TypeInfo,
    prop_ids: &HashMap<ObjKey, u16>,
    expression: &Expr,
) -> Vec<u16> {
    let mut finder = PropUseFinder {
        type_info,
        prop_ids,
        found: BTreeSet::new(),
    };
    finder.visit_expr(expression);
    finder.found.into_iter().collect()
}

struct PropUseFinder<'a> {
    type_info: &'a TypeInfo,
    prop_ids: &'a HashMap<ObjKey, u16>,
    found: BTreeSet<u16>,
}

impl Visitor for PropUseFinder<'_> {
    fn visit_expr(&mut self, expression: &Expr) {
        if let ExprKind::Ident(identifier) = &expression.kind {
            if let Some(prop) = self
                .type_info
                .get_use(identifier)
                .and_then(|object| self.prop_ids.get(&object))
            {
                self.found.insert(*prop);
            }
        }
        ast::walk_expr(self, expression);
    }
}

fn expect_arity(
    span: Span,
    function: &str,
    call: &CallExpr,
    expected: usize,
) -> Result<(), CompileError> {
    if !call.spread && call.args.len() == expected {
        return Ok(());
    }
    Err(error(
        span,
        CompileErrorKind::InvalidCallArity {
            function: function.to_string(),
            expected: match expected {
                1 => "1",
                2 => "2",
                _ => "fixed",
            },
            found: call.args.len(),
        },
    ))
}

fn ui_function_name<'a>(
    project: &'a Project,
    type_info: &TypeInfo,
    expression: &Expr,
) -> Option<&'a str> {
    let ExprKind::Selector(selector) = &strip_parens(expression).kind else {
        return None;
    };
    let ExprKind::Ident(package_name) = &strip_parens(&selector.expr).kind else {
        return None;
    };
    let package_object = type_info.get_use(package_name)?;
    let package_object = &project.tc_objs.lobjs[package_object];
    if !package_object.entity_type().is_pkg_name() {
        return None;
    }
    let imported = package_object.pkg_name_imported();
    if project.tc_objs.pkgs[imported].path() != UI_MODULE_PATH {
        return None;
    }
    let function_object = type_info.get_use(&selector.sel)?;
    let object = &project.tc_objs.lobjs[function_object];
    let owner = object.pkg()?;
    (project.tc_objs.pkgs[owner].path() == UI_MODULE_PATH).then_some(object.name())
}

fn called_function_object(type_info: &TypeInfo, expression: &Expr) -> Option<ObjKey> {
    match &strip_parens(expression).kind {
        ExprKind::Ident(identifier) => type_info.get_use(identifier),
        ExprKind::Selector(selector) => type_info.get_use(&selector.sel),
        _ => None,
    }
}

fn stable_component_call_site_id(
    parent: &ComponentTypeId,
    callee: &ComponentTypeId,
    span: Span,
) -> ComponentCallSiteId {
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for byte in b"volang-ui-component-call-v1\0"
        .iter()
        .copied()
        .chain(parent.module().bytes())
        .chain([0])
        .chain(parent.object().bytes())
        .chain([0])
        .chain(callee.module().bytes())
        .chain([0])
        .chain(callee.object().bytes())
        .chain(span.start.to_u32().to_le_bytes())
    {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    ComponentCallSiteId::new(hash)
}

fn strip_parens(mut expression: &Expr) -> &Expr {
    while let ExprKind::Paren(inner) = &expression.kind {
        expression = inner;
    }
    expression
}

fn error(span: Span, kind: CompileErrorKind) -> CompileError {
    CompileError { span, kind }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use vo_analysis::project::PackageIdentity;
    use vo_analysis::vfs::{
        analyze_file_set_with_package_identity, ModSource, PackageResolverMixed, StdSource,
    };
    use vo_common::vfs::{FileSet, MemoryFs};

    fn analyze(source: &str) -> Project {
        let module_fs = MemoryFs::new()
            .with_file(
                "github.com/vo-lang/ui/vo.mod",
                include_str!("../../../../ui/vo.mod"),
            )
            .with_file(
                "github.com/vo-lang/ui/ui.vo",
                include_str!("../../../../ui/ui.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/kit/kit.vo",
                include_str!("../../../../ui/kit/kit.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/kit/components/components.vo",
                include_str!("../../../../ui/kit/components/components.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/kit/data/data.vo",
                include_str!("../../../../ui/kit/data/data.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/kit/headless/headless.vo",
                include_str!("../../../../ui/kit/headless/headless.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/kit/tokens/tokens.vo",
                include_str!("../../../../ui/kit/tokens/tokens.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/desktop/desktop.vo",
                include_str!("../../../../ui/desktop/desktop.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/forms/forms.vo",
                include_str!("../../../../ui/forms/forms.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/gesture/gesture.vo",
                include_str!("../../../../ui/gesture/gesture.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/i18n/core/core.vo",
                include_str!("../../../../ui/i18n/core/core.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/i18n/i18n.vo",
                include_str!("../../../../ui/i18n/i18n.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/motion/motion.vo",
                include_str!("../../../../ui/motion/motion.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/resource/resource.vo",
                include_str!("../../../../ui/resource/resource.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/task/task.vo",
                include_str!("../../../../ui/task/task.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/commands/commands.vo",
                include_str!("../../../../ui/commands/commands.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/navigation/navigation.vo",
                include_str!("../../../../ui/navigation/navigation.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/persistence/persistence.vo",
                include_str!("../../../../ui/persistence/persistence.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/platform/platform.vo",
                include_str!("../../../../ui/platform/platform.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/web/web.vo",
                include_str!("../../../../ui/web/web.vo"),
            )
            .with_file(
                "github.com/vo-lang/ui/web/server/server.vo",
                include_str!("../../../../ui/web/server/server.vo"),
            )
            .with_file(
                "github.com/acme/components/vo.mod",
                r#"format = 1
module = "github.com/acme/components"
version = "0.1.0"
vo = "0.1.0"
"#,
            )
            .with_file(
                "github.com/acme/components/components.vo",
                r#"package components
import "github.com/vo-lang/ui"
func Label(value string) ui.View { return ui.Text(value) }
"#,
            );
        let resolver = PackageResolverMixed {
            std: StdSource::new(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../stdlib")),
            r#mod: ModSource::with_fs(module_fs),
        };
        let mut files = FileSet::new(PathBuf::from("."));
        files
            .files
            .insert(PathBuf::from("main.vo"), source.to_string());
        analyze_file_set_with_package_identity(
            files,
            resolver,
            MemoryFs::new(),
            PathBuf::from("."),
            Some("github.com/acme/app".to_string()),
            Some(PackageIdentity::new("github.com/acme/app").unwrap()),
        )
        .unwrap()
    }

    #[test]
    fn typed_alias_calls_lower_to_static_nodes_slots_and_handlers() {
        let project = analyze(
            r#"
package main

import vu "github.com/vo-lang/ui"

var title = "Counter"

func App() vu.View {
	return vu.Padding(vu.Column(
		vu.Text(title),
		vu.Button("Increment", func(event vu.Event) {}),
	), 12)
}

func main() {
	if err := vu.Mount(App); err != nil {
		panic(err.Error())
	}
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .unwrap();
        assert_eq!(program.root.name, "App");
        assert_eq!(program.root.plan.nodes().len(), 5);
        assert_eq!(
            program.root.plan.slots(),
            &[SlotKind::Text, SlotKind::Property]
        );
        assert_eq!(program.root.slot_bindings.len(), 2);
        assert_eq!(program.root.handler_bindings.len(), 1);
        assert_eq!(program.root.handler_bindings[0].handler.index(), 0);
        assert_eq!(
            program.root.plan.node(LocalNodeId::new(0)).kind,
            TemplateNodeKind::Element(Primitive::Column)
        );
        assert_eq!(
            program.root.plan.node(LocalNodeId::new(1)).kind,
            TemplateNodeKind::Element(Primitive::Text)
        );
        assert_eq!(
            program.root.plan.node(LocalNodeId::new(2)).kind,
            TemplateNodeKind::Text
        );
        assert_eq!(
            program.root.plan.node(LocalNodeId::new(3)).kind,
            TemplateNodeKind::Element(Primitive::Button)
        );
    }

    #[test]
    fn typed_callback_adapters_preserve_direct_handler_captures() {
        let project = analyze(
            r#"
package main

import "github.com/vo-lang/ui"

func Counter(prefix string) ui.View {
	count := 0
	return ui.Button(prefix, ui.Action(func() { count++ }))
}

func WrappedAction(onAction func()) ui.View {
	return ui.Button("Wrapped", ui.Action(onAction))
}

func App() ui.View {
	count := 0
	return ui.Column(Counter("Count"), WrappedAction(func() { count++ }))
}

func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .unwrap();
        let counter = program
            .components
            .iter()
            .find(|component| component.name == "Counter")
            .unwrap();

        assert_eq!(counter.handler_bindings.len(), 1);
        assert_eq!(
            counter.handler_bindings[0].captured_state,
            vec![StateId::new(0)]
        );
        assert_eq!(
            counter.handler_bindings[0].captured_props,
            Vec::<u16>::new()
        );
        assert_eq!(
            counter.state_bindings[0].captured_by_handlers,
            vec![HandlerId::new(0, 1)]
        );

        let wrapped = program
            .components
            .iter()
            .find(|component| component.name == "WrappedAction")
            .unwrap();
        assert_eq!(wrapped.handler_bindings.len(), 1);
        assert_eq!(wrapped.handler_bindings[0].captured_props, vec![0]);

        let bundle =
            build_component_bundle_with_functions(&program, |_, expression| Some(expression.0))
                .unwrap();
        let definition = bundle
            .definitions
            .iter()
            .find(|definition| definition.display_name == "Counter")
            .unwrap();
        assert_eq!(definition.mode, ExecutionMode::Direct);
        assert_eq!(
            definition.handlers[0].captured_state,
            vec![StateFieldId::new(0)]
        );
    }

    #[test]
    fn direct_artifacts_accept_explicit_and_automatic_cells_with_complete_evaluators() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
func App() ui.View {
	name := ui.UseStringState("Volang")
	return ui.TextInput(ui.StringStateValue(name), "Name", func(event ui.Event) {
		ui.SetStringState(name, event.Text)
	})
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .unwrap();
        assert_eq!(
            program.root.state_bindings[0].runtime_cell,
            Some(RuntimeCellKind::String)
        );
        assert!(program.root.state_bindings[0]
            .initializer_dependencies
            .is_empty());
        let direct =
            build_component_artifact_with_functions(&program, |expression| Some(expression.0));
        assert_eq!(direct.mode, ExecutionMode::Direct);
        assert!(direct.states[0].initializer_func.is_some());
        assert!(direct.slots[0].evaluator_func.is_some());

        let incomplete = build_component_artifact(&program);
        assert_eq!(incomplete.mode, ExecutionMode::RootFallback);

        let ordinary = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
func App() ui.View {
	name := "Volang"
	return ui.Text(name)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let ordinary = compile_project_ui(&ordinary, PlanLimits::default())
            .unwrap()
            .unwrap();
        let fallback =
            build_component_artifact_with_functions(&ordinary, |expression| Some(expression.0));
        assert_eq!(fallback.mode, ExecutionMode::Direct);
        assert!(ordinary.root.state_bindings[0].automatic_cell);
        assert_eq!(
            ordinary.root.state_bindings[0].runtime_cell,
            Some(RuntimeCellKind::String)
        );
    }

    #[test]
    fn derived_markers_do_not_persist_immutable_locals_as_component_state() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
func App() ui.View {
	count := ui.UseIntState(1)
	label := ui.DerivedString("Count")
	doubled := ui.DerivedInt(ui.IntStateValue(count) * 2)
	return ui.Text(label)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .unwrap();
        assert_eq!(program.root.state_bindings.len(), 3);
        assert_eq!(
            program.root.state_bindings[0].runtime_cell,
            Some(RuntimeCellKind::Int)
        );
        assert!(!program.root.state_bindings[1].automatic_cell);
        assert_eq!(program.root.state_bindings[1].runtime_cell, None);
        assert!(!program.root.state_bindings[2].automatic_cell);
        assert_eq!(program.root.state_bindings[2].runtime_cell, None);
        assert_eq!(
            build_component_artifact_with_functions(&program, |expression| Some(expression.0)).mode,
            ExecutionMode::RootFallback
        );
    }

    #[test]
    fn non_ui_projects_do_not_gain_ui_artifacts() {
        let project = analyze(
            r#"
package main
func main() {}
"#,
        );
        assert!(compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .is_none());
        assert_eq!(
            compile_project_ui_outcome(&project, PlanLimits::default()).unwrap(),
            UiCompileOutcome::NoMount
        );
    }

    #[test]
    fn library_composition_uses_the_generic_runtime_fallback() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
func Card(child ui.View) ui.View {
	return ui.Padding(ui.Background(child, 0xffffffff), 16)
}
func App() ui.View {
	return Card(ui.Text("from library"))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        assert!(compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .is_none());
    }

    #[test]
    fn reachable_zero_prop_components_emit_one_canonical_bundle_definition_each() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
func Child() ui.View { return ui.Text("child") }
func App() ui.View { return ui.Column(Child(), Child()) }
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .expect("component graph");
        assert_eq!(program.root.type_id.object(), "App");
        assert_eq!(program.root.component_calls.len(), 2);
        assert_ne!(
            program.root.component_calls[0].id,
            program.root.component_calls[1].id
        );
        assert_eq!(program.components.len(), 1);
        assert_eq!(program.components[0].type_id.object(), "Child");

        let bytes = encode_ui_component_bundle_with_functions(
            &program,
            BundleLimits::default(),
            PlanLimits::default(),
            |_, expression| Some(expression.0),
        )
        .expect("encode component graph");
        let bundle = vo_ui_artifact::decode_component_bundle(
            &bytes,
            BundleLimits::default(),
            PlanLimits::default(),
        )
        .expect("decode component graph");
        assert_eq!(bundle.definitions.len(), 2);
        assert_eq!(bundle.definitions[0].type_id.object(), "App");
        assert_eq!(bundle.definitions[1].type_id.object(), "Child");
        assert_eq!(bundle.definitions[0].call_sites.len(), 2);
        assert!(bundle.definitions[1].call_sites.is_empty());
    }

    #[test]
    fn keyed_static_component_calls_emit_typed_key_evaluators() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
func Child(label string) ui.View { return ui.Text(label) }
func App() ui.View { return ui.Column(ui.Key(Child("child"), "stable")) }
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .expect("keyed component graph");
        let call = &program.root.component_calls[0];
        assert!(call.key.is_some());
        let bundle =
            build_component_bundle_with_functions(&program, |_, expression| Some(expression.0))
                .unwrap();
        let root = bundle
            .definitions
            .iter()
            .find(|definition| definition.type_id == bundle.root)
            .unwrap();
        let key = root.call_sites[0].key_binding.expect("key binding");
        assert!(root
            .bindings
            .iter()
            .any(|binding| binding.id == key && binding.evaluator_func.is_some()));
        assert_eq!(root.mode, ExecutionMode::Direct);
    }

    #[test]
    fn typed_props_flow_into_child_initializers_and_bindings() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
func Label(initial string) ui.View {
	value := initial
	return ui.Text(value)
}
func App() ui.View {
	title := "ready"
	return Label(title)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .expect("typed component graph");
        assert_eq!(program.root.component_calls.len(), 1);
        assert_eq!(
            program.root.component_calls[0].props[0].dependencies,
            vec![StateId::new(0)]
        );
        assert!(program.root.component_calls[0].props[0]
            .prop_dependencies
            .is_empty());

        let child = &program.components[0];
        assert_eq!(child.type_id.object(), "Label");
        assert_eq!(child.props_arity, 1);
        assert_eq!(
            child.state_bindings[0].initializer_prop_dependencies,
            vec![0]
        );

        let bundle =
            build_component_bundle_with_functions(&program, |_, expression| Some(expression.0))
                .expect("build typed component bundle");
        vo_ui_artifact::validate_component_bundle(&bundle, BundleLimits::default())
            .expect("validate typed component bundle");
        let child = bundle
            .definitions
            .iter()
            .find(|definition| definition.type_id.object() == "Label")
            .unwrap();
        assert_eq!(child.interface.props_arity, 1);
        assert_eq!(child.states[0].initializer_props, vec![0]);
        assert_eq!(child.bindings[0].dependencies, vec![StateFieldId::new(0)]);
    }

    #[test]
    fn imported_source_components_link_by_canonical_object_identity() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
import components "github.com/acme/components"
func App() ui.View {
	label := "linked"
	return components.Label(label)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .expect("cross-package component graph");
        assert_eq!(program.components.len(), 1);
        assert_eq!(
            program.components[0].type_id.module(),
            "github.com/acme/components"
        );
        assert_eq!(program.components[0].type_id.object(), "Label");

        let bundle =
            build_component_bundle_with_functions(&program, |_, expression| Some(expression.0))
                .expect("build linked component bundle");
        vo_ui_artifact::validate_component_bundle(&bundle, BundleLimits::default())
            .expect("validate linked component bundle");
        assert_eq!(
            bundle.linked_modules,
            vec!["github.com/acme/components".to_string()]
        );
        assert!(bundle
            .definitions
            .iter()
            .any(|definition| definition.type_id
                == ComponentTypeId::new("github.com/acme/components", "Label")));
    }

    #[test]
    fn dynamic_component_control_flow_uses_the_generic_runtime_fallback() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
var detailed = true
func App() ui.View {
	if detailed { return ui.Text("details") }
	return ui.Text("summary")
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        assert!(compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .is_none());
        assert!(matches!(
            compile_project_ui_outcome(&project, PlanLimits::default()).unwrap(),
            UiCompileOutcome::GenericFallback(CompileError {
                kind: CompileErrorKind::UnsupportedComponentPrelude,
                ..
            })
        ));
    }

    #[test]
    fn multi_result_component_locals_remain_owned_by_generic_volang_execution() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
func dimensions() (float64, float64) { return 320, 180 }
func App() ui.View {
	width, height := dimensions()
	return ui.Width(ui.Height(ui.Text("preview"), height), width)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        assert!(matches!(
            compile_project_ui_outcome(&project, PlanLimits::default()).unwrap(),
            UiCompileOutcome::GenericFallback(CompileError {
                kind: CompileErrorKind::UnsupportedComponentPrelude,
                ..
            })
        ));
        let discovery = discover_project_ui_runtime(&project)
            .unwrap()
            .expect("UI runtime discovery");
        assert!(discovery.state_bindings.is_empty());
    }

    #[test]
    fn dynamic_keyed_components_keep_runtime_scopes_and_component_state() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
func Counter(label string) ui.View {
	count := int64(0)
	return ui.Button(label, func(event ui.Event) { count++ })
}
func App() ui.View {
	reversed := false
	if reversed {
		return ui.Column(
			ui.Key(Counter("Beta"), "beta"),
			ui.Key(Counter("Alpha"), "alpha"),
		)
	}
	return ui.Column(
		ui.Key(Counter("Alpha"), "alpha"),
		ui.Key(Counter("Beta"), "beta"),
	)
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
        );
        assert!(compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .is_none());
        let discovery = discover_project_ui_runtime(&project)
            .unwrap()
            .expect("runtime discovery");
        assert_eq!(
            discovery
                .state_bindings
                .iter()
                .map(|state| state.key.as_str())
                .collect::<Vec<_>>(),
            vec!["reversed", "count"]
        );
        assert_eq!(discovery.component_scopes.len(), 4);
        assert!(discovery
            .component_scopes
            .iter()
            .all(|scope| scope.key.is_some() && scope.identity.ends_with("::Counter")));
        assert_eq!(
            discovery
                .component_scopes
                .iter()
                .map(|scope| scope.call_site)
                .collect::<HashSet<_>>()
                .len(),
            4
        );
    }

    #[test]
    fn imported_components_inside_control_flow_are_runtime_scoped() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
import "github.com/acme/components"
func App() ui.View {
	detailed := true
	if detailed {
		return ui.Key(components.Label("Details"), "details")
	}
	return ui.Key(components.Label("Summary"), "summary")
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
        );
        let discovery = discover_project_ui_runtime(&project)
            .unwrap()
            .expect("runtime discovery");
        assert_eq!(discovery.component_scopes.len(), 2);
        assert!(discovery.component_scopes.iter().all(|scope| {
            scope.key.is_some() && scope.identity == "github.com/acme/components::Label"
        }));
        assert_eq!(
            discovery
                .state_bindings
                .iter()
                .map(|state| state.key.as_str())
                .collect::<Vec<_>>(),
            vec!["detailed"]
        );
    }

    #[test]
    fn official_kit_package_compiles_through_the_generic_runtime_path() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
import "github.com/vo-lang/ui/kit"
var title = "Settings"
func App() ui.View {
	theme := kit.LightTheme()
	window := kit.VisibleRange(100, 20, 200, 100, 2)
	if window.Start != 8 || window.End != 18 { panic("invalid virtual range") }
	list := kit.VirtualList(100, 20, 200, 100, 2, func(index int64) ui.View {
		return ui.Text(title)
	}, func(event ui.Event) {})
	return kit.Screen(theme, kit.Card(theme, list))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        assert!(compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .is_none());
    }

    #[test]
    fn official_resource_package_type_checks_with_compiled_root_projection() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
import "github.com/vo-lang/ui/resource"
func App() ui.View {
	loaded := resource.UseString("key", func() (string, error) { return "ready", nil })
	return ui.Text(loaded.Value())
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .unwrap();
        assert_eq!(program.root.state_bindings.len(), 1);
        assert_eq!(program.root.plan.slots(), &[SlotKind::Text]);
    }

    #[test]
    fn official_navigation_package_type_checks_through_the_generic_runtime_path() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
import "github.com/vo-lang/ui/navigation"
func App() ui.View {
	return navigation.Resolve(ui.Text("missing"),
		navigation.Route{Path: "/", View: navigation.Link("Home", "/")},
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        assert!(compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .is_none());
    }

    #[test]
    fn official_e4_application_packages_type_check_together() {
        let project = analyze(
            r#"
package main
import (
	"github.com/vo-lang/ui"
	"github.com/vo-lang/ui/commands"
	"github.com/vo-lang/ui/forms"
	"github.com/vo-lang/ui/navigation"
	"github.com/vo-lang/ui/persistence"
	"github.com/vo-lang/ui/resource"
)
func App() ui.View {
	form := forms.Use(forms.FieldSpec{Name: "name", Label: "Name", Initial: "Ada", Required: true})
	history := persistence.NewHistory(8, "Ada")
	_ = history.Push(form.Field("name").Value)
	page := resource.NewPage(0, 25, 80)
	pattern := navigation.NewPattern("member", navigation.Static("members"), navigation.Parameter("id"))
	_, _ = pattern.Match(navigation.Location{Path: "/members/42"})
	scope := commands.NewScope(commands.Action("save", "Save", func() { form.Validate() }))
	content := ui.Column(forms.TextField(form, "name"), ui.Text(string(rune(page.Count()))), scope.Palette("Commands", "save", 8))
	return scope.Bind(content)
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
        );
        let _ = compile_project_ui(&project, PlanLimits::default()).unwrap();
    }

    #[test]
    fn dynamic_button_label_maps_one_expression_to_text_and_accessibility_slots() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
var label = "Save"
func App() ui.View {
	return ui.Button(label, func(event ui.Event) {})
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .unwrap();
        assert_eq!(program.root.slot_bindings.len(), 1);
        assert_eq!(program.root.slot_bindings[0].slots.len(), 2);
        assert_eq!(
            program.root.plan.slots(),
            &[SlotKind::Text, SlotKind::Property]
        );
        assert_eq!(program.root.plan.as_plan().updates.len(), 2);
    }

    #[test]
    fn form_controls_lower_value_accessibility_and_typed_handlers() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
var value = ""
var checked = false
var quality = 50.0
var minimum = 0.0
var maximum = 100.0
var step = 5.0
func App() ui.View {
	return ui.Column(
		ui.SelectionLengthUTF16(ui.SelectionStartUTF16(ui.Invalid(ui.Required(ui.AccessibleDescription(
			ui.TextInput(value, "Name", func(event ui.Event) {}),
			"Name is required",
		), true), true), 1), 2),
		ui.Toggle(checked, "Enabled", func(event ui.Event) {}),
		ui.Slider(quality, minimum, maximum, step, "Quality", func(event ui.Event) {}),
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .unwrap();
        assert_eq!(program.root.plan.nodes().len(), 4);
        assert_eq!(program.root.handler_bindings.len(), 3);
        assert_eq!(program.root.slot_bindings.len(), 11);
        assert!(program
            .root
            .plan
            .as_plan()
            .updates
            .iter()
            .any(|update| matches!(
                update.mutation,
                vo_ui_plan::DirectMutation::SetProperty {
                    property: PropertyId::CHECKED,
                    ..
                }
            )));
        for property in [
            PropertyId::ACCESSIBLE_DESCRIPTION,
            PropertyId::REQUIRED,
            PropertyId::INVALID,
            PropertyId::SELECTION_START_UTF16,
            PropertyId::SELECTION_LENGTH_UTF16,
        ] {
            assert!(program.root.plan.as_plan().updates.iter().any(|update| {
                matches!(
                    update.mutation,
                    vo_ui_plan::DirectMutation::SetProperty {
                        target,
                        property: found,
                    } if target == LocalNodeId::new(1) && found == property
                )
            }));
        }
        let toggle = program.root.plan.node(LocalNodeId::new(2));
        assert!(toggle.children.is_empty());
        assert!(toggle
            .properties
            .iter()
            .any(|property| property.id == PropertyId::ACCESSIBLE_NAME));
        let slider = program.root.plan.node(LocalNodeId::new(3));
        assert_eq!(slider.kind, TemplateNodeKind::Element(Primitive::Slider));
        assert_eq!(slider.listeners[0].event, EventType::INPUT);
        for property in [
            PropertyId::VALUE,
            PropertyId::MIN_VALUE,
            PropertyId::MAX_VALUE,
            PropertyId::STEP_VALUE,
        ] {
            assert!(program.root.plan.as_plan().updates.iter().any(|update| {
                matches!(
                    update.mutation,
                    vo_ui_plan::DirectMutation::SetProperty {
                        target,
                        property: found,
                    } if target == LocalNodeId::new(3) && found == property
                )
            }));
        }
    }

    #[test]
    fn named_grid_modifiers_lower_to_typed_property_sites() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
var areas = "header header / sidebar main"
var area = "main"
func App() ui.View {
	return ui.GridTemplateAreas(ui.Grid(
		ui.GridArea(ui.Text("content"), area),
	), areas)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .unwrap();
        for property in [PropertyId::GRID_TEMPLATE_AREAS, PropertyId::GRID_AREA] {
            assert!(program.root.plan.as_plan().updates.iter().any(|update| {
                matches!(
                    update.mutation,
                    vo_ui_plan::DirectMutation::SetProperty {
                        property: found,
                        ..
                    } if found == property
                )
            }));
        }
    }

    #[test]
    fn modal_focus_and_pointer_modifiers_lower_to_typed_property_sites() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
var open = true
var preferred = true
var pointerMode = "auto"
func App() ui.View {
	return ui.Portal(
		ui.Modal(ui.PointerEvents(ui.FocusRequest(ui.AutoFocus(ui.Focusable(ui.AccessibilityHidden(
			ui.Hidden(ui.Current(ui.Pressed(ui.Expanded(ui.Selected(ui.Stack(), true), true), false), ui.CurrentPage), false), false), true),
			preferred,
		), 1), pointerMode), open),
		ui.OverlayModal,
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .unwrap();
        for property in [
            PropertyId::MODAL,
            PropertyId::AUTO_FOCUS,
            PropertyId::POINTER_EVENTS,
            PropertyId::PORTAL_LAYER,
            PropertyId::FOCUS_REQUEST,
            PropertyId::SELECTED,
            PropertyId::EXPANDED,
            PropertyId::PRESSED,
            PropertyId::CURRENT,
            PropertyId::HIDDEN,
            PropertyId::ACCESSIBILITY_HIDDEN,
            PropertyId::FOCUSABLE,
        ] {
            assert!(program.root.plan.as_plan().updates.iter().any(|update| {
                matches!(
                    update.mutation,
                    vo_ui_plan::DirectMutation::SetProperty {
                        property: found,
                        ..
                    } if found == property
                )
            }));
        }
    }

    #[test]
    fn typed_event_modifiers_lower_to_static_listener_sites() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
func App() ui.View {
	return ui.OnLayout(
		ui.OnSelectionChange(ui.OnCompositionEnd(
			ui.OnPointerMove(
				ui.OnKeyDownCapture(ui.Box(), func(event ui.Event) {}),
				func(event ui.Event) {},
			),
			func(event ui.Event) {},
		), func(event ui.Event) {}),
		func(event ui.Event) {},
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .unwrap();
        assert_eq!(program.root.handler_bindings.len(), 5);
        let listeners = &program.root.plan.nodes()[0].listeners;
        let events = listeners
            .iter()
            .map(|listener| listener.event)
            .collect::<Vec<_>>();
        assert_eq!(
            events,
            vec![
                EventType::KEY_DOWN,
                EventType::POINTER_MOVE,
                EventType::COMPOSITION_END,
                EventType::SELECTION_CHANGE,
                EventType::LAYOUT,
            ]
        );
        assert!(listeners[0].options.capture);
    }

    #[test]
    fn component_locals_form_stable_state_dependency_metadata() {
        let project = analyze(
            r#"
package main
import "github.com/vo-lang/ui"
func App() ui.View {
	label := "Count"
	count := len(label)
	return ui.Button(label, func(event ui.Event) { count++ })
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        );
        let program = compile_project_ui(&project, PlanLimits::default())
            .unwrap()
            .unwrap();

        assert_eq!(program.root.identity, "github.com/acme/app::App");
        assert_eq!(program.root.state_bindings.len(), 2);
        assert_eq!(program.root.state_bindings[0].key, "label");
        assert_eq!(
            program.root.state_bindings[0].dependent_slots,
            vec![SlotId::new(0), SlotId::new(1)]
        );
        assert_eq!(
            program.root.slot_bindings[0].dependencies,
            vec![StateId::new(0)]
        );
        assert_eq!(program.root.state_bindings[1].key, "count");
        assert_eq!(
            program.root.state_bindings[1].initializer_dependencies,
            vec![StateId::new(0)]
        );
        let bundle = build_component_bundle(&program).unwrap();
        let definition = bundle
            .definitions
            .iter()
            .find(|definition| definition.type_id == program.root.type_id)
            .unwrap();
        assert_eq!(
            definition.states[1].initializer_dependencies,
            vec![StateFieldId::new(0)]
        );
        assert_eq!(program.root.reload_schema.identity, program.root.identity);
        assert_eq!(program.root.reload_schema.state.len(), 2);
        assert_ne!(
            program.root.reload_schema.state[0].type_fingerprint,
            program.root.reload_schema.state[1].type_fingerprint
        );
        assert_eq!(
            program.root.handler_bindings[0].captured_state,
            vec![StateId::new(1)]
        );
        assert_eq!(
            program.root.state_bindings[1].captured_by_handlers,
            vec![HandlerId::new(0, 1)]
        );
    }
}
