use alloc::collections::BTreeSet;
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;
use vo_ui_core::HandlerId;
use vo_ui_plan::{
    decode_plan, encode_plan, LocalNodeId, PlanCodecError, PlanLimits, SlotId, TemplateNodeKind,
    ValidatedPlan,
};

pub const COMPONENT_BUNDLE_ARTIFACT_NAME: &str = "volang.ui.component-bundle";
pub const COMPONENT_BUNDLE_ARTIFACT_VERSION: u32 = 1;
pub const COMPONENT_BUNDLE_FORMAT_VERSION: u16 = 1;
pub const COMPONENT_BUNDLE_ABI_VERSION: u16 = 2;

const BUNDLE_MAGIC: &[u8; 4] = b"VUB1";

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ComponentTypeId {
    module: String,
    object: String,
}

impl ComponentTypeId {
    pub fn new(module: impl Into<String>, object: impl Into<String>) -> Self {
        Self {
            module: module.into(),
            object: object.into(),
        }
    }

    pub fn module(&self) -> &str {
        &self.module
    }

    pub fn object(&self) -> &str {
        &self.object
    }
}

impl fmt::Display for ComponentTypeId {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}::{}", self.module, self.object)
    }
}

macro_rules! local_id {
    ($name:ident, $inner:ty) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct $name($inner);

        impl $name {
            pub const fn new(value: $inner) -> Self {
                Self(value)
            }

            pub const fn value(self) -> $inner {
                self.0
            }
        }
    };
}

local_id!(ComponentCallSiteId, u64);
local_id!(StateFieldId, u32);
local_id!(BindingId, u32);
local_id!(HandlerSiteId, u32);
local_id!(EffectId, u32);
local_id!(TaskSiteId, u32);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ComponentCallMode {
    Static,
    Dynamic,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ComponentCallSite {
    pub id: ComponentCallSiteId,
    pub mode: ComponentCallMode,
    pub callee: Option<ComponentTypeId>,
    pub mount_parent: LocalNodeId,
    pub mount_before: Option<LocalNodeId>,
    pub props_bindings: Vec<BindingId>,
    pub key_binding: Option<BindingId>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ComponentInterface {
    pub props_arity: u16,
    pub props_type_fingerprint: u64,
    pub child_contract_fingerprint: u64,
    pub slot_contract_fingerprint: u64,
}

impl ComponentInterface {
    pub const fn empty() -> Self {
        Self {
            props_arity: 0,
            props_type_fingerprint: 0,
            child_contract_fingerprint: 0,
            slot_contract_fingerprint: 0,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StateValueKind {
    Opaque,
    String,
    Bool,
    Int,
    Float,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StateFieldDefinition {
    pub id: StateFieldId,
    pub key: String,
    pub type_fingerprint: u64,
    pub value_kind: StateValueKind,
    pub has_initializer: bool,
    pub initializer_func: Option<u32>,
    pub initializer_dependencies: Vec<StateFieldId>,
    pub initializer_props: Vec<u16>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BindingDefinition {
    pub id: BindingId,
    pub evaluator_func: Option<u32>,
    pub slots: Vec<SlotId>,
    pub dependencies: Vec<StateFieldId>,
    pub prop_dependencies: Vec<u16>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HandlerDefinition {
    pub id: HandlerSiteId,
    pub plan_handler: HandlerId,
    pub evaluator_func: Option<u32>,
    pub captured_state: Vec<StateFieldId>,
    pub captured_props: Vec<u16>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EffectDefinition {
    pub id: EffectId,
    pub start_func: u32,
    pub cleanup_func: Option<u32>,
    pub dependencies: Vec<StateFieldId>,
    pub prop_dependencies: Vec<u16>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TaskPolicy {
    RestartLatest,
    KeepFirst,
    Queue,
    ParallelBounded { max_in_flight: u16 },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TaskDefinition {
    pub id: TaskSiteId,
    pub start_func: u32,
    pub reducer_func: u32,
    pub policy: TaskPolicy,
    pub dependencies: Vec<StateFieldId>,
    pub prop_dependencies: Vec<u16>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LifecycleDefinition {
    pub mounted_func: Option<u32>,
    pub updated_func: Option<u32>,
    pub disposing_func: Option<u32>,
}

impl LifecycleDefinition {
    pub const fn empty() -> Self {
        Self {
            mounted_func: None,
            updated_func: None,
            disposing_func: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ComponentDefinition {
    pub type_id: ComponentTypeId,
    pub display_name: String,
    pub mode: super::ExecutionMode,
    pub interface: ComponentInterface,
    pub plan: ValidatedPlan,
    pub call_sites: Vec<ComponentCallSite>,
    pub states: Vec<StateFieldDefinition>,
    pub bindings: Vec<BindingDefinition>,
    pub handlers: Vec<HandlerDefinition>,
    pub effects: Vec<EffectDefinition>,
    pub tasks: Vec<TaskDefinition>,
    pub lifecycle: LifecycleDefinition,
    pub reload_schema_fingerprint: u64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BundleImportRequirement {
    pub module_identity: String,
    pub min_abi_version: u16,
    pub max_abi_version: u16,
    pub bundle_digest: [u8; 32],
    pub component_types: Vec<ComponentTypeId>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BundleSourceMetadata {
    pub source_digest: [u8; 32],
    pub compiler_identity: String,
    pub reload_schema_version: u32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ComponentBundle {
    pub abi_version: u16,
    pub module_identity: String,
    pub root: ComponentTypeId,
    /// Source-distributed modules linked into this application bundle.
    pub linked_modules: Vec<String>,
    pub definitions: Vec<ComponentDefinition>,
    pub imports: Vec<BundleImportRequirement>,
    pub capabilities: Vec<String>,
    pub source: BundleSourceMetadata,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct BundleLimits {
    pub max_bundle_bytes: usize,
    pub max_identity_bytes: usize,
    pub max_display_name_bytes: usize,
    pub max_state_key_bytes: usize,
    pub max_compiler_identity_bytes: usize,
    pub max_capability_bytes: usize,
    pub max_definitions: usize,
    pub max_linked_modules: usize,
    pub max_imports: usize,
    pub max_imported_types: usize,
    pub max_capabilities: usize,
    pub max_call_sites_per_definition: usize,
    pub max_states_per_definition: usize,
    pub max_bindings_per_definition: usize,
    pub max_handlers_per_definition: usize,
    pub max_effects_per_definition: usize,
    pub max_tasks_per_definition: usize,
    pub max_dependencies_per_entry: usize,
    pub max_static_nesting: usize,
}

impl Default for BundleLimits {
    fn default() -> Self {
        Self {
            max_bundle_bytes: 16 * 1024 * 1024,
            max_identity_bytes: 4 * 1024,
            max_display_name_bytes: 4 * 1024,
            max_state_key_bytes: 4 * 1024,
            max_compiler_identity_bytes: 4 * 1024,
            max_capability_bytes: 512,
            max_definitions: 16_384,
            max_linked_modules: 4_096,
            max_imports: 4_096,
            max_imported_types: 65_536,
            max_capabilities: 4_096,
            max_call_sites_per_definition: 65_536,
            max_states_per_definition: 65_536,
            max_bindings_per_definition: 200_000,
            max_handlers_per_definition: 200_000,
            max_effects_per_definition: 65_536,
            max_tasks_per_definition: 65_536,
            max_dependencies_per_entry: 65_536,
            max_static_nesting: 256,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BundleError {
    SizeLimitExceeded,
    LengthOverflow,
    AllocationFailed,
    Truncated,
    InvalidMagic,
    UnsupportedFormat(u16),
    UnsupportedAbi(u16),
    InvalidReservedBits,
    InvalidTag(u8),
    InvalidUtf8,
    TrailingBytes,
    InvalidModuleIdentity,
    InvalidObjectIdentity,
    InvalidDisplayName,
    InvalidCompilerIdentity,
    InvalidCapability,
    InvalidSourceDigest,
    InvalidImportDigest,
    InvalidImportAbi,
    TableLimitExceeded,
    DefinitionsNotCanonical,
    LinkedModulesNotCanonical,
    ImportsNotCanonical,
    CapabilitiesNotCanonical,
    ImportedTypesNotCanonical,
    DefinitionModuleMismatch(ComponentTypeId),
    LinkedImportCollision(String),
    UnusedLinkedModule(String),
    ImportedTypeModuleMismatch(ComponentTypeId),
    MissingRoot(ComponentTypeId),
    LocalImportCollision(ComponentTypeId),
    CallSitesNotCanonical(ComponentTypeId),
    StatesNotCanonical(ComponentTypeId),
    BindingsNotCanonical(ComponentTypeId),
    HandlersNotCanonical(ComponentTypeId),
    EffectsNotCanonical(ComponentTypeId),
    TasksNotCanonical(ComponentTypeId),
    DuplicateStateKey(ComponentTypeId),
    InvalidStateKey(ComponentTypeId),
    InvalidMountParent(ComponentCallSiteId),
    InvalidMountBefore(ComponentCallSiteId),
    InvalidStaticCallee(ComponentCallSiteId),
    InvalidDynamicCallee(ComponentCallSiteId),
    InvalidPropArity(ComponentCallSiteId),
    MissingComponent(ComponentTypeId),
    MissingState(StateFieldId),
    MissingProp(u16),
    MissingBinding(BindingId),
    InvalidSlot(SlotId),
    DuplicateSlot(SlotId),
    MissingSlot(SlotId),
    MissingDirectEvaluator,
    DuplicatePlanHandler(HandlerId),
    MissingPlanHandler(HandlerId),
    UnexpectedPlanHandler(HandlerId),
    InvalidTaskPolicy,
    StaticComponentCycle(ComponentTypeId),
    StaticNestingLimitExceeded,
    Plan(PlanCodecError),
}

impl fmt::Display for BundleError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "invalid UI component bundle: {self:?}")
    }
}

pub fn encode_component_bundle(
    bundle: &ComponentBundle,
    limits: BundleLimits,
    plan_limits: PlanLimits,
) -> Result<Vec<u8>, BundleError> {
    validate_component_bundle(bundle, limits)?;
    let mut writer = Writer::new(limits.max_bundle_bytes);
    writer.bytes(BUNDLE_MAGIC)?;
    writer.u16(COMPONENT_BUNDLE_FORMAT_VERSION)?;
    writer.u16(bundle.abi_version)?;
    writer.u16(0)?;
    writer.u16(0)?;
    writer.string(&bundle.module_identity)?;
    writer.component_type(&bundle.root)?;
    writer.count(bundle.linked_modules.len())?;
    for module in &bundle.linked_modules {
        writer.string(module)?;
    }
    writer.count(bundle.definitions.len())?;
    for definition in &bundle.definitions {
        writer.component_type(&definition.type_id)?;
        writer.string(&definition.display_name)?;
        writer.u8(match definition.mode {
            super::ExecutionMode::RootFallback => 1,
            super::ExecutionMode::Direct => 2,
        })?;
        writer.u16(definition.interface.props_arity)?;
        writer.u64(definition.interface.props_type_fingerprint)?;
        writer.u64(definition.interface.child_contract_fingerprint)?;
        writer.u64(definition.interface.slot_contract_fingerprint)?;
        let plan = encode_plan(&definition.plan, plan_limits).map_err(BundleError::Plan)?;
        writer.byte_string(&plan)?;
        writer.count(definition.call_sites.len())?;
        for call_site in &definition.call_sites {
            writer.u64(call_site.id.value())?;
            writer.u8(match call_site.mode {
                ComponentCallMode::Static => 1,
                ComponentCallMode::Dynamic => 2,
            })?;
            writer.option_component_type(call_site.callee.as_ref())?;
            writer.u32(call_site.mount_parent.index())?;
            writer.option_u32(call_site.mount_before.map(LocalNodeId::index))?;
            writer.count(call_site.props_bindings.len())?;
            for binding in &call_site.props_bindings {
                writer.u32(binding.value())?;
            }
            writer.option_u32(call_site.key_binding.map(BindingId::value))?;
        }
        writer.count(definition.states.len())?;
        for state in &definition.states {
            writer.u32(state.id.value())?;
            writer.string(&state.key)?;
            writer.u64(state.type_fingerprint)?;
            writer.u8(match state.value_kind {
                StateValueKind::Opaque => 0,
                StateValueKind::String => 1,
                StateValueKind::Bool => 2,
                StateValueKind::Int => 3,
                StateValueKind::Float => 4,
            })?;
            writer.u8(u8::from(state.has_initializer))?;
            writer.option_u32(state.initializer_func)?;
            writer.state_ids(&state.initializer_dependencies)?;
            writer.u16s(&state.initializer_props)?;
        }
        writer.count(definition.bindings.len())?;
        for binding in &definition.bindings {
            writer.u32(binding.id.value())?;
            writer.option_u32(binding.evaluator_func)?;
            writer.count(binding.slots.len())?;
            for slot in &binding.slots {
                writer.u32(slot.index())?;
            }
            writer.state_ids(&binding.dependencies)?;
            writer.u16s(&binding.prop_dependencies)?;
        }
        writer.count(definition.handlers.len())?;
        for handler in &definition.handlers {
            writer.u32(handler.id.value())?;
            writer.u32(handler.plan_handler.index())?;
            writer.u32(handler.plan_handler.generation())?;
            writer.option_u32(handler.evaluator_func)?;
            writer.state_ids(&handler.captured_state)?;
            writer.u16s(&handler.captured_props)?;
        }
        writer.count(definition.effects.len())?;
        for effect in &definition.effects {
            writer.u32(effect.id.value())?;
            writer.u32(effect.start_func)?;
            writer.option_u32(effect.cleanup_func)?;
            writer.state_ids(&effect.dependencies)?;
            writer.u16s(&effect.prop_dependencies)?;
        }
        writer.count(definition.tasks.len())?;
        for task in &definition.tasks {
            writer.u32(task.id.value())?;
            writer.u32(task.start_func)?;
            writer.u32(task.reducer_func)?;
            match task.policy {
                TaskPolicy::RestartLatest => writer.u8(1)?,
                TaskPolicy::KeepFirst => writer.u8(2)?,
                TaskPolicy::Queue => writer.u8(3)?,
                TaskPolicy::ParallelBounded { max_in_flight } => {
                    writer.u8(4)?;
                    writer.u16(max_in_flight)?;
                }
            }
            writer.state_ids(&task.dependencies)?;
            writer.u16s(&task.prop_dependencies)?;
        }
        writer.option_u32(definition.lifecycle.mounted_func)?;
        writer.option_u32(definition.lifecycle.updated_func)?;
        writer.option_u32(definition.lifecycle.disposing_func)?;
        writer.u64(definition.reload_schema_fingerprint)?;
    }
    writer.count(bundle.imports.len())?;
    for import in &bundle.imports {
        writer.string(&import.module_identity)?;
        writer.u16(import.min_abi_version)?;
        writer.u16(import.max_abi_version)?;
        writer.bytes(&import.bundle_digest)?;
        writer.count(import.component_types.len())?;
        for type_id in &import.component_types {
            writer.component_type(type_id)?;
        }
    }
    writer.count(bundle.capabilities.len())?;
    for capability in &bundle.capabilities {
        writer.string(capability)?;
    }
    writer.bytes(&bundle.source.source_digest)?;
    writer.string(&bundle.source.compiler_identity)?;
    writer.u32(bundle.source.reload_schema_version)?;
    writer.finish()
}

pub fn decode_component_bundle(
    bytes: &[u8],
    limits: BundleLimits,
    plan_limits: PlanLimits,
) -> Result<ComponentBundle, BundleError> {
    if bytes.len() > limits.max_bundle_bytes {
        return Err(BundleError::SizeLimitExceeded);
    }
    let mut reader = Reader::new(bytes);
    if reader.take(BUNDLE_MAGIC.len())? != BUNDLE_MAGIC {
        return Err(BundleError::InvalidMagic);
    }
    let format = reader.u16()?;
    if format != COMPONENT_BUNDLE_FORMAT_VERSION {
        return Err(BundleError::UnsupportedFormat(format));
    }
    let abi_version = reader.u16()?;
    if abi_version != COMPONENT_BUNDLE_ABI_VERSION {
        return Err(BundleError::UnsupportedAbi(abi_version));
    }
    if reader.u16()? != 0 || reader.u16()? != 0 {
        return Err(BundleError::InvalidReservedBits);
    }
    let module_identity = reader.string(limits.max_identity_bytes)?;
    let root = reader.component_type(limits.max_identity_bytes)?;
    let linked_module_count = reader.count(limits.max_linked_modules)?;
    let mut linked_modules = reserved_vec(linked_module_count)?;
    for _ in 0..linked_module_count {
        linked_modules.push(reader.string(limits.max_identity_bytes)?);
    }
    let definition_count = reader.count(limits.max_definitions)?;
    let mut definitions = reserved_vec(definition_count)?;
    for _ in 0..definition_count {
        let type_id = reader.component_type(limits.max_identity_bytes)?;
        let display_name = reader.string(limits.max_display_name_bytes)?;
        let mode = match reader.u8()? {
            1 => super::ExecutionMode::RootFallback,
            2 => super::ExecutionMode::Direct,
            tag => return Err(BundleError::InvalidTag(tag)),
        };
        let interface = ComponentInterface {
            props_arity: reader.u16()?,
            props_type_fingerprint: reader.u64()?,
            child_contract_fingerprint: reader.u64()?,
            slot_contract_fingerprint: reader.u64()?,
        };
        let plan = decode_plan(reader.byte_string(plan_limits.max_plan_bytes)?, plan_limits)
            .map_err(BundleError::Plan)?;
        let call_site_count = reader.count(limits.max_call_sites_per_definition)?;
        let mut call_sites = reserved_vec(call_site_count)?;
        for _ in 0..call_site_count {
            let id = ComponentCallSiteId::new(reader.u64()?);
            let call_mode = match reader.u8()? {
                1 => ComponentCallMode::Static,
                2 => ComponentCallMode::Dynamic,
                tag => return Err(BundleError::InvalidTag(tag)),
            };
            let callee = reader.option_component_type(limits.max_identity_bytes)?;
            let mount_parent = LocalNodeId::new(reader.u32()?);
            let mount_before = reader.option_u32()?.map(LocalNodeId::new);
            let props_count = reader.count(limits.max_bindings_per_definition)?;
            let mut props_bindings = reserved_vec(props_count)?;
            for _ in 0..props_count {
                props_bindings.push(BindingId::new(reader.u32()?));
            }
            call_sites.push(ComponentCallSite {
                id,
                mode: call_mode,
                callee,
                mount_parent,
                mount_before,
                props_bindings,
                key_binding: reader.option_u32()?.map(BindingId::new),
            });
        }
        let state_count = reader.count(limits.max_states_per_definition)?;
        let mut states = reserved_vec(state_count)?;
        for _ in 0..state_count {
            states.push(StateFieldDefinition {
                id: StateFieldId::new(reader.u32()?),
                key: reader.string(limits.max_state_key_bytes)?,
                type_fingerprint: reader.u64()?,
                value_kind: match reader.u8()? {
                    0 => StateValueKind::Opaque,
                    1 => StateValueKind::String,
                    2 => StateValueKind::Bool,
                    3 => StateValueKind::Int,
                    4 => StateValueKind::Float,
                    tag => return Err(BundleError::InvalidTag(tag)),
                },
                has_initializer: reader.bool()?,
                initializer_func: reader.option_u32()?,
                initializer_dependencies: reader.state_ids(limits.max_dependencies_per_entry)?,
                initializer_props: reader.u16s(limits.max_dependencies_per_entry)?,
            });
        }
        let binding_count = reader.count(limits.max_bindings_per_definition)?;
        let mut bindings = reserved_vec(binding_count)?;
        for _ in 0..binding_count {
            let id = BindingId::new(reader.u32()?);
            let evaluator_func = reader.option_u32()?;
            let slot_count = reader.count(limits.max_bindings_per_definition)?;
            let mut slots = reserved_vec(slot_count)?;
            for _ in 0..slot_count {
                slots.push(SlotId::new(reader.u32()?));
            }
            bindings.push(BindingDefinition {
                id,
                evaluator_func,
                slots,
                dependencies: reader.state_ids(limits.max_dependencies_per_entry)?,
                prop_dependencies: reader.u16s(limits.max_dependencies_per_entry)?,
            });
        }
        let handler_count = reader.count(limits.max_handlers_per_definition)?;
        let mut handlers = reserved_vec(handler_count)?;
        for _ in 0..handler_count {
            handlers.push(HandlerDefinition {
                id: HandlerSiteId::new(reader.u32()?),
                plan_handler: HandlerId::new(reader.u32()?, reader.u32()?),
                evaluator_func: reader.option_u32()?,
                captured_state: reader.state_ids(limits.max_dependencies_per_entry)?,
                captured_props: reader.u16s(limits.max_dependencies_per_entry)?,
            });
        }
        let effect_count = reader.count(limits.max_effects_per_definition)?;
        let mut effects = reserved_vec(effect_count)?;
        for _ in 0..effect_count {
            effects.push(EffectDefinition {
                id: EffectId::new(reader.u32()?),
                start_func: reader.u32()?,
                cleanup_func: reader.option_u32()?,
                dependencies: reader.state_ids(limits.max_dependencies_per_entry)?,
                prop_dependencies: reader.u16s(limits.max_dependencies_per_entry)?,
            });
        }
        let task_count = reader.count(limits.max_tasks_per_definition)?;
        let mut tasks = reserved_vec(task_count)?;
        for _ in 0..task_count {
            let id = TaskSiteId::new(reader.u32()?);
            let start_func = reader.u32()?;
            let reducer_func = reader.u32()?;
            let policy = match reader.u8()? {
                1 => TaskPolicy::RestartLatest,
                2 => TaskPolicy::KeepFirst,
                3 => TaskPolicy::Queue,
                4 => TaskPolicy::ParallelBounded {
                    max_in_flight: reader.u16()?,
                },
                tag => return Err(BundleError::InvalidTag(tag)),
            };
            tasks.push(TaskDefinition {
                id,
                start_func,
                reducer_func,
                policy,
                dependencies: reader.state_ids(limits.max_dependencies_per_entry)?,
                prop_dependencies: reader.u16s(limits.max_dependencies_per_entry)?,
            });
        }
        definitions.push(ComponentDefinition {
            type_id,
            display_name,
            mode,
            interface,
            plan,
            call_sites,
            states,
            bindings,
            handlers,
            effects,
            tasks,
            lifecycle: LifecycleDefinition {
                mounted_func: reader.option_u32()?,
                updated_func: reader.option_u32()?,
                disposing_func: reader.option_u32()?,
            },
            reload_schema_fingerprint: reader.u64()?,
        });
    }
    let import_count = reader.count(limits.max_imports)?;
    let mut imports = reserved_vec(import_count)?;
    let mut imported_type_total = 0_usize;
    for _ in 0..import_count {
        let module_identity = reader.string(limits.max_identity_bytes)?;
        let min_abi_version = reader.u16()?;
        let max_abi_version = reader.u16()?;
        let bundle_digest = reader.array_32()?;
        let type_count = reader.count(limits.max_imported_types)?;
        imported_type_total = imported_type_total
            .checked_add(type_count)
            .filter(|count| *count <= limits.max_imported_types)
            .ok_or(BundleError::TableLimitExceeded)?;
        let mut component_types = reserved_vec(type_count)?;
        for _ in 0..type_count {
            component_types.push(reader.component_type(limits.max_identity_bytes)?);
        }
        imports.push(BundleImportRequirement {
            module_identity,
            min_abi_version,
            max_abi_version,
            bundle_digest,
            component_types,
        });
    }
    let capability_count = reader.count(limits.max_capabilities)?;
    let mut capabilities = reserved_vec(capability_count)?;
    for _ in 0..capability_count {
        capabilities.push(reader.string(limits.max_capability_bytes)?);
    }
    let source = BundleSourceMetadata {
        source_digest: reader.array_32()?,
        compiler_identity: reader.string(limits.max_compiler_identity_bytes)?,
        reload_schema_version: reader.u32()?,
    };
    if !reader.is_empty() {
        return Err(BundleError::TrailingBytes);
    }
    let bundle = ComponentBundle {
        abi_version,
        module_identity,
        root,
        linked_modules,
        definitions,
        imports,
        capabilities,
        source,
    };
    validate_component_bundle(&bundle, limits)?;
    Ok(bundle)
}

pub fn validate_component_bundle(
    bundle: &ComponentBundle,
    limits: BundleLimits,
) -> Result<(), BundleError> {
    if bundle.abi_version != COMPONENT_BUNDLE_ABI_VERSION {
        return Err(BundleError::UnsupportedAbi(bundle.abi_version));
    }
    validate_identity(&bundle.module_identity, limits.max_identity_bytes, true)?;
    validate_component_type(&bundle.root, limits)?;
    if bundle.linked_modules.len() > limits.max_linked_modules {
        return Err(BundleError::TableLimitExceeded);
    }
    require_strict_order(
        bundle.linked_modules.iter().map(String::as_str),
        BundleError::LinkedModulesNotCanonical,
    )?;
    for module in &bundle.linked_modules {
        validate_identity(module, limits.max_identity_bytes, true)?;
        if module == &bundle.module_identity {
            return Err(BundleError::LinkedModulesNotCanonical);
        }
    }
    if bundle.definitions.is_empty() || bundle.definitions.len() > limits.max_definitions {
        return Err(BundleError::TableLimitExceeded);
    }
    require_strict_order(
        bundle
            .definitions
            .iter()
            .map(|definition| &definition.type_id),
        BundleError::DefinitionsNotCanonical,
    )?;
    if bundle
        .definitions
        .binary_search_by(|definition| definition.type_id.cmp(&bundle.root))
        .is_err()
    {
        return Err(BundleError::MissingRoot(bundle.root.clone()));
    }

    let mut imported_types = BTreeSet::new();
    if bundle.imports.len() > limits.max_imports {
        return Err(BundleError::TableLimitExceeded);
    }
    require_strict_order(
        bundle
            .imports
            .iter()
            .map(|requirement| requirement.module_identity.as_str()),
        BundleError::ImportsNotCanonical,
    )?;
    let mut imported_type_total = 0_usize;
    for import in &bundle.imports {
        validate_identity(&import.module_identity, limits.max_identity_bytes, true)?;
        if bundle
            .linked_modules
            .binary_search(&import.module_identity)
            .is_ok()
        {
            return Err(BundleError::LinkedImportCollision(
                import.module_identity.clone(),
            ));
        }
        if import.min_abi_version == 0 || import.min_abi_version > import.max_abi_version {
            return Err(BundleError::InvalidImportAbi);
        }
        if import.bundle_digest.iter().all(|byte| *byte == 0) {
            return Err(BundleError::InvalidImportDigest);
        }
        imported_type_total = imported_type_total
            .checked_add(import.component_types.len())
            .filter(|count| *count <= limits.max_imported_types)
            .ok_or(BundleError::TableLimitExceeded)?;
        require_strict_order(
            import.component_types.iter(),
            BundleError::ImportedTypesNotCanonical,
        )?;
        for type_id in &import.component_types {
            validate_component_type(type_id, limits)?;
            if type_id.module() != import.module_identity {
                return Err(BundleError::ImportedTypeModuleMismatch(type_id.clone()));
            }
            if !imported_types.insert(type_id.clone()) {
                return Err(BundleError::ImportedTypesNotCanonical);
            }
        }
    }

    let mut used_linked_modules = BTreeSet::new();
    for definition in &bundle.definitions {
        validate_component_type(&definition.type_id, limits)?;
        if definition.type_id.module() != bundle.module_identity
            && bundle
                .linked_modules
                .binary_search_by(|module| module.as_str().cmp(definition.type_id.module()))
                .is_err()
        {
            return Err(BundleError::DefinitionModuleMismatch(
                definition.type_id.clone(),
            ));
        }
        if definition.type_id.module() != bundle.module_identity {
            used_linked_modules.insert(definition.type_id.module());
        }
        if imported_types.contains(&definition.type_id) {
            return Err(BundleError::LocalImportCollision(
                definition.type_id.clone(),
            ));
        }
        validate_definition(bundle, definition, &imported_types, limits)?;
    }
    for module in &bundle.linked_modules {
        if !used_linked_modules.contains(module.as_str()) {
            return Err(BundleError::UnusedLinkedModule(module.clone()));
        }
    }

    if bundle.capabilities.len() > limits.max_capabilities {
        return Err(BundleError::TableLimitExceeded);
    }
    require_strict_order(
        bundle.capabilities.iter().map(String::as_str),
        BundleError::CapabilitiesNotCanonical,
    )?;
    for capability in &bundle.capabilities {
        if capability.is_empty()
            || capability.len() > limits.max_capability_bytes
            || !capability.bytes().all(|byte| {
                byte.is_ascii_lowercase()
                    || byte.is_ascii_digit()
                    || matches!(byte, b'.' | b'-' | b'/' | b':')
            })
        {
            return Err(BundleError::InvalidCapability);
        }
    }
    if bundle.source.source_digest.iter().all(|byte| *byte == 0) {
        return Err(BundleError::InvalidSourceDigest);
    }
    if validate_identity(
        &bundle.source.compiler_identity,
        limits.max_compiler_identity_bytes,
        false,
    )
    .is_err()
    {
        return Err(BundleError::InvalidCompilerIdentity);
    }
    validate_static_graph(bundle, limits)
}

fn validate_definition(
    bundle: &ComponentBundle,
    definition: &ComponentDefinition,
    imported_types: &BTreeSet<ComponentTypeId>,
    limits: BundleLimits,
) -> Result<(), BundleError> {
    if definition.display_name.is_empty()
        || definition.display_name.len() > limits.max_display_name_bytes
        || definition.display_name.chars().any(char::is_control)
    {
        return Err(BundleError::InvalidDisplayName);
    }
    require_order_by_key(
        &definition.call_sites,
        |call_site| call_site.id,
        BundleError::CallSitesNotCanonical(definition.type_id.clone()),
    )?;
    require_order_by_key(
        &definition.states,
        |state| state.id,
        BundleError::StatesNotCanonical(definition.type_id.clone()),
    )?;
    require_order_by_key(
        &definition.bindings,
        |binding| binding.id,
        BundleError::BindingsNotCanonical(definition.type_id.clone()),
    )?;
    require_order_by_key(
        &definition.handlers,
        |handler| handler.id,
        BundleError::HandlersNotCanonical(definition.type_id.clone()),
    )?;
    require_order_by_key(
        &definition.effects,
        |effect| effect.id,
        BundleError::EffectsNotCanonical(definition.type_id.clone()),
    )?;
    require_order_by_key(
        &definition.tasks,
        |task| task.id,
        BundleError::TasksNotCanonical(definition.type_id.clone()),
    )?;
    if definition.call_sites.len() > limits.max_call_sites_per_definition
        || definition.states.len() > limits.max_states_per_definition
        || definition.bindings.len() > limits.max_bindings_per_definition
        || definition.handlers.len() > limits.max_handlers_per_definition
        || definition.effects.len() > limits.max_effects_per_definition
        || definition.tasks.len() > limits.max_tasks_per_definition
    {
        return Err(BundleError::TableLimitExceeded);
    }

    let mut state_keys = BTreeSet::new();
    for state in &definition.states {
        if state.key.is_empty()
            || state.key.len() > limits.max_state_key_bytes
            || state.key.chars().any(char::is_control)
        {
            return Err(BundleError::InvalidStateKey(definition.type_id.clone()));
        }
        if !state_keys.insert(state.key.as_str()) {
            return Err(BundleError::DuplicateStateKey(definition.type_id.clone()));
        }
        if definition.mode == super::ExecutionMode::Direct
            && state.has_initializer
            && state.initializer_func.is_none()
        {
            return Err(BundleError::MissingDirectEvaluator);
        }
        validate_state_dependencies(definition, &state.initializer_dependencies, limits)?;
        if state
            .initializer_dependencies
            .iter()
            .any(|dependency| dependency.value() >= state.id.value())
        {
            return Err(BundleError::MissingState(state.id));
        }
        validate_prop_dependencies(
            definition.interface.props_arity,
            &state.initializer_props,
            limits,
        )?;
    }

    let mut covered_slots = BTreeSet::new();
    for binding in &definition.bindings {
        if definition.mode == super::ExecutionMode::Direct && binding.evaluator_func.is_none() {
            return Err(BundleError::MissingDirectEvaluator);
        }
        validate_state_dependencies(definition, &binding.dependencies, limits)?;
        validate_prop_dependencies(
            definition.interface.props_arity,
            &binding.prop_dependencies,
            limits,
        )?;
        for slot in &binding.slots {
            if definition.plan.slot_kind(*slot).is_none() {
                return Err(BundleError::InvalidSlot(*slot));
            }
            if !covered_slots.insert(*slot) {
                return Err(BundleError::DuplicateSlot(*slot));
            }
        }
    }
    for index in 0..definition.plan.slots().len() {
        let slot = SlotId::new(index as u32);
        if !covered_slots.contains(&slot) {
            return Err(BundleError::MissingSlot(slot));
        }
    }

    let mut plan_handlers = BTreeSet::new();
    for node in definition.plan.nodes() {
        for listener in &node.listeners {
            plan_handlers.insert(listener.handler);
        }
    }
    let mut defined_plan_handlers = BTreeSet::new();
    for handler in &definition.handlers {
        if definition.mode == super::ExecutionMode::Direct && handler.evaluator_func.is_none() {
            return Err(BundleError::MissingDirectEvaluator);
        }
        validate_state_dependencies(definition, &handler.captured_state, limits)?;
        validate_prop_dependencies(
            definition.interface.props_arity,
            &handler.captured_props,
            limits,
        )?;
        if !plan_handlers.contains(&handler.plan_handler) {
            return Err(BundleError::UnexpectedPlanHandler(handler.plan_handler));
        }
        if !defined_plan_handlers.insert(handler.plan_handler) {
            return Err(BundleError::DuplicatePlanHandler(handler.plan_handler));
        }
    }
    for handler in plan_handlers {
        if !defined_plan_handlers.contains(&handler) {
            return Err(BundleError::MissingPlanHandler(handler));
        }
    }

    for effect in &definition.effects {
        validate_state_dependencies(definition, &effect.dependencies, limits)?;
        validate_prop_dependencies(
            definition.interface.props_arity,
            &effect.prop_dependencies,
            limits,
        )?;
    }
    for task in &definition.tasks {
        validate_state_dependencies(definition, &task.dependencies, limits)?;
        validate_prop_dependencies(
            definition.interface.props_arity,
            &task.prop_dependencies,
            limits,
        )?;
        if matches!(
            task.policy,
            TaskPolicy::ParallelBounded { max_in_flight: 0 }
        ) {
            return Err(BundleError::InvalidTaskPolicy);
        }
    }

    for call_site in &definition.call_sites {
        let parent = definition
            .plan
            .nodes()
            .get(call_site.mount_parent.index() as usize)
            .ok_or(BundleError::InvalidMountParent(call_site.id))?;
        if !matches!(parent.kind, TemplateNodeKind::Element(_)) {
            return Err(BundleError::InvalidMountParent(call_site.id));
        }
        if let Some(before) = call_site.mount_before {
            if !parent.children.contains(&before) {
                return Err(BundleError::InvalidMountBefore(call_site.id));
            }
        }
        if call_site
            .props_bindings
            .windows(2)
            .any(|pair| pair[0] >= pair[1])
        {
            return Err(BundleError::BindingsNotCanonical(
                definition.type_id.clone(),
            ));
        }
        for binding in &call_site.props_bindings {
            require_binding(definition, *binding)?;
        }
        if let Some(binding) = call_site.key_binding {
            require_binding(definition, binding)?;
        }
        match (&call_site.mode, &call_site.callee) {
            (ComponentCallMode::Static, Some(callee)) => {
                let local = bundle
                    .definitions
                    .binary_search_by(|candidate| candidate.type_id.cmp(callee));
                if local.is_err() && !imported_types.contains(callee) {
                    return Err(BundleError::MissingComponent(callee.clone()));
                }
                if let Ok(index) = local {
                    let expected = usize::from(bundle.definitions[index].interface.props_arity);
                    if call_site.props_bindings.len() != expected {
                        return Err(BundleError::InvalidPropArity(call_site.id));
                    }
                }
            }
            (ComponentCallMode::Static, None) => {
                return Err(BundleError::InvalidStaticCallee(call_site.id));
            }
            (ComponentCallMode::Dynamic, None) => {}
            (ComponentCallMode::Dynamic, Some(_)) => {
                return Err(BundleError::InvalidDynamicCallee(call_site.id));
            }
        }
    }
    Ok(())
}

fn validate_state_dependencies(
    definition: &ComponentDefinition,
    dependencies: &[StateFieldId],
    limits: BundleLimits,
) -> Result<(), BundleError> {
    if dependencies.len() > limits.max_dependencies_per_entry
        || dependencies.windows(2).any(|pair| pair[0] >= pair[1])
    {
        return Err(BundleError::TableLimitExceeded);
    }
    for dependency in dependencies {
        if definition
            .states
            .binary_search_by_key(dependency, |state| state.id)
            .is_err()
        {
            return Err(BundleError::MissingState(*dependency));
        }
    }
    Ok(())
}

fn require_binding(
    definition: &ComponentDefinition,
    binding: BindingId,
) -> Result<(), BundleError> {
    if definition
        .bindings
        .binary_search_by_key(&binding, |candidate| candidate.id)
        .is_err()
    {
        Err(BundleError::MissingBinding(binding))
    } else {
        Ok(())
    }
}

fn validate_prop_dependencies(
    props_arity: u16,
    dependencies: &[u16],
    limits: BundleLimits,
) -> Result<(), BundleError> {
    if dependencies.len() > limits.max_dependencies_per_entry
        || dependencies.windows(2).any(|pair| pair[0] >= pair[1])
    {
        return Err(BundleError::TableLimitExceeded);
    }
    for dependency in dependencies {
        if *dependency >= props_arity {
            return Err(BundleError::MissingProp(*dependency));
        }
    }
    Ok(())
}

fn validate_static_graph(
    bundle: &ComponentBundle,
    limits: BundleLimits,
) -> Result<(), BundleError> {
    #[derive(Clone, Copy)]
    struct Frame {
        definition: usize,
        next_call: usize,
    }

    let mut state = alloc::vec![0_u8; bundle.definitions.len()];
    for start in 0..bundle.definitions.len() {
        if state[start] != 0 {
            continue;
        }
        let mut stack = Vec::new();
        stack
            .try_reserve_exact(limits.max_static_nesting.min(bundle.definitions.len()))
            .map_err(|_| BundleError::AllocationFailed)?;
        state[start] = 1;
        stack.push(Frame {
            definition: start,
            next_call: 0,
        });
        while !stack.is_empty() {
            if stack.len() > limits.max_static_nesting {
                return Err(BundleError::StaticNestingLimitExceeded);
            }
            let frame = stack
                .last_mut()
                .expect("a non-empty component traversal stack has a frame");
            let definition = &bundle.definitions[frame.definition];
            let Some(call_site) = definition.call_sites.get(frame.next_call) else {
                state[frame.definition] = 2;
                stack.pop();
                continue;
            };
            frame.next_call += 1;
            if call_site.mode != ComponentCallMode::Static {
                continue;
            }
            let Some(callee) = call_site.callee.as_ref() else {
                continue;
            };
            let Ok(callee_index) = bundle
                .definitions
                .binary_search_by(|candidate| candidate.type_id.cmp(callee))
            else {
                continue;
            };
            match state[callee_index] {
                0 => {
                    state[callee_index] = 1;
                    stack.push(Frame {
                        definition: callee_index,
                        next_call: 0,
                    });
                }
                1 => return Err(BundleError::StaticComponentCycle(callee.clone())),
                _ => {}
            }
        }
    }
    Ok(())
}

fn validate_component_type(
    type_id: &ComponentTypeId,
    limits: BundleLimits,
) -> Result<(), BundleError> {
    validate_identity(type_id.module(), limits.max_identity_bytes, true)?;
    validate_identity(type_id.object(), limits.max_identity_bytes, false)
        .map_err(|_| BundleError::InvalidObjectIdentity)
}

fn validate_identity(value: &str, max: usize, module: bool) -> Result<(), BundleError> {
    if value.is_empty()
        || value.len() > max
        || value.trim() != value
        || value.chars().any(char::is_control)
        || value.contains('\\')
    {
        if module {
            Err(BundleError::InvalidModuleIdentity)
        } else {
            Err(BundleError::InvalidObjectIdentity)
        }
    } else {
        Ok(())
    }
}

fn require_strict_order<'a, T: Ord + ?Sized + 'a>(
    values: impl Iterator<Item = &'a T>,
    error: BundleError,
) -> Result<(), BundleError> {
    let mut previous = None;
    for value in values {
        if previous.is_some_and(|previous| previous >= value) {
            return Err(error);
        }
        previous = Some(value);
    }
    Ok(())
}

fn require_order_by_key<T, K: Copy + Ord>(
    values: &[T],
    key: impl Fn(&T) -> K,
    error: BundleError,
) -> Result<(), BundleError> {
    if values.windows(2).any(|pair| key(&pair[0]) >= key(&pair[1])) {
        Err(error)
    } else {
        Ok(())
    }
}

struct Writer {
    bytes: Vec<u8>,
    limit: usize,
}

impl Writer {
    fn new(limit: usize) -> Self {
        Self {
            bytes: Vec::new(),
            limit,
        }
    }

    fn reserve(&mut self, additional: usize) -> Result<(), BundleError> {
        if self
            .bytes
            .len()
            .checked_add(additional)
            .is_none_or(|length| length > self.limit)
        {
            return Err(BundleError::SizeLimitExceeded);
        }
        self.bytes
            .try_reserve(additional)
            .map_err(|_| BundleError::AllocationFailed)
    }

    fn finish(self) -> Result<Vec<u8>, BundleError> {
        Ok(self.bytes)
    }

    fn bytes(&mut self, value: &[u8]) -> Result<(), BundleError> {
        self.reserve(value.len())?;
        self.bytes.extend_from_slice(value);
        Ok(())
    }

    fn u8(&mut self, value: u8) -> Result<(), BundleError> {
        self.bytes(&[value])
    }

    fn u16(&mut self, value: u16) -> Result<(), BundleError> {
        self.bytes(&value.to_le_bytes())
    }

    fn u32(&mut self, value: u32) -> Result<(), BundleError> {
        self.bytes(&value.to_le_bytes())
    }

    fn u64(&mut self, value: u64) -> Result<(), BundleError> {
        self.bytes(&value.to_le_bytes())
    }

    fn count(&mut self, value: usize) -> Result<(), BundleError> {
        self.u32(u32::try_from(value).map_err(|_| BundleError::LengthOverflow)?)
    }

    fn byte_string(&mut self, value: &[u8]) -> Result<(), BundleError> {
        self.count(value.len())?;
        self.bytes(value)
    }

    fn string(&mut self, value: &str) -> Result<(), BundleError> {
        self.byte_string(value.as_bytes())
    }

    fn option_u32(&mut self, value: Option<u32>) -> Result<(), BundleError> {
        match value {
            Some(value) => {
                self.u8(1)?;
                self.u32(value)
            }
            None => self.u8(0),
        }
    }

    fn component_type(&mut self, type_id: &ComponentTypeId) -> Result<(), BundleError> {
        self.string(type_id.module())?;
        self.string(type_id.object())
    }

    fn option_component_type(
        &mut self,
        type_id: Option<&ComponentTypeId>,
    ) -> Result<(), BundleError> {
        match type_id {
            Some(type_id) => {
                self.u8(1)?;
                self.component_type(type_id)
            }
            None => self.u8(0),
        }
    }

    fn state_ids(&mut self, values: &[StateFieldId]) -> Result<(), BundleError> {
        self.count(values.len())?;
        for value in values {
            self.u32(value.value())?;
        }
        Ok(())
    }

    fn u16s(&mut self, values: &[u16]) -> Result<(), BundleError> {
        self.count(values.len())?;
        for value in values {
            self.u16(*value)?;
        }
        Ok(())
    }
}

struct Reader<'a> {
    bytes: &'a [u8],
    cursor: usize,
}

impl<'a> Reader<'a> {
    const fn new(bytes: &'a [u8]) -> Self {
        Self { bytes, cursor: 0 }
    }

    fn is_empty(&self) -> bool {
        self.cursor == self.bytes.len()
    }

    fn take(&mut self, length: usize) -> Result<&'a [u8], BundleError> {
        let end = self
            .cursor
            .checked_add(length)
            .filter(|end| *end <= self.bytes.len())
            .ok_or(BundleError::Truncated)?;
        let value = &self.bytes[self.cursor..end];
        self.cursor = end;
        Ok(value)
    }

    fn u8(&mut self) -> Result<u8, BundleError> {
        Ok(self.take(1)?[0])
    }

    fn u16(&mut self) -> Result<u16, BundleError> {
        Ok(u16::from_le_bytes(
            self.take(2)?
                .try_into()
                .map_err(|_| BundleError::Truncated)?,
        ))
    }

    fn bool(&mut self) -> Result<bool, BundleError> {
        match self.u8()? {
            0 => Ok(false),
            1 => Ok(true),
            tag => Err(BundleError::InvalidTag(tag)),
        }
    }

    fn u32(&mut self) -> Result<u32, BundleError> {
        Ok(u32::from_le_bytes(
            self.take(4)?
                .try_into()
                .map_err(|_| BundleError::Truncated)?,
        ))
    }

    fn u64(&mut self) -> Result<u64, BundleError> {
        Ok(u64::from_le_bytes(
            self.take(8)?
                .try_into()
                .map_err(|_| BundleError::Truncated)?,
        ))
    }

    fn count(&mut self, max: usize) -> Result<usize, BundleError> {
        let count = self.u32()? as usize;
        if count > max || count > self.bytes.len().saturating_sub(self.cursor) {
            Err(BundleError::TableLimitExceeded)
        } else {
            Ok(count)
        }
    }

    fn byte_string(&mut self, max: usize) -> Result<&'a [u8], BundleError> {
        let length = self.u32()? as usize;
        if length > max {
            return Err(BundleError::SizeLimitExceeded);
        }
        self.take(length)
    }

    fn string(&mut self, max: usize) -> Result<String, BundleError> {
        let value =
            core::str::from_utf8(self.byte_string(max)?).map_err(|_| BundleError::InvalidUtf8)?;
        let mut owned = String::new();
        owned
            .try_reserve_exact(value.len())
            .map_err(|_| BundleError::AllocationFailed)?;
        owned.push_str(value);
        Ok(owned)
    }

    fn option_u32(&mut self) -> Result<Option<u32>, BundleError> {
        match self.u8()? {
            0 => Ok(None),
            1 => Ok(Some(self.u32()?)),
            tag => Err(BundleError::InvalidTag(tag)),
        }
    }

    fn component_type(&mut self, max: usize) -> Result<ComponentTypeId, BundleError> {
        Ok(ComponentTypeId::new(self.string(max)?, self.string(max)?))
    }

    fn option_component_type(
        &mut self,
        max: usize,
    ) -> Result<Option<ComponentTypeId>, BundleError> {
        match self.u8()? {
            0 => Ok(None),
            1 => Ok(Some(self.component_type(max)?)),
            tag => Err(BundleError::InvalidTag(tag)),
        }
    }

    fn state_ids(&mut self, max: usize) -> Result<Vec<StateFieldId>, BundleError> {
        let count = self.count(max)?;
        let mut values = reserved_vec(count)?;
        for _ in 0..count {
            values.push(StateFieldId::new(self.u32()?));
        }
        Ok(values)
    }

    fn u16s(&mut self, max: usize) -> Result<Vec<u16>, BundleError> {
        let count = self.count(max)?;
        let mut values = reserved_vec(count)?;
        for _ in 0..count {
            values.push(self.u16()?);
        }
        Ok(values)
    }

    fn array_32(&mut self) -> Result<[u8; 32], BundleError> {
        self.take(32)?
            .try_into()
            .map_err(|_| BundleError::Truncated)
    }
}

fn reserved_vec<T>(capacity: usize) -> Result<Vec<T>, BundleError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(capacity)
        .map_err(|_| BundleError::AllocationFailed)?;
    Ok(values)
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use vo_ui_core::{EventType, Listener, Primitive};
    use vo_ui_plan::{ComponentPlan, SlotKind, TemplateNode, UpdateSite};

    fn plan(handler: Option<HandlerId>) -> ValidatedPlan {
        let mut plan = ComponentPlan::new(LocalNodeId::new(0));
        plan.slots.push(SlotKind::Text);
        let mut root = TemplateNode::element(LocalNodeId::new(0), Primitive::Button)
            .child(LocalNodeId::new(1));
        if let Some(handler) = handler {
            root = root.listener(Listener::new(EventType::CLICK, handler));
        }
        plan.nodes.push(root);
        plan.nodes.push(TemplateNode::text(LocalNodeId::new(1), ""));
        plan.updates
            .push(UpdateSite::text(SlotId::new(0), LocalNodeId::new(1)));
        plan.validate(PlanLimits::default()).unwrap()
    }

    fn definition(
        type_id: ComponentTypeId,
        handler: HandlerId,
        call_sites: Vec<ComponentCallSite>,
    ) -> ComponentDefinition {
        ComponentDefinition {
            type_id,
            display_name: "Component".into(),
            mode: super::super::ExecutionMode::RootFallback,
            interface: ComponentInterface::empty(),
            plan: plan(Some(handler)),
            call_sites,
            states: vec![StateFieldDefinition {
                id: StateFieldId::new(7),
                key: "count".into(),
                type_fingerprint: 42,
                value_kind: StateValueKind::Int,
                has_initializer: true,
                initializer_func: None,
                initializer_dependencies: vec![],
                initializer_props: vec![],
            }],
            bindings: vec![BindingDefinition {
                id: BindingId::new(9),
                evaluator_func: None,
                slots: vec![SlotId::new(0)],
                dependencies: vec![StateFieldId::new(7)],
                prop_dependencies: vec![],
            }],
            handlers: vec![HandlerDefinition {
                id: HandlerSiteId::new(11),
                plan_handler: handler,
                evaluator_func: None,
                captured_state: vec![StateFieldId::new(7)],
                captured_props: vec![],
            }],
            effects: vec![EffectDefinition {
                id: EffectId::new(13),
                start_func: 20,
                cleanup_func: Some(21),
                dependencies: vec![StateFieldId::new(7)],
                prop_dependencies: vec![],
            }],
            tasks: vec![TaskDefinition {
                id: TaskSiteId::new(15),
                start_func: 22,
                reducer_func: 23,
                policy: TaskPolicy::RestartLatest,
                dependencies: vec![StateFieldId::new(7)],
                prop_dependencies: vec![],
            }],
            lifecycle: LifecycleDefinition {
                mounted_func: Some(24),
                updated_func: Some(25),
                disposing_func: Some(26),
            },
            reload_schema_fingerprint: 99,
        }
    }

    fn fixture() -> ComponentBundle {
        let child = ComponentTypeId::new("github.com/acme/app", "main.Child");
        let root = ComponentTypeId::new("github.com/acme/app", "main.Root");
        let mut child_definition = definition(child.clone(), HandlerId::new(1, 1), vec![]);
        child_definition.interface.props_arity = 1;
        let root_definition = definition(
            root.clone(),
            HandlerId::new(2, 1),
            vec![ComponentCallSite {
                id: ComponentCallSiteId::new(101),
                mode: ComponentCallMode::Static,
                callee: Some(child),
                mount_parent: LocalNodeId::new(0),
                mount_before: Some(LocalNodeId::new(1)),
                props_bindings: vec![BindingId::new(9)],
                key_binding: None,
            }],
        );
        ComponentBundle {
            abi_version: COMPONENT_BUNDLE_ABI_VERSION,
            module_identity: "github.com/acme/app".into(),
            root,
            linked_modules: vec![],
            definitions: vec![child_definition, root_definition],
            imports: vec![BundleImportRequirement {
                module_identity: "github.com/acme/ui".into(),
                min_abi_version: COMPONENT_BUNDLE_ABI_VERSION,
                max_abi_version: COMPONENT_BUNDLE_ABI_VERSION,
                bundle_digest: [7; 32],
                component_types: vec![ComponentTypeId::new("github.com/acme/ui", "kit.Button")],
            }],
            capabilities: vec!["ui.core".into(), "ui.task".into()],
            source: BundleSourceMetadata {
                source_digest: [9; 32],
                compiler_identity: "vo-ui-compiler/0.1.4".into(),
                reload_schema_version: 1,
            },
        }
    }

    #[test]
    fn component_bundle_roundtrips_canonically() {
        let bundle = fixture();
        let bytes =
            encode_component_bundle(&bundle, BundleLimits::default(), PlanLimits::default())
                .unwrap();
        let decoded =
            decode_component_bundle(&bytes, BundleLimits::default(), PlanLimits::default())
                .unwrap();
        assert_eq!(decoded, bundle);
        assert_eq!(
            encode_component_bundle(&decoded, BundleLimits::default(), PlanLimits::default())
                .unwrap(),
            bytes
        );
    }

    #[test]
    fn bundle_decoder_rejects_every_truncation_and_trailing_bytes() {
        let bytes =
            encode_component_bundle(&fixture(), BundleLimits::default(), PlanLimits::default())
                .unwrap();
        for end in 0..bytes.len() {
            assert!(decode_component_bundle(
                &bytes[..end],
                BundleLimits::default(),
                PlanLimits::default()
            )
            .is_err());
        }
        let mut trailing = bytes;
        trailing.push(0);
        assert_eq!(
            decode_component_bundle(&trailing, BundleLimits::default(), PlanLimits::default()),
            Err(BundleError::TrailingBytes)
        );
    }

    #[test]
    fn bundle_rejects_noncanonical_tables_missing_targets_and_static_cycles() {
        let mut bundle = fixture();
        bundle.definitions.swap(0, 1);
        assert_eq!(
            validate_component_bundle(&bundle, BundleLimits::default()),
            Err(BundleError::DefinitionsNotCanonical)
        );

        let mut bundle = fixture();
        bundle.definitions[1].call_sites[0].callee =
            Some(ComponentTypeId::new("github.com/acme/missing", "Widget"));
        assert!(matches!(
            validate_component_bundle(&bundle, BundleLimits::default()),
            Err(BundleError::MissingComponent(_))
        ));

        let mut bundle = fixture();
        bundle.definitions[0].call_sites.push(ComponentCallSite {
            id: ComponentCallSiteId::new(77),
            mode: ComponentCallMode::Static,
            callee: Some(bundle.root.clone()),
            mount_parent: LocalNodeId::new(0),
            mount_before: None,
            props_bindings: vec![],
            key_binding: None,
        });
        assert!(matches!(
            validate_component_bundle(&bundle, BundleLimits::default()),
            Err(BundleError::StaticComponentCycle(_))
        ));
    }

    #[test]
    fn bundle_rejects_invalid_mount_binding_and_task_policy() {
        let mut bundle = fixture();
        bundle.definitions[1].call_sites[0].mount_parent = LocalNodeId::new(1);
        assert_eq!(
            validate_component_bundle(&bundle, BundleLimits::default()),
            Err(BundleError::InvalidMountParent(ComponentCallSiteId::new(
                101
            )))
        );

        let mut bundle = fixture();
        bundle.definitions[1].call_sites[0].props_bindings = vec![BindingId::new(999)];
        assert_eq!(
            validate_component_bundle(&bundle, BundleLimits::default()),
            Err(BundleError::MissingBinding(BindingId::new(999)))
        );

        let mut bundle = fixture();
        bundle.definitions[1].tasks[0].policy = TaskPolicy::ParallelBounded { max_in_flight: 0 };
        assert_eq!(
            validate_component_bundle(&bundle, BundleLimits::default()),
            Err(BundleError::InvalidTaskPolicy)
        );
    }
}
